;;; sql.lisp --- The ,sql command: query data with DuckDB.
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; MVP: shells out to the `duckdb' CLI. DuckDB reads CSV/Parquet/JSON files
;;; directly, so `,sql SELECT * FROM 'data.csv'' works with no load step — the
;;; natural companion to the data-file notebook templates. Session data frames
;;; referenced by name are exported to temporary CSV and registered as views
;;; (best-effort; requires a CSV writer in the backend). The eventual native
;;; libduckdb + Arrow binding (zero-copy, results straight into Perspective)
;;; replaces the CLI without changing this surface.

(in-package #:icl)

(defvar *duckdb-program* "duckdb"
  "The DuckDB CLI executable used by the ,sql command.")

(defun duckdb-available-p ()
  "T when the DuckDB CLI (*duckdb-program*) can be run."
  (handler-case
      (progn (uiop:run-program (list *duckdb-program* "--version")
                               :output :string :error-output nil)
             t)
    (error () nil)))

;;; ─── CSV parsing (DuckDB's -csv output) ──────────────────────────────────────

(defun %parse-csv-line (line)
  "Parse one RFC-4180-ish CSV LINE into a list of field strings (handles quoted
   fields containing commas and doubled quotes; assumes no embedded newlines)."
  (let ((fields '()) (buf (make-string-output-stream)) (inq nil) (i 0)
        (n (length line)))
    (loop while (< i n)
          for c = (char line i) do
            (cond
              (inq (cond ((char= c #\")
                          (if (and (< (1+ i) n) (char= (char line (1+ i)) #\"))
                              (progn (write-char #\" buf) (incf i))
                              (setf inq nil)))
                         (t (write-char c buf))))
              ((char= c #\") (setf inq t))
              ((char= c #\,) (push (get-output-stream-string buf) fields))
              (t (write-char c buf)))
            (incf i))
    (push (get-output-stream-string buf) fields)
    (nreverse fields)))

(defun %parse-csv (text)
  "Parse DuckDB CSV TEXT into (values columns rows): COLUMNS a list of header
   strings, ROWS a list of field-string lists."
  (let ((lines (remove-if (lambda (l) (zerop (length l)))
                          (uiop:split-string
                           (string-right-trim '(#\Newline #\Return) text)
                           :separator (string #\Newline)))))
    (if (null lines)
        (values '() '())
        (values (%parse-csv-line (first lines))
                (mapcar #'%parse-csv-line (rest lines))))))

;;; ─── SQL sources: attach SQLite / Postgres / DuckDB databases ────────────────
;;;
;;; DuckDB federates: it can ATTACH other databases and query them as local
;;; tables, so one `,sql' reaches SQLite, Postgres, DuckDB files, CSV/Parquet, and
;;; your session data frames — joinable in a single query. Sources come from two
;;; scopes: global (registered in ~/.iclrc via REGISTER-SQL-SOURCE) and
;;; notebook-local (`,source' cells, gathered per notebook). A source is a list
;;; (NAME TYPE CONNINFO); TYPE is :postgres | :sqlite | :duckdb.

(defvar *sql-sources* (make-hash-table :test 'equal)
  "Global named SQL sources: NAME -> (TYPE . CONNINFO). Populated from ~/.iclrc.")

(defun %source-type (type)
  "Normalize a source TYPE designator to :postgres | :sqlite | :duckdb."
  (let ((s (string-downcase (string type))))
    (cond ((member s '("postgres" "postgresql" "pg") :test #'string=) :postgres)
          ((member s '("sqlite" "sqlite3") :test #'string=) :sqlite)
          ((member s '("duckdb" "ddb") :test #'string=) :duckdb)
          (t (error "unknown SQL source type ~S (use postgres, sqlite, or duckdb)" type)))))

(defun register-sql-source (name type conninfo)
  "Register a global SQL source NAME (string) of TYPE (:postgres/:sqlite/:duckdb)
   with CONNINFO (a connection string or file path). Intended for ~/.iclrc.
   `,sql' then reaches it as tables under NAME (e.g. NAME.schema.table)."
  (setf (gethash (string name) *sql-sources*) (cons (%source-type type) conninfo))
  name)

(defun %global-sources ()
  "The globally-registered sources as (NAME TYPE CONNINFO) triples."
  (loop for name being the hash-keys of *sql-sources* using (hash-value tc)
        collect (list name (car tc) (cdr tc))))

(defun %expand-env (str)
  "Expand ${VAR} references in STR from the environment (empty if unset) — so a
   source's conninfo carries the shape of a connection, not embedded secrets."
  (with-output-to-string (o)
    (loop with i = 0 with n = (length str)
          while (< i n)
          do (let ((c (char str i)))
               (cond ((and (char= c #\$) (< (1+ i) n) (char= (char str (1+ i)) #\{)
                           (position #\} str :start (+ i 2)))
                      (let ((close (position #\} str :start (+ i 2))))
                        (write-string (or (uiop:getenv (subseq str (+ i 2) close)) "") o)
                        (setf i (1+ close))))
                     (t (write-char c o) (incf i)))))))

(defun %sql-quote (str)
  "STR with single quotes doubled, for embedding in a SQL string literal."
  (with-output-to-string (o)
    (loop for c across str do (when (char= c #\') (write-char #\' o)) (write-char c o))))

(defun %source-attach-sql (name type conninfo)
  "The INSTALL/LOAD/ATTACH statements that make source NAME available in DuckDB."
  (let ((ci (%sql-quote (%expand-env conninfo))))
    (ecase type
      (:postgres (format nil "INSTALL postgres; LOAD postgres; ATTACH '~A' AS ~A (TYPE POSTGRES);~%" ci name))
      (:sqlite   (format nil "INSTALL sqlite; LOAD sqlite; ATTACH '~A' AS ~A (TYPE SQLITE);~%" ci name))
      (:duckdb   (format nil "ATTACH '~A' AS ~A;~%" ci name)))))

(defun %sources-prologue (sources)
  "DuckDB prologue (a string) attaching each SOURCE, a (NAME TYPE CONNINFO) triple."
  (with-output-to-string (s)
    (dolist (src sources)
      (destructuring-bind (name type conninfo) src
        (write-string (%source-attach-sql name type conninfo) s)))))

;;; ─── ${...} interpolation: splice backend Lisp values into the query ─────────
;;;
;;; `,sql SELECT * FROM 'people.csv' WHERE age > ${*min-age*}' evaluates the Lisp
;;; form inside ${...} in the backend and substitutes its value as a SQL literal
;;; (numbers inline, strings quoted, lists as `(a, b)' for IN). Values are always
;;; rendered as literals, so a string value can't inject SQL.

(defun %sql-literal (value)
  "Render a Lisp VALUE as a DuckDB SQL literal."
  (cond ((null value) "NULL")
        ((eq value t) "TRUE")
        ((numberp value) (princ-to-string value))
        ((listp value) (format nil "(~{~A~^, ~})" (mapcar #'%sql-literal value)))
        (t (format nil "'~A'" (%sql-quote (if (stringp value) value (princ-to-string value)))))))

(defun %backend-value (expr-string)
  "Evaluate EXPR-STRING in the backend (without touching *) and return its value."
  (unless *slynk-connected-p*
    (error "a running backend is needed for ${...} interpolation"))
  (let ((*read-eval* nil))
    (read-from-string (or (first (backend-eval-internal expr-string)) "nil"))))

(defun %interpolate-sql (query &optional (resolver #'%backend-value))
  "Replace each ${LISP-FORM} in QUERY with the SQL literal of RESOLVER's value for
   that form (RESOLVER takes the form's source text and returns a Lisp value)."
  (if (not (search "${" query))
      query
      (with-output-to-string (o)
        (let ((i 0) (n (length query)))
          (loop while (< i n) do
            (let ((c (char query i)))
              (if (and (char= c #\$) (< (1+ i) n) (char= (char query (1+ i)) #\{))
                  (let ((close (position #\} query :start (+ i 2))))
                    (if close
                        (progn
                          (write-string
                           (%sql-literal (funcall resolver (subseq query (+ i 2) close))) o)
                          (setf i (1+ close)))
                        (progn (write-char c o) (incf i))))
                  (progn (write-char c o) (incf i)))))))))

;;; ─── The engine ──────────────────────────────────────────────────────────────

(defun run-sql (sql &key views sources (program *duckdb-program*))
  "Run SQL through the DuckDB CLI and return (values columns rows). VIEWS is an
   alist of (view-name . csv-pathname) registered with CREATE VIEW; SOURCES is a
   list of (NAME TYPE CONNINFO) attached before the query. Signals an error
   carrying DuckDB's stderr on failure."
  (let ((script (with-output-to-string (s)
                  (write-string (%sources-prologue sources) s)
                  (loop for (name . path) in views do
                    (format s "CREATE OR REPLACE VIEW ~A AS ~
                               SELECT * FROM read_csv_auto('~A');~%"
                            name (namestring path)))
                  (write-string sql s)
                  (unless (and (plusp (length sql))
                               (char= (char sql (1- (length sql))) #\;))
                    (write-char #\; s)))))
    (multiple-value-bind (out err code)
        (uiop:run-program (list program "-csv" "-c" script)
                          :output :string :error-output :string
                          :ignore-error-status t)
      (unless (eql code 0)
        (error "duckdb: ~A" (string-trim '(#\Newline #\Space) (or err ""))))
      (%parse-csv out))))

;;; ─── Pretty-printing a result table ──────────────────────────────────────────

(defun %format-table (columns rows &key (stream *standard-output*) (max-rows 50))
  "Print COLUMNS/ROWS as an aligned text table to STREAM (truncated to MAX-ROWS)."
  (if (null columns)
      (format stream "~&(no columns)~%")
      (let* ((shown (if (> (length rows) max-rows) (subseq rows 0 max-rows) rows))
             (widths (mapcar (lambda (col j)
                               (reduce #'max shown
                                       :key (lambda (r) (length (or (nth j r) "")))
                                       :initial-value (length col)))
                             columns (loop for j below (length columns) collect j))))
        (flet ((row (cells)
                 (format stream "~&~{~A~^  ~}~%"
                         (mapcar (lambda (c w) (format nil "~vA" w (or c ""))) cells widths))))
          (row columns)
          (row (mapcar (lambda (w) (make-string w :initial-element #\-)) widths))
          (dolist (r shown) (row r))
          (format stream "~&~[no rows~:;~:*~D row~:p~]~@[ (showing first ~D)~]~%"
                  (length rows) (when (> (length rows) max-rows) max-rows))))))

(defun %format-plist-table (rows &key (stream *standard-output*) (max-rows 50))
  "Print a list of plists (as returned by the backend ,sql form) as a table."
  (if (null rows)
      (format stream "~&no rows~%")
      (let ((cols (loop for (k) on (first rows) by #'cddr collect k)))
        (%format-table
         (mapcar (lambda (k) (string-downcase (symbol-name k))) cols)
         (mapcar (lambda (row)
                   (mapcar (lambda (k) (let ((v (getf row k)))
                                         (if (null v) "" (princ-to-string v))))
                           cols))
                 rows)
         :stream stream :max-rows max-rows))))

;;; ─── Session data-frame bridge (best-effort) ─────────────────────────────────

(defun %sql-identifiers (sql)
  "Bare identifiers appearing in SQL — candidate data-frame variable names."
  (let ((out '()) (buf (make-string-output-stream)))
    (flet ((flush () (let ((s (get-output-stream-string buf)))
                       (when (plusp (length s)) (pushnew s out :test #'string-equal)))))
      (loop for c across sql do
        (if (or (alphanumericp c) (char= c #\_) (char= c #\-))
            (write-char c buf)
            (flush)))
      (flush))
    out))

(defun %dump-session-dataframes (names)
  "For each candidate NAME bound to a data frame in the backend, write it to a
   temp CSV and return an alist of (name . pathname). Best-effort: returns NIL if
   the backend has no data frames or no CSV writer. Never signals."
  (when (and names *slynk-connected-p*)
    (ignore-errors
     (let* ((form
              (format nil
                      "(let ((out '()))~
                         (dolist (nm '(~{~S~^ ~}) (nreverse out))~
                           (let ((sym (find-symbol (string-upcase nm))))~
                             (when (and sym (boundp sym)~
                                        (find-package :data-frame)~
                                        (typep (symbol-value sym)~
                                               (find-symbol \"DATA-FRAME\" :data-frame)))~
                               (let ((p (format nil \"/tmp/icl-sql-~~A.csv\" nm)))~
                                 (ignore-errors~
                                   (funcall (find-symbol \"WRITE-CSV\" :lisp-stat)~
                                            (symbol-value sym) p)~
                                   (push (cons nm p) out)))))))"
                      names))
            (result (first (backend-eval-internal form))))
       ;; RESULT is the backend's printed alist like ((\"df\" . \"/tmp/..\")).
       (when (and (stringp result) (plusp (length result)))
         (let ((*read-eval* nil))
           (ignore-errors (read-from-string result))))))))

;;; ─── Notebook cell magic:  ,sql <query>  ->  a data frame (renders as a grid) ──
;;;
;;; A notebook cell evaluates its source as Lisp in the backend, so the terminal
;;; ,sql command (which prints text) does nothing there. Instead we rewrite a
;;; `,sql <query>' cell into a backend form — (icl-sql:run ...) — that runs DuckDB
;;; via the injected ICL-SQL protocol and returns the result through the selected
;;; adaptor (a data-frame, vellum table, or plists), which the notebook renders as
;;; an interactive Perspective grid.

(defun sql-cell-magic-p (source)
  "T when notebook cell SOURCE is a ,sql magic; returns (values t query) then."
  (let ((s (string-left-trim '(#\Space #\Tab #\Newline #\Return) (or source ""))))
    (when (and (>= (length s) 5)
               (string-equal (subseq s 0 5) ",sql ")
               t)
      (values t (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq s 5))))))

(defun %parse-source-decl (text)
  "Parse a `NAME TYPE CONNINFO' declaration into (NAME TYPE CONNINFO), or NIL.
   CONNINFO is the rest of the line and may contain spaces."
  (let* ((s (string-trim '(#\Space #\Tab) text))
         (p1 (position #\Space s)))
    (when (and p1 (plusp p1))
      (let* ((name (subseq s 0 p1))
             (rest (string-left-trim " " (subseq s p1)))
             (p2 (position #\Space rest)))
        (when p2
          (ignore-errors
           (list name (%source-type (subseq rest 0 p2))
                 (string-trim " " (subseq rest (1+ p2))))))))))

(defun sql-source-magic-p (source)
  "When notebook cell SOURCE is a ,attach declaration, return (NAME TYPE CONNINFO)."
  (let ((s (string-left-trim '(#\Space #\Tab #\Newline #\Return) (or source ""))))
    (when (and (>= (length s) 8) (string-equal (subseq s 0 8) ",attach "))
      (%parse-source-decl (subseq s 8)))))

(defun notebook-sql-sources (nb)
  "The (NAME TYPE CONNINFO) sources declared by NB's `,source' cells."
  (when nb
    (loop for cell across (notebook-cells nb)
          for decl = (sql-source-magic-p (notebook-cell-source cell))
          when decl collect decl)))

;;; ─── Result adaptors (backend CLOS protocol, extensible) ─────────────────────
;;;
;;; The adaptor protocol lives in the inferior Lisp as the ICL-SQL package
;;; (vendor/icl-sql-adaptors.lisp), injected on first use. An adaptor turns a
;;; query result (a list of plists) into a table object, back into rows (for the
;;; terminal table), and can read a data file directly (for data-file notebooks).
;;; Built-ins: `plist', `df' (Lisp-Stat data-frame), `vellum' (vellum table), and
;;; `auto' (the default: df if Lisp-Stat is loaded, else vellum if vellum is
;;; loaded, else plists). Register your own with `register-sql-adaptor'.

(defparameter +builtin-sql-adaptors+ '("auto" "df" "vellum" "plist")
  "Adaptor names defined by the injected ICL-SQL base module.")

(defvar *sql-adaptors* (make-hash-table :test 'equal)
  "User-registered ,sql adaptors: lowercase name -> backend source (a string)
that defines and registers the adaptor in the inferior Lisp. Built-ins are not
listed here; they come from the injected ICL-SQL module.")

(defun %inject-sql-adaptor-source (source)
  "Load one adaptor SOURCE string into the inferior Lisp (best effort)."
  (when (and *slynk-connected-p* source)
    (slynk-client:slime-eval
     `(cl:with-input-from-string (cl-user::icl-sql-stream ,source)
        (cl:handler-bind ((cl:warning #'cl:muffle-warning))
          (cl:load cl-user::icl-sql-stream))
        t)
     *slynk-connection*)))

(defun inject-user-sql-adaptors ()
  "Inject every user-registered adaptor source into the inferior Lisp."
  (maphash (lambda (name source)
             (declare (ignore name))
             (ignore-errors (%inject-sql-adaptor-source source)))
           *sql-adaptors*))

(defun register-sql-adaptor (name source)
  "Register a ,sql result adaptor NAME, selectable with `--adaptor NAME' (`-a').
SOURCE is backend Lisp source (a string) that defines and registers an
ICL-SQL:ADAPTOR in the inferior Lisp — most simply with ICL-SQL:DEFINE-SIMPLE-ADAPTOR:

  (icl:register-sql-adaptor \"firsts\"
    \"(icl-sql:define-simple-adaptor \\\"firsts\\\" (rows)
       (mapcar (lambda (r) (getf r :name)) rows))\")

It is injected once the backend protocol is up (immediately if it already is)."
  (check-type name (or string symbol))
  (check-type source string)
  (let ((key (string-downcase (string name))))
    (setf (gethash key *sql-adaptors*) source)
    (when (and *slynk-connected-p* *sql-adaptors-injected*)
      (ignore-errors (%inject-sql-adaptor-source source)))
    key))

(defun list-sql-adaptors ()
  "Sorted names of all known ,sql adaptors (built-ins plus user-registered)."
  (sort (union (copy-list +builtin-sql-adaptors+)
               (loop for k being the hash-keys of *sql-adaptors* collect k)
               :test #'string=)
        #'string<))

(defun %known-sql-adaptor-p (name)
  "T if NAME is a built-in or user-registered ,sql adaptor."
  (let ((n (string-downcase (string (or name "auto")))))
    (or (member n +builtin-sql-adaptors+ :test #'string=)
        (nth-value 1 (gethash n *sql-adaptors*)))))

(defun sql-cell-form (query &optional (prologue "") (adaptor "auto"))
  "Backend Lisp source (string) that runs QUERY (after any source-attach PROLOGUE)
   through the injected ICL-SQL protocol and returns the result via the named
   ADAPTOR (default \"auto\"). Signals an error if ADAPTOR is unknown."
  (unless (%known-sql-adaptor-p adaptor)
    (error "unknown ,sql adaptor ~S (known: ~{~A~^, ~})" adaptor (list-sql-adaptors)))
  (format nil "(icl-sql:run ~A ~A (quote ~A) ~A)"
          (prin1-to-string (or prologue ""))
          (prin1-to-string query)
          (prin1-to-string (%sql-identifiers query))
          (prin1-to-string (string-downcase (string (or adaptor "auto"))))))

(defun %sql-display-form (adaptor)
  "Backend form (string) that turns `*' into a list of plists for the terminal
table, via ADAPTOR's ICL-SQL:RESULT->ROWS method."
  (format nil "(icl-sql:result->rows (icl-sql:find-adaptor ~A) *)"
          (prin1-to-string (string-downcase (string (or adaptor "auto"))))))

(defun %strip-adaptor-flag (query)
  "Strip a leading `--adaptor NAME' (or `-a NAME') from QUERY; returns
   (values adaptor query), with ADAPTOR defaulting to \"auto\"."
  (let ((s (string-left-trim '(#\Space #\Tab) (or query ""))))
    (dolist (flag '("--adaptor" "-a") (values "auto" query))
      (let ((n (length flag)))
        (when (and (>= (length s) n) (string-equal (subseq s 0 n) flag)
                   (or (= (length s) n) (member (char s n) '(#\Space #\Tab))))
          (let* ((rest (string-left-trim '(#\Space #\Tab) (subseq s n)))
                 (sp (position-if (lambda (c) (member c '(#\Space #\Tab))) rest)))
            (return (if sp
                        (values (subseq rest 0 sp)
                                (string-left-trim '(#\Space #\Tab) (subseq rest sp)))
                        (values rest "")))))))))

;;; ─── The command ─────────────────────────────────────────────────────────────

(defun %parse-sql-tokens (tokens)
  "Split TOKENS into (values out-name adaptor sql-string). Leading `-o NAME'
   captures the output variable name; leading `--adaptor NAME' selects the result
   adaptor (default \"auto\"). The rest is rejoined as the query."
  (let ((out-name nil) (adaptor "auto") (toks tokens))
    (loop
      (cond
        ((and (stringp (first toks)) (string= (first toks) "-o") (second toks))
         (setf out-name (second toks) toks (cddr toks)))
        ((and (stringp (first toks))
              (member (first toks) '("--adaptor" "-a") :test #'string=) (second toks))
         (setf adaptor (second toks) toks (cddr toks)))
        (t (return))))
    (values out-name adaptor (format nil "~{~A~^ ~}" toks))))

(define-command sql (&rest tokens)
  "Query data with SQL (DuckDB).
Examples:
  ,sql SELECT * FROM 'data.csv' LIMIT 5
  ,sql SELECT * FROM df WHERE age > ${*min-age*}        ; splice a Lisp value
  ,sql -o hot SELECT comm, count(*) AS n FROM df GROUP BY comm ORDER BY n DESC
Reference a CSV/Parquet/JSON file in quotes, or a session data-frame by name.
${LISP-FORM} evaluates in the backend and is spliced as a SQL literal (numbers
inline, strings quoted, lists as (a, b) for IN). The result is left in `*'
(usable by your next form); -o NAME also binds *NAME*. `--adaptor NAME' (short
`-a') picks the result type: `auto' (default) yields a Lisp-Stat data-frame or
vellum table if either is loaded, else plists; `df', `vellum', `plist', or one
you register with icl:register-sql-adaptor. Needs the `duckdb' CLI."
  (unless *slynk-connected-p*
    (format *error-output* "~&,sql needs a running backend.~%")
    (return-from cmd-sql))
  (unless (duckdb-available-p)
    (format *error-output*
            "~&,sql needs the `duckdb' CLI on your PATH. Install it from ~
             https://duckdb.org/ and retry.~%")
    (return-from cmd-sql))
  (multiple-value-bind (out-name adaptor sql) (%parse-sql-tokens tokens)
    (when (zerop (length (string-trim '(#\Space #\Tab) sql)))
      (format *error-output* "~&Usage: ,sql [-o NAME] [--adaptor|-a NAME] <query>~%")
      (return-from cmd-sql))
    (handler-case
        ;; Run the query IN THE BACKEND via the injected ICL-SQL protocol: session
        ;; data-frames are read in-process, and the result lands in the backend's
        ;; `*' (backend-eval updates history) — whatever the chosen ADAPTOR yields
        ;; (data-frame, vellum table, plists, ...). For the text table we ask the
        ;; adaptor to turn `*' back into rows, since those tables do not print
        ;; readably.
        (progn
          (ensure-sql-adaptors-injected)
          (let* ((form (sql-cell-form (%interpolate-sql sql)
                                      (%sources-prologue (%global-sources))
                                      adaptor))
                 (disp (progn (backend-eval form)
                              (ignore-errors
                               (backend-eval-internal (%sql-display-form adaptor)))))
                 (rows (ignore-errors
                        (let ((*read-eval* nil)) (read-from-string (or (first disp) ""))))))
            (%format-plist-table (and (listp rows) rows))
            (when out-name
              (ignore-errors
               (backend-eval-internal (format nil "(defparameter *~A* *)" out-name))
               (format t "~&; bound to *~A*~%" out-name)))))
      (error (e)
        (format *error-output* "~&,sql: ~A~%" e)))))

(define-command attach (&rest tokens)
  "Attach a database for ,sql to query (SQLite, Postgres, or a DuckDB file).
Examples:
  ,attach cache sqlite /var/tmp/cache.db
  ,attach pg postgres host=localhost dbname=app user=me
Then:  ,sql SELECT * FROM pg.public.users u JOIN cache.orders o USING (user_id)
Conninfo may use ${ENV_VARS}; passwords come from the environment (PGPASSWORD) or
~/.pgpass, not the command. In a NOTEBOOK use an ,attach CELL instead — it's saved
with the notebook and scoped to it. With no args, lists attached databases."
  (if (null tokens)
      (if (zerop (hash-table-count *sql-sources*))
          (format t "~&No databases attached.  e.g.  ,attach cache sqlite /tmp/x.db~%")
          (dolist (src (%global-sources))
            (destructuring-bind (name type conninfo) src
              (format t "~&  ~A  ~(~A~)  ~A~%" name type conninfo))))
      (let ((decl (%parse-source-decl (format nil "~{~A~^ ~}" tokens))))
        (if decl
            (destructuring-bind (name type conninfo) decl
              (register-sql-source name type conninfo)
              (format t "~&; attached ~A (~(~A~))~%" name type))
            (format *error-output*
                    "~&Usage: ,attach NAME TYPE CONNINFO  (TYPE: sqlite | postgres | duckdb)~%")))))
