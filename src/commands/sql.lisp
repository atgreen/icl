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
;;; `,sql <query>' cell into a backend form that runs DuckDB and returns the
;;; result as a Lisp-Stat data frame — which the notebook already renders as an
;;; interactive Perspective grid. The form registers any bound data frame named
;;; in the query as a view, then reads the result back with lisp-stat:read-csv.

(defun %lisp-string (s)
  "S as a readable Lisp string literal (escaping \\ and \")."
  (with-output-to-string (o)
    (write-char #\" o)
    (loop for c across s do
      (when (or (char= c #\") (char= c #\\)) (write-char #\\ o))
      (write-char c o))
    (write-char #\" o)))

(defun %replace-all (string part replacement)
  "Every occurrence of PART in STRING replaced by REPLACEMENT."
  (with-output-to-string (o)
    (loop with plen = (length part) with i = 0
          for pos = (search part string :start2 i)
          do (cond (pos (write-string string o :start i :end pos)
                        (write-string replacement o)
                        (setf i (+ pos plen)))
                   (t (write-string string o :start i) (loop-finish))))))

(defparameter +sql-cell-template+
  "(labels ((split-line (line)
             (let ((fields '()) (buf (make-string-output-stream)) (inq nil)
                   (i 0) (n (length line)))
               (loop while (< i n) for c = (char line i) do
                 (cond (inq (cond ((char= c #\\\")
                                   (if (and (< (1+ i) n) (char= (char line (1+ i)) #\\\"))
                                       (progn (write-char #\\\" buf) (incf i))
                                       (setf inq nil)))
                                  (t (write-char c buf))))
                       ((char= c #\\\") (setf inq t))
                       ((char= c #\\,) (push (get-output-stream-string buf) fields))
                       (t (write-char c buf)))
                 (incf i))
               (push (get-output-stream-string buf) fields)
               (nreverse fields)))
           (coerce-val (s)
             (if (zerop (length s)) nil
                 (let ((*read-eval* nil))
                   (multiple-value-bind (v pos) (ignore-errors (read-from-string s nil nil))
                     (if (and (realp v) (eql pos (length s))) v s))))))
    (let* ((query @@QUERY@@)
           (names '@@NAMES@@)
           (script
            (with-output-to-string (s)
              (write-string @@PROLOGUE@@ s)
              (dolist (nm names)
                (let ((sym (ignore-errors (find-symbol (string-upcase nm)))))
                  (when (and sym (boundp sym) (find-package :data-frame)
                             (ignore-errors
                              (typep (symbol-value sym)
                                     (find-symbol \"DATA-FRAME\" :data-frame))))
                    (let ((p (format nil \"/tmp/icl-sql-~A.csv\" nm))
                          (w (and (find-package :lisp-stat)
                                  (find-symbol \"WRITE-CSV\" :lisp-stat))))
                      (when (and w (fboundp w)
                                 (ignore-errors (funcall w (symbol-value sym) p) t))
                        (format s \"CREATE OR REPLACE VIEW ~A AS SELECT * FROM read_csv_auto('~A');~%\"
                                nm p))))))
              (write-string query s)
              (unless (and (plusp (length query))
                           (char= (char query (1- (length query))) #\\;))
                (write-char #\\; s)))))
      (multiple-value-bind (out err code)
          (uiop:run-program (list \"duckdb\" \"-csv\" \"-c\" script)
                            :output :string :error-output :string :ignore-error-status t)
        (unless (eql code 0)
          (error \"duckdb: ~A\" (string-trim '(#\\Newline #\\Space) (or err \"\"))))
        (let* ((lines (remove-if (lambda (l) (zerop (length l)))
                                 (uiop:split-string
                                  (string-right-trim '(#\\Newline #\\Return) out)
                                  :separator (string #\\Newline))))
               (cols (and lines (mapcar (lambda (c) (intern (string-upcase c) :keyword))
                                        (split-line (first lines))))))
          (mapcar (lambda (line)
                    (loop for k in cols for v in (split-line line)
                          append (list k (coerce-val v))))
                  (rest lines))))))"
  "Backend form template for a ,sql notebook cell; @@QUERY@@/@@NAMES@@ filled in.
Returns a list of plists (rows) — which the notebook renders as a grid — with no
Lisp-Stat dependency, so it works in any notebook.")

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
  "When notebook cell SOURCE is a ,source declaration, return (NAME TYPE CONNINFO)."
  (let ((s (string-left-trim '(#\Space #\Tab #\Newline #\Return) (or source ""))))
    (when (and (>= (length s) 8) (string-equal (subseq s 0 8) ",source "))
      (%parse-source-decl (subseq s 8)))))

(defun notebook-sql-sources (nb)
  "The (NAME TYPE CONNINFO) sources declared by NB's `,source' cells."
  (when nb
    (loop for cell across (notebook-cells nb)
          for decl = (sql-source-magic-p (notebook-cell-source cell))
          when decl collect decl)))

(defun sql-cell-form (query &optional (prologue ""))
  "Backend Lisp source (string) that runs QUERY (after any source-attach PROLOGUE)
   and returns rows as plists."
  (%replace-all
   (%replace-all
    (%replace-all +sql-cell-template+ "@@QUERY@@" (%lisp-string query))
    "@@NAMES@@" (prin1-to-string (%sql-identifiers query)))
   "@@PROLOGUE@@" (%lisp-string (or prologue ""))))

;;; ─── The command ─────────────────────────────────────────────────────────────

(defun %parse-sql-tokens (tokens)
  "Split TOKENS into (values out-name sql-string). A leading `-o NAME' captures
   the output variable name; the rest is rejoined as the query."
  (if (and (stringp (first tokens)) (string= (first tokens) "-o") (second tokens))
      (values (second tokens) (format nil "~{~A~^ ~}" (cddr tokens)))
      (values nil (format nil "~{~A~^ ~}" tokens))))

(define-command sql (&rest tokens)
  "Query data with SQL (DuckDB).
Examples:
  ,sql SELECT * FROM 'data.csv' LIMIT 5
  ,sql -o hot SELECT comm, count(*) AS n FROM df GROUP BY comm ORDER BY n DESC
Reference a CSV/Parquet/JSON file in quotes, or a session data-frame by name.
With -o NAME, the result is also bound to *NAME* in your session.
Requires the `duckdb' CLI on your PATH (https://duckdb.org/)."
  (unless (duckdb-available-p)
    (format *error-output*
            "~&,sql needs the `duckdb' CLI on your PATH. Install it from ~
             https://duckdb.org/ and retry.~%")
    (return-from cmd-sql))
  (multiple-value-bind (out-name sql) (%parse-sql-tokens tokens)
    (when (zerop (length (string-trim '(#\Space #\Tab) sql)))
      (format *error-output* "~&Usage: ,sql [-o NAME] <query>~%")
      (return-from cmd-sql))
    (handler-case
        (let ((views (%dump-session-dataframes (%sql-identifiers sql))))
          (multiple-value-bind (columns rows) (run-sql sql :views views
                                                           :sources (%global-sources))
            (%format-table columns rows)
            (when (and out-name *slynk-connected-p*)
              ;; Re-read the result into a backend data frame for reuse/rendering.
              (let ((csv (format nil "/tmp/icl-sql-result.csv")))
                (ignore-errors
                 (with-open-file (s csv :direction :output :if-exists :supersede
                                        :if-does-not-exist :create)
                   (format s "~{~A~^,~}~%" columns)
                   (dolist (r rows) (format s "~{~A~^,~}~%" r)))
                 (backend-eval-internal
                  (format nil "(defparameter *~A* (lisp-stat:read-csv #P~S))"
                          out-name csv))
                 (format t "~&; bound to *~A*~%" out-name))))))
      (error (e)
        (format *error-output* "~&,sql: ~A~%" e)))))

(define-command source (&rest tokens)
  "Attach a SQL source for ,sql to query (SQLite, Postgres, or a DuckDB file).
Examples:
  ,source cache sqlite /var/tmp/cache.db
  ,source pg postgres host=localhost dbname=app user=me
Then:  ,sql SELECT * FROM pg.public.users u JOIN cache.orders o USING (user_id)
Conninfo may use ${ENV_VARS}; passwords come from the environment (PGPASSWORD) or
~/.pgpass, not the command. In a NOTEBOOK use a ,source CELL instead — it's saved
with the notebook and scoped to it. With no args, lists registered sources."
  (if (null tokens)
      (if (zerop (hash-table-count *sql-sources*))
          (format t "~&No SQL sources registered.  e.g.  ,source cache sqlite /tmp/x.db~%")
          (dolist (src (%global-sources))
            (destructuring-bind (name type conninfo) src
              (format t "~&  ~A  ~(~A~)  ~A~%" name type conninfo))))
      (let ((decl (%parse-source-decl (format nil "~{~A~^ ~}" tokens))))
        (if decl
            (destructuring-bind (name type conninfo) decl
              (register-sql-source name type conninfo)
              (format t "~&; source ~A (~(~A~)) registered~%" name type))
            (format *error-output*
                    "~&Usage: ,source NAME TYPE CONNINFO  (TYPE: sqlite | postgres | duckdb)~%")))))
