;;; icl-sql-adaptors.lisp --- Backend ,sql result-adaptor protocol.
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; This module is injected into the inferior Lisp (like the cl-arrow adapter).
;;; It defines the ICL-SQL adaptor protocol: an adaptor turns the result of a
;;; DuckDB query — a list of plists — into a table object (a Lisp-Stat
;;; data-frame, a vellum table, a list of plists, or something you define), and
;;; knows how to turn that object back into rows (for the terminal table) and how
;;; to read a data file directly (for `--notebook data.csv').
;;;
;;; The frontend never manipulates these objects; it calls ICL-SQL:RUN /
;;; ICL-SQL:READ-SOURCE / ICL-SQL:RESULT->ROWS by name and marshals the printed
;;; result back. Neither Lisp-Stat nor vellum is required — each adaptor probes
;;; for its library at run time and falls back to plists when it is absent.

(cl:defpackage #:icl-sql
  (:use #:cl)
  (:export #:adaptor #:adaptor-name
           #:auto-adaptor #:df-adaptor #:vellum-adaptor #:plist-adaptor
           #:build-result #:result->rows #:read-source
           #:*adaptors* #:register-adaptor #:find-adaptor #:list-adaptors
           #:run #:define-simple-adaptor))

(cl:in-package #:icl-sql)

;;; ─── Protocol ────────────────────────────────────────────────────────────────

(defclass adaptor ()
  ((name :initarg :name :reader adaptor-name :initform "?"))
  (:documentation "Base class for ,sql result adaptors."))

(defgeneric build-result (adaptor rows)
  (:documentation "Turn ROWS (a list of plists) into ADAPTOR's table object."))

(defgeneric result->rows (adaptor result)
  (:documentation "Turn RESULT (an object ADAPTOR produced) back into a list of
plists, for the terminal's text table."))

(defgeneric read-source (adaptor path &key format)
  (:documentation "Read the data file at PATH into ADAPTOR's table object.
FORMAT defaults to :csv."))

;;; ─── Registry ────────────────────────────────────────────────────────────────

(defvar *adaptors* (make-hash-table :test 'equal)
  "Registry of adaptor instances, keyed by lowercase name.")

(defun register-adaptor (name instance)
  "Register adaptor INSTANCE under NAME (case-insensitive)."
  (setf (gethash (string-downcase (string name)) *adaptors*) instance)
  (string-downcase (string name)))

(defun find-adaptor (name)
  "The adaptor instance named NAME (case-insensitive), or NIL."
  (and name (gethash (string-downcase (string name)) *adaptors*)))

(defun list-adaptors ()
  "Sorted names of the registered adaptors."
  (sort (loop for k being the hash-keys of *adaptors* collect k) #'string<))

;;; ─── Shared helpers ──────────────────────────────────────────────────────────

(defun %keys (rows)
  "Column keys of a list of plists (from the first row)."
  (loop for (k v) on (first rows) by #'cddr collect k))

(defun %row-values (row keys) (mapcar (lambda (k) (getf row k)) keys))

(defun %split-csv-line (line)
  "Split one line of DuckDB CSV output into fields (handles quoting)."
  (let ((fields '()) (buf (make-string-output-stream)) (inq nil)
        (i 0) (n (length line)))
    (loop while (< i n) for c = (char line i) do
      (cond (inq (cond ((char= c #\")
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

(defun %coerce-val (s)
  "Coerce a CSV field string: empty -> NIL, a real number -> that number, else S."
  (if (zerop (length s)) nil
      (let ((*read-eval* nil))
        (multiple-value-bind (v pos) (ignore-errors (read-from-string s nil nil))
          (if (and (realp v) (eql pos (length s))) v s)))))

(defun %csv->rows (csv)
  "Parse DuckDB -csv output text CSV into a list of plists."
  (let* ((lines (remove-if (lambda (l) (zerop (length l)))
                           (uiop:split-string
                            (string-right-trim '(#\Newline #\Return) csv)
                            :separator (string #\Newline))))
         (cols (and lines (mapcar (lambda (c) (intern (string-upcase c) :keyword))
                                  (%split-csv-line (first lines))))))
    (mapcar (lambda (line)
              (loop for k in cols for v in (%split-csv-line line)
                    append (list k (%coerce-val v))))
            (rest lines))))

(defun %duckdb (script)
  "Run SCRIPT through the DuckDB CLI, returning its CSV output (signals on error)."
  (multiple-value-bind (out err code)
      (uiop:run-program (list "duckdb" "-csv" "-c" script)
                        :output :string :error-output :string :ignore-error-status t)
    (unless (eql code 0)
      (error "duckdb: ~A" (string-trim '(#\Newline #\Space) (or err ""))))
    out))

(defun %session-df-views (names)
  "SQL that exposes each session data-frame among NAMES as a DuckDB view (by
exporting it to a temp CSV). Returns a string (possibly empty)."
  (with-output-to-string (s)
    (dolist (nm names)
      (let ((sym (ignore-errors (find-symbol (string-upcase nm)))))
        (when (and sym (boundp sym) (find-package :data-frame)
                   (ignore-errors
                    (typep (symbol-value sym) (find-symbol "DATA-FRAME" :data-frame))))
          (let ((p (format nil "/tmp/icl-sql-~A.csv" nm))
                (w (and (find-package :lisp-stat)
                        (find-symbol "WRITE-CSV" :lisp-stat))))
            (when (and w (fboundp w)
                       (ignore-errors (funcall w (symbol-value sym) p) t))
              (format s "CREATE OR REPLACE VIEW ~A AS SELECT * FROM read_csv_auto('~A');~%"
                      nm p))))))))

;;; ─── Library bridges (all optional, probed at run time) ──────────────────────

(defun %make-df (rows)
  "A Lisp-Stat data-frame from ROWS, or NIL if the data-frame system is absent."
  (let ((f (and rows (find-package "DATA-FRAME") (find-symbol "MAKE-DF" "DATA-FRAME"))))
    (when (and f (fboundp f))
      (let ((keys (%keys rows)))
        (funcall f keys (mapcar (lambda (k) (map 'vector (lambda (r) (getf r k)) rows)) keys))))))

(defun %df-p (x)
  (let ((c (and (find-package "DATA-FRAME") (find-symbol "DATA-FRAME" "DATA-FRAME"))))
    (and c (find-class c nil) (typep x (find-class c nil)))))

(defun %df->rows (df)
  (let* ((keys (coerce (funcall (find-symbol "KEYS" "DATA-FRAME") df) 'list))
         (cols (coerce (funcall (find-symbol "COLUMNS" "DATA-FRAME") df) 'list))
         (n (if cols (length (elt cols 0)) 0)))
    (loop for i below n
          collect (loop for k in keys for col in cols append (list k (elt col i))))))

(defun %make-vellum (rows)
  "A vellum table from ROWS, or NIL if vellum is absent."
  (let ((f (and rows (find-package "VELLUM") (find-symbol "TO-TABLE" "VELLUM"))))
    (when (and f (fboundp f))
      (let ((keys (%keys rows)))
        (funcall f (mapcar (lambda (r) (%row-values r keys)) rows) :columns keys)))))

(defun %vellum-p (x)
  (let ((c (and (find-package "VELLUM.TABLE") (find-symbol "STANDARD-TABLE" "VELLUM.TABLE"))))
    (and c (find-class c nil) (typep x (find-class c nil)))))

(defun %vellum->rows (tbl)
  (let* ((names (funcall (find-symbol "COLUMN-NAMES" "VELLUM") tbl))
         (nc (length names))
         (nr (funcall (find-symbol "ROW-COUNT" "VELLUM") tbl))
         (at (find-symbol "AT" "VELLUM")))
    (loop for i below nr
          collect (loop for j below nc for nm in names
                        append (list (intern (string-upcase (princ-to-string nm)) :keyword)
                                     (funcall at tbl i j))))))

(defun %read-csv-plists (path)
  "Read a CSV file at PATH into a list of plists via DuckDB (no library needed)."
  (%csv->rows (%duckdb (format nil "SELECT * FROM read_csv_auto('~A');"
                               (namestring path)))))

;;; ─── Built-in adaptors ───────────────────────────────────────────────────────

(defclass plist-adaptor (adaptor) ())
(defmethod build-result ((a plist-adaptor) rows) rows)
(defmethod result->rows ((a plist-adaptor) result) result)
(defmethod read-source ((a plist-adaptor) path &key (format :csv))
  (declare (ignore format))
  (%read-csv-plists path))

(defclass df-adaptor (adaptor) ())
(defmethod build-result ((a df-adaptor) rows) (or (%make-df rows) rows))
(defmethod result->rows ((a df-adaptor) result)
  (if (%df-p result) (%df->rows result) result))
(defmethod read-source ((a df-adaptor) path &key (format :csv))
  (declare (ignore format))
  ;; Prefer Lisp-Stat's typed reader; otherwise read to plists and build a df.
  (let ((f (and (find-package "LISP-STAT") (find-symbol "READ-CSV" "LISP-STAT"))))
    (or (and f (fboundp f) (ignore-errors (funcall f path)))
        (%make-df (%read-csv-plists path))
        (%read-csv-plists path))))

(defclass vellum-adaptor (adaptor) ())
(defmethod build-result ((a vellum-adaptor) rows) (or (%make-vellum rows) rows))
(defmethod result->rows ((a vellum-adaptor) result)
  (if (%vellum-p result) (%vellum->rows result) result))
(defmethod read-source ((a vellum-adaptor) path &key (format :csv))
  (declare (ignore format))
  ;; Read to plists via DuckDB, then build a vellum table (reliable across
  ;; vellum versions; avoids vellum:copy-from's format-specific arglist).
  (or (%make-vellum (%read-csv-plists path)) (%read-csv-plists path)))

;; AUTO: data-frame if Lisp-Stat is loaded, else vellum if vellum is loaded, else
;; plists. RESULT->ROWS detects which of the three it actually got.
(defclass auto-adaptor (adaptor) ())
(defmethod build-result ((a auto-adaptor) rows)
  (or (%make-df rows) (%make-vellum rows) rows))
(defmethod result->rows ((a auto-adaptor) result)
  (cond ((%df-p result) (%df->rows result))
        ((%vellum-p result) (%vellum->rows result))
        (t result)))
(defmethod read-source ((a auto-adaptor) path &key (format :csv))
  (declare (ignore format))
  (cond ((and (find-package "DATA-FRAME") (find-symbol "MAKE-DF" "DATA-FRAME"))
         (read-source (or (find-adaptor "df") (make-instance 'df-adaptor)) path))
        ((and (find-package "VELLUM") (find-symbol "TO-TABLE" "VELLUM"))
         (read-source (or (find-adaptor "vellum") (make-instance 'vellum-adaptor)) path))
        (t (%read-csv-plists path))))

(register-adaptor "plist"  (make-instance 'plist-adaptor  :name "plist"))
(register-adaptor "df"     (make-instance 'df-adaptor     :name "df"))
(register-adaptor "vellum" (make-instance 'vellum-adaptor :name "vellum"))
(register-adaptor "auto"   (make-instance 'auto-adaptor   :name "auto"))

;;; ─── Convenience for user-defined adaptors ───────────────────────────────────

(defmacro define-simple-adaptor (name (rows-var) &body build-body)
  "Define and register an adaptor NAME whose BUILD-RESULT evaluates BUILD-BODY
with ROWS-VAR bound to the result rows (a list of plists). RESULT->ROWS and
READ-SOURCE fall back to plists unless you specialize them yourself."
  (let ((class (intern (format nil "~:@(~A~)-ADAPTOR" name))))
    `(progn
       (defclass ,class (adaptor) ())
       (defmethod build-result ((a ,class) ,rows-var) ,@build-body)
       (defmethod result->rows ((a ,class) result)
         (if (listp result) result (call-next-method)))
       (register-adaptor ,(string-downcase (string name))
                         (make-instance ',class :name ,(string-downcase (string name)))))))

(defmethod result->rows ((a adaptor) result)
  "Fallback: assume RESULT is already a list of plists."
  result)

;;; ─── Entry point used by the ICL frontend ────────────────────────────────────

(defun run (prologue query names adaptor-name)
  "Run QUERY through DuckDB (with any source-attach PROLOGUE and session
data-frame views for NAMES) and return the result via the named adaptor."
  (let* ((script (with-output-to-string (s)
                   (write-string (or prologue "") s)
                   (write-string (%session-df-views names) s)
                   (write-string query s)
                   (unless (and (plusp (length query))
                                (char= (char query (1- (length query))) #\;))
                     (write-char #\; s))))
         (rows (%csv->rows (%duckdb script)))
         (adaptor (or (find-adaptor (or adaptor-name "auto"))
                      (find-adaptor "auto"))))
    (build-result adaptor rows)))
