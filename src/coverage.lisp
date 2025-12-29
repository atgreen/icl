;;; coverage.lisp - Code coverage analysis with sb-cover
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package :icl)

;;; Coverage data storage (in-memory, works with remote Slynk)
(defvar *coverage-reports* (make-hash-table :test 'equal)
  "Storage for coverage report files, keyed by report ID.
   Each entry is a hash table mapping filename to content.")

(defvar *current-coverage-id* nil
  "The ID of the most recent coverage report.")

(defvar *coverage-counter* 0
  "Counter for generating unique coverage report IDs.")

;;; Coverage control functions

(defun backend-coverage-start ()
  "Enable coverage instrumentation in the backend."
  (unless *slynk-connected-p*
    (error "Not connected to backend"))
  (backend-eval-internal
   "(progn
      (require 'sb-cover)
      (proclaim (list 'optimize (list (find-symbol \"STORE-COVERAGE-DATA\" \"SB-COVER\") 3)))
      \"Coverage instrumentation enabled\")"))

(defun backend-coverage-stop ()
  "Disable coverage instrumentation in the backend."
  (unless *slynk-connected-p*
    (error "Not connected to backend"))
  (backend-eval-internal
   "(progn
      (require 'sb-cover)
      (proclaim (list 'optimize (list (find-symbol \"STORE-COVERAGE-DATA\" \"SB-COVER\") 0)))
      \"Coverage instrumentation disabled\")"))

(defun backend-coverage-reset ()
  "Clear all coverage data in the backend."
  (unless *slynk-connected-p*
    (error "Not connected to backend"))
  (backend-eval-internal
   "(progn
      (require 'sb-cover)
      (funcall (find-symbol \"RESET-COVERAGE\" \"SB-COVER\"))
      \"Coverage data cleared\")"))

(defun backend-coverage-load-file (filepath)
  "Load FILEPATH with coverage instrumentation enabled.
Uses COMPILE-FILE + LOAD to ensure coverage data is collected."
  (unless *slynk-connected-p*
    (error "Not connected to backend"))
  ;; Must use COMPILE-FILE (not just LOAD) to get coverage instrumentation
  ;; Use PROCLAIM with find-symbol since SB-COVER package doesn't exist until required
  (backend-eval-internal
   (format nil
           "(progn
              (require 'sb-cover)
              (proclaim (list 'optimize (list (find-symbol \"STORE-COVERAGE-DATA\" \"SB-COVER\") 3)))
              (let ((fasl (compile-file ~S :verbose t)))
                (load fasl)
                (delete-file fasl))
              (proclaim (list 'optimize (list (find-symbol \"STORE-COVERAGE-DATA\" \"SB-COVER\") 0)))
              \"File compiled and loaded with coverage\")"
           filepath)))

(defun backend-coverage-load-system (system-name)
  "Load SYSTEM-NAME with coverage instrumentation using ocicl/Quicklisp/ASDF."
  (unless *slynk-connected-p*
    (error "Not connected to backend"))
  (let* ((name (string-trim '(#\Space #\Tab #\") system-name))
         (loader-code (format nil "
(progn
  (require 'sb-cover)
  (proclaim (list 'optimize (list (find-symbol \"STORE-COVERAGE-DATA\" \"SB-COVER\") 3)))
  (flet ((try-load ()
           (let ((system '~A))
             (cond
               ;; Try ocicl first
               ((find-package '#:OCICL-RUNTIME)
                (progv (list (find-symbol \"*DOWNLOAD*\" '#:OCICL-RUNTIME)
                             (find-symbol \"*VERBOSE*\" '#:OCICL-RUNTIME))
                    (list t nil)
                  (asdf:load-system system :force t))
                (format t \"~~&Loaded ~~A via ocicl (with coverage)~~%\" system))
               ;; Try Quicklisp
               ((find-package '#:QUICKLISP)
                (funcall (find-symbol \"QUICKLOAD\" '#:QUICKLISP) system :silent nil :force t)
                (format t \"~~&Loaded ~~A via Quicklisp (with coverage)~~%\" system))
               ;; Fall back to plain ASDF
               ((find-package '#:ASDF)
                (asdf:load-system system :force t)
                (format t \"~~&Loaded ~~A via ASDF (with coverage)~~%\" system))
               (t
                (error \"No system loader available (ocicl, Quicklisp, or ASDF)\"))))))
    (try-load))
  (proclaim (list 'optimize (list (find-symbol \"STORE-COVERAGE-DATA\" \"SB-COVER\") 0)))
  (values))" name)))
    (backend-eval-internal loader-code)
    (fresh-line)
    nil))

(defun backend-coverage-json ()
  "Extract coverage data with line/column positions for Monaco display.
Returns JSON string with file contents and coverage annotations."
  (unless *slynk-connected-p*
    (error "Not connected to backend"))
  ;; This code runs in the backend to extract detailed coverage data
  (let* ((extract-code "
(handler-case
(progn
  (require 'sb-cover)

  ;; Helper: convert file position to (line . column), 1-based
  ;; Use labels for recursive navigate-to-subform
  (labels ((pos-to-line-col (content pos)
           (let ((line 1) (col 1))
             (loop for i from 0 below (min pos (length content))
                   for ch = (char content i)
                   do (if (char= ch #\\Newline)
                          (progn (incf line) (setf col 1))
                          (incf col)))
             (cons line col)))

         ;; Navigate to subform using source path (with error handling for special forms)
         (navigate-to-subform (form path)
           (handler-case
               (if (null path)
                   form
                   (let ((index (car path)))
                     (cond
                       ((and (consp form) (integerp index))
                        (let ((len (list-length form)))  ; Returns NIL for circular/improper
                          (when (and len (< index len))
                            (navigate-to-subform (nth index form) (cdr path)))))
                       (t nil))))
             (error () nil)))

         ;; Read file to string
         (read-file-to-string (path)
           (with-open-file (stream path :direction :input)
             (let ((content (make-string (file-length stream))))
               (read-sequence content stream)
               content))))

    ;; Refresh coverage info to update records with execution state
    (funcall (find-symbol \"REFRESH-COVERAGE-INFO\" \"SB-COVER\"))

    (let* ((ht (sb-cover::code-coverage-hashtable))
           (files-data nil)
           (file-count 0)
           (max-files 30)        ; Limit number of files to prevent overwhelming
           (max-file-size 200000)) ; Skip files larger than 200KB

      ;; Process each covered file (with limits)
      (maphash
       (lambda (file records)
         (when (and (< file-count max-files)
                    (probe-file file))
           ;; Check file size before processing
           (let ((file-size (with-open-file (s file) (file-length s))))
             (when (< file-size max-file-size)
               ;; Wrap in handler-case to skip files with reader errors
               (handler-case
                   (progn
                     (incf file-count)
                     (let* ((content (read-file-to-string file))
                            (annotations nil)
                            (expr-covered 0) (expr-total 0)
                            (branch-covered 0) (branch-total 0)
                            (all-maps (make-hash-table :test 'eq))
                            (forms nil)
                            (max-records 2000)  ; Limit records per file
                            (record-count 0))

                       ;; Read all forms to build source map
                       (with-open-file (stream file :direction :input)
                         (let ((sb-cover::*current-package* *package*))
                           (loop
                             (multiple-value-bind (form source-map)
                                 (funcall (find-symbol \"READ-AND-RECORD-SOURCE-MAP\" \"SB-COVER\") stream)
                               (when (eq form sb-int:*eof-object*)
                                 (return))
                               (push form forms)
                               (maphash (lambda (k v) (setf (gethash k all-maps) v)) source-map)))))
                       (setf forms (nreverse forms))

             ;; Process each coverage record (with limit)
             (dolist (record records)
               (when (>= record-count max-records)
                 (return))  ; Stop processing if we hit the limit
               (incf record-count)
               (let* ((path (car record))
                      (state (cdr record))
                      (is-branch (member (car path) '(:then :else)))
                      ;; Path is (subform-idx ... top-level-idx), reverse to navigate
                      (reversed-path (reverse (remove-if #'keywordp path)))
                      (form-idx (car reversed-path))
                      (subform-path (cdr reversed-path)))

                 ;; Count coverage
                 (if is-branch
                     (progn
                       (incf branch-total)
                       (when state (incf branch-covered)))
                     (progn
                       (incf expr-total)
                       (when state (incf expr-covered))))

                 ;; Find positions
                 (when (and (integerp form-idx) (< form-idx (length forms)))
                   (let* ((top-form (nth form-idx forms))
                          (subform (navigate-to-subform top-form subform-path))
                          (positions (when subform (gethash subform all-maps))))
                     (when positions
                       (let* ((pos-info (first positions))
                              (start (first pos-info))
                              (end (second pos-info))
                              (start-lc (pos-to-line-col content start))
                              (end-lc (pos-to-line-col content end)))
                         (push (list :start-line (car start-lc)
                                     :start-col (cdr start-lc)
                                     :end-line (car end-lc)
                                     :end-col (cdr end-lc)
                                     :state (cond
                                              ;; Branch coverage: :then/:else is in path, state is T/NIL
                                              ((eq (car path) :then)
                                               (if state :then-taken :then-not-taken))
                                              ((eq (car path) :else)
                                               (if state :else-taken :else-not-taken))
                                              ;; Expression coverage
                                              ((eq state t) :executed)
                                              ((null state) :not-executed)
                                              (t :unknown))
                                     :is-branch is-branch)
                               annotations)))))))

                       ;; Add file data
                       (push (list :path file
                                   :content content
                                   :annotations (nreverse annotations)
                                   :summary (list :expr-covered expr-covered
                                                 :expr-total expr-total
                                                 :branch-covered branch-covered
                                                 :branch-total branch-total))
                             files-data)))
                 ;; Skip files that cause reader errors
                 (error () nil))))))
       ht)

      ;; Return the data
      files-data)))
  (error (e) (format nil \"(:error . ~S)\" (princ-to-string e))))")
         (raw-result (backend-eval-internal extract-code))
         (result-string (first raw-result))
         (result (when (and result-string (stringp result-string))
                   (ignore-errors (read-from-string result-string)))))
    ;; Check for error result (dotted pair like (:error . "message"))
    (when (and (consp result) (eq (car result) :error))
      (format *error-output* "~&Coverage extraction failed: ~A~%" (cdr result))
      (return-from backend-coverage-json nil))
    (if (and result (listp result))
        ;; Convert to JSON
        (let ((json-obj (make-hash-table :test 'equal))
              (files-array (make-array (length result) :fill-pointer 0)))
          (dolist (file-data result)
            (let ((file-obj (make-hash-table :test 'equal))
                  (ann-array (make-array (length (getf file-data :annotations)) :fill-pointer 0)))
              (setf (gethash "path" file-obj) (getf file-data :path))
              (setf (gethash "content" file-obj) (getf file-data :content))
              ;; Convert annotations
              (dolist (ann (getf file-data :annotations))
                (let ((ann-obj (make-hash-table :test 'equal)))
                  (setf (gethash "startLine" ann-obj) (getf ann :start-line))
                  (setf (gethash "startCol" ann-obj) (getf ann :start-col))
                  (setf (gethash "endLine" ann-obj) (getf ann :end-line))
                  (setf (gethash "endCol" ann-obj) (getf ann :end-col))
                  (setf (gethash "state" ann-obj) (string-downcase (symbol-name (getf ann :state))))
                  (setf (gethash "isBranch" ann-obj) (getf ann :is-branch))
                  (vector-push-extend ann-obj ann-array)))
              (setf (gethash "annotations" file-obj) ann-array)
              ;; Summary
              (let ((summary (getf file-data :summary))
                    (sum-obj (make-hash-table :test 'equal)))
                (setf (gethash "exprCovered" sum-obj) (getf summary :expr-covered))
                (setf (gethash "exprTotal" sum-obj) (getf summary :expr-total))
                (setf (gethash "branchCovered" sum-obj) (getf summary :branch-covered))
                (setf (gethash "branchTotal" sum-obj) (getf summary :branch-total))
                (setf (gethash "summary" file-obj) sum-obj))
              (vector-push-extend file-obj files-array)))
          (setf (gethash "files" json-obj) files-array)
          (com.inuoe.jzon:stringify json-obj))
        nil)))

(defun backend-coverage-report ()
  "Generate coverage HTML report in backend and transfer files to ICL.
Returns the report ID for serving."
  (unless *slynk-connected-p*
    (error "Not connected to backend"))
  ;; Generate report in backend's temp dir, read all files, return as alist
  ;; Note: We use standard CL functions since the backend may not have Alexandria
  (let* ((report-code "
(progn
  (require 'sb-cover)
  (flet ((read-file-to-string (path)
           (with-open-file (stream path :direction :input)
             (let ((content (make-string (file-length stream))))
               (read-sequence content stream)
               content))))
    (let* ((temp-dir (merge-pathnames
                      (format nil \"icl-cover-~A/\" (get-universal-time))
                      (uiop:temporary-directory))))
      (ensure-directories-exist temp-dir)
      (funcall (find-symbol \"REPORT\" \"SB-COVER\") temp-dir)
      ;; Read all generated HTML files
      (let ((files nil))
        (dolist (path (directory (merge-pathnames \"*.html\" temp-dir)))
          (let ((name (file-namestring path)))
            (push (cons name (read-file-to-string path)) files)))
        ;; Clean up temp dir
        (dolist (path (directory (merge-pathnames \"*.*\" temp-dir)))
          (delete-file path))
        (ignore-errors (delete-directory temp-dir))
        ;; Return alist of (filename . content)
        files))))")
         (raw-result (backend-eval-internal report-code))
         (result-string (first raw-result))
         ;; Parse the string back to Lisp data
         (result (when (and result-string (stringp result-string))
                   (ignore-errors (read-from-string result-string)))))
    (if (and result (listp result))
        ;; Store files in memory
        (let ((report-id (format nil "coverage-~A" (incf *coverage-counter*)))
              (files-ht (make-hash-table :test 'equal)))
          (dolist (entry result)
            (setf (gethash (car entry) files-ht) (cdr entry)))
          (setf (gethash report-id *coverage-reports*) files-ht)
          (setf *current-coverage-id* report-id)
          report-id)
        ;; No files returned
        (progn
          (format *error-output* "~&No coverage data generated.~%")
          nil))))

;;; Serving coverage reports (from memory)

(defun serve-coverage-asset (filename)
  "Serve a coverage report file from memory."
  (when *current-coverage-id*
    (let ((files-ht (gethash *current-coverage-id* *coverage-reports*)))
      (when files-ht
        (let ((content (gethash filename files-ht)))
          (when content
            (setf (hunchentoot:content-type*)
                  (cond ((alexandria:ends-with-subseq ".html" filename) "text/html")
                        ((alexandria:ends-with-subseq ".css" filename) "text/css")
                        (t "text/plain")))
            content))))))

(defun clear-old-coverage-reports (&optional (keep-latest t))
  "Clear coverage reports from memory, optionally keeping the latest."
  (if keep-latest
      (let ((latest *current-coverage-id*))
        (maphash (lambda (id data)
                   (declare (ignore data))
                   (unless (string= id latest)
                     (remhash id *coverage-reports*)))
                 *coverage-reports*))
      (progn
        (clrhash *coverage-reports*)
        (setf *current-coverage-id* nil))))

;;; Panel integration

(defun open-coverage-panel (&optional title use-monaco)
  "Send message to browser to open a coverage report panel.
If USE-MONACO is true, sends JSON data for Monaco display.
Otherwise, opens the legacy HTML report in an iframe."
  (when *repl-resource*
    (dolist (client (hunchensocket:clients *repl-resource*))
      (let ((obj (make-hash-table :test 'equal)))
        (if use-monaco
            (let ((json-data (backend-coverage-json)))
              (if json-data
                  (progn
                    (setf (gethash "type" obj) "open-monaco-coverage")
                    (setf (gethash "data" obj) json-data)
                    (when title
                      (setf (gethash "title" obj) title)))
                  (progn
                    (format *error-output* "~&Failed to extract coverage data.~%")
                    (return-from open-coverage-panel nil))))
            (progn
              (setf (gethash "type" obj) "open-coverage")
              (when title
                (setf (gethash "title" obj) title))))
        (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))))))
