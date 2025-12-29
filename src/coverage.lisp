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
  "Extract coverage data by generating sb-cover HTML and parsing it.
This approach works with files using custom reader macros because
sb-cover generates HTML in the backend where the macros are available."
  (unless *slynk-connected-p*
    (error "Not connected to backend"))
  ;; Generate HTML report in backend, parse it, extract coverage data
  (let* ((extract-code "
(handler-case
(progn
  (require 'sb-cover)

  (labels (;; Read file to string
           (read-file-to-string (path)
             (with-open-file (stream path :direction :input)
               (let ((content (make-string (file-length stream))))
                 (read-sequence content stream)
                 content)))

           ;; Decode HTML entities
           (decode-entities (str)
             (let ((result str))
               (setf result (cl-ppcre:regex-replace-all \"&#160;\" result \" \"))
               (setf result (cl-ppcre:regex-replace-all \"&#40;\" result \"(\"))
               (setf result (cl-ppcre:regex-replace-all \"&#41;\" result \")\"))
               (setf result (cl-ppcre:regex-replace-all \"&#59;\" result \";\"))
               (setf result (cl-ppcre:regex-replace-all \"&#39;\" result \"'\"))
               (setf result (cl-ppcre:regex-replace-all \"&#34;\" result \"\\\"\"))
               (setf result (cl-ppcre:regex-replace-all \"&#35;\" result \"#\"))
               (setf result (cl-ppcre:regex-replace-all \"&#38;\" result \"&\"))
               (setf result (cl-ppcre:regex-replace-all \"&#60;\" result \"<\"))
               (setf result (cl-ppcre:regex-replace-all \"&#62;\" result \">\"))
               (setf result (cl-ppcre:regex-replace-all \"&#46;\" result \".\"))
               (setf result (cl-ppcre:regex-replace-all \"&#45;\" result \"-\"))
               (setf result (cl-ppcre:regex-replace-all \"&#58;\" result \":\"))
               (setf result (cl-ppcre:regex-replace-all \"&#44;\" result \",\"))
               (setf result (cl-ppcre:regex-replace-all \"&#47;\" result \"/\"))
               (setf result (cl-ppcre:regex-replace-all \"&#64;\" result \"@\"))
               (setf result (cl-ppcre:regex-replace-all \"&#42;\" result \"*\"))
               (setf result (cl-ppcre:regex-replace-all \"&#43;\" result \"+\"))
               (setf result (cl-ppcre:regex-replace-all \"&#91;\" result \"[\"))
               (setf result (cl-ppcre:regex-replace-all \"&#93;\" result \"]\"))
               (setf result (cl-ppcre:regex-replace-all \"&#126;\" result \"~\"))
               (setf result (cl-ppcre:regex-replace-all \"&#95;\" result \"_\"))
               (setf result (cl-ppcre:regex-replace-all \"&#36;\" result \"$\"))
               (setf result (cl-ppcre:regex-replace-all \"&#124;\" result \"|\"))
               (setf result (cl-ppcre:regex-replace-all \"&#92;\" result \"\\\\\"))
               (setf result (cl-ppcre:regex-replace-all \"&#123;\" result \"{\"))
               (setf result (cl-ppcre:regex-replace-all \"&#125;\" result \"}\"))
               (setf result (cl-ppcre:regex-replace-all \"&#96;\" result \"`\"))
               (setf result (cl-ppcre:regex-replace-all \"&#94;\" result \"^\"))
               (setf result (cl-ppcre:regex-replace-all \"&#37;\" result \"%\"))
               (cl-ppcre:regex-replace-all \"&#[0-9]+;\" result \"\")))

           ;; Parse a single HTML file, return file data
           (parse-coverage-html (html-path source-path)
             (let* ((html (read-file-to-string html-path))
                    (lines nil)
                    (line-coverages (make-hash-table))
                    (expr-covered 0) (expr-total 0)
                    (branch-covered 0) (branch-total 0))
               ;; Extract summary from table
               (cl-ppcre:register-groups-bind (ec et)
                   (\"expression</td><td>([0-9]+)</td><td>([0-9]+)\" html)
                 (setf expr-covered (parse-integer ec)
                       expr-total (parse-integer et)))
               (cl-ppcre:register-groups-bind (bc bt)
                   (\"branch</td><td>([0-9]+)</td><td>([0-9]+)\" html)
                 (setf branch-covered (parse-integer bc)
                       branch-total (parse-integer bt)))
               ;; Parse each source line
               (cl-ppcre:do-register-groups (line-num line-content)
                   (\"<div class='line-number'><code>([0-9]+)</code></div><code>([^<]*(?:<[^>]+>[^<]*)*)</code>\" html)
                 (let ((ln (parse-integer line-num))
                       (states nil))
                   ;; Extract states from spans in this line
                   (cl-ppcre:do-register-groups (state-num)
                       (\"state-([0-9]+)\" line-content)
                     (let ((s (parse-integer state-num)))
                       (unless (member s '(0 15))  ; Skip not-instrumented and conditionalized-out
                         (pushnew s states))))
                   ;; Determine line state
                   (when states
                     (setf (gethash ln line-coverages)
                           (cond
                             ((member 1 states) :executed)
                             ((member 5 states) :both-branches)
                             ((or (member 6 states) (member 9 states)) :one-branch)
                             ((member 2 states) :not-executed)
                             ((member 10 states) :neither-branch)
                             (t nil))))
                   ;; Build source line (strip HTML)
                   (push (cons ln (decode-entities
                                   (cl-ppcre:regex-replace-all \"<[^>]+>\" line-content \"\")))
                         lines)))
               ;; Build annotations from line-coverages
               (let ((annotations nil))
                 (maphash (lambda (line state)
                            (when state
                              (push (list :start-line line :start-col 1
                                          :end-line line :end-col 1000
                                          :state state
                                          :is-branch (member state '(:both-branches :one-branch :neither-branch)))
                                    annotations)))
                          line-coverages)
                 ;; Reconstruct source from lines
                 (let* ((sorted-lines (sort lines #'< :key #'car))
                        (source (with-output-to-string (s)
                                  (dolist (l sorted-lines)
                                    (format s \"~A~%\" (cdr l))))))
                   (list :path source-path
                         :content source
                         :annotations annotations
                         :summary (list :expr-covered expr-covered
                                       :expr-total expr-total
                                       :branch-covered branch-covered
                                       :branch-total branch-total)))))))

    ;; Generate HTML report to temp directory
    (let* ((temp-dir (merge-pathnames
                      (format nil \"icl-cover-~A/\" (get-universal-time))
                      (uiop:temporary-directory)))
           (files-data nil))
      (ensure-directories-exist temp-dir)
      (sb-cover:report temp-dir)

      ;; Read cover-index.html to get file list
      (let* ((index-path (merge-pathnames \"cover-index.html\" temp-dir))
             (index-html (read-file-to-string index-path)))
        ;; Parse file links: <a href='HASH.html'>PATH</a>
        (cl-ppcre:do-register-groups (hash path)
            (\"<a href='([a-f0-9]+)\\.html'>([^<]+)</a>\" index-html)
          (let ((html-path (merge-pathnames (format nil \"~A.html\" hash) temp-dir)))
            (when (probe-file html-path)
              (handler-case
                  (let ((file-data (parse-coverage-html html-path path)))
                    (when file-data
                      (push file-data files-data)))
                (error () nil))))))

      ;; Clean up temp files
      (dolist (path (directory (merge-pathnames \"*.html\" temp-dir)))
        (ignore-errors (delete-file path)))
      (ignore-errors (uiop:delete-directory-tree temp-dir :validate t))

      (or files-data (list :error \"No coverage data found.\")))))
  (error (e) (list :error (princ-to-string e))))")
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
