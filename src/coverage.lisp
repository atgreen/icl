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
Returns JSON string with file contents and coverage annotations.
Uses stored position data when available to avoid reader macro issues."
  (unless *slynk-connected-p*
    (error "Not connected to backend"))
  ;; This code runs in the backend to extract detailed coverage data
  ;; Uses compute-file-states when stored positions are available (no reader needed)
  (let* ((extract-code "
(handler-case
(progn
  (require 'sb-cover)

  (labels (;; Read file to string (plain text, no Lisp parsing)
           (read-file-to-string (path)
             (with-open-file (stream path :direction :input)
               (let ((content (make-string (file-length stream))))
                 (read-sequence content stream)
                 content)))

           ;; Convert character position to (line . column) using line lengths
           (pos-to-line-col-from-lengths (pos linelengths)
             (let ((line 1)
                   (remaining pos))
               (loop for i from 0 below (length linelengths)
                     for len = (aref linelengths i)
                     do (if (<= remaining len)
                            (return-from pos-to-line-col-from-lengths
                              (cons line (1+ remaining)))  ; 1-based column
                            (progn
                              (decf remaining (1+ len))  ; +1 for newline
                              (incf line))))
               ;; Past end of file
               (cons line 1)))

           ;; State nibble to coverage state keyword
           ;; Based on sb-cover state encoding
           (state-to-keyword (state-nibble)
             (cond
               ((= state-nibble 0) nil)  ; not instrumented
               ((= state-nibble 1) :executed)
               ((= state-nibble 2) :not-executed)
               ;; Branch states (bits: else-state << 2 | then-state)
               ;; state 1 = executed, state 2 = not executed
               ((= state-nibble 5) :both-branches)      ; 01|01 = both taken
               ((= state-nibble 6) :one-branch)         ; 01|10 or 10|01
               ((= state-nibble 9) :one-branch)         ; 10|01
               ((= state-nibble 10) :neither-branch)    ; 10|10 = neither taken
               ((= state-nibble 15) nil)  ; conditionalized out
               (t nil))))

    ;; Refresh coverage info
    (let ((refresh-fn (find-symbol \"REFRESH-COVERAGE-BITS\" \"SB-COVER\")))
      (when refresh-fn (funcall refresh-fn)))

    (let* ((ht (sb-cover::code-coverage-hashtable))
           (files-data nil)
           (file-count 0)
           (skipped-no-positions 0)
           (max-files 30)
           (max-file-size 200000))

      (maphash
       (lambda (file file-info)
         (when (and (< file-count max-files)
                    (probe-file file))
           (let ((file-size (ignore-errors (with-open-file (s file) (file-length s)))))
             (when (and file-size (< file-size max-file-size))
               (handler-case
                   (let* ((loc-accessor (find-symbol \"COVERAGE-INSTRUMENTED-FILE-LOCATIONS\" \"SB-C\"))
                          (lines-accessor (find-symbol \"COVERAGE-INSTRUMENTED-FILE-LINES\" \"SB-C\"))
                          (locations (and loc-accessor (funcall loc-accessor file-info)))
                          (linelengths (and lines-accessor (funcall lines-accessor file-info))))
                     ;; Only process if stored positions are available
                     (unless (and locations linelengths)
                       (incf skipped-no-positions))
                     (when (and locations linelengths)
                       (incf file-count)
                       (multiple-value-bind (counts states)
                           (funcall (find-symbol \"COMPUTE-FILE-STATES\" \"SB-COVER\") file)
                         (when states
                           (let* ((content (read-file-to-string file))
                                  (annotations nil)
                                  (expr-count (getf counts :expression))
                                  (branch-count (getf counts :branch))
                                  ;; Extract spans from state changes
                                  (current-state 0)
                                  (span-start 0))
                             ;; Walk through states array, emit spans on state changes
                             (loop for i from 0 below (length states)
                                   for state = (aref states i)
                                   do (when (/= state current-state)
                                        ;; Emit previous span if it was instrumented
                                        (when (and (> current-state 0)
                                                   (/= current-state 15)
                                                   (> i span-start))
                                          (let* ((start-lc (pos-to-line-col-from-lengths span-start linelengths))
                                                 (end-lc (pos-to-line-col-from-lengths (1- i) linelengths))
                                                 (state-kw (state-to-keyword current-state)))
                                            (when state-kw
                                              (push (list :start-line (car start-lc)
                                                          :start-col (cdr start-lc)
                                                          :end-line (car end-lc)
                                                          :end-col (cdr end-lc)
                                                          :state state-kw
                                                          :is-branch (member current-state '(5 6 9 10)))
                                                    annotations))))
                                        (setf current-state state
                                              span-start i)))
                             ;; Emit final span
                             (when (and (> current-state 0)
                                        (/= current-state 15))
                               (let* ((start-lc (pos-to-line-col-from-lengths span-start linelengths))
                                      (end-lc (pos-to-line-col-from-lengths (1- (length states)) linelengths))
                                      (state-kw (state-to-keyword current-state)))
                                 (when state-kw
                                   (push (list :start-line (car start-lc)
                                               :start-col (cdr start-lc)
                                               :end-line (car end-lc)
                                               :end-col (cdr end-lc)
                                               :state state-kw
                                               :is-branch (member current-state '(5 6 9 10)))
                                         annotations))))
                             ;; Add file data - use find-symbol for sb-cover accessors
                             (let ((ok-of-fn (find-symbol \"OK-OF\" \"SB-COVER\"))
                                   (all-of-fn (find-symbol \"ALL-OF\" \"SB-COVER\")))
                               (push (list :path file
                                           :content content
                                           :annotations (nreverse annotations)
                                           :summary (list :expr-covered (funcall ok-of-fn expr-count)
                                                         :expr-total (funcall all-of-fn expr-count)
                                                         :branch-covered (funcall ok-of-fn branch-count)
                                                         :branch-total (funcall all-of-fn branch-count)))
                                     files-data)))))))
                 (error () nil))))))
       ht)

      ;; Return files-data, or error info if no files processed
      (if files-data
          files-data
          (format nil \"(:error . \\\"No files with stored positions. Skipped ~D files. Try recompiling with recent SBCL.\\\")\" skipped-no-positions)))))
  (error (e) (format nil \"(:error . ~S)\" (princ-to-string e))))")
         (raw-result (backend-eval-internal extract-code))
         (result-string (first raw-result))
         (result (when (and result-string (stringp result-string))
                   (ignore-errors (read-from-string result-string)))))
    ;; Debug output
    (format *error-output* "~&Coverage debug: raw-result type=~A, result-string=~S~%"
            (type-of raw-result) (if (> (length (princ-to-string result-string)) 200)
                                     (subseq (princ-to-string result-string) 0 200)
                                     result-string))
    (format *error-output* "~&Coverage debug: result type=~A, result=~S~%"
            (type-of result) (if (and result (> (length (princ-to-string result)) 200))
                                 (subseq (princ-to-string result) 0 200)
                                 result))
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
