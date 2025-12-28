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

(defun open-coverage-panel (&optional title)
  "Send message to browser to open a coverage report panel."
  (when *repl-resource*
    (dolist (client (hunchensocket:clients *repl-resource*))
      (let ((obj (make-hash-table :test 'equal)))
        (setf (gethash "type" obj) "open-coverage")
        (when title
          (setf (gethash "title" obj) title))
        (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))))))
