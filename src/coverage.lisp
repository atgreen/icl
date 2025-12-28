;;; coverage.lisp - Code coverage analysis with sb-cover
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package :icl)

;;; Coverage data storage
(defvar *coverage-report-dir* nil
  "Directory containing the current coverage HTML report.")

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
  "Load FILEPATH with coverage instrumentation enabled."
  (unless *slynk-connected-p*
    (error "Not connected to backend"))
  (backend-eval-internal
   (format nil
           "(progn
              (require 'sb-cover)
              (proclaim (list 'optimize (list (find-symbol \"STORE-COVERAGE-DATA\" \"SB-COVER\") 3)))
              (load ~S :verbose t)
              (proclaim (list 'optimize (list (find-symbol \"STORE-COVERAGE-DATA\" \"SB-COVER\") 0)))
              \"File loaded with coverage\")"
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
  "Generate coverage HTML report and return the directory path."
  (unless *slynk-connected-p*
    (error "Not connected to backend"))
  ;; Create a unique temp directory for this report
  (let* ((report-id (incf *coverage-counter*))
         (report-dir (merge-pathnames
                      (format nil "icl-coverage-~A/" report-id)
                      (uiop:temporary-directory))))
    ;; Ensure directory exists
    (ensure-directories-exist report-dir)
    ;; Generate report in backend
    (backend-eval-internal
     (format nil
             "(progn
                (require 'sb-cover)
                (funcall (find-symbol \"REPORT\" \"SB-COVER\") ~S)
                ~S)"
             (namestring report-dir)
             (namestring report-dir)))
    ;; Store for serving
    (setf *coverage-report-dir* report-dir)
    report-dir))

;;; Serving coverage reports

(defun serve-coverage-asset (filename)
  "Serve a coverage report file."
  (when *coverage-report-dir*
    (let ((filepath (merge-pathnames filename *coverage-report-dir*)))
      (when (probe-file filepath)
        (setf (hunchentoot:content-type*)
              (cond ((alexandria:ends-with-subseq ".html" filename) "text/html")
                    ((alexandria:ends-with-subseq ".css" filename) "text/css")
                    (t "text/plain")))
        (alexandria:read-file-into-string filepath)))))

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
