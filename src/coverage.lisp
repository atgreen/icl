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

(defvar *coverage-temp-dir* nil
  "Path to current coverage HTML temp directory for on-demand file parsing.")

(defun backend-coverage-json ()
  "Generate sb-cover HTML and return lightweight file index with percentages.
Does NOT parse individual files - that's done on-demand by backend-coverage-file-detail."
  (unless *slynk-connected-p*
    (error "Not connected to backend"))
  ;; Generate HTML report and return just the index (lightweight)
  (let* ((extract-code "
(handler-case
(progn
  (require 'sb-cover)
  (let* ((temp-dir (merge-pathnames
                    (format nil \"icl-cover-~A/\" (get-universal-time))
                    (uiop:temporary-directory)))
         (files-list nil))
    (ensure-directories-exist temp-dir)
    ;; Suppress warnings from sb-cover:report (source location errors)
    (handler-bind ((warning #'muffle-warning))
      (sb-cover:report temp-dir))
    ;; Parse only the index file for file list with percentages
    (let* ((index-path (merge-pathnames \"cover-index.html\" temp-dir)))
      (with-open-file (stream index-path :direction :input :if-does-not-exist nil)
        (when stream
          (let* ((html (make-string (file-length stream))))
            (read-sequence html stream)
            ;; Parse table rows: <a href='HASH.html'>FILENAME</a> then <td>covered</td><td>total</td><td>%</td>
            (let ((pos 0))
              (loop
                (let ((href-start (search \"<a href='\" html :start2 pos)))
                  (unless href-start (return))
                  (let* ((hash-start (+ href-start 9))
                         (dot-pos (search \".html'>\" html :start2 hash-start))
                         (hash (when dot-pos (subseq html hash-start dot-pos)))
                         (name-start (when dot-pos (+ dot-pos 7)))
                         (name-end (when name-start (search \"</a>\" html :start2 name-start)))
                         (filename (when name-end (subseq html name-start name-end))))
                    ;; Find the <tr class='subheading'> rows to get directory context
                    ;; For now just use filename, we can enhance later
                    (when (and hash filename (> (length hash) 0))
                      ;; Parse the percentage from following <td> cells
                      (let* ((td1-start (search \"<td>\" html :start2 name-end))
                             (td1-end (when td1-start (search \"</td>\" html :start2 (+ td1-start 4))))
                             (expr-covered (when td1-end (ignore-errors (parse-integer (subseq html (+ td1-start 4) td1-end)))))
                             (td2-start (when td1-end (search \"<td>\" html :start2 td1-end)))
                             (td2-end (when td2-start (search \"</td>\" html :start2 (+ td2-start 4))))
                             (expr-total (when td2-end (ignore-errors (parse-integer (subseq html (+ td2-start 4) td2-end))))))
                        (push (list :hash hash
                                    :filename filename
                                    :expr-covered (or expr-covered 0)
                                    :expr-total (or expr-total 0))
                              files-list)))
                    (setf pos (or name-end (1+ pos)))))))))))
    ;; Return temp-dir and file list
    (list :temp-dir (namestring temp-dir)
          :files (nreverse files-list))))
  (error (e) (list :error (princ-to-string e))))")
         (raw-result (handler-case
                         (backend-eval-internal extract-code)
                       (error () nil)))
         (result-string (first raw-result))
         (result (when (and result-string (stringp result-string))
                   (ignore-errors (read-from-string result-string)))))
    ;; Store temp-dir for on-demand file parsing
    (when (and result (listp result) (getf result :temp-dir))
      (setf *coverage-temp-dir* (getf result :temp-dir)))
    ;; Check for error
    (when (and (consp result) (eq (car result) :error))
      (format *error-output* "~&Coverage extraction failed: ~A~%" (getf result :error))
      (return-from backend-coverage-json nil))
    ;; Convert to JSON format for frontend
    (if (and result (listp result) (getf result :files))
        (let ((json-obj (make-hash-table :test 'equal))
              (files-array (make-array (length (getf result :files)) :fill-pointer 0)))
          (setf (gethash "tempDir" json-obj) (getf result :temp-dir))
          (dolist (file-info (getf result :files))
            (let ((file-obj (make-hash-table :test 'equal)))
              (setf (gethash "hash" file-obj) (getf file-info :hash))
              (setf (gethash "filename" file-obj) (getf file-info :filename))
              (setf (gethash "exprCovered" file-obj) (getf file-info :expr-covered))
              (setf (gethash "exprTotal" file-obj) (getf file-info :expr-total))
              (vector-push-extend file-obj files-array)))
          (setf (gethash "files" json-obj) files-array)
          (with-output-to-string (s)
            (yason:encode json-obj s)))
        nil)))

(defun backend-coverage-file-detail (hash)
  "Parse a single coverage HTML file on-demand and return content + annotations.
HASH is the filename hash from the index (e.g. '58047fd52011a5ec4d2f31d3d209b75d')."
  (unless *slynk-connected-p*
    (error "Not connected to backend"))
  (unless *coverage-temp-dir*
    (error "No coverage data available. Run ,cover-load first."))
  ;; Parse the specific HTML file in the backend
  (let* ((parse-code (format nil "
(handler-case
(progn
  (labels ((starts-with-at (str prefix pos)
             (let ((plen (length prefix)))
               (and (<= (+ pos plen) (length str))
                    (string= prefix str :start2 pos :end2 (+ pos plen)))))
           (state-keyword (n)
             (case n
               (0 :not-instrumented)
               (1 :executed)
               (2 :not-executed)
               (5 :both-branches)
               ((6 9) :one-branch)
               (10 :neither-branch)
               (15 :conditionalized-out)
               (t :unknown)))
           (branch-state-p (n)
             (member n '(5 6 9 10)))
           (parse-line-spans (line-html line-num)
             ;; span-stack entries: (initial-col state-num first-content-col)
             ;; first-content-col is nil until first non-whitespace char
             (let ((annotations nil) (col 0) (pos 0) (span-stack nil) (len (length line-html)))
               (labels ((update-first-content ()
                          ;; Set first-content-col for any spans that don't have it yet
                          (dolist (entry span-stack)
                            (when (null (third entry))
                              (setf (third entry) col))))
                        (whitespace-char-p (c)
                          (or (char= c #\\Space) (char= c #\\Tab))))
                 (loop while (< pos len) do
                   (let ((ch (char line-html pos)))
                     (cond
                       ((char= ch #\\<)
                        (cond
                          ((starts-with-at line-html \"<span class='state-\" pos)
                           (let* ((state-pos (+ pos 19))
                                  (quote-pos (position #\\' line-html :start state-pos))
                                  (state-num (parse-integer (subseq line-html state-pos quote-pos)))
                                  (gt-pos (position #\\> line-html :start quote-pos)))
                             (push (list col state-num nil) span-stack)
                             (setf pos (1+ gt-pos))))
                          ((starts-with-at line-html \"</span>\" pos)
                           (when span-stack
                             (let* ((start-info (pop span-stack))
                                    (first-content-col (or (third start-info) (first start-info)))
                                    (state-num (second start-info)))
                               (when (> col first-content-col)
                                 (push (list :start-line line-num :start-col (1+ first-content-col)
                                             :end-line line-num :end-col (1+ col)
                                             :state (state-keyword state-num)
                                             :is-branch (branch-state-p state-num))
                                       annotations))))
                           (setf pos (+ pos 7)))
                          (t (let ((gt-pos (position #\\> line-html :start pos)))
                               (setf pos (1+ (or gt-pos pos)))))))
                       ((char= ch #\\&)
                        (let ((semi-pos (position #\\; line-html :start pos)))
                          (if semi-pos
                              (let ((entity (subseq line-html pos (1+ semi-pos))))
                                ;; Treat &nbsp; and &#160; as whitespace (don't set first-content-col)
                                (unless (and span-stack
                                             (or (string= entity \"&nbsp;\")
                                                 (string= entity \"&#160;\")))
                                  (update-first-content))
                                (incf col) (setf pos (1+ semi-pos)))
                              (progn (update-first-content) (incf col) (incf pos)))))
                       (t
                        (unless (whitespace-char-p ch)
                          (update-first-content))
                        (incf col) (incf pos))))))
               (nreverse annotations))))
    (let* ((html-path (merge-pathnames \"~A.html\" \"~A\"))
           (html nil)
           (source-path nil)
           (all-annotations nil)
           (source-lines nil))
      (with-open-file (stream html-path :direction :input :if-does-not-exist nil)
        (when stream
          (setf html (make-string (file-length stream)))
          (read-sequence html stream)))
      (when html
        ;; Extract source path
        (let ((title-start (search \"Coverage report: \" html)))
          (when title-start
            (let* ((path-start (+ title-start 17))
                   (path-end (position #\\< html :start path-start)))
              (when path-end
                (setf source-path (string-trim \" \" (subseq html path-start path-end)))))))
        ;; Parse lines
        (let ((pos 0))
          (loop
            (let ((line-start (search \"<div class='line-number'><code>\" html :start2 pos)))
              (unless line-start (return))
              (let* ((num-start (+ line-start 31))
                     (num-end (search \"</code>\" html :start2 num-start))
                     (line-num (parse-integer (subseq html num-start num-end)))
                     (content-marker (search \"</div>\" html :start2 num-end))
                     (content-start (when content-marker (+ content-marker 6)))
                     (line-end (search \"</div></nobr>\" html :start2 (or content-start num-end))))
                (when (and content-start line-end)
                  (let* ((line-html (subseq html content-start line-end))
                         ;; Skip <code>&#160; that sb-cover adds to every line (12 chars)
                         (skip-leading (if (and (>= (length line-html) 12)
                                                (string= \"<code>&#160;\" (subseq line-html 0 12)))
                                           12 0))
                         (line-html-trimmed (subseq line-html skip-leading)))
                    (setf all-annotations (nconc all-annotations (parse-line-spans line-html-trimmed line-num)))
                    ;; Decode HTML to plain text
                    (let ((decoded (make-string-output-stream)) (lpos 0) (llen (length line-html-trimmed)))
                      (loop while (< lpos llen) do
                        (let ((ch (char line-html-trimmed lpos)))
                          (cond
                            ((char= ch #\\<)
                             (let ((gt (position #\\> line-html-trimmed :start lpos)))
                               (setf lpos (1+ (or gt lpos)))))
                            ((char= ch #\\&)
                             (let ((semi (position #\\; line-html-trimmed :start lpos)))
                               (if semi
                                   (let ((entity (subseq line-html-trimmed (1+ lpos) semi)))
                                     (write-char
                                      (cond ((string= entity \"lt\") #\\<)
                                            ((string= entity \"gt\") #\\>)
                                            ((string= entity \"amp\") #\\&)
                                            ((string= entity \"quot\") #\\\")
                                            ((string= entity \"apos\") #\\')
                                            ((string= entity \"nbsp\") #\\Space)
                                            ((string= entity \"#160\") #\\Space)  ;; NBSP -> regular space
                                            ((and (> (length entity) 1) (char= (char entity 0) #\\#))
                                             (let ((code (parse-integer entity :start 1)))
                                               (if (= code 160) #\\Space (code-char code))))
                                            (t #\\?))
                                      decoded)
                                     (setf lpos (1+ semi)))
                                   (progn (write-char ch decoded) (incf lpos)))))
                            (t (write-char ch decoded) (incf lpos)))))
                      (push (get-output-stream-string decoded) source-lines))))
                (setf pos (or line-end (1+ pos))))))))
      (list :path source-path
            :content (format nil \"~~{~~A~~^~~%~~}\" (nreverse source-lines))
            :annotations all-annotations))))
  (error (e) (list :error (princ-to-string e))))" hash *coverage-temp-dir*))
         (raw-result (handler-case
                         (backend-eval-internal parse-code)
                       (error (e)
                         (format *error-output* "~&backend-coverage-file-detail error: ~A~%" e)
                         nil)))
         (result-string (first raw-result))
         (result (when (and result-string (stringp result-string))
                   (ignore-errors (read-from-string result-string)))))
    ;; Check for error
    (when (and (consp result) (eq (car result) :error))
      (format *error-output* "~&File detail extraction failed: ~A~%" (getf result :error))
      (return-from backend-coverage-file-detail nil))
    ;; Convert to JSON
    (when result
      (let ((json-obj (make-hash-table :test 'equal)))
        (setf (gethash "path" json-obj) (or (getf result :path) ""))
        (setf (gethash "content" json-obj) (or (getf result :content) ""))
        ;; Convert annotations
        (let ((annotations (getf result :annotations))
              (ann-array (make-array 0 :fill-pointer 0 :adjustable t)))
          (dolist (ann annotations)
            (let ((ann-obj (make-hash-table :test 'equal)))
              (setf (gethash "startLine" ann-obj) (getf ann :start-line))
              (setf (gethash "startCol" ann-obj) (getf ann :start-col))
              (setf (gethash "endLine" ann-obj) (getf ann :end-line))
              (setf (gethash "endCol" ann-obj) (getf ann :end-col))
              (setf (gethash "state" ann-obj) (string-downcase (symbol-name (getf ann :state))))
              (setf (gethash "isBranch" ann-obj) (if (getf ann :is-branch) t nil))
              (vector-push-extend ann-obj ann-array)))
          (setf (gethash "annotations" json-obj) ann-array))
        (with-output-to-string (s)
          (yason:encode json-obj s))))))

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

;;; Panel integration

(defun send-coverage-loading-message (&optional title)
  "Send message to browser to show coverage loading spinner."
  (when *repl-resource*
    (dolist (client (hunchensocket:clients *repl-resource*))
      (let ((obj (make-hash-table :test 'equal)))
        (setf (gethash "type" obj) "coverage-loading")
        (setf (gethash "title" obj) (or title "Coverage Report"))
        (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))))))

(defun open-coverage-panel (&optional title use-monaco)
  "Send message to browser to open a coverage report panel.
If USE-MONACO is true, sends JSON data for Monaco display.
Otherwise, opens the legacy HTML report in an iframe."
  (when *repl-resource*
    ;; First, send loading message to show spinner immediately
    (when use-monaco
      (send-coverage-loading-message title))

    ;; Generate coverage data (this takes time) - suppress warnings/output
    (let ((json-data (when use-monaco
                       (handler-bind ((warning #'muffle-warning))
                         (let ((*standard-output* (make-broadcast-stream))
                               (*error-output* (make-broadcast-stream)))
                           (backend-coverage-json))))))
      ;; Now send the actual data
      (dolist (client (hunchensocket:clients *repl-resource*))
        (let ((obj (make-hash-table :test 'equal)))
          (if use-monaco
              (if json-data
                  (progn
                    (setf (gethash "type" obj) "open-monaco-coverage")
                    (setf (gethash "data" obj) json-data)
                    (when title
                      (setf (gethash "title" obj) title)))
                  (return-from open-coverage-panel nil))
              (progn
                (setf (gethash "type" obj) "open-coverage")
                (when title
                  (setf (gethash "title" obj) title))))
          (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj)))))))
