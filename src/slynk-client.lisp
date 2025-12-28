;;; slynk-client.lisp --- Slynk protocol client for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Connection State
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *slynk-connection* nil
  "Current Slynk connection (slynk-client:swank-connection object).")

(defvar *slynk-connection-lock* (bt:make-lock "slynk-connection")
  "Lock for serializing access to the Slynk connection.")

(defvar *slynk-port* 4005
  "Default port for Slynk connections.")

(defvar *slynk-host* "127.0.0.1"
  "Default host for Slynk connections.")

(defvar *slynk-connected-p* nil
  "T when connected to a backend server.")

(defmacro with-slynk-connection (&body body)
  "Execute BODY with the Slynk connection lock held."
  `(bt:with-lock-held (*slynk-connection-lock*)
     ,@body))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Connection Management
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *icl-runtime-injected* nil
  "T when ICL runtime has been injected into the inferior Lisp.")

(defun slynk-verify-connection (connection &optional (timeout-seconds 10))
  "Verify that CONNECTION is responsive by doing a simple eval with timeout.
   Returns T if the connection is responsive, NIL otherwise."
  #+sbcl
  (handler-case
      (sb-ext:with-timeout timeout-seconds
        ;; Try a minimal eval - just return T
        (let ((result (slynk-client:slime-eval 'cl:t connection)))
          (eq result t)))
    (sb-ext:timeout ()
      (when *verbose*
        (format *error-output* "~&; Slynk connection verification timed out~%"))
      nil)
    (error (e)
      (when *verbose*
        (format *error-output* "~&; Slynk connection verification failed: ~A~%" e))
      nil))
  #-sbcl
  ;; On non-SBCL, we can't easily add timeouts, so just return T
  t)

(defun slynk-connect (&key (host *slynk-host*) (port *slynk-port*) (verify t))
  "Connect to a backend server at HOST:PORT.
   If VERIFY is T (default), verifies the connection is responsive before returning."
  (when *slynk-connection*
    (slynk-disconnect))
  (setf *icl-runtime-injected* nil)
  (handler-case
      (let ((conn (slynk-client:slime-connect host port)))
        (when conn
          (if (or (not verify) (slynk-verify-connection conn))
              (progn
                (setf *slynk-connection* conn)
                (setf *slynk-connected-p* t)
                t)
              ;; Connection established but not responsive - close it
              (progn
                (ignore-errors (slynk-client:slime-close conn))
                nil))))
    (error (e)
      (setf *slynk-connected-p* nil)
      (format *error-output* "~&; Failed to connect to Slynk: ~A~%" e)
      nil)))

;; ICL Runtime - Phase 1: Create package and ensure exports
(defvar *icl-runtime-phase1*
  "(cl:progn
     (cl:unless (cl:find-package :icl-runtime)
       (cl:defpackage #:icl-runtime
         (:use #:cl)
         (:export #:+version+
                  #:usb8-array-to-base64-string
                  #:*eval-generation*
                  #:setup-eval-generation-hook
                  #:visualize)))
     ;; Ensure symbols are exported even if package already existed
     ;; Use intern to get symbols in the inferior Lisp's context
     (cl:let ((pkg (cl:find-package :icl-runtime)))
       (cl:export (cl:list (cl:intern \"+VERSION+\" pkg)
                           (cl:intern \"USB8-ARRAY-TO-BASE64-STRING\" pkg)
                           (cl:intern \"*EVAL-GENERATION*\" pkg)
                           (cl:intern \"SETUP-EVAL-GENERATION-HOOK\" pkg)
                           (cl:intern \"VISUALIZE\" pkg))
                  pkg))
     t)"
  "Phase 1: Create the ICL runtime package and ensure exports.")

;; ICL Runtime - Phase 2: Define functions (load as a file)
;; Using LOAD with a string stream ensures proper top-level processing
(defvar *icl-runtime-phase2-template*
  "(in-package :icl-runtime)
   ;; Runtime version (matches ICL version that injected it)
   (defvar +version+ ~S)
   ;; Base64 encoding (only define if not already present)
   (defvar *base64-chars*
     \"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/\")
   (unless (fboundp 'usb8-array-to-base64-string)
     (defun usb8-array-to-base64-string (bytes)
     (let* ((len (length bytes))
            (pad (mod (- 3 (mod len 3)) 3))
            (out (make-array (* 4 (ceiling (+ len pad) 3))
                             :element-type 'character
                             :fill-pointer 0)))
       (labels ((emit (idx)
                  (vector-push (char *base64-chars* idx) out))
                (byte-at (i)
                  (if (< i len) (aref bytes i) 0)))
         (loop for i from 0 below len by 3
               for b0 = (byte-at i)
               for b1 = (byte-at (+ i 1))
               for b2 = (byte-at (+ i 2))
               do (emit (ash b0 -2))
                  (emit (logior (ash (logand b0 #x03) 4)
                                (ash b1 -4)))
                  (emit (logior (ash (logand b1 #x0F) 2)
                                (ash b2 -6)))
                  (emit (logand b2 #x3F))))
       (when (> pad 0)
         (setf (char out (- (length out) 1)) #\\=))
       (when (> pad 1)
         (setf (char out (- (length out) 2)) #\\=))
       out)))
   ;; Eval generation tracking - for REPL and editor interactions
   (defvar *eval-generation* 0)
   (defvar *eval-hook-installed* nil)
   ;; Only define if not already defined (avoids redefinition warnings on reconnect)
   (unless (fboundp 'setup-eval-generation-hook)
     ;; Helper to wrap a function with eval-generation increment
     (defun wrap-with-generation-increment (pkg-name fn-name)
       (let* ((pkg (find-package pkg-name))
              (fn-symbol (and pkg (find-symbol fn-name pkg))))
         (when (and fn-symbol (fboundp fn-symbol))
           (let ((original (fdefinition fn-symbol)))
             (setf (fdefinition fn-symbol)
                   (lambda (&rest args)
                     (prog1 (apply original args)
                       (incf *eval-generation*))))))))
     (defun setup-eval-generation-hook ()
       (unless *eval-hook-installed*
         ;; SLY hooks
         (wrap-with-generation-increment :slynk-mrepl \"MREPL-EVAL\")
         (wrap-with-generation-increment :slynk \"INTERACTIVE-EVAL\")
         (wrap-with-generation-increment :slynk \"EVAL-AND-GRAB-OUTPUT\")
         (wrap-with-generation-increment :slynk \"PPRINT-EVAL\")
         (wrap-with-generation-increment :slynk \"COMPILE-STRING-FOR-EMACS\")
         ;; SLIME hooks
         (wrap-with-generation-increment :swank \"LISTENER-EVAL\")
         (wrap-with-generation-increment :swank \"INTERACTIVE-EVAL\")
         (wrap-with-generation-increment :swank \"EVAL-AND-GRAB-OUTPUT\")
         (wrap-with-generation-increment :swank \"PPRINT-EVAL\")
         (wrap-with-generation-increment :swank \"COMPILE-STRING-FOR-EMACS\")
         (setf *eval-hook-installed* t))
       t))
   ;; Custom visualization generic function (only define if not exists)
   (unless (fboundp 'visualize)
     (defgeneric visualize (object)
       (:documentation \"Return a visualization specification for OBJECT.
Returns a list where the first element is a keyword indicating the type:
  (:html string) - Render HTML in sandboxed iframe
  (:svg string) - Render SVG graphics
  (:json string) - Display formatted JSON
  (:vega-lite spec-string) - Render Vega-Lite chart
  (:mermaid definition-string) - Render Mermaid diagram
  (:image-base64 mime-type base64-string) - Display image from base64
Return NIL to use default ICL visualization.\"))
     (defmethod visualize (object)
       \"Default method returns NIL to use built-in visualization.\"
       (declare (ignore object))
       nil))"
  "Phase 2 template: Define ICL runtime functions. Use ~S for version.")

(defun inject-icl-runtime ()
  "Inject the ICL runtime package into the connected inferior Lisp."
  (when *slynk-connected-p*
    (handler-case
        (let ((phase2-code (format nil *icl-runtime-phase2-template* +version+)))
          ;; Phase 1: Create the package
          (slynk-client:slime-eval
           (read-from-string *icl-runtime-phase1*)
           *slynk-connection*)
          ;; Phase 2: Load definitions using string stream
          ;; LOAD processes each form as a top-level form, so defvar/defun work correctly
          ;; Use CL-USER:: for the stream variable to avoid ICL package references
          (slynk-client:slime-eval
           `(cl:with-input-from-string (cl-user::icl-load-stream ,phase2-code)
              (cl:load cl-user::icl-load-stream)
              t)
           *slynk-connection*))
      (error (e)
        (format *error-output* "~&; Warning: Failed to inject ICL runtime: ~A~%" e)))))

(defun slynk-disconnect ()
  "Disconnect from the current backend server."
  (when *slynk-connection*
    (ignore-errors (slynk-client:slime-close *slynk-connection*))
    (setf *slynk-connection* nil)
    (setf *slynk-connected-p* nil)
    (format t "~&; Disconnected from Slynk~%")))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; High-level Operations
;;; ─────────────────────────────────────────────────────────────────────────────

(defun slynk-eval (string &key (package "CL-USER"))
  "Evaluate STRING in the backend server.
   Returns the result."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval `(cl:eval (cl:read-from-string ,string))
                           *slynk-connection*))

(defvar *last-error-backtrace* nil
  "Backtrace from the last error, if available.")

(defvar *last-error-condition* nil
  "Condition string from the last error.")

(defun write-slynk-string-to-active-repl (string)
  "Write STRING to the session that initiated the current evaluation."
  (let* ((eval-out (evaluating-session-output-stream))
         (active-out (active-repl-output-stream))
         (out (or eval-out active-out)))
    (if out
        (progn
          (write-string string out)
          (finish-output out))
        (progn
          ;; Fallback to standard output (usually TUI)
          (write-string string)
          (finish-output)))))

;; Verify we have ICL's vendored slynk-client (with *write-string-hook* support)
;; The upstream slynk-client from Quicklisp/Ultralisp doesn't have this feature.
;; If this check fails, ASDF loaded the wrong version.
(unless (boundp 'slynk-client:*write-string-hook*)
  (error "Wrong slynk-client version loaded. ICL requires its vendored slynk-client ~
          with *write-string-hook* support. The upstream version from Quicklisp/Ultralisp ~
          was loaded instead. Please ensure ICL's 3rd-party/slynk-client/ is in ~
          asdf:*central-registry* BEFORE any Quicklisp dist directories."))

(setf slynk-client:*write-string-hook* #'write-slynk-string-to-active-repl)

(defun slynk-eval-form (string &key (package "CL-USER"))
  "Evaluate STRING and return result values.
   Output is captured and printed before results.
   Errors are caught on the remote side to avoid Slynk debugger issues.
   Backtraces are captured and stored in *last-error-backtrace*."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  ;; Don't redirect output streams - let output go to the inferior process's stdout
  ;; which is picked up by the output reader thread. This ensures libraries like llog
  ;; that capture *standard-output* at initialization time continue to work.
  (let ((wrapper-code (format nil "(handler-case
  (let ((vals (multiple-value-list (eval (read-from-string ~S)))))
    ;; Update standard REPL history variables so ,i works
    (setf *** **
          ** *
          * (first vals))
    (force-output)
    (list :ok nil (mapcar (lambda (v) (write-to-string v :readably nil :pretty nil)) vals)))
  (error (err)
    (list :error
          ;; Clean error message: strip Stream: lines from reader errors
          (let ((msg (princ-to-string err)))
            (string-right-trim '(#\\Newline #\\Space)
              (with-output-to-string (s)
                (with-input-from-string (in msg)
                  (loop for line = (read-line in nil nil)
                        while line
                        unless (and (> (length line) 2)
                                    (char= (char line 0) #\\Space)
                                    (char= (char line 1) #\\Space)
                                    (search \"Stream:\" line))
                        do (write-line line s))))))
          (ignore-errors
            (slynk:backtrace 0 30)))))" string)))
    (handler-case
        (let ((result (with-slynk-connection
                        (slynk-client:slime-eval
                         `(cl:eval
                           (cl:let ((cl:*package* (cl:find-package "CL-USER")))
                             (cl:read-from-string ,wrapper-code)))
                         *slynk-connection*))))
          (cond
            ;; Unexpected non-list result: treat as plain output with no values.
            ((not (consp result))
             (let ((output (princ-to-string result)))
               (when (and output (> (length output) 0))
                 (write-string output)
                 (force-output))
               nil))
            (t
             (case (first result)
               (:ok
                (setf *last-error-backtrace* nil
                      *last-error-condition* nil
                      *last-was-error* nil)
                ;; Print captured output first
                (let ((output (second result))
                      (vals (third result)))
                  (when (and output (> (length output) 0))
                    (write-string output)
                    (force-output))
                  vals))
               (:error
                (setf *last-error-condition* (second result)
                      *last-error-backtrace* (third result)
                      *last-was-error* t)
                (error "~A" (second result)))
               (otherwise result)))))
      (slynk-client:slime-network-error (e)
        (setf *slynk-connected-p* nil)
        (error "Backend connection lost: ~A" e)))))

(defun slynk-eval-form-internal (string &key (package "CL-USER"))
  "Evaluate STRING for internal ICL operations without updating REPL history.
   Same as slynk-eval-form but does not modify *, **, ***, etc."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  ;; Same wrapper as slynk-eval-form but WITHOUT the setf for history variables
  (let ((wrapper-code (format nil "(handler-case
  (let ((vals (multiple-value-list (eval (read-from-string ~S)))))
    (force-output)
    (list :ok nil (mapcar (lambda (v) (write-to-string v :readably nil :pretty nil)) vals)))
  (error (err)
    (list :error
          (let ((msg (princ-to-string err)))
            (string-right-trim '(#\\Newline #\\Space)
              (with-output-to-string (s)
                (with-input-from-string (in msg)
                  (loop for line = (read-line in nil nil)
                        while line
                        unless (and (> (length line) 2)
                                    (char= (char line 0) #\\Space)
                                    (char= (char line 1) #\\Space)
                                    (search \"Stream:\" line))
                        do (write-line line s))))))
          (ignore-errors
            (slynk:backtrace 0 30)))))" string)))
    (handler-case
        (let ((result (with-slynk-connection
                        (slynk-client:slime-eval
                         `(cl:eval
                           (cl:let ((cl:*package* (cl:find-package "CL-USER")))
                             (cl:read-from-string ,wrapper-code)))
                         *slynk-connection*))))
          (cond
            ((not (consp result))
             (let ((output (princ-to-string result)))
               (when (and output (> (length output) 0))
                 (write-string output)
                 (force-output))
               nil))
            (t
             (case (first result)
               (:ok
                (let ((output (second result))
                      (vals (third result)))
                  (when (and output (> (length output) 0))
                    (write-string output)
                    (force-output))
                  vals))
               (:error
                (error "~A" (second result)))
               (otherwise result)))))
      (slynk-client:slime-network-error (e)
        (setf *slynk-connected-p* nil)
        (error "Backend connection lost: ~A" e)))))

(defun slynk-eval-form-capturing (string &key (package "CL-USER"))
  "Evaluate STRING but keep all stdout/stderr in a string.
Returns (values output-string value-strings). Does not print to the local REPL."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (let* ((wrapper-code (format nil "(handler-case
  (with-output-to-string (out)
    (let* ((*standard-output* out)
           (*error-output* out)
           (*trace-output* out)
           (*debug-io* out)
           (*terminal-io* (make-two-way-stream (make-string-input-stream \"\") out))
           (*query-io* *terminal-io*))
      (let ((vals (multiple-value-list (eval (read-from-string ~S)))))
        (setf *** ** ** * * (first vals))
        (force-output)
        (list :ok out (mapcar (lambda (v) (write-to-string v :readably nil :pretty nil)) vals)))))
  (error (err)
    (list :error (princ-to-string err) nil)))" string))
         (result (with-slynk-connection
                   (slynk-client:slime-eval
                    `(cl:eval
                      (cl:let ((cl:*package* (cl:find-package "CL-USER")))
                        (cl:read-from-string ,wrapper-code)))
                    *slynk-connection*))))
    (cond
      ((not (consp result))
       (values (princ-to-string result) nil))
      (t
       (case (first result)
         (:ok
          (values (second result) (third result)))
         (:error
          (error "~A" (second result)))
         (otherwise
          (error "Unexpected slynk response: ~A" result)))))))

(defun slynk-complete-simple (prefix &key (package "CL-USER"))
  "Get simple completions for PREFIX.
   Returns list of completion strings."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (let ((result (slynk-client:slime-eval
                 `(cl:funcall (cl:read-from-string "slynk:simple-completions") ,prefix ,package)
                 *slynk-connection*)))
    ;; Result is (completions common-prefix)
    (if (listp result)
        (first result)
        nil)))

(defun slynk-arglist (name &key (package "CL-USER"))
  "Get arglist for function NAME."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:operator-arglist") ,name ,package)
   *slynk-connection*))

(defun slynk-documentation (name type &key (package "CL-USER"))
  "Get documentation for NAME of TYPE (:function, :variable, etc)."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:let ((sym (cl:read-from-string ,name)))
      (and (cl:symbolp sym)
           (cl:documentation sym ',type)))
   *slynk-connection*))

(defun slynk-describe (name &key (package "CL-USER"))
  "Get full description of NAME."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:describe-symbol") ,name)
   *slynk-connection*))

(defun slynk-apropos (pattern &key (package nil))
  "Search for symbols matching PATTERN.
   Returns list of (symbol-name package-name kind) for each match."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  ;; Use standard CL apropos-list instead of Slynk-specific function
  ;; Use cl-user::s as the lambda var to avoid package issues
  (slynk-client:slime-eval
   `(cl:mapcar
     (cl:lambda (cl-user::s)
       (cl:list (cl:symbol-name cl-user::s)
                (cl:package-name (cl:symbol-package cl-user::s))
                (cl:cond
                  ((cl:macro-function cl-user::s) "macro")
                  ((cl:fboundp cl-user::s) "function")
                  ((cl:boundp cl-user::s) "variable")
                  ((cl:find-class cl-user::s cl:nil) "class")
                  (cl:t "symbol"))))
     (cl:apropos-list ,pattern))
   *slynk-connection*))

(defun slynk-macroexpand (form &key (package "CL-USER"))
  "Macroexpand FORM once."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:slynk-macroexpand-1") ,form)
   *slynk-connection*))

(defun slynk-macroexpand-all (form &key (package "CL-USER"))
  "Fully macroexpand FORM."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:slynk-macroexpand-all") ,form)
   *slynk-connection*))

(defun slynk-who-calls (name)
  "Find all functions that call NAME."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk-backend:who-calls") ',name)
   *slynk-connection*))

(defun slynk-who-references (name)
  "Find all code that references variable NAME."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk-backend:who-references") ',name)
   *slynk-connection*))

(defun slynk-list-callers (name)
  "List functions that call NAME."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk-backend:list-callers") ',name)
   *slynk-connection*))

(defun slynk-list-callees (name)
  "List functions called by NAME."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk-backend:list-callees") ',name)
   *slynk-connection*))

(defun slynk-list-packages ()
  "Get list of all packages."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:list-all-package-names") t)
   *slynk-connection*))

(defun slynk-set-package (package-name)
  "Change the current package in Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  ;; Wrap in error handler to avoid debugger for non-existent packages
  ;; Use cl-user::err to avoid package issues (ICL package doesn't exist in inferior)
  ;; Note: can't use gensym because uninterned symbols don't survive print/read roundtrip
  (slynk-client:slime-eval
   `(cl:handler-case
        (cl:funcall (cl:read-from-string "slynk:set-package") ,package-name)
      (cl:error (cl-user::err)
        (cl:error "~A" cl-user::err)))
   *slynk-connection*))

(defun slynk-current-package ()
  "Return the current package name from the backend."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   '(cl:package-name cl:*package*)
   *slynk-connection*))

(defun slynk-list-threads ()
  "Get list of all threads from Slynk.
   Returns (LABELS (ID NAME STATUS ATTRS ...) ...)."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:list-threads"))
   *slynk-connection*))

(defun slynk-lisp-info ()
  "Get the Lisp implementation type and version from the backend.
   Returns a plist with :type and :version keys, or NIL if not connected."
  (when *slynk-connected-p*
    (handler-case
        (slynk-client:slime-eval
         '(cl:list :type (cl:lisp-implementation-type)
                   :version (cl:lisp-implementation-version))
         *slynk-connection*)
      (error () nil))))
