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

(defun slynk-verify-connection (connection &optional (timeout-seconds 15))
  "Verify that CONNECTION is responsive by doing a simple eval with timeout.
   Returns T if the connection is responsive, NIL otherwise.
   Default timeout is 15 seconds because some backends (CCL, ABCL) take
   longer to become responsive after Slynk starts."
  #+sbcl
  (handler-case
      (sb-ext:with-timeout timeout-seconds
        ;; Try a minimal eval - just return T
        (let ((result (slynk-client:slime-eval 'cl:t connection)))
          (eq result t)))
    (sb-ext:timeout ()
      (when *verbose*
        (format *error-output* "~&; Slynk connection verification timed out after ~Ds~%" timeout-seconds))
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
              ;; Give dispatcher thread time to clean up before retrying
              (progn
                (ignore-errors (slynk-client:slime-close conn))
                (sleep 0.5)
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
  (:regexp pattern-string) - Render regex railroad diagram
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

(defun package-excluded-p (package-name)
  "Check if PACKAGE-NAME matches any exclusion pattern in *viz-package-exclusions*."
  (when *viz-package-exclusions*
    (some (lambda (pattern)
            (cl-ppcre:scan pattern package-name))
          *viz-package-exclusions*)))

(defun process-library-visualizations ()
  "Call REGISTER-ICL-VIZ in any package that defines it.
Libraries can define this function to register their visualization methods
with icl-runtime:visualize after ICL connects.
Tracks which packages have been processed to avoid duplicate registrations.
Respects *viz-package-exclusions* for filtering packages by regex."
  (when *slynk-connected-p*
    (handler-case
        (let ((candidates (slynk-client:slime-eval
                           '(cl:progn
                              ;; Ensure tracking hash table exists
                              (cl:unless (cl:boundp 'cl-user::*icl-viz-registered-packages*)
                                (cl:setf (cl:symbol-value 'cl-user::*icl-viz-registered-packages*)
                                         (cl:make-hash-table :test 'cl:equal)))
                              ;; Collect unprocessed packages with REGISTER-ICL-VIZ
                              (cl:let ((cl-user::ht (cl:symbol-value 'cl-user::*icl-viz-registered-packages*))
                                       (cl-user::result nil))
                                (cl:dolist (cl-user::pkg (cl:list-all-packages) cl-user::result)
                                  (cl:let ((cl-user::fn (cl:find-symbol "REGISTER-ICL-VIZ" cl-user::pkg))
                                           (cl-user::name (cl:package-name cl-user::pkg)))
                                    (cl:when (cl:and cl-user::fn
                                                     (cl:fboundp cl-user::fn)
                                                     (cl:not (cl:gethash cl-user::name cl-user::ht)))
                                      (cl:push cl-user::name cl-user::result))))))
                           *slynk-connection*)))
          ;; Filter out excluded packages (regex matching in ICL where cl-ppcre is available)
          (dolist (pkg-name candidates)
            (unless (package-excluded-p pkg-name)
              (handler-case
                  (slynk-client:slime-eval
                   `(cl:let ((cl-user::pkg (cl:find-package ,pkg-name)))
                      (cl:when cl-user::pkg
                        (cl:let ((cl-user::fn (cl:find-symbol "REGISTER-ICL-VIZ" cl-user::pkg)))
                          (cl:when (cl:and cl-user::fn (cl:fboundp cl-user::fn))
                            (cl:funcall cl-user::fn)
                            (cl:setf (cl:gethash ,pkg-name
                                                 (cl:symbol-value 'cl-user::*icl-viz-registered-packages*))
                                     cl:t)))))
                   *slynk-connection*)
                (error (e)
                  (format *error-output* "~&; Warning: ~A:REGISTER-ICL-VIZ failed: ~A~%"
                          pkg-name e))))))
      (error (e)
        (format *error-output* "~&; Warning: Failed to process library visualizations: ~A~%" e)))))

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

(setf slynk-client:*debug-hook*
      (lambda (thread level condition restarts frames conts)
        (declare (ignore conts))
        (setf *pending-debug-event*
              (list :thread thread :level level
                    :condition condition :restarts restarts
                    :frames frames))))

(setf slynk-client:*debug-return-hook*
      (lambda (thread level stepping)
        (declare (ignore thread level stepping))
        (setf *debug-return-received* t)))

(defvar *interactive-debugger-enabled* t
  "When T, REPL evaluation uses the interactive debugger for errors.
   When NIL, falls back to the handler-case wrapper.")

(defvar *eval-in-progress* nil
  "T when backend evaluation is in progress.
   Used by the websocket handler to detect Ctrl-C during evaluation.")

(defun interrupt-backend-eval ()
  "Send an interrupt to the backend Slynk server.
   Interrupts the current evaluation thread."
  (when (and *slynk-connected-p* *slynk-connection*)
    (ignore-errors
      (with-slynk-connection
        (slynk-client::slime-send `(:emacs-interrupt t) *slynk-connection*)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Backend Input Redirection (:read-string protocol)
;;; ─────────────────────────────────────────────────────────────────────────────

;; Evaluated in the backend Lisp after connecting. Rebinds worker-thread
;; *STANDARD-INPUT* to a Slynk gray stream that requests input from ICL with
;; a (:read-string thread tag) event and blocks until ICL answers with
;; (:emacs-return-string thread tag string). Base Slynk (SLY) never generates
;; :read-string itself - that machinery lives in the mrepl contrib, which ICL
;; does not use - but its event dispatcher forwards the event to the client
;; and routes the reply back to the waiting thread, so the SLIME-era protocol
;; works without loading mrepl. Uses FIND-SYMBOL throughout because the form
;; must also be readable by Lisps where these internals are absent.
(defparameter +backend-input-redirection-code+
  "(handler-case
       (let ((bindings-var (find-symbol \"*DEFAULT-WORKER-THREAD-BINDINGS*\" :slynk))
             (make-input-stream (find-symbol \"MAKE-INPUT-STREAM\" :slynk-backend))
             (send-to-emacs (find-symbol \"SEND-TO-EMACS\" :slynk))
             (wait-for-event (find-symbol \"WAIT-FOR-EVENT\" :slynk))
             (make-tag (find-symbol \"MAKE-TAG\" :slynk))
             (current-thread-id (find-symbol \"CURRENT-THREAD-ID\" :slynk)))
         (when (and bindings-var make-input-stream send-to-emacs
                    wait-for-event make-tag current-thread-id)
           (let* ((out *standard-output*)
                  (in (funcall make-input-stream
                               (lambda ()
                                 (force-output out)
                                 (let ((tag (funcall make-tag)))
                                   (funcall send-to-emacs
                                            (list :read-string
                                                  (funcall current-thread-id)
                                                  tag))
                                   (or (third (funcall wait-for-event
                                                       (list :emacs-return-string
                                                             tag 'value)))
                                       \"\")))))
                  (io (make-two-way-stream in out)))
             (setf (symbol-value bindings-var)
                   (list (cons '*standard-input* in)
                         (cons '*debug-io* io)
                         (cons '*query-io* io)
                         (cons '*terminal-io* io)))
             t)))
     (error () nil))"
  "Backend code that routes worker-thread input reads through :read-string.")

(defun configure-backend-input-redirection ()
  "Make READ-LINE etc. in backend evaluations request input from ICL.
   Evaluates +BACKEND-INPUT-REDIRECTION-CODE+ in the backend, replacing the
   worker-thread stream bindings set up at backend startup (which point at
   the inferior process's stdin pipe - a pipe ICL never writes to, so reads
   on it block forever; see issue #42). Returns T if redirection was
   installed, NIL otherwise."
  (when *slynk-connected-p*
    (handler-case
        (slynk-client:slime-eval
         `(cl:eval (cl:let ((cl:*package* (cl:find-package "CL-USER")))
                     (cl:read-from-string ,+backend-input-redirection-code+)))
         *slynk-connection*)
      (error (e)
        (format *error-output*
                "~&; Warning: Failed to configure backend input redirection: ~A~%" e)
        nil))))

(defgeneric backend-read-line (in out)
  (:documentation
   "Read one line of user input from IN for a backend :read-string request.
    OUT is the evaluating session's output stream, for echoing on stream
    types whose terminal does not echo locally. Returns the line with a
    trailing newline, \"\" on end-of-file, or NIL if the evaluation ended
    before a full line arrived."))

(defmethod backend-read-line ((in t) (out t))
  ;; TUI: the terminal is in cooked mode during evaluation, so the tty
  ;; handles echo and line editing. Poll rather than block so we can give
  ;; up if the evaluation is interrupted; a READ-LINE blocked here would
  ;; otherwise steal the first line the user types at the next REPL prompt.
  (loop
    (cond ((not *eval-in-progress*)
           (return nil))
          ((listen in)
           (let ((line (read-line in nil nil)))
             (return (if line
                         (concatenate 'string line (string #\Newline))
                         ""))))
          (t (sleep 0.02)))))

(defun read-string-for-backend ()
  "Provide one line of user input for a blocking read in the backend Lisp.
   Called on a dedicated thread when the backend sends a :read-string event,
   i.e. when evaluated code reads from *STANDARD-INPUT* (e.g. READ-LINE).
   Returns the line with a trailing newline, \"\" on end-of-file, or NIL to
   leave the request unanswered (evaluation ended, or the evaluating session
   has no input stream to read from)."
  (let* ((session (bt:with-lock-held (*evaluating-session-lock*)
                    *evaluating-session*))
         (in (if session
                 (repl-session-input-stream session)
                 *standard-input*))
         (out (or (evaluating-session-output-stream) *standard-output*)))
    (when in
      ;; Give the output reader a moment to drain backend output written
      ;; just before the read, then flush it so a partial line (e.g. a
      ;; "Name: " prompt, which the newline-triggered flush in the output
      ;; reader would hold back) is visible before we wait for input.
      (sleep 0.05)
      (ignore-errors (force-output out))
      (backend-read-line in out))))

(setf slynk-client:*read-string-hook* #'read-string-for-backend)

(defun debugger-eval-in-thread (sexp thread)
  "Synchronously evaluate SEXP on debug THREAD via slynk."
  (with-slynk-connection
    (slynk-client:slime-eval-in-thread sexp *slynk-connection* thread)))

(defun send-debugger-restart (event choice)
  "Send restart CHOICE for debug EVENT to the backend.
   CHOICE is :abort, a restart index (integer), or (index . value-string)
   for restarts that require a value (e.g. USE-VALUE)."
  (let ((thread (getf event :thread))
        (level (getf event :level)))
    (with-slynk-connection
      (cond
        ((eq choice :abort)
         (slynk-client:slime-eval-async-in-thread
          '(slynk:throw-to-toplevel) *slynk-connection* thread))
        ((consp choice)
         ;; Interactive restart with value: (index . value-string)
         ;; Bypass invoke-nth-restart-for-emacs because it rebinds *query-io*
         ;; to a stream that calls read-from-minibuffer-in-emacs (no Emacs here).
         ;; Instead, directly invoke the restart with the evaluated value.
         (let ((index (car choice))
               (value-string (cdr choice)))
           (slynk-client:slime-eval-async-in-thread
            `(cl:when (cl:= ,level slynk::*sly-db-level*)
               (cl:let ((cl-user::r (slynk::nth-restart ,index)))
                 (cl:when cl-user::r
                   (cl:invoke-restart cl-user::r
                     (cl:eval (cl:read-from-string ,value-string))))))
            *slynk-connection* thread)))
        (t
         (slynk-client:slime-eval-async-in-thread
          `(slynk:invoke-nth-restart-for-emacs ,level ,choice)
          *slynk-connection* thread))))))

(defun slynk-eval-form-with-debugger (string)
  "Evaluate STRING via Slynk without handler-case wrapping.
   Errors propagate to Slynk's debugger, allowing interactive restart invocation.
   The dispatcher thread receives :debug events and sets *pending-debug-event*.
   This function polls for debug events and shows the TUI."
  (let* ((eval-code (format nil "
  (let ((vals (multiple-value-list (eval (read-from-string ~S)))))
    (setf *** **
          ** *
          * (first vals))
    (force-output)
    (list :ok nil (mapcar (lambda (v) (write-to-string v :readably nil :pretty nil)) vals)))" string))
         (result-lock (bt:make-lock "debugger-eval"))
         (result-cv (bt:make-condition-variable))
         (result-available nil)
         (result nil))
    ;; Clear debug event state
    (setf *pending-debug-event* nil
          *debug-return-received* nil
          *eval-in-progress* t)
    ;; Send eval asynchronously - errors will trigger :debug events
    (with-slynk-connection
      (slynk-client:slime-eval-async
       `(cl:eval
          (cl:let ((cl:*package* (cl:find-package "CL-USER")))
            (cl:read-from-string ,eval-code)))
       *slynk-connection*
       (lambda (x)
         (bt:with-lock-held (result-lock)
           (setf result x
                 result-available t)
           (bt:condition-notify result-cv)))))
    ;; Poll loop: wait for result or debug events
    (unwind-protect
        (loop
          ;; Check for result
          (bt:with-lock-held (result-lock)
            (when result-available
              (return-from slynk-eval-form-with-debugger
                (process-debugger-eval-result result))))
          ;; Check for debug event from dispatcher thread
          (let ((debug-event *pending-debug-event*))
            (when debug-event
              (setf *pending-debug-event* nil)
              ;; Store debug info for post-mortem ,debug / ,bt
              (let ((condition (getf debug-event :condition)))
                (setf *last-debug-info*
                      (list :condition-type
                            (if (and (listp condition) (second condition))
                                (second condition)
                                "ERROR")
                            :condition-message
                            (if (listp condition) (first condition) "")
                            :restarts (getf debug-event :restarts)
                            :frames (getf debug-event :frames))))
              ;; Show interactive debugger TUI
              (let ((choice (run-debugger-interactive debug-event)))
                ;; Send the user's choice back to Slynk
                (send-debugger-restart debug-event choice)
                ;; Reset for potential re-entry (nested errors)
                (setf *debug-return-received* nil))))
          ;; Brief sleep to avoid busy-waiting
          (sleep 0.05))
      (setf *eval-in-progress* nil))))

(defun process-debugger-eval-result (result)
  "Process the result from a debugger-enabled eval.
   Returns value strings on success, signals error on abort."
  (cond
    ;; Aborted evaluation (restart that aborted, or error in debug command)
    ((and (consp result) (eq (car result) slynk-client::+abort+))
     (setf *last-was-error* t
           *last-error-condition* (cdr result)
           *last-error-backtrace* nil)
     (error "~A" (or (cdr result) "Evaluation aborted")))
    ;; Successful structured result
    ((and (consp result) (eq (first result) :ok))
     (setf *last-error-backtrace* nil
           *last-error-condition* nil
           *last-was-error* nil)
     (let ((output (second result))
           (vals (third result)))
       (when (and output (stringp output) (> (length output) 0))
         (write-string output)
         (force-output))
       vals))
    ;; Plain value (e.g. from a restart that returns a value)
    (t
     (setf *last-error-backtrace* nil
           *last-error-condition* nil
           *last-was-error* nil)
     (if result
         (list (princ-to-string result))
         nil))))

(defun slynk-eval-form (string &key (package "CL-USER"))
  "Evaluate STRING and return result values.
   When *interactive-debugger-enabled* is T, errors trigger the interactive
   debugger TUI with live restart invocation.
   Otherwise, uses handler-case wrapper for error catching."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (if *interactive-debugger-enabled*
      (slynk-eval-form-with-debugger string)
      (slynk-eval-form-wrapped string)))

(defun %eval-wrapper-code (string &key update-history)
  "Build the remote error-capturing wrapper code that evaluates STRING.
   When UPDATE-HISTORY, the generated code also updates the *, **, ***
   REPL history variables. Shared by the wrapped and internal eval paths."
  (format nil "(let ((captured-restarts nil) (captured-ctype nil))
  (handler-case
    (handler-bind
      ((error (lambda (c)
                (setf captured-ctype (princ-to-string (type-of c)))
                (setf captured-restarts
                      (mapcar (lambda (r)
                                (list (princ-to-string (restart-name r))
                                      (handler-case (princ-to-string r) (error () \"\"))))
                              (compute-restarts c))))))
      (let ((vals (multiple-value-list (eval (read-from-string ~S)))))
~A        (force-output)
        (list :ok nil (mapcar (lambda (v) (write-to-string v :readably nil :pretty nil)) vals))))
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
              (slynk:backtrace 0 30))
            captured-ctype
            captured-restarts))))" string
          (if update-history
              "        ;; Update standard REPL history variables so ,i works
        (setf *** **
              ** *
              * (first vals))
"
              "")))

(defun slynk-eval-form-wrapped (string)
  "Evaluate STRING with handler-case wrapping (original error handling).
   Used as fallback when interactive debugger is disabled."
  ;; Don't redirect output streams - let output go to the inferior process's stdout
  ;; which is picked up by the output reader thread. This ensures libraries like llog
  ;; that capture *standard-output* at initialization time continue to work.
  (setf *eval-in-progress* t)
  (let ((wrapper-code (%eval-wrapper-code string :update-history t)))
    (unwind-protect
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
                          *last-was-error* nil
                          *last-debug-info* nil)
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
                          *last-was-error* t
                          *last-debug-info* (list :condition-type (or (fourth result) "ERROR")
                                                  :condition-message (second result)
                                                  :restarts (fifth result)
                                                  :frames (third result)))
                    (error "~A" (second result)))
                   (otherwise result)))))
          (slynk-client:slime-network-error (e)
            (setf *slynk-connected-p* nil)
            (error "Backend connection lost: ~A" e)))
      (setf *eval-in-progress* nil))))

(defun slynk-eval-form-internal (string &key (package "CL-USER"))
  "Evaluate STRING for internal ICL operations without updating REPL history.
   Same as slynk-eval-form but does not modify *, **, ***, etc."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  ;; Same wrapper as slynk-eval-form but WITHOUT the setf for history variables
  (let ((wrapper-code (%eval-wrapper-code string)))
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
  (let* ((wrapper-code (format nil "(let* ((out (make-string-output-stream))
       (*standard-output* out)
       (*error-output* out)
       (*trace-output* out)
       (*debug-io* out)
       (*terminal-io* (make-two-way-stream (make-string-input-stream \"\") out))
       (*query-io* *terminal-io*))
  (handler-case
    (let ((vals (multiple-value-list (eval (read-from-string ~S)))))
      (setf *** ** ** * * (first vals))
      (force-output)
      (list :ok (get-output-stream-string out)
            (mapcar (lambda (v) (write-to-string v :readably nil :pretty nil)) vals)))
    (error (err)
      (list :error (princ-to-string err) (get-output-stream-string out)))))" string))
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
   `(cl:let ((cl-user::s (cl:read-from-string ,name)))
      (cl:and (cl:symbolp cl-user::s)
              (cl:documentation cl-user::s ',type)))
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
