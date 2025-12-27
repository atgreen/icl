;;; repl.lisp --- Main REPL loop for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; REPL Entry Point
;;; ─────────────────────────────────────────────────────────────────────────────

(defun run-repl (&key (banner t))
  "Main REPL entry point.
   If BANNER is T, print startup banner."
  ;; Create TUI session for output routing
  (let* ((session (make-repl-session
                   :name "TUI"
                   :output-stream *standard-output*
                   :input-stream *standard-input*))
         (*current-session* session)
         (*in-repl* t)
         (*input-count* 0))
    ;; TUI is always the primary session
    (setf *primary-session* session)
    ;; Load history into this session
    (load-history session)
    (unwind-protect
         (progn
           (when banner
             (print-banner))
           (repl-loop session))
      ;; Cleanup session on exit
      (setf (repl-session-active-p session) nil)
      (unregister-session session))))

(defun cleanup-on-exit (session)
  "Clean up resources when exiting the REPL.
   Suppresses thread-related errors that can occur during Hunchentoot shutdown."
  ;; Save session history first (before any potential errors)
  (ignore-errors (save-history session))
  ;; Stop browser if running
  (ignore-errors (stop-browser))
  ;; Brief pause to let any pending I/O complete
  (sleep 0.1)
  ;; Stop HTTP MCP server if running
  (ignore-errors (stop-mcp-http-server))
  ;; Stop inferior Lisp if running
  (when (inferior-lisp-alive-p)
    (ignore-errors (stop-inferior-lisp)))
  ;; Redirect error output to suppress thread cleanup errors from Clack/Hunchentoot
  ;; that occur when SBCL terminates threads during process exit.
  ;; This is a workaround for: https://github.com/edicl/hunchentoot/issues/131
  #+sbcl
  (setf sb-sys:*stderr* (make-broadcast-stream)))

(defun repl-loop (&optional (session *current-session*))
  "Main REPL loop with restart handling for SESSION."
  (unwind-protect
       (loop
         (restart-case
             (repl-iteration)
           (abort ()
             :report "Return to ICL REPL"
             (format t "~&; Aborted~%"))
           (quit ()
             :report "Exit ICL"
             (format t "~&; Goodbye~%")
             (return-from repl-loop))))
    ;; Cleanup on exit
    (cleanup-on-exit session)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Single REPL Iteration
;;; ─────────────────────────────────────────────────────────────────────────────

(defun repl-iteration ()
  "Process a single REPL iteration: read, dispatch, and handle."
  (let ((input (read-complete-input)))
    ;; Handle cancel (Ctrl-C) from multiline editor
    (when (eql input :cancel)
      (invoke-restart 'abort))
    ;; Handle EOF (Ctrl-D)
    (when (null input)
      (format t "~&")
      (invoke-restart 'quit))
    ;; Trim and skip empty input
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) input)))
      (when (zerop (length trimmed))
        (return-from repl-iteration))
      ;; Dispatch based on input type
      (cond
        ;; Command: starts with comma
        ((char= (char trimmed 0) #\,)
         (handle-command trimmed))
        ;; Lisp expression
        (t
         (eval-and-print input))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Configuration Loading
;;; ─────────────────────────────────────────────────────────────────────────────

(defun load-user-config ()
  "Load user configuration file if it exists."
  (let ((cfile (config-file)))
    (when (probe-file cfile)
      (handler-case
          (progn
            (load cfile :verbose nil :print nil)
            (format t "~&; Loaded ~A~%" cfile))
        (error (e)
          (format *error-output*
                  "~&; Warning: Error loading ~A: ~A~%"
                  cfile e))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Full REPL with Configuration
;;; ─────────────────────────────────────────────────────────────────────────────

(defun start-repl (&key (load-config t) (banner t))
  "Start ICL REPL with optional configuration loading.
   LOAD-CONFIG: if T, load ~/.iclrc
   BANNER: if T, print startup banner"
  ;; Theme initialization is done early in handle-cli (before inferior lisp)
  ;; to avoid race conditions with terminal queries. Only initialize here
  ;; if called directly (not through handle-cli).
  (unless *current-terminal-theme*
    (initialize-themes)
    (setup-highlight-colors))
  ;; Load user config (may override theme settings)
  (when load-config
    (load-user-config))
  ;; Note: History is loaded inside run-repl after session is created
  (run-repl :banner banner))
