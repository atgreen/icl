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
  (let ((*in-repl* t)
        (*input-count* 0))
    (when banner
      (print-banner))
    (repl-loop)))

(defun repl-loop ()
  "Main REPL loop with restart handling."
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
    (save-history)
    ;; Stop inferior Lisp if running
    (when (and *use-slynk* (inferior-lisp-alive-p))
      (stop-inferior-lisp))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Single REPL Iteration
;;; ─────────────────────────────────────────────────────────────────────────────

(defun repl-iteration ()
  "Process a single REPL iteration: read, dispatch, and handle."
  (let ((input (read-complete-input)))
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
  (when (probe-file *config-file*)
    (handler-case
        (progn
          (load *config-file* :verbose nil :print nil)
          (format t "~&; Loaded ~A~%" *config-file*))
      (error (e)
        (format *error-output*
                "~&; Warning: Error loading ~A: ~A~%"
                *config-file* e)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Full REPL with Configuration
;;; ─────────────────────────────────────────────────────────────────────────────

(defun start-repl (&key (load-config t) (banner t))
  "Start ICL REPL with optional configuration loading.
   LOAD-CONFIG: if T, load ~/.iclrc
   BANNER: if T, print startup banner"
  ;; Load persistent history
  (load-history)
  ;; Load user config
  (when load-config
    (load-user-config))
  (run-repl :banner banner))
