;;; eval.lisp --- Evaluation for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; History Management
;;; ─────────────────────────────────────────────────────────────────────────────

(defun update-result-history (form values)
  "Update result history variables with VALUES. FORM is unused but kept for hook compatibility."
  (declare (ignore form))
  ;; Rotate result history (both icl-* and IRB-style _)
  (setf icl-*** icl-**
        icl-** icl-*
        icl-* (first values)
        ___ __
        __ _
        _ (first values))
  ;; Rotate values history
  (setf icl-/// icl-//
        icl-// icl-/
        icl-/ values))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Hooks
;;; ─────────────────────────────────────────────────────────────────────────────

(defun run-before-eval-hooks (form)
  "Run all before-eval hooks with FORM."
  (dolist (hook *before-eval-hook*)
    (handler-case
        (funcall hook form)
      (error (e)
        (format *error-output* "~&;; Warning: before-eval hook error: ~A~%" e)))))

(defun run-after-eval-hooks (form values)
  "Run all after-eval hooks with FORM and VALUES."
  (dolist (hook *after-eval-hook*)
    (handler-case
        (funcall hook form values)
      (error (e)
        (format *error-output* "~&;; Warning: after-eval hook error: ~A~%" e)))))

(defun sync-package-context-from-backend ()
  "Query backend for current package and update prompt context."
  (when *slynk-connected-p*
    (ignore-errors
      (let ((pkg-name (slynk-current-package)))
        (when (and pkg-name (stringp pkg-name))
          (setf *icl-package-name* (string-upcase pkg-name))
          (let ((pkg (find-package *icl-package-name*)))
            (when pkg
              (setf *icl-package* pkg))))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Safe Evaluation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun read-form (input)
  "Read a Lisp form from INPUT string.
   Returns the form, or signals a condition on error."
  (let ((*package* *icl-package*))
    (read-from-string input)))

(defun update-input-history (form)
  "Update input history (icl-+) with FORM. Called before evaluation."
  (setf icl-+++ icl-++
        icl-++ icl-+
        icl-+ form)
  ;; Increment input count
  (incf *input-count*))

(defun record-repl-interaction (input result &optional error-p)
  "Record a REPL interaction in *repl-history* for MCP access.
   INPUT is the string input, RESULT is the result string (or error message).
   ERROR-P is T if this was an error."
  (push (list input result error-p) *repl-history*)
  ;; Trim to max size
  (when (> (length *repl-history*) *repl-history-max*)
    (setf *repl-history* (subseq *repl-history* 0 *repl-history-max*))))

(defun eval-and-print (input)
  "Parse, evaluate, and print results from INPUT string.
   Handles all errors gracefully. Uses Slynk backend for evaluation."
  ;; Brief yield - fixes timing issue with external slynk connections
  (sleep 0.01)
  ;; Track which session is evaluating for output routing
  (bt:with-lock-held (*evaluating-session-lock*)
    (setf *evaluating-session* *current-session*))
  ;; Keep backward compat for old mechanism
  (set-active-repl-output *standard-output*)
  (unwind-protect
       ;; Try to read form locally for hooks/history, but don't fail if it doesn't work
       ;; (e.g., if the form uses packages only defined in the backend)
       (let ((form (ignore-errors (read-form input))))
    (when form
      ;; Update input history BEFORE evaluation so it's captured even on error
      (update-input-history form)
      (run-before-eval-hooks form))
    (handler-case
        (let ((result nil)
              (captured-output nil))
          (if *capture-backend-output*
              (multiple-value-setq (captured-output result)
                (backend-eval-capture input))
              (setf result (backend-eval input)))
          (when (and *capture-backend-output*
                     captured-output
                     (not (string= captured-output "")))
            (write-string captured-output *standard-output*)
            (force-output *standard-output*))
          ;; Update result history/hooks with best-effort values
          (let ((values (cond
                          ((listp result) result)
                          ((null result) nil)
                          (t (list result)))))
            (when form
              (update-result-history form values)
              (run-after-eval-hooks form values)))
          ;; Always sync prompt package from backend after successful eval.
          (sync-package-context-from-backend)
          ;; Record for MCP history access
          (let ((result-str (cond
                              ((null result) "")
                              ((stringp result) result)
                              ((listp result) (format nil "~{~S~^, ~}" result))
                              (t (format nil "~S" result)))))
            (record-repl-interaction input result-str nil))
          ;; Handle the result for display
          ;; Note: Editor already printed newline, so we're at start of line
          ;; For browser sessions, give async output time to arrive before printing result
          (when *browser-terminal-active*
            (sleep 0.1)
            (finish-output *standard-output*))
          (cond
            ((null result)
             ;; No result - might be output already printed
             nil)
            ((stringp result)
             ;; listener-eval returns string representation
             (unless (string= result "")
               (format t "~A~A~%"
                       (colorize *result-prefix* *color-prefix*)
                       result)))
            ((listp result)
             ;; Structured result
             (print-values result))
            (t
             ;; Unexpected result type - print as-is
             (format t "~A~S~%" (colorize *result-prefix* *color-prefix*) result)))
          ;; Refresh browser package/symbol lists and visualizations after successful evaluation
          (when *browser-terminal-active*
            (refresh-browser-lists)
            (refresh-browser-visualizations)))
      (undefined-function (e)
        (let ((msg (format nil "Undefined function: ~A" (cell-error-name e))))
          (sync-package-context-from-backend)
          (record-repl-interaction input msg t)
          (format *error-output* "~&~A~%" msg)))
      (unbound-variable (e)
        (let ((msg (format nil "Unbound variable: ~A" (cell-error-name e))))
          (sync-package-context-from-backend)
          (record-repl-interaction input msg t)
          (format *error-output* "~&~A~%" msg)))
      (error (e)
        ;; Give the process a moment to die (quit may be async)
        (sleep 0.1)
        ;; If backend connection lost or process died, exit the REPL quietly
        ;; For external connections (--connect), only check *slynk-connected-p*
        ;; For spawned processes, also check if the process is still alive
        (unless (if *external-slynk-connection*
                    *slynk-connected-p*
                    (and *slynk-connected-p* (inferior-lisp-alive-p)))
          (setf *slynk-connected-p* nil)
          (invoke-restart 'quit))
        ;; Record error for MCP
        (record-repl-interaction input (format nil "Error: ~A" e) t)
        ;; Print the error for other cases
        (format *error-output* "~&Error: ~A~%" e)
        ;; Sync prompt package even after errors (best-effort).
        (sync-package-context-from-backend)
        ;; Hint about backtrace if available
        (when *last-error-backtrace*
          (format *error-output* "~&~A~%" (colorize "  Use ,bt for backtrace, ,debug for interactive debugger" *color-dim*)))
        ;; Optionally invoke error hook
        (when *error-hook*
          (funcall *error-hook* e)))))
    ;; Note: We intentionally do NOT clear *evaluating-session* here.
    ;; Backend output can arrive asynchronously after backend-eval returns.
    ;; Keeping *evaluating-session* set ensures delayed output goes to the
    ;; correct session. It will be updated when a new evaluation starts.
    ))
