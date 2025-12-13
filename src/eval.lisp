;;; eval.lisp --- Evaluation for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; History Management
;;; ─────────────────────────────────────────────────────────────────────────────

(defun update-history (form values)
  "Update history variables with new form and values."
  ;; Rotate result history (both icl-* and IRB-style _)
  (setf icl-*** icl-**
        icl-** icl-*
        icl-* (first values)
        ___ __
        __ _
        _ (first values))
  ;; Rotate input history
  (setf icl-+++ icl-++
        icl-++ icl-+
        icl-+ form)
  ;; Rotate values history
  (setf icl-/// icl-//
        icl-// icl-/
        icl-/ values)
  ;; Increment input count
  (incf *input-count*))

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

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Safe Evaluation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun read-form (input)
  "Read a Lisp form from INPUT string.
   Returns the form, or signals a condition on error."
  (let ((*package* *icl-package*))
    (read-from-string input)))

(defun eval-form (form)
  "Evaluate FORM in the ICL context.
   Returns multiple values as a list."
  (let ((*package* *icl-package*))
    (multiple-value-list (eval form))))

(defun eval-input (input)
  "Parse and evaluate INPUT string.
   Returns (VALUES values-list form) on success.
   Signals conditions on errors."
  (let ((form (read-form input)))
    (run-before-eval-hooks form)
    (let ((values (eval-form form)))
      (update-history form values)
      (run-after-eval-hooks form values)
      (values values form))))

(defun eval-and-print (input)
  "Parse, evaluate, and print results from INPUT string.
   Handles all errors gracefully. Uses backend abstraction for evaluation."
  (handler-case
      (if *use-slynk*
          ;; Slynk backend evaluation
          (multiple-value-bind (result output)
              (backend-eval input)
            ;; Output any printed output from the inferior Lisp
            (when (and output (plusp (length output)))
              (write-string output)
              (unless (char= (char output (1- (length output))) #\Newline)
                (terpri)))
            ;; Handle the result
            (cond
              ((null result)
               ;; No result - might be output already printed
               nil)
              ((stringp result)
               ;; listener-eval returns string representation
               (unless (string= result "")
                 (format t "~&~A~A~%"
                         (colorize *result-prefix* *color-prefix*)
                         result)))
              ((listp result)
               ;; Structured result
               (print-values result))))
          ;; Local evaluation (original behavior)
          (multiple-value-bind (values form)
              (eval-input input)
            (declare (ignore form))
            (print-values values)))
    (reader-error (e)
      (format *error-output* "~&Read error: ~A~%" e))
    (package-error (e)
      (format *error-output* "~&Package error: ~A~%" e))
    (undefined-function (e)
      (format *error-output* "~&Undefined function: ~A~%"
              (cell-error-name e)))
    (unbound-variable (e)
      (format *error-output* "~&Unbound variable: ~A~%"
              (cell-error-name e)))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e)
      ;; Optionally invoke error hook
      (when *error-hook*
        (funcall *error-hook* e)))))
