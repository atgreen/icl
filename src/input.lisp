;;; input.lisp --- Input handling for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Terminal Detection
;;; ─────────────────────────────────────────────────────────────────────────────

(defun terminal-capable-p ()
  "Check if we have a capable terminal for advanced input."
  (or *browser-terminal-active*
      (and (termp:termp)
           (not (string= (uiop:getenv "TERM") "dumb")))))

(defun select-input-backend ()
  "Select appropriate input backend based on terminal capabilities.
   Respects *use-multiline-editor* setting."
  (if (and *use-multiline-editor* (terminal-capable-p))
      :multiline
      :simple))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Prompt Generation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun make-prompt ()
  "Generate the primary prompt string with dimmed package name."
  (if *prompt-hook*
      (funcall *prompt-hook* *icl-package*)
      (format nil (or *prompt-string* "~A> ")
              (colorize *icl-package-name* *ansi-prompt*))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Input Reading
;;; ─────────────────────────────────────────────────────────────────────────────

(defun read-simple ()
  "Simple fallback input for dumb terminals."
  (format t "~A" (make-prompt))
  (force-output)
  (let ((line (read-line *standard-input* nil nil)))
    (when line
      (string-trim '(#\Space #\Tab) line))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Multi-line Input
;;; ─────────────────────────────────────────────────────────────────────────────

(defun make-continuation-prompt ()
  "Generate continuation prompt aligned with primary prompt (dimmed)."
  (let* ((primary (make-prompt))
         (width (visible-string-length primary)))
    (cond
      ;; Allow users to override with a literal continuation prompt string.
      ((and (stringp *continuation-prompt*)
            (not (string= *continuation-prompt* ".. ")))
       (colorize *continuation-prompt* *ansi-prompt*))
      ;; Default: aligned dots matching primary prompt width.
      (t
       (colorize (concatenate 'string
                              (make-string (max 0 (- width 2)) :initial-element #\.)
                              ". ")
                 *ansi-prompt*)))))

(defun read-complete-input ()
  "Read a complete Lisp form with appropriate backend.
   Returns the complete input string or NIL on EOF."
  (let ((backend (select-input-backend)))
    (case backend
      (:multiline
       (read-with-multiline-editor))
      (otherwise
       (read-with-simple-continuation)))))

(defun read-with-multiline-editor ()
  "Read using the built-in multi-line editor.
   Falls back to simple-continuation if multiline editor fails to initialize."
  (let ((result (multiline-edit
                 :prompt (make-prompt)
                 :continuation-prompt (make-continuation-prompt))))
    (cond
      ((eql result :cancel)
       ;; User hit Ctrl-C; propagate sentinel for REPL abort handling
       :cancel)
      ((eql result :not-a-tty)
       ;; Not a real terminal, fall back to simple
       (read-with-simple-continuation))
      ((null result)
       ;; EOF
       nil)
      (t result))))

(defun read-with-simple-continuation ()
  "Read using simple input with continuation prompts."
  (let ((lines nil)
        (cont-prompt nil))
    (loop
      (let ((line (cond
                    (lines
                     (unless cont-prompt
                       (setf cont-prompt (make-continuation-prompt)))
                     (format t "~A" cont-prompt)
                     (force-output)
                     (read-line *standard-input* nil nil))
                    (t
                     (read-simple)))))
        (unless line
          (return (if lines
                      (format nil "~{~A~%~}" (nreverse lines))
                      nil)))
        (push line lines)
        (let ((combined (format nil "~{~A~%~}" (nreverse (copy-list lines)))))
          (when (input-complete-p combined)
            (return combined)))))))

(defun input-complete-p (string)
  "Check if STRING contains a complete Lisp form.
   Returns T if complete, NIL if more input needed."
  (handler-case
      (progn
        (read-from-string string)
        t)
    (end-of-file () nil)
    ;; Reader errors (e.g. unknown package) - only complete if parens are balanced
    (reader-error ()
      (zerop (count-unmatched-delimiters string)))))
