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
  (and (termp:termp)
       (not (string= (uiop:getenv "TERM") "dumb"))))

(defvar *use-multiline-editor* t
  "If T, use the built-in multi-line editor. If NIL, use linedit.")

(defun select-input-backend ()
  "Select appropriate input backend based on terminal capabilities."
  (cond
    ((not (terminal-capable-p)) :simple)
    (*use-multiline-editor* :multiline)
    (t :linedit)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Prompt Generation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun make-prompt ()
  "Generate the primary prompt string."
  (if *prompt-hook*
      (funcall *prompt-hook* *icl-package*)
      (format nil *prompt-string*
              (package-name *icl-package*))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Input Reading
;;; ─────────────────────────────────────────────────────────────────────────────

(defun read-input ()
  "Read input from user using appropriate backend.
   Returns input string or NIL on EOF."
  (case (select-input-backend)
    (:linedit (read-with-linedit))
    (:simple (read-simple))))

(defun read-simple ()
  "Simple fallback input for dumb terminals."
  (format t "~A" (make-prompt))
  (force-output)
  (let ((line (read-line *standard-input* nil nil)))
    (when line
      (string-trim '(#\Space #\Tab) line))))

(defun read-with-linedit ()
  "Read input using linedit with line editing."
  (handler-case
      (let ((input (linedit:linedit
                    :prompt (make-prompt)
                    :history (namestring *history-file*))))
        (when input
          (string-trim '(#\Space #\Tab) input)))
    ;; Handle EOF (Ctrl-D)
    (end-of-file ()
      nil)
    ;; Handle linedit errors (e.g., no terminal) by falling back to simple
    (error (e)
      (declare (ignore e))
      (read-simple))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Multi-line Input
;;; ─────────────────────────────────────────────────────────────────────────────

(defun make-continuation-prompt ()
  "Generate continuation prompt aligned with primary prompt."
  (let* ((primary (make-prompt))
         (width (length primary)))
    ;; Create aligned continuation: dots followed by space
    (concatenate 'string
                 (make-string (max 0 (- width 2)) :initial-element #\.)
                 ". ")))

(defun read-complete-input ()
  "Read a complete Lisp form with appropriate backend.
   Returns the complete input string or NIL on EOF."
  (let ((backend (select-input-backend)))
    (case backend
      (:multiline
       (read-with-multiline-editor))
      (:linedit
       (read-with-linedit-continuation))
      (:simple
       (read-with-simple-continuation)))))

(defun read-with-multiline-editor ()
  "Read using the built-in multi-line editor.
   Falls back to linedit-continuation if multiline editor fails to initialize."
  (let ((result (multiline-edit
                 :prompt (make-prompt)
                 :continuation-prompt (make-continuation-prompt))))
    (cond
      ((eq result :cancel)
       ;; User hit Ctrl-C, return empty to trigger abort restart
       "")
      ((eq result :not-a-tty)
       ;; Not a real terminal, fall back to linedit or simple
       (if (terminal-capable-p)
           (read-with-linedit-continuation)
           (read-with-simple-continuation)))
      ((null result)
       ;; EOF
       nil)
      (t result))))

(defun read-with-linedit-continuation ()
  "Read using linedit with continuation prompts for incomplete forms."
  (let ((lines '())
        (cont-prompt nil))
    (loop
      (let ((line (if lines
                      (progn
                        (unless cont-prompt
                          (setf cont-prompt (make-continuation-prompt)))
                        (format t "~A" cont-prompt)
                        (force-output)
                        (read-line *standard-input* nil nil))
                      (read-with-linedit))))
        (unless line
          (return (if lines
                      (format nil "~{~A~%~}" (nreverse lines))
                      nil)))
        (push line lines)
        (let ((combined (format nil "~{~A~%~}" (nreverse (copy-list lines)))))
          (when (input-complete-p combined)
            (return combined)))))))

(defun read-with-simple-continuation ()
  "Read using simple input with continuation prompts."
  (let ((lines '())
        (cont-prompt nil))
    (loop
      (let ((line (if lines
                      (progn
                        (unless cont-prompt
                          (setf cont-prompt (make-continuation-prompt)))
                        (format t "~A" cont-prompt)
                        (force-output)
                        (read-line *standard-input* nil nil))
                      (read-simple))))
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
    (reader-error () t)))  ; Syntax errors count as "complete" (will error on eval)
