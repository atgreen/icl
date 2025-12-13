;;; indent.lisp --- Lisp-aware indentation for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Indentation Constants
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *indent-width* 2
  "Default indentation width in spaces.")

(defparameter *special-form-indent*
  '((defun . 2)
    (defmacro . 2)
    (defmethod . 2)
    (defgeneric . 2)
    (defclass . 2)
    (defstruct . 2)
    (defvar . 2)
    (defparameter . 2)
    (defconstant . 2)
    (deftype . 2)
    (defpackage . 1)
    (define-condition . 2)
    (lambda . 1)
    (let . 1)
    (let* . 1)
    (flet . 1)
    (labels . 1)
    (macrolet . 1)
    (symbol-macrolet . 1)
    (if . 1)
    (when . 1)
    (unless . 1)
    (cond . 0)
    (case . 1)
    (ecase . 1)
    (typecase . 1)
    (etypecase . 1)
    (handler-case . 1)
    (handler-bind . 1)
    (restart-case . 1)
    (restart-bind . 1)
    (unwind-protect . 1)
    (catch . 1)
    (throw . 1)
    (block . 1)
    (return-from . 1)
    (tagbody . 0)
    (prog . 1)
    (prog* . 1)
    (progn . 0)
    (progv . 2)
    (prog1 . 1)
    (prog2 . 2)
    (multiple-value-bind . 2)
    (multiple-value-prog1 . 1)
    (destructuring-bind . 2)
    (do . 2)
    (do* . 2)
    (dolist . 1)
    (dotimes . 1)
    (loop . 0)
    (with-open-file . 1)
    (with-open-stream . 1)
    (with-input-from-string . 1)
    (with-output-to-string . 1)
    (with-slots . 2)
    (with-accessors . 2)
    (with-standard-io-syntax . 0)
    (print-unreadable-object . 1)
    (eval-when . 1))
  "Alist of special forms and their body-start argument index.
   Body args get indented by *indent-width*, earlier args align to first arg.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Parsing Utilities
;;; ─────────────────────────────────────────────────────────────────────────────

(defun skip-whitespace (string start)
  "Skip whitespace starting at START in STRING. Return new position."
  (loop for i from start below (length string)
        while (member (char string i) '(#\Space #\Tab))
        finally (return i)))

(defun skip-to-end-of-line (string start)
  "Skip to end of line or string from START. Return new position."
  (loop for i from start below (length string)
        until (char= (char string i) #\Newline)
        finally (return i)))

(defun find-string-end (string start)
  "Find end of string literal starting after opening quote at START."
  (loop for i from start below (length string)
        do (let ((c (char string i)))
             (cond
               ((char= c #\\)
                (incf i))  ; Skip escaped char
               ((char= c #\")
                (return (1+ i)))))
        finally (return (length string))))

(defun find-matching-close (string start open-char close-char)
  "Find matching close paren/bracket starting after OPEN-CHAR at START."
  (let ((depth 1))
    (loop for i from start below (length string)
          do (let ((c (char string i)))
               (cond
                 ((char= c open-char) (incf depth))
                 ((char= c close-char)
                  (decf depth)
                  (when (zerop depth)
                    (return (1+ i))))
                 ((char= c #\")
                  (setf i (1- (find-string-end string (1+ i)))))
                 ((char= c #\;)
                  (setf i (1- (skip-to-end-of-line string i))))))
          finally (return (length string)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Token Extraction
;;; ─────────────────────────────────────────────────────────────────────────────

(defun extract-first-symbol (string start)
  "Extract first symbol from STRING starting at START.
   Returns (values symbol-string end-position) or (values nil start) if no symbol."
  (let ((pos (skip-whitespace string start)))
    (when (>= pos (length string))
      (return-from extract-first-symbol (values nil pos)))
    (let ((c (char string pos)))
      ;; Skip reader macros like #', #., etc.
      (when (char= c #\#)
        (incf pos)
        (when (>= pos (length string))
          (return-from extract-first-symbol (values nil pos)))
        (setf c (char string pos))
        (when (member c '(#\' #\. #\+ #\- #\p #\P))
          (incf pos)
          (when (>= pos (length string))
            (return-from extract-first-symbol (values nil pos)))
          (setf c (char string pos))))
      ;; Skip quote, backquote, comma
      (when (member c '(#\' #\` #\,))
        (incf pos)
        (when (and (< pos (length string)) (char= (char string pos) #\@))
          (incf pos))
        (when (>= pos (length string))
          (return-from extract-first-symbol (values nil pos)))
        (setf c (char string pos)))
      ;; Skip opening paren
      (when (char= c #\()
        (incf pos)
        (setf pos (skip-whitespace string pos))
        (when (>= pos (length string))
          (return-from extract-first-symbol (values nil pos)))
        (setf c (char string pos)))
      ;; Now extract the symbol
      (if (or (char= c #\() (char= c #\)) (char= c #\") (char= c #\;))
          (values nil pos)
          (let ((end pos))
            (loop while (and (< end (length string))
                             (not (member (char string end)
                                          '(#\Space #\Tab #\Newline #\( #\) #\" #\;))))
                  do (incf end))
            (values (subseq string pos end) end))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Indentation Calculation
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct paren-info
  "Information about an open parenthesis."
  (column 0 :type fixnum)         ; Column of the open paren (0-based)
  (first-arg-col nil)             ; Column of first argument (if any)
  (symbol nil :type (or null string))  ; First symbol after paren
  (arg-count 0 :type fixnum))     ; Number of args seen

(defun calculate-indent (text)
  "Calculate indentation for the next line after TEXT.
   Returns number of spaces to indent."
  (let ((stack '())               ; Stack of paren-info
        (col 0)                   ; Current column
        (i 0)                     ; Current position
        (len (length text)))
    (loop while (< i len)
          do (let ((c (char text i)))
               (cond
                 ;; String - skip to end
                 ((char= c #\")
                  (let ((end (find-string-end text (1+ i))))
                    (incf col (- end i))
                    (setf i end)))
                 ;; Comment - skip to end of line
                 ((char= c #\;)
                  (let ((end (skip-to-end-of-line text i)))
                    (setf i end)))
                 ;; Newline
                 ((char= c #\Newline)
                  (setf col 0)
                  (incf i))
                 ;; Open paren/bracket
                 ((member c '(#\( #\[ #\{))
                  (push (make-paren-info :column col) stack)
                  (incf col)
                  (incf i)
                  ;; Look ahead for first symbol
                  (let ((ws-end (skip-whitespace text i)))
                    (when (< ws-end len)
                      (multiple-value-bind (sym sym-end) (extract-first-symbol text i)
                        (declare (ignore sym-end))
                        (when sym
                          (setf (paren-info-symbol (first stack)) sym)
                          (setf (paren-info-first-arg-col (first stack))
                                (+ col (- ws-end i))))))))
                 ;; Close paren/bracket
                 ((member c '(#\) #\] #\}))
                  (pop stack)
                  (incf col)
                  (incf i))
                 ;; Regular char - track if we're starting an arg
                 ((member c '(#\Space #\Tab))
                  (when (and stack (paren-info-symbol (first stack)))
                    ;; After whitespace, next non-ws starts a new arg
                    (let ((next (skip-whitespace text i)))
                      (when (and (< next len)
                                 (not (member (char text next) '(#\) #\] #\} #\Newline))))
                        (incf (paren-info-arg-count (first stack))))))
                  (incf col)
                  (incf i))
                 ;; Other characters
                 (t
                  (incf col)
                  (incf i)))))
    ;; Calculate indent based on stack state
    (if (null stack)
        0
        (let* ((top (first stack))
               (sym-str (paren-info-symbol top))
               (sym (when sym-str
                      (ignore-errors
                        (intern (string-upcase sym-str) :cl))))
               (special (assoc sym *special-form-indent*))
               (body-start (if special (cdr special) nil)))
          (cond
            ;; Special form with body
            ((and body-start (>= (paren-info-arg-count top) body-start))
             (+ (paren-info-column top) *indent-width*))
            ;; Has first arg column - align to it
            ((paren-info-first-arg-col top)
             (+ (paren-info-column top) 1 (paren-info-first-arg-col top)))
            ;; Just after open paren - standard indent
            (t
             (+ (paren-info-column top) *indent-width*)))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Form Reindentation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun reindent-string (text)
  "Reindent a multi-line Lisp form in TEXT.
   Returns the reindented string."
  (let* ((lines (split-string-by-newline text))
         (result-lines '()))
    ;; First line keeps its indentation (it's where user started typing)
    (push (first lines) result-lines)
    ;; Reindent subsequent lines
    (loop for i from 1 below (length lines)
          for line = (nth i lines)
          for prefix-text = (format nil "~{~A~%~}" (reverse result-lines))
          for indent = (calculate-indent prefix-text)
          for trimmed = (string-left-trim '(#\Space #\Tab) line)
          do (push (concatenate 'string
                                (make-string indent :initial-element #\Space)
                                trimmed)
                   result-lines))
    (format nil "~{~A~^~%~}" (reverse result-lines))))

(defun make-indent-string (n)
  "Create a string of N spaces."
  (make-string n :initial-element #\Space))
