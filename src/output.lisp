;;; output.lisp --- Output formatting for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; ANSI Colors
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *colors-enabled* t
  "Enable/disable colored output.")

(defvar *color-reset* (format nil "~C[0m" #\Escape))
(defvar *color-bold* (format nil "~C[1m" #\Escape))
(defvar *color-dim* (format nil "~C[2m" #\Escape))

;; Basic colors
(defvar *color-red* (format nil "~C[31m" #\Escape))
(defvar *color-green* (format nil "~C[32m" #\Escape))
(defvar *color-yellow* (format nil "~C[33m" #\Escape))
(defvar *color-blue* (format nil "~C[34m" #\Escape))
(defvar *color-magenta* (format nil "~C[35m" #\Escape))
(defvar *color-cyan* (format nil "~C[36m" #\Escape))

;; Bright colors (256-color mode for better visibility)
(defvar *color-number* (format nil "~C[38;5;33m" #\Escape))    ; Blue
(defvar *color-string* (format nil "~C[38;5;178m" #\Escape))   ; Gold/Yellow
(defvar *color-symbol* (format nil "~C[38;5;141m" #\Escape))   ; Purple
(defvar *color-keyword* (format nil "~C[38;5;37m" #\Escape))   ; Cyan
(defvar *color-nil* (format nil "~C[38;5;245m" #\Escape))      ; Gray
(defvar *color-t* (format nil "~C[38;5;40m" #\Escape))         ; Green
(defvar *color-list* (format nil "~C[38;5;252m" #\Escape))     ; Light gray
(defvar *color-error* (format nil "~C[38;5;196m" #\Escape))    ; Bright red
(defvar *color-prefix* (format nil "~C[38;5;244m" #\Escape))   ; Gray for =>

(defun colorize (text color)
  "Wrap TEXT with COLOR codes if colors are enabled."
  (if (and *colors-enabled* (terminal-capable-p))
      (format nil "~A~A~A" color text *color-reset*)
      text))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Colorized Value Formatting
;;; ─────────────────────────────────────────────────────────────────────────────

(defun format-value-colored (value)
  "Format VALUE with syntax highlighting, returning a string."
  (if (not (and *colors-enabled* (terminal-capable-p)))
      (format nil "~S" value)
      (typecase value
        (null (colorize "NIL" *color-nil*))
        ((eql t) (colorize "T" *color-t*))
        (keyword (colorize (format nil ":~A" (symbol-name value)) *color-keyword*))
        (symbol (colorize (format-symbol value) *color-symbol*))
        (string (colorize (format nil "~S" value) *color-string*))
        (character (colorize (format nil "~S" value) *color-string*))
        (integer (colorize (format nil "~D" value) *color-number*))
        (ratio (colorize (format nil "~A" value) *color-number*))
        (float (colorize (format nil "~G" value) *color-number*))
        (complex (colorize (format nil "~S" value) *color-number*))
        (cons (format-list-colored value))
        (vector (format-vector-colored value))
        (hash-table (format-hash-colored value))
        (function (colorize (format nil "~S" value) *color-cyan*))
        (otherwise (format nil "~S" value)))))

(defun format-symbol (sym)
  "Format a symbol with package prefix if needed."
  (let ((pkg (symbol-package sym)))
    (cond
      ((null pkg) (format nil "#:~A" (symbol-name sym)))
      ((eq pkg (find-package :keyword)) (format nil ":~A" (symbol-name sym)))
      ((eq pkg *icl-package*) (symbol-name sym))
      ((eq pkg (find-package :cl)) (symbol-name sym))
      (t (format nil "~A:~A" (package-name pkg) (symbol-name sym))))))

(defun format-list-colored (lst)
  "Format a list with colored elements."
  (if (> (list-length-bounded lst 20) 20)
      ;; Long list - don't colorize all elements
      (format nil "~S" lst)
      (with-output-to-string (s)
        (write-char #\( s)
        (loop for (elem . rest) on lst
              for first = t then nil
              do (unless first (write-char #\Space s))
                 (write-string (format-value-colored elem) s)
              while (listp rest))
        (write-char #\) s))))

(defun format-vector-colored (vec)
  "Format a vector with colored elements."
  (if (> (length vec) 20)
      (format nil "~S" vec)
      (with-output-to-string (s)
        (write-string "#(" s)
        (loop for elem across vec
              for i from 0
              do (unless (zerop i) (write-char #\Space s))
                 (write-string (format-value-colored elem) s))
        (write-char #\) s))))

(defun format-hash-colored (ht)
  "Format a hash table summary."
  (colorize (format nil "#<HASH-TABLE :COUNT ~D>" (hash-table-count ht))
            *color-cyan*))

(defun list-length-bounded (list max)
  "Return the length of LIST, or MAX+1 if longer than MAX."
  (loop for l on list
        for i from 0
        when (> i max) return (1+ max)
        finally (return i)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Value Printing
;;; ─────────────────────────────────────────────────────────────────────────────

(defun print-values (values)
  "Print evaluation results with syntax highlighting."
  (let ((prefix (colorize *result-prefix* *color-prefix*)))
    (cond
      ((null values)
       (format t "~&~A~A~%" prefix (colorize "; No values" *color-dim*)))
      ((= 1 (length values))
       (format t "~&~A~A~%" prefix (format-value-colored (first values))))
      (t
       ;; Multiple values
       (loop for v in values
             for i from 0
             do (format t "~&~A~A ~A~%"
                        prefix
                        (colorize (format nil "[~D]" i) *color-dim*)
                        (format-value-colored v)))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Banner
;;; ─────────────────────────────────────────────────────────────────────────────

(defun print-banner ()
  "Print ICL startup banner."
  (format t "~&icl ~A" +version+)
  (if *use-slynk*
      ;; Get version from inferior Lisp
      (handler-case
          (let ((impl-type (first (backend-eval "(lisp-implementation-type)")))
                (impl-version (first (backend-eval "(lisp-implementation-version)"))))
            (format t " (~A ~A)~%" impl-type impl-version))
        (error ()
          (format t "~%")))
      ;; Local mode
      (format t " (~A ~A)~%"
              (lisp-implementation-type)
              (lisp-implementation-version)))
  (format t "Type ,help for commands, ,quit to exit.~2%"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Formatted Output
;;; ─────────────────────────────────────────────────────────────────────────────

(defun print-divider (&optional (char #\─) (width 60))
  "Print a horizontal divider."
  (format t "~&~V,,,VA~%" width char char))

(defun print-table (headers rows)
  "Print a simple table with HEADERS and ROWS."
  (let* ((widths (loop for i below (length headers)
                       collect (max (length (nth i headers))
                                    (loop for row in rows
                                          maximize (length (princ-to-string (nth i row))))))))
    ;; Print headers
    (format t "~&")
    (loop for header in headers
          for width in widths
          do (format t "~VA  " width header))
    (format t "~%")
    ;; Print separator
    (loop for width in widths
          do (format t "~V,,,'-A  " width ""))
    (format t "~%")
    ;; Print rows
    (dolist (row rows)
      (loop for cell in row
            for width in widths
            do (format t "~VA  " width cell))
      (format t "~%"))))
