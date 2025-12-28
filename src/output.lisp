;;; output.lisp --- Output formatting for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; ANSI Colors
;;; ─────────────────────────────────────────────────────────────────────────────

(defun no-color-p ()
  "Return T if NO_COLOR environment variable is set and non-empty."
  (let ((val (uiop:getenv "NO_COLOR")))
    (and val (plusp (length val)))))

(defvar *colors-enabled* (not (no-color-p))
  "Enable/disable colored output. Defaults to T unless NO_COLOR is set.")

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

(defun colors-enabled-p ()
  "Return T if colors should be used.
   Checks both *colors-enabled* and NO_COLOR environment variable at runtime."
  (and *colors-enabled*
       (not (no-color-p))
       (terminal-capable-p)))

(defun colorize (text color)
  "Wrap TEXT with COLOR codes if colors are enabled.
   COLOR can be an ANSI escape string or a 256-color code number."
  (if (colors-enabled-p)
      (let ((color-seq (if (integerp color)
                           (format nil "~C[38;5;~Dm" #\Escape color)
                           color)))
        (format nil "~A~A~A" color-seq text *color-reset*))
      text))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Spinner
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *spinner-frames* '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Braille spinner frames.")

(defvar *spinner-index* 0
  "Current spinner frame index.")

(defun spinner-frame ()
  "Return the next spinner frame and advance the index."
  (let ((frame (nth *spinner-index* *spinner-frames*)))
    (setf *spinner-index* (mod (1+ *spinner-index*) (length *spinner-frames*)))
    frame))

(defun show-spinner (&optional message)
  "Display spinner with optional MESSAGE. Call repeatedly to animate."
  (format t "~C[2K~C[G~A ~A"
          #\Escape #\Escape
          (colorize (spinner-frame) *ansi-prompt*)
          (or message ""))
  (force-output))

(defun clear-spinner ()
  "Clear the spinner line."
  (format t "~C[2K~C[G" #\Escape #\Escape)
  (force-output))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; String Utilities
;;; ─────────────────────────────────────────────────────────────────────────────

(defun visible-string-length (string)
  "Return the visible length of STRING, ignoring ANSI escape sequences."
  (let ((len 0)
        (i 0)
        (slen (length string)))
    (loop while (< i slen) do
      (let ((char (char string i)))
        (cond
          ;; Start of escape sequence
          ((char= char #\Escape)
           ;; Skip ESC[...m sequences
           (incf i)
           (when (and (< i slen) (char= (char string i) #\[))
             (incf i)
             ;; Skip until 'm' or end of string
             (loop while (and (< i slen)
                              (not (char= (char string i) #\m)))
                   do (incf i))
             (when (< i slen) (incf i))))  ; Skip the 'm'
          (t
           (incf len)
           (incf i)))))
    len))

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

(defun format-result-string (str)
  "Colorize a string representation of a Lisp value.
   STR is the printed representation from the backend."
  (if (not (and *colors-enabled* (terminal-capable-p)))
      str
      (cond
        ;; Unreadable objects like #<PACKAGE ...>
        ((and (>= (length str) 2)
              (char= (char str 0) #\#)
              (char= (char str 1) #\<))
         (colorize str *color-dim*))
        ;; NIL
        ((string-equal str "NIL")
         (colorize str *color-nil*))
        ;; T
        ((string-equal str "T")
         (colorize str *color-t*))
        ;; Keywords
        ((and (plusp (length str))
              (char= (char str 0) #\:))
         (colorize str *color-keyword*))
        ;; Strings (already quoted)
        ((and (>= (length str) 2)
              (char= (char str 0) #\"))
         (colorize str *color-string*))
        ;; Numbers (starts with digit or sign followed by digit)
        ((and (plusp (length str))
              (or (digit-char-p (char str 0))
                  (and (>= (length str) 2)
                       (member (char str 0) '(#\+ #\-))
                       (digit-char-p (char str 1)))))
         (colorize str *color-number*))
        ;; Lists
        ((and (plusp (length str))
              (char= (char str 0) #\())
         (colorize str *color-string*))
        ;; Default
        (t (colorize str *color-string*)))))

(defun print-values (values)
  "Print evaluation results with syntax highlighting.
   VALUES is a list of strings (printed representations from backend).
   Note: Caller is responsible for ensuring we start on a fresh line."
  (let ((prefix (colorize *result-prefix* *color-prefix*)))
    (cond
      ((null values)
       (format t "~A~A~%" prefix (colorize "; No values" *color-dim*)))
      ((= 1 (length values))
       (let ((v (first values)))
         (format t "~A~A~%"
                 prefix
                 (if (stringp v)
                     (format-result-string v)
                     (format-value-colored v)))))
      (t
       ;; Multiple values
       (loop for v in values
             for i from 0
             do (format t "~A~A ~A~%"
                        prefix
                        (colorize (format nil "[~D]" i) *color-dim*)
                        (if (stringp v)
                            (format-result-string v)
                            (format-value-colored v))))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Banner
;;; ─────────────────────────────────────────────────────────────────────────────

(defun print-banner ()
  "Print ICL startup banner."
  (format t "icl ~A" +version+)
  ;; Get version from inferior Lisp with a short timeout
  ;; Some backends (like CCL) can hang on Slynk communication
  (handler-case
      #+sbcl
      (sb-ext:with-timeout 5
        (let ((impl-type (first (backend-eval-internal "(lisp-implementation-type)")))
              (impl-version (first (backend-eval-internal "(lisp-implementation-version)"))))
          (format t " (~A ~A)" impl-type impl-version)))
      #-sbcl
      (let ((impl-type (first (backend-eval-internal "(lisp-implementation-type)")))
            (impl-version (first (backend-eval-internal "(lisp-implementation-version)"))))
        (format t " (~A ~A)" impl-type impl-version))
    (error () nil)
    #+sbcl (sb-ext:timeout () nil))
  (when *paredit-mode*
    (format t " [paredit]"))
  (format t "~%by Anthony Green • https://github.com/atgreen/icl~%")
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
