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

;; Basic colors (always available, used for simple output)
(defvar *color-red* (format nil "~C[31m" #\Escape))
(defvar *color-green* (format nil "~C[32m" #\Escape))
(defvar *color-yellow* (format nil "~C[33m" #\Escape))
(defvar *color-blue* (format nil "~C[34m" #\Escape))
(defvar *color-magenta* (format nil "~C[35m" #\Escape))
(defvar *color-cyan* (format nil "~C[36m" #\Escape))

;; Value formatting colors — resolved from theme by refresh-value-colors
(defvar *color-number* nil)    ; Numbers
(defvar *color-string* nil)    ; Strings
(defvar *color-symbol* nil)    ; Symbols
(defvar *color-keyword* nil)   ; Keywords
(defvar *color-nil* nil)       ; NIL
(defvar *color-t* nil)         ; T
(defvar *color-list* nil)      ; Lists
(defvar *color-prefix* nil)    ; Result prefix (=>)

;; Default value colors (used before theme is loaded and for non-theme elements)
(defvar *default-nil-color* (tuition:make-complete-color :truecolor "#8A8A8A"))
(defvar *default-t-color* (tuition:make-complete-color :truecolor "#00D700"))
(defvar *default-list-color* (tuition:make-complete-color :truecolor "#D0D0D0"))
(defvar *default-prefix-color* (tuition:make-complete-color :truecolor "#808080"))

(defun refresh-value-colors ()
  "Regenerate value formatting colors from current theme.
   Called by refresh-ansi-codes after theme application."
  (setf *color-number* (if *hl-number-color*
                            (tuition:resolve-color-foreground *hl-number-color*)
                            (tuition:resolve-color-foreground *default-nil-color*))
        *color-string* (if *hl-string-color*
                            (tuition:resolve-color-foreground *hl-string-color*)
                            (tuition:resolve-color-foreground *default-nil-color*))
        *color-symbol* (if *hl-special-color*
                            (tuition:resolve-color-foreground *hl-special-color*)
                            (tuition:resolve-color-foreground *default-nil-color*))
        *color-keyword* (if *hl-keyword-color*
                             (tuition:resolve-color-foreground *hl-keyword-color*)
                             (tuition:resolve-color-foreground *default-nil-color*))
        *color-nil* (tuition:resolve-color-foreground *default-nil-color*)
        *color-t* (tuition:resolve-color-foreground *default-t-color*)
        *color-list* (tuition:resolve-color-foreground *default-list-color*)
        *color-prefix* (tuition:resolve-color-foreground *default-prefix-color*)))

(defun colors-enabled-p ()
  "Return T if colors should be used.
   Checks both *colors-enabled* and NO_COLOR environment variable at runtime."
  (and *colors-enabled*
       (not (no-color-p))
       (terminal-capable-p)))

(defun colorize (text color)
  "Wrap TEXT with COLOR codes if colors are enabled.
   COLOR can be a tuition:complete-color, hex string (#RRGGBB),
   ANSI escape string, or a 256-color code integer."
  (if (colors-enabled-p)
      (let ((color-seq (cond
                         ((typep color 'tuition:complete-color)
                          (tuition:resolve-color-foreground color))
                         ((and (stringp color)
                               (plusp (length color))
                               (char= (char color 0) #\#))
                          (tuition:resolve-color-foreground
                           (tuition:make-complete-color :truecolor color)))
                         ((integerp color)
                          (format nil "~C[38;5;~Dm" #\Escape color))
                         ((stringp color) color)
                         (t nil))))
        (if color-seq
            (format nil "~A~A~A" color-seq text *color-reset*)
            text))
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
