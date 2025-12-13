;;; terminal.lisp --- Terminal handling for multi-line editing
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Terminal State
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *saved-termios* nil
  "Saved terminal settings for restoration.")

(defvar *terminal-raw-p* nil
  "T when terminal is in raw mode.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Raw Mode (using CFFI like linedit)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun enter-raw-mode ()
  "Put terminal into raw mode for character-by-character input."
  (when *terminal-raw-p*
    (return-from enter-raw-mode t))
  (when (zerop (osicat-posix:isatty 0))
    (return-from enter-raw-mode nil))
  ;; Save current terminal state
  (setf *saved-termios* (cffi:foreign-alloc '(:struct osicat-posix:termios)))
  (when (minusp (osicat-posix:tcgetattr 0 *saved-termios*))
    (cffi:foreign-free *saved-termios*)
    (setf *saved-termios* nil)
    (return-from enter-raw-mode nil))
  ;; Set up raw mode
  (cffi:with-foreign-object (tmp '(:struct osicat-posix:termios))
    (when (minusp (osicat-posix:tcgetattr 0 tmp))
      (cffi:foreign-free *saved-termios*)
      (setf *saved-termios* nil)
      (return-from enter-raw-mode nil))
    ;; Use cfmakeraw to set up raw mode
    (cffi:foreign-funcall "cfmakeraw" :pointer tmp :void)
    ;; Keep OPOST so output processing works (newlines expand to CR LF)
    ;; OPOST = 1 on Linux
    (cffi:with-foreign-slots ((osicat-posix:oflag) tmp (:struct osicat-posix:termios))
      (setf osicat-posix:oflag (logior osicat-posix:oflag 1)))
    (when (minusp (osicat-posix:tcsetattr 0 osicat-posix:tcsaflush tmp))
      (cffi:foreign-free *saved-termios*)
      (setf *saved-termios* nil)
      (return-from enter-raw-mode nil)))
  (setf *terminal-raw-p* t)
  t)

(defun exit-raw-mode ()
  "Restore terminal to original settings."
  (when (and *terminal-raw-p* *saved-termios*)
    (osicat-posix:tcsetattr 0 osicat-posix:tcsanow *saved-termios*)
    (cffi:foreign-free *saved-termios*)
    (setf *saved-termios* nil
          *terminal-raw-p* nil))
  t)

(defmacro with-raw-mode (&body body)
  "Execute BODY with terminal in raw mode, ensuring cleanup."
  `(let ((entered (enter-raw-mode)))
     (unwind-protect
          (when entered
            ,@body)
       (when entered
         (exit-raw-mode)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; ANSI Escape Codes
;;; ─────────────────────────────────────────────────────────────────────────────

(defconstant +esc+ #\Escape)

(defun cursor-up (&optional (n 1))
  "Move cursor up N lines."
  (when (plusp n)
    (format t "~C[~DA" +esc+ n))
  (force-output))

(defun cursor-down (&optional (n 1))
  "Move cursor down N lines."
  (when (plusp n)
    (format t "~C[~DB" +esc+ n))
  (force-output))

(defun cursor-forward (&optional (n 1))
  "Move cursor forward N columns."
  (when (plusp n)
    (format t "~C[~DC" +esc+ n))
  (force-output))

(defun cursor-backward (&optional (n 1))
  "Move cursor backward N columns."
  (when (plusp n)
    (format t "~C[~DD" +esc+ n))
  (force-output))

(defun cursor-to-column (col)
  "Move cursor to column COL (1-based)."
  (format t "~C[~DG" +esc+ col)
  (force-output))

(defun cursor-position (row col)
  "Move cursor to ROW, COL (1-based)."
  (format t "~C[~D;~DH" +esc+ row col)
  (force-output))

(defun save-cursor ()
  "Save cursor position."
  (format t "~C[s" +esc+)
  (force-output))

(defun restore-cursor ()
  "Restore cursor position."
  (format t "~C[u" +esc+)
  (force-output))

(defun clear-line ()
  "Clear from cursor to end of line."
  (format t "~C[K" +esc+)
  (force-output))

(defun clear-line-full ()
  "Clear entire line."
  (format t "~C[2K" +esc+)
  (force-output))

(defun clear-below ()
  "Clear from cursor to end of screen."
  (format t "~C[J" +esc+)
  (force-output))

(defun clear-screen-full ()
  "Clear entire screen."
  (format t "~C[2J" +esc+)
  (force-output))

(defun get-terminal-size ()
  "Get terminal size as (values columns rows)."
  (handler-case
      (cffi:with-foreign-object (size '(:struct osicat-posix:winsize))
        (if (zerop (osicat-posix:ioctl 0 osicat-posix:tiocgwinsz size))
            (values (cffi:foreign-slot-value size '(:struct osicat-posix:winsize)
                                             'osicat-posix:col)
                    (cffi:foreign-slot-value size '(:struct osicat-posix:winsize)
                                             'osicat-posix:row))
            (values 80 24)))
    (error ()
      (values 80 24))))

(defun get-cursor-position ()
  "Get current cursor position as (values row col), 1-based.
   Returns (values 1 1) if unable to determine."
  (handler-case
      (progn
        ;; Send cursor position query
        (format t "~C[6n" +esc+)
        (force-output)
        ;; Read response: ESC [ row ; col R
        (let ((c (read-char-raw)))
          (unless (and c (char= c +esc+))
            (return-from get-cursor-position (values 1 1)))
          (setf c (read-char-raw))
          (unless (and c (char= c #\[))
            (return-from get-cursor-position (values 1 1)))
          ;; Read row number
          (let ((row 0) (col 0))
            (setf c (read-char-raw))
            (loop while (and c (digit-char-p c))
                  do (setf row (+ (* row 10) (- (char-code c) (char-code #\0))))
                     (setf c (read-char-raw)))
            ;; c should now be semicolon
            (unless (and c (char= c #\;))
              (return-from get-cursor-position (values 1 1)))
            ;; Read col number
            (setf c (read-char-raw))
            (loop while (and c (digit-char-p c))
                  do (setf col (+ (* col 10) (- (char-code c) (char-code #\0))))
                     (setf c (read-char-raw)))
            ;; c should now be R (consumed)
            (values (max 1 row) (max 1 col)))))
    (error ()
      (values 1 1))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Key Reading
;;; ─────────────────────────────────────────────────────────────────────────────

(defun read-char-raw ()
  "Read a single character in raw mode."
  (read-char *standard-input* nil nil))

(defun char-available-p ()
  "Check if a character is available without blocking."
  (listen *standard-input*))

(defun read-key ()
  "Read a key, handling escape sequences.
   Returns a keyword for special keys or a character for regular input."
  (let ((c (read-char-raw)))
    (unless c
      (return-from read-key :eof))
    (cond
      ;; Escape sequence
      ((char= c +esc+)
       (if (char-available-p)
           (parse-escape-sequence)
           :escape))
      ;; Control characters
      ((char= c #\Rubout) :backspace)      ; DEL (127)
      ((char= c (code-char 8)) :backspace) ; Backspace (8)
      ((char= c (code-char 13)) :enter)    ; CR
      ((char= c (code-char 10)) :enter)    ; LF
      ((char= c (code-char 4)) :eof)       ; Ctrl-D
      ((char= c (code-char 1)) :home)      ; Ctrl-A
      ((char= c (code-char 5)) :end)       ; Ctrl-E
      ((char= c (code-char 11)) :kill-line) ; Ctrl-K
      ((char= c (code-char 21)) :clear-line) ; Ctrl-U
      ((char= c (code-char 12)) :clear-screen) ; Ctrl-L
      ((char= c (code-char 3)) :interrupt) ; Ctrl-C
      ((char= c #\Tab) :tab)
      ;; Regular character
      (t c))))

(defun parse-escape-sequence ()
  "Parse an escape sequence after ESC has been read."
  (let ((c (read-char-raw)))
    (unless c
      (return-from parse-escape-sequence :escape))
    (cond
      ;; CSI sequences (ESC [)
      ((char= c #\[)
       (parse-csi-sequence))
      ;; Alt+key (ESC followed by key)
      (t
       (cons :alt c)))))

(defun parse-csi-sequence ()
  "Parse a CSI sequence after ESC [ has been read."
  (let ((c (read-char-raw)))
    (unless c
      (return-from parse-csi-sequence :unknown))
    (cond
      ;; Arrow keys
      ((char= c #\A) :up)
      ((char= c #\B) :down)
      ((char= c #\C) :right)
      ((char= c #\D) :left)
      ;; Home/End
      ((char= c #\H) :home)
      ((char= c #\F) :end)
      ;; Extended sequences (e.g., ESC[1~ for Home)
      ((digit-char-p c)
       (parse-extended-csi c))
      (t :unknown))))

(defun parse-extended-csi (first-digit)
  "Parse extended CSI sequence starting with a digit."
  (let ((num (- (char-code first-digit) (char-code #\0))))
    ;; Read remaining digits and tilde
    (loop for c = (read-char-raw)
          while (and c (digit-char-p c))
          do (setf num (+ (* num 10) (- (char-code c) (char-code #\0))))
          finally
             (case num
               (1 (return :home))
               (2 (return :insert))
               (3 (return :delete))
               (4 (return :end))
               (5 (return :page-up))
               (6 (return :page-down))
               (otherwise (return :unknown))))))
