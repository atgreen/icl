;;; terminal-windows.lisp --- Windows terminal handling for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; This file provides Windows-specific terminal handling using the
;;; Windows Console API via CFFI. It implements the same interface as
;;; terminal.lisp (which uses POSIX APIs).

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Windows Console API Constants
;;; ─────────────────────────────────────────────────────────────────────────────

(defconstant +std-input-handle+ -10)
(defconstant +std-output-handle+ -11)

;; Input mode flags
(defconstant +enable-processed-input+ #x0001)
(defconstant +enable-line-input+ #x0002)
(defconstant +enable-echo-input+ #x0004)
(defconstant +enable-window-input+ #x0008)
(defconstant +enable-mouse-input+ #x0010)
(defconstant +enable-insert-mode+ #x0020)
(defconstant +enable-quick-edit-mode+ #x0040)
(defconstant +enable-virtual-terminal-input+ #x0200)

;; Output mode flags
(defconstant +enable-processed-output+ #x0001)
(defconstant +enable-wrap-at-eol-output+ #x0002)
(defconstant +enable-virtual-terminal-processing+ #x0004)
(defconstant +disable-newline-auto-return+ #x0008)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Windows Console API CFFI Definitions
;;; ─────────────────────────────────────────────────────────────────────────────

(cffi:defctype handle :pointer)
(cffi:defctype dword :uint32)
(cffi:defctype bool :int)

(cffi:defcstruct coord
  (x :short)
  (y :short))

(cffi:defcstruct small-rect
  (left :short)
  (top :short)
  (right :short)
  (bottom :short))

(cffi:defcstruct console-screen-buffer-info
  (size (:struct coord))
  (cursor-position (:struct coord))
  (attributes :uint16)
  (window (:struct small-rect))
  (maximum-window-size (:struct coord)))

(cffi:defcfun ("GetStdHandle" get-std-handle) handle
  (std-handle dword))

(cffi:defcfun ("GetConsoleMode" get-console-mode) bool
  (handle handle)
  (mode (:pointer dword)))

(cffi:defcfun ("SetConsoleMode" set-console-mode) bool
  (handle handle)
  (mode dword))

(cffi:defcfun ("GetConsoleScreenBufferInfo" get-console-screen-buffer-info) bool
  (handle handle)
  (info (:pointer (:struct console-screen-buffer-info))))

(cffi:defcfun ("GetFileType" get-file-type) dword
  (handle handle))

(defconstant +file-type-char+ 2)  ; Character device (console)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Terminal State
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *saved-input-mode* nil
  "Saved console input mode for restoration.")

(defvar *saved-output-mode* nil
  "Saved console output mode for restoration.")

(defvar *terminal-raw-p* nil
  "T when terminal is in raw mode.")

(defvar *console-input-handle* nil
  "Cached console input handle.")

(defvar *console-output-handle* nil
  "Cached console output handle.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Console Handle Management
;;; ─────────────────────────────────────────────────────────────────────────────

(defun get-console-handles ()
  "Get and cache console handles. Returns T if successful."
  (unless *console-input-handle*
    (setf *console-input-handle* (get-std-handle +std-input-handle+)))
  (unless *console-output-handle*
    (setf *console-output-handle* (get-std-handle +std-output-handle+)))
  (and *console-input-handle*
       *console-output-handle*
       (not (cffi:null-pointer-p *console-input-handle*))
       (not (cffi:null-pointer-p *console-output-handle*))))

(defun console-p ()
  "Check if stdin is a console (not redirected)."
  (get-console-handles)
  (and *console-input-handle*
       (= (get-file-type *console-input-handle*) +file-type-char+)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Raw Mode
;;; ─────────────────────────────────────────────────────────────────────────────

(defun enter-raw-mode ()
  "Put console into raw mode for character-by-character input.
   Enables virtual terminal processing for ANSI escape sequences."
  (when *browser-terminal-active*
    (setf *terminal-raw-p* t)
    ;; Enable bracketed paste mode for xterm.js
    (format t "~C[?2004h" +esc+)
    (force-output)
    (return-from enter-raw-mode t))
  (when *terminal-raw-p*
    (return-from enter-raw-mode t))
  (unless (console-p)
    (return-from enter-raw-mode nil))
  (handler-case
      (progn
        ;; Save current modes
        (cffi:with-foreign-object (mode 'dword)
          (unless (plusp (get-console-mode *console-input-handle* mode))
            (return-from enter-raw-mode nil))
          (setf *saved-input-mode* (cffi:mem-ref mode 'dword)))
        (cffi:with-foreign-object (mode 'dword)
          (unless (plusp (get-console-mode *console-output-handle* mode))
            (return-from enter-raw-mode nil))
          (setf *saved-output-mode* (cffi:mem-ref mode 'dword)))
        ;; Set raw input mode:
        ;; - Disable line input (get chars immediately)
        ;; - Disable echo
        ;; - Enable virtual terminal input (for escape sequences)
        (let ((new-input-mode (logior +enable-virtual-terminal-input+
                                      (logand *saved-input-mode*
                                              (lognot (logior +enable-line-input+
                                                              +enable-echo-input+))))))
          (unless (plusp (set-console-mode *console-input-handle* new-input-mode))
            (return-from enter-raw-mode nil)))
        ;; Enable virtual terminal processing on output (for ANSI colors)
        (let ((new-output-mode (logior *saved-output-mode*
                                       +enable-virtual-terminal-processing+)))
          (set-console-mode *console-output-handle* new-output-mode))
        (setf *terminal-raw-p* t)
        t)
    (error ()
      nil)))

(defun exit-raw-mode ()
  "Restore console to original settings."
  (when *browser-terminal-active*
    (when *terminal-raw-p*
      (format t "~C[?2004l" +esc+)
      (force-output))
    (setf *terminal-raw-p* nil)
    (return-from exit-raw-mode t))
  (when (and *terminal-raw-p* *saved-input-mode*)
    (handler-case
        (progn
          (set-console-mode *console-input-handle* *saved-input-mode*)
          (when *saved-output-mode*
            (set-console-mode *console-output-handle* *saved-output-mode*)))
      (error () nil))
    (setf *saved-input-mode* nil
          *saved-output-mode* nil
          *terminal-raw-p* nil))
  t)

(defmacro with-raw-mode (&body body)
  "Execute BODY with console in raw mode, ensuring cleanup."
  `(let ((entered (enter-raw-mode)))
     (unwind-protect
          (when entered
            ,@body)
       (when entered
         (exit-raw-mode)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Process Suspend/Resume (Ctrl-Z) - Not supported on Windows
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *suspend-hook* nil
  "Function to call before suspending (not used on Windows).")

(defvar *resume-hook* nil
  "Function to call after resuming (not used on Windows).")

(defun suspend-process ()
  "Suspend not supported on Windows - just beep."
  (format t "~C" (code-char 7))  ; Bell
  (force-output)
  nil)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; ANSI Escape Codes (same as POSIX version - Windows 10+ supports these)
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

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Terminal Size
;;; ─────────────────────────────────────────────────────────────────────────────

(defun get-terminal-size ()
  "Get terminal size as (values columns rows)."
  (handler-case
      (progn
        (get-console-handles)
        (cffi:with-foreign-object (info '(:struct console-screen-buffer-info))
          (if (and *console-output-handle*
                   (plusp (get-console-screen-buffer-info *console-output-handle* info)))
              (let ((window-left (cffi:foreign-slot-value
                                  (cffi:foreign-slot-pointer info '(:struct console-screen-buffer-info) 'window)
                                  '(:struct small-rect) 'left))
                    (window-right (cffi:foreign-slot-value
                                   (cffi:foreign-slot-pointer info '(:struct console-screen-buffer-info) 'window)
                                   '(:struct small-rect) 'right))
                    (window-top (cffi:foreign-slot-value
                                 (cffi:foreign-slot-pointer info '(:struct console-screen-buffer-info) 'window)
                                 '(:struct small-rect) 'top))
                    (window-bottom (cffi:foreign-slot-value
                                    (cffi:foreign-slot-pointer info '(:struct console-screen-buffer-info) 'window)
                                    '(:struct small-rect) 'bottom)))
                (values (1+ (- window-right window-left))
                        (1+ (- window-bottom window-top))))
              (values 80 24))))
    (error ()
      (values 80 24))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Cursor Position Query (using ANSI - works on Windows 10+)
;;; ─────────────────────────────────────────────────────────────────────────────

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
      ((char= c (code-char 4)) :ctrl-d)    ; Ctrl-D (delete char or EOF)
      ((char= c (code-char 1)) :home)      ; Ctrl-A
      ((char= c (code-char 2)) :left)      ; Ctrl-B (backward)
      ((char= c (code-char 5)) :end)       ; Ctrl-E
      ((char= c (code-char 6)) :right)     ; Ctrl-F (forward)
      ((char= c (code-char 11)) :kill-line) ; Ctrl-K
      ((char= c (code-char 14)) :ctrl-n)   ; Ctrl-N (next line)
      ((char= c (code-char 15)) :open-line) ; Ctrl-O (open line)
      ((char= c (code-char 16)) :ctrl-p)   ; Ctrl-P (previous line)
      ((char= c (code-char 20)) :transpose) ; Ctrl-T (transpose chars)
      ((char= c (code-char 21)) :clear-line) ; Ctrl-U
      ((char= c (code-char 12)) :clear-screen) ; Ctrl-L
      ((char= c (code-char 3)) :interrupt) ; Ctrl-C
      ((char= c (code-char 26)) :suspend)  ; Ctrl-Z
      ((char= c (code-char 18)) :reverse-search) ; Ctrl-R
      ((char= c (code-char 7)) :cancel-search)  ; Ctrl-G
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
      ;; Extended sequences
      ((digit-char-p c)
       (parse-extended-csi c))
      (t :unknown))))

(defun parse-extended-csi (first-digit)
  "Parse extended CSI sequence starting with a digit."
  (let ((num1 (- (char-code first-digit) (char-code #\0)))
        (num2 nil)
        (c nil))
    ;; Read first number
    (loop do (setf c (read-char-raw))
          while (and c (digit-char-p c))
          do (setf num1 (+ (* num1 10) (- (char-code c) (char-code #\0)))))
    (unless c
      (return-from parse-extended-csi :unknown))
    ;; If semicolon, read modifier
    (when (char= c #\;)
      (setf num2 0)
      (loop do (setf c (read-char-raw))
            while (and c (digit-char-p c))
            do (setf num2 (+ (* num2 10) (- (char-code c) (char-code #\0))))))
    (unless c
      (return-from parse-extended-csi :unknown))
    ;; Handle based on terminator
    (cond
      ;; Kitty keyboard protocol
      ((char= c #\u)
       (if (and (= num1 13) num2 (= (logand num2 1) 1))
           :shift-enter
           :unknown))
      ;; Modified arrow keys
      ((and (= num1 1) num2)
       (cond
         ((char= c #\A) :up)
         ((char= c #\B) :down)
         ((char= c #\C) :right)
         ((char= c #\D) :left)
         (t :unknown)))
      ;; Function key sequences
      ((char= c #\~)
       (case num1
         (1 :home)
         (2 :insert)
         (3 :delete)
         (4 :end)
         (5 :page-up)
         (6 :page-down)
         (otherwise :unknown)))
      (t :unknown))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Terminal Color Detection
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *terminal-background* nil
  "Cached terminal background: :dark, :light, or nil if unknown.")

(defun query-terminal-background ()
  "Query terminal for its background color.
   Windows Terminal supports OSC 11 query, older consoles don't.
   Returns (values r g b) as integers 0-65535, or NIL if query fails."
  (handler-case
      (with-raw-mode
        ;; Send OSC 11 query
        (format t "~C]11;?~C" +esc+ (code-char 7))
        (force-output)
        ;; Give terminal time to respond
        (sleep 0.05)
        (unless (char-available-p)
          (return-from query-terminal-background nil))
        ;; Read response
        (let ((c (read-char-raw)))
          (unless (and c (char= c +esc+))
            (return-from query-terminal-background nil))
          (setf c (read-char-raw))
          (unless (and c (char= c #\]))
            (return-from query-terminal-background nil))
          ;; Skip "11;"
          (dotimes (i 3) (read-char-raw))
          ;; Read until terminator
          (let ((buf (make-array 64 :element-type 'character :fill-pointer 0)))
            (loop for ch = (read-char-raw)
                  while (and ch
                             (not (char= ch +esc+))
                             (not (char= ch (code-char 7)))
                             (< (length buf) 64))
                  do (vector-push ch buf))
            ;; Parse rgb:RRRR/GGGG/BBBB format
            (let ((str (coerce buf 'string)))
              (when (and (>= (length str) 4)
                         (string-equal (subseq str 0 4) "rgb:"))
                (let* ((rgb-part (subseq str 4))
                       (parts (split-sequence:split-sequence #\/ rgb-part)))
                  (when (= (length parts) 3)
                    (handler-case
                        (values (parse-integer (first parts) :radix 16)
                                (parse-integer (second parts) :radix 16)
                                (parse-integer (third parts) :radix 16))
                      (error () nil)))))))))
    (error () nil)))

(defun compute-luminance (r g b)
  "Compute relative luminance from RGB values (0-65535 range)."
  (let ((r-norm (/ r 65535.0))
        (g-norm (/ g 65535.0))
        (b-norm (/ b 65535.0)))
    (+ (* 0.2126 r-norm)
       (* 0.7152 g-norm)
       (* 0.0722 b-norm))))

(defun detect-terminal-background ()
  "Detect if terminal has light or dark background.
   Returns :dark, :light, or nil if unable to determine.

   Environment variable ICL_BACKGROUND can override detection:
   - ICL_BACKGROUND=dark  - assume dark background
   - ICL_BACKGROUND=light - assume light background"
  (unless *terminal-background*
    (let ((override (uiop:getenv "ICL_BACKGROUND")))
      (cond
        ((and override (string-equal override "dark"))
         (setf *terminal-background* :dark))
        ((and override (string-equal override "light"))
         (setf *terminal-background* :light))
        (t
         ;; Try terminal query (works on Windows Terminal)
         (multiple-value-bind (r g b) (query-terminal-background)
           (if (and r g b)
               (let ((luminance (compute-luminance r g b)))
                 (setf *terminal-background*
                       (if (< luminance 0.5) :dark :light)))
               ;; Default to dark on Windows (most common)
               (setf *terminal-background* :dark)))))))
  *terminal-background*)

(defun terminal-dark-p ()
  "Return T if terminal has dark background."
  (let ((bg (detect-terminal-background)))
    (or (null bg) (eq bg :dark))))

(defun terminal-light-p ()
  "Return T if terminal has light background."
  (eq (detect-terminal-background) :light))
