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
  (when *browser-terminal-active*
    (setf *terminal-raw-p* t)
    ;; Note: We don't send bracketed paste mode (ESC[?2004h) for browser terminals.
    ;; xterm.js handles paste via JavaScript events, and sending escape sequences
    ;; character-by-character through gray streams can cause display artifacts.
    (return-from enter-raw-mode t))
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
  ;; Enable bracketed paste mode
  (format t "~C[?2004h" +esc+)
  (force-output)
  t)

(defun exit-raw-mode ()
  "Restore terminal to original settings."
  (when *browser-terminal-active*
    ;; No cleanup needed - we didn't send bracketed paste mode for browser terminals
    (setf *terminal-raw-p* nil)
    (return-from exit-raw-mode t))
  (when (and *terminal-raw-p* *saved-termios*)
    ;; Disable bracketed paste mode
    (format t "~C[?2004l" +esc+)
    (force-output)
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
;;; Process Suspend/Resume (Ctrl-Z)
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *suspend-hook* nil
  "Function to call before suspending (e.g., to save editor state).")

(defvar *resume-hook* nil
  "Function to call after resuming (e.g., to redraw screen).")

(defun suspend-process ()
  "Suspend the process (like Ctrl-Z in a normal shell).
   Restores terminal, sends SIGTSTP, and re-enters raw mode on resume."
  ;; Don't suspend in browser mode
  (when *browser-terminal-active*
    (return-from suspend-process nil))
  ;; Call pre-suspend hook
  (when *suspend-hook*
    (funcall *suspend-hook*))
  ;; Exit raw mode to restore terminal
  (exit-raw-mode)
  ;; Print newline so shell prompt appears on fresh line
  (fresh-line)
  (force-output)
  ;; Send SIGTSTP to our process group
  (osicat-posix:kill 0 osicat-posix:sigtstp)
  ;; When we get here, we've been resumed (SIGCONT was received)
  ;; Re-enter raw mode
  (enter-raw-mode)
  ;; Call post-resume hook
  (when *resume-hook*
    (funcall *resume-hook*))
  t)

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
       ;; In browser mode, always parse escape sequences (xterm.js sends complete sequences)
       ;; In terminal mode, only parse if more chars are immediately available
       (if (or *browser-terminal-active* (char-available-p))
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
      ;; Arrow keys (possibly with modifiers: ESC[1;2A = Shift+Up)
      ((char= c #\A) :up)
      ((char= c #\B) :down)
      ((char= c #\C) :right)
      ((char= c #\D) :left)
      ;; Home/End
      ((char= c #\H) :home)
      ((char= c #\F) :end)
      ;; Extended sequences (e.g., ESC[1~ for Home, ESC[13;2u for Shift+Enter)
      ((digit-char-p c)
       (parse-extended-csi c))
      (t :unknown))))

(defun parse-extended-csi (first-digit)
  "Parse extended CSI sequence starting with a digit.
   Handles formats like:
   - ESC[N~ (function keys)
   - ESC[N;Mu (kitty keyboard protocol, M=modifier)
   - ESC[1;MA (modified arrow keys)"
  (let ((num1 (- (char-code first-digit) (char-code #\0)))
        (num2 nil)
        (c nil))
    ;; Read first number
    (loop do (setf c (read-char-raw))
          while (and c (digit-char-p c))
          do (setf num1 (+ (* num1 10) (- (char-code c) (char-code #\0)))))
    ;; Check what follows
    (unless c
      (return-from parse-extended-csi :unknown))
    ;; If semicolon, read modifier
    (when (char= c #\;)
      (setf num2 0)
      (loop do (setf c (read-char-raw))
            while (and c (digit-char-p c))
            do (setf num2 (+ (* num2 10) (- (char-code c) (char-code #\0))))))
    ;; Now c is the terminator
    (unless c
      (return-from parse-extended-csi :unknown))
    ;; Handle based on terminator
    (cond
      ;; Kitty keyboard protocol: ESC[key;modifieru
      ;; key=13 is CR (Enter), modifier=2 is Shift
      ((char= c #\u)
       (if (and (= num1 13) num2 (= (logand num2 1) 1))  ; Shift bit set
           :shift-enter
           :unknown))
      ;; Modified arrow keys: ESC[1;2A = Shift+Up, etc.
      ((and (= num1 1) num2)
       (cond
         ((char= c #\A) :up)
         ((char= c #\B) :down)
         ((char= c #\C) :right)
         ((char= c #\D) :left)
         (t :unknown)))
      ;; Standard function key sequences: ESC[N~
      ((char= c #\~)
       (case num1
         (1 :home)
         (2 :insert)
         (3 :delete)
         (4 :end)
         (5 :page-up)
         (6 :page-down)
         (200 (read-bracketed-paste))  ; Bracketed paste start
         (201 :paste-end)              ; Shouldn't see this alone
         (otherwise :unknown)))
      (t :unknown))))

(defun read-bracketed-paste ()
  "Read pasted text until ESC[201~ (paste end).
   Returns a cons of (:paste . string)."
  (let ((chars nil))
    (loop
      (let ((c (read-char-raw)))
        (unless c
          ;; EOF during paste - return what we have
          (return (cons :paste (coerce (nreverse chars) 'string))))
        (cond
          ;; Check for ESC - might be end of paste
          ((char= c +esc+)
           (let ((next (read-char-raw)))
             (cond
               ((null next)
                (push c chars)
                (return (cons :paste (coerce (nreverse chars) 'string))))
               ((char= next #\[)
                ;; Could be ESC[201~ - check for 201~
                (let ((num 0)
                      (nc nil))
                  (loop do (setf nc (read-char-raw))
                        while (and nc (digit-char-p nc))
                        do (setf num (+ (* num 10) (- (char-code nc) (char-code #\0)))))
                  (if (and nc (char= nc #\~) (= num 201))
                      ;; End of paste
                      (return (cons :paste (coerce (nreverse chars) 'string)))
                      ;; Not end of paste - push the escape sequence chars
                      (progn
                        (push c chars)
                        (push next chars)
                        ;; Push digits
                        (let ((num-str (format nil "~D" num)))
                          (dotimes (i (length num-str))
                            (push (char num-str i) chars)))
                        (when nc (push nc chars))))))
               (t
                ;; Not CSI, push both chars
                (push c chars)
                (push next chars)))))
          (t
           (push c chars)))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Terminal Color Detection
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *terminal-background* nil
  "Cached terminal background: :dark, :light, or nil if unknown.")

(defun query-terminal-background ()
  "Query terminal for its background color using OSC 11.
   Returns (values r g b) as integers 0-65535, or NIL if query fails."
  (handler-case
      (with-raw-mode
        ;; Send OSC 11 query: ESC ] 11 ; ? BEL
        (format t "~C]11;?~C" +esc+ (code-char 7))
        (force-output)
        ;; Give terminal time to respond
        (sleep 0.05)
        ;; Check if response available
        (unless (char-available-p)
          (return-from query-terminal-background nil))
        ;; Read response: ESC ] 11 ; rgb:RRRR/GGGG/BBBB ESC \ or BEL
        (let ((c (read-char-raw)))
          (unless (and c (char= c +esc+))
            (return-from query-terminal-background nil))
          (setf c (read-char-raw))
          (unless (and c (char= c #\]))
            (return-from query-terminal-background nil))
          ;; Skip "11;"
          (dotimes (i 3) (read-char-raw))
          ;; Read until we find "rgb:" or similar
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
  "Compute relative luminance from RGB values (0-65535 range).
   Returns a value 0.0 (black) to 1.0 (white)."
  (let ((r-norm (/ r 65535.0))
        (g-norm (/ g 65535.0))
        (b-norm (/ b 65535.0)))
    ;; Standard luminance formula
    (+ (* 0.2126 r-norm)
       (* 0.7152 g-norm)
       (* 0.0722 b-norm))))

(defun detect-terminal-background ()
  "Detect if terminal has light or dark background.
   Returns :dark, :light, or nil if unable to determine.
   Caches result in *terminal-background*.

   Environment variable ICL_BACKGROUND can override detection:
   - ICL_BACKGROUND=dark  - assume dark background
   - ICL_BACKGROUND=light - assume light background"
  ;; For browser terminals, always return dark (the xterm.js theme uses #111).
  ;; Don't query via OSC 11 as it won't work through gray streams.
  (when *browser-terminal-active*
    (return-from detect-terminal-background :dark))
  (unless *terminal-background*
    ;; Check for environment variable override first
    (let ((override (uiop:getenv "ICL_BACKGROUND")))
      (cond
        ((and override (string-equal override "dark"))
         (setf *terminal-background* :dark))
        ((and override (string-equal override "light"))
         (setf *terminal-background* :light))
        ;; No override - try terminal query
        (t
         (multiple-value-bind (r g b) (query-terminal-background)
           (when (and r g b)
             (let ((luminance (compute-luminance r g b)))
               (setf *terminal-background*
                     (if (< luminance 0.5) :dark :light)))))))))
  *terminal-background*)

(defun terminal-dark-p ()
  "Return T if terminal has dark background, NIL otherwise.
   Defaults to assuming dark background if detection fails."
  (let ((bg (detect-terminal-background)))
    (or (null bg) (eq bg :dark))))

(defun terminal-light-p ()
  "Return T if terminal has light background."
  (eq (detect-terminal-background) :light))
