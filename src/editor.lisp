;;; editor.lisp --- Multi-line editor for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Editor State
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *editor-history* nil
  "List of previous inputs for history navigation.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Persistent History
;;; ─────────────────────────────────────────────────────────────────────────────

(defun load-history ()
  "Load history from history file."
  (let ((hfile (history-file)))
    (when (and hfile (probe-file hfile))
      (handler-case
          (with-open-file (in hfile :direction :input
                                           :if-does-not-exist nil)
          (when in
            (let ((entries nil))
              (loop for line = (read-line in nil nil)
                    while line
                    ;; Decode the entry (restore newlines)
                    do (push (decode-history-entry line) entries))
              ;; History is stored oldest-first in file, we need newest-first
              (setf *editor-history* entries)  ; entries is already newest-first from push
              ;; Trim to max size
              (when (> (length *editor-history*) *history-size*)
                (setf *editor-history*
                      (subseq *editor-history* 0 *history-size*))))))
        (error (e)
          (declare (ignore e))
          nil)))))

(defun save-history ()
  "Save history to history file."
  (let ((hfile (history-file)))
    (when hfile
      (handler-case
          (ensure-directories-exist hfile)
        (error () nil))
      (handler-case
          (with-open-file (out hfile :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
          ;; Save newest-first (reverse to get oldest-first in file)
          (let ((to-save (if (> (length *editor-history*) *history-size*)
                             (subseq *editor-history* 0 *history-size*)
                             *editor-history*)))
            (dolist (entry (reverse to-save))
              ;; Replace newlines with escaped version for storage
              (write-line (encode-history-entry entry) out))))
        (error (e)
          (declare (ignore e))
          nil)))))

(defun encode-history-entry (entry)
  "Encode a history entry for file storage (handle multi-line)."
  ;; Replace actual newlines with literal \\n for single-line storage
  (with-output-to-string (out)
    (loop for char across entry
          do (if (char= char #\Newline)
                 (write-string "\\n" out)
                 (write-char char out)))))

(defun decode-history-entry (line)
  "Decode a history entry from file storage."
  ;; Replace literal \\n with actual newlines
  (with-output-to-string (out)
    (let ((i 0)
          (len (length line)))
      (loop while (< i len)
            do (cond
                 ((and (< (1+ i) len)
                       (char= (char line i) #\\)
                       (char= (char line (1+ i)) #\n))
                  (write-char #\Newline out)
                  (incf i 2))
                 (t
                  (write-char (char line i) out)
                  (incf i)))))))

(defvar *history-index* nil
  "Current position in history during navigation.")

(defvar *history-saved-buffer* nil
  "Saved buffer state when navigating history.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Reverse Search State
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *search-mode* nil
  "T when in reverse-search mode.")

(defvar *search-pattern* ""
  "Current search pattern.")

(defvar *search-match-index* 0
  "Index of current match in *search-matches*.")

(defvar *search-matches* nil
  "List of history indices matching the current pattern.")

(defvar *screen-row* 0
  "Current screen row relative to line 0 of the buffer.")

(defvar *last-key-was-tab* nil
  "T if the last key pressed was Tab (for completion cycling).")

(defvar *completion-line-col* nil
  "Cons of (line . col) at start of completion for tracking changes.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Rendering
;;; ─────────────────────────────────────────────────────────────────────────────

(defun render-buffer (buf)
  "Render the buffer to the terminal, using *screen-row* to know current position."
  (let ((line-count (buffer-line-count buf))
        (cursor-row (edit-buffer-row buf))
        (cursor-col (edit-buffer-col buf))
        ;; Get full buffer content for multi-line highlighting
        (full-content (buffer-contents buf))
        ;; Get absolute cursor position for paren matching
        (cursor-pos (buffer-cursor-position buf)))
    ;; Move up to line 0 from current screen position
    (when (plusp *screen-row*)
      (cursor-up *screen-row*))
    (cursor-to-column 1)
    ;; Highlight the full content with cursor position for paren matching
    (let* ((highlighted (highlight-string full-content cursor-pos))
           (highlighted-lines (split-sequence:split-sequence #\Newline highlighted)))
      ;; Draw each line
      (dotimes (i line-count)
        (let ((prompt (buffer-prompt-for-line buf i))
              (hl-line (if (< i (length highlighted-lines))
                           (nth i highlighted-lines)
                           "")))
          (clear-line)
          (format t "~A~A" prompt hl-line)
          (when (< i (1- line-count))
            (format t "~%")))))
    ;; Clear any lines below (from previous longer input)
    (clear-below)
    ;; Position cursor at buffer position
    (let ((lines-to-go-up (- (1- line-count) cursor-row)))
      (when (plusp lines-to-go-up)
        (cursor-up lines-to-go-up)))
    (let ((prompt-len (visible-string-length (buffer-prompt-for-line buf cursor-row))))
      (cursor-to-column (+ prompt-len cursor-col 1)))
    ;; Update screen row tracking
    (setf *screen-row* cursor-row)
    (force-output)))

(defun render-buffer-final (buf)
  "Render the buffer without paren matching (for final display after submission)."
  (let ((line-count (buffer-line-count buf))
        (cursor-row (edit-buffer-row buf))
        (cursor-col (edit-buffer-col buf))
        (full-content (buffer-contents buf)))
    ;; Move up to line 0 from current screen position
    (when (plusp *screen-row*)
      (cursor-up *screen-row*))
    (cursor-to-column 1)
    ;; Highlight without cursor position (no paren matching)
    (let* ((highlighted (highlight-string full-content nil))
           (highlighted-lines (split-sequence:split-sequence #\Newline highlighted)))
      ;; Draw each line
      (dotimes (i line-count)
        (let ((prompt (buffer-prompt-for-line buf i))
              (hl-line (if (< i (length highlighted-lines))
                           (nth i highlighted-lines)
                           "")))
          (clear-line)
          (format t "~A~A" prompt hl-line)
          (when (< i (1- line-count))
            (format t "~%")))))
    ;; Clear any lines below
    (clear-below)
    ;; Position cursor at end
    (let ((lines-to-go-up (- (1- line-count) cursor-row)))
      (when (plusp lines-to-go-up)
        (cursor-up lines-to-go-up)))
    (let ((prompt-len (visible-string-length (buffer-prompt-for-line buf cursor-row))))
      (cursor-to-column (+ prompt-len cursor-col 1)))
    (setf *screen-row* cursor-row)
    (force-output)))

(defun render-current-line (buf)
  "Render just the current line (optimization for single-line edits)."
  (let* ((row (edit-buffer-row buf))
         (col (edit-buffer-col buf))
         (prompt (buffer-prompt-for-line buf row))
         (line (buffer-line buf row))
         (prompt-len (visible-string-length prompt))
         ;; For accurate highlighting, we need full buffer context
         (full-content (buffer-contents buf))
         ;; Get cursor position for paren matching
         (cursor-pos (buffer-cursor-position buf))
         (highlighted (highlight-string full-content cursor-pos))
         (highlighted-lines (split-sequence:split-sequence #\Newline highlighted))
         (hl-line (if (< row (length highlighted-lines))
                      (nth row highlighted-lines)
                      line)))
    (cursor-to-column 1)
    (clear-line)
    (format t "~A~A" prompt hl-line)
    (cursor-to-column (+ prompt-len col 1))
    (force-output)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; History
;;; ─────────────────────────────────────────────────────────────────────────────

(defun history-add (input)
  "Add INPUT to history if non-empty and different from last entry."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) input)))
    (when (and (plusp (length trimmed))
               (or (null *editor-history*)
                   (string/= trimmed (first *editor-history*))))
      (push trimmed *editor-history*))))

(defun history-previous (buf)
  "Navigate to previous history entry."
  (when (null *history-index*)
    ;; Save current buffer
    (setf *history-saved-buffer* (buffer-contents buf))
    (setf *history-index* -1))
  (when (< (1+ *history-index*) (length *editor-history*))
    (incf *history-index*)
    (let ((entry (nth *history-index* *editor-history*)))
      (buffer-set-contents buf entry))
    t))

(defun history-next (buf)
  "Navigate to next history entry (or back to current input)."
  (when *history-index*
    (cond
      ((zerop *history-index*)
       ;; Restore original input
       (buffer-set-contents buf *history-saved-buffer*)
       (setf *history-index* nil
             *history-saved-buffer* nil)
       t)
      (t
       (decf *history-index*)
       (let ((entry (nth *history-index* *editor-history*)))
         (buffer-set-contents buf entry))
       t))))

(defun buffer-set-contents (buf string)
  "Set buffer contents from STRING, splitting on newlines."
  (let* ((lines (split-string-by-newline string))
         (vec (make-array (length lines)
                          :initial-contents lines
                          :adjustable t
                          :fill-pointer (length lines))))
    (setf (edit-buffer-lines buf) vec)
    (setf (edit-buffer-row buf) (1- (length lines)))
    (setf (edit-buffer-col buf) (length (aref vec (1- (length lines)))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Reverse History Search
;;; ─────────────────────────────────────────────────────────────────────────────

(defun search-history-for (pattern)
  "Find all history entries containing PATTERN (case-insensitive).
   Returns list of indices into *editor-history*."
  (when (plusp (length pattern))
    (let ((pat (string-downcase pattern))
          (matches nil))
      (loop for entry in *editor-history*
            for i from 0
            when (search pat (string-downcase entry))
            do (push i matches))
      (nreverse matches))))

(defun enter-search-mode (buf)
  "Enter reverse-search mode."
  (setf *search-mode* t
        *search-pattern* ""
        *search-match-index* 0
        *search-matches* nil
        *search-display-lines* 0)
  ;; Save current buffer
  (unless *history-saved-buffer*
    (setf *history-saved-buffer* (buffer-contents buf))))

(defun exit-search-mode (buf accept)
  "Exit reverse-search mode.
   If ACCEPT is T, keep the current match in buffer."
  (when (and accept *search-matches* (< *search-match-index* (length *search-matches*)))
    (let* ((hist-idx (nth *search-match-index* *search-matches*))
           (entry (nth hist-idx *editor-history*)))
      (buffer-set-contents buf entry)))
  (setf *search-mode* nil
        *search-pattern* ""
        *search-match-index* 0
        *search-matches* nil
        *search-display-lines* 0
        *history-saved-buffer* nil))

(defun update-search (buf)
  "Update search matches based on current pattern and update buffer."
  (setf *search-matches* (search-history-for *search-pattern*)
        *search-match-index* 0)
  (if *search-matches*
      (let* ((hist-idx (first *search-matches*))
             (entry (nth hist-idx *editor-history*)))
        (buffer-set-contents buf entry))
      ;; No matches - restore original
      (when *history-saved-buffer*
        (buffer-set-contents buf *history-saved-buffer*))))

(defun search-next-match (buf)
  "Move to next match in search results."
  (when (and *search-matches*
             (< (1+ *search-match-index*) (length *search-matches*)))
    (incf *search-match-index*)
    (let* ((hist-idx (nth *search-match-index* *search-matches*))
           (entry (nth hist-idx *editor-history*)))
      (buffer-set-contents buf entry))))

(defvar *search-display-lines* 0
  "Number of lines used by current search display (for redrawing in place).")

(defun render-search-prompt ()
  "Render just the search prompt (for initial entry into search mode)."
  ;; Move up to clear previous display if any
  (when (plusp *search-display-lines*)
    (cursor-up *search-display-lines*))
  (cursor-to-column 1)
  (clear-line)
  (format t "~A`~A'~A"
          (colorize "(reverse-i-search)" *color-dim*)
          *search-pattern*
          (colorize "[no match]" *color-red*))
  (clear-below)
  (setf *search-display-lines* 0)
  (force-output))

(defun render-search-with-buffer (buf)
  "Render the search prompt and full buffer content, updating in place."
  ;; Move up to start of our display area
  (when (plusp *search-display-lines*)
    (cursor-up *search-display-lines*))
  (cursor-to-column 1)

  ;; Line 0: Search prompt
  (clear-line)
  (let ((status (if *search-matches*
                    (format nil "~D/~D" (1+ *search-match-index*) (length *search-matches*))
                    "no match")))
    (format t "~A`~A'~A"
            (colorize "(reverse-i-search)" *color-dim*)
            *search-pattern*
            (colorize (format nil "[~A]" status)
                      (if *search-matches* *color-green* *color-red*))))

  ;; Buffer content with syntax highlighting
  (let* ((full-content (buffer-contents buf))
         (highlighted (highlight-string full-content nil))
         (highlighted-lines (split-sequence:split-sequence #\Newline highlighted))
         (line-count (length highlighted-lines)))
    ;; Draw each buffer line
    (dotimes (i line-count)
      (format t "~%")
      (clear-line)
      (let ((prompt (buffer-prompt-for-line buf i))
            (hl-line (nth i highlighted-lines)))
        (format t "~A~A" prompt hl-line)))
    ;; Clear any leftover lines from previous longer content
    (clear-below)
    ;; Track lines: search prompt doesn't count since we start there,
    ;; just count buffer lines we moved down
    (setf *search-display-lines* line-count))
  (force-output))

(defun handle-search-key (buf key)
  "Handle KEY in reverse-search mode.
   Returns :continue, :accept, :cancel, or :next."
  (cond
    ;; Ctrl-R - next match
    ((eql key :reverse-search)
     (search-next-match buf)
     :next)
    ;; Enter - accept and exit
    ((eql key :enter)
     :accept)
    ;; Escape or Ctrl-G - cancel
    ((or (eql key :escape) (eql key :cancel-search))
     :cancel)
    ;; Backspace - delete from pattern
    ((eql key :backspace)
     (when (plusp (length *search-pattern*))
       (setf *search-pattern* (subseq *search-pattern* 0 (1- (length *search-pattern*))))
       (update-search buf))
     :continue)
    ;; Regular character - add to pattern
    ((characterp key)
     (setf *search-pattern* (concatenate 'string *search-pattern* (string key)))
     (update-search buf)
     :continue)
    ;; Any other key - accept and pass through
    (t :accept-and-pass)))

(defun split-string-by-newline (string)
  "Split STRING by newlines, returning a list of strings."
  (let ((lines nil)
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) #\Newline)
          do (push (subseq string start i) lines)
             (setf start (1+ i)))
    (push (subseq string start) lines)
    (nreverse lines)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Form Completion Check
;;; ─────────────────────────────────────────────────────────────────────────────

(defun form-complete-p (string)
  "Check if STRING contains a complete Lisp form."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) string)))
    (when (zerop (length trimmed))
      (return-from form-complete-p nil))
    (handler-case
        (progn
          (read-from-string trimmed)
          t)
      (end-of-file () nil)
      (reader-error () t)))) ; Syntax errors are "complete" (will error on eval)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Completion Menu Handling
;;; ─────────────────────────────────────────────────────────────────────────────

(defun handle-completion-tab (buf)
  "Handle Tab key for completion.
   Opens dropdown menu if multiple candidates."
  (let* ((col (edit-buffer-col buf))
         (line (buffer-current-line buf)))
    (if (completion-menu-active-p)
        ;; Menu is open - Tab accepts current selection
        (progn
          (multiple-value-bind (new-line new-col)
              (apply-selected-completion line col)
            (when new-line
              (setf (buffer-current-line buf) new-line)
              (setf (edit-buffer-col buf) new-col)))
          (close-completion-menu)
          :menu-closed)
        ;; Menu not open - try to complete
        (multiple-value-bind (prefix start type)
            (extract-completion-prefix line col)
          (declare (ignore type))
          (multiple-value-bind (new-line new-col candidates)
              (attempt-completion line col)
            (cond
              ;; No completions
              ((and (string= new-line line) (null candidates))
               :continue)
              ;; Single completion - insert directly
              ((null candidates)
               (setf (buffer-current-line buf) new-line)
               (setf (edit-buffer-col buf) new-col)
               :continue)
              ;; Multiple completions - open dropdown menu
              (t
               (setf (buffer-current-line buf) new-line)
               (setf (edit-buffer-col buf) new-col)
               (open-completion-menu candidates prefix start)
               :menu-opened)))))))

(defun handle-menu-key (buf key)
  "Handle KEY when completion menu is active.
   Returns :menu-nav, :menu-closed, or nil to pass through."
  (cond
    ;; Down arrow - next item
    ((eql key :down)
     (menu-select-next)
     :menu-nav)
    ;; Up arrow - previous item
    ((eql key :up)
     (menu-select-previous)
     :menu-nav)
    ;; Enter - accept selection
    ((eql key :enter)
     (let* ((col (edit-buffer-col buf))
            (line (buffer-current-line buf)))
       (multiple-value-bind (new-line new-col)
           (apply-selected-completion line col)
         (when new-line
           (setf (buffer-current-line buf) new-line)
           (setf (edit-buffer-col buf) new-col))))
     (close-completion-menu)
     :menu-closed)
    ;; Tab - also accept selection
    ((eql key :tab)
     (let* ((col (edit-buffer-col buf))
            (line (buffer-current-line buf)))
       (multiple-value-bind (new-line new-col)
           (apply-selected-completion line col)
         (when new-line
           (setf (buffer-current-line buf) new-line)
           (setf (edit-buffer-col buf) new-col))))
     (close-completion-menu)
     :menu-closed)
    ;; Escape - cancel
    ((eql key :escape)
     (close-completion-menu)
     :menu-closed)
    ;; Any other key - close menu, clear display, and let key be processed
    ;; Return :menu-dismissed so caller knows to do full redraw after processing
    (t
     (close-completion-menu)
     (clear-completion-menu 12)
     :menu-dismissed)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Key Handling
;;; ─────────────────────────────────────────────────────────────────────────────

(defun handle-key (buf key)
  "Handle KEY input, updating BUF. Returns :done, :cancel, :continue, or :redraw."
  (cond
    ;; EOF
    ((eql key :eof)
     (if (buffer-empty-p buf)
         :eof
         :continue))
    ;; Interrupt
    ((eql key :interrupt)
     :cancel)
    ;; Enter - check if form is complete
    ;; In paredit mode, also require cursor at end (since forms are always balanced)
    ((eql key :enter)
     (let ((contents (buffer-contents buf)))
       (if (and (form-complete-p contents)
                (or (not *paredit-mode*)
                    (buffer-at-end-p buf)))
           :done
           (progn
             (buffer-insert-newline buf)
             :newline))))  ; Special case - just need to show new line
    ;; Shift+Enter or Alt+Enter - always insert newline (don't submit even if form is complete)
    ((or (eql key :shift-enter)
         (and (consp key)
              (eql (first key) :alt)
              (member (rest key) '(#\Return #\Newline) :test #'char=)))
     (buffer-insert-newline buf)
     :newline)
    ;; Navigation - need full redraw for multi-line to update paren highlighting
    ((eql key :left)
     (buffer-move-left buf)
     (if (> (buffer-line-count buf) 1) :redraw :continue))
    ((eql key :right)
     (buffer-move-right buf)
     (if (> (buffer-line-count buf) 1) :redraw :continue))
    ((eql key :up)
     (if (zerop (edit-buffer-row buf))
         ;; At first line - try history
         (when (history-previous buf)
           :redraw)
         (progn
           (buffer-move-up buf)
           :redraw)))
    ((eql key :down)
     (if (= (edit-buffer-row buf) (1- (buffer-line-count buf)))
         ;; At last line - try history forward
         (when (history-next buf)
           :redraw)
         (progn
           (buffer-move-down buf)
           :redraw)))
    ((eql key :home)
     (buffer-move-to-line-start buf)
     (if (> (buffer-line-count buf) 1) :redraw :continue))
    ((eql key :end)
     (buffer-move-to-line-end buf)
     (if (> (buffer-line-count buf) 1) :redraw :continue))
    ;; Deletion (with paredit support)
    ((eql key :backspace)
     (if (if *paredit-mode*
             (paredit-backspace buf)
             (buffer-delete-char-before buf))
         :redraw
         :continue))
    ((eql key :delete)
     (if (if *paredit-mode*
             (paredit-delete buf)
             (buffer-delete-char-at buf))
         :redraw
         :continue))
    ((eql key :kill-line)
     (buffer-kill-line buf)
     :redraw)
    ((eql key :clear-line)
     (buffer-clear-line buf)
     :redraw)
    ((eql key :clear-screen)
     (clear-screen-full)
     (cursor-position 1 1)
     :redraw)
    ;; Tab - completion
    ((eql key :tab)
     (handle-completion-tab buf))
    ;; Alt+Q - reformat/reindent
    ((and (consp key) (eql (first key) :alt) (char-equal (rest key) #\q))
     (buffer-reindent buf)
     :redraw)
    ;; Alt+F - forward sexp (paredit)
    ((and (consp key) (eql (first key) :alt) (char-equal (rest key) #\f))
     (when (buffer-forward-sexp buf)
       :redraw))
    ;; Alt+B - backward sexp (paredit)
    ((and (consp key) (eql (first key) :alt) (char-equal (rest key) #\b))
     (when (buffer-backward-sexp buf)
       :redraw))
    ;; Ctrl-R - enter reverse search mode
    ((eql key :reverse-search)
     (enter-search-mode buf)
     :search-mode)
    ;; Bracketed paste - insert entire string at once
    ((and (consp key) (eql (car key) :paste))
     (let ((text (cdr key)))
       (dotimes (i (length text))
         (let ((c (char text i)))
           (cond
             ;; Convert CR or CRLF to newline
             ((char= c #\Return)
              ;; Skip if followed by LF
              (unless (and (< (1+ i) (length text))
                           (char= (char text (1+ i)) #\Newline))
                (buffer-insert-newline buf)))
             ((char= c #\Newline)
              (buffer-insert-newline buf))
             ;; Regular character
             (t
              (if (and *paredit-mode*
                       (eql (paredit-handle-char buf c) :handled))
                  nil  ; Paredit handled it
                  (buffer-insert-char buf c)))))))
     :redraw)
    ;; Regular character (with paredit support)
    ((characterp key)
     (if (and *paredit-mode*
              (eql (paredit-handle-char buf key) :handled))
         ;; Paredit handled it
         (if (> (buffer-line-count buf) 1) :redraw :continue)
         ;; Normal character insertion
         (progn
           (buffer-insert-char buf key)
           (if (> (buffer-line-count buf) 1)
               :redraw
               :continue))))
    ;; Unknown - ignore
    (t :continue)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Main Editor Loop
;;; ─────────────────────────────────────────────────────────────────────────────

(defun run-editor (prompt continuation-prompt)
  "Run the multi-line editor.
   Returns the input string, :EOF on EOF, :CANCEL on interrupt, or :NOT-A-TTY if raw mode fails."
  (let ((buf (make-edit-buffer :prompt prompt
                               :continuation-prompt continuation-prompt)))
    (setf *history-index* nil
          *history-saved-buffer* nil
          *screen-row* 0
          *last-key-was-tab* nil
          *completion-line-col* nil
          *search-mode* nil)
    (reset-completion-state)
    ;; Try to enter raw mode
    (let ((entered (enter-raw-mode)))
      (unless entered
        (return-from run-editor :not-a-tty))
      (unwind-protect
           (progn
             ;; Initial render
             (format t "~A" prompt)
             (force-output)
             ;; Main loop
             (loop
               (let ((key (read-key))
                     (process-normal-key t))
                 ;; Handle search mode separately
                 (when *search-mode*
                   (let ((search-result (handle-search-key buf key)))
                     (cond
                       ((eql search-result :continue)
                        (render-search-with-buffer buf)
                        (setf process-normal-key nil))
                       ((eql search-result :next)
                        (render-search-with-buffer buf)
                        (setf process-normal-key nil))
                       ((eql search-result :accept)
                        (exit-search-mode buf t)
                        (render-buffer buf)
                        (setf process-normal-key nil))
                       ((eql search-result :cancel)
                        ;; Restore original buffer
                        (when *history-saved-buffer*
                          (buffer-set-contents buf *history-saved-buffer*))
                        (exit-search-mode buf nil)
                        (render-buffer buf)
                        (setf process-normal-key nil))
                       ((eql search-result :accept-and-pass)
                        ;; Accept current match and process key normally
                        (exit-search-mode buf t)
                        (render-buffer buf)
                        ;; Let key be processed below
                        (setf process-normal-key t)))))
                 ;; Process key normally if not consumed by search mode
                 (when process-normal-key
                   (let* ((prompt-len (visible-string-length (buffer-prompt-for-line buf (edit-buffer-row buf))))
                          ;; Check if completion menu is active
                          (menu-result (when (completion-menu-active-p)
                                         (handle-menu-key buf key)))
                          ;; Handle key normally if menu didn't consume it
                          ;; For :menu-dismissed, process the key then force redraw
                          (dismissed (eql menu-result :menu-dismissed))
                          (result (cond
                                    (dismissed
                                     ;; Menu was dismissed by this key - process key and force redraw
                                     (handle-key buf key)
                                     :redraw)
                                    (menu-result menu-result)
                                    (t (handle-key buf key)))))
                     ;; Handle result
                     (cond
                       ;; Search mode entered
                       ((eql result :search-mode)
                        (render-search-prompt))
                       ;; Menu navigation - redraw line and menu
                       ((eql result :menu-nav)
                        (render-current-line buf)
                        (render-completion-menu prompt-len (edit-buffer-col buf)))
                       ;; Menu opened - render line and show menu
                       ((eql result :menu-opened)
                        (render-current-line buf)
                        (render-completion-menu prompt-len (edit-buffer-col buf)))
                       ;; Menu closed - clear menu area and redraw
                       ((eql result :menu-closed)
                        (clear-completion-menu 12)
                        (render-buffer buf))
                       ;; Standard cases
                       ((eql result :done)
                        ;; Close menu if open
                        (when (completion-menu-active-p)
                          (clear-completion-menu 12)
                          (close-completion-menu))
                        ;; Move to end, render without paren matching, print newline
                        (buffer-move-to-end buf)
                        (render-buffer-final buf)
                        (format t "~%")
                        (force-output)
                        (let ((contents (buffer-contents buf)))
                          (history-add contents)
                          (return contents)))
                       ((eql result :eof)
                        (when (completion-menu-active-p)
                          (clear-completion-menu 12))
                        (format t "~%")
                        (force-output)
                        (return :eof))
                       ((eql result :cancel)
                        (when (completion-menu-active-p)
                          (clear-completion-menu 12))
                        (format t "~%")
                        (force-output)
                        (return :cancel))
                       ((eql result :redraw)
                        (render-buffer buf)
                        ;; Re-render menu if still active
                        (when (completion-menu-active-p)
                          (render-completion-menu prompt-len (edit-buffer-col buf))))
                       ((eql result :newline)
                        ;; Close menu on newline
                        (when (completion-menu-active-p)
                          (clear-completion-menu 12)
                          (close-completion-menu))
                        ;; Full redraw to update paren highlighting on all lines
                        (render-buffer buf))
                       ((eql result :continue)
                        (render-current-line buf)
                        ;; Re-render menu if still active
                        (when (completion-menu-active-p)
                          (render-completion-menu prompt-len (edit-buffer-col buf))))
                       (t nil)))))))
        (exit-raw-mode)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Public Interface
;;; ─────────────────────────────────────────────────────────────────────────────

(defun multiline-edit (&key (prompt "") (continuation-prompt ""))
  "Read a complete Lisp form with multi-line editing.
   Returns the input string, NIL on EOF, :CANCEL on interrupt, or :NOT-A-TTY if not a terminal."
  (if (terminal-capable-p)
      (handler-case
          (let ((result (run-editor prompt continuation-prompt)))
            (case result
              (:eof nil)           ; Convert :eof to nil for consistency
              (:not-a-tty :not-a-tty)
              (:cancel :cancel)
              (otherwise result)))
        (error (e)
          (declare (ignore e))
          ;; Fall back on any error
          :not-a-tty))
      ;; Non-interactive: signal to use fallback
      :not-a-tty))
