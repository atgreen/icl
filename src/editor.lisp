;;; editor.lisp --- Multi-line editor for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Editor State
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *editor-history* '()
  "List of previous inputs for history navigation.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Persistent History
;;; ─────────────────────────────────────────────────────────────────────────────

(defun load-history ()
  "Load history from *history-file*."
  (when (and *history-file* (probe-file *history-file*))
    (handler-case
        (with-open-file (in *history-file* :direction :input
                                           :if-does-not-exist nil)
          (when in
            (let ((entries '()))
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
        nil))))

(defun save-history ()
  "Save history to *history-file*."
  (when *history-file*
    (handler-case
        (ensure-directories-exist *history-file*)
      (error () nil))
    (handler-case
        (with-open-file (out *history-file* :direction :output
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
        nil))))

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
            do (if (and (< (1+ i) len)
                        (char= (char line i) #\\)
                        (char= (char line (1+ i)) #\n))
                   (progn
                     (write-char #\Newline out)
                     (incf i 2))
                   (progn
                     (write-char (char line i) out)
                     (incf i)))))))

(defvar *history-index* nil
  "Current position in history during navigation.")

(defvar *history-saved-buffer* nil
  "Saved buffer state when navigating history.")

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
        (cursor-col (edit-buffer-col buf)))
    ;; Move up to line 0 from current screen position
    (when (plusp *screen-row*)
      (cursor-up *screen-row*))
    (cursor-to-column 1)
    ;; Draw each line
    (dotimes (i line-count)
      (let ((prompt (buffer-prompt-for-line buf i))
            (line (buffer-line buf i)))
        (clear-line)
        (format t "~A~A" prompt line)
        (when (< i (1- line-count))
          (format t "~%"))))
    ;; Clear any lines below (from previous longer input)
    (clear-below)
    ;; Position cursor at buffer position
    (let ((lines-to-go-up (- (1- line-count) cursor-row)))
      (when (plusp lines-to-go-up)
        (cursor-up lines-to-go-up)))
    (let ((prompt-len (length (buffer-prompt-for-line buf cursor-row))))
      (cursor-to-column (+ prompt-len cursor-col 1)))
    ;; Update screen row tracking
    (setf *screen-row* cursor-row)
    (force-output)))

(defun render-current-line (buf)
  "Render just the current line (optimization for single-line edits)."
  (let* ((row (edit-buffer-row buf))
         (col (edit-buffer-col buf))
         (prompt (buffer-prompt-for-line buf row))
         (line (buffer-line buf row))
         (prompt-len (length prompt)))
    (cursor-to-column 1)
    (clear-line)
    (format t "~A~A" prompt line)
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

(defun split-string-by-newline (string)
  "Split STRING by newlines, returning a list of strings."
  (let ((lines '())
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
    ((eq key :down)
     (menu-select-next)
     :menu-nav)
    ;; Up arrow - previous item
    ((eq key :up)
     (menu-select-previous)
     :menu-nav)
    ;; Enter - accept selection
    ((eq key :enter)
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
    ((eq key :tab)
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
    ((eq key :escape)
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
    ((eq key :eof)
     (if (buffer-empty-p buf)
         :eof
         :continue))
    ;; Interrupt
    ((eq key :interrupt)
     :cancel)
    ;; Enter - check if form is complete
    ((eq key :enter)
     (let ((contents (buffer-contents buf)))
       (if (form-complete-p contents)
           :done
           (progn
             (buffer-insert-newline buf)
             :newline))))  ; Special case - just need to show new line
    ;; Navigation
    ((eq key :left)
     (buffer-move-left buf)
     :continue)
    ((eq key :right)
     (buffer-move-right buf)
     :continue)
    ((eq key :up)
     (if (zerop (edit-buffer-row buf))
         ;; At first line - try history
         (when (history-previous buf)
           :redraw)
         (progn
           (buffer-move-up buf)
           :redraw)))
    ((eq key :down)
     (if (= (edit-buffer-row buf) (1- (buffer-line-count buf)))
         ;; At last line - try history forward
         (when (history-next buf)
           :redraw)
         (progn
           (buffer-move-down buf)
           :redraw)))
    ((eq key :home)
     (buffer-move-to-line-start buf)
     :continue)
    ((eq key :end)
     (buffer-move-to-line-end buf)
     :continue)
    ;; Deletion
    ((eq key :backspace)
     (if (buffer-delete-char-before buf)
         :redraw
         :continue))
    ((eq key :delete)
     (if (buffer-delete-char-at buf)
         :redraw
         :continue))
    ((eq key :kill-line)
     (buffer-kill-line buf)
     :redraw)
    ((eq key :clear-line)
     (buffer-clear-line buf)
     :redraw)
    ((eq key :clear-screen)
     (clear-screen-full)
     (cursor-position 1 1)
     :redraw)
    ;; Tab - completion
    ((eq key :tab)
     (handle-completion-tab buf))
    ;; Alt+Q - reformat/reindent
    ((and (consp key) (eq (car key) :alt) (char-equal (cdr key) #\q))
     (buffer-reindent buf)
     :redraw)
    ;; Regular character
    ((characterp key)
     (buffer-insert-char buf key)
     :continue)
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
          *completion-line-col* nil)
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
               (let* ((key (read-key))
                      (prompt-len (length (buffer-prompt-for-line buf (edit-buffer-row buf))))
                      ;; Check if completion menu is active
                      (menu-result (when (completion-menu-active-p)
                                     (handle-menu-key buf key)))
                      ;; Handle key normally if menu didn't consume it
                      ;; For :menu-dismissed, process the key then force redraw
                      (dismissed (eq menu-result :menu-dismissed))
                      (result (cond
                                (dismissed
                                 ;; Menu was dismissed by this key - process key and force redraw
                                 (handle-key buf key)
                                 :redraw)
                                (menu-result menu-result)
                                (t (handle-key buf key)))))
                 ;; Handle result
                 (cond
                   ;; Menu navigation - redraw line and menu
                   ((eq result :menu-nav)
                    (render-current-line buf)
                    (render-completion-menu prompt-len (edit-buffer-col buf)))
                   ;; Menu opened - render line and show menu
                   ((eq result :menu-opened)
                    (render-current-line buf)
                    (render-completion-menu prompt-len (edit-buffer-col buf)))
                   ;; Menu closed - clear menu area and redraw
                   ((eq result :menu-closed)
                    (clear-completion-menu 12)
                    (render-buffer buf))
                   ;; Standard cases
                   ((eq result :done)
                    ;; Close menu if open
                    (when (completion-menu-active-p)
                      (clear-completion-menu 12)
                      (close-completion-menu))
                    ;; Move to end, print newline, return contents
                    (buffer-move-to-end buf)
                    (render-buffer buf)
                    (format t "~%")
                    (force-output)
                    (let ((contents (buffer-contents buf)))
                      (history-add contents)
                      (return contents)))
                   ((eq result :eof)
                    (when (completion-menu-active-p)
                      (clear-completion-menu 12))
                    (format t "~%")
                    (force-output)
                    (return :eof))
                   ((eq result :cancel)
                    (when (completion-menu-active-p)
                      (clear-completion-menu 12))
                    (format t "~%")
                    (force-output)
                    (return :cancel))
                   ((eq result :redraw)
                    (render-buffer buf)
                    ;; Re-render menu if still active
                    (when (completion-menu-active-p)
                      (render-completion-menu prompt-len (edit-buffer-col buf))))
                   ((eq result :newline)
                    ;; Close menu on newline
                    (when (completion-menu-active-p)
                      (clear-completion-menu 12)
                      (close-completion-menu))
                    ;; Just started a new line - output newline and continuation prompt
                    (format t "~%~A" (edit-buffer-continuation-prompt buf))
                    (force-output)
                    (setf *screen-row* (edit-buffer-row buf)))
                   ((eq result :continue)
                    (render-current-line buf)
                    ;; Re-render menu if still active
                    (when (completion-menu-active-p)
                      (render-completion-menu prompt-len (edit-buffer-col buf))))
                   (t nil)))))
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
