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

(defun load-history (&optional (session *current-session*))
  "Load history from history file into SESSION.
   If SESSION is nil, loads into global *editor-history* for backward compatibility."
  (let ((hfile (session-history-file session)))
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
                (if session
                    (setf (repl-session-history session) entries)
                    (setf *editor-history* entries))
                ;; Trim to max size
                (let ((hist (if session (repl-session-history session) *editor-history*)))
                  (when (> (length hist) *history-size*)
                    (let ((trimmed (subseq hist 0 *history-size*)))
                      (if session
                          (setf (repl-session-history session) trimmed)
                          (setf *editor-history* trimmed))))))))
        (error (e)
          (declare (ignore e))
          nil)))))

(defun save-history (&optional (session *current-session*))
  "Save SESSION's history to history file.
   If SESSION is nil, saves global *editor-history* for backward compatibility."
  (let ((hfile (session-history-file session))
        (hist (if session (repl-session-history session) *editor-history*)))
    (when (and hfile hist)
      (handler-case
          (ensure-directories-exist hfile)
        (error () nil))
      (handler-case
          (with-open-file (out hfile :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
            ;; Save newest-first (reverse to get oldest-first in file)
            (let ((to-save (if (> (length hist) *history-size*)
                               (subseq hist 0 *history-size*)
                               hist)))
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

(defvar *prefix-search-prefix* nil
  "The prefix for history-search-backward/forward (text before cursor when started).")

(defvar *prefix-search-index* nil
  "Current history index in prefix search.")

(defvar *screen-row* 0
  "Current screen row relative to line 0 of the buffer.")

(defvar *last-key-was-tab* nil
  "T if the last key pressed was Tab (for completion cycling).")

(defvar *completion-line-col* nil
  "Cons of (line . col) at start of completion for tracking changes.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Rendering
;;; ─────────────────────────────────────────────────────────────────────────────

(defun calculate-visual-rows (visible-len term-width)
  "Calculate the number of visual rows a line of VISIBLE-LEN chars occupies in a terminal of TERM-WIDTH.
   Returns at least 1 row even for empty lines."
  (if (<= visible-len 0)
      1
      (ceiling visible-len term-width)))

(defun calculate-cursor-visual-position (prompt-len col term-width)
  "Calculate the visual row offset and column for cursor at logical COL.
   PROMPT-LEN is the visible length of the prompt.
   Returns (values visual-row-offset visual-col) where visual-row-offset is 0-indexed
   from the start of this logical line."
  (let* ((total-offset (+ prompt-len col))
         (visual-row (floor total-offset term-width))
         (visual-col (mod total-offset term-width)))
    (values visual-row visual-col)))

(defun render-buffer (buf)
  "Render the buffer to the terminal, using *screen-row* to know current position."
  (let ((line-count (buffer-line-count buf))
        (cursor-row (edit-buffer-row buf))
        (cursor-col (edit-buffer-col buf))
        ;; Get full buffer content for multi-line highlighting
        (full-content (buffer-contents buf))
        ;; Get absolute cursor position for paren matching
        (cursor-pos (buffer-cursor-position buf))
        ;; Get terminal width for wrapping calculations
        (term-width (or (get-terminal-size) 80)))
    ;; Move up to line 0 from current screen position
    (when (plusp *screen-row*)
      (cursor-up *screen-row*))
    (cursor-to-column 1)
    ;; Highlight the full content with cursor position for paren matching
    (let* ((highlighted (highlight-string full-content cursor-pos))
           (highlighted-lines (split-sequence:split-sequence #\Newline highlighted)))
      ;; Draw each line
      (dotimes (i line-count)
        (let* ((prompt (buffer-prompt-for-line buf i))
               (hl-line (if (< i (length highlighted-lines))
                            (nth i highlighted-lines)
                            "")))
          (clear-line)
          (format t "~A~A" prompt hl-line)
          (when (< i (1- line-count))
            (format t "~%")))))
    ;; Clear any lines below (from previous longer input)
    (clear-below)
    ;; Position cursor accounting for line wrapping
    (let* ((prompt-len (visible-string-length (buffer-prompt-for-line buf cursor-row)))
           (line-visible-len (visible-string-length (buffer-line buf cursor-row)))
           (line-total-len (+ prompt-len line-visible-len))
           (line-visual-rows (calculate-visual-rows line-total-len term-width)))
      ;; Calculate visual rows after cursor's logical line
      (multiple-value-bind (cursor-row-offset cursor-visual-col)
          (calculate-cursor-visual-position prompt-len cursor-col term-width)
        ;; Calculate how many visual rows to go up from current position (end of buffer)
        ;; Current position is at end of last logical line
        (let* ((last-line-prompt-len (visible-string-length (buffer-prompt-for-line buf (1- line-count))))
               (last-line-len (visible-string-length (buffer-line buf (1- line-count))))
               (last-line-total (+ last-line-prompt-len last-line-len))
               (last-line-visual-rows (calculate-visual-rows last-line-total term-width))
               ;; We're at the last visual row of the last logical line
               ;; Need to go up to the correct visual row for cursor
               (visual-rows-from-end 0))
          ;; Count visual rows from end of buffer to cursor position
          ;; First, rows remaining in last logical line
          (incf visual-rows-from-end (1- last-line-visual-rows))
          ;; Then, visual rows for all logical lines between cursor's line and last line
          (loop for i from (1+ cursor-row) below (1- line-count)
                do (let* ((p-len (visible-string-length (buffer-prompt-for-line buf i)))
                          (l-len (visible-string-length (buffer-line buf i)))
                          (l-total (+ p-len l-len)))
                     (incf visual-rows-from-end (calculate-visual-rows l-total term-width))))
          ;; Finally, visual rows after cursor within cursor's logical line
          (incf visual-rows-from-end (- line-visual-rows cursor-row-offset 1))
          ;; Move cursor up
          (when (plusp visual-rows-from-end)
            (cursor-up visual-rows-from-end))
          ;; Position cursor at correct column (1-indexed)
          (cursor-to-column (1+ cursor-visual-col)))))
    ;; Update screen row tracking - this now tracks visual rows, not logical rows
    ;; For simplicity, track the cursor's logical row (used for going back to line 0)
    (let* ((prompt-len (visible-string-length (buffer-prompt-for-line buf cursor-row))))
      (multiple-value-bind (cursor-row-offset cursor-visual-col)
          (calculate-cursor-visual-position prompt-len cursor-col term-width)
        (declare (ignore cursor-visual-col))
        ;; Calculate total visual rows from line 0 to cursor position
        (let ((visual-rows 0))
          (dotimes (i cursor-row)
            (let* ((p-len (visible-string-length (buffer-prompt-for-line buf i)))
                   (l-len (visible-string-length (buffer-line buf i)))
                   (l-total (+ p-len l-len)))
              (incf visual-rows (calculate-visual-rows l-total term-width))))
          (incf visual-rows cursor-row-offset)
          (setf *screen-row* visual-rows))))
    (force-output)))

(defun render-buffer-final (buf)
  "Render the buffer without paren matching (for final display after submission)."
  (let ((line-count (buffer-line-count buf))
        (cursor-row (edit-buffer-row buf))
        (cursor-col (edit-buffer-col buf))
        (full-content (buffer-contents buf))
        (term-width (or (get-terminal-size) 80)))
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
    ;; Position cursor at end, accounting for wrapping
    (let* ((prompt-len (visible-string-length (buffer-prompt-for-line buf cursor-row)))
           (line-visible-len (visible-string-length (buffer-line buf cursor-row)))
           (line-total-len (+ prompt-len line-visible-len))
           (line-visual-rows (calculate-visual-rows line-total-len term-width)))
      (multiple-value-bind (cursor-row-offset cursor-visual-col)
          (calculate-cursor-visual-position prompt-len cursor-col term-width)
        ;; Calculate visual rows from end to cursor
        (let* ((last-line-prompt-len (visible-string-length (buffer-prompt-for-line buf (1- line-count))))
               (last-line-len (visible-string-length (buffer-line buf (1- line-count))))
               (last-line-total (+ last-line-prompt-len last-line-len))
               (last-line-visual-rows (calculate-visual-rows last-line-total term-width))
               (visual-rows-from-end 0))
          (incf visual-rows-from-end (1- last-line-visual-rows))
          (loop for i from (1+ cursor-row) below (1- line-count)
                do (let* ((p-len (visible-string-length (buffer-prompt-for-line buf i)))
                          (l-len (visible-string-length (buffer-line buf i)))
                          (l-total (+ p-len l-len)))
                     (incf visual-rows-from-end (calculate-visual-rows l-total term-width))))
          (incf visual-rows-from-end (- line-visual-rows cursor-row-offset 1))
          (when (plusp visual-rows-from-end)
            (cursor-up visual-rows-from-end))
          (cursor-to-column (1+ cursor-visual-col)))))
    ;; Update screen row tracking with visual rows
    (let* ((prompt-len (visible-string-length (buffer-prompt-for-line buf cursor-row))))
      (multiple-value-bind (cursor-row-offset cursor-visual-col)
          (calculate-cursor-visual-position prompt-len cursor-col term-width)
        (declare (ignore cursor-visual-col))
        (let ((visual-rows 0))
          (dotimes (i cursor-row)
            (let* ((p-len (visible-string-length (buffer-prompt-for-line buf i)))
                   (l-len (visible-string-length (buffer-line buf i)))
                   (l-total (+ p-len l-len)))
              (incf visual-rows (calculate-visual-rows l-total term-width))))
          (incf visual-rows cursor-row-offset)
          (setf *screen-row* visual-rows))))
    (force-output)))

(defun render-current-line (buf)
  "Render just the current line (optimization for single-line edits).
   For wrapped lines, this does a full redraw to handle visual row changes."
  (let* ((row (edit-buffer-row buf))
         (col (edit-buffer-col buf))
         (prompt (buffer-prompt-for-line buf row))
         (line (buffer-line buf row))
         (prompt-len (visible-string-length prompt))
         (term-width (or (get-terminal-size) 80))
         (line-total-len (+ prompt-len (length line))))
    ;; If line wraps, we need a full redraw to handle visual rows correctly
    (if (> line-total-len term-width)
        (render-buffer buf)
        ;; Simple case: line fits in one visual row
        (let* ((full-content (buffer-contents buf))
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
          (force-output)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; History
;;; ─────────────────────────────────────────────────────────────────────────────

(defun history-add (input &optional (session *current-session*))
  "Add INPUT to SESSION's history if non-empty and different from last entry."
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) input))
        (hist (if session (repl-session-history session) *editor-history*)))
    (when (and (plusp (length trimmed))
               (or (null hist)
                   (string/= trimmed (first hist))))
      (if session
          (push trimmed (repl-session-history session))
          (push trimmed *editor-history*)))))

(defun history-previous (buf &optional (session *current-session*))
  "Navigate to previous history entry in SESSION."
  (let ((hist (if session (repl-session-history session) *editor-history*))
        (idx (if session (repl-session-history-index session) *history-index*)))
    (when (null idx)
      ;; Save current buffer
      (if session
          (setf (repl-session-history-saved-buffer session) (buffer-contents buf)
                (repl-session-history-index session) -1)
          (setf *history-saved-buffer* (buffer-contents buf)
                *history-index* -1))
      (setf idx -1))
    (when (< (1+ idx) (length hist))
      (incf idx)
      (if session
          (setf (repl-session-history-index session) idx)
          (setf *history-index* idx))
      (buffer-set-contents buf (nth idx hist))
      t)))

(defun history-next (buf &optional (session *current-session*))
  "Navigate to next history entry (or back to current input) in SESSION."
  (let ((idx (if session (repl-session-history-index session) *history-index*))
        (saved (if session (repl-session-history-saved-buffer session) *history-saved-buffer*))
        (hist (if session (repl-session-history session) *editor-history*)))
    (when idx
      (cond
        ((zerop idx)
         ;; Restore original input
         (buffer-set-contents buf saved)
         (if session
             (setf (repl-session-history-index session) nil
                   (repl-session-history-saved-buffer session) nil)
             (setf *history-index* nil
                   *history-saved-buffer* nil))
         t)
        (t
         (decf idx)
         (if session
             (setf (repl-session-history-index session) idx)
             (setf *history-index* idx))
         (buffer-set-contents buf (nth idx hist))
         t)))))

(defun reset-prefix-search (&optional (session *current-session*))
  "Reset prefix search state for SESSION."
  (if session
      (setf (repl-session-prefix-search-prefix session) nil
            (repl-session-prefix-search-index session) nil)
      (setf *prefix-search-prefix* nil
            *prefix-search-index* nil)))

(defun history-search-backward (buf &optional (session *current-session*))
  "Search backward for history entry starting with current line content in SESSION.
   On first call, saves the prefix. Subsequent calls find older matches."
  (let ((current-content (buffer-contents buf))
        (hist (if session (repl-session-history session) *editor-history*))
        (prefix-search-prefix (if session (repl-session-prefix-search-prefix session) *prefix-search-prefix*))
        (prefix-search-index (if session (repl-session-prefix-search-index session) *prefix-search-index*))
        (saved-buffer (if session (repl-session-history-saved-buffer session) *history-saved-buffer*)))
    ;; Initialize search on first call or if content changed
    (when (or (null prefix-search-prefix)
              (not (and prefix-search-index
                        (alexandria:starts-with-subseq prefix-search-prefix current-content))))
      ;; Save current buffer if starting fresh
      (unless saved-buffer
        (if session
            (setf (repl-session-history-saved-buffer session) current-content)
            (setf *history-saved-buffer* current-content)))
      (if session
          (setf (repl-session-prefix-search-prefix session) current-content
                (repl-session-prefix-search-index session) -1)
          (setf *prefix-search-prefix* current-content
                *prefix-search-index* -1))
      (setf prefix-search-prefix current-content
            prefix-search-index -1))
    ;; Search for next match
    (let ((prefix (string-downcase prefix-search-prefix)))
      (loop for i from (1+ prefix-search-index) below (length hist)
            for entry = (nth i hist)
            when (alexandria:starts-with-subseq prefix (string-downcase entry))
            do (if session
                   (setf (repl-session-prefix-search-index session) i)
                   (setf *prefix-search-index* i))
               (buffer-set-contents buf entry)
               (return t)
            finally (return nil)))))

(defun history-search-forward (buf &optional (session *current-session*))
  "Search forward for history entry starting with the prefix in SESSION.
   Moves toward more recent entries."
  (let ((prefix-search-prefix (if session (repl-session-prefix-search-prefix session) *prefix-search-prefix*))
        (prefix-search-index (if session (repl-session-prefix-search-index session) *prefix-search-index*))
        (saved-buffer (if session (repl-session-history-saved-buffer session) *history-saved-buffer*))
        (hist (if session (repl-session-history session) *editor-history*)))
    (when (and prefix-search-prefix prefix-search-index)
      (if (zerop prefix-search-index)
          ;; Restore original input
          (progn
            (buffer-set-contents buf saved-buffer)
            (reset-prefix-search session)
            (if session
                (setf (repl-session-history-saved-buffer session) nil)
                (setf *history-saved-buffer* nil))
            t)
          ;; Search for previous (more recent) match
          (let ((prefix (string-downcase prefix-search-prefix)))
            (loop for i from (1- prefix-search-index) downto 0
                  for entry = (nth i hist)
                  when (alexandria:starts-with-subseq prefix (string-downcase entry))
                  do (if session
                         (setf (repl-session-prefix-search-index session) i)
                         (setf *prefix-search-index* i))
                     (buffer-set-contents buf entry)
                     (return t)
                  finally
                  ;; No more matches - restore original
                  (buffer-set-contents buf saved-buffer)
                  (reset-prefix-search session)
                  (if session
                      (setf (repl-session-history-saved-buffer session) nil)
                      (setf *history-saved-buffer* nil))
                  (return t)))))))

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

(defun search-history-for (pattern &optional (session *current-session*))
  "Find all history entries containing PATTERN (case-insensitive) in SESSION.
   Returns list of indices into session's history."
  (when (plusp (length pattern))
    (let ((pat (string-downcase pattern))
          (hist (if session (repl-session-history session) *editor-history*))
          (matches nil))
      (loop for entry in hist
            for i from 0
            when (search pat (string-downcase entry))
            do (push i matches))
      (nreverse matches))))

(defun enter-search-mode (buf &optional (session *current-session*))
  "Enter reverse-search mode for SESSION."
  (if session
      (setf (repl-session-search-mode session) t
            (repl-session-search-pattern session) ""
            (repl-session-search-match-index session) 0
            (repl-session-search-matches session) nil)
      (setf *search-mode* t
            *search-pattern* ""
            *search-match-index* 0
            *search-matches* nil))
  (setf *search-display-lines* 0)
  ;; Save current buffer
  (let ((saved (if session (repl-session-history-saved-buffer session) *history-saved-buffer*)))
    (unless saved
      (if session
          (setf (repl-session-history-saved-buffer session) (buffer-contents buf))
          (setf *history-saved-buffer* (buffer-contents buf))))))

(defun clear-search-display ()
  "Clear the search display area and move cursor to start."
  ;; Move up past the buffer lines to the search prompt line
  (when (plusp *search-display-lines*)
    (cursor-up *search-display-lines*))
  (cursor-to-column 1)
  ;; Clear the search prompt line and everything below
  (clear-line)
  (clear-below)
  (setf *search-display-lines* 0))

(defun exit-search-mode (buf accept &optional (session *current-session*))
  "Exit reverse-search mode for SESSION.
   If ACCEPT is T, keep the current match in buffer."
  (let ((matches (if session (repl-session-search-matches session) *search-matches*))
        (match-idx (if session (repl-session-search-match-index session) *search-match-index*))
        (hist (if session (repl-session-history session) *editor-history*)))
    (when (and accept matches (< match-idx (length matches)))
      (let* ((hist-idx (nth match-idx matches))
             (entry (nth hist-idx hist)))
        (buffer-set-contents buf entry))))
  ;; Clear the search display before exiting
  (clear-search-display)
  (if session
      (setf (repl-session-search-mode session) nil
            (repl-session-search-pattern session) ""
            (repl-session-search-match-index session) 0
            (repl-session-search-matches session) nil
            (repl-session-history-saved-buffer session) nil)
      (setf *search-mode* nil
            *search-pattern* ""
            *search-match-index* 0
            *search-matches* nil
            *history-saved-buffer* nil)))

(defun update-search (buf &optional (session *current-session*))
  "Update search matches based on current pattern and update buffer for SESSION."
  (let* ((pattern (if session (repl-session-search-pattern session) *search-pattern*))
         (hist (if session (repl-session-history session) *editor-history*))
         (saved (if session (repl-session-history-saved-buffer session) *history-saved-buffer*))
         (matches (search-history-for pattern session)))
    (if session
        (setf (repl-session-search-matches session) matches
              (repl-session-search-match-index session) 0)
        (setf *search-matches* matches
              *search-match-index* 0))
    (if matches
        (let* ((hist-idx (first matches))
               (entry (nth hist-idx hist)))
          (buffer-set-contents buf entry))
        ;; No matches - restore original
        (when saved
          (buffer-set-contents buf saved)))))

(defun search-next-match (buf &optional (session *current-session*))
  "Move to next match in search results for SESSION."
  (let ((matches (if session (repl-session-search-matches session) *search-matches*))
        (match-idx (if session (repl-session-search-match-index session) *search-match-index*))
        (hist (if session (repl-session-history session) *editor-history*)))
    (when (and matches (< (1+ match-idx) (length matches)))
      (incf match-idx)
      (if session
          (setf (repl-session-search-match-index session) match-idx)
          (setf *search-match-index* match-idx))
      (let* ((hist-idx (nth match-idx matches))
             (entry (nth hist-idx hist)))
        (buffer-set-contents buf entry)))))

(defvar *search-display-lines* 0
  "Number of lines used by current search display (for redrawing in place).")

(defun render-search-prompt (&optional (session *current-session*))
  "Render just the search prompt (for initial entry into search mode)."
  (let ((pattern (if session (repl-session-search-pattern session) *search-pattern*)))
    ;; Move up to clear previous display if any
    (when (plusp *search-display-lines*)
      (cursor-up *search-display-lines*))
    (cursor-to-column 1)
    (clear-line)
    (format t "~A`~A'~A"
            (colorize "(reverse-i-search)" *color-dim*)
            pattern
            (colorize "[no match]" *color-red*))
    (clear-below)
    (setf *search-display-lines* 0)
    (force-output)))

(defun render-search-with-buffer (buf &optional (session *current-session*))
  "Render the search prompt and full buffer content, updating in place."
  ;; Move up to start of our display area
  (when (plusp *search-display-lines*)
    (cursor-up *search-display-lines*))
  (cursor-to-column 1)

  ;; Line 0: Search prompt
  (clear-line)
  (let* ((matches (if session (repl-session-search-matches session) *search-matches*))
         (match-idx (if session (repl-session-search-match-index session) *search-match-index*))
         (pattern (if session (repl-session-search-pattern session) *search-pattern*))
         (status (if matches
                     (format nil "~D/~D" (1+ match-idx) (length matches))
                     "no match")))
    (format t "~A`~A'~A"
            (colorize "(reverse-i-search)" *color-dim*)
            pattern
            (colorize (format nil "[~A]" status)
                      (if matches *color-green* *color-red*))))

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

(defun handle-search-key (buf key &optional (session *current-session*))
  "Handle KEY in reverse-search mode for SESSION.
   Returns :continue, :accept, :cancel, or :next."
  (cond
    ;; Ctrl-R - next match
    ((eql key :reverse-search)
     (search-next-match buf session)
     :next)
    ;; Enter - accept and exit
    ((eql key :enter)
     :accept)
    ;; Escape or Ctrl-G - cancel
    ((or (eql key :escape) (eql key :cancel-search))
     :cancel)
    ;; Backspace - delete from pattern
    ((eql key :backspace)
     (let ((pattern (if session (repl-session-search-pattern session) *search-pattern*)))
       (when (plusp (length pattern))
         (let ((new-pattern (subseq pattern 0 (1- (length pattern)))))
           (if session
               (setf (repl-session-search-pattern session) new-pattern)
               (setf *search-pattern* new-pattern)))
         (update-search buf session)))
     :continue)
    ;; Regular character - add to pattern
    ((characterp key)
     (let ((pattern (if session (repl-session-search-pattern session) *search-pattern*)))
       (let ((new-pattern (concatenate 'string pattern (string key))))
         (if session
             (setf (repl-session-search-pattern session) new-pattern)
             (setf *search-pattern* new-pattern))))
     (update-search buf session)
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

(defun handle-key (buf key &optional (session *current-session*))
  "Handle KEY input, updating BUF for SESSION. Returns :done, :cancel, :continue, or :redraw."
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
         (when (history-previous buf session)
           :redraw)
         (progn
           (buffer-move-up buf)
           :redraw)))
    ((eql key :down)
     (if (= (edit-buffer-row buf) (1- (buffer-line-count buf)))
         ;; At last line - try history forward
         (when (history-next buf session)
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
     (reset-prefix-search session)
     (if (if *paredit-mode*
             (paredit-backspace buf)
             (buffer-delete-char-before buf))
         :redraw
         :continue))
    ((eql key :delete)
     (reset-prefix-search session)
     (if (if *paredit-mode*
             (paredit-delete buf)
             (buffer-delete-char-at buf))
         :redraw
         :continue))
    ;; Ctrl-D: delete char at cursor, or EOF if buffer empty (Emacs behavior)
    ((eql key :ctrl-d)
     (reset-prefix-search session)
     (if (buffer-empty-p buf)
         :eof
         (if (if *paredit-mode*
                 (paredit-delete buf)
                 (buffer-delete-char-at buf))
             :redraw
             :continue)))
    ((eql key :kill-line)
     (reset-prefix-search session)
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
    ;; Alt+P - history search backward (prefix match)
    ((and (consp key) (eql (first key) :alt) (char-equal (rest key) #\p))
     (when (history-search-backward buf session)
       :redraw))
    ;; Alt+N - history search forward (prefix match)
    ((and (consp key) (eql (first key) :alt) (char-equal (rest key) #\n))
     (when (history-search-forward buf session)
       :redraw))
    ;; Ctrl-R - enter reverse search mode
    ((eql key :reverse-search)
     (enter-search-mode buf session)
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
     ;; Reset prefix search since buffer is being modified
     (reset-prefix-search session)
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

(defun run-editor (prompt continuation-prompt &optional (session *current-session*))
  "Run the multi-line editor for SESSION.
   Returns the input string, :EOF on EOF, :CANCEL on interrupt, or :NOT-A-TTY if raw mode fails."
  (let ((buf (make-edit-buffer :prompt prompt
                               :continuation-prompt continuation-prompt)))
    ;; Reset global editor state (non-session-specific)
    (setf *screen-row* 0
          *last-key-was-tab* nil
          *completion-line-col* nil)
    ;; Reset session-local state
    (if session
        (setf (repl-session-history-index session) nil
              (repl-session-history-saved-buffer session) nil
              (repl-session-search-mode session) nil)
        (setf *history-index* nil
              *history-saved-buffer* nil
              *search-mode* nil))
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
                     (process-normal-key t)
                     (search-mode (if session (repl-session-search-mode session) *search-mode*)))
                 ;; Handle search mode separately
                 (when search-mode
                   (let ((search-result (handle-search-key buf key session)))
                     (cond
                       ((eql search-result :continue)
                        (render-search-with-buffer buf session)
                        (setf process-normal-key nil))
                       ((eql search-result :next)
                        (render-search-with-buffer buf session)
                        (setf process-normal-key nil))
                       ((eql search-result :accept)
                        (exit-search-mode buf t session)
                        (render-buffer buf)
                        (setf process-normal-key nil))
                       ((eql search-result :cancel)
                        ;; Restore original buffer
                        (let ((saved (if session (repl-session-history-saved-buffer session) *history-saved-buffer*)))
                          (when saved
                            (buffer-set-contents buf saved)))
                        (exit-search-mode buf nil session)
                        (render-buffer buf)
                        (setf process-normal-key nil))
                       ((eql search-result :accept-and-pass)
                        ;; Accept current match and process key normally
                        (exit-search-mode buf t session)
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
                                     (handle-key buf key session)
                                     :redraw)
                                    (menu-result menu-result)
                                    (t (handle-key buf key session)))))
                     ;; Handle result
                     (cond
                       ;; Search mode entered
                       ((eql result :search-mode)
                        (render-search-prompt session))
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
                          (history-add contents session)
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

(defun multiline-edit (&key (prompt "") (continuation-prompt "") (session *current-session*))
  "Read a complete Lisp form with multi-line editing for SESSION.
   Returns the input string, NIL on EOF, :CANCEL on interrupt, or :NOT-A-TTY if not a terminal."
  (if (terminal-capable-p)
      (handler-case
          (let ((result (run-editor prompt continuation-prompt session)))
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
