;;; buffer.lisp --- Multi-line buffer for editing
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Buffer Structure
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (edit-buffer (:constructor %make-edit-buffer))
  "Multi-line editing buffer with cursor tracking."
  (lines (vector "") :type vector)  ; Vector of strings, one per line
  (row 0 :type fixnum)              ; Current line (0-indexed)
  (col 0 :type fixnum)              ; Current column (0-indexed)
  (prompt "" :type string)          ; Primary prompt
  (continuation-prompt "" :type string)) ; Continuation line prompt

(defun make-edit-buffer (&key (prompt "") (continuation-prompt ""))
  "Create a new edit buffer with a single empty line."
  (%make-edit-buffer
   :lines (make-array 1 :initial-element "" :adjustable t :fill-pointer 1)
   :row 0
   :col 0
   :prompt prompt
   :continuation-prompt continuation-prompt))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Buffer Accessors
;;; ─────────────────────────────────────────────────────────────────────────────

(defun buffer-line-count (buf)
  "Return the number of lines in BUF."
  (length (edit-buffer-lines buf)))

(defun buffer-current-line (buf)
  "Return the current line string."
  (aref (edit-buffer-lines buf) (edit-buffer-row buf)))

(defun (setf buffer-current-line) (new-line buf)
  "Set the current line string."
  (setf (aref (edit-buffer-lines buf) (edit-buffer-row buf)) new-line))

(defun buffer-line (buf n)
  "Return line N (0-indexed)."
  (aref (edit-buffer-lines buf) n))

(defun buffer-prompt-for-line (buf n)
  "Return the appropriate prompt for line N."
  (if (zerop n)
      (edit-buffer-prompt buf)
      (edit-buffer-continuation-prompt buf)))

(defun buffer-line-length (buf &optional (n (edit-buffer-row buf)))
  "Return length of line N."
  (length (buffer-line buf n)))

(defun buffer-contents (buf)
  "Return buffer contents as a single string with newlines."
  (format nil "~{~A~^~%~}"
          (coerce (edit-buffer-lines buf) 'list)))

(defun buffer-empty-p (buf)
  "Return T if buffer has only one empty line."
  (and (= 1 (buffer-line-count buf))
       (zerop (length (buffer-line buf 0)))))

(defun buffer-cursor-position (buf)
  "Return the absolute cursor position in the buffer content string.
   This counts characters from the start, including newlines."
  (let ((pos 0)
        (target-row (edit-buffer-row buf))
        (target-col (edit-buffer-col buf)))
    ;; Add lengths of all lines before cursor row (plus newlines)
    (dotimes (i target-row)
      (incf pos (length (buffer-line buf i)))
      (incf pos))  ; +1 for newline
    ;; Add column position in current row
    (incf pos target-col)
    pos))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Cursor Movement
;;; ─────────────────────────────────────────────────────────────────────────────

(defun buffer-move-left (buf)
  "Move cursor left one character. Returns T if moved."
  (cond
    ;; Can move within line
    ((plusp (edit-buffer-col buf))
     (decf (edit-buffer-col buf))
     t)
    ;; Can move to end of previous line
    ((plusp (edit-buffer-row buf))
     (decf (edit-buffer-row buf))
     (setf (edit-buffer-col buf) (buffer-line-length buf))
     t)
    ;; At beginning of buffer
    (t nil)))

(defun buffer-move-right (buf)
  "Move cursor right one character. Returns T if moved."
  (let ((line-len (buffer-line-length buf)))
    (cond
      ;; Can move within line
      ((< (edit-buffer-col buf) line-len)
       (incf (edit-buffer-col buf))
       t)
      ;; Can move to beginning of next line
      ((< (edit-buffer-row buf) (1- (buffer-line-count buf)))
       (incf (edit-buffer-row buf))
       (setf (edit-buffer-col buf) 0)
       t)
      ;; At end of buffer
      (t nil))))

(defun buffer-move-up (buf)
  "Move cursor up one line. Returns T if moved."
  (when (plusp (edit-buffer-row buf))
    (decf (edit-buffer-row buf))
    ;; Clamp column to line length
    (setf (edit-buffer-col buf)
          (min (edit-buffer-col buf) (buffer-line-length buf)))
    t))

(defun buffer-move-down (buf)
  "Move cursor down one line. Returns T if moved."
  (when (< (edit-buffer-row buf) (1- (buffer-line-count buf)))
    (incf (edit-buffer-row buf))
    ;; Clamp column to line length
    (setf (edit-buffer-col buf)
          (min (edit-buffer-col buf) (buffer-line-length buf)))
    t))

(defun buffer-move-to-line-start (buf)
  "Move cursor to start of current line."
  (setf (edit-buffer-col buf) 0))

(defun buffer-move-to-line-end (buf)
  "Move cursor to end of current line."
  (setf (edit-buffer-col buf) (buffer-line-length buf)))

(defun buffer-move-to-start (buf)
  "Move cursor to start of buffer."
  (setf (edit-buffer-row buf) 0
        (edit-buffer-col buf) 0))

(defun buffer-move-to-end (buf)
  "Move cursor to end of buffer."
  (setf (edit-buffer-row buf) (1- (buffer-line-count buf)))
  (buffer-move-to-line-end buf))

(defun buffer-at-end-p (buf)
  "Return T if cursor is at the end of the buffer."
  (and (= (edit-buffer-row buf) (1- (buffer-line-count buf)))
       (= (edit-buffer-col buf) (length (buffer-current-line buf)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Text Insertion
;;; ─────────────────────────────────────────────────────────────────────────────

(defun buffer-insert-char (buf char)
  "Insert CHAR at cursor position."
  (let* ((line (buffer-current-line buf))
         (col (edit-buffer-col buf))
         (new-line (concatenate 'string
                                (subseq line 0 col)
                                (string char)
                                (subseq line col))))
    (setf (buffer-current-line buf) new-line)
    (incf (edit-buffer-col buf))))

(defun buffer-insert-newline (buf &key (auto-indent t))
  "Insert a newline at cursor position, splitting the current line.
   If AUTO-INDENT is T (default), indent the new line appropriately."
  (let* ((line (buffer-current-line buf))
         (col (edit-buffer-col buf))
         (before (subseq line 0 col))
         (after (string-left-trim '(#\Space #\Tab) (subseq line col)))
         (row (edit-buffer-row buf))
         (lines (edit-buffer-lines buf))
         ;; Calculate indent based on text before cursor
         (indent (if auto-indent
                     (calculate-indent (buffer-contents-up-to buf row col))
                     0))
         (indent-str (make-string indent :initial-element #\Space)))
    ;; Update current line with text before cursor
    (setf (buffer-current-line buf) before)
    ;; Insert new line with text after cursor
    (vector-push-extend "" lines) ; Ensure space
    ;; Shift lines down
    (loop for i from (1- (length lines)) downto (+ row 2)
          do (setf (aref lines i) (aref lines (1- i))))
    ;; Insert the new line with indentation
    (setf (aref lines (1+ row)) (concatenate 'string indent-str after))
    ;; Move cursor to after indentation on new line
    (incf (edit-buffer-row buf))
    (setf (edit-buffer-col buf) indent)))

(defun buffer-contents-up-to (buf row col)
  "Return buffer contents up to (but not including) position ROW, COL."
  (let ((result nil))
    ;; Add complete lines before current row
    (dotimes (i row)
      (push (buffer-line buf i) result)
      (push (string #\Newline) result))
    ;; Add partial current line up to col
    (push (subseq (buffer-line buf row) 0 col) result)
    (apply #'concatenate 'string (nreverse result))))

(defun buffer-reindent (buf)
  "Reindent all lines in the buffer based on Lisp structure."
  (let* ((text (buffer-contents buf))
         (reindented (reindent-string text))
         (lines (split-string-by-newline reindented))
         (vec (make-array (length lines)
                          :initial-contents lines
                          :adjustable t
                          :fill-pointer (length lines))))
    (setf (edit-buffer-lines buf) vec)
    ;; Keep cursor at end
    (setf (edit-buffer-row buf) (1- (length lines)))
    (setf (edit-buffer-col buf) (length (aref vec (1- (length lines)))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Text Deletion
;;; ─────────────────────────────────────────────────────────────────────────────

(defun buffer-delete-char-before (buf)
  "Delete character before cursor (backspace). Returns T if deleted."
  (let ((col (edit-buffer-col buf))
        (row (edit-buffer-row buf)))
    (cond
      ;; Delete within line
      ((plusp col)
       (let* ((line (buffer-current-line buf))
              (new-line (concatenate 'string
                                     (subseq line 0 (1- col))
                                     (subseq line col))))
         (setf (buffer-current-line buf) new-line)
         (decf (edit-buffer-col buf)))
       t)
      ;; Join with previous line
      ((plusp row)
       (let* ((prev-line (buffer-line buf (1- row)))
              (curr-line (buffer-current-line buf))
              (join-col (length prev-line))
              (lines (edit-buffer-lines buf)))
         ;; Merge lines
         (setf (aref lines (1- row))
               (concatenate 'string prev-line curr-line))
         ;; Remove current line by shifting
         (loop for i from row below (1- (length lines))
               do (setf (aref lines i) (aref lines (1+ i))))
         (decf (fill-pointer lines))
         ;; Move cursor
         (decf (edit-buffer-row buf))
         (setf (edit-buffer-col buf) join-col))
       t)
      ;; At start of buffer
      (t nil))))

(defun buffer-delete-char-at (buf)
  "Delete character at cursor (delete key). Returns T if deleted."
  (let ((col (edit-buffer-col buf))
        (row (edit-buffer-row buf))
        (line-len (buffer-line-length buf)))
    (cond
      ;; Delete within line
      ((< col line-len)
       (let* ((line (buffer-current-line buf))
              (new-line (concatenate 'string
                                     (subseq line 0 col)
                                     (subseq line (1+ col)))))
         (setf (buffer-current-line buf) new-line))
       t)
      ;; Join with next line
      ((< row (1- (buffer-line-count buf)))
       (let* ((curr-line (buffer-current-line buf))
              (next-line (buffer-line buf (1+ row)))
              (lines (edit-buffer-lines buf)))
         ;; Merge lines
         (setf (buffer-current-line buf)
               (concatenate 'string curr-line next-line))
         ;; Remove next line by shifting
         (loop for i from (1+ row) below (1- (length lines))
               do (setf (aref lines i) (aref lines (1+ i))))
         (decf (fill-pointer lines)))
       t)
      ;; At end of buffer
      (t nil))))

(defun buffer-kill-line (buf)
  "Kill from cursor to end of line."
  (let* ((line (buffer-current-line buf))
         (col (edit-buffer-col buf)))
    (setf (buffer-current-line buf) (subseq line 0 col))))

(defun buffer-clear-line (buf)
  "Clear entire current line."
  (setf (buffer-current-line buf) "")
  (setf (edit-buffer-col buf) 0))

(defun buffer-clear (buf)
  "Clear entire buffer."
  (setf (edit-buffer-lines buf)
        (make-array 1 :initial-element "" :adjustable t :fill-pointer 1))
  (setf (edit-buffer-row buf) 0
        (edit-buffer-col buf) 0))

(defun buffer-transpose-chars (buf)
  "Transpose character before cursor with character at cursor.
   If at end of line, transpose the two characters before cursor.
   Returns T if transposed."
  (let* ((line (buffer-current-line buf))
         (col (edit-buffer-col buf))
         (len (length line)))
    (cond
      ;; At end of line with at least 2 chars - transpose last two
      ((and (= col len) (>= len 2))
       (let ((new-line (concatenate 'string
                                    (subseq line 0 (- len 2))
                                    (string (char line (1- len)))
                                    (string (char line (- len 2))))))
         (setf (buffer-current-line buf) new-line))
       t)
      ;; In middle of line with char before cursor
      ((and (plusp col) (< col len))
       (let ((new-line (concatenate 'string
                                    (subseq line 0 (1- col))
                                    (string (char line col))
                                    (string (char line (1- col)))
                                    (subseq line (1+ col)))))
         (setf (buffer-current-line buf) new-line)
         (incf (edit-buffer-col buf)))
       t)
      (t nil))))

(defun buffer-open-line (buf)
  "Insert newline after cursor, keeping cursor in place (Ctrl+O)."
  (let* ((line (buffer-current-line buf))
         (col (edit-buffer-col buf))
         (before (subseq line 0 col))
         (after (subseq line col))
         (row (edit-buffer-row buf))
         (lines (edit-buffer-lines buf)))
    ;; Update current line with text before cursor
    (setf (buffer-current-line buf) before)
    ;; Insert new line with text after cursor
    (vector-push-extend "" lines)
    ;; Shift lines down
    (loop for i from (1- (length lines)) downto (+ row 2)
          do (setf (aref lines i) (aref lines (1- i))))
    ;; Insert the new line
    (setf (aref lines (1+ row)) after)
    ;; Cursor stays in place
    t))

(defun word-char-p (char)
  "Return T if CHAR is a word constituent."
  (or (alphanumericp char)
      (char= char #\-)
      (char= char #\_)
      (char= char #\*)
      (char= char #\+)))

(defun buffer-kill-word-forward (buf)
  "Delete from cursor to end of current word. Returns T if deleted."
  (let* ((line (buffer-current-line buf))
         (col (edit-buffer-col buf))
         (len (length line)))
    (when (< col len)
      ;; Skip non-word chars first
      (let ((end col))
        (loop while (and (< end len) (not (word-char-p (char line end))))
              do (incf end))
        ;; Then skip word chars
        (loop while (and (< end len) (word-char-p (char line end)))
              do (incf end))
        (when (> end col)
          (setf (buffer-current-line buf)
                (concatenate 'string
                             (subseq line 0 col)
                             (subseq line end)))
          t)))))

(defun buffer-kill-word-backward (buf)
  "Delete from cursor back to start of previous word. Returns T if deleted."
  (let* ((line (buffer-current-line buf))
         (col (edit-buffer-col buf)))
    (when (plusp col)
      ;; Skip non-word chars first (going backward)
      (let ((start col))
        (loop while (and (plusp start) (not (word-char-p (char line (1- start)))))
              do (decf start))
        ;; Then skip word chars (going backward)
        (loop while (and (plusp start) (word-char-p (char line (1- start))))
              do (decf start))
        (when (< start col)
          (setf (buffer-current-line buf)
                (concatenate 'string
                             (subseq line 0 start)
                             (subseq line col)))
          (setf (edit-buffer-col buf) start)
          t)))))
