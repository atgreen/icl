;;; tests/buffer-tests.lisp --- Tests for buffer operations
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite buffer-tests
  :description "Tests for edit-buffer structure and operations"
  :in icl-tests)

(in-suite buffer-tests)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Accessors
;;; ─────────────────────────────────────────────────────────────────────────────

(test buffer-initial-state
  "A fresh buffer has one empty line with cursor at 0,0."
  (let ((buf (icl::make-edit-buffer)))
    (is (= 1 (icl::buffer-line-count buf)))
    (is (string= "" (icl::buffer-current-line buf)))
    (is (= 0 (icl::edit-buffer-row buf)))
    (is (= 0 (icl::edit-buffer-col buf)))))

(test buffer-line-count-single
  "Buffer with one line reports count 1."
  (let ((buf (icl::make-edit-buffer)))
    (is (= 1 (icl::buffer-line-count buf)))))

(test buffer-line-accessor
  "buffer-line returns the correct line by index."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (is (string= "a" (icl::buffer-line buf 0)))))

(test buffer-prompt-for-line-zero
  "Line 0 gets the primary prompt."
  (let ((buf (icl::make-edit-buffer :prompt ">>> " :continuation-prompt "... ")))
    (is (string= ">>> " (icl::buffer-prompt-for-line buf 0)))))

(test buffer-prompt-for-continuation
  "Lines > 0 get the continuation prompt."
  (let ((buf (icl::make-edit-buffer :prompt ">>> " :continuation-prompt "... ")))
    (is (string= "... " (icl::buffer-prompt-for-line buf 1)))))

(test buffer-line-length-empty
  "Empty buffer line has length 0."
  (let ((buf (icl::make-edit-buffer)))
    (is (= 0 (icl::buffer-line-length buf)))))

(test buffer-line-length-after-insert
  "Line length updates after character insertion."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (is (= 2 (icl::buffer-line-length buf)))))

(test buffer-contents-empty
  "Empty buffer contents is empty string."
  (let ((buf (icl::make-edit-buffer)))
    (is (string= "" (icl::buffer-contents buf)))))

(test buffer-contents-single-line
  "Single-line buffer contents."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\h)
    (icl::buffer-insert-char buf #\i)
    (is (string= "hi" (icl::buffer-contents buf)))))

(test buffer-empty-p-true
  "Fresh buffer is empty."
  (let ((buf (icl::make-edit-buffer)))
    (is (icl::buffer-empty-p buf))))

(test buffer-empty-p-false
  "Buffer with content is not empty."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\x)
    (is (not (icl::buffer-empty-p buf)))))

(test buffer-cursor-position-start
  "Cursor position at start is 0."
  (let ((buf (icl::make-edit-buffer)))
    (is (= 0 (icl::buffer-cursor-position buf)))))

(test buffer-cursor-position-after-insert
  "Cursor position advances with insertions."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (is (= 2 (icl::buffer-cursor-position buf)))))

(test buffer-at-end-p-empty
  "Cursor at end of empty buffer."
  (let ((buf (icl::make-edit-buffer)))
    (is (icl::buffer-at-end-p buf))))

(test buffer-at-end-p-after-move-left
  "Cursor not at end after moving left."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-move-left buf)
    (is (not (icl::buffer-at-end-p buf)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Cursor Movement
;;; ─────────────────────────────────────────────────────────────────────────────

(test buffer-move-left-at-start
  "Moving left at start of buffer returns NIL."
  (let ((buf (icl::make-edit-buffer)))
    (is (null (icl::buffer-move-left buf)))))

(test buffer-move-left-within-line
  "Moving left within a line decrements column."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (is (icl::buffer-move-left buf))
    (is (= 0 (icl::edit-buffer-col buf)))))

(test buffer-move-right-at-end
  "Moving right at end of buffer returns NIL."
  (let ((buf (icl::make-edit-buffer)))
    (is (null (icl::buffer-move-right buf)))))

(test buffer-move-right-within-line
  "Moving right within a line increments column."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-move-left buf)
    (is (icl::buffer-move-right buf))
    (is (= 1 (icl::edit-buffer-col buf)))))

(test buffer-move-up-at-top
  "Moving up at top returns NIL."
  (let ((buf (icl::make-edit-buffer)))
    (is (null (icl::buffer-move-up buf)))))

(test buffer-move-down-at-bottom
  "Moving down at bottom returns NIL."
  (let ((buf (icl::make-edit-buffer)))
    (is (null (icl::buffer-move-down buf)))))

(test buffer-move-to-line-start
  "Move to line start sets column to 0."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (icl::buffer-move-to-line-start buf)
    (is (= 0 (icl::edit-buffer-col buf)))))

(test buffer-move-to-line-end
  "Move to line end sets column to line length."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (icl::buffer-move-to-line-start buf)
    (icl::buffer-move-to-line-end buf)
    (is (= 2 (icl::edit-buffer-col buf)))))

(test buffer-move-to-end
  "Move to end positions cursor at last line, last col."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-open-line buf)
    ;; open-line keeps cursor on line 0; move to line 1 to insert there
    (setf (icl::edit-buffer-row buf) 1)
    (setf (icl::edit-buffer-col buf) 0)
    (icl::buffer-insert-char buf #\b)
    (icl::buffer-insert-char buf #\c)
    ;; Move to start
    (setf (icl::edit-buffer-row buf) 0)
    (setf (icl::edit-buffer-col buf) 0)
    ;; Move to end
    (icl::buffer-move-to-end buf)
    (is (= 1 (icl::edit-buffer-row buf)))
    (is (= 2 (icl::edit-buffer-col buf)))))

(test buffer-move-left-wraps-to-prev-line
  "Moving left at start of line 1 wraps to end of line 0."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (icl::buffer-open-line buf)
    ;; Now on line 1, col 0
    (setf (icl::edit-buffer-row buf) 1)
    (setf (icl::edit-buffer-col buf) 0)
    (is (icl::buffer-move-left buf))
    (is (= 0 (icl::edit-buffer-row buf)))
    (is (= 2 (icl::edit-buffer-col buf)))))

(test buffer-move-right-wraps-to-next-line
  "Moving right at end of line 0 wraps to start of line 1."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-open-line buf)
    ;; open-line keeps cursor on line 0; move to line 1 to insert there
    (setf (icl::edit-buffer-row buf) 1)
    (setf (icl::edit-buffer-col buf) 0)
    (icl::buffer-insert-char buf #\b)
    ;; Go to end of line 0
    (setf (icl::edit-buffer-row buf) 0)
    (setf (icl::edit-buffer-col buf) 1)
    (is (icl::buffer-move-right buf))
    (is (= 1 (icl::edit-buffer-row buf)))
    (is (= 0 (icl::edit-buffer-col buf)))))

(test buffer-move-up-clamps-column
  "Moving up clamps column to shorter line length."
  (let ((buf (icl::make-edit-buffer)))
    ;; Line 0: "a"
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-open-line buf)
    ;; Line 1: "bcd"
    (setf (icl::edit-buffer-row buf) 1)
    (setf (icl::edit-buffer-col buf) 0)
    (icl::buffer-insert-char buf #\b)
    (icl::buffer-insert-char buf #\c)
    (icl::buffer-insert-char buf #\d)
    ;; col is 3 on line 1, line 0 has length 1
    (is (icl::buffer-move-up buf))
    (is (= 0 (icl::edit-buffer-row buf)))
    (is (= 1 (icl::edit-buffer-col buf)))))

(test buffer-move-down-clamps-column
  "Moving down clamps column to shorter line length."
  (let ((buf (icl::make-edit-buffer)))
    ;; Line 0: "abcd"
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (icl::buffer-insert-char buf #\c)
    (icl::buffer-insert-char buf #\d)
    (icl::buffer-open-line buf)
    ;; Line 1: "e"
    (setf (icl::edit-buffer-row buf) 1)
    (setf (icl::edit-buffer-col buf) 0)
    (icl::buffer-insert-char buf #\e)
    ;; Go back to line 0, col 4
    (setf (icl::edit-buffer-row buf) 0)
    (setf (icl::edit-buffer-col buf) 4)
    (is (icl::buffer-move-down buf))
    (is (= 1 (icl::edit-buffer-row buf)))
    (is (= 1 (icl::edit-buffer-col buf)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Text Insertion
;;; ─────────────────────────────────────────────────────────────────────────────

(test buffer-insert-char-at-start
  "Insert char at start of empty buffer."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\x)
    (is (string= "x" (icl::buffer-current-line buf)))
    (is (= 1 (icl::edit-buffer-col buf)))))

(test buffer-insert-char-in-middle
  "Insert char in middle of existing text."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\c)
    (icl::buffer-move-left buf)
    (icl::buffer-insert-char buf #\b)
    (is (string= "abc" (icl::buffer-current-line buf)))))

(test buffer-open-line
  "Open line splits line at cursor, keeps cursor in place."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    ;; Move cursor between a and b
    (icl::buffer-move-left buf)
    (icl::buffer-open-line buf)
    (is (= 2 (icl::buffer-line-count buf)))
    (is (string= "a" (icl::buffer-line buf 0)))
    (is (string= "b" (icl::buffer-line buf 1)))
    ;; Cursor stays on line 0
    (is (= 0 (icl::edit-buffer-row buf)))))

(test buffer-contents-up-to
  "buffer-contents-up-to returns partial content."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (icl::buffer-open-line buf)
    (setf (icl::edit-buffer-row buf) 1)
    (setf (icl::edit-buffer-col buf) 0)
    (icl::buffer-insert-char buf #\c)
    (icl::buffer-insert-char buf #\d)
    ;; Get content up to row 1, col 1
    (let ((content (icl::buffer-contents-up-to buf 1 1)))
      (is (string= (format nil "ab~%c") content)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Text Deletion
;;; ─────────────────────────────────────────────────────────────────────────────

(test buffer-delete-char-before-at-start
  "Backspace at start of buffer returns NIL."
  (let ((buf (icl::make-edit-buffer)))
    (is (null (icl::buffer-delete-char-before buf)))))

(test buffer-delete-char-before-within-line
  "Backspace within a line deletes the previous character."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (is (icl::buffer-delete-char-before buf))
    (is (string= "a" (icl::buffer-current-line buf)))
    (is (= 1 (icl::edit-buffer-col buf)))))

(test buffer-delete-char-before-joins-lines
  "Backspace at start of line joins with previous line."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-open-line buf)
    (setf (icl::edit-buffer-row buf) 1)
    (setf (icl::edit-buffer-col buf) 0)
    (icl::buffer-insert-char buf #\b)
    ;; Move to start of line 1
    (icl::buffer-move-to-line-start buf)
    (is (icl::buffer-delete-char-before buf))
    (is (= 1 (icl::buffer-line-count buf)))
    (is (string= "ab" (icl::buffer-current-line buf)))
    (is (= 0 (icl::edit-buffer-row buf)))
    (is (= 1 (icl::edit-buffer-col buf)))))

(test buffer-delete-char-at-end
  "Delete at end of buffer returns NIL."
  (let ((buf (icl::make-edit-buffer)))
    (is (null (icl::buffer-delete-char-at buf)))))

(test buffer-delete-char-at-within-line
  "Delete within line removes char at cursor."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (icl::buffer-move-to-line-start buf)
    (is (icl::buffer-delete-char-at buf))
    (is (string= "b" (icl::buffer-current-line buf)))))

(test buffer-delete-char-at-joins-lines
  "Delete at end of line joins with next line."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-open-line buf)
    (setf (icl::edit-buffer-row buf) 1)
    (setf (icl::edit-buffer-col buf) 0)
    (icl::buffer-insert-char buf #\b)
    ;; Go to end of line 0
    (setf (icl::edit-buffer-row buf) 0)
    (icl::buffer-move-to-line-end buf)
    (is (icl::buffer-delete-char-at buf))
    (is (= 1 (icl::buffer-line-count buf)))
    (is (string= "ab" (icl::buffer-current-line buf)))))

(test buffer-kill-line
  "Kill line removes text from cursor to end."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (icl::buffer-insert-char buf #\c)
    (setf (icl::edit-buffer-col buf) 1)
    (icl::buffer-kill-line buf)
    (is (string= "a" (icl::buffer-current-line buf)))))

(test buffer-clear-line
  "Clear line empties current line and resets column."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (icl::buffer-clear-line buf)
    (is (string= "" (icl::buffer-current-line buf)))
    (is (= 0 (icl::edit-buffer-col buf)))))

(test buffer-clear
  "Clear buffer resets to single empty line."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-open-line buf)
    (setf (icl::edit-buffer-row buf) 1)
    (setf (icl::edit-buffer-col buf) 0)
    (icl::buffer-insert-char buf #\b)
    (icl::buffer-clear buf)
    (is (= 1 (icl::buffer-line-count buf)))
    (is (string= "" (icl::buffer-line buf 0)))
    (is (= 0 (icl::edit-buffer-row buf)))
    (is (= 0 (icl::edit-buffer-col buf)))))

(test buffer-transpose-chars-at-end
  "Transpose at end swaps last two characters."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    ;; cursor at col 2 = end of "ab"
    (is (icl::buffer-transpose-chars buf))
    (is (string= "ba" (icl::buffer-current-line buf)))))

(test buffer-transpose-chars-in-middle
  "Transpose in middle swaps char before cursor with char at cursor."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (icl::buffer-insert-char buf #\c)
    (setf (icl::edit-buffer-col buf) 1)
    (is (icl::buffer-transpose-chars buf))
    (is (string= "bac" (icl::buffer-current-line buf)))
    (is (= 2 (icl::edit-buffer-col buf)))))

(test buffer-transpose-chars-too-short
  "Transpose with less than 2 chars returns NIL."
  (let ((buf (icl::make-edit-buffer)))
    (is (null (icl::buffer-transpose-chars buf)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Word Operations
;;; ─────────────────────────────────────────────────────────────────────────────

(test word-char-p-alpha
  "Alphanumeric characters are word chars."
  (is (icl::word-char-p #\a))
  (is (icl::word-char-p #\Z))
  (is (icl::word-char-p #\5)))

(test word-char-p-special
  "Dash, underscore, asterisk, plus are word chars."
  (is (icl::word-char-p #\-))
  (is (icl::word-char-p #\_))
  (is (icl::word-char-p #\*))
  (is (icl::word-char-p #\+)))

(test word-char-p-non-word
  "Spaces and parens are not word chars."
  (is (not (icl::word-char-p #\Space)))
  (is (not (icl::word-char-p #\())))

(test buffer-kill-word-forward-basic
  "Kill word forward removes the next word."
  (let ((buf (icl::make-edit-buffer)))
    (dolist (c (coerce "hello world" 'list))
      (icl::buffer-insert-char buf c))
    (setf (icl::edit-buffer-col buf) 0)
    (is (icl::buffer-kill-word-forward buf))
    (is (string= " world" (icl::buffer-current-line buf)))))

(test buffer-kill-word-forward-at-end
  "Kill word forward at end of line returns NIL."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (is (null (icl::buffer-kill-word-forward buf)))))

(test buffer-kill-word-backward-basic
  "Kill word backward removes the previous word."
  (let ((buf (icl::make-edit-buffer)))
    (dolist (c (coerce "hello world" 'list))
      (icl::buffer-insert-char buf c))
    ;; cursor at end (col 11)
    (is (icl::buffer-kill-word-backward buf))
    (is (string= "hello " (icl::buffer-current-line buf)))))

(test buffer-kill-word-backward-at-start
  "Kill word backward at start of line returns NIL."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (setf (icl::edit-buffer-col buf) 0)
    (is (null (icl::buffer-kill-word-backward buf)))))
