;;; tests/editor-tests.lisp --- Tests for editor pure functions
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite editor-tests
  :description "Tests for editor history encoding/decoding and visual calculations"
  :in icl-tests)

(in-suite editor-tests)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; History Encoding/Decoding
;;; ─────────────────────────────────────────────────────────────────────────────

(test encode-history-single-line
  "Single-line entry encodes unchanged."
  (is (string= "hello world" (icl::encode-history-entry "hello world"))))

(test encode-history-multi-line
  "Multi-line entry has newlines replaced with \\n."
  (is (string= "(defun f (x)\\n  (+ x 1))"
               (icl::encode-history-entry (format nil "(defun f (x)~%  (+ x 1))")))))

(test encode-history-empty
  "Empty string encodes to empty string."
  (is (string= "" (icl::encode-history-entry ""))))

(test decode-history-single-line
  "Single-line entry decodes unchanged."
  (is (string= "hello world" (icl::decode-history-entry "hello world"))))

(test decode-history-multi-line
  "Encoded \\n sequences decode to actual newlines."
  (is (string= (format nil "(defun f (x)~%  (+ x 1))")
               (icl::decode-history-entry "(defun f (x)\\n  (+ x 1))"))))

(test decode-history-empty
  "Empty string decodes to empty string."
  (is (string= "" (icl::decode-history-entry ""))))

(test history-round-trip-single
  "Round-trip encode/decode preserves single-line entry."
  (let ((input "simple input"))
    (is (string= input (icl::decode-history-entry (icl::encode-history-entry input))))))

(test history-round-trip-multi
  "Round-trip encode/decode preserves multi-line entry."
  (let ((input (format nil "(defun factorial (n)~%  (if (<= n 1)~%      1~%      (* n (factorial (1- n)))))")))
    (is (string= input (icl::decode-history-entry (icl::encode-history-entry input))))))

(test history-round-trip-special-chars
  "Round-trip preserves entries with special characters."
  (let ((input "(format nil \"hello~%world\")"))
    (is (string= input (icl::decode-history-entry (icl::encode-history-entry input))))))

(test encode-history-multiple-newlines
  "Multiple newlines are each encoded."
  (let ((encoded (icl::encode-history-entry (format nil "a~%b~%c"))))
    (is (string= "a\\nb\\nc" encoded))))

(test decode-history-trailing-backslash
  "Trailing backslash without n is preserved literally."
  (is (string= "abc\\" (icl::decode-history-entry "abc\\"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Visual Calculations
;;; ─────────────────────────────────────────────────────────────────────────────

(test calculate-visual-rows-empty
  "Empty line (length 0) occupies 1 visual row."
  (is (= 1 (icl::calculate-visual-rows 0 80))))

(test calculate-visual-rows-single
  "Short line fits in one row."
  (is (= 1 (icl::calculate-visual-rows 40 80))))

(test calculate-visual-rows-exact-width
  "Line exactly at terminal width is 1 row."
  (is (= 1 (icl::calculate-visual-rows 80 80))))

(test calculate-visual-rows-wrapping
  "Line longer than terminal width wraps."
  (is (= 2 (icl::calculate-visual-rows 81 80))))

(test calculate-visual-rows-multi-wrap
  "Very long line wraps multiple times."
  (is (= 3 (icl::calculate-visual-rows 200 80))))

(test calculate-cursor-visual-position-start
  "Cursor at start of line with no prompt."
  (multiple-value-bind (row col)
      (icl::calculate-cursor-visual-position 0 0 80)
    (is (= 0 row))
    (is (= 0 col))))

(test calculate-cursor-visual-position-with-prompt
  "Cursor position accounts for prompt width."
  (multiple-value-bind (row col)
      (icl::calculate-cursor-visual-position 4 5 80)
    (is (= 0 row))
    (is (= 9 col))))

(test calculate-cursor-visual-position-wrapping
  "Cursor wraps when prompt + col exceeds terminal width."
  (multiple-value-bind (row col)
      (icl::calculate-cursor-visual-position 4 78 80)
    (is (= 1 row))
    (is (= 2 col))))

(test buffer-visual-info-single-empty-line
  "Single empty line with no prompt."
  (let ((buf (icl::make-edit-buffer :prompt "" :continuation-prompt "")))
    (multiple-value-bind (total-rows cursor-row cursor-col)
        (icl::buffer-visual-info buf 80)
      (is (= 1 total-rows))
      (is (= 0 cursor-row))
      (is (= 0 cursor-col)))))

(test buffer-visual-info-single-line-with-content
  "Single line with content and prompt."
  (let ((buf (icl::make-edit-buffer :prompt ">>> " :continuation-prompt "... ")))
    (dolist (c (coerce "hello" 'list))
      (icl::buffer-insert-char buf c))
    (multiple-value-bind (total-rows cursor-row cursor-col)
        (icl::buffer-visual-info buf 80)
      (is (= 1 total-rows))
      (is (= 0 cursor-row))
      ;; prompt ">>> " is 4 chars, cursor at col 5 -> visual col 9
      (is (= 9 cursor-col)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Mouse / selection mapping
;;; ─────────────────────────────────────────────────────────────────────────────

(test screen-to-buffer-click-on-text
  "Click past the prompt maps onto the matching character."
  (let ((buf (icl::make-edit-buffer :prompt ">> " :continuation-prompt ".. ")))
    (dolist (c (coerce "hello" 'list))
      (icl::buffer-insert-char buf c))
    (multiple-value-bind (row col)
        (icl::screen-to-buffer-position buf 5 1 :origin-row 1 :term-width 80)
      ;; prompt ">> " is 3 cols; 1-based x=5 is the second text char
      (is (= 0 row))
      (is (= 1 col)))))

(test screen-to-buffer-click-on-prompt
  "Click on the prompt clamps to column 0."
  (let ((buf (icl::make-edit-buffer :prompt ">> " :continuation-prompt ".. ")))
    (dolist (c (coerce "hello" 'list))
      (icl::buffer-insert-char buf c))
    (multiple-value-bind (row col)
        (icl::screen-to-buffer-position buf 2 1 :origin-row 1 :term-width 80)
      (is (= 0 row))
      (is (= 0 col)))))

(test screen-to-buffer-click-past-end
  "Click past the end of the line clamps to the line length."
  (let ((buf (icl::make-edit-buffer :prompt ">> " :continuation-prompt ".. ")))
    (dolist (c (coerce "hi" 'list))
      (icl::buffer-insert-char buf c))
    (multiple-value-bind (row col)
        (icl::screen-to-buffer-position buf 40 1 :origin-row 1 :term-width 80)
      (is (= 0 row))
      (is (= 2 col)))))

(test screen-to-buffer-second-line
  "Click on the continuation line maps to row 1."
  (let ((buf (icl::make-edit-buffer :prompt ">> " :continuation-prompt ".. ")))
    (icl::buffer-set-contents buf (format nil "ab~%cd"))
    (setf (icl::edit-buffer-row buf) 0
          (icl::edit-buffer-col buf) 0)
    (multiple-value-bind (row col)
        (icl::screen-to-buffer-position buf 5 2 :origin-row 1 :term-width 80)
      (is (= 1 row))
      (is (= 1 col)))))

(test apply-reverse-range-plain
  "Reverse video wraps the requested visible columns."
  (let* ((on icl::*ansi-reverse*)
         (off (format nil "~C[27m" #\Escape))
         (result (icl::apply-reverse-range "hello" 1 4)))
    (is (string= (format nil "h~Aell~Ao" on off) result))))

(test apply-reverse-range-survives-reset
  "Reverse video is restored after an SGR reset inside the selection."
  (let* ((on icl::*ansi-reverse*)
         (off (format nil "~C[27m" #\Escape))
         (reset (format nil "~C[0m" #\Escape))
         (input (format nil "a~Ab" reset))
         (result (icl::apply-reverse-range input 0 2)))
    (is (string= (format nil "~Aa~A~Ab~A" on reset on off) result))))

(test parse-sgr-mouse-press
  "SGR press encodes as :press :left with 1-based coordinates."
  (is (equal '(:mouse :press :left 10 5)
             (with-input-from-string (*standard-input* "0;10;5M")
               (icl::parse-sgr-mouse)))))

(test parse-sgr-mouse-drag
  "SGR motion with button 32 is a left drag."
  (is (equal '(:mouse :drag :left 11 5)
             (with-input-from-string (*standard-input* "32;11;5M")
               (icl::parse-sgr-mouse)))))

(test parse-sgr-mouse-release
  "SGR lowercase m is a release."
  (is (equal '(:mouse :release :left 11 5)
             (with-input-from-string (*standard-input* "0;11;5m")
               (icl::parse-sgr-mouse)))))

(test handle-mouse-drag-selects
  "Press then drag creates a selection covering the dragged range."
  (let ((buf (icl::make-edit-buffer :prompt ">> " :continuation-prompt ".. ")))
    (dolist (c (coerce "hello" 'list))
      (icl::buffer-insert-char buf c))
    (icl::handle-mouse buf '(:mouse :press :left 4 1))
    (icl::handle-mouse buf '(:mouse :drag :left 7 1))
    (icl::handle-mouse buf '(:mouse :release :left 7 1))
    (is (icl::buffer-has-selection-p buf))
    (is (string= "hel" (icl::buffer-selection-text buf)))))

(test handle-key-delete-selection
  "Backspace deletes a selection instead of a single character."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-set-contents buf "hello")
    (icl::buffer-set-mark buf 0 1)
    (setf (icl::edit-buffer-row buf) 0
          (icl::edit-buffer-col buf) 4)
    (icl::handle-key buf :backspace)
    (is (string= "ho" (icl::buffer-contents buf)))
    (is (not (icl::buffer-has-selection-p buf)))))

(test handle-key-cut-selection
  "Ctrl-X cuts the selection out of the buffer."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-set-contents buf "hello")
    (icl::buffer-set-mark buf 0 1)
    (setf (icl::edit-buffer-row buf) 0
          (icl::edit-buffer-col buf) 4)
    (icl::handle-key buf :cut)
    (is (string= "ho" (icl::buffer-contents buf)))
    (is (not (icl::buffer-has-selection-p buf)))))

(test handle-key-type-replaces-selection
  "Typing replaces the selected text."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-set-contents buf "hello")
    (icl::buffer-set-mark buf 0 1)
    (setf (icl::edit-buffer-row buf) 0
          (icl::edit-buffer-col buf) 4)
    (icl::handle-key buf #\X)
    (is (string= "hXo" (icl::buffer-contents buf)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Ordinary Enter submission and newline behavior
;;; ─────────────────────────────────────────────────────────────────────────────

(test handle-key-enter-complete-at-buffer-end
  "Ordinary Enter submits a complete form at the end in either paredit mode."
  (dolist (paredit-enabled '(nil t))
    (let ((icl::*paredit-mode* paredit-enabled)
          (buf (icl::make-edit-buffer)))
      (icl::buffer-set-contents buf "(+ 1 2)")
      (icl::buffer-move-to-end buf)
      (is (eql :done (icl::handle-key buf :enter)))
      (is (string= "(+ 1 2)" (icl::buffer-contents buf))))))

(test handle-key-enter-complete-in-middle-inserts-newline
  "Ordinary Enter inserts a newline when the cursor is inside a complete form."
  (dolist (paredit-enabled '(nil t))
    (let ((icl::*paredit-mode* paredit-enabled)
          (buf (icl::make-edit-buffer)))
      (icl::buffer-set-contents buf "(+ 1 2)")
      (setf (icl::edit-buffer-row buf) 0
            (icl::edit-buffer-col buf) 3)
      (is (eql :newline (icl::handle-key buf :enter)))
      (is (= 2 (icl::buffer-line-count buf)))
      (is (string= "(+ " (icl::buffer-line buf 0)))
      (is (string= "1 2)"
                   (string-trim '(#\Space #\Tab)
                                (icl::buffer-line buf 1)))))))

(test handle-key-enter-earlier-line-inserts-newline
  "Ordinary Enter inserts a newline on an earlier line of a complete buffer."
  (dolist (paredit-enabled '(nil t))
    (let ((icl::*paredit-mode* paredit-enabled)
          (buf (icl::make-edit-buffer)))
      (icl::buffer-set-contents buf (format nil "(+ 1)~%(+ 2)"))
      (setf (icl::edit-buffer-row buf) 0
            (icl::edit-buffer-col buf) (length (icl::buffer-line buf 0)))
      (is (eql :newline (icl::handle-key buf :enter)))
      (is (= 3 (icl::buffer-line-count buf)))
      (is (string= "(+ 1)" (icl::buffer-line buf 0)))
      (is (string= "" (icl::buffer-line buf 1)))
      (is (string= "(+ 2)" (icl::buffer-line buf 2))))))

(test handle-key-enter-selection-cancels-before-submission
  "Enter cancels a selection without changing text before submitting at buffer end."
  (dolist (paredit-enabled '(nil t))
    (let ((icl::*paredit-mode* paredit-enabled)
          (buf (icl::make-edit-buffer)))
      (icl::buffer-set-contents buf "(+ 1 2)")
      (icl::buffer-move-to-end buf)
      (icl::buffer-set-mark buf 0 0)
      (is (icl::buffer-has-selection-p buf))
      (is (eql :done (icl::handle-key buf :enter)))
      (is (string= "(+ 1 2)" (icl::buffer-contents buf)))
      (is (not (icl::buffer-has-selection-p buf))))))

(test handle-key-enter-selection-cancels-before-newline
  "Enter cancels a selection without changing text before inserting a newline."
  (dolist (paredit-enabled '(nil t))
    (let ((icl::*paredit-mode* paredit-enabled)
          (buf (icl::make-edit-buffer)))
      (icl::buffer-set-contents buf "(+ 1 2)")
      (setf (icl::edit-buffer-row buf) 0
            (icl::edit-buffer-col buf) 3)
      (icl::buffer-set-mark buf 0 0)
      (is (eql :newline (icl::handle-key buf :enter)))
      (is (not (icl::buffer-has-selection-p buf)))
      (is (string= "(+ " (icl::buffer-line buf 0)))
      (is (string= "1 2)"
                   (string-trim '(#\Space #\Tab)
                                (icl::buffer-line buf 1)))))))

(test base64-encode-known-value
  "Base64 encoding matches the RFC 4648 example."
  (is (string= "TWFu" (icl::%base64-utf8 "Man"))))
