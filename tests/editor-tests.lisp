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
