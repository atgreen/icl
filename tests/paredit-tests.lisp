;;; tests/paredit-tests.lisp --- Tests for paredit structural editing
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite paredit-tests
  :description "Tests for paredit-style structural editing"
  :in icl-tests)

(in-suite paredit-tests)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Delimiter Predicates
;;; ─────────────────────────────────────────────────────────────────────────────

(test open-delimiter-p-parens
  "Open paren is an opening delimiter."
  (is (icl::open-delimiter-p #\())
  (is (icl::open-delimiter-p #\[))
  (is (icl::open-delimiter-p #\{)))

(test open-delimiter-p-non-delimiters
  "Non-delimiters return NIL."
  (is (null (icl::open-delimiter-p #\))))
  (is (null (icl::open-delimiter-p #\a))))

(test close-delimiter-p-parens
  "Close paren is a closing delimiter."
  (is (icl::close-delimiter-p #\)))
  (is (icl::close-delimiter-p #\]))
  (is (icl::close-delimiter-p #\})))

(test close-delimiter-p-non-delimiters
  "Non-delimiters return NIL."
  (is (null (icl::close-delimiter-p #\()))
  (is (null (icl::close-delimiter-p #\a))))

(test matching-close
  "Opening delimiters map to correct closing delimiters."
  (is (char= #\) (icl::matching-close #\()))
  (is (char= #\] (icl::matching-close #\[)))
  (is (char= #\} (icl::matching-close #\{))))

(test matching-open
  "Closing delimiters map to correct opening delimiters."
  (is (char= #\( (icl::matching-open #\))))
  (is (char= #\[ (icl::matching-open #\])))
  (is (char= #\{ (icl::matching-open #\}))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; String/Comment Detection
;;; ─────────────────────────────────────────────────────────────────────────────

(test in-string-p-not-in-string
  "Position outside string literal returns NIL."
  (is (not (icl::in-string-p "(hello)" 3))))

(test in-string-p-inside-string
  "Position inside string literal returns T."
  (is (icl::in-string-p "(\"hello\")" 3)))

(test in-string-p-escaped-quote
  "Escaped quote does not end string."
  ;; "he\"llo" — position 5 is still inside the string
  (is (icl::in-string-p "\"he\\\"llo\"" 5)))

(test in-string-p-after-string
  "Position after closing quote is not in string."
  (is (not (icl::in-string-p "\"hi\" x" 5))))

(test in-comment-p-not-in-comment
  "Position before semicolon is not in comment."
  (is (not (icl::in-comment-p "abc ; comment" 2))))

(test in-comment-p-after-semicolon
  "Position after semicolon is in comment."
  (is (icl::in-comment-p "abc ; comment" 6)))

(test in-comment-p-semicolon-in-string
  "Semicolon inside string does not create a comment."
  (is (not (icl::in-comment-p "\"a;b\" x" 6))))

(test in-string-or-comment-p-string
  "Reports T for string context."
  (is (icl::in-string-or-comment-p "\"hello\"" 3)))

(test in-string-or-comment-p-comment
  "Reports T for comment context."
  (is (icl::in-string-or-comment-p "; comment" 5)))

(test in-string-or-comment-p-code
  "Reports NIL for code context."
  (is (not (icl::in-string-or-comment-p "(+ 1 2)" 3))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Balance Checking
;;; ─────────────────────────────────────────────────────────────────────────────

(test count-unmatched-balanced
  "Balanced parens return 0."
  (is (= 0 (icl::count-unmatched-delimiters "(+ 1 2)"))))

(test count-unmatched-open
  "One unmatched open paren returns 1."
  (is (= 1 (icl::count-unmatched-delimiters "(+ 1"))))

(test count-unmatched-nested
  "Nested balanced parens return 0."
  (is (= 0 (icl::count-unmatched-delimiters "(defun f (x) (+ x 1))"))))

(test count-unmatched-extra-close
  "Extra close paren returns negative."
  (is (= -1 (icl::count-unmatched-delimiters "(+ 1 2))"))))

(test count-unmatched-parens-in-string
  "Parens inside strings are ignored."
  (is (= 0 (icl::count-unmatched-delimiters "(format nil \"(~A)\" x)"))))

(test count-unmatched-empty
  "Empty string has 0 unmatched."
  (is (= 0 (icl::count-unmatched-delimiters ""))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Whitespace Skipping
;;; ─────────────────────────────────────────────────────────────────────────────

(test skip-whitespace-forward-basic
  "Skips spaces forward."
  (is (= 3 (icl::skip-whitespace-forward "   abc" 0))))

(test skip-whitespace-forward-no-ws
  "Returns same position if no whitespace."
  (is (= 0 (icl::skip-whitespace-forward "abc" 0))))

(test skip-whitespace-forward-at-end
  "Returns length at end of string."
  (is (= 3 (icl::skip-whitespace-forward "   " 0))))

(test skip-whitespace-backward-basic
  "Skips spaces backward."
  (is (= 3 (icl::skip-whitespace-backward "abc   " 6))))

(test skip-whitespace-backward-no-ws
  "Returns same position if no whitespace before."
  (is (= 3 (icl::skip-whitespace-backward "abc" 3))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; S-expression Parsing
;;; ─────────────────────────────────────────────────────────────────────────────

(test find-sexp-end-symbol
  "Find end of symbol."
  (is (= 5 (icl::find-sexp-end "hello world" 0))))

(test find-sexp-end-list
  "Find end of parenthesized list."
  (is (= 7 (icl::find-sexp-end "(+ 1 2)" 0))))

(test find-sexp-end-nested
  "Find end of nested list."
  (is (= 11 (icl::find-sexp-end "(+ (* 2 3))" 0))))

(test find-sexp-end-string
  "Find end of string literal."
  (is (= 7 (icl::find-sexp-end "\"hello\" rest" 0))))

(test find-sexp-end-quote
  "Find end of quoted form."
  (is (= 6 (icl::find-sexp-end "'(1 2)" 0))))

(test find-sexp-end-past-end
  "Returns NIL when starting past end."
  (is (null (icl::find-sexp-end "abc" 5))))

(test find-sexp-start-symbol
  "Find start of symbol."
  (is (= 6 (icl::find-sexp-start "hello world" 11))))

(test find-sexp-start-list
  "Find start of parenthesized list."
  (is (= 0 (icl::find-sexp-start "(+ 1 2)" 7))))

(test find-sexp-start-string
  "Find start of string literal."
  (is (= 0 (icl::find-sexp-start "\"hello\"" 7))))

(test find-sexp-start-at-zero
  "Returns NIL when at position 0."
  (is (null (icl::find-sexp-start "hello" 0))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Balanced Insertion (Paredit Buffer Operations)
;;; ─────────────────────────────────────────────────────────────────────────────

(test paredit-open-delimiter-inserts-pair
  "Opening delimiter inserts matching pair."
  (let ((buf (icl::make-edit-buffer)))
    (icl::paredit-open-delimiter buf #\()
    (is (string= "()" (icl::buffer-contents buf)))
    ;; Cursor should be between them
    (is (= 1 (icl::edit-buffer-col buf)))))

(test paredit-open-delimiter-bracket
  "Opening bracket inserts [] pair."
  (let ((buf (icl::make-edit-buffer)))
    (icl::paredit-open-delimiter buf #\[)
    (is (string= "[]" (icl::buffer-contents buf)))))

(test paredit-close-delimiter-skips
  "Closing delimiter skips if on matching close."
  (let ((buf (icl::make-edit-buffer)))
    (icl::paredit-open-delimiter buf #\()
    ;; cursor is between ( and )
    (icl::paredit-close-delimiter buf #\))
    (is (string= "()" (icl::buffer-contents buf)))
    ;; cursor should be past the )
    (is (= 2 (icl::edit-buffer-col buf)))))

(test paredit-close-delimiter-inserts-when-not-on-close
  "Closing delimiter inserts when not on matching close."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::paredit-close-delimiter buf #\))
    (is (string= "a)" (icl::buffer-contents buf)))))

(test paredit-double-quote-inserts-pair
  "Double quote outside string inserts pair."
  (let ((buf (icl::make-edit-buffer)))
    (icl::paredit-double-quote buf)
    (is (string= "\"\"" (icl::buffer-contents buf)))
    ;; Cursor between quotes
    (is (= 1 (icl::edit-buffer-col buf)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Safe Deletion
;;; ─────────────────────────────────────────────────────────────────────────────

(test paredit-backspace-empty-pair
  "Backspace inside empty pair deletes both delimiters."
  (let ((buf (icl::make-edit-buffer)))
    (icl::paredit-open-delimiter buf #\()
    ;; cursor between ( and )
    (is (icl::paredit-backspace buf))
    (is (string= "" (icl::buffer-contents buf)))))

(test paredit-backspace-blocks-structural
  "Backspace on opening delimiter with content is blocked."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\()
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\))
    ;; Move cursor to after (
    (setf (icl::edit-buffer-col buf) 1)
    (is (null (icl::paredit-backspace buf)))))

(test paredit-backspace-at-start
  "Backspace at start returns NIL."
  (let ((buf (icl::make-edit-buffer)))
    (is (null (icl::paredit-backspace buf)))))

(test paredit-backspace-normal-char
  "Backspace on normal character deletes normally."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (is (icl::paredit-backspace buf))
    (is (string= "a" (icl::buffer-contents buf)))))

(test paredit-delete-empty-pair
  "Delete inside empty pair deletes both delimiters."
  (let ((buf (icl::make-edit-buffer)))
    (icl::paredit-open-delimiter buf #\()
    ;; Cursor between ( and ), move left so delete hits (
    ;; Actually cursor is at col 1, which is on )
    ;; We need to test from position where char before is ( and char at is )
    ;; paredit-open-delimiter leaves cursor between them at col 1
    ;; For paredit-delete, we need the cursor ON the close delimiter
    ;; But the empty pair check looks at (1- pos) for open and pos for close
    ;; So we need to move left to be at col 0 (char-at = '('), which is blocked
    ;; Actually, the cursor at col 1 in "()" means content pos 1, char-at is )
    ;; and char-before is (, so it should delete both
    (is (icl::paredit-delete buf))
    (is (string= "" (icl::buffer-contents buf)))))

(test paredit-delete-blocks-structural
  "Delete on opening delimiter with content is blocked."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\()
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\))
    ;; Move cursor to col 0, on (
    (setf (icl::edit-buffer-col buf) 0)
    (is (null (icl::paredit-delete buf)))))

(test paredit-delete-at-end
  "Delete at end returns NIL."
  (let ((buf (icl::make-edit-buffer)))
    (is (null (icl::paredit-delete buf)))))

(test paredit-delete-normal-char
  "Delete on normal character deletes normally."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (setf (icl::edit-buffer-col buf) 0)
    (is (icl::paredit-delete buf))
    (is (string= "b" (icl::buffer-contents buf)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; S-expression Navigation (Buffer)
;;; ─────────────────────────────────────────────────────────────────────────────

(test buffer-forward-sexp-symbol
  "Forward sexp over a symbol."
  (let ((buf (icl::make-edit-buffer)))
    (dolist (c (coerce "hello world" 'list))
      (icl::buffer-insert-char buf c))
    (setf (icl::edit-buffer-col buf) 0)
    (is (icl::buffer-forward-sexp buf))
    (is (= 5 (icl::edit-buffer-col buf)))))

(test buffer-forward-sexp-list
  "Forward sexp over a list."
  (let ((buf (icl::make-edit-buffer)))
    (dolist (c (coerce "(+ 1 2) x" 'list))
      (icl::buffer-insert-char buf c))
    (setf (icl::edit-buffer-col buf) 0)
    (is (icl::buffer-forward-sexp buf))
    (is (= 7 (icl::edit-buffer-col buf)))))

(test buffer-forward-sexp-at-end
  "Forward sexp at end returns NIL."
  (let ((buf (icl::make-edit-buffer)))
    (is (null (icl::buffer-forward-sexp buf)))))

(test buffer-backward-sexp-symbol
  "Backward sexp over a symbol."
  (let ((buf (icl::make-edit-buffer)))
    (dolist (c (coerce "hello world" 'list))
      (icl::buffer-insert-char buf c))
    ;; cursor at end (col 11)
    (is (icl::buffer-backward-sexp buf))
    (is (= 6 (icl::edit-buffer-col buf)))))

(test buffer-backward-sexp-at-start
  "Backward sexp at start returns NIL."
  (let ((buf (icl::make-edit-buffer)))
    (is (null (icl::buffer-backward-sexp buf)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Buffer Position Helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(test buffer-set-cursor-position-start
  "Set cursor to position 0."
  (let ((buf (icl::make-edit-buffer)))
    (icl::buffer-insert-char buf #\a)
    (icl::buffer-insert-char buf #\b)
    (icl::buffer-set-cursor-position buf 0)
    (is (= 0 (icl::edit-buffer-row buf)))
    (is (= 0 (icl::edit-buffer-col buf)))))

(test buffer-set-cursor-position-middle
  "Set cursor to middle of content."
  (let ((buf (icl::make-edit-buffer)))
    (dolist (c (coerce "abcde" 'list))
      (icl::buffer-insert-char buf c))
    (icl::buffer-set-cursor-position buf 3)
    (is (= 0 (icl::edit-buffer-row buf)))
    (is (= 3 (icl::edit-buffer-col buf)))))
