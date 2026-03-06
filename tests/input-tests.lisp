;;; tests/input-tests.lisp --- Tests for input completeness checking
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite input-tests
  :description "Tests for input completeness checking"
  :in icl-tests)

(in-suite input-tests)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; input-complete-p
;;; ─────────────────────────────────────────────────────────────────────────────

(test input-complete-p-complete-form
  "Complete Lisp form returns T."
  (is (icl::input-complete-p "(+ 1 2)")))

(test input-complete-p-incomplete-form
  "Incomplete form (missing close paren) returns NIL."
  (is (not (icl::input-complete-p "(+ 1"))))

(test input-complete-p-unmatched-string
  "Unmatched double quote returns NIL."
  (is (not (icl::input-complete-p "(format nil \"hello"))))

(test input-complete-p-reader-error
  "Reader errors are treated as complete (will error on eval)."
  (is (icl::input-complete-p "#<invalid>")))

(test input-complete-p-multiple-forms
  "Multiple complete forms return T."
  (is (icl::input-complete-p "(+ 1 2) (- 3 4)")))

(test input-complete-p-simple-atom
  "Simple atom is complete."
  (is (icl::input-complete-p "42")))

(test input-complete-p-string-literal
  "Complete string literal is complete."
  (is (icl::input-complete-p "\"hello\"")))

(test input-complete-p-nested-incomplete
  "Nested incomplete form returns NIL."
  (is (not (icl::input-complete-p "(let ((x 1))"))))
