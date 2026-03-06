;;; tests/inspector-tests.lisp --- Tests for inspector pure functions
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite inspector-tests
  :description "Tests for inspector string utilities, content parsing, and navigation"
  :in icl-tests)

(in-suite inspector-tests)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; String Utilities
;;; ─────────────────────────────────────────────────────────────────────────────

(test newline-string-p-newline
  "String containing newline returns truthy."
  (is (icl::newline-string-p (format nil "~%"))))

(test newline-string-p-plain
  "Plain string without newline returns NIL."
  (is (not (icl::newline-string-p "hello"))))

(test newline-string-p-empty
  "Empty string returns NIL."
  (is (not (icl::newline-string-p ""))))

(test newline-string-p-non-string
  "Non-string returns NIL."
  (is (not (icl::newline-string-p 42))))

(test ensure-string-string
  "String input is returned as-is."
  (is (string= "hello" (icl::ensure-string "hello"))))

(test ensure-string-nil
  "NIL converts to empty string."
  (is (string= "" (icl::ensure-string nil))))

(test ensure-string-number
  "Number converts to its printed representation."
  (is (string= "42" (icl::ensure-string 42))))

(test truncate-string-short
  "Short string is returned as-is."
  (is (string= "hi" (icl::truncate-string "hi" 10))))

(test truncate-string-exact
  "String at exact max length is returned as-is."
  (is (string= "hello" (icl::truncate-string "hello" 5))))

(test truncate-string-long
  "Long string is truncated with ellipsis."
  (is (string= "hell..." (icl::truncate-string "hello world" 7))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Content Parsing
;;; ─────────────────────────────────────────────────────────────────────────────

(test parse-inspector-content-empty
  "Empty content list produces no entries."
  (is (null (icl::parse-inspector-content nil))))

(test parse-inspector-content-simple-string
  "Simple string entries delimited by newlines."
  (let ((entries (icl::parse-inspector-content
                  (list "Name" (format nil "~%")))))
    (is (= 1 (length entries)))
    (is (string= "Name" (first (first entries))))))

(test parse-inspector-content-value-item
  "Content with :value items extracts value and action."
  (let ((entries (icl::parse-inspector-content
                  (list "Type: " '(:value "CONS" 0) (format nil "~%")))))
    (is (= 1 (length entries)))
    (let ((entry (first entries)))
      (is (string= "Type" (first entry)))
      (is (string= "CONS" (second entry)))
      (is (= 0 (third entry))))))

(test parse-inspector-content-action-item
  "Content with :action items."
  (let ((entries (icl::parse-inspector-content
                  (list '(:action "Inspect" 5) (format nil "~%")))))
    (is (= 1 (length entries)))
    (is (string= "Inspect" (first (first entries))))
    (is (= 5 (third (first entries))))))

(test parse-inspector-content-multiple-entries
  "Multiple entries separated by newlines."
  (let ((entries (icl::parse-inspector-content
                  (list "A: " '(:value "1" 0) (format nil "~%")
                        "B: " '(:value "2" 1) (format nil "~%")))))
    (is (= 2 (length entries)))))

(test parse-inspector-content-trailing-entry
  "Entry without trailing newline is still captured."
  (let ((entries (icl::parse-inspector-content
                  (list "Trailing: " '(:value "val" 3)))))
    (is (= 1 (length entries)))
    (is (string= "val" (second (first entries))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Entry Lookup
;;; ─────────────────────────────────────────────────────────────────────────────

(test find-entry-action-found
  "Finds action index by label (case-insensitive)."
  (let ((entries '(("Car" "value1" 10) ("Cdr" "value2" 20))))
    (is (= 10 (icl::find-entry-action entries "car")))
    (is (= 20 (icl::find-entry-action entries "CDR")))))

(test find-entry-action-not-found
  "Returns NIL for missing label."
  (let ((entries '(("Car" "value1" 10))))
    (is (null (icl::find-entry-action entries "missing")))))

(test find-entry-action-empty
  "Returns NIL for empty entries list."
  (is (null (icl::find-entry-action nil "anything"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Inspector Navigation
;;; ─────────────────────────────────────────────────────────────────────────────

(test inspector-move-down
  "Moving down increments selected index."
  (let ((icl::*inspector* (icl::make-inspector-state
                            :entries '(("a" "1" nil) ("b" "2" nil) ("c" "3" nil))
                            :selected 0)))
    (icl::inspector-move :down)
    (is (= 1 (icl::inspector-state-selected icl::*inspector*)))))

(test inspector-move-up
  "Moving up decrements selected index."
  (let ((icl::*inspector* (icl::make-inspector-state
                            :entries '(("a" "1" nil) ("b" "2" nil) ("c" "3" nil))
                            :selected 2)))
    (icl::inspector-move :up)
    (is (= 1 (icl::inspector-state-selected icl::*inspector*)))))

(test inspector-move-up-at-top
  "Moving up at top clamps to 0."
  (let ((icl::*inspector* (icl::make-inspector-state
                            :entries '(("a" "1" nil) ("b" "2" nil))
                            :selected 0)))
    (icl::inspector-move :up)
    (is (= 0 (icl::inspector-state-selected icl::*inspector*)))))

(test inspector-move-down-at-bottom
  "Moving down at bottom clamps to last index."
  (let ((icl::*inspector* (icl::make-inspector-state
                            :entries '(("a" "1" nil) ("b" "2" nil))
                            :selected 1)))
    (icl::inspector-move :down)
    (is (= 1 (icl::inspector-state-selected icl::*inspector*)))))

(test inspector-move-empty
  "Moving in empty entries list does nothing."
  (let ((icl::*inspector* (icl::make-inspector-state :entries nil :selected 0)))
    (icl::inspector-move :down)
    (is (= 0 (icl::inspector-state-selected icl::*inspector*)))))

(test inspector-move-scroll-adjustment
  "Moving down past visible area adjusts scroll offset."
  (let ((icl::*inspector* (icl::make-inspector-state
                            :entries (loop for i below 20
                                          collect (list (format nil "~D" i) "v" nil))
                            :selected 14
                            :max-visible 15)))
    (icl::inspector-move :down)
    (is (= 15 (icl::inspector-state-selected icl::*inspector*)))
    (is (= 1 (icl::inspector-state-scroll-offset icl::*inspector*)))))
