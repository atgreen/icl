;;; tests/completion-tests.lisp --- Tests for completion functions
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite completion-tests
  :description "Tests for completion functions"
  :in icl-tests)

(in-suite completion-tests)

;;; completion-word-char-p tests

(test completion-word-char-alphanumeric
  "Test that alphanumeric chars are completion word chars"
  (is-true (icl::completion-word-char-p #\a))
  (is-true (icl::completion-word-char-p #\Z))
  (is-true (icl::completion-word-char-p #\5)))

(test completion-word-char-special-lisp
  "Test Lisp-specific word characters"
  (is-true (icl::completion-word-char-p #\-))
  (is-true (icl::completion-word-char-p #\_))
  (is-true (icl::completion-word-char-p #\*))
  (is-true (icl::completion-word-char-p #\+))
  (is-true (icl::completion-word-char-p #\/))
  (is-true (icl::completion-word-char-p #\=))
  (is-true (icl::completion-word-char-p #\<))
  (is-true (icl::completion-word-char-p #\>))
  (is-true (icl::completion-word-char-p #\!))
  (is-true (icl::completion-word-char-p #\?))
  (is-true (icl::completion-word-char-p #\:)))

(test completion-word-char-non-word
  "Test that whitespace and parens are not word chars"
  (is-false (icl::completion-word-char-p #\Space))
  (is-false (icl::completion-word-char-p #\Tab))
  (is-false (icl::completion-word-char-p #\Newline))
  (is-false (icl::completion-word-char-p #\())
  (is-false (icl::completion-word-char-p #\))))

;;; path-like-p tests

(test path-like-absolute-unix
  "Test Unix absolute path detection"
  (is-true (icl::path-like-p "/usr/bin"))
  (is-true (icl::path-like-p "/etc/passwd")))

(test path-like-home-directory
  "Test home directory path detection"
  (is-true (icl::path-like-p "~/"))
  (is-true (icl::path-like-p "~/.bashrc")))

(test path-like-relative
  "Test relative path detection"
  (is-true (icl::path-like-p "./"))
  (is-true (icl::path-like-p "./foo/bar")))

(test path-like-not-path
  "Test non-path strings"
  (is-false (icl::path-like-p ""))
  (is-false (icl::path-like-p "hello"))
  (is-false (icl::path-like-p "defun")))

;;; classify-prefix tests

(test classify-prefix-empty
  "Test empty prefix classification"
  (is (eql (icl::classify-prefix "") :symbol)))

(test classify-prefix-command
  "Test command prefix classification"
  (is (eql (icl::classify-prefix ",help") :command))
  (is (eql (icl::classify-prefix ",ql") :command)))

(test classify-prefix-keyword
  "Test keyword prefix classification"
  (is (eql (icl::classify-prefix ":test") :keyword))
  (is (eql (icl::classify-prefix ":key") :keyword)))

(test classify-prefix-path
  "Test path prefix classification"
  (is (eql (icl::classify-prefix "/usr") :path))
  (is (eql (icl::classify-prefix "~/") :path))
  (is (eql (icl::classify-prefix "./src") :path)))

(test classify-prefix-qualified
  "Test package-qualified prefix classification"
  (is (eql (icl::classify-prefix "cl:defun") :qualified))
  (is (eql (icl::classify-prefix "alexandria::foo") :qualified)))

(test classify-prefix-symbol
  "Test regular symbol prefix classification"
  (is (eql (icl::classify-prefix "defun") :symbol))
  (is (eql (icl::classify-prefix "my-function") :symbol)))

;;; string-prefix-p tests

(test string-prefix-p-match
  "Test string prefix matching"
  (is-true (icl::string-prefix-p "hello" "hello world"))
  (is-true (icl::string-prefix-p "" "anything"))
  (is-true (icl::string-prefix-p "abc" "abc")))

(test string-prefix-p-no-match
  "Test string prefix non-matching"
  (is-false (icl::string-prefix-p "world" "hello world"))
  (is-false (icl::string-prefix-p "hello world extra" "hello world")))

(test string-prefix-p-case-insensitive
  "Test that string-prefix-p is case-insensitive"
  (is-true (icl::string-prefix-p "HELLO" "hello world"))
  (is-true (icl::string-prefix-p "Hello" "HELLO WORLD")))

;;; prefix-match-p tests

(test prefix-match-p-match
  "Test prefix matching"
  (is-true (icl::prefix-match-p "DEF" "DEFUN"))
  (is-true (icl::prefix-match-p "" "ANYTHING"))
  (is-true (icl::prefix-match-p "FULL" "FULL")))

(test prefix-match-p-no-match
  "Test prefix non-matching"
  (is-false (icl::prefix-match-p "FUN" "DEFUN"))
  (is-false (icl::prefix-match-p "DEFUNX" "DEFUN")))

;;; find-common-prefix tests

(test find-common-prefix-empty-list
  "Test common prefix of empty list"
  (is (string= (icl::find-common-prefix nil) "")))

(test find-common-prefix-single
  "Test common prefix of single string"
  (is (string= (icl::find-common-prefix '("hello")) "hello")))

(test find-common-prefix-common
  "Test common prefix of multiple strings"
  (is (string= (icl::find-common-prefix '("defun" "defmacro" "defvar")) "def"))
  (is (string= (icl::find-common-prefix '("abc" "abd" "abe")) "ab")))

(test find-common-prefix-no-common
  "Test when no common prefix exists"
  (is (string= (icl::find-common-prefix '("apple" "banana")) "")))

(test find-common-prefix-full-match
  "Test when all strings are identical"
  (is (string= (icl::find-common-prefix '("test" "test" "test")) "test")))

;;; completion-menu-visible-line-count tests
;;; The notebook cell sizes itself to the reported editor height plus these
;;; menu lines so the completion dropdown is not clipped (icl-7vs).

(test completion-menu-visible-line-count-inactive
  "No menu open -> zero extra lines"
  (icl::reset-completion-menu)
  (is (= 0 (icl::completion-menu-visible-line-count))))

(test completion-menu-visible-line-count-fits
  "Fewer candidates than max-visible -> one line per candidate, no count line"
  (icl::reset-completion-menu)
  (icl::open-completion-menu '("foo" "bar" "baz") "" 0)
  (is (= 3 (icl::completion-menu-visible-line-count)))
  (icl::reset-completion-menu))

(test completion-menu-visible-line-count-overflow
  "More candidates than max-visible -> max-visible rows plus the [n/total] line"
  (icl::reset-completion-menu)
  (let* ((max (icl::completion-menu-max-visible icl::*completion-menu*))
         (candidates (loop for i from 0 below (+ max 5)
                           collect (format nil "cand~D" i))))
    (icl::open-completion-menu candidates "" 0)
    (is (= (1+ max) (icl::completion-menu-visible-line-count))))
  (icl::reset-completion-menu))
