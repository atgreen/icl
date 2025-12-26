;;; tests/package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(defpackage #:sanitize-html/tests
  (:use #:cl #:fiveam #:sanitize-html)
  (:documentation "Test suite for sanitize-html"))

(in-package #:sanitize-html/tests)

(def-suite sanitize-html-tests
  :description "Test suite for sanitize-html")

(in-suite sanitize-html-tests)
