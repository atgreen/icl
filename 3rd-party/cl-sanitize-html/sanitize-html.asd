;;; sanitize-html.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(asdf:defsystem "sanitize-html"
  :description "OWASP-style HTML sanitization library for Common Lisp"
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :version     "0.1.0"
  :depends-on  ("plump"
                "lquery"
                "cl-ppcre"
                "alexandria")
  :serial t
  :components ((:file "src/package")
               (:file "src/policies")
               (:file "src/sanitizer"))
  :in-order-to ((test-op (test-op "sanitize-html/tests"))))

(asdf:defsystem "sanitize-html/tests"
  :description "Tests for sanitize-html"
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :depends-on  ("sanitize-html"
                "fiveam")
  :serial t
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "sanitizer-tests"))))
  :perform (test-op (op c)
             (uiop:symbol-call :fiveam :run!
                               (uiop:find-symbol* :sanitize-html-tests
                                                  :sanitize-html/tests))))
