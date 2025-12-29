;;; icl-tests.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(asdf:defsystem "icl-tests"
  :description "Test suite for ICL"
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :version     "1.0.0"
  :depends-on (:icl :fiveam)
  :serial t
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "main-tests")
                 (:file "mcp-tests")
                 (:file "backend-tests")
                 (:file "output-tests")
                 (:file "theme-tests")
                 (:file "completion-tests")
                 (:file "highlight-tests")
                 (:file "indent-tests")
                 (:file "command-tests")))))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :icl-tests)))
