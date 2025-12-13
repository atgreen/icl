;;; icl.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(asdf:defsystem #:icl
  :description "Interactive Common Lisp: An enhanced REPL"
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :version     "0.1.0"
  :depends-on (:clingon
               :version-string
               ;; Input/Terminal
               :linedit
               :termp
               :osicat
               :cffi
               ;; Slynk client
               :slynk-client
               ;; Utilities
               :alexandria
               :split-sequence)
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "specials")
                 (:file "terminal")
                 (:file "buffer")
                 (:file "indent")
                 (:file "completion")
                 (:file "editor")
                 (:file "input")
                 (:file "slynk-client")
                 (:file "backend")
                 (:file "output")
                 (:file "eval")
                 (:module "commands"
                  :components
                  ((:file "registry")
                   (:file "core")))
                 (:file "repl")
                 (:file "main"))))
  :build-operation "program-op"
  :build-pathname "icl"
  :entry-point "icl:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
