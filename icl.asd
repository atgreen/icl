;;; icl.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(asdf:defsystem "icl"
  :description "Interactive Common Lisp: An enhanced REPL"
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :version     "1.5.1"
  :depends-on (:clingon
               :version-string
               ;; Input/Terminal
               :termp
               :cffi
               ;; Slynk client
               :slynk-client
               ;; Utilities
               :alexandria
               :split-sequence
               ;; POSIX-only dependencies
               (:feature (:not :windows) :osicat))
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "specials")
                 (:file "terminal-posix" :if-feature (:not :windows))
                 (:file "terminal-windows" :if-feature :windows)
                 (:file "buffer")
                 (:file "paredit")
                 (:file "indent")
                 (:file "completion")
                 (:file "editor")
                 (:file "input")
                 (:file "slynk-client")
                 (:file "backend")
                 (:file "output")
                 (:file "highlight")
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
