;;; icl.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(asdf:defsystem "icl"
  :description "Interactive Common Lisp: An enhanced REPL"
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :version     "1.15.0"
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
               ;; JSON for MCP protocol
               :yason
               :com.inuoe.jzon
               ;; Markdown rendering for AI output
               :tuition
               ;; HTTP server for MCP and browser
               :hunchentoot
               ;; WebSocket support for browser
               :hunchensocket
               :chanl
               ;; ZIP extraction for embedded slynk
               :zip
               :flexi-streams
               ;; POSIX-only dependencies
               (:feature (:not :windows) :osicat))
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "specials")
                 (:file "themes")
                 (:file "terminal-posix" :if-feature (:not :windows))
                 (:file "terminal-windows" :if-feature :windows)
                 (:file "buffer")
                 (:file "paredit")
                 (:file "indent")
                 (:file "completion")
                 (:file "inspector")
                 (:file "editor")
                 (:file "input")
                 (:file "slynk-client")
                 (:file "embedded-slynk")
                 (:file "backend")
                 (:file "output")
                 (:file "highlight")
                 (:file "eval")
                 (:file "profiler")
                 (:module "commands"
                  :components
                  ((:file "registry")
                   (:file "core")))
                 (:file "mcp-server")
                 (:file "embedded-assets")
                 (:file "browser")
                 (:file "browser-query")
                 (:file "browser-websocket")
                 (:file "browser-ui")
                 (:file "browser-server")
                 (:file "repl")
                 (:file "main"))))
  :build-operation "program-op"
  :build-pathname "icl"
  :entry-point "icl:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
