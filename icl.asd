;;; icl.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

;;; Load pure-tls compatibility layer before any cl+ssl-dependent systems.
;;; Guarded with find-system so this .asd parses on Quicklisp-only installs
;;; (e.g. `ros install atgreen/icl` without `ocicl install`); roswell/icl.ros
;;; detects the missing dep and prints actionable instructions.
(eval-when (:load-toplevel :execute)
  (when (asdf:find-system :pure-tls/cl+ssl-compat nil)
    (asdf:load-system :pure-tls/cl+ssl-compat)
    (asdf:register-immutable-system "cl+ssl")))

;;; The `embedded-assets' source file bakes assets/*.js and assets/*.css into the
;;; image (read at compile/load time). asdf only tracks .lisp inputs by default,
;;; so a pure-asset edit (e.g. assets/notebook.js) wouldn't invalidate the fasl.
;;; Declare the asset files as extra compile inputs so editing one forces
;;; embedded-assets.lisp to recompile — and thus re-embed — on the next build.
(defclass asset-embedding-file (asdf:cl-source-file) ())
(defmethod asdf:input-files ((op asdf:compile-op) (c asset-embedding-file))
  (append (call-next-method)
          (ignore-errors
            (let ((dir (asdf:system-relative-pathname (asdf:component-system c)
                                                      "assets/")))
              (append (directory (merge-pathnames "*.js" dir))
                      (directory (merge-pathnames "*.css" dir)))))))

(asdf:defsystem "icl"
  :description "Interactive Common Lisp: An enhanced REPL"
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :version     "1.25.1"
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
               :cl-base64
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
               ;; HTML sanitization for secure visualizations
               :sanitize-html
               ;; ZIP extraction for embedded slynk
               :zip
               :flexi-streams
               ;; Self-update from GitHub releases
               :cl-selfupdate/drakma
               ;; POSIX-only dependencies
               (:feature (:not :windows) :osicat))
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "specials")
                 (:file "embedded-cl-arrow")
                 (:file "themes")
                 (:file "terminal-posix" :if-feature (:not :windows))
                 (:file "terminal-windows" :if-feature :windows)
                 (:file "terminal-common")
                 (:file "buffer")
                 (:file "paredit")
                 (:file "indent")
                 (:file "completion")
                 (:file "inspector-nav")
                 (:file "inspector")
                 (:file "debugger")
                 (:file "editor")
                 (:file "input")
                 (:file "slynk-client")
                 (:file "embedded-slynk")
                 (:file "image-cache")
                 (:file "backend")
                 (:file "output")
                 (:file "highlight")
                 (:file "eval")
                 (:file "notebook")
                 (:file "profiler")
                 (:file "coverage")
                 (:module "commands"
                  :components
                  ((:file "registry")
                   (:file "core")
                   (:file "sql")))
                 (:file "mcp-server")
                 (asset-embedding-file "embedded-assets")
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
