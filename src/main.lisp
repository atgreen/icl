;;; main.lisp --- CLI entry point for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;; +version+ is defined in specials.lisp

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CLI Options
;;; ─────────────────────────────────────────────────────────────────────────────

(defun make-eval-option ()
  "Create -e/--eval option for evaluating expressions."
  (clingon:make-option
   :string
   :short-name #\e
   :long-name "eval"
   :key :eval
   :description "Evaluate expression and print result"))

(defun make-load-option ()
  "Create -l/--load option for loading files."
  (clingon:make-option
   :string
   :short-name #\l
   :long-name "load"
   :key :load
   :description "Load a Lisp file before starting REPL"))

(defun make-no-config-option ()
  "Create --no-config option to skip loading ~/.iclrc."
  (clingon:make-option
   :flag
   :long-name "no-config"
   :key :no-config
   :description "Don't load ~/.iclrc"))

(defun make-no-banner-option ()
  "Create --no-banner option to suppress startup banner."
  (clingon:make-option
   :flag
   :long-name "no-banner"
   :key :no-banner
   :description "Don't print startup banner"))

(defun make-lisp-option ()
  "Create --lisp option to specify the Lisp implementation."
  (clingon:make-option
   :string
   :long-name "lisp"
   :key :lisp
   :description "Lisp implementation (roswell, sbcl, ccl, ecl, clisp, abcl, clasp)"))

(defun make-connect-option ()
  "Create --connect option to connect to an existing Slynk server."
  (clingon:make-option
   :string
   :long-name "connect"
   :key :connect
   :description "Connect to existing Slynk server (host:port)"))

(defun make-verbose-option ()
  "Create --verbose option for debugging startup."
  (clingon:make-option
   :flag
   :short-name #\v
   :long-name "verbose"
   :key :verbose
   :description "Show verbose startup information"))

(defun make-mcp-server-option ()
  "Create --mcp-server option to run as MCP server."
  (clingon:make-option
   :string
   :long-name "mcp-server"
   :key :mcp-server
   :description "Run as MCP server, connecting to Slynk at host:port"))

(defun make-browser-option ()
  "Create -b/--browser option to start with browser interface."
  (clingon:make-option
   :flag
   :short-name #\b
   :long-name "browser"
   :key :browser
   :description "Start with browser interface instead of terminal REPL"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CLI Handler
;;; ─────────────────────────────────────────────────────────────────────────────

(defun parse-connect-string (connect-str)
  "Parse HOST:PORT connection string. Returns (values host port).
   Signals an error with a user-friendly message if port is invalid."
  (let ((colon-pos (position #\: connect-str)))
    (if colon-pos
        (let ((host (subseq connect-str 0 colon-pos))
              (port-str (subseq connect-str (1+ colon-pos))))
          (handler-case
              (let ((port (parse-integer port-str)))
                (unless (and (plusp port) (<= port 65535))
                  (error "Port must be between 1 and 65535"))
                (values host port))
            (error (e)
              (error "Invalid port in '~A': ~A" connect-str e))))
        (values connect-str *slynk-port*))))

(defun handle-cli (cmd)
  "Handle CLI command execution."
  (let ((eval-expr (clingon:getopt cmd :eval))
        (load-file (clingon:getopt cmd :load))
        (no-config (clingon:getopt cmd :no-config))
        (no-banner (clingon:getopt cmd :no-banner))
        (lisp-impl (clingon:getopt cmd :lisp))
        (connect-str (clingon:getopt cmd :connect))
        (verbose (clingon:getopt cmd :verbose))
        (mcp-server (clingon:getopt cmd :mcp-server))
        (browser-mode (clingon:getopt cmd :browser)))
    ;; MCP server mode - special handling, runs without config
    (when mcp-server
      (multiple-value-bind (host port)
          (parse-connect-string mcp-server)
        (run-mcp-server :host host :port port))
      (uiop:quit 0))
    ;; Set verbose mode
    (setf *verbose* verbose)
    ;; Load config FIRST so *default-lisp* can be set
    ;; (command line --lisp will override it below)
    (unless no-config
      (load-user-config))
    ;; Configure backend mode
    (cond
      ;; Connect to existing Slynk server
      (connect-str
       (multiple-value-bind (host port)
           (parse-connect-string connect-str)
         (setf *slynk-host* host)
         (setf *slynk-port* port)
         (setf *external-slynk-connection* t)  ; Mark as external connection
         (unless (slynk-connect :host host :port port)
           (format *error-output* "~&Failed to connect to ~A:~D~%" host port)
           (uiop:quit 1))))
      ;; Start inferior Lisp with specified implementation (overrides config)
      (lisp-impl
       (let ((impl (intern (string-upcase lisp-impl) :keyword)))
         (setf *default-lisp* impl)
         (handler-case
             (start-inferior-lisp :lisp impl)
           (error (e)
             (format *error-output* "~&Failed to start ~A: ~A~%" lisp-impl e)
             (uiop:quit 1)))))
      ;; Use *default-lisp* from config, or auto-detect
      (t
       (let ((impl (if (lisp-available-p *default-lisp*)
                       *default-lisp*
                       (find-available-lisp))))
         (cond
           (impl
            (setf *default-lisp* impl)
            (handler-case
                (start-inferior-lisp :lisp impl)
              (error (e)
                (format *error-output* "~&Failed to start ~A: ~A~%" impl e)
                (uiop:quit 1))))
           (t
            (format *error-output* "~&No Lisp implementation found in PATH.~%")
            (format *error-output* "~&Checked: ~{~A~^, ~}~%" *lisp-implementation-order*)
            (uiop:quit 1))))))
    ;; Load file if specified
    (when load-file
      (handler-case
          (load load-file :verbose t)
        (error (e)
          (format *error-output* "~&Error loading ~A: ~A~%" load-file e)
          (uiop:quit 1))))
    ;; Evaluate expression if specified
    (when eval-expr
      (handler-case
          (let ((values (backend-eval eval-expr)))
            ;; Output streams automatically via :write-string events
            ;; Print return values
            (dolist (v values)
              (format t "~S~%" v))
            (uiop:quit 0))
        (error (e)
          (format *error-output* "~&Error: ~A~%" e)
          (uiop:quit 1))))
    ;; Start browser if requested
    (when browser-mode
      (format t "Starting ICL browser interface...~%")
      (let ((url (start-browser :open-browser t)))
        (format t "Browser started at ~A~%" url)))
    ;; If --connect -b, run browser-only mode (no terminal REPL)
    (if (and connect-str browser-mode)
        (progn
          (format t "~&; Browser-only mode (connected to ~A)~%" connect-str)
          (format t "~&; Press Ctrl-C to exit~%")
          ;; Just keep the process alive - browser server runs in background
          (loop (sleep 60)))
        ;; Start terminal REPL (config already loaded)
        (start-repl :load-config nil
                    :banner (not no-banner)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CLI Application
;;; ─────────────────────────────────────────────────────────────────────────────

(defun make-app ()
  "Create the ICL CLI application."
  (clingon:make-command
   :name "icl"
   :version +version+
   :description "Interactive Common Lisp - An enhanced REPL"
   :long-description "ICL provides a modern, feature-rich REPL for Common Lisp
with readline-style editing, persistent history, tab completion,
and an extensible command system."
   :authors '("Anthony Green <green@moxielogic.com>")
   :license "MIT"
   :usage "[options]"
   :options (list (make-eval-option)
                  (make-load-option)
                  (make-no-config-option)
                  (make-no-banner-option)
                  (make-lisp-option)
                  (make-connect-option)
                  (make-verbose-option)
                  (make-mcp-server-option)
                  (make-browser-option))
   :handler #'handle-cli
   :examples '(("Start REPL (auto-detects Lisp):" . "icl")
               ("Evaluate an expression:" . "icl -e '(+ 1 2)'")
               ("Load a file then start REPL:" . "icl -l init.lisp")
               ("Start without config:" . "icl --no-config")
               ("Use a specific Lisp:" . "icl --lisp ccl")
               ("Connect to existing Slynk:" . "icl --connect localhost:4005")
               ("Start with browser interface:" . "icl -b"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Entry Point
;;; ─────────────────────────────────────────────────────────────────────────────

(defun main ()
  "The main entry point for ICL."
  (handler-case
      (clingon:run (make-app))
    (error (e)
      (format *error-output* "~&Fatal error: ~A~%" e)
      (uiop:quit 1))))
