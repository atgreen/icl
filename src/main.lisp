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
  "Create -e/--eval option for evaluating expressions (can be repeated)."
  (clingon:make-option
   :list
   :short-name #\e
   :long-name "eval"
   :key :eval
   :description "Evaluate expression and print result (can be repeated)"))

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

(defun make-no-cache-option ()
  "Create --no-cache option to disable cached SBCL image."
  (clingon:make-option
   :flag
   :long-name "no-cache"
   :key :no-cache
   :description "Don't create or use cached SBCL image"))

(defun make-lisp-option ()
  "Create --lisp option to specify the Lisp implementation."
  (clingon:make-option
   :string
   :long-name "lisp"
   :key :lisp
   :description "Lisp implementation (roswell, sbcl, ccl, ecl, clisp, abcl, clasp). Pass arguments after -- (e.g. icl --lisp sbcl -- --dynamic-space-size 8192)"))

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

(defun make-unsafe-visualizations-option ()
  "Create --unsafe-visualizations option to allow JS in visualizations."
  (clingon:make-option
   :flag
   :long-name "unsafe-visualizations"
   :key :unsafe-visualizations
   :description "Allow JavaScript in visualizations (disables security sandbox)"))

(defun make-no-open-option ()
  "Create --no-open option to prevent automatic browser opening."
  (clingon:make-option
   :flag
   :long-name "no-open"
   :key :no-open
   :description "Don't automatically open browser (use with -b)"))

(defun make-notebook-option ()
  "Create --notebook option to start in a browser notebook."
  (clingon:make-option
   :flag
   :long-name "notebook"
   :key :notebook
   :description "Start a browser notebook (optionally: icl --notebook FILE.iclnb)"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Update Subcommand
;;; ─────────────────────────────────────────────────────────────────────────────

(defun handle-update-cli (cmd)
  "Handle the 'icl update' subcommand."
  (let ((check-only (clingon:getopt cmd :check))
        (dry-run (clingon:getopt cmd :dry-run)))
    (handler-case
        (progn
          (setf cl-selfupdate:*current-version* +version+)
          (cond
            ;; --check: Just check if update is available
            (check-only
             (multiple-value-bind (release newer-p)
                 (cl-selfupdate:update-available-p "atgreen" "icl")
               (if newer-p
                   (progn
                     (format t "Update available: ~A -> ~A~%"
                             +version+ (cl-selfupdate:release-tag release))
                     (uiop:quit 0))
                   (progn
                     (format t "ICL is up to date (version ~A).~%" +version+)
                     (uiop:quit 0)))))
            ;; --dry-run: Download but don't install
            (dry-run
             (multiple-value-bind (release newer-p)
                 (cl-selfupdate:update-available-p "atgreen" "icl")
               (if newer-p
                   (progn
                     (format t "Would update: ~A -> ~A~%"
                             +version+ (cl-selfupdate:release-tag release))
                     (format t "Downloading (dry run)...~%")
                     (let ((new-exe (cl-selfupdate:download-update
                                     "atgreen" "icl"
                                     :executable-name "icl")))
                       (format t "Downloaded to: ~A~%" new-exe)
                       (format t "Dry run complete. No changes made.~%")))
                   (format t "ICL is already up to date (version ~A).~%" +version+))))
            ;; Default: Apply update
            (t
             (multiple-value-bind (updated-p new-version old-version notes)
                 (cl-selfupdate:update-self "atgreen" "icl"
                                            :executable-name "icl")
               (if updated-p
                   (progn
                     (format t "Updated ICL from ~A to ~A~%" old-version new-version)
                     (when notes
                       (format t "~%Release notes:~%~A~%" notes)))
                   (format t "ICL is already up to date (version ~A).~%" +version+))))))
      (error (e)
        (format *error-output* "Update failed: ~A~%" e)
        (uiop:quit 1))))
  (uiop:quit 0))

(defun make-update-command ()
  "Create the 'update' subcommand."
  (clingon:make-command
   :name "update"
   :description "Update ICL to the latest version"
   :usage "[options]"
   :options (list
             (clingon:make-option
              :flag
              :long-name "check"
              :short-name #\c
              :key :check
              :description "Check if an update is available without installing")
             (clingon:make-option
              :flag
              :long-name "dry-run"
              :short-name #\n
              :key :dry-run
              :description "Download update but don't install it"))
   :handler #'handle-update-cli))

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
        (no-cache (clingon:getopt cmd :no-cache))
        (lisp-impl (clingon:getopt cmd :lisp))
        (connect-str (clingon:getopt cmd :connect))
        (verbose (clingon:getopt cmd :verbose))
        (mcp-server (clingon:getopt cmd :mcp-server))
        (browser-mode (clingon:getopt cmd :browser))
        (notebook-mode (clingon:getopt cmd :notebook))
        (notebook-file (and (clingon:getopt cmd :notebook)
                            (first (clingon:command-arguments cmd))))
        (unsafe-viz (clingon:getopt cmd :unsafe-visualizations))
        (no-open (clingon:getopt cmd :no-open))
        ;; With --notebook, the first free argument is the notebook file, not
        ;; an inferior-Lisp argument — don't pass it through to SBCL.
        (inferior-args (if (clingon:getopt cmd :notebook)
                           (rest (clingon:command-arguments cmd))
                           (clingon:command-arguments cmd))))
    ;; Disable image caching if requested
    (when no-cache
      (setf *use-image-cache* nil))
    ;; MCP server mode - special handling, runs without config
    (when mcp-server
      (multiple-value-bind (host port)
          (parse-connect-string mcp-server)
        (run-mcp-server :host host :port port))
      (uiop:quit 0))
    ;; Set verbose mode
    (setf *verbose* verbose)
    ;; Set unsafe visualizations mode
    (setf *unsafe-visualizations* unsafe-viz)
    ;; Load config FIRST so *default-lisp* can be set
    ;; (command line --lisp will override it below)
    (unless no-config
      (load-user-config))
    ;; Initialize themes early, before starting inferior lisp
    ;; This queries terminal background while terminal is in clean state
    (initialize-themes)
    (setup-highlight-colors)
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
             (start-inferior-lisp :lisp impl :extra-args inferior-args)
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
                (start-inferior-lisp :lisp impl :extra-args inferior-args)
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
    ;; Evaluate expressions if specified (process all in order)
    (when eval-expr
      (handler-case
          (progn
            (dolist (expr eval-expr)
              (let ((values (backend-eval expr)))
                ;; Output streams automatically via :write-string events
                ;; Print return values
                (dolist (v values)
                  (format t "~S~%" v))))
            (uiop:quit 0))
        (error (e)
          (format *error-output* "~&Error: ~A~%" e)
          (uiop:quit 1))))
    ;; Start browser if requested (--browser or --notebook)
    (when (or browser-mode notebook-mode)
      ;; For --notebook, prepare the notebook so it opens once the page connects
      ;; (see the terminal-ready websocket handler).
      (when notebook-mode
        (setf *current-notebook*
              (if (and notebook-file (probe-file notebook-file))
                  (load-notebook notebook-file)
                  (let ((nb (make-notebook :path notebook-file)))
                    (notebook-add-cell nb :kind :code :source "")
                    nb))))
      (format t "Starting ICL browser interface...~%")
      (let ((url (start-browser :open-browser (not no-open))))
        (if no-open
            (format t "Direct your browser to ~A~%" url)
            (format t "Browser started at ~A~%" url))))
    ;; If --connect with -b/--notebook, run browser-only mode (no terminal REPL)
    (if (and connect-str (or browser-mode notebook-mode))
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
   :description (format nil "Interactive Common Lisp - An enhanced REPL (v~A)" +version+)
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
                  (make-no-cache-option)
                  (make-lisp-option)
                  (make-connect-option)
                  (make-verbose-option)
                  (make-mcp-server-option)
                  (make-browser-option)
                  (make-no-open-option)
                  (make-notebook-option)
                  (make-unsafe-visualizations-option))
   :sub-commands (list (make-update-command))
   :handler #'handle-cli))

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
