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
   :description "Lisp implementation to use (sbcl, ccl, ecl, clisp, abcl, clasp). If not specified, auto-detects first available."))

(defun make-connect-option ()
  "Create --connect option to connect to an existing Slynk server."
  (clingon:make-option
   :string
   :long-name "connect"
   :key :connect
   :description "Connect to existing Slynk server (host:port)"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CLI Handler
;;; ─────────────────────────────────────────────────────────────────────────────

(defun parse-connect-string (connect-str)
  "Parse HOST:PORT connection string. Returns (values host port)."
  (let ((colon-pos (position #\: connect-str)))
    (if colon-pos
        (values (subseq connect-str 0 colon-pos)
                (parse-integer (subseq connect-str (1+ colon-pos))))
        (values connect-str *slynk-port*))))

(defun handle-cli (cmd)
  "Handle CLI command execution."
  (let ((eval-expr (clingon:getopt cmd :eval))
        (load-file (clingon:getopt cmd :load))
        (no-config (clingon:getopt cmd :no-config))
        (no-banner (clingon:getopt cmd :no-banner))
        (lisp-impl (clingon:getopt cmd :lisp))
        (connect-str (clingon:getopt cmd :connect)))
    ;; Configure backend mode
    (cond
      ;; Connect to existing Slynk server
      (connect-str
       (multiple-value-bind (host port)
           (parse-connect-string connect-str)
         (setf *use-slynk* t)
         (setf *slynk-host* host)
         (setf *slynk-port* port)
         (unless (slynk-connect :host host :port port)
           (format *error-output* "~&Failed to connect to ~A:~D~%" host port)
           (uiop:quit 1))))
      ;; Start inferior Lisp with specified implementation
      (lisp-impl
       (setf *use-slynk* t)
       (let ((impl (intern (string-upcase lisp-impl) :keyword)))
         (setf *default-lisp* impl)
         (handler-case
             (start-inferior-lisp :lisp impl)
           (error (e)
             (format *error-output* "~&Failed to start ~A: ~A~%" lisp-impl e)
             (uiop:quit 1)))))
      ;; Default: auto-detect available Lisp implementation
      (t
       (let ((impl (find-available-lisp)))
         (if impl
             (progn
               (setf *use-slynk* t)
               (setf *default-lisp* impl)
               (handler-case
                   (start-inferior-lisp :lisp impl)
                 (error (e)
                   (format *error-output* "~&Failed to start ~A: ~A~%" impl e)
                   (uiop:quit 1))))
             (progn
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
            (dolist (v values)
              (format t "~S~%" v))
            (uiop:quit 0))
        (error (e)
          (format *error-output* "~&Error: ~A~%" e)
          (uiop:quit 1))))
    ;; Otherwise start REPL
    (start-repl :load-config (not no-config)
                :banner (not no-banner))))

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
                  (make-connect-option))
   :handler #'handle-cli
   :examples '(("Start REPL (auto-detects Lisp):" . "icl")
               ("Evaluate an expression:" . "icl -e '(+ 1 2)'")
               ("Load a file then start REPL:" . "icl -l init.lisp")
               ("Start without config:" . "icl --no-config")
               ("Use a specific Lisp:" . "icl --lisp ccl")
               ("Connect to existing Slynk:" . "icl --connect localhost:4005"))))

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
