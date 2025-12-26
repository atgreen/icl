;;; specials.lisp --- Special variables and constants for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Foreign Library Configuration
;;; ─────────────────────────────────────────────────────────────────────────────

;;; Close osicat's foreign library before image is saved.
;;; This prevents the absolute build path from being saved.
;;; The library will be reopened at runtime via ldconfig.
;;; Only needed on POSIX systems where osicat is used.
#+(and sbcl (not windows))
(pushnew
 (lambda ()
   (ignore-errors
     (cffi:close-foreign-library :libosicat)))
 sb-ext:*save-hooks*)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Version
;;; ─────────────────────────────────────────────────────────────────────────────

(version-string:define-version-parameter +version+ :icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Package Context
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *icl-package* (find-package :cl-user)
  "Current package context for evaluation (local package object, may be NIL for remote-only packages).")

(defvar *icl-package-name* "COMMON-LISP-USER"
  "Name of the current package for prompt display.
   This tracks the package in the inferior Lisp, which may not exist locally.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Prompts
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *prompt-string* "~A> "
  "Format string for primary prompt. ~A is replaced with package name.")

(defvar *continuation-prompt* ".. "
  "Prompt displayed for multi-line input continuation.")

(defvar *result-prefix* "=> "
  "Prefix for displaying evaluation results.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; History Variables (Standard CL Convention)
;;; ─────────────────────────────────────────────────────────────────────────────

;;; Note: We shadow these from CL package to maintain our own REPL history
;;; independent of any outer REPL.

(defvar icl-* nil "Most recent primary value.")
(defvar icl-** nil "Second most recent primary value.")
(defvar icl-*** nil "Third most recent primary value.")

;;; IRB-style shortcuts (easier to type than icl:icl-*)
(defvar _ nil "Most recent primary value (IRB-style).")
(defvar __ nil "Second most recent primary value (IRB-style).")
(defvar ___ nil "Third most recent primary value (IRB-style).")

(defvar icl-+ nil "Most recent input form.")
(defvar icl-++ nil "Second most recent input form.")
(defvar icl-+++ nil "Third most recent input form.")

(defvar icl-/ nil "Most recent list of all values returned.")
(defvar icl-// nil "Second most recent list of values.")
(defvar icl-/// nil "Third most recent list of values.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Configuration
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *use-multiline-editor* t
  "If T (default), use the multiline editor when terminal supports it.
   If NIL, always use simple line-based input.")

(defvar *browser-terminal-active* nil
  "T when running the REPL against a browser-based terminal (xterm.js).")

(defvar *unsafe-visualizations* nil
  "When T, disable security restrictions on visualizations (HTML, Mermaid, Vega).
   Allows JavaScript execution in custom visualize methods.
   Set via --unsafe-visualizations command line option.")

(defvar *capture-backend-output* nil
  "When T, capture backend stdout/stderr and print through the current REPL stream.")

(defvar *active-repl-output* nil
  "Output stream for the currently active REPL, or NIL to use *standard-output*.")

(defvar *active-repl-output-lock* (bt:make-lock "icl-active-repl-output"))

(defun active-repl-output-stream ()
  "Return the currently active REPL output stream."
  (bt:with-lock-held (*active-repl-output-lock*)
    *active-repl-output*))

(defun set-active-repl-output (stream)
  "Set STREAM as the active REPL output stream."
  (bt:with-lock-held (*active-repl-output-lock*)
    (setf *active-repl-output* stream)))

(defun clear-active-repl-output-if (stream)
  "Clear active output stream if it matches STREAM."
  (bt:with-lock-held (*active-repl-output-lock*)
    (when (eq *active-repl-output* stream)
      (setf *active-repl-output* nil))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Session Management
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (repl-session (:constructor %make-repl-session))
  "Represents an ICL REPL session."
  (id (gensym "SESSION-") :type symbol :read-only t)
  (name "unnamed" :type string)
  (output-stream nil :type (or null stream))
  (input-stream nil :type (or null stream))
  (active-p t :type boolean)
  (created-at (get-universal-time) :type integer :read-only t)
  ;; Per-session history (Phase 3)
  (history nil :type list)
  (history-index nil :type (or null integer))
  (history-saved-buffer nil :type (or null string))
  ;; Per-session search state (Phase 3)
  (search-mode nil :type boolean)
  (search-pattern "" :type string)
  (search-matches nil :type list)
  (search-match-index 0 :type integer)
  (prefix-search-prefix nil :type (or null string))
  (prefix-search-index nil :type (or null integer)))

(defvar *session-registry* (make-hash-table :test 'eq)
  "Registry of all active sessions, keyed by session ID.")

(defvar *session-registry-lock* (bt:make-lock "session-registry"))

(defvar *current-session* nil
  "The session for this thread. Dynamically bound per REPL thread.")

(defvar *primary-session* nil
  "The primary session (TUI) for fallback output routing.")

(defvar *evaluating-session* nil
  "Session that initiated the current Slynk evaluation.")

(defvar *evaluating-session-lock* (bt:make-lock "evaluating-session"))

(defun register-session (session)
  "Register SESSION in the global registry."
  (bt:with-lock-held (*session-registry-lock*)
    (setf (gethash (repl-session-id session) *session-registry*) session)
    (unless *primary-session*
      (setf *primary-session* session)))
  session)

(defun unregister-session (session)
  "Remove SESSION from the global registry."
  (bt:with-lock-held (*session-registry-lock*)
    (remhash (repl-session-id session) *session-registry*)
    (when (eq *primary-session* session)
      (setf *primary-session*
            (loop for s being the hash-values of *session-registry*
                  when (repl-session-active-p s) return s)))))

(defun make-repl-session (&key name output-stream input-stream)
  "Create and register a new REPL session."
  (let ((session (%make-repl-session
                  :name (or name "unnamed")
                  :output-stream output-stream
                  :input-stream input-stream)))
    (register-session session)
    session))

(defun evaluating-session-output-stream ()
  "Return output stream for the session that initiated current evaluation.
   Returns NIL if no session is currently evaluating, allowing callers
   to fall back to *active-repl-output* for async output routing."
  (bt:with-lock-held (*evaluating-session-lock*)
    (let ((session *evaluating-session*))
      (when (and session (repl-session-active-p session))
        (repl-session-output-stream session)))))

(defun browser-history-file ()
  "Return the shared browser history file path."
  (merge-pathnames "history-browser" (state-directory)))

(defun session-history-file (&optional (session *current-session*))
  "Return history file path for SESSION.
   TUI uses main history file, browser REPLs share history-browser."
  (if (or (null session)
          (string= (repl-session-name session) "TUI"))
      (history-file)
      (browser-history-file)))

(defvar *history-file* nil
  "Path to persistent command history file. Computed at runtime.")

(defvar *history-size* 1000
  "Maximum number of history entries to keep.")

(defvar *config-file* nil
  "User configuration file path. Computed at runtime.")

;;; Platform-specific configuration directories
;;; POSIX: XDG Base Directory Specification (https://specifications.freedesktop.org/basedir/latest/)
;;; Windows: %APPDATA% and %LOCALAPPDATA%

(defun config-directory ()
  "Return the configuration directory for ICL.
   Windows: %APPDATA%/icl/
   POSIX: $XDG_CONFIG_HOME/icl/ (default: ~/.config/icl/)"
  #+windows
  (let ((appdata (uiop:getenv "APPDATA")))
    (if (and appdata (> (length appdata) 0))
        (merge-pathnames "icl/" (pathname (concatenate 'string appdata "/")))
        (merge-pathnames "icl/" (user-homedir-pathname))))
  #-windows
  (let ((xdg (uiop:getenv "XDG_CONFIG_HOME")))
    (if (and xdg (> (length xdg) 0))
        (merge-pathnames "icl/" (pathname (concatenate 'string xdg "/")))
        (merge-pathnames ".config/icl/" (user-homedir-pathname)))))

(defun state-directory ()
  "Return the state directory for ICL (for history, logs, etc).
   Windows: %LOCALAPPDATA%/icl/
   POSIX: $XDG_STATE_HOME/icl/ (default: ~/.local/state/icl/)"
  #+windows
  (let ((localappdata (uiop:getenv "LOCALAPPDATA")))
    (if (and localappdata (> (length localappdata) 0))
        (merge-pathnames "icl/" (pathname (concatenate 'string localappdata "/")))
        (merge-pathnames "icl/" (user-homedir-pathname))))
  #-windows
  (let ((xdg (uiop:getenv "XDG_STATE_HOME")))
    (if (and xdg (> (length xdg) 0))
        (merge-pathnames "icl/" (pathname (concatenate 'string xdg "/")))
        (merge-pathnames ".local/state/icl/" (user-homedir-pathname)))))

(defun data-directory ()
  "Return the data directory for ICL (for extracted resources, etc).
   Windows: %LOCALAPPDATA%/icl/
   POSIX: $XDG_DATA_HOME/icl/ (default: ~/.local/share/icl/)"
  #+windows
  (let ((localappdata (uiop:getenv "LOCALAPPDATA")))
    (if (and localappdata (> (length localappdata) 0))
        (merge-pathnames "icl/" (pathname (concatenate 'string localappdata "/")))
        (merge-pathnames "icl/" (user-homedir-pathname))))
  #-windows
  (let ((xdg (uiop:getenv "XDG_DATA_HOME")))
    (if (and xdg (> (length xdg) 0))
        (merge-pathnames "icl/" (pathname (concatenate 'string xdg "/")))
        (merge-pathnames ".local/share/icl/" (user-homedir-pathname)))))

(defun config-file ()
  "Return the config file path, computing it if needed.
   Checks ~/.iclrc first for backwards compatibility, then XDG location."
  (or *config-file*
      (setf *config-file*
            (let ((legacy-config (merge-pathnames ".iclrc" (user-homedir-pathname))))
              (if (probe-file legacy-config)
                  legacy-config
                  (merge-pathnames "config.lisp" (config-directory)))))))

(defun history-file ()
  "Return the history file path, computing it if needed."
  (or *history-file*
      (setf *history-file*
            (merge-pathnames "history" (state-directory)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Hooks
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *before-eval-hook* nil
  "List of functions called before evaluation. Each receives the form.")

(defvar *after-eval-hook* nil
  "List of functions called after evaluation. Each receives form and values list.")

(defvar *prompt-hook* nil
  "Function to customize prompt. Receives package, returns prompt string.
   If NIL, uses default *prompt-string* format.")

(defvar *error-hook* nil
  "Function called on errors. Receives condition, can handle or decline.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; State
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *input-count* 0
  "Count of inputs processed in this session.")

(defvar *in-repl* nil
  "T when ICL REPL is active.")

(defvar *verbose* nil
  "When T, print verbose startup information.")

(defvar *external-slynk-connection* nil
  "T when connected to an external Slynk server via --connect.
   When NIL, we spawned our own inferior lisp process.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Command Output Capture
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *last-command-output* nil
  "The output from the last command (for use with ,explain).")

(defvar *last-command-name* nil
  "The name of the last command that produced output.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Last Action Tracking
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *last-was-error* nil
  "T if the last REPL action resulted in an error.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Extended REPL History (for MCP access)
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *repl-history* nil
  "List of recent REPL interactions: ((input result error-p) ...).
   Most recent first. Used by MCP server for get_repl_history tool.")

(defvar *repl-history-max* 50
  "Maximum number of REPL interactions to keep in *repl-history*.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; AI CLI Configuration
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *ai-cli* nil
  "AI CLI tool to use for ,explain command.
   Valid values: :claude, :gemini, :codex, or NIL for auto-detect.
   Auto-detection tries claude, gemini, codex in order.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Color Configuration (256-color codes)
;;; ─────────────────────────────────────────────────────────────────────────────

;;; Syntax highlighting color codes (0-255)
(defvar *hl-keyword-color* 37
  "Color code for keywords (like :foo).")
(defvar *hl-string-color* 178
  "Color code for string literals.")
(defvar *hl-comment-color* 243
  "Color code for comments.")
(defvar *hl-number-color* 33
  "Color code for numeric literals.")
(defvar *hl-symbol-color* 252
  "Color code for regular symbols.")
(defvar *hl-package-color* 75
  "Color code for package prefixes.")
(defvar *hl-special-color* 205
  "Color code for special forms and macros.")

;;; UI color codes
(defvar *color-prompt* 75
  "Color code for main prompt.")
(defvar *color-package* 243
  "Color code for package name in prompt.")
(defvar *color-error* 196
  "Color code for error messages.")
(defvar *color-warning* 214
  "Color code for warning messages.")
(defvar *color-info* 75
  "Color code for info messages.")
(defvar *color-dim* 243
  "Color code for dimmed/secondary text.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; ANSI Escape Sequences
;;; ─────────────────────────────────────────────────────────────────────────────

;;; Common control sequences
(defvar *ansi-reset* (format nil "~C[0m" #\Escape)
  "Reset all text attributes.")
(defvar *ansi-bold* (format nil "~C[1m" #\Escape)
  "Bold text.")
(defvar *ansi-dim* (format nil "~C[2m" #\Escape)
  "Dim text.")
(defvar *ansi-reverse* (format nil "~C[7m" #\Escape)
  "Reverse video.")

;;; Color sequences (generated from color codes)
(defvar *ansi-prompt* (format nil "~C[38;5;75m" #\Escape)
  "ANSI sequence for prompt color.")
(defvar *ansi-package* (format nil "~C[38;5;243m" #\Escape)
  "ANSI sequence for package color.")
(defvar *ansi-error* (format nil "~C[38;5;196m" #\Escape)
  "ANSI sequence for error color.")
(defvar *ansi-warning* (format nil "~C[38;5;214m" #\Escape)
  "ANSI sequence for warning color.")
(defvar *ansi-info* (format nil "~C[38;5;75m" #\Escape)
  "ANSI sequence for info color.")
(defvar *ansi-fg-gray* (format nil "~C[38;5;245m" #\Escape)
  "ANSI sequence for gray text.")
