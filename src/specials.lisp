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

(defvar *history-file* nil
  "Path to persistent command history file. Computed at runtime.")

(defun history-file ()
  "Return the history file path, computing it if needed."
  (or *history-file*
      (setf *history-file*
            (merge-pathnames ".icl_history" (user-homedir-pathname)))))

(defvar *history-size* 1000
  "Maximum number of history entries to keep.")

(defvar *config-file* nil
  "User configuration file path. Computed at runtime.")

(defun config-file ()
  "Return the config file path, computing it if needed."
  (or *config-file*
      (setf *config-file*
            (merge-pathnames ".iclrc" (user-homedir-pathname)))))

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
;;; AI CLI Configuration
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *ai-cli* nil
  "AI CLI tool to use for ,explain command.
   Valid values: :claude, :gemini, :codex, or NIL for auto-detect.
   Auto-detection tries claude, gemini, codex in order.")


