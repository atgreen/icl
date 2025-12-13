;;; specials.lisp --- Special variables and constants for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Version
;;; ─────────────────────────────────────────────────────────────────────────────

(version-string:define-version-parameter +version+ :icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Package Context
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *icl-package* (find-package :cl-user)
  "Current package context for evaluation.")

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

(defvar *history-file*
  (merge-pathnames ".icl_history" (user-homedir-pathname))
  "Path to persistent command history file.")

(defvar *history-size* 1000
  "Maximum number of history entries to keep.")

(defvar *config-file*
  (merge-pathnames ".iclrc" (user-homedir-pathname))
  "User configuration file path.")

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

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Backend Configuration
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *use-slynk* nil
  "T when using Slynk backend for evaluation.")

