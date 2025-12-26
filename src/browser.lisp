;;; browser.lisp --- Core configuration for ICL browser
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; This module provides the VS Code-style system browser using Dockview,
;;; with dockable panels and xterm.js terminals.
;;;
;;; The browser functionality is split across multiple files:
;;;   - browser.lisp (this file) - Configuration and logging
;;;   - browser-query.lisp - Backend query helpers
;;;   - browser-websocket.lisp - WebSocket handlers and streams
;;;   - browser-ui.lisp - HTML/JS generation
;;;   - browser-server.lisp - Server setup and public API

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Debug Logging
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *browser-debug* nil
  "When T, enable verbose browser debug logging.")

(defvar *browser-log-stream* *error-output*
  "Stream for browser debug logging. Set at load time to the real terminal.")

(defun browser-log (format-string &rest args)
  "Log a debug message if *browser-debug* is enabled."
  (when *browser-debug*
    (apply #'format *browser-log-stream*
           (concatenate 'string "~&;; [BROWSER] " format-string "~%")
           args)
    (force-output *browser-log-stream*)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Configuration
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *browser-port* 9000
  "Port for the browser server.")

(defvar *browser-acceptor* nil
  "The Hunchentoot acceptor for the browser.")

(defvar *browser-token* nil
  "Security token for browser URL path.")

(defvar *browser-path* nil
  "Random path prefix for the browser (e.g., /icl/abc123).")

(defvar *eval-generation-poller* nil
  "Thread that polls for external eval generation changes.")

(defvar *last-eval-generation* -1
  "Last seen eval generation from external Lisp.")

(defvar *repl-resource* nil
  "The singleton REPL WebSocket resource.")

(defvar *ws-flush-timer* nil
  "Timer thread for flushing WebSocket output.")

(defvar *ws-flush-timer-lock* (bt:make-lock "ws-flush-timer-lock")
  "Lock for WebSocket flush timer.")

(defvar *assets-directory* nil
  "Directory containing browser assets (JS, CSS). Computed lazily at runtime.")
