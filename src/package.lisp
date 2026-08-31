;;; package.lisp --- Package definition for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(defpackage #:icl
  (:documentation "Interactive Common Lisp - an enhanced REPL for Common Lisp.")
  (:use #:cl)
  (:export
   ;; Main entry points
   #:main
   #:run-repl
   #:start-repl

   ;; Notebook data-file templates (extensible via ~/.iclrc)
   #:*notebook-templates*
   #:register-notebook-template
   #:notebook-template-for
   #:make-notebook-from-template

   ;; Configuration
   #:*icl-package*
   #:*icl-package-name*
   #:*prompt-string*
   #:*continuation-prompt*
   #:*result-prefix*
   #:*history-file*
   #:*history-size*
   #:*config-file*
   #:*use-multiline-editor*
   #:*inline-hints-enabled*
   #:*colors-enabled*
   #:*syntax-highlighting-enabled*
   #:*paredit-mode*
   #:*viz-package-exclusions*

   ;; Hooks
   #:*before-eval-hook*
   #:*after-eval-hook*
   #:*prompt-hook*
   #:*error-hook*

   ;; History variables (ICL-prefixed to avoid shadowing)
   #:icl-*
   #:icl-**
   #:icl-***
   #:icl-+
   #:icl-++
   #:icl-+++
   #:icl-/
   #:icl-//
   #:icl-///

   ;; IRB-style history shortcuts
   #:_
   #:__
   #:___

   ;; Command system
   #:define-command
   #:find-command
   #:list-commands

   ;; Slynk/Backend configuration
   #:*slynk-port*
   #:*slynk-host*
   #:*default-lisp*
   #:*slynk-connected-p*
   #:*external-slynk-connection*
   #:configure-lisp
   #:slynk-connect
   #:slynk-disconnect
   #:start-inferior-lisp
   #:stop-inferior-lisp

   ;; Browser
   #:start-browser
   #:stop-browser
   #:*browser-port*

   ;; Debug logging
   #:*browser-debug*
   #:*inspector-debug*

   ;; Theming
   #:*current-terminal-theme*
   #:*current-browser-theme*
   #:*default-dark-terminal-theme*
   #:*default-light-terminal-theme*
   #:*default-dark-browser-theme*
   #:*default-light-browser-theme*
   #:define-terminal-theme
   #:define-browser-theme
   #:set-terminal-theme
   #:set-browser-theme
   #:apply-terminal-theme
   #:apply-browser-theme
   #:list-terminal-themes
   #:list-browser-themes
   #:find-terminal-theme
   #:find-browser-theme
   #:auto-select-terminal-theme
   #:auto-select-browser-theme
   #:detect-terminal-dark-mode))
