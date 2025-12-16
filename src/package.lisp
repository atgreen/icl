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
   #:*colors-enabled*
   #:*syntax-highlighting-enabled*
   #:*paredit-mode*

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
   #:configure-lisp
   #:slynk-connect
   #:slynk-disconnect
   #:start-inferior-lisp
   #:stop-inferior-lisp))
