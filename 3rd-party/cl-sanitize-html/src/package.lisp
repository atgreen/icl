;;; package.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025 Anthony Green

(defpackage #:sanitize-html
  (:use #:cl)
  (:import-from #:alexandria
                #:when-let
                #:if-let)
  (:documentation "OWASP-style HTML sanitization library for Common Lisp")
  (:export
   ;; Main API
   #:sanitize
   #:sanitize-html

   ;; Policies
   #:*default-policy*
   #:*strict-policy*
   #:*email-policy*
   #:make-policy

   ;; Policy accessors
   #:policy-allowed-tags
   #:policy-allowed-attributes
   #:policy-allowed-protocols
   #:policy-allowed-css-properties
   #:policy-remove-comments
   #:policy-escape-cdata

   ;; Utilities
   #:safe-url-p
   #:sanitize-url))

(in-package #:sanitize-html)
