;;; embedded-assets.lisp --- Embedded web assets for self-contained ICL distribution
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; This module provides embedded browser assets (JS, CSS) that are loaded
;;; at compile time and served directly from memory.

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Embedded Asset Data
;;; ─────────────────────────────────────────────────────────────────────────────

;; Define the hash table at all times (compile, load, execute)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *embedded-assets* (make-hash-table :test 'equal)
    "Hash table mapping asset filenames to their contents (strings)."))

;; Load assets at compile time and load time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((assets-dir (merge-pathnames "assets/" (asdf:system-source-directory :icl))))
    (dolist (filename '("dockview.min.js"
                        "dockview.css"
                        "xterm.min.js"
                        "xterm.css"
                        "xterm-addon-fit.min.js"
                        "WEB-LICENSES"))
      (let ((path (merge-pathnames filename assets-dir)))
        (when (probe-file path)
          (setf (gethash filename *embedded-assets*)
                (alexandria:read-file-into-string path)))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Asset Access
;;; ─────────────────────────────────────────────────────────────────────────────

(defun get-embedded-asset (filename)
  "Get an embedded asset by filename. Returns the content string or NIL."
  (gethash filename *embedded-assets*))

(defun embedded-asset-exists-p (filename)
  "Check if an embedded asset exists."
  (nth-value 1 (gethash filename *embedded-assets*)))
