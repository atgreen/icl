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

;; Define the hash tables at all times (compile, load, execute)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *embedded-assets* (make-hash-table :test 'equal)
    "Hash table mapping asset filenames to their contents (strings).")
  (defvar *embedded-binary-assets* (make-hash-table :test 'equal)
    "Hash table mapping asset filenames to their contents (byte vectors)."))

;; Load assets at compile time and load time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((assets-dir (merge-pathnames "assets/" (asdf:system-source-directory :icl))))
    ;; Main browser assets
    (dolist (filename '("dockview.min.js"
                        "dockview.css"
                        "xterm.min.js"
                        "xterm.css"
                        "xterm-addon-fit.min.js"
                        "viz-standalone.js"
                        "browser.css"
                        "browser.js"
                        "WEB-LICENSES"))
      (let ((path (merge-pathnames filename assets-dir)))
        (when (probe-file path)
          (setf (gethash filename *embedded-assets*)
                (alexandria:read-file-into-string path)))))
    ;; Speedscope assets (for flame graph profiling)
    (let ((speedscope-dir (merge-pathnames "speedscope/" assets-dir)))
      (dolist (filename '("index.html"
                          "speedscope.80eb88d2.js"
                          "import.7f8cb9f9.js"
                          "demangle-cpp.1768f4cc.js"
                          "source-map.438fa06b.js"
                          "reset.8c46b7a1.css"
                          "file-format-schema.json"
                          "LICENSE"))
        (let ((path (merge-pathnames filename speedscope-dir)))
          (when (probe-file path)
            (setf (gethash (concatenate 'string "speedscope/" filename) *embedded-assets*)
                  (alexandria:read-file-into-string path)))))
      ;; Binary assets (favicons)
      (dolist (filename '("favicon-16x16.f74b3187.png"
                          "favicon-32x32.bc503437.png"))
        (let ((path (merge-pathnames filename speedscope-dir)))
          (when (probe-file path)
            (setf (gethash (concatenate 'string "speedscope/" filename) *embedded-binary-assets*)
                  (alexandria:read-file-into-byte-vector path))))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Asset Access
;;; ─────────────────────────────────────────────────────────────────────────────

(defun get-embedded-asset (filename)
  "Get an embedded asset by filename. Returns the content string or NIL."
  (gethash filename *embedded-assets*))

(defun get-embedded-binary-asset (filename)
  "Get an embedded binary asset by filename. Returns the byte vector or NIL."
  (gethash filename *embedded-binary-assets*))

(defun embedded-asset-exists-p (filename)
  "Check if an embedded asset exists (text or binary)."
  (or (nth-value 1 (gethash filename *embedded-assets*))
      (nth-value 1 (gethash filename *embedded-binary-assets*))))
