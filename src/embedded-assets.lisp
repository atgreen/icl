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
                        "vega.min.js"
                        "vega-lite.min.js"
                        "vega-embed.min.js"
                        "mermaid.min.js"
                        "monaco-init.js"
                        "regulex.js"
                        "browser.css"
                        "browser.js"
                        "notebook.js"
                        "WEB-LICENSES"
                        "OPEN-SOURCE-NOTICES.txt"))
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
                  (alexandria:read-file-into-byte-vector path))))))
    ;; ICL favicon assets
    (dolist (filename '("favicon.ico"
                        "favicon-16.png"
                        "favicon-32.png"
                        "favicon-48.png"
                        "favicon-192.png"
                        "apple-touch-icon.png"))
      (let ((path (merge-pathnames filename assets-dir)))
        (when (probe-file path)
          (setf (gethash filename *embedded-binary-assets*)
                (alexandria:read-file-into-byte-vector path)))))
    ;; KaTeX assets (math in markdown cells): js/css as text, fonts as binary
    (let ((katex-dir (merge-pathnames "katex/" assets-dir)))
      (when (probe-file katex-dir)
        (labels ((collect-files (dir)
                   (let ((files nil))
                     (dolist (entry (uiop:directory-files dir)) (push entry files))
                     (dolist (subdir (uiop:subdirectories dir))
                       (setf files (nconc files (collect-files subdir))))
                     files)))
          (dolist (file (collect-files katex-dir))
            (let* ((relative-path (enough-namestring file katex-dir))
                   (key (concatenate 'string "katex/" relative-path))
                   (extension (pathname-type file)))
              (cond ((member extension '("woff2" "woff" "ttf") :test #'string=)
                     (setf (gethash key *embedded-binary-assets*)
                           (alexandria:read-file-into-byte-vector file)))
                    ((member extension '("js" "css") :test #'string=)
                     (setf (gethash key *embedded-assets*)
                           (alexandria:read-file-into-string file)))))))))
    ;; Monaco editor assets - recursively embed all files
    ;; Monaco Editor is pre-built with vite, generating ~120+ hashed .js/.css files
    ;; Rather than manually listing each file, we recursively discover all Monaco files
    (let ((monaco-dir (merge-pathnames "monaco/" assets-dir)))
      (when (probe-file monaco-dir)
        (labels ((collect-files (dir)
                   "Recursively collect all files in DIR"
                   (let ((files nil))
                     (dolist (entry (uiop:directory-files dir))
                       (push entry files))
                     (dolist (subdir (uiop:subdirectories dir))
                       (setf files (nconc files (collect-files subdir))))
                     files)))
          (let ((all-files (collect-files monaco-dir)))
            (dolist (file all-files)
              (let* ((relative-path (enough-namestring file monaco-dir))
                     (key (concatenate 'string "monaco/" relative-path))
                     (extension (pathname-type file)))
                ;; Binary assets (fonts)
                (cond ((string= extension "ttf")
                       (setf (gethash key *embedded-binary-assets*)
                             (alexandria:read-file-into-byte-vector file)))
                      ;; Text assets (js, css, html, txt)
                      ((member extension '("js" "css" "html" "txt" "LICENSE") :test #'string=)
                       (setf (gethash key *embedded-assets*)
                             (alexandria:read-file-into-string file)))
                      ;; Also handle LICENSE files without extension
                      ((and (null extension) (search "LICENSE" (pathname-name file)))
                       (setf (gethash key *embedded-assets*)
                             (alexandria:read-file-into-string file))))))))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Asset Access
;;; ─────────────────────────────────────────────────────────────────────────────

(defun get-embedded-asset (filename)
  "Get an embedded asset by filename. Returns the content string or NIL."
  (gethash filename *embedded-assets*))

(defun get-embedded-binary-asset (filename)
  "Get an embedded binary asset by filename. Returns the byte vector or NIL."
  (gethash filename *embedded-binary-assets*))
