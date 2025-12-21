;;; embedded-slynk.lisp --- Embedded Slynk for self-contained ICL distribution
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; This module provides embedded Slynk sources that can be extracted
;;; at runtime if Slynk is not found in standard locations.

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Embedded Slynk Data
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *embedded-slynk-zip* nil
  "Byte vector containing compressed Slynk sources.
   Loaded at compile time from slynk.zip if present.")

(defvar *embedded-slynk-version* nil
  "Version string for embedded Slynk (from ocicl package name).
   Used to create versioned extraction directory.")

;; Load slynk.zip and extract version at compile time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((zip-path (merge-pathnames "slynk.zip"
                                   (asdf:system-source-directory :icl))))
    (when (probe-file zip-path)
      (with-open-file (stream zip-path :element-type '(unsigned-byte 8))
        (let ((data (make-array (file-length stream)
                                :element-type '(unsigned-byte 8))))
          (read-sequence data stream)
          (setf *embedded-slynk-zip* data)))))
  ;; Extract version from ocicl sly-* directory name
  (let* ((ocicl-dir (merge-pathnames "ocicl/" (asdf:system-source-directory :icl)))
         (sly-dirs (directory (merge-pathnames "sly-*/" ocicl-dir))))
    (when sly-dirs
      (let* ((sly-dir (first sly-dirs))
             (dir-name (car (last (pathname-directory sly-dir)))))
        ;; Extract version part after "sly-" prefix
        (when (and dir-name (> (length dir-name) 4))
          (setf *embedded-slynk-version* (subseq dir-name 4)))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Extraction
;;; ─────────────────────────────────────────────────────────────────────────────

(defun user-slynk-directory ()
  "Return the user's versioned Slynk directory.
   E.g., ~/.local/share/icl/slynk-20251214-b01993c/ (Unix)
   or %LOCALAPPDATA%/icl/slynk-20251214-b01993c/ (Windows)."
  (let ((subdir (if *embedded-slynk-version*
                    (format nil "slynk-~A/" *embedded-slynk-version*)
                    "slynk/")))
    (merge-pathnames subdir (data-directory))))

(defun normalize-zip-path (name)
  "Convert zip entry name to native pathname.
   Zip files always use forward slashes, but Windows needs backslashes."
  #+windows
  (substitute #\\ #\/ name)
  #-windows
  name)

(defun extract-embedded-slynk ()
  "Extract embedded Slynk to user data directory.
   Returns the path to slynk-loader.lisp, or NIL if extraction failed."
  (unless *embedded-slynk-zip*
    (return-from extract-embedded-slynk nil))
  (let ((target-dir (user-slynk-directory)))
    (handler-case
        (progn
          (format *error-output* "~&; Extracting embedded Slynk to ~A...~%" target-dir)
          (ensure-directories-exist target-dir)
          ;; Use flexi-streams to create an in-memory stream from the byte vector
          ;; Note: open-zipfile-from-stream is internal but available
          (flexi-streams:with-input-from-sequence (stream *embedded-slynk-zip*)
            (let ((zf (zip::open-zipfile-from-stream stream)))
              (unwind-protect
                   (zip:do-zipfile-entries (name entry zf)
                     (let* ((native-name (normalize-zip-path name))
                            (path (merge-pathnames native-name target-dir)))
                       (ensure-directories-exist path)
                       ;; Skip directories (entries ending with / or \)
                       (unless (or (char= (char name (1- (length name))) #\/)
                                   (char= (char name (1- (length name))) #\\))
                         (with-open-file (out path
                                              :direction :output
                                              :if-exists :supersede
                                              :element-type '(unsigned-byte 8))
                           (write-sequence (zip:zipfile-entry-contents entry) out)))))
                (zip:close-zipfile zf))))
          ;; Return path to slynk-loader.lisp
          (let ((loader (merge-pathnames "slynk-loader.lisp" target-dir)))
            (when (probe-file loader)
              (format *error-output* "~&; Slynk extracted successfully.~%")
              loader)))
      (error (e)
        (format *error-output* "~&; Failed to extract Slynk: ~A~%" e)
        nil))))

(defun slynk-available-p ()
  "Check if embedded Slynk data is available."
  (and *embedded-slynk-zip* (> (length *embedded-slynk-zip*) 0)))

(defun user-slynk-loader ()
  "Return path to user's extracted slynk-loader.lisp if it exists."
  (let ((loader (merge-pathnames "slynk-loader.lisp" (user-slynk-directory))))
    (when (probe-file loader)
      loader)))
