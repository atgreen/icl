;;; image-cache.lisp --- Cached SBCL image with Slynk pre-loaded for fast startup
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; This module manages a cached SBCL core image with Slynk already loaded,
;;; enabling much faster inferior Lisp startup times.

(in-package #:icl)

;;; -------------------------------------------------------------------------
;;; Cache Path Management
;;; -------------------------------------------------------------------------

(defun icl-cache-directory ()
  "Return the ICL cache directory (~/.cache/icl/).
   Creates the directory if it doesn't exist."
  (let* ((xdg-cache (uiop:getenv "XDG_CACHE_HOME"))
         (base-cache (if (and xdg-cache (plusp (length xdg-cache)))
                         (pathname (concatenate 'string xdg-cache "/"))
                         (merge-pathnames ".cache/" (user-homedir-pathname))))
         (cache-dir (merge-pathnames "icl/" base-cache)))
    (ensure-directories-exist cache-dir)
    cache-dir))

(defun sbcl-version-string ()
  "Get SBCL version string for cache invalidation.
   Uses the system sbcl, not the one ICL was built with."
  (let ((result (ignore-errors
                  (uiop:run-program '("sbcl" "--version")
                                    :output '(:string :stripped t)
                                    :ignore-error-status t))))
    (if (and result (stringp result))
        ;; Extract version number from "SBCL 2.x.y" format
        (let ((space-pos (position #\Space result)))
          (if space-pos
              (subseq result (1+ space-pos))
              "unknown"))
        "unknown")))

;;; -------------------------------------------------------------------------
;;; Init File Hashing
;;; -------------------------------------------------------------------------

(defun sbcl-init-file-path ()
  "Return the path to the user's SBCL init file (.sbclrc).
   Returns NIL if not found."
  (let ((sbclrc (merge-pathnames ".sbclrc" (user-homedir-pathname))))
    (when (probe-file sbclrc)
      sbclrc)))

(defun file-content-hash (path)
  "Compute a short hash of file contents for cache invalidation.
   Returns a 7-character hex string, or 'noinit' if file doesn't exist."
  (if (and path (probe-file path))
      (let ((hash 0))
        ;; Simple but effective: FNV-1a-like hash of file contents
        (with-open-file (stream path :element-type '(unsigned-byte 8))
          (loop for byte = (read-byte stream nil nil)
                while byte
                do (setf hash (logxor hash byte))
                   (setf hash (logand #xFFFFFFFF
                                      (* hash 16777619)))))
        ;; Return first 7 hex chars
        (subseq (format nil "~8,'0X" (logand hash #xFFFFFFFF)) 0 7))
      "noinit"))

(defun sbcl-init-hash ()
  "Get hash of user's .sbclrc for cache key.
   Changes to .sbclrc will invalidate the cached image."
  (file-content-hash (sbcl-init-file-path)))

(defun cached-sbcl-image-path ()
  "Return the path for the cached SBCL+Slynk image.
   Includes SBCL version, ICL version, and .sbclrc hash for automatic invalidation.
   The init hash ensures cache rebuilds when user modifies their .sbclrc
   (e.g., adding Quicklisp), so --eval expressions have access to init-time setup."
  (let ((sbcl-version (sbcl-version-string))
        (icl-version +version+)
        (init-hash (sbcl-init-hash)))
    (merge-pathnames (format nil "icl-~A-sbcl-~A-~A.core"
                             icl-version sbcl-version init-hash)
                     (icl-cache-directory))))

(defun cached-sbcl-image-exists-p ()
  "Check if a valid cached SBCL image exists."
  (let ((path (cached-sbcl-image-path)))
    (and (probe-file path) path)))

;;; -------------------------------------------------------------------------
;;; Image Creation
;;; -------------------------------------------------------------------------

(defun generate-image-creation-code (output-path slynk-dir asdf-file)
  "Generate Lisp code that creates the cached SBCL+Slynk image.
   The image will have Slynk loaded and configured, ready to start a server."
  (format nil "(progn
  ;; Ensure ASDF is available
  (require :asdf)
  ~@[;; Load bundled ASDF if needed
  (load ~S)~]

  ;; Add Slynk to ASDF registry and load it
  (push ~S (symbol-value (read-from-string \"asdf:*central-registry*\")))
  (let ((*debug-io* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream))
        (*standard-output* (make-broadcast-stream)))
    (handler-bind ((warning #'muffle-warning))
      (funcall (read-from-string \"asdf:load-system\") :slynk)))

  ;; Pre-configure Slynk (disable auth/secret)
  (let ((secret (find-symbol \"SLY-SECRET\" :slynk)))
    (when secret (setf (symbol-function secret) (lambda () nil))))
  (let ((auth (find-symbol \"AUTHENTICATE-CLIENT\" :slynk)))
    (when auth (setf (symbol-function auth) (lambda (stream) (declare (ignore stream)) nil))))
  (let ((x (find-symbol \"*TRANSLATING-SWANK-TO-SLYNK*\" :slynk-rpc)))
    (when x (setf (symbol-value x) nil)))

  ;; Define the toplevel function for the cached image
  (defun icl-cached-slynk-main ()
    \"Toplevel for cached Slynk image. Reads port from command line.\"
    (let* ((args sb-ext:*posix-argv*)
           (port-arg (second args))
           (port (when port-arg (parse-integer port-arg :junk-allowed t))))
      (unless port
        (format *error-output* \"Usage: sbcl --core <image> <port>~%%\")
        (sb-ext:exit :code 1))
      ;; Add current working directory to ASDF registry (allows loading local .asd files)
      (push (uiop:getcwd) (symbol-value (read-from-string \"asdf:*central-registry*\")))
      ;; Configure worker thread bindings
      (let ((bindings-var (find-symbol \"*DEFAULT-WORKER-THREAD-BINDINGS*\" :slynk)))
        (when bindings-var
          (let ((io (make-two-way-stream *standard-input* *standard-output*)))
            (setf (symbol-value bindings-var)
                  (list (cons '*debug-io* io)
                        (cons '*query-io* io)
                        (cons '*terminal-io* io))))))
      ;; Start Slynk server (suppress startup message)
      ;; Slynk's init hook sets *log-output* to *error-output*, so we bind both
      (let* ((null-stream (make-broadcast-stream))
             (*standard-output* null-stream)
             (*error-output* null-stream))
        (funcall (read-from-string \"slynk:create-server\")
                 :port port :dont-close t))
      ;; Keep process alive
      (loop (sleep 60))))

  ;; Save the image
  (format t \"~~&Saving cached Slynk image to ~~A...~~%\" ~S)
  (sb-ext:save-lisp-and-die ~S
    :toplevel #'icl-cached-slynk-main
    :executable nil
    :compression t))"
          (when asdf-file (uiop:unix-namestring asdf-file))
          (uiop:unix-namestring slynk-dir)
          (namestring output-path)
          (namestring output-path)))

(defun create-cached-sbcl-image ()
  "Create a cached SBCL image with Slynk pre-loaded.
   Returns the path to the created image, or NIL on failure."
  (let* ((output-path (cached-sbcl-image-path))
         (slynk-dir (find-slynk-asd))
         ;; Don't load bundled ASDF - SBCL always has ASDF built-in
         (asdf-file nil))
    (unless slynk-dir
      (warn "Cannot find Slynk directory for image caching")
      (return-from create-cached-sbcl-image nil))

    (format t "~&; Creating cached SBCL+Slynk image (one-time setup)...~%")
    (force-output)

    (let* ((init-code (generate-image-creation-code output-path slynk-dir asdf-file))
           (process (sb-ext:run-program "sbcl"
                                        (list "--noinform" "--eval" init-code)
                                        :search t
                                        :wait t
                                        :output *standard-output*
                                        :error *error-output*)))
      (if (and (zerop (sb-ext:process-exit-code process))
               (probe-file output-path))
          (progn
            (format t "~&; Cached image created: ~A~%" output-path)
            output-path)
          (progn
            (warn "Failed to create cached SBCL image")
            nil)))))

(defun ensure-cached-sbcl-image ()
  "Ensure a cached SBCL image exists, creating it if necessary.
   Returns the path to the image, or NIL if caching is not possible."
  (or (cached-sbcl-image-exists-p)
      (create-cached-sbcl-image)))

;;; -------------------------------------------------------------------------
;;; Cache Management Commands
;;; -------------------------------------------------------------------------

(defun clear-image-cache ()
  "Remove all cached SBCL images."
  (let ((cache-dir (icl-cache-directory)))
    ;; Match all cached image formats (old and new naming schemes)
    (dolist (file (append (directory (merge-pathnames "sbcl-*-slynk.core" cache-dir))
                          (directory (merge-pathnames "icl-*.core" cache-dir))))
      (format t "~&; Removing ~A~%" file)
      (delete-file file))
    (format t "~&; Image cache cleared~%")))

(defun image-cache-info ()
  "Display information about the image cache."
  (let* ((cache-dir (icl-cache-directory))
         (images (append (directory (merge-pathnames "sbcl-*-slynk.core" cache-dir))
                         (directory (merge-pathnames "icl-*.core" cache-dir))))
         (current-path (cached-sbcl-image-path))
         (init-file (sbcl-init-file-path))
         (init-hash (sbcl-init-hash)))
    (format t "~&Cache directory: ~A~%" cache-dir)
    (format t "ICL version: ~A~%" +version+)
    (format t "SBCL version: ~A~%" (sbcl-version-string))
    (format t "Init file: ~A~%" (or init-file "(none)"))
    (format t "Init hash: ~A~%" init-hash)
    (format t "Expected image: ~A~%" (file-namestring current-path))
    (if images
        (progn
          (format t "~&Cached images:~%")
          (dolist (img images)
            (let* ((size (ignore-errors
                           (with-open-file (f img) (file-length f))))
                   (current (and (probe-file current-path)
                                 (equal (truename img) (truename current-path)))))
              (format t "  ~A~A~@[ (~,1f MB)~]~%"
                      (file-namestring img)
                      (if current " [current]" "")
                      (when size (/ size 1048576.0))))))
        (format t "~&No cached images found~%"))))
