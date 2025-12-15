;;; backend.lisp --- Inferior Lisp process management for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;; Load socket support for port checking
#+sbcl (eval-when (:compile-toplevel :load-toplevel :execute)
         (require :sb-bsd-sockets))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Configuration
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *inferior-process* nil
  "The inferior Lisp process.")

(defvar *lisp-implementations*
  '((:sbcl :program "sbcl" :args ("--noinform") :eval-arg "--eval")
    (:ccl :program "ccl" :args nil :eval-arg "--eval")
    (:ecl :program "ecl" :args nil :eval-arg "-eval")
    (:clisp :program "clisp" :args nil :eval-arg "-x")
    (:abcl :program "abcl" :args nil :eval-arg "--eval")
    (:clasp :program "clasp" :args nil :eval-arg "--eval"))
  "Known Lisp implementations and how to invoke them.")

(defvar *default-lisp* :sbcl
  "Default Lisp implementation to use.")

(defvar *lisp-implementation-order*
  '(:sbcl :ccl :ecl :clisp :abcl :clasp)
  "Order in which to try Lisp implementations when auto-detecting.")

(defvar *current-lisp* nil
  "Currently running Lisp implementation.")

(defun configure-lisp (impl &key program args eval-arg)
  "Configure how to invoke a Lisp implementation.
   IMPL - keyword like :sbcl, :ccl, etc.
   PROGRAM - path to executable (e.g., \"/opt/sbcl/bin/sbcl\")
   ARGS - list of extra command-line arguments
   EVAL-ARG - the eval flag (e.g., \"--eval\")

   Example in ~/.iclrc:
     (icl:configure-lisp :sbcl
       :program \"/opt/sbcl-dev/bin/sbcl\"
       :args '(\"--dynamic-space-size\" \"8192\"))"
  (let ((entry (assoc impl *lisp-implementations*)))
    (if entry
        ;; Update existing entry
        (let ((plist (rest entry)))
          (when program (setf (getf plist :program) program))
          (when args (setf (getf plist :args) args))
          (when eval-arg (setf (getf plist :eval-arg) eval-arg))
          (setf (rest entry) plist))
        ;; Add new entry
        (push (list* impl
                     :program (or program (string-downcase (symbol-name impl)))
                     :args args
                     :eval-arg (or eval-arg "--eval"))
              *lisp-implementations*))
    impl))

(defun lisp-available-p (impl)
  "Check if Lisp implementation IMPL is available in PATH."
  (let ((program (find-lisp-program impl)))
    (and program (program-exists-p program))))

(defun find-available-lisp ()
  "Find the first available Lisp implementation according to *lisp-implementation-order*.
   Returns the implementation keyword or NIL if none found."
  (dolist (impl *lisp-implementation-order*)
    (when (lisp-available-p impl)
      (return-from find-available-lisp impl)))
  nil)

(defun list-available-lisps ()
  "Return list of available Lisp implementations."
  (remove-if-not #'lisp-available-p *lisp-implementation-order*))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Slynk Loader Generation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun find-bundled-slynk ()
  "Find bundled Slynk shipped with ICL.
   Search order:
   1. ICL_SLYNK_PATH environment variable
   2. ./slynk/ relative to executable (development)
   3. ./ocicl/sly-*/slynk/ relative to executable (ocicl development)
   4. /usr/share/icl/sly/slynk/ (installed, Unix only)"
  (let ((env-path (uiop:getenv "ICL_SLYNK_PATH")))
    ;; 1. Environment variable override
    (when (and env-path (probe-file env-path))
      (let ((loader (merge-pathnames "slynk-loader.lisp" env-path)))
        (when (probe-file loader)
          (return-from find-bundled-slynk loader)))))
  ;; 2. Relative to executable (for development/build directory)
  (let* ((exe-dir (or (and (boundp 'sb-ext:*runtime-pathname*)
                           (symbol-value 'sb-ext:*runtime-pathname*)
                           (uiop:pathname-directory-pathname
                            (symbol-value 'sb-ext:*runtime-pathname*)))
                      *default-pathname-defaults*))
         (local-slynk (merge-pathnames "slynk/slynk-loader.lisp" exe-dir)))
    (when (probe-file local-slynk)
      (return-from find-bundled-slynk local-slynk))
    ;; 3. Check ocicl directory for sly-*/slynk (wildcard match)
    (let ((ocicl-pattern (merge-pathnames "ocicl/sly-*/slynk/slynk-loader.lisp" exe-dir)))
      (let ((matches (directory ocicl-pattern)))
        (when matches
          (return-from find-bundled-slynk (first matches))))))
  ;; 4. System install location (Unix only)
  #-windows
  (let ((system-slynk (pathname "/usr/share/icl/sly/slynk/slynk-loader.lisp")))
    (when (probe-file system-slynk)
      (return-from find-bundled-slynk system-slynk)))
  nil)

(defun find-slynk-loader ()
  "Find the slynk-loader.lisp file.
   First checks bundled Slynk, then falls back to system locations."
  ;; Try bundled Slynk first
  (let ((bundled (find-bundled-slynk)))
    (when bundled
      (return-from find-slynk-loader bundled)))
  ;; Fall back to system Slynk locations
  (let ((candidates
         (list
          ;; Quicklisp location
          (merge-pathnames "dists/quicklisp/software/sly-*/slynk/slynk-loader.lisp"
                           (or #+quicklisp ql:*quicklisp-home*
                               (merge-pathnames "quicklisp/" (user-homedir-pathname))))
          ;; Common manual install locations
          (merge-pathnames ".emacs.d/elpa/sly-*/slynk/slynk-loader.lisp"
                           (user-homedir-pathname))
          (merge-pathnames ".emacs.d/straight/build/sly/slynk/slynk-loader.lisp"
                           (user-homedir-pathname))
          (merge-pathnames ".emacs.d/straight/repos/sly/slynk/slynk-loader.lisp"
                           (user-homedir-pathname))
          "/usr/share/common-lisp/source/sly/slynk/slynk-loader.lisp"
          "/usr/local/share/sly/slynk/slynk-loader.lisp")))
    ;; Try each candidate (some have wildcards, so we use directory)
    (dolist (pattern candidates)
      (let ((matches (directory pattern)))
        (when matches
          (return-from find-slynk-loader (first matches)))))
    nil))

(defvar *use-embedded-slynk* nil
  "If T, use embedded Slynk server. If NIL, try to find system Slynk.")

(defun find-slynk-asd ()
  "Find the slynk.asd file (ASDF system definition).
   Returns the directory containing slynk.asd, or NIL."
  ;; Check bundled location first (relative to slynk-loader.lisp)
  (let ((loader (find-slynk-loader)))
    (when loader
      (let ((asd (merge-pathnames "slynk.asd" (uiop:pathname-directory-pathname loader))))
        (when (probe-file asd)
          (return-from find-slynk-asd (uiop:pathname-directory-pathname asd))))))
  nil)

(defun generate-slynk-init (port)
  "Generate Lisp code to load and start Slynk on PORT."
  (if *use-embedded-slynk*
      ;; Use our embedded minimal Slynk
      (generate-embedded-slynk-init port)
      ;; Try to find system Slynk
      (let ((slynk-dir (find-slynk-asd)))
        (if slynk-dir
            ;; Use ASDF to load Slynk (leverages FASL caching)
            (format nil "(progn
  (require :asdf)
  (push ~S asdf:*central-registry*)
  (let ((*debug-io* (make-broadcast-stream)))
    (asdf:load-system :slynk))
  ;; Disable auth/secret expectations and SWANK->SLYNK translation
  (let ((secret (find-symbol \"SLY-SECRET\" :slynk)))
    (when secret (setf (symbol-function secret) (lambda () nil))))
  (let ((auth (find-symbol \"AUTHENTICATE-CLIENT\" :slynk)))
    (when auth (setf (symbol-function auth) (lambda (stream) (declare (ignore stream)) nil))))
  (let ((x (find-symbol \"*TRANSLATING-SWANK-TO-SLYNK*\" :slynk-rpc)))
    (when x (setf (symbol-value x) nil)))
  (funcall (read-from-string \"slynk:create-server\")
           :port ~D :dont-close t))"
                    (namestring slynk-dir) port)
            ;; Try Quicklisp
            (format nil "(progn
  (unless (find-package :quicklisp)
    (let ((ql-init (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname))))
      (when (probe-file ql-init)
        (load ql-init))))
  (if (find-package :quicklisp)
      (progn
        (funcall (read-from-string \"ql:quickload\") :slynk :silent t)
        (funcall (read-from-string \"slynk:create-server\")
                 :port ~D :dont-close t))
      (error \"Cannot find Slynk. Install SLY or Quicklisp.\")))"
                    port)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Process Management
;;; ─────────────────────────────────────────────────────────────────────────────

(defun find-lisp-program (impl)
  "Find the program name for Lisp implementation IMPL."
  (let ((entry (assoc impl *lisp-implementations*)))
    (when entry
      (getf (rest entry) :program))))

(defun program-exists-p (program)
  "Return T if PROGRAM can be found in PATH.
   Uses portable PATH search instead of shell commands."
  ;; First check if it's an absolute/relative path that exists
  (when (probe-file program)
    (return-from program-exists-p t))
  ;; Search PATH directories
  (let ((path-dirs (uiop:getenv-absolute-directories "PATH"))
        ;; On Windows, check common executable extensions
        #+windows (extensions '("" ".exe" ".cmd" ".bat" ".com"))
        #-windows (extensions '("")))
    (dolist (dir path-dirs)
      (dolist (ext extensions)
        (let ((full-path (merge-pathnames (concatenate 'string program ext) dir)))
          (when (probe-file full-path)
            (return-from program-exists-p t)))))
    nil))

(defun get-lisp-args (impl)
  "Get command-line arguments for Lisp implementation IMPL."
  (let ((entry (assoc impl *lisp-implementations*)))
    (when entry
      (getf (rest entry) :args))))

(defun get-lisp-eval-arg (impl)
  "Get the eval command-line argument for Lisp implementation IMPL."
  (let ((entry (assoc impl *lisp-implementations*)))
    (when entry
      (or (getf (rest entry) :eval-arg) "--eval"))))

(defun port-in-use-p (port)
  "Check if PORT is already in use (by any service, not just Slynk)."
  #+sbcl
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream :protocol :tcp)))
        (unwind-protect
             (progn
               (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
               (sb-bsd-sockets:socket-bind socket #(127 0 0 1) port)
               nil)  ; Port is free
          (sb-bsd-sockets:socket-close socket)))
    (sb-bsd-sockets:address-in-use-error () t)
    (error () t))
  #-sbcl
  nil)

(defun find-free-port (&optional (start-port 10000) (max-attempts 100))
  "Find a free port starting from START-PORT."
  (loop for port from start-port
        for attempts from 0 below max-attempts
        unless (port-in-use-p port)
          return port
        finally (error "Could not find a free port after ~D attempts" max-attempts)))

(defun start-inferior-lisp (&key (lisp *default-lisp*) (port nil))
  "Start an inferior Lisp process with Slynk.
   If PORT is NIL, automatically finds a free port.
   If PORT is specified and in use, errors."
  ;; Find a free port if not specified
  (let ((actual-port (or port (find-free-port))))
    (when (and port (port-in-use-p port))
      (error "Port ~D is already in use" port))
    (setf *slynk-port* actual-port)
    ;; Start the inferior Lisp
    (let ((program (find-lisp-program lisp)))
    (unless program
      (error "Unknown Lisp implementation: ~A" lisp))
    ;; Check if program exists
    (unless (program-exists-p program)
      (error "Cannot find ~A in PATH" program))
    ;; Build the Slynk initialization code
    (let* ((init-code (generate-slynk-init actual-port))
           (eval-arg (get-lisp-eval-arg lisp))
           (args (append (get-lisp-args lisp)
                         (list eval-arg init-code))))
      #+sbcl
      (setf *inferior-process*
            (sb-ext:run-program program args
                                :search t
                                :wait nil
                                :input :stream
                                :output :stream
                                :error :output))
      #-sbcl
      (error "Process spawning not implemented for this Lisp")
      (setf *current-lisp* lisp)
      ;; Wait for Slynk to start with spinner
      (let ((ticks 0)
            (max-ticks 100)  ; 10 seconds max
            (message (format nil "Starting ~A..." lisp)))
        (loop
          (show-spinner message)
          (sleep 0.1)
          (incf ticks)
          ;; Try to connect after initial delay, then every 0.5s
          (when (and (>= ticks 15)  ; First attempt after 1.5s
                     (zerop (mod ticks 5)))
            (when (slynk-connect :port actual-port)
              (clear-spinner)
              (return t)))
          (when (>= ticks max-ticks)
            (clear-spinner)
            (stop-inferior-lisp)
            (error "Failed to connect to Slynk after ~D seconds" (/ max-ticks 10)))))))))

(defun stop-inferior-lisp ()
  "Stop the inferior Lisp process."
  (when *inferior-process*
    ;; Try graceful shutdown first
    (when *slynk-connected-p*
      (ignore-errors
        (slynk-client:slime-eval '(cl-user::quit) *slynk-connection*)))
    (slynk-disconnect)
    ;; Give it a moment
    (sleep 0.5)
    ;; Force kill if still running
    #+sbcl
    (when (sb-ext:process-alive-p *inferior-process*)
      (sb-ext:process-kill *inferior-process* 15)  ; SIGTERM
      (sleep 0.5)
      (when (sb-ext:process-alive-p *inferior-process*)
        (sb-ext:process-kill *inferior-process* 9)))  ; SIGKILL
    (setf *inferior-process* nil)
    (setf *current-lisp* nil)
    (format t "~&; Inferior Lisp stopped~%")))

(defun inferior-lisp-alive-p ()
  "Check if the inferior Lisp process is still running."
  (and *inferior-process*
       #+sbcl (sb-ext:process-alive-p *inferior-process*)
       #-sbcl nil))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Backend Interface
;;; ─────────────────────────────────────────────────────────────────────────────

(defun ensure-backend ()
  "Ensure a Lisp backend is available via Slynk."
  (when (not *slynk-connected-p*)
    (unless (inferior-lisp-alive-p)
      (start-inferior-lisp))
    (unless *slynk-connected-p*
      (error "Cannot connect to Slynk backend"))))

(defun backend-eval (string)
  "Evaluate STRING using the Slynk backend.
   Output streams to terminal. Returns result values."
  (ensure-backend)
  (slynk-eval-form string))

(defun backend-complete (prefix package)
  "Get completions for PREFIX in PACKAGE using Slynk backend."
  (ensure-backend)
  (slynk-complete-simple prefix :package package))

(defun backend-documentation (name type)
  "Get documentation for NAME of TYPE using Slynk backend."
  (ensure-backend)
  (slynk-documentation name type))

(defun backend-describe (name)
  "Describe NAME using Slynk backend."
  (ensure-backend)
  (slynk-describe name))

(defun backend-apropos (pattern)
  "Search for symbols matching PATTERN using Slynk backend."
  (ensure-backend)
  (slynk-apropos pattern))

(defun backend-set-package (package-name)
  "Change current package using Slynk backend."
  (ensure-backend)
  (slynk-set-package package-name))
