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

(defvar *inferior-output-thread* nil
  "Thread reading output from inferior process.")

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

(defun lisp-available-p (impl)
  "Check if Lisp implementation IMPL is available in PATH."
  (let ((program (find-lisp-program impl)))
    (when program
      (or (probe-file (format nil "/usr/bin/~A" program))
          (ignore-errors
            (uiop:run-program (list "which" program)
                              :output :string
                              :ignore-error-status t))))))

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
   3. /usr/share/icl/slynk/ (installed)"
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
      (return-from find-bundled-slynk local-slynk)))
  ;; 3. System install location
  (let ((system-slynk (pathname "/usr/share/icl/slynk/slynk-loader.lisp")))
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

(defun generate-slynk-init (port)
  "Generate Lisp code to load and start Slynk on PORT."
  (if *use-embedded-slynk*
      ;; Use our embedded minimal Slynk
      (generate-embedded-slynk-init port)
      ;; Try to find system Slynk
      (let ((slynk-loader (find-slynk-loader)))
        (if slynk-loader
            ;; Use existing Slynk installation
            (format nil "(progn
  (load ~S)
  (funcall (read-from-string \"slynk-loader:init\"))
  ;; Disable auth/secret expectations and SWANK->SLYNK translation
  (let ((secret (find-symbol \"SLY-SECRET\" :slynk)))
    (when secret (setf (symbol-function secret) (lambda () nil))))
  (let ((auth (find-symbol \"AUTHENTICATE-CLIENT\" :slynk)))
    (when auth (setf (symbol-function auth) (lambda (stream) (declare (ignore stream)) nil))))
  (let ((x (find-symbol \"*TRANSLATING-SWANK-TO-SLYNK*\" :slynk-rpc)))
    (when x (setf (symbol-value x) nil)))
  (funcall (read-from-string \"slynk:create-server\")
           :port ~D :dont-close t))"
                    (namestring slynk-loader) port)
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
      (getf (cdr entry) :program))))

(defun get-lisp-args (impl)
  "Get command-line arguments for Lisp implementation IMPL."
  (let ((entry (assoc impl *lisp-implementations*)))
    (when entry
      (getf (cdr entry) :args))))

(defun get-lisp-eval-arg (impl)
  "Get the eval command-line argument for Lisp implementation IMPL."
  (let ((entry (assoc impl *lisp-implementations*)))
    (when entry
      (or (getf (cdr entry) :eval-arg) "--eval"))))

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
    (unless (probe-file (format nil "/usr/bin/~A" program))
      (unless (ignore-errors
                (uiop:run-program (list "which" program)
                                  :output :string))
        (error "Cannot find ~A in PATH" program)))
    ;; Build the Slynk initialization code
    (let* ((init-code (generate-slynk-init actual-port))
           (eval-arg (get-lisp-eval-arg lisp))
           (args (append (get-lisp-args lisp)
                         (list eval-arg init-code))))
      (format t "~&; Starting ~A on port ~D...~%" lisp actual-port)
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
      ;; Wait for Slynk to start
      (sleep 2)  ; Give it time to initialize
      ;; Try to connect
      (let ((attempts 0)
            (max-attempts 10))
        (loop
          (when (slynk-connect :port actual-port)
            (return t))
          (incf attempts)
          (when (>= attempts max-attempts)
            (stop-inferior-lisp)
            (error "Failed to connect to Slynk after ~D attempts" max-attempts))
          (format t "~&; Waiting for Slynk to start (attempt ~D/~D)...~%"
                  attempts max-attempts)
          (sleep 1)))))))

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

;; *use-slynk* is defined in specials.lisp

(defun ensure-backend ()
  "Ensure a Lisp backend is available.
   If *use-slynk* is T, ensures Slynk connection.
   Otherwise uses local evaluation."
  (when (and *use-slynk* (not *slynk-connected-p*))
    (unless (inferior-lisp-alive-p)
      (start-inferior-lisp))
    (unless *slynk-connected-p*
      (error "Cannot connect to Slynk backend"))))

(defun backend-eval (string)
  "Evaluate STRING using the current backend.
   Returns (values result-values output-string)."
  (if *use-slynk*
      (progn
        (ensure-backend)
        (slynk-eval-form string))
      ;; Local evaluation (original behavior)
      (let ((form (read-form string)))
        (run-before-eval-hooks form)
        (let ((values (eval-form form)))
          (update-history form values)
          (run-after-eval-hooks form values)
          (values values nil)))))

(defun backend-complete (prefix package)
  "Get completions for PREFIX in PACKAGE using current backend."
  (if *use-slynk*
      (progn
        (ensure-backend)
        (slynk-complete-simple prefix :package package))
      ;; Local completion (use existing completion functions)
      (complete-symbol prefix (find-package package))))

(defun backend-documentation (name type)
  "Get documentation for NAME of TYPE using current backend."
  (if *use-slynk*
      (progn
        (ensure-backend)
        (slynk-documentation name type))
      ;; Local documentation
      (documentation (find-symbol (string-upcase name) *icl-package*) type)))

(defun backend-describe (name)
  "Describe NAME using current backend."
  (if *use-slynk*
      (progn
        (ensure-backend)
        (slynk-describe name))
      ;; Local describe - capture output to string
      (with-output-to-string (*standard-output*)
        (describe (parse-symbol-arg name)))))

(defun backend-apropos (pattern)
  "Search for symbols matching PATTERN using current backend."
  (if *use-slynk*
      (progn
        (ensure-backend)
        (slynk-apropos pattern))
      ;; Local apropos
      (apropos-list pattern)))

(defun backend-set-package (package-name)
  "Change current package using current backend."
  (if *use-slynk*
      (progn
        (ensure-backend)
        (slynk-set-package package-name))
      ;; Local package change
      (let ((pkg (find-package (string-upcase package-name))))
        (when pkg
          (setf *icl-package* pkg)
          pkg))))
