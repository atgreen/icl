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

(defvar *output-reader-thread* nil
  "Background thread that reads output from the inferior Lisp process.")

(defvar *lisp-implementations*
  '((:sbcl :program "sbcl" :args ("--noinform") :eval-arg "--eval")
    (:ccl :program "ccl" :args nil :eval-arg "--eval")
    (:ecl :program "ecl" :args nil :eval-arg "-eval")
    (:clisp :program "clisp" :args nil :eval-arg "-x")
    (:abcl :program "abcl" :args nil :eval-arg "--eval")
    (:clasp :program "clasp" :args nil :eval-arg "--eval")
    (:roswell :program "ros" :args ("run" "--") :eval-arg "--eval"))
  "Known Lisp implementations and how to invoke them.")

(defvar *default-lisp* :sbcl
  "Default Lisp implementation to use.")

(defvar *lisp-implementation-order*
  '(:roswell :sbcl :ccl :ecl :clisp :abcl :clasp)
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

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Slynk Loader Generation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun find-bundled-asdf ()
  "Find bundled ASDF shipped with ICL.
   Search order:
   1. ICL_ASDF_PATH environment variable
   2. ./3rd-party/asdf/asdf.lisp relative to executable (development)
   3. /usr/share/icl/asdf/asdf.lisp (installed, Unix only)"
  ;; 1. Environment variable override
  (let ((env-path (uiop:getenv "ICL_ASDF_PATH")))
    (when (and env-path (probe-file env-path))
      (return-from find-bundled-asdf (pathname env-path))))
  ;; 2. Relative to executable (for development/build directory)
  (let* ((exe-dir (or (and (boundp 'sb-ext:*runtime-pathname*)
                           (symbol-value 'sb-ext:*runtime-pathname*)
                           (uiop:pathname-directory-pathname
                            (symbol-value 'sb-ext:*runtime-pathname*)))
                      *default-pathname-defaults*))
         (local-asdf (merge-pathnames "3rd-party/asdf/asdf.lisp" exe-dir)))
    (when (probe-file local-asdf)
      (return-from find-bundled-asdf local-asdf)))
  ;; 3. System install location (Unix only)
  #-windows
  (let ((system-asdf (pathname "/usr/share/icl/asdf/asdf.lisp")))
    (when (probe-file system-asdf)
      (return-from find-bundled-asdf system-asdf)))
  nil)

(defun find-bundled-slynk ()
  "Find bundled Slynk shipped with ICL.
   Search order:
   1. ICL_SLYNK_PATH environment variable
   2. ./slynk/ relative to executable (development)
   3. ./ocicl/sly-*/slynk/ relative to executable (ocicl development)
   4. ~/.local/share/icl/slynk-VERSION/ (user-extracted, versioned)
   5. Extract from embedded slynk.zip if available"
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
  ;; 4. Check user's extracted slynk directory
  (let ((user-slynk (user-slynk-loader)))
    (when user-slynk
      (return-from find-bundled-slynk user-slynk)))
  ;; 5. Extract from embedded slynk.zip if available
  (when (slynk-available-p)
    (let ((extracted (extract-embedded-slynk)))
      (when extracted
        (return-from find-bundled-slynk extracted))))
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
      (let ((slynk-dir (find-slynk-asd))
            (asdf-file (find-bundled-asdf)))
        ;; Verbose: show paths
        (when *verbose*
          (format t "~&; Slynk directory: ~A~%" (or slynk-dir "(not found - trying Quicklisp)"))
          (format t "~&; Bundled ASDF: ~A~%" (or asdf-file "(not found)")))
        (if slynk-dir
            ;; Use ASDF to load Slynk (leverages FASL caching)
            ;; Use uiop:unix-namestring to ensure forward slashes on all platforms
            ;; Note: Use read-from-string for asdf: symbols to avoid reader errors
            ;; when ASDF isn't loaded yet (Windows SBCL doesn't preload ASDF)
            ;; First ensure ASDF is available - some Lisps (CLISP) don't bundle it
            (format nil "(progn
  ;; Ensure ASDF is available (some Lisps like CLISP don't bundle it)
  (unless (find-package :asdf)
    (handler-case
        (require :asdf)
      (error ()~@[
        ;; ASDF not built in, load bundled version
        (load ~S)~])))
  ;; Add current working directory to ASDF registry (allows loading local .asd files)
  ;; Use read-from-string for uiop:getcwd since UIOP package doesn't exist until ASDF loads
  (push (funcall (read-from-string \"uiop:getcwd\")) (symbol-value (read-from-string \"asdf:*central-registry*\")))
  (push ~S (symbol-value (read-from-string \"asdf:*central-registry*\")))
  ;; Load Slynk quietly (suppress all output including ASDF loader messages)
  (let* ((null-stream (make-broadcast-stream))
         (*standard-output* null-stream)
         (*error-output* null-stream)
         (*trace-output* null-stream)
         (*debug-io* (make-two-way-stream (make-concatenated-stream) null-stream)))
    (handler-bind ((warning #'muffle-warning))
      (funcall (read-from-string \"asdf:load-system\") :slynk)))
  ;; Disable auth/secret expectations and SWANK->SLYNK translation
  (let ((secret (find-symbol \"SLY-SECRET\" :slynk)))
    (when secret (setf (symbol-function secret) (lambda () nil))))
  (let ((auth (find-symbol \"AUTHENTICATE-CLIENT\" :slynk)))
    (when auth (setf (symbol-function auth) (lambda (stream) (declare (ignore stream)) nil))))
  (let ((x (find-symbol \"*TRANSLATING-SWANK-TO-SLYNK*\" :slynk-rpc)))
    (when x (setf (symbol-value x) nil)))
  ;; Configure worker thread bindings so *debug-io* output goes through stdout
  ;; instead of directly to the terminal. This allows ICL to capture all output.
  ;; NOTE: On CCL, binding IO streams to the main thread's streams in worker threads
  ;; can cause hangs, so we skip this for CCL.
  #-ccl
  (let ((bindings-var (find-symbol \"*DEFAULT-WORKER-THREAD-BINDINGS*\" :slynk)))
    (when bindings-var
      (let ((io (make-two-way-stream *standard-input* *standard-output*)))
        (setf (symbol-value bindings-var)
              (list (cons '*debug-io* io)
                    (cons '*query-io* io)
                    (cons '*terminal-io* io))))))
  ;; Start Slynk server quietly
  (let ((*standard-output* (make-broadcast-stream)))
    (funcall (read-from-string \"slynk:create-server\")
             :port ~D :dont-close t))
  ;; Give the server thread time to fully initialize before we consider it ready.
  ;; Some implementations (CCL, ABCL) need this delay for the accept thread to start.
  (sleep 2)
  ;; Keep process alive (needed for CLISP with -x which exits after eval)
  (loop (sleep 60)))"
                    (when asdf-file (uiop:unix-namestring asdf-file))
                    (uiop:unix-namestring slynk-dir)
                    port)
            ;; Slynk not found - error
            (error "Cannot find Slynk. This should not happen with embedded Slynk.")))))

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

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Output Reader Thread
;;; ─────────────────────────────────────────────────────────────────────────────

(defun start-output-reader ()
  "Start background thread to read and display inferior Lisp output."
  #+sbcl
  (when (and *inferior-process* (not *output-reader-thread*))
    (let ((stream (sb-ext:process-output *inferior-process*)))
      (when stream
        (setf *output-reader-thread*
              (sb-thread:make-thread
               (lambda ()
                 (unwind-protect
                      (loop
                        (handler-case
                            (let ((char (read-char stream nil :eof)))
                              (when (eq char :eof)
                                (return))
                              (let* ((eval-out (evaluating-session-output-stream))
                                     (active-out (active-repl-output-stream))
                                     (out (or eval-out active-out *standard-output*)))
                                (write-char char out)
                                ;; Flush on newlines for responsive output
                                (when (char= char #\Newline)
                                  (force-output out))))
                          (error () (return))))
                   ;; Cleanup
                   (setf *output-reader-thread* nil)))
               :name "icl-output-reader"))))))

(defun stop-output-reader ()
  "Stop the output reader thread."
  #+sbcl
  (when *output-reader-thread*
    (ignore-errors
      (sb-thread:terminate-thread *output-reader-thread*))
    (setf *output-reader-thread* nil)))

(defvar *use-image-cache* t
  "If T, use cached SBCL images for faster startup. Set to NIL to disable.")

(defun start-inferior-lisp (&key (lisp *default-lisp*) (port nil))
  "Start an inferior Lisp process with Slynk.
   If PORT is NIL, automatically finds a free port.
   If PORT is specified and in use, errors."
  ;; Find a free port if not specified
  (let ((actual-port (or port (find-free-port))))
    (when (and port (port-in-use-p port))
      (error "Port ~D is already in use" port))
    (setf *slynk-port* actual-port)
    ;; Verbose: show port
    (when *verbose*
      (format t "~&; Slynk port: ~D~%" actual-port))

    ;; For SBCL, try to use cached image for faster startup
    (when (and (eq lisp :sbcl) *use-image-cache*)
      (let ((cached-image (ensure-cached-sbcl-image)))
        (when cached-image
          (return-from start-inferior-lisp
            (start-inferior-sbcl-with-cache actual-port cached-image)))))

    ;; Fallback: start normally (for non-SBCL or when caching fails)
    (start-inferior-lisp-uncached lisp actual-port)))

(defun start-inferior-sbcl-with-cache (port cached-image)
  "Start SBCL using the cached Slynk image for fast startup."
  (when *verbose*
    (format t "~&; Using cached image: ~A~%" cached-image))

  ;; Start SBCL with the cached core
  #+sbcl
  (setf *inferior-process*
        (sb-ext:run-program "sbcl"
                            (list "--noinform"
                                  "--core" (namestring cached-image)
                                  (princ-to-string port))
                            :search t
                            :wait nil
                            :input :stream
                            :output :stream
                            :error :output))
  #-sbcl
  (error "Cached image startup only supported when running under SBCL")

  (setf *current-lisp* :sbcl)

  ;; Verbose: check if process started
  (when *verbose*
    (format t "~&; Process created: ~A~%" (if *inferior-process* "yes" "no"))
    #+sbcl
    (format t "~&; Process status: ~A~%" (sb-ext:process-status *inferior-process*)))

  ;; Wait for Slynk to start - should be much faster with cached image
  (let ((ticks 0)
        (max-ticks 100)  ; 10 seconds max (faster with cache)
        (message "Starting SBCL...")
        #+sbcl (proc-output (sb-ext:process-output *inferior-process*)))
    (loop
      (show-spinner message)
      (sleep 0.1)
      (incf ticks)
      ;; Verbose: show any output from inferior process
      #+sbcl
      (when (and *verbose* proc-output (listen proc-output))
        (clear-spinner)
        (loop while (listen proc-output)
              do (let ((char (read-char proc-output nil nil)))
                   (when char (write-char char))))
        (force-output))
      ;; Try to connect - with cached image, should connect quickly
      (when (and (>= ticks 5)  ; First attempt after 0.5s
                 (zerop (mod ticks 3)))  ; Then every 0.3s
        (when (slynk-connect :port port)
          (clear-spinner)
          (start-output-reader)
          (return t)))
      (when (>= ticks max-ticks)
        (clear-spinner)
        #+sbcl
        (when (and *verbose* proc-output)
          (loop while (listen proc-output)
                do (let ((char (read-char proc-output nil nil)))
                     (when char (write-char char))))
          (force-output))
        (stop-inferior-lisp)
        (error "Failed to connect to Slynk after ~D seconds" (/ max-ticks 10))))))

(defun start-inferior-lisp-uncached (lisp port)
  "Start an inferior Lisp process without using cached images."
  (let ((program (find-lisp-program lisp)))
    (unless program
      (error "Unknown Lisp implementation: ~A" lisp))
    ;; Verbose: show program
    (when *verbose*
      (format t "~&; Lisp program: ~A~%" program))
    ;; Check if program exists
    (unless (program-exists-p program)
      (error "Cannot find ~A in PATH" program))
    ;; Build the Slynk initialization code
    (let* ((init-code (generate-slynk-init port))
           (eval-arg (get-lisp-eval-arg lisp))
           (args (append (get-lisp-args lisp)
                         (list eval-arg init-code))))
      ;; Verbose: show init code
      (when *verbose*
        (format t "~&; Init code:~%~A~%~%" init-code))
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
      ;; Verbose: check if process started
      (when *verbose*
        (format t "~&; Process created: ~A~%" (if *inferior-process* "yes" "no"))
        #+sbcl
        (format t "~&; Process status: ~A~%" (sb-ext:process-status *inferior-process*)))
      ;; Wait for Slynk to start with spinner
      ;; Use real elapsed time instead of ticks to account for verification time
      (let ((start-time (get-internal-real-time))
            (max-seconds 60)  ; 60 seconds max (was 42s but CCL needs more time)
            (last-attempt-time 0)
            (attempt-interval 0.5)  ; seconds between attempts
            (initial-delay 4.0)  ; seconds before first attempt (allow time for Slynk init + 2s settle delay)
            (message (format nil "Starting ~A..." lisp))
            #+sbcl (proc-output (sb-ext:process-output *inferior-process*)))
        (loop
          (show-spinner message)
          (sleep 0.1)
          (let ((elapsed (/ (- (get-internal-real-time) start-time)
                            internal-time-units-per-second)))
            ;; Verbose: show any output from inferior process
            #+sbcl
            (when (and *verbose* proc-output (listen proc-output))
              (clear-spinner)
              (loop while (listen proc-output)
                    do (let ((char (read-char proc-output nil nil)))
                         (when char (write-char char))))
              (force-output))
            ;; Try to connect after initial delay, then at regular intervals
            (when (and (>= elapsed initial-delay)
                       (>= (- elapsed last-attempt-time) attempt-interval))
              (setf last-attempt-time elapsed)
              (when (slynk-connect :port port)
                (clear-spinner)
                ;; Start background thread to stream inferior Lisp output
                (start-output-reader)
                (return t)))
            (when (>= elapsed max-seconds)
              (clear-spinner)
              ;; Verbose: show final output before stopping
              #+sbcl
              (when (and *verbose* proc-output)
                (loop while (listen proc-output)
                      do (let ((char (read-char proc-output nil nil)))
                           (when char (write-char char))))
                (force-output))
              (stop-inferior-lisp)
              (error "Failed to connect to Slynk after ~D seconds" max-seconds))))))))

(defun stop-inferior-lisp ()
  "Stop the inferior Lisp process."
  (when *inferior-process*
    ;; Stop the output reader thread first
    (stop-output-reader)
    ;; Just disconnect - the process will be killed below if needed
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
    ;; For external connections, don't try to spawn a new process
    (when *external-slynk-connection*
      (error "Lost connection to external Slynk server"))
    (unless (inferior-lisp-alive-p)
      (start-inferior-lisp))
    (unless *slynk-connected-p*
      (error "Cannot connect to Slynk backend")))
  ;; Inject ICL runtime on first use
  (unless *icl-runtime-injected*
    (inject-icl-runtime)
    (setf *icl-runtime-injected* t)))

(defun backend-eval (string)
  "Evaluate STRING using the Slynk backend.
   Output streams to terminal. Returns result values.
   Updates REPL history variables (*, **, ***, etc.)."
  (ensure-backend)
  (slynk-eval-form string))

(defun backend-eval-internal (string)
  "Evaluate STRING using the Slynk backend for internal ICL operations.
   Output streams to terminal. Returns result values.
   Does NOT update REPL history variables (*, **, ***, etc.)."
  (ensure-backend)
  (slynk-eval-form-internal string))

(defun backend-eval-capture (string)
  "Evaluate STRING using the Slynk backend capturing stdout/stderr.
Returns two values: output-string and list of result strings."
  (ensure-backend)
  (slynk-eval-form-capturing string))

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
