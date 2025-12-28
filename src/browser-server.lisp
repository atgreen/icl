;;; browser-server.lisp --- HTTP server setup and public API for ICL browser
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; This module provides the Hunchentoot server setup, request dispatch,
;;; asset serving, and the public API (start-browser, stop-browser).

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Server Setup
;;; ─────────────────────────────────────────────────────────────────────────────

(defun find-browser-websocket-resource (request)
  "Find WebSocket resource for REQUEST."
  ;; WebSocket URL is /ws/{token}
  (let ((ws-path (format nil "/ws/~A" *browser-token*)))
    (when (string= (hunchentoot:script-name request) ws-path)
      *repl-resource*)))

;; Define a subclass to handle both HTTP and WebSocket
(defclass browser-acceptor (hunchensocket:websocket-acceptor)
  ()
  (:documentation "Acceptor for the browser that handles both HTTP and WebSocket."))

(defun find-assets-directory ()
  "Find the assets directory (fallback for development when assets aren't embedded).
   Search order:
   1. ICL_ASSETS_PATH environment variable
   2. ./assets/ relative to executable
   3. ./assets/ relative to ASDF system source (development)"
  ;; 1. Environment variable override
  (let ((env-path (uiop:getenv "ICL_ASSETS_PATH")))
    (when (and env-path (probe-file env-path))
      (return-from find-assets-directory (pathname env-path))))
  ;; 2. Relative to executable
  (let* ((exe-dir (or (and (boundp 'sb-ext:*runtime-pathname*)
                           (symbol-value 'sb-ext:*runtime-pathname*)
                           (uiop:pathname-directory-pathname
                            (symbol-value 'sb-ext:*runtime-pathname*)))
                      *default-pathname-defaults*))
         (local-assets (merge-pathnames "assets/" exe-dir)))
    (when (probe-file local-assets)
      (return-from find-assets-directory local-assets)))
  ;; 3. Fall back to ASDF system source (development)
  (merge-pathnames "assets/" (asdf:system-source-directory :icl)))

(defun get-assets-directory ()
  "Get the assets directory, computing it lazily if needed."
  (or *assets-directory*
      (setf *assets-directory* (find-assets-directory))))

(defun serve-asset (filename)
  "Serve an asset file, returning content and setting content-type.
   First checks embedded assets, then falls back to filesystem."
  (flet ((set-content-type ()
           (setf (hunchentoot:content-type*)
                 (cond
                   ((alexandria:ends-with-subseq ".css" filename) "text/css")
                   ((alexandria:ends-with-subseq ".js" filename) "application/javascript")
                   ((alexandria:ends-with-subseq ".png" filename) "image/png")
                   ((alexandria:ends-with-subseq ".ico" filename) "image/x-icon")
                   ((alexandria:ends-with-subseq ".txt" filename) "text/plain")
                   (t "application/octet-stream"))))
         (binary-asset-p ()
           (or (alexandria:ends-with-subseq ".png" filename)
               (alexandria:ends-with-subseq ".ico" filename))))
    ;; First try embedded text assets
    (let ((embedded (get-embedded-asset filename)))
      (when embedded
        (set-content-type)
        (return-from serve-asset embedded)))
    ;; Try embedded binary assets (favicons, images)
    (let ((binary (get-embedded-binary-asset filename)))
      (when binary
        (set-content-type)
        (return-from serve-asset binary)))
    ;; Fall back to filesystem (for development)
    (let ((filepath (merge-pathnames filename (get-assets-directory))))
      (when (probe-file filepath)
        (set-content-type)
        (if (binary-asset-p)
            (alexandria:read-file-into-byte-vector filepath)
            (alexandria:read-file-into-string filepath))))))

(defun serve-speedscope-asset (filename)
  "Serve a speedscope asset file."
  (let ((key (concatenate 'string "speedscope/" filename)))
    (flet ((set-content-type ()
             (setf (hunchentoot:content-type*)
                   (cond
                     ((alexandria:ends-with-subseq ".html" filename) "text/html")
                     ((alexandria:ends-with-subseq ".css" filename) "text/css")
                     ((alexandria:ends-with-subseq ".js" filename) "application/javascript")
                     ((alexandria:ends-with-subseq ".json" filename) "application/json")
                     ((alexandria:ends-with-subseq ".png" filename) "image/png")
                     ((alexandria:ends-with-subseq ".txt" filename) "text/plain")
                     (t "application/octet-stream")))))
      ;; Check text assets first
      (let ((embedded (get-embedded-asset key)))
        (when embedded
          (set-content-type)
          (return-from serve-speedscope-asset embedded)))
      ;; Check binary assets (favicons)
      (let ((binary (get-embedded-binary-asset key)))
        (when binary
          (set-content-type)
          (return-from serve-speedscope-asset binary)))
      ;; Fall back to filesystem (for development)
      (let ((filepath (merge-pathnames key (get-assets-directory))))
        (when (probe-file filepath)
          (set-content-type)
          (if (alexandria:ends-with-subseq ".png" filename)
              (alexandria:read-file-into-byte-vector filepath)
              (alexandria:read-file-into-string filepath))))
      ;; Default index.html for root
      (when (or (string= filename "") (string= filename "/"))
        (setf (hunchentoot:content-type*) "text/html")
        (or (get-embedded-asset "speedscope/index.html")
            (let ((filepath (merge-pathnames "speedscope/index.html" (get-assets-directory))))
              (when (probe-file filepath)
                (alexandria:read-file-into-string filepath))))))))

(defun serve-profile-data (profile-id)
  "Serve profile data JSON for the given PROFILE-ID."
  ;; Remove .json extension if present
  (let ((id (if (alexandria:ends-with-subseq ".json" profile-id)
                (subseq profile-id 0 (- (length profile-id) 5))
                profile-id)))
    (let ((data (get-profile id)))
      (if data
          (progn
            (setf (hunchentoot:content-type*) "application/json")
            ;; Add CORS header for speedscope
            (setf (hunchentoot:header-out :access-control-allow-origin) "*")
            data)
          (progn
            (setf (hunchentoot:return-code*) 404)
            "Profile not found")))))

(defun set-security-headers ()
  "Set security headers for HTTP responses.
   Content-Security-Policy restricts resource loading to prevent XSS.
   Other headers provide defense-in-depth."
  ;; Content Security Policy - defense in depth against XSS
  ;; - 'self' allows resources from same origin
  ;; - 'unsafe-inline' needed for inline styles (Dockview, Vega, xterm.js)
  ;; - 'unsafe-eval' needed for Vega expression evaluation (sandboxed by Vega)
  ;; - blob: needed for Vega image export
  ;; - data: needed for embedded images
  ;; - api.github.com needed for "Check for Updates" feature
  (setf (hunchentoot:header-out :content-security-policy)
        "default-src 'self'; script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline'; img-src 'self' blob: data:; font-src 'self'; connect-src 'self' ws://127.0.0.1:* wss://127.0.0.1:* https://api.github.com; frame-ancestors 'self';")
  ;; Allow same-origin framing (needed for flame graph panel)
  (setf (hunchentoot:header-out :x-frame-options) "SAMEORIGIN")
  ;; Prevent MIME type sniffing
  (setf (hunchentoot:header-out :x-content-type-options) "nosniff")
  ;; Referrer policy - don't leak URLs
  (setf (hunchentoot:header-out :referrer-policy) "no-referrer"))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor browser-acceptor) request)
  "Handle HTTP requests."
  ;; Set security headers for all responses
  (set-security-headers)
  (let ((path (hunchentoot:script-name request))
        (ws-path (format nil "/ws/~A" *browser-token*)))
    (cond
      ;; WebSocket upgrade - let hunchensocket handle it
      ((string= path ws-path)
       (call-next-method))
      ;; Main page at /icl/{token}
      ((string= path *browser-path*)
       ;; Only allow one browser connection
       (if (and *repl-resource* (hunchensocket:clients *repl-resource*))
           (progn
             (setf (hunchentoot:return-code*) 409)
             (setf (hunchentoot:content-type*) "text/plain")
             "Browser session already active. Only one connection allowed.")
           (progn
             (setf (hunchentoot:content-type*) "text/html")
             (browser-html))))
      ;; Serve assets
      ((and (> (length path) 8)
            (string= (subseq path 0 8) "/assets/"))
       (let ((asset (serve-asset (subseq path 8))))
         (if asset
             asset
             (progn
               (setf (hunchentoot:return-code*) 404)
               "Asset not found"))))
      ;; Serve speedscope files
      ((and (> (length path) 12)
            (string= (subseq path 0 12) "/speedscope/"))
       (serve-speedscope-asset (subseq path 12)))
      ;; Serve profile data
      ((and (> (length path) 14)
            (string= (subseq path 0 14) "/profile-data/"))
       (serve-profile-data (subseq path 14)))
      ;; Serve coverage report files
      ((and (> (length path) 10)
            (string= (subseq path 0 10) "/coverage/"))
       (let ((asset (serve-coverage-asset (subseq path 10))))
         (if asset
             asset
             (progn
               (setf (hunchentoot:return-code*) 404)
               "Coverage report not found"))))
      ;; 404 for all other paths (security)
      (t
       (setf (hunchentoot:return-code*) 404)
       "Not found"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Public API
;;; ─────────────────────────────────────────────────────────────────────────────

(defun start-browser (&key (port :auto) (open-browser t))
  "Start the Dockview browser.
   Returns the URL."
  (unless *slynk-connected-p*
    (error "Must be connected to a Lisp backend to use the browser"))

  (when *browser-acceptor*
    (stop-browser))

  (when (or (null port) (eq port :auto))
    ;; Pick a random port between 10000-60000 and verify it's free
    (loop for attempt from 0 below 100
          for candidate = (+ 10000 (random 50000))
          unless (port-in-use-p candidate)
            do (setf port candidate) (return)
          finally (error "Could not find a free random port")))

  (setf *browser-port* port)
  (setf *browser-token* (generate-session-token))
  (setf *browser-path* (format nil "/icl/~A" *browser-token*))
  (setf *repl-resource* (make-instance 'repl-resource))

  ;; Register WebSocket dispatch function
  (pushnew 'find-browser-websocket-resource hunchensocket:*websocket-dispatch-table*)

  ;; Start Hunchentoot with WebSocket support
  (setf *browser-acceptor*
        (make-instance 'browser-acceptor
                       :port port
                       :address "127.0.0.1"
                       :access-log-destination nil
                       :message-log-destination nil))

  (hunchentoot:start *browser-acceptor*)

  ;; REPL thread will be started when first client connects

  ;; Start eval generation poller for external connections
  ;; This refreshes visualizations when Emacs/Sly evaluates expressions
  (when *external-slynk-connection*
    (start-eval-generation-poller))

  (let ((url (format nil "http://127.0.0.1:~A~A" port *browser-path*)))
    (when open-browser
      (ignore-errors (uiop:run-program (list "xdg-open" url))))
    url))

(defun stop-browser ()
  "Stop the Dockview browser."
  ;; Stop the eval generation poller if running
  (stop-eval-generation-poller)
  (when *browser-acceptor*
    ;; Signal the browser-repl thread to exit by sending nil (EOF) to input queue
    (when *repl-resource*
      (ignore-errors
        (chanl:send (input-queue *repl-resource*) nil)))
    ;; Stop the HTTP server
    (ignore-errors (hunchentoot:stop *browser-acceptor*))
    (setf *browser-acceptor* nil)
    (setf *repl-resource* nil)
    t))

(defun start-eval-generation-poller ()
  "Start polling for eval generation changes from external Lisp.
   Used when connected to an external Slynk server (via --connect)
   to refresh visualizations when Emacs/Sly evaluates expressions."
  (when *eval-generation-poller*
    (stop-eval-generation-poller))
  ;; Install the listener-eval hook in the inferior Lisp
  ;; This ensures only real REPL evaluations increment the counter
  ;; Use find-symbol to avoid read-time package issues (package is in inferior, not ICL)
  ;; Use cl-user:: for local vars to avoid ICL package reference in inferior
  (handler-case
      (with-slynk-connection
        (slynk-client:slime-eval
         '(cl:let ((cl-user::icl-pkg (cl:find-package :icl-runtime)))
            (cl:when cl-user::icl-pkg
              (cl:let ((cl-user::icl-sym (cl:find-symbol "SETUP-EVAL-GENERATION-HOOK" cl-user::icl-pkg)))
                (cl:when (cl:and cl-user::icl-sym (cl:fboundp cl-user::icl-sym))
                  (cl:funcall cl-user::icl-sym)))))
         *slynk-connection*))
    (error (e)
      (format *error-output* "~&; Warning: Failed to setup eval hook: ~A~%" e)))
  (setf *last-eval-generation* -1)
  (setf *eval-generation-poller*
        (bt:make-thread
         (lambda ()
           (loop
             (sleep 1)
             (handler-case
                 ;; Use with-slynk-connection for proper locking
                 ;; Use find-symbol to avoid read-time package issues
                ;; (icl-runtime package exists in inferior, not in ICL process)
                ;; Use cl-user:: for local vars to avoid ICL package reference in inferior
                (let ((gen (ignore-errors
                              (with-slynk-connection
                                (slynk-client:slime-eval
                                 '(cl:let ((cl-user::icl-pkg (cl:find-package :icl-runtime)))
                                    (cl:if cl-user::icl-pkg
                                        (cl:let ((cl-user::icl-sym (cl:find-symbol "*EVAL-GENERATION*" cl-user::icl-pkg)))
                                          (cl:if (cl:and cl-user::icl-sym (cl:boundp cl-user::icl-sym))
                                              (cl:symbol-value cl-user::icl-sym)
                                              0))
                                        0))
                                 *slynk-connection*)))))
                  (when (and gen (numberp gen) (/= gen *last-eval-generation*))
                    (setf *last-eval-generation* gen)
                    ;; Don't refresh on first poll (gen was -1)
                    (when (>= *last-eval-generation* 0)
                      (refresh-browser-visualizations))))
               (error (e)
                 (format *error-output* "~&; POLLER error: ~A~%" e)))
             ;; Exit if browser stopped
             (unless *browser-acceptor*
               (return))))
         :name "icl-eval-generation-poller")))

(defun stop-eval-generation-poller ()
  "Stop the eval generation poller thread."
  (when *eval-generation-poller*
    (ignore-errors
      (bt:destroy-thread *eval-generation-poller*))
    (setf *eval-generation-poller* nil)))

(defun start-browser-repl-thread ()
  "Start the REPL thread that processes input from WebSocket.
   Uses the real ICL editor for full functionality."
  (when *repl-resource*
    (let ((in-stream (make-instance 'ws-input-stream :resource *repl-resource*))
          (out-stream (make-instance 'ws-output-stream :client nil)))
      (setf (repl-thread *repl-resource*)
            (bt:make-thread
             (lambda ()
               ;; Create a browser session for history support
               (let* ((session (make-repl-session
                                :name "Browser"
                                :output-stream out-stream
                                :input-stream in-stream))
                      (*current-session* session)
                      (*standard-input* in-stream)
                      (*standard-output* out-stream)
                      (*error-output* out-stream)
                      (*trace-output* out-stream)
                      (*terminal-io* (make-two-way-stream in-stream out-stream))
                      (*query-io* (make-two-way-stream in-stream out-stream))
                      (*in-repl* t)
                      (*input-count* 0)
                      (*browser-terminal-active* t))
                 ;; Load history for browser session (shares with TUI)
                 (load-history session)
                 ;; Use the real REPL loop with full editor support
                 (unwind-protect
                      (repl-loop session)
                   ;; Save history on exit
                   (ignore-errors (save-history session)))))
             :name "browser-repl")))))
