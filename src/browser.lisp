;;; browser.lisp --- Dockview Browser for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; This module provides a VS Code-style system browser using Dockview,
;;; with dockable panels and xterm.js terminals.

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Debug Logging
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *browser-debug* nil
  "When T, enable verbose browser debug logging.")

(defvar *browser-log-stream* *error-output*
  "Stream for browser debug logging. Set at load time to the real terminal.")

(defun browser-log (format-string &rest args)
  "Log a debug message if *browser-debug* is enabled."
  (when *browser-debug*
    (apply #'format *browser-log-stream*
           (concatenate 'string "~&;; [BROWSER] " format-string "~%")
           args)
    (force-output *browser-log-stream*)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Configuration
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *browser-port* 9000
  "Port for the browser server.")

(defvar *browser-acceptor* nil
  "The Hunchentoot acceptor for the browser.")

(defvar *browser-token* nil
  "Security token for browser URL path.")

(defvar *browser-path* nil
  "Random path prefix for the browser (e.g., /icl/abc123).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Backend Query Helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun browser-query (code)
  "Execute CODE in the backend and return parsed result.
   Returns nil on error."
  (browser-log "browser-query: code length=~D code-preview=~S"
               (length code)
               (subseq code 0 (min 100 (length code))))
  (handler-case
      (let ((result-string (first (backend-eval code))))
        (browser-log "browser-query: result-string length=~A preview=~S"
                     (if result-string (length result-string) "NIL")
                     (when result-string
                       (subseq result-string 0 (min 200 (length result-string)))))
        (when result-string
          (handler-case
              (let ((parsed (read-from-string result-string)))
                (browser-log "browser-query: parsed type=~A null=~A"
                             (type-of parsed) (null parsed))
                parsed)
            (error (e)
              (browser-log "browser-query: READ ERROR: ~A" e)
              (browser-log "browser-query: full result-string: ~S" result-string)
              nil))))
    (error (e)
      (browser-log "browser-query: ERROR ~A" e)
      nil)))

(defun get-all-packages ()
  "Get list of all package names from the backend."
  (browser-query
   "(sort (mapcar #'package-name (list-all-packages)) #'string<)"))

(defun get-package-symbols (package-name)
  "Get all symbols from PACKAGE-NAME.
   Returns alist of (name . type)."
  (browser-query
   (format nil
           "(let ((pkg (find-package ~S))
                  (results nil))
              (when pkg
                (do-symbols (sym pkg)
                  (when (eq (symbol-package sym) pkg)
                    (let ((name (symbol-name sym)))
                      (cond
                        ((find-class sym nil)
                         (push (cons name :class) results))
                        ((and (fboundp sym)
                              (typep (fdefinition sym) 'generic-function))
                         (push (cons name :generic) results))
                        ((macro-function sym)
                         (push (cons name :macro) results))
                        ((fboundp sym)
                         (push (cons name :function) results))
                        ((boundp sym)
                         (push (cons name :variable) results)))))))
              (sort results #'string< :key #'car))"
           package-name)))

(defun get-symbol-info (symbol-name package-name)
  "Get information about a symbol. Returns all bindings (class, function, variable)."
  (browser-log "get-symbol-info: symbol=~S package=~S" symbol-name package-name)
  (let ((result (browser-query
   (format nil
           "(let ((sym (find-symbol ~S (find-package ~S))))
              (when sym
                (let ((result (list :name (symbol-name sym)
                                    :package (package-name (symbol-package sym)))))
                  ;; Check for class binding
                  (when (find-class sym nil)
                    (let ((class (find-class sym)))
                      (setf (getf result :class)
                            (list :superclasses (ignore-errors
                                                  (mapcar (lambda (c) (prin1-to-string (class-name c)))
                                                          (funcall (find-symbol \"CLASS-DIRECT-SUPERCLASSES\"
                                                                                (or (find-package :closer-mop)
                                                                                    (find-package :sb-mop)))
                                                                   class)))
                                  :slots (ignore-errors
                                           (mapcar (lambda (s)
                                                     (prin1-to-string
                                                       (funcall (find-symbol \"SLOT-DEFINITION-NAME\"
                                                                             (or (find-package :closer-mop)
                                                                                 (find-package :sb-mop)))
                                                                s)))
                                                   (funcall (find-symbol \"CLASS-DIRECT-SLOTS\"
                                                                         (or (find-package :closer-mop)
                                                                             (find-package :sb-mop)))
                                                            class)))))))
                  ;; Check for function/macro/generic binding
                  (when (fboundp sym)
                    (setf (getf result :function)
                          (list :type (cond
                                        ((typep (fdefinition sym) 'generic-function) :generic)
                                        ((macro-function sym) :macro)
                                        (t :function))
                                :arglist (ignore-errors
                                           (let ((args (slynk-backend:arglist sym)))
                                             (if args
                                                 (prin1-to-string args)
                                                 \"()\")))
                                :documentation (documentation sym 'function))))
                  ;; Check for variable binding
                  (when (boundp sym)
                    (setf (getf result :variable)
                          (list :value (ignore-errors
                                         (let ((*print-length* 10)
                                               (*print-level* 3)
                                               (*print-circle* t)
                                               (*print-pretty* nil))
                                           (prin1-to-string (symbol-value sym))))
                                :documentation (documentation sym 'variable)
                                :constantp (constantp sym))))
                  ;; Check for special operator
                  (when (special-operator-p sym)
                    (setf (getf result :special-operator) t))
                  result)))"
           symbol-name package-name))))
    (browser-log "get-symbol-info: result type=~A" (type-of result))
    (when result
      (browser-log "get-symbol-info: :name=~S :package=~S"
                   (getf result :name) (getf result :package))
      (browser-log "get-symbol-info: has-class=~A has-function=~A has-variable=~A"
                   (not (null (getf result :class)))
                   (not (null (getf result :function)))
                   (not (null (getf result :variable))))
      (when (getf result :variable)
        (browser-log "get-symbol-info: variable value=~S"
                     (getf (getf result :variable) :value))))
    result))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WebSocket Resource
;;; ─────────────────────────────────────────────────────────────────────────────

(defclass repl-resource (hunchensocket:websocket-resource)
  ((repl-thread :accessor repl-thread :initform nil)
   (input-queue :accessor input-queue :initform (make-instance 'chanl:unbounded-channel)))
  (:documentation "WebSocket resource for REPL connections."))

(defclass repl-client (hunchensocket:websocket-client)
  ()
  (:documentation "A connected REPL client."))

(defvar *repl-resource* nil
  "The singleton REPL WebSocket resource.")

(defmethod hunchensocket:client-connected ((resource repl-resource) client)
  ;; Only allow one browser connection at a time
  (let ((existing-clients (remove client (hunchensocket:clients resource))))
    (when existing-clients
      ;; Close the new connection - only one browser allowed
      (hunchensocket:close-connection client :reason "Only one browser connection allowed")
      (return-from hunchensocket:client-connected)))
  ;; Send current theme to newly connected client
  (when *current-browser-theme*
    (send-browser-theme-to-client client *current-browser-theme*))
  ;; REPL thread will be started when terminal sends 'terminal-ready'
  )

(defmethod hunchensocket:client-disconnected ((resource repl-resource) client)
  (declare (ignore client)))

(defmethod hunchensocket:text-message-received ((resource repl-resource) client message)
  "Handle incoming WebSocket messages."
  (let ((json (ignore-errors (com.inuoe.jzon:parse message))))
    (when json
      (let ((type (gethash "type" json)))
        (browser-log "WS message received: type=~S" type)
        (cond
          ;; Terminal ready - start the REPL thread
          ((string= type "terminal-ready")
           (unless (repl-thread resource)
             (start-browser-repl-thread)))

          ;; Terminal input - send each character
          ((string= type "input")
           (let ((data (gethash "data" json)))
             (when data
               ;; Send each character to the input queue
               (loop for char across data
                     do (chanl:send (input-queue resource) char)))))

          ;; Request packages list
          ((string= type "get-packages")
           (send-packages-list client))

          ;; Request symbols for a package
          ((string= type "get-symbols")
           (let ((pkg (gethash "package" json)))
             (when pkg
               (send-symbols-list client pkg))))

          ;; Request symbol info
          ((string= type "get-symbol-info")
           (let ((pkg (gethash "package" json))
                 (name (gethash "name" json)))
             (browser-log "WS get-symbol-info: pkg=~S name=~S" pkg name)
             (when (and pkg name)
               (send-symbol-info client pkg name))))

          ;; Inspect object (with panel ID for multi-inspector support)
          ((string= type "inspect")
           (let ((form (gethash "form" json))
                 (panel-id (gethash "panelId" json))
                 (pkg (gethash "package" json)))
             (browser-log "WS inspect: form=~S panel-id=~S pkg=~S" form panel-id pkg)
             (when form
               (bt:make-thread
                (lambda ()
                  (send-inspection client form panel-id pkg))
                :name "inspect-handler"))))

          ;; Inspector drill-down action
          ((string= type "inspector-action")
           (let ((index (gethash "index" json))
                 (panel-id (gethash "panelId" json)))
             (browser-log "WS inspector-action: index=~S panel-id=~S" index panel-id)
             (when index
               (bt:make-thread
                (lambda ()
                  (send-inspector-action client index panel-id))
                :name "inspector-action-handler"))))

          ;; Inspector go back
          ((string= type "inspector-pop")
           (let ((panel-id (gethash "panelId" json)))
             (browser-log "WS inspector-pop: panel-id=~S" panel-id)
             (bt:make-thread
              (lambda ()
                (send-inspector-pop client panel-id))
              :name "inspector-pop-handler")))

          ;; Click on symbol in REPL - update all panels
          ;; Run in separate thread to avoid blocking WebSocket handler
          ((string= type "symbol-click")
           (let ((symbol-string (gethash "symbol" json)))
             (browser-log "WS symbol-click: symbol=~S" symbol-string)
             (when symbol-string
               (bt:make-thread
                (lambda ()
                  (send-symbol-click-response client symbol-string))
                :name "symbol-click-handler"))))

          ;; Client reports dark mode preference
          ((string= type "dark-mode-preference")
           (let ((dark-p (gethash "dark" json)))
             ;; Auto-select browser theme based on client preference
             (auto-select-browser-theme dark-p)))

          ;; Request current theme
          ((string= type "get-theme")
           (when *current-browser-theme*
             (send-browser-theme-to-client client *current-browser-theme*))))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; WebSocket Send Helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun plist-to-hash (plist)
  "Convert a plist to a hash table for JSON serialization."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for (key val) on plist by #'cddr
          do (setf (gethash (string-downcase (string key)) ht)
                   (if (and val (listp val) (keywordp (first val)))
                       (plist-to-hash val)  ; Nested plist
                       val)))
    ht))

(defun ws-send (client type &rest plist)
  "Send a JSON message to CLIENT with TYPE and additional PLIST data."
  (browser-log "ws-send: type=~S plist-keys=~S" type
               (loop for (k v) on plist by #'cddr collect k))
  (let ((obj (make-hash-table :test 'equal)))
    (setf (gethash "type" obj) type)
    (loop for (key val) on plist by #'cddr
          do (setf (gethash (string-downcase (string key)) obj)
                   (cond
                     ;; Convert nested plist to hash table
                     ((and val (listp val) (keywordp (first val)))
                      (plist-to-hash val))
                     ;; Convert alist of (name . type) to arrays for symbols
                     ;; Alist entries are (name . type) where cdr is an atom
                     ((and val (listp val) (consp (first val))
                           (atom (cdr (first val))))
                      (mapcar (lambda (pair)
                                (list (car pair) (string-downcase (string (cdr pair)))))
                              val))
                     (t val))))
    (let ((json-str (com.inuoe.jzon:stringify obj)))
      (browser-log "ws-send: json length=~D" (length json-str))
      (hunchensocket:send-text-message client json-str)
      (browser-log "ws-send: message sent successfully"))))

(defun send-packages-list (client)
  "Send list of all packages to CLIENT."
  (let ((packages (browser-query
                   "(sort (mapcar #'package-name (list-all-packages)) #'string<)")))
    (ws-send client "packages" :data packages)))

(defun send-symbols-list (client package-name)
  "Send symbols from PACKAGE-NAME to CLIENT."
  (let ((symbols (get-package-symbols package-name)))
    (ws-send client "symbols" :package package-name :data symbols)))

(defun refresh-browser-lists ()
  "Refresh package lists for all connected browser clients.
   Called after REPL evaluation to pick up new definitions."
  (when (and *repl-resource* *browser-terminal-active*)
    (ignore-errors
      (dolist (client (hunchensocket:clients *repl-resource*))
        (send-packages-list client)))))

(defun send-symbol-info (client package-name symbol-name)
  "Send symbol info to CLIENT."
  (browser-log "send-symbol-info: package=~S symbol=~S" package-name symbol-name)
  (handler-case
      (let ((info (get-symbol-info symbol-name package-name)))
        (browser-log "send-symbol-info: got info, type=~A null=~A"
                     (type-of info) (null info))
        (when info
          (browser-log "send-symbol-info: info keys: ~S"
                       (loop for (k v) on info by #'cddr collect k)))
        (browser-log "send-symbol-info: sending ws-send symbol-info message")
        (ws-send client "symbol-info" :package package-name :name symbol-name :data info)
        (browser-log "send-symbol-info: ws-send completed"))
    (error (e)
      (browser-log "send-symbol-info: ERROR ~A" e)
      (format *error-output* "~&; Error getting symbol info for ~A:~A: ~A~%"
              package-name symbol-name e))))

(defun needs-case-escape-p (str)
  "Return T if STR contains lowercase letters that would be upcased by the reader."
  (and (stringp str)
       (some #'lower-case-p str)))

(defun escape-symbol-case (form)
  "Escape symbol names in FORM that contain lowercase letters.
   Finds all PKG::symbol patterns and escapes the symbol part if needed.
   Works on both simple symbols and complex expressions."
  (browser-log "escape-symbol-case: input=~S" form)
  (let ((result form)
        (start 0))
    ;; Find all PKG::sym patterns and escape sym if it has lowercase
    (loop
      (let ((pos (search "::" result :start2 start)))
        (unless pos (return))
        ;; Find the package name (go backwards to find start)
        (let ((pkg-start pos))
          (loop while (and (> pkg-start 0)
                           (let ((c (char result (1- pkg-start))))
                             (or (alphanumericp c)
                                 (char= c #\/)
                                 (char= c #\-)
                                 (char= c #\_)
                                 (char= c #\.))))
                do (decf pkg-start))
          ;; Find the symbol name (go forwards to find end)
          ;; Track paren depth to only include balanced parens
          (let ((sym-start (+ pos 2))
                (sym-end (+ pos 2))
                (paren-depth 0))
            (loop while (< sym-end (length result))
                  for c = (char result sym-end)
                  do (cond
                       ;; Opening paren increases depth
                       ((char= c #\()
                        (incf paren-depth)
                        (incf sym-end))
                       ;; Closing paren: only include if we have matching open
                       ((char= c #\))
                        (if (> paren-depth 0)
                            (progn (decf paren-depth) (incf sym-end))
                            (return)))  ; Unmatched ), stop here
                       ;; Other valid symbol chars
                       ((or (alphanumericp c)
                            (char= c #\/)
                            (char= c #\-)
                            (char= c #\_)
                            (char= c #\.)
                            (char= c #\;)
                            (char= c #\[)
                            (char= c #\]))
                        (incf sym-end))
                       ;; Invalid char, stop
                       (t (return))))
            (let* ((pkg (if (> sym-end sym-start) (subseq result pkg-start pos) ""))
                   (sym (if (> sym-end sym-start) (subseq result sym-start sym-end) "")))
              (if (and (> (length pkg) 0)
                       (> (length sym) 0)
                       (needs-case-escape-p sym))
                  ;; Replace this occurrence
                  (let ((replacement (concatenate 'string pkg "::|" sym "|")))
                    (setf result (concatenate 'string
                                              (subseq result 0 pkg-start)
                                              replacement
                                              (subseq result sym-end)))
                    ;; Adjust start for the replacement
                    (setf start (+ pkg-start (length replacement))))
                  ;; No replacement, just advance
                  (setf start (+ pos 2))))))))
    (browser-log "escape-symbol-case: output=~S" result)
    result))

(defun qualify-symbol-form (form &optional package-name)
  "If FORM looks like an unqualified symbol, qualify it with its home package.
   If PACKAGE-NAME is provided, use it for qualification.
   Returns the qualified form string, or the original form if not a simple symbol."
  (browser-log "qualify-symbol-form: form=~S package-name=~S" form package-name)
  (let ((trimmed (string-trim '(#\Space #\Tab) form)))
    (browser-log "qualify-symbol-form: trimmed=~S" trimmed)
    ;; Check if it looks like a simple symbol (no special chars except symbol chars)
    (let ((is-simple (and (> (length trimmed) 0)
                          (not (find #\( trimmed))      ; not an expression
                          (not (find #\' trimmed))      ; not quoted
                          (not (find #\" trimmed))      ; not a string
                          (not (find #\# trimmed))      ; not reader macro
                          (not (find #\: trimmed)))))   ; already qualified
      (browser-log "qualify-symbol-form: is-simple-symbol=~A" is-simple)
      (if is-simple
          (if package-name
              (let ((result (format nil "~A::~A" package-name trimmed)))
                (browser-log "qualify-symbol-form: using provided package -> ~S" result)
                result)
              ;; Try to resolve as a symbol
              (let ((resolved (find-symbol-home-package trimmed)))
                (browser-log "qualify-symbol-form: find-symbol-home-package returned ~S" resolved)
                (if resolved
                    ;; Return package-qualified form
                    (let ((result (format nil "~A::~A" (car resolved) (cdr resolved))))
                      (browser-log "qualify-symbol-form: resolved to ~S" result)
                      result)
                    ;; Not found, return original
                    (progn
                      (browser-log "qualify-symbol-form: not resolved, returning original")
                      form))))
          ;; Not a simple symbol, return as-is
          (progn
            (browser-log "qualify-symbol-form: not a simple symbol, returning as-is")
            form)))))

(defun send-inspection (client form &optional panel-id package-name)
  "Send inspection data for FORM to CLIENT."
  (browser-log "send-inspection: form=~S panel-id=~S package-name=~S" form panel-id package-name)
  (handler-case
      (let* ((qualified-form (qualify-symbol-form form package-name))
             (escaped-form (escape-symbol-case qualified-form)))
        (browser-log "send-inspection: qualified-form=~S" qualified-form)
        (browser-log "send-inspection: escaped-form=~S" escaped-form)
        (multiple-value-bind (data err) (slynk-inspect-object escaped-form)
          (browser-log "send-inspection: slynk-inspect-object returned data=~A err=~S"
                       (if data "non-nil" "NIL") err)
          (if data
              (let* ((raw-content (getf data :content))
                     (content (if (and (listp raw-content) (listp (first raw-content)))
                                  (first raw-content)
                                  raw-content))
                     (parsed (when (listp content)
                               (parse-inspector-content content))))
                (browser-log "send-inspection: title=~S raw-content-type=~A content-type=~A"
                             (getf data :title) (type-of raw-content) (type-of content))
                (browser-log "send-inspection: parsed entries count=~A"
                             (if parsed (length parsed) 0))
                (when parsed
                  (browser-log "send-inspection: first entry=~S" (first parsed)))
                (browser-log "send-inspection: sending ws-send inspection message (new)")
                (ws-send client "inspection"
                         :title (getf data :title)
                         :action "new"
                         :panel-id panel-id
                         :entries (or parsed nil))
                (browser-log "send-inspection: ws-send completed"))
              (let ((msg (or err "Inspector returned no data")))
                (browser-log "send-inspection: NO DATA - sending error message: ~S" msg)
                (ws-send client "inspection"
                         :title "Inspector unavailable"
                         :action "new"
                         :panel-id panel-id
                         :entries (list (list "Error" msg nil)))))))
    (error (e)
      (browser-log "send-inspection: EXCEPTION: ~A" e)
      (format *error-output* "~&; Error inspecting ~A: ~A~%" form e))))

(defun send-inspector-action (client index &optional panel-id)
  "Drill down into inspector item at INDEX and send result to CLIENT."
  (browser-log "send-inspector-action: index=~D panel-id=~S" index panel-id)
  (handler-case
      (let ((data (slynk-inspector-action index)))
        (browser-log "send-inspector-action: slynk-inspector-action returned data=~A"
                     (if data "non-nil" "NIL"))
        (if data
            (let* ((raw-content (getf data :content))
                   (content (if (and (listp raw-content) (listp (first raw-content)))
                                (first raw-content)
                                raw-content))
                   (parsed (when (listp content)
                             (parse-inspector-content content))))
              (browser-log "send-inspector-action: title=~S parsed-count=~A"
                           (getf data :title) (if parsed (length parsed) 0))
              (browser-log "send-inspector-action: sending ws-send inspection message (push)")
              (ws-send client "inspection"
                       :title (getf data :title)
                       :action "push"
                       :panel-id panel-id
                       :entries (or parsed nil))
              (browser-log "send-inspector-action: ws-send completed"))
            (browser-log "send-inspector-action: NO DATA returned from slynk-inspector-action")))
    (error (e)
      (browser-log "send-inspector-action: EXCEPTION: ~A" e)
      (format *error-output* "~&; Error in inspector action ~A: ~A~%" index e))))

(defun send-inspector-pop (client &optional panel-id)
  "Go back in inspector and send result to CLIENT."
  (browser-log "send-inspector-pop: panel-id=~S" panel-id)
  (handler-case
      (let ((data (slynk-inspector-pop)))
        (browser-log "send-inspector-pop: slynk-inspector-pop returned data=~A"
                     (if data "non-nil" "NIL"))
        (if data
            (let* ((raw-content (getf data :content))
                   (content (if (and (listp raw-content) (listp (first raw-content)))
                                (first raw-content)
                                raw-content))
                   (parsed (when (listp content)
                             (parse-inspector-content content))))
              (browser-log "send-inspector-pop: title=~S parsed-count=~A"
                           (getf data :title) (if parsed (length parsed) 0))
              (browser-log "send-inspector-pop: sending ws-send inspection message (pop)")
              (ws-send client "inspection"
                       :title (getf data :title)
                       :action "pop"
                       :panel-id panel-id
                       :entries (or parsed nil))
              (browser-log "send-inspector-pop: ws-send completed"))
            (browser-log "send-inspector-pop: NO DATA returned from slynk-inspector-pop")))
    (error (e)
      (browser-log "send-inspector-pop: EXCEPTION: ~A" e)
      (format *error-output* "~&; Error in inspector pop: ~A~%" e))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Theme Helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun send-browser-theme-to-client (client theme)
  "Send THEME to a specific WebSocket CLIENT."
  (handler-case
      (let ((json-str (browser-theme-to-json theme)))
        (let ((msg (make-hash-table :test 'equal)))
          (setf (gethash "type" msg) "theme")
          (setf (gethash "data" msg) (com.inuoe.jzon:parse json-str))
          (hunchensocket:send-text-message client (com.inuoe.jzon:stringify msg))))
    (error (e)
      (format *error-output* "~&; Error sending theme: ~A~%" e))))

(defun broadcast-browser-theme-impl (theme)
  "Broadcast THEME to all connected WebSocket clients.
   This is the real implementation called from themes.lisp."
  (when *repl-resource*
    (dolist (client (hunchensocket:clients *repl-resource*))
      (send-browser-theme-to-client client theme))))

;; Replace the placeholder in themes.lisp with this implementation
(setf (symbol-function 'broadcast-browser-theme) #'broadcast-browser-theme-impl)

(defun find-symbol-home-package (symbol-string)
  "Find a symbol and return (package-name . symbol-name) for its HOME package.
   For unqualified symbols, finds the symbol and returns its symbol-package."
  (browser-log "find-symbol-home-package: symbol-string=~S" symbol-string)
  (let ((result (browser-query
   (format nil
           "(let* ((str ~S)
                   (colon (position #\\: str)))
              (if colon
                  ;; Qualified - parse and find
                  (let* ((pkg-name (subseq str 0 colon))
                         (sym-start (position-if-not (lambda (c) (char= c #\\:)) str :start colon))
                         (sym-name (if sym-start (subseq str sym-start) \"\"))
                         (pkg (find-package (string-upcase pkg-name))))
                    (when pkg
                      (cons (package-name pkg) (string-upcase sym-name))))
                  ;; Unqualified - find in current package and get home package
                  (let ((sym (find-symbol (string-upcase str))))
                    (when sym
                      (let ((home-pkg (symbol-package sym)))
                        (cons (if home-pkg (package-name home-pkg) \"COMMON-LISP-USER\")
                              (symbol-name sym)))))))"
           symbol-string))))
    (browser-log "find-symbol-home-package: result=~S" result)
    result))

(defun send-symbol-click-response (client symbol-string)
  "Handle a click on a symbol in the REPL.
   Updates all three panels: Packages, Symbols, and Inspector."
  (browser-log "send-symbol-click-response: symbol-string=~S" symbol-string)
  (handler-case
      (let* ((parsed (find-symbol-home-package symbol-string))
             (pkg-name (if parsed (car parsed) nil))
             (sym-name (if parsed (cdr parsed) (string-upcase symbol-string))))
        (browser-log "send-symbol-click-response: parsed=~S pkg-name=~S sym-name=~S"
                     parsed pkg-name sym-name)
        (unless pkg-name
          ;; Symbol not found, just inspect it
          (browser-log "send-symbol-click-response: package not found, falling back to send-inspection")
          (send-inspection client symbol-string)
          (return-from send-symbol-click-response))
        ;; Send combined response with all panel data
        ;; Use get-symbol-info for consistent display with Symbol list clicks
        (let* ((symbols (get-package-symbols pkg-name))
               (symbol-info (get-symbol-info sym-name pkg-name)))
          (browser-log "send-symbol-click-response: got ~D symbols from package"
                       (if symbols (length symbols) 0))
          (browser-log "send-symbol-click-response: symbol-info=~A"
                       (if symbol-info "non-nil" "NIL"))
          (when symbol-info
            (browser-log "send-symbol-click-response: symbol-info keys: ~S"
                         (loop for (k v) on symbol-info by #'cddr collect k)))
          ;; Send symbol-click response with all data
          (let ((obj (make-hash-table :test 'equal)))
            (setf (gethash "type" obj) "symbol-clicked")
            (setf (gethash "package" obj) pkg-name)
            (setf (gethash "symbol" obj) sym-name)
            (setf (gethash "symbols" obj)
                  (mapcar (lambda (pair)
                            (list (car pair) (string-downcase (string (cdr pair)))))
                          symbols))
            ;; Include symbol info (same format as Symbol list click)
            ;; Convert plist to hash table for JSON serialization
            (when symbol-info
              (setf (gethash "symbolInfo" obj) (plist-to-hash symbol-info)))
            (browser-log "send-symbol-click-response: sending symbol-clicked message")
            (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))
            (browser-log "send-symbol-click-response: message sent"))))
    (error (e)
      (browser-log "send-symbol-click-response: EXCEPTION: ~A" e)
      (format *error-output* "~&; Error handling symbol click for ~A: ~A~%"
              symbol-string e))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; REPL I/O Streams
;;; ─────────────────────────────────────────────────────────────────────────────

(defclass ws-input-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((resource :accessor stream-resource :initarg :resource)
   (unread-char-slot :initform nil))
  (:documentation "Input stream that reads from WebSocket via channel."))

(defclass ws-output-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((client :accessor stream-client :initarg :client)
   (buffer :accessor buffer-of :initform (make-string-output-stream))
   (lock :accessor stream-lock :initform (bt:make-lock "ws-output-lock")))
  (:documentation "Output stream that writes to WebSocket."))

(defmethod trivial-gray-streams:stream-read-char ((stream ws-input-stream))
  (with-slots (resource unread-char-slot) stream
    ;; Check for unread character first
    (if unread-char-slot
        (prog1 unread-char-slot
          (setf unread-char-slot nil))
        ;; Read a character directly from the queue
        (let ((char (chanl:recv (input-queue resource))))
          (if char char :eof)))))

(defmethod trivial-gray-streams:stream-unread-char ((stream ws-input-stream) char)
  (with-slots (resource unread-char-slot) stream
    ;; Push the character back - we'll read it next time
    ;; Use a simple slot for unreading
    (setf (slot-value stream 'unread-char-slot) char)))

(defmethod trivial-gray-streams:stream-listen ((stream ws-input-stream))
  (with-slots (resource unread-char-slot) stream
    (or unread-char-slot
        (not (chanl:recv-blocks-p (input-queue resource))))))

(defvar *ws-flush-timer* nil)
(defvar *ws-flush-timer-lock* (bt:make-lock "ws-flush-timer-lock"))

(defun ws-schedule-flush (stream)
  "Schedule a delayed flush for non-newline output."
  (bt:with-lock-held (*ws-flush-timer-lock*)
    (unless *ws-flush-timer*
      (setf *ws-flush-timer*
            (bt:make-thread
             (lambda ()
               (sleep 0.05)
               (bt:with-lock-held (*ws-flush-timer-lock*)
                 (setf *ws-flush-timer* nil))
               (trivial-gray-streams:stream-force-output stream))
             :name "ws-flush-timer")))))

(defmethod trivial-gray-streams:stream-write-char ((stream ws-output-stream) char)
  (bt:with-lock-held ((stream-lock stream))
    (write-char char (buffer-of stream)))
  ;; Flush on newline or schedule a flush for prompt-like output
  (if (char= char #\Newline)
      (trivial-gray-streams:stream-force-output stream)
      (ws-schedule-flush stream)))

(defmethod trivial-gray-streams:stream-write-string ((stream ws-output-stream) string &optional start end)
  (bt:with-lock-held ((stream-lock stream))
    (write-string string (buffer-of stream) :start (or start 0) :end end))
  ;; Check if string contains newline to flush
  (let ((s (subseq string (or start 0) end)))
    (if (find #\Newline s)
        (trivial-gray-streams:stream-force-output stream)
        (ws-schedule-flush stream)))
  string)

(defmethod trivial-gray-streams:stream-force-output ((stream ws-output-stream))
  (let ((text (bt:with-lock-held ((stream-lock stream))
                (prog1 (get-output-stream-string (buffer-of stream))
                  (setf (buffer-of stream) (make-string-output-stream))))))
    (when (and (plusp (length text)) *repl-resource*)
      ;; Send to all connected clients
      (dolist (client (hunchensocket:clients *repl-resource*))
        (ws-send client "output" :data text)))))

(defmethod trivial-gray-streams:stream-finish-output ((stream ws-output-stream))
  (trivial-gray-streams:stream-force-output stream))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; HTTP Handlers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun browser-html ()
  "Return the main HTML page for the browser."
  (format nil "<!DOCTYPE html>
<html>
<head>
  <meta charset='utf-8'>
  <title>ICL Browser</title>
  <link rel='stylesheet' href='/assets/dockview.css'>
  <link rel='stylesheet' href='/assets/xterm.css'>
  <style>
    /* Default theme variables (will be overridden by server-sent theme) */
    :root {
      --bg-primary: #1e1e2e;
      --bg-secondary: #313244;
      --bg-tertiary: #45475a;
      --fg-primary: #cdd6f4;
      --fg-secondary: #bac2de;
      --fg-muted: #6c7086;
      --accent: #89b4fa;
      --accent-hover: #b4befe;
      --border: #45475a;
    }
    * { box-sizing: border-box; }
    html, body { margin: 0; padding: 0; overflow: hidden; }
    body { font-family: 'JetBrains Mono', monospace; font-size: 13px; background: var(--bg-primary); color: var(--fg-primary); }
    #layout-container { height: 100vh; width: 100vw; }
    .panel { height: 100%%; display: flex; flex-direction: column; overflow: hidden; background: var(--bg-primary); }
    .panel-header { padding: 8px; background: var(--bg-secondary); border-bottom: 1px solid var(--border); flex-shrink: 0; }
    .panel-header input { width: 100%%; padding: 4px 8px; background: var(--bg-tertiary); border: 1px solid var(--border); color: var(--fg-primary); border-radius: 3px; }
    .panel-header input:focus { outline: 1px solid var(--accent); }
    .panel-content { flex: 1; overflow-y: auto; overflow-x: hidden; min-height: 0; }
    .panel-content::-webkit-scrollbar { width: 8px; }
    .panel-content::-webkit-scrollbar-track { background: var(--bg-primary); }
    .panel-content::-webkit-scrollbar-thumb { background: var(--bg-tertiary); border-radius: 4px; }
    .panel-content::-webkit-scrollbar-thumb:hover { background: var(--fg-muted); }
    .list-item { padding: 4px 8px; cursor: pointer; color: var(--fg-primary); }
    .list-item:hover { background: var(--bg-secondary); }
    .list-item.selected { background: var(--accent); color: var(--bg-primary); }
    .terminal-container { position: absolute; top: 0; left: 0; right: 0; bottom: 0; }
    .terminal-container .xterm { height: 100%%; width: 100%%; }
    .terminal-container .xterm-screen { height: 100%%; }
    .terminal-container .xterm-viewport { height: 100%%; overflow-y: auto !important; }
    .dv-content-container { position: relative; height: 100%%; }
    .dv-content-container > .panel { position: absolute; top: 0; left: 0; right: 0; bottom: 0; }
    .detail-content { padding: 8px; white-space: pre-wrap; font-family: 'JetBrains Mono', monospace; }
    .inspector-entry { padding: 2px 8px; }
    .inspector-label { color: var(--fg-muted); }
    .inspector-value { color: var(--accent); }
    .inspector-link { color: var(--accent-hover); cursor: pointer; text-decoration: underline; }
    .binding-header { color: var(--fg-secondary); font-weight: bold; }
    .inspect-link { color: var(--accent); cursor: pointer; margin-left: 8px; font-size: 0.9em; }
    .inspect-link:hover { text-decoration: underline; color: var(--accent-hover); }
    .panel-header button { background: var(--bg-tertiary); border: 1px solid var(--border); color: var(--fg-primary); }
    .panel-header button:hover { background: var(--accent); color: var(--bg-primary); }
  </style>
</head>
<body>
  <div id='layout-container'></div>

  <script src='/assets/dockview.min.js'></script>
  <script src='/assets/xterm.min.js'></script>
  <script src='/assets/xterm-addon-fit.min.js'></script>
  <script>
    // WebSocket connection
    const ws = new WebSocket('ws://' + location.host + '/ws/~A');
    let terminal, fitAddon;
    let selectedPackage = null;
    let selectedSymbol = null;
    let packages = [];
    let symbols = [];

    // Dockview API and inspector management
    let dockviewApi = null;
    let inspectorCounter = 0;
    const inspectorStates = new Map();  // panelId -> {depth, element, header}
    const pendingInspections = new Map();  // panelId -> last inspection msg

    ws.onopen = () => {
      console.log('Connected');
      // Send dark mode preference for theme auto-selection
      sendDarkModePreference();
      ws.send(JSON.stringify({type: 'get-packages'}));
    };

    ws.onmessage = (e) => {
      const msg = JSON.parse(e.data);
      switch(msg.type) {
        case 'output':
          if (terminal) terminal.write(msg.data.replace(/\\n/g, '\\r\\n'));
          break;
        case 'packages':
          packages = msg.data || [];
          // Preserve current filter when re-rendering
          renderPackages(document.getElementById('package-filter')?.value || '');
          // Re-fetch symbols for selected package (may have new definitions)
          if (selectedPackage) {
            ws.send(JSON.stringify({type: 'get-symbols', package: selectedPackage}));
          }
          break;
        case 'symbols':
          symbols = msg.data || [];
          // Preserve current filter when re-rendering
          renderSymbols(document.getElementById('symbol-filter')?.value || '');
          break;
        case 'symbol-info':
          renderSymbolInfo(msg.data);
          break;
        case 'inspection':
          console.log('inspection received:', msg, 'panelId:', msg['panel-id']);
          renderInspection(msg, msg['panel-id']);
          break;
        case 'symbol-clicked':
          handleSymbolClicked(msg);
          break;
        case 'theme':
          applyTheme(msg.data);
          break;
      }
    };

    // Theme application
    function applyTheme(themeData) {
      if (!themeData) return;
      console.log('Applying theme:', themeData.displayName);

      // Apply CSS variables by injecting a style tag
      let styleEl = document.getElementById('theme-styles');
      if (!styleEl) {
        styleEl = document.createElement('style');
        styleEl.id = 'theme-styles';
        document.head.appendChild(styleEl);
      }
      styleEl.textContent = themeData.css;

      // Update body styles using CSS variables
      document.body.style.background = 'var(--bg-primary)';
      document.body.style.color = 'var(--fg-primary)';

      // Apply xterm.js theme if terminal exists
      if (terminal && themeData.xterm) {
        const xtermTheme = {};
        Object.entries(themeData.xterm).forEach(([k, v]) => {
          xtermTheme[k] = v;
        });
        terminal.options.theme = xtermTheme;
      }

      // Apply dockview theme class
      if (themeData.dockviewTheme && dockviewApi) {
        const container = document.getElementById('layout-container');
        container.className = themeData.dockviewTheme;
      }
    }

    // Send dark mode preference on connect
    function sendDarkModePreference() {
      const isDark = window.matchMedia('(prefers-color-scheme: dark)').matches;
      ws.send(JSON.stringify({type: 'dark-mode-preference', dark: isDark}));
    }

    // Listen for system dark mode changes
    window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', (e) => {
      if (ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify({type: 'dark-mode-preference', dark: e.matches}));
      }
    });

    function handleSymbolClicked(msg) {
      // Clear filters when navigating via REPL symbol click
      const pkgFilter = document.getElementById('package-filter');
      const symFilter = document.getElementById('symbol-filter');
      if (pkgFilter) pkgFilter.value = '';
      if (symFilter) symFilter.value = '';

      // Update Packages panel - select the package
      selectedPackage = msg.package;
      renderPackages();
      // Scroll package into view
      setTimeout(() => {
        const pkgEl = document.querySelector('#package-list .selected');
        if (pkgEl) pkgEl.scrollIntoView({block: 'center'});
      }, 50);

      // Update Symbols panel - set symbols and select the symbol
      symbols = msg.symbols || [];
      selectedSymbol = msg.symbol;
      renderSymbols();
      // Scroll symbol into view
      setTimeout(() => {
        const symEl = document.querySelector('#symbol-list .selected');
        if (symEl) symEl.scrollIntoView({block: 'center'});
      }, 50);

      // Update Symbol Info panel - use same rendering as Symbol list click
      if (msg.symbolInfo) {
        renderSymbolInfo(msg.symbolInfo);
      }
      // Activate the Symbol Info panel to bring it to the surface
      if (dockviewApi) {
        const panel = dockviewApi.getPanel('inspector');
        if (panel) panel.api.setActive();
      }
      // Restore focus to terminal after all updates
      setTimeout(() => { if (terminal) terminal.focus(); }, 100);
    }

    // Panel rendering functions
    function renderPackages(filter = '') {
      const el = document.getElementById('package-list');
      if (!el) return;
      const f = filter.toLowerCase();
      el.innerHTML = packages
        .filter(p => !f || p.toLowerCase().includes(f))
        .map(p => {
          const selected = p === selectedPackage ? 'selected' : '';
          const escaped = p.replace(/'/g, \"\\\\'\");
          return '<div class=\"list-item ' + selected + '\" onclick=\"selectPackage(\\'' + escaped + '\\')\"><span>' + p + '</span></div>';
        }).join('');
    }

    function renderSymbols(filter = '') {
      const el = document.getElementById('symbol-list');
      if (!el) return;
      const f = filter.toLowerCase();
      el.innerHTML = (symbols || [])
        .filter(s => !f || s[0].toLowerCase().includes(f))
        .map(s => {
          const [name] = s;
          const selected = (selectedSymbol && name.toUpperCase() === selectedSymbol.toUpperCase()) ? 'selected' : '';
          return `<div class='list-item ${selected}' onclick='selectSymbol(\"${name.replace(/'/g, \"\\\\'\")}\")'>${name}</div>`;
        }).join('');
    }

    // Store current symbol info for inspect links
    let currentSymbolInfo = null;

    function inspectClass(name, pkg) {
      const prefix = pkg ? (pkg + \"::\") : \"\";
      openInspector(\"(find-class '\" + prefix + name + \")\", pkg);
    }

    function inspectFunction(name, pkg, type) {
      const prefix = pkg ? (pkg + \"::\") : \"\";
      if (type === 'macro') {
        openInspector(\"(macro-function '\" + prefix + name + \")\", pkg);
      } else {
        openInspector(\"#'\" + prefix + name, pkg);
      }
    }

    function inspectVariable(name, pkg) {
      const prefix = pkg ? (pkg + \"::\") : \"\";
      openInspector(prefix + name, pkg);
    }

    function renderSymbolInfo(info) {
      const el = document.getElementById('detail-content');
      if (!el || !info) return;
      currentSymbolInfo = info;
      const name = info.name || 'Symbol';
      const pkg = info.package || selectedPackage || null;
      const pkgStr = pkg || '';
      let html = `<strong>${name}</strong>\\n\\n`;

      // Class binding
      if (info.class) {
        html += `<span class='binding-header'>[Class]</span>`;
        html += `<span class='inspect-link' onclick='inspectClass(\"${name}\", \"${pkgStr}\")'>[Inspect]</span>\\n`;
        if (info.class.superclasses && info.class.superclasses.length > 0) {
          html += `<strong>Superclasses:</strong> ${info.class.superclasses.join(', ')}\\n`;
        }
        if (info.class.slots && info.class.slots.length > 0) {
          html += `<strong>Slots:</strong> ${info.class.slots.join(', ')}\\n`;
        }
        html += '\\n';
      }

      // Function/macro/generic binding
      if (info.function) {
        const typeLabel = {function: 'Function', macro: 'Macro', generic: 'Generic Function'}[info.function.type] || 'Function';
        const fnType = info.function.type || 'function';
        html += `<span class='binding-header'>[${typeLabel}]</span>`;
        html += `<span class='inspect-link' onclick='inspectFunction(\"${name}\", \"${pkgStr}\", \"${fnType}\")'>[Inspect]</span>\\n`;
        if (info.function.arglist) html += `<strong>Arguments:</strong> ${info.function.arglist}\\n`;
        if (info.function.documentation) html += `<strong>Documentation:</strong>\\n${info.function.documentation}\\n`;
        html += '\\n';
      }

      // Variable binding
      if (info.variable) {
        const varType = info.variable.constantp ? 'Constant' : 'Variable';
        html += `<span class='binding-header'>[${varType}]</span>`;
        html += `<span class='inspect-link' onclick='inspectVariable(\"${name}\", \"${pkgStr}\")'>[Inspect]</span>\\n`;
        if (info.variable.value) html += `<strong>Value:</strong> ${info.variable.value}\\n`;
        if (info.variable.documentation) html += `<strong>Documentation:</strong>\\n${info.variable.documentation}\\n`;
        html += '\\n';
      }

      // Special operator
      if (info['special-operator']) {
        html += `<span class='binding-header'>[Special Operator]</span>\\n`;
        html += 'Built-in special form\\n\\n';
      }

      // If no bindings found
      if (!info.class && !info.function && !info.variable && !info['special-operator']) {
        html += '<em>Unbound symbol</em>\\n';
      }

      el.innerHTML = html;
    }

    function renderInspection(msg, panelId) {
      console.log('renderInspection panelId:', panelId, 'states:', inspectorStates);
      const state = inspectorStates.get(panelId);
      if (!state) {
        console.log('No state found for panelId:', panelId, 'stashing pending inspection');
        pendingInspections.set(panelId, msg);
        return;
      }
      console.log('state found:', state);

      const { element, header } = state;
      if (!element) return;

      // Track depth for Back button
      if (msg.action === 'push') state.depth++;
      else if (msg.action === 'pop') state.depth = Math.max(0, state.depth - 1);
      else state.depth = 1;

      // Show/hide Back button
      if (header) header.style.display = state.depth > 1 ? 'block' : 'none';

      let html = `<strong>${msg.title || 'Object'}</strong>\\n\\n`;
      (msg.entries || []).forEach(([label, value, action]) => {
        const escapedValue = String(value).replace(/</g, '&lt;').replace(/>/g, '&gt;');
        if (action !== null) {
          html += `<span class='inspector-label'>${label}: </span><span class='inspector-link' onclick='inspectAction(${action}, \"${panelId}\")'>${escapedValue}</span>\\n`;
        } else {
          html += `<span class='inspector-label'>${label}: </span><span class='inspector-value'>${escapedValue}</span>\\n`;
        }
      });
      element.innerHTML = html;
    }

    function selectPackage(pkg) {
      selectedPackage = pkg;
      selectedSymbol = null;  // Clear symbol selection when package changes
      renderPackages(document.getElementById('package-filter')?.value || '');
      ws.send(JSON.stringify({type: 'get-symbols', package: pkg}));
    }

    function selectSymbol(name) {
      selectedSymbol = name;
      renderSymbols(document.getElementById('symbol-filter')?.value || '');
      ws.send(JSON.stringify({type: 'get-symbol-info', package: selectedPackage, name: name}));
      // Activate the Symbol Info panel to bring it to the surface
      if (dockviewApi) {
        const panel = dockviewApi.getPanel('inspector');
        if (panel) panel.api.setActive();
      }
    }

    function inspectAction(action, panelId) {
      ws.send(JSON.stringify({type: 'inspector-action', index: action, panelId: panelId}));
    }

    function inspectBack(panelId) {
      ws.send(JSON.stringify({type: 'inspector-pop', panelId: panelId}));
    }

    function openInspector(form, pkg) {
      const panelId = 'inspector-' + (++inspectorCounter);
      console.log('openInspector:', form, panelId);
      // Create new inspector panel in Dockview
      if (dockviewApi) {
        dockviewApi.addPanel({
          id: panelId,
          component: 'dynamic-inspector',
          title: 'Inspector',
          params: { panelId: panelId },
          position: { referencePanel: 'inspector', direction: 'within' }
        });
      }
      // Send inspect request to server after panel creation to reduce races
      ws.send(JSON.stringify({type: 'inspect', form: form, panelId: panelId, package: pkg}));
    }

    // Lisp symbol characters: alphanumeric, -, *, +, /, <, >, =, ?, !, $, %, &, :
    const isSymbolChar = (c) => /[a-zA-Z0-9\\-*+/<>=?!$%&:_]/.test(c);

    // Extract Lisp symbol at position in terminal buffer
    function getSymbolAtPosition(col, row) {
      const info = getSymbolBounds(col, row);
      return info ? info.symbol : null;
    }

    // Get symbol bounds (start, end, symbol) at position
    function getSymbolBounds(col, row) {
      if (!terminal) return null;
      const buffer = terminal.buffer.active;
      const line = buffer.getLine(row);
      if (!line) return null;
      const lineText = line.translateToString();

      if (col >= lineText.length || !isSymbolChar(lineText[col])) return null;

      // Find symbol boundaries
      let start = col, end = col;
      while (start > 0 && isSymbolChar(lineText[start - 1])) start--;
      while (end < lineText.length && isSymbolChar(lineText[end])) end++;

      const symbol = lineText.substring(start, end).trim();
      return symbol.length > 0 ? { start, end, symbol } : null;
    }

    function inspectSymbolAtCursor(e) {
      if (!terminal) return;
      const rect = terminal.element.getBoundingClientRect();
      const renderer = terminal._core._renderService.dimensions;
      const col = Math.floor((e.clientX - rect.left) / renderer.css.cell.width);
      const row = Math.floor((e.clientY - rect.top) / renderer.css.cell.height) + terminal.buffer.active.viewportY;

      const symbol = getSymbolAtPosition(col, row);
      if (symbol) {
        ws.send(JSON.stringify({type: 'inspect', form: symbol}));
      }
    }

    // Panel classes for Dockview
    class PackagesPanel {
      constructor() {
        this._element = document.createElement('div');
        this._element.className = 'panel';
        this._element.innerHTML = `
          <div class='panel-header'>
            <input id='package-filter' placeholder='Filter packages...' oninput='renderPackages(this.value)'>
          </div>
          <div class='panel-content' id='package-list'></div>`;
      }
      get element() { return this._element; }
      init(params) { setTimeout(renderPackages, 100); }
    }

    class SymbolsPanel {
      constructor() {
        this._element = document.createElement('div');
        this._element.className = 'panel';
        this._element.innerHTML = `
          <div class='panel-header'>
            <input id='symbol-filter' placeholder='Filter symbols...' oninput='renderSymbols(this.value)'>
          </div>
          <div class='panel-content' id='symbol-list'></div>`;
      }
      get element() { return this._element; }
      init(params) {}
    }

    class InspectorPanel {
      constructor() {
        this._element = document.createElement('div');
        this._element.className = 'panel';
        this._element.innerHTML = `
          <div class='panel-content detail-content' id='detail-content'>
            Click a symbol to see its bindings.
          </div>`;
      }
      get element() { return this._element; }
      init(params) {}
    }

    class DynamicInspectorPanel {
      constructor() {
        this._element = document.createElement('div');
        this._element.className = 'panel';
        this._panelId = null;
      }
      get element() { return this._element; }
      init(params) {
        this._panelId = params.params?.panelId;
        const headerId = 'header-' + this._panelId;
        const contentId = 'content-' + this._panelId;
        this._element.innerHTML = `
          <div class='panel-header' id='${headerId}' style='display:none;'>
            <button onclick='inspectBack(\"${this._panelId}\")' style='padding:2px 8px;background:var(--bg-tertiary);border:1px solid var(--border);color:var(--fg-primary);border-radius:3px;cursor:pointer;'>← Back</button>
          </div>
          <div class='panel-content detail-content' id='${contentId}'>
            Loading...
          </div>`;
        // Register this panel's state - use querySelector on this element since it's not in DOM yet
        const contentEl = this._element.querySelector('#' + contentId);
        const headerEl = this._element.querySelector('#' + headerId);
        console.log('DynamicInspectorPanel init:', this._panelId, 'contentEl:', contentEl, 'headerEl:', headerEl);
        inspectorStates.set(this._panelId, {
          depth: 0,
          element: contentEl,
          header: headerEl
        });
        // Apply any inspection that arrived before this panel initialized.
        const pending = pendingInspections.get(this._panelId);
        if (pending) {
          pendingInspections.delete(this._panelId);
          renderInspection(pending, this._panelId);
        }
      }
      dispose() {
        if (this._panelId) {
          inspectorStates.delete(this._panelId);
        }
      }
    }

    class TerminalPanel {
      constructor() {
        this._element = document.createElement('div');
        this._element.className = 'terminal-container';
        this._element.id = 'terminal';
      }
      get element() { return this._element; }
      init(params) {
        setTimeout(() => {
          terminal = new Terminal({
            cursorBlink: true,
            fontFamily: \"'JetBrains Mono', monospace\",
            fontSize: 14,
            theme: { background: '#1e1e1e' }
          });
          fitAddon = new FitAddon.FitAddon();
          terminal.loadAddon(fitAddon);
          terminal.open(this._element);
          // Don't fit immediately - wait for Dockview layout to settle

          // Send all input directly to Lisp - the editor handles everything
          terminal.onData(data => {
            ws.send(JSON.stringify({type: 'input', data: data}));
          });

          // Click to inspect symbol - use click (not mousedown) to allow text selection
          // Store detected symbol from mousemove for click to use
          let hoveredSymbol = null;
          let mouseDownPos = null;
          let isDragging = false;

          // Track mousedown position to detect drag vs click
          terminal.element.addEventListener('mousedown', (e) => {
            mouseDownPos = { x: e.clientX, y: e.clientY };
            isDragging = false;
          });

          // Inspect on mouseup when no drag selection was made
          terminal.element.addEventListener('mouseup', (e) => {
            // Skip symbol inspect if a selection exists (click-drag selection)
            if (terminal && terminal.hasSelection && terminal.hasSelection()) {
              mouseDownPos = null;
              return;
            }
            // Only inspect if we have a symbol and didn't drag
            if (hoveredSymbol && mouseDownPos && !isDragging) {
              const dx = Math.abs(e.clientX - mouseDownPos.x);
              const dy = Math.abs(e.clientY - mouseDownPos.y);
              // If mouse moved less than 5 pixels, treat as a click not a drag
              if (dx < 5 && dy < 5) {
                ws.send(JSON.stringify({type: 'symbol-click', symbol: hoveredSymbol}));
                terminal.focus();
              }
            }
            mouseDownPos = null;
          });

          // Enable right-click copy via context menu
          terminal.attachCustomKeyEventHandler((e) => {
            // Allow Ctrl+C to copy when there's a selection (not send SIGINT)
            if (e.ctrlKey && e.key === 'c' && terminal.hasSelection()) {
              return false; // Let browser handle copy
            }
            // Allow Ctrl+Shift+C for copy
            if (e.ctrlKey && e.shiftKey && e.key === 'C') {
              return false;
            }
            // Allow Ctrl+Shift+V for paste
            if (e.ctrlKey && e.shiftKey && e.key === 'V') {
              return false;
            }
            return true; // Let xterm handle other keys
          });

          // Hover highlight box for symbols
          let highlightBox = null;
          const termEl = this._element;
          this._element.addEventListener('mousemove', (e) => {
            if (!terminal) return;
            if (mouseDownPos) {
              const dx = Math.abs(e.clientX - mouseDownPos.x);
              const dy = Math.abs(e.clientY - mouseDownPos.y);
              if (dx > 5 || dy > 5) {
                isDragging = true;
                hoveredSymbol = null;
                if (highlightBox) highlightBox.style.display = 'none';
                terminal.element.style.cursor = '';
                return;
              }
            }
            const rect = terminal.element.getBoundingClientRect();
            const renderer = terminal._core._renderService.dimensions;
            if (!renderer.css.cell.width) return;

            const col = Math.floor((e.clientX - rect.left) / renderer.css.cell.width);
            const row = Math.floor((e.clientY - rect.top) / renderer.css.cell.height);
            const bufferRow = row + terminal.buffer.active.viewportY;
            const symbolInfo = getSymbolBounds(col, bufferRow);

            if (symbolInfo) {
              hoveredSymbol = symbolInfo.symbol;  // Store for click handler
              if (!highlightBox) {
                highlightBox = document.createElement('div');
                highlightBox.style.cssText = 'position:absolute;border:1px solid var(--accent);border-radius:2px;pointer-events:none;z-index:10;';
                termEl.appendChild(highlightBox);
              }
              const cellW = renderer.css.cell.width;
              const cellH = renderer.css.cell.height;
              highlightBox.style.left = (symbolInfo.start * cellW) + 'px';
              highlightBox.style.top = (row * cellH) + 'px';
              highlightBox.style.width = ((symbolInfo.end - symbolInfo.start) * cellW) + 'px';
              highlightBox.style.height = cellH + 'px';
              highlightBox.style.display = 'block';
              terminal.element.style.cursor = 'pointer';
            } else {
              hoveredSymbol = null;  // Clear when not over symbol
              if (highlightBox) highlightBox.style.display = 'none';
              terminal.element.style.cursor = '';
            }
          });

          this._element.addEventListener('mouseleave', () => {
            hoveredSymbol = null;
            if (highlightBox) highlightBox.style.display = 'none';
            if (terminal) terminal.element.style.cursor = '';
          });

          // Refit on resize
          const doFit = () => { try { fitAddon.fit(); } catch(e) {} };
          new ResizeObserver(doFit).observe(this._element);

          // Signal terminal ready after layout has settled
          const signalReady = () => {
            doFit();
            if (ws.readyState === WebSocket.OPEN) {
              ws.send(JSON.stringify({type: 'terminal-ready'}));
            } else {
              ws.addEventListener('open', () => {
                ws.send(JSON.stringify({type: 'terminal-ready'}));
              }, {once: true});
            }
          };

          // Wait for Dockview layout to complete before signaling ready
          setTimeout(signalReady, 300);
        }, 100);
      }
    }

    // Create Dockview layout
    const container = document.getElementById('layout-container');
    const dv = window['dockview-core'];

    const api = dv.createDockview(container, {
      className: 'icl-dockview-theme',
      disableAutoResizing: false,
      createComponent: (options) => {
        switch (options.name) {
          case 'packages': return new PackagesPanel();
          case 'symbols': return new SymbolsPanel();
          case 'inspector': return new InspectorPanel();
          case 'dynamic-inspector': return new DynamicInspectorPanel();
          case 'terminal': return new TerminalPanel();
        }
      }
    });

    // Store API globally for dynamic panel creation
    dockviewApi = api;

    // Layout: Packages/Symbols/Symbol Info side by side at top (1/5 height), REPL Console below (4/5)
    api.addPanel({ id: 'terminal', component: 'terminal', title: 'REPL Console' });
    api.addPanel({ id: 'packages', component: 'packages', title: 'Packages', position: { referencePanel: 'terminal', direction: 'above' } });
    api.addPanel({ id: 'symbols', component: 'symbols', title: 'Symbols', position: { referencePanel: 'packages', direction: 'right' } });
    api.addPanel({ id: 'inspector', component: 'inspector', title: 'Symbol Info', position: { referencePanel: 'symbols', direction: 'right' } });

    // Set the top row to 20% height (1/5)
    setTimeout(() => {
      try {
        const groups = api.groups;
        if (groups && groups.length >= 2) {
          api.layout(window.innerWidth, window.innerHeight);
          // Find the top group and resize
          const topGroup = groups.find(g => g.panels.some(p => p.id === 'packages'));
          if (topGroup) {
            topGroup.api.setSize({ height: Math.floor(window.innerHeight * 0.2) });
          }
        }
      } catch(e) { console.log('Layout resize:', e); }
    }, 300);
  </script>
</body>
</html>" *browser-token*))

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

(defvar *assets-directory* nil
  "Directory containing browser assets (JS, CSS). Computed lazily at runtime.")

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
                   (t "application/octet-stream")))))
    ;; First try embedded assets
    (let ((embedded (get-embedded-asset filename)))
      (when embedded
        (set-content-type)
        (return-from serve-asset embedded)))
    ;; Fall back to filesystem (for development)
    (let ((filepath (merge-pathnames filename (get-assets-directory))))
      (when (probe-file filepath)
        (set-content-type)
        (alexandria:read-file-into-string filepath)))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor browser-acceptor) request)
  "Handle HTTP requests."
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

  (let ((url (format nil "http://127.0.0.1:~A~A" port *browser-path*)))
    (when open-browser
      (ignore-errors (uiop:run-program (list "xdg-open" url))))
    url))

(defun stop-browser ()
  "Stop the Dockview browser."
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
