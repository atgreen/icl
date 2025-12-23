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

(defvar *eval-generation-poller* nil
  "Thread that polls for external eval generation changes.")

(defvar *last-eval-generation* -1
  "Last seen eval generation from external Lisp.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Backend Query Helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun browser-query (code)
  "Execute CODE in the backend and return parsed result.
   Preserves REPL history variables (*, **, ***, etc.).
   Returns nil on error."
  (browser-log "browser-query: code length=~D code-preview=~S"
               (length code)
               (subseq code 0 (min 100 (length code))))
  (handler-case
      (let ((result-string (first (backend-eval-internal code))))
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

(defun needs-pipe-escape-p (name)
  "Check if symbol NAME needs pipe escaping for Common Lisp reader.
   Pipes are needed for: slashes, spaces, parens, or lowercase letters."
  (or (find #\/ name)
      (find #\Space name)
      (find #\( name)
      (find #\) name)
      (find #\' name)
      (find #\" name)
      (find #\; name)
      (find #\| name)
      ;; Lowercase letters require pipes (unless interned that way)
      (some #'lower-case-p name)))

(defun format-symbol-ref (package-name symbol-name)
  "Format a qualified symbol reference for the reader.
   Returns something like PKG::|symbol-name| or PKG::SYMBOL-NAME."
  (if (needs-pipe-escape-p symbol-name)
      (format nil "~A::|~A|" package-name symbol-name)
      (format nil "~A::~A" package-name symbol-name)))

(defun get-class-hierarchy (class-name package-name &key (depth 3))
  "Get class hierarchy graph data for CLASS-NAME.
   Returns (:nodes ((name pkg (slot-names...)) ...) :edges ((from to) ...))."
  (let ((sym-ref (format-symbol-ref package-name class-name)))
    (browser-log "get-class-hierarchy: class=~S pkg=~S sym-ref=~S"
                 class-name package-name sym-ref)
    (browser-query
     (format nil
             "(let ((root (find-class '~A nil)))
                (when root
                  (let ((nodes nil) (edges nil) (seen (make-hash-table)))
                    (labels ((get-slot-names (class)
                               (handler-case
                                   (or (mapcar (lambda (slot)
                                                 (symbol-name (sb-mop:slot-definition-name slot)))
                                               (sb-mop:class-direct-slots class))
                                       (list))
                                 (error () (list))))
                             (add-node (class)
                               (let ((name (class-name class)))
                                 (push (list (symbol-name name)
                                             (package-name (symbol-package name))
                                             (get-slot-names class))
                                       nodes)))
                             (walk-up (class d)
                               (unless (or (gethash class seen) (< d 0))
                                 (setf (gethash class seen) t)
                                 (add-node class)
                                 (dolist (super (sb-mop:class-direct-superclasses class))
                                   (push (list (symbol-name (class-name super))
                                               (symbol-name (class-name class)))
                                         edges)
                                   (walk-up super (1- d)))))
                             (walk-down (class d)
                               (unless (or (gethash class seen) (< d 0))
                                 (setf (gethash class seen) t)
                                 (dolist (sub (sb-mop:class-direct-subclasses class))
                                   (add-node sub)
                                   (push (list (symbol-name (class-name class))
                                               (symbol-name (class-name sub)))
                                         edges)
                                   (walk-down sub (1- d))))))
                      (walk-up root ~D)
                      (setf (gethash root seen) nil)
                      (walk-down root ~D))
                    (list :nodes (remove-duplicates nodes :test #'equal :key #'car)
                          :edges (remove-duplicates edges :test #'equal)))))"
             sym-ref depth depth))))

(defun get-class-children (class-name package-name)
  "Get direct subclasses and superclasses for CLASS-NAME.
   Returns (:nodes ((name pkg slots) ...) :edges ((from to) ...)).
   Includes both children (for expansion) and parents (for multiple inheritance)."
  (let ((sym-ref (format-symbol-ref package-name class-name)))
    (browser-log "get-class-children: class=~S pkg=~S sym-ref=~S"
                 class-name package-name sym-ref)
    (browser-query
     (format nil
             "(let ((root (find-class '~A nil)))
                (when root
                  (let ((nodes nil) (edges nil)
                        (root-name (class-name root)))
                    ;; Add direct subclasses
                    (dolist (sub (sb-mop:class-direct-subclasses root))
                      (let ((sub-name (class-name sub)))
                        (push (list (symbol-name sub-name)
                                    (package-name (symbol-package sub-name))
                                    (handler-case
                                        (mapcar (lambda (s) (symbol-name (sb-mop:slot-definition-name s)))
                                                (sb-mop:class-direct-slots sub))
                                      (error () nil)))
                              nodes)
                        (push (list (symbol-name root-name)
                                    (symbol-name sub-name))
                              edges)))
                    ;; Add direct superclasses (for multiple inheritance support)
                    (dolist (super (sb-mop:class-direct-superclasses root))
                      (let ((super-name (class-name super)))
                        (push (list (symbol-name super-name)
                                    (package-name (symbol-package super-name))
                                    (handler-case
                                        (mapcar (lambda (s) (symbol-name (sb-mop:slot-definition-name s)))
                                                (sb-mop:class-direct-slots super))
                                      (error () nil)))
                              nodes)
                        (push (list (symbol-name super-name)
                                    (symbol-name root-name))
                              edges)))
                    (list :nodes (remove-duplicates nodes :test #'equal :key #'car)
                          :edges (remove-duplicates edges :test #'equal)))))"
             sym-ref))))

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
           (let ((symbol-string (gethash "symbol" json))
                 (source (gethash "source" json)))
             (browser-log "WS symbol-click: symbol=~S source=~S" symbol-string source)
             (when symbol-string
               (bt:make-thread
                (lambda ()
                  (send-symbol-click-response client symbol-string source))
                :name "symbol-click-handler"))))

          ;; Client reports dark mode preference
          ((string= type "dark-mode-preference")
           (let ((dark-p (gethash "dark" json)))
             ;; Auto-select browser theme based on client preference
             (auto-select-browser-theme dark-p)))

          ;; Request current theme
          ((string= type "get-theme")
           (when *current-browser-theme*
             (send-browser-theme-to-client client *current-browser-theme*)))

          ;; Request class hierarchy graph
          ((string= type "get-class-graph")
           (let ((class-name (gethash "className" json))
                 (package-name (gethash "packageName" json))
                 (panel-id (gethash "panelId" json)))
             (when (and class-name package-name)
               (bt:make-thread
                (lambda ()
                  (send-class-graph client class-name package-name panel-id))
                :name "class-graph-handler"))))

          ;; Request class graph expansion (direct subclasses)
          ((string= type "expand-class-graph")
           (let ((class-name (gethash "className" json))
                 (package-name (gethash "packageName" json))
                 (panel-id (gethash "panelId" json)))
             (when (and class-name package-name)
               (bt:make-thread
                (lambda ()
                  (send-class-graph-expand client class-name package-name panel-id))
                :name "class-graph-expand-handler"))))

          ;; List children names only (for selector popup)
          ((string= type "list-class-children")
           (let ((class-name (gethash "className" json))
                 (package-name (gethash "packageName" json))
                 (panel-id (gethash "panelId" json)))
             (when (and class-name package-name)
               (bt:make-thread
                (lambda ()
                  (send-class-children-list client class-name package-name panel-id))
                :name "list-children-handler"))))

          ;; Add single child to graph
          ((string= type "add-class-child")
           (let ((parent-name (gethash "parentName" json))
                 (child-name (gethash "childName" json))
                 (child-pkg (gethash "childPackage" json))
                 (panel-id (gethash "panelId" json)))
             (when (and parent-name child-name)
               (bt:make-thread
                (lambda ()
                  (send-single-child client parent-name child-name child-pkg panel-id))
                :name "add-child-handler"))))

          ;; Refresh hash-table data
          ((string= type "refresh-hashtable")
           (let ((source-expr (gethash "sourceExpr" json))
                 (panel-id (gethash "panelId" json)))
             (when source-expr
               (bt:make-thread
                (lambda ()
                  (send-hashtable-refresh client source-expr panel-id))
                :name "refresh-hashtable-handler"))))

          ;; Refresh Venn diagram data
          ((string= type "refresh-venn")
           (let ((source-expr (gethash "sourceExpr" json))
                 (panel-id (gethash "panelId" json)))
             (when source-expr
               (bt:make-thread
                (lambda ()
                  (send-venn-refresh client source-expr panel-id))
                :name "refresh-venn-handler"))))

          ;; Refresh HTML panel data
          ((string= type "refresh-html")
           (let ((source-expr (gethash "sourceExpr" json))
                 (panel-id (gethash "panelId" json)))
             (when source-expr
               (bt:make-thread
                (lambda ()
                  (send-html-refresh client source-expr panel-id))
                :name "refresh-html-handler"))))

          ;; Refresh SVG panel data
          ((string= type "refresh-svg")
           (let ((source-expr (gethash "sourceExpr" json))
                 (panel-id (gethash "panelId" json)))
             (when source-expr
               (bt:make-thread
                (lambda ()
                  (send-svg-refresh client source-expr panel-id))
                :name "refresh-svg-handler"))))

          ;; Unified viz refresh - evaluates expression and returns appropriate type
          ((string= type "refresh-viz")
           (let ((source-expr (gethash "sourceExpr" json))
                 (panel-id (gethash "panelId" json)))
             (when source-expr
               (bt:make-thread
                (lambda ()
                  (send-viz-refresh client source-expr panel-id))
                :name "refresh-viz-handler")))))))))

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
  (when *repl-resource*
    (let ((clients (hunchensocket:clients *repl-resource*)))
      (when clients
        (ignore-errors
          (dolist (client clients)
            (send-packages-list client)))))))

(defun refresh-browser-visualizations ()
  "Signal browser to refresh visualization panels (hash-tables, class graphs).
   Called after REPL evaluation to pick up data changes."
  (when *repl-resource*
    (let ((clients (hunchensocket:clients *repl-resource*)))
      (when clients
        (ignore-errors
          (dolist (client clients)
            (let ((obj (make-hash-table :test 'equal)))
              (setf (gethash "type" obj) "refresh-visualizations")
              (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj)))))))))

(defun open-speedscope-panel (profile-id &optional title)
  "Send message to browser to open a Speedscope panel for PROFILE-ID."
  (when *repl-resource*
    (dolist (client (hunchensocket:clients *repl-resource*))
      (let ((obj (make-hash-table :test 'equal)))
        (setf (gethash "type" obj) "open-speedscope")
        (setf (gethash "profileId" obj) profile-id)
        (when title
          (setf (gethash "title" obj) title))
        (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))))))

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

(defun send-class-graph (client class-name package-name panel-id)
  "Send class hierarchy graph data to CLIENT."
  (browser-log "send-class-graph: class=~S package=~S panel-id=~S"
               class-name package-name panel-id)
  (handler-case
      (let ((data (get-class-hierarchy class-name package-name)))
        (if data
            (ws-send client "class-graph"
                     :panel-id panel-id
                     :class-name class-name
                     :nodes (getf data :nodes)
                     :edges (getf data :edges))
            (ws-send client "class-graph"
                     :panel-id panel-id
                     :error "Class not found")))
    (error (e)
      (browser-log "send-class-graph: ERROR ~A" e)
      (ws-send client "class-graph"
               :panel-id panel-id
               :error (format nil "~A" e)))))

(defun send-class-graph-expand (client class-name package-name panel-id)
  "Send class graph expansion (direct subclasses) to CLIENT."
  (browser-log "send-class-graph-expand: class=~S package=~S panel-id=~S"
               class-name package-name panel-id)
  (handler-case
      (let ((data (get-class-children class-name package-name)))
        (if data
            (ws-send client "class-graph-expand"
                     :panel-id panel-id
                     :class-name class-name
                     :nodes (getf data :nodes)
                     :edges (getf data :edges))
            (ws-send client "class-graph-expand"
                     :panel-id panel-id
                     :error "Class not found")))
    (error (e)
      (browser-log "send-class-graph-expand: ERROR ~A" e)
      (ws-send client "class-graph-expand"
               :panel-id panel-id
               :error (format nil "~A" e)))))

(defun send-class-children-list (client class-name package-name panel-id)
  "Send list of direct subclass names for child selector popup."
  (browser-log "send-class-children-list: class=~S package=~S" class-name package-name)
  (handler-case
      (let* ((sym-ref (format-symbol-ref package-name class-name))
             (children (browser-query
                        (format nil
                                "(let ((root (find-class '~A nil)))
                                   (when root
                                     (mapcar (lambda (sub)
                                               (let ((name (class-name sub)))
                                                 (list (symbol-name name)
                                                       (package-name (symbol-package name)))))
                                             (sb-mop:class-direct-subclasses root))))"
                                sym-ref))))
        (ws-send client "class-children-list"
                 :panel-id panel-id
                 :parent-name class-name
                 :parent-package package-name
                 :children (or children nil)))
    (error (e)
      (browser-log "send-class-children-list: ERROR ~A" e)
      (ws-send client "class-children-list"
               :panel-id panel-id
               :error (format nil "~A" e)))))

(defun send-single-child (client parent-name child-name child-pkg panel-id)
  "Send a single child node and edge to the graph."
  (browser-log "send-single-child: parent=~S child=~S pkg=~S" parent-name child-name child-pkg)
  (handler-case
      (let* ((sym-ref (format-symbol-ref child-pkg child-name))
             (slots (browser-query
                     (format nil
                             "(let ((class (find-class '~A nil)))
                                (when class
                                  (handler-case
                                      (mapcar (lambda (s) (symbol-name (sb-mop:slot-definition-name s)))
                                              (sb-mop:class-direct-slots class))
                                    (error () nil))))"
                             sym-ref))))
        (ws-send client "class-graph-expand"
                 :panel-id panel-id
                 :class-name child-name
                 :nodes (list (list child-name child-pkg (or slots nil)))
                 :edges (list (list parent-name child-name))))
    (error (e)
      (browser-log "send-single-child: ERROR ~A" e)
      (ws-send client "class-graph-expand"
               :panel-id panel-id
               :error (format nil "~A" e)))))

(defun send-hashtable-refresh (client source-expr panel-id)
  "Re-evaluate SOURCE-EXPR and send updated hash-table data to CLIENT."
  (browser-log "send-hashtable-refresh: expr=~S panel-id=~S" source-expr panel-id)
  (handler-case
      (let* ((query (format nil "(let ((obj ~A))
                                   (if (hash-table-p obj)
                                       (list :count (hash-table-count obj)
                                             :entries (loop for k being the hash-keys of obj using (hash-value v)
                                                            for i from 0 below 100
                                                            collect (list (princ-to-string k)
                                                                          (princ-to-string v))))
                                       (list :error \"No longer a hash-table\")))"
                            source-expr))
             (result (browser-query query)))
        (when result
          (let ((obj (make-hash-table :test 'equal)))
            (setf (gethash "type" obj) "hashtable-refresh")
            (setf (gethash "panelId" obj) panel-id)
            (if (getf result :error)
                (setf (gethash "error" obj) (getf result :error))
                (progn
                  (setf (gethash "count" obj) (getf result :count))
                  (setf (gethash "entries" obj) (getf result :entries))))
            (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj)))))
    (error (e)
      (browser-log "send-hashtable-refresh: ERROR ~A" e))))

(defun send-venn-refresh (client source-expr panel-id)
  "Re-evaluate SOURCE-EXPR (space-separated set expressions) and send updated Venn data."
  (browser-log "send-venn-refresh: expr=~S panel-id=~S" source-expr panel-id)
  (handler-case
      ;; Parse the source expression to extract individual set expressions
      (let* ((expressions (uiop:split-string source-expr :separator " "))
             (expressions (remove-if (lambda (s) (zerop (length s))) expressions))
             (query (format nil "(let ((sets (list ~{~A~^ ~}))
                                   (fset-pkg (find-package :fset))
                                   (fset-set?-sym nil)
                                   (fset-convert-sym nil))
                                   (when fset-pkg
                                     (setf fset-set?-sym (intern \"SET?\" fset-pkg))
                                     (setf fset-convert-sym (intern \"CONVERT\" fset-pkg)))
                                   (if (and fset-set?-sym (fboundp fset-set?-sym)
                                            (every (lambda (s)
                                                     (funcall (symbol-function fset-set?-sym) s))
                                                   sets))
                                       (list :members
                                             (mapcar (lambda (s)
                                                       (loop for m in (funcall (symbol-function fset-convert-sym) 'list s)
                                                             for i from 0 below 50
                                                             collect (princ-to-string m)))
                                                     sets))
                                       (list :error \"Not all values are FSet sets\")))"
                            expressions))
             (result (browser-query query)))
        (when result
          (let ((obj (make-hash-table :test 'equal)))
            (setf (gethash "type" obj) "venn-refresh")
            (setf (gethash "panelId" obj) panel-id)
            (if (getf result :error)
                (setf (gethash "error" obj) (getf result :error))
                (setf (gethash "setMembers" obj)
                      (mapcar (lambda (members) (coerce members 'vector))
                              (getf result :members))))
            (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj)))))
    (error (e)
      (browser-log "send-venn-refresh: ERROR ~A" e))))

(defun send-html-refresh (client source-expr panel-id)
  "Re-evaluate SOURCE-EXPR and send updated HTML content, or signal type change."
  (browser-log "send-html-refresh: expr=~S panel-id=~S" source-expr panel-id)
  (handler-case
      (let* ((query (format nil "(let ((obj ~A))
                                   (cond
                                     ((and (stringp obj)
                                           (let ((trimmed (string-left-trim '(#\\Space #\\Tab #\\Newline) obj)))
                                             (or (and (>= (length trimmed) 9)
                                                      (string-equal (subseq trimmed 0 9) \"<!DOCTYPE\"))
                                                 (and (>= (length trimmed) 5)
                                                      (string-equal (subseq trimmed 0 5) \"<html\")))))
                                      (list :html obj))
                                     ((hash-table-p obj)
                                      (list :hash-table
                                            (hash-table-count obj)
                                            (loop for k being the hash-keys of obj using (hash-value v)
                                                  for i from 0 below 100
                                                  collect (list (princ-to-string k) (princ-to-string v)))))
                                     (t (list :other (princ-to-string (type-of obj))))))"
                            source-expr))
             (result (browser-query query)))
        (when result
          (let ((obj (make-hash-table :test 'equal))
                (parsed (ignore-errors (read-from-string result))))
            (cond
              ((and parsed (eq (first parsed) :html))
               (setf (gethash "type" obj) "html-refresh")
               (setf (gethash "panelId" obj) panel-id)
               (setf (gethash "content" obj) (second parsed)))
              ((and parsed (eq (first parsed) :hash-table))
               ;; Type changed to hash-table - signal panel replacement
               (setf (gethash "type" obj) "viz-type-changed")
               (setf (gethash "panelId" obj) panel-id)
               (setf (gethash "sourceExpr" obj) source-expr)
               (setf (gethash "newType" obj) "hash-table")
               (setf (gethash "count" obj) (second parsed))
               (setf (gethash "entries" obj) (third parsed)))
              (t
               (setf (gethash "type" obj) "html-refresh")
               (setf (gethash "panelId" obj) panel-id)
               (setf (gethash "error" obj) (format nil "Value changed to ~A" (second parsed)))))
            (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj)))))
    (error (e)
      (browser-log "send-html-refresh: ERROR ~A" e))))

(defun send-svg-refresh (client source-expr panel-id)
  "Re-evaluate SOURCE-EXPR and send updated SVG content to panel."
  (browser-log "send-svg-refresh: expr=~S panel-id=~S" source-expr panel-id)
  (handler-case
      (let* ((query (format nil "(let ((obj ~A))
                                   (if (stringp obj) obj nil))"
                            source-expr))
             (result (browser-query query)))
        (let ((obj (make-hash-table :test 'equal)))
          (setf (gethash "type" obj) "svg-refresh")
          (setf (gethash "panelId" obj) panel-id)
          (if result
              (setf (gethash "content" obj) result)
              (setf (gethash "error" obj) "Expression did not evaluate to a string"))
          (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))))
    (error (e)
      (browser-log "send-svg-refresh: ERROR ~A" e))))

(defun send-viz-refresh (client source-expr panel-id)
  "Re-evaluate SOURCE-EXPR, detect type, and send appropriate viz data."
  (browser-log "send-viz-refresh: expr=~S panel-id=~S" source-expr panel-id)
  (handler-case
      (let* ((query (format nil "(let ((obj ~A))
                                   (cond
                                     ;; HTML string
                                     ((and (stringp obj)
                                           (let ((trimmed (string-left-trim '(#\\Space #\\Tab #\\Newline) obj)))
                                             (or (and (>= (length trimmed) 9)
                                                      (string-equal (subseq trimmed 0 9) \"<!DOCTYPE\"))
                                                 (and (>= (length trimmed) 5)
                                                      (string-equal (subseq trimmed 0 5) \"<html\")))))
                                      (list :html obj))
                                     ;; SVG string
                                     ((and (stringp obj)
                                           (let ((trimmed (string-left-trim '(#\\Space #\\Tab #\\Newline) obj)))
                                             (or (and (>= (length trimmed) 5)
                                                      (string-equal (subseq trimmed 0 5) \"<?xml\"))
                                                 (and (>= (length trimmed) 4)
                                                      (string-equal (subseq trimmed 0 4) \"<svg\")))))
                                      (list :svg obj))
                                     ;; Hash table
                                     ((hash-table-p obj)
                                      (list :hash-table
                                            (hash-table-count obj)
                                            (loop for k being the hash-keys of obj using (hash-value v)
                                                  for i from 0 below 100
                                                  collect (list (princ-to-string k) (princ-to-string v)))))
                                     ;; Unknown type
                                     (t (list :unknown (princ-to-string (type-of obj))))))"
                            source-expr))
             (parsed (browser-query query)))  ; browser-query already parses
        (when parsed
          (let ((obj (make-hash-table :test 'equal)))
            (setf (gethash "type" obj) "viz-refresh")
            (setf (gethash "panelId" obj) panel-id)
            (setf (gethash "sourceExpr" obj) source-expr)
            (cond
              ((and (listp parsed) (eq (first parsed) :html))
               (setf (gethash "vizType" obj) "html")
               (setf (gethash "content" obj) (second parsed)))
              ((and (listp parsed) (eq (first parsed) :svg))
               (setf (gethash "vizType" obj) "svg")
               (setf (gethash "content" obj) (second parsed)))
              ((and (listp parsed) (eq (first parsed) :hash-table))
               (setf (gethash "vizType" obj) "hash-table")
               (setf (gethash "count" obj) (second parsed))
               (setf (gethash "entries" obj) (third parsed)))
              (t
               (setf (gethash "vizType" obj) "unknown")
               (setf (gethash "error" obj) (format nil "Unknown type: ~A" parsed))))
            (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj)))))
    (error (e)
      (browser-log "send-viz-refresh: ERROR ~A" e))))

(defun open-class-graph-panel (class-name package-name)
  "Send message to browser to open a class graph panel."
  (when *repl-resource*
    (dolist (client (hunchensocket:clients *repl-resource*))
      (let ((obj (make-hash-table :test 'equal)))
        (setf (gethash "type" obj) "open-class-graph")
        (setf (gethash "className" obj) class-name)
        (setf (gethash "packageName" obj) package-name)
        (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))))))

(defun open-hash-table-panel (title count entries &optional source-expr)
  "Send message to browser to open a hash-table visualization panel."
  (when *repl-resource*
    (dolist (client (hunchensocket:clients *repl-resource*))
      (let ((obj (make-hash-table :test 'equal)))
        (setf (gethash "type" obj) "open-hash-table")
        (setf (gethash "title" obj) title)
        (setf (gethash "count" obj) count)
        (setf (gethash "entries" obj) entries)
        (when source-expr
          (setf (gethash "sourceExpr" obj) source-expr))
        (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))))))

(defun open-venn-panel (set-names set-members source-expr)
  "Send message to browser to open a Venn diagram panel.
   SET-NAMES is a list of expression strings (e.g., '(\"*foo*\" \"*bar*\")).
   SET-MEMBERS is a list of lists, each containing member strings for that set.
   SOURCE-EXPR is the original expression for refresh purposes."
  (when *repl-resource*
    (dolist (client (hunchensocket:clients *repl-resource*))
      (let ((obj (make-hash-table :test 'equal)))
        (setf (gethash "type" obj) "open-venn")
        (setf (gethash "setNames" obj) (coerce set-names 'vector))
        (setf (gethash "setMembers" obj)
              (mapcar (lambda (members) (coerce members 'vector)) set-members))
        (setf (gethash "sourceExpr" obj) source-expr)
        (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))))))

(defun open-svg-panel (title content source-expr)
  "Send message to browser to open an SVG visualization panel."
  (when *repl-resource*
    (dolist (client (hunchensocket:clients *repl-resource*))
      (let ((obj (make-hash-table :test 'equal)))
        (setf (gethash "type" obj) "open-svg")
        (setf (gethash "title" obj) title)
        (setf (gethash "content" obj) content)
        (setf (gethash "sourceExpr" obj) source-expr)
        (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))))))

(defun open-html-panel (title content source-expr)
  "Send message to browser to open an HTML visualization panel."
  (when *repl-resource*
    (dolist (client (hunchensocket:clients *repl-resource*))
      (let ((obj (make-hash-table :test 'equal)))
        (setf (gethash "type" obj) "open-html")
        (setf (gethash "title" obj) title)
        (setf (gethash "content" obj) content)
        (setf (gethash "sourceExpr" obj) source-expr)
        (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))))))

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

(defun send-symbol-click-response (client symbol-string &optional source)
  "Handle a click on a symbol in the REPL.
   Updates all three panels: Packages, Symbols, and Inspector.
   SOURCE indicates where the click originated (e.g., 'class-graph')."
  (browser-log "send-symbol-click-response: symbol-string=~S source=~S" symbol-string source)
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
            ;; Pass through source to prevent tab switching for class-graph clicks
            (when source
              (setf (gethash "source" obj) source))
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
    .terminal-container .xterm-viewport { height: 100%%; overflow-y: auto !important; scrollbar-width: thin; scrollbar-color: var(--bg-tertiary) var(--bg-primary); }
    .terminal-container .xterm-viewport::-webkit-scrollbar { width: 10px; }
    .terminal-container .xterm-viewport::-webkit-scrollbar-track { background: var(--bg-primary); }
    .terminal-container .xterm-viewport::-webkit-scrollbar-thumb { background: var(--bg-tertiary); border-radius: 4px; }
    .terminal-container .xterm-viewport::-webkit-scrollbar-thumb:hover { background: var(--fg-muted); }
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
  <script src='/assets/viz-standalone.js'></script>
  <script>
    // WebSocket connection
    const ws = new WebSocket('ws://' + location.host + '/ws/~A');
    let terminal, fitAddon;
    let pendingTheme = null;  // Store theme if received before terminal is ready
    let selectedPackage = null;
    let selectedSymbol = null;
    let packages = [];
    let symbols = [];

    // Dockview API and inspector management
    let dockviewApi = null;
    let inspectorCounter = 0;
    const inspectorStates = new Map();  // panelId -> {depth, element, header}
    const pendingInspections = new Map();  // panelId -> last inspection msg

    // Graphviz class graph state management
    let classGraphCounter = 0;
    const graphvizStates = new Map();  // panelId -> GraphvizPanel instance
    const pendingClassGraphs = new Map();  // panelId -> pending graph messages

    ws.onopen = () => {
      console.log('Connected');
      // Send dark mode preference for theme auto-selection
      sendDarkModePreference();
      ws.send(JSON.stringify({type: 'get-packages'}));
    };

    ws.onclose = () => {
      console.log('Connection closed - ICL process terminated');
      // Close the browser window/tab when ICL dies
      window.close();
      // If window.close() doesn't work (e.g., not opened by script), show message
      document.body.innerHTML = '<div style=\"display:flex;align-items:center;justify-content:center;height:100vh;background:#1a1a2e;color:#eee;font-family:monospace;font-size:1.2em;\">ICL process terminated. You may close this tab.</div>';
    };

    ws.onerror = (err) => {
      console.error('WebSocket error:', err);
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
        case 'open-speedscope':
          openSpeedscopePanel(msg.profileId, msg.title);
          break;
        case 'class-graph':
          handleClassGraph(msg);
          break;
        case 'class-graph-expand':
          handleClassGraphExpand(msg);
          break;
        case 'class-children-list':
          handleClassChildrenList(msg);
          break;
        case 'open-class-graph':
          openClassGraphPanel(msg.className, msg.packageName);
          break;
        case 'open-hash-table':
          openHashTablePanel(msg.title, msg.count, msg.entries, msg.sourceExpr);
          break;
        case 'open-svg':
          openSvgPanel(msg.title, msg.content, msg.sourceExpr);
          break;
        case 'open-html':
          openHtmlPanel(msg.title, msg.content, msg.sourceExpr);
          break;
        case 'refresh-visualizations':
          refreshAllVisualizations();
          break;
        case 'hashtable-refresh':
          handleHashtableRefresh(msg);
          break;
        case 'open-venn':
          openVennPanel(msg.setNames, msg.setMembers, msg.sourceExpr);
          break;
        case 'venn-refresh':
          handleVennRefresh(msg);
          break;
        case 'html-refresh':
          handleHtmlRefresh(msg);
          break;
        case 'svg-refresh':
          handleSvgRefresh(msg);
          break;
        case 'viz-type-changed':
          handleVizTypeChanged(msg);
          break;
        case 'viz-refresh':
          handleVizRefresh(msg);
          break;
      }
    };

    // Open Speedscope flame graph panel
    let speedscopeCounter = 0;
    function openSpeedscopePanel(profileId, title) {
      const panelId = 'speedscope-' + (++speedscopeCounter);
      const profileUrl = '/profile-data/' + profileId + '.json';
      if (dockviewApi) {
        dockviewApi.addPanel({
          id: panelId,
          component: 'speedscope',
          title: title || 'Flame Graph',
          params: { profileUrl: profileUrl },
          position: { referencePanel: 'terminal', direction: 'right' }
        });
      }
    }

    // Handle class graph data from server
    function handleClassGraph(msg) {
      const panelId = msg['panel-id'] || msg.panelId;
      const panel = graphvizStates.get(panelId);
      if (panel) {
        panel.updateGraph(msg);
      } else {
        const pending = pendingClassGraphs.get(panelId) || [];
        pending.push(msg);
        pendingClassGraphs.set(panelId, pending);
      }
    }

    function handleClassGraphExpand(msg) {
      const panelId = msg['panel-id'] || msg.panelId;
      const panel = graphvizStates.get(panelId);
      if (panel) {
        panel.addGraph(msg);
      } else {
        const pending = pendingClassGraphs.get(panelId) || [];
        pending.push(msg);
        pendingClassGraphs.set(panelId, pending);
      }
    }

    function handleClassChildrenList(msg) {
      const panelId = msg['panel-id'] || msg.panelId;
      const panel = graphvizStates.get(panelId);
      if (panel) {
        panel.showChildSelector(msg);
      }
    }

    // Open class graph panel
    function openClassGraphPanel(className, packageName) {
      console.log('openClassGraphPanel called:', className, packageName);
      const panelId = 'classgraph-' + (++classGraphCounter);
      if (dockviewApi) {
        console.log('Adding panel:', panelId);
        dockviewApi.addPanel({
          id: panelId,
          component: 'graphviz',
          title: 'Classes: ' + className,
          params: { panelId, className, packageName },
          position: { referencePanel: 'terminal', direction: 'right' }
        });
      } else {
        console.error('dockviewApi not available');
      }
    }

    // Open hash-table panel
    let hashTableCounter = 0;
    const hashtableStates = new Map();  // panelId -> HashTablePanel instance

    function openHashTablePanel(title, count, entries, sourceExpr) {
      console.log('openHashTablePanel called:', title, count, entries?.length, sourceExpr);
      const panelId = 'hashtable-' + (++hashTableCounter);
      if (dockviewApi) {
        dockviewApi.addPanel({
          id: panelId,
          component: 'hashtable',
          title: title || 'Hash Table',
          params: { panelId, count, entries, sourceExpr },
          position: { referencePanel: 'terminal', direction: 'right' }
        });
      } else {
        console.error('dockviewApi not available');
      }
    }

    // Sanitize expression for use as panel ID (remove special chars)
    function sanitizeForPanelId(expr) {
      if (!expr) return String(Date.now());
      // Replace special characters with safe alternatives
      return expr.replace(/[^a-zA-Z0-9-]/g, '_');
    }

    // Unified visualization state - tracks all viz panels by source expression
    const vizStates = new Map();  // panelId -> {sourceExpr, element, type}

    function registerVizPanel(panelId, sourceExpr, element, type) {
      vizStates.set(panelId, { sourceExpr, element, type });
    }

    function unregisterVizPanel(panelId) {
      vizStates.delete(panelId);
    }

    // Open SVG panel - tracks by source expression for updates
    const svgStates = new Map();
    function openSvgPanel(title, content, sourceExpr) {
      console.log('openSvgPanel called:', title, content?.length, sourceExpr);
      const panelId = 'svg-' + sanitizeForPanelId(sourceExpr);
      console.log('SVG panelId:', panelId, 'svgStates has:', svgStates.has(panelId));
      if (dockviewApi) {
        // Check if panel exists and update it
        if (svgStates.has(panelId)) {
          console.log('Updating existing SVG panel');
          const panelState = svgStates.get(panelId);
          if (panelState && panelState._element) {
            panelState._element.innerHTML = content;
            const svg = panelState._element.querySelector('svg');
            if (svg) {
              svg.style.maxWidth = '100%';
              svg.style.maxHeight = '100%';
              svg.style.width = 'auto';
              svg.style.height = 'auto';
            }
          }
        } else {
          console.log('Creating new SVG panel');
          dockviewApi.addPanel({
            id: panelId,
            component: 'svg',
            title: title || 'SVG',
            params: { content, sourceExpr, panelId },
            position: { referencePanel: 'terminal', direction: 'right' }
          });
        }
      }
    }

    // Open HTML panel - tracks by source expression for updates
    const htmlStates = new Map();
    function openHtmlPanel(title, content, sourceExpr) {
      console.log('openHtmlPanel called:', title, content?.length, sourceExpr);
      const panelId = 'html-' + sanitizeForPanelId(sourceExpr);
      console.log('HTML panelId:', panelId, 'htmlStates has:', htmlStates.has(panelId));
      if (dockviewApi) {
        // Check if panel exists and update it
        if (htmlStates.has(panelId)) {
          console.log('Updating existing HTML panel');
          const panelState = htmlStates.get(panelId);
          if (panelState && panelState._iframe) {
            panelState._iframe.srcdoc = content;
          }
        } else {
          console.log('Creating new HTML panel');
          dockviewApi.addPanel({
            id: panelId,
            component: 'html',
            title: title || 'HTML',
            params: { content, sourceExpr, panelId },
            position: { referencePanel: 'terminal', direction: 'right' }
          });
        }
      }
    }

    // Refresh all visualization panels
    function refreshAllVisualizations() {
      // Refresh class graph panels (specialized - not expression-based)
      graphvizStates.forEach((panel, panelId) => {
        if (panel._className && panel._packageName) {
          ws.send(JSON.stringify({
            type: 'get-class-graph',
            className: panel._className,
            packageName: panel._packageName,
            panelId: panelId
          }));
        }
      });
      // Refresh Venn diagram panels (specialized - multi-expression)
      vennStates.forEach((panel, panelId) => {
        if (panel._sourceExpr) {
          ws.send(JSON.stringify({
            type: 'refresh-venn',
            sourceExpr: panel._sourceExpr,
            panelId: panelId
          }));
        }
      });
      // Unified refresh for all viz panels (HTML, SVG, hash-table, etc.)
      // This handles type changes automatically
      vizStates.forEach((state, panelId) => {
        if (state.sourceExpr) {
          ws.send(JSON.stringify({
            type: 'refresh-viz',
            sourceExpr: state.sourceExpr,
            panelId: panelId
          }));
        }
      });
    }

    // Handle hash-table refresh data
    function handleHashtableRefresh(msg) {
      const panelId = msg.panelId;
      const panel = hashtableStates.get(panelId);
      if (panel) {
        if (msg.error) {
          console.log('Hash-table refresh error:', msg.error);
        } else {
          panel.updateData(msg.count, msg.entries);
        }
      }
    }

    // Handle HTML panel refresh data
    function handleHtmlRefresh(msg) {
      const panelId = msg.panelId;
      const panel = htmlStates.get(panelId);
      if (panel) {
        if (msg.error) {
          console.log('HTML refresh error:', msg.error);
        } else if (panel._iframe) {
          panel._iframe.srcdoc = msg.content;
        }
      }
    }

    // Handle SVG panel refresh data
    function handleSvgRefresh(msg) {
      const panelId = msg.panelId;
      const panel = svgStates.get(panelId);
      if (panel) {
        if (msg.error) {
          console.log('SVG refresh error:', msg.error);
        } else {
          panel._element.innerHTML = msg.content;
          const svg = panel._element.querySelector('svg');
          if (svg) {
            svg.style.maxWidth = '100%';
            svg.style.maxHeight = '100%';
            svg.style.width = 'auto';
            svg.style.height = 'auto';
          }
        }
      }
    }

    // Handle visualization type change - close old panel and open new one
    function handleVizTypeChanged(msg) {
      const oldPanelId = msg.panelId;
      const sourceExpr = msg.sourceExpr;
      const newType = msg.newType;
      console.log('handleVizTypeChanged:', oldPanelId, '->', newType, 'for', sourceExpr);

      // Remove old panel from state maps
      if (htmlStates.has(oldPanelId)) {
        htmlStates.delete(oldPanelId);
      }
      if (svgStates.has(oldPanelId)) {
        svgStates.delete(oldPanelId);
      }

      // Close old panel in dockview
      const oldPanel = dockviewApi?.getPanel(oldPanelId);
      if (oldPanel) {
        dockviewApi.removePanel(oldPanel);
      }

      // Open new panel of appropriate type
      if (newType === 'hash-table') {
        openHashTablePanel(sourceExpr, msg.count, msg.entries, sourceExpr);
      }
      // Add other type handlers as needed
    }

    // Unified viz refresh - updates panel content based on current value type
    function handleVizRefresh(msg) {
      const panelId = msg.panelId;
      const sourceExpr = msg.sourceExpr;
      const vizType = msg.vizType;
      const state = vizStates.get(panelId);

      if (!state) {
        console.log('handleVizRefresh: panel not found:', panelId);
        return;
      }

      console.log('handleVizRefresh:', panelId, 'rendering as:', vizType);

      // Update the panel's content based on current type
      // The panel stays the same, only its content changes
      state.type = vizType;  // Track current type

      if (state.element) {
        state.element.innerHTML = '';  // Clear existing content

        switch (vizType) {
          case 'html': {
            const iframe = document.createElement('iframe');
            iframe.style.cssText = 'width:100%;height:100%;border:none;background:white;';
            iframe.sandbox = 'allow-same-origin';
            iframe.srcdoc = msg.content;
            state.element.appendChild(iframe);
            break;
          }
          case 'svg': {
            state.element.innerHTML = msg.content;
            state.element.style.display = 'flex';
            state.element.style.alignItems = 'center';
            state.element.style.justifyContent = 'center';
            const svg = state.element.querySelector('svg');
            if (svg) {
              svg.style.maxWidth = '100%';
              svg.style.maxHeight = '100%';
            }
            break;
          }
          case 'hash-table': {
            renderHashTable(state.element, msg.count, msg.entries);
            break;
          }
          default:
            state.element.innerHTML = '<div style=\"padding:20px;color:var(--fg-secondary);\">Unknown type: ' + vizType + '</div>';
        }
      }
    }

    // Render hash table into element
    function renderHashTable(element, count, entries) {
      let html = '<div style=\"font-family:monospace;font-size:13px;color:var(--fg-primary);padding:10px;overflow:auto;height:100%;\">';
      html += '<div style=\"margin-bottom:8px;color:var(--fg-secondary);\">Hash Table (' + count + ' entries)</div>';
      html += '<table style=\"border-collapse:collapse;width:100%;\">';
      html += '<thead><tr style=\"background:var(--bg-tertiary);\">';
      html += '<th style=\"text-align:left;padding:4px 8px;border:1px solid var(--border);\">Key</th>';
      html += '<th style=\"text-align:left;padding:4px 8px;border:1px solid var(--border);\">Value</th>';
      html += '</tr></thead><tbody>';
      (entries || []).forEach(([key, value]) => {
        const escKey = (key || '').replace(/</g, '&lt;').replace(/>/g, '&gt;');
        const escValue = (value || '').replace(/</g, '&lt;').replace(/>/g, '&gt;');
        html += '<tr style=\"background:var(--bg-secondary);\">';
        html += '<td style=\"padding:4px 8px;border:1px solid var(--border);white-space:pre-wrap;\">' + escKey + '</td>';
        html += '<td style=\"padding:4px 8px;border:1px solid var(--border);white-space:pre-wrap;\">' + escValue + '</td>';
        html += '</tr>';
      });
      if (count > (entries || []).length) {
        html += '<tr style=\"background:var(--bg-secondary);color:var(--fg-secondary);\">';
        html += '<td colspan=\"2\" style=\"padding:4px 8px;border:1px solid var(--border);text-align:center;font-style:italic;\">';
        html += '... and ' + (count - entries.length) + ' more entries</td></tr>';
      }
      html += '</tbody></table></div>';
      element.innerHTML = html;
    }

    // Open Venn diagram panel
    let vennCounter = 0;
    const vennStates = new Map();  // panelId -> VennPanel instance

    function openVennPanel(setNames, setMembers, sourceExpr) {
      console.log('openVennPanel called:', setNames, setMembers?.map(m => m?.length), sourceExpr);
      const panelId = 'venn-' + (++vennCounter);
      const title = setNames.length === 1 ? setNames[0] : 'Venn: ' + setNames.join(' ∩ ');
      if (dockviewApi) {
        dockviewApi.addPanel({
          id: panelId,
          component: 'venn',
          title: title,
          params: { panelId, setNames, setMembers, sourceExpr },
          position: { referencePanel: 'terminal', direction: 'right' }
        });
      } else {
        console.error('dockviewApi not available');
      }
    }

    // Handle Venn refresh data
    function handleVennRefresh(msg) {
      const panelId = msg.panelId;
      const panel = vennStates.get(panelId);
      if (panel) {
        if (msg.error) {
          console.log('Venn refresh error:', msg.error);
        } else {
          panel.updateData(msg.setMembers);
        }
      }
    }

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

      // Apply xterm.js theme if terminal exists, otherwise store for later
      if (themeData.xterm) {
        if (terminal) {
          const xtermTheme = {};
          Object.entries(themeData.xterm).forEach(([k, v]) => {
            xtermTheme[k] = v;
          });
          terminal.options.theme = xtermTheme;
        } else {
          // Terminal not ready yet, store theme for when it's created
          pendingTheme = themeData;
        }
      }

      // Apply dockview theme class
      if (themeData.dockviewTheme && dockviewApi) {
        const container = document.getElementById('layout-container');
        container.className = themeData.dockviewTheme;
      }

      // Re-render all graphviz panels to apply new theme colors
      graphvizStates.forEach((panel) => {
        if (panel._nodes && panel._nodes.size > 0) {
          panel._render();
        }
      });
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
      // (only for REPL clicks, not class graph clicks which should stay in place)
      if (dockviewApi && msg.source !== 'class-graph') {
        const panel = dockviewApi.getPanel('inspector');
        if (panel) panel.api.setActive();
      }
      // Restore focus to terminal after all updates (only for REPL clicks)
      if (msg.source !== 'class-graph') {
        setTimeout(() => { if (terminal) terminal.focus(); }, 100);
      }
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

    class SpeedscopePanel {
      constructor() {
        this._element = document.createElement('div');
        this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;';
      }
      get element() { return this._element; }
      init(params) {
        const profileUrl = params.params?.profileUrl || '';
        const iframeSrc = '/speedscope/index.html#profileURL=' + encodeURIComponent(profileUrl);
        this._element.innerHTML = '<iframe src=\"' + iframeSrc + '\" style=\"width:100%;height:100%;border:none;\"></iframe>';
      }
    }

    class HashTablePanel {
      constructor() {
        this._element = document.createElement('div');
        this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:var(--bg-primary);overflow:auto;padding:8px;';
        this._panelId = null;
        this._sourceExpr = null;
      }
      get element() { return this._element; }
      init(params) {
        this._panelId = params.params?.panelId;
        this._sourceExpr = params.params?.sourceExpr;
        const count = params.params?.count || 0;
        const entries = params.params?.entries || [];

        // Register with unified vizStates for refresh and type-change detection
        if (this._panelId) {
          registerVizPanel(this._panelId, this._sourceExpr, this._element, 'hash-table');
        }

        this._renderTable(count, entries);
      }

      updateData(count, entries) {
        this._renderTable(count, entries);
      }

      _renderTable(count, entries) {
        // Build HTML table
        let html = '<div style=\"font-family:monospace;font-size:13px;color:var(--fg-primary);\">';
        html += '<div style=\"margin-bottom:8px;color:var(--fg-secondary);\">Hash Table (' + count + ' entries)</div>';
        html += '<table style=\"border-collapse:collapse;width:100%;\">';
        html += '<thead><tr style=\"background:var(--bg-tertiary);\">';
        html += '<th style=\"text-align:left;padding:4px 8px;border:1px solid var(--border);\">Key</th>';
        html += '<th style=\"text-align:left;padding:4px 8px;border:1px solid var(--border);\">Value</th>';
        html += '</tr></thead><tbody>';

        (entries || []).forEach(([key, value]) => {
          const escKey = (key || '').replace(/</g, '&lt;').replace(/>/g, '&gt;');
          const escValue = (value || '').replace(/</g, '&lt;').replace(/>/g, '&gt;');
          html += '<tr style=\"background:var(--bg-secondary);\">';
          html += '<td style=\"padding:4px 8px;border:1px solid var(--border);white-space:pre-wrap;\">' + escKey + '</td>';
          html += '<td style=\"padding:4px 8px;border:1px solid var(--border);white-space:pre-wrap;\">' + escValue + '</td>';
          html += '</tr>';
        });

        if (count > (entries || []).length) {
          html += '<tr style=\"background:var(--bg-secondary);color:var(--fg-secondary);\">';
          html += '<td colspan=\"2\" style=\"padding:4px 8px;border:1px solid var(--border);text-align:center;font-style:italic;\">';
          html += '... and ' + (count - entries.length) + ' more entries</td></tr>';
        }

        html += '</tbody></table></div>';
        this._element.innerHTML = html;
      }
    }

    // SVG Panel - renders SVG content directly
    class SvgPanel {
      constructor() {
        this._element = document.createElement('div');
        this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:var(--bg-primary);overflow:auto;display:flex;align-items:center;justify-content:center;';
        this._panelId = null;
        this._sourceExpr = null;
      }
      get element() { return this._element; }
      init(params) {
        // Use panelId from params (set by openSvgPanel) or fall back to dockview id
        this._panelId = params.params?.panelId || params.id;
        this._sourceExpr = params.params?.sourceExpr;
        const content = params.params?.content || '';
        console.log('SvgPanel.init panelId:', this._panelId, 'sourceExpr:', this._sourceExpr);
        this._element.innerHTML = content;
        // Scale SVG to fit if needed
        const svg = this._element.querySelector('svg');
        if (svg) {
          svg.style.maxWidth = '100%';
          svg.style.maxHeight = '100%';
          svg.style.width = 'auto';
          svg.style.height = 'auto';
        }
        // Register with unified vizStates
        if (this._panelId) {
          registerVizPanel(this._panelId, this._sourceExpr, this._element, 'svg');
          console.log('SvgPanel registered:', this._panelId);
        }
      }
    }

    // HTML Panel - renders HTML content in a sandboxed iframe
    class HtmlPanel {
      constructor() {
        this._element = document.createElement('div');
        this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:var(--bg-primary);';
        this._panelId = null;
        this._sourceExpr = null;
        this._iframe = null;
      }
      get element() { return this._element; }
      init(params) {
        // Use panelId from params (set by openHtmlPanel) or fall back to dockview id
        this._panelId = params.params?.panelId || params.id;
        this._sourceExpr = params.params?.sourceExpr;
        const content = params.params?.content || '';
        console.log('HtmlPanel.init panelId:', this._panelId, 'sourceExpr:', this._sourceExpr);
        // Use srcdoc for sandboxed HTML rendering
        this._iframe = document.createElement('iframe');
        this._iframe.style.cssText = 'width:100%;height:100%;border:none;background:white;';
        this._iframe.sandbox = 'allow-same-origin';  // Minimal permissions
        this._iframe.srcdoc = content;
        this._element.appendChild(this._iframe);
        // Register with unified vizStates
        if (this._panelId) {
          registerVizPanel(this._panelId, this._sourceExpr, this._element, 'html');
          console.log('HtmlPanel registered:', this._panelId);
        }
      }
    }

    class VennPanel {
      constructor() {
        this._element = document.createElement('div');
        this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:var(--bg-primary);overflow:auto;';
        this._panelId = null;
        this._sourceExpr = null;
        this._setNames = [];
        this._setMembers = [];
      }
      get element() { return this._element; }

      init(params) {
        this._panelId = params.params?.panelId;
        this._sourceExpr = params.params?.sourceExpr;
        this._setNames = params.params?.setNames || [];
        this._setMembers = params.params?.setMembers || [];

        // Register for refresh updates
        if (this._panelId) {
          vennStates.set(this._panelId, this);
        }

        this._render();

        // Re-render on resize (debounced)
        let resizeTimeout = null;
        const resizeObserver = new ResizeObserver(() => {
          if (resizeTimeout) clearTimeout(resizeTimeout);
          resizeTimeout = setTimeout(() => this._render(), 100);
        });
        resizeObserver.observe(this._element);
      }

      updateData(setMembers) {
        this._setMembers = setMembers || [];
        this._render();
      }

      _render() {
        const width = this._element.clientWidth || 400;
        const height = this._element.clientHeight || 300;
        const cx = width / 2;
        const cy = height / 2;

        if (this._setMembers.length === 1) {
          this._renderSingleSet(width, height, cx, cy);
        } else if (this._setMembers.length === 2) {
          this._renderTwoSets(width, height, cx, cy);
        } else if (this._setMembers.length === 3) {
          this._renderThreeSets(width, height, cx, cy);
        } else {
          this._element.innerHTML = '<div style=\"padding:20px;color:var(--fg-secondary);\">Venn diagrams support 1-3 sets</div>';
        }
      }

      _renderSingleSet(width, height, cx, cy) {
        const r = Math.min(width, height) * 0.35;
        const members = this._setMembers[0] || [];
        const name = this._setNames[0] || 'Set';

        let svg = '<svg width=\"' + width + '\" height=\"' + height + '\" style=\"display:block;\">';
        svg += '<circle cx=\"' + cx + '\" cy=\"' + cy + '\" r=\"' + r + '\" fill=\"var(--accent)\" fill-opacity=\"0.3\" stroke=\"var(--accent)\" stroke-width=\"2\"/>';
        svg += '<text x=\"' + cx + '\" y=\"' + (cy - r - 10) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"14\" font-weight=\"bold\">' + this._escapeHtml(name) + '</text>';
        svg += '<text x=\"' + cx + '\" y=\"' + (cy - r + 20) + '\" text-anchor=\"middle\" fill=\"var(--fg-secondary)\" font-size=\"12\">(' + members.length + ' members)</text>';

        // List members inside circle
        const maxShow = 15;
        const lineHeight = 16;
        const startY = cy - Math.min(members.length, maxShow) * lineHeight / 2;
        members.slice(0, maxShow).forEach((m, i) => {
          svg += '<text x=\"' + cx + '\" y=\"' + (startY + i * lineHeight) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"11\" font-family=\"monospace\">' + this._escapeHtml(m) + '</text>';
        });
        if (members.length > maxShow) {
          svg += '<text x=\"' + cx + '\" y=\"' + (startY + maxShow * lineHeight) + '\" text-anchor=\"middle\" fill=\"var(--fg-secondary)\" font-size=\"11\" font-style=\"italic\">... and ' + (members.length - maxShow) + ' more</text>';
        }

        svg += '</svg>';
        this._element.innerHTML = svg;
      }

      _renderTwoSets(width, height, cx, cy) {
        const r = Math.min(width, height) * 0.3;
        const offset = r * 0.6;  // Circle centers offset from center
        const setA = new Set(this._setMembers[0] || []);
        const setB = new Set(this._setMembers[1] || []);
        const nameA = this._setNames[0] || 'Set A';
        const nameB = this._setNames[1] || 'Set B';

        // Compute regions
        const onlyA = [...setA].filter(x => !setB.has(x));
        const onlyB = [...setB].filter(x => !setA.has(x));
        const intersection = [...setA].filter(x => setB.has(x));

        const cxA = cx - offset;
        const cxB = cx + offset;

        let svg = '<svg width=\"' + width + '\" height=\"' + height + '\" style=\"display:block;\">';

        // Circle A
        svg += '<circle cx=\"' + cxA + '\" cy=\"' + cy + '\" r=\"' + r + '\" fill=\"#89b4fa\" fill-opacity=\"0.35\" stroke=\"#89b4fa\" stroke-width=\"2\"/>';
        // Circle B
        svg += '<circle cx=\"' + cxB + '\" cy=\"' + cy + '\" r=\"' + r + '\" fill=\"#f38ba8\" fill-opacity=\"0.35\" stroke=\"#f38ba8\" stroke-width=\"2\"/>';

        // Labels
        svg += '<text x=\"' + cxA + '\" y=\"' + (cy - r - 10) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"13\" font-weight=\"bold\">' + this._escapeHtml(nameA) + '</text>';
        svg += '<text x=\"' + cxB + '\" y=\"' + (cy - r - 10) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"13\" font-weight=\"bold\">' + this._escapeHtml(nameB) + '</text>';

        // Member counts
        svg += '<text x=\"' + (cxA - r * 0.5) + '\" y=\"' + (cy - r * 0.6) + '\" text-anchor=\"middle\" fill=\"var(--fg-secondary)\" font-size=\"11\">' + onlyA.length + ' only</text>';
        svg += '<text x=\"' + cx + '\" y=\"' + (cy - r * 0.6) + '\" text-anchor=\"middle\" fill=\"var(--fg-secondary)\" font-size=\"11\">' + intersection.length + ' shared</text>';
        svg += '<text x=\"' + (cxB + r * 0.5) + '\" y=\"' + (cy - r * 0.6) + '\" text-anchor=\"middle\" fill=\"var(--fg-secondary)\" font-size=\"11\">' + onlyB.length + ' only</text>';

        // Render members in each region
        const maxPerRegion = 8;
        const lineHeight = 14;

        // Only A (left region)
        const leftX = cxA - r * 0.5;
        const startYA = cy - Math.min(onlyA.length, maxPerRegion) * lineHeight / 2;
        onlyA.slice(0, maxPerRegion).forEach((m, i) => {
          svg += '<text x=\"' + leftX + '\" y=\"' + (startYA + i * lineHeight) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"10\" font-family=\"monospace\">' + this._escapeHtml(this._truncate(m, 12)) + '</text>';
        });
        if (onlyA.length > maxPerRegion) {
          svg += '<text x=\"' + leftX + '\" y=\"' + (startYA + maxPerRegion * lineHeight) + '\" text-anchor=\"middle\" fill=\"var(--fg-secondary)\" font-size=\"9\">+' + (onlyA.length - maxPerRegion) + '</text>';
        }

        // Intersection (center)
        const startYI = cy - Math.min(intersection.length, maxPerRegion) * lineHeight / 2;
        intersection.slice(0, maxPerRegion).forEach((m, i) => {
          svg += '<text x=\"' + cx + '\" y=\"' + (startYI + i * lineHeight) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"10\" font-family=\"monospace\" font-weight=\"bold\">' + this._escapeHtml(this._truncate(m, 12)) + '</text>';
        });
        if (intersection.length > maxPerRegion) {
          svg += '<text x=\"' + cx + '\" y=\"' + (startYI + maxPerRegion * lineHeight) + '\" text-anchor=\"middle\" fill=\"var(--fg-secondary)\" font-size=\"9\">+' + (intersection.length - maxPerRegion) + '</text>';
        }

        // Only B (right region)
        const rightX = cxB + r * 0.5;
        const startYB = cy - Math.min(onlyB.length, maxPerRegion) * lineHeight / 2;
        onlyB.slice(0, maxPerRegion).forEach((m, i) => {
          svg += '<text x=\"' + rightX + '\" y=\"' + (startYB + i * lineHeight) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"10\" font-family=\"monospace\">' + this._escapeHtml(this._truncate(m, 12)) + '</text>';
        });
        if (onlyB.length > maxPerRegion) {
          svg += '<text x=\"' + rightX + '\" y=\"' + (startYB + maxPerRegion * lineHeight) + '\" text-anchor=\"middle\" fill=\"var(--fg-secondary)\" font-size=\"9\">+' + (onlyB.length - maxPerRegion) + '</text>';
        }

        svg += '</svg>';
        this._element.innerHTML = svg;
      }

      _renderThreeSets(width, height, cx, cy) {
        const r = Math.min(width, height) * 0.25;
        const offset = r * 0.7;
        const setA = new Set(this._setMembers[0] || []);
        const setB = new Set(this._setMembers[1] || []);
        const setC = new Set(this._setMembers[2] || []);
        const nameA = this._setNames[0] || 'A';
        const nameB = this._setNames[1] || 'B';
        const nameC = this._setNames[2] || 'C';

        // Circle positions (triangle arrangement)
        const cxA = cx;
        const cyA = cy - offset * 0.8;
        const cxB = cx - offset;
        const cyB = cy + offset * 0.5;
        const cxC = cx + offset;
        const cyC = cy + offset * 0.5;

        // Compute regions
        const onlyA = [...setA].filter(x => !setB.has(x) && !setC.has(x)).length;
        const onlyB = [...setB].filter(x => !setA.has(x) && !setC.has(x)).length;
        const onlyC = [...setC].filter(x => !setA.has(x) && !setB.has(x)).length;
        const abOnly = [...setA].filter(x => setB.has(x) && !setC.has(x)).length;
        const acOnly = [...setA].filter(x => setC.has(x) && !setB.has(x)).length;
        const bcOnly = [...setB].filter(x => setC.has(x) && !setA.has(x)).length;
        const abc = [...setA].filter(x => setB.has(x) && setC.has(x)).length;

        let svg = '<svg width=\"' + width + '\" height=\"' + height + '\" style=\"display:block;\">';

        // Circles
        svg += '<circle cx=\"' + cxA + '\" cy=\"' + cyA + '\" r=\"' + r + '\" fill=\"#89b4fa\" fill-opacity=\"0.3\" stroke=\"#89b4fa\" stroke-width=\"2\"/>';
        svg += '<circle cx=\"' + cxB + '\" cy=\"' + cyB + '\" r=\"' + r + '\" fill=\"#f38ba8\" fill-opacity=\"0.3\" stroke=\"#f38ba8\" stroke-width=\"2\"/>';
        svg += '<circle cx=\"' + cxC + '\" cy=\"' + cyC + '\" r=\"' + r + '\" fill=\"#a6e3a1\" fill-opacity=\"0.3\" stroke=\"#a6e3a1\" stroke-width=\"2\"/>';

        // Labels
        svg += '<text x=\"' + cxA + '\" y=\"' + (cyA - r - 8) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"12\" font-weight=\"bold\">' + this._escapeHtml(nameA) + '</text>';
        svg += '<text x=\"' + (cxB - r * 0.7) + '\" y=\"' + (cyB + r * 0.3) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"12\" font-weight=\"bold\">' + this._escapeHtml(nameB) + '</text>';
        svg += '<text x=\"' + (cxC + r * 0.7) + '\" y=\"' + (cyC + r * 0.3) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"12\" font-weight=\"bold\">' + this._escapeHtml(nameC) + '</text>';

        // Counts in regions (approximate positions)
        svg += '<text x=\"' + cxA + '\" y=\"' + (cyA - r * 0.3) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"11\">' + onlyA + '</text>';
        svg += '<text x=\"' + (cxB - r * 0.4) + '\" y=\"' + (cyB + r * 0.2) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"11\">' + onlyB + '</text>';
        svg += '<text x=\"' + (cxC + r * 0.4) + '\" y=\"' + (cyC + r * 0.2) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"11\">' + onlyC + '</text>';
        svg += '<text x=\"' + (cx - offset * 0.35) + '\" y=\"' + (cy - offset * 0.15) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"10\">' + abOnly + '</text>';
        svg += '<text x=\"' + (cx + offset * 0.35) + '\" y=\"' + (cy - offset * 0.15) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"10\">' + acOnly + '</text>';
        svg += '<text x=\"' + cx + '\" y=\"' + (cy + offset * 0.5) + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"10\">' + bcOnly + '</text>';
        svg += '<text x=\"' + cx + '\" y=\"' + cy + '\" text-anchor=\"middle\" fill=\"var(--fg-primary)\" font-size=\"12\" font-weight=\"bold\">' + abc + '</text>';

        svg += '</svg>';
        this._element.innerHTML = svg;
      }

      _escapeHtml(str) {
        return (str || '').replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
      }

      _truncate(str, len) {
        return str.length > len ? str.slice(0, len - 1) + '…' : str;
      }
    }

    class GraphvizPanel {
      constructor() {
        console.log('GraphvizPanel constructor');
        this._element = document.createElement('div');
        this._element.style.cssText = 'position:absolute;top:0;left:0;right:0;bottom:0;background:var(--bg-primary);overflow:auto;user-select:none;-webkit-user-select:none;';
        this._element.tabIndex = 0;  // Make focusable for keyboard events
        this._panelId = null;
        this._className = null;
        this._packageName = null;
        this._nodes = new Map();  // name -> {pkg, slots, expanded}
        this._edges = new Set();  // 'from->to'
        this._rootClass = null;
        this._focusNode = null;  // Node to center on after render
        this._viz = null;
        this._zoom = 1.0;
        this._baseWidth = 0;
        this._baseHeight = 0;
        this._svg = null;

        // Create tooltip element for showing slots on hover
        this._tooltip = document.createElement('div');
        this._tooltip.style.cssText = 'position:fixed;display:none;padding:8px 12px;background:var(--bg-secondary);color:var(--fg-primary);border:1px solid var(--border);border-radius:4px;font-size:12px;font-family:monospace;max-width:300px;z-index:1000;pointer-events:none;opacity:0.95;box-shadow:0 2px 8px rgba(0,0,0,0.3);';
        document.body.appendChild(this._tooltip);

        // Create child selector popup
        this._childSelector = document.createElement('div');
        this._childSelector.style.cssText = 'position:fixed;display:none;padding:4px 0;background:var(--bg-secondary);color:var(--fg-primary);border:1px solid var(--border);border-radius:4px;font-size:12px;font-family:monospace;max-height:300px;overflow-y:auto;z-index:1001;box-shadow:0 4px 12px rgba(0,0,0,0.4);min-width:150px;';
        document.body.appendChild(this._childSelector);
        this._pendingParent = null;  // Parent node waiting for child selection

        // Keyboard zoom handlers
        this._element.addEventListener('keydown', (e) => {
          if (e.key === '+' || e.key === '=') {
            e.preventDefault();
            this._setZoom(this._zoom * 1.2);
          } else if (e.key === '-' || e.key === '_') {
            e.preventDefault();
            this._setZoom(this._zoom / 1.2);
          } else if (e.key === '0') {
            e.preventDefault();
            this._setZoom(1.0);  // Reset zoom
          }
        });

        // Mouse wheel zoom (with Ctrl)
        this._element.addEventListener('wheel', (e) => {
          if (e.ctrlKey) {
            e.preventDefault();
            const delta = e.deltaY > 0 ? 0.9 : 1.1;
            this._setZoom(this._zoom * delta);
          }
        }, { passive: false });

        // Click-and-drag panning
        this._isPanning = false;
        this._panStartX = 0;
        this._panStartY = 0;
        this._scrollStartX = 0;
        this._scrollStartY = 0;
        this._didPan = false;  // Track if we actually panned (vs clicked)

        this._element.addEventListener('mousedown', (e) => {
          if (e.button !== 0) return;
          this._isPanning = true;
          this._didPan = false;
          this._panStartX = e.clientX;
          this._panStartY = e.clientY;
          this._scrollStartX = this._element.scrollLeft;
          this._scrollStartY = this._element.scrollTop;
        });

        this._element.addEventListener('mousemove', (e) => {
          if (!this._isPanning) return;
          const dx = e.clientX - this._panStartX;
          const dy = e.clientY - this._panStartY;
          // Only start panning after moving 5+ pixels (prevents accidental drags)
          if (Math.abs(dx) > 5 || Math.abs(dy) > 5) {
            this._didPan = true;
            this._element.style.cursor = 'grabbing';
          }
          if (this._didPan) {
            this._element.scrollLeft = this._scrollStartX - dx;
            this._element.scrollTop = this._scrollStartY - dy;
          }
        });

        this._element.addEventListener('mouseup', () => {
          this._isPanning = false;
          this._element.style.cursor = 'grab';
        });

        this._element.addEventListener('mouseleave', () => {
          this._isPanning = false;
          this._element.style.cursor = 'grab';
        });

        // Set initial cursor
        this._element.style.cursor = 'grab';
      }
      get element() { return this._element; }

      _setZoom(newZoom) {
        // Clamp zoom between 0.1 and 5.0
        this._zoom = Math.max(0.1, Math.min(5.0, newZoom));
        this._applyZoom();
      }

      _applyZoom() {
        if (!this._svg || !this._baseWidth) return;
        const w = Math.round(this._baseWidth * this._zoom);
        const h = Math.round(this._baseHeight * this._zoom);
        this._svg.style.width = w + 'px';
        this._svg.style.height = h + 'px';
      }

      async init(params) {
        console.log('GraphvizPanel init:', params);
        this._panelId = params.params?.panelId;
        this._className = params.params?.className;
        this._packageName = params.params?.packageName;

        // Register for data updates
        graphvizStates.set(this._panelId, this);

        // Initialize viz.js
        this._viz = await Viz.instance();

        // Request data if we have class info
        if (this._className && this._packageName) {
          ws.send(JSON.stringify({
            type: 'get-class-graph',
            className: this._className,
            packageName: this._packageName,
            panelId: this._panelId
          }));
        }

        // Handle pending data
        const pending = pendingClassGraphs.get(this._panelId);
        if (pending && pending.length) {
          pendingClassGraphs.delete(this._panelId);
          pending.forEach((msg) => {
            if (msg.type === 'class-graph-expand') this.addGraph(msg);
            else this.updateGraph(msg);
          });
        }

        // Re-render on resize (debounced)
        let resizeTimeout = null;
        const resizeObserver = new ResizeObserver(() => {
          if (resizeTimeout) clearTimeout(resizeTimeout);
          resizeTimeout = setTimeout(() => {
            if (this._nodes.size > 0) {
              this._render();
            }
          }, 150);
        });
        resizeObserver.observe(this._element);
      }

      _escapeLabel(str) {
        // Escape for DOT format
        return str.replace(/\\\\/g, '\\\\\\\\').replace(/\"/g, '\\\\\"').replace(/</g, '&lt;').replace(/>/g, '&gt;');
      }

      _getThemeColors() {
        // Read current theme colors from CSS variables
        const style = getComputedStyle(document.body);
        return {
          bg: style.getPropertyValue('--bg-primary').trim() || '#1e1e2e',
          nodeFill: style.getPropertyValue('--bg-tertiary').trim() || '#45475a',
          rootFill: style.getPropertyValue('--accent').trim() || '#89b4fa',
          text: style.getPropertyValue('--fg-primary').trim() || '#cdd6f4',
          rootText: '#ffffff',  // White text on accent background for readability
          edge: style.getPropertyValue('--fg-muted').trim() || '#6c7086',
          border: style.getPropertyValue('--border').trim() || '#45475a'
        };
      }

      _getAncestors(nodeName) {
        // Find all ancestors of a node by traversing edges upward
        // Edges are stored as 'superclass->subclass'
        const ancestors = new Set();
        const toVisit = [nodeName];
        while (toVisit.length > 0) {
          const current = toVisit.pop();
          for (const edge of this._edges) {
            const [parent, child] = edge.split('->');
            if (child === current && !ancestors.has(parent)) {
              ancestors.add(parent);
              toVisit.push(parent);
            }
          }
        }
        return ancestors;
      }

      _highlightAncestors(nodeName, highlight) {
        // Add or remove highlight from ancestor nodes AND the hovered node itself
        const ancestors = this._getAncestors(nodeName);
        ancestors.add(nodeName);  // Include the hovered node
        const svg = this._svg;
        if (!svg) return;

        svg.querySelectorAll('g.node').forEach(g => {
          const name = g.dataset.nodeName;
          if (ancestors.has(name)) {
            const polygon = g.querySelector('polygon') || g.querySelector('path');
            if (polygon) {
              if (highlight) {
                polygon.setAttribute('stroke', 'var(--accent)');
                polygon.setAttribute('stroke-width', '2.5');
              } else {
                // Restore original
                const colors = this._getThemeColors();
                polygon.setAttribute('stroke', colors.border);
                polygon.setAttribute('stroke-width', '1');
              }
            }
          }
        });
      }

      _generateDot(direction = 'TB') {
        const colors = this._getThemeColors();

        let dot = 'digraph G {\\n';
        dot += '  rankdir=' + direction + ';\\n';  // TB=top-to-bottom, LR=left-to-right
        dot += '  newrank=true;\\n';  // Use newer ranking algorithm for consistent hierarchy
        dot += '  splines=ortho;\\n';  // Orthogonal edges for cleaner look
        dot += '  ranksep=0.5;\\n';  // Vertical spacing between ranks
        dot += '  nodesep=0.3;\\n';  // Horizontal spacing between nodes
        dot += '  bgcolor=\"' + colors.bg + '\";\\n';
        dot += '  node [shape=box, style=\"filled,rounded\", fontname=\"Helvetica\", fontsize=10, fontcolor=\"' + colors.text + '\", color=\"' + colors.border + '\", margin=\"0.15,0.08\"];\\n';
        dot += '  edge [arrowhead=empty, color=\"' + colors.edge + '\"];\\n';
        dot += '\\n';

        // Add nodes (simple labels - slots shown via hover tooltip)
        for (const [name, info] of this._nodes) {
          const escName = this._escapeLabel(name);
          const isRoot = name === this._rootClass;
          const fillColor = isRoot ? colors.rootFill : colors.nodeFill;
          const textColor = isRoot ? colors.rootText : colors.text;
          dot += '  \"' + name + '\" [label=\"' + escName + '\", fillcolor=\"' + fillColor + '\", fontcolor=\"' + textColor + '\"];\\n';
        }

        dot += '\\n';

        // Add edges - stored as 'superclass->subclass', output same way
        for (const edge of this._edges) {
          const [from, to] = edge.split('->');
          dot += '  \"' + from + '\" -> \"' + to + '\";\\n';
        }

        dot += '}\\n';
        return dot;
      }

      _render() {
        if (!this._viz || this._nodes.size === 0) return;

        // Get container dimensions (with fallbacks)
        const containerWidth = this._element.clientWidth || 400;
        const containerHeight = this._element.clientHeight || 300;

        // Helper to parse SVG dimensions
        const parseSvgDimensions = (svg) => {
          let w = 300, h = 200;
          const widthAttr = svg.getAttribute('width');
          const heightAttr = svg.getAttribute('height');
          if (widthAttr) {
            w = parseFloat(widthAttr);
            if (widthAttr.endsWith('pt')) w *= 1.33;
          }
          if (heightAttr) {
            h = parseFloat(heightAttr);
            if (heightAttr.endsWith('pt')) h *= 1.33;
          }
          return { width: w, height: h };
        };

        try {
          // Try TB (top-to-bottom) first
          let dot = this._generateDot('TB');
          let svg = this._viz.renderSVGElement(dot);
          let dims = parseSvgDimensions(svg);

          // If graph is too wide (more than 2x container width), switch to LR (left-to-right)
          if (dims.width > containerWidth * 2) {
            console.log('Graph too wide (' + dims.width + 'px), switching to LR layout');
            dot = this._generateDot('LR');
            svg = this._viz.renderSVGElement(dot);
            dims = parseSvgDimensions(svg);
          }

          const svgWidth = dims.width;
          const svgHeight = dims.height;

          // Don't shrink huge graphs to unreadable sizes - just show part and let user pan
          // Use scale 1.0 (natural size) for most graphs, only shrink if reasonably small
          const fitScale = Math.min(containerWidth / svgWidth, containerHeight / svgHeight);
          const initialScale = fitScale > 0.3 ? fitScale : 1.0;  // If would shrink below 30%, just use natural size

          // Store base dimensions for zoom calculations
          this._baseWidth = Math.round(svgWidth * initialScale);
          this._baseHeight = Math.round(svgHeight * initialScale);
          this._zoom = 1.0;  // Reset zoom on new render
          this._svg = svg;

          // Apply dimensions
          svg.removeAttribute('width');
          svg.removeAttribute('height');
          svg.style.width = this._baseWidth + 'px';
          svg.style.height = this._baseHeight + 'px';
          svg.style.display = 'block';

          // Center small graphs that fit within the container
          if (this._baseWidth < containerWidth) {
            svg.style.marginLeft = Math.round((containerWidth - this._baseWidth) / 2) + 'px';
          }
          if (this._baseHeight < containerHeight) {
            svg.style.marginTop = Math.round((containerHeight - this._baseHeight) / 2) + 'px';
          }

          // Process nodes: extract names, then remove ALL title elements to disable native tooltips
          svg.querySelectorAll('g.node').forEach(g => {
            const title = g.querySelector('title');
            if (title) {
              g.dataset.nodeName = title.textContent;
            }
          });
          svg.querySelectorAll('title').forEach(t => t.remove());

          // Clear and add SVG
          this._element.innerHTML = '';
          this._element.appendChild(svg);
          this._element.focus();  // Focus for keyboard events

          // Add click and hover handlers to nodes
          svg.querySelectorAll('g.node').forEach(g => {
            g.style.cursor = 'pointer';
            const name = g.dataset.nodeName;
            if (name) {
              const info = this._nodes.get(name);

              // Click handler
              g.addEventListener('click', (e) => {
                // Ignore click if we just finished panning
                if (this._didPan) {
                  this._didPan = false;
                  return;
                }
                this._onNodeClick(name, e.clientX, e.clientY);
              });

              // Hover handlers for slot tooltip and ancestor highlighting
              g.addEventListener('mouseenter', (e) => {
                if (!info) {
                  console.log('No info for node:', name, 'available:', Array.from(this._nodes.keys()));
                  return;
                }
                // Highlight ancestor path
                this._highlightAncestors(name, true);

                // Show tooltip
                let content = '<strong>' + name + '</strong>';
                if (info.pkg) content += '<br><span style=\"color:var(--fg-muted);\">' + info.pkg + '</span>';
                if (info.slots && info.slots.length > 0) {
                  const slotList = info.slots.slice(0, 15).join('\\n');
                  const more = info.slots.length > 15 ? '\\n...' + (info.slots.length - 15) + ' more' : '';
                  content += '<br><br><em>Direct slots:</em><pre style=\"margin:4px 0 0 0;white-space:pre-wrap;\">' + slotList + more + '</pre>';
                } else {
                  content += '<br><span style=\"color:var(--fg-muted);font-style:italic;\">No direct slots</span>';
                }
                this._tooltip.innerHTML = content;
                this._tooltip.style.display = 'block';
                this._tooltip.style.left = (e.clientX + 10) + 'px';
                this._tooltip.style.top = (e.clientY + 10) + 'px';
              });

              g.addEventListener('mousemove', (e) => {
                if (this._tooltip.style.display === 'block') {
                  this._tooltip.style.left = (e.clientX + 10) + 'px';
                  this._tooltip.style.top = (e.clientY + 10) + 'px';
                }
              });

              g.addEventListener('mouseleave', () => {
                this._tooltip.style.display = 'none';
                // Remove ancestor highlighting
                this._highlightAncestors(name, false);
              });
            }
          });

          // Scroll to center on focus node (clicked node, or root on initial load)
          const targetNode = this._focusNode || this._rootClass;
          if (targetNode) {
            const nodes = svg.querySelectorAll('g.node');
            for (const g of nodes) {
              if (g.dataset.nodeName === targetNode) {
                const bbox = g.getBoundingClientRect();
                const containerRect = this._element.getBoundingClientRect();
                // Scroll to center the node horizontally and vertically
                const scrollX = (bbox.left - containerRect.left) - (containerWidth / 2) + (bbox.width / 2);
                const scrollY = (bbox.top - containerRect.top) - (containerHeight / 2) + (bbox.height / 2);
                this._element.scrollLeft = Math.max(0, scrollX);
                this._element.scrollTop = Math.max(0, scrollY);
                break;
              }
            }
          }
        } catch (e) {
          console.error('Graphviz render error:', e);
          this._element.innerHTML = '<div style=\"padding:20px;color:#f38ba8;\">Render error: ' + e.message + '</div>';
        }
      }

      _onNodeClick(name, mouseX, mouseY) {
        const info = this._nodes.get(name);
        if (!info) return;

        // Send symbol click for info panel (with source to prevent tab switching)
        ws.send(JSON.stringify({type: 'symbol-click', symbol: info.pkg + '::' + name, source: 'class-graph'}));

        // Request children list for selector popup (if not already expanded)
        if (!info.expanded) {
          this._pendingParent = { name, pkg: info.pkg, x: mouseX, y: mouseY };
          ws.send(JSON.stringify({
            type: 'list-class-children',
            className: name,
            packageName: info.pkg,
            panelId: this._panelId
          }));
        }
      }

      showChildSelector(msg) {
        const children = msg.children || [];
        const parentName = msg['parent-name'] || msg.parentName;
        const parentPkg = msg['parent-package'] || msg.parentPackage;

        // Hide selector if no children
        if (children.length === 0) {
          this._childSelector.style.display = 'none';
          // Mark as expanded (no children to show)
          const info = this._nodes.get(parentName);
          if (info) info.expanded = true;
          return;
        }

        // Position near the click
        const pos = this._pendingParent || { x: 100, y: 100 };
        this._childSelector.style.left = pos.x + 'px';
        this._childSelector.style.top = pos.y + 'px';

        // Build list of children
        this._childSelector.innerHTML = '';
        children.forEach(([childName, childPkg]) => {
          // Skip if already in graph
          if (this._nodes.has(childName)) return;

          const item = document.createElement('div');
          item.style.cssText = 'padding:6px 12px;cursor:pointer;white-space:nowrap;';
          item.textContent = childName;
          item.addEventListener('mouseenter', () => {
            item.style.background = 'var(--accent)';
            item.style.color = '#ffffff';
          });
          item.addEventListener('mouseleave', () => {
            item.style.background = '';
            item.style.color = '';
          });
          item.addEventListener('click', (e) => {
            e.stopPropagation();
            this._childSelector.style.display = 'none';
            // Add just this child
            this._focusNode = childName;
            ws.send(JSON.stringify({
              type: 'add-class-child',
              parentName: parentName,
              childName: childName,
              childPackage: childPkg,
              panelId: this._panelId
            }));
          });
          this._childSelector.appendChild(item);
        });

        // If all children already in graph, mark as expanded
        if (this._childSelector.children.length === 0) {
          const info = this._nodes.get(parentName);
          if (info) info.expanded = true;
          return;
        }

        this._childSelector.style.display = 'block';

        // Close on click outside
        const closeHandler = (e) => {
          if (!this._childSelector.contains(e.target)) {
            this._childSelector.style.display = 'none';
            document.removeEventListener('click', closeHandler);
          }
        };
        setTimeout(() => document.addEventListener('click', closeHandler), 0);
      }

      updateGraph(msg) {
        console.log('updateGraph called:', msg);
        if (!this._viz) {
          const pending = pendingClassGraphs.get(this._panelId) || [];
          pending.push(msg);
          pendingClassGraphs.set(this._panelId, pending);
          return;
        }
        if (msg.error) {
          this._element.innerHTML = '<div style=\"padding:20px;color:#a6adc8;\">Error: ' + msg.error + '</div>';
          return;
        }

        this._rootClass = msg['class-name'] || msg.className;
        this._focusNode = this._rootClass;  // Center on root initially
        this._nodes.clear();
        this._edges.clear();

        // Add nodes - format: [name, pkg, [slot1, slot2, ...]]
        (msg.nodes || []).forEach((nodeData) => {
          const name = nodeData[0];
          const pkg = nodeData[1];
          const slots = Array.isArray(nodeData[2]) ? nodeData[2] : [];
          this._nodes.set(name, { pkg, slots, expanded: false });
        });

        // Add edges (from superclass to subclass in msg, stored as 'from->to')
        (msg.edges || []).forEach(([from, to]) => {
          if (this._nodes.has(from) && this._nodes.has(to)) {
            this._edges.add(from + '->' + to);
          }
        });

        this._render();
      }

      addGraph(msg) {
        console.log('addGraph called:', msg);
        if (!this._viz) {
          const pending = pendingClassGraphs.get(this._panelId) || [];
          pending.push(msg);
          pendingClassGraphs.set(this._panelId, pending);
          return;
        }
        if (msg.error) {
          console.warn('class graph expand error:', msg.error);
          return;
        }

        let changed = false;

        (msg.nodes || []).forEach(([name, pkg, slots]) => {
          if (!this._nodes.has(name)) {
            this._nodes.set(name, { pkg, slots: slots || [], expanded: false });
            changed = true;
          }
        });

        (msg.edges || []).forEach(([from, to]) => {
          const edgeKey = from + '->' + to;
          if (!this._edges.has(edgeKey) && this._nodes.has(from) && this._nodes.has(to)) {
            this._edges.add(edgeKey);
            changed = true;
          }
        });

        if (changed) {
          this._render();
        }
      }

      dispose() {
        if (this._panelId) graphvizStates.delete(this._panelId);
        if (this._tooltip && this._tooltip.parentNode) {
          this._tooltip.parentNode.removeChild(this._tooltip);
        }
        if (this._childSelector && this._childSelector.parentNode) {
          this._childSelector.parentNode.removeChild(this._childSelector);
        }
      }
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
          // Build initial theme - use pending theme if available, otherwise default dark
          let initialTheme = { background: '#1e1e1e' };
          if (pendingTheme && pendingTheme.xterm) {
            initialTheme = {};
            Object.entries(pendingTheme.xterm).forEach(([k, v]) => {
              initialTheme[k] = v;
            });
            pendingTheme = null;
          }
          terminal = new Terminal({
            cursorBlink: true,
            fontFamily: \"'JetBrains Mono', monospace\",
            fontSize: 14,
            theme: initialTheme
          });
          fitAddon = new FitAddon.FitAddon();
          terminal.loadAddon(fitAddon);
          terminal.open(this._element);

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
          case 'speedscope': return new SpeedscopePanel();
          case 'hashtable': return new HashTablePanel();
          case 'svg': return new SvgPanel();
          case 'html': return new HtmlPanel();
          case 'venn': return new VennPanel();
          case 'graphviz': return new GraphvizPanel();
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

    // Double-click on tab to maximize/restore panel
    document.addEventListener('dblclick', (e) => {
      const tab = e.target.closest('.dv-tab');
      if (tab) {
        const activePanel = api.activePanel;
        if (activePanel) {
          if (activePanel.api.isMaximized()) {
            activePanel.api.exitMaximized();
          } else {
            activePanel.api.maximize();
          }
        }
      }
    });

    // Keyboard shortcut: F11 or Escape to maximize/restore active panel
    document.addEventListener('keydown', (e) => {
      // F11 to toggle maximize (prevent default fullscreen)
      if (e.key === 'F11') {
        e.preventDefault();
        const activePanel = api.activePanel;
        if (activePanel) {
          if (activePanel.api.isMaximized()) {
            activePanel.api.exitMaximized();
          } else {
            activePanel.api.maximize();
          }
        }
      }
      // Escape to exit maximized state
      if (e.key === 'Escape' && api.hasMaximizedGroup()) {
        api.exitMaximizedGroup();
      }
    });
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
      ;; Serve speedscope files
      ((and (> (length path) 12)
            (string= (subseq path 0 12) "/speedscope/"))
       (serve-speedscope-asset (subseq path 12)))
      ;; Serve profile data
      ((and (> (length path) 14)
            (string= (subseq path 0 14) "/profile-data/"))
       (serve-profile-data (subseq path 14)))
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
  (setf *last-eval-generation* -1)
  (setf *eval-generation-poller*
        (bt:make-thread
         (lambda ()
           (loop
             (sleep 1)
             (handler-case
                 ;; Use with-slynk-connection for proper locking
                 (let ((gen (ignore-errors
                              (with-slynk-connection
                                (slynk-client:slime-eval
                                 '(cl:if (cl:boundp (cl:quote cl-user::*icl-eval-generation*))
                                         (cl:symbol-value (cl:quote cl-user::*icl-eval-generation*))
                                         0)
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
