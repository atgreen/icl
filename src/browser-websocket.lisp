;;; browser-websocket.lisp --- WebSocket handlers and streams for ICL browser
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; This module provides WebSocket resource classes, message handlers,
;;; send helpers, and Gray streams for the browser REPL.

(in-package #:icl)

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
                                     ;; JSON string (starts with { or [)
                                     ((and (stringp obj)
                                           (let ((trimmed (string-left-trim '(#\\Space #\\Tab #\\Newline) obj)))
                                             (and (> (length trimmed) 0)
                                                  (or (char= (char trimmed 0) #\\{)
                                                      (char= (char trimmed 0) #\\[)))))
                                      (list :json obj))
                                     ;; Hash table
                                     ((hash-table-p obj)
                                      (list :hash-table
                                            (hash-table-count obj)
                                            (loop for k being the hash-keys of obj using (hash-value v)
                                                  for i from 0 below 100
                                                  collect (list (princ-to-string k) (princ-to-string v)))))
                                     ;; Image byte array (check magic bytes)
                                     ((and (typep obj '(simple-array (unsigned-byte 8) (*)))
                                           (>= (length obj) 4)
                                           (or ;; PNG
                                               (and (= (aref obj 0) #x89) (= (aref obj 1) #x50)
                                                    (= (aref obj 2) #x4E) (= (aref obj 3) #x47))
                                               ;; JPEG
                                               (and (= (aref obj 0) #xFF) (= (aref obj 1) #xD8) (= (aref obj 2) #xFF))
                                               ;; GIF
                                               (and (= (aref obj 0) #x47) (= (aref obj 1) #x49)
                                                    (= (aref obj 2) #x46) (= (aref obj 3) #x38))
                                               ;; WebP
                                               (and (>= (length obj) 12)
                                                    (= (aref obj 0) #x52) (= (aref obj 1) #x49)
                                                    (= (aref obj 2) #x46) (= (aref obj 3) #x46)
                                                    (= (aref obj 8) #x57) (= (aref obj 9) #x45)
                                                    (= (aref obj 10) #x42) (= (aref obj 11) #x50))))
                                      (let ((mime (cond
                                                    ((and (= (aref obj 0) #x89) (= (aref obj 1) #x50)) \"image/png\")
                                                    ((and (= (aref obj 0) #xFF) (= (aref obj 1) #xD8)) \"image/jpeg\")
                                                    ((and (= (aref obj 0) #x47) (= (aref obj 1) #x49)) \"image/gif\")
                                                    (t \"image/webp\"))))
                                        (list :image-bytes mime (icl-runtime:usb8-array-to-base64-string obj))))
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
              ((and (listp parsed) (eq (first parsed) :json))
               (setf (gethash "vizType" obj) "json")
               (setf (gethash "content" obj) (second parsed)))
              ((and (listp parsed) (eq (first parsed) :hash-table))
               (setf (gethash "vizType" obj) "hash-table")
               (setf (gethash "count" obj) (second parsed))
               (setf (gethash "entries" obj) (third parsed)))
              ((and (listp parsed) (eq (first parsed) :image-bytes))
               (let* ((mime (second parsed))
                      (base64 (third parsed))
                      (data-url (format nil "data:~A;base64,~A" mime base64)))
                 (setf (gethash "vizType" obj) "image")
                 (setf (gethash "imageUrl" obj) data-url)))
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

(defun open-json-panel (title content source-expr)
  "Send message to browser to open a JSON visualization panel."
  (when *repl-resource*
    (dolist (client (hunchensocket:clients *repl-resource*))
      (let ((obj (make-hash-table :test 'equal)))
        (setf (gethash "type" obj) "open-json")
        (setf (gethash "title" obj) title)
        (setf (gethash "content" obj) content)
        (setf (gethash "sourceExpr" obj) source-expr)
        (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))))))

(defun open-image-panel (title image-url content-type source-expr)
  "Send message to browser to open an image visualization panel."
  (when *repl-resource*
    (dolist (client (hunchensocket:clients *repl-resource*))
      (let ((obj (make-hash-table :test 'equal)))
        (setf (gethash "type" obj) "open-image")
        (setf (gethash "title" obj) title)
        (setf (gethash "imageUrl" obj) image-url)
        (setf (gethash "contentType" obj) content-type)
        (setf (gethash "sourceExpr" obj) source-expr)
        (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))))))

(defun open-vega-lite-panel (title spec source-expr)
  "Send message to browser to open a Vega-Lite visualization panel."
  (when *repl-resource*
    (dolist (client (hunchensocket:clients *repl-resource*))
      (let ((obj (make-hash-table :test 'equal)))
        (setf (gethash "type" obj) "open-vega-lite")
        (setf (gethash "title" obj) title)
        (setf (gethash "spec" obj) spec)
        (setf (gethash "sourceExpr" obj) source-expr)
        (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))))))

(defun open-mermaid-panel (title definition source-expr)
  "Send message to browser to open a Mermaid diagram panel."
  (when *repl-resource*
    (dolist (client (hunchensocket:clients *repl-resource*))
      (let ((obj (make-hash-table :test 'equal)))
        (setf (gethash "type" obj) "open-mermaid")
        (setf (gethash "title" obj) title)
        (setf (gethash "definition" obj) definition)
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

