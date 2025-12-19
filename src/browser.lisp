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
  (handler-case
      (let ((result-string (first (backend-eval code))))
        (when result-string
          (ignore-errors (read-from-string result-string))))
    (error () nil)))

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
  "Get information about a symbol."
  (browser-query
   (format nil
           "(let ((sym (find-symbol ~S (find-package ~S))))
              (when sym
                (cond
                  ((find-class sym nil)
                   (let ((class (find-class sym)))
                     (list :type :class
                           :name sym
                           :superclasses (ignore-errors
                                           (mapcar #'class-name
                                                   (funcall (find-symbol \"CLASS-DIRECT-SUPERCLASSES\"
                                                                         (or (find-package :closer-mop)
                                                                             (find-package :sb-mop)))
                                                            class)))
                           :slots (ignore-errors
                                    (mapcar (lambda (s)
                                              (funcall (find-symbol \"SLOT-DEFINITION-NAME\"
                                                                    (or (find-package :closer-mop)
                                                                        (find-package :sb-mop)))
                                                       s))
                                            (funcall (find-symbol \"CLASS-DIRECT-SLOTS\"
                                                                  (or (find-package :closer-mop)
                                                                      (find-package :sb-mop)))
                                                     class))))))
                  ((fboundp sym)
                   (list :type (cond
                                 ((typep (fdefinition sym) 'generic-function) :generic)
                                 ((macro-function sym) :macro)
                                 (t :function))
                         :name sym
                         :arglist (ignore-errors (slynk-backend:arglist sym))
                         :documentation (documentation sym 'function)))
                  ((boundp sym)
                   (list :type :variable
                         :name sym
                         :value (prin1-to-string (symbol-value sym))
                         :documentation (documentation sym 'variable)
                         :constantp (constantp sym)))
                  (t (list :type :symbol :name sym)))))"
           symbol-name package-name)))

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
  ;; REPL thread will be started when terminal sends 'terminal-ready'
  )

(defmethod hunchensocket:client-disconnected ((resource repl-resource) client)
  (declare (ignore client)))

(defmethod hunchensocket:text-message-received ((resource repl-resource) client message)
  "Handle incoming WebSocket messages."
  (let ((json (ignore-errors (com.inuoe.jzon:parse message))))
    (when json
      (let ((type (gethash "type" json)))
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
             (when (and pkg name)
               (send-symbol-info client pkg name))))

          ;; Inspect object
          ((string= type "inspect")
           (let ((form (gethash "form" json)))
             (when form
               (send-inspection client form))))

          ;; Inspector drill-down action
          ((string= type "inspector-action")
           (let ((index (gethash "index" json)))
             (when index
               (send-inspector-action client index))))

          ;; Inspector go back
          ((string= type "inspector-pop")
           (send-inspector-pop client)))))))

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
  (let ((obj (make-hash-table :test 'equal)))
    (setf (gethash "type" obj) type)
    (loop for (key val) on plist by #'cddr
          do (setf (gethash (string-downcase (string key)) obj)
                   (cond
                     ;; Convert nested plist to hash table
                     ((and val (listp val) (keywordp (first val)))
                      (plist-to-hash val))
                     ;; Convert alist of (name . type) to arrays
                     ((and val (listp val) (consp (first val)))
                      (mapcar (lambda (pair)
                                (list (car pair) (string-downcase (string (cdr pair)))))
                              val))
                     (t val))))
    (hunchensocket:send-text-message client (com.inuoe.jzon:stringify obj))))

(defun send-packages-list (client)
  "Send list of all packages to CLIENT."
  (let ((packages (browser-query
                   "(sort (mapcar #'package-name (list-all-packages)) #'string<)")))
    (ws-send client "packages" :data packages)))

(defun send-symbols-list (client package-name)
  "Send symbols from PACKAGE-NAME to CLIENT."
  (let ((symbols (get-package-symbols package-name)))
    (ws-send client "symbols" :package package-name :data symbols)))

(defun send-symbol-info (client package-name symbol-name)
  "Send symbol info to CLIENT."
  (handler-case
      (let ((info (get-symbol-info symbol-name package-name)))
        (ws-send client "symbol-info" :package package-name :name symbol-name :data info))
    (error (e)
      (format *error-output* "~&; Error getting symbol info for ~A:~A: ~A~%"
              package-name symbol-name e))))

(defun send-inspection (client form)
  "Send inspection data for FORM to CLIENT."
  (handler-case
      (let ((data (slynk-inspect-object form)))
        (when data
          (let* ((raw-content (getf data :content))
                 (content (if (and (listp raw-content) (listp (first raw-content)))
                              (first raw-content)
                              raw-content))
                 (parsed (when (listp content)
                           (parse-inspector-content content))))
            (ws-send client "inspection"
                     :title (getf data :title)
                     :action "new"
                     :entries (or parsed nil)))))
    (error (e)
      (format *error-output* "~&; Error inspecting ~A: ~A~%" form e))))

(defun send-inspector-action (client index)
  "Drill down into inspector item at INDEX and send result to CLIENT."
  (handler-case
      (let ((data (slynk-inspector-action index)))
        (when data
          (let* ((raw-content (getf data :content))
                 (content (if (and (listp raw-content) (listp (first raw-content)))
                              (first raw-content)
                              raw-content))
                 (parsed (when (listp content)
                           (parse-inspector-content content))))
            (ws-send client "inspection"
                     :title (getf data :title)
                     :action "push"
                     :entries (or parsed nil)))))
    (error (e)
      (format *error-output* "~&; Error in inspector action ~A: ~A~%" index e))))

(defun send-inspector-pop (client)
  "Go back in inspector and send result to CLIENT."
  (handler-case
      (let ((data (slynk-inspector-pop)))
        (when data
          (let* ((raw-content (getf data :content))
                 (content (if (and (listp raw-content) (listp (first raw-content)))
                              (first raw-content)
                              raw-content))
                 (parsed (when (listp content)
                           (parse-inspector-content content))))
            (ws-send client "inspection"
                     :title (getf data :title)
                     :action "pop"
                     :entries (or parsed nil)))))
    (error (e)
      (format *error-output* "~&; Error in inspector pop: ~A~%" e))))

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
    (when (plusp (length text))
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
    * { box-sizing: border-box; }
    html, body { margin: 0; padding: 0; overflow: hidden; }
    body { font-family: 'JetBrains Mono', monospace; font-size: 13px; background: #1e1e1e; color: #d4d4d4; }
    #layout-container { height: 100vh; width: 100vw; }
    .lm_header { background: #2d2d2d !important; }
    .lm_tab { background: #2d2d2d !important; color: #d4d4d4 !important; }
    .lm_tab.lm_active { background: #1e1e1e !important; }
    .panel { height: 100%%; display: flex; flex-direction: column; overflow: hidden; }
    .panel-header { padding: 8px; background: #2d2d2d; border-bottom: 1px solid #3d3d3d; flex-shrink: 0; }
    .panel-header input { width: 100%%; padding: 4px 8px; background: #3d3d3d; border: 1px solid #4d4d4d; color: #d4d4d4; border-radius: 3px; }
    .panel-content { flex: 1; overflow-y: auto; overflow-x: hidden; min-height: 0; }
    .panel-content::-webkit-scrollbar { width: 8px; }
    .panel-content::-webkit-scrollbar-track { background: #1e1e1e; }
    .panel-content::-webkit-scrollbar-thumb { background: #555; border-radius: 4px; }
    .panel-content::-webkit-scrollbar-thumb:hover { background: #777; }
    .list-item { padding: 4px 8px; cursor: pointer; }
    .list-item:hover { background: #2d2d2d; }
    .list-item.selected { background: #094771; }
    .badge { color: #888; margin-right: 4px; }
    .terminal-container { position: absolute; top: 0; left: 0; right: 0; bottom: 0; }
    .terminal-container .xterm { height: 100%%; width: 100%%; }
    .terminal-container .xterm-screen { height: 100%%; }
    .terminal-container .xterm-viewport { height: 100%%; overflow-y: auto !important; }
    .dv-content-container { position: relative; height: 100%%; }
    .dv-content-container > .panel { position: absolute; top: 0; left: 0; right: 0; bottom: 0; }
    .detail-content { padding: 8px; white-space: pre-wrap; font-family: 'JetBrains Mono', monospace; }
    .inspector-entry { padding: 2px 8px; }
    .inspector-label { color: #888; }
    .inspector-value { color: #9cdcfe; }
    .inspector-link { color: #4fc1ff; cursor: pointer; text-decoration: underline; }
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
    let packages = [];
    let symbols = [];

    ws.onopen = () => {
      console.log('Connected');
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
          renderPackages();
          break;
        case 'symbols':
          symbols = msg.data || [];
          renderSymbols();
          break;
        case 'symbol-info':
          renderSymbolInfo(msg.data);
          break;
        case 'inspection':
          renderInspection(msg);
          break;
      }
    };

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
          const [name, type] = s;
          const badge = {class:'C',generic:'G',function:'F',macro:'M',variable:'V'}[type] || 'S';
          return `<div class='list-item' onclick='selectSymbol(\"${name.replace(/'/g, \"\\\\'\")}\")'>` +
                 `<span class='badge'>[${badge}]</span>${name}</div>`;
        }).join('');
    }

    function renderSymbolInfo(info) {
      const el = document.getElementById('detail-content');
      if (!el || !info) return;
      let html = `<strong>${info.type || 'Symbol'}:</strong> ${info.name || ''}\\n\\n`;
      if (info.arglist) html += `<strong>Arguments:</strong> ${info.arglist}\\n\\n`;
      if (info.value) html += `<strong>Value:</strong> ${info.value}\\n\\n`;
      if (info.documentation) html += `<strong>Documentation:</strong>\\n${info.documentation}\\n`;
      el.innerHTML = html;
    }

    let inspectorDepth = 0;

    function renderInspection(msg) {
      const el = document.getElementById('detail-content');
      const header = document.getElementById('inspector-header');
      if (!el) return;

      // Track depth for Back button
      if (msg.action === 'push') inspectorDepth++;
      else if (msg.action === 'pop') inspectorDepth = Math.max(0, inspectorDepth - 1);
      else inspectorDepth = 1;

      // Show/hide Back button
      if (header) header.style.display = inspectorDepth > 1 ? 'block' : 'none';

      let html = `<strong>${msg.title || 'Object'}</strong>\\n\\n`;
      (msg.entries || []).forEach(([label, value, action]) => {
        const escapedValue = String(value).replace(/</g, '&lt;').replace(/>/g, '&gt;');
        if (action !== null) {
          html += `<span class='inspector-label'>${label}: </span><span class='inspector-link' onclick='inspectAction(${action})'>${escapedValue}</span>\\n`;
        } else {
          html += `<span class='inspector-label'>${label}: </span><span class='inspector-value'>${escapedValue}</span>\\n`;
        }
      });
      el.innerHTML = html;
    }

    function selectPackage(pkg) {
      selectedPackage = pkg;
      renderPackages(document.getElementById('package-filter')?.value || '');
      ws.send(JSON.stringify({type: 'get-symbols', package: pkg}));
    }

    function selectSymbol(name) {
      ws.send(JSON.stringify({type: 'get-symbol-info', package: selectedPackage, name: name}));
    }

    function inspectAction(action) {
      ws.send(JSON.stringify({type: 'inspector-action', index: action}));
    }

    function inspectBack() {
      ws.send(JSON.stringify({type: 'inspector-pop'}));
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
          <div class='panel-header' id='inspector-header' style='display:none;'>
            <button onclick='inspectBack()' style='padding:2px 8px;background:#3d3d3d;border:1px solid #4d4d4d;color:#d4d4d4;border-radius:3px;cursor:pointer;'>← Back</button>
          </div>
          <div class='panel-content detail-content' id='detail-content'>
            Ctrl+click a symbol in the terminal to inspect it.
          </div>`;
      }
      get element() { return this._element; }
      init(params) {}
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
          fitAddon.fit();

          // Send all input directly to Lisp - the editor handles everything
          terminal.onData(data => {
            ws.send(JSON.stringify({type: 'input', data: data}));
          });

          // Click to inspect symbol - capture phase to intercept before xterm
          // Store detected symbol from mousemove for click to use
          let hoveredSymbol = null;

          terminal.element.addEventListener('mousedown', (e) => {
            // If we have a symbol under cursor from hover, inspect it on click
            if (hoveredSymbol) {
              e.preventDefault();
              e.stopPropagation();
              ws.send(JSON.stringify({type: 'inspect', form: hoveredSymbol}));
              setTimeout(() => terminal.focus(), 10);
            }
          }, { capture: true });

          // Hover highlight box for symbols
          let highlightBox = null;
          const termEl = this._element;
          this._element.addEventListener('mousemove', (e) => {
            if (!terminal) return;
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
                highlightBox.style.cssText = 'position:absolute;border:1px solid #4fc1ff;border-radius:2px;pointer-events:none;z-index:10;';
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

          // Refit on resize and after short delay for initial sizing
          const doFit = () => { try { fitAddon.fit(); } catch(e) {} };
          new ResizeObserver(doFit).observe(this._element);
          setTimeout(doFit, 50);
          setTimeout(doFit, 200);

          // Signal that terminal is ready - this triggers REPL start
          // Make sure WebSocket is open first
          if (ws.readyState === WebSocket.OPEN) {
            ws.send(JSON.stringify({type: 'terminal-ready'}));
          } else {
            ws.addEventListener('open', () => {
              ws.send(JSON.stringify({type: 'terminal-ready'}));
            }, {once: true});
          }
        }, 100);
      }
    }

    // Create Dockview layout
    const container = document.getElementById('layout-container');
    const dv = window['dockview-core'];

    const api = dv.createDockview(container, {
      className: 'dockview-theme-abyss',
      disableAutoResizing: false,
      createComponent: (options) => {
        switch (options.name) {
          case 'packages': return new PackagesPanel();
          case 'symbols': return new SymbolsPanel();
          case 'inspector': return new InspectorPanel();
          case 'terminal': return new TerminalPanel();
        }
      }
    });

    // Layout: Packages/Symbols/Inspector side by side at top (1/5 height), REPL Console below (4/5)
    api.addPanel({ id: 'terminal', component: 'terminal', title: 'REPL Console' });
    api.addPanel({ id: 'packages', component: 'packages', title: 'Packages', position: { referencePanel: 'terminal', direction: 'above' } });
    api.addPanel({ id: 'symbols', component: 'symbols', title: 'Symbols', position: { referencePanel: 'packages', direction: 'right' } });
    api.addPanel({ id: 'inspector', component: 'inspector', title: 'Inspector', position: { referencePanel: 'symbols', direction: 'right' } });

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
  "Find the assets directory.
   Search order:
   1. ICL_ASSETS_PATH environment variable
   2. ./assets/ relative to executable
   3. /usr/share/icl/assets/ (installed, Unix only)
   4. ./assets/ relative to ASDF system source (development)"
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
  ;; 3. System install location (Unix only)
  #-windows
  (let ((system-assets (pathname "/usr/share/icl/assets/")))
    (when (probe-file system-assets)
      (return-from find-assets-directory system-assets)))
  ;; 4. Fall back to ASDF system source (development)
  (merge-pathnames "assets/" (asdf:system-source-directory :icl)))

(defvar *assets-directory* (find-assets-directory)
  "Directory containing browser assets (JS, CSS).")

(defun serve-asset (filename)
  "Serve an asset file, returning content and setting content-type."
  (let ((filepath (merge-pathnames filename *assets-directory*)))
    (when (probe-file filepath)
      (setf (hunchentoot:content-type*)
            (cond
              ((alexandria:ends-with-subseq ".css" filename) "text/css")
              ((alexandria:ends-with-subseq ".js" filename) "application/javascript")
              (t "application/octet-stream")))
      (alexandria:read-file-into-string filepath))))

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
               (let ((*standard-input* in-stream)
                     (*standard-output* out-stream)
                     (*error-output* out-stream)
                     (*trace-output* out-stream)
                     (*terminal-io* (make-two-way-stream in-stream out-stream))
                     (*query-io* (make-two-way-stream in-stream out-stream))
                     (*in-repl* t)
                     (*input-count* 0)
                     (*browser-terminal-active* t))
                 ;; Use the real REPL loop with full editor support
                 (repl-loop)))
             :name "browser-repl")))))
