;;; mcp-server.lisp --- MCP (Model Context Protocol) server for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; This module implements an MCP server that allows AI assistants like
;;; Gemini CLI to query the Lisp environment during explanations.

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; MCP Protocol Constants
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter +mcp-protocol-version+ "2024-11-05"
  "MCP protocol version we support.")

(defparameter *mcp-log-file* (merge-pathnames ".icl-mcp.log" (user-homedir-pathname))
  "Log file for MCP server activity.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; MCP Logging
;;; ─────────────────────────────────────────────────────────────────────────────

(defun mcp-log (format-string &rest args)
  "Log a message to the MCP log file with timestamp."
  (ignore-errors
    (with-open-file (out *mcp-log-file*
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (multiple-value-bind (sec min hour day month year)
          (get-decoded-time)
        (format out "[~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D] "
                year month day hour min sec))
      (apply #'format out format-string args)
      (terpri out))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; JSON Utilities
;;; ─────────────────────────────────────────────────────────────────────────────

(defun json-getf (plist key)
  "Get value from JSON-decoded plist. KEY is a string."
  (loop for (k v) on plist by #'cddr
        when (string= k key) return v))

(defun make-json-response (id result)
  "Create a JSON-RPC response."
  (yason:with-output-to-string* ()
    (yason:with-object ()
      (yason:encode-object-element "jsonrpc" "2.0")
      (yason:encode-object-element "id" id)
      (yason:encode-object-element "result" result))))

(defun make-json-error (id code message)
  "Create a JSON-RPC error response."
  (yason:with-output-to-string* ()
    (yason:with-object ()
      (yason:encode-object-element "jsonrpc" "2.0")
      (yason:encode-object-element "id" id)
      (yason:with-object-element ("error")
        (yason:with-object ()
          (yason:encode-object-element "code" code)
          (yason:encode-object-element "message" message))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; MCP Tools Definition
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *mcp-tools*
  '(("get_documentation"
     "Get the documentation string for a Lisp symbol (function, variable, or type)"
     (("symbol" "string" "The symbol name (e.g., MAPCAR, *PACKAGE*, DEFUN)" t)
      ("type" "string" "Documentation type: FUNCTION, VARIABLE, TYPE, or ALL (default: ALL)" nil)))
    ("describe_symbol"
     "Get full description of a Lisp symbol including type, value, and documentation"
     (("symbol" "string" "The symbol name to describe" t)))
    ("apropos_search"
     "Search for symbols whose names contain a substring"
     (("pattern" "string" "Substring to search for in symbol names" t)))
    ("list_package_symbols"
     "List all exported symbols from a package"
     (("package" "string" "Package name (e.g., COMMON-LISP, CL-USER, ALEXANDRIA)" t)))
    ("get_function_arglist"
     "Get the parameter list (lambda list) for a function or macro"
     (("function" "string" "The function or macro name" t)))
    ("read_source_file"
     "Read source code from a library file in the ocicl directory"
     (("path" "string" "Relative path within ocicl directory (e.g., alexandria-20240101/alexandria.lisp)" t)))
    ("list_source_files"
     "List Lisp source files in the ocicl directory, optionally filtered by pattern"
     (("pattern" "string" "Optional glob pattern to filter (e.g., alexandria* or *.asd)" nil))))
  "List of MCP tools: (name description ((arg-name type description required)...).
   NOTE: No eval tool - we only provide read-only inspection for security.")

(defun format-tool-schema (tool)
  "Format a tool definition as MCP JSON schema."
  (destructuring-bind (name description args) tool
    (let ((properties (make-hash-table :test 'equal))
          (required nil))
      (dolist (arg args)
        (destructuring-bind (arg-name arg-type arg-desc arg-required) arg
          (let ((prop (make-hash-table :test 'equal)))
            (setf (gethash "type" prop) arg-type)
            (setf (gethash "description" prop) arg-desc)
            (setf (gethash arg-name properties) prop))
          (when arg-required
            (push arg-name required))))
      (let ((schema (make-hash-table :test 'equal)))
        (setf (gethash "type" schema) "object")
        (setf (gethash "properties" schema) properties)
        (when required
          (setf (gethash "required" schema) (nreverse required)))
        (let ((result (make-hash-table :test 'equal)))
          (setf (gethash "name" result) name)
          (setf (gethash "description" result) description)
          (setf (gethash "inputSchema" result) schema)
          result)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tool Implementations
;;; ─────────────────────────────────────────────────────────────────────────────

(defun mcp-get-documentation (symbol-name &optional doc-type)
  "Get documentation string for SYMBOL-NAME. DOC-TYPE can be FUNCTION, VARIABLE, TYPE, or ALL.
   SYMBOL-NAME can be package-qualified like PACKAGE:SYMBOL or just SYMBOL."
  (handler-case
      (let* ((type-str (string-upcase (or doc-type "ALL")))
             ;; Use READ-FROM-STRING to properly handle package-qualified names
             (code (if (string= type-str "ALL")
                       ;; Try all documentation types
                       (format nil
                               "(handler-case
                                  (let* ((sym (read-from-string ~S))
                                         (fn-doc (and (symbolp sym) (documentation sym 'function)))
                                         (var-doc (and (symbolp sym) (documentation sym 'variable)))
                                         (type-doc (and (symbolp sym) (documentation sym 'type))))
                                    (if (symbolp sym)
                                        (with-output-to-string (s)
                                          (when fn-doc (format s \"Function: ~~A~~%%\" fn-doc))
                                          (when var-doc (format s \"Variable: ~~A~~%%\" var-doc))
                                          (when type-doc (format s \"Type: ~~A~~%%\" type-doc))
                                          (unless (or fn-doc var-doc type-doc)
                                            (format s \"No documentation found for ~~A\" sym)))
                                        \"Not a symbol.\"))
                                  (error (e) (format nil \"Error: ~~A\" e)))"
                               (string-upcase symbol-name))
                       ;; Specific doc type
                       (format nil
                               "(handler-case
                                  (let ((sym (read-from-string ~S)))
                                    (if (symbolp sym)
                                        (or (documentation sym '~A) \"No ~A documentation.\")
                                        \"Not a symbol.\"))
                                  (error (e) (format nil \"Error: ~~A\" e)))"
                               (string-upcase symbol-name)
                               type-str type-str))))
        (let ((result (first (backend-eval code))))
          (or result "No documentation available.")))
    (error (e)
      (format nil "Error: ~A" e))))

(defun mcp-describe-symbol (symbol-name)
  "Get full description of SYMBOL-NAME via Slynk.
   SYMBOL-NAME can be package-qualified like PACKAGE:SYMBOL."
  (handler-case
      (let* ((code (format nil
                           "(handler-case
                              (let ((sym (read-from-string ~S)))
                                (if (symbolp sym)
                                    (with-output-to-string (*standard-output*)
                                      (describe sym))
                                    \"Not a symbol.\"))
                              (error (e) (format nil \"Error: ~~A\" e)))"
                           (string-upcase symbol-name))))
        (let ((result (first (backend-eval code))))
          (or result "Symbol not found or no description available.")))
    (error (e)
      (format nil "Error: ~A" e))))

(defun mcp-apropos-search (pattern)
  "Search for symbols matching PATTERN."
  (handler-case
      (let* ((code (format nil
                           "(with-output-to-string (*standard-output*)
                              (apropos ~S))"
                           pattern)))
        (let ((result (first (backend-eval code))))
          (if (and result (> (length result) 0))
              result
              "No matching symbols found.")))
    (error (e)
      (format nil "Error: ~A" e))))

(defun mcp-list-package-symbols (package-name)
  "List exported symbols from PACKAGE-NAME."
  (handler-case
      (let* ((code (format nil
                           "(let ((pkg (find-package ~S)))
                              (if pkg
                                  (let ((syms nil))
                                    (do-external-symbols (s pkg)
                                      (push (symbol-name s) syms))
                                    (format nil \"~~{~~A~~^~~%~~}\" (sort syms #'string<)))
                                  \"Package not found.\"))"
                           (string-upcase package-name))))
        (let ((result (first (backend-eval code))))
          (or result "No symbols found.")))
    (error (e)
      (format nil "Error: ~A" e))))

(defun mcp-get-function-arglist (function-name)
  "Get argument list for FUNCTION-NAME.
   FUNCTION-NAME can be package-qualified like PACKAGE:FUNCTION."
  (handler-case
      (let* ((code (format nil
                           "(handler-case
                              (let ((sym (read-from-string ~S)))
                                (if (and (symbolp sym) (fboundp sym))
                                    (format nil \"~~S\" (slynk-backend:arglist sym))
                                    (format nil \"Function ~~A not found or not bound.\" ~S)))
                              (error (e) (format nil \"Error: ~~A\" e)))"
                           (string-upcase function-name)
                           (string-upcase function-name))))
        (let ((result (first (backend-eval code))))
          (or result "Could not get arglist.")))
    (error (e)
      (format nil "Error: ~A" e))))

(defun safe-ocicl-path-p (path)
  "Check if PATH is safe (within ocicl directory, no ..)."
  (and (stringp path)
       (> (length path) 0)
       (not (search ".." path))
       (not (char= (char path 0) #\/))))

(defun mcp-read-source-file (relative-path)
  "Read a source file from the ocicl directory."
  (if (not (safe-ocicl-path-p relative-path))
      "Error: Invalid path. Must be relative path within ocicl directory."
      (let ((full-path (merge-pathnames relative-path
                                        (merge-pathnames "ocicl/" (uiop:getcwd)))))
        (if (probe-file full-path)
            (handler-case
                (uiop:read-file-string full-path)
              (error (e)
                (format nil "Error reading file: ~A" e)))
            (format nil "File not found: ~A" relative-path)))))

(defun mcp-list-source-files (pattern)
  "List Lisp source files in ocicl directory matching PATTERN."
  (let* ((ocicl-dir (merge-pathnames "ocicl/" (uiop:getcwd)))
         (glob-pattern (if (and pattern (> (length pattern) 0))
                           (format nil "~A/**/*.lisp" pattern)
                           "**/*.lisp"))
         (files (handler-case
                    (directory (merge-pathnames glob-pattern ocicl-dir))
                  (error () nil))))
    (if files
        (format nil "~{~A~^~%~}"
                (mapcar (lambda (f)
                          (enough-namestring f ocicl-dir))
                        (sort files #'string< :key #'namestring)))
        (if (and pattern (> (length pattern) 0))
            (format nil "No .lisp files matching '~A' found in ocicl/" pattern)
            "No .lisp files found in ocicl/"))))

(defun args-get (args key &optional default)
  "Get value from ARGS which may be a hash table or plist. KEY is a string."
  (cond
    ((hash-table-p args)
     (gethash key args default))
    ((listp args)
     ;; It's a plist from JSON parsing
     (loop for (k v) on args by #'cddr
           when (string= k key) return v
           finally (return default)))
    (t default)))

(defun execute-tool (name arguments)
  "Execute MCP tool NAME with ARGUMENTS. Returns result string.
   Only read-only inspection tools are available for security."
  (let ((args arguments))
    (cond
      ((string= name "get_documentation")
       (mcp-get-documentation (args-get args "symbol" "")
                              (args-get args "type" nil)))
      ((string= name "describe_symbol")
       (mcp-describe-symbol (args-get args "symbol" "")))
      ((string= name "apropos_search")
       (mcp-apropos-search (args-get args "pattern" "")))
      ((string= name "list_package_symbols")
       (mcp-list-package-symbols (args-get args "package" "CL-USER")))
      ((string= name "get_function_arglist")
       (mcp-get-function-arglist (args-get args "function" "")))
      ((string= name "read_source_file")
       (mcp-read-source-file (args-get args "path" "")))
      ((string= name "list_source_files")
       (mcp-list-source-files (args-get args "pattern" "")))
      (t
       (format nil "Unknown tool: ~A" name)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; MCP Request Handlers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun handle-initialize (id params)
  "Handle MCP initialize request."
  (declare (ignore params))
  (let ((result (make-hash-table :test 'equal))
        (capabilities (make-hash-table :test 'equal))
        (tools-cap (make-hash-table :test 'equal))
        (server-info (make-hash-table :test 'equal)))
    ;; Server capabilities - use yason:false for JSON false (not nil which becomes null)
    (setf (gethash "listChanged" tools-cap) yason:false)
    (setf (gethash "tools" capabilities) tools-cap)
    ;; Server info
    (setf (gethash "name" server-info) "icl-mcp-server")
    (setf (gethash "version" server-info) +version+)
    ;; Build result
    (setf (gethash "protocolVersion" result) +mcp-protocol-version+)
    (setf (gethash "capabilities" result) capabilities)
    (setf (gethash "serverInfo" result) server-info)
    (make-json-response id result)))

(defun handle-tools-list (id params)
  "Handle tools/list request."
  (declare (ignore params))
  (let ((result (make-hash-table :test 'equal))
        (tools (mapcar #'format-tool-schema *mcp-tools*)))
    (setf (gethash "tools" result) tools)
    (make-json-response id result)))

(defun handle-tools-call (id params)
  "Handle tools/call request."
  (let* ((name (json-getf params "name"))
         (arguments (json-getf params "arguments")))
    (mcp-log "Tool call: ~A args=~S" name arguments)
    ;; Log to stderr - Gemini CLI will forward with "MCP STDERR" prefix
    ;; Leading newline separates from AI output that may not end with newline
    (format *error-output* "~%Tool: ~A~%" name)
    (force-output *error-output*)
    (let* ((output (execute-tool name arguments))
           (result (make-hash-table :test 'equal))
           (content-item (make-hash-table :test 'equal))
           (content (list content-item)))
      (mcp-log "Tool result: ~A" (subseq output 0 (min 200 (length output))))
      (setf (gethash "type" content-item) "text")
      (setf (gethash "text" content-item) output)
      (setf (gethash "content" result) content)
      (make-json-response id result))))

(defun handle-mcp-request (request)
  "Handle an MCP JSON-RPC request. Returns response string."
  (let* ((id (json-getf request "id"))
         (method (json-getf request "method"))
         (params (json-getf request "params")))
    (mcp-log "Request: method=~A id=~A" method id)
    (cond
      ((string= method "initialize")
       (handle-initialize id params))
      ((string= method "notifications/initialized")
       ;; No response needed for notifications
       nil)
      ((string= method "tools/list")
       (handle-tools-list id params))
      ((string= method "tools/call")
       (handle-tools-call id params))
      ((string= method "ping")
       (make-json-response id (make-hash-table :test 'equal)))
      (t
       (if id
           (make-json-error id -32601 (format nil "Method not found: ~A" method))
           nil)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; MCP Server Main Loop
;;; ─────────────────────────────────────────────────────────────────────────────

(defun run-mcp-server (&key (host "127.0.0.1") (port *slynk-port*))
  "Run the MCP server, reading JSON-RPC from stdin and writing to stdout.
   Connects to Slynk at HOST:PORT."
  ;; Connect to Slynk backend
  (unless (slynk-connect :host host :port port)
    (format *error-output* "Failed to connect to Slynk at ~A:~D~%" host port)
    (mcp-log "FAILED to connect to Slynk at ~A:~D" host port)
    (uiop:quit 1))
  ;; Log startup
  (mcp-log "MCP server started, connected to Slynk at ~A:~D" host port)
  ;; Log to stderr only
  (format *error-output* "ICL MCP server started, connected to Slynk at ~A:~D~%" host port)
  (force-output *error-output*)
  ;; Main loop: read JSON-RPC from stdin, write responses to stdout
  (loop
    (handler-case
        (let ((line (read-line *standard-input* nil :eof)))
          (when (eq line :eof)
            (mcp-log "MCP server shutting down (EOF)")
            (return))
          (when (and line (plusp (length (string-trim '(#\Space #\Tab) line))))
            (let* ((request (yason:parse line :object-as :plist))
                   (response (handle-mcp-request request)))
              (when response
                (write-line response *standard-output*)
                (force-output *standard-output*)))))
      (error (e)
        (mcp-log "MCP server error: ~A" e)
        (format *error-output* "MCP server error: ~A~%" e)
        (force-output *error-output*)))))
