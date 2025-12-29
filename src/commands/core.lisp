;;; core.lisp --- Core commands for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Help
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command (help h ?) (&optional command-name)
  "Show help for commands. Without argument, lists all commands."
  (if command-name
      (show-command-help command-name)
      (list-all-commands)))

(defun show-command-help (name)
  "Show detailed help for a specific command."
  (let ((cmd (find-command name)))
    (cond
      (cmd
       (format t "~&,~A" (string-downcase (symbol-name (command-name cmd))))
       (when (command-aliases cmd)
         (format t " (aliases: ~{,~A~^, ~})"
                 (mapcar (lambda (a) (string-downcase (symbol-name a)))
                         (command-aliases cmd))))
       (format t "~%")
       (when (command-argspec cmd)
         (format t "  Arguments: ~{~A~^ ~}~%"
                 (mapcar #'string-downcase
                         (mapcar #'symbol-name (command-argspec cmd)))))
       (when (command-documentation cmd)
         (format t "  ~A~%" (command-documentation cmd))))
      (t
       (format *error-output* "~&Unknown command: ~A~%" name)))))

(defun list-all-commands ()
  "List all available commands with brief descriptions."
  (format t "~&Available commands:~%~%")
  (let ((commands (list-commands)))
    (dolist (cmd commands)
      (format t "  ,~A"
              (string-downcase (symbol-name (command-name cmd))))
      (when (command-aliases cmd)
        (format t " (~{,~A~^, ~})"
                (mapcar (lambda (a) (string-downcase (symbol-name a)))
                        (command-aliases cmd))))
      (when (command-documentation cmd)
        (let ((doc (command-documentation cmd)))
          ;; Only show first line
          (let ((newline (position #\Newline doc)))
            (format t "~%      ~A" (if newline (subseq doc 0 newline) doc)))))
      (format t "~%")))
  (format t "~%Type ,help <command> for detailed help.~%"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Exit/Quit
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command (quit exit q) ()
  "Exit ICL."
  (invoke-restart 'quit))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Clear
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command clear ()
  "Clear the terminal screen."
  (format t "~C[2J~C[H" #\Escape #\Escape)
  (force-output))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Package Navigation
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command (cd in-package) (package-designator)
  "Change the current package context."
  (let ((pkg-name (string-upcase package-designator)))
    ;; Handle keyword syntax
    (when (and (plusp (length pkg-name))
               (char= (char pkg-name 0) #\:))
      (setf pkg-name (subseq pkg-name 1)))
    ;; Change package in both ICL and Slynk
    (handler-case
        (progn
          (backend-set-package pkg-name)
          ;; Always update the package name for prompt display
          (setf *icl-package-name* pkg-name)
          ;; Update *icl-package* if the package exists locally (for local lookups)
          (let ((pkg (find-package pkg-name)))
            (when pkg
              (setf *icl-package* pkg)))
          (format t "~&~A~%" pkg-name))
      (error (e)
        (format *error-output* "~&Failed to change package: ~A~%" e)))))

(define-command pwd ()
  "Show the current package."
  (format t "~&~A~%" *icl-package-name*))

(define-command (ls list-symbols) (&optional filter)
  "List symbols in current package.
Optional filter: functions, macros, variables, classes, all, external, internal"
  (let* ((filter-key (when (and filter (plusp (length filter)))
                       (intern (string-upcase filter) :keyword)))
         (symbols (collect-package-symbols *icl-package* filter-key)))
    (if symbols
        (progn
          (format t "~&~A symbols in ~A~A:~%"
                  (length symbols)
                  (package-name *icl-package*)
                  (if filter-key (format nil " (~A)" filter-key) ""))
          (let ((sorted (sort (copy-list symbols) #'string< :key #'symbol-name)))
            (dolist (sym sorted)
              (format t "  ~A~A~%"
                      (string-downcase (symbol-name sym))
                      (symbol-type-indicator sym)))))
        (format t "~&No symbols found.~%"))))

(defun collect-package-symbols (package filter)
  "Collect symbols from PACKAGE matching FILTER."
  (let ((results '()))
    (flet ((matches-filter-p (sym)
             (case filter
               (:functions (and (fboundp sym)
                                (not (macro-function sym))
                                (not (special-operator-p sym))))
               (:macros (macro-function sym))
               (:variables (and (boundp sym)
                                (not (constantp sym))))
               (:constants (constantp sym))
               (:classes (find-class sym nil))
               (:specials (special-operator-p sym))
               ((:all nil) t)
               (otherwise t))))
      (case filter
        (:internal
         (do-symbols (sym package)
           (when (eq (symbol-package sym) package)
             (push sym results))))
        ((:external nil)
         (do-external-symbols (sym package)
           (when (matches-filter-p sym)
             (push sym results))))
        (:all
         (do-symbols (sym package)
           (when (matches-filter-p sym)
             (push sym results))))
        (otherwise
         (do-external-symbols (sym package)
           (when (matches-filter-p sym)
             (push sym results))))))
    results))

(defun symbol-type-indicator (sym)
  "Return a type indicator string for SYM."
  (cond
    ((special-operator-p sym) " [special]")
    ((macro-function sym) " [macro]")
    ((fboundp sym) " [function]")
    ((find-class sym nil) " [class]")
    ((constantp sym) " [constant]")
    ((boundp sym) " [variable]")
    (t "")))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Info
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command info ()
  "Show ICL session information."
  (format t "~&ICL Information:~%")
  (format t "  Version:        ~A~%" +version+)
  (format t "  Backend:        Slynk (~A)~%"
          (if *slynk-connected-p* "connected" "disconnected"))
  (when *current-lisp*
    (format t "  Inferior Lisp:  ~A~%" *current-lisp*))
  (format t "  Slynk host:     ~A:~D~%" *slynk-host* *slynk-port*)
  (format t "  Package:        ~A~%" *icl-package-name*)
  (format t "  History file:   ~A~%" (history-file))
  (format t "  Input count:    ~A~%" *input-count*)
  (format t "  Terminal:       ~A~%" (if (terminal-capable-p) "capable" "dumb"))
  (format t "  MCP server:     ~A~%"
          (if (mcp-http-server-running-p)
              (format nil "running on port ~D" *mcp-http-port*)
              "not running")))

(define-command (lisp backend) (&optional lisp-name)
  "Show or change the current Lisp backend.
Without argument, shows current backend.
With argument, attempts to switch to specified Lisp (sbcl, ccl, ecl, etc.)
Example: ,lisp ccl"
  (if lisp-name
      ;; Switch to different Lisp
      (let ((impl (intern (string-upcase lisp-name) :keyword)))
        (handler-case
            (progn
              ;; Stop current inferior if running
              (when (inferior-lisp-alive-p)
                (stop-inferior-lisp))
              ;; Start new one
              (start-inferior-lisp :lisp impl)
              (format t "~&Switched to ~A~%" impl))
          (error (e)
            (format *error-output* "~&Failed to switch to ~A: ~A~%" lisp-name e))))
      ;; Show current backend
      (format t "~&Backend: ~A~%"
              (cond
                (*current-lisp* *current-lisp*)
                (*slynk-connected-p* (format nil "Slynk at ~A:~D" *slynk-host* *slynk-port*))
                (t "none")))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; History
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command history ()
  "Show input/result history variables."
  (format t "~&Recent values (use icl:_ or icl:icl-* to access):~%")
  (format t "  _   (icl-*)   = ~S~%" icl-*)
  (format t "  __  (icl-**)  = ~S~%" icl-**)
  (format t "  ___ (icl-***) = ~S~%" icl-***)
  (format t "~%Recent inputs (use icl:icl-+ to access):~%")
  (format t "  icl-+   = ~S~%" icl-+)
  (format t "  icl-++  = ~S~%" icl-++)
  (format t "  icl-+++ = ~S~%" icl-+++)
  (format t "~%Recent value lists (use icl:icl-/ to access):~%")
  (format t "  icl-/   = ~S~%" icl-/)
  (format t "  icl-//  = ~S~%" icl-//)
  (format t "  icl-/// = ~S~%" icl-///))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Documentation Commands
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command (doc d) (symbol-name)
  "Show documentation for a symbol.
Example: ,doc format"
  (handler-case
      (let ((doc (backend-documentation symbol-name 'function)))
        (if (and doc (not (string= doc "")))
            (format t "~&~A~%" doc)
            (let ((var-doc (backend-documentation symbol-name 'variable)))
              (if (and var-doc (not (string= var-doc "")))
                  (format t "~&~A~%" var-doc)
                  (format *error-output* "~&No documentation found for: ~A~%" symbol-name)))))
    (error (e)
      (format *error-output* "~&Error getting documentation: ~A~%" e))))

(define-command (apropos ap) (pattern)
  "Search for symbols matching a pattern.
Example: ,apropos string"
  (handler-case
      (let ((matches (backend-apropos pattern)))
        (if matches
            (progn
              (format t "~&~D symbols matching \"~A\":~%" (length matches) pattern)
              (let ((display (if (> (length matches) 50)
                                 (subseq matches 0 50)
                                 matches)))
                (dolist (item display)
                  ;; Item format: (symbol-name package-name kind)
                  (if (and (listp item) (>= (length item) 3))
                      (format t "  ~A:~A (~A)~%"
                              (second item) (first item) (third item))
                      (format t "  ~A~%" item)))
                (when (> (length matches) 50)
                  (format t "  ... and ~D more~%" (- (length matches) 50)))))
            (format t "~&No symbols matching \"~A\"~%" pattern)))
    (error (e)
      (format *error-output* "~&Error in apropos: ~A~%" e))))

(define-command (describe desc) (symbol-name)
  "Show full description of a symbol.
Example: ,describe format"
  (handler-case
      (let ((desc (backend-describe symbol-name)))
        (if (and desc (not (string= desc "")))
            (format t "~&~A~%" desc)
            (format *error-output* "~&No description for: ~A~%" symbol-name)))
    (error (e)
      (format *error-output* "~&Error describing symbol: ~A~%" e))))

(define-command (macroexpand mx) (form-string)
  "Expand a macro form once.
Example: ,macroexpand (when t 1)"
  (handler-case
      (let ((expanded (slynk-macroexpand form-string)))
        (if expanded
            (format t "~&~A~%" expanded)
            (format *error-output* "~&Macroexpand returned nil~%")))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(define-command (macroexpand-all mxa) (form-string)
  "Fully expand all macros in a form.
Example: ,macroexpand-all (when t (unless nil 1))"
  (handler-case
      (let ((expanded (slynk-macroexpand-all form-string)))
        (if expanded
            (format t "~&~A~%" expanded)
            (format *error-output* "~&Macroexpand-all returned nil~%")))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(defun parse-symbol-arg (string)
  "Parse STRING as a symbol reference. Returns the symbol or NIL."
  (let* ((str (string-trim '(#\Space #\Tab #\' #\") string))
         ;; Handle package:symbol or package::symbol
         (colon-pos (position #\: str))
         (double-colon (search "::" str)))
    (handler-case
        (cond
          ;; package::symbol (internal)
          (double-colon
           (let ((pkg-name (subseq str 0 double-colon))
                 (sym-name (subseq str (+ double-colon 2))))
             (let ((pkg (find-package (string-upcase pkg-name))))
               (when pkg
                 (find-symbol (string-upcase sym-name) pkg)))))
          ;; package:symbol (external)
          (colon-pos
           (let ((pkg-name (subseq str 0 colon-pos))
                 (sym-name (subseq str (1+ colon-pos))))
             (let ((pkg (find-package (string-upcase pkg-name))))
               (when pkg
                 (find-symbol (string-upcase sym-name) pkg)))))
          ;; Unqualified symbol - search current package then CL
          (t
           (let ((upcased (string-upcase str)))
             (or (find-symbol upcased *icl-package*)
                 (find-symbol upcased :cl)))))
      (error () nil))))

(defun show-documentation (sym)
  "Display documentation for symbol SYM."
  (let ((found nil))
    ;; Function documentation
    (when (fboundp sym)
      (setf found t)
      (let ((doc (documentation sym 'function))
            (arglist (get-arglist sym)))
        (format t "~&~A~A is a ~A~%"
                (if (symbol-package sym)
                    (format nil "~A:" (package-name (symbol-package sym)))
                    "")
                (string-downcase (symbol-name sym))
                (cond
                  ((special-operator-p sym) "special operator")
                  ((macro-function sym) "macro")
                  (t "function")))
        (when arglist
          (format t "  Arguments: ~{~A~^ ~}~%"
                  (mapcar #'string-downcase
                          (mapcar #'princ-to-string arglist))))
        (if doc
            (format t "~%~A~%" doc)
            (format t "  (no documentation)~%"))))
    ;; Variable documentation
    (when (and (boundp sym) (not (fboundp sym)))
      (setf found t)
      (let ((doc (documentation sym 'variable)))
        (format t "~&~A is a ~A~%"
                (string-downcase (symbol-name sym))
                (if (constantp sym) "constant" "variable"))
        (format t "  Value: ~S~%" (symbol-value sym))
        (if doc
            (format t "~%~A~%" doc)
            (format t "  (no documentation)~%"))))
    ;; Type/class documentation
    (let ((class (find-class sym nil)))
      (when class
        (setf found t)
        (let ((doc (documentation sym 'type)))
          (format t "~&~A is a ~A~%"
                  (string-downcase (symbol-name sym))
                  (class-name (class-of class)))
          (if doc
              (format t "~%~A~%" doc)
              (format t "  (no documentation)~%")))))
    ;; Not found as anything
    (unless found
      (format t "~&~A has no bindings~%" (string-downcase (symbol-name sym))))))

(defun get-arglist (sym)
  "Get the argument list for function SYM."
  #+sbcl (ignore-errors
           (funcall (find-symbol "FUNCTION-LAMBDA-LIST" :sb-introspect) sym))
  #-sbcl nil)

(defun macroexpand-all-form (form)
  "Recursively expand all macros in FORM."
  #+sbcl (ignore-errors
           (funcall (find-symbol "MACROEXPAND-ALL" :sb-cltl2) form))
  #-sbcl (labels ((expand (f)
                    (if (atom f)
                        f
                        (let ((expanded (macroexpand f)))
                          (if (eq expanded f)
                              (mapcar #'expand f)
                              (expand expanded))))))
           (expand form)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Arglist
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command (arglist args) (symbol-name)
  "Show the argument list for a function or macro.
Example: ,arglist format"
  (handler-case
      (let ((arglist (slynk-arglist symbol-name)))
        (if arglist
            (format t "~&(~A ~A)~%" (string-downcase symbol-name) arglist)
            (format *error-output* "~&No arglist found for: ~A~%" symbol-name)))
    (error (e)
      (format *error-output* "~&Error getting arglist: ~A~%" e))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Inspection
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command (inspect i) (&optional expr-string)
  "Interactively inspect the result of evaluating an expression.
Use arrow keys or j/k to navigate, Enter to drill in, b to go back, q to quit.
Without argument, inspects the last result (*).
Example: ,i
Example: ,inspect *package*
Example: ,inspect (make-hash-table)"
  (handler-case
      (run-inspector (or expr-string "*"))
    (error (e)
      (format *error-output* "~&Error inspecting: ~A~%" e))))

(define-command inspect-static (expr-string)
  "Inspect an expression (non-interactive output).
Example: ,inspect-static *package*"
  (handler-case
      (let ((inspection (slynk-inspect expr-string)))
        (if inspection
            (format-inspection inspection)
            (format *error-output* "~&No inspection result~%")))
    (error (e)
      (format *error-output* "~&Error inspecting: ~A~%" e))))

(defun slynk-inspect (expr-string)
  "Inspect expression via Slynk, returns inspection result."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:init-inspector") ,expr-string)
   *slynk-connection*))

(defun format-inspection (inspection)
  "Format and print inspection result from Slynk."
  ;; Slynk returns (:title "..." :id N :content (...))
  (when (listp inspection)
    (let ((title (getf inspection :title))
          (content (getf inspection :content)))
      (when title
        (format t "~&~A~%" title))
      (format t "~&--------------------~%")
      (when content
        (dolist (item content)
          (cond
            ((stringp item)
             (format t "~A" item))
            ((and (listp item) (eq (first item) :value))
             ;; (:value "display" id)
             (format t "~A" (second item)))
            ((and (listp item) (eq (first item) :action))
             ;; (:action "label" id)
             (format t "[~A]" (second item)))
            ((eq item :newline)
             (format t "~%"))))))))

(define-command slots (expr-string)
  "Show slots of a class instance.
Example: ,slots (find-class 'standard-class)
Example: ,slots *package*"
  (handler-case
      (let ((value (first (backend-eval-internal expr-string))))
        (if (typep value 'standard-object)
            (show-object-slots value)
            (format *error-output* "~&Not a standard-object: ~S~%" value)))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(defun show-object-slots (object)
  "Display all slots of OBJECT."
  (let* ((class (class-of object))
         (slots #+sbcl (funcall (find-symbol "CLASS-SLOTS" :sb-mop) class)
                #-sbcl nil))
    (format t "~&~A~%" (class-name class))
    (format t "~&--------------------~%")
    (if slots
        (dolist (slot slots)
          (let ((name #+sbcl (funcall (find-symbol "SLOT-DEFINITION-NAME" :sb-mop) slot)
                      #-sbcl nil))
            (when name
              (format t "  ~A: ~S~%"
                      (string-downcase (symbol-name name))
                      (if (slot-boundp object name)
                          (slot-value object name)
                          '#:unbound)))))
        (format t "  (slot introspection not available)~%"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Source Location
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command source (symbol-name)
  "Show the source location of a function or macro.
Example: ,source my-function"
  (handler-case
      (let ((locations (slynk-find-definitions symbol-name)))
        (if locations
            (open-source-for-definitions locations symbol-name)
            (format *error-output* "~&No source location found for: ~A~%" symbol-name)))
    (error (e)
      (format *error-output* "~&Error finding source: ~A~%" e))))

(defun slynk-find-definitions (name)
  "Find definitions for NAME via Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:find-definitions-for-emacs") ,name)
   *slynk-connection*))

(defun position-to-line-from-string (content position)
  "Convert character POSITION (1-based) to line number in CONTENT string."
  (when (and content position (> position 0))
    (let ((line 1))
      (loop for i from 0 below (min position (length content))
            for ch = (char content i)
            when (char= ch #\Newline)
              do (incf line))
      line)))

(defun slynk-read-file (file-path)
  "Read file contents via Slynk (for remote files).
Returns the file content as a string, or NIL if not available."
  (when *slynk-connected-p*
    (handler-case
        (slynk-client:slime-eval
         `(cl:ignore-errors
           (cl:with-open-file (s ,file-path :direction :input)
             (cl:let* ((len (cl:file-length s))
                       (str (cl:make-string len)))
               (cl:read-sequence str s)
               str)))
         *slynk-connection*)
      (error () nil))))

(defun extract-location-info (location)
  "Extract file, position, and label from a Slynk LOCATION.
Returns (values label file position) or NIL if invalid."
  ;; Location formats vary:
  ;; (label (:location (:file "...") (:position N) ...))
  ;; (label ((:file "...") (:position N) ...))
  ;; (label (:error "..."))
  (when (listp location)
    (let* ((label (first location))
           (loc-data (second location))
           file pos)
      (cond
        ;; (:location (:file ...) (:position ...))
        ((and (listp loc-data) (eq (first loc-data) :location))
         (let ((props (rest loc-data)))
           (setf file (second (assoc :file props)))
           (setf pos (second (assoc :position props)))))
        ;; ((:file ...) (:position ...)) - direct alist
        ((and (listp loc-data) (listp (first loc-data)) (eq (first (first loc-data)) :file))
         (setf file (second (assoc :file loc-data)))
         (setf pos (second (assoc :position loc-data))))
        ;; (:error "message") - skip errors
        ((and (listp loc-data) (eq (first loc-data) :error))
         (return-from extract-location-info nil))
        (t
         (return-from extract-location-info nil)))
      (when file
        (values label file pos)))))

(defun open-source-for-definitions (locations symbol-name)
  "Open Monaco source panel with all LOCATIONS for SYMBOL-NAME.
Shows a dropdown when multiple definitions exist."
  (let ((definitions nil))
    ;; Process each location
    (dolist (location locations)
      (multiple-value-bind (label file pos) (extract-location-info location)
        (when file
          (let* ((content (if (probe-file file)
                              (uiop:read-file-string file)
                              (slynk-read-file file)))
                 (line (when (and content pos)
                         (position-to-line-from-string content pos))))
            (when content
              (push (list :label label
                          :path (if (pathnamep file) (namestring file) file)
                          :content content
                          :line line)
                    definitions))))))
    ;; Send to browser
    (if definitions
        (icl::open-source-panel-multi symbol-name (nreverse definitions))
        (format *error-output* "~&No accessible source files found~%"))))

(defun open-source-for-definition (location symbol-name)
  "Open Monaco source panel for a source LOCATION from Slynk."
  ;; Location formats vary:
  ;; (label (:location (:file "...") (:position N) ...))
  ;; (label ((:file "...") (:position N) ...))
  ;; (label (:error "..."))
  (when (listp location)
    (let* ((label (first location))
           (loc-data (second location))
           file pos)
      ;; Extract file and position
      (cond
        ;; (:location (:file ...) (:position ...))
        ((and (listp loc-data) (eq (first loc-data) :location))
         (let ((props (rest loc-data)))
           (setf file (second (assoc :file props)))
           (setf pos (second (assoc :position props)))))
        ;; ((:file ...) (:position ...)) - direct alist
        ((and (listp loc-data) (listp (first loc-data)) (eq (first (first loc-data)) :file))
         (setf file (second (assoc :file loc-data)))
         (setf pos (second (assoc :position loc-data))))
        ;; (:error "message")
        ((and (listp loc-data) (eq (first loc-data) :error))
         (format *error-output* "~&~A: ~A~%" label (second loc-data))
         (return-from open-source-for-definition nil))
        (t
         (format *error-output* "~&Unknown location format: ~S~%" loc-data)
         (return-from open-source-for-definition nil)))
      ;; Open the source panel - try local first, then remote via Slynk
      (cond
        ((null file)
         (format *error-output* "~&No file path in location~%"))
        (t
         (let* ((content (if (probe-file file)
                             ;; Local file available
                             (uiop:read-file-string file)
                             ;; Try fetching via Slynk (remote file)
                             (slynk-read-file file)))
                (line (when (and content pos)
                        (position-to-line-from-string content pos))))
           (if content
               (icl::open-source-panel symbol-name file :line line :content content)
               (format *error-output* "~&File not accessible: ~A~%" file))))))))

(defun format-source-location (location)
  "Format and print a source location from Slynk."
  ;; Location formats vary:
  ;; (label (:location (:file "...") (:position N) ...))
  ;; (label ((:file "...") (:position N) ...))
  ;; (label (:error "..."))
  (when (listp location)
    (let ((label (first location))
          (loc-data (second location)))
      (format t "~&~A:~%" label)
      (cond
        ;; (:location (:file ...) (:position ...))
        ((and (listp loc-data) (eq (first loc-data) :location))
         (let* ((props (rest loc-data))
                (file (second (assoc :file props)))
                (pos (second (assoc :position props))))
           (when file
             (format t "  File: ~A~%" file))
           (when pos
             (format t "  Position: ~A~%" pos))))
        ;; ((:file ...) (:position ...)) - direct alist
        ((and (listp loc-data) (listp (first loc-data)) (eq (first (first loc-data)) :file))
         (let ((file (second (assoc :file loc-data)))
               (pos (second (assoc :position loc-data))))
           (when file
             (format t "  File: ~A~%" file))
           (when pos
             (format t "  Position: ~A~%" pos))))
        ;; (:error "message")
        ((and (listp loc-data) (eq (first loc-data) :error))
         (format t "  ~A~%" (second loc-data)))
        (t
         (format t "  ~S~%" loc-data))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Cross-Reference (Xref)
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command (callers xc) (symbol-name)
  "Show functions that call SYMBOL-NAME.
Example: ,callers mapcar"
  (handler-case
      (let ((callers (slynk-list-callers (make-symbol (string-upcase symbol-name)))))
        (if callers
            (progn
              (format t "~&Functions that call ~A:~%" (string-upcase symbol-name))
              (dolist (caller callers)
                (format-xref-entry caller)))
            (format t "~&No callers found for: ~A~%" symbol-name)))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(define-command (callees xe) (symbol-name)
  "Show functions called by SYMBOL-NAME.
Example: ,callees my-function"
  (handler-case
      (let ((callees (slynk-list-callees (make-symbol (string-upcase symbol-name)))))
        (if callees
            (progn
              (format t "~&Functions called by ~A:~%" (string-upcase symbol-name))
              (dolist (callee callees)
                (format-xref-entry callee)))
            (format t "~&No callees found for: ~A~%" symbol-name)))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(define-command (references xr) (symbol-name)
  "Show code that references variable SYMBOL-NAME.
Example: ,references *standard-output*"
  (handler-case
      (let ((refs (slynk-who-references (make-symbol (string-upcase symbol-name)))))
        (if refs
            (progn
              (format t "~&References to ~A:~%" (string-upcase symbol-name))
              (dolist (ref refs)
                (format-xref-entry ref)))
            (format t "~&No references found for: ~A~%" symbol-name)))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(defun format-xref-entry (entry)
  "Format an xref entry for display.
   Entry format varies: (name location) or just name."
  (cond
    ((and (listp entry) (>= (length entry) 2))
     (let ((name (first entry))
           (loc (second entry)))
       (format t "  ~A" name)
       (when (and (listp loc) (eq (first loc) :location))
         (let* ((props (rest loc))
                (file (second (assoc :file props))))
           (when file
             (format t " (~A)" (file-namestring file)))))
       (format t "~%")))
    ((symbolp entry)
     (format t "  ~A~%" entry))
    (t
     (format t "  ~S~%" entry))))

(defun byte-offset-to-line (file offset)
  "Convert byte OFFSET to line number in FILE.
Returns 1 if file cannot be read or offset is invalid."
  (handler-case
      (with-open-file (s file :element-type '(unsigned-byte 8))
        (loop with line-num = 1
              for byte-pos from 0 below offset
              for byte = (read-byte s nil nil)
              while byte
              when (= byte 10)  ; newline
              do (incf line-num)
              finally (return line-num)))
    (error () 1)))

(defun default-editor ()
  "Return the default editor command for the current platform."
  (or (uiop:getenv "EDITOR")
      (uiop:getenv "VISUAL")
      #+windows "notepad"
      #+darwin "open"
      #-(or windows darwin) "vi"))

(defun editor-open-command (editor file line)
  "Return the command list to open FILE at LINE in EDITOR."
  (let ((editor-name (pathname-name (pathname editor))))
    (cond
      ;; macOS open command - use -t for text editor
      ((string-equal editor "open")
       (list "open" "-t" file))
      ;; Windows notepad doesn't support line numbers
      ((string-equal editor-name "notepad")
       (list editor file))
      ;; VS Code
      ((or (string-equal editor-name "code")
           (string-equal editor-name "code-insiders"))
       (list editor "--goto" (format nil "~A:~D" file line)))
      ;; Most editors support +LINE syntax (vim, emacs, nano, micro, etc.)
      (t
       (list editor (format nil "+~D" line) file)))))

(define-command (edit ed) (symbol-name)
  "Open the source file for a symbol in $EDITOR.
Uses $EDITOR or $VISUAL environment variable, or platform default.
Example: ,edit my-function"
  (handler-case
      (let ((locations (slynk-find-definitions symbol-name)))
        (if locations
            ;; Use the first location (usually the main definition)
            (let* ((loc (first locations))
                   (loc-data (second loc)))
              (multiple-value-bind (file pos)
                  (extract-file-and-position loc-data)
                (if file
                    (if (probe-file file)
                        (let* ((line (if pos (byte-offset-to-line file pos) 1))
                               (editor (default-editor))
                               (cmd (editor-open-command editor file line)))
                          (format t "~&Opening ~A at line ~D...~%" file line)
                          (force-output)
                          ;; Use :interactive to connect editor to terminal
                          (uiop:run-program cmd
                                            :input :interactive
                                            :output :interactive
                                            :ignore-error-status t))
                        (format *error-output* "~&File not found: ~A~%" file))
                    (format *error-output* "~&No file location for: ~A~%" symbol-name))))
            (format *error-output* "~&No source location found for: ~A~%" symbol-name)))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(defun extract-file-and-position (loc-data)
  "Extract file path and position from Slynk location data.
Returns (VALUES file position) or (VALUES NIL NIL)."
  (cond
    ;; (:location (:file ...) (:position ...))
    ((and (listp loc-data) (eq (first loc-data) :location))
     (let* ((props (rest loc-data))
            (file (second (assoc :file props)))
            (pos (second (assoc :position props))))
       (values file pos)))
    ;; ((:file ...) (:position ...)) - direct alist
    ((and (listp loc-data) (listp (first loc-data)) (eq (first (first loc-data)) :file))
     (let ((file (second (assoc :file loc-data)))
           (pos (second (assoc :position loc-data))))
       (values file pos)))
    (t
     (values nil nil))))

(defun show-source-location (sym)
  "Show source location for symbol SYM (local/SBCL)."
  #+sbcl
  (let ((sources (ignore-errors
                   (funcall (find-symbol "FIND-DEFINITION-SOURCES-BY-NAME"
                                         :sb-introspect)
                            sym :function))))
    (if sources
        (dolist (source sources)
          (let ((file (ignore-errors
                        (funcall (find-symbol "DEFINITION-SOURCE-PATHNAME"
                                              :sb-introspect)
                                 source)))
                (form-num (ignore-errors
                            (funcall (find-symbol "DEFINITION-SOURCE-FORM-NUMBER"
                                                  :sb-introspect)
                                     source))))
            (format t "~&~A~%" (string-downcase (symbol-name sym)))
            (if file
                (progn
                  (format t "  File: ~A~%" file)
                  (when form-num
                    (format t "  Form: ~A~%" form-num)))
                (format t "  (no source file recorded)~%"))))
        (format *error-output* "~&No source location found for: ~A~%" sym)))
  #-sbcl
  (format *error-output* "~&Source location not available on this implementation~%"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Tracing
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command (trace tr) (symbol-name)
  "Enable tracing for a function.
Example: ,trace my-function"
  (let ((trimmed (string-trim '(#\Space #\Tab) symbol-name)))
    (if (and (> (length trimmed) 0) (char= (char trimmed 0) #\())
        (progn
          (format *error-output* "~&Error: ,trace expects a symbol name, not a form.~%")
          (format *error-output* "~&Usage: ,trace ~A~%"
                  (string-trim '(#\( #\) #\Space) trimmed)))
        (handler-case
            (let ((result (slynk-toggle-trace symbol-name)))
              (format t "~&~A~%" result))
          (error (e)
            (format *error-output* "~&Error: ~A~%" e))))))

(define-command (untrace untr) (symbol-name)
  "Disable tracing for a function.
Example: ,untrace my-function"
  (let ((trimmed (string-trim '(#\Space #\Tab) symbol-name)))
    (if (and (> (length trimmed) 0) (char= (char trimmed 0) #\())
        (progn
          (format *error-output* "~&Error: ,untrace expects a symbol name, not a form.~%")
          (format *error-output* "~&Usage: ,untrace ~A~%"
                  (string-trim '(#\( #\) #\Space) trimmed)))
        (handler-case
            (let ((result (slynk-untrace symbol-name)))
              (format t "~&~A~%" result))
          (error (e)
            (format *error-output* "~&Error: ~A~%" e))))))

(define-command (untrace-all untr-all) ()
  "Disable all tracing.
Example: ,untrace-all"
  (handler-case
      (let ((result (slynk-untrace-all)))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(defun slynk-toggle-trace (name)
  "Toggle tracing for NAME via Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:slynk-toggle-trace") ,name)
   *slynk-connection*))

(defun slynk-untrace (name)
  "Untrace NAME via Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  ;; Use read-from-string in remote Lisp to handle symbols from remote-only packages
  (let ((code (format nil "(cl:untrace ~A)" (string-upcase name))))
    (slynk-client:slime-eval
     `(cl:eval (cl:read-from-string ,code))
     *slynk-connection*))
  (format nil "~A is now untraced." name))

(defun slynk-untrace-all ()
  "Untrace all functions via Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (slynk-client:slime-eval
   '(cl:untrace)
   *slynk-connection*))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Thread Inspection
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command threads ()
  "List all threads in the inferior Lisp.
Shows thread ID, name, and status for each thread."
  (handler-case
      (let ((thread-data (slynk-list-threads)))
        (if thread-data
            (format-thread-list thread-data)
            (format *error-output* "~&No thread information available~%")))
    (error (e)
      (format *error-output* "~&Error listing threads: ~A~%" e))))

(defun format-thread-list (thread-data)
  "Format and display thread list from Slynk.
   THREAD-DATA is (LABELS ROW1 ROW2 ...) where LABELS is (:id :name :status ...)."
  (when (and (listp thread-data) (> (length thread-data) 1))
    (let* ((labels (first thread-data))
           (rows (rest thread-data))
           (id-idx (position :id labels))
           (name-idx (position :name labels))
           (status-idx (position :status labels)))
      ;; Print header
      (format t "~&~A~%" (colorize "Threads:" *color-cyan*))
      (format t "~&~60,,,'-A~%" "")
      ;; Print each thread
      (dolist (row rows)
        (let ((id (when id-idx (nth id-idx row)))
              (name (when name-idx (nth name-idx row)))
              (status (when status-idx (nth status-idx row))))
          (format t "~&~A ~A~%"
                  (colorize (format nil "[~2D]" id) *color-number*)
                  (colorize (or name "unnamed") *color-symbol*))
          (format t "     Status: ~A~%"
                  (colorize (or status "unknown")
                            (if (string-equal status "Active")
                                *color-green*
                                *color-dim*)))))
      (format t "~&~60,,,'-A~%" "")
      (format t "~&~A threads total~%" (length rows)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Debugger Commands
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command (bt backtrace) ()
  "Show the backtrace from the last error.
When an error occurs during evaluation, ICL captures the backtrace.
Use this command to view it after seeing an error message."
  (cond
    (*last-error-backtrace*
     (format t "~&~A~%" (colorize "Last error:" *color-red*))
     (format t "~&  ~A~%~%" (colorize *last-error-condition* *color-yellow*))
     (format t "~A~%" (colorize "Backtrace:" *color-cyan*))
     (format t "~&~60,,,'-A~%" "")
     (format t "~A~%" *last-error-backtrace*))
    (*last-error-condition*
     (format t "~&~A~%" (colorize "Last error:" *color-red*))
     (format t "~&  ~A~%~%" (colorize *last-error-condition* *color-yellow*))
     (format *error-output* "~&No backtrace available (non-SBCL backend?)~%"))
    (t
     (format t "~&No error backtrace recorded.~%")
     (format t "~&Backtraces are captured when evaluation errors occur.~%"))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Time Command
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command time (form-string)
  "Time the evaluation of a form.
Shows real time, run time, and memory allocation.
Example: ,time (dotimes (i 1000000) (sqrt i))"
  (handler-case
      (let ((result (slynk-time form-string)))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(defun slynk-time (form-string)
  "Evaluate FORM-STRING with timing via Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (let ((wrapper (format nil "(with-output-to-string (*trace-output*) (time (progn ~A nil)))" form-string)))
    (slynk-client:slime-eval
     `(cl:eval (cl:read-from-string ,wrapper))
     *slynk-connection*)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Load Command
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command (load ld) (filename)
  "Load a Lisp file.
Example: ,load myfile.lisp
Example: ,load /path/to/file.lisp"
  (handler-case
      (let* ((expanded (expand-filename filename))
             (result (slynk-load-file expanded)))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error loading file: ~A~%" e))))

(defun expand-filename (filename)
  "Expand ~ in FILENAME to home directory.
   Handles ~, ~/, and ~/path. Does not support ~user syntax."
  (let ((name (string-trim '(#\Space #\Tab #\") filename)))
    (cond
      ;; Empty string
      ((zerop (length name)) name)
      ;; Doesn't start with ~
      ((char/= (char name 0) #\~) name)
      ;; Just "~"
      ((= (length name) 1)
       (user-homedir-pathname))
      ;; "~/" or "~/..."
      ((char= (char name 1) #\/)
       (merge-pathnames (subseq name 2) (user-homedir-pathname)))
      ;; "~user" syntax - not supported, return as-is
      (t name))))

(defun slynk-load-file (filename)
  "Load FILENAME via Slynk with streaming output."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (let ((namestring (namestring (truename filename))))
    ;; Use backend-eval for streaming output support
    (backend-eval (format nil "(cl:load ~S)" namestring))
    (format nil "Loaded ~A" namestring)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Compile Command
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command (compile-file cf) (filename)
  "Compile a Lisp file.
Example: ,compile-file myfile.lisp"
  (handler-case
      (let* ((expanded (expand-filename filename))
             (result (slynk-compile-file expanded)))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error compiling file: ~A~%" e))))

(defun slynk-compile-file (filename)
  "Compile FILENAME via Slynk with streaming output."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (let ((namestring (namestring (truename filename))))
    ;; Use backend-eval for streaming output support
    (backend-eval (format nil "(cl:compile-file ~S)" namestring))
    (format nil "Compiled ~A" namestring)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Disassemble Command
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command (disassemble dis) (symbol-name)
  "Disassemble a function.
Example: ,disassemble my-function
Example: ,dis mapcar"
  (handler-case
      (let ((result (slynk-disassemble symbol-name)))
        (if result
            (progn
              ;; Store for ,explain
              (setf *last-command-output* result
                    *last-command-name* (format nil "disassemble ~A" symbol-name))
              (format t "~&~A~%" result))
            (format *error-output* "~&No disassembly available for: ~A~%" symbol-name)))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(defun slynk-disassemble (name)
  "Disassemble function NAME via Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (slynk-client:slime-eval
   `(cl:with-output-to-string (cl:*standard-output*)
      (cl:disassemble (cl:quote ,(read-from-string (string-upcase name)))))
   *slynk-connection*))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Profiling Commands
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command profile (form-string)
  "Profile a form using the statistical profiler.
Shows CPU time spent in each function.
Example: ,profile (dotimes (i 100000) (sqrt i))
Example: ,profile (my-expensive-function)"
  (handler-case
      (let ((result (slynk-profile form-string)))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error profiling: ~A~%" e))))

(defun slynk-profile (form-string)
  "Profile FORM-STRING using the statistical profiler via Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  ;; First, ensure sb-sprof is loaded
  (slynk-client:slime-eval
   '(cl:eval (cl:read-from-string "(require 'sb-sprof)"))
   *slynk-connection*)
  ;; Now run with profiling - package exists so we can reference it
  ;; Use :threads :all to capture slynk worker threads
  ;; Use small sample interval (1ms) for better coverage of fast operations
  (let ((wrapper (format nil "
(progn
  #+sbcl
  (progn
    (sb-sprof:with-profiling (:report :flat :loop nil :threads :all :sample-interval 0.001)
      ~A)
    (with-output-to-string (*standard-output*)
      (sb-sprof:report :type :flat)))
  #-sbcl
  \"Profiling only available on SBCL\")" form-string)))
    (slynk-client:slime-eval
     `(cl:eval (cl:read-from-string ,wrapper))
     *slynk-connection*)))

(define-command (profile-start ps) ()
  "Start the statistical profiler for ongoing profiling.
Use ,profile-stop to stop and see results.
Example: ,profile-start"
  (handler-case
      (let ((result (slynk-profile-start)))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(defun slynk-profile-start ()
  "Start statistical profiler via Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (slynk-client:slime-eval
   `(cl:eval (cl:read-from-string
              "(progn (require 'sb-sprof) (funcall (find-symbol \"START-PROFILING\" \"SB-SPROF\") :threads :all :sample-interval 0.001) \"Profiler started\")"))
   *slynk-connection*))

(define-command (profile-stop pst) ()
  "Stop the statistical profiler and show results.
Example: ,profile-stop"
  (handler-case
      (let ((result (slynk-profile-stop)))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(defun slynk-profile-stop ()
  "Stop statistical profiler and get report via Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (slynk-client:slime-eval
   `(cl:eval (cl:read-from-string
              "(progn
                 (funcall (find-symbol \"STOP-PROFILING\" \"SB-SPROF\"))
                 (with-output-to-string (*standard-output*)
                   (funcall (find-symbol \"REPORT\" \"SB-SPROF\") :type :flat)))"))
   *slynk-connection*))

(define-command profile-reset ()
  "Reset the profiler, clearing all collected data.
Example: ,profile-reset"
  (handler-case
      (let ((result (slynk-profile-reset)))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(defun slynk-profile-reset ()
  "Reset profiler via Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (slynk-client:slime-eval
   `(cl:eval (cl:read-from-string
              "(progn (funcall (find-symbol \"RESET\" \"SB-SPROF\")) \"Profiler reset\")"))
   *slynk-connection*))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Flame Graph Profiling (Speedscope)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun run-flame-profiler (form-string mode mode-description)
  "Run flame graph profiler with MODE and display results.
MODE-DESCRIPTION is a human-readable description like \"CPU\" or \"wall-clock\"."
  (handler-case
      (progn
        ;; Ensure browser server is running
        (unless *browser-acceptor*
          (format t "~&Starting browser server...~%")
          (start-browser :open-browser nil))
        (format t "~&Profiling (~A)...~%" mode-description)
        (multiple-value-bind (profile-id has-samples) (profile-and-store form-string :mode mode :name form-string)
          (if has-samples
              ;; Check if browser has connected clients
              (if (and *repl-resource*
                       (hunchensocket:clients *repl-resource*))
                  ;; Open in Dockview panel
                  (progn
                    (format t "~&Opening flame graph in panel...~%")
                    (open-speedscope-panel profile-id form-string))
                  ;; Fall back to opening a new browser tab
                  (let ((url (format nil "http://127.0.0.1:~A/speedscope/index.html#profileURL=/profile-data/~A.json"
                                     *browser-port* profile-id)))
                    (format t "~&Opening flame graph: ~A~%" url)
                    (ignore-errors (uiop:run-program (list "xdg-open" url)))))
              ;; No samples collected
              (format t "~&No profiling samples collected. The code may have executed too quickly.~%Try profiling longer-running code or increasing iterations.~%"))))
    (error (e)
      (format *error-output* "~&Error profiling: ~A~%" e))))

(define-command (flame flamegraph fg) (form-string)
  "Profile a form using CPU sampling and display interactive flame graph.
Samples when the CPU is active. Best for compute-bound code.
For I/O-bound code (network, disk), use ,flame-time instead.
Example: ,flame (dotimes (i 100000) (sqrt i))"
  (run-flame-profiler form-string :cpu "CPU"))

(define-command (flame-time ftime ft) (form-string)
  "Profile a form using wall-clock sampling and display interactive flame graph.
Samples based on elapsed time, including I/O wait. Best for I/O-bound code.
Example: ,flame-time (do-libyear)
Example: ,ftime (dex:fetch \"https://example.com\")"
  (run-flame-profiler form-string :time "wall-clock"))

(define-command (flame-alloc falloc fa) (form-string)
  "Profile a form by sampling on memory allocation and display flame graph.
Samples when memory is allocated. Best for finding allocation hotspots.
Example: ,flame-alloc (make-list 1000000)"
  (run-flame-profiler form-string :alloc "allocation"))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Code Coverage (sb-cover)
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command cover-start ()
  "Enable code coverage instrumentation for subsequent compilations.
Code compiled after this will have coverage data collected.
Use ,cover-report to view results, ,cover-reset to clear data.
SBCL only.
Example: ,cover-start"
  (handler-case
      (let ((result (first (backend-coverage-start))))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(define-command cover-stop ()
  "Disable code coverage instrumentation for subsequent compilations.
Previously instrumented code continues to collect coverage data.
SBCL only.
Example: ,cover-stop"
  (handler-case
      (let ((result (first (backend-coverage-stop))))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(define-command cover-reset ()
  "Clear all collected coverage data.
SBCL only.
Example: ,cover-reset"
  (handler-case
      (let ((result (first (backend-coverage-reset))))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(define-command (cover-load cl) (filepath)
  "Load a file with coverage instrumentation enabled.
The file will be compiled with coverage data collection.
Use ,cover-report to view results after running the code.
SBCL only.
Example: ,cover-load \"mylib.lisp\"
Example: ,cl tests/test-utils.lisp"
  (handler-case
      (progn
        ;; Ensure browser server is running for report display
        (unless *browser-acceptor*
          (format t "~&Starting browser server...~%")
          (start-browser :open-browser nil))
        (let ((result (first (backend-coverage-load-file filepath))))
          (format t "~&~A~%" result)
          (format t "~&Run your code, then use ,cover-report to see coverage.~%")))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(define-command (cover-ql cql) (system-name)
  "Load a system with coverage instrumentation enabled.
Uses ocicl, Quicklisp, or ASDF (whichever is available).
Use ,cover-report to view results after running the code.
SBCL only.
Example: ,cover-ql my-library
Example: ,cql alexandria"
  (handler-case
      (progn
        ;; Ensure browser server is running for report display
        (unless *browser-acceptor*
          (format t "~&Starting browser server...~%")
          (start-browser :open-browser nil))
        (backend-coverage-load-system system-name)
        (format t "~&Run your code, then use ,cover-report to see coverage.~%"))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(define-command cover-report ()
  "Generate and display coverage report with syntax highlighting.
Shows which lines of instrumented code were executed.
SBCL only.
Example: ,cover-report"
  (handler-case
      (progn
        ;; Ensure browser server is running
        (unless *browser-acceptor*
          (format t "~&Starting browser server...~%")
          (start-browser :open-browser nil))
        (format t "~&Generating coverage report...~%")
        (if (and *repl-resource*
                 (hunchensocket:clients *repl-resource*))
            ;; Open Monaco-style panel with syntax highlighting
            (progn
              (format t "~&Opening coverage report in panel...~%")
              (open-coverage-panel "Coverage Report" t))
            ;; Fall back to legacy HTML report in browser tab
            (progn
              (backend-coverage-report)  ; Generate HTML report
              (let ((url (format nil "http://127.0.0.1:~A/coverage/cover-index.html"
                                 *browser-port*)))
                (format t "~&Opening coverage report: ~A~%" url)
                (ignore-errors (uiop:run-program (list "xdg-open" url)))))))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Data Visualization
;;; ─────────────────────────────────────────────────────────────────────────────

(defun split-viz-expressions (input)
  "Split INPUT into separate expressions for ,viz command.
   Handles simple tokens and parenthesized expressions."
  (let ((result nil)
        (current "")
        (paren-depth 0)
        (in-string nil)
        (escape-next nil))
    (loop for char across input do
      (cond
        (escape-next
         (setf current (concatenate 'string current (string char)))
         (setf escape-next nil))
        ((char= char #\\)
         (setf current (concatenate 'string current (string char)))
         (setf escape-next t))
        ((char= char #\")
         (setf current (concatenate 'string current (string char)))
         (setf in-string (not in-string)))
        (in-string
         (setf current (concatenate 'string current (string char))))
        ((char= char #\()
         (setf current (concatenate 'string current (string char)))
         (incf paren-depth))
        ((char= char #\))
         (setf current (concatenate 'string current (string char)))
         (decf paren-depth))
        ((and (member char '(#\Space #\Tab)) (zerop paren-depth))
         (let ((trimmed (string-trim '(#\Space #\Tab) current)))
           (when (plusp (length trimmed))
             (push trimmed result)))
         (setf current ""))
        (t
         (setf current (concatenate 'string current (string char))))))
    ;; Don't forget the last token
    (let ((trimmed (string-trim '(#\Space #\Tab) current)))
      (when (plusp (length trimmed))
        (push trimmed result)))
    (nreverse result)))

(define-command viz (&rest expressions)
  "Visualize data in the browser based on its type.
Evaluates the expression and displays an appropriate visualization:
  - Function → disassembly listing
  - Symbol (class name) → class hierarchy graph with slots
  - Hash-table → key-value table
  - FSet map → key-value table
  - FSet bag → element-count table (multiset)
  - FSet set → Venn diagram (single or multiple sets)
Requires browser mode to be active.
Examples:
  ,viz #'car                ; disassembly of CAR
  ,viz #'my-function        ; disassembly of MY-FUNCTION
  ,viz 'hash-table          ; class hierarchy for HASH-TABLE
  ,viz 'standard-object     ; class hierarchy for STANDARD-OBJECT
  ,viz *my-hash-table*      ; hash-table contents
  ,viz *my-map*             ; FSet map contents
  ,viz *my-bag*             ; FSet bag with element counts
  ,viz *my-set*             ; single FSet set as circle
  ,viz *set-a* *set-b*      ; two FSet sets as Venn diagram"
  ;; Always check for new library visualizations before processing
  (process-library-visualizations)
  (cond
    ((not *browser-terminal-active*)
     (format t "~&; Browser not active. Use ,browser to start.~%"))
    ((null expressions)
     (format t "~&; Usage: ,viz <expression> [<expression2>]~%"))
    (t
     (let ((num-exprs (length expressions)))
       (cond
         ;; Multiple expressions - try as FSet sets for Venn diagram
         ((> num-exprs 1)
          (viz-fset-sets expressions))
         ;; Single expression - detect type
         ((= num-exprs 1)
          (let ((trimmed (first expressions)))
            (handler-case
                (viz-single-expression trimmed)
              (error (e)
                (format *error-output* "~&; Error: ~A~%" e)))))
         (t
          (format t "~&; Usage: ,viz <expression>~%")))))))

(defun viz-single-expression (trimmed)
  "Visualize a single expression - detect type and dispatch."
  ;; Query backend to determine type and get visualization data
  ;; First checks for custom icl-runtime:visualize method, then falls back to built-in detection
  (let* ((query (format nil "(let* ((obj ~A)
                                (fset-pkg (find-package :fset))
                                (custom-viz (and (find-package :icl-runtime)
                                                 (fboundp 'icl-runtime:visualize)
                                                 (icl-runtime:visualize obj))))
                           (if custom-viz
                               custom-viz
                               (flet ((fset-sym (name)
                                        (and fset-pkg (intern name fset-pkg)))
                                      (fset-fn (name)
                                        (and fset-pkg
                                             (let ((s (intern name fset-pkg)))
                                               (and (fboundp s) (symbol-function s))))))
                               (let ((set?-fn (fset-fn \"SET?\"))
                                     (map?-fn (fset-fn \"MAP?\"))
                                     (bag?-fn (fset-fn \"BAG?\"))
                                     (convert-fn (fset-fn \"CONVERT\"))
                                     (multiplicity-fn (fset-fn \"MULTIPLICITY\")))
                                 (cond
                                   ((symbolp obj)
                                    (if (find-class obj nil)
                                        (list :class
                                              (symbol-name obj)
                                              (package-name (symbol-package obj)))
                                        (list :symbol (princ-to-string obj))))
                                   ((hash-table-p obj)
                                    (list :hash-table
                                          (hash-table-count obj)
                                          (loop for k being the hash-keys of obj using (hash-value v)
                                                for i from 0 below 100
                                                collect (list (princ-to-string k)
                                                              (princ-to-string v)))))
                                   ;; FSet map - key/value pairs
                                   ((and map?-fn (funcall map?-fn obj))
                                    (let ((alist (funcall convert-fn 'list obj)))
                                      (list :fset-map
                                            (length alist)
                                            (loop for (k . v) in alist
                                                  for i from 0 below 100
                                                  collect (list (princ-to-string k)
                                                                (princ-to-string v))))))
                                   ;; FSet bag - element/count pairs
                                   ((and bag?-fn (funcall bag?-fn obj))
                                    (let* ((elements (remove-duplicates
                                                       (funcall convert-fn 'list obj)
                                                       :test #'equal))
                                           (pairs (loop for e in elements
                                                        for i from 0 below 100
                                                        collect (list (princ-to-string e)
                                                                      (funcall multiplicity-fn obj e)))))
                                      (list :fset-bag
                                            (length elements)
                                            pairs)))
                                   ;; FSet set - members
                                   ((and set?-fn (funcall set?-fn obj))
                                    (let ((members (funcall convert-fn 'list obj)))
                                      (list :fset-set
                                            (length members)
                                            (loop for m in members
                                                  for i from 0 below 100
                                                  collect (princ-to-string m)))))
                                   ;; Function - show disassembly
                                   ((functionp obj)
                                    (let* ((name (or (ignore-errors
                                                       (sb-impl::%fun-name obj))
                                                     \"anonymous\"))
                                           (disasm (with-output-to-string (*standard-output*)
                                                     (disassemble obj))))
                                      (list :function (princ-to-string name) disasm)))
                                   ;; SVG string detection
                                   ((and (stringp obj)
                                         (let ((trimmed (string-left-trim '(#\\Space #\\Tab #\\Newline) obj)))
                                           (or (and (>= (length trimmed) 5)
                                                    (string-equal (subseq trimmed 0 5) \"<?xml\"))
                                               (and (>= (length trimmed) 4)
                                                    (string-equal (subseq trimmed 0 4) \"<svg\")))))
                                    (list :svg obj))
                                   ;; HTML string detection
                                   ((and (stringp obj)
                                         (let ((trimmed (string-left-trim '(#\\Space #\\Tab #\\Newline) obj)))
                                           (or (and (>= (length trimmed) 9)
                                                    (string-equal (subseq trimmed 0 9) \"<!DOCTYPE\"))
                                               (and (>= (length trimmed) 5)
                                                    (string-equal (subseq trimmed 0 5) \"<html\")))))
                                    (list :html obj))
                                   ;; Vega-Lite spec detection (JSON with $schema containing vega-lite, or mark+encoding keys)
                                   ((and (stringp obj)
                                         (let ((trimmed (string-left-trim '(#\\Space #\\Tab #\\Newline) obj)))
                                           (and (> (length trimmed) 0)
                                                (char= (char trimmed 0) #\\{)
                                                (or (search \"vega-lite\" obj :test #'char-equal)
                                                    (and (search \"\\\"mark\\\"\" obj)
                                                         (search \"\\\"encoding\\\"\" obj))))))
                                    (list :vega-lite obj))
                                   ;; Mermaid diagram detection (common diagram type keywords)
                                   ((and (stringp obj)
                                         (let ((trimmed (string-left-trim '(#\\Space #\\Tab #\\Newline) obj)))
                                           (and (> (length trimmed) 0)
                                                (or (and (>= (length trimmed) 5) (string= (subseq trimmed 0 5) \"graph\"))
                                                    (and (>= (length trimmed) 9) (string= (subseq trimmed 0 9) \"flowchart\"))
                                                    (and (>= (length trimmed) 15) (string= (subseq trimmed 0 15) \"sequenceDiagram\"))
                                                    (and (>= (length trimmed) 12) (string= (subseq trimmed 0 12) \"classDiagram\"))
                                                    (and (>= (length trimmed) 12) (string= (subseq trimmed 0 12) \"stateDiagram\"))
                                                    (and (>= (length trimmed) 9) (string= (subseq trimmed 0 9) \"erDiagram\"))
                                                    (and (>= (length trimmed) 5) (string= (subseq trimmed 0 5) \"gantt\"))
                                                    (and (>= (length trimmed) 3) (string= (subseq trimmed 0 3) \"pie\"))
                                                    (and (>= (length trimmed) 7) (string= (subseq trimmed 0 7) \"journey\"))
                                                    (and (>= (length trimmed) 8) (string= (subseq trimmed 0 8) \"gitgraph\"))
                                                    (and (>= (length trimmed) 7) (string= (subseq trimmed 0 7) \"mindmap\"))
                                                    (and (>= (length trimmed) 8) (string= (subseq trimmed 0 8) \"timeline\"))))))
                                    (list :mermaid obj))
                                   ;; JSON string detection
                                   ((and (stringp obj)
                                         (let ((trimmed (string-left-trim '(#\\Space #\\Tab #\\Newline) obj)))
                                           (and (> (length trimmed) 0)
                                                (or (char= (char trimmed 0) #\\{)
                                                    (char= (char trimmed 0) #\\[)))))
                                    (list :json obj))
                                   ;; Image byte array detection (check magic bytes)
                                   ((and (typep obj '(simple-array (unsigned-byte 8) (*)))
                                         (>= (length obj) 4)
                                         (or ;; PNG: 89 50 4E 47
                                             (and (= (aref obj 0) #x89)
                                                  (= (aref obj 1) #x50)
                                                  (= (aref obj 2) #x4E)
                                                  (= (aref obj 3) #x47))
                                             ;; JPEG: FF D8 FF
                                             (and (= (aref obj 0) #xFF)
                                                  (= (aref obj 1) #xD8)
                                                  (= (aref obj 2) #xFF))
                                             ;; GIF: GIF8
                                             (and (= (aref obj 0) #x47)
                                                  (= (aref obj 1) #x49)
                                                  (= (aref obj 2) #x46)
                                                  (= (aref obj 3) #x38))
                                             ;; WebP: RIFF....WEBP
                                             (and (>= (length obj) 12)
                                                  (= (aref obj 0) #x52)
                                                  (= (aref obj 1) #x49)
                                                  (= (aref obj 2) #x46)
                                                  (= (aref obj 3) #x46)
                                                  (= (aref obj 8) #x57)
                                                  (= (aref obj 9) #x45)
                                                  (= (aref obj 10) #x42)
                                                  (= (aref obj 11) #x50))))
                                    (let ((mime (cond
                                                  ((and (= (aref obj 0) #x89) (= (aref obj 1) #x50)) \"image/png\")
                                                  ((and (= (aref obj 0) #xFF) (= (aref obj 1) #xD8)) \"image/jpeg\")
                                                  ((and (= (aref obj 0) #x47) (= (aref obj 1) #x49)) \"image/gif\")
                                                  (t \"image/webp\"))))
                                      (list :image-bytes mime (icl-runtime:usb8-array-to-base64-string obj))))
                                   (t (list :unknown (type-of obj) (princ-to-string obj))))))))"
                        trimmed))
         (result (backend-eval-internal query)))
    (when (and result (listp result) (first result))
      (let ((parsed (read-from-string (first result))))
        (case (first parsed)
          (:class
           (let ((sym-name (second parsed))
                 (pkg-name (third parsed)))
             (open-class-graph-panel sym-name pkg-name)
             (format t "~&; Visualizing class hierarchy: ~A~%" sym-name)))
          (:hash-table
           (let ((count (second parsed))
                 (entries (third parsed)))
             (open-hash-table-panel trimmed count entries trimmed)
             (format t "~&; Visualizing hash-table (~A entries)~%" count)))
          (:fset-map
           (let ((count (second parsed))
                 (entries (third parsed)))
             (open-hash-table-panel (format nil "FSet Map: ~A" trimmed)
                                    count entries trimmed)
             (format t "~&; Visualizing FSet map (~A entries)~%" count)))
          (:fset-bag
           (let ((count (second parsed))
                 (entries (third parsed)))
             (open-hash-table-panel (format nil "FSet Bag: ~A" trimmed)
                                    count entries trimmed)
             (format t "~&; Visualizing FSet bag (~A unique elements)~%" count)))
          (:fset-set
           (let ((count (second parsed))
                 (members (third parsed)))
             (open-venn-panel (list trimmed) (list members) trimmed)
             (format t "~&; Visualizing FSet set (~A members)~%" count)))
          (:svg
           (let ((content (second parsed)))
             (open-svg-panel trimmed content trimmed)
             (format t "~&; Visualizing SVG content~%")))
          (:html
           (let ((content (second parsed)))
             (open-html-panel trimmed content trimmed)
             (format t "~&; Visualizing HTML content~%")))
          (:json
           (let ((content (second parsed)))
             (open-json-panel trimmed content trimmed)
             (format t "~&; Visualizing JSON content~%")))
          (:vega-lite
           (let ((spec (second parsed)))
             (open-vega-lite-panel trimmed spec trimmed)
             (format t "~&; Visualizing Vega-Lite chart~%")))
          (:mermaid
           (let ((definition (second parsed)))
             (open-mermaid-panel trimmed definition trimmed)
             (format t "~&; Visualizing Mermaid diagram~%")))
          (:regexp
           (let ((pattern (second parsed)))
             (open-regexp-panel trimmed pattern trimmed)
             (format t "~&; Visualizing regexp pattern~%")))
          (:image-bytes
           (let* ((mime (second parsed))
                  (base64 (third parsed))
                  (data-url (format nil "data:~A;base64,~A" mime base64)))
             (open-image-panel "Image" data-url mime trimmed)
             (format t "~&; Visualizing image (~A, ~A bytes)~%" mime (length base64))))
          (:function
           (let* ((name (second parsed))
                  (disasm (third parsed))
                  (escaped (with-output-to-string (s)
                             (loop for c across disasm do
                               (case c
                                 (#\< (write-string "&lt;" s))
                                 (#\> (write-string "&gt;" s))
                                 (#\& (write-string "&amp;" s))
                                 (otherwise (write-char c s))))))
                  (html (format nil "<html><head>
<style>
body { padding: 1em; margin: 0; }
h3 { margin-top: 0; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif; }
pre { margin: 0; font-family: 'SF Mono', Monaco, 'Cascadia Code', 'Roboto Mono', Consolas, monospace; font-size: 13px; line-height: 1.5; white-space: pre; overflow-x: auto; }
@media (prefers-color-scheme: dark) {
  body { background: #0d1117; color: #c9d1d9; }
  h3 { color: #58a6ff; }
}
@media (prefers-color-scheme: light) {
  body { background: #ffffff; color: #24292f; }
  h3 { color: #0969da; }
}
</style>
</head><body>
<h3>~A</h3>
<pre>~A</pre>
</body></html>" name escaped)))
             (open-html-panel (format nil "Disassembly: ~A" name) html trimmed :sanitize nil)
             (format t "~&; Visualizing disassembly of ~A~%" name)))
          (:symbol
           (format t "~&; Symbol ~A is not a class name~%" (second parsed)))
          (:unknown
           (format t "~&; Don't know how to visualize ~A: ~A~%"
                   (second parsed) (third parsed)))
          (otherwise
           (format t "~&; Unexpected result: ~S~%" parsed)))))))

(defun viz-fset-sets (expressions)
  "Visualize multiple FSet sets as a Venn diagram."
  (handler-case
      (let* ((set-count (length expressions))
             ;; Build query to get all set members and compute intersections
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
                                       (let ((members (mapcar (lambda (s)
                                                                (loop for m in (funcall (symbol-function fset-convert-sym) 'list s)
                                                                      for i from 0 below 50
                                                                      collect (princ-to-string m)))
                                                              sets)))
                                         (list :fset-venn ~D members))
                                       (list :not-sets)))"
                            expressions set-count))
             (result (backend-eval-internal query)))
        (when (and result (listp result) (first result))
          (let ((parsed (read-from-string (first result))))
            (case (first parsed)
              (:fset-venn
               (let ((count (second parsed))
                     (all-members (third parsed)))
                 (open-venn-panel expressions all-members
                                  (format nil "~{~A~^ ~}" expressions))
                 (format t "~&; Visualizing ~D FSet sets as Venn diagram~%" count)))
              (:not-sets
               (format t "~&; All expressions must be FSet sets for Venn diagram~%"))
              (otherwise
               (format t "~&; Unexpected result: ~S~%" parsed))))))
    (error (e)
      (format *error-output* "~&; Error: ~A~%" e))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Stepping/Debugging Commands
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command step (form-string)
  "Show evaluation steps for a form using trace output.
Temporarily traces key functions to show what gets called.
Example: ,step (mapcar #'1+ '(1 2 3))
Note: For full interactive stepping, connect to Slynk from SLY/SLIME."
  (handler-case
      (let ((result (slynk-step form-string)))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error during stepping: ~A~%" e))))

(defun slynk-step (form-string)
  "Show evaluation steps for FORM-STRING via Slynk using trace."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  ;; Use trace to show function calls during evaluation
  ;; Extract function names from the form and trace them temporarily
  (let ((wrapper (format nil "
(let ((*trace-output* (make-string-output-stream))
      (traced-fns nil))
  (labels ((find-functions (form)
             ;; Find function names in the form to trace
             (when (consp form)
               (let ((op (car form)))
                 (when (and (symbolp op)
                            (fboundp op)
                            (not (special-operator-p op))
                            (not (macro-function op)))
                   (pushnew op traced-fns)))
               (dolist (sub (cdr form))
                 (find-functions sub)))))
    ;; Parse and analyze the form
    (let ((parsed (read-from-string ~S)))
      (find-functions parsed)
      ;; Trace found functions
      (dolist (fn traced-fns)
        (ignore-errors (eval `(trace ,fn))))
      (unwind-protect
          (let ((result (multiple-value-list (eval parsed))))
            (let ((trace-str (get-output-stream-string *trace-output*)))
              (format nil \"~~@[Trace:~~%~~A~~%~~]Result: ~~{~~S~~^, ~~}\"
                      (if (plusp (length trace-str)) trace-str nil)
                      result)))
        ;; Untrace everything
        (dolist (fn traced-fns)
          (ignore-errors (eval `(untrace ,fn))))))))" form-string)))
    (slynk-client:slime-eval
     `(cl:eval (cl:read-from-string ,wrapper))
     *slynk-connection*)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; System Loading (trivial-system-loader style)
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command libyear ()
  "Show dependency freshness metric using OCICL.
Libyear measures how out-of-date your dependencies are.
Requires ocicl to be installed and an ocicl.csv in the current directory."
  (handler-case
      (let ((output (uiop:run-program '("ocicl" "libyear")
                                      :output :string
                                      :error-output :output
                                      :ignore-error-status t)))
        (format t "~&~A" output))
    (error (e)
      (format *error-output* "~&Error: ~A~%Is ocicl installed? See https://github.com/ocicl/ocicl~%" e))))

(define-command changes (&optional system)
  "Show LLM-generated changelog for OCICL packages.
Without argument, shows changes for all outdated dependencies.
With SYSTEM, shows changes for that specific system.
Example: ,changes
Example: ,changes alexandria"
  (handler-case
      (let* ((args (if system
                       (list "ocicl" "changes" system)
                       '("ocicl" "changes")))
             (output (uiop:run-program args
                                       :output :string
                                       :error-output :output
                                       :ignore-error-status t)))
        (format t "~&~A" output))
    (error (e)
      (format *error-output* "~&Error: ~A~%Is ocicl installed? See https://github.com/ocicl/ocicl~%" e))))

(define-command (load-system ql) (system-name)
  "Load a system using ocicl, Quicklisp, or ASDF (whichever is available).
Tries in order: ocicl (with download), Quicklisp, plain ASDF.
Example: ,load-system alexandria
Example: ,ql cl-ppcre"
  (handler-case
      (slynk-load-system system-name)
    (error (e)
      (format *error-output* "~&Error loading system: ~A~%" e))))

(defun slynk-load-system (system-name)
  "Load SYSTEM-NAME via Slynk using trivial-system-loader approach.
   Uses streaming eval so output appears incrementally."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (let* ((name (string-trim '(#\Space #\Tab #\") system-name))
         ;; Code that tries ocicl, then Quicklisp, then ASDF
         ;; Prints result directly to avoid quote issues with returned strings
         (loader-code (format nil "
(flet ((try-load ()
         (let ((system '~A))
           (cond
             ;; Try ocicl first
             ((find-package '#:OCICL-RUNTIME)
              (progv (list (find-symbol \"*DOWNLOAD*\" '#:OCICL-RUNTIME)
                           (find-symbol \"*VERBOSE*\" '#:OCICL-RUNTIME))
                  (list t nil)
                (asdf:load-system system))
              (format t \"~~&Loaded ~~A via ocicl~~%\" system))
             ;; Try Quicklisp
             ((find-package '#:QUICKLISP)
              (funcall (find-symbol \"QUICKLOAD\" '#:QUICKLISP) system :silent nil)
              (format t \"~~&Loaded ~~A via Quicklisp~~%\" system))
             ;; Fall back to plain ASDF
             ((find-package '#:ASDF)
              (asdf:load-system system)
              (format t \"~~&Loaded ~~A via ASDF~~%\" system))
             (t
              (error \"No system loader available (ocicl, Quicklisp, or ASDF)\"))))))
  (try-load)
  (values))" name)))
    (backend-eval-internal loader-code)
    (fresh-line)
    nil))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Configuration Commands
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command (reload-config rc) ()
  "Reload the user configuration file (~/.iclrc).
Example: ,reload-config"
  (let ((cfile (config-file)))
    (if (probe-file cfile)
        (handler-case
            (progn
              (load cfile :verbose nil :print nil)
              (format t "~&Reloaded ~A~%" cfile))
          (error (e)
            (format *error-output* "~&Error loading ~A: ~A~%" cfile e)))
        (format t "~&Config file not found: ~A~%" cfile))))

(define-command (show-config sc) ()
  "Show configuration file location and customization options.
Example: ,show-config"
  (format t "~&Configuration:~%")
  (format t "  Config file: ~A~%" (config-file))
  (format t "  History file: ~A~%" (history-file))
  (format t "  History size: ~D~%" *history-size*)
  (format t "~%File locations:~%")
  #+windows
  (progn
    (format t "  Config: %APPDATA%/icl/config.lisp~%")
    (format t "  History: %LOCALAPPDATA%/icl/history~%"))
  #-windows
  (progn
    (format t "  Config: $XDG_CONFIG_HOME/icl/config.lisp (default: ~~/.config/icl/)~%")
    (format t "  History: $XDG_STATE_HOME/icl/history (default: ~~/.local/state/icl/)~%"))
  (format t "~%Available customization variables:~%")
  (format t "  *default-lisp*     - Lisp implementation (default: :SBCL)~%")
  (format t "  *prompt-string*    - Prompt format (default: \"~~A> \")~%")
  (format t "  *result-prefix*    - Result prefix (default: \"=> \")~%")
  (format t "  *colors-enabled*   - Enable colors (default: T)~%")
  (format t "  *history-size*     - Max history entries (default: 1000)~%")
  (format t "  *paredit-mode*     - Structural editing (default: NIL)~%")
  (format t "  *viz-package-exclusions* - Regex list to skip in ,viz (default: NIL)~%")
  (format t "~%Available hooks:~%")
  (format t "  *before-eval-hook* - Called before evaluation~%")
  (format t "  *after-eval-hook*  - Called after evaluation~%")
  (format t "  *prompt-hook*      - Custom prompt function~%")
  (format t "  *error-hook*       - Custom error handler~%")
  (format t "~%Functions:~%")
  (format t "  (configure-lisp impl &key program args eval-arg)~%")
  (format t "~%Example config file:~%")
  (format t "  (setf icl:*default-lisp* :ccl)~%")
  (format t "  (setf icl:*prompt-string* ~S)~%" "λ ~A> ")
  (format t "  (setf icl:*colors-enabled* t)~%")
  (format t "  (setf icl:*paredit-mode* t)~%")
  (format t "~%  ;; Custom SBCL with extra memory~%")
  (format t "  (icl:configure-lisp :sbcl~%")
  (format t "    :program \"/opt/sbcl/bin/sbcl\"~%")
  (format t "    :args '(\"--dynamic-space-size\" \"8192\"))~%"))

(define-command paredit (&optional state)
  "Toggle or set paredit mode for structural editing.
When enabled: auto-closes parens/quotes, safe deletion, sexp navigation.
Keys: Alt-F (forward-sexp), Alt-B (backward-sexp)
Example: ,paredit        ; toggle
         ,paredit on     ; enable
         ,paredit off    ; disable"
  (cond
    ((null state)
     (setf *paredit-mode* (not *paredit-mode*))
     (format t "~&Paredit mode: ~A~%" (if *paredit-mode* "enabled" "disabled")))
    ((member state '("on" "t" "true" "1") :test #'string-equal)
     (setf *paredit-mode* t)
     (format t "~&Paredit mode: enabled~%"))
    ((member state '("off" "nil" "false" "0") :test #'string-equal)
     (setf *paredit-mode* nil)
     (format t "~&Paredit mode: disabled~%"))
    (t
     (format *error-output* "~&Invalid argument: ~A (use on/off)~%" state))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Theme Commands
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command theme (&optional subcommand arg)
  "Manage terminal and browser themes.
Example: ,theme                    - Show current themes
         ,theme list               - List all available themes
         ,theme terminal <name>    - Set terminal theme
         ,theme browser <name>     - Set browser theme
         ,theme auto               - Auto-detect and apply based on dark mode"
  (cond
    ;; No args - show current themes
    ((null subcommand)
     (format t "~&Current themes:~%")
     (format t "  Terminal: ~A~@[ (~A)~]~%"
             (or (current-terminal-theme-name) "none")
             (when *current-terminal-theme*
               (terminal-theme-display-name *current-terminal-theme*)))
     (format t "  Browser:  ~A~@[ (~A)~]~%"
             (or (current-browser-theme-name) "none")
             (when *current-browser-theme*
               (browser-theme-display-name *current-browser-theme*)))
     (format t "~%Use ,theme list to see available themes.~%"))

    ;; List themes
    ((string-equal subcommand "list")
     (format t "~&Terminal themes:~%")
     (let ((dark-themes nil)
           (light-themes nil))
       (dolist (theme (list-terminal-themes))
         (if (terminal-theme-dark-p theme)
             (push theme dark-themes)
             (push theme light-themes)))
       (format t "  Dark:~%")
       (dolist (theme (nreverse dark-themes))
         (let ((current (eq theme *current-terminal-theme*)))
           (format t "    ~A~A - ~A~%"
                   (if current "*" " ")
                   (string-downcase (symbol-name (terminal-theme-name theme)))
                   (terminal-theme-display-name theme))))
       (format t "  Light:~%")
       (dolist (theme (nreverse light-themes))
         (let ((current (eq theme *current-terminal-theme*)))
           (format t "    ~A~A - ~A~%"
                   (if current "*" " ")
                   (string-downcase (symbol-name (terminal-theme-name theme)))
                   (terminal-theme-display-name theme)))))
     (format t "~%Browser themes:~%")
     (let ((dark-themes nil)
           (light-themes nil))
       (dolist (theme (list-browser-themes))
         (if (browser-theme-dark-p theme)
             (push theme dark-themes)
             (push theme light-themes)))
       (format t "  Dark:~%")
       (dolist (theme (nreverse dark-themes))
         (let ((current (eq theme *current-browser-theme*)))
           (format t "    ~A~A - ~A~%"
                   (if current "*" " ")
                   (string-downcase (symbol-name (browser-theme-name theme)))
                   (browser-theme-display-name theme))))
       (format t "  Light:~%")
       (dolist (theme (nreverse light-themes))
         (let ((current (eq theme *current-browser-theme*)))
           (format t "    ~A~A - ~A~%"
                   (if current "*" " ")
                   (string-downcase (symbol-name (browser-theme-name theme)))
                   (browser-theme-display-name theme)))))
     (format t "~%(* = current)~%"))

    ;; Set terminal theme
    ((string-equal subcommand "terminal")
     (if (null arg)
         (format *error-output* "~&Usage: ,theme terminal <theme-name>~%")
         (let ((name (intern (string-upcase arg) :keyword)))
           (if (find-terminal-theme name)
               (progn
                 (apply-terminal-theme name)
                 (format t "~&Terminal theme set to: ~A~%"
                         (terminal-theme-display-name *current-terminal-theme*)))
               (format *error-output* "~&Unknown terminal theme: ~A~%Use ,theme list to see available themes.~%" arg)))))

    ;; Set browser theme
    ((string-equal subcommand "browser")
     (if (null arg)
         (format *error-output* "~&Usage: ,theme browser <theme-name>~%")
         (let ((name (intern (string-upcase arg) :keyword)))
           (if (find-browser-theme name)
               (progn
                 (apply-browser-theme name)
                 (format t "~&Browser theme set to: ~A~%"
                         (browser-theme-display-name *current-browser-theme*)))
               (format *error-output* "~&Unknown browser theme: ~A~%Use ,theme list to see available themes.~%" arg)))))

    ;; Auto-detect
    ((string-equal subcommand "auto")
     (auto-select-terminal-theme)
     (auto-select-browser-theme)
     (format t "~&Auto-detected themes:~%")
     (format t "  Terminal: ~A~%" (terminal-theme-display-name *current-terminal-theme*))
     (format t "  Browser:  ~A~%" (browser-theme-display-name *current-browser-theme*)))

    ;; Unknown subcommand - try as theme name for both
    (t
     (let ((name (intern (string-upcase subcommand) :keyword)))
       (cond
         ;; Check if it's a known theme name
         ((and (find-terminal-theme name) (find-browser-theme name))
          (apply-terminal-theme name)
          (apply-browser-theme name)
          (format t "~&Theme set to: ~A (terminal and browser)~%"
                  (terminal-theme-display-name *current-terminal-theme*)))
         ((find-terminal-theme name)
          (apply-terminal-theme name)
          (format t "~&Terminal theme set to: ~A~%"
                  (terminal-theme-display-name *current-terminal-theme*)))
         ((find-browser-theme name)
          (apply-browser-theme name)
          (format t "~&Browser theme set to: ~A~%"
                  (browser-theme-display-name *current-browser-theme*)))
         (t
          (format *error-output* "~&Unknown subcommand or theme: ~A~%"subcommand)
          (format *error-output* "~&Use ,theme list to see available options.~%")))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; AI CLI Integration
;;; ─────────────────────────────────────────────────────────────────────────────

(defun ai-cli-available-p (cli)
  "Check if an AI CLI tool is available in PATH."
  (let ((program (ecase cli
                   (:claude "claude")
                   (:gemini "gemini")
                   (:codex "codex"))))
    (ignore-errors
      (multiple-value-bind (output error-output exit-code)
          (uiop:run-program (list program "--version")
                            :output nil
                            :error-output nil
                            :ignore-error-status t)
        (declare (ignore output error-output))
        (zerop exit-code)))))

(defun detect-ai-cli ()
  "Detect available AI CLI, trying gemini, claude, codex in order."
  (cond
    (*ai-cli* *ai-cli*)  ; User configured
    ((ai-cli-available-p :gemini) :gemini)
    ((ai-cli-available-p :claude) :claude)
    ((ai-cli-available-p :codex) :codex)
    (t nil)))

(defun ai-cli-program (cli)
  "Return the program name for an AI CLI."
  (ecase cli
    (:claude "claude")
    (:gemini "gemini")
    (:codex "codex")))

(defvar *spinner-frames* '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Braille spinner animation frames.")

(defvar *spinner-running* nil
  "Flag to control spinner thread.")

(defun start-spinner (message)
  "Start a spinner with MESSAGE. Returns the spinner thread."
  (setf *spinner-running* t)
  (let ((frames *spinner-frames*)
        (idx 0))
    (sb-thread:make-thread
     (lambda ()
       (loop while *spinner-running* do
         (format t "~C[2K~A ~A~C[0G"
                 #\Escape        ; ESC[2K clears line
                 (nth idx frames)
                 message
                 #\Escape)       ; ESC[0G returns to column 0
         (force-output)
         (setf idx (mod (1+ idx) (length frames)))
         (sleep 0.08)))
     :name "spinner")))

(defun stop-spinner (thread)
  "Stop the spinner THREAD and clear the line."
  (setf *spinner-running* nil)
  (ignore-errors
    (sb-thread:join-thread thread :timeout 0.5))
  ;; Clear the spinner line
  (format t "~C[2K~C[0G" #\Escape #\Escape)
  (force-output))

(defun setup-gemini-mcp-config ()
  "Create temporary Gemini MCP config for ICL tools. Returns config file path.
   Uses Streamable HTTP transport if MCP server is running, otherwise falls back to stdio."
  (let* ((gemini-dir (merge-pathnames ".gemini/" (uiop:getcwd)))
         (config-file (merge-pathnames "settings.json" gemini-dir))
         (use-http (mcp-http-server-running-p)))
    ;; Ensure .gemini directory exists
    (ensure-directories-exist config-file)
    ;; Write MCP config
    (with-open-file (out config-file :direction :output :if-exists :supersede)
      (format out "{~%")
      (format out "  \"mcpServers\": {~%")
      (format out "    \"icl\": {~%")
      (if use-http
          ;; Streamable HTTP transport - use httpUrl (not url which is SSE)
          (format out "      \"httpUrl\": ~S,~%" (mcp-http-server-url))
          ;; Stdio transport - spawns a subprocess
          (let ((icl-path (or (uiop:argv0)
                              (namestring (merge-pathnames "icl" (uiop:getcwd)))))
                (slynk-addr (format nil "~A:~D" *slynk-host* *slynk-port*)))
            (format out "      \"command\": ~S,~%" icl-path)
            (format out "      \"args\": [\"--mcp-server\", ~S],~%" slynk-addr)))
      (format out "      \"timeout\": 30000,~%")
      (format out "      \"trust\": true~%")
      (format out "    }~%")
      (format out "  }~%")
      (format out "}~%"))
    config-file))

(defun cleanup-gemini-mcp-config ()
  "Remove temporary Gemini MCP config."
  (let ((config-file (merge-pathnames ".gemini/settings.json" (uiop:getcwd))))
    (when (probe-file config-file)
      (delete-file config-file))
    ;; Try to remove .gemini dir if empty
    (ignore-errors
      (uiop:delete-empty-directory (merge-pathnames ".gemini/" (uiop:getcwd))))))

(defun setup-claude-mcp-config ()
  "Create temporary Claude MCP config for ICL tools. Returns config file path.
   Uses Streamable HTTP transport if MCP server is running, otherwise falls back to stdio."
  (let* ((config-file (merge-pathnames ".mcp.json" (uiop:getcwd)))
         (use-http (mcp-http-server-running-p)))
    (with-open-file (out config-file :direction :output :if-exists :supersede)
      (format out "{~%")
      (format out "  \"mcpServers\": {~%")
      (format out "    \"icl\": {~%")
      (if use-http
          ;; Streamable HTTP transport - requires type and url
          (progn
            (format out "      \"type\": \"http\",~%")
            (format out "      \"url\": ~S~%" (mcp-http-server-url)))
          ;; Stdio transport - spawns a subprocess
          (let ((icl-path (or (uiop:argv0)
                              (namestring (merge-pathnames "icl" (uiop:getcwd)))))
                (slynk-addr (format nil "~A:~D" *slynk-host* *slynk-port*)))
            (format out "      \"type\": \"stdio\",~%")
            (format out "      \"command\": ~S,~%" icl-path)
            (format out "      \"args\": [\"--mcp-server\", ~S]~%" slynk-addr)))
      (format out "    }~%")
      (format out "  }~%")
      (format out "}~%"))
    config-file))

(defun cleanup-claude-mcp-config ()
  "Remove temporary Claude MCP config."
  (let ((config-file (merge-pathnames ".mcp.json" (uiop:getcwd))))
    (when (probe-file config-file)
      (delete-file config-file))))

(defun run-ai-cli (cli prompt)
  "Run an AI CLI with the given prompt, streaming output with markdown rendering."
  (let* ((program (ai-cli-program cli))
         (use-mcp (and (member cli '(:gemini :claude :codex)) *slynk-connected-p*)))
    ;; Start HTTP MCP server on-demand for AI integration
    (when use-mcp
      (start-mcp-http-server))
    (let ((args (ecase cli
                  (:claude (if use-mcp
                               ;; Use --mcp-config to load our MCP server in -p mode
                               ;; Pre-allow all ICL tools to avoid permission prompts
                               (list program "-p" prompt
                                     "--mcp-config" ".mcp.json"
                                     "--allowedTools" "mcp__icl__*")
                               (list program "-p" prompt)))
                  (:gemini (list program "-p" prompt))
                  (:codex (if use-mcp
                              ;; Use exec subcommand for non-interactive mode
                              ;; --enable rmcp_client required for HTTP transport
                              ;; --skip-git-repo-check allows running outside git repos
                              (list program "exec" prompt
                                    "--skip-git-repo-check"
                                    "--enable" "rmcp_client"
                                    "--config" (format nil "mcp_servers.icl.url=~S"
                                                       (mcp-http-server-url)))
                              (list program "exec" prompt "--skip-git-repo-check"))))))
      ;; Setup MCP config file if needed (gemini/claude use config files)
      (when use-mcp
        (case cli
          (:gemini (setup-gemini-mcp-config))
          (:claude (setup-claude-mcp-config))))
      (unwind-protect
          (run-ai-cli-streaming program args)
        ;; Cleanup
        (when use-mcp
          (case cli
            (:gemini (cleanup-gemini-mcp-config))
            (:claude (cleanup-claude-mcp-config)))
          (stop-mcp-http-server))))))

(defun run-ai-cli-streaming (program args)
  "Run AI CLI, buffering output and rendering as markdown at the end.
   Shows spinner with progress and elapsed time while working."
  (let* ((process (uiop:launch-program args
                                       :output :stream
                                       :error-output :stream))
         (out-stream (uiop:process-info-output process))
         (err-stream (uiop:process-info-error-output process))
         ;; Buffer for accumulating all output
         (output-buffer (make-array 0 :element-type 'character
                                      :adjustable t :fill-pointer 0))
         (char-count 0)
         (start-time (get-internal-real-time))
         (spinner-frames *spinner-frames*)
         (spinner-idx 0))
    (unwind-protect
        (loop
          (let ((out-ready (listen out-stream))
                (err-ready (listen err-stream)))
            ;; Check if process is done
            (when (and (not out-ready)
                       (not err-ready)
                       (not (uiop:process-alive-p process)))
              ;; Clear spinner line
              (format t "~C[2K~C[0G" #\Escape #\Escape)
              ;; Render accumulated output as markdown
              (when (> (length output-buffer) 0)
                (format t "~A~%"
                        (string-right-trim
                         '(#\Newline #\Space)
                         (tuition:render-markdown
                          (coerce output-buffer 'string)
                          :width 78))))
              (return))
            ;; Main dispatch
            (cond
              ;; Drain stderr to prevent blocking (discard output)
              (err-ready
               (read-line err-stream nil nil))
              ;; Read stdout - accumulate
              (out-ready
               (let ((char (read-char out-stream nil nil)))
                 (when char
                   (vector-push-extend char output-buffer)
                   (incf char-count))))
              ;; No input - show spinner with elapsed time
              (t
               (let ((elapsed-secs (floor (- (get-internal-real-time) start-time)
                                          internal-time-units-per-second)))
                 (format t "~C[2K~A Thinking (~A, ~Ds, ~D chars)...~C[0G"
                         #\Escape
                         (nth spinner-idx spinner-frames)
                         program
                         elapsed-secs
                         char-count
                         #\Escape))
               (force-output)
               (setf spinner-idx (mod (1+ spinner-idx) (length spinner-frames)))
               (sleep 0.05)))))
      ;; Cleanup
      (ignore-errors (close out-stream))
      (ignore-errors (close err-stream))
      (uiop:wait-process process))))

(defun get-system-context (&key with-mcp-tools)
  "Get system context string for AI prompts.
   If WITH-MCP-TOOLS is true, include info about available MCP tools."
  (let ((base (format nil "Context: ~A ~A~@[, Slynk connected to ~A~]"
                      (or *current-lisp* "Unknown Lisp")
                      (or (ignore-errors
                            (first (backend-eval-internal "(lisp-implementation-version)")))
                          "")
                      (when *slynk-connected-p*
                        (format nil "~A:~D" *slynk-host* *slynk-port*)))))
    (if with-mcp-tools
        (format nil "~A

You have access to ICL MCP tools that query the LIVE Lisp environment. This is crucial - you can see actual definitions, current values, and real documentation from the running system.

IMPORTANT: Use these tools proactively! Don't guess about Lisp symbols - look them up. The user is in an interactive session and expects accurate, specific information.

Available tools:
- describe_symbol: Get full description including type, value, and docs. USE THIS FIRST for any symbol.
- get_documentation: Get docstrings (function/variable/type)
- get_function_arglist: Get parameter list for functions/macros
- apropos_search: Find symbols by pattern - great for discovering related functions
- list_package_symbols: See what a package exports
- list_source_files: Check what library source files exist in ocicl/ (e.g., pattern \"alexandria*\")
- read_source_file: Read source code and documentation from ocicl/ directory

CHECK THE ocicl/ DIRECTORY: It likely contains source code and documentation for loaded libraries.
- Use list_source_files to discover available files
- Libraries are in subdirs like alexandria-*/, cl-ppcre-*/, with .lisp source and READMEs
- Use read_source_file to read actual implementations and documentation

When explaining library code:
1. describe_symbol to get the symbol's info
2. list_source_files with a pattern to find the library's files
3. read_source_file to get the actual source code
4. Quote relevant code in your explanation

Show tool usage with markers like: *[Looking up symbol...]* or *[Reading source...]*" base)
        base)))

(define-command explain (&rest args)
  "Ask AI to explain the last expression/command output, or specific code.
Uses claude, gemini, or codex CLI (auto-detected or set via *ai-cli*).

Examples:
  ,explain              - Explain the last result (or error if one occurred)
  ,explain (defmacro...)- Explain specific code"
  (let ((cli (detect-ai-cli)))
    (if (not cli)
        (format *error-output* "~&No AI CLI found. Install one of: claude, gemini, codex~%")
        ;; Capture error state BEFORE get-system-context, which may clear it
        (let* ((was-error *last-was-error*)
               (error-condition *last-error-condition*)
               (error-backtrace *last-error-backtrace*)
               (last-form icl-+)
               (last-result icl-*)
               (input (string-trim '(#\Space #\Tab) (format nil "~{~A~^ ~}" args)))
               ;; Include MCP tools info for CLIs that support MCP
               (context (get-system-context :with-mcp-tools (member cli '(:gemini :claude :codex))))
               (prompt
                 (cond
                   ;; No args - explain based on what happened last
                   ((or (null args) (string= input ""))
                    (cond
                      ;; Command output takes priority (e.g., disassembly)
                      (*last-command-output*
                       (prog1
                           (format nil "~A~%~%Explain this Common Lisp ~A output:~%~%~A"
                                   context
                                   (or *last-command-name* "command")
                                   *last-command-output*)
                         ;; Clear after use
                         (setf *last-command-output* nil
                               *last-command-name* nil)))
                      ;; If last action was an error, explain it
                      (was-error
                       (format nil "~A~%~%Explain this Common Lisp error and suggest how to fix it:~%~%Expression: ~S~%~%Error: ~A~%~%~@[Backtrace:~%~A~]"
                               context
                               last-form
                               error-condition
                               error-backtrace))
                      ;; Otherwise explain last expression and result
                      ;; But skip if there's nothing meaningful to explain
                      ((and (null last-form) (null last-result))
                       (format t "~&Nothing to explain. Try evaluating an expression first.~%")
                       nil)
                      (t
                       (format nil "~A~%~%Explain this Common Lisp expression and its result:~%~%Expression: ~S~%~%Result: ~A"
                               context
                               last-form last-result))))
                   ;; Specific code/expression
                   (t
                    (format nil "~A~%~%Explain this Common Lisp code:~%~%~A"
                            context input)))))
          (when prompt
            (run-ai-cli cli prompt))))))

(define-command (ai-cli set-ai) (&optional cli-name)
  "Show or set which AI CLI to use for ,explain.
Example: ,ai-cli          - Show current setting and available CLIs
         ,ai-cli claude   - Use Claude CLI
         ,ai-cli gemini   - Use Gemini CLI
         ,ai-cli codex    - Use Codex CLI
         ,ai-cli auto     - Auto-detect (default)"
  (if cli-name
      ;; Set the CLI
      (let ((cli (cond
                   ((string-equal cli-name "claude") :claude)
                   ((string-equal cli-name "gemini") :gemini)
                   ((string-equal cli-name "codex") :codex)
                   ((string-equal cli-name "auto") :auto)
                   (t nil))))
        (if (not cli)
            (format *error-output* "~&Unknown CLI: ~A (use claude, gemini, codex, or auto)~%" cli-name)
            (progn
              (setf *ai-cli* (if (eq cli :auto) nil cli))
              (format t "~&AI CLI set to: ~A~%" (if (eq cli :auto) "auto-detect" cli)))))
      ;; Show current setting
      (progn
        (format t "~&AI CLI Configuration:~%")
        (format t "  Current setting: ~A~%" (or *ai-cli* "auto-detect"))
        (format t "  Detected CLI: ~A~%" (or (detect-ai-cli) "none"))
        (format t "~%Available CLIs:~%")
        (dolist (cli '(:claude :gemini :codex))
          (format t "  ~(~A~): ~A~%"
                  cli
                  (if (ai-cli-available-p cli) "installed" "not found"))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; System Browser
;;; ─────────────────────────────────────────────────────────────────────────────

(define-command browser (&optional action)
  "Open the System Browser for exploring packages, classes, and methods.
A VS Code-style browser with dockable panels for navigating the Lisp environment.
Example: ,browser          - Start browser and open in web browser
         ,browser stop     - Stop the browser server
         ,browser test     - Run browser self-test (for CI)"
  (cond
    (*browser-terminal-active*
     (format t "~&; Already in browser mode.~%"))
    ((and action (string-equal action "stop"))
     (if *browser-acceptor*
         (progn
           (stop-browser)
           (format t "~&Browser stopped.~%"))
         (format t "~&Browser is not running.~%")))
    ((and action (string-equal action "test"))
     ;; Self-test mode for CI
     (handler-case
         (let ((url (start-browser :open-browser nil)))
           (format t "Browser started at ~A~%" url)
           (sleep 1)
           (let ((result (ignore-errors
                           (uiop:run-program (list "curl" "-s" url)
                                             :output :string))))
             (cond
               ((null result)
                (format *error-output* "~&FAIL: Could not fetch page~%")
                (stop-browser)
                (values))
               ((not (search "dockview" result))
                (format *error-output* "~&FAIL: dockview not found in response~%")
                (stop-browser)
                (values))
               ((not (search "xterm" result))
                (format *error-output* "~&FAIL: xterm not found in response~%")
                (stop-browser)
                (values))
               (t
                (format t "~&Browser test passed!~%")
                (stop-browser)
                t))))
       (error (e)
         (format *error-output* "~&FAIL: ~A~%" e)
         (ignore-errors (stop-browser)))))
    (t
     (handler-case
         (let ((url (start-browser :open-browser t)))
           (format t "Browser started at ~A~%" url))
       (error (e)
         (format *error-output* "~&Failed to start browser: ~A~%" e))))))
