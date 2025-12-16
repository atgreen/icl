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
          (let ((pkg (find-package pkg-name)))
            (when pkg
              (setf *icl-package* pkg)))
          (format t "~&~A~%" pkg-name))
      (error (e)
        (format *error-output* "~&Failed to change package: ~A~%" e)))))

(define-command pwd ()
  "Show the current package."
  (format t "~&~A~%" (package-name *icl-package*)))

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
  (format t "  Package:        ~A~%" (package-name *icl-package*))
  (format t "  History file:   ~A~%" (history-file))
  (format t "  Input count:    ~A~%" *input-count*)
  (format t "  Terminal:       ~A~%" (if (terminal-capable-p) "capable" "dumb")))

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
      (let ((value (first (backend-eval expr-string))))
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
            (dolist (loc locations)
              (format-source-location loc))
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
  (handler-case
      (let ((result (slynk-toggle-trace symbol-name)))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

(define-command (untrace untr) (symbol-name)
  "Disable tracing for a function.
Example: ,untrace my-function"
  (handler-case
      (let ((result (slynk-untrace symbol-name)))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error: ~A~%" e))))

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
   `(cl:funcall (cl:read-from-string "slynk:toggle-trace-fdefinition") ,name)
   *slynk-connection*))

(defun slynk-untrace (name)
  "Untrace NAME via Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (slynk-client:slime-eval
   `(cl:untrace ,(read-from-string (string-upcase name)))
   *slynk-connection*))

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
  "Expand ~ in FILENAME to home directory."
  (let ((name (string-trim '(#\Space #\Tab #\") filename)))
    (if (and (plusp (length name))
             (char= (char name 0) #\~))
        (merge-pathnames (subseq name 2)
                         (user-homedir-pathname))
        name)))

(defun slynk-load-file (filename)
  "Load FILENAME via Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (let ((namestring (namestring (truename filename))))
    (slynk-client:slime-eval
     `(cl:load ,namestring)
     *slynk-connection*)
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
  "Compile FILENAME via Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (let ((namestring (namestring (truename filename))))
    (slynk-client:slime-eval
     `(cl:compile-file ,namestring)
     *slynk-connection*)
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
              (format nil \"~~@[Trace:~~%%~~A~~%%~~]Result: ~~{~~S~~^, ~~}\"
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
      (let ((result (slynk-load-system system-name)))
        (format t "~&~A~%" result))
    (error (e)
      (format *error-output* "~&Error loading system: ~A~%" e))))

(defun slynk-load-system (system-name)
  "Load SYSTEM-NAME via Slynk using trivial-system-loader approach.
   Uses streaming eval so output appears incrementally."
  (unless *slynk-connected-p*
    (error "Not connected to Slynk server"))
  (let* ((name (string-trim '(#\Space #\Tab #\") system-name))
         ;; Code that tries ocicl, then Quicklisp, then ASDF
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
              (format nil \"Loaded ~~A via ocicl\" system))
             ;; Try Quicklisp
             ((find-package '#:QUICKLISP)
              (funcall (find-symbol \"QUICKLOAD\" '#:QUICKLISP) system :silent nil)
              (format nil \"Loaded ~~A via Quicklisp\" system))
             ;; Fall back to plain ASDF
             ((find-package '#:ASDF)
              (asdf:load-system system)
              (format nil \"Loaded ~~A via ASDF\" system))
             (t
              (error \"No system loader available (ocicl, Quicklisp, or ASDF)\"))))))
  (try-load))" name)))
    (first (backend-eval loader-code))))

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
  (format t "~%Available customization variables:~%")
  (format t "  *default-lisp*     - Lisp implementation (default: :SBCL)~%")
  (format t "  *prompt-string*    - Prompt format (default: \"~~A> \")~%")
  (format t "  *result-prefix*    - Result prefix (default: \"=> \")~%")
  (format t "  *colors-enabled*   - Enable colors (default: T)~%")
  (format t "  *history-size*     - Max history entries (default: 1000)~%")
  (format t "  *paredit-mode*     - Structural editing (default: NIL)~%")
  (format t "~%Available hooks:~%")
  (format t "  *before-eval-hook* - Called before evaluation~%")
  (format t "  *after-eval-hook*  - Called after evaluation~%")
  (format t "  *prompt-hook*      - Custom prompt function~%")
  (format t "  *error-hook*       - Custom error handler~%")
  (format t "~%Functions:~%")
  (format t "  (configure-lisp impl &key program args eval-arg)~%")
  (format t "~%Example ~~/.iclrc:~%")
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
  "Create temporary Gemini MCP config for ICL tools. Returns config file path."
  (let* ((gemini-dir (merge-pathnames ".gemini/" (uiop:getcwd)))
         (config-file (merge-pathnames "settings.json" gemini-dir))
         (icl-path (or (uiop:argv0)
                       (namestring (merge-pathnames "icl" (uiop:getcwd)))))
         (slynk-addr (format nil "~A:~D" *slynk-host* *slynk-port*)))
    ;; Ensure .gemini directory exists
    (ensure-directories-exist config-file)
    ;; Write MCP config
    (with-open-file (out config-file :direction :output :if-exists :supersede)
      (format out "{~%")
      (format out "  \"mcpServers\": {~%")
      (format out "    \"icl\": {~%")
      (format out "      \"command\": ~S,~%" icl-path)
      (format out "      \"args\": [\"--mcp-server\", ~S],~%" slynk-addr)
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

(defun run-ai-cli (cli prompt)
  "Run an AI CLI with the given prompt, streaming output with markdown rendering."
  (let* ((program (ai-cli-program cli))
         (args (ecase cli
                 (:claude (list program "-p" prompt))
                 (:gemini (list program "-p" prompt))
                 (:codex (list program "-p" prompt))))
         (use-mcp (and (eq cli :gemini) *slynk-connected-p*)))
    ;; Setup MCP config for Gemini if connected to Slynk
    (when use-mcp
      (setup-gemini-mcp-config))
    (unwind-protect
        (run-ai-cli-streaming program args)
      ;; Cleanup
      (when use-mcp
        (cleanup-gemini-mcp-config)))))

(defun run-ai-cli-streaming (program args)
  "Run AI CLI with streaming output."
  (let* ((process (uiop:launch-program args
                                       :output :stream
                                       :error-output :stream))
         (out-stream (uiop:process-info-output process))
         (err-stream (uiop:process-info-error-output process))
         (first-output nil)
         (need-newline nil)  ; Track if we need newline before next stdout
         (spinner-frames *spinner-frames*)
         (spinner-idx 0))
    (unwind-protect
        (loop
          (let ((out-ready (listen out-stream))
                (err-ready (listen err-stream)))
            ;; Check if process is done and streams are empty
            (when (and (not out-ready)
                       (not err-ready)
                       (not (uiop:process-alive-p process)))
              (when first-output (terpri))
              (return))
            ;; Show spinner while waiting for first output
            (cond
              ((and (not out-ready) (not err-ready) (not first-output))
               (format t "~C[2K~A Thinking (~A)...~C[0G"
                       #\Escape
                       (nth spinner-idx spinner-frames)
                       program
                       #\Escape)
               (force-output)
               (setf spinner-idx (mod (1+ spinner-idx) (length spinner-frames)))
               (sleep 0.08))
              ;; Read stderr (MCP messages, etc)
              (err-ready
               (unless first-output
                 (format t "~C[2K~C[0G" #\Escape #\Escape)
                 (setf first-output t)
                 (terpri))
               (let ((line (read-line err-stream nil nil)))
                 ;; Gemini CLI writes MCP stderr directly, bypassing our stream
                 ;; Just consume the line - visual separation comes from MCP server
                 (when line
                   (setf need-newline t))))
              ;; Stream stdout directly
              (out-ready
               (unless first-output
                 (format t "~C[2K~C[0G" #\Escape #\Escape)
                 (setf first-output t)
                 (terpri))
               ;; Add newline after stderr if needed
               (when need-newline
                 (terpri)
                 (setf need-newline nil))
               (let ((char (read-char out-stream nil nil)))
                 (when char
                   (write-char char)
                   (force-output))))
              ;; Small sleep to avoid busy-waiting
              (t
               (sleep 0.01)))))
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
                            (first (backend-eval "(lisp-implementation-version)")))
                          "")
                      (when *slynk-connected-p*
                        (format nil "~A:~D" *slynk-host* *slynk-port*)))))
    (if with-mcp-tools
        (format nil "~A~%~%You have access to ICL MCP tools to query the Lisp environment:
- get_documentation: Get docstring for a symbol (function, variable, or type)
- describe_symbol: Get full description of a symbol
- apropos_search: Search for symbols by name pattern
- list_package_symbols: List all exported symbols from a package
- get_function_arglist: Get the lambda list for a function/macro
- read_source_file: Read source code from library files in ocicl/
- list_source_files: List available source files in ocicl/
Use these tools to look up documentation, explore symbols, or read library source code." base)
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
               ;; Include MCP tools info for gemini
               (context (get-system-context :with-mcp-tools (eq cli :gemini)))
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
