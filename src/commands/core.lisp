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
              ;; Slynk returns structured data
              (let ((display (if (> (length matches) 50)
                                 (subseq matches 0 50)
                                 matches)))
                (dolist (item display)
                  ;; Item format: (:designator "pkg:name" :function/:variable/etc)
                  (if (listp item)
                      (format t "  ~A~%" (getf item :designator))
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

(define-command (inspect i) (expr-string)
  "Inspect the result of evaluating an expression.
Example: ,inspect *package*
Example: ,inspect (make-hash-table)"
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
