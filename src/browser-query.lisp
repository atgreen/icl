;;; browser-query.lisp --- Backend query helpers for ICL browser
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; This module provides functions to query the Lisp backend for
;;; package, symbol, and class information.

(in-package #:icl)

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
