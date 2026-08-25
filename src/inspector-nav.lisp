;;; inspector-nav.lisp --- Zipper-style navigation for ICL inspector
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Navigation Node
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct inspector-nav
  "Zipper-style navigation node for inspector."
  (object nil)                            ; Current object being inspected
  (parent nil :type (or null inspector-nav)) ; Parent node (for up navigation)
  (accessor nil :type keyword)            ; How we got here: :car :cdr :nth :slot :key :value
  (accessor-arg nil)                      ; Slot name, index, key depending on accessor
  (siblings nil :type list)               ; List of sibling accessors at this level
  (sibling-index 0 :type fixnum))         ; Current position in siblings list

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Sibling Computation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun compute-cons-siblings ()
  "Return siblings for a cons cell navigation point."
  (list (cons :car nil) (cons :cdr nil)))

(defun compute-list-siblings (lst)
  "Return siblings for navigating list elements."
  (loop for i from 0 below (length lst)
        collect (cons :nth i)))

(defun compute-hash-table-siblings (ht)
  "Return siblings for navigating hash table entries."
  (let ((siblings nil))
    (maphash (lambda (k v)
               (declare (ignore v))
               (push (cons :key k) siblings))
             ht)
    (nreverse siblings)))

(defun compute-slot-siblings (object)
  "Return siblings for navigating CLOS object slots."
  #+sbcl
  (let ((class (class-of object)))
    (mapcar (lambda (slot)
              (cons :slot (sb-mop:slot-definition-name slot)))
            (sb-mop:class-slots class)))
  #-sbcl
  nil)

(defun compute-siblings (nav)
  "Compute sibling accessors for the parent object at NAV's current position."
  (let ((parent (inspector-nav-parent nav)))
    (when parent
      (let ((parent-obj (inspector-nav-object parent)))
        (cond
          ;; At car or cdr of a cons
          ((consp parent-obj)
           (compute-cons-siblings))
          ;; At an element of a list (via :nth)
          ((and (listp parent-obj) (eq (inspector-nav-accessor nav) :nth))
           (compute-list-siblings parent-obj))
          ;; At a slot of a CLOS object
          ((and (eq (inspector-nav-accessor nav) :slot)
                (typep parent-obj 'standard-object))
           (compute-slot-siblings parent-obj))
          ;; At a key's value in a hash table
          ((and (eq (inspector-nav-accessor nav) :key)
                (hash-table-p parent-obj))
           (compute-hash-table-siblings parent-obj))
          (t nil))))))

(defun find-sibling-index (nav siblings)
  "Find the index of NAV's accessor in SIBLINGS."
  (let ((accessor (inspector-nav-accessor nav))
        (arg (inspector-nav-accessor-arg nav)))
    (position-if (lambda (sib)
                   (and (eq (car sib) accessor)
                        (equal (cdr sib) arg)))
                 siblings)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Navigation Operations
;;; ─────────────────────────────────────────────────────────────────────────────

(defun nav-car (nav)
  "Navigate into the CAR of a cons cell. Returns new nav node or NIL."
  (let ((obj (inspector-nav-object nav)))
    (when (consp obj)
      (let* ((new-obj (car obj))
             (new-nav (make-inspector-nav
                       :object new-obj
                       :parent nav
                       :accessor :car
                       :accessor-arg nil)))
        (let ((siblings (compute-siblings new-nav)))
          (setf (inspector-nav-siblings new-nav) siblings)
          (setf (inspector-nav-sibling-index new-nav)
                (or (find-sibling-index new-nav siblings) 0)))
        new-nav))))

(defun nav-cdr (nav)
  "Navigate into the CDR of a cons cell. Returns new nav node or NIL."
  (let ((obj (inspector-nav-object nav)))
    (when (consp obj)
      (let* ((new-obj (cdr obj))
             (new-nav (make-inspector-nav
                       :object new-obj
                       :parent nav
                       :accessor :cdr
                       :accessor-arg nil)))
        (let ((siblings (compute-siblings new-nav)))
          (setf (inspector-nav-siblings new-nav) siblings)
          (setf (inspector-nav-sibling-index new-nav)
                (or (find-sibling-index new-nav siblings) 0)))
        new-nav))))

(defun nav-nth (nav n)
  "Navigate to the Nth element of a list. Returns new nav node or NIL."
  (let ((obj (inspector-nav-object nav)))
    (when (and (listp obj) (< n (length obj)))
      (let* ((new-obj (nth n obj))
             (new-nav (make-inspector-nav
                       :object new-obj
                       :parent nav
                       :accessor :nth
                       :accessor-arg n)))
        (let ((siblings (compute-siblings new-nav)))
          (setf (inspector-nav-siblings new-nav) siblings)
          (setf (inspector-nav-sibling-index new-nav)
                (or (find-sibling-index new-nav siblings) 0)))
        new-nav))))

(defun nav-slot (nav slot-name)
  "Navigate to a CLOS object's slot. Returns new nav node or NIL."
  (let ((obj (inspector-nav-object nav)))
    (when (and (typep obj 'standard-object)
               (slot-boundp obj slot-name))
      (let* ((new-obj (slot-value obj slot-name))
             (new-nav (make-inspector-nav
                       :object new-obj
                       :parent nav
                       :accessor :slot
                       :accessor-arg slot-name)))
        (let ((siblings (compute-siblings new-nav)))
          (setf (inspector-nav-siblings new-nav) siblings)
          (setf (inspector-nav-sibling-index new-nav)
                (or (find-sibling-index new-nav siblings) 0)))
        new-nav))))

(defun nav-key (nav key)
  "Navigate to a hash table key's value. Returns new nav node or NIL."
  (let ((obj (inspector-nav-object nav)))
    (when (hash-table-p obj)
      (multiple-value-bind (value present-p) (gethash key obj)
        (when present-p
          (let* ((new-nav (make-inspector-nav
                           :object value
                           :parent nav
                           :accessor :key
                           :accessor-arg key)))
            (let ((siblings (compute-siblings new-nav)))
              (setf (inspector-nav-siblings new-nav) siblings)
              (setf (inspector-nav-sibling-index new-nav)
                    (or (find-sibling-index new-nav siblings) 0)))
            new-nav))))))

(defun nav-up (nav)
  "Navigate up to parent. Returns parent nav node or NIL if at root."
  (inspector-nav-parent nav))

(defun nav-to-accessor (nav accessor arg)
  "Navigate from NAV using ACCESSOR and ARG."
  (case accessor
    (:car (nav-car nav))
    (:cdr (nav-cdr nav))
    (:nth (nav-nth nav arg))
    (:slot (nav-slot nav arg))
    (:key (nav-key nav arg))
    (otherwise nil)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Breadcrumb Path
;;; ─────────────────────────────────────────────────────────────────────────────

(defun nav-breadcrumb-path (nav)
  "Return list of (accessor . arg) pairs from root to NAV."
  (let ((path nil)
        (current nav))
    (loop while current
          do (push (cons (inspector-nav-accessor current)
                         (inspector-nav-accessor-arg current))
                   path)
             (setf current (inspector-nav-parent current)))
    path))

(defun format-breadcrumb-element (accessor arg)
  "Format a single breadcrumb element for display."
  (case accessor
    (:root "root")
    (:car "car")
    (:cdr "cdr")
    (:nth (format nil "[~D]" arg))
    (:slot (format nil ".~A" arg))
    (:key (format nil "[~S]" arg))
    (otherwise (format nil "~A" accessor))))

(defun nav-path-string (nav &optional (separator " > "))
  "Return string representation of navigation path."
  (let ((elements (mapcar (lambda (p)
                            (format-breadcrumb-element (car p) (cdr p)))
                          (nav-breadcrumb-path nav))))
    (with-output-to-string (out)
      (loop for (element . rest) on elements
            do (write-string element out)
            when rest do (write-string separator out)))))

(defun nav-sibling-position (nav)
  "Return (current . total) sibling position, or NIL if no siblings."
  (let ((siblings (inspector-nav-siblings nav)))
    (when (and siblings (> (length siblings) 1))
      (cons (1+ (inspector-nav-sibling-index nav))
            (length siblings)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Visit History
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct visit-history
  "History of visited navigation positions."
  (entries (make-array 100 :adjustable t :fill-pointer 0))
  (current-index -1 :type fixnum)
  (max-size 100 :type fixnum))

(defun make-empty-history (&optional (max-size 100))
  "Create a new empty visit history."
  (make-visit-history
   :entries (make-array max-size :adjustable t :fill-pointer 0)
   :current-index -1
   :max-size max-size))

