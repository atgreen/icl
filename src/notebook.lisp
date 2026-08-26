;;; notebook.lisp --- Notebook document model for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; A notebook is an ordered list of cells (code or markdown) with their
;;; captured outputs. This file is the protocol-independent document model:
;;; the in-memory structs, cell evaluation over the capturing-eval path, and
;;; load/save of the ".iclnb" s-expression format ("Format A"). The browser
;;; panel and websocket protocol are wired on top of this elsewhere.

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Data model
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (notebook-cell (:constructor %make-notebook-cell))
  "A single notebook cell."
  (id 0 :type fixnum)                       ; Stable id, unique within a notebook
  (kind :code :type keyword)                ; :code or :markdown
  (source "" :type string)                  ; Cell text
  (exec-count nil :type (or null fixnum))   ; Times a code cell has run, or NIL
  (outputs nil :type list))                 ; List of output blobs (see below)

(defstruct (notebook (:constructor %make-notebook))
  "An ordered collection of cells plus document metadata."
  (title "Untitled" :type string)
  (path nil)                                ; Pathname on disk, or NIL if unsaved
  (backend-info nil)                        ; Description of the backend image
  (next-id 1 :type fixnum)                  ; Next cell id to hand out
  (cells (make-array 0 :adjustable t :fill-pointer 0)))

;;; An output blob is a plist (:kind K :payload P). KIND mirrors the browser's
;;; existing render taxonomy so blobs map straight onto the client renderers:
;;;   :value :stdout :error   (text, rendered in the MVP)
;;;   :markdown :html :vega-lite :mermaid :hash-table :svg :image  (rich, later)
(defun make-cell-output (kind payload)
  "Build an output blob of KIND (a keyword) carrying PAYLOAD."
  (list :kind kind :payload payload))

(defun cell-output-kind (output) (getf output :kind))
(defun cell-output-payload (output) (getf output :payload))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Construction and cell operations
;;; ─────────────────────────────────────────────────────────────────────────────

(defun make-notebook (&key (title "Untitled") path backend-info)
  "Create an empty notebook."
  (%make-notebook :title title
                  :path (and path (pathname path))
                  :backend-info backend-info))

(defun notebook-cell-count (nb)
  "Number of cells in NB."
  (fill-pointer (notebook-cells nb)))

(defun notebook-cell-list (nb)
  "The cells of NB as a fresh list, in order."
  (coerce (notebook-cells nb) 'list))

(defun notebook-cell-index (nb id)
  "Index of the cell with ID in NB, or NIL."
  (position id (notebook-cells nb) :key #'notebook-cell-id))

(defun notebook-cell-by-id (nb id)
  "The cell with ID in NB, or NIL."
  (find id (notebook-cells nb) :key #'notebook-cell-id))

(defun %vector-insert-at (vec index elt)
  "Insert ELT into adjustable VEC at INDEX, shifting the tail right."
  (vector-push-extend elt vec)          ; grow by one; value overwritten below
  (loop for i from (1- (fill-pointer vec)) above index
        do (setf (aref vec i) (aref vec (1- i))))
  (setf (aref vec index) elt)
  vec)

(defun notebook-add-cell (nb &key (kind :code) (source "") after-id)
  "Add a new cell to NB and return it. When AFTER-ID names an existing cell,
   insert directly after it; otherwise append."
  (let ((cell (%make-notebook-cell :id (notebook-next-id nb)
                                   :kind kind
                                   :source source))
        (cells (notebook-cells nb)))
    (incf (notebook-next-id nb))
    (let ((idx (and after-id (notebook-cell-index nb after-id))))
      (if idx
          (%vector-insert-at cells (1+ idx) cell)
          (vector-push-extend cell cells)))
    cell))

(defun notebook-remove-cell (nb id)
  "Remove the cell with ID from NB. Returns T if a cell was removed."
  (let ((idx (notebook-cell-index nb id))
        (cells (notebook-cells nb)))
    (when idx
      (loop for i from idx below (1- (fill-pointer cells))
            do (setf (aref cells i) (aref cells (1+ i))))
      (decf (fill-pointer cells))
      t)))

(defun notebook-move-cell (nb id direction)
  "Move the cell with ID one slot :UP or :DOWN. Returns T if it moved."
  (let ((idx (notebook-cell-index nb id))
        (cells (notebook-cells nb)))
    (when idx
      (let ((j (ecase direction (:up (1- idx)) (:down (1+ idx)))))
        (when (and (>= j 0) (< j (fill-pointer cells)))
          (rotatef (aref cells idx) (aref cells j))
          t)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Evaluation
;;; ─────────────────────────────────────────────────────────────────────────────

(defun notebook-eval-cell (cell &key (evaluator #'backend-eval-capture))
  "Evaluate CELL and store its OUTPUTS; returns CELL.

   EVALUATOR is called on the cell source and must return
   (values output-string value-strings), signalling an error on failure —
   the contract of BACKEND-EVAL-CAPTURE. It is a parameter so the model can
   be tested without a live backend.

   Markdown cells are not evaluated; their output is the source itself, to be
   rendered by the client."
  (ecase (notebook-cell-kind cell)
    (:markdown
     (setf (notebook-cell-outputs cell)
           (list (make-cell-output :markdown (notebook-cell-source cell)))))
    (:code
     (setf (notebook-cell-outputs cell)
           (handler-case
               (multiple-value-bind (output values)
                   (funcall evaluator (notebook-cell-source cell))
                 (let ((outputs '()))
                   (when (and output (plusp (length output)))
                     (push (make-cell-output :stdout output) outputs))
                   (when values
                     (push (make-cell-output
                            :value (format nil "~{~A~^, ~}" values))
                           outputs))
                   (nreverse outputs)))
             (error (e)
               (list (make-cell-output :error (princ-to-string e))))))
     (setf (notebook-cell-exec-count cell)
           (1+ (or (notebook-cell-exec-count cell) 0)))))
  cell)

(defun notebook-eval-all (nb &key (evaluator #'backend-eval-capture))
  "Evaluate every cell in NB in order. Returns NB."
  (loop for cell across (notebook-cells nb)
        do (notebook-eval-cell cell :evaluator evaluator))
  nb)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Serialization — Format A (.iclnb s-expression)
;;; ─────────────────────────────────────────────────────────────────────────────
;;;
;;; The on-disk form is a plain, readable s-expression decoupled from the
;;; in-memory structs, so the file format can evolve independently:
;;;
;;;   (:iclnb 1
;;;    :title "..." :backend-info "..." :next-id N
;;;    :cells ((:id 1 :kind :code :source "..." :exec-count 2
;;;             :outputs ((:kind :value :payload "3")))
;;;            ...))

(defun cell->sexp (cell)
  "Serialize CELL to a readable plist."
  (list :id (notebook-cell-id cell)
        :kind (notebook-cell-kind cell)
        :source (notebook-cell-source cell)
        :exec-count (notebook-cell-exec-count cell)
        :outputs (notebook-cell-outputs cell)))

(defun sexp->cell (plist)
  "Reconstruct a cell from a serialized PLIST."
  (%make-notebook-cell :id (getf plist :id 0)
                       :kind (getf plist :kind :code)
                       :source (getf plist :source "")
                       :exec-count (getf plist :exec-count)
                       :outputs (getf plist :outputs)))

(defun notebook->sexp (nb)
  "Serialize NB to a readable s-expression."
  (list :iclnb 1
        :title (notebook-title nb)
        :backend-info (notebook-backend-info nb)
        :next-id (notebook-next-id nb)
        :cells (map 'list #'cell->sexp (notebook-cells nb))))

(defun sexp->notebook (form)
  "Reconstruct a notebook from a serialized FORM."
  (unless (and (consp form) (eq (first form) :iclnb))
    (error "Not an ICL notebook (expected an :iclnb form)."))
  (let ((nb (%make-notebook :title (getf form :title "Untitled")
                            :backend-info (getf form :backend-info)
                            :next-id (getf form :next-id 1))))
    (dolist (cp (getf form :cells))
      (vector-push-extend (sexp->cell cp) (notebook-cells nb)))
    ;; Guarantee NEXT-ID never collides with a loaded cell id.
    (setf (notebook-next-id nb)
          (max (notebook-next-id nb)
               (1+ (reduce #'max (notebook-cells nb)
                           :key #'notebook-cell-id :initial-value 0))))
    nb))

(defun save-notebook (nb &optional (path (notebook-path nb)))
  "Write NB to PATH as an .iclnb s-expression. Returns NB."
  (unless path
    (error "Notebook has no path; supply one to SAVE-NOTEBOOK."))
  (let ((path (pathname path)))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (let ((*print-pretty* t)
            (*print-case* :downcase)
            (*print-readably* nil)
            (*print-circle* nil))
        (prin1 (notebook->sexp nb) out)
        (terpri out)))
    (setf (notebook-path nb) path)
    nb))

(defun load-notebook (path)
  "Read a notebook from the .iclnb file at PATH. Returns the notebook."
  (with-open-file (in path :direction :input)
    (let* ((*read-eval* nil)
           (nb (sexp->notebook (read in))))
      (setf (notebook-path nb) (pathname path))
      nb)))
