;;; tests/notebook-tests.lisp --- Tests for the notebook document model
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite notebook-tests
  :description "Tests for the notebook document model and .iclnb serialization"
  :in icl-tests)

(in-suite notebook-tests)

;;; Stub evaluators standing in for BACKEND-EVAL-CAPTURE, which returns
;;; (values output-string value-strings) and signals on error.

(defun %stub-ok (output values)
  (lambda (source) (declare (ignore source)) (values output values)))

(defun %stub-error (message)
  (lambda (source) (declare (ignore source)) (error message)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Cell operations
;;; ─────────────────────────────────────────────────────────────────────────────

(test notebook-starts-empty
  "A fresh notebook has no cells and a title."
  (let ((nb (icl::make-notebook :title "T")))
    (is (= 0 (icl::notebook-cell-count nb)))
    (is (string= "T" (icl::notebook-title nb)))))

(test notebook-add-cell-appends-and-assigns-ids
  "Added cells append in order with unique, increasing ids."
  (let* ((nb (icl::make-notebook))
         (a (icl::notebook-add-cell nb :source "a"))
         (b (icl::notebook-add-cell nb :source "b")))
    (is (= 2 (icl::notebook-cell-count nb)))
    (is (/= (icl::notebook-cell-id a) (icl::notebook-cell-id b)))
    (is (equal (list a b) (icl::notebook-cell-list nb)))
    (is (eq a (icl::notebook-cell-by-id nb (icl::notebook-cell-id a))))))

(test notebook-add-cell-after-id-inserts
  "AFTER-ID inserts the new cell directly after the named cell."
  (let* ((nb (icl::make-notebook))
         (a (icl::notebook-add-cell nb :source "a"))
         (b (icl::notebook-add-cell nb :source "b"))
         (mid (icl::notebook-add-cell nb :source "mid"
                                      :after-id (icl::notebook-cell-id a))))
    (is (equal (list a mid b) (icl::notebook-cell-list nb)))))

(test notebook-remove-cell
  "Removing a cell drops it and reports success/failure."
  (let* ((nb (icl::make-notebook))
         (a (icl::notebook-add-cell nb :source "a"))
         (b (icl::notebook-add-cell nb :source "b")))
    (is (icl::notebook-remove-cell nb (icl::notebook-cell-id a)))
    (is (equal (list b) (icl::notebook-cell-list nb)))
    (is (not (icl::notebook-remove-cell nb 9999)))))

(test notebook-move-cell
  "Cells move up and down, and refuse to move past the ends."
  (let* ((nb (icl::make-notebook))
         (a (icl::notebook-add-cell nb :source "a"))
         (b (icl::notebook-add-cell nb :source "b")))
    (is (icl::notebook-move-cell nb (icl::notebook-cell-id b) :up))
    (is (equal (list b a) (icl::notebook-cell-list nb)))
    (is (not (icl::notebook-move-cell nb (icl::notebook-cell-id b) :up)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Evaluation
;;; ─────────────────────────────────────────────────────────────────────────────

(test notebook-eval-code-produces-stdout-then-value
  "A code cell yields a :stdout blob then a :value blob, in that order."
  (let* ((nb (icl::make-notebook))
         (c (icl::notebook-add-cell nb :source "(princ 1) 42")))
    (icl::notebook-eval-cell c :evaluator (%stub-ok "1" (list "42")))
    (let ((outs (icl::notebook-cell-outputs c)))
      (is (= 2 (length outs)))
      (is (eq :stdout (icl::cell-output-kind (first outs))))
      (is (string= "1" (icl::cell-output-payload (first outs))))
      (is (eq :value (icl::cell-output-kind (second outs))))
      (is (string= "42" (icl::cell-output-payload (second outs)))))))

(test notebook-eval-joins-multiple-values
  "Multiple return values are joined into one :value blob."
  (let* ((nb (icl::make-notebook))
         (c (icl::notebook-add-cell nb :source "(values 1 2)")))
    (icl::notebook-eval-cell c :evaluator (%stub-ok "" (list "1" "2")))
    (let ((outs (icl::notebook-cell-outputs c)))
      (is (= 1 (length outs)))
      (is (eq :value (icl::cell-output-kind (first outs))))
      (is (string= "1, 2" (icl::cell-output-payload (first outs)))))))

(test notebook-eval-omits-empty-stdout
  "No :stdout blob is produced when the captured output is empty."
  (let* ((nb (icl::make-notebook))
         (c (icl::notebook-add-cell nb :source "42")))
    (icl::notebook-eval-cell c :evaluator (%stub-ok "" (list "42")))
    (is (= 1 (length (icl::notebook-cell-outputs c))))
    (is (eq :value (icl::cell-output-kind (first (icl::notebook-cell-outputs c)))))))

(test notebook-eval-error-becomes-error-blob
  "A signalled error becomes a single :error blob carrying the message."
  (let* ((nb (icl::make-notebook))
         (c (icl::notebook-add-cell nb :source "(/ 1 0)")))
    (icl::notebook-eval-cell c :evaluator (%stub-error "boom"))
    (let ((outs (icl::notebook-cell-outputs c)))
      (is (= 1 (length outs)))
      (is (eq :error (icl::cell-output-kind (first outs))))
      (is (search "boom" (icl::cell-output-payload (first outs)))))))

(test notebook-eval-markdown-is-not-run
  "Evaluating a markdown cell echoes its source as a :markdown blob."
  (let* ((nb (icl::make-notebook))
         (c (icl::notebook-add-cell nb :kind :markdown :source "## Hi")))
    ;; The evaluator must never be called for markdown cells.
    (icl::notebook-eval-cell c :evaluator (%stub-error "should not run"))
    (let ((outs (icl::notebook-cell-outputs c)))
      (is (= 1 (length outs)))
      (is (eq :markdown (icl::cell-output-kind (first outs))))
      (is (string= "## Hi" (icl::cell-output-payload (first outs))))
      (is (null (icl::notebook-cell-exec-count c))))))

(test notebook-eval-increments-exec-count
  "Each code-cell run bumps the exec count from NIL upward."
  (let* ((nb (icl::make-notebook))
         (c (icl::notebook-add-cell nb :source "1")))
    (is (null (icl::notebook-cell-exec-count c)))
    (icl::notebook-eval-cell c :evaluator (%stub-ok "" (list "1")))
    (is (= 1 (icl::notebook-cell-exec-count c)))
    (icl::notebook-eval-cell c :evaluator (%stub-ok "" (list "1")))
    (is (= 2 (icl::notebook-cell-exec-count c)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Serialization (Format A)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %sample-notebook ()
  (let ((nb (icl::make-notebook :title "Sample" :backend-info "sbcl")))
    (let ((c (icl::notebook-add-cell nb :source "(+ 1 2)")))
      (icl::notebook-eval-cell c :evaluator (%stub-ok "side" (list "3"))))
    (icl::notebook-add-cell nb :kind :markdown :source "# Notes")
    nb))

(test notebook-sexp-roundtrips-in-memory
  "notebook->sexp / sexp->notebook preserves cells, kinds, sources, outputs."
  (let* ((nb (%sample-notebook))
         (nb2 (icl::sexp->notebook (icl::notebook->sexp nb))))
    (is (string= "Sample" (icl::notebook-title nb2)))
    (is (= (icl::notebook-cell-count nb) (icl::notebook-cell-count nb2)))
    (let ((c1 (first (icl::notebook-cell-list nb2)))
          (c2 (second (icl::notebook-cell-list nb2))))
      (is (eq :code (icl::notebook-cell-kind c1)))
      (is (string= "(+ 1 2)" (icl::notebook-cell-source c1)))
      (is (equal (icl::notebook-cell-outputs
                  (first (icl::notebook-cell-list nb)))
                 (icl::notebook-cell-outputs c1)))
      (is (eq :markdown (icl::notebook-cell-kind c2))))))

(test notebook-save-load-roundtrips-on-disk
  "A notebook survives a save/load cycle to a .iclnb file."
  (uiop:with-temporary-file (:pathname path :type "iclnb")
    (let ((nb (%sample-notebook)))
      (icl::save-notebook nb path)
      (let ((nb2 (icl::load-notebook path)))
        (is (string= (icl::notebook-title nb) (icl::notebook-title nb2)))
        (is (= (icl::notebook-cell-count nb) (icl::notebook-cell-count nb2)))
        (is (string= "(+ 1 2)"
                     (icl::notebook-cell-source
                      (first (icl::notebook-cell-list nb2)))))
        (is (equalp path (icl::notebook-path nb2)))))))

(test notebook-load-keeps-next-id-past-existing-ids
  "After load, NEXT-ID never collides with a loaded cell id."
  (let* ((nb (%sample-notebook))
         (nb2 (icl::sexp->notebook (icl::notebook->sexp nb)))
         (max-id (reduce #'max (icl::notebook-cell-list nb2)
                         :key #'icl::notebook-cell-id :initial-value 0))
         (new (icl::notebook-add-cell nb2 :source "new")))
    (is (> (icl::notebook-cell-id new) max-id))))
