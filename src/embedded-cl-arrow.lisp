;;; embedded-cl-arrow.lisp --- Embedded cl-arrow bundle + ICL adapter source.
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; The zero-dependency cl-arrow library (vendor/cl-arrow-bundle.lisp, generated
;;; by `make bundle` in the cl-arrow repo) and the ICL adapter
;;; (vendor/icl-arrow-adapter.lisp) are embedded as source strings at build time
;;; and LOADed into the inferior Lisp on demand — the same "inject like Slynk"
;;; pattern used for the ICL runtime.  Regenerate the bundle and re-copy it into
;;; vendor/ whenever cl-arrow changes.

(in-package #:icl)

(defparameter *cl-arrow-bundle-source* nil
  "Source text of the cl-arrow bundle, injected into the inferior Lisp.")
(defparameter *icl-arrow-adapter-source* nil
  "Source text of the ICL cl-arrow adapter, injected after the bundle.")

;; Slurp the vendored sources at build time; guarded so a delivered binary
;; (no vendor/ tree) keeps the value baked in at compile/load, exactly like
;; embedded-slynk does with slynk.zip.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((slurp (rel)
           (let ((p (merge-pathnames rel (asdf:system-source-directory :icl))))
             (when (probe-file p)
               (with-open-file (in p :external-format :utf-8)
                 (let* ((s (make-string (file-length in)))
                        (n (read-sequence s in)))
                   (subseq s 0 n)))))))
    (let ((bundle  (slurp "vendor/cl-arrow-bundle.lisp"))
          (adapter (slurp "vendor/icl-arrow-adapter.lisp")))
      (when bundle  (setf *cl-arrow-bundle-source* bundle))
      (when adapter (setf *icl-arrow-adapter-source* adapter)))))
