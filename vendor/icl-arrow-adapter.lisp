;;; icl-arrow-adapter.lisp --- injected into the inferior Lisp after cl-arrow.
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Adapts a tabular Common Lisp value (a Lisp-Stat DATA-FRAME, or a list of
;;; plists / alists) into generic cl-arrow columns and returns a base64-encoded
;;; Arrow IPC stream.  Everything is resolved dynamically against the CL-ARROW
;;; and ICL-RUNTIME packages, so this file LOADs even when cl-arrow is absent —
;;; ICL-ARROW-ENCODE simply returns NIL in that case and ICL falls back to its
;;; JSON table path.

(in-package :cl-user)

(defvar *icl-arrow-na* '#:na
  "Unique sentinel marking a missing cell before it becomes an Arrow null.")

(defun %icl-arrow-fn (name)
  (let ((s (find-symbol name :cl-arrow))) (and s (fboundp s) (symbol-function s))))
(defun %icl-arrow-val (name)
  (let ((s (find-symbol name :cl-arrow))) (and s (boundp s) (symbol-value s))))

(defun %icl-arrow-to-string (v)
  (cond ((eq v *icl-arrow-na*) v)
        ((stringp v) v)
        ((null v) "")
        (t (princ-to-string v))))

(defun %icl-arrow-infer (values)
  "Given a column's VALUES (already NA-substituted), return (TYPE COERCE-FN).
Numbers keep their type; anything non-uniform falls back to utf8 text."
  (let ((any nil) (ints t) (reals t) (strs t))
    (dolist (v values)
      (unless (eq v *icl-arrow-na*)
        (setf any t)
        (cond ((integerp v))
              ((realp v)   (setf ints nil))
              ((stringp v) (setf ints nil reals nil))
              (t           (setf ints nil reals nil strs nil)))))
    (cond
      ((and any ints)  (list (%icl-arrow-val "*INT64*")   #'identity))
      ((and any reals) (list (%icl-arrow-val "*FLOAT64*")
                             (lambda (x) (coerce x 'double-float))))
      ((and any strs)  (list (%icl-arrow-val "*UTF8*")     #'identity))
      (t               (list (%icl-arrow-val "*UTF8*")     #'%icl-arrow-to-string)))))

(defun %icl-arrow-column-spec (name values)
  "Build a cl-arrow column spec (NAME TYPE VECTOR :na .. :na-test ..) from a
column NAME and its NA-substituted VALUES."
  (destructuring-bind (type coerce) (%icl-arrow-infer values)
    (when type
      (list name type
            (mapcar (lambda (v) (if (eq v *icl-arrow-na*) v (funcall coerce v))) values)
            :na *icl-arrow-na* :na-test #'eq))))

;;; --- data-frame ------------------------------------------------------------

(defun %icl-arrow-df-columns (obj)
  "Column specs for a Lisp-Stat DATA-FRAME, or NIL if OBJ is not one."
  (let ((df-pkg (find-package "DATA-FRAME")))
    (when df-pkg
      (let* ((df-class (let ((s (find-symbol "DATA-FRAME" df-pkg)))
                         (and s (find-class s nil)))))
        (when (and df-class (typep obj df-class))
          (let ((keys-fn (let ((s (find-symbol "KEYS" df-pkg)))
                           (and s (fboundp s) (symbol-function s))))
                (cols-fn (let ((s (find-symbol "COLUMNS" df-pkg)))
                           (and s (fboundp s) (symbol-function s)))))
            (when (and keys-fn cols-fn)
              (let ((keys (funcall keys-fn obj))
                    (cols (funcall cols-fn obj)))
                (loop for k across (coerce keys 'vector)
                      for c across (coerce cols 'vector)
                      collect (%icl-arrow-column-spec
                               (princ-to-string k) (coerce c 'list)))))))))))

;;; --- list of plists / alists ----------------------------------------------

(defun %icl-arrow-row->alist (row)
  (cond
    ((consp (first row))
     (loop for pair in row when (consp pair)
           collect (cons (princ-to-string (car pair)) (cdr pair))))
    ((and (symbolp (first row)) (first row)
          (ignore-errors (evenp (length row))))
     (loop for (k v) on row by #'cddr collect (cons (princ-to-string k) v)))
    (t nil)))

(defun %icl-arrow-plist-columns (obj)
  "Column specs for a list of plists/alists, or NIL if OBJ is not tabular."
  (when (and (consp obj) (listp obj) (ignore-errors (<= (length obj) 1000000)))
    (let ((rows (mapcar #'%icl-arrow-row->alist obj)))
      (when (and rows (every #'identity rows))
        (let ((columns nil))
          (dolist (r rows) (dolist (cell r)
                             (pushnew (car cell) columns :test #'string=)))
          (setf columns (nreverse columns))
          (when columns
            (loop for col in columns
                  collect (%icl-arrow-column-spec
                           col
                           (mapcar (lambda (r)
                                     (let ((cell (assoc col r :test #'string=)))
                                       (if cell (cdr cell) *icl-arrow-na*)))
                                   rows)))))))))

(defun icl-arrow-columns (obj)
  "Generic cl-arrow column specs for a tabular OBJ, or NIL."
  (or (ignore-errors (%icl-arrow-df-columns obj))
      (ignore-errors (%icl-arrow-plist-columns obj))))

(defun icl-arrow-encode (obj)
  "Base64 Arrow IPC stream for tabular OBJ, or NIL when unavailable / not
tabular.  Safe to call unconditionally; returns NIL if cl-arrow is absent."
  (let ((to-ipc (%icl-arrow-fn "COLUMNS->IPC-STREAM"))
        (b64 (let ((s (find-symbol "USB8-ARRAY-TO-BASE64-STRING" :icl-runtime)))
               (and s (fboundp s) (symbol-function s)))))
    (when (and to-ipc b64)
      (ignore-errors
        (let ((cols (icl-arrow-columns obj)))
          (when (and cols (every #'identity cols))
            (funcall b64 (funcall to-ipc cols))))))))
