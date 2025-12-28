;;; regexp.lisp --- Regex visualization as state machine diagram
;;;
;;; Usage: In ICL browser mode, load this file and visualize:
;;;   (load "examples/regexp.lisp")
;;;   ,viz (make-regexp "^[a-z]+@[a-z]+\\.com$")
;;;   ,viz (make-regexp "(foo|bar)+")
;;;   ,viz (make-regexp "https?://[^\\s]+")
;;;
;;; ---------------------------------------------------------------------------
;;; Regex Wrapper Class
;;; ---------------------------------------------------------------------------

(defpackage :regexp-viz
  (:use :cl)
  (:export :make-regexp :regexp :regexp-pattern :regexp-tree))

(in-package :regexp-viz)

(defclass regexp ()
  ((pattern :initarg :pattern :accessor regexp-pattern
            :documentation "The original regex pattern string.")
   (tree :initarg :tree :accessor regexp-tree
         :documentation "The parsed CL-PPCRE parse tree."))
  (:documentation "A wrapper for regex patterns with their parse trees."))

(defun make-regexp (pattern)
  "Create a regexp object from a pattern string."
  (make-instance 'regexp
                 :pattern pattern
                 :tree (cl-ppcre:parse-string pattern)))

;;; ---------------------------------------------------------------------------
;;; Parse Tree to Mermaid State Diagram
;;; ---------------------------------------------------------------------------

(defvar *state-counter* 0)

(defun reset-counter ()
  (setf *state-counter* 0))

(defun next-state ()
  (format nil "s~D" (incf *state-counter*)))

(defun escape-label (str)
  "Escape special characters for Mermaid state labels."
  (with-output-to-string (out)
    (loop for char across (princ-to-string str)
          do (case char
               (#\" (write-string "'" out))
               (#\\ (write-string "\\\\" out))
               (#\[ (write-string "#91;" out))
               (#\] (write-string "#93;" out))
               (#\Newline (write-string " " out))
               (otherwise (write-char char out))))))

(defun format-char-class (spec)
  "Format a character class specification for display."
  (cond
    ((characterp spec) (escape-label (string spec)))
    ((and (consp spec) (eq (car spec) :range))
     (format nil "~A-~A" (escape-label (string (second spec)))
             (escape-label (string (third spec)))))
    ((eq spec :digit-class) "\\\\d")
    ((eq spec :non-digit-class) "\\\\D")
    ((eq spec :word-char-class) "\\\\w")
    ((eq spec :non-word-char-class) "\\\\W")
    ((eq spec :whitespace-char-class) "\\\\s")
    ((eq spec :non-whitespace-char-class) "\\\\S")
    (t (escape-label (princ-to-string spec)))))

;; Forward declaration
(declaim (ftype (function (t t t) list) emit-state))

(defun tree-to-mermaid (tree pattern)
  "Convert a CL-PPCRE parse tree to Mermaid stateDiagram format."
  (reset-counter)
  (with-output-to-string (s)
    (format s "stateDiagram-v2~%")
    (format s "    direction LR~%")
    ;; Add title with the pattern
    (format s "    note left of [*]: /~A/~%" (escape-label pattern))
    (let ((end-states (emit-state s tree "[*]")))
      ;; Connect all end states to final
      (dolist (state end-states)
        (format s "    ~A --> [*]~%" state)))))

(defun emit-state (stream tree from-state)
  "Emit Mermaid states for TREE, starting from FROM-STATE.
   Returns list of states that are the 'exit' points."
  (cond
    ;; Simple string literal
    ((stringp tree)
     (if (= (length tree) 0)
         (list from-state)
         (let ((to-state (next-state)))
           (format stream "    ~A --> ~A: ~A~%" from-state to-state (escape-label tree))
           (list to-state))))

    ;; Single character
    ((characterp tree)
     (let ((to-state (next-state)))
       (format stream "    ~A --> ~A: ~A~%" from-state to-state (escape-label (string tree)))
       (list to-state)))

    ;; Compound expressions
    ((consp tree)
     (case (car tree)
       ;; Sequence of elements
       (:sequence
        (let ((current-states (list from-state)))
          (dolist (elem (cdr tree))
            (let ((new-states nil))
              (dolist (state current-states)
                (setf new-states (append new-states (emit-state stream elem state))))
              (setf current-states new-states)))
          current-states))

       ;; Alternation (choice) - parallel paths
       (:alternation
        (let ((all-ends nil))
          (dolist (alt (cdr tree))
            (let ((ends (emit-state stream alt from-state)))
              (setf all-ends (append ends all-ends))))
          all-ends))

       ;; Groups (capturing and non-capturing) - just process contents
       ((:register :group)
        (emit-state stream (second tree) from-state))

       ;; Greedy repetition
       (:greedy-repetition
        (let* ((min (second tree))
               (max (third tree))
               (inner (fourth tree)))
          (cond
            ;; * (zero or more)
            ((and (= min 0) (null max))
             (let* ((loop-state (next-state))
                    (inner-ends (emit-state stream inner loop-state)))
               ;; Epsilon transition to loop state
               (format stream "    ~A --> ~A: [epsilon]~%" from-state loop-state)
               ;; Loop back
               (dolist (end inner-ends)
                 (format stream "    ~A --> ~A~%" end loop-state))
               (list loop-state)))

            ;; + (one or more)
            ((and (= min 1) (null max))
             (let* ((inner-ends (emit-state stream inner from-state))
                    (loop-state (next-state)))
               (dolist (end inner-ends)
                 (format stream "    ~A --> ~A~%" end loop-state))
               ;; Loop back to re-match
               (format stream "    ~A --> ~A: [repeat]~%" loop-state from-state)
               (list loop-state)))

            ;; ? (zero or one)
            ((and (= min 0) (eql max 1))
             (let ((inner-ends (emit-state stream inner from-state))
                   (skip-state (next-state)))
               ;; Skip path (epsilon)
               (format stream "    ~A --> ~A: [skip]~%" from-state skip-state)
               ;; Connect inner ends to skip state
               (dolist (end inner-ends)
                 (format stream "    ~A --> ~A~%" end skip-state))
               (list skip-state)))

            ;; {n} (exactly n)
            ((eql min max)
             (let ((current (list from-state)))
               (dotimes (i min)
                 (let ((new-ends nil))
                   (dolist (state current)
                     (setf new-ends (append new-ends (emit-state stream inner state))))
                   (setf current new-ends)))
               current))

            ;; {n,} (n or more)
            ((null max)
             (let ((current (list from-state)))
               ;; First emit min required matches
               (dotimes (i min)
                 (let ((new-ends nil))
                   (dolist (state current)
                     (setf new-ends (append new-ends (emit-state stream inner state))))
                   (setf current new-ends)))
               ;; Then add optional loop
               (let ((loop-state (next-state)))
                 (dolist (end current)
                   (format stream "    ~A --> ~A~%" end loop-state))
                 (format stream "    ~A --> ~A: [repeat]~%" loop-state (car current))
                 (list loop-state))))

            ;; {n,m} (between n and m)
            (t
             (let ((current (list from-state)))
               ;; First emit min required matches
               (dotimes (i min)
                 (let ((new-ends nil))
                   (dolist (state current)
                     (setf new-ends (append new-ends (emit-state stream inner state))))
                   (setf current new-ends)))
               ;; Then add optional matches up to max
               (let ((all-ends current))
                 (dotimes (i (- max min))
                   (let ((new-ends nil))
                     (dolist (state current)
                       (setf new-ends (append new-ends (emit-state stream inner state))))
                     (setf all-ends (append all-ends new-ends))
                     (setf current new-ends)))
                 all-ends))))))

       ;; Non-greedy repetition (same structure, just different matching behavior)
       (:non-greedy-repetition
        (emit-state stream `(:greedy-repetition ,@(cdr tree)) from-state))

       ;; Character class
       (:char-class
        (let ((to-state (next-state))
              (chars (mapcar #'format-char-class (cdr tree))))
          (format stream "    ~A --> ~A: [~{~A~}]~%" from-state to-state chars)
          (list to-state)))

       ;; Inverted character class
       (:inverted-char-class
        (let ((to-state (next-state))
              (chars (mapcar #'format-char-class (cdr tree))))
          (format stream "    ~A --> ~A: [^~{~A~}]~%" from-state to-state chars)
          (list to-state)))

       ;; Anchors (don't consume input, just assertions)
       (:start-anchor
        (let ((to-state (next-state)))
          (format stream "    ~A --> ~A: ^~%" from-state to-state)
          (list to-state)))

       (:end-anchor
        (let ((to-state (next-state)))
          (format stream "    ~A --> ~A: $~%" from-state to-state)
          (list to-state)))

       (:word-boundary
        (let ((to-state (next-state)))
          (format stream "    ~A --> ~A: \\\\b~%" from-state to-state)
          (list to-state)))

       (:non-word-boundary
        (let ((to-state (next-state)))
          (format stream "    ~A --> ~A: \\\\B~%" from-state to-state)
          (list to-state)))

       ;; Everything-matcher (dot)
       (:everything
        (let ((to-state (next-state)))
          (format stream "    ~A --> ~A: .~%" from-state to-state)
          (list to-state)))

       ;; Lookahead/lookbehind (assertions, don't consume)
       ((:positive-lookahead :negative-lookahead
         :positive-lookbehind :negative-lookbehind)
        (let* ((type-name (case (car tree)
                           (:positive-lookahead "(?=...)")
                           (:negative-lookahead "(?!...)")
                           (:positive-lookbehind "(?<=...)")
                           (:negative-lookbehind "(?<!...)")))
               (to-state (next-state)))
          (format stream "    ~A --> ~A: ~A~%" from-state to-state type-name)
          (list to-state)))

       ;; Back-reference
       (:back-reference
        (let ((to-state (next-state))
              (num (second tree)))
          (format stream "    ~A --> ~A: \\\\~D~%" from-state to-state num)
          (list to-state)))

       ;; Special character classes
       ((:digit-class :non-digit-class :word-char-class
         :non-word-char-class :whitespace-char-class :non-whitespace-char-class)
        (let ((to-state (next-state))
              (label (case (car tree)
                       (:digit-class "\\\\d")
                       (:non-digit-class "\\\\D")
                       (:word-char-class "\\\\w")
                       (:non-word-char-class "\\\\W")
                       (:whitespace-char-class "\\\\s")
                       (:non-whitespace-char-class "\\\\S"))))
          (format stream "    ~A --> ~A: ~A~%" from-state to-state label)
          (list to-state)))

       ;; Flags - pass through
       (:flags
        (list from-state))

       ;; Unknown
       (otherwise
        (let ((to-state (next-state)))
          (format stream "    ~A --> ~A: ~A~%" from-state to-state
                  (escape-label (princ-to-string tree)))
          (list to-state)))))

    ;; Symbols (character class shortcuts)
    ((symbolp tree)
     (let ((to-state (next-state))
           (label (case tree
                    (:digit-class "\\\\d")
                    (:non-digit-class "\\\\D")
                    (:word-char-class "\\\\w")
                    (:non-word-char-class "\\\\W")
                    (:whitespace-char-class "\\\\s")
                    (:non-whitespace-char-class "\\\\S")
                    (:everything ".")
                    (otherwise (escape-label (symbol-name tree))))))
       (format stream "    ~A --> ~A: ~A~%" from-state to-state label)
       (list to-state)))

    ;; Fallback
    (t
     (let ((to-state (next-state)))
       (format stream "    ~A --> ~A: ~A~%" from-state to-state
               (escape-label (princ-to-string tree)))
       (list to-state)))))

;;; ---------------------------------------------------------------------------
;;; ICL Visualization Integration
;;; ---------------------------------------------------------------------------

(defun register-icl-viz ()
  "Register regexp visualization with ICL."
  (defmethod icl-runtime:visualize ((obj regexp))
    (list :mermaid (tree-to-mermaid (regexp-tree obj) (regexp-pattern obj)))))

;;; ---------------------------------------------------------------------------
;;; Example Patterns
;;; ---------------------------------------------------------------------------

(defvar *email-pattern* (make-regexp "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$")
  "A simple email validation pattern.")

(defvar *phone-pattern* (make-regexp "\\d{3}[-.\\s]?\\d{3}[-.\\s]?\\d{4}")
  "US phone number pattern.")

(defvar *url-pattern* (make-regexp "https?://[^\\s]+")
  "Simple URL pattern.")

(defvar *alternation-pattern* (make-regexp "(foo|bar|baz)+")
  "Alternation with repetition.")

;;; To test, run in ICL browser mode:
;;;   ,viz *email-pattern*       ; Email regex structure
;;;   ,viz *phone-pattern*       ; Phone number regex
;;;   ,viz *url-pattern*         ; URL regex
;;;   ,viz *alternation-pattern* ; Shows branching paths
;;;   ,viz (make-regexp "(a|b)*c")  ; Custom pattern
