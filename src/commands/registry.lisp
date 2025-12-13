;;; registry.lisp --- Command registration and dispatch for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Command Registry
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *commands* (make-hash-table :test 'equal)
  "Registry of available commands, keyed by lowercase name.")

(defstruct (command (:constructor make-command))
  "A registered ICL command."
  (name nil :type symbol)
  (aliases nil :type list)
  (function nil :type function)
  (argspec nil :type list)
  (documentation nil :type (or null string)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Registration
;;; ─────────────────────────────────────────────────────────────────────────────

(defun register-command (name aliases argspec function documentation)
  "Register a command in the registry."
  (let ((cmd (make-command :name name
                           :aliases aliases
                           :function function
                           :argspec argspec
                           :documentation documentation)))
    ;; Register under primary name
    (setf (gethash (string-downcase (symbol-name name)) *commands*) cmd)
    ;; Register under aliases
    (dolist (alias aliases)
      (setf (gethash (string-downcase (symbol-name alias)) *commands*) cmd))
    cmd))

(defmacro define-command (name-spec (&rest args) &body body)
  "Define an ICL command.

NAME-SPEC can be:
  - A symbol: (define-command help ...)
  - A list: (define-command (help h ?) ...) for aliases

ARGS is a lambda list for command arguments (all strings).
First string in BODY is documentation."
  (let* ((names (if (listp name-spec) name-spec (list name-spec)))
         (primary (first names))
         (aliases (rest names))
         (fn-name (intern (format nil "CMD-~A" primary) :icl))
         (docstring (when (stringp (first body)) (first body)))
         (actual-body (if docstring (rest body) body)))
    `(progn
       (defun ,fn-name (,@args)
         ,@(when docstring (list docstring))
         ,@actual-body)
       (register-command ',primary ',aliases ',args #',fn-name ,docstring)
       ',primary)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Lookup
;;; ─────────────────────────────────────────────────────────────────────────────

(defun find-command (name)
  "Look up a command by NAME (string). Returns command struct or NIL."
  (gethash (string-downcase name) *commands*))

(defun list-commands ()
  "Return a list of all unique registered commands."
  (let ((seen (make-hash-table :test 'eq))
        (commands '()))
    (maphash (lambda (k v)
               (declare (ignore k))
               (unless (gethash v seen)
                 (setf (gethash v seen) t)
                 (push v commands)))
             *commands*)
    (sort commands #'string< :key (lambda (c) (symbol-name (command-name c))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Parsing
;;; ─────────────────────────────────────────────────────────────────────────────

(defun split-command-line (line)
  "Split command line into tokens, respecting quoted strings.
   Returns list of strings."
  (let ((tokens '())
        (current (make-string-output-stream))
        (in-quote nil)
        (escape nil))
    (loop for char across line
          do (cond
               (escape
                (write-char char current)
                (setf escape nil))
               ((char= char #\\)
                (setf escape t))
               ((char= char #\")
                (setf in-quote (not in-quote)))
               ((and (not in-quote) (member char '(#\Space #\Tab)))
                (let ((token (get-output-stream-string current)))
                  (when (plusp (length token))
                    (push token tokens)))
                (setf current (make-string-output-stream)))
               (t
                (write-char char current))))
    ;; Don't forget the last token
    (let ((token (get-output-stream-string current)))
      (when (plusp (length token))
        (push token tokens)))
    (nreverse tokens)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Dispatch
;;; ─────────────────────────────────────────────────────────────────────────────

(defun handle-command (input)
  "Parse and dispatch a command from INPUT (including leading comma)."
  (let* ((line (string-trim '(#\Space #\Tab) (subseq input 1))) ; Remove leading comma
         (parts (split-command-line line))
         (cmd-name (first parts))
         (args (rest parts)))
    (if (zerop (length cmd-name))
        (format t "~&Type ,help for available commands.~%")
        (let ((cmd (find-command cmd-name)))
          (if cmd
              (handler-case
                  (apply (command-function cmd) args)
                (error (e)
                  (format *error-output* "~&Command error: ~A~%" e)))
              (format *error-output* "~&Unknown command: ,~A~%Type ,help for available commands.~%"
                      cmd-name))))))
