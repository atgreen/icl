;;; registry.lisp --- Command registration and dispatch for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; The COMMAND struct and *COMMANDS* registry live in specials.lisp so that
;;; completion.lisp (loaded earlier) sees them at compile time.

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
        (commands nil))
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
  "Split command line into tokens, respecting quoted strings and parentheses.
   Returns list of strings."
  (let ((tokens nil)
        (current (make-string-output-stream))
        (in-quote nil)
        (paren-depth 0)
        (escape nil))
    (loop for char across line
          do (cond
               (escape
                (write-char char current)
                (setf escape nil))
               ((char= char #\\)
                (setf escape t)
                (write-char char current))
               ((and (not in-quote) (char= char #\())
                (incf paren-depth)
                (write-char char current))
               ((and (not in-quote) (char= char #\)))
                (decf paren-depth)
                (write-char char current))
               ((char= char #\")
                (setf in-quote (not in-quote))
                (write-char char current))
               ((and (not in-quote) (zerop paren-depth) (member char '(#\Space #\Tab)))
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
  ;; Track which session is evaluating for output routing
  (bt:with-lock-held (*evaluating-session-lock*)
    (setf *evaluating-session* *current-session*))
  ;; Keep backward compat for old mechanism
  (set-active-repl-output *standard-output*)
  (unwind-protect
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
                           cmd-name)))))
    ;; Note: We intentionally do NOT clear *evaluating-session* here.
    ;; Backend output can arrive asynchronously after the command returns.
    ;; Keeping *evaluating-session* set ensures delayed output goes to the
    ;; correct session. It will be updated when a new evaluation starts.
    ))
