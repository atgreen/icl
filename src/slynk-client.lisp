;;; slynk-client.lisp --- Slynk protocol client for ICL
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Connection State
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *slynk-connection* nil
  "Current Slynk connection (slynk-client:swank-connection object).")

(defvar *slynk-port* 4005
  "Default port for Slynk connections.")

(defvar *slynk-host* "127.0.0.1"
  "Default host for Slynk connections.")

(defvar *slynk-connected-p* nil
  "T when connected to a backend server.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Connection Management
;;; ─────────────────────────────────────────────────────────────────────────────

(defun slynk-connect (&key (host *slynk-host*) (port *slynk-port*))
  "Connect to a backend server at HOST:PORT."
  (when *slynk-connection*
    (slynk-disconnect))
  (handler-case
      (let ((conn (slynk-client:slime-connect host port)))
        (when conn
          (setf *slynk-connection* conn)
          (setf *slynk-connected-p* t)
          (format t "~&; Connected to Slynk at ~A:~D~%" host port)
          t))
    (error (e)
      (setf *slynk-connected-p* nil)
      (format *error-output* "~&; Failed to connect to Slynk: ~A~%" e)
      nil)))

(defun slynk-disconnect ()
  "Disconnect from the current backend server."
  (when *slynk-connection*
    (ignore-errors (slynk-client:slime-close *slynk-connection*))
    (setf *slynk-connection* nil)
    (setf *slynk-connected-p* nil)
    (format t "~&; Disconnected from Slynk~%")))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; High-level Operations
;;; ─────────────────────────────────────────────────────────────────────────────

(defun slynk-eval (string &key (package "CL-USER"))
  "Evaluate STRING in the backend server.
   Returns the result."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval `(cl:eval (cl:read-from-string ,string))
                           *slynk-connection*))

(defun slynk-eval-form (string &key (package "CL-USER"))
  "Evaluate STRING and return structured results.
   Returns (values result output-string).
   Errors are caught on the remote side to avoid Slynk debugger issues."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  ;; Wrap evaluation in error handling on the remote side
  ;; This prevents Slynk from entering debug mode and trying to notify Emacs
  ;; Build the wrapper code as a string to avoid package prefix issues
  (let ((wrapper-code (format nil
                              "(handler-case ~
                                 (list :ok (multiple-value-list (eval (read-from-string ~S)))) ~
                                 (error (err) (list :error (princ-to-string err))))"
                              string)))
    (handler-case
        (let ((result (slynk-client:slime-eval
                       `(cl:eval (cl:read-from-string ,wrapper-code))
                       *slynk-connection*)))
          ;; result is either (:ok (values...)) or (:error "message")
          (case (first result)
            (:ok (values (second result) nil))
            (:error (error "~A" (second result)))
            (otherwise (values result nil))))
      (slynk-client:slime-network-error (e)
        ;; Connection lost - mark as disconnected
        (setf *slynk-connected-p* nil)
        (error "Backend connection lost: ~A" e)))))

(defun slynk-complete-simple (prefix &key (package "CL-USER"))
  "Get simple completions for PREFIX.
   Returns list of completion strings."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (let ((result (slynk-client:slime-eval
                 `(cl:funcall (cl:read-from-string "slynk:simple-completions") ,prefix ,package)
                 *slynk-connection*)))
    ;; Result is (completions common-prefix)
    (if (listp result)
        (first result)
        nil)))

(defun slynk-arglist (name &key (package "CL-USER"))
  "Get arglist for function NAME."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:operator-arglist") ,name ,package)
   *slynk-connection*))

(defun slynk-documentation (name type &key (package "CL-USER"))
  "Get documentation for NAME of TYPE (:function, :variable, etc)."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:documentation (cl:find-symbol ,(string-upcase name)) ',type)
   *slynk-connection*))

(defun slynk-describe (name &key (package "CL-USER"))
  "Get full description of NAME."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:describe-symbol") ,name)
   *slynk-connection*))

(defun slynk-apropos (pattern &key (package nil) (external-only t))
  "Search for symbols matching PATTERN."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:apropos-list-for-emacs") ,pattern ,external-only nil ,package)
   *slynk-connection*))

(defun slynk-macroexpand (form &key (package "CL-USER"))
  "Macroexpand FORM once."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:slynk-macroexpand-1") ,form)
   *slynk-connection*))

(defun slynk-macroexpand-all (form &key (package "CL-USER"))
  "Fully macroexpand FORM."
  (declare (ignore package))
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:slynk-macroexpand-all") ,form)
   *slynk-connection*))

(defun slynk-list-packages ()
  "Get list of all packages."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:list-all-package-names") t)
   *slynk-connection*))

(defun slynk-set-package (package-name)
  "Change the current package in Slynk."
  (unless *slynk-connected-p*
    (error "Not connected to backend server"))
  (slynk-client:slime-eval
   `(cl:funcall (cl:read-from-string "slynk:set-package") ,package-name)
   *slynk-connection*))
