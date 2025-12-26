;;; icl.el --- Emacs integration for ICL browser -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anthony Green <green@moxielogic.com>
;; SPDX-License-Identifier: MIT
;;
;; Author: Anthony Green <green@moxielogic.com>
;; Maintainer: Anthony Green <green@moxielogic.com>
;; URL: https://github.com/moxielogic/icl
;; Version: 1.15.2
;; Package-Requires: ((emacs "26.1"))
;; Keywords: lisp, tools, repl

;;; Commentary:
;; Provides integration between SLY/SLIME and ICL's browser visualization interface.
;; Start ICL browser with M-x icl when you have an active Sly connection.

;;; Code:

(require 'sly nil t)
(require 'slime nil t)

(defgroup icl nil
  "ICL browser integration."
  :group 'tools)

(defcustom icl-program "icl"
  "Path to the ICL executable."
  :type 'string
  :group 'icl)

(defcustom icl-backend 'auto
  "Which Emacs backend to use: 'auto, 'sly, or 'slime."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "SLY" sly)
                 (const :tag "SLIME" slime))
  :group 'icl)

(defcustom icl-auto-stop-on-disconnect t
  "When non-nil, stop the ICL process after the SLY/SLIME connection closes."
  :type 'boolean
  :group 'icl)

(defvar icl--slynk-port nil
  "Port of the Slynk server created for ICL.")

(defvar icl--sly-hooks-installed nil
  "Non-nil when disconnect hooks have been installed.")

(defun icl--backend ()
  "Return the active backend symbol: 'sly or 'slime."
  (pcase icl-backend
    ('sly
     (unless (or (featurep 'sly) (require 'sly nil t))
       (user-error "SLY not available. Install sly or set icl-backend to 'slime"))
     'sly)
    ('slime
     (unless (or (featurep 'slime) (require 'slime nil t))
       (user-error "SLIME not available. Install slime or set icl-backend to 'sly"))
     'slime)
    (_
     (cond
      ((or (featurep 'sly) (require 'sly nil t)) 'sly)
      ((or (featurep 'slime) (require 'slime nil t)) 'slime)
      (t (user-error "Neither SLY nor SLIME is available"))))))

(defun icl--connected-p ()
  "Return non-nil when the selected backend has an active connection."
  (pcase (icl--backend)
    ('sly (and (fboundp 'sly-current-connection) (sly-current-connection)))
    ('slime (or (and (fboundp 'slime-connected-p) (slime-connected-p))
                (and (fboundp 'slime-current-connection) (slime-current-connection))))))

(defun icl--eval (form)
  "Evaluate FORM in the backend connection."
  (pcase (icl--backend)
    ('sly (sly-eval form))
    ('slime (slime-eval form))))

(defun icl--ensure-server-and-create ()
  "Ensure Slynk/Swank is available in the Lisp image and create a server.
Uses Slynk for SLY backend, Swank for SLIME backend."
  (pcase (icl--backend)
    ('sly
     (icl--eval
      '(cl:progn
         (cl:when (cl:not (cl:find-package :slynk))
           (cl:handler-case
               (asdf:load-system :slynk)
             (error (e)
               (error "Failed to load Slynk: ~A" e))))
         (slynk:create-server :port 0 :dont-close t))))
    ('slime
     (icl--eval
      '(cl:progn
         (cl:when (cl:not (cl:find-package :swank))
           (cl:handler-case
               (asdf:load-system :swank)
             (error (e)
               (error "Failed to load Swank: ~A" e))))
         (swank:create-server :port 0 :dont-close t))))))

(defun icl--on-sly-disconnect (&rest _)
  "Stop ICL shortly after the connection disconnects."
  (when icl-auto-stop-on-disconnect
    (run-at-time 0.2 nil #'icl-stop)))

(defun icl--maybe-install-sly-hooks ()
  "Install hooks to stop ICL when the connection disconnects."
  (unless icl--sly-hooks-installed
    ;; Hook variants across SLY versions.
    (when (boundp 'sly-disconnect-hook)
      (add-hook 'sly-disconnect-hook #'icl--on-sly-disconnect))
    (when (boundp 'sly-disconnected-hook)
      (add-hook 'sly-disconnected-hook #'icl--on-sly-disconnect))
    (when (boundp 'sly-net-close-hook)
      (add-hook 'sly-net-close-hook #'icl--on-sly-disconnect))
    ;; SLIME hook variants.
    (when (boundp 'slime-disconnect-hook)
      (add-hook 'slime-disconnect-hook #'icl--on-sly-disconnect))
    (when (boundp 'slime-net-close-hook)
      (add-hook 'slime-net-close-hook #'icl--on-sly-disconnect))
    (when (boundp 'slime-connection-closed-hook)
      (add-hook 'slime-connection-closed-hook #'icl--on-sly-disconnect))
    (setq icl--sly-hooks-installed t)))

(defconst icl--runtime-phase1
  "(cl:progn
     (cl:unless (cl:find-package :icl-runtime)
       (cl:defpackage #:icl-runtime
         (:use #:cl)
         (:export #:usb8-array-to-base64-string
                  #:*eval-generation*
                  #:setup-eval-generation-hook)))
     t)"
  "Phase 1: Create ICL runtime package.")

(defconst icl--runtime-phase2
  "(in-package :icl-runtime)
   (defvar *eval-generation* 0)
   (defvar *eval-hook-installed* nil)
   (defvar *original-mrepl-eval* nil)
   (defvar *original-listener-eval* nil)
   (defun setup-eval-generation-hook ()
     (unless *eval-hook-installed*
       ;; SLY uses slynk-mrepl::mrepl-eval for REPL evaluations
       (when (find-package :slynk-mrepl)
         (let ((fn-symbol (find-symbol \"MREPL-EVAL\" :slynk-mrepl)))
           (when (and fn-symbol (fboundp fn-symbol))
             (setf *original-mrepl-eval* (fdefinition fn-symbol))
             (setf (fdefinition fn-symbol)
                   (lambda (repl string)
                     (prog1 (funcall *original-mrepl-eval* repl string)
                       (incf *eval-generation*)))))))
       ;; SLIME uses swank::listener-eval for REPL evaluations
       (when (find-package :swank)
         (let ((fn-symbol (find-symbol \"LISTENER-EVAL\" :swank)))
           (when (and fn-symbol (fboundp fn-symbol))
             (setf *original-listener-eval* (fdefinition fn-symbol))
             (setf (fdefinition fn-symbol)
                   (lambda (string)
                     (prog1 (funcall *original-listener-eval* string)
                       (incf *eval-generation*)))))))
       (setf *eval-hook-installed* t))
     t)"
  "Phase 2: Define ICL runtime functions.")

(defun icl--setup-eval-hook ()
  "Set up eval generation counter in the Lisp image.
Uses icl-runtime package with wrappers around REPL eval functions
\(slynk-mrepl::mrepl-eval for SLY, swank::listener-eval for SLIME)."
  ;; Phase 1: Create the package
  (icl--eval (read icl--runtime-phase1))
  ;; Phase 2: Load definitions via string stream
  (icl--eval `(cl:with-input-from-string (cl-user::icl-load-stream ,icl--runtime-phase2)
                (cl:load cl-user::icl-load-stream)
                t))
  ;; Call the setup function to install the hook
  (icl--eval '(icl-runtime:setup-eval-generation-hook)))

;;;###autoload
(defun icl ()
  "Start ICL browser connected to the current Sly session."
  (interactive)
  (unless (icl--connected-p)
    (user-error "No active SLY/SLIME connection"))
  (when (get-process "icl")
    (user-error "ICL already running. Use M-x icl-stop first"))
  (icl--maybe-install-sly-hooks)
  ;; Set up eval generation counter for visualization refresh
  (icl--setup-eval-hook)
  ;; Create a Slynk/Swank server that accepts connections
  (let ((port (icl--ensure-server-and-create)))
    (setq icl--slynk-port port)
    (message "ICL connecting on port %d" port)
    (start-process "icl" "*icl*" icl-program
                   "--connect" (format "localhost:%d" port) "-b")))

;;;###autoload
(defun icl-stop ()
  "Stop ICL process."
  (interactive)
  (when-let ((proc (get-process "icl")))
    (kill-process proc)
    (message "ICL stopped")))

;;;###autoload
(defun icl-restart ()
  "Restart ICL."
  (interactive)
  (icl-stop)
  (run-at-time 0.5 nil #'icl))

(provide 'icl)
;;; icl.el ends here
