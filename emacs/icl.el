;;; icl.el --- Emacs integration for ICL browser -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Anthony Green <green@moxielogic.com>
;; SPDX-License-Identifier: MIT
;;
;; Author: Anthony Green <green@moxielogic.com>
;; Maintainer: Anthony Green <green@moxielogic.com>
;; URL: https://github.com/moxielogic/icl
;; Version: 1.19.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: lisp, tools, repl

;;; Commentary:
;; Provides integration between SLY/SLIME and ICL's browser visualization interface.
;; Start ICL browser with M-x icl when you have an active SLY/SLIME connection.
;;
;; Multiple connections are supported - each SLY/SLIME connection can have its
;; own ICL browser instance. Use M-x icl-list to see running instances,
;; M-x icl-stop to stop the current connection's ICL, or M-x icl-stop-all
;; to stop all instances.

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

(defvar icl--connections (make-hash-table :test 'eq)
  "Hash table mapping SLY/SLIME connections to ICL state.
Each entry is a plist with :port, :process, and :buffer keys.")

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

(defun icl--current-connection ()
  "Return the current SLY/SLIME connection object."
  (pcase (icl--backend)
    ('sly (and (fboundp 'sly-current-connection) (sly-current-connection)))
    ('slime (and (fboundp 'slime-current-connection) (slime-current-connection)))))

(defun icl--connection-name (conn)
  "Return a short name for connection CONN."
  (pcase (icl--backend)
    ('sly (if (and (fboundp 'sly-connection-name) conn)
              (sly-connection-name conn)
            "default"))
    ('slime (if (and (fboundp 'slime-connection-name) conn)
                (slime-connection-name conn)
              "default"))))

(defun icl--process-name (&optional conn)
  "Return unique process name for connection CONN."
  (format "icl<%s>" (icl--connection-name (or conn (icl--current-connection)))))

(defun icl--buffer-name (&optional conn)
  "Return unique buffer name for connection CONN."
  (format "*icl<%s>*" (icl--connection-name (or conn (icl--current-connection)))))

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

(defun icl--on-sly-disconnect (&optional conn &rest _)
  "Stop ICL for connection CONN when it disconnects."
  (when icl-auto-stop-on-disconnect
    ;; Use the passed connection or try to determine from context
    (let ((connection (or conn (icl--current-connection))))
      (when connection
        (run-at-time 0.2 nil #'icl--stop-for-connection connection)))))

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
   ;; Only define if not already defined (avoids redefinition warnings on reconnect)
   (unless (fboundp 'setup-eval-generation-hook)
     ;; Helper to wrap a function with eval-generation increment
     (defun wrap-with-generation-increment (pkg-name fn-name)
       (let* ((pkg (find-package pkg-name))
              (fn-symbol (and pkg (find-symbol fn-name pkg))))
         (when (and fn-symbol (fboundp fn-symbol))
           (let ((original (fdefinition fn-symbol)))
             (setf (fdefinition fn-symbol)
                   (lambda (&rest args)
                     (prog1 (apply original args)
                       (incf *eval-generation*))))))))
     (defun setup-eval-generation-hook ()
       (unless *eval-hook-installed*
         ;; SLY hooks
         (wrap-with-generation-increment :slynk-mrepl \"MREPL-EVAL\")
         (wrap-with-generation-increment :slynk \"INTERACTIVE-EVAL\")
         (wrap-with-generation-increment :slynk \"EVAL-AND-GRAB-OUTPUT\")
         (wrap-with-generation-increment :slynk \"PPRINT-EVAL\")
         (wrap-with-generation-increment :slynk \"COMPILE-STRING-FOR-EMACS\")
         ;; SLIME hooks
         (wrap-with-generation-increment :swank \"LISTENER-EVAL\")
         (wrap-with-generation-increment :swank \"INTERACTIVE-EVAL\")
         (wrap-with-generation-increment :swank \"EVAL-AND-GRAB-OUTPUT\")
         (wrap-with-generation-increment :swank \"PPRINT-EVAL\")
         (wrap-with-generation-increment :swank \"COMPILE-STRING-FOR-EMACS\")
         (setf *eval-hook-installed* t))
       t))"
  "Phase 2: Define ICL runtime functions.")

(defun icl--setup-eval-hook ()
  "Set up eval generation counter in the Lisp image.
Wraps various Slynk/Swank eval functions to increment *eval-generation*
on REPL input, C-x C-e, C-c C-c, and other evaluation commands."
  ;; Phase 1: Create the package
  (icl--eval (read icl--runtime-phase1))
  ;; Phase 2: Load definitions via string stream
  (icl--eval `(cl:with-input-from-string (cl-user::icl-load-stream ,icl--runtime-phase2)
                (cl:load cl-user::icl-load-stream)
                t))
  ;; Call the setup function to install the hook
  (icl--eval '(icl-runtime:setup-eval-generation-hook)))

(defun icl--stop-for-connection (conn)
  "Stop ICL process for connection CONN and clean up state."
  (when-let ((state (gethash conn icl--connections)))
    (when-let ((proc (plist-get state :process)))
      (when (process-live-p proc)
        (kill-process proc)))
    (remhash conn icl--connections)))

;;;###autoload
(defun icl ()
  "Start ICL browser connected to the current Sly session."
  (interactive)
  (unless (icl--connected-p)
    (user-error "No active SLY/SLIME connection"))
  (let* ((conn (icl--current-connection))
         (state (gethash conn icl--connections))
         (existing-proc (and state (plist-get state :process))))
    ;; Check if ICL is already running for this connection
    (when existing-proc
      (if (process-live-p existing-proc)
          (user-error "ICL already running for this connection. Use M-x icl-stop first")
        ;; Process exists but is dead - clean it up
        (remhash conn icl--connections))))
  (icl--maybe-install-sly-hooks)
  ;; Set up eval generation counter for visualization refresh
  (icl--setup-eval-hook)
  ;; Create a Slynk/Swank server that accepts connections
  (let* ((conn (icl--current-connection))
         (port (icl--ensure-server-and-create))
         (proc-name (icl--process-name conn))
         (buf-name (icl--buffer-name conn))
         (proc (start-process proc-name buf-name icl-program
                              "--connect" (format "localhost:%d" port) "-b")))
    ;; Store state for this connection
    (puthash conn (list :port port :process proc :buffer buf-name) icl--connections)
    (message "ICL connecting on port %d for %s" port (icl--connection-name conn))))

;;;###autoload
(defun icl-stop ()
  "Stop ICL process for the current connection."
  (interactive)
  (let ((conn (icl--current-connection)))
    (if (gethash conn icl--connections)
        (progn
          (icl--stop-for-connection conn)
          (message "ICL stopped for %s" (icl--connection-name conn)))
      (message "No ICL running for current connection"))))

;;;###autoload
(defun icl-stop-all ()
  "Stop all ICL processes."
  (interactive)
  (let ((count 0))
    (maphash (lambda (conn state)
               (when-let ((proc (plist-get state :process)))
                 (when (process-live-p proc)
                   (kill-process proc)
                   (cl-incf count))))
             icl--connections)
    (clrhash icl--connections)
    (message "Stopped %d ICL instance(s)" count)))

;;;###autoload
(defun icl-list ()
  "List all running ICL instances."
  (interactive)
  (if (zerop (hash-table-count icl--connections))
      (message "No ICL instances running")
    (let ((instances '()))
      (maphash (lambda (conn state)
                 (let ((proc (plist-get state :process))
                       (port (plist-get state :port)))
                   (push (format "  %s (port %d) - %s"
                                 (icl--connection-name conn)
                                 port
                                 (if (and proc (process-live-p proc))
                                     "running"
                                   "dead"))
                         instances)))
               icl--connections)
      (message "ICL instances:\n%s" (string-join (nreverse instances) "\n")))))

;;;###autoload
(defun icl-restart ()
  "Restart ICL for the current connection."
  (interactive)
  (icl-stop)
  (run-at-time 0.5 nil #'icl))

(provide 'icl)
;;; icl.el ends here
