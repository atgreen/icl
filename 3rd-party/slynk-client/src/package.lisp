(in-package #:common-lisp-user)

(defpackage #:slynk-client
  (:documentation "A client interface to Slynk servers.")
  (:use #:common-lisp #:alexandria)
  (:export #:slynk-connection
           #:slime-connect
           #:slime-close
           #:slime-eval
           #:slime-eval-async
           #:slime-migrate-evals
           #:slime-network-error
           #:slime-pending-evals-p
           #:with-slime-connection))
