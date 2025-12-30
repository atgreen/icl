;;; package.lisp - Package definition for ICL version checker
;;;
;;; SPDX-License-Identifier: MIT

(defpackage :icl-version-checker
  (:use :cl)
  (:export #:*tracked-libraries*
           #:check-library-updates
           #:format-update-report
           #:create-github-issue
           #:main))
