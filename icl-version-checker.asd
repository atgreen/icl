;;; icl-version-checker.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; ASDF system definition for the ICL version checker tool.
;;; This is a standalone tool for checking JavaScript library updates.

(asdf:defsystem "icl-version-checker"
  :description "Check for updates to JavaScript libraries used by ICL"
  :author      "Anthony Green <green@moxielogic.com>"
  :license     "MIT"
  :version     "1.0.0"
  :depends-on (:dexador
               :yason
               :split-sequence)
  :serial t
  :components ((:module "tools"
                :components
                ((:file "package")
                 (:file "tracked-versions")
                 (:file "version-checker"))))
  :build-operation "program-op"
  :build-pathname "version-checker"
  :entry-point "icl-version-checker:main")
