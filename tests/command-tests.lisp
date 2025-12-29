;;; tests/command-tests.lisp --- Tests for command functions
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite command-tests
  :description "Tests for command functions"
  :in icl-tests)

(in-suite command-tests)

;;; Package context tests for symbol reading

(test read-symbol-in-current-package
  "Test that symbols are read in the current *icl-package* context"
  ;; Create a test package with a test symbol
  (let ((test-pkg (or (find-package :icl-test-pkg)
                      (make-package :icl-test-pkg))))
    (unwind-protect
        (progn
          ;; Intern a symbol in our test package
          (intern "TEST-SYMBOL" test-pkg)
          ;; Set ICL's package context to our test package
          (let ((icl::*icl-package* test-pkg))
            ;; Read a symbol name - should intern in test package
            (let ((sym (let ((*package* (or icl::*icl-package* *package*)))
                         (read-from-string "TEST-SYMBOL"))))
              (is (eq (symbol-package sym) test-pkg)
                  "Symbol should be read in *icl-package* context"))))
      ;; Cleanup
      (delete-package test-pkg))))

(test read-symbol-defaults-to-current-package
  "Test that symbol reading defaults to *package* when *icl-package* is nil"
  (let ((icl::*icl-package* nil))
    (let ((sym (let ((*package* (or icl::*icl-package* *package*)))
                 (read-from-string "SOME-SYMBOL"))))
      ;; Should be read in the current package (icl-tests)
      (is (eq (symbol-package sym) (find-package :icl-tests))
          "Symbol should default to current *package* when *icl-package* is nil"))))

(test read-qualified-symbol-ignores-context
  "Test that fully qualified symbols ignore package context"
  (let ((test-pkg (or (find-package :icl-test-pkg2)
                      (make-package :icl-test-pkg2))))
    (unwind-protect
        (let ((icl::*icl-package* test-pkg))
          ;; Reading CL:CAR should always return the CL symbol
          (let ((sym (let ((*package* (or icl::*icl-package* *package*)))
                       (read-from-string "CL:CAR"))))
            (is (eq sym 'cl:car)
                "Qualified symbols should resolve to their specified package")))
      (delete-package test-pkg))))

;;; Command parsing tests

(test split-command-line-simple
  "Test splitting simple command arguments"
  (let ((parts (icl::split-command-line "arg1 arg2 arg3")))
    (is (equal parts '("arg1" "arg2" "arg3")))))

(test split-command-line-quoted
  "Test splitting command with quoted arguments (quotes preserved)"
  (let ((parts (icl::split-command-line "arg1 \"arg with spaces\" arg3")))
    (is (equal parts '("arg1" "\"arg with spaces\"" "arg3")))))

(test split-command-line-empty
  "Test splitting empty command line"
  (let ((parts (icl::split-command-line "")))
    (is (null parts))))
