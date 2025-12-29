;;; coverage-test.lisp - Test file for sb-cover demonstration
;;;
;;; Usage:
;;;   ,cover-load examples/coverage-test.lisp
;;;   (run-coverage-test)
;;;   ,cover-report

(defpackage :coverage-test
  (:use :cl)
  (:export :run-coverage-test :factorial :fizzbuzz))

(in-package :coverage-test)

(defun factorial (n)
  "Compute factorial of N."
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

(defun fizzbuzz (n)
  "Return fizzbuzz string for N."
  (cond
    ((zerop (mod n 15)) "FizzBuzz")
    ((zerop (mod n 3)) "Fizz")
    ((zerop (mod n 5)) "Buzz")
    (t (princ-to-string n))))

(defun run-coverage-test ()
  "Run tests to generate coverage data."
  (format t "~&Testing factorial:~%")
  (format t "  (factorial 5) = ~A~%" (factorial 5))
  (format t "  (factorial 0) = ~A~%" (factorial 0))

  (format t "~&Testing fizzbuzz:~%")
  (dolist (n '(1 3 5 15))
    (format t "  (fizzbuzz ~A) = ~A~%" n (fizzbuzz n)))

  (format t "~&Coverage test complete.~%")
  t)
