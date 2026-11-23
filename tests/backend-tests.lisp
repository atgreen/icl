;;; tests/backend-tests.lisp --- Tests for backend functions
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>

(in-package #:icl-tests)

(def-suite backend-tests
  :description "Tests for backend functions"
  :in icl-tests)

(in-suite backend-tests)

;;; port-in-use-p tests

(test port-in-use-p-returns-boolean
  "Test that port-in-use-p returns a boolean"
  (let ((result (icl::port-in-use-p 59999)))
    (is (typep result 'boolean))))

(test find-free-port-returns-integer
  "Test that find-free-port returns a valid port number"
  (let ((port (icl::find-free-port 50000)))
    (is (integerp port))
    (is (>= port 50000))
    (is (<= port 65535))))

(test find-free-port-different-starts
  "Test find-free-port with different starting points"
  (let ((port1 (icl::find-free-port 51000))
        (port2 (icl::find-free-port 52000)))
    (is (>= port1 51000))
    (is (>= port2 52000))))

;;; Lisp implementation detection tests

(test lisp-available-nil
  "Test that nil implementation returns nil"
  (is (not (icl::lisp-available-p nil))))

(test get-lisp-args-unknown
  "Test that get-lisp-args returns nil for unknown implementation"
  ;; Should return nil for unknown implementation
  (is (null (icl::get-lisp-args :nonexistent-lisp))))

(test get-lisp-eval-arg-defaults
  "Test that get-lisp-eval-arg returns default for known implementations"
  ;; SBCL should have --eval
  (is (stringp (icl::get-lisp-eval-arg :sbcl))))

(test configure-lisp-new-implementation
  "Test that configure-lisp builds a proper plist for unknown implementations (issue #44)"
  (let ((icl::*lisp-implementations* (copy-tree icl::*lisp-implementations*)))
    (icl:configure-lisp :test-lisp-44 :program "test-lisp")
    (let ((entry (assoc :test-lisp-44 icl::*lisp-implementations*)))
      (is (not (null entry)))
      ;; A dotted tail here would make getf signal "malformed property list"
      (is (string= "test-lisp" (getf (rest entry) :program)))
      (is (string= "--eval" (getf (rest entry) :eval-arg))))))

;;; --embed-origin: another loopback page may frame the browser interface

(test embed-origin-accepts-only-loopback-origins
  "Only a loopback page, such as a local dashboard, may be named."
  (is-true (icl::valid-embed-origin-p "http://127.0.0.1:8080"))
  (is-true (icl::valid-embed-origin-p "http://localhost:3000"))
  (is-true (icl::valid-embed-origin-p "https://[::1]:8443"))
  (is-true (icl::valid-embed-origin-p "http://127.0.0.1"))
  (is-false (icl::valid-embed-origin-p "http://example.com:8080"))
  (is-false (icl::valid-embed-origin-p "http://127.0.0.1:8080/path"))
  (is-false (icl::valid-embed-origin-p "http://127.0.0.1.evil.example"))
  (is-false (icl::valid-embed-origin-p "*"))
  (is-false (icl::valid-embed-origin-p "http://127.0.0.1:8080 https://evil.example")))

(test browser-policy-frames-only-self-unless-embedded
  "frame-ancestors names ICL itself, and each embed origin."
  (let ((icl::*embed-origins* nil))
    (is (search "frame-ancestors 'self';" (icl::browser-content-security-policy))))
  (let ((icl::*embed-origins* '("http://127.0.0.1:8080" "http://localhost:9090")))
    (is (search "frame-ancestors 'self' http://127.0.0.1:8080 http://localhost:9090;"
                (icl::browser-content-security-policy)))))
