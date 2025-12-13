(in-package #:common-lisp-user)

(defpackage #:slynk-client-test
  (:documentation "Test code in the SLYNK-CLIENT package.")
  (:use #:common-lisp
	#:slynk-client
	#:fiveam)
  (:export #:test-slynk-client))

(in-package #:slynk-client-test)

(def-suite test-slynk-client 
  :description "Main test suite for slynk tests")

(in-suite test-slynk-client)

(alexandria:define-constant +server-count+ 4
  :documentation "Number of maximum servers to run"
  :test #'=)

(defun create-slynk-server ()
  (setf slynk:*configure-emacs-indentation* nil)
  (slynk:create-server :port 0))

(test no-connection ()
  (signals slynk-client:slime-network-error
    (with-slime-connection (connection "localhost" 12345)
      (slime-eval 42 connection))))

(test simple-eval ()
  (with-slime-connection (connection "localhost" (create-slynk-server))
    (is (= (slime-eval 123 connection) 123))))

(test simple-eval-async ()
  (with-slime-connection (connection "localhost" (create-slynk-server))
    (let ((result nil)
          (result-lock (bordeaux-threads:make-lock "result lock")))
      (slime-eval-async 123
                        connection
                        (lambda (x)
                          (bordeaux-threads:with-lock-held (result-lock)
                            (setf result x))))
      (loop until (bordeaux-threads:with-lock-held (result-lock) result))
      (is (= result 123)))))

(test several-connections ()
  (let* ((server-ports (loop repeat +server-count+ collect (create-slynk-server)))
         (connections (loop for port in server-ports collect (slime-connect "localhost" port)))
         (work (make-array +server-count+
                           :initial-contents (loop repeat +server-count+ for i from 2 collect i)))
         (golden (map 'vector (lambda (x) (* x 2)) work)))
    (unwind-protect
         (let ((results (make-array +server-count+ :initial-element nil))
               (results-lock (bordeaux-threads:make-lock "results lock")))
           ;; Synchronous
           (loop for i below (length work)
                 for connection in connections
                 do (setf (aref results i) (slime-eval `(* 2 ,(aref work i)) connection)))
           (is (equalp results golden))
           ;; Reset results.
           (loop for i below (length results) do (setf (aref results i) nil))
           ;; Asynchronous
           (loop for i below (length work)
                 for connection in connections
                 do (let ((index i))
                      (slime-eval-async `(* 2 ,(aref work i))
                                        connection
                                        (lambda (result)
                                          (bordeaux-threads:with-lock-held (results-lock)
                                            (setf (aref results index) result))))))
           (loop while (bordeaux-threads:with-lock-held (results-lock) (some #'null results)))
           (is (equalp results golden)))
      (dolist (connection connections)
        (slime-close connection)))))

(test non-ascii-characters
  (flet ((create-string (code)
           (concatenate 'string "hello " (string (code-char code)) " world")))
    (with-slime-connection (connection "localhost" (create-slynk-server))
      (loop for code from 0 below 2000 by 100 do
	       (let ((string (create-string code)))
		 (is (string= (slime-eval string connection) string)))))))
