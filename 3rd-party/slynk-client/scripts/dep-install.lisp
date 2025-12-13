(require "asdf")

(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(asdf:load-system :quicklisp)

;; Insert the quicklisp external dependencies

(ql:quickload :slynk)
(ql:quickload :fiveam)
(ql:quickload :bordeaux-threads)
(ql:quickload :usocket)
