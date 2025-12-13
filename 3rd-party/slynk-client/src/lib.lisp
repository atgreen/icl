(in-package #:slynk-client)

(defmacro destructure-case (value &body patterns)
  "Dispatches VALUE to one of PATTERNS.  A cross between CASE and DESTRUCTURING-BIND.
The pattern syntax is: ((HEAD . ARGS) . BODY).  The list of patterns is searched
for a HEAD that's EQ to the car of VALUE.  If one is found, BODY is executed
with ARGS bound to the corresponding values in the CDR of VALUE."
  (let ((operator (gensym "op-"))
        (operands (gensym "rand-"))
        (tmp (gensym "tmp-")))
    `(let* ((,tmp ,value)
            (,operator (car ,tmp))
            (,operands (cdr ,tmp)))
       (case ,operator
	 ,@(mapcar (lambda (clause)
		     (if (eq (car clause) t)
			 `(t ,@(cdr clause))
			 (destructuring-bind ((op &rest rands) &rest body) clause
			   `(,op (destructuring-bind ,rands ,operands
				   . ,body)))))
	    patterns)
	 ,@(if (eq (caar (last patterns)) t)
	       '()
	       `((t (error "destructure-case failed: ~S" ,tmp))))))))

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector (&optional length) `(simple-array octet (,length)))

(define-constant +maximum-vector-index+ (1- array-dimension-limit)
  :documentation "Largest valid vector index."
  :test #'=)

(deftype vector-index ()
  "Integer that can be used as a subscript for accessing an array or vector element."
  '(integer 0 #.+maximum-vector-index+))

(defun octet-vector (&rest octets)
  (coerce octets 'octet-vector))

(defun string-to-utf8-octets (string &key (start 0) (end (length string)))
  "Convert STRING into an OCTET-VECTOR by UTF-8 encoding each character."
  (declare (type string string)
           (type vector-index start end))
  #+allegro
  (excl:string-to-octets string :start start :end end :null-terminate nil :external-format :utf8)
  #+ccl
  (ccl:encode-string-to-octets string :start start :end end :external-format :utf-8)
  #+clisp
  (ext:convert-string-to-bytes string charset:utf-8 :start start :end end)
  #+sbcl
  (sb-ext:string-to-octets string :start start :end end :external-format :utf-8)
  #-(or allegro ccl clisp sbcl)
  (trivial-utf-8:string-to-utf-8-bytes (subseq string start end)))

(defun utf8-octets-to-string (octets &key (start 0) (end (length octets)))
  "Convert OCTETS, a vector of UTF-8 encoded octets, into a string."
  (declare (type octet-vector octets)
           (type vector-index start end))
  #+allegro
  (excl:octets-to-string octets :start start :end end :external-format :utf8)
  #+ccl
  (ccl:decode-string-from-octets octets :start start :end end :external-format :utf-8)
  #+clisp
  (ext:convert-string-from-bytes octets charset:utf-8 :start start :end end)
  #+sbcl
  (sb-ext:octets-to-string octets :start start :end end :external-format :utf8)
  #-(or allegro ccl clisp sbcl)
  (trivial-utf-8:utf-8-bytes-to-string (subseq octets start end)))

(defun make-octet-vector (octet-count &key initial-contents)
  "Create an OCTET-VECTOR containing OCTET-COUNT octets.  If INITIAL-CONTENTS
is not supplied, each element of the vector is initialized to zero.  Otherwise,
the vector is initialized to the contents of list INITIAL-CONTENTS."
  (declare (type vector-index octet-count)
           (type list initial-contents))
  (if initial-contents
      (make-array octet-count :element-type 'octet :initial-contents initial-contents)
      (make-array octet-count :element-type 'octet :initial-element 0)))

