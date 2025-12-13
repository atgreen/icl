int-compile-sbcl:
	sbcl --load ./scripts/run-compile.lisp

int-compile-clisp:
	clisp -x '(load "./scripts/run-compile.lisp")'

clean:
	find -name "*.fasl" -exec rm {} \;
	find -name "*.fas" -exec rm {} \;

int-test-sbcl: 
	sbcl --load ./scripts/run-tests.lisp	

int-test-clisp: 
	clisp -x '(load "./scripts/run-tests.lisp")'

compile-sbcl: clean int-compile-sbcl

compile-clisp: clean int-compile-clisp

test-sbcl: clean int-test-sbcl

test-clisp: clean int-test-clisp
