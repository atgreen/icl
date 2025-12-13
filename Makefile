icl: src/*.lisp *.asd
	sbcl --eval "(require 'asdf)" \
	     --eval "(asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :directory (uiop:getcwd)) (list :tree (merge-pathnames \"ocicl/\" (uiop:getcwd))) (list :tree (merge-pathnames \"3rd-party/\" (uiop:getcwd)))))" \
	     --eval "(asdf:make :icl)" --quit

clean:
	rm -rf *~ icl
