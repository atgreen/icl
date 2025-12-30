.PHONY: all clean lint test check check-versions

all: icl

assets/OPEN-SOURCE-NOTICES.txt: assets/WEB-LICENSES ocicl.csv
	@echo "Generating OPEN-SOURCE-NOTICES.txt..."
	@echo "================================================================================" > $@
	@echo "ICL OPEN SOURCE NOTICES" >> $@
	@echo "================================================================================" >> $@
	@echo "" >> $@
	@echo "Interactive Common Lisp (ICL) is built using many open source components." >> $@
	@echo "This document contains the license notices for all third-party software" >> $@
	@echo "used in ICL." >> $@
	@echo "" >> $@
	@echo "ICL itself is licensed under the MIT License." >> $@
	@echo "Copyright (C) 2025 Anthony Green <green@moxielogic.com>" >> $@
	@echo "" >> $@
	@echo "================================================================================" >> $@
	@echo "TABLE OF CONTENTS" >> $@
	@echo "================================================================================" >> $@
	@echo "" >> $@
	@echo "PART 1: Web Browser Components" >> $@
	@echo "PART 2: Common Lisp Libraries" >> $@
	@echo "" >> $@
	@echo "================================================================================" >> $@
	@echo "PART 1: WEB BROWSER COMPONENTS" >> $@
	@echo "================================================================================" >> $@
	@echo "" >> $@
	@cat assets/WEB-LICENSES >> $@
	@echo "" >> $@
	@echo "================================================================================" >> $@
	@echo "PART 2: COMMON LISP LIBRARIES" >> $@
	@echo "================================================================================" >> $@
	@echo "" >> $@
	@ocicl collect-licenses 2>/dev/null >> $@

slynk.zip:
	@echo "Creating slynk.zip..."
	@if [ -z "$$(ls -d ocicl/sly-*/slynk 2>/dev/null)" ]; then ocicl install slynk; fi
	@rm -f slynk.zip
	sbcl --non-interactive \
	     --eval "(require 'asdf)" \
	     --eval "(asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :directory (uiop:getcwd)) (list :tree (merge-pathnames \"ocicl/\" (uiop:getcwd)))))" \
	     --eval "(asdf:load-system :zip)" \
	     --eval "(zip:zip \"slynk.zip\" (first (directory \"ocicl/sly-*/slynk/\")) :if-exists :supersede)" \
	     --quit

icl: slynk.zip src/*.lisp *.asd
	rm -rf ~/.cache/icl/*
	sbcl --eval "(require 'asdf)" \
	     --eval "(asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :directory (uiop:getcwd)) (list :tree (merge-pathnames \"ocicl/\" (uiop:getcwd))) (list :tree (merge-pathnames \"3rd-party/\" (uiop:getcwd)))))" \
	     --eval "(asdf:make :icl)" --quit

clean:
	rm -rf *~ icl slynk.zip

lint:
	ocicl lint icl.asd

test check:
	sbcl --non-interactive \
	     --eval "(require 'asdf)" \
	     --eval "(asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :directory (uiop:getcwd)) (list :tree (merge-pathnames \"ocicl/\" (uiop:getcwd))) (list :tree (merge-pathnames \"3rd-party/\" (uiop:getcwd)))))" \
	     --eval "(asdf:load-system :icl-tests :force t)" \
	     --eval "(let ((results (fiveam:run 'icl-tests:icl-tests))) (format t \"~&~%Test Results:~%\") (fiveam:explain! results) (finish-output) (if (fiveam:results-status results) (sb-ext:exit :code 0) (sb-ext:exit :code 1)))"

check-versions:
	@sbcl --non-interactive \
	     --eval "(require 'asdf)" \
	     --eval "(asdf:initialize-source-registry (list :source-registry :inherit-configuration (list :directory (uiop:getcwd)) (list :tree (merge-pathnames \"ocicl/\" (uiop:getcwd)))))" \
	     --eval "(asdf:load-system :icl-version-checker)" \
	     --eval "(let ((updates (icl-version-checker:check-library-updates :verbose nil))) (format t \"~A~%\" (icl-version-checker:format-update-report updates)))"
