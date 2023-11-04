.POSIX:
EMACS = emacs

COMPILATIONS = copy-data-mode.elc copy-data-mode-test.elc

.PHONY: all check clean

all : $(COMPILATIONS)

copy-data-mode-test.elc : copy-data-mode.elc

%.elc : %.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<

check: copy-data-mode-test.elc 	#This test needs interactive Emacs environment
	$(EMACS) -Q -nw -L . -l copy-data-mode-test.elc -f ert-run-tests-batch --eval '(switch-to-buffer "*Messages*")'

clean:
	rm -f $(COMPILATIONS)

