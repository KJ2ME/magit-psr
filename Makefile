EMACS ?= emacs

test:
	$(EMACS) -batch -l ert -l test/magit-psr-test.el \
		-f ert-run-tests-batch-and-exit

.PHONY: test
