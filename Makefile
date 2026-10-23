EMACS ?= emacs
# lisp/ of lispm-agent, whose editor's agents this adds to: a checkout
# beside this one by default.
LISPM_AGENT ?= ../lispm-agent/lisp

.PHONY: test compile

# The tests stand in for primitives such as message; without native
# trampolines for them, doing so starts no compiler.
test:
	$(EMACS) -Q --batch -L . -L $(LISPM_AGENT) --eval '(setq native-comp-enable-subr-trampolines nil)' \
	  -l test/lispm-holon-test.el -f ert-run-tests-batch-and-exit

# A check: the .elc file is removed again, as it would shadow the source
# of a checkout Emacs loads from.
compile:
	$(EMACS) -Q --batch -L . -L $(LISPM_AGENT) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile lispm-holon.el; \
	status=$$?; rm -f lispm-holon.elc; exit $$status
