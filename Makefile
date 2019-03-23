elisps := $(wildcard *.el)
elisp_objects = $(patsubst 	%.el,%.elc,${elisps})

.PHONY:
all: byte-compile test

byte-compile: ${elisp_objects}

%.elc: %.el
	emacs -batch --no-site-file -L . -f batch-byte-compile $<

.PHONY: test
test:
	${MAKE} -C test
