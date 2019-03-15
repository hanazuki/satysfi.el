elisps := $(wildcard *.el)
elisp_objects = $(patsubst 	%.el,%.elc,${elisps})

all: ${elisp_objects}

%.elc: %.el
	emacs -batch -L . -f batch-byte-compile $<
