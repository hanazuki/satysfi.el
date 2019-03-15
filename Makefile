compile: satysfi-mode.elc

%.elc: %.el
	emacs -batch -f batch-byte-compile $<
