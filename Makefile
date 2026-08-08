.PHONY: mcluv

mcluv:
	nix develop -c sbcl --script build-mcluv.lisp
