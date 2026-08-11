.DEFAULT_GOAL := all

.PHONY: all luvcraft run test smoke mcluv clean

all: luvcraft

luvcraft:
	nix develop -c sbcl --script build-luvcraft.lisp

run: luvcraft
	./luvcraft

test:
	nix develop -c sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:test-system :luv)'

smoke: luvcraft
	mkdir -p build
	./luvcraft --smoke-test build/luvcraft-smoke.png

mcluv:
	nix develop -c sbcl --script build-mcluv.lisp

clean:
	rm -f ./luvcraft ./build/luvcraft-smoke.png
