.DEFAULT_GOAL := all

.PHONY: all luvcraft run test shader-validate smoke mcluv clean

all: luvcraft

luvcraft:
	nix develop -c sbcl --script build-luvcraft.lisp

run: luvcraft
	./luvcraft

test: shader-validate
	nix develop -c sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:test-system :luv)'

shader-validate:
	mkdir -p build
	nix develop -c sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:load-system :luv/spir-v)' \
		--eval '(luv.spir-v:write-spir-v (luv.spir-v:block-world-vertex-shader) #p"build/block-world.vert.spv")' \
		--eval '(luv.spir-v:write-spir-v (luv.spir-v:block-world-fragment-shader) #p"build/block-world.frag.spv")'
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world.vert.spv
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world.frag.spv

smoke: luvcraft
	mkdir -p build
	./luvcraft --smoke-test build/luvcraft-smoke.png

mcluv:
	nix develop -c sbcl --script build-mcluv.lisp

clean:
	rm -f ./luvcraft ./build/luvcraft-smoke.png
