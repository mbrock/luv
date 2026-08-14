.DEFAULT_GOAL := all

.PHONY: all luvcraft run test parinfer-check shader-validate smoke mcluv clean

all: luvcraft

luvcraft:
	nix develop -c sbcl --script build-luvcraft.lisp

run: luvcraft
	nix develop -c ./build/luvcraft

test: parinfer-check shader-validate
	nix develop -c sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:test-system :luv)'

parinfer-check:
	@nix develop -c sh -c 'tmp=$$(mktemp); trap "rm -f $$tmp" EXIT; for file in $$(rg --files -g"*.lisp"); do if ! ./sly parinfer --strict --check "$$file" >"$$tmp" 2>&1; then cat "$$tmp"; exit 1; fi; done; echo "parinfer: strict check passed."'

shader-validate:
	mkdir -p build
	nix develop -c sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:load-system :luv/luvcraft/shaders)' \
		--eval '(luv.spir-v:write-spir-v (luv.spir-v:block-world-vertex-shader) #p"build/block-world.vert.spv")' \
		--eval '(luv.spir-v:write-spir-v (luv.spir-v:block-world-fragment-shader) #p"build/block-world.frag.spv")' \
		--eval '(luv.spir-v:write-spir-v (luv.spir-v:block-world-crosshair-vertex-shader) #p"build/block-world-crosshair.vert.spv")' \
		--eval '(luv.spir-v:write-spir-v (luv.spir-v:block-world-crosshair-fragment-shader) #p"build/block-world-crosshair.frag.spv")' \
		--eval '(luv.spir-v:write-spir-v (luv.spir-v:block-world-sky-vertex-shader) #p"build/block-world-sky.vert.spv")' \
		--eval '(luv.spir-v:write-spir-v (luv.spir-v:block-world-sky-fragment-shader) #p"build/block-world-sky.frag.spv")'
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world.vert.spv
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world.frag.spv
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world-crosshair.vert.spv
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world-crosshair.frag.spv
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world-sky.vert.spv
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world-sky.frag.spv

smoke: luvcraft
	mkdir -p build
	nix develop -c ./build/luvcraft --smoke-test build/luvcraft-smoke.png

mcluv:
	nix develop -c sbcl --script build-mcluv.lisp

clean:
	rm -f ./build/luvcraft ./build/mcluv ./build/luvcraft-smoke.png
