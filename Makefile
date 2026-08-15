.DEFAULT_GOAL := all

.PHONY: all luvcraft run test parinfer-check shader-validate msl-validate smoke mcluv wiki wiki-cli objective-c-probe metal-clear metal-shader metal-pipeline clean

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
		--eval '(luv.spir-v:write-spir-v (luv.spir-v:block-world-sky-fragment-shader) #p"build/block-world-sky.frag.spv")' \
		--eval '(luv.spir-v:write-spir-v (luv.spir-v:block-world-shadow-vertex-shader) #p"build/block-world-shadow.vert.spv")'
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world.vert.spv
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world.frag.spv
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world-crosshair.vert.spv
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world-crosshair.frag.spv
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world-sky.vert.spv
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world-sky.frag.spv
	nix develop -c spirv-val --target-env vulkan1.0 build/block-world-shadow.vert.spv

msl-validate:
	mkdir -p build
	nix develop -c sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:load-system :luv/msl)' \
		--eval '(asdf:load-system :luv/luvcraft/shaders)' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.spir-v:block-world-vertex-specification)) #p"build/block-world.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.spir-v:block-world-fragment-specification)) #p"build/block-world.frag.metal")'
	xcrun metal -std=metal4.0 -c build/block-world.vert.metal -o build/block-world.vert.air
	xcrun metal -std=metal4.0 -c build/block-world.frag.metal -o build/block-world.frag.air

smoke: luvcraft
	mkdir -p build
	nix develop -c ./build/luvcraft --smoke-test build/luvcraft-smoke.png

mcluv:
	nix develop -c sbcl --script build-mcluv.lisp

wiki-cli:
	nix develop -c sbcl --script build-wiki.lisp

wiki:
	nix develop -c sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:make :luv/wiki)'

objective-c-probe:
	nix develop -c sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:load-system :luv/metal/probe)' \
		--eval '(format t "~S~%" (luv.metal:probe-system-default-device))'

metal-clear:
	nix develop -c sbcl --script tools/metal-clear.lisp

metal-shader:
	nix develop -c sbcl --script tools/metal-shader.lisp

metal-pipeline:
	nix develop -c sbcl --script tools/metal-pipeline.lisp

clean:
	rm -f ./build/luvcraft ./build/mcluv ./build/luvcraft-smoke.png
	rm -f ./build/block-world.vert.metal ./build/block-world.vert.air
	rm -f ./build/block-world.frag.metal ./build/block-world.frag.air
	rm -f ./build/objective-c-exception-bridge-*.dylib
	rm -rf ./build/wiki ./build/wiki-cli
