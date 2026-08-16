.DEFAULT_GOAL := all

LUVCRAFT_BENCHMARK_FRAMES ?= 120
LUVCRAFT_BENCHMARK_CSV ?= build/luvcraft-metal-benchmark.csv
LUVCRAFT_BENCHMARK_SCENARIO ?= steady
LUVCRAFT_STREAMING_BENCHMARK_CSV ?= build/luvcraft-metal-streaming-benchmark.csv
TRACY_STREAMING_TRACE ?= build/luvcraft-streaming.tracy

.PHONY: all luvcraft run test parinfer-check shader-validate msl-validate smoke metal-smoke metal-text-closeup metal-benchmark metal-streaming-benchmark tracy-streaming mcluv readme-screenshots mcclim-gallery wiki wiki-cli objective-c-probe metal-clear metal-shader metal-pipeline metal-draw slug-proof slug-text-proof clean

all: luvcraft

luvcraft:
	./scripts/dev sbcl --script luvcraft/build.lisp

run: luvcraft
	./scripts/dev ./build/luvcraft

test: parinfer-check shader-validate
	./scripts/dev sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:load-asd (truename "luvcraft.asd"))' \
		--eval '(asdf:load-asd (truename "luv-wiki.asd"))' \
		--eval '(asdf:load-asd (truename "luv-wiki-site.asd"))' \
		--eval '(asdf:test-system :luv)' \
		--eval '(asdf:test-system :luv/ghostty)' \
		--eval '(asdf:test-system :luvcraft)' \
		--eval '(asdf:test-system :luv-wiki)'

parinfer-check:
	@./scripts/dev sh -c 'tmp=$$(mktemp); trap "rm -f $$tmp" EXIT; if ! ./sly parinfer --batch --strict --check $$(rg --files -g"*.lisp") >"$$tmp" 2>&1; then cat "$$tmp"; exit 1; fi; echo "parinfer: strict check passed."'

shader-validate:
	mkdir -p build
	./scripts/dev sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:load-asd (truename "luvcraft.asd"))' \
		--eval '(asdf:load-system :luvcraft)' \
		--eval '(luv.spir-v:write-spir-v (luvcraft.shaders:block-world-vertex-shader) #p"build/block-world.vert.spv")' \
		--eval '(luv.spir-v:write-spir-v (luvcraft.shaders:block-world-fragment-shader) #p"build/block-world.frag.spv")' \
		--eval '(luv.spir-v:write-spir-v (luvcraft.shaders:block-world-crosshair-vertex-shader) #p"build/block-world-crosshair.vert.spv")' \
		--eval '(luv.spir-v:write-spir-v (luvcraft.shaders:block-world-crosshair-fragment-shader) #p"build/block-world-crosshair.frag.spv")' \
		--eval '(luv.spir-v:write-spir-v (luvcraft.shaders:block-world-sky-vertex-shader) #p"build/block-world-sky.vert.spv")' \
		--eval '(luv.spir-v:write-spir-v (luvcraft.shaders:block-world-sky-fragment-shader) #p"build/block-world-sky.frag.spv")' \
		--eval '(luv.spir-v:write-spir-v (luvcraft.shaders:block-world-shadow-vertex-shader) #p"build/block-world-shadow.vert.spv")' \
		--eval '(luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luvcraft.shaders:block-world-text-vertex-specification)) #p"build/block-world-text.vert.spv")' \
		--eval '(luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luvcraft.shaders:block-world-text-fragment-specification)) #p"build/block-world-text.frag.spv")' \
		--eval '(luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luv.slug:slug-bezier-vertex-specification)) #p"build/slug-bezier.vert.spv")' \
		--eval '(luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luv.slug:slug-bezier-fragment-specification)) #p"build/slug-bezier.frag.spv")'
	./scripts/dev spirv-val --target-env vulkan1.0 build/block-world.vert.spv
	./scripts/dev spirv-val --target-env vulkan1.0 build/block-world.frag.spv
	./scripts/dev spirv-val --target-env vulkan1.0 build/block-world-crosshair.vert.spv
	./scripts/dev spirv-val --target-env vulkan1.0 build/block-world-crosshair.frag.spv
	./scripts/dev spirv-val --target-env vulkan1.0 build/block-world-sky.vert.spv
	./scripts/dev spirv-val --target-env vulkan1.0 build/block-world-sky.frag.spv
	./scripts/dev spirv-val --target-env vulkan1.0 build/block-world-shadow.vert.spv
	./scripts/dev spirv-val --target-env vulkan1.0 build/block-world-text.vert.spv
	./scripts/dev spirv-val --target-env vulkan1.0 build/block-world-text.frag.spv
	./scripts/dev spirv-val --target-env vulkan1.0 build/slug-bezier.vert.spv
	./scripts/dev spirv-val --target-env vulkan1.0 build/slug-bezier.frag.spv

msl-validate:
	mkdir -p build
	./scripts/dev sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:load-asd (truename "luvcraft.asd"))' \
		--eval '(asdf:load-system :luvcraft)' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luvcraft.shaders:block-world-vertex-specification)) #p"build/block-world.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luvcraft.shaders:block-world-fragment-specification)) #p"build/block-world.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luvcraft.shaders:block-world-text-vertex-specification)) #p"build/block-world-text.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luvcraft.shaders:block-world-text-fragment-specification)) #p"build/block-world-text.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.slug:slug-bezier-vertex-specification)) #p"build/slug-bezier.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.slug:slug-bezier-fragment-specification)) #p"build/slug-bezier.frag.metal")'
	xcrun metal -std=metal4.0 -c build/block-world.vert.metal -o build/block-world.vert.air
	xcrun metal -std=metal4.0 -c build/block-world.frag.metal -o build/block-world.frag.air
	xcrun metal -std=metal4.0 -c build/block-world-text.vert.metal -o build/block-world-text.vert.air
	xcrun metal -std=metal4.0 -c build/block-world-text.frag.metal -o build/block-world-text.frag.air
	xcrun metal -std=metal4.0 -c build/slug-bezier.vert.metal -o build/slug-bezier.vert.air
	xcrun metal -std=metal4.0 -c build/slug-bezier.frag.metal -o build/slug-bezier.frag.air

smoke: luvcraft
	mkdir -p build
	./scripts/dev ./build/luvcraft --smoke-test build/luvcraft-smoke.png

metal-smoke: luvcraft
	mkdir -p build
	MTL_DEBUG_LAYER=1 ./scripts/dev ./build/luvcraft --metal-smoke-test build/luvcraft-metal-smoke.png

metal-text-closeup: luvcraft
	mkdir -p build
	MTL_DEBUG_LAYER=1 ./scripts/dev ./build/luvcraft --metal-text-closeup build/luvcraft-metal-text-closeup.png

metal-benchmark: luvcraft
	mkdir -p build
	./scripts/dev ./build/luvcraft --metal-benchmark $(LUVCRAFT_BENCHMARK_FRAMES) $(LUVCRAFT_BENCHMARK_CSV) $(LUVCRAFT_BENCHMARK_SCENARIO)

metal-streaming-benchmark:
	$(MAKE) metal-benchmark LUVCRAFT_BENCHMARK_SCENARIO=streaming LUVCRAFT_BENCHMARK_CSV=$(LUVCRAFT_STREAMING_BENCHMARK_CSV)

tracy-streaming: luvcraft
	./scripts/trace-luvcraft-streaming $(TRACY_STREAMING_TRACE)

mcluv:
	./scripts/dev sbcl --script mcclim/build.lisp

readme-screenshots:
	./scripts/dev sbcl --script scripts/readme-screenshots.lisp screenshots

mcclim-gallery:
	./scripts/dev sbcl --script scripts/mcclim-gallery.lisp build/mcclim-gallery

wiki-cli:
	./scripts/dev sbcl --script wiki/build.lisp

wiki:
	./scripts/wiki build

objective-c-probe:
	./scripts/dev sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:load-system :luv)' \
		--eval '(format t "~S~%" (luv.metal:probe-system-default-device))'

metal-clear:
	./scripts/dev sbcl --script hal/metal/probes/clear.lisp

metal-shader:
	./scripts/dev sbcl --script hal/metal/probes/shader.lisp

metal-pipeline:
	./scripts/dev sbcl --script hal/metal/probes/pipeline.lisp

metal-draw:
	./scripts/dev sbcl --script hal/metal/probes/draw.lisp

slug-proof:
	./scripts/dev sbcl --script hal/metal/probes/slug-bezier.lisp build/slug-bezier-proof.png

slug-text-proof:
	./scripts/dev sbcl --script hal/metal/probes/slug-text.lisp build/slug-text-proof.png

clean:
	rm -f ./build/luvcraft ./build/mcluv ./build/luvcraft-smoke.png ./build/luvcraft-metal-smoke.png
	rm -f ./build/block-world.vert.metal ./build/block-world.vert.air
	rm -f ./build/block-world.frag.metal ./build/block-world.frag.air
	rm -f ./build/slug-bezier.vert.spv ./build/slug-bezier.frag.spv
	rm -f ./build/slug-bezier.vert.metal ./build/slug-bezier.vert.air
	rm -f ./build/slug-bezier.frag.metal ./build/slug-bezier.frag.air
	rm -f ./build/slug-bezier-proof.png
	rm -f ./build/slug-text-proof.png
	rm -f ./build/objective-c-exception-bridge-*.dylib
	rm -rf ./build/wiki ./build/wiki-cli
