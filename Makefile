.DEFAULT_GOAL := all

LUVCRAFT_BENCHMARK_FRAMES ?= 120
LUVCRAFT_BENCHMARK_CSV ?= build/luvcraft-metal-benchmark.csv
LUVCRAFT_BENCHMARK_SCENARIO ?= steady
LUVCRAFT_STREAMING_BENCHMARK_CSV ?= build/luvcraft-metal-streaming-benchmark.csv
TRACY_STREAMING_TRACE ?= build/luvcraft-streaming.tracy
TRACY_MCCLIM_ROUNDRECT_TRACE ?= build/mcclim-roundrect.tracy
TRACY_MCCLIM_PAINT_TRACE ?= build/mcclim-paints.tracy

.PHONY: all luvcraft run test parinfer-check shader-validate msl-validate smoke vulkan-smoke metal-smoke metal-text-closeup metal-benchmark metal-streaming-benchmark tracy-streaming tracy-mcclim-roundrect tracy-mcclim-paints readme-screenshots mcclim-gallery wiki wiki-cli objective-c-probe metal-clear metal-shader metal-pipeline metal-draw roundrect-proof slug-proof slug-text-proof clean

all: luvcraft

luvcraft:
	./scripts/dev sbcl --script luvcraft/build.lisp

run: luvcraft
	./scripts/dev ./build/luvcraft

test: parinfer-check shader-validate
	@./scripts/dev sbcl --noinform --load scripts/test.lisp --quit

parinfer-check:
	@./scripts/dev sh -c 'tmp=$$(mktemp); trap "rm -f $$tmp" EXIT; if ! ./sly parinfer --batch --strict --check $$(rg --files -g"*.lisp") >"$$tmp" 2>&1; then cat "$$tmp"; exit 1; fi; echo "parinfer: strict check passed."'

shader-validate:
	@mkdir -p build
	@./scripts/dev sbcl --noinform --non-interactive \
		--eval '(require :asdf)' \
		--eval '(handler-bind ((warning (function muffle-warning))) (progn (asdf:load-asd (truename "luv.asd")) (asdf:load-asd (truename "luvcraft.asd")) (asdf:load-asd (truename "mcluv.asd")) (asdf:load-system :luvcraft) (asdf:load-system :mcluv/backend) (asdf:load-system :mcluv/luvcraft)))' \
		--eval '(handler-bind ((warning (function muffle-warning))) (progn (luv.spir-v:write-spir-v (luvcraft.shaders:block-world-vertex-shader) #p"build/block-world.vert.spv") (luv.spir-v:write-spir-v (luvcraft.shaders:block-world-fragment-shader) #p"build/block-world.frag.spv") (luv.spir-v:write-spir-v (luvcraft.shaders:block-world-crosshair-vertex-shader) #p"build/block-world-crosshair.vert.spv") (luv.spir-v:write-spir-v (luvcraft.shaders:block-world-crosshair-fragment-shader) #p"build/block-world-crosshair.frag.spv") (luv.spir-v:write-spir-v (luvcraft.shaders:block-world-sky-vertex-shader) #p"build/block-world-sky.vert.spv") (luv.spir-v:write-spir-v (luvcraft.shaders:block-world-sky-fragment-shader) #p"build/block-world-sky.frag.spv") (luv.spir-v:write-spir-v (luvcraft.shaders:block-world-shadow-vertex-shader) #p"build/block-world-shadow.vert.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luvcraft.shaders:block-world-text-vertex-specification)) #p"build/block-world-text.vert.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luvcraft.shaders:block-world-text-fragment-specification)) #p"build/block-world-text.frag.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luv.spir-v:shader-specification-for :terminal-cell :vertex)) #p"build/terminal-cell.vert.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luv.spir-v:shader-specification-for :terminal-cell :fragment)) #p"build/terminal-cell.frag.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luv.spir-v:shader-specification-for :video-screen :vertex)) #p"build/video-screen.vert.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luv.spir-v:shader-specification-for :video-screen :fragment)) #p"build/video-screen.frag.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luv.spir-v:shader-specification-for :terminal-screen :vertex)) #p"build/terminal-screen.vert.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luv.spir-v:shader-specification-for :terminal-screen :fragment)) #p"build/terminal-screen.frag.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luv.spir-v:shader-specification-for :terminal-faceplate :fragment)) #p"build/terminal-faceplate.frag.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luv.analytic:roundrect-vertex-specification)) #p"build/analytic-roundrect.vert.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luv.analytic:roundrect-fragment-specification)) #p"build/analytic-roundrect.frag.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luv.slug:slug-bezier-vertex-specification)) #p"build/slug-bezier.vert.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (luv.slug:slug-bezier-fragment-specification)) #p"build/slug-bezier.frag.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (mcluv::gradient-roundrect-vertex-specification)) #p"build/mcluv-gradient.vert.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (mcluv::gradient-roundrect-fragment-specification)) #p"build/mcluv-gradient.frag.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (mcluv::relief-roundrect-vertex-specification)) #p"build/mcluv-relief.vert.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (mcluv::relief-roundrect-fragment-specification)) #p"build/mcluv-relief.frag.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (mcluv::widget-relief-world-vertex-specification)) #p"build/mcluv-world-relief.vert.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (mcluv::widget-relief-world-fragment-specification)) #p"build/mcluv-world-relief.frag.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (mcluv::image-roundrect-vertex-specification)) #p"build/mcluv-image.vert.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (mcluv::image-roundrect-fragment-specification)) #p"build/mcluv-image.frag.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (mcluv::spinning-texture-vertex-specification)) #p"build/mcluv-compositor.vert.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (mcluv::spinning-texture-fragment-specification)) #p"build/mcluv-compositor.frag.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (mcluv::lisp-machine-chassis-vertex-specification)) #p"build/mcluv-chassis.vert.spv") (luv.spir-v:write-spir-v (luv.spir-v:assemble-shader-specification (mcluv::lisp-machine-chassis-fragment-specification)) #p"build/mcluv-chassis.frag.spv")))'
	@./scripts/dev spirv-val --target-env vulkan1.0 build/block-world.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/block-world.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/block-world-crosshair.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/block-world-crosshair.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/block-world-sky.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/block-world-sky.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/block-world-shadow.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/block-world-text.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/block-world-text.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/terminal-cell.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/terminal-cell.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/video-screen.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/video-screen.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/terminal-screen.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/terminal-screen.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/terminal-faceplate.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/analytic-roundrect.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/analytic-roundrect.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/slug-bezier.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/slug-bezier.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/mcluv-gradient.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/mcluv-gradient.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/mcluv-relief.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/mcluv-relief.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/mcluv-world-relief.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/mcluv-world-relief.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/mcluv-image.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/mcluv-image.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/mcluv-compositor.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/mcluv-compositor.frag.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/mcluv-chassis.vert.spv
	@./scripts/dev spirv-val --target-env vulkan1.0 build/mcluv-chassis.frag.spv
	@echo "shader-validate: all SPIR-V shaders valid."

msl-validate:
	mkdir -p build
	./scripts/dev sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:load-asd (truename "luvcraft.asd"))' \
		--eval '(asdf:load-asd (truename "mcluv.asd"))' \
		--eval '(asdf:load-system :luvcraft)' \
		--eval '(asdf:load-system :mcluv/backend)' \
		--eval '(asdf:load-system :mcluv/luvcraft)' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luvcraft.shaders:block-world-vertex-specification)) #p"build/block-world.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luvcraft.shaders:block-world-fragment-specification)) #p"build/block-world.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luvcraft.shaders:block-world-text-vertex-specification)) #p"build/block-world-text.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luvcraft.shaders:block-world-text-fragment-specification)) #p"build/block-world-text.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.spir-v:shader-specification-for :terminal-cell :vertex)) #p"build/terminal-cell.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.spir-v:shader-specification-for :terminal-cell :fragment)) #p"build/terminal-cell.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.spir-v:shader-specification-for :terminal-screen :vertex)) #p"build/terminal-screen.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.spir-v:shader-specification-for :terminal-screen :fragment)) #p"build/terminal-screen.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.spir-v:shader-specification-for :terminal-faceplate :fragment)) #p"build/terminal-faceplate.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.analytic:roundrect-vertex-specification)) #p"build/analytic-roundrect.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.analytic:roundrect-fragment-specification)) #p"build/analytic-roundrect.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.slug:slug-bezier-vertex-specification)) #p"build/slug-bezier.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.slug:slug-bezier-fragment-specification)) #p"build/slug-bezier.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::gradient-roundrect-vertex-specification)) #p"build/mcluv-gradient.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::gradient-roundrect-fragment-specification)) #p"build/mcluv-gradient.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::relief-roundrect-vertex-specification)) #p"build/mcluv-relief.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::relief-roundrect-fragment-specification)) #p"build/mcluv-relief.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::widget-relief-world-vertex-specification)) #p"build/mcluv-world-relief.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::widget-relief-world-fragment-specification)) #p"build/mcluv-world-relief.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::image-roundrect-vertex-specification)) #p"build/mcluv-image.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::image-roundrect-fragment-specification)) #p"build/mcluv-image.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::spinning-texture-vertex-specification)) #p"build/mcluv-compositor.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::spinning-texture-fragment-specification)) #p"build/mcluv-compositor.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::lisp-machine-chassis-vertex-specification)) #p"build/mcluv-chassis.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::lisp-machine-chassis-fragment-specification)) #p"build/mcluv-chassis.frag.metal")'
	xcrun metal -std=metal4.0 -c build/block-world.vert.metal -o build/block-world.vert.air
	xcrun metal -std=metal4.0 -c build/block-world.frag.metal -o build/block-world.frag.air
	xcrun metal -std=metal4.0 -c build/block-world-text.vert.metal -o build/block-world-text.vert.air
	xcrun metal -std=metal4.0 -c build/block-world-text.frag.metal -o build/block-world-text.frag.air
	xcrun metal -std=metal4.0 -c build/terminal-cell.vert.metal -o build/terminal-cell.vert.air
	xcrun metal -std=metal4.0 -c build/terminal-cell.frag.metal -o build/terminal-cell.frag.air
	xcrun metal -std=metal4.0 -c build/terminal-screen.vert.metal -o build/terminal-screen.vert.air
	xcrun metal -std=metal4.0 -c build/terminal-screen.frag.metal -o build/terminal-screen.frag.air
	xcrun metal -std=metal4.0 -c build/terminal-faceplate.frag.metal -o build/terminal-faceplate.frag.air
	xcrun metal -std=metal4.0 -c build/analytic-roundrect.vert.metal -o build/analytic-roundrect.vert.air
	xcrun metal -std=metal4.0 -c build/analytic-roundrect.frag.metal -o build/analytic-roundrect.frag.air
	xcrun metal -std=metal4.0 -c build/slug-bezier.vert.metal -o build/slug-bezier.vert.air
	xcrun metal -std=metal4.0 -c build/slug-bezier.frag.metal -o build/slug-bezier.frag.air
	xcrun metal -std=metal4.0 -c build/mcluv-gradient.vert.metal -o build/mcluv-gradient.vert.air
	xcrun metal -std=metal4.0 -c build/mcluv-gradient.frag.metal -o build/mcluv-gradient.frag.air
	xcrun metal -std=metal4.0 -c build/mcluv-relief.vert.metal -o build/mcluv-relief.vert.air
	xcrun metal -std=metal4.0 -c build/mcluv-relief.frag.metal -o build/mcluv-relief.frag.air
	xcrun metal -std=metal4.0 -c build/mcluv-world-relief.vert.metal -o build/mcluv-world-relief.vert.air
	xcrun metal -std=metal4.0 -c build/mcluv-world-relief.frag.metal -o build/mcluv-world-relief.frag.air
	xcrun metal -std=metal4.0 -c build/mcluv-image.vert.metal -o build/mcluv-image.vert.air
	xcrun metal -std=metal4.0 -c build/mcluv-image.frag.metal -o build/mcluv-image.frag.air
	xcrun metal -std=metal4.0 -c build/mcluv-compositor.vert.metal -o build/mcluv-compositor.vert.air
	xcrun metal -std=metal4.0 -c build/mcluv-compositor.frag.metal -o build/mcluv-compositor.frag.air
	xcrun metal -std=metal4.0 -c build/mcluv-chassis.vert.metal -o build/mcluv-chassis.vert.air
	xcrun metal -std=metal4.0 -c build/mcluv-chassis.frag.metal -o build/mcluv-chassis.frag.air

smoke: luvcraft
	mkdir -p build
	./scripts/dev ./build/luvcraft --smoke-test build/luvcraft-smoke.png

vulkan-smoke: luvcraft
	mkdir -p build
	./scripts/dev ./build/luvcraft --vulkan-smoke-test build/luvcraft-vulkan-smoke.png

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

tracy-mcclim-roundrect:
	./scripts/trace-mcclim-roundrect $(TRACY_MCCLIM_ROUNDRECT_TRACE)

tracy-mcclim-paints:
	./scripts/trace-mcclim-paints $(TRACY_MCCLIM_PAINT_TRACE)

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

roundrect-proof:
	./scripts/dev sbcl --script hal/metal/probes/analytic-roundrect.lisp build/analytic-roundrect-proof.png

slug-proof:
	./scripts/dev sbcl --script hal/metal/probes/slug-bezier.lisp build/slug-bezier-proof.png

slug-text-proof:
	./scripts/dev sbcl --script hal/metal/probes/slug-text.lisp build/slug-text-proof.png

clean:
	rm -f ./build/luvcraft ./build/mcluv ./build/luvcraft-smoke.png ./build/luvcraft-metal-smoke.png
	rm -f ./build/block-world.vert.metal ./build/block-world.vert.air
	rm -f ./build/block-world.frag.metal ./build/block-world.frag.air
	rm -f ./build/slug-bezier.vert.spv ./build/slug-bezier.frag.spv
	rm -f ./build/analytic-roundrect.vert.spv ./build/analytic-roundrect.frag.spv
	rm -f ./build/analytic-roundrect.vert.metal ./build/analytic-roundrect.vert.air
	rm -f ./build/analytic-roundrect.frag.metal ./build/analytic-roundrect.frag.air
	rm -f ./build/slug-bezier.vert.metal ./build/slug-bezier.vert.air
	rm -f ./build/slug-bezier.frag.metal ./build/slug-bezier.frag.air
	rm -f ./build/slug-bezier-proof.png
	rm -f ./build/analytic-roundrect-proof.png
	rm -f ./build/slug-text-proof.png
	rm -f ./build/objective-c-exception-bridge-*.dylib
	rm -rf ./build/wiki ./build/wiki-cli
