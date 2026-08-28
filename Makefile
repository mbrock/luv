LUV_DEV_SHELL ?= default

ifneq ($(LUV_DEV_ENVIRONMENT_MODE),nix-develop)

.DEFAULT_GOAL := luv-development-environment

.PHONY: luv-development-environment $(MAKECMDGOALS)

luv-development-environment:
	+@LUV_PROJECT_ROOT="$(CURDIR)" exec nix develop --accept-flake-config \
		--no-write-lock-file "path:$(CURDIR)/nix#$(LUV_DEV_SHELL)" \
		-c $(MAKE) $(MAKECMDGOALS)

ifneq ($(strip $(MAKECMDGOALS)),)
$(MAKECMDGOALS): luv-development-environment
	@:
endif

else

.DEFAULT_GOAL := all

LUVCRAFT_BENCHMARK_FRAMES ?= 120
TEST_JOBS ?= $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)
LUVCRAFT_BENCHMARK_CSV ?= build/luvcraft-metal-benchmark.csv
LUVCRAFT_BENCHMARK_SCENARIO ?= steady
LUVCRAFT_BENCHMARK_DENSITY ?= standard
LUVCRAFT_RETINA_BENCHMARK_CSV ?= build/luvcraft-metal-retina-benchmark.csv
LUVCRAFT_STREAMING_BENCHMARK_CSV ?= build/luvcraft-metal-streaming-benchmark.csv
LUFT_MESHER_PROFILE_DIRECTORY ?= build/luft-mesher-profile
LUFT_MESHER_PROFILE_SECONDS ?= 2
LUFT_MESHER_PROFILE_INTERVAL ?= 0.0005
LUFT_MESHER_PROFILE_TIMING_SECONDS ?= 0.25
LUFT_MESHER_COHORT_OUTPUT ?= build/luft-mesher-cohort.txt
LUFT_MESHER_COHORT_WARM_ITERATIONS ?= 5
LUFT_Z_FIBER_BENCHMARK_CSV ?= build/luft-z-fiber-benchmark.csv
LUFT_Z_FIBER_BENCHMARK_WIDTHS ?= 16,32
LUFT_Z_FIBER_BENCHMARK_PATTERNS ?= solid,terrain,architecture,caves,checkerboard
LUFT_Z_FIBER_BENCHMARK_SAMPLES ?= 15
LUFT_Z_FIBER_BENCHMARK_WARMUPS ?= 3
TRACY_STREAMING_TRACE ?= build/luvcraft-streaming.tracy
TRACY_MCCLIM_ROUNDRECT_TRACE ?= build/mcclim-roundrect.tracy
TRACY_MCCLIM_PAINT_TRACE ?= build/mcclim-paints.tracy

FASL_CACHE := $(HOME)/.cache/common-lisp

.PHONY: all sly-client sly-dependency-core luvcraft luft luft-core luft-test run test test-suite capture showcase-bootstrap showcase-render showcase-deploy showcase-publish showcase-status clean-fasls parinfer-check sly-build-lock-check shader-validate luft-shader-validate msl-validate smoke vulkan-smoke metal-smoke metal-text-closeup metal-benchmark metal-streaming-benchmark metal-retina-benchmark luft-mesher-profile luft-mesher-cohort luft-z-fiber-benchmark tracy-streaming tracy-mcclim-roundrect tracy-mcclim-paints readme-screenshots mcclim-gallery wiki wiki-cli objective-c-probe metal-clear metal-shader metal-pipeline metal-draw roundrect-proof slug-proof slug-text-proof clean

ifeq ($(LUV_SLY_SYSTEM),luft)
all: sly-client luft-core sly-dependency-core
sly-dependency-core: luft-core
else
all: sly-client luvcraft luft sly-dependency-core
sly-dependency-core: luvcraft luft
endif

sly-client:
	@./scripts/build-sly-client

sly-dependency-core: sly-client
	@./scripts/build-sly-dependency-core
	@sbcl --core build/sly-dependencies.core --noinform \
		--script scripts/warm-sly-system.lisp

luvcraft:
	@status=build/.luvcraft-build-policy; \
		rm -f "$$status"; \
		LUV_BUILD_POLICY_STATUS="$$status" sbcl --script luvcraft/build.lisp; result=$$?; \
		if [ "$$result" -eq 0 ] && [ -e "$$status" ]; then result=1; fi; \
		rm -f "$$status"; \
		exit "$$result"

luft:
	@status=build/.luft-build-policy; \
		rm -f "$$status"; \
		LUV_BUILD_POLICY_STATUS="$$status" sbcl --script luft/build.lisp; result=$$?; \
		if [ "$$result" -eq 0 ] && [ -e "$$status" ]; then result=1; fi; \
		rm -f "$$status"; \
		exit "$$result"

luft-core:
	@status=build/.luft-core-build-policy; \
		rm -f "$$status"; \
		LUV_BUILD_POLICY_STATUS="$$status" sbcl --script luft/build-core.lisp; result=$$?; \
		if [ "$$result" -eq 0 ] && [ -e "$$status" ]; then result=1; fi; \
		rm -f "$$status"; \
		exit "$$result"

run: luvcraft
	./build/luvcraft

ifeq ($(LUV_SLY_SYSTEM),luft)
test: sly-client
	@$(MAKE) --no-print-directory -j3 parinfer-check sly-build-lock-check luft-test
else
test: sly-client
	@$(MAKE) --no-print-directory -j3 parinfer-check sly-build-lock-check test-suite

test-suite:
	@./scripts/build-sly-dependency-core
	@sbcl --core build/sly-dependencies.core --noinform \
		--script scripts/test.lisp --jobs $(TEST_JOBS)
endif

luft-test:
	@sbcl --script scripts/test-luft.lisp

capture:
	./scripts/captures render

showcase-bootstrap:
	./scripts/showcase bootstrap

showcase-render:
	./scripts/showcase render

showcase-deploy:
	./scripts/showcase deploy

showcase-publish:
	./scripts/showcase publish

showcase-status:
	./scripts/showcase status

parinfer-check:
	@sh -c 'tmp=$$(mktemp); trap "rm -f $$tmp" EXIT; if ! ./sly parinfer --batch --strict --check $$(rg --files -g"*.lisp") >"$$tmp" 2>&1; then cat "$$tmp"; exit 1; fi; echo "parinfer: strict check passed."'

sly-build-lock-check:
	@python3 scripts/with-build-lock-tests.py

shader-validate:
	@sbcl --noinform --non-interactive \
		--eval '(require :asdf)' \
		--eval '(handler-bind ((warning (function muffle-warning))) (progn (asdf:load-asd (truename "luv.asd")) (asdf:load-asd (truename "openai.asd")) (asdf:load-asd (truename "telegram.asd")) (asdf:load-asd (truename "mqtt.asd")) (asdf:load-asd (truename "luvcraft.asd")) (asdf:load-system :luvcraft/agent)))' \
		--load scripts/shader-validation.lisp \
		--eval '(handler-bind ((warning (function muffle-warning))) (luv.shader-validation:validate-production-shaders))'

luft-shader-validate:
	@mkdir -p build
	@rm -f build/luft-*.spv
	@sbcl --noinform --non-interactive \
		--eval '(require :asdf)' \
		--eval '(handler-bind ((warning (function muffle-warning))) (progn (asdf:load-asd (truename "luv.asd")) (asdf:load-asd (truename "luft.asd")) (asdf:load-system :luft/renderer)))' \
		--eval '(handler-bind ((warning (function muffle-warning))) (luft.render.shaders:write-production-spir-v #p"build/"))'
	@sh -c 'status=0; for f in build/luft-*.spv; do spirv-val --target-env vulkan1.0 "$$f" || status=1; done; exit $$status'
	@sh -c 'sha256sum build/luft-*.spv'
	@echo "luft-shader-validate: all LUFT SPIR-V modules valid."

msl-validate:
	mkdir -p build
	sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:load-asd (truename "luvcraft.asd"))' \
		--eval '(asdf:load-system :luvcraft)' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luvcraft.shaders:block-world-vertex-specification)) #p"build/block-world.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luvcraft.shaders:block-world-fragment-specification)) #p"build/block-world.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luvcraft.shaders:block-world-text-vertex-specification)) #p"build/block-world-text.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luvcraft.shaders:block-world-text-fragment-specification)) #p"build/block-world-text.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.shader:shader-specification-for :terminal-cell :vertex)) #p"build/terminal-cell.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.shader:shader-specification-for :terminal-cell :fragment)) #p"build/terminal-cell.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.shader:shader-specification-for :terminal-screen :vertex)) #p"build/terminal-screen.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.shader:shader-specification-for :terminal-screen :fragment)) #p"build/terminal-screen.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.shader:shader-specification-for :terminal-faceplate :fragment)) #p"build/terminal-faceplate.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.analytic:roundrect-vertex-specification)) #p"build/analytic-roundrect.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.analytic:roundrect-fragment-specification)) #p"build/analytic-roundrect.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.slug:slug-bezier-vertex-specification)) #p"build/slug-bezier.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (luv.slug:slug-bezier-fragment-specification)) #p"build/slug-bezier.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::gradient-roundrect-vertex-specification)) #p"build/mcluv-gradient.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::gradient-roundrect-fragment-specification)) #p"build/mcluv-gradient.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::relief-roundrect-vertex-specification)) #p"build/mcluv-relief.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::relief-roundrect-fragment-specification)) #p"build/mcluv-relief.frag.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::direct-widget-relief-vertex-specification)) #p"build/mcluv-world-relief.vert.metal")' \
		--eval '(luv.msl:write-msl (luv.msl:compile-msl (mcluv::relief-roundrect-fragment-specification)) #p"build/mcluv-world-relief.frag.metal")' \
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
	./build/luvcraft --smoke-test build/luvcraft-smoke.png

vulkan-smoke: luvcraft
	mkdir -p build
	./build/luvcraft --vulkan-smoke-test build/luvcraft-vulkan-smoke.png

metal-smoke: luvcraft
	mkdir -p build
	MTL_DEBUG_LAYER=1 ./build/luvcraft --metal-smoke-test build/luvcraft-metal-smoke.png

metal-text-closeup: luvcraft
	mkdir -p build
	MTL_DEBUG_LAYER=1 ./build/luvcraft --metal-text-closeup build/luvcraft-metal-text-closeup.png

metal-benchmark: luvcraft
	mkdir -p build
	./build/luvcraft --metal-benchmark $(LUVCRAFT_BENCHMARK_FRAMES) $(LUVCRAFT_BENCHMARK_CSV) $(LUVCRAFT_BENCHMARK_SCENARIO) $(LUVCRAFT_BENCHMARK_DENSITY)

metal-streaming-benchmark:
	$(MAKE) metal-benchmark LUVCRAFT_BENCHMARK_SCENARIO=streaming LUVCRAFT_BENCHMARK_CSV=$(LUVCRAFT_STREAMING_BENCHMARK_CSV)

metal-retina-benchmark:
	$(MAKE) metal-benchmark LUVCRAFT_BENCHMARK_DENSITY=retina LUVCRAFT_BENCHMARK_CSV=$(LUVCRAFT_RETINA_BENCHMARK_CSV)

luft-mesher-profile:
	mkdir -p $(LUFT_MESHER_PROFILE_DIRECTORY)
	sbcl --script scripts/luft-mesher-profile.lisp \
		$(LUFT_MESHER_PROFILE_DIRECTORY) \
		$(LUFT_MESHER_PROFILE_SECONDS) \
		$(LUFT_MESHER_PROFILE_INTERVAL) \
		$(LUFT_MESHER_PROFILE_TIMING_SECONDS)

luft-mesher-cohort:
	mkdir -p build
	sbcl --script scripts/luft-mesher-cohort.lisp \
		$(LUFT_MESHER_COHORT_OUTPUT) \
		$(LUFT_MESHER_COHORT_WARM_ITERATIONS)

luft-z-fiber-benchmark:
	mkdir -p build
	sbcl --script scripts/luft-z-fiber-benchmark.lisp \
		$(LUFT_Z_FIBER_BENCHMARK_CSV) \
		$(LUFT_Z_FIBER_BENCHMARK_WIDTHS) \
		$(LUFT_Z_FIBER_BENCHMARK_PATTERNS) \
		$(LUFT_Z_FIBER_BENCHMARK_SAMPLES) \
		$(LUFT_Z_FIBER_BENCHMARK_WARMUPS)

tracy-streaming: luvcraft
	./scripts/trace-luvcraft-streaming $(TRACY_STREAMING_TRACE)

tracy-mcclim-roundrect:
	./scripts/trace-mcclim-roundrect $(TRACY_MCCLIM_ROUNDRECT_TRACE)

tracy-mcclim-paints:
	./scripts/trace-mcclim-paints $(TRACY_MCCLIM_PAINT_TRACE)

readme-screenshots:
	sbcl --script scripts/readme-screenshots.lisp screenshots

mcclim-gallery:
	sbcl --script scripts/mcclim-gallery.lisp build/mcclim-gallery

wiki-cli:
	sbcl --script wiki/build.lisp

wiki:
	./scripts/wiki build

objective-c-probe:
	sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:load-system :luv)' \
		--eval '(format t "~S~%" (luv.metal:probe-system-default-device))'

metal-clear:
	sbcl --script hal/metal/probes/clear.lisp

metal-shader:
	sbcl --script hal/metal/probes/shader.lisp

metal-pipeline:
	sbcl --script hal/metal/probes/pipeline.lisp

metal-draw:
	sbcl --script hal/metal/probes/draw.lisp

roundrect-proof:
	sbcl --script hal/metal/probes/analytic-roundrect.lisp build/analytic-roundrect-proof.png

slug-proof:
	sbcl --script hal/metal/probes/slug-bezier.lisp build/slug-bezier-proof.png

slug-text-proof:
	sbcl --script hal/metal/probes/slug-text.lisp build/slug-text-proof.png

clean-fasls:
	@for dir in $(FASL_CACHE)/*$(CURDIR); do \
		[ -d "$$dir" ] || continue; \
		echo "removing $$dir"; \
		rm -rf "$$dir"; \
	done

clean:
	rm -rf ./build/logs
	rm -f ./build/luvcraft ./build/luft-atelier ./build/mcluv ./build/luvcraft-smoke.png ./build/luvcraft-metal-smoke.png
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

endif
