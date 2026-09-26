# Shader validation, GPU smoke tests, and Metal probes.

.PHONY: shader-validate luft-shader-validate msl-validate smoke vulkan-smoke metal-smoke \
	metal-text-closeup objective-c-probe metal-clear metal-shader metal-pipeline metal-draw \
	roundrect-proof slug-proof slug-text-proof

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
	@# Mesh stages emit SPIR-V 1.4, supported by Vulkan 1.2 and later.
	@sh -c 'status=0; for f in build/luft-*.spv; do spirv-val --target-env vulkan1.2 "$$f" || status=1; done; exit $$status'
	@sh -c 'sha256sum build/luft-*.spv'
	@echo "luft-shader-validate: all LUFT SPIR-V modules valid."

MSL_SHADERS := block-world.vert \
	block-world.frag \
	block-world-text.vert \
	block-world-text.frag \
	terminal-cell.vert \
	terminal-cell.frag \
	terminal-screen.vert \
	terminal-screen.frag \
	terminal-faceplate.frag \
	analytic-roundrect.vert \
	analytic-roundrect.frag \
	slug-bezier.vert \
	slug-bezier.frag \
	mcluv-gradient.vert \
	mcluv-gradient.frag \
	mcluv-relief.vert \
	mcluv-relief.frag \
	mcluv-world-relief.vert \
	mcluv-world-relief.frag \
	mcluv-image.vert \
	mcluv-image.frag \
	mcluv-compositor.vert \
	mcluv-compositor.frag \
	mcluv-chassis.vert \
	mcluv-chassis.frag

msl-validate:
	@sbcl --script scripts/write-production-msl.lisp
	@set -e; for shader in $(MSL_SHADERS); do \
		xcrun metal -std=metal4.0 -c "build/$$shader.metal" -o "build/$$shader.air"; \
	done

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

objective-c-probe:
	sbcl --non-interactive \
		--eval '(require :asdf)' \
		--eval '(asdf:load-asd (truename "luv.asd"))' \
		--eval '(asdf:load-system :luv)' \
		--eval '(format t "~S~%" (luv.metal:probe-system-default-device))'

metal-clear metal-shader metal-pipeline metal-draw:
	sbcl --script hal/metal/probes/$(patsubst metal-%,%,$@).lisp

roundrect-proof:
	sbcl --script hal/metal/probes/analytic-roundrect.lisp build/analytic-roundrect-proof.png

slug-proof:
	sbcl --script hal/metal/probes/slug-bezier.lisp build/slug-bezier-proof.png

slug-text-proof:
	sbcl --script hal/metal/probes/slug-text.lisp build/slug-text-proof.png

