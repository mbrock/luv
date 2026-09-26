# Enter Nix once, then use its make. Apple's /usr/bin/make cannot run under
# Nix's SDKROOT: it looks for gnumake inside the SDK. The + preserves recursive
# make flags and the jobserver even though the handoff uses a different binary.
LUV_DEV_SHELL ?= default
.DEFAULT_GOAL := all

ifneq ($(LUV_DEV_ENVIRONMENT_MODE),nix-develop)

.PHONY: all $(MAKECMDGOALS)
all:
	+@LUV_DEV_SHELL="$(LUV_DEV_SHELL)" ./env make $(MAKECMDGOALS)

$(filter-out all,$(MAKECMDGOALS)): all
	@:

else

TEST_JOBS ?= $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1)
FASL_CACHE ?= $(HOME)/.cache/common-lisp

.PHONY: all help sly-client sly-dependency-core luvcraft luft luft-core run \
	test test-suite luft-test parinfer-check sly-build-lock-check clean clean-fasls

# The slim environment builds and tests the non-graphical Luft system.
ifeq ($(LUV_DEV_SHELL),slim)
export LUV_SLY_SYSTEM := luft
BUILD_TARGETS := luft-core
TEST_TARGET := luft-test
else ifeq ($(LUV_SLY_SYSTEM),luft)
BUILD_TARGETS := luft-core
TEST_TARGET := luft-test
else
BUILD_TARGETS := luvcraft luft
TEST_TARGET := test-suite
endif

all: sly-dependency-core

sly-client:
	@./scripts/build-sly-client

sly-dependency-core: sly-client $(BUILD_TARGETS)
	@./scripts/build-sly-dependency-core
	@sbcl --core build/sly-dependencies.core --noinform \
		--script scripts/warm-sly-system.lisp

luvcraft: BUILD_SCRIPT = luvcraft/build.lisp
luft: BUILD_SCRIPT = luft/build.lisp
luft-core: BUILD_SCRIPT = luft/build-core.lisp
luvcraft luft luft-core:
	@./scripts/build-application $(BUILD_SCRIPT)

run: luvcraft
	./build/luvcraft

test: sly-client
	+@$(MAKE) --no-print-directory -j4 parinfer-check sly-build-lock-check typst-prty-test $(TEST_TARGET)

test-suite:
	@./scripts/build-sly-dependency-core
	@sbcl --core build/sly-dependencies.core --noinform \
		--script scripts/test.lisp --jobs $(TEST_JOBS)

luft-test:
	@sbcl --script scripts/test-luft.lisp

parinfer-check:
	@sh -c 'tmp=$$(mktemp); trap "rm -f $$tmp" EXIT; if ! ./sly parinfer --batch --strict --check $$(rg --files -g"*.lisp") >"$$tmp" 2>&1; then cat "$$tmp"; exit 1; fi; echo "parinfer: strict check passed."'

sly-build-lock-check:
	@python3 scripts/with-build-lock-tests.py

include make/graphics.mk
include make/benchmarks.mk
include make/publishing.mk

help:
	@printf '%s\n' \
		'make                 Build applications and warm the Sly image' \
		'make test            Run source checks and tests (TEST_JOBS=N)' \
		'make LUV_DEV_SHELL=slim [test]  Build or test non-graphical Luft' \
		'make clean           Remove generated programs and proof artifacts' \
		'make clean-fasls     Remove this checkout’s compiled Lisp cache' \
		'GPU validation and probes: make/graphics.mk' \
		'Benchmarks and profiling:  make/benchmarks.mk' \
		'Wiki and publishing:       make/publishing.mk'

clean-fasls:
	@for dir in $(FASL_CACHE)/*$(CURDIR); do \
		[ -d "$$dir" ] || continue; \
		echo "removing $$dir"; \
		rm -rf "$$dir"; \
	done

# Name owned outputs explicitly; build/ also holds captures and experiment data.
CLEAN_SHADERS := block-world.vert block-world.frag \
	analytic-roundrect.vert analytic-roundrect.frag slug-bezier.vert slug-bezier.frag
CLEAN_PROOFS := analytic-roundrect slug-bezier slug-text

clean:
	rm -rf build/logs build/wiki build/wiki-cli
	rm -f build/luvcraft build/luft-atelier build/mcluv \
		build/luvcraft-smoke.png build/luvcraft-metal-smoke.png \
		$(addprefix build/,$(addsuffix .metal,$(CLEAN_SHADERS))) \
		$(addprefix build/,$(addsuffix .air,$(CLEAN_SHADERS))) \
		$(addprefix build/,$(addsuffix .spv,$(filter-out block-world.%,$(CLEAN_SHADERS)))) \
		$(addprefix build/,$(addsuffix -proof.png,$(CLEAN_PROOFS))) \
		build/objective-c-exception-bridge-*.dylib

endif
