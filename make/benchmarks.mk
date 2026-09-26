# Profiling and benchmark experiments.

.PHONY: metal-benchmark metal-streaming-benchmark metal-retina-benchmark \
	luft-mesher-profile luft-mesher-cohort luft-z-fiber-benchmark tracy-streaming \
	tracy-mcclim-roundrect tracy-mcclim-paints

LUVCRAFT_BENCHMARK_FRAMES ?= 120
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

