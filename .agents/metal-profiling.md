# Metal profiling: get evidence before changing shaders

Validated on yawl, Apple M5, macOS 27.2, Xcode 27.1 (27A9275),
`gpudebug` 1.0, 2026-10-23. This is a versioned workaround, not a claim that
all CLI profiling is broken. Campaign, measurements, and unresolved questions:
[Luft rendering campaign](../wiki/luft-rendering-campaign.org).

## The shortest working route

1. Capture one representative, warmed frame as a `.gputrace`.
2. Open it in Xcode, enable **Profile after replay**, and click **Replay**.
3. Open **Performance** in the left Debug navigator. Verify nonzero shader
   costs and note **Sampled Cores**, execution mode, and performance state.
4. **File → Export… → Embed performance data → Save**, to a new capture path.
5. Load that exported capture with `gpudebug`. Do not immediately run another
   profile: the point is to analyze the data Xcode already collected.

```sh
open -a Xcode "$PWD/build/frame.gputrace"
# After exporting WITH performance data to build/frame-profiled.gputrace:
gpudebug -q -t "$PWD/build/frame-profiled.gputrace" \
  -c 'profile list' -c 'profile load' -c 'go performance/shaders'
# Copy the returned session number; SESSION below is a placeholder.
gpudebug -s SESSION -c 'go /' -c 'go performance/encoders' -c 'list --all'
gpudebug -s SESSION -c 'go /' -c 'go performance/shaders/frag0' -c 'info'
gpudebug --terminate SESSION
gpudebug -l
```

Discover IDs from the current trace: `frag0`, `re1`, `draw0`, library IDs,
and texture IDs are not stable API names. A shader's `Invocations` column
here counts draw uses, not GPU fragment invocations. Use hardware counters
for the latter. Starting a session launches a GPU replayer; it is not just
offline parsing. Reuse one session and terminate it explicitly. Check again
after a short wait if termination has not yet removed it from the list.

## Recognize the failure quickly

In this episode, `profile run` completed, source was embedded, and the
capture said `full`, but every shader cost was zero, shader timelines were
empty, and heatmap fetch said `heatmap builder unavailable`. Basic replay
timings and some hardware counters still worked. This did **not** mean the
shaders were free or that the application lacked source metadata.

The same failure occurred with Luft, Moppe's C++ `.app`, a tiny handwritten
shader, and Apple's deferred-lighting sample. Runtime `MTLDevice`, runtime
`MTL4Compiler`, and offline compilation with
`-gline-tables-only -frecord-sources` did not fix it. Xcode profiling did;
the exported data then worked in the CLI. The exact CLI collection defect
is still unknown.

One CLI attempt is reasonable on a new tool version:
`profile run --gpu-state high --exec overlapping`, then `profile load`.
If the zero-cost/empty-timeline pattern persists, use the GUI bridge rather
than cycling compiler flags, changing shader optimization, reverse-engineering
private replayers, or assuming a missing hardware feature. A `.app` bundle
does not imply offline shader compilation: inspected Moppe used runtime MSL.

## Capture reproducibly

Read [the benchmark contract](../wiki/gpu-frame-benchmarks.org) first. Pin
source revision **and local changes**, fixture/seed/camera, warmup, output
and internal extents, temporal mode, sample count, and resident state.
`BENCH_CAPTURE_DIR` saves PNGs, not GPU traces. `./sly screenshot` likewise
captures the frame image, not shader profiling data.

The already-tested isolated capture recipe and Objective-C capture helper
are in [the native replay bundle](../wiki/measurements/yawl-gpudebug-7480638/README.org).
Its `SCENE_MERGE=0` path captures the ordinary renderer; other settings load
experimental code. Read the harness before use and keep experiments out of
the user's durable Lisp image. Do not run benchmarks alongside a profiler
or leave orphan replay processes competing for the GPU.

Check benchmark logs for native watchdog sampling too: a long offscreen
request can look like a stalled canvas. Two archived relief runs actually
launched `sample` during measurement. The isolated benchmark now suppresses
that sampler; do not disable the production game's watchdog to benchmark it.
Multi-second A/B blocks can also hide a shader gain when GPU frequency varies
by roughly 2×. The [adjacent-frame runtime experiment](../wiki/measurements/yawl-runtime-f87a44f/README.org)
records power telemetry, a null control, reversed order, and the limits of
comparing variants under a shared governor state. Do not translate those
mixed-workload timings directly into displayed FPS or candidate-only clocks.

For an independent control, Apple's
[Rendering a Scene with Deferred Lighting in C++](https://developer.apple.com/documentation/metal/rendering-a-scene-with-deferred-lighting-in-c++)
worked. Its Debug build included line tables and recorded sources. Our SDK
required a command-line deployment-target override to macOS 14.0; no shader
changes were needed. Use one small control to distinguish tooling failure
from application failure, not an expanding collection of sample apps.

## Navigate Xcode without wasting time

- **Heat Map:** select a render encoder in Performance's inner navigator,
  select **Fragment**, then the **Shader Execution Cost** attachment. With
  nothing selected, the tab merely asks for a selection. Black/uncolored
  regions in a sampled heatmap are not proof of zero cost.
- **Source costs:** expand encoder → pipeline in the inner navigator and
  select a draw. Open that draw in the *left Debug navigator*, then in
  **Bound Resources** open its fragment function under **Render Pipeline
  State**. This reliably opened the shader editor; double-clicking the
  Performance shader ranking did not. **Editor → Show Editor Only** removes
  the crowded attachment pane. Hover a cost pie for instruction categories,
  divergence, and register information.
- **Export:** use **File → Export…**, not Performance's share button (which
  offered encoder-counter export). Check **Embed performance data** every
  time; it was not remembered. Wait for archive writing to finish.

Prefer available desktop tooling. If none is connected, native automation
is possible **with the user's authorization and granted macOS permissions**:
System Events for accessible controls, CoreGraphics for actual clicks,
`screencapture` for inspection. Read the applicable computer-use guidance.
Never change TCC databases, disable SIP, or bypass approval prompts.

On this runner, the user had to grant **Amp.app** Automation access to
System Events, Accessibility, and Screen Recording. Grants to **Amp Desktop
Helper** alone did not suffice. Process identity matters. Passwordless sudo
does not grant those permissions. Debugger authorization is another layer:
an earlier `authd`/`task_read_for_pid` stall was resolved by the user approving
`DevToolsSecurity -enable`; that system policy change is not an automatic
setup step.

For authorized automation:

- Inspect a fresh screenshot and the frontmost application before clicking;
  the user may be using the same desktop remotely. Stop if the target changed.
- CoreGraphics uses logical points. This screen was 2560×1664 pixels at
  1280×832 points; do not copy coordinates or divide a resized preview by two.
- AX clicks on static text sometimes did nothing. A real CoreGraphics click
  or double-click worked. Use named AX buttons where possible.
- Materialize `entire contents` before AppleScript iteration, but scope it
  to a small pane or dialog. Enumerating every attribute of thousands of GPU
  command rows is extremely slow. Prefer screenshots over exhaustive AX dumps.
- Keep commands in the shell tool, not typed into Xcode. Do not repeatedly
  steal focus from a user configuring another app.

## Extract useful evidence from the exported trace

After discovering the relevant encoder and command IDs:

```sh
gpudebug -s SESSION -c 'go /' -c 'go performance/encoders/re1' -c 'info --all'
gpudebug -s SESSION -c 'go /' \
  -c 'go performance/encoders/re1/counters/fragment_shader' -c 'list --all' \
  -c 'info fs_helper_invocations_inefficiency'
gpudebug -s SESSION -c 'go /' \
  -c 'go performance/encoders/re1/counters/performance_limiters' -c 'list --all'
gpudebug -s SESSION -c 'go /' -c 'go performance/commands/draw0' -c 'info --all' \
  -c 'go :d' -c 'info'
gpudebug -s SESSION -c 'go /' \
  -c 'go performance/encoders/re1/heatmap/fragment' \
  -c "fetch cost --out $PWD/build/fragment-cost.png"
```

Heatmap fetch also writes a top-100 JSON sidecar. Preserve raw responses,
counter descriptions, and capture provenance, not only a transcribed table.
Use `go /` followed by a relative path if an absolute path resolves oddly.
Do not assume a source-file `info` response contains per-line costs: it may
only print source. The GUI supplied our source-cost view.

## Interpret measurements honestly

- Overlapping stage intervals are not additive. Isolated encoder/command
  counters, sampled shader cost, static compiler counts, and unprofiled
  runtime timings are different measurements.
- A limiter is relative to that unit's peak, not a percentage of frame time.
  Divergence/ALU inefficiency can include partial SIMD groups and uncovered
  pixel quads, not just divergent material branches. Read its description.
- Source costs may be attributed to a function declaration or surprising
  lines after optimization. Do not blame the displayed expression without
  validating its mapping. Turning optimization off changes the workload.
- Record sampled-core coverage. We got 9/10 on Apple's sample, 3/10 on Luft,
  and 2/10 on Moppe. Reasons and repeatability remain unresolved.
- Smaller source, fewer triangles, fewer static instructions, and faster
  replay are hypotheses for runtime gains, not substitutes for interleaved
  unprofiled A/B runs and image/motion checks.

References: installed `gpudebug(1)` and `gpucapture(1)`, Apple's
[shader inspection](https://developer.apple.com/documentation/xcode/inspecting-shaders),
[GPU profiling semantics](https://developer.apple.com/documentation/xcode/optimizing-gpu-performance),
and [counter statistics](https://developer.apple.com/documentation/xcode/analyzing-apple-gpu-performance-using-counter-statistics).
