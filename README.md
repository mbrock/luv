# luv

`luv` is an experimental Common Lisp GPU workshop: a WebGPU-shaped API,
a hand-owned CFFI Vulkan layer, an SDL canvas host, a tiny SPIR-V assembler,
and a few live demos that keep the whole thing honest.

It is not trying to be a packaged engine yet. It is a place to make GPU,
windowing, shader, canvas, and eventually game-shaped ideas tangible from a
live Lisp image.

## What's here

The systems are the useful map; modules inside them follow the
directory tree instead of pretending that every implementation layer is an
independently loadable product:

```text
:luv                 GPU, shader, native-canvas, Vulkan, and Metal atelier
:luvcraft/world      renderer-independent voxel coordinates and resident data
:luvcraft            the interactive block-world application
:luvcraft/tools      one-shot block-world and gazetteer tools
:luvcraft/program    the standalone game and its live Slynk endpoint
:mcluv/backend       McCLIM presented through luv canvases
:mcluv/shader-lab    McCLIM browser for luvcraft's live shaders
:mcluv               the standalone Listener and shader lab
:luv-wiki            reusable Org reader and site renderer
:luv-wiki-site       this repository's Org corpus and rendered site
```

The five root ASDF files are deliberate ownership boundaries. `luv.asd`,
`luvcraft.asd`, and `mcluv.asd` own the HAL, game, and McCLIM application;
`luv-wiki.asd` owns the reusable wiki machinery, while `luv-wiki-site.asd`
owns the corpus that uses its custom ASDF component. Keeping those primary
definitions at the root lets ASDF discover them normally while their
components live with the code they own.

The implementation follows the same contracts physically under
[`hal/`](hal/).  Shared GPU, canvas, shader, and tracing protocols sit at the
root; [`hal/vulkan/`](hal/vulkan/) and [`hal/metal/`](hal/metal/) each gather
one backend's native vocabulary, shader target, GPU realization, presentation
context, tests, and bring-up probes.  [`hal/sdl/`](hal/sdl/) owns the common
window and event host.  The general Objective-C foreign object system remains
separate in [`objective-c/`](objective-c/); Metal depends on it, but does not
own it.

Backend-neutral quantity semantics and compilation live together under
[`arithmetic/`](arithmetic/): the semantic algebra at the root, the inspectable
source language in [`arithmetic/language/`](arithmetic/language/), and its
ordinary Common Lisp realization in [`arithmetic/lisp/`](arithmetic/lisp/).
Each layer keeps its executable claims beside its implementation.

Luvcraft — the block world — lives in [`luvcraft/`](luvcraft/), from the
renderer-independent world model up through terrain generation, meshing,
player simulation, live shader pipelines, tests, command-line tools, and the
interactive application.  The McCLIM backend and its labs live in
[`mcclim/`](mcclim/).  The Org design corpus and the Lisp implementation that
reads and renders it now form one neighborhood under [`wiki/`](wiki/).

The packages express the same boundaries: `LUV` is the GPU and canvas HAL,
`LUVCRAFT.WORLD` is the renderer-independent voxel model, `LUVCRAFT` is the
game, `LUVCRAFT.SHADERS` owns its shaders, and `MCLUV` owns the McCLIM
application. Lower layers retain focused packages such as `LUV.SPIR-V`,
`LUV.MSL`, and `LUV.OBJECTIVE-C`.

## Quick Start

The flake pins nixpkgs and provides the project's SBCL 2.6.7, including
`sb-simd` on both arm64/NEON and x86-64, alongside SDL3, Vulkan tools,
MoltenVK on macOS, Mesa/lavapipe for offscreen Linux captures, and the pinned
local Lisp projects:

```sh
nix develop
```

For ordinary repeated work, install the same environment into your user Nix
profile once:

```sh
nix profile add .#dev
```

This installs `luv-env`, which carries the pinned Lisp, native libraries, and
project environment without evaluating the flake on every invocation.  The
repository's `./sly`, `scripts/luv`, `scripts/wiki`, and Make targets use it
automatically.  They fall back to `nix develop` when it is not installed.

From there, load the project in Lisp:

```lisp
(asdf:load-asd (truename "luv.asd"))
(asdf:load-system :luv)
```

The project launchers enter that environment themselves, so a one-shot command
does not depend on whichever `sbcl` happens to be installed by Homebrew or the
host system. `./scripts/dev --status` shows the active provider, pinned SBCL,
ASDF cache, checkout identity, and checkout-specific Slynk port:

```sh
scripts/luv eval '(luvcraft:make-little-block-world)'
scripts/luv block-world /tmp/luv-block-world.png
```

For the standalone interactive block world, no Emacs or running Lisp image is
needed:

```sh
make              # builds ./build/luvcraft
./build/luvcraft   # opens the game window
./sly --luvcraft eval '(type-of luvcraft:*session*)' # evaluates in that game
make test          # runs the model and block-world test suites
make smoke         # runs the built program headlessly and writes a PNG
```

The world model can be loaded and tested without SDL or Vulkan:

```lisp
(asdf:load-asd (truename "luvcraft.asd"))
(asdf:load-system :luvcraft/world)
(asdf:test-system :luvcraft)
```

## Live Workflow

This project is meant to be poked through a durable Lisp image. If you are an
agent or you want the local one-shot SLY client details, read
[`AGENTS.md`](AGENTS.md). The short version:

```sh
./sly start
./sly eval '(defparameter *demo* (luv:start-clear-color-demo))' --package LUV
./sly inspect '*demo*' --package LUV
./sly eval '(luv:stop-clear-color-demo *demo*)' --package LUV
./sly stop
```

Every standalone luvcraft process also embeds its own Slynk listener on an
available loopback port. While its window is open, `./sly --luvcraft ...`
attaches to that exact game process; `luvcraft:*session*` is its live session.
Plain `./sly ...` continues to address the separate durable development image.
The primary checkout uses port 4005; each linked worktree derives a stable
independent port and verifies that the listener loaded that same checkout.
Concurrent `./sly start` calls are serialized so they cannot orphan the winner.

In Emacs, this checkout's directory locals define a `luv` SLY implementation
that enters the same profile-aware `scripts/dev` environment, loads
`sly-init.lisp`, and opens the checkout-specific durable listener.

## Demos

```lisp
(defparameter *demo* (luv:start-clear-color-demo))
(luv:stop-clear-color-demo *demo*)

(defparameter *compute* (luv:start-compute-gradient-demo))
(luv:stop-compute-gradient-demo *compute*)

(defparameter *world* (luvcraft:start-luvcraft))
(luvcraft:capture-luvcraft-screenshot *world* #P"/tmp/luv-block-world.png")
(luvcraft:stop-luvcraft *world*)

(asdf:load-asd (truename "mcluv.asd"))
(asdf:load-system :mcluv/shader-lab)
(defparameter *shader-lab* (mcluv:open-shader-lab))
(mcluv:refresh-shader-lab *shader-lab*)
(multiple-value-bind (status report)
    (mcluv:shader-lab-health *shader-lab*)
  (list status
        (mcluv:shader-lab-health-report-mirror-count report)
        (mcluv:shader-lab-health-report-canvas-state report)))
;; => (:responsive 1 :open)
(mcluv:close-shader-lab *shader-lab*)
```

Click the block-world window once to capture the pointer. Walk with WASD and
jump with Space; one-block ledges autojump, and Shift sprints. The outlined
centre crosshair is the edit ray: left click removes, right click places,
middle click picks, and the number keys 1–7 select grass, dirt, stone, wood,
leaves, sand, or snow.
Escape releases the pointer.

Terrain generation and meshing run on one sleeping SBCL worker rather than in
the frame callback. The world/canvas thread remains the only writer of
residency and the only owner of Vulkan objects: it sends immutable dense chunk
or mesh snapshots, validates incarnation/revision tokens on return, then
publishes only a small number of CPU/GPU products per frame. Rapid travel
coalesces work by chunk key instead of accumulating a history-sized queue.
Prebuilt worlds keep caller-owned residency while using the same asynchronous
meshing and render-thread publication path.

The shader lab is also a luvcraft material workbench. Its live atlas cards and
shader-definition tabs are McCLIM presentations; click between block geometry,
block surface, and crosshair methods to recompile their current CLOS definitions,
then select expressions or SSA occurrences to follow the compiler's provenance
in either direction. Refresh and health checks use bounded event-loop
acknowledgements. Health also verifies the frame state, owning process,
registered mirror, native canvas, and event handler; a stuck command loop
reports `:unresponsive` with a best-effort thread backtrace.

The block-world vertex and fragment methods are hot-replaced at their CLOS
role/stage coordinates. Luvcraft notices either MOP revision on its next frame,
builds a coherent vertex-plus-fragment candidate pipeline, and publishes it only
after Vulkan creation succeeds. A broken edit is retained as a diagnostic while
the last good pipeline continues rendering. The current Cocoa host supports one
native canvas, so close luvcraft before opening the standalone shader lab.

The hidden screenshot path is useful in CI-ish or server-ish environments:

```sh
scripts/luv block-world /tmp/luv-block-world.png
scripts/luv block-world /tmp/luv-block-world-frames/ --count 6
```

## Notes

The workshop wiki starts at [`wiki/index.org`](wiki/index.org). It is the right
place for the current GPU architecture and backend proofs, the block world,
source studies, and other evolving design notes. It is also rendered as a
static site at
<https://mbrock.github.io/luv/>; `make wiki` (or `(asdf:make :luv-wiki-site)`)
builds it into `build/wiki/`.

The current implementation is deliberately incomplete. The Vulkan binding grows
when the higher-level experiments need a new capability, and the design should
stay easy to change while the shape is still being discovered.
