# luv

`luv` is an experimental Common Lisp GPU workshop: a WebGPU-shaped API,
a hand-owned CFFI Vulkan layer, an SDL canvas host, a tiny SPIR-V assembler,
and a few live demos that keep the whole thing honest.

It is not trying to be a packaged engine yet. It is a place to make GPU,
windowing, shader, canvas, and eventually game-shaped ideas tangible from a
live Lisp image.

## What's here

The ASDF systems are the useful map:

```text
:luv/gpu/api          portable GPU classes, descriptors, commands, generics
:luv/vulkan/fundament Vulkan loader, invocation bridge, binding macros, tracing
:luv/vulkan/defs      hand-owned Vulkan enums, structs, and raw entry points
:luv/vulkan           Lisp-shaped helpers over the raw Vulkan vocabulary
:luv/gpu/vulkan       Vulkan implementation of the GPU API
:luv/gpu              GPU API plus the default Vulkan backend
:luv/canvas/api       native canvas, events, frame clocks, context protocol
:luv/canvas/sdl       SDL window host and event translation
:luv/canvas/vulkan    Vulkan swapchain presentation for SDL canvases
:luv/canvas           SDL canvas presentation for the GPU API
:luv/examples         demos, PNG capture, and the block world
:luv/mcclim           experimental McCLIM backend on luv canvases
:luv/tools            one-shot command-line tools
```

The public package is still mostly `LUV`, with `LVK` for the lower Vulkan
helpers and `SPV` for the SPIR-V pieces.

## Quick Start

The flake pins nixpkgs and provides the project's SBCL 2.6.7, including
arm64/NEON `sb-simd` support, alongside SDL3, Vulkan tools, MoltenVK on macOS,
Mesa/lavapipe for offscreen Linux captures, and the pinned local Lisp projects:

```sh
nix develop
```

From there, load the project in Lisp:

```lisp
(asdf:load-asd (truename "luv.asd"))
(asdf:load-system :luv)
```

The project launchers enter that environment themselves, so a one-shot command
does not depend on whichever `sbcl` happens to be installed by Homebrew or the
host system:

```sh
scripts/luv eval '(luv:make-little-block-world)'
scripts/luv block-world /tmp/luv-block-world.png
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

In Emacs, this checkout's directory locals define a `luv` SLY implementation
that enters `nix develop`, loads `sly-init.lisp`, and connects to the same kind
of durable image.

## Demos

```lisp
(defparameter *demo* (luv:start-clear-color-demo))
(luv:stop-clear-color-demo *demo*)

(defparameter *compute* (luv:start-compute-gradient-demo))
(luv:stop-compute-gradient-demo *compute*)

(defparameter *world* (luv:start-cube-world-demo))
(luv:capture-cube-world-screenshot *world* #P"/tmp/luv-block-world.png")
(luv:stop-cube-world-demo *world*)
```

The hidden screenshot path is useful in CI-ish or server-ish environments:

```sh
scripts/luv block-world /tmp/luv-block-world.png
scripts/luv block-world /tmp/luv-block-world-frames/ --count 6
```

## Notes

The workshop wiki starts at [`wiki/index.org`](wiki/index.org). It is the right
place for longer explanations of WebGPU-shaped semantics, Vulkan lifetime
decisions, frame slots, the block world, source studies, and other evolving
design notes.

The current implementation is deliberately incomplete. The Vulkan binding grows
when the higher-level experiments need a new capability, and the design should
stay easy to change while the shape is still being discovered.
