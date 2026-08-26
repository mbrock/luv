# Working in Luv

Luv is a Common Lisp GPU workshop: a small WebGPU-shaped HAL with hand-built
Vulkan and native Metal 4 backends, an SDL3 canvas host, Lisp-defined
mathematical shaders, and a McCLIM GPU backend. It is inspectable machinery,
not a general-purpose engine.

Two game experiments exercise it. **Luvcraft** is the original procedural
block world, with streaming, simulation, persistence, and tools embedded in
the world. **Luft** is the current second-generation experiment: canonical
cubical topology, packed integer manifold-sheet meshes, and a playable McCLIM
atelier.

## Develop in the live image

Ordinary work happens in a durable SBCL image supervised by Swash. `./sly`
selects the image for this checkout and opens a fresh Slynk connection for
each command. Keep the application alive while inspecting and redefining its
code, classes, shaders, and tools.

```sh
./sly start                             # boot without opening a game
./sly play luft                         # current experiment
./sly play                              # Luvcraft instead
./sly status                            # image, game, and canvas health
./sly screenshot build/frame.png       # capture the GPU frame
./sly stop-playing                      # close the window, keep the Lisp
```

A cold start narrates compilation and ends with a useful summary. One real
start in this checkout said:

```text
;; Built (:luv-workbench) in 21s.
;; 214 files compiled, 250 loaded, 41 systems.
Lisp MKK067 (luv) is ready on 127.0.0.1:35435 (pid 936991).
```

Opening Luft returns the live viewer. The following commands then reported:

```text
$ ./sly status
Selected MKK067 (luv) for this checkout.
LUFT is playing: ./sly screenshot PNG; ./sly stop-playing closes it.
The canvas loop is healthy (waiting, 737 iterations, 10 frames).

$ ./sly screenshot build/frame.png
("/home/mbrock/luv/build/frame.png" 950 1188 :BGRA8-UNORM-SRGB)
```

The health comes from the canvas loop, not merely the Lisp connection. A frame
error parks drawing but leaves the window responsive; `./sly failures` shows
the retained conditions and backtraces, and `./sly resume` runs frames again
after the cause is fixed. Screenshots read the game's render target, not the
desktop.

## Explore before reading everything

The live roots are `luft.render:*viewer*` and `luvcraft:*session*`:

```sh
./sly eval '(type-of luft.render:*viewer*)'
./sly inspect 'luft.render:*viewer*'
./sly describe mesh-chunk --package LUFT
./sly apropos bevel --package LUFT
./sly edit mesh-chunk --package LUFT
./sly xref callers mesh-chunk --package LUFT
```

`describe` prints the real lambda list, derived type, documentation, and source
file. `xref` prints callers with file, line, and a source excerpt; asking for
callers of `mesh-chunk` leads directly into the chunked-meshing tests and
`luft/render/render.lisp`. `inspect` is interactive (`?` for commands, `q` to
leave).

An evaluation error prints a backtrace and Lisp restarts instead of killing
the image. It waits for a restart number or `a` to abort, so keep its stdin
visible rather than piping it through `head` or `tail`.

```sh
./sly load luft/render                  # hold frames during load, then fence
./sly systems                           # current, dirty, and unloaded systems
./sly system luft/render                # dependencies and pending ASDF actions
./sly stale                             # loaded systems with pending actions
```

The ASDF reports are read-only: `CURRENT` means a fresh `load-op` plan is
empty, `DIRTY` gives its action count, and `UNLOADED` means no successful load
is recorded in this image.

Swash also makes multiple images and worktrees explicit. `./sly list` shows
them all; create one with `./sly start --name experiment` and select it with
`./sly --lisp experiment ...`. An unqualified command refuses to guess when
several images match. `./sly log`, `restart`, and `stop` operate on the same
selected image.

## Environment and other tools

The dependencies live in a durable Nix profile. Install or refresh it with
`./scripts/install-dev-profile`; login shells and `.envrc` activate it.
`./scripts/dev --status` explains the active environment, and
`./scripts/dev COMMAND` is the fallback when activation was missed.

Use `./sly` for interactive development. Use `make`, `sbcl`, or
`./scripts/luv COMMAND` for isolated builds, tests, and one-shot tools.
`build/luft-atelier` and `build/luvcraft` are standalone release/CI programs,
separate from the managed image. `./sly` itself is a cached ASDF program in
`build/sly-client`; `./sly --help` is its full command map.

`make test` includes a strict repository-wide Parinfer check. For one file,
use Parinfer as a tree/indentation diagnostic rather than a formatter:

```sh
./sly parinfer --strict --check path/to/file.lisp
./sly parinfer --diff path/to/file.lisp
./sly parinfer --write path/to/file.lisp
```

`--diff` shows the tree implied by indentation. `--write` repairs validated
unbalanced source but refuses to rewrite balanced, merely suspicious code.

## The wiki is part of the source

`wiki/*.org` is the design memory behind the code and the source for
<https://mbrock.github.io/luv/>. Its unit is a small figure with a stable
six-character ID. Figures link to one another, collect backlinks, and can be
cited from comments or docstrings; the site connects those citations back to
the definitions that embody them.

```sh
scripts/wiki toc luft                   # figures and work marks on a page
scripts/wiki marks next todo            # current small bets
scripts/wiki figure UTRJ2T              # prose, subfigures, backlinks, code refs
scripts/wiki mentions UTRJ2T            # every incoming mention
scripts/wiki defs mesh-chunk            # matching Lisp definitions
scripts/wiki dangling                    # unresolved figure references
```

For example, `figure UTRJ2T` returns “The studio: a world of stars under the
same cameras,” its prose, six subfigures, and the figures that mention it. Get
new IDs from `scripts/wiki ids`; keep one idea per figure, and update a nearby
`NEXT`, `TODO`, `WAIT`, `IDEA`, or `DONE` work mark when an experiment changes
the plan. Run `scripts/wiki dangling` before committing wiki work and
`scripts/wiki build` when the rendered result matters.

## Working rhythm

Prefer small experiments, live inspection, and useful save points to ceremony.
Preserve unrelated work. Run the quick relevant checks, then commit and push
`origin main` unless the work is explicitly on another branch. A pushed commit
is a durable iteration, not a claim that the experiment is finished.
