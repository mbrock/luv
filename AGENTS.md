# Luv in one page

Luv is a Common Lisp GPU workshop, not a conventional engine. It owns a small
WebGPU-shaped HAL (hand-built Vulkan and native Metal 4), SDL3 canvas hosting,
Lisp-defined mathematical shaders, and a McCLIM GPU backend. **Luvcraft** is the
first-generation procedural block-world game. **Luft** is the current
second-generation game experiment: canonical cubical topology, packed integer
manifold-sheet meshing, and a playable McCLIM atelier.

The unusual fact that governs development is that the application lives in a
durable SBCL image. Swash supervises images across checkouts; `./sly` finds the
one for this checkout and opens a fresh Slynk connection for each command. Keep
the image and its window alive while inspecting and redefining the program.

## The normal loop

```sh
./sly play luft                         # current experiment
./sly play                              # Luvcraft (the default target)
./sly status                            # selected image, target, canvas health
./sly screenshot build/frame.png       # GPU capture, not a desktop screenshot
./sly load luft/render                  # load safely while frames are held
./sly stop-playing                      # close the target, keep the image
```

`status` is meant to answer whether the application is genuinely alive. Its
output has this shape (IDs and counters vary):

```text
Selected 2f6a91 (luv) for this checkout.
LUFT is playing: ./sly screenshot PNG; ./sly stop-playing closes it.
The canvas loop is healthy (waiting, 18423 iterations, 18210 frames).
```

A frame error parks rendering but keeps the window event loop responsive.
`./sly failures` prints every retained condition and backtrace; fix the cause,
then `./sly resume`. `./sly load SYSTEM...` holds frames during ASDF loading
and fences the next frame, so prefer it to an uncoordinated load while playing.

The live roots are `luvcraft:*session*` and `luft.render:*viewer*`:

```sh
./sly inspect 'luft.render:*viewer*'
./sly eval '(type-of luft.render:*viewer*)'
./sly describe start-viewer --package LUFT.RENDER
./sly apropos mesh --package LUFT.RENDER
./sly edit mesh-chunk --package LUFT
./sly xref callers mesh-chunk --package LUFT
```

`inspect` is interactive (`?` for commands, `q` to quit). `describe`,
`apropos`, and `edit` accept several names. An evaluation error prints a
backtrace and waits for a restart number or `a` to abort, so do not hide or
pipe away its stdin.

## Images and selection

```sh
./sly list                              # running Lisps in every checkout
./sly start --name experiment
./sly --lisp experiment status
./sly restart                           # replace this checkout's selected image
./sly log                               # recent Swash output
./sly stop
```

`list` makes ownership explicit:

```text
ID      NAME             STATE    PID     PORT   STARTED              ACTIVE               ROOT
2f6a91  luv              ready    81234   41927  2026-08-26 09:41:12  2026-08-26 10:03:54  /home/mbrock/luv/
```

Without `--lisp`, commands select the sole running image rooted at this
checkout. If several match, `./sly` refuses to guess; select a name or the
six-character Swash ID. `restart` is the right recovery when the image predates
a changed `.asd`, flake environment, package, readtable, or native dependency.
Do not route around a stale managed image with an unmanaged SBCL.

Only one game target should own the image's window. Stop Luvcraft before
starting Luft and vice versa. Lower-level `open-canvas` forms intentionally
create another ownership lifecycle; use them only for HAL-level work.

## Know what ASDF would do

```sh
./sly systems                           # project systems: current/dirty/unloaded
./sly system luft/render                # dependencies, timestamps, pending actions
./sly stale                             # loaded systems with pending load actions
```

These are read-only reports from the selected image. A typical table is:

```text
STATE      PLAN   LOADED  SYSTEM                        DEFINITION
current    0      yes     luft/render                   luft.asd
dirty      4      yes     luv                           luv.asd
unloaded   -      no      luv/test                      luv.asd
```

`CURRENT` means a fresh ASDF `load-op` plan is empty; `DIRTY` reports how many
actions ASDF would take; `UNLOADED` means the image has no successful load to
assess. No command above performs the plan.

## Which entry point to use

- `./sly`: persistent interactive development; use this by default.
- Native `make`, `sbcl`, and `swash`: one-shot work in the activated profile.
- `./scripts/luv COMMAND`: named one-shot Luvcraft tools such as `gazetteer`.
- `build/luft-atelier` and `build/luvcraft`: standalone release/CI programs.
  `./sly --luvcraft ...` deliberately attaches to the latter; do not run it
  beside a managed game window.

`./sly` itself is a cached `sly-client` ASDF program at `build/sly-client`, not
a fresh `sbcl --script`; the launcher rebuilds it when its sources or profile
SBCL change. `make sly-client` requests that build explicitly.

The development environment is a durable Nix profile. Install or refresh it
with `./scripts/install-dev-profile`; login shells and `.envrc` activate it.
Run `./scripts/dev --status` if activation looks wrong, and use
`./scripts/dev COMMAND` only as the fallback activator. `nix develop -c` is the
from-scratch/CI path, not the normal loop. Restart managed images after a
profile refresh.

## Checks and project memory

Run quick checks proportionate to the change. `make test` is the main suite;
`./sly parinfer --check --file FILE` is the low-noise Lisp balance check, and
`--strict` also detects a balanced tree that disagrees with indentation.
`--diff` shows the candidate and `--write` repairs only validated unbalanced
input.

The Org files in `wiki/` are the design notebook and render to
<https://mbrock.github.io/luv/>. Use `scripts/wiki` to find figures and
mentions before editing them; source comments use stable wiki figure IDs.

This is an experimental project: optimize for short iterations and useful
save points. After a coherent change, run the quick relevant checks, commit,
and push `origin main` unless the work is explicitly on another branch.
Preserve unrelated changes; a pushed commit is a save point, not a release.

`./sly --help` is the complete command map.
