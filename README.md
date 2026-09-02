# luv

![A snowy forest in Luvcraft](screenshots/shadow-forest.png)

Luv is an experimental Common Lisp GPU workshop: a small WebGPU-shaped
hardware layer with hand-built Vulkan and native Metal 4 backends, an SDL3
canvas host, Lisp-defined mathematical shaders, and a McCLIM backend for live
tools on GPU surfaces. The aim is not a general-purpose engine; it is graphics
machinery small enough to inspect and redefine while it is running.

The repository contains two game experiments. **Luvcraft** is the original
procedural block world that grew the renderer, simulation, persistence, and
in-world tools. **Luft** is the current second-generation experiment: canonical
cubical topology, packed integer manifold-sheet meshes, and a playable McCLIM
atelier for developing the world from inside it.

## Try it with Nix

Run either game directly from GitHub, without cloning the repository:

```sh
nix run --accept-flake-config github:mbrock/luv          # Luvcraft
nix run --accept-flake-config github:mbrock/luv#luft     # Luft
```

The flake supports x86-64 and ARM64 Linux and Apple Silicon macOS. The first
run downloads or builds the game and its dependencies; later runs reuse the
Nix store. Luvcraft keeps its world under `$XDG_DATA_HOME/luvcraft` (or
`~/.local/share/luvcraft`) rather than inside the immutable package.

## Run the current experiment

Development happens in a durable SBCL image supervised by
[Swash](https://github.com/lessrest/swash):

```sh
./sly play luft                     # start the image and open the Luft atelier
./sly status                        # identify the image, target, and canvas health
./sly screenshot build/frame.png    # capture the next rendered frame
./sly stop-playing                  # close the window; keep the Lisp
```

`./sly` and `make` enter the checkout's Nix environment automatically. For an
arbitrary command, use `./env COMMAND`; `./env --slim COMMAND` provides just
SBCL, Sly/Swash, Python, and Bash. The slim managed image loads the `luft`
mesher rather than the graphical workbench; use the full environment for
rendering and game features.

`./sly play` without a target opens Luvcraft. The same command also provides
`eval`, `inspect`, `describe`, `apropos`, `edit`, and `xref` against the live
image. See [AGENTS.md](AGENTS.md) for the concise development workflow and
concrete output examples.

Standalone executables are available for release-style testing:

```sh
make
./build/luft-atelier                # current Luft experiment
./build/luvcraft                    # original block world
```

Metal 4 is used natively on macOS; Vulkan is used elsewhere and remains
available on macOS for comparison.

## Explore

- [The workshop wiki](https://mbrock.github.io/luv/) is the design notebook and
  rendered source browser.
- [Luft](wiki/luft.org), [Luft sites](wiki/luft-sites.org), and
  [Luvcraft](wiki/block-world.org) introduce the game experiments.
- [GPU architecture](wiki/gpu-architecture.org),
  [mathematical shaders](wiki/mathematical-shaders.org), and
  [quantities and measurement](wiki/quantities-and-measurement.org) describe
  the machinery beneath them.

Everything here is experimental and under active construction.
