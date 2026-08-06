# luv

`luv` is an experimental atelier for hacking on Vulkan graphics with Common
Lisp. The initial spike uses
[`JolifantoBambla/vk`](https://github.com/JolifantoBambla/vk) to load Vulkan,
and [`aiffc/cl-sdl3`](https://github.com/aiffc/cl-sdl3) to let SDL own the
native Wayland window while luv owns the Vulkan instance and surface queries.

## One-time Lisp setup

`cl-sdl3` is not in Quicklisp, so install it as a local project:

```sh
git clone https://github.com/aiffc/cl-sdl3 \
  ~/quicklisp/local-projects/cl-sdl3
```

Its ordinary Lisp dependencies are fetched by Quicklisp when `luv` is loaded.
Nix supplies SDL3, its companion native libraries, libffi, Vulkan, and SBCL.

## Run the probe

Enter the reproducible development environment:

```sh
nix develop
```

Then start `sbcl`, load the ASDF system, and open the ambient Vulkan window:

```lisp
(load #P"~/quicklisp/setup.lisp")
(asdf:load-asd (truename "luv.asd"))
(ql:quickload :luv)
(luv:open-window)
```

`open-window` returns after presenting yellow while a background context thread
keeps the SDL window, Vulkan instance, device, surface, queue, and swapchain
alive. Rendering is then ordinary REPL interaction:

```lisp
(luv:render-color 1.0 0.0 1.0) ; magenta
(luv:render-color 0.0 1.0 1.0) ; cyan
(luv:close-window)
```

The live handles are exported as specials such as `luv:*window*`,
`luv:*instance*`, `luv:*device*`, and `luv:*swapchain*`. `luv:yellow-window`
remains as an alias for `luv:open-window`. `(luv:surface-probe)` performs the
native-window setup without creating a device or swapchain, and `(luv:probe)`
is the original windowless loader/device check. Set `SDL_VIDEODRIVER=wayland`
to require native Wayland rather than allowing SDL to choose another backend.

Quicklisp supplies ordinary Lisp systems, including `cl-mcp` and the
dependencies of the locally installed `cl-sdl3`. Nix supplies SBCL, the large
generated `vk` binding, native SDL/Vulkan/OpenSSL libraries, and `vulkaninfo`.
A Vulkan implementation/ICD still comes from the host graphics stack. If the
probe cannot see a device, `vulkaninfo --summary` is the first diagnostic to
try.

## Lisp-aware MCP server

[`cl-ai-project/cl-mcp`](https://github.com/cl-ai-project/cl-mcp) is installed
as a Quicklisp local project. The preferred workflow runs its TCP server inside
the same Lisp image as SLY, with the worker pool explicitly disabled.

This repository's `.dir-locals.el` gives SLY a `luv` implementation that starts
SBCL through `nix develop`. This does not Nix-package the Lisp dependencies;
Quicklisp still supplies those. It only ensures OpenSSL, SDL, Vulkan, and the
other native libraries are in the process environment before SBCL starts. Let
Emacs accept the directory-local variables, then run `M-x sly`. If another Lisp
is already connected, use `M-- M-x sly` and choose `luv`.

The SLY command loads `sly-init.lisp`, which sets up Quicklisp, loads
`:luv/mcp`, and starts the MCP listener automatically. No REPL incantation is
needed.

This leaves the SLY REPL usable while a background listener runs on
`127.0.0.1:12345`. Because `:worker-pool nil` is passed explicitly, Codex MCP
evaluations, definitions, loaded systems, and objects inhabit that exact Lisp
process. Check it with `(luv/mcp:status)` and stop only the listener with
`(luv/mcp:stop)`.

The repository's `.codex/config.toml` runs `nix run .#mcp`, which is now a thin
stdio-to-TCP bridge rather than another Lisp process. Its effective
configuration is:

```toml
[mcp_servers.cl-mcp]
command = "nix"
args = ["run", ".#mcp"]
```

Restart Codex or begin a new session after changing MCP configuration. All
repository paths are derived from the current checkout. Quicklisp defaults to
`~/quicklisp`; set `QUICKLISP_HOME` if it lives elsewhere.

Start the listener in SLY before starting Codex. The bridge is intentionally
required, so a missing listener makes the setup failure obvious instead of
silently creating an unrelated Lisp image. Port overrides must agree on both
sides: `(luv/mcp:start :port 23456)` and `LUV_MCP_PORT=23456` for Codex.

For debugging or sessions where no SLY image is available, the old isolated
stdio launcher remains available as `nix run .#mcp-stdio`. It is not used by
the project Codex configuration.
