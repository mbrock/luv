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

Then load the ASDF system and open the yellow Vulkan window:

```sh
sbcl --non-interactive \
  --load ~/quicklisp/setup.lisp \
  --eval '(asdf:load-asd (truename "luv.asd"))' \
  --eval '(ql:quickload :luv :silent t)' \
  --eval '(luv:main)'
```

This creates an SDL Vulkan window and swapchain, clears one acquired image to
yellow, presents it, and dispatches SDL events until the window is closed. Set
`SDL_VIDEODRIVER=wayland` to require native Wayland rather than allowing SDL to
choose another available backend.

For interactive hacking, start `sbcl`, load Quicklisp and `luv`, then call
`(luv:yellow-window)`. `(luv:surface-probe)` performs the same native-window
setup without creating a device or swapchain, and the original windowless
loader/device check remains available as `(luv:probe)`. A timed smoke test can
use `(luv:yellow-window :duration 2)`.

Quicklisp supplies ordinary Lisp systems, including `cl-mcp` and the
dependencies of the locally installed `cl-sdl3`. Nix supplies SBCL, the large
generated `vk` binding, native SDL/Vulkan/OpenSSL libraries, and `vulkaninfo`.
A Vulkan implementation/ICD still comes from the host graphics stack. If the
probe cannot see a device, `vulkaninfo --summary` is the first diagnostic to
try.

## Lisp-aware MCP server

[`cl-ai-project/cl-mcp`](https://github.com/cl-ai-project/cl-mcp) is installed
as a Quicklisp local project. The flake only wraps it with the native libraries
needed on NixOS. Start its stdio server from the project root with:

```sh
nix run .#mcp
```

The repository's `.codex/config.toml` configures this server automatically for
Codex when the project is trusted. Its effective configuration is:

```toml
[mcp_servers.cl-mcp]
command = "nix"
args = ["run", "path:/home/mbrock/luv#mcp"]
cwd = "/home/mbrock/luv"
```

Restart Codex or begin a new session after changing MCP configuration. If the
checkout moves, update both absolute paths in `.codex/config.toml`.

The server defaults to isolated worker processes. This is useful while calling
Vulkan through CFFI because a crashed worker can be replaced. Set
`MCP_NO_WORKER_POOL=1` only when you specifically want evaluation to happen in
the server's own Lisp image.
