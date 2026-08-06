# luv

`luv` is an experimental atelier for hacking on Vulkan graphics with Common
Lisp.  The initial spike uses
[`JolifantoBambla/vk`](https://github.com/JolifantoBambla/vk) to load Vulkan,
create an instance, enumerate physical devices, and clean the instance up.

## Run the probe

Enter the reproducible development environment:

```sh
nix develop
```

Then load the ASDF system and run the probe:

```sh
sbcl --non-interactive \
  --eval '(require :asdf)' \
  --load luv.asd \
  --eval '(asdf:load-system :luv)' \
  --eval '(luv:main)'
```

For interactive hacking, start `sbcl`, evaluate the first three forms above,
and call `(luv:probe)` whenever you want to check the Vulkan connection.

Quicklisp supplies ordinary Lisp systems, including `cl-mcp`. Nix supplies
SBCL, the large generated `vk` binding, native Vulkan and OpenSSL libraries,
and `vulkaninfo`. A Vulkan implementation/ICD still comes from the host
graphics stack. If the probe cannot see a device, `vulkaninfo --summary` is
the first diagnostic to try.

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
