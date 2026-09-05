# Luv development environment

This directory is the complete local source of Luv's Nix development
environment.  Keeping its flake boundary here prevents Nix from copying the
application checkout, build products, captures, or linked worktrees merely to
provide development tools.

Use an explicit path flake reference so Nix does not discover the enclosing
Git repository as the source:

```sh
nix develop path:./nix
nix develop path:./nix#slim
nix build path:./nix#environment
```

From the repository root, `./env COMMAND` is the short explicit wrapper.
Ordinary launchers and `make` enter the environment automatically.

ASDF's compiled files live under the user cache, separated by the Nix
dependency closure as well as the Lisp implementation and source path.
Changing the closure rebuilds Lisp files and CFFI header probes together;
otherwise an unchanged probe could retain struct offsets from an older
FFmpeg while loading the new library. The full and slim shells have separate
caches. The Sly dependency core also records this cache configuration.

The repository-root flake is a separate release interface. It has to live at
the root for `nix run github:mbrock/luv` discovery, and packages the complete
source plus its runtime resources. Keep using this directory's explicit path
flake for development so entering a shell does not copy the full checkout to
the Nix store.
