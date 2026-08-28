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
nix build path:./nix#dev
```

The usual setup remains `./scripts/install-dev-profile`, which builds the same
`dev` output and installs it as a durable user-profile generation.
