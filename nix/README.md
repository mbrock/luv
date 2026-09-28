# Luv's Nix support files

The flake itself is `flake.nix` at the repository root: one flake provides
the development shells, the released `luvcraft` and `luft` programs, and the
supporting packages.  This directory keeps only the files it builds from:
the cl-sdl3 patch that drops SDL_mixer and the Tracy context shim.

```sh
nix develop                        # the full development shell
nix develop .#slim                 # without Ghostty's Zig closure
nix build .#environment            # the development closure itself
nix run .                          # Luvcraft, as `nix run github:mbrock/luv` builds it
```

From the repository root, `./env COMMAND` is the short explicit wrapper.
Ordinary launchers and `make` enter the environment automatically.  A local
flake reference is the Git checkout, so Nix sees tracked files only: add a new
file to Git before a flake build can use it.

ASDF's compiled files live under the user cache, separated by the Nix
dependency closure as well as the Lisp implementation and source path.
Changing the closure rebuilds Lisp files and CFFI header probes together;
otherwise an unchanged probe could retain struct offsets from an older
FFmpeg while loading the new library. The full and slim shells have separate
caches. The Sly dependency core also records this cache configuration.

The released programs build from `applicationPackages` and
`applicationEnvironment`, not from the development shell: only the Lisp
closure, SDL, Vulkan, FFmpeg, HarfBuzz, and libghostty-vt. Workstation tools
(Go, Zig, Node, Typst, MuPDF, yt-dlp, Mesa, validation layers) stay out of
what `nix run` downloads. Check a change with `nix path-info -rSh .#luvcraft`
after building it; none of those tools, nor Emacs or a C toolchain, should
appear in the list.
