# Downloadable Luvcraft builds

The `downloadable game` GitHub Actions workflow builds the current
Luvcraft-on-Luft game on pushes to `main`, on published releases, and on
manual dispatch. Push/manual builds appear in the run's **Artifacts**;
published releases receive the same files as **Release assets**, only after
the portability smoke test succeeds. A release build checks out its tag,
not whatever `main` happens to contain later.

## Linux x86-64: experimental single-file download

Download `luvcraft-linux-x86_64.run` and optionally `SHA256SUMS`. No SBCL,
Quicklisp, Nix installation, or source checkout is required on the player’s
machine. GitHub's Actions artifact ZIP must be extracted first.

```sh
sha256sum --check SHA256SUMS
chmod +x luvcraft-linux-x86_64.run
./luvcraft-linux-x86_64.run
```

The file contains the SBCL image, native libraries, fonts, companion meshes,
Bash, and the other runtime dependencies. It extracts into `~/.cache` and uses a private
Linux user/mount namespace to give those files their original paths. This
is a single-file **distribution**, not a statically linked executable.
First launch needs extraction time and additional disk space. It does not
install a system-wide Nix store or require root.

Requirements: a Vulkan 1.4-capable driver/device with shaderInt64, an X11 or
Wayland desktop, standard shell/archive utilities (`sh`, `tar`, `bzip2`),
and permission to create unprivileged user namespaces. Hardened systems,
containers, and some AppArmor policies disable those namespaces; this
experimental bundle will not run there. Do not weaken system policy just
for the game. The existing Nix installation path is an alternative.
GPU drivers belong to the host and are not included in the download.

Worlds remain under `$XDG_DATA_HOME/luvcraft` or
`~/.local/share/luvcraft`, outside the extracted application. This package
retains the game's local Slynk development endpoint on a random loopback
port. It is not a sandbox for untrusted Lisp or terminal commands.

Build and test locally on Linux:

```sh
nix build .#luvcraft-portable --out-link build/portable
# The test needs host Xvfb, xauth, Mesa lavapipe, and user namespaces.
sh scripts/test-portable-release build/portable build/release-smoke.png
```

The test copies the download into a path containing spaces, gives it a
fresh home, hides the builder's `/nix` and source checkout, removes the
development environment, checks `--help` and failure exit status, and
renders the existing hidden Vulkan smoke scene with host lavapipe. It also
loads every companion mesh/rig and starts the current game twice with a
temporary save, checking its frame loop, capture, and save/reload path.
This is not interactive gameplay testing on every GPU or distribution.
Hardware-driver testing remains necessary.

## macOS: app packaging in progress

ASDF's `program-op` already creates the native SBCL executables; it does
not bundle their dynamic libraries or relocate their resource paths.
Uploading `build/luvcraft` directly would therefore be misleading.

For **macOS**, the next package should be an Apple Silicon `Luvcraft.app`
distributed as a ZIP or DMG. A `.app` is a directory bundle presented as
one application by Finder. Use [Deploy](https://shinmera.com/docs/deploy/)
to close/reopen CFFI libraries at relocated paths, include the fonts and
generated Objective-C helpers, recursively relocate non-system dylibs,
and test after moving the app away from Nix and the checkout. The native
Metal 4 backend requires macOS 26 and compatible hardware. Developer ID
signing and notarization need Apple credentials; an unsigned test build
is not equivalent to a frictionless public download. The existing Nix
package supports Apple Silicon, not Intel Macs.

## Windows x86-64: native build qualification

The Windows job builds with native SBCL and MSYS2 UCRT64, using the same
locked Lisp sources as the Nix build. Linux exports sources only; it does
not cross-compile Lisp images or copy Unix ABI layouts into Windows.
FFmpeg is groveled against the native headers and checked against the DLLs
from the same MSYS2 installation.

The intended download is `luvcraft-windows-x86_64.zip`: extract the entire
directory and run `luvcraft.exe`. DLLs, fonts, and companion meshes must
stay beside the EXE. This is not yet a single-file Windows distribution.
The executable is unsigned; there is no Authenticode certificate configured.

The Windows player excludes the Unix PTY backend, Luft terminal overlays,
the Slynk listener, and Tracy/source-update commands. It retains the
SDL/Vulkan game renderer. Saves use `%LOCALAPPDATA%/luvcraft/worlds` unless
`XDG_DATA_HOME` or `--world` overrides that location.

The job must successfully launch a moved copy with a clean environment,
check CLI failure behavior, and render with Mesa's software Vulkan driver
before uploading a download. Both the dependency sources and temporary game
checkout are removed first. The current game's companion assets, frame
loop, and save/reload are checked too. This is build qualification, not a claim of
interactive hardware-driver testing on Windows. Native verification is
still in progress; use the job's result and artifacts, not the presence of
build scripts, to decide whether a particular revision has a usable build.

To reproduce the native build, export the source archive on Linux:

```sh
nix build --impure --expr 'import ./scripts/windows-sources.nix {}' \
  --out-link build/windows-sources
```

Transfer `lisp-sources.tar.gz` to the same checkout revision on Windows.
In an updated MSYS2 UCRT64 shell run
`bash scripts/windows-bootstrap.sh --with-mesa`, then in PowerShell with
native Python 3.12 or newer:

```powershell
python scripts/windows-build.py --sources lisp-sources.tar.gz --software-smoke-test
```

References: [ASDF program-op](https://asdf.common-lisp.dev/asdf.html#Building-programs),
[SBCL executable delivery](https://www.sbcl.org/manual/index.html#Saving-a-Core-Image),
[nix-bundle](https://github.com/nix-community/nix-bundle), and
[Kandria's deployment experience](https://raw.githubusercontent.com/Shinmera/talks/master/els2023-kandria/paper.pdf).
