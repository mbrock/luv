---
name: luv-development
description: Use when starting or diagnosing development in a luv checkout or worktree; running builds, tests, or SBCL scripts; managing Swash-supervised ./sly images or a live game process; checking the Nix development profile or ASDF cache; or investigating slow, stale, crossed, or apparently hung Lisp work.
---

# Luv development

Keep the checkout, environment, compiled artifacts, and selected live Lisp aligned. Treat an unexplained pause as a process to inspect, not a reason to launch unmanaged SBCLs.

## Start here: managed Lisps, one selected game

Swash supervises durable Lisp images across checkouts, and the game normally
runs inside one selected image. This is the ordinary workflow:

```sh
./sly play                              # boot the image and open the real game
./sly status                            # identify the image and game state
./sly screenshot build/frame.png        # capture what the game shows
./sly stop-playing                      # checkpoint and close the game
./sly restart                           # explicit recovery if the image is wrecked
./sly list                              # all managed Lisps across checkouts
./sly systems                           # live ASDF registration and freshness
./sly stale                             # loaded systems with pending ASDF work
```

- Work in small evals against `luvcraft:*session*`; redefine code with
  `./sly eval '(load "luvcraft/foo.lisp")'`.  Shader methods rebuild their
  pipelines live at the next frame; a `defclass` change is fine too — the
  image survives errors, unlike the standalone binary.
- Never pipe `./sly eval` through `tail`/`head`: an error opens the Slynk
  debugger prompt on stdin, which then looks like a hang.
- Learn the tools before improvising: `./sly --help`, `./sly describe`,
  `apropos`, `edit`, `xref`, `inspect`.
- Multiple Lisps are explicit: create one with `./sly start --name NAME` and
  select it with `./sly --lisp ID-or-NAME COMMAND`. An unqualified command
  refuses to guess when several Lisps match the checkout.
- `build/luvcraft` (`make luvcraft`) is the standalone executable for
  shipping and CI (`make smoke`); `./sly --luvcraft ...` attaches to it.
  Do not run it alongside the image while developing — two windows, two
  states, confusion.

## Start in a checkout

1. Run `git status --short --branch` and preserve unrelated work.
2. Confirm `LUV_DEV_ENVIRONMENT=1` and `command -v sbcl` resolves into the Nix profile. Use `./scripts/dev --status` only when the inherited environment is absent or suspect.
3. Run `./sly list`, then `./sly status`. Swash assigns each image a stable session identity and publishes its kernel-assigned Slynk port in the journal.
4. Run one-shot native tools directly and use `./sly COMMAND` for Lisp exploration. `./scripts/dev COMMAND` is the fallback activator, not the ordinary prefix. Never use an ambient Homebrew or system SBCL.

The durable Nix development profile is the ordinary environment. Install or
refresh it only when locked dependencies change:

```sh
./scripts/install-dev-profile
. "$HOME/.nix-profile/share/luv/env.sh"
direnv allow         # once; .envrc only sources the same activation file
```

Normal commands do not evaluate Nix and do not enter a subshell. The profile
is a deliberate GC-rooted dependency checkpoint; `flake.lock` still makes it
reproducible when refreshed. CI may enter the equivalent environment once
around a whole job with `nix develop -c`. Orb login shells activate the full
environment and export `BASH_ENV` so nested non-interactive Bash commands keep
it. Restart each selected image that should enter a refreshed profile.

## Choose the execution path

- Use `./sly play`, then `eval`, `inspect`, `describe`, `apropos`, `edit`, or `xref` for iterative work. Each invocation opens a fresh client connection to the selected image.
- Use `./scripts/luv COMMAND` for named one-shot luvcraft tools such as `gazetteer`; these do not share the live game.
- Use `./sly --luvcraft ...` only to inspect the standalone game named by `build/luvcraft.slynk`; `luvcraft:*session*` is its live session.
- Use `sbcl --non-interactive ...` for isolated verification that must start clean; add `./scripts/dev` only if the shell missed activation.
- Use Make targets for their intended artifacts. Expect the first build in a new absolute checkout path to compile local systems into a distinct ASDF cache subtree; later loads should be much faster.
- Use `./sly parinfer ...` for its connection-free Lisp indentation checks.

Do not replace a broken managed image with an ad hoc sequence of fresh SBCL processes. That hides stale state and leaves the shared development surface broken.

## Manage images

```sh
./sly play
./sly list
./sly status
./sly restart
./sly log
./sly stop-playing
./sly stop
./sly start --name experiment
./sly --lisp experiment status
./sly systems
./sly system luv/test
./sly stale
```

`./sly` asks Swash to create and supervise each image, normally lets the kernel
choose its Slynk port, and discovers the port, PID, project root, and name from
the journaled ready event. Commands select by checkout when exactly one image
matches, or by the explicit `--lisp` name or six-character session ID.

If the image lacks a new system, package, readtable, native dependency, or method:

1. Check `./sly list`, `./sly status`, `./sly systems`, and `./sly log`.
2. Stop the selected image with `./sly stop` when no interactive client needs its state.
3. Refresh and reactivate the development profile if the flake environment
   changed.
4. Run `./sly start`, then confirm the missing capability with a small eval.

Never kill a managed image by PID or disturb one belonging to another checkout.
Select it by Swash identity and stop it through `./sly`; use `./sly list` to make
ownership visible before lifecycle actions.

## Five seconds of silence means broken

**A command that runs longer than about five seconds without printing anything is not slow. It is broken, and finding out how is the task now.** Waiting it out, polling it, chaining sleeps, or running it again the same way are all refusals to look. So is reporting "it seems to be taking a while": that is not a status.

Silence is almost always something that was told not to speak, or something waiting for input nobody is going to send:

- **A pipe ate the progress.** `-L`, `--verbose`, `-x`, and `--progress` can go mute into `tee`, `head`, or a pager. Never pipe a progress-reporting command. Start long work in Swash and inspect its journal with `poll` or `follow`; use tmux only when Swash is unavailable or the work is inherently interactive.
- **Stdin was left open.** A failed `./sly eval` prints a backtrace and waits for a restart number. Redirect `< /dev/null` so it aborts and says why instead of waiting forever for a keystroke.
- **It is talking to the network.** `nix build nixpkgs#foo` resolves the registry alias over `channels.nixos.org` and can hang there with nothing on screen; prefer a reference through this repo's pinned flake.

Make it talk, then find it:

1. `ps ax -o pid,ppid,etime,state,command | grep -E '[s]bcl|[n]ix|[c]c1'` — is anything actually burning CPU, or is it all sleeping?
2. `strace -f -p PID`, `cat /proc/PID/wchan`, `cat /proc/PID/stack` — one of these names the syscall it is parked in within seconds.
3. `ss -tnp` for a socket that will never answer.
4. Split the silent step into several small ones. The one that does not come back has named itself.

A responsive image and a stuck call are different things: if `./sly eval "(+ 1 2)" < /dev/null` returns instantly while another eval does not, the image is fine and the block is inside that specific form — take it apart rather than blaming the connection.

For a detached one-shot build, let Swash select the available backend:

```sh
swash start -- make all
swash poll SESSION
swash follow SESSION
```

`start` prints the session ID, `poll` shows recent output, and `follow` streams
to completion and returns the build's exit status. Bare `swash` lists sessions.
The portable backend writes directly to an SQLite/WAL journal and needs no
journal socket or daemon. Ordinary builds do not need `--tty`; reserve it for
commands that are actually interactive.

## Diagnose slow or stuck work

1. Observe the last emitted compilation unit; a fresh worktree compiles its own source-path cache, but a normal cold project load should keep printing progress.
2. Check `LUV_DEV_ENVIRONMENT`, `command -v sbcl`, `./sly list`, and `./sly status` in another terminal; use `./scripts/dev --status` if activation looks wrong.
3. Inspect processes with `ps ax -o pid,ppid,etime,state,command | grep -E '[s]wash|[s]bcl|[n]ix (develop|build)|[l]uv-env'`.
4. For a managed image, inspect its selected `./sly log` and Swash identity. For a Make/SBCL process, capture its command and parent before interrupting it.
5. Distinguish CPU-heavy compilation from sleeping/waiting. Do not start a duplicate build: concurrent work obscures ownership and can contend for outputs.
6. If a failed `./sly eval` enters the Slynk debugger, choose a printed restart number or `a` to abort; it is waiting for input, not compiling.

A cold checkout-local load and a full program image build are different timings: ASDF compilation writes under `~/.cache/common-lisp/.../<absolute-checkout-path>/`, while `program-op` also writes a large executable core. Report the phase and elapsed time when diagnosing regressions.

## Turn the Vulkan validation layer on

The layer is packaged but not loaded; a run asks for it, and it has to be
asked before the image starts, because the instance is created once:

```sh
env VK_LOADER_LAYERS_ENABLE='*validation*' ./sly start
```

luv installs its own debug messenger whenever `VK_EXT_debug_utils` is
available, so anything the layer says arrives as a `:vulkan` log line with a
Lisp backtrace taken where the offending call was made, and becomes a
`VULKAN-VALIDATION-FAILURE` at the end of the frame -- caught by the canvas
guard, retained, and reported by `./sly` like any other frame failure.
Nothing speaks through the messenger unless a layer is loaded, so an
ordinary run is unaffected.

`WITH-VULKAN-VALIDATION` wraps any other Vulkan work the same way. Bind
`*VULKAN-VALIDATION-ENABLED-P*` to NIL to keep a provider quiet, or
`*VULKAN-VALIDATION-BACKTRACE-P*` to NIL when the backtraces cost more than
they tell.

## Verify what the game shows

`./sly screenshot build/frame.png` renders the playing game's frame once
and writes the PNG; read the PNG.  Move the camera or open overlays with
small evals first.  Named hidden views (no window) live in
`luvcraft/gazetteer.lisp`: `scripts/luv gazetteer build/gazetteer`.
Do not screenshot the desktop.

## Finish

Run only quick checks relevant to the change. Stop temporary checkout-managed images if their state is no longer useful. Commit and push coherent work according to `AGENTS.md`, without deleting caches or unrelated user files merely to make the tree look clean.
