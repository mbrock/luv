---
name: luv-development
description: Use when starting or diagnosing development in a luv checkout or worktree; running builds, tests, or SBCL scripts; managing the durable ./sly image or a live luvcraft process; checking the Nix/profile environment or ASDF cache; or investigating slow, stale, crossed, or apparently hung Lisp work.
---

# Luv development

Keep the checkout, environment, compiled artifacts, and live Lisp image aligned. Treat an unexplained pause as a process to inspect, not a reason to launch more SBCLs.

## Start in a checkout

1. Run `git status --short --branch` and preserve unrelated work.
2. Run `./scripts/dev --status`. Note the checkout-specific Slynk port, whether `luv-env` or `nix develop` supplies the environment, the SBCL version, and the ASDF cache root.
3. Run `./sly status`. A linked worktree has its own stable port and image; the primary checkout keeps port 4005.
4. Prefer `./scripts/dev COMMAND` for one-shot native tools and `./sly COMMAND` for Lisp exploration. Do not invoke an ambient Homebrew or system SBCL.

A profile install avoids reevaluating the flake on every command:

```sh
nix profile install .#dev
```

After `flake.nix`, `flake.lock`, an `.asd`, or Lisp dependency changes, remember that both the installed `luv-env` profile and an already-running image may describe the old environment. Reinstall the profile when needed, then restart the checkout's image.

## Choose the execution path

- Use `./sly eval`, `inspect`, `describe`, `apropos`, `edit`, or `xref` for iterative work. Each invocation opens a fresh client connection to the checkout's durable image.
- Use `./sly --luvcraft ...` only to inspect the standalone game named by `build/luvcraft.slynk`; `luvcraft:*session*` is its live session.
- Use `./scripts/dev sbcl --non-interactive ...` for isolated verification that must start clean.
- Use Make targets for their intended artifacts. Expect the first build in a new absolute checkout path to compile local systems into a distinct ASDF cache subtree; later loads should be much faster.
- Use `./sly parinfer ...` for its connection-free Lisp indentation checks.

Do not replace a broken durable image with an ad hoc sequence of fresh SBCL processes. That hides stale state and leaves the shared development surface broken.

## Manage the image

```sh
./sly status
./sly start
./sly log
./sly stop
```

`./sly` derives a stable port from the linked-worktree path, serializes concurrent startup, and verifies the live image's project root before evaluating. Override with `LUV_SLYNK_PORT=PORT` only to resolve the unlikely case of two derived ports colliding.

If the image lacks a new system, package, readtable, native dependency, or method:

1. Check `./sly status` and `./sly log`.
2. Stop it with `./sly stop` when it is managed by this checkout and no interactive client needs its state.
3. Update/reinstall `luv-env` if the flake environment changed.
4. Run `./sly start`, then confirm the missing capability with a small eval.

Never kill an image reported as belonging to another checkout or as Emacs/external. Stop it through its owner.

## Diagnose slow or stuck work

1. Observe the last emitted compilation unit; a fresh worktree compiles its own source-path cache, but a normal cold project load should keep printing progress.
2. Run `./scripts/dev --status` and `./sly status` in another terminal.
3. Inspect processes with `ps ax -o pid,ppid,etime,state,command | grep -E '[s]bcl|[n]ix (develop|build)|[l]uv-env'`.
4. For a managed image, inspect `./sly log`. For a Make/SBCL process, capture its command and parent before interrupting it.
5. Distinguish CPU-heavy compilation from sleeping/waiting. Do not start a duplicate build: concurrent work obscures ownership and can contend for outputs.
6. If a failed `./sly eval` enters the Slynk debugger, choose a printed restart number or `a` to abort; it is waiting for input, not compiling.

A cold checkout-local load and a full program image build are different timings: ASDF compilation writes under `~/.cache/common-lisp/.../<absolute-checkout-path>/`, while `program-op` also writes a large executable core. Report the phase and elapsed time when diagnosing regressions.

## Verify native windows

Computer Use currently fails to enumerate or attach to some visible windows
owned by unbundled command-line SBCL processes, including SDL/Cocoa windows.
Do not treat its missing app/window state as evidence that such a window is
absent, and do not spend time trying alternate Computer Use app names.

Use macOS screenshots instead. First capture the displays, inspect the actual
pixels to find the window and its screen coordinates, then make a bounded
capture when useful:

```sh
/usr/sbin/screencapture -x build/native-window-desktop.png
/usr/sbin/screencapture -x -R<X>,<Y>,<WIDTH>,<HEIGHT> build/native-window.png
```

`-R` uses logical screen coordinates, while the resulting image may have twice
those dimensions on a Retina display. Verify the title, contents, and visible
frame from the screenshot itself; a Dock icon, live process, Cocoa event-loop
stack, or loaded graphics driver is not visible-window proof. Keep using
Computer Use for ordinary bundled apps where it can actually observe the
target, but prefer `screencapture` for these Lisp-hosted native windows until
the attachment failure is understood.

## Finish

Run only quick checks relevant to the change. Stop temporary checkout-managed images if their state is no longer useful. Commit and push coherent work according to `AGENTS.md`, without deleting caches or unrelated user files merely to make the tree look clean.
