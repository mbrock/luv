# Start here: one Lisp, one game

Everything happens in one durable SBCL image per checkout, and the game
normally runs inside it:

```sh
./sly play                              # boot the image and open the real game
./sly status                            # identify the image and game state
./sly screenshot build/frame.png        # capture what the game is showing
./sly stop-playing                      # checkpoint and close the game
./sly restart                           # explicit recovery if the image is wrecked
```

`play` starts the checkout's durable image when necessary. `./sly eval`,
`inspect`, `describe`, `apropos`, `edit`, and `xref` all talk to that same
process; `luvcraft:*session*` is the live game. `./sly --help` is the command
map. Do not start a second Lisp or run `build/luvcraft` alongside it.

The surfaces have distinct jobs:

- `./sly`: persistent interactive development; use this by default.
- `./scripts/dev COMMAND`: one clean process in the checkout environment;
  use it for tests, builds, and native command-line tools.
- `./scripts/luv COMMAND`: named one-shot luvcraft tools such as `gazetteer`.
- `build/luvcraft`: shipped/CI executable. `./sly --luvcraft ...` is only for
  deliberately attaching to that standalone process.

# Working style

This is a fast-moving experimental project. Optimize for iteration speed,
playful hacking, and frequent save points rather than ceremony.

After finishing a coherent round of work, commit the changes by default and
push them to `origin main` unless we are explicitly on a branch adventure.
Treat `git commit` plus `git push origin main` like **File → Save** for the
project: a pushed commit does not imply that the work is polished, exhaustively
reviewed, or ready for release. Small, imperfect, exploratory commits are
welcome and preferable to leaving work uncommitted or unpushed where it can be
forgotten.

Before committing, do the checks that are quick and relevant to the change,
but do not turn every commit into a heavyweight verification or review cycle.
Use a short commit message that describes the iteration. Preserve unrelated
user changes, and never rewrite or discard existing work just to make a clean
commit.

# Project skills

Reusable design guidance lives in `.agents/skills/` using the open agent
skills format (`SKILL.md` with name/description frontmatter), so any
skill-aware agent can load it; `.claude/skills` is a symlink there for Claude
Code. Start with `luv-development` when entering a checkout, running builds,
or working with Nix, ASDF, SBCL, or Sly. Start with `clos-design` before
designing a new subsystem or refactoring dispatch code. Start with
`luv-systems-design` for performance-sensitive architecture involving dense
iteration, allocation and extent, materialized fields, SIMD, voxel/chunk
work, quantities, or the arithmetic language; use it alongside `clos-design`
when both representation and dispatch are at issue.

# The wiki and scripts/wiki

Design memory lives in the Org wiki (`wiki/*.org`), rendered to
https://mbrock.github.io/luv/ on every push.  `scripts/wiki marks`,
`scripts/wiki toc`, `scripts/wiki figure ID`, and `scripts/wiki mentions ID`
print the corpus from a shell; see the `wiki-work` skill before editing.

# Live Lisp interaction

Prefer `./sly` over `emacsclient` or a second SBCL when exploring, testing, or
changing the running project. It talks directly to the durable SLY image, where
the window and other live Lisp state already exist, but opens a fresh Slynk
connection for each invocation so there is no persistent client to go stale.
Start the `luv` SLY implementation in Emacs first if its listener is not up.

Useful starting points while the game is running:

```sh
./sly inspect 'luvcraft:*session*'
./sly describe play --package LUVCRAFT
./sly apropos terminal --package LUVCRAFT
./sly edit render-luvcraft-frame --package LUVCRAFT
./sly xref uses render-luvcraft-frame --package LUVCRAFT
./sly describe-system luv
```

Use lower-level `open-canvas`, device, and context forms only when the task is
specifically below luvcraft; they create another window and another ownership
lifecycle by design.

`./sly play`, `status`, `restart`, `start`, and `stop` manage the image when it is
the `./sly`-managed one (`sly-server.lisp`, which loads `luv` and `luv-wiki`).
The standalone `./build/luvcraft` (for shipping and `make smoke`) embeds its
own Slynk listener; `./sly --luvcraft ...` attaches to it.  Do not run it
alongside the durable image while developing: one Lisp, one game window.

The image is only as current as the environment it was started in: when it
cannot find a system or component that the flake now provides (`Component
SPINNERET not found`), or otherwise reflects an old world (a package or
readtable that should exist does not, `flake.nix` or an `.asd` has changed
since it started), **fix the image rather than routing around it** -- check
`./sly status` for who owns it and that no client is connected, then
`./sly stop && ./sly start`, and confirm with an eval that the missing thing
is there.  Do not fall back to a fresh `sbcl` for the rest of a session
because the durable image is broken; that leaves it broken for everyone.

`describe`, `apropos`, and `edit` accept multiple names. `apropos` shows only
external symbols by default; pass `--all` for internals. Failed evaluations
print a backtrace and wait on stdin for a restart number or `a` to abort.
`inspect` is interactive and keeps its one connection open while navigating
the object (`?` lists its commands, `q` quits).

The connection-free `./sly parinfer [--check|--diff|--write] [--strict] [--file FILE|CODE|FILE]`
filter repairs common parenthesis mistakes using indentation. Ordinary
`--check` is a low-noise guard for validated balance repairs. Add `--strict`
to also fail when a file is already paren-balanced but indentation suggests a
different tree. The indentation candidate knows a few Lisp layout conventions,
including top-level blank-line fences and common binding-list shapes. `--diff`
shows the indentation candidate in either case; `--write` only applies
validated repairs for unbalanced input and refuses to
rewrite suspicious-but-balanced source. With no argument it reads multiline
source from stdin, for example `./sly parinfer < unfinished.lisp`. It is still
a repair heuristic rather than a full Common Lisp reader.
