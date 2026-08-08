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

# Live Lisp interaction

Prefer `./sly` over `emacsclient` or a second SBCL when exploring, testing, or
changing the running project. It talks directly to the durable SLY image, where
the window and other live Lisp state already exist, but opens a fresh Slynk
connection for each invocation so there is no persistent client to go stale.
Start the `luv` SLY implementation in Emacs first if its listener is not up.

Useful starting points:

```sh
./sly eval '(defparameter *canvas* (open-canvas (make-sdl-canvas)))' --package LUV
./sly eval '(defparameter *device* (request-gpu-device *gpu-provider*))' --package LUV
./sly eval '(defparameter *context* (make-canvas-context *canvas* *gpu-provider* (make-canvas-configuration :device *device*)))' --package LUV
./sly eval '(render-canvas-color *context* 1.0 0.0 1.0)' --package LUV
./sly inspect '*context*' --package LUV
./sly describe render-canvas-color --package LUV
./sly describe-package luv
./sly describe-system luv
./sly apropos color --package LUV
./sly edit luv:render-canvas-color
./sly xref uses render-canvas-color --package LUV
```

`describe`, `apropos`, and `edit` accept multiple names. `apropos` shows only
external symbols by default; pass `--all` for internals. Failed evaluations
print a backtrace and wait on stdin for a restart number or `a` to abort.
`inspect` is interactive and keeps its one connection open while navigating
the object (`?` lists its commands, `q` quits).

The connection-free `./sly parinfer [CODE]` filter repairs common parenthesis
mistakes using indentation. With no argument it reads multiline source from
stdin, for example `./sly parinfer < unfinished.lisp`. It is intentionally a
lossy heuristic, so inspect its output before replacing a file.
