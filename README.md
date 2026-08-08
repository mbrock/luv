# luv

`luv` is an experimental atelier for hacking on Vulkan graphics with Common
Lisp. The initial spike uses
[`JolifantoBambla/vk`](https://github.com/JolifantoBambla/vk) to load Vulkan,
and [`aiffc/cl-sdl3`](https://github.com/aiffc/cl-sdl3) to let SDL own the
native Wayland window while luv owns the Vulkan instance and surface queries.

The vendored `vk` binding and generator are an in-tree hard fork rather than
an opaque compatibility boundary. They have already been updated to Vulkan
1.4, and luv can change their generated API when a better Common Lisp shape
emerges.

## Second spike GPU API

The independently loadable `:luv/gpu` system is the beginning of a
WebGPU-shaped API implemented by the Vulkan backend. Its first vertical slice
owns a Vulkan instance, logical device, default graphics queue, command pool,
and primary command buffer:

```lisp
(asdf:load-system :luv/gpu)
(let ((device (luv:request-gpu-device luv:*gpu-provider*))
      (encoder nil)
      (commands nil))
  (unwind-protect
       (progn
         (setf encoder
               (luv:create device (luv:make-command-encoder-descriptor))
               commands (luv:finish encoder))
         (luv:submit (luv:device-queue device) (vector commands)))
    (when commands (luv:destroy commands))
    (when encoder (luv:destroy encoder))
    (luv:destroy device)))
```

Submission currently waits for the Vulkan queue to become idle. This keeps
the initial ownership rules honest; presentation work can replace the wait
with tracked in-flight frames without changing the public operation.

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

On a headless Linux server, the shell points the Vulkan loader at Mesa's
Lavapipe software ICD. That is enough to load the Vulkan-only system and create
a real `VK_EXT_headless_surface` swapchain without SDL or a display server:

```lisp
(asdf:clear-system :vk)
(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(namestring (truename "vendor/vk/")))
   :inherit-configuration))
(asdf:load-asd (truename "vendor/vk/vk.asd"))
(asdf:load-asd (truename "luv.asd"))
(asdf:load-system :luv/headless)
(luv:probe)
(luv:headless-probe)
(luv:open-headless :width 320 :height 240)
(luv:render-color 0.2 0.4 1.0)
(luv:capture-color #P"capture.ppm" 0.2 0.4 1.0)
(luv:close-window)
```

`capture-color` renders into the current headless swapchain image, copies that
surface image back to host memory, and writes a binary PPM. Convert it with
`pnmtopng capture.ppm > capture.png` if your viewer does not open PPM files.

The ordinary `:luv` system still owns the SDL-backed native window path:

Then start `sbcl --dynamic-space-size 6144`, load the ASDF system, and open the
ambient Vulkan window:

```lisp
(load #P"~/quicklisp/setup.lisp")
(ql:quickload '(:sdl3 :rove))
(asdf:clear-system :vk)
(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,(namestring (truename "vendor/vk/")))
   :inherit-configuration))
(asdf:load-asd (truename "vendor/vk/vk.asd"))
(asdf:load-asd (truename "luv.asd"))
(asdf:load-system :luv)
(luv:open-window)
```

`open-window` returns after presenting yellow while a background context thread
keeps the SDL window, Vulkan instance, device, surface, queue, and swapchain
alive. Rendering is then ordinary REPL interaction:

```lisp
(luv:render-color 1.0 0.0 1.0) ; magenta
(luv:render-color 0.0 1.0 1.0) ; cyan
(luv:close-window)
```

The live handles are exported as specials such as `luv:*window*`,
`luv:*instance*`, `luv:*device*`, and `luv:*swapchain*`. `luv:yellow-window`
remains as an alias for `luv:open-window`. `(luv:surface-probe)` performs the
native-window setup without creating a device or swapchain, and `(luv:probe)`
is the original windowless loader/device check. Set `SDL_VIDEODRIVER=wayland`
to require native Wayland rather than allowing SDL to choose another backend.

Quicklisp supplies ordinary Lisp systems, including the dependencies of the
locally installed `cl-sdl3`. The Vulkan 1.4.358 `vk` binding is generated and
vendored under `vendor/vk`; Nix supplies SBCL, native SDL/Vulkan libraries,
and `vulkaninfo`.
A Vulkan implementation/ICD still comes from the host graphics stack. On
Apple Silicon, the development shell supplies MoltenVK and points the Vulkan
loader at it; the same code also accepts KosmicKrisp when its SDK driver is
selected. If the probe cannot see a device, `vulkaninfo --summary` is the first
diagnostic to try.

On macOS, SDL and the Vulkan context run on the process main thread as Cocoa
requires. `open-window` still returns to the SLY evaluation thread, so
`render-color` and `close-window` retain the same live REPL interface as on
Linux.

## SLY and the one-shot client

The server-friendly workflow does not need Emacs. Inside `nix develop`,
`./sly` can start a durable Slynk image by itself, loaded with `:luv/headless`:

```sh
./sly start
./sly eval '(probe)' --package LUV
./sly eval '(headless-probe)' --package LUV
./sly eval '(open-headless :width 160 :height 100)' --package LUV
./sly eval '(capture-color #P"capture.ppm" 0.2 0.4 1.0)' --package LUV
./sly eval '(close-window)' --package LUV
./sly stop
```

Normal Slynk-backed commands auto-start the server if it is not already
listening. `./sly status` reports the pid/socket state, and `./sly log` prints
the server log tail. The connection-free `./sly parinfer` command still works
without starting anything.

This repository's `.dir-locals.el` gives SLY a `luv` implementation that starts
SBCL through `nix develop`. This does not Nix-package the Lisp dependencies;
Quicklisp still supplies those. It only ensures SDL, Vulkan, and the other
native libraries are in the process environment before SBCL starts. Let Emacs
accept the directory-local variables, then run `M-x sly`. If another Lisp is
already connected, use `M-- M-x sly` and choose `luv`.

The Emacs SLY command loads `sly-init.lisp`, which loads `:luv` and starts a
durable Slynk listener on `127.0.0.1:4005`. The executable `./sly` is a small
Common Lisp Slynk client:

```sh
./sly eval '(+ 1 1)'
./sly eval '(render-color 1.0 0.0 1.0)' --package LUV
./sly eval '(list *window* *device* *swapchain*)' --package LUV
./sly inspect '(list *window* *device* *swapchain*)' --package LUV
```

The project-local SLY launcher establishes SBCL's `(debug 3)` compiler policy
before loading the project, and `luv.asd` applies the same policy specifically
while compiling its source files even outside that launcher. Besides richer
locations, locals, stepping, and backtraces, this raises SBCL's derived
`STORE-SOURCE-FORM` policy to `3`, which embeds function source forms in
compiled FASLs. Consequently tools can recover bodies with
`function-lambda-expression` instead of seeing only an opaque compiled
function. This intentionally trades some compile time, code size, and
optimization freedom for a more inspectable hacking image.

Each invocation opens a new TCP connection, sends one `:emacs-rex`, waits for
its `:return`, and disconnects. There is no long-lived client or bridge to go
stale. If evaluation enters the debugger, the client prints the available
restarts and the initial Slynk backtrace, then waits for a restart number on
stdin; `a`, `abort`, `q`, or EOF aborts the evaluation. Interactive restarts
can ask follow-up questions on stdin too. The client honors `~/.sly-secret`
when present. Override the endpoint with `LUV_SLYNK_HOST` and
`LUV_SLYNK_PORT`; the latter must match the value in the SLY process
environment.

`sly inspect` keeps its connection open and uses Slynk's real object inspector,
so numbered values can be expanded without serializing or re-evaluating them.
Enter a value number to inspect it, `l`/`n` to move through inspector history,
`g` to refresh, `v` for verbose printing, `aNUMBER` to invoke an inspector
action, `>` to fetch all remaining parts, or `e FORM` to evaluate with `*`
bound to the current object. Enter `?` for the command summary and `q` to close
the inspector and release its connection-scoped state.

Live-image discovery uses the same package convention:

```sh
./sly describe RENDER-COLOR OPEN-WINDOW --package LUV
./sly describe RENDER-COLOR --function --package LUV
./sly describe-package luv
./sly describe-system luv
./sly apropos COLOR WINDOW --package LUV
./sly apropos RUN-WINDOW-CONTEXT --package LUV --all
./sly edit luv:render-color
./sly edit RENDER-COLOR --package LUV # equivalent
./sly edit luv:render-color luv:open-window
./sly xref calls RENDER-COLOR --package LUV
./sly xref uses '*WINDOW*' --package LUV
```

`describe`, `apropos`, `edit`, and `xref` accept multiple names or patterns in
one invocation. Symbol names may be package-qualified directly, so
`sly edit luv:render-color` and `sly edit RENDER-COLOR --package LUV` are
equivalent. `describe-package` reports the live package's nicknames, use-list,
used-by list, shadowing symbols, and internal-symbol count. Every export gets
one line with its definition kinds (function, macro, variable, type, and so
on), arglist when available, and the first documentation line. Unbound exports
are retained as plain symbols. `describe-system` accepts only ASDF systems
already loaded in the image and reports the live system object, including its
source file, version, metadata, dependencies, components, and operation state.
Both commands accept multiple names. `apropos` searches external symbols by
default, like SLY's usual Apropos command; add `--all` to include internal
symbols or `--case-sensitive` to narrow the match. `edit` is the terminal
equivalent of SLY's `M-.` definition lookup. Xref types are `calls`, `calls-who`,
`references`, `binds`, `sets`, `macroexpands`, `specializes`, `callers`, and
`callees`. The aggregate `uses` query runs all of SLY's ordinary use-site
queries and groups the nonempty results. File-backed definitions and xrefs are
printed as `file:line:column` with their first source-snippet line.

The client also includes a connection-free, minimal Parinfer-like repair
filter adapted from cl-mcp. Give it one source argument, or pipe multiline
source through stdin:

```sh
./sly parinfer $'(defun twice (x)\n  (* x 2)'
./sly parinfer < unfinished.lisp
```

It closes open forms on dedent or at end of input and drops unmatched closing
parentheses while ignoring strings, line comments, and character literals.
This is a deliberately small, lossy heuristic rather than a complete Common
Lisp parser, so inspect its output before replacing a source file with it.
