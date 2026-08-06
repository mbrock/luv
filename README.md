# luv

`luv` is an experimental atelier for hacking on Vulkan graphics with Common
Lisp. The initial spike uses
[`JolifantoBambla/vk`](https://github.com/JolifantoBambla/vk) to load Vulkan,
and [`aiffc/cl-sdl3`](https://github.com/aiffc/cl-sdl3) to let SDL own the
native Wayland window while luv owns the Vulkan instance and surface queries.

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

Then start `sbcl`, load the ASDF system, and open the ambient Vulkan window:

```lisp
(load #P"~/quicklisp/setup.lisp")
(asdf:load-asd (truename "luv.asd"))
(ql:quickload :luv)
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
locally installed `cl-sdl3`. Nix supplies SBCL, the large generated `vk`
binding, native SDL/Vulkan libraries, and `vulkaninfo`.
A Vulkan implementation/ICD still comes from the host graphics stack. If the
probe cannot see a device, `vulkaninfo --summary` is the first diagnostic to
try.

## SLY and the one-shot client

This repository's `.dir-locals.el` gives SLY a `luv` implementation that starts
SBCL through `nix develop`. This does not Nix-package the Lisp dependencies;
Quicklisp still supplies those. It only ensures SDL, Vulkan, and the other
native libraries are in the process environment before SBCL starts. Let Emacs
accept the directory-local variables, then run `M-x sly`. If another Lisp is
already connected, use `M-- M-x sly` and choose `luv`.

The SLY command loads `sly-init.lisp`, which loads `:luv` and starts a durable
Slynk listener on `127.0.0.1:4005`. The executable `./sly` is a small Common
Lisp Slynk client:

```sh
./sly eval '(+ 1 1)'
./sly eval '(render-color 1.0 0.0 1.0)' --package LUV
./sly eval '(list *window* *device* *swapchain*)' --package LUV
./sly inspect '(list *window* *device* *swapchain*)' --package LUV
```

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
equivalent. `apropos` searches external symbols by default, like SLY's usual
Apropos command; add `--all` to include internal symbols or `--case-sensitive`
to narrow the match. `edit` is the terminal equivalent of SLY's `M-.`
definition lookup. Xref types are `calls`, `calls-who`,
`references`, `binds`, `sets`, `macroexpands`, `specializes`, `callers`, and
`callees`. The aggregate `uses` query runs all of SLY's ordinary use-site
queries and groups the nonempty results. File-backed definitions and xrefs are
printed as `file:line:column` with their first source-snippet line.
