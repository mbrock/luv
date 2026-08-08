# luv

`luv` is an experimental Common Lisp GPU atelier.  Its WebGPU-shaped API is
implemented by a deliberately incomplete, hand-owned CFFI Vulkan layer, while
SDL supplies the native Cocoa or Wayland window.  The binding grows only when
the higher-level API needs another Vulkan capability.

The owned layer uses CFFI's translating types, enums, and bitfields directly.
`defvkstruct`, `define-enumerator`, and `define-creator` keep the Vulkan treaty
text explicit while naming its recurring allocation, checking, and count/fetch
patterns once.

## GPU API

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

The next slice owns 2D textures and records explicit clear and copy commands.
Clearing is deliberately a transfer command for now; render-pass clears can
reuse the same texture and layout tracking later:

```lisp
(let* ((device (luv:request-gpu-device luv:*gpu-provider*))
       (source (luv:create
                device
                (luv:make-texture-descriptor
                 :size '(8 8) :dimensions :2d :format :rgba8-unorm
                 :usage '(:copy-src :copy-dst))))
       (destination (luv:create
                     device
                     (luv:make-texture-descriptor
                      :size '(8 8) :dimensions :2d :format :rgba8-unorm
                      :usage '(:copy-src :copy-dst))))
       (encoder (luv:create device (luv:make-command-encoder-descriptor)))
       (commands nil))
  (unwind-protect
       (progn
         (luv:encode
          encoder
          (luv:make-gpu-clear-texture-command
           :texture source :color #(0.25 0.5 0.75 1.0)))
         (luv:encode
          encoder
          (luv:make-gpu-copy-texture-command
           :source source :destination destination))
         (setf commands (luv:finish encoder))
         (luv:submit (luv:device-queue device) (vector commands)))
    (when commands (luv:destroy commands))
    (luv:destroy encoder)
    (luv:destroy destination)
    (luv:destroy source)
    (luv:destroy device)))
```

## SPIR-V from s-expressions

`:luv/spir-v` is a small assembler whose vocabulary grows with shaders luv
actually runs. IDs are ordinary Lisp symbols and may be referenced before
definitions; result IDs lead their instruction. Its backend vocabulary is
declared with `spv:define-instruction` and `spv:define-enumeration`, with small
family definers for repeated instruction shapes:

Each instruction definition is a CLOS class whose `spv:instruction-class`
metaclass owns its opcode, result convention, and operand encoding. Parsing a
form constructs an instance with named operand slots, so a module can be
inspected and changed as ordinary live Lisp objects before assembly.

```lisp
(asdf:load-system :luv/spir-v)
(spv:assemble
 '((capability shader)
   (memory-model logical glsl-450)
   (%void type-void)
   (%function-type type-function %void)
   ;; ...
   (%main function %void none %function-type)
   (%entry label)
   (return)
   (function-end)))
```

`shader.lisp` adds a deliberately higher-level, live representation:
`spv:spir-v-module` objects contain entry points, execution modes, function
definitions, and basic blocks.  `spv:lower-spir-v` erases that structure into
the instruction instances understood by `spv:assemble`; the literal assembler
in `spir-v.lisp` remains a small and independent bottom layer.

`spv:gradient-compute-module` constructs the demo in that structured IR, and
`spv:gradient-compute-shader` lowers and assembles it into a complete Vulkan
1.0 module. It maps `GlobalInvocationId.xy` to an RGBA8 storage-image gradient
without a GLSL compiler or generated registry binding.

The complete compute-to-canvas path is a live one-liner:

```lisp
(defparameter *compute-demo* (luv:start-compute-gradient-demo))
;; The shader dispatches into an RGBA8 storage texture, then copies it into
;; the acquired surface texture for presentation.
(luv:stop-compute-gradient-demo *compute-demo*)
```

## Canvas

`:luv/canvas` adds the small native counterpart to WebGPU's canvas context
without making SDL a dependency of the offscreen `:luv/gpu` system.  Its CLOS
protocol separates the two sides hidden by a browser: a canvas owns its native
lifetime, size, event source, and frame clock; a canvas context owns the GPU
presentation relationship.  The SDL canvas and Vulkan context meet through a
method specialized on both concrete implementations.

`get-current-texture` is valid only inside a frame callback; the returned
swapchain texture is borrowed from the context rather than destroyed as an
ordinary owned image.

```lisp
(asdf:load-system :luv/canvas)
(defparameter *canvas*
  (luv:make-sdl-canvas
   :title "luv second spike" :width 800 :height 600))
(luv:open-canvas *canvas*)

(defparameter *context*
  (luv:make-canvas-context *canvas* luv:*gpu-provider*))

(luv:render-canvas-color *context* 0.15 0.35 0.95)

(luv:present-canvas-frame
 *context*
 (lambda (texture encoder)
   (assert (eq texture
               (luv:get-current-texture *context*)))
   (luv:encode
    encoder
    (luv:make-gpu-clear-texture-command
     :texture texture :color #(0.7 0.1 1.0 1.0)))))

(luv:close-canvas *canvas*)
```

On Cocoa, the canvas's event loop and frame callbacks run on the process main
thread.  Calls from SLY workers are sent to the canvas thread and wait for the
frame.  `request-canvas-frame` owns that native scheduling step;
`call-with-canvas-frame` owns texture acquisition and presentation.

Because the Cocoa process is a durable Lisp rather than a disposable game
executable, its application policy follows its windows: opening the first SDL
canvas activates and raises a regular macOS application, while closing it
returns the process to background-only status.  Thus closing a canvas removes its
Dock and Stage Manager presence without terminating the SLY image.

Every canvas has a clock policy.  The default `demand-clock` sleeps in SDL
until an OS event or explicit frame request wakes it.  A `cadence-clock` gives
the same canvas a regular frame phase; clocks can be replaced while the window
is open:

```lisp
(setf (luv:canvas-clock *canvas*)
      (luv:make-cadence-clock
       (lambda (canvas timestamp)
         (declare (ignore canvas timestamp))
         (luv:render-canvas-color *context* 0.1 0.2 0.4))
       :frames-per-second 60))

;; Return to explicit REPL-driven frames.
(setf (luv:canvas-clock *canvas*) (luv:make-demand-clock))
```

Cadence clocks skip elapsed frames rather than replaying them after a slow
frame or debugger visit.  SDL user events wake cross-thread requests, and
canvas startup and shutdown use completion semaphores rather than polling.

For a tiny end-to-end animated demo:

```lisp
(defparameter *demo* (luv:start-clear-color-demo))
;; The returned demo, canvas, context, and clock remain live and inspectable.
(luv:stop-clear-color-demo *demo*)
```

Try `:speed 0.25` or `:frames-per-second 30` when starting it to change the
color-cycle rate or cadence.

## McCLIM

`:luv/mcclim` is an optional McCLIM backend in its first deliberately small
form.  It defines a renderer-independent port and mirror substrate, plus a
`:luv` raster port whose medium uses `mcclim-render`. Its first presentation
target is a luv canvas:

```lisp
(asdf:load-system :luv/mcclim)
(setf clim:*default-server-path* '(:luv))
```

The mirror does not equate a CLIM sheet with a native window.  It owns a
relationship between a sheet and a presentation target.  Today that target is
an SDL canvas; later it may be a texture displayed on a quad in a luv scene.
McCLIM rasterizes drawing into an inspectable image. Finishing medium output
uploads that image through `GPUQueue.writeTexture`-shaped machinery and copies
it to the mirror's canvas surface. Native input is not translated into CLIM
events yet.

The small backend laboratory avoids loading the full examples collection:

```lisp
(defparameter *sheet* (luv.mcclim:open-lab-sheet))
;; The same raster image that was uploaded remains inspectable.
(luv.mcclim:lab-sheet-image *sheet*)
(luv.mcclim:close-lab-sheet *sheet*)
```

This experiment follows current McCLIM Git `master`, rather than a Quicklisp
release. Install it as a local project before loading `:luv/mcclim`:

```sh
git clone https://codeberg.org/McCLIM/McCLIM.git \
  ~/quicklisp/local-projects/mcclim
```

## One-time Lisp setup

`cl-sdl3` is not in Quicklisp, so install it as a local project:

```sh
git clone https://github.com/aiffc/cl-sdl3 \
  ~/quicklisp/local-projects/cl-sdl3
```

Its ordinary Lisp dependencies are fetched by Quicklisp when `luv` is loaded.
Nix supplies SDL3, its companion native libraries, libffi, Vulkan, and SBCL.

## Run

Enter the reproducible environment with `nix develop`, load `luv.asd`, and
load either the complete atelier or its SDL-free GPU core:

```lisp
(asdf:load-asd (truename "luv.asd"))
(asdf:load-system :luv)       ; GPU plus native canvas
;; (asdf:load-system :luv/gpu) ; GPU core only
```

Nix supplies SBCL, SDL, the Vulkan loader and tools, and MoltenVK on Apple
Silicon.  A native Vulkan implementation still comes from the host graphics
stack.  Set `SDL_VIDEODRIVER=wayland` when you want to require Wayland rather
than allowing SDL to choose another video backend.

## SLY and the one-shot client

The server-friendly workflow does not need Emacs. Inside `nix develop`,
`./sly` can start a durable Slynk image loaded with `:luv`:

```sh
./sly start
./sly eval '(defparameter *canvas* (open-canvas (make-sdl-canvas)))' --package LUV
./sly eval '(defparameter *context* (make-canvas-context *canvas* *gpu-provider*))' --package LUV
./sly eval '(render-canvas-color *context* 0.7 0.1 1.0)' --package LUV
./sly eval '(close-canvas *canvas*)' --package LUV
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
./sly eval '(render-canvas-color *context* 1.0 0.0 1.0)' --package LUV
./sly eval '(list (canvas-state *canvas*) (canvas-context-state *context*))' --package LUV
./sly inspect '*context*' --package LUV
```

The project-local SLY launcher establishes SBCL's `(debug 3)` compiler policy
before loading the project, and the GPU system applies the same policy when
compiled independently. Besides richer locations, locals, stepping, and
backtraces, this raises SBCL's derived
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
./sly describe RENDER-CANVAS-COLOR OPEN-CANVAS --package LUV
./sly describe RENDER-CANVAS-COLOR --function --package LUV
./sly describe-package luv
./sly describe-system luv
./sly apropos COLOR CANVAS --package LUV
./sly apropos VULKAN-CANVAS --package LUV --all
./sly edit luv:render-canvas-color
./sly edit RENDER-CANVAS-COLOR --package LUV # equivalent
./sly edit luv:render-canvas-color luv:open-canvas
./sly xref calls RENDER-CANVAS-COLOR --package LUV
./sly xref uses MAKE-CANVAS-CONTEXT --package LUV
```

`describe`, `apropos`, `edit`, and `xref` accept multiple names or patterns in
one invocation. Symbol names may be package-qualified directly, so
`sly edit luv:render-canvas-color` and
`sly edit RENDER-CANVAS-COLOR --package LUV` are equivalent.
`describe-package` reports the live package's nicknames, use-list,
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
