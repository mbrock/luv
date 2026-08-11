# luv

`luv` is an experimental Common Lisp GPU atelier.  Its WebGPU-shaped API is
implemented by a deliberately incomplete, hand-owned CFFI Vulkan layer, while
SDL supplies the native Cocoa or Wayland window.  The binding grows only when
the higher-level API needs another Vulkan capability.

The owned layer uses CFFI's translating types, enums, and bitfields directly.
`defvkfun`, `defvkstruct`, `define-enumerator`, and `define-creator` keep the
Vulkan treaty text explicit while naming its recurring call, allocation,
checking, and count/fetch patterns once.

## Wiki

The [workshop wiki](wiki/index.org) is where luv develops its understanding of
GPU APIs, implementation mechanisms, and the emerging block world.  It treats
WebGPU, Moppe, and small experiments as useful evidence rather than standards
that luv has already adopted, and treats the current luv code as experimental
evidence rather than settled design.

## System Structure

The loading structure separates the experiment by contract:

```text
:luv/gpu/api          portable GPU classes, descriptors, commands, generics
:luv/vulkan/fundament Vulkan loader, invocation bridge, binding macros, tracing
:luv/vulkan/defs      hand-owned Vulkan enums, structs, and raw entry points
:luv/vulkan           Lisp-shaped helpers over the raw Vulkan vocabulary
:luv/gpu/vulkan       Vulkan implementation of the GPU API
:luv/canvas/api       native canvas, events, frame clocks, context protocol
:luv/canvas/sdl       SDL window host and event translation
:luv/canvas/vulkan    Vulkan swapchain presentation for SDL canvases
:luv/examples         demos, PNG capture, and the block world
```

`:luv/gpu` is the convenient GPU bundle with the Vulkan backend and default
provider. `:luv/canvas` adds presentation without loading the demos.
Top-level `:luv` loads the atelier bundle, including examples.

## GPU API

The independently loadable `:luv/gpu/api` system is the beginning of a
WebGPU-shaped HAL vocabulary. The `:luv/gpu` convenience system loads that API
with the Vulkan backend. Its first vertical slice owns a Vulkan instance,
logical device, default graphics queue, command pool, and primary command
buffer:

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

Every host-to-Vulkan call is reified through the general invocation protocol
in `invocation.lisp`, which does for calls what the condition system does for
situations and is meant to be adopted by the next API worth reifying.
`defvkfun` defines each entry point as a class in the `VK` package—
`vkCreateImage` becomes `vk:create-image`—whose instances are invocations
carrying the arguments in slots, and calling `(vk:create-image ...)` hands
one such invocation to the FFI object in `lvk:*vulkan-ffi*` through the
`lvk:invoke` generic function.  The
plain FFI crosses straight into the driver; subclasses observe or replace
crossings with ordinary methods, specialized on one entry point by class, on
the `lvk:vulkan-command` family of `vkCmd*` calls, or on everything.  The
`vkCamelCaseKHR` treaty names live on in the class metadata, and
`do-external-symbols` over `VK` enumerates the whole owned binding.

Tracing is one such subclass: it retains each invocation—with named argument
snapshots, returned values, thread, timing, and signaled conditions—in a
trace.  Record this thread's calls for a dynamic extent:

```lisp
(lvk:with-vulkan-trace (trace)
  ;; Interact with or render a few frames here.
  (lvk:vulkan-trace-presentation-intervals trace))
```

or record process-wide across threads:

```lisp
(lvk:start-vulkan-trace)
;; Interact with or render a few frames here.
(defparameter *vulkan-trace* (lvk:stop-vulkan-trace))
(lvk:vulkan-trace-presentation-intervals *vulkan-trace*)
```

Presentation intervals contain the calls after one `vkQueuePresentKHR`
through the next one.  Pass `:include-prefix t` to include the partial interval
from trace startup through its first presentation.  This is a host API and
command-recording trace, not a measurement of when the GPU executes commands.

The public queue `submit` operation is asynchronous.  Each submission advances
a Vulkan timeline semaphore and retains its command buffers and dependencies
until the completed value passes that submission.  `submitted-work-done`
waits for the current frontier explicitly.  Canvas presentation uses the same
frontier through two rotating frame slots; acquire semaphores belong to slots,
while render-finished semaphores belong to swapchain images.  Recording can
therefore overlap the preceding GPU frame without making resource lifetime
implicit.

GPU work is represented by inspectable command structures.  Command,
render-pass, and compute-pass encoders are sibling subclasses of the abstract
`gpu-encoder`; a pass encoder is not itself a command encoder.  Commands have
matching scope types such as `gpu-command-encoder-command`,
`gpu-render-pass-command`, and `gpu-compute-pass-command`.  `encode` therefore
means exactly one thing: record a command on an encoder of the appropriate
scope.  The WebGPU-flavored verbs are convenient constructors around that
double-dispatch protocol:

```lisp
(luv:encode
 pass
 (luv:make-gpu-draw-command :vertex-count 4))
```

Texture uploads deliberately occupy a different scope.  WebGPU's
`GPUQueue.writeTexture` is a queue convenience operation, represented here by
`gpu-write-texture-command` under `gpu-queue-command` and issued by `enqueue`.
The `write-texture` function is constructor sugar for that pair.
The Vulkan backend currently lowers it to a coherent staging buffer and a
private buffer-to-image copy submission.  Vulkan 1.4's optional host image
copy facility may provide a more direct lowering later; neither choice leaks
into the GPU API.

The first concrete `gpu-buffer` slice keeps host-visible, coherent
`(:uniform)`, `(:vertex)`, and readback `(:copy-dst)` buffers mapped for their
lifetime. `write-buffer` copies a one-dimensional single-float array into an
upload buffer. `read-buffer` waits for the queue completion frontier before
copying mapped bytes back to Lisp. Bind groups may contain a uniform buffer by
itself or combine one with a sampled texture and sampler.

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

(defparameter *device*
  (luv:request-gpu-device luv:*gpu-provider*))

(defparameter *context*
  (luv:make-canvas-context
   *canvas* luv:*gpu-provider*
   (luv:make-canvas-configuration :device *device*)))

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
(luv:destroy *device*)
```

Device acquisition is independent of canvas context creation.  A context does
not own its configured device: destroy or close every context using a device
before destroying the device itself.  The Vulkan provider loaded with the SDL
canvas integration requests the SDL surface extensions and swapchain support;
configuration then verifies that the chosen queue can present to this canvas.
Request the device after opening an SDL Vulkan canvas when presentation is
needed.  A device requested earlier remains useful for offscreen work, but
canvas configuration will reject it because its instance lacks SDL's surface
extensions.

When the driver exposes `VK_EXT_debug_utils`, the Vulkan provider enables it.
Give a provider a `:debug-callback` to install a messenger for the lifetime of
each requested device:

```lisp
(defparameter *debug-provider*
  (make-instance
   'luv:vulkan-gpu-provider
   :debug-callback
   (lambda (message)
     (format *debug-io* "~&[~{~A~^, ~}] ~A~%"
             (lvk:debug-message-types message)
             (lvk:debug-message-text message)))))

(defparameter *device* (luv:request-gpu-device *debug-provider*))
```

The default filter accepts `:warning` and `:error` severities and the
`:general`, `:validation`, and `:performance` message types; customize those
with the provider's `:debug-severities` and `:debug-types` initargs.  The
callback can run on a driver thread and must not retain foreign pointers (the
message passed to it contains copied Lisp strings).  Callback errors are
reported and stopped at the foreign boundary.  Debug utils transports
messages but does not itself enable a validation layer.

At the lower-level Vulkan boundary, `lvk:install-debug-messenger` and
`lvk:destroy-debug-messenger` manage the same instance-scoped lifetime
directly.  Destroy the messenger before its Vulkan instance.

On Cocoa, the canvas's event loop and frame callbacks run on the process main
thread.  Calls from SLY workers are sent to the canvas thread and wait for the
frame.  `request-canvas-frame` owns that native scheduling step;
`call-with-canvas-frame` owns texture acquisition and presentation.
Its Vulkan context rotates two frame slots.  Reusing a slot waits only for
that slot's submission timeline value, then releases the completed framebuffer
and command pool; it never drains the presentation queue between ordinary
frames.

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

## Little block world

The first game-shaped slice is intentionally CPU-authored and small.  A
`block-world` owns a three-dimensional Lisp array of inspectable `block-kind`
objects.  An `exposed-face-mesher` walks it and emits only faces whose neighbor
is empty into one interleaved XYZ/RGB vertex vector.  Face lighting, subtle
per-block variation, and corner ambient occlusion are all ordinary Lisp; the
vertex and fragment stages remain structured s-expression SPIR-V modules.

```lisp
(defparameter *cube-world* (luv:start-cube-world-demo))

;; Click the window, look with the mouse, and fly with WASD.
;; Space rises, either Shift descends, and Escape releases the pointer.

(luv:capture-cube-world-screenshot
 *cube-world* #P"/tmp/luv-block-world.png")

;; World, mesh, and camera are live CLOS objects.
(luv:block-mesh-face-count (luv:cube-world-demo-mesh *cube-world*))
(setf (luv:camera-yaw (luv:cube-world-demo-camera *cube-world*)) 0.8)

(luv:stop-cube-world-demo *cube-world*)
```

Screenshot capture does not depend on a visible window.  It records a real
Vulkan image-to-buffer copy from the rendered color attachment, waits through
the queue completion frontier, reads the mapped buffer, and writes PNG bytes
directly from Lisp.  The first headless-ish workflow is the same SDL canvas
path with the native window kept hidden:

```lisp
(luv:capture-hidden-cube-world-screenshot
 #P"/tmp/luv-block-world.png")

(luv:capture-hidden-cube-world-frames
 #P"/tmp/luv-block-world-frames/" :count 6)
```

From a fresh shell, the same path is scriptable:

```sh
nix develop -c sbcl --script scripts/capture-hidden-block-world.lisp /tmp/luv-block-world.png
nix develop -c sbcl --script scripts/capture-hidden-block-world.lisp /tmp/luv-block-world-frames/ 6
```

The script uses `SDL_VIDEODRIVER=offscreen` automatically when neither
`DISPLAY` nor `WAYLAND_DISPLAY` is present, and the Nix shell points that mode
at Mesa lavapipe so captures work through a CPU Vulkan device.  A desktop
session can keep its normal driver and the window still stays hidden.
`SDL_VIDEODRIVER=dummy` is not enough for this path because SDL cannot create
Vulkan windows on that driver.

## McCLIM

`:luv/mcclim` is an optional McCLIM backend in its first deliberately small
form.  It defines a renderer-independent port and mirror substrate, plus a
`:luv` raster port whose medium uses `mcclim-render`. Its first presentation
target is a luv canvas:

```lisp
(ql:quickload :luv/mcclim)
(setf clim:*default-server-path* '(:luv))
```

The mirror does not equate a CLIM sheet with a native window.  It owns a
relationship between a sheet and a presentation target.  Today that target is
an SDL canvas; later it may be a texture displayed on a quad in a luv scene.
McCLIM rasterizes drawing into an inspectable image. Finishing medium output
uploads that image as a GPU command and copies it to the mirror's canvas
surface.

SDL pointer, keyboard, focus, and close events are translated first into
renderer-independent canvas event objects. The McCLIM mirror then turns those
into CLIM events. Callback-only gadget demos drain their frame queues on luv's
canvas thread; conventional applications retain their own
`run-frame-top-level` process and consume the same translated events from
McCLIM's concurrent queue.

The small backend laboratory avoids loading the full examples collection:

```lisp
(defparameter *sheet* (luv.mcclim:open-lab-sheet))
;; The same raster image that was uploaded remains inspectable.
(luv.mcclim:lab-sheet-image *sheet*)
(luv.mcclim:close-lab-sheet *sheet*)
```

There is also a small real-gadget proof with a push button and toggle:

```lisp
(defparameter *widgets* (luv.mcclim:open-widget-lab))
(luv.mcclim:widget-lab-click-count *widgets*)
(luv.mcclim:widget-lab-toggle-value *widgets*)
(luv.mcclim:close-widget-lab *widgets*)
```

The first compositing proof keeps that McCLIM raster as a sampled texture,
draws a vertexless perspective quad into an offscreen render attachment, and
copies the result to the canvas.  Both shader stages come from the
s-expression SPIR-V IR:

```lisp
(defparameter *spinning-widgets*
  (luv.mcclim:open-spinning-widget-lab :speed 0.08))
(luv.mcclim:close-widget-lab *spinning-widgets*)
```

The animation sine/cosine live in a 16-byte uniform block.  The compositor
keeps one persistently mapped buffer and bind group per swapchain image, so a
host update never races a preceding frame and requires no upload submission.
Pointer events still use the flat sheet coordinates; inverse-projecting them
through the quad is the next part of making transformed CLIM sheets fully
interactive.

The optional Listener component runs McCLIM's real Listener reader and frame
top level on that second regime:

```lisp
(ql:quickload :luv/mcclim/listener)
(multiple-value-bind (process frame) (luv.mcclim:open-listener)
  (defparameter *listener-process* process)
  (defparameter *listener* frame))
```

The `mcluv` program packages that Listener and the luv backend into an SBCL
executable. Build it from the checkout, then run it inside the development
environment:

```sh
make mcluv
nix develop -c ./mcluv
```

Closing the Listener frame ends its McCLIM process and then exits `mcluv`.

Its menu bar is temporarily disabled: McCLIM menus are separate popup frames,
while luv's current Cocoa SDL host owns only one native canvas at a time. The
Listener panes, input editor, evaluator, presentations, and keyboard gestures
otherwise run unchanged. A shared multi-canvas SDL host is the next step for
native menus and dialogs.

This experiment follows McCLIM's Git `master`, rather than Quicklisp's McCLIM
snapshot. The Nix flake supplies a locked upstream checkout to ASDF through
`CL_SOURCE_REGISTRY`. `ql:quickload` still fetches McCLIM's ordinary transitive
Lisp dependencies, such as `cluffer`; luv declares only its direct dependency
on `mcclim-render`.

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
load whichever slice you want to hack on:

```lisp
(asdf:load-asd (truename "luv.asd"))
(asdf:load-system :luv)         ; canvas plus demos and experiments
;; (asdf:load-system :luv/gpu)  ; GPU API plus Vulkan backend
;; (asdf:load-system :luv/gpu/api)
;; (asdf:load-system :luv/canvas)
```

Nix supplies SBCL, SDL, the Vulkan loader and tools, and MoltenVK on Apple
Silicon.  A native Vulkan implementation still comes from the host graphics
stack.  Set `SDL_VIDEODRIVER=wayland` when you want to require Wayland rather
than allowing SDL to choose another video backend.  On Linux, the development
shell leaves Vulkan driver discovery to the host so a hardware ICD can be used.
To test explicitly with Nix's software renderer instead, set `VK_DRIVER_FILES`
to the appropriate `lvp_icd.*.json` under the Nix Mesa package's
`share/vulkan/icd.d` directory.

## SLY and the one-shot client

The server-friendly workflow does not need Emacs. Inside `nix develop`,
`./sly` can start a durable Slynk image loaded with `:luv`:

```sh
./sly start
./sly eval '(defparameter *canvas* (open-canvas (make-sdl-canvas)))' --package LUV
./sly eval '(defparameter *device* (request-gpu-device *gpu-provider*))' --package LUV
./sly eval '(defparameter *context* (make-canvas-context *canvas* *gpu-provider* (make-canvas-configuration :device *device*)))' --package LUV
./sly eval '(render-canvas-color *context* 0.7 0.1 1.0)' --package LUV
./sly eval '(close-canvas *canvas*)' --package LUV
./sly eval '(destroy *device*)' --package LUV
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
