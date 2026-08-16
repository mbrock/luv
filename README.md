# luv

![A snowy forest in luvcraft](screenshots/shadow-forest.png)

`luv` is a Common Lisp GPU workshop. It has a WebGPU-ish hardware abstraction
layer with hand-built Vulkan and native Metal 4 backends, an SDL3 canvas host,
a mathematical shader language, a little voxel world called **luvcraft**, and
a McCLIM backend for putting live Lisp tools on GPU surfaces.

It is all rather experimental. The point is not to hide graphics programming
behind an enormous engine. The point is to make the interesting machinery small
enough to inspect, change, and keep running while we change it.

## A GPU system we can understand

The HAL borrows the useful shape of WebGPU—devices, queues, resources,
descriptors, encoders, passes, submission—but WebGPU is a landmark, not a
specification luv is trying to reproduce.

Both backends are tailored to this project. Vulkan is a hand-owned CFFI layer;
Metal goes straight through a small Objective-C bridge to Metal 4. There is no
continent of generated bindings in between. SDL3 owns the window, input, and
native event loop while the GPU backend owns presentation and synchronization.

That makes low-level things unusually reachable from Lisp. A command is an
ordinary inspectable object. A shader method can be redefined in a running
image. A queue submission has an explicit lifetime story. When an abstraction
is wrong, we can follow it all the way down and change it.

The longer version lives in [the GPU architecture
notes](wiki/gpu-architecture.org), [the Vulkan field
notes](wiki/luv-vulkan-hal.org), and [the native Metal 4
notes](wiki/metal-backend.org).

## Arithmetic that knows what its numbers mean

Luv's shader language is also a small mathematical language. Its expressions
remember representation types, but they can additionally carry dimensions,
units, affine character, tensor order, and domain meaning. A position is not a
difference; opacity is not probability; two things do not become
interchangeable merely because both happen to fit in a `vec3`.

The same arithmetic frontend can lower checked expressions to ordinary Common
Lisp or feed the shader compilers. Shader methods become SPIR-V for Vulkan and
structured MSL for Metal 4, while retaining enough provenance for tools to
connect a source expression with the SSA instructions it produced.

[Mathematical shaders](wiki/mathematical-shaders.org) explains the live shader
objects. [Quantities and measurement](wiki/quantities-and-measurement.org) is
the more ambitious account of what the arithmetic system is trying to keep
honest.

## Luvcraft is where the abstractions have to survive

![A luminous crystal in the luvcraft screenshot gazetteer](screenshots/glow-floor.png)

Luvcraft is an editable, procedural block world—not because the world needs
another Minecraft clone, but because a game is a good way to stop a rendering
API from becoming a collection of attractive diagrams.

There is terrain generation, chunk streaming, meshing, collision, block
editing, a moving sky, shadows, block light, textured materials, persistence,
and live shader replacement. CPU work crosses explicit ownership boundaries;
GPU objects stay on the render thread; stale asynchronous results are thrown
away instead of becoming mysterious scenery.

The checked-in pictures come from a small screenshot *gazetteer*: named worlds,
cameras, and times of day rendered through the same hidden SDL/GPU path as the
real application. They are visual fixtures as well as illustrations. See [the
block-world notes](wiki/block-world.org) for the current proof and the possible
worlds beyond it.

## McCLIM, on the GPU, inside the world

![The McCLIM shader browser rendered by luv](screenshots/mcclim-shader-lab.png)

CLIM—the Common Lisp Interface Manager—is an old and still unusual way to build
interfaces around semantic objects and commands rather than a pile of inert
pixels. [McCLIM](https://mcclim.common-lisp.dev/) is its open-source Common Lisp
implementation.

Luv has a custom McCLIM backend. Today it can take McCLIM's raster output,
upload it to a GPU texture, and present or composite that texture through the
same canvas system. The shader browser above is a real McCLIM application: the
materials, mathematical expressions, definitions, and lowered instructions are
presentations you can point at and inspect.

The next step is more peculiar: bypass the software rasterizer, retain the
actual CLIM geometry, and render curves and glyphs directly on the GPU. From
there, a listener, inspector, map, or editor does not have to live in a separate
desktop window. It can be a screen, panel, instrument, or object in the 3D
world.

## Live hacking, including by agents

Common Lisp already assumes that a program can stay alive while you inspect and
redefine it. Luv leans into that. `./sly` talks to a durable development image;
every standalone luvcraft process also advertises its own Slynk endpoint, so an
agent can inspect the exact running game, follow cross-references, redefine a
shader, and watch the dependency machinery publish a replacement pipeline.

The repository is arranged for that style of work. [AGENTS.md](AGENTS.md) gives
an agent the concrete operating rules, while the [workshop wiki](wiki/index.org)
keeps architecture, source studies, experiments, and unfinished questions in a
form that both people and agents can navigate from the terminal.

The larger ambition is to stop treating agents as creatures permanently
outside the application. Luvcraft could contain terminals and simulated
devices through which you talk to them, or NPC-like avatars with tools and a
place in the world. Instead of leaving the world to ask an agent to change it,
you might remain inside it and work together there. That part is a direction,
not a finished feature—but much of the introspective plumbing it would need is
already becoming real.

## Poking it

The universal installation instruction in 2026 is: point Codex at this
repository and say **set this thing up**. The project-specific facts it needs
are in [AGENTS.md](AGENTS.md).

Once the environment exists, the short human version is:

```sh
make
./build/luvcraft
```

While the game is running, this evaluates inside that exact process:

```sh
./sly --luvcraft eval '(type-of luvcraft:*session*)'
```

And this regenerates every image used by this README from the real renderers:

```sh
make readme-screenshots
```

This is a workshop, not an authority, and it is very much still under
construction.
