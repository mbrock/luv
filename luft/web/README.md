# Luft in the browser

`luft/web` is a small ParenScript application in the existing `luv-wiki`
resource model. It runs on WebGL 2 with Three.js 0.180.0 and needs no Lisp
process after publication. Three.js and its addons load from the pinned
jsDelivr URLs in the import map.

Start it in a managed image:

```sh
./sly --lisp luft-web load luft/web
./sly --lisp luft-web eval '(luft.web:serve-demo)'
```

If that image does not exist, first run `./sly start --name luft-web`.
Open <http://127.0.0.1:8777/luft-demo.html>. The default listener is loopback;
`serve-demo` also accepts `:host` and `:port`. Reload the system and refresh
the browser to see changes. The server retains generated-resource functions,
so it does not need restarting for ordinary source edits.

Export the identical four resources (HTML, CSS, main module, CPU worker) for
any static HTTP host:

```lisp
(luft.web:publish-demo #P"build/luft-web/")
```

The full workshop website also includes `luft-demo.html` and advertises it as
**Play Luft** in the shared navigation. Its existing Pages workflow publishes
it on pushes to main.

## The shared boundary

- `data.lisp` exports `luft:star-atlas-owned-triangles` and
  `luft:star-atlas-owned-appearance-masks` for every occupancy pattern. These
  are the native renderer's unfolded, oriented, ownership-filtered atlas
  packets. Mesh ticks become unit cells by division by eight; XYZ stays Z-up.
- `core.lisp` compiles the browser's cell map, lattice occupancy selection,
  ray traversal, and body collision through ParenScript. A site reads its
  eight incident cells using the native bit convention.
- `streaming.lisp` separates deterministic terrain/halo generation from
  main-thread residency and the session's sparse edit overlay.
- `worker.lisp` compiles a DOM/Three-free worker and a bounded transport.
  Cell and mesh arrays transfer ownership rather than being copied back.
- `meshing.lisp` expands the exact native atlas into one draw per chunk.
  Each triangle's native material mask selects its equal-weight material
  mean. Positions stay exact; normalized byte normals/colors halve storage.
  Three.js owns the standard material, culling and shadows, plus
  EffectComposer, RenderPass, SSAOPass, UnrealBloomPass, and OutputPass.
- `page.lisp` supplies Spinneret markup, styling, resource registration, and
  optional live/static entry points. There is no separate handwritten JS app.

## Streaming highland

The original 48×48 authored scene, including its air, remains intact inside
a continuous procedural landscape. The old terrain function continues over
the border and blends into broader hills over 32 cells. Walking, jumping and
editing work on keyboard/mouse and touch; there is no longer an XY edit fence.
The vertical build range is 0–63.

Chunks are 16×16×64 cells with a sample halo. Each owns disjoint lattice
sites, so borders use the same occupancy and appearance as a single mesh.
Nine nearby chunks finish before spawning the player. Generation and mesh
expansion run in one Web Worker, nearest ring first, with at most one job in
flight and no queued backlog. The main thread only realizes the returned
mesh and collision data. The target radius is five
chunks (121 total); a sixth eviction ring avoids thrashing (at most 169).
Far chunks release both their typed cell arrays and GPU geometry. Unloaded
cells block physics until their render product is ready. Edits invalidate
in-flight snapshots by epoch; results outside current demand are discarded
after travel. Dirty resident chunks have priority over expansion and keep
their old mesh until replacement is ready. Stopping streaming terminates the
worker. Worker failures surface a reload-to-retry message, not an expensive
silent main-thread fallback. CPU fixtures inject synchronous generation into
the same residency policy.

Edits live outside residency, including explicit air tombstones. Returning
to an evicted chunk replays them. A boundary edit updates neighboring halos
and only remeshes the affected chunks. Edits survive travel, not page reload.
Geometric LOD, persistent saves and voxel-light propagation remain future work.

## Movement and Safari

Walking automatically jumps one-block steps when the landing and overhead
space are clear; tall walls and low ceilings do not trigger it. Manual jump
remains available. The touch stick has a dead zone, analog movement and a
full-forward run threshold. Movement, looking and held editing use independent
pointer captures with cancellation/blur cleanup.

The canvas follows dynamic viewport height, safe-area insets and an actual
size observer. Touch-action, overscroll, callout/selection suppression and
non-passive Safari gesture handlers suppress browser zoom/pan where permitted.
Fullscreen requests target the whole document so controls remain available.
The button checks the API, not a browser-version guess. When unavailable it
explains Safari's Share → Add to Home Screen option; standalone metadata also
supports older iOS. OS/browser accessibility overrides cannot be disabled.
Chromium coarse-pointer checks are not real iPhone Safari verification.

## Lighting

The sun keeps native Luft's direction, but uses gentler color and brighter
diffuse sky/ground fill. Three's irradiance is scaled by pi to match native
radiance. ACES uses fixed rather than adaptive exposure. Distance fog fades
terrain before the resident boundary and follows loading progress at startup.
A shader sky adds a blue zenith, luminous horizon, sun haze and subtle wisps.
The final SSAO multiplier fades to white using the same view-depth fog law,
so already-hidden distant bevels cannot reappear as dark ghost outlines.
Selection is a derivative-antialiased warm inset on the targeted face, not a
wire cube. Sky and selection do not contribute false depth to contact AO.
A 2048² shadow map follows the player on a light-space texel-aligned grid;
edits and chunk changes invalidate it. Sixteen-sample, 0.65-cell contact AO runs
before bloom and the single output color transform, including on touch
devices. Touch rendering is capped at 1.5× pixel density and omits bloom.

Terrain explicitly casts **front-face** shadows. Three's default back-face
casting records exit surfaces and leaks sunlight along the concave bevels;
PCF spreads the leak into a bright fringe. Disabling AO/bloom or zeroing the
old bias does not cure it. Front-face casting removes the leak, with bias
tuned for the fixed dusk direction and shadow frustum to avoid self-shadow
acne. Recheck the bias if changing that direction, frustum, or filter.

This matches the native warm/cool lighting separation, not the entire native
renderer: voxel-propagated crystal/torch illumination, temporal reconstruction,
and the native atmospheric scattering model are still absent. Browser performance must be checked
on actual phones; software rendering in an orb is not a mobile benchmark.

## Checks

`(asdf:test-system "luft/web")` executes the compiled browser selection code
in Node against native Lisp results for all 256 translated occupancy fixtures,
then checks ray picking, reach, body collision, streaming seams, bounded
residency, edit replay and all 256 packed mesh templates. Real Node worker
threads check transferred products against synchronous generation; mocked
transport checks cover epochs, travel, priority, teardown and failures. Input
tests integrate autojump physics through takeoff and landing. Node is needed only for
these development tests. Browser QA should additionally cover WebGL shader
compilation, shadows, a walk/jump/edit cycle, resource counts after repeated
edits, resizing, and the static export under a URL prefix.

The shadow regression executes real GPU draws and reads linear HDR pixels:

```sh
agent-browser open http://localhost:8777/luft-demo.html
agent-browser set viewport 900 700 2
agent-browser wait --fn 'window.luftDemo?.ready'
agent-browser eval "$(cat luft/web/shadow-check.js)"
```

It replaces the world with a wall/floor contact, disables AO, bloom and fog, and
checks four camera offsets. Old settings must reproduce excess contact light;
current settings must match ambient-only contact lighting within 5%, while
sunlit surfaces must retain at least 98% of their unshadowed brightness (no
acne). Reload afterward to restore the demo and animation.

On a fresh ready demo, run `agent-browser eval "$(cat luft/web/streaming-check.js)"`
to exercise GPU chunk replacement, distant/negative-coordinate travel, edit
replay and disposal. Reload after this test too. `luftDemo.rebuild()` explicitly
stops streaming and renders `luftDemo.cells` as a finite inspection fixture;
ordinary gameplay does not use that full-scene rebuild path.

`presentation-check.js` checks six selection-face orientations and actual HDR
readback: AO remains visible nearby but cannot reveal fully fogged terrain.
Run it with `agent-browser eval` on a fresh ready demo, then reload afterward.
