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

Export the identical three resources for any static HTTP host:

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
- `client.lisp` groups sites by star. Each occupied pattern gets one
  `InstancedMesh`, with the owned triangle patch shared by every instance.
  Two instance attributes carry the eight material IDs; each triangle's
  native material mask selects their equal-weight mean. Three.js owns the
  standard material, normal transforms, shadows, and instancing, plus
  EffectComposer, RenderPass, UnrealBloomPass, and OutputPass.
- `page.lisp` supplies Spinneret markup, styling, resource registration, and
  optional live/static entry points. There is no separate handwritten JS app.

This first demo uses a finite 48×48 highland and width-one bevels. It has
orbit/touch inspection, pointer-lock walking, jumping, block edits, five
materials, and wireframe inspection. It rebuilds the finite scene's instance
batches after an edit and disposes the old GPU resources. Changes are local
to the page and disappear on reset/reload. Touch devices get orbit controls;
walking currently needs a mouse and keyboard. Native streaming, voxel-light
propagation, and native postprocessing shaders are outside this version.

## Checks

`(asdf:test-system "luft/web")` executes the compiled browser selection code
in Node against native Lisp results for all 256 translated occupancy fixtures,
then checks ray picking, reach, and body collision. Node is needed only for
these development tests. Browser QA should additionally cover WebGL shader
compilation, shadows, a walk/jump/edit cycle, resource counts after repeated
edits, resizing, and the static export under a URL prefix.
