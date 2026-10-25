# Fantus — editable felt mesh and IK walk

An original procedural mesh interpretation of NRK's Fantus, adapted from the
seated plush to standing and walking. This is a weighted mesh, not an SDF, a
collection of animated rigid primitives, or a baked vertex cache.

## Files

- `build-fantus.py` — reproducible construction, rig, actions, exporters and checks.
- `fantus.blend` — editable surface, weights, materials, foot/knee controls, actions,
  UVs, portrait camera and a separate, unexported lighting studio.
- `fantus.glb` — skinned mesh with evaluated/baked **Idle** and **Walk** animations;
  no Blender IK dependency at playback time.
- `fantus.fmesh` — the game's FNT1 mesh and sampled skin matrices.
- `verification.json` — machine-generated numerical evidence and bone ordering.

The delivered mesh has **21,647 editable vertices**, **22,087 runtime vertices**,
**41,860 triangles**, **16 material colors**, **21 deform bones** and **26 total
bones**. Material-boundary duplication accounts for the runtime difference.
The runtime file is 2,053,652 bytes. All vertices have normalized weights, with
at most four influences; unused bone-index slots contain valid index zero.

## Rebuild and inspect

From the checkout root, with Blender 5.2 or later:

```sh
/Applications/Blender.app/Contents/MacOS/Blender --background \
  --python-exit-code 1 --python luvcraft/fantus/build-fantus.py

# Also render front, three-quarter and lifted-foot walk views:
/Applications/Blender.app/Contents/MacOS/Blender --background \
  --python-exit-code 1 --python luvcraft/fantus/build-fantus.py -- --render

# Also produce 24 preview PNGs, every other frame of the 60 FPS action:
/Applications/Blender.app/Contents/MacOS/Blender --background \
  --python-exit-code 1 --python luvcraft/fantus/build-fantus.py -- --preview
```

Review images are written only to `.amp/in/artifacts/fantus-*.png`.
The delivered review set also includes `fantus-walk.gif` and `fantus-walk.mp4`.
Encode a newly generated preview with the repository's FFmpeg:

```sh
./env --slim ffmpeg -y -framerate 30 -pattern_type glob \
  -i '.amp/in/artifacts/fantus-preview-*.png' \
  -c:v libx264 -crf 19 -pix_fmt yuv420p -movflags +faststart \
  .amp/in/artifacts/fantus-walk.mp4
```

Open `fantus.blend`; it is saved in the **Idle · grounded rest** action. Select
`FantusRig`, switch the Action Editor to **Walk · foot IK · 0.80 units**, and play
frames **1–48**. Frame **49** is the repeated loop endpoint. The action's location
and rotation channels are editable; the constraints remain live.

The `Animator · feet and poles` bone collection contains `CTRL_foot.L/R`,
`CTRL_knee.L/R` and `ROOT`. Move a foot target to pose a leg; move its pole to
change the knee's direction. Deforming bones are in their own collection.
The material-separated surface is one mesh; linked-component selection and
material-slot selection provide access to the sewn panels and small details.

## Modeling and rig decisions

- The torso, arms, mittens, and neck are voxel-joined and smoothed into one
  continuous felt surface. Each leg and foot is likewise joined before skinning.
- The broad head, notched cream mask, cupped paddle ears, curved ringed trunk,
  shallow wide oval eyes and tailored nappy have deliberately shaped surfaces.
  The nappy has two actual leg openings, thickness, green bindings and buttons.
- The eye lenses follow the face instead of exposing spherical side walls.
  Raised cream upper lids, olive-hazel irises, pupils and matched catchlights
  give the resting expression an alert, gentle look.
- The concave mask is properly triangulated before being draped onto the head.
  A projected-area assertion guards against overlapping triangles above the
  trunk. The mask and delicate eye geometry are excluded from final decimation.
- Felt materials use restrained procedural micro-bump and sheen in Blender.
  The GLB retains material colors and PBR properties; the custom runtime uses
  linear RGB only, without procedural fabric bump or texture dependencies.
- Both shins have actual **two-bone IK constraints**, dedicated foot targets and
  forward knee poles. Stretch is disabled. World-space rotation and position
  constraints keep the sole exactly level and at the target, independent of the
  IK solver's very small convergence residual. The shin-tail residual is checked
  separately, rather than hidden by the foot constraint.
- The walk includes double support, a grounded stance, a lifted return swing,
  body weight shift, arm counter-swing, and delayed ear/trunk motion. The lower
  nappy follows the thighs. The idle pose has relaxed knees and both feet down.
- Bone scales are never animated. GLB export checks and removes identity scale
  tracks, including tiny solver roundoff. Animation optimization is disabled to
  preserve the measured foot motion; animation time starts at zero.

## Size, coordinates and cadence

Blender uses **+Z up, -Y forward**. Both exported formats use **+Y up, +Z forward**:

```text
runtime = (Blender X, Blender Z, -Blender Y)
```

The bind-space bounds, in runtime figure units, are approximately:

```text
X: -0.70256 .. +0.70256
Y:  0.00000 ..  1.60000
Z: -0.30291 .. +0.50411
```

The relaxed idle crown is about 1.586 high; the face center is about 1.25.
The game's proposed sphere centered at `(0, 0.80, 0)` with radius `1.15` is
conservative for this figure. Dimensions here exclude the game's stature scale.

**Stride is 0.80 figure units per full two-step cycle.** There are **48 samples**
per cycle, excluding the repeated endpoint. The source action is authored at
60 FPS: a cycle lasts **0.8 seconds**, or **2.5 steps/second** at one figure unit
per second. With stature `0.8`, this corresponds to **0.8 world cells/second**.
Each foot spends 58% of its cycle in stance; maximum swing clearance is about
0.078 figure units. The short double-support interval is intentional.

The clips are in place. Drive phase by **distance / stride**, not elapsed time
alone, and apply actor movement separately. A planted foot's backwards motion
then cancels forward actor motion. Blend the skin matrices to the saved idle
pose when stopped; do not apply root translation from a second source.

## FNT1 layout

All values are little-endian, with no padding:

1. ASCII `FNT1`.
2. Four `uint32`: vertex count, index count, deform-bone count, walk-frame count.
3. One `float32`: stride length. The entire header is **24 bytes**.
4. Vertex records, **68 bytes** each: position `3f`, normal `3f`, linear RGB `3f`,
   bone indices `4I`, normalized weights `4f`.
5. Triangle indices, `index_count` values of `uint32`.
6. The idle pose, followed by 48 walk poses. Each pose contains one row-major
   **3×4 float32 skin matrix** per deform bone.

The matrices are evaluated **pose-world × inverse-bind**, already converted
into runtime coordinates. They act directly on the undeformed positions in
the file. They are not raw bone transforms and must not receive another
inverse-bind multiplication. Use their upper 3×3 parts for weighted normals,
then normalize. Bone ordering is recorded in `verification.json`.

## Verification

Every build parses its actual FNT1 bytes and compares CPU weighted skinning
with Blender's evaluated armature mesh, using idle plus asymmetric walk frames
4, 11, 19, 28, 37 and 45. It also evaluates the actual GLB hierarchy, inverse-bind
matrices and baked animation tracks at sixteen poses. The checks assert valid
counts, indices, finite/unit normals, normalized weights, periodic endpoints,
no stretching, level feet, stance cancellation and nonzero swing clearance.

Recorded on Blender 5.2.2, in figure units unless stated otherwise:

- FNT1 versus Blender: maximum vertex error **3.70e-7**.
- Negative control, substituting idle matrices for walking: **0.222** error.
- Foot target error: **0**; maximum stance ground error **2.10e-7**.
- Stance world-slide error: **1.27e-8**, across 216 interior stance subframe samples.
- IK shin-tail convergence residual: **4.06e-5**.
- Maximum swing lift: **0.07799**.
- GLB versus evaluated skin matrices: maximum element error **4.62e-6**.

Full unrounded measurements are generated in `verification.json`. The stance
subframe check excludes the single sampling interval containing contact/liftoff;
the source action uses linear interpolation between 48 samples. The custom
renderer likewise interpolates matrices rather than rerunning IK at runtime.

The build has no dependencies on the game renderer and changes no Lisp code.
Game integration and live route playback are separate checks owned by the
renderer implementation, not claims made by this build script.

## References and rights

Proportions and identifying features follow `wiki/agent-in-the-world.org`
(figure JGEQ4U), the existing Fantus reference images, and the licensed
[Fantus plush product photo](https://lekekassen.no/fantus-bamse-18-cm-nrk-super-112054).
No reference photograph or third-party mesh is embedded in these assets.
Fantus is an NRK character; this workshop interpretation does not grant rights
to the character or imply an official licensed asset.
