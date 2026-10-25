# Gnome and cat — editable bodies and evaluated IK walks

Original mesh interpretations of the gnome and tabby in
`luvcraft/agent/shaders.lisp` and `cat-shaders.lisp`. These are weighted skeletal
meshes, not rigid-object animation or vertex caches. No downloaded geometry,
image textures, or external Python packages are required.

## Deliverables

| Model | Editable source | Baked skeletal interchange | Native runtime | Numerical evidence |
|---|---|---|---|---|
| Gnome | `gnome.blend` | `gnome.glb` | `gnome.fmesh`, `gnome.rig` | `gnome-verification.json` |
| Cat | `cat.blend` | `cat.glb` | `cat.fmesh`, `cat.rig` | `cat-verification.json` |

`build-companions.py` constructs both. It imports construction primitives and
the actual FNT1/GLB exporters and GLB verifier from `../fantus/build-fantus.py`,
without running that builder or changing any Fantus file. Imports disable
bytecode output. Model-specific construction and weighting are separate;
rigging, gait sampling, color-boundary cutting, staging and verification are
shared where useful.

| Budget | Gnome | Cat |
|---|---:|---:|
| Editable vertices | 35,122 | 29,309 |
| Runtime vertices | 35,122 | 34,305 |
| Triangles | 68,214 | 57,634 |
| Deform / total bones | 17 / 22 | 19 / 28 |
| Linear material colors | 18 | 10 |
| FNT1 bytes | 3,246,872 | 3,069,060 |

The cat's additional runtime vertices preserve genuine material boundaries
without textures. The continuous sculpt has its budget reduction **before**
painting. Pattern-directed refinement prevents thin bands being skipped by
long triangles; root-finding cuts each triangle along the actual color-field
boundary. The cream bib, socks and tabby bands therefore survive native export
with smooth borders, rather than selecting whole polygons or depending on
Blender-only procedural colors. No decimation follows those cuts.

## Rebuild

From the checkout root, using Blender 5.2.2 or later:

```sh
/Applications/Blender.app/Contents/MacOS/Blender --background \
  --python-exit-code 1 --python luvcraft/companions/build-companions.py

# Build either one, render front / three-quarter / walking stills,
# and render 24 preview frames at 30 FPS:
/Applications/Blender.app/Contents/MacOS/Blender --background \
  --python-exit-code 1 --python luvcraft/companions/build-companions.py \
  -- --model cat --render --preview
```

`--model` accepts `gnome`, `cat`, or `both` (the default). Review output goes
only to `.amp/in/artifacts/gnome-*` and `cat-*`. The build generates and verifies
the exports, saves the Blender source, **reopens that saved source**, and
repeats FNT1, IK and GLB checks against it. Rendering uses the reopened scene.
No Lisp integration or Git operation is performed by the builder.

After rebuilding either model, regenerate its mesh-matched rest chains with
the engine exporter. This does not modify the blend or extend FNT1; `.rig`
records the mesh MD5, bone IDs, rest joints, poles and phase offsets. The
runtime rejects stale metadata. For example:

```sh
/Applications/Blender.app/Contents/MacOS/Blender --background \
  luvcraft/companions/cat.blend --python-exit-code 1 \
  --python luvcraft/agent/export-rig.py
```

Encode the preview as a repeating GIF and four-cycle MP4:

```sh
for name in gnome cat; do
  ./env --slim ffmpeg -y -framerate 30 -pattern_type glob \
    -i ".amp/in/artifacts/$name-preview-*.png" \
    -filter_complex '[0:v]split[a][b];[a]palettegen[p];[b][p]paletteuse' \
    -loop 0 ".amp/in/artifacts/$name-walk.gif"
  ./env --slim ffmpeg -y -framerate 30 -pattern_type glob \
    -i ".amp/in/artifacts/$name-preview-*.png" \
    -vf 'loop=loop=3:size=24:start=0' -c:v libx264 -crf 19 \
    -pix_fmt yuv420p -movflags +faststart ".amp/in/artifacts/$name-walk.mp4"
done
```

## Edit the rigs

Each source opens on the model, with the rig selected and the **Idle** action
active. In the Action Editor select **Walk**, then play frames **1–48**. Frame
**49** is the repeated endpoint; it is deliberately not an FNT1 sample.
The lighting studio and portrait camera are editable but excluded from exports.
Surface meshes have ordinary vertex groups, materials, UVs and one live linear
Armature modifier. Material and linked-component selection expose small details.

Named bone collections:

- **Animator · foot targets and knee-elbow poles** — nondeforming `ROOT`,
  `CTRL_foot.*`, and `CTRL_pole.*`.
- **Deformation · weighted body** — all deforming joints.
- **Animator · secondary motion** — head and gnome arm/beard/hat or cat tail.

Every leg has an actual two-bone IK constraint on `lower.*`, with a dedicated
target and pole. Both IK stretch and inherited bone scale are disabled.
Foot copy-location/copy-rotation constraints keep the contact plane level and
exact; the **lower-bone endpoint residual is tested independently**, rather
than hidden behind the exact foot control. Bone scale is never animated.

### Gnome

The refined gardener keeps the original stature, blue coat, red cap and leg
placement. A Painter comparison, made from both the original studio render and
native screenshot, informed the folded felt tip, flowing ivory beard and more
carefully tailored clothing. These are actual editable geometry, not painted
overlays or reference images substituted for the model.

Lifted cheeks, tapered brows, lower eyelids and a small smile soften the face.
Conformal eye lenses stop behind the nose. Broad swept beard locks are united
into one continuous sculpt, with shallow comb grooves cut into its surface.
The moustache lifts at its tips; the cap's sections follow its curved centerline
through the folded tip. The old exposed straight hat seam is gone.

The short coat leaves room for the original continuous trousers and crotch.
Smooth tapered sleeves, a standing collar, shaped pocket panels with brass
closures, fine hem stitches, distinct mitten thumbs and saddle stitches give it
a handmade finish. Boot uppers are shaped over a shoe last, with open lined
shafts, stitched welts and flat soles. All important details survive the
linear-color native export. Materials are overridden only within `build_gnome`;
shared helpers and cat output are unchanged. The additional geometry costs
about 1.66× the original vertex budget.

Controls still use suffixes `L` and `R`. The 17 deform bones, rest chains,
48 walk samples and in-place gait are unchanged. Walk includes arm
counter-swing, weight shift, and delayed beard and hat motion. The separate
`gnome.rig` native-IK sidecar is generated from the final saved source using the
integration owner's `luvcraft/agent/export-rig.py`; its mesh MD5 binds it to the
matching FNT1. Regenerate that sidecar after rebuilding the model.

### Cat

The tabby is a lean young-adult quadruped: a deep narrow thorax, tucked lumbar
waist, sloping scapulae and forward-folded thighs. These forms and all four
legs are one continuous sculpt. Slender pasterns meet small oval cream paws
with three rounded toe lobes. The smaller cheekbone-shaped head has a paired
cream muzzle, conformal almond green eyes, pink pinnae and nose, flat tabby
facial markings and six tapered whiskers. The lifted tail has a rounded tip.

Controls retain `front.L`, `front.R`, `hind.L`, and `hind.R`, all 19 deform bone
names/order, four two-bone nonstretching IK chains and the original gait phases.
Foreleg elbow poles point backward; hind knee poles point forward. `CAT_LIMBS`
is the common source for rest joints, leg construction and distance-based
weights. Shoulder/thigh weights fade into the torso without assigning the
midline abdomen to a leg. Tail weights interpolate around the actual four
rest segments, including a blend into the pelvis at the root.
The preserved two-bone linear-skinning rig still loses some joint volume at
extreme flexion; no extra hock bones, corrective shapes or animated scale are
required by these exports.

Painter's young-adult direction guided the proportions; generated imagery is
not mesh evidence. Review artifacts contain matched-camera before/after
portraits, profiles, face closeups and actual Blender walking captures. The
revised model was rendered on Cycles CPU with two threads; the native game
was left untouched while another task measured its rendering stability.

## Coordinate system, dimensions and interaction

Blender is X right, Z up, **−Y forward**. Both exports use the unchanged Fantus
convention: runtime **X right, Y up, +Z forward**:

```text
runtime = (Blender X, Blender Z, -Blender Y)
```

Bind-space bounds in figure units (full precision is in verification JSON):

| Axis | Gnome minimum … maximum | Cat minimum … maximum |
|---|---|---|
| X | −0.404043972 … +0.404000014 | −0.325079471 … +0.325079471 |
| Y | 0 … 1.527324200 | 0 … 1.116243124 |
| Z | −0.229000002 … +0.311712712 | −1.169787049 … +0.723999977 |

Gnome dimensions are **0.808043957 × 1.527324200 × 0.540712714**. Its idle
hat height is **1.511324406**; nominal face target is **0.92** in bind pose,
**0.904** in idle. The asset verifier's alternate tighter sphere is centered at
**(0, 0.78, 0)**, radius **1.00** (animated requirement including margin:
0.980320014); this is **not the sphere selected by native integration**.
This is separate from the original SDF birthday troupe's center/radius helper;
do not change that helper to match this mesh.

Cat dimensions are **0.650158942 × 1.116243124 × 1.893787026** including
whiskers and tail. Its highest idle point is now the lifted tail, at
**1.100243330**; nominal face target remains **0.78** in bind pose, **0.764**
in idle. The asset verifier's alternate sphere is centered at
**(0, 0.53, −0.25)**, with animated requirement including margin **1.150631471**;
its rearward offset makes it tighter, but it is **not used by native integration**.
The complete authored idle/walk envelope remains under 2.0 units in
forward/back length. All these bounds exclude the game's stature multiplier.

Native integration deliberately retains horizontally origin-centered spheres:

| Model | Actual native sphere center | Native radius | CPU-measured maximum radius |
|---|---|---:|---:|
| Gnome | (0, 0.85, 0) | 1.05 | 0.969724 |
| Cat | (0, 0.52, 0) | 1.40 | 1.2942 |

These values evaluate all exported vertices at idle and 192 interpolated walk
poses. Gnome idle/walk palettes remain byte-identical to the original asset.
Earlier native walk/arrival/idle verification and the 0.2266 cat tail-tip motion
measurement predate these visual refinements. Live runtime IK is verified
separately from the Blender asset checks.

## Cadence and unchanged FNT1 contract

Both models target **one figure unit/second**, with stride **0.80** per full
cycle, **48 samples**, authored at **60 FPS**. A cycle lasts **0.8 seconds**:
the gnome makes 2.5 footfalls/second and the cat 5 footfalls/second. Gnome feet
are phased 0 and 0.5, with 58% stance. Cat phases are hind.L=0, front.L=0.75,
hind.R=0.5, front.R=0.25, with 66% stance: a regular four-beat lateral sequence.
The maximum foot lifts are approximately 0.078 and 0.075 respectively.

The clips are **in place**. Backward motion of a stance foot is intentional:
it cancels the separately applied forward actor displacement. An in-place
studio preview consequently shows feet moving backward relative to its fixed
floor; the numerical world-cancellation checks include the actor displacement.
A baked-clip player advances phase by `distance / (stature * stride)` and
blends sampled skin matrices to idle when stopped. Do not add a second root
translation. A live-IK player instead uses the rest chains in `.rig` for
world-space foot contacts; this does not change the FNT1 mesh layout.

FNT1 is exactly the [Fantus contract](../fantus/README.md#fnt1-layout):

1. 24-byte little-endian header: `FNT1`, four uint32 counts (vertices, indices,
   deform bones, walk frames), one float32 stride.
2. 68-byte vertices: position `3f`, normal `3f`, **linear RGB** `3f`, bone IDs
   `4I`, normalized weights `4f`; at most four nonzero influences.
3. uint32 triangle indices.
4. Idle palette followed by 48 cyclic walk palettes, each bone a row-major
   **3×4 float32 skin matrix**: evaluated pose-world × inverse bind, already
   in runtime coordinates. No additional inverse bind is required.

There is **no per-animation seconds field**. JSON cadence metadata describes
the authoring process, not an extension to the binary contract. GLBs contain
baked evaluated **Idle** and **Walk** skeletal animations including all IK;
consumers need no Blender constraints. Redundant identity scale tracks are
checked and omitted. Blender's slight felt micro-bump is a preview treatment;
runtime colors and all important markings are independent of it.

## Verification measurements

Every build parses the actual FNT1 bytes. CPU weighted skinning is compared
to Blender's evaluated mesh at idle plus asymmetric walk frames
4, 11, 19, 28, 37 and 45. The GLB hierarchy, inverse binds and baked tracks
are independently evaluated at sixteen poses. Solver checks run at 192
quarter-frame samples; baked-clip runtime checks apply matrix interpolation
to **all exported vertices** and every identified sole vertex.
Contact/liftoff-straddling intervals are excluded from stance-velocity checks.
The cat also checks that lower-leg and toe vertices have no torso influence:
that regression can distort paws while leaving the sole-contact tests green.

| Maximum error / measurement | Gnome | Cat |
|---|---:|---:|
| FNT1 vs evaluated mesh | 3.502e−7 | 3.08e−7 |
| Wrong-pose negative control (idle instead of walk) | 0.2183 | 0.2684 |
| Independent IK lower-tail residual | 1.104e−6 | 7.196e−6 |
| Foot target error | 0 | 0 |
| Interpolated runtime sole ground error | 3.556e−8 | 1.267e−7 |
| Interpolated runtime stance world slide | 1.656e−7 | 1.077e−7 |
| Minimum runtime vertex height | −3.556e−8 | −1.267e−7 |
| Swing clearance | 0.077988 | 0.074929 |
| GLB skin matrix element error | 5.246e−6 | 1.103e−6 |
| Loop endpoint matrix error | 6.13e−18 | 2.94e−18 |

Both saved `.blend` files were reopened and passed the same checks. The
negative control detects wrong poses by over five orders of magnitude more
than the correct skinning error. Native game integration is deliberately a
separate responsibility; these checks establish the asset/rig/export contract,
not a claim to have exercised the game's renderer.
