# Gnome and cat — editable bodies and evaluated IK walks

Original mesh interpretations of the gnome and tabby in
`luvcraft/agent/shaders.lisp` and `cat-shaders.lisp`. These are weighted skeletal
meshes, not rigid-object animation or vertex caches. No downloaded geometry,
image textures, or external Python packages are required.

## Deliverables

| Model | Editable source | Baked skeletal interchange | Native runtime | Numerical evidence |
|---|---|---|---|---|
| Gnome | `gnome.blend` | `gnome.glb` | `gnome.fmesh` | `gnome-verification.json` |
| Cat | `cat.blend` | `cat.glb` | `cat.fmesh` | `cat-verification.json` |

`build-companions.py` constructs both. It imports construction primitives and
the actual FNT1/GLB exporters and GLB verifier from `../fantus/build-fantus.py`,
without running that builder or changing any Fantus file. Imports disable
bytecode output. Model-specific construction and weighting are separate;
rigging, gait sampling, color-boundary cutting, staging and verification are
shared where useful.

| Budget | Gnome | Cat |
|---|---:|---:|
| Editable vertices | 21,162 | 28,020 |
| Runtime vertices | 21,162 | 33,738 |
| Triangles | 41,368 | 55,334 |
| Deform / total bones | 17 / 22 | 19 / 28 |
| Linear material colors | 17 | 10 |
| FNT1 bytes | 1,975,440 | 3,002,904 |

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

The short slate-blue coat leaves real room for trousered legs and leather
boots. The pelvis, crotch and both trouser legs are a single continuous sculpt;
sleeves and mittens are softened joined forms. The head/nose is sculpted,
the cream beard and swept moustache have shallow combed details, and the eye
lenses follow the face instead of exposing spherical eyeballs. The scarlet
cap bends sideways and forward. Walk includes arm counter-swing, weight shift,
and delayed beard and hat motion. Controls use suffixes `L` and `R`.

### Cat

The standing tabby has a continuous head/chest/ribcage/shoulder/haunch/four-leg
sculpt, four distinct cream paws, conformal green slit-pupil eyes, pink pinnae
and nose, six tapered whiskers, and a four-bone expressive ringed tail.
Controls use `front.L`, `front.R`, `hind.L`, and `hind.R`. Foreleg elbow poles
point backward; hind knee poles point forward. The upper-leg weighting fades
into the correct shoulder/haunch, without assigning the belly to a fifth leg.

## Coordinate system, dimensions and interaction

Blender is X right, Z up, **−Y forward**. Both exports use the unchanged Fantus
convention: runtime **X right, Y up, +Z forward**:

```text
runtime = (Blender X, Blender Z, -Blender Y)
```

Bind-space bounds in figure units (full precision is in verification JSON):

| Axis | Gnome minimum … maximum | Cat minimum … maximum |
|---|---|---|
| X | −0.405493319 … +0.405313909 | −0.360146850 … +0.360146850 |
| Y | 0 … 1.559999943 | 0 … 1.045979738 |
| Z | −0.229000002 … +0.307112515 | −1.297990203 … +0.741999984 |

Gnome dimensions are **0.810807228 × 1.559999943 × 0.536112547**. Its idle
hat height is **1.544000149**; nominal face target is **0.92** in bind pose,
**0.904** in idle. The asset verifier's alternate tighter sphere is centered at
**(0, 0.78, 0)**, radius **1.00** (animated requirement including margin:
0.981999421); this is **not the sphere selected by native integration**.
This is separate from the original SDF birthday troupe's center/radius helper;
do not change that helper to match this mesh.

Cat dimensions are **0.720293701 × 1.045979738 × 2.039990187** including
whiskers and tail. Idle ear-tip height is **1.029979944**; nominal face target
is **0.78** in bind pose, **0.764** in idle. The asset verifier's alternate sphere
is centered at **(0, 0.53, −0.25)**, radius **1.25** (animated requirement including
margin: 1.246358249); its rearward offset makes it tighter, but it is **not used
by native integration**. The complete idle/walk envelope remains under 2.2 units
in forward/back length. All these bounds exclude the game's stature multiplier.

Native integration deliberately retains horizontally origin-centered spheres:

| Model | Actual native sphere center | Native radius | Runtime-measured maximum radius |
|---|---|---:|---:|
| Gnome | (0, 0.85, 0) | 1.05 | 0.9711 |
| Cat | (0, 0.52, 0) | 1.40 | 1.3756 |

The runtime integration owner verified both native walks, arrival, and blending
back to idle, and measured 0.2266 figure units of lateral cat tail-tip motion.
Those are separate runtime results, not measurements made by this asset builder.

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
Native phase advances by `distance / (stature * stride)`. Blend the sampled
skin matrices to idle when stopped. Do not add a second root translation.

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
quarter-frame samples; runtime checks apply the same matrix interpolation
used by the game to **all exported vertices** and every identified sole vertex.
Contact/liftoff-straddling intervals are excluded from stance-velocity checks.

| Maximum error / measurement | Gnome | Cat |
|---|---:|---:|
| FNT1 vs evaluated mesh | 3.61e−7 | 3.21e−7 |
| Wrong-pose negative control (idle instead of walk) | 0.2183 | 0.2684 |
| Independent IK lower-tail residual | 1.104e−6 | 3.336e−7 |
| Foot target error | 0 | 0 |
| Interpolated runtime sole ground error | 3.13e−8 | 1.061e−7 |
| Interpolated runtime stance world slide | 1.337e−7 | 9.593e−8 |
| Minimum runtime vertex height | −3.13e−8 | −3.09e−8 |
| Swing clearance | 0.077988 | 0.074929 |
| GLB skin matrix element error | 5.246e−6 | 1.193e−6 |
| Loop endpoint matrix error | 6.13e−18 | 2.94e−18 |

Both saved `.blend` files were reopened and passed the same checks. The
negative control detects wrong poses by over five orders of magnitude more
than the correct skinning error. Native game integration is deliberately a
separate responsibility; these checks establish the asset/rig/export contract,
not a claim to have exercised the game's renderer.
