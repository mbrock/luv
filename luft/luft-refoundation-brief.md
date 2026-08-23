# LUFT manifold-sheet spike

This is the current implementation note for LUFT. The former face-local
shape-word renderer has been deleted; its lifecycle, camera, temporal resolve,
and atelier were retained around a new mesh boundary.

## Principle

The cubical boundary is not always a two-manifold at lattice edges and
vertices. A mesher must therefore resolve a singular occupancy star into
separate oriented surface sheets before it chooses bevel geometry. It must
not ask Blender's bevel modifier to choose topology.

The spike uses face-connected occupied cells. Around each signed lattice-edge
ray, ordinary transitions pair normally. A checkerboard ray pairs transitions
around each occupied quadrant, duplicating the radial direction when two solid
sheets only touch there. Walking those pairs produces the boundary-link cycles
owned by one vertex star.

The regular junction geometry for each resulting sheet comes from
`blender-arc-stars.sexp`. That corpus is an exact integer oracle for regular
stars only.

## Integer geometry

The starting scale is:

- cell size `a = 8`;
- bevel width `m = 1`;
- face, band, and junction vertices are integer triples;
- the GPU divides positions by eight.

The CPU emits three geometric families into one indexed triangle mesh:

1. inset exposed-face polygons;
2. crease-edge band quads, including two independent bands at a checkerboard;
3. junction polygons selected by each resolved vertex-link cycle.

Every triangle currently owns three vertices. This is intentionally wasteful
for the spike: it makes the per-triangle normal and barycentric construction
coordinate explicit and keeps the ABI easy to inspect.

## GPU boundary

One vertex is one `uvec4`:

| word | meaning |
|---|---|
| x, y, z | unsigned integer position at scale 8 |
| w bits 0-5 | signed normal components encoded in two bits each |
| w bits 6-9 | stock |
| w bits 10-11 | barycentric corner |
| w bits 12-13 | face, band, or junction kind |

Each resident chunk owns compact instance and local template storage. The
renderer retains unchanged chunk slots, shares camera and material tables, and
issues at most one triangle-instance and one quad-instance draw per chunk. A
residency cohort uploads only its replacements and atomically swaps those slots
with any departures. The vertex shader decodes integer site records, projects
them, and emits temporal motion. The fragment shader retains the paper palette,
lighting, fog, MetalFX motion output, and barycentric construction lines.
Close-study construction mode can additionally project integer lattice points;
streamed terrain omits that million-point-per-chunk diagnostic.

## Executable scope

The isolated topology fixture contains:

- `#x06`, an edge-touching occupied pair;
- `#x18`, a corner-touching occupied pair;
- `#x69`, four parity sheets at one vertex.

These cases decompose into ordinary regular-star junctions and are checked for
geometric closure by exact integer edge equality. The exhaustive link walk
also checks all 256 masks and classifies 128 as singular.

The viewer now opens on the connected miter study: a plinth, wall, and two
L-shaped terraces containing ordinary convex, concave, five-, six-, and
seven-cell stars. Flat face continuations no longer emit spurious bevel bands,
and exact polygon normals are reduced to signed direction codes only at the
packed GPU boundary.

That first connected render also names the next geometric problem. The Arc
junction corpus can meet an exposed face along a diagonal corner cut, while the
spike still emits every inset face as a rectangle. The resulting mixed miters
render, but retain visible triangular folds or gaps and 126 unmatched exact
triangle edges in the study. Face corners must therefore be clipped by their
adjacent junction boundary before this is a watertight bevel mesher.

This is not yet the final 256-star mesher. Some connected high-occupancy
singular stars, such as `#x6f`, produce a link cycle that revisits a geometric
radial direction through distinct topological copies. No ordinary eight-bit
star can encode that junction. The spike signals this case explicitly. After
the regular face-to-junction seams close, the remaining singular work is a
covered-junction representation whose vertices are keyed by sheet copy as well
as radial direction, followed by the terrain acceptance scene.
