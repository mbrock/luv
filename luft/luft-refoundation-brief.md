# LUFT re-foundation brief

## Task for a fresh implementation agent

You are given this brief together with the current basic `luft.lisp`. Write a
new, single Common Lisp source file that can become the executable foundation
for LUFT. Do not merely patch or concatenate the old implementation. Rebuild
the system coherently from the ideas and invariants below.

The new file should contain:

1. the packed cubical-site and world-domain model;
2. canonical incidence, boundary, and coface operations;
3. a compact, normalized chain representation without hash-table ledgers;
4. the strict-minority moment chamfer classifier;
5. packing and unpacking of one shape word per oriented boundary face;
6. a CPU reference realization of the sixteen points of a face patch;
7. canonical positive- and negative-winding index templates for its eighteen
   raster triangles;
8. focused executable tests of the mathematical and representational
   invariants.

This first file is an executable specification and a sound CPU-side basis. Do
not implement Metal, Vulkan, or the renderer command protocol in it. Do define
the exact face-record and topology ABI that those backends will consume later.

Prefer direct functions, packed integers, and unboxed arrays. Do not construct
a CLOS object graph of cells, faces, edges, or points. The old code is an
oracle to investigate, not an authority to imitate when it conflicts with the
site-local mathematical definition below.

---

## 1. What LUFT is trying to represent

LUFT begins from a dissatisfaction with treating every spatial thing as an
anonymous three-coordinate record. A lattice point, an edge, a square face,
and a cubic cell are not four unrelated objects that happen to carry similar
coordinates. They are the dimensions of one cubical lattice, related by
canonical incidence.

A **site** therefore consists of:

- a lattice-point anchor `(x,y,z)`;
- a three-bit extent mask saying which unit intervals extend forward from the
  anchor;
- one polarity bit giving the site's orientation.

The spatial extent bits have the following meanings:

| Extent bits | Site |
| --- | --- |
| `000` | vertex |
| `001`, `010`, `100` | X, Y, or Z edge |
| `011`, `101`, `110` | XY, XZ, or YZ face |
| `111` | cubic cell |

Dimension is simply the population count of the extent mask.

This representation makes geometric identity canonical. The high-X boundary
of the cell at `(x,y,z)` is exactly the same YZ face as the low-X boundary of
the cell at `(x+1,y,z)`. It is not a duplicated decoration owned once by each
cell. The two incidences have opposite orientations, represented by opposite
polarities of the same geometric site.

Clearing the polarity bit gives `site-geometry`, the common identity of the
two orientations. Toggling it gives `opposite-site`. Ordinary fixnum equality
within one world domain is therefore geometric equality plus orientation.

The companion `luft.lisp` supplies the intended 60-bit layout and domain
rules. Preserve that ABI unless a test exposes a contradiction:

- three low extent bits;
- one low polarity bit;
- 24 bits of X;
- 24 bits of Y;
- eight bits of Z;
- two unused fixnum payload bits on 64-bit SBCL.

X and Y wrap according to independently chosen power-of-two domain masks. Z
does not wrap. A Z-extended site may not begin on the uppermost lattice plane.
All construction and stepping must produce the domain's canonical packed
coordinates.

### Orientation and incidence

Axes are ordered X, Y, Z. Removing one present extent bit produces a low and a
high boundary site. Boundary polarity is the canonical alternating incidence
sign induced by that axis order, multiplied by the parent site's polarity.

The boundary operator must have the expected consequences:

- a cell produces six oriented faces;
- a face produces four oriented edges;
- an edge produces two oriented vertices;
- neighbouring positive cells contribute opposite orientations of their
  shared face;
- `boundary(boundary(chain))` normalizes to zero.

Coface operations are the inverse local incidence operation: given a site and
an absent extent axis, construct the forward or backward coface with whichever
polarity makes the original site its exact signed boundary.

These are not renderer conveniences. They are the core topology.

---

## 2. Chains: topology as normalized signed sites

A **chain** is a finite collection of signed sites. Opposite occurrences of
one geometry annihilate; equal occurrences accumulate. Algebraically, it is a
sparse integer-valued function on geometric sites, although its public
elements remain oriented sites rather than `(geometry, coefficient)` objects.

The old pair of positive and negative hash-table ledgers is useful as a simple
reference, but it is an unsuitable durable representation. A packed site was
designed to be compact, comparable, sortable, and streamable. Surrounding
every eight-byte site with unordered hash-table structure loses most of that
advantage.

Implement the new chain as a normalized, immutable, sorted unboxed vector of
signed sites, accompanied by its world domain. Its invariant is:

- occurrences are ordered primarily by `site-geometry` and secondarily by
  polarity;
- one geometry never has surviving occurrences of both polarities;
- repeated unmatched occurrences of the surviving polarity may remain
  repeated, preserving the existing multiplicity semantics;
- zero is represented by absence.

Use a separate transient **chain builder** for mutation. A builder appends
signed occurrences to a growable unboxed vector. Finishing it sorts by the
geometry-first key, cancels opposite occurrences run by run, and returns an
immutable normalized chain.

The first correct implementation may use Common Lisp `sort`; keep the
normalization boundary explicit so a packed radix sort can replace it later
without changing the model.

The main operations should follow from this representation:

- adding two normalized chains is a linear merge with cancellation;
- `chain-site-count` is an equal-range or binary-search query;
- mapping a chain is a sequential scan;
- `boundary-chain` emits the fixed fan-out of each occurrence into a builder
  and normalizes once;
- equality is domain equality plus vector equality.

Do not make an incrementally edited solid world dictate the mathematical chain
representation. If the file includes mutable solid occupancy, keep it as a
separate abstraction, preferably a compact or chunked bit representation from
which a solid chain or surface chain can be materialized. The chain itself is
the algebraic value.

`surface-chain(solid)` is the oriented two-chain obtained by taking the
boundary of the solid three-chain. It is already the drawable surface
**topologically**. Raster triangles are a later lowering, not the primary mesh.

---

## 3. Keep the representational levels distinct

The word “vertex” is dangerously overloaded. Distinguish at least these
levels:

- A **zero-site** is a canonical lattice vertex.
- A **realized point** is a geometric position obtained from a site and its
  occupancy star.
- A **render vertex** is an equivalence class of all vertex-stage attributes;
  a shared position may legitimately have multiple render vertices when
  normals or UVs are discontinuous.
- A **triangle corner** is an incidence of a render vertex in a raster
  triangle.

An index buffer exists precisely to represent the last incidence relation.
Never copy a complete vertex record once for every triangle corner when the
corners refer to the same render vertex.

For one LUFT face patch there are sixteen distinct local points, eighteen
triangles, and 54 triangle-corner incidences. The correct ordinary indexed
description is sixteen vertices plus 54 indices—not 54 copied vertices.

The durable LUFT renderer can be still more compact: it need not transmit the
sixteen positions at all. It transmits the oriented face site and the CPU's
compact geometric decision. The GPU realizes the sixteen points from those
facts.

---

## 4. Occupancy stars

Let cell occupancy be

\[
\chi(c)\in\{0,1\},
\]

where one means solid.

For a cubical site `s`, its **star** is the set of incident cells. An edge has
four incident cells and a vertex has eight. For each incident cell `c`, define
the direction from the site toward the cell centre, discarding the irrelevant
factor of one half:

\[
d(c)=\sum_{i=1}^{r}\epsilon_i b_i,
\qquad \epsilon_i\in\{-1,+1\}.
\]

The `b_i` are the world axes normal to the site:

- for an edge, `r=2` and the four directions are the diagonals of its normal
  plane;
- for a vertex, `r=3` and the eight directions are the cube diagonals.

Implement star enumeration generically from the site's anchor and extent.
Along every absent extent axis, an incident cell is anchored either one unit
below the site or directly at the site. Associate those choices with direction
components `-1` and `+1` respectively.

The classifier should consume occupancy through one small, explicit function
or callback. The non-wrapping Z boundary convention—missing cells as air,
clamping, or another extension—is logically separate from classification and
must be centralized there. Do not hide different vertical conventions in edge
and vertex helpers. If exact compatibility with the companion implementation
is required, preserve its convention and test it explicitly.

---

## 5. The strict-minority moment chamfer rule

Let

\[
k=\sum_{c\in\operatorname{star}(s)}\chi(c),
\qquad N=2^r.
\]

Choose the **strict minority**:

\[
M(s)=
\begin{cases}
\{c\mid\chi(c)=1\}, & k<N/2,\\
\{c\mid\chi(c)=0\}, & k>N/2,\\
\varnothing, & k=N/2.
\end{cases}
\]

Thus an edge star is convex at one solid cell, balanced at two, and concave at
three. A vertex star is convex-type at one through three solid cells, balanced
at four, and concave-type at five through seven.

Compute the raw minority moment:

\[
m(s)=\sum_{c\in M(s)}d(c).
\]

Reduce it componentwise to a signed direction:

\[
q_i(s)=\operatorname{sign}(m_i)
\in\{-1,0,+1\}.
\]

A balanced star has an empty strict minority and therefore `m=q=0`. It remains
sharp and unmoved. The raw moment retains agreement strength; `q` retains only
the directions that survive cancellation.

The construction is complement-symmetric. Replacing every occupancy bit by
its complement selects the same physical minority cells and therefore gives
the same displacement.

### Reach

Let `w`, with `0 < w < 1/2`, be the reserved chamfer width. Normally the reach
is one half:

\[
\rho(s)=\frac12.
\]

At a vertex, use the centroid reach precisely when the raw minority moment is a
unit cube diagonal:

\[
\rho(s)=\frac23
\quad\Longleftrightarrow\quad
|m_x|=|m_y|=|m_z|=1.
\]

Equivalently, `norm(q)^2 = 3` and `norm(m)^2 < 4`. Because `m` is integral,
this means exactly that all three moment components have magnitude one.

The final displacement is

\[
\boxed{\delta(s)=w\,\rho(s)\,q(s)}.
\]

The half reach is the midpoint of a two-axis 45-degree chamfer band. The
two-thirds reach is the centroid of the three truncation points `(w,w,0)`,
`(w,0,w)`, and `(0,w,w)`.

Do not simplify the two-thirds rule to “the minority contains exactly one
cell.” Every singleton minority has a unit-diagonal moment, but some
three-cell minority configurations do too. Changing the predicate would
change the geometry.

The whole rule is:

```text
complete site star
    → strict minority
    → minority moment
    → componentwise signed direction
    → half or centroid reach
```

It is site-local, complement-symmetric, and equivariant under permutations and
reflections of the world axes.

---

## 6. Realizing one face patch

For a canonical positive face, choose tangent axes `u,v` in increasing extent
order and let the canonical normal be `n = u × v`. This reproduces the cubical
orientation convention:

- YZ face: `u=Y`, `v=Z`, `n=+X`;
- XZ face: `u=X`, `v=Z`, `n=-Y`;
- XY face: `u=X`, `v=Y`, `n=+Z`.

The site's polarity reverses the oriented normal. It does not change the
geometric anchor or the canonical positive tangent basis. Consequently two
index templates, differing only in winding, can render positive and negative
faces while all position arithmetic remains canonical.

Let

\[
\lambda=(0,w,1-w,1).
\]

The flat local grid point is

\[
p_0(i,j)=a+\lambda_i u+\lambda_j v,
\qquad i,j\in\{0,1,2,3\}.
\]

Realize it as follows:

\[
p(i,j)=
\begin{cases}
p_0(i,j), & i,j\in\{1,2\},\\
p_0(i,j)+\delta(e), & \text{exactly one index is 0 or 3},\\
p_0(i,j)+\delta(v_s), & \text{both indices are 0 or 3}.
\end{cases}
\]

Here `e` is the corresponding canonical cubical edge site and `v_s` is the
corresponding canonical cubical vertex site. Derive those sites from the face
anchor and extent; do not define them by face-owned object identity.

One face therefore contains:

- four implicit interior points;
- eight edge-point occurrences sharing four edge classifications;
- four corner points using four vertex classifications.

The watertightness invariant is structural:

\[
s_1=s_2\Longrightarrow\delta(s_1)=\delta(s_2).
\]

The classifier depends only on the canonical edge or vertex site and its full
star, never on which face requested it. Every incident face must therefore
decode the same world-space coordinate without stitching.

Do not initially build a global cache graph of edge and vertex realizations.
Duplicating one four-byte shape word per face is cheap. First prove that a pure
site classifier gives identical codes and coordinates at every incidence. A
chunk-local dense cache may be introduced later if profiling justifies it.

---

## 7. One 32-bit shape word per face

The four edge classifications and four vertex results fit in one `u32`.

### Edge fields

An edge's complete boundary star can contain one, two, or three solid cells.
Encode that site-local class in two bits:

| Code | Meaning |
| --- | --- |
| `00` | balanced: two solid and two air; displacement zero |
| `01` | convex: one solid, the solid cell is the strict minority |
| `10` | concave: three solid, the air cell is the strict minority |
| `11` | reserved/invalid for a boundary edge |

For a particular face incidence, let `n` be the face's outward normal and let
`t` point outward across the local edge within the face plane. The known face
pair contains solid on the `-n` side and air on the `+n` side. Decode the edge
direction as:

```text
balanced → q = 0
convex   → q = -n - t
concave  → q = +n - t
```

Although the two-bit state omits an explicit world-space direction, the face
incidence supplies `n` and `t`; every incident face must reconstruct the same
canonical `q`.

Use this field order:

| Bits | Local edge |
| --- | --- |
| `1:0` | `u-low` (`i=0`) |
| `3:2` | `u-high` (`i=3`) |
| `5:4` | `v-low` (`j=0`) |
| `7:6` | `v-high` (`j=3`) |

### Corner fields

Pack the world-space vertex direction `q` and reach into six bits. Map each
component to a ternary digit:

```text
-1 → 0
 0 → 1
+1 → 2
```

Then

```text
direction-code = tx + 3*ty + 9*tz    ; 0 through 26
corner-code    = direction-code
                 | (two-thirds-p << 5)
```

Bit five is clear for half reach and set for two-thirds reach. Values 27
through 31 in the direction subfield are reserved.

Use this field order:

| Bits | Local corner |
| --- | --- |
| `13:8` | `(u-low, v-low)` |
| `19:14` | `(u-low, v-high)` |
| `25:20` | `(u-high, v-low)` |
| `31:26` | `(u-high, v-high)` |

Pack and unpack through named constants and small functions. Do not scatter
literal shifts through the implementation.

### CPU classification work

Do not reproduce the old pattern of six occupancy reads for every one of the
sixteen face points. The four interior points require no classification. The
four edge sites and four vertex sites should each be classified once.

Across the whole face there are only sixteen distinct neighbouring cells
outside the known solid/air face pair. A high-performance implementation can
form one compact face-star mask from those sixteen occupancies, then extract
the edge and corner subsets. Keep the generic site-star classifier as the
obviously correct reference, and test any mask-based classifier exhaustively
against it.

---

## 8. The 16-byte face-record ABI

One materialization owner stores a dense array of four `u32` words per exposed
face:

| Word | Meaning |
| --- | --- |
| 0 | low 32 bits of decorated oriented face site |
| 1 | high 32 bits of decorated oriented face site |
| 2 | shape word |
| 3 | zero for now; reserved for ABI revision or future data |

The underlying topological site occupies the low 60 bits. If the renderer uses
the spare high nibble for a stock/material slot, call the result a **decorated
site** and mask that nibble before applying topological site operations.

Define the wire record as four `u32`s even when the CPU conveniently manipulates
the site as one 64-bit integer. This avoids making the GPU ABI depend on native
64-bit shader integer operations.

The eventual renderer uses:

- `instance_index` to select one face record;
- indexed `vertex_index` in the range 0–15 to select one local grid point;
- face-site anchor, extent, polarity, and stock to derive basis, normal, and
  material;
- local point index and `w` to derive UV and flat position;
- the shape word to realize the edge or corner displacement.

Positive and negative faces may be partitioned into two instance batches and
drawn through two permanent winding templates. The permanent index data is
only `54 × u16 = 108` bytes per template. Alternatively, a later backend may
reflect a tangent basis and use one template, but that choice must preserve UV
and coordinate conventions explicitly.

An isolated cell then occupies six 16-byte face records—96 bytes—rather than
2,916 copied floats, or 11,664 bytes. The globally shared topology is not a
per-cell cost.

---

## 9. Canonical raster topology

Number the sixteen local points by

```text
local-index(i,j) = 4*i + j
```

or choose the transposed convention, but define it once and use it everywhere.
For each of the nine grid quads, name its corners in cyclic positive order:

```text
c00 = (i,   j)
c10 = (i+1, j)
c11 = (i+1, j+1)
c01 = (i,   j+1)
```

For the `c00–c11` diagonal, positive winding is:

```text
(c00, c10, c11)
(c00, c11, c01)
```

For the `c10–c01` diagonal, positive winding is:

```text
(c00, c10, c01)
(c10, c11, c01)
```

Negative winding swaps the final two indices of every triangle. If preserving
the old per-quad diagonal-selection predicate matters, express that predicate
over `(i,j)` while retaining these cyclic corner definitions.

Do not copy the old triangle emitter blindly. In the pasted implementation,
the integer labels assumed cyclic quad corners while the label-to-grid mapping
appeared to exchange `c11` and `c01`. That can create inconsistent winding or
even a bow-tie partition. The new templates must be generated from explicit
coordinates and tested.

---

## 10. A reconciliation issue in the old specialized edge helper

The generic strict-minority definition is authoritative. Check the old
specialized `edge-minority` helper rather than assuming it implements the same
sign convention.

For example, consider the low-X edge of the positive top face of one isolated
solid cell. Let:

- `n=+Z` be the outward face normal;
- `t=-X` point outward across that local face edge.

The edge star has one solid cell. The direction from the edge toward that
minority solid cell is

```text
-n - t = -Z + X.
```

The adjacent low-X face must reconstruct the same world-space direction from
its own normal and local tangent. This is the closure invariant.

The pasted scalar helper used a form equivalent to `side*n - t`; for the
both-air/one-solid case that gives `+n - t`, reversing the normal component.
That appears inconsistent with both the generic minority moment and the
adjacent-face invariant. Resolve this through exhaustive truth tables and
incidence tests. Do not preserve a specialized sign error merely to obtain
bitwise agreement with an old image.

The CPU reference implementation should therefore enumerate the complete star
and compute `m`, `q`, and reach literally. Optimized edge-state decoding is
accepted only after proving equivalence to that reference.

---

## 11. Required tests

Put a small, directly callable test entry point in the file. It need not depend
on an external test framework. Fail loudly with informative conditions.

At minimum test:

### Packed sites and topology

- make/decode round trips for anchors, extents, and polarities;
- X/Y wrapping and Z rejection rules;
- every boundary part has one lower dimension;
- opposite parent polarity reverses every boundary polarity;
- the shared face of two neighbouring cells annihilates;
- the boundary of the boundary of every representative site is zero;
- forward and backward cofaces reproduce the requested signed boundary.

### Chains

- builder normalization is independent of input order;
- opposite occurrences cancel and equal occurrences accumulate;
- normalized order is geometry-first;
- merging normalized chains agrees with normalizing their concatenation;
- `chain-site-count`, mapping, and equality respect multiplicity.

### Strict-minority classification

- enumerate all 16 edge occupancy patterns and all 256 vertex patterns;
- balanced patterns give `m=q=0`;
- complementing occupancy leaves displacement unchanged;
- axis permutations and reflections transform `m` and `q` equivariantly;
- singleton minorities and their complements give a unit-diagonal moment and
  two-thirds vertex reach;
- every three-cell minority whose moment is unit-diagonal also receives
  two-thirds reach;
- no other vertex pattern receives two-thirds reach.

### Face records and closure

- shape-word pack/unpack round trips and rejects reserved codes;
- generic per-site classification agrees with any face-mask fast path;
- every incidence of one canonical edge decodes the same `q` and displacement;
- every incidence of one canonical vertex decodes the same corner code and
  displacement;
- adjacent realized face patches have bit-identical shared coordinates under
  the chosen floating-point evaluation order;
- positive and negative index templates contain 54 indices in `0..15`;
- all triangles in each template have consistent expected winding;
- the two triangles of every quad cover that quad without crossing or gaps.

If the old scalar mesher is available, add a differential test, but classify
disagreements: a mismatch caused by the suspected edge-sign or old corner-map
bug is evidence to investigate, not an automatic reason to change the new
mathematical implementation.

---

## 12. Performance shape, without premature cleverness

Correctness comes from the generic site functions and exhaustive small truth
tables. Performance comes from arranging the same work into dense batches:

- normalized chains are sequential arrays, not pointer-rich ledgers;
- boundary construction is emit, sort, and reduce;
- face records are a contiguous four-word array;
- classification loads each necessary occupancy once;
- faces can be partitioned across CPU threads without shared writes;
- after classification, realizing four grid points at a time is a regular SIMD
  kernel;
- dense occupancy can later use word-parallel shifted bitplanes to classify
  many neighbouring cells simultaneously.

Do not obscure the reference rules with SIMD intrinsics in this first file.
Expose pure, small kernels and unboxed layouts so vectorized replacements can
be proved against them later.

---

## 13. Definition of done

The new single Lisp file is a satisfactory foundation when:

- it loads cleanly in the intended Common Lisp implementation;
- the site ABI and domain rules are explicit;
- chains contain no hash tables and normalize deterministically;
- canonical incidence and `boundary²=0` are executable facts;
- the strict-minority moment classifier is written directly from the complete
  star definition;
- one oriented face plus one shape word is sufficient to reproduce its sixteen
  reference positions;
- the shape word and 16-byte face-record layouts are explicit and tested;
- the index templates are valid, consistently wound triangulations;
- edge and vertex incidence tests establish watertight realization;
- no expanded per-triangle vertex array is part of the core representation;
- comments explain why each representation exists, not merely what each line
  does.

The intended conceptual pipeline is:

```text
editable occupancy or solid input
    → normalized solid 3-chain
    → canonical oriented surface 2-chain
    → site-local edge and vertex star classification
    → one compact shape word per face
    → 16-byte dense face records
    → indexed instanced patch realization
    → raster triangles
```

Each stage adds only information belonging at that stage. The chain owns
topology. The CPU authors the discrete geometric meaning. Indices preserve
raster incidence. The GPU eventually performs only regular realization,
transformation, and interpolation.

That is the new basis: not a mesh made from copied coordinates, but a compact
lowering of canonical cubical incidence into geometry.
