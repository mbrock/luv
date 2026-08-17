---
name: luv-systems-design
description: Use when designing, refactoring, reviewing, or optimizing performance-sensitive luv or luvcraft systems, especially work involving allocation and extent, dense iteration, columnar or field materializations, SIMD, voxel domains/chunks/windows/frontiers, dimensional quantities, arithmetic compilation, derived products, or transactional publication. Provides the project's broad systems-design compass and a source-led workflow. Pair with clos-design for object and generic-function choices, luv-development for live execution, and wiki-work when editing design memory.
---

# Designing systems in luv

> **Five seconds of silence means broken.** Any command here that runs more
> than about five seconds without printing anything is a defect to be named,
> not a wait to be endured — usually a pipe that ate the progress output or a
> process parked on stdin. Make it talk, `strace` it, or split it up; never
> poll it or run it again the same way. See `luv-development` and AGENTS.md.

Use one recurring shape: put semantic structure around dense, explicit work.
Let objects and protocols name meanings, owners, domains, policies, and
lifecycle. Let specialized arrays, packed identifiers, and closed loops move
the bulk data.

The short form is:

> Bind meaning once; run raw lanes many times.

Treat this skill as a compass, not a snapshot of the repository. Do not infer
current APIs, layouts, or performance from examples remembered here. Use the
wiki tooling, live image, callers, tests, and profiler to discover the current
system before proposing or editing it.

## Begin from current evidence

1. Read the repository instructions and inspect worktree state. Preserve
   unrelated experiments.
2. Run `scripts/wiki marks` and `scripts/wiki toc`. Search the table of
   contents for the task's concepts before reading pages wholesale.
3. Use `scripts/wiki figure`, `mentions`, and `defs` to move between claims,
   backlinks, and definitions. Use `rg` to find actual storage, loops, callers,
   tests, and measurements.
4. Use `./sly` to inspect or exercise the live system when runtime identity,
   compilation, allocation, or redefinition matters. Read `luv-development`
   before managing images, builds, tests, or a live luvcraft process.
5. Read `clos-design` as well when choosing objects, classes, instances,
   symbols, generic functions, or dispatch grain. Read `wiki-work` before
   editing the wiki. Do not duplicate either skill into this one.

Keep three voices separate throughout the work:

- *Verified now*: source, live behavior, measurements, and tests observed in
  this checkout.
- *Design consequence*: a constraint forced by those observations.
- *Proposal*: a promising representation or protocol still awaiting evidence.

If a name, benchmark, compiler property, or backend capability can drift,
verify it cheaply rather than turning yesterday's note into today's fact.

## Draw two maps before choosing abstractions

Draw a *semantic map*:

- What has durable identity?
- What is merely a value at a site or row?
- Which domain makes an index meaningful?
- Who owns storage, revisions, candidates, and publication?
- Which distinctions must survive equal representations?
- What does missing, unavailable, unknown, or stale mean?

Draw a *cost map*:

- What executes once per subsystem, frame, chunk, row, site, neighbor, vertex,
  or pixel?
- What allocates, escapes, dispatches, checks bounds, hashes, converts units,
  crosses a chunk, or touches a cache line at each grain?
- Which data is retained, borrowed, stack-temporary, or GPU-owned?
- Which operation dominates the actual requested path?

Do not accept a beautiful semantic decomposition with an implicit per-element
object graph. Do not accept a fast array loop whose rows have lost their
meaning, provenance, or owner. The design must survive both maps.

## Put identity at boundaries, not at every address

Do not give something object identity merely because it is addressable. A
cell, light level, vertex, glyph instance, or pixel usually belongs to a dense
domain and has no independent lifecycle. A world, domain, materialization,
policy, source, candidate, or device resource often does.

Use semantic objects to select and bind dense representations at coarse
boundaries. Dispatch once per operation, batch, product, chunk, or pass when
that is the natural extensibility seam. Enter a closed loop after the choice.

Keep independently meaningful fields independent even when they share type,
shape, or storage strategy. Equal dimensions do not make two quantities
interchangeable; equal array lengths do not make two fields one field; equal
coordinates do not make two spaces the same domain.

Use clean semantic API names. Put allocation and performance contracts in
documentation and tests rather than encoding warnings into awkward names.
Make dense-loop violations conspicuous through ownership and call shape.

## Treat allocation as a lifetime question

Ask where a value lives and whether it escapes before deciding whether its
surface syntax is acceptable.

- Retain objects that genuinely cross calls, frames, queues, tables, or owner
  boundaries.
- Keep loop-local coordinates, steps, views, and small products at dynamic
  extent when they cannot escape.
- Declare dynamic extent only when the consumer body cannot retain the value.
  A queue insertion, closure capture, hash key, return value, or stored callback
  invalidates that promise unless the value is explicitly copied or packed.
- Keep constructors and iteration visible enough for the current compiler to
  honor stack allocation. Verify with allocation measurements and, when
  important, compiler output; a declaration is a promise, not proof.
- Reuse packed frontier and scratch storage. Clear retained object lanes when
  logical contents are removed so reusable capacity does not become accidental
  retention.
- Avoid allocating a neighbor merely so the caller can reject it. Bind a
  dynamic-extent destination around the consumer, or pass primitive offsets
  through a compiler-visible iteration form.

Do not make “no consing” a superstition. Cold setup, retained semantic state,
and clear control-plane objects may allocate naturally. Remove allocation from
the multiplication factors: sites times neighbors times frames.

## Make iteration own its grain and extent

Prefer iteration forms that state the domain, lifetime, and crossing behavior
of the loop.

- Validate public inputs at the boundary, then enter a trusted scope which can
  use primitive offsets without repeating the same type and bounds checks for
  every neighbor.
- Keep arbitrary callers checked. Optimize by narrowing a proved scope, not by
  globally weakening invariants.
- Separate an interior step from a boundary crossing. The common case should
  stay in local arithmetic; only crossings consult an environment or window.
- Use functions for ordinary call boundaries. Use macros when the body must be
  compiler-visible to preserve dynamic extent, eliminate callback allocation,
  bind storage once, or specialize a measured hot loop.
- Keep essential control flow readable in the expansion. A macro should expose
  a loop's contract, not create an invisible framework.
- Count visits, crossings, stale pops, strict improvements, invalidations, and
  publications. Time alone cannot say whether representation or scheduling
  changed the amount of work.

Treat adjacency as a relation supplied by a domain and neighborhood policy,
not as an instruction to enqueue. Admission, priority, deduplication, and
coalescing belong to the maintenance rule that understands the field.

## Design columnar materializations, not loose parallel arrays

A useful materialization binds four things:

1. a finite or explicitly managed domain;
2. a schema of semantic fields;
3. physical storage for each field; and
4. provenance, revision, validity, and ownership.

Columnar storage is not merely an optimization. It lets one operation borrow
only the lanes it needs, gives each lane a homogeneous representation, and
makes batch execution possible without erasing row meaning.

Bind field meaning, representation policy, and domain agreement once. Run
inner loops over specialized storage. Keep variable-length side data in owned
arenas or tables whose replacement and compaction policy is explicit; do not
smuggle indefinite growth behind fixed columns.

Distinguish logical field identity from physical encoding. The same field may
be constant, packed, scalar, SIMD-expanded, or GPU-resident at different
boundaries. Different fields may deliberately use identical bytes without
becoming interchangeable.

Publish derived products coherently. Build or mutate a candidate against
stamped source materializations, reject stale candidates, and install a
complete revision at the owner boundary. Do not expose a half-solved light
field, half-rebuilt mesh cohort, or half-updated terminal row merely because
the underlying arrays are mutable.

## Make SIMD the consequence of the data model

SIMD begins with domains, storage, and evaluation order, not with intrinsic
names.

- Choose a batch whose lanes perform the same operation on independent rows.
- Arrange homogeneous columns, alignment, tails, masks, and scratch ownership
  before expecting vectorization to help.
- Keep semantic quantity and field specifications outside the lanes while
  preserving their contract at the kernel boundary.
- Maintain a scalar reference kernel with the same evaluation order when
  determinism or differential testing matters.
- Select a supported SIMD implementation outside the hot loop. Do not query
  capabilities or dispatch per row.
- Measure generated code and the real workload. A vector type in source is not
  evidence of vector instructions or useful speedup.

Do not contort irregular control work into SIMD. Frontier scheduling, sparse
publication, and chunk crossings may feed dense kernels without themselves
becoming four-wide arithmetic.

## Keep voxel locality and world truth distinct

A chunk domain owns local indexing and traversal. It does not decide what lies
beyond its boundary. Continue a crossing through an explicit window or
environment that can distinguish resident data, open space, closed space,
unknown terrain, and unavailable products.

Do not silently equate a missing neighbor with air, zero light, empty support,
or a valid sample. Let each subsystem interpret availability according to its
own semantics.

Keep these layers distinct:

- discrete addresses and primitive neighbor steps;
- physical positions, displacements, areas, and volumes;
- field values over sites, faces, edges, or other domains;
- chunk residency and boundary availability;
- derived-product revisions and publication.

For frontier work, identify the dynamic family before extracting machinery:

- *Discover once*: connected regions, membership, reachability.
- *Relax monotonically*: best-known light, distance, influence, or signal.
- *Invalidate and rebuild*: removal, changed dependencies, split components,
  residency departure.

Share packed storage, iteration, and lifecycle where evidence supports it.
Keep transfer laws, admission rules, removal semantics, and stopping
conditions client-owned. Wait for multiple concrete clients before turning a
recurring shape into a universal solver.

## Preserve quantity meaning through dense execution

Treat dimension as one law, not the whole meaning of a number. Distinguish
quantity kind, dimension, unit, affine character, space, and representation
where the domain requires them. In particular, do not collapse points,
absolute values, and differences merely because their components match.

Keep declarations and numeric storage parallel:

- Bind semantic quantity specifications at owned API, field, bundle, shader,
  or kernel boundaries.
- Make unit conversion explicit. Do not let a convenient scalar silently
  change its interpretation.
- Check representation and arithmetic compatibility when assembling a
  materialization or compiling an expression, not once per scalar operation.
- Let dense lanes remain ordinary supported numeric representations after the
  contract is bound.

Use the arithmetic language as a semantic compiler boundary: parse and check
meaning, retain provenance, then lower a shared computation to the required
CPU or GPU realization. Do not turn a simple per-site law into a retained
expression graph or per-operation dispatch merely to advertise abstraction.
Conversely, do not duplicate a meaningful calculation across backends when a
checked shared specification can generate both.

Inspect the current arithmetic and shader protocols before extending them.
They are live, evolving vocabularies; remembered operator names and type forms
are not a stable substitute for source reading.

## Make ownership and publication visible

Name the owner of every retained buffer, candidate, queue, side table, worker
result, and device artifact. State:

- who creates it;
- which inputs and revisions justify it;
- who may mutate it;
- when readers may observe it;
- how stale work is rejected;
- when storage can be reused or released.

Separate authored state from derived state. Derived products can be rebuilt;
their revisions and publication rules must still be explicit. If work crosses
threads or GPU submission, distinguish logical invalidation from physical
reclamation and use the actual completion boundary owned by that subsystem.

For live definitions, record and coalesce invalidation cheaply. Rebuild outside
definition mutation and resource locks, then install the complete candidate at
an owner-controlled boundary. Preserve the last-known-good artifact when a
new definition fails.

## Demand evidence at the claimed boundary

Match proof to the claim:

- Use exact unit and differential tests for indexing, field equality,
  quantities, revisions, and scalar/SIMD agreement.
- Measure allocations after warm-up on the actual loop whose extent matters.
- Profile the requested path, not an IDE proxy or an adjacent benchmark.
- Compare visits and work distribution when changing frontier order.
- Exercise cold images when stale live definitions could hide missing source.
- Verify live redefinition in the live owner when that is the promised
  benefit.
- Judge visual work in the actual renderer across motion, distance, and angle;
  a deterministic image can prove stability but not beauty.

Keep a simple reference path when optimizing a subtle derived field. The fast
path should be able to prove agreement with something easier to understand.

## Reject these recurring false economies

- An object per cell, vertex, contact, frontier item, or glyph merely for
  conceptual neatness.
- Generic dispatch, hash lookup, coordinate construction, or unit conversion
  inside a dense per-element loop when the choice can bind outside it.
- Loose parallel arrays with no shared domain, schema, revision, or owner.
- One universal manager, graph, property bag, or solver erasing
  relation-specific meaning.
- A convenient world-level lookup used repeatedly after the operation already
  knows its chunk and dense offsets.
- An array-oriented design that promises to “add SIMD later” while choosing a
  layout hostile to homogeneous batches.
- Treating unloaded, unknown, absent, air, zero, and stale as synonyms.
- Calling an abstraction allocation-free without measuring retention and
  escape on the real path.
- Calling a benchmark successful after it reduced bytes but multiplied visits,
  or reduced visits while moving work into publication or conversion.

These are review alarms, not commandments. A sparse inspector, editor action,
or cold compiler phase may reasonably use objects, hashes, and allocation.
State the grain and evidence that make the exception honest.

## Leave an inspectable design record

For a substantial design or review, report at least:

- semantic owners and identities;
- domains, fields, and physical representations;
- retained versus borrowed versus dynamic-extent values;
- iteration and dispatch grain;
- boundary, missing-data, and stale-input semantics;
- candidate and publication lifecycle;
- quantity and arithmetic contracts;
- expected allocation and memory-access shape; and
- the smallest evidence gate that could falsify the proposal.

When the understanding is durable, use `wiki-work` to add or revise small,
addressable figures. Cite those figures from code only where the implementation
embodies the decision. Let future agents follow the links back into current
source rather than treating this skill as the final authority.
