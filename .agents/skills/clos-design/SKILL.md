---
name: clos-design
description: Use when designing or refactoring Lisp code in this repo — a new subsystem, protocol, or vocabulary, or any dispatch written as CASE, TYPECASE, ECASE, or COND+TYPEP that might want to become generic functions. Captures how luv designs with CLOS — open protocols, EQL methods on symbols, semantic objects at boundaries with dense data inside — and, just as important, when NOT to make something an object.
---

# Designing with CLOS in luv

CLOS is this project's primary design instrument, not an implementation
detail. The aim is always the same: the *meaningful* entities of a domain
become inspectable, redefinable, documentable objects and protocols, while
bulk data stays as dense specialized arrays. "Object oriented" undersells
what CLOS gives us — multiple dispatch, EQL specializers, late-bound names,
method redefinition in a live image — so design with those, not with
message-passing habits from other languages.

## Smells that call for generic dispatch

Each of these appeared in this codebase and was converted; the referenced
commits/files are the worked examples.

- **ETYPECASE over classes you defined.** The mesher dispatched over three
  sample sources by etypecase; now `sample-block-at` is a generic and a new
  source is one method (`luvcraft/mesher.lisp`).
- **COND + TYPEP chains.** The chunk-production drain loop type-tested each
  request class; now `publish-production-result` mirrors
  `perform-production-request`, so a request class carries both its worker
  side and its owner side (`luvcraft/streaming.lisp`, `luvcraft/production.lisp`).
- **TYPEP as a policy switch.** Residency strategy checked
  `(typep source 'little-world-source)`; now `maintain-block-world-residency`
  and `make-block-chunk-load-request` dispatch on the world source, and the
  default methods encode the safe degraded behavior (static residency,
  "cannot load off-thread" → NIL).
- **CASE on the name slot of an instance.** `block-face-local-uv` cased on
  `(block-face-name face)` — dispatching on a *label* of an object instead of
  the object's own data. The projection now derives from the face's normal,
  beside the class (`luvcraft/blocks.lisp`). Do not dispatch indirectly on an
  object's label when its class or domain data already expresses the choice.
- **Parallel CASE tables that grow together.** Adding a shader operator once
  meant editing the parser's cond, the type-inference case, the lowering
  case, the provenance case, and the shader lab's label case. That shotgun
  surgery is the strongest signal: each table became a generic function and
  an operator is now one cohesive method cluster (`hal/shader/language.lisp`).
- **String-matched symbol vocabularies.** Operators were matched by
  STRING-EQUAL on symbol names — identity-free. Names should be symbols
  compared by EQ, with the package system as the namespace.

## The patterns

**Protocol pairs.** When an object crosses an ownership boundary (worker
thread → render thread), give it a generic on each side:
`perform-production-request` computes, `publish-production-result`
validates and installs. New product kinds plug in without touching the
transport loop.

**Default methods encode a genuinely safe fallback.** A source without
residency methods keeps caller-owned chunks; an unknown load request returns
NIL and the chunk simply stays desired. Define a `(specializer T)` method only
when that behavior is semantically valid for every uncustomized participant.
If the operation is unsupported or a missing extension is a bug, leave the
generic without a default or signal an explicit condition; silent degradation
would erase useful evidence.

**Choose the representation deliberately.** The same small vocabulary can
plausibly be represented four ways; choose according to what gives it meaning:

- Use `CASE` for a closed, usually externally owned enumeration.
- Use EQL-specialized symbols when the interned name is the durable identity
  and several independent protocols contribute behavior for that name.
- Use instances when entries carry runtime data, configuration, or identity
  that users should inspect and change.
- Use classes when entries form behavioral families and inheritance is part of
  the domain model.

**Dispatch on the relationship.** If an operation depends symmetrically on
several semantic participants, specialize all of them instead of making one
participant interrogate the others. `block-face-tile` belongs to both a block
kind and a face; `invoke` belongs to both an invoker and a reified invocation.
This is the ordinary CLOS alternative to visitors, double-dispatch scaffolding,
and policy switches hidden inside one privileged object.

**EQL methods on symbols — the compiler pattern.** The shader language's
operators are ordinary symbols (`cl:+` where CL has the word, `spv:dot`
where it doesn't), and parsing/typing/lowering are EQL-specialized generics.
This is how SBCL itself keys compiler knowledge off standard names it never
funcalls. Two rules make it work:

- *Data stores the symbol; dispatch resolves late.* A `shader-call` holds
  `dot`, not a behavior object, so redefining the operator's methods reaches
  every existing specification on its next compile. Same reason CL forms
  name functions instead of capturing them.
- *A generic keyed by symbol is a distributed slot.* Per-operator metadata
  (`shader-operator-result-name`, `binary-arithmetic-instruction`) doesn't
  need a metaobject with slots — a small generic with EQL methods and a
  sensible default carries it.

EQL methods also work on integers when the domain is numbered:
`paint-block-atlas-tile` gives each atlas tile its own hot-replaceable
method (`luvcraft/blocks.lisp`).

**Reuse CL's own symbols when the semantics match.** The shader DSL treats
itself as a compiled subset of CL: `+ - * / let*` are the standard symbols.
Discoverability comes free, and `documentation` with a custom doc-type
(`(documentation 'cl:+ 'shader-operator)`) attaches domain meaning without
touching the standard documentation. Only intern new names for words CL
lacks, and export them from one designated package.

**Vocabulary as instances.** Semantic kinds — `block-kind`, `block-face` —
are CLOS instances carrying their data (face tiles, corners, normals).
Behavior reads the object's data or dispatches on its class; it never cases
on which instance it is.

**Method combination for orthogonal policy.** `:before`, `:after`, and
`:around` methods are useful when a concern composes independently with the
primary operation: Cocoa host activation surrounds the shared canvas protocol,
and `tracing-invoker` records any `invoke` implementation. Keep the essential
control sequence in primary methods. An `:around` method either calls
`call-next-method` or makes its deliberate replacement semantics obvious;
auxiliary methods should not turn ordinary lifecycle code into archaeology.

**Metaclasses when definitions are domain objects.** Use the MOP when the
class definition itself carries meaning, not merely to manufacture a hidden
registry. `invocation-class` stores an entry point's argument specification on
its class metaobject; `instruction-class` stores SPIR-V opcode, operand, and
result conventions. Keep the resulting instances and generic protocols
ordinary, and keep the metaclass surface as narrow as the definition metadata
requires.

**Thin definer macros.** `define-shader-operator`, `define-shader-method`
are definition sites (M-. finds them, re-evaluation replaces cleanly) that
expand to plain defmethods and documentation. The macro is ergonomics; the
protocol is the generics. Never hide a registry inside a macro that methods
couldn't also reach.

**Live definitions are runtime state.** Preserve ordinary `DEFMETHOD` identity
so re-evaluating the same qualifier and specializer coordinate replaces the
existing definition. When runtime artifacts depend on methods, MOP callbacks
should only record and coalesce a revision; never compile shaders, touch the
GPU, or call back into the mutating generic function while its definition is
being changed. Rebuild outside the generic-function lock, install the complete
candidate transactionally at the owning frame boundary, retain the
last-known-good artifact on failure, and explicitly unsubscribe dependents
when their owner is released (`hal/shader/language.lisp`,
`examples/block-world.lisp`).

## When CASE is right — do not CLOSify these

- **ABI and enum translation tables**: Vulkan usage flags, SDL scancodes and
  button numbers, CLIM modifier constants. Closed vocabularies owned by an
  external spec; the case *is* the table, and methods would scatter it.
- **Protocol message loops**: the production worker's `(ecase message
  (:stop ...) (:work ...))` mailbox protocol.
- **Condition report formatting** over reason keywords.
- **Coercion typecases** normalizing caller convenience (scalar-or-list
  extents, keyword-or-list usages).
- **Small closed geometric enums**: the player controller's `(ecase axis
  (:x ...) (:y ...) (:z ...))`. Axis objects would be ceremony.
- **Configuration keywords at API boundaries**: `:vertex`, `:fragment`,
  `:block-surface` roles. These are settings, not extensible vocabularies —
  though a role keyword *is* a fine EQL-dispatch target
  (`shader-specification-for`).

The dividing question: **who grows this set, and how?** If growth means
"a user or future subsystem adds a member with its own behavior," use
generics. If growth means "the external spec added an enum value," extend
the table.

## Objects at the boundary, dense data inside

The counterweight to all of the above, and the reason CLOSification never
means "every micro entity becomes a heap-allocated instance":

- CLOS owns the *meaningful* objects — world and source, chunk and domain,
  block-kind, mesher and policy, operator vocabulary. Generic dispatch
  selects a representation **at those boundaries**, ideally once per batch,
  frame, or chunk.
- Inside the boundary, bulk data is dense and specialized: palette-indexed
  `(unsigned-byte 16)` columns for chunk content, interleaved single-float
  vertex arrays, packed `(unsigned-byte 32)` atlas pixels. Cells, vertices,
  and pixels do not gain object identity merely because they are
  addressable.
- Where single-site world access is genuinely useful (inspectors, sparse
  edits, ray hits), use the explicitly world-level `world-block-at`; its
  documentation advertises the coordinate and lookup allocation, and its
  appearance in a dense loop should look suspicious in review. Domain-shaped
  code borrows the dense storage once via `with-block-content-storage` instead.
- Per-call dispatch cost is a boundary question, not a prohibition: a
  generic replacing an ETYPECASE in the same spot (as `sample-block-at` did)
  does not move the decision into a hotter-grained loop; introducing dispatch
  *per element of a dense loop* where none existed deserves a second look and
  a measurement.

## Proving a dispatch refactor

Behavior-preserving conversions here are verified, not asserted: run the
full suite and compare the deterministic smoke render byte-for-byte —

```sh
make test
make smoke
md5 -q build/luvcraft-smoke.png
```

Four successive CLOS refactors (mesher sampling, production publication,
world-source protocol, shader operators) each kept the render hash
identical. That is the standard to hold new ones to. When the claimed benefit
is live redefinition, a cold test process is not sufficient: use `./sly` to
re-evaluate the same definition coordinate and verify that the running owner
observes the replacement. For transactional definitions, also verify that an
invalid edit preserves the last-known-good artifact and that a subsequent
valid edit recovers automatically.
