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
  beside the class (`luvcraft/blocks.lisp`). A name slot is for printing,
  never for dispatch.
- **Parallel CASE tables that grow together.** Adding a shader operator once
  meant editing the parser's cond, the type-inference case, the lowering
  case, the provenance case, and the shader lab's label case. That shotgun
  surgery is the strongest signal: each table became a generic function and
  an operator is now one cohesive method cluster (`shader-expression.lisp`).
- **String-matched symbol vocabularies.** Operators were matched by
  STRING-EQUAL on symbol names — identity-free. Names should be symbols
  compared by EQ, with the package system as the namespace.

## The patterns

**Protocol pairs.** When an object crosses an ownership boundary (worker
thread → render thread), give it a generic on each side:
`perform-production-request` computes, `publish-production-result`
validates and installs. New product kinds plug in without touching the
transport loop.

**Default methods encode the safe fallback.** A source without residency
methods keeps caller-owned chunks; an unknown load request returns NIL and
the chunk simply stays desired. Design the `(specializer T)` method first —
it states what the protocol means when nobody customizes it.

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

**Thin definer macros.** `define-shader-operator`, `define-shader-method`
are definition sites (M-. finds them, re-evaluation replaces cleanly) that
expand to plain defmethods and documentation. The macro is ergonomics; the
protocol is the generics. Never hide a registry inside a macro that methods
couldn't also reach.

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
- Where a per-site object *description* is genuinely useful (inspectors,
  sparse edits, ray hits), name it conspicuously —
  `describe-block-allocatingly` — so its appearance in a dense loop looks
  suspicious in review. Domain-shaped code borrows the dense storage once
  via `with-block-content-storage` instead.
- Per-call dispatch cost is a boundary question, not a prohibition: a
  generic replacing an equal-cost ETYPECASE in the same spot (as
  `sample-block-at` did) changes nothing; introducing dispatch *per element
  of a dense loop* where none existed deserves a second look.

## Proving a dispatch refactor

Behavior-preserving conversions here are verified, not asserted: run the
full suite and compare the deterministic smoke render byte-for-byte —

```sh
make test
./build/luvcraft --smoke-test build/luvcraft-smoke.png && md5 -q build/luvcraft-smoke.png
```

Four successive CLOS refactors (mesher sampling, production publication,
world-source protocol, shader operators) each kept the render hash
identical. That is the standard to hold new ones to.
