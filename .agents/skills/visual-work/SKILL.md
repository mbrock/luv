---
name: visual-work
description: "Use when implementing, changing, reviewing, or claiming success for anything judged visually: rendered geometry and meshes, shaders and materials, animation, game presentation, screenshots or films, and user interfaces. Covers code-defined captures, close inspection, adversarial visual evaluation, geometric test oracles, responsive UI quality, A/B evidence, video handoff, and human signoff."
---

# Visual work

Treat the visible artifact as primary evidence. Automated checks can prove
important structural properties, but they cannot promote an ugly, incoherent,
or misleading result into a successful visual result.

## Make the actual thing easy to see

Establish a fast, repeatable capture path before trusting an iteration.

- Render the production implementation, data, shaders, layout, and interaction
  path. Do not substitute a mock or an adjacent test scene for the claimed
  result.
- Define the scene or UI state, camera, projection, time, viewport, resolution,
  scale, and relevant toggles in code. Prefer a named capture recipe that can be
  rerun without reconstructing manual state.
- Frame the specific feature deliberately. Supply both a contextual view and a
  closeup from an angle that exposes its shape. Make the target large enough to
  judge at original resolution.
- Render high enough to crop natively. Do not enlarge a tiny screenshot and
  mistake interpolation for detail. Crop intentionally and retain the source
  frame when it helps establish context.
- Inspect the artifact itself after writing it. A successful render command is
  not evidence about what the render contains.
- Exercise overlays both on and off when construction lines, diagnostics,
  selection, or guides could either reveal or obscure the underlying result.
- Use consistent cameras, state, exposure, and resolution for before/after or
  A/B comparisons. Change one relevant variable at a time.

If making a useful closeup is slow or improvised, improve the capture surface
first. Visual iteration depends on being able to ask the renderer a precise
question quickly.

For motion, interaction, transitions, temporal artifacts, or responsive
behavior, make a short representative video. Attach the original artifact or
place it in the project's intended web root and provide the exact link. A still
frame cannot establish how something moves or responds.

## Keep visual claims epistemically honest

Assume model vision is fallible. It is especially vulnerable to low
resolution, ambiguous perspective, confirmation bias from implementation
effort, and the false authority of a large green test count.

- Describe observations before explaining them. Name pinches, gaps, spikes,
  aliasing, clipping, crowding, jumps, and broken hierarchy directly.
- Do not rationalize an obvious artifact as an intentional diagnostic or an
  acceptable approximation merely because the implementation is elegant.
- Separate “the program rendered,” “the output is internally consistent,” and
  “the output is visually successful.” They are different claims with
  different evidence.
- Treat human signoff as the meaningful success gate for visual work unless the
  user explicitly waives it. Before signoff, say that the result is ready for
  review or awaiting approval; do not say that it looks correct or mark it
  complete.
- Report exactly which artifact was inspected, at what resolution and state.
  Preserve uncertainty when the framing or image quality limits judgment.

Tests and agents can reduce the human review burden. They do not replace the
human whose taste, intent, and perception define success.

## Iterate adversarially

Ask what could make the result look wrong, not only what would justify the
current implementation.

1. State the visual claim and list concrete failure modes before implementing
   its tests.
2. Build an isolated fixture that exposes the feature, plus a realistic scene
   that reveals interactions with surrounding content.
3. Inspect silhouettes, closeups, alternate angles, stressful content, and
   motion. Look especially where topology, compositing, or layout decisions
   meet.
4. Compare alternatives as A/B artifacts under identical conditions. When
   practical, hide implementation labels and randomize presentation order.
5. Turn observed failures into tests that would have rejected the bad artifact,
   not tests that merely restate the chosen algorithm.
6. Present the strongest artifact to the user for signoff.

When independent agents are available and the task warrants it, use them as
fallible adversarial reviewers:

- Give a contextless reviewer the raw artifact and a neutral question such as
  “What looks geometrically wrong?” or “What would make this UI hard to use?”
- Give another reviewer a blind A/B choice.
- Ask a specialist pass about geometry, compositing, typography, or interaction
  without leaking the suspected bug or desired answer.
- Prefer several narrow independent judgments to one richly briefed request.

Subagent agreement is useful evidence, not human signoff. Disagreement is a
reason to improve the artifact, framing, or question rather than average the
answers into confidence.

## Give geometric tests an independent oracle

An exhaustive test suite can exhaustively verify the wrong specification.
Count checks only after asking whether their predicates express the visible
claim.

For meshes, curves, bevels, fields, and procedural geometry, consider tests for:

- agreement with an analytic surface, trusted reference implementation, or
  deliberately authored reference fixture;
- silhouette and cross-section shape, constant width, radius or offset error,
  angular order, and monotonicity;
- the intended crease graph and bounded dihedral changes rather than merely
  nondegenerate outward triangles;
- minimum triangle angle, aspect ratio, edge-length ratios, slivers, pinches,
  self-intersection, foldovers, and unexpected high-valence fans;
- manifoldness, winding, shared-boundary agreement, watertightness, and
  complement or symmetry invariants;
- behavior at representative scale, extreme parameters, adversarial
  configurations such as acute, obtuse, reflex, near-collinear, and multi-way
  joins, multiple cameras, and motion;
- temporal continuity and agreement between the visible geometry and related
  state such as collision, selection, or interaction;
- the compiled GPU or production realization, not only a hand-written CPU
  transcription of the same formulas.

Use golden images to retain an already approved result, not to establish that
the first result is good. A deterministic picture proves stability relative to
its baseline; the baseline still needs meaningful approval.

Prefer tests that falsify plausible disasters. After any embarrassing visual
failure, retain a small regression whose failure condition describes what was
visibly wrong.

## Review interfaces as composed visual systems

Judge a UI in its real container and interaction model, not as isolated
widgets. Check at least:

- coherent hierarchy, alignment, spacing, rhythm, and information density;
- responsive behavior across narrow, wide, short, and high-density viewports;
- text wrapping, truncation, overflow, clipping, scrolling, and long or empty
  content;
- typography, contrast, icon legibility, focus, hover, pressed, disabled,
  loading, error, and empty states;
- aliasing, sampling, pixel alignment, alpha blending, compositing order,
  transparency, and color-space assumptions;
- input reachability, focus movement and restoration, pointer targets, feedback
  timing, live resize or reflow, and motion continuity;
- legibility and compositing against bright, dark, and visually busy underlying
  content;
- avoidance of extraneous widgets, labels, frames, decoration, and duplicated
  explanations.

Test real content lengths and awkward states. A spacious happy-path mock often
hides the wrapping, density, and interaction failures users actually see.

## Report evidence without laundering it

At handoff, distinguish:

- structural evidence: builds, invariants, unit tests, geometry metrics;
- production-path evidence: exact capture recipe, runtime/backend, screenshot or
  video artifact;
- adversarial evidence: alternate views, A/B results, independent critiques;
- perceptual status: human approved, rejected, or awaiting signoff.

Never use the quantity of structural evidence as a substitute for the missing
perceptual gate.
