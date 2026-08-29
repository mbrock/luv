# Knuth diagrammatic and digital-typography reference set

Private visual-research set assembled 2026-08-29. This is a second, disjoint set: the target directory was empty before this curation. The first twelve images are rasterized from PDFs published by Donald Knuth on his official Stanford Computer Science site; the thirteenth is from the TeX Users Group's authorized journal archive. Page numbers below are PDF page numbers.

## Selected references

1. **`01-xqueen-array-and-moves.png`** — Donald E. Knuth, *Xqueens and Xqueenons*, p. 1.
   URL: <https://cs.stanford.edu/~knuth/papers/Xqueens-and-Xqueenons.pdf>
   Identity: author-posted preprint on Knuth's Stanford site. A gridded occupancy problem is introduced with a large board, tiny move schematics, and a compact legend. Inspiring for showing a lattice at overview scale while preserving a local move vocabulary.

2. **`02-xqueen-pattern-families.png`** — Donald E. Knuth, *Xqueens and Xqueenons*, p. 2.
   URL: <https://cs.stanford.edu/~knuth/papers/Xqueens-and-Xqueenons.pdf>
   Identity: author-posted preprint. Several dense boards become a visual parameter sweep: same frame and mark language, varied data. Inspiring for Luft chunk/topology comparisons.

3. **`03-fibonacci-matchings-trees.png`** — Donald E. Knuth, *Fibonacci Matchings*, p. 1.
   URL: <https://cs.stanford.edu/~knuth/papers/fibonacci-matchings.pdf>
   Identity: author-posted preprint. Tree diagrams are laid out as literal derivations, with repeated rectangular node payloads and a second, compressed level. Inspiring for visualizing refinement histories without abandoning exact data.

4. **`04-hyperbolic-triangulated-disk.png`** — Donald E. Knuth, *A Finite Model of the Hyperbolic Plane* (graphic), single page.
   URL: <https://cs.stanford.edu/~knuth/hyperbolicdisk.pdf>
   Identity: downloadable graphic on Knuth's official “Downloadable Graphics” page. An exceptionally clear mesh-only object: nested rings, triangulation, variable projected scale, and a hard circular silhouette. Inspiring for manifold-sheet meshes and topology-first rendering.

5. **`05-signed-skeleton-local-rewrite.png`** — Donald E. Knuth, *Signed Skeletons*, p. 2.
   URL: <https://cs.stanford.edu/~knuth/papers/signed-skeletons.pdf>
   Identity: author-posted preprint. Small before/after boxes isolate a local rewrite while equations and prose establish invariants. Inspiring for bevel rules presented as inspectable transformations.

6. **`06-signed-skeleton-state-sequence.png`** — Donald E. Knuth, *Signed Skeletons*, p. 3.
   URL: <https://cs.stanford.edu/~knuth/papers/signed-skeletons.pdf>
   Identity: author-posted preprint. A horizontal sequence of tiny geometric states is followed by symbolic encodings and a compact matrix-like summary. Inspiring for composing geometry, packed representation, and transition history on one plate.

7. **`07-literate-tree-pipeline.png`** — Donald E. Knuth, *Skew Ternary Calculus*, p. 2.
   URL: <https://cs.stanford.edu/~knuth/programs/skew-ternary-calc.pdf>
   Identity: author-released, TeX-typeset output of the CWEB program linked from Knuth's “Downloadable Programs and Data” page. Tree drawings sit beside the arrays and formulas that generate them. Inspiring for keeping rendered topology and source-level representation mutually legible.

8. **`08-colored-tree-cycles.png`** — Donald E. Knuth, *Skew Ternary Calculus*, p. 13.
   URL: <https://cs.stanford.edu/~knuth/programs/skew-ternary-calc.pdf>
   Identity: author-released CWEB program output. Muted red/green curves and nodes distinguish paths and operations without overwhelming the black structural skeleton. Inspiring for sparse semantic color in voxel diagnostics.

9. **`09-geometric-local-move.png`** — Donald E. Knuth, *Skew Ternary Calculus*, p. 21.
   URL: <https://cs.stanford.edu/~knuth/programs/skew-ternary-calc.pdf>
   Identity: author-released CWEB program output. A large colored geometric construction is paired with a small canonical rewrite diagram. Inspiring for showing both spatial consequence and abstract rule.

10. **`10-symbolic-shape-catalog.png`** — Donald E. Knuth, *Skew Ternary Calculus*, p. 22.
    URL: <https://cs.stanford.edu/~knuth/programs/skew-ternary-calc.pdf>
    Identity: author-released CWEB program output. A vertical catalog aligns each tiny shape with its exact textual term. Inspiring for a Luft “visual alphabet” of corner, edge, bevel, and sheet primitives.

11. **`11-tree-output-from-code.png`** — Donald E. Knuth, *Skew Ternary Calculus*, p. 27.
    URL: <https://cs.stanford.edu/~knuth/programs/skew-ternary-calc.pdf>
    Identity: author-released CWEB program output. A source-code page culminates in its tree-shaped result rather than segregating implementation and illustration. Inspiring for literate debug figures generated directly from mesh code/data.

12. **`12-green-region-rewrite.png`** — Donald E. Knuth, *Skew Ternary Calculus*, p. 28.
    URL: <https://cs.stanford.edu/~knuth/programs/skew-ternary-calc.pdf>
    Identity: author-released CWEB program output. Pale green filled regions identify affected cells while black contours preserve structure; arrows make the local rewrite unambiguous. Inspiring for bevel-region overlays on lattice diagrams.

13. **`13-metafont-outline-and-pipeline.png`** — Linus Romer, “Fetamont: An extended logo typeface,” *TUGboat* 35:1 (2014), p. 19 (PDF p. 2).
    URL: <https://tug.org/TUGboat/tb35-1/tb109romer.pdf>
    Identity: article published in the TeX Users Group's public journal archive; it discusses and reproduces METAFONT/MetaPost examples and sources from Knuth and successors. The page combines a glyph construction with control points/handles, a METAFONT-to-outline processing flow, and variant glyph specimens. Inspiring for annotated bevel control geometry and visible asset pipelines.

## Concrete principles for Luft figures

- **Show global lattice and local rule together.** Pair a whole chunk/sheet with a magnified corner or bevel rewrite, using identical marks in both scales.
- **Make topology primary.** Start with thin black edges, nodes, and incidence; reserve fill and shading for derived state, not basic structure.
- **Use semantic color sparingly.** One muted accent for the active path/region and a second only when a binary distinction matters; leave the canonical geometry black.
- **Turn iteration into small multiples.** Hold projection, crop, scale, and legend fixed while changing one mesh parameter or packed field.
- **Align drawings with exact encodings.** Place packed integers, symbolic terms, or short code fragments on the same baseline as the shape they denote.
- **Build a visual alphabet.** Define reusable icons for corner types, sheet orientation, occupancy, bevel class, boundary, and nonmanifold/error states.
- **Diagram transformations, not just outcomes.** Use before/after frames and restrained arrows; annotate invariants (vertex count, winding, sheet ID) beside them.
- **Let generated output prove the program.** Prefer figures emitted from canonical Luft mesh/topology data so illustration and implementation cannot silently diverge.
- **Exploit density hierarchy.** A strong silhouette and coarse grid should read at thumbnail size; labels, packed values, and control points reward inspection at full size.
- **Annotate construction geometry openly.** Control points, tangents, offsets, normals, and bevel widths are explanatory content, not drafting debris.

## Reuse caveats

- Public availability is not the same as a permissive license. Knuth's PDFs and graphics remain copyrighted; TUGboat content also retains its applicable copyright. These PNGs are page rasterizations for private internal visual study, not cleared production assets.
- Do not redistribute the rasterizations, publish them in Luft documentation, train a model on them, or trace/copy distinctive figures without checking the source's current terms and obtaining permission where required.
- It is safer to reuse **ideas**—layout, visual hierarchy, semantic-color strategy, code/diagram adjacency—than expressive linework or complete compositions.
- The METAFONT distribution's inclusion of source is specifically not blanket permission to typeset or republish *The METAFONTbook*. This set therefore does **not** rasterize that book or any unofficial scan.
- URLs are exact landing files used for this set. Knuth's source indexes are <https://cs.stanford.edu/~knuth/preprints.html>, <https://cs.stanford.edu/~knuth/programs.html>, and <https://cs.stanford.edu/~knuth/graphics.html>.
