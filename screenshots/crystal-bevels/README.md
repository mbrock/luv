# Bridge-crystal bevel gallery

Reference captures of the same subject — the crystal capping a bridge-rail
post at authored cell (25, 10, 16) in the mountain sanctuary, dusk light —
across bevel configurations. Captured live via
`(luft.render:refresh-viewer-renderer luft.render:*viewer* ...)` with the
player at world (59.5, 35.5, 14) and `*isometric-height*` 9.0.
`seam-macro-width-2.png` is a close study of the crystal/post seam at
uniform width 2.

| Image | Configuration |
| --- | --- |
| `width-1.png` | uniform `:bevel-width 1` (metabar "1/8") |
| `width-2.png` | uniform `:bevel-width 2` ("1/4") |
| `width-4.png` | uniform `:bevel-width 4` ("1/2", fully medial) |
| `mixed-default.png` | `(make-material-bevel-profile)` — terrain 4, architecture 1, crystal 4, contact 2 |
| `mixed-arch-crystal-1.png` | default profile with `:architecture-crystal-width 1` |
| `mixed-contact-1.png` | default profile with `:contact-width 1` (all mixed relations 1) |
| `mixed-contact-4.png` | default profile with `:contact-width 4` (all mixed relations 4) |
| `seam-macro-width-2.png` | uniform width 2, camera at `*isometric-height*` 4.5 |

## The stone sliver in the crystal's base ring

Every width except fully-medial 4 shows one stone quad interrupting the
crystal's bottom bevel ring. A five-cell fixture (rail run, post, crystal
cap) meshed through the scene material program shows the exact emission
around the horizontal material seam, at every uniform width:

- Each flat side splits at the seam: the post's top band is plain
  `:LIMESTONE`, the crystal's bottom band is plain `:AETHER-CRYSTAL`.
  Side geometry never crosses the material boundary, so each band takes its
  own cell's material and the seam is sharp.
- Each vertical corner is emitted as one `:JUNCTION` patch that spans both
  cells across the seam (z from one bevel width below to one above it).
  A seam-crossing patch must pick a single material; the closure algebra
  resolves stone-plus-luminous to the contact assembly
  `(:CONTACT :DRESSED-LIMESTONE)` — stone-primary with crystal provenance
  (`material-closure-surface-assembly`, `luft/render/materials.lisp`).

So the base ring interleaves crystal sides with dressed-limestone corners.
From the isometric camera one corner column faces the viewer, and at widths
one and two it is a thin sliver inside an otherwise glassy ring: the
"one incoherent stone quad." At width 4 the same corner patches grow into
the full medial taper between post and gem and read as a deliberate collar,
which is why the medial mesh looks fine.

The structural question this raises: should junction geometry split at a
material seam the way band geometry already does (making the ring fully
crystal and the collar fully stone), or should the seam-crossing contact
assembly render crystal-primary instead of stone-primary?
