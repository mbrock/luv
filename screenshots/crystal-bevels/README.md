# Bridge-crystal bevel gallery

Reference captures of the same subject — the crystal capping a bridge-rail
post at authored cell (25, 10, 16) in the mountain sanctuary, dusk light —
across bevel configurations. Captured live via
`(luft.render:refresh-viewer-renderer luft.render:*viewer* ...)` with the
player at world (59.5, 35.5, 14) and `*isometric-height*` 9.0.

| Image | Configuration |
| --- | --- |
| `width-1.png` | uniform `:bevel-width 1` (metabar "1/8") |
| `width-2.png` | uniform `:bevel-width 2` ("1/4") |
| `width-4.png` | uniform `:bevel-width 4` ("1/2", fully medial) |
| `mixed-default.png` | `(make-material-bevel-profile)` — terrain 4, architecture 1, crystal 4, contact 2 |
| `mixed-arch-crystal-1.png` | default profile with `:architecture-crystal-width 1` |
| `mixed-contact-1.png` | default profile with `:contact-width 1` (all mixed relations 1) |
| `mixed-contact-4.png` | default profile with `:contact-width 4` (all mixed relations 4) |

What the set shows:

- Uniform widths are internally coherent: every contact chamfer has the same
  width, so the crystal/stone boundary reads as one continuous seam. Width 1
  keeps masonry but loses the jewel; width 4 gets the jewel but melts the
  rails and posts into ridge tents.
- The mixed profile buys the jewel-on-masonry contrast, and the contact width
  decides how much of the supporting post the pale crystal *bezel* assembly
  covers (`material-closure-surface-assembly` in
  `luft/render/materials.lisp`: any chamfer closure containing a luminous
  reading takes the crystal bezel appearance in its host).
- The known incoherence: in `mixed-default.png` (and clearest in wider
  mixed-mode captures) exactly one quad of the crystal's bottom bevel ring
  renders as plain stone. The luminous-bezel rule sees only the faces
  combinatorially incident to a patch's own site, so a patch in the ring
  whose incident faces are both stone (post rim / rail top) never receives a
  luminous reading, while its visual neighbors do. Visual adjacency and
  combinatorial incidence disagree, and the ring breaks at that one patch.
