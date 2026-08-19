;;; An exact box-filtered lattice ink: shapes that are constant on unit
;;; cells (a QR code is the canonical one), rendered with the Slug text
;;; renderer's philosophy -- recompute exact analytic coverage at the
;;; pixel's true derivative footprint, every frame -- but with the outline
;;; integral replaced by a lattice integral.
;;;
;;; The compositing rule is the whole point.  Drawing a module grid as many
;;; rectangles gives every shared edge two half-coverage fragments that
;;; OVER-composite to three quarters of either ink: a light hairline on
;;; every module boundary, which shimmers as the surface moves.  Here the
;;; entire grid is one primitive whose fragment computes one coverage value
;;; for the whole footprint, so there is no interior boundary to leak.
;;;
;;; The closed form is a summed-area table.  The SAT of a function constant
;;; on unit cells is exactly bilinear within each cell, so the continuous
;;; integral over [0,x]*[0,y] is four integer texel loads and two lerps --
;;; not approximately, exactly.  A box filter needs that integral at its
;;; four corners; sixteen loads make the coverage exact at every scale,
;;; magnified or minified, one code path, no mip chain.  Where Slug pays a
;;; fold over a band's curve list per fragment, the lattice pays O(1).
;;;
;;; The filter footprint and width come from the same derivatives and the
;;; same knobs as SLUG-PIXELS-PER-EM, so a code and the text beside it on
;;; one panel are filtered identically.

(in-package #:luv.analytic)

(spv:define-shader-abstraction lattice-tap (lattice column row)
  "One summed-area node as a float: the count of inked cells below-left."
  `(spv:float
    (spv:swizzle (spv:texel-load ,lattice (spv:uvec2 ,column ,row)) :x)))

(spv:define-shader-abstraction lattice-node (lattice i j a b)
  "The continuous summed-area value at cell (I,J) plus offset (A,B).
Bilinear over the cell's four nodes, which for a unit-cell-constant
integrand is the integral itself, not an interpolation of it."
  `(spv:mix
    (spv:mix (lattice-tap ,lattice ,i ,j)
             (lattice-tap ,lattice (+ ,i (spv:uint 1.0)) ,j)
             ,a)
    (spv:mix (lattice-tap ,lattice ,i (+ ,j (spv:uint 1.0)))
             (lattice-tap ,lattice
                          (+ ,i (spv:uint 1.0))
                          (+ ,j (spv:uint 1.0)))
             ,a)
    ,b))

;; Vertex lanes arrive through the analytic family's own vertex stage: the
;; local coordinate carries cell units, the half-size lanes carry the grid's
;; column and row counts, and the color is the premultiplied ink of the
;; INKED cells.  The zero cells are white paper, and the paper's own outer
;; edge is part of the same integral: alpha is the footprint's overlap with
;; the grid rectangle, so even the card boundary is filtered, and no part of
;; the drawing is composited against another part of itself.
(spv:define-live-shader lattice-fragment-specification
    (:stage :fragment
     :inputs ((coordinate :vec2 :location 0)
              (lattice-size :vec3 :location 1)
              (color :vec4 :location 2))
     :resources ((lattice-data :uint-texture-2d :binding 0))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((columns (spv:swizzle lattice-size :x))
         (rows (spv:swizzle lattice-size :y))
         ;; The pixel's footprint in cell units, per axis, exactly as
         ;; SLUG-PIXELS-PER-EM measures it in em units.
         (coordinate-dx (spv:derivative-x coordinate))
         (coordinate-dy (spv:derivative-y coordinate))
         (x-gradient (spv:vec2 (spv:swizzle coordinate-dx :x)
                               (spv:swizzle coordinate-dy :x)))
         (y-gradient (spv:vec2 (spv:swizzle coordinate-dx :y)
                               (spv:swizzle coordinate-dy :y)))
         (length-footprint
           (spv:vec2 (sqrt (spv:dot x-gradient x-gradient))
                     (sqrt (spv:dot y-gradient y-gradient))))
         (width-footprint (+ (abs coordinate-dx) (abs coordinate-dy)))
         (footprint
           (spv:mix length-footprint width-footprint
                    luv.slug:slug-footprint-norm))
         (half-x
           (max (* (spv:swizzle footprint :x)
                   (* luv.slug:slug-filter-width 0.5))
                +analytic-coverage-epsilon+))
         (half-y
           (max (* (spv:swizzle footprint :y)
                   (* luv.slug:slug-filter-width 0.5))
                +analytic-coverage-epsilon+))
         (area (* (* half-x half-y) 4.0))
         ;; The box's corners, clamped to the grid: the integrand is zero
         ;; outside, so clamping the integration bound changes nothing.
         (x0 (spv:clamp (- (spv:swizzle coordinate :x) half-x) 0.0 columns))
         (x1 (spv:clamp (+ (spv:swizzle coordinate :x) half-x) 0.0 columns))
         (y0 (spv:clamp (- (spv:swizzle coordinate :y) half-y) 0.0 rows))
         (y1 (spv:clamp (+ (spv:swizzle coordinate :y) half-y) 0.0 rows))
         ;; Cell addresses, clamped to the last cell so an offset of exactly
         ;; one stays on the lattice; bilinear is exact on the closed cell.
         (column0 (min (floor x0) (- columns 1.0)))
         (column1 (min (floor x1) (- columns 1.0)))
         (row0 (min (floor y0) (- rows 1.0)))
         (row1 (min (floor y1) (- rows 1.0)))
         (a0 (- x0 column0))
         (a1 (- x1 column1))
         (b0 (- y0 row0))
         (b1 (- y1 row1))
         (i0 (spv:uint column0))
         (i1 (spv:uint column1))
         (j0 (spv:uint row0))
         (j1 (spv:uint row1))
         (s00 (lattice-node lattice-data i0 j0 a0 b0))
         (s10 (lattice-node lattice-data i1 j0 a1 b0))
         (s01 (lattice-node lattice-data i0 j1 a0 b1))
         (s11 (lattice-node lattice-data i1 j1 a1 b1))
         ;; Ink fraction and grid (paper) fraction of the footprint.
         (inked (/ (+ (- s11 s10) (- s00 s01)) area))
         (card (/ (* (- x1 x0) (- y1 y0)) area))
         (alpha (spv:swizzle color :w))
         (paper (* (max (- card inked) 0.0) alpha)))
    (spv:set-output
     color-output
     (+ (spv:vec4 paper paper paper (* card alpha))
        (spv:vec4 (* (spv:swizzle color :xyz) inked) 0.0)))))
