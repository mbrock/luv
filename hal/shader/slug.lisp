;;; Slug's quadratic outline calculation, first as a fixed proof and then as
;;; the data-driven band texture path used by real outlines.

(in-package #:luv.slug)

(defconstant +slug-root-epsilon+ (/ 1.0 65536.0))

(defun slug-root-eligibility (y1 y2 y3)
  "Return the two Slug root-eligibility bits as numeric masks.

Only strict positivity matters.  The arithmetic form is Table 1 of Lengyel's
2017 paper without an integer lookup, and is the form generated into the proof
pixel shader.  #S2F8SA"
  (let ((s1 (if (plusp y1) 1 0))
        (s2 (if (plusp y2) 1 0))
        (s3 (if (plusp y3) 1 0)))
    (values (slug-first-root-eligibility s1 s2 s3)
            (slug-second-root-eligibility s1 s2 s3))))

(arith-lisp:define-lisp-arithmetic-function
    slug-first-root-eligibility ((s1) (s2) (s3))
  (+ (* s1 (- 1 (* s2 s3)))
     (* (- 1 s1) s2 (- 1 s3))))

(arith-lisp:define-lisp-arithmetic-function
    slug-second-root-eligibility ((s1) (s2) (s3))
  (+ (* s3 (- 1 (* s1 s2)))
     (* (- 1 s3) s2 (- 1 s1))))

(spv:define-shader-function slug-axis-contribution
    (p1-axis p2-axis p3-axis p1-other p2-other p3-other pixels-per-em)
  "Return coverage1, coverage2, weight1, and weight2 for one ray axis."
  (let* ((axis-a (+ (- p1-axis (* p2-axis 2.0)) p3-axis))
         (axis-b (- p1-axis p2-axis))
         (other-a (+ (- p1-other (* p2-other 2.0)) p3-other))
         (other-b (- p1-other p2-other))
         (linear
           (- 1.0
              (spv:step +slug-root-epsilon+ (abs axis-a))))
         (safe-a (spv:mix axis-a 1.0 linear))
         (safe-b
           (spv:mix axis-b 1.0
                    (- 1.0
                       (spv:step +slug-root-epsilon+ (abs axis-b)))))
         (discriminant
           (max (- (* axis-b axis-b) (* axis-a p1-axis)) 0.0))
         (root-distance (sqrt discriminant))
         (quadratic-t1 (/ (- axis-b root-distance) safe-a))
         (quadratic-t2 (/ (+ axis-b root-distance) safe-a))
         (linear-t (/ (* p1-axis 0.5) safe-b))
         (t1 (spv:mix quadratic-t1 linear-t linear))
         (t2 (spv:mix quadratic-t2 linear-t linear))
         ;; FSign makes zero exactly "not positive", matching the eligibility
         ;; table without epsilon classification at shared control points.
         (s1 (max (signum p1-axis) 0.0))
         (s2 (max (signum p2-axis) 0.0))
         (s3 (max (signum p3-axis) 0.0))
         (eligible1 (slug-first-root-eligibility s1 s2 s3))
         (eligible2 (slug-second-root-eligibility s1 s2 s3))
         (crossing1
           (+ (* (- (* other-a t1) (* other-b 2.0)) t1)
              p1-other))
         (crossing2
           (+ (* (- (* other-a t2) (* other-b 2.0)) t2)
              p1-other))
         (scaled1 (* crossing1 pixels-per-em))
         (scaled2 (* crossing2 pixels-per-em))
         (coverage1
           (* eligible1 (spv:clamp (+ scaled1 0.5) 0.0 1.0)))
         (coverage2
           (* eligible2 (spv:clamp (+ scaled2 0.5) 0.0 1.0)))
         (weight1
           (* eligible1
              (spv:clamp (- 1.0 (* (abs scaled1) 2.0)) 0.0 1.0)))
         (weight2
           (* eligible2
              (spv:clamp (- 1.0 (* (abs scaled2) 2.0)) 0.0 1.0))))
    (spv:vec4 coverage1 coverage2 weight1 weight2)))

(spv:define-shader-function slug-horizontal-contribution
    (p1 p2 p3 pixels-per-em)
  "Evaluate one quadratic against the pixel's horizontal winding ray."
  (slug-axis-contribution
   (spv:swizzle p1 :y) (spv:swizzle p2 :y) (spv:swizzle p3 :y)
   (spv:swizzle p1 :x) (spv:swizzle p2 :x) (spv:swizzle p3 :x)
   (spv:swizzle pixels-per-em :x)))

(spv:define-shader-function slug-vertical-contribution
    (p1 p2 p3 pixels-per-em)
  "Evaluate one quadratic against the pixel's vertical winding ray."
  (slug-axis-contribution
   (spv:swizzle p1 :x) (spv:swizzle p2 :x) (spv:swizzle p3 :x)
   (spv:swizzle p1 :y) (spv:swizzle p2 :y) (spv:swizzle p3 :y)
   (spv:swizzle pixels-per-em :y)))

(spv:define-shader-function slug-combine-coverage
    (horizontal0 horizontal1 horizontal2 horizontal3
     vertical0 vertical1 vertical2 vertical3)
  "Combine four curves' horizontal and vertical ray contributions."
  (let* ((xcov
           (+ 0.0
              (- (spv:swizzle horizontal0 :x)
                 (spv:swizzle horizontal0 :y))
              (- (spv:swizzle horizontal1 :x)
                 (spv:swizzle horizontal1 :y))
              (- (spv:swizzle horizontal2 :x)
                 (spv:swizzle horizontal2 :y))
              (- (spv:swizzle horizontal3 :x)
                 (spv:swizzle horizontal3 :y))))
         (ycov
           (+ 0.0
              (- (spv:swizzle vertical0 :y)
                 (spv:swizzle vertical0 :x))
              (- (spv:swizzle vertical1 :y)
                 (spv:swizzle vertical1 :x))
              (- (spv:swizzle vertical2 :y)
                 (spv:swizzle vertical2 :x))
              (- (spv:swizzle vertical3 :y)
                 (spv:swizzle vertical3 :x))))
         (xweight
           (max 0.0
                (spv:swizzle horizontal0 :z)
                (spv:swizzle horizontal0 :w)
                (spv:swizzle horizontal1 :z)
                (spv:swizzle horizontal1 :w)
                (spv:swizzle horizontal2 :z)
                (spv:swizzle horizontal2 :w)
                (spv:swizzle horizontal3 :z)
                (spv:swizzle horizontal3 :w)))
         (yweight
           (max 0.0
                (spv:swizzle vertical0 :z)
                (spv:swizzle vertical0 :w)
                (spv:swizzle vertical1 :z)
                (spv:swizzle vertical1 :w)
                (spv:swizzle vertical2 :z)
                (spv:swizzle vertical2 :w)
                (spv:swizzle vertical3 :z)
                (spv:swizzle vertical3 :w))))
    (spv:clamp
     (max
      (/ (abs (+ (* xcov xweight) (* ycov yweight)))
         (max (+ xweight yweight) +slug-root-epsilon+))
      (min (abs xcov) (abs ycov)))
     0.0 1.0)))

(spv:define-shader-function slug-combine-band-coverage
    (xcov xweight ycov yweight)
  "Combine the accumulated horizontal and vertical band traversals."
  (spv:clamp
   (max
    (/ (abs (+ (* xcov xweight) (* ycov yweight)))
       (max (+ xweight yweight) +slug-root-epsilon+))
    (min (abs xcov) (abs ycov)))
   0.0 1.0))

(spv:define-shader-function slug-texel-coordinate (address)
  "Map Slug's fixed-width linear texture ADDRESS to exact uint coordinates."
  (spv:uvec2 (mod address (spv:uint 4096.0))
             (/ address (spv:uint 4096.0))))

(spv:define-shader-function slug-quadratic-outline
    (coordinate pixels-per-em color p0 c0 p1 c1 p2 c2 p3 c3)
  "Render one connected four-quadratic contour with Slug's two-ray pixel math.

This is a typed shader function: its LET* bindings and nested calls are parsed
directly into shader objects.  The fixed curve count remains an atelier proof,
not the band-texture font renderer.  #OWR8OZ"
  (let* ((q0p0 (- p0 coordinate))
         (q0c0 (- c0 coordinate))
         (q0p1 (- p1 coordinate))
         (q1c1 (- c1 coordinate))
         (q1p2 (- p2 coordinate))
         (q2c2 (- c2 coordinate))
         (q2p3 (- p3 coordinate))
         (q3c3 (- c3 coordinate))
         (horizontal0
           (slug-horizontal-contribution
            q0p0 q0c0 q0p1 pixels-per-em))
         (horizontal1
           (slug-horizontal-contribution
            q0p1 q1c1 q1p2 pixels-per-em))
         (horizontal2
           (slug-horizontal-contribution
            q1p2 q2c2 q2p3 pixels-per-em))
         (horizontal3
           (slug-horizontal-contribution
            q2p3 q3c3 q0p0 pixels-per-em))
         (vertical0
           (slug-vertical-contribution
            q0p0 q0c0 q0p1 pixels-per-em))
         (vertical1
           (slug-vertical-contribution
            q0p1 q1c1 q1p2 pixels-per-em))
         (vertical2
           (slug-vertical-contribution
            q1p2 q2c2 q2p3 pixels-per-em))
         (vertical3
           (slug-vertical-contribution
            q2p3 q3c3 q0p0 pixels-per-em))
         (coverage
           (slug-combine-coverage
            horizontal0 horizontal1 horizontal2 horizontal3
            vertical0 vertical1 vertical2 vertical3)))
    (* color coverage)))

(spv:define-shader slug-bezier-vertex-specification
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (outline-coordinate :vec3 :location 1)
              (pixels-per-em :vec3 :location 2))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-pixels-per-em :vec2 :location 1)))
  (let* ((clip (spv:vec4 (spv:swizzle position :xy) 0.0 1.0)))
    (spv:set-output clip-position clip)
    (spv:set-output render-coordinate
                    (spv:swizzle outline-coordinate :xy))
    (spv:set-output render-pixels-per-em
                    (spv:swizzle pixels-per-em :xy))))

(spv:define-shader slug-bezier-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (pixels-per-em :vec2 :location 1))
     :outputs ((color-output :vec4 :location 0)))
  (spv:set-output
   color-output
   (slug-quadratic-outline
    render-coordinate pixels-per-em
    (spv:vec4 0.96 0.32 0.48 1.0)
    (spv:vec2 0.50 0.08) (spv:vec2 0.08 0.38)
    (spv:vec2 0.14 0.70) (spv:vec2 0.18 0.98)
    (spv:vec2 0.50 0.74) (spv:vec2 0.82 0.98)
    (spv:vec2 0.86 0.70) (spv:vec2 0.92 0.38))))

(spv:define-shader slug-banded-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (pixels-per-em :vec2 :location 1))
     :resources ((band-data :uint-texture-2d :binding 0)
                 (curve-data :texture-2d :binding 1))
     :outputs ((color-output :vec4 :location 0)))
  ;; One band per axis is the complete correctness path for an outline: every
  ;; serialized curve participates.  Subdividing the same lists into spatial
  ;; bands is the subsequent culling optimization, not a different algorithm.
  (let* ((zero (spv:uint 0.0))
         (horizontal-header-location (spv:uvec2 zero zero))
         (vertical-header-location
           (spv:uvec2 (spv:uint 1.0) zero))
         (horizontal-header
           (spv:texel-load band-data horizontal-header-location))
         (vertical-header
           (spv:texel-load band-data vertical-header-location))
         (horizontal-count (spv:swizzle horizontal-header :x))
         (horizontal-offset (spv:swizzle horizontal-header :y))
         (vertical-count (spv:swizzle vertical-header :x))
         (vertical-offset (spv:swizzle vertical-header :y))
         (horizontal
           (spv:counted-fold
               (index horizontal-count state (spv:vec2 0.0 0.0))
             (let* ((entry-address (+ horizontal-offset index))
                    (curve-location
                      (spv:swizzle
                       (spv:texel-load
                        band-data
                        (slug-texel-coordinate entry-address))
                       :xy))
                    (next-location
                      (slug-texel-coordinate
                       (+ (spv:swizzle curve-location :x)
                          (* (spv:swizzle curve-location :y)
                             (spv:uint 4096.0))
                          (spv:uint 1.0))))
                    (curve (spv:texel-load curve-data curve-location))
                    (next (spv:texel-load curve-data next-location))
                    (p1 (- (spv:swizzle curve :xy) render-coordinate))
                    (p2 (- (spv:swizzle curve :zw) render-coordinate))
                    (p3 (- (spv:swizzle next :xy) render-coordinate))
                    (contribution
                      (slug-horizontal-contribution
                       p1 p2 p3 pixels-per-em)))
               (spv:vec2
                (+ (spv:swizzle state :x)
                   (- (spv:swizzle contribution :x)
                      (spv:swizzle contribution :y)))
                (max (spv:swizzle state :y)
                     (spv:swizzle contribution :z)
                     (spv:swizzle contribution :w))))))
         (vertical
           (spv:counted-fold
               (index vertical-count state (spv:vec2 0.0 0.0))
             (let* ((entry-address (+ vertical-offset index))
                    (curve-location
                      (spv:swizzle
                       (spv:texel-load
                        band-data
                        (slug-texel-coordinate entry-address))
                       :xy))
                    (next-location
                      (slug-texel-coordinate
                       (+ (spv:swizzle curve-location :x)
                          (* (spv:swizzle curve-location :y)
                             (spv:uint 4096.0))
                          (spv:uint 1.0))))
                    (curve (spv:texel-load curve-data curve-location))
                    (next (spv:texel-load curve-data next-location))
                    (p1 (- (spv:swizzle curve :xy) render-coordinate))
                    (p2 (- (spv:swizzle curve :zw) render-coordinate))
                    (p3 (- (spv:swizzle next :xy) render-coordinate))
                    (contribution
                      (slug-vertical-contribution
                       p1 p2 p3 pixels-per-em)))
               (spv:vec2
                (+ (spv:swizzle state :x)
                   (- (spv:swizzle contribution :y)
                      (spv:swizzle contribution :x)))
                (max (spv:swizzle state :y)
                     (spv:swizzle contribution :z)
                     (spv:swizzle contribution :w))))))
         (coverage
           (slug-combine-band-coverage
            (spv:swizzle horizontal :x) (spv:swizzle horizontal :y)
            (spv:swizzle vertical :x) (spv:swizzle vertical :y))))
    (spv:set-output
     color-output
     (* (spv:vec4 0.96 0.32 0.48 1.0) coverage))))

(spv:define-shader slug-atlas-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (atlas-base :vec2 :location 1)
              (outline-bounds :vec4 :location 2)
              (band-counts :vec2 :location 3))
     :resources ((band-data :uint-texture-2d :binding 0)
                 (curve-data :texture-2d :binding 1))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((one (spv:uint 1.0))
         (width (spv:uint 4096.0))
         (band-base (spv:uint (spv:swizzle atlas-base :x)))
         (curve-base (spv:uint (spv:swizzle atlas-base :y)))
         (horizontal-band-count
           (spv:uint (spv:swizzle band-counts :x)))
         (vertical-band-count
           (spv:uint (spv:swizzle band-counts :y)))
         (coordinate-dx (spv:derivative-x render-coordinate))
         (coordinate-dy (spv:derivative-y render-coordinate))
         (x-gradient
           (spv:vec2 (spv:swizzle coordinate-dx :x)
                     (spv:swizzle coordinate-dy :x)))
         (y-gradient
           (spv:vec2 (spv:swizzle coordinate-dx :y)
                     (spv:swizzle coordinate-dy :y)))
         (pixels-per-em
           (spv:vec2
            (/ 1.0 (max (sqrt (spv:dot x-gradient x-gradient))
                        +slug-root-epsilon+))
            (/ 1.0 (max (sqrt (spv:dot y-gradient y-gradient))
                        +slug-root-epsilon+))))
         (horizontal-position
           (spv:clamp
            (/ (- (spv:swizzle render-coordinate :y)
                  (spv:swizzle outline-bounds :y))
               (max (- (spv:swizzle outline-bounds :w)
                       (spv:swizzle outline-bounds :y))
                    +slug-root-epsilon+))
            0.0 1.0))
         (vertical-position
           (spv:clamp
            (/ (- (spv:swizzle render-coordinate :x)
                  (spv:swizzle outline-bounds :x))
               (max (- (spv:swizzle outline-bounds :z)
                       (spv:swizzle outline-bounds :x))
                    +slug-root-epsilon+))
            0.0 1.0))
         (horizontal-band-candidate
           (spv:uint
            (* horizontal-position (spv:float horizontal-band-count))))
         (vertical-band-candidate
           (spv:uint
            (* vertical-position (spv:float vertical-band-count))))
         (horizontal-band
           (if (< horizontal-band-candidate horizontal-band-count)
               horizontal-band-candidate
               (- horizontal-band-count one)))
         (vertical-band
           (if (< vertical-band-candidate vertical-band-count)
               vertical-band-candidate
               (- vertical-band-count one)))
         (horizontal-header-address (+ band-base horizontal-band))
         (vertical-header-address
           (+ band-base horizontal-band-count vertical-band))
         (horizontal-header-location
           (spv:uvec2 (mod horizontal-header-address width)
                      (/ horizontal-header-address width)))
         (vertical-header-location
           (spv:uvec2 (mod vertical-header-address width)
                      (/ vertical-header-address width)))
         (horizontal-header
           (spv:texel-load band-data horizontal-header-location))
         (vertical-header
           (spv:texel-load band-data vertical-header-location))
         (horizontal-count (spv:swizzle horizontal-header :x))
         (horizontal-offset (spv:swizzle horizontal-header :y))
         (vertical-count (spv:swizzle vertical-header :x))
         (vertical-offset (spv:swizzle vertical-header :y))
         (horizontal
           (spv:counted-fold
               (index horizontal-count state (spv:vec2 0.0 0.0))
             (let* ((entry-address (+ band-base horizontal-offset index))
                    (entry-location
                      (spv:uvec2 (mod entry-address width)
                                 (/ entry-address width)))
                    (local-location
                      (spv:swizzle
                       (spv:texel-load band-data entry-location)
                       :xy))
                    (curve-address
                      (+ curve-base
                         (spv:swizzle local-location :x)
                         (* (spv:swizzle local-location :y) width)))
                    (curve-location
                      (spv:uvec2 (mod curve-address width)
                                 (/ curve-address width)))
                    (next-location
                      (spv:uvec2 (mod (+ curve-address one) width)
                                 (/ (+ curve-address one) width)))
                    (curve (spv:texel-load curve-data curve-location))
                    (next (spv:texel-load curve-data next-location))
                    (p1 (- (spv:swizzle curve :xy) render-coordinate))
                    (p2 (- (spv:swizzle curve :zw) render-coordinate))
                    (p3 (- (spv:swizzle next :xy) render-coordinate))
                    (contribution
                      (slug-horizontal-contribution
                       p1 p2 p3 pixels-per-em)))
               (spv:vec2
                (+ (spv:swizzle state :x)
                   (- (spv:swizzle contribution :x)
                      (spv:swizzle contribution :y)))
                (max (spv:swizzle state :y)
                     (spv:swizzle contribution :z)
                     (spv:swizzle contribution :w))))))
         (vertical
           (spv:counted-fold
               (index vertical-count state (spv:vec2 0.0 0.0))
             (let* ((entry-address (+ band-base vertical-offset index))
                    (entry-location
                      (spv:uvec2 (mod entry-address width)
                                 (/ entry-address width)))
                    (local-location
                      (spv:swizzle
                       (spv:texel-load band-data entry-location)
                       :xy))
                    (curve-address
                      (+ curve-base
                         (spv:swizzle local-location :x)
                         (* (spv:swizzle local-location :y) width)))
                    (curve-location
                      (spv:uvec2 (mod curve-address width)
                                 (/ curve-address width)))
                    (next-location
                      (spv:uvec2 (mod (+ curve-address one) width)
                                 (/ (+ curve-address one) width)))
                    (curve (spv:texel-load curve-data curve-location))
                    (next (spv:texel-load curve-data next-location))
                    (p1 (- (spv:swizzle curve :xy) render-coordinate))
                    (p2 (- (spv:swizzle curve :zw) render-coordinate))
                    (p3 (- (spv:swizzle next :xy) render-coordinate))
                    (contribution
                      (slug-vertical-contribution p1 p2 p3 pixels-per-em)))
               (spv:vec2
                (+ (spv:swizzle state :x)
                   (- (spv:swizzle contribution :y)
                      (spv:swizzle contribution :x)))
                (max (spv:swizzle state :y)
                     (spv:swizzle contribution :z)
                     (spv:swizzle contribution :w))))))
         (coverage
           (slug-combine-band-coverage
            (spv:swizzle horizontal :x) (spv:swizzle horizontal :y)
            (spv:swizzle vertical :x) (spv:swizzle vertical :y))))
    (spv:set-output
     color-output (* (spv:vec4 0.96 0.32 0.48 1.0) coverage))))
