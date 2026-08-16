;;; A deliberately fixed-data proof of Slug's quadratic outline calculation.
;;;
;;; This proves the mathematical pixel stage in luv's shader language.  It is
;;; not yet the texture-banded, variable-length font renderer described by the
;;; reference shaders; that boundary is recorded in #OWR8OZ.

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
    (values (+ (* s1 (- 1 (* s2 s3)))
               (* (- 1 s1) s2 (- 1 s3)))
            (+ (* s3 (- 1 (* s1 s2)))
               (* (- 1 s3) s2 (- 1 s1))))))

(spv:define-shader-function slug-first-root-eligibility (s1 s2 s3)
  "Return Slug's first root-eligibility mask from three sign masks."
  (+ (* s1 (- 1.0 (* s2 s3)))
     (* (- 1.0 s1) s2 (- 1.0 s3))))

(spv:define-shader-function slug-second-root-eligibility (s1 s2 s3)
  "Return Slug's second root-eligibility mask from three sign masks."
  (+ (* s3 (- 1.0 (* s1 s2)))
     (* (- 1.0 s3) s2 (- 1.0 s1))))

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
              (outline-coordinate :vec3 :location 1))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)))
  (let* ((clip (spv:vec4 (spv:swizzle position :xy) 0.0 1.0)))
    (spv:set-output clip-position clip)
    (spv:set-output render-coordinate
                    (spv:swizzle outline-coordinate :xy))))

(spv:define-shader slug-bezier-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0)))
  ;; The quad spans 80 percent of a 256-pixel proof target, hence 204.8
  ;; pixels per em.  Keeping this literal is the boundary of the fixed proof.
  (spv:set-output
   color-output
   (slug-quadratic-outline
    render-coordinate (spv:vec2 204.8 204.8)
    (spv:vec4 0.96 0.32 0.48 1.0)
    (spv:vec2 0.50 0.08) (spv:vec2 0.08 0.38)
    (spv:vec2 0.14 0.70) (spv:vec2 0.18 0.98)
    (spv:vec2 0.50 0.74) (spv:vec2 0.82 0.98)
    (spv:vec2 0.86 0.70) (spv:vec2 0.92 0.38))))
