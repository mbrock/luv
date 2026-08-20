;;; Small analytic GUI primitives evaluated directly in the fragment shader.

(in-package #:luv.analytic)

(defconstant +analytic-coverage-epsilon+ (/ 1.0 65536.0))

;; Negative values are inside. RADIUS is clamped to the shape's shorter
;; half-extent, so rectangles, roundrects, capsules, and circles share one
;; definition.
(arith-lisp:define-lisp-arithmetic-function
    roundrect-signed-distance
    ((x) (y) (half-width) (half-height) (radius))
  (let* ((safe-radius
           (min (max radius 0.0) (min half-width half-height)))
         (qx (- (abs x) (- half-width safe-radius)))
         (qy (- (abs y) (- half-height safe-radius)))
         (outside-x (max qx 0.0))
         (outside-y (max qy 0.0))
         (outside
           (sqrt (+ (* outside-x outside-x)
                    (* outside-y outside-y))))
         (inside (min (max qx qy) 0.0)))
    (- (+ outside inside) safe-radius)))

(shader:define-shader-function roundrect-coverage
    (coordinate half-size-radius)
  "Evaluate an analytic roundrect and turn its screen gradient into coverage."
  (let* ((distance
           (roundrect-signed-distance
            (shader:swizzle coordinate :x)
            (shader:swizzle coordinate :y)
            (shader:swizzle half-size-radius :x)
            (shader:swizzle half-size-radius :y)
            (shader:swizzle half-size-radius :z)))
         (distance-dx (shader:derivative-x distance))
         (distance-dy (shader:derivative-y distance))
         (pixel-span
           (max
            (sqrt (+ (* distance-dx distance-dx)
                     (* distance-dy distance-dy)))
            +analytic-coverage-epsilon+)))
    (shader:clamp (- 0.5 (/ distance pixel-span)) 0.0 1.0)))

(shader:define-shader roundrect-vertex-specification
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (local-coordinate :vec3 :location 1)
              (half-size-radius :vec3 :location 2)
              (color :vec3 :location 3))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-half-size-radius :vec3 :location 1)
               (render-color :vec4 :location 2)))
  (let* ((alpha (shader:swizzle position :z)))
    (shader:set-output
     clip-position (shader:vec4 (shader:swizzle position :xy) 0.0 1.0))
    (shader:set-output render-coordinate
                    (shader:swizzle local-coordinate :xy))
    (shader:set-output render-half-size-radius half-size-radius)
    (shader:set-output render-color
                    (shader:vec4 (* color alpha) alpha))))

(shader:define-shader roundrect-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (half-size-radius :vec3 :location 1)
              (color :vec4 :location 2))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((coverage (roundrect-coverage render-coordinate half-size-radius)))
    (shader:set-output color-output (* color coverage))))
