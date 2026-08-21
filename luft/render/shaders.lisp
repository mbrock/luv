(in-package #:luft.render.shaders)

;;; A face record is one UVec4.  The vertex stage pulls one record per
;;; instance and realizes one of its sixteen implicit points.  All discrete
;;; geometric decisions were authored by the CPU in the shape word.

(define-shader face-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (world-position-output :vec3 :location 0)
               (face-normal-output :vec3 :location 1
                                   :interpolation :flat)
               (stock-output :float :location 2
                             :interpolation :flat)
               (lattice-output :vec2 :location 3))
     :resources ((faces :storage-buffer :binding 0 :element :uvec4)
                 (camera-state :uniform-block :binding 1
                  :members ((camera-position :vec4)
                            (camera-right :vec4)
                            (camera-up :vec4)
                            (camera-forward :vec4)
                            (camera-projection :vec4)
                            (chamfer-parameters :vec4)))))
  (let* ((record (buffer-element faces instance-index))
         (site-low (swizzle record :x))
         (site-high (swizzle record :y))
         (shape (swizzle record :z))
         (extent (uint (ldb (byte 3 0) site-low)))
         (negative (uint (ldb (byte 1 3) site-low)))
         (zero (uint 0.0))
         (one (uint 1.0))
         (three (uint 3.0))
         (four (uint 4.0))
         (i (/ vertex-index four))
         (j (mod vertex-index four))
         (i-boundary (if (= i zero) (= i zero) (= i three)))
         (j-boundary (if (= j zero) (= j zero) (= j three)))
         (point-kind
           (if i-boundary
               (if j-boundary (uint 2.0) one)
               (if j-boundary one zero)))
         (x (float (ldb (byte 24 4) site-low)))
         (y (float (+ (ldb (byte 4 28) site-low)
                      (* (ldb (byte 20 0) site-high) (uint 16.0)))))
         (z (float (ldb (byte 8 20) site-high)))
         (stock (float (ldb (byte 4 28) site-high)))
         (u (if (= extent (uint 6.0))
                (vec3 0.0 1.0 0.0)
                (vec3 1.0 0.0 0.0)))
         (v (if (= extent (uint 3.0))
                (vec3 0.0 1.0 0.0)
                (vec3 0.0 0.0 1.0)))
         (canonical-normal
           (if (= extent (uint 3.0))
               (vec3 0.0 0.0 1.0)
               (if (= extent (uint 5.0))
                   (vec3 0.0 -1.0 0.0)
                   (vec3 1.0 0.0 0.0))))
         (face-normal (if (= negative one)
                          (* canonical-normal -1.0)
                          canonical-normal))
         (width (swizzle chamfer-parameters :x))
         (lambda-i (if (= i zero) 0.0
                       (if (= i one) width
                           (if (= i (uint 2.0)) (- 1.0 width) 1.0))))
         (lambda-j (if (= j zero) 0.0
                       (if (= j one) width
                           (if (= j (uint 2.0)) (- 1.0 width) 1.0))))
         (flat-position (+ (vec3 x y z)
                           (+ (* u lambda-i) (* v lambda-j))))
         (corner-code
           (if (= i zero)
               (if (= j zero)
                   (uint (ldb (byte 6 8) shape))
                   (uint (ldb (byte 6 14) shape)))
               (if (= j zero)
                   (uint (ldb (byte 6 20) shape))
                   (uint (ldb (byte 6 26) shape)))))
         (corner-direction (uint (ldb (byte 5 0) corner-code)))
         (corner-q
           (vec3 (- (float (mod corner-direction three)) 1.0)
                 (- (float (mod (/ corner-direction three) three)) 1.0)
                 (- (float (/ corner-direction (uint 9.0))) 1.0)))
         (corner-reach
           (if (= (uint (ldb (byte 1 5) corner-code)) one) 0.6666667 0.5))
         (edge-code
           (if i-boundary
               (if (= i zero)
                   (uint (ldb (byte 2 0) shape))
                   (uint (ldb (byte 2 2) shape)))
               (if (= j zero)
                   (uint (ldb (byte 2 4) shape))
                   (uint (ldb (byte 2 6) shape)))))
         (edge-tangent (if i-boundary u v))
         (edge-side (if i-boundary
                        (if (= i zero) -1.0 1.0)
                        (if (= j zero) -1.0 1.0)))
         (edge-outward (* edge-tangent edge-side))
         (edge-normal-sign (if (= edge-code one) -1.0 1.0))
         (edge-q (if (= edge-code zero)
                     (vec3 0.0 0.0 0.0)
                     (- (* face-normal edge-normal-sign) edge-outward)))
         (q (if (= point-kind (uint 2.0))
                corner-q
                (if (= point-kind one) edge-q (vec3 0.0 0.0 0.0))))
         (reach (if (= point-kind (uint 2.0))
                    corner-reach
                    (if (= point-kind one) 0.5 0.0)))
         (world-position (+ flat-position (* q (* width reach))))
         (relative (- world-position (swizzle camera-position :xyz)))
         (view-x (dot relative (swizzle camera-right :xyz)))
         (view-y (dot relative (swizzle camera-up :xyz)))
         (view-z (dot relative (swizzle camera-forward :xyz))))
    ;; The projection lane is packed so that both projections share these
    ;; three rows; only the homogeneous divisor tells them apart.  Dividing
    ;; by the view depth is what makes a picture perspective, and dividing
    ;; by one is what makes it isometric.
    (set-output clip-position
                (vec4 (* view-x (swizzle camera-projection :x))
                      (- (* view-y (swizzle camera-projection :y)))
                      (+ (* view-z (swizzle camera-projection :z))
                         (swizzle camera-projection :w))
                      (mix 1.0 view-z (swizzle chamfer-parameters :z))))
    (set-output world-position-output world-position)
    (set-output face-normal-output face-normal)
    (set-output stock-output stock)
    ;; The lattice coordinate carries the 4x4 point grid into the fragment
    ;; stage, where the whole wireframe is one distance-to-integer field.
    (set-output lattice-output (vec2 (float i) (float j)))))

(define-shader face-fragment-specification
    (:stage :fragment
     :inputs ((world-position :vec3 :location 0)
              (face-normal :vec3 :location 1 :interpolation :flat)
              (stock :float :location 2 :interpolation :flat)
              (lattice :vec2 :location 3))
     :outputs ((color-output :vec4 :location 0))
     ;; The same block the vertex stage reads, declared identically so the
     ;; one uniform buffer serves both stages.
     :resources ((camera-state :uniform-block :binding 1
                  :members ((camera-position :vec4)
                            (camera-right :vec4)
                            (camera-up :vec4)
                            (camera-forward :vec4)
                            (camera-projection :vec4)
                            (chamfer-parameters :vec4)))))
  (let* ((dx (derivative-x world-position))
         (dy (derivative-y world-position))
         (geometric-normal
           (normalize
            (vec3 (- (* (swizzle dx :y) (swizzle dy :z))
                     (* (swizzle dx :z) (swizzle dy :y)))
                  (- (* (swizzle dx :z) (swizzle dy :x))
                     (* (swizzle dx :x) (swizzle dy :z)))
                  (- (* (swizzle dx :x) (swizzle dy :y))
                     (* (swizzle dx :y) (swizzle dy :x))))))
         (normal (if (< (dot geometric-normal face-normal) 0.0)
                     (* geometric-normal -1.0)
                     geometric-normal))
         ;; A studio: one warm key over the left shoulder, a cool hemisphere
         ;; for the sky with a dim warm bounce off the floor, and a weak cool
         ;; fill opposite the key, so a face turned away from everything
         ;; still reads as a plane instead of a silhouette.
         (albedo (vec3 0.58 0.58 0.57))
         (sun (normalize (vec3 0.42 -0.55 0.72)))
         (fill-direction (normalize (vec3 -0.62 0.44 0.18)))
         (key (max 0.0 (dot normal sun)))
         (fill (max 0.0 (dot normal fill-direction)))
         (sky-mix (+ 0.5 (* 0.5 (swizzle normal :z))))
         (ambient (mix (vec3 0.17 0.15 0.13) (vec3 0.33 0.37 0.44) sky-mix))
         (paper (* albedo
                   (+ ambient
                      (+ (* (vec3 1.02 0.96 0.85) key)
                         (* (vec3 0.15 0.18 0.24) fill)))))
         ;; Every wire is one family of lattice lines, and a family is just
         ;; the set where a linear lattice function hits an integer.  U and V
         ;; are the 4x4 grid; U-V is the shared diagonal of both triangles in
         ;; each cell, because within cell (i,j) the diagonal is exactly
         ;; U-V = i-j.  Dividing the distance by the screen-space gradient
         ;; gives pixels, so every wire is one pixel wide at any depth.
         (u (swizzle lattice :x))
         (v (swizzle lattice :y))
         (d (- u v))
         (lattice-dx (derivative-x lattice))
         (lattice-dy (derivative-y lattice))
         (u-width (+ (abs (swizzle lattice-dx :x))
                     (abs (swizzle lattice-dy :x))))
         (v-width (+ (abs (swizzle lattice-dx :y))
                     (abs (swizzle lattice-dy :y))))
         (d-width (+ (abs (- (swizzle lattice-dx :x) (swizzle lattice-dx :y)))
                     (abs (- (swizzle lattice-dy :x) (swizzle lattice-dy :y)))))
         (u-line (floor (+ u 0.5)))
         (v-line (floor (+ v 0.5)))
         (d-line (floor (+ d 0.5)))
         (u-pixels (/ (abs (- u u-line)) (max u-width 0.000001)))
         (v-pixels (/ (abs (- v v-line)) (max v-width 0.000001)))
         (d-pixels (/ (abs (- d d-line)) (max d-width 0.000001)))
         ;; Lattice lines 0 and 3 are the face outline; 1 and 2 bound the
         ;; chamfer skirt.  The outline gets the darker ink.
         (u-rim (- 1.0 (step 0.5 (min u-line (- 3.0 u-line)))))
         (v-rim (- 1.0 (step 0.5 (min v-line (- 3.0 v-line)))))
         (u-wire (* (- 1.0 (smoothstep 0.4 1.4 u-pixels)) (mix 0.55 1.0 u-rim)))
         (v-wire (* (- 1.0 (smoothstep 0.4 1.4 v-pixels)) (mix 0.55 1.0 v-rim)))
         (d-wire (* (- 1.0 (smoothstep 0.2 1.0 d-pixels)) 0.22))
         (wire (* (swizzle chamfer-parameters :y)
                  (max (max u-wire v-wire) d-wire)))
         (ink (vec3 0.06 0.07 0.10))
         (radiance (mix paper ink wire)))
    (set-output color-output (vec4 radiance 1.0))))
