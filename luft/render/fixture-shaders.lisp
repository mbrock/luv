(in-package #:luft.render.shaders)

;;; Fixture bodies (fixture-body.lisp): canonical object triangles,
;;; instanced at body cells and lit like the stars around them.  An instance
;;; is four Vec4 rows: cell corner and code, side tone and yaw, top tone and
;;; seed, sampled voxel light. A vertex is two: position/part and normal/U.

(define-shader-function fixture-turn (local yaw)
  "LOCAL turned by YAW about its cell's vertical centre line."
  (let* ((c (cos yaw))
         (s (sin yaw))
         (x (- (swizzle local :x) 0.5))
         (y (- (swizzle local :y) 0.5)))
    (vec3 (+ 0.5 (- (* c x) (* s y)))
          (+ 0.5 (+ (* s x) (* c y)))
          (swizzle local :z))))

(define-shader-function fixture-turn-direction (direction yaw)
  (let* ((c (cos yaw))
         (s (sin yaw))
         (x (swizzle direction :x))
         (y (swizzle direction :y)))
    (vec3 (- (* c x) (* s y)) (+ (* s x) (* c y)) (swizzle direction :z))))

(define-live-shader fixture-body-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (world-position-output :vec3 :location 0
                                      :quantity quantities:world-position
                                      :unit quantities:cell)
               (mesh-normal-output :vec3 :location 1
                                   :quantity quantities:world-orientation
                                   :unit :one)
               (current-clip-output :vec4 :location 2)
               (previous-clip-output :vec4 :location 3)
               (shadow-sample-output :vec3 :location 4
                                     :quantity quantities:shadow-coordinate
                                     :unit :one)
               (local-output :vec4 :location 5)
               (part-output :vec4 :location 6 :interpolation :flat)
               (side-tone-output :vec3 :location 7 :interpolation :flat)
               (top-tone-output :vec3 :location 8 :interpolation :flat)
               (voxel-light-output :vec3 :location 9 :interpolation :flat))
     :resources
     ((fixture-bodies :storage-buffer :binding 0 :element :vec4)
      (fixture-body-vertices :storage-buffer :binding 1 :element :vec4)
      (camera-state :uniform-block :binding 2
       :members #.(scene-uniform-prefix 23))))
  (let* ((body-base (* instance-index (uint 4.0)))
         (corner-row (buffer-element fixture-bodies body-base))
         (side-row (buffer-element fixture-bodies (+ body-base (uint 1.0))))
         (top-row (buffer-element fixture-bodies (+ body-base (uint 2.0))))
         (yaw (swizzle side-row :w))
         (vertex-base (* vertex-index (uint 2.0)))
         (position-row (buffer-element fixture-body-vertices vertex-base))
         (normal-row (buffer-element fixture-body-vertices
                                     (+ vertex-base (uint 1.0))))
         (world-position
           (assume-quantity
            (+ (swizzle corner-row :xyz)
               (fixture-turn (swizzle position-row :xyz) yaw))
            :quantity quantities:world-position :unit quantities:cell))
         (world-normal
           (assume-quantity
            (normalize (fixture-turn-direction (swizzle normal-row :xyz) yaw))
            :quantity quantities:world-orientation :unit :one))
         (divisor (swizzle (representation render-parameters) :z))
         (current-clip
           (mesh-view-clip world-position camera-position camera-right
                           camera-up camera-forward camera-projection divisor))
         (previous-clip
           (mesh-view-clip world-position previous-camera-position
                           previous-camera-right previous-camera-up
                           previous-camera-forward previous-camera-projection
                           divisor))
         (light-clip
           (light-clip-position world-position shadow-row-x shadow-row-y
                                shadow-row-z shadow-row-w))
         (jitter (representation (swizzle temporal-parameters :xy))))
    (set-output clip-position
                (vec4 (+ (swizzle current-clip :x)
                         (* (swizzle jitter :x) (swizzle current-clip :w)))
                      (+ (swizzle current-clip :y)
                         (* (swizzle jitter :y) (swizzle current-clip :w)))
                      (swizzle current-clip :z)
                      (swizzle current-clip :w)))
    (set-output world-position-output world-position)
    (set-output mesh-normal-output world-normal)
    (set-output current-clip-output current-clip)
    (set-output previous-clip-output previous-clip)
    (set-output shadow-sample-output
                (assume-quantity
                 (vec3 (+ (* (swizzle light-clip :x) 0.5) 0.5)
                       (+ (* (swizzle light-clip :y) 0.5) 0.5)
                       (swizzle light-clip :z))
                 :quantity quantities:shadow-coordinate :unit :one))
    (set-output local-output
                (vec4 (swizzle position-row :xyz) (swizzle normal-row :w)))
    (set-output part-output
                (vec4 (swizzle position-row :w) (swizzle top-row :w) 0.0 0.0))
    (set-output side-tone-output (swizzle side-row :xyz))
    (set-output top-tone-output (swizzle top-row :xyz))
    (set-output voxel-light-output
                (swizzle (buffer-element fixture-bodies (+ body-base (uint 3.0))) :xyz))))

(define-live-shader fixture-body-shadow-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position))
     :resources
     ((fixture-bodies :storage-buffer :binding 0 :element :vec4)
      (fixture-body-vertices :storage-buffer :binding 1 :element :vec4)
      (camera-state :uniform-block :binding 2
       :members #.(scene-uniform-prefix 23))))
  (let* ((body-base (* instance-index (uint 4.0)))
         (corner-row (buffer-element fixture-bodies body-base))
         (side-row (buffer-element fixture-bodies (+ body-base (uint 1.0))))
         (position-row
           (buffer-element fixture-body-vertices (* vertex-index (uint 2.0))))
         (world-position
           (assume-quantity
            (+ (swizzle corner-row :xyz)
               (fixture-turn (swizzle position-row :xyz) (swizzle side-row :w)))
            :quantity quantities:world-position :unit quantities:cell)))
    (set-output clip-position
                (light-clip-position world-position
                                     shadow-row-x shadow-row-y
                                     shadow-row-z shadow-row-w))))

;;; Wood and iron, part by part.  Every pattern is in the body's own
;;; coordinates, so it turns with the body.

(define-shader-function fixture-board-tone (wood coordinate across-scale seed salt)
  "WOOD cut into boards along COORDINATE, with seams, variety, and grain."
  (let* ((boards coordinate)
         (across (fract boards))
         (seam (smoothstep 0.0 0.08 (min across (- 1.0 across))))
         (variety (paper-hash (vec3 (floor boards) (* seed 97.0) salt)))
         (grain (paper-noise (vec3 (* boards 5.0) (* across-scale 2.5)
                                   (+ (* seed 31.0) salt))))
         (streak (paper-noise (vec3 (* boards 17.0) (* across-scale 0.8)
                                    (+ (* seed 13.0) salt)))))
    (* wood
       (* (+ 0.74 (* 0.36 variety))
          (* (+ 0.84 (* 0.26 grain))
             (* (+ 0.9 (* 0.16 streak)) (mix 0.32 1.0 seam)))))))

(define-shader-function fixture-iron-tone (local seed)
  (let* ((mottle (paper-noise (* (+ local (vec3 seed seed seed)) 23.0)))
         (rust (paper-noise (* (+ local (vec3 seed 0.0 seed)) 7.0))))
    (mix (vec3 0.035 0.034 0.034) (vec3 0.11 0.05 0.02)
         (* (smoothstep 0.55 0.85 rust) (+ 0.6 (* 0.4 mottle))))))

(define-shader-function fixture-part-wood (part side top)
  "The wood tone of PART: staves and frames are side wood, heads and panels top."
  (if (< part 0.5) (* side 1.35)
      (if (< part 2.5) (* top 1.2)
          (if (< part 3.5) (* side 0.6)
              (if (< part 4.5) (* side 0.95)
                  (if (< part 5.5) (* top 1.15) (* side 1.05)))))))

(define-shader-function fixture-part-boards (part local u normal)
  "Which board of PART a point lies in, and along it: a Vec2."
  (if (< part 0.5)
      (vec2 u (swizzle local :z))
      (if (< part 2.5)
          (vec2 (* (swizzle local :y) 6.0) (swizzle local :x))
          (if (< part 4.5)
              (vec2 (+ (* (swizzle local :x) 0.5) (* (swizzle local :y) 0.5))
                    (swizzle local :z))
              (if (< part 5.5)
                  (vec2 (* (- (if (> (abs (swizzle normal :z)) 0.5)
                                  (swizzle local :x) (swizzle local :z))
                              0.06)
                           4.55)
                        (+ (swizzle local :x) (swizzle local :y)))
                  (vec2 (+ (swizzle local :x) (swizzle local :y))
                        (swizzle local :z)))))))

(define-shader-function fixture-part-tone (part local u normal seed side top)
  (let* ((boards (fixture-part-boards part local u normal))
         (wood (fixture-board-tone (fixture-part-wood part side top)
                                   (swizzle boards :x) (swizzle boards :y)
                                   seed part)))
    (if (< (abs (- part 1.0)) 0.5)
        (fixture-iron-tone local seed)
        (if (< (abs (- part 3.0)) 0.5) (* side 0.6) wood))))

(define-shader-function fixture-object-tone (part local side top time)
  "Object finishes; the clock advects fluid streaks and scrolls phosphor rows."
  (if (< part 7.5) (* side 0.45)
      (if (< part 8.5)
          (let* ((row (floor (+ (* (swizzle local :z) 24.0) (floor (* time 2.0)))))
                 (mark (* (step 0.3 (fract (* (swizzle local :z) 24.0)))
                          (step 0.35 (fract (+ (* (swizzle local :x) 31.0)
                                               (* row 0.37)))))))
            (+ (vec3 0.005 0.008 0.006) (* top mark)))
          (if (< part 9.5) (vec3 0.35 0.34 0.31)
              (if (< part 10.5) (vec3 0.035 0.022 0.014)
                  (if (< part 11.5) (* side 0.7)
                      (* top (+ 0.65 (* 0.35 (sin (+ (* (swizzle local :z) 45.0)
                                                     (* time -7.0)
                                                     (* (swizzle local :x) 18.0))))))))))))

(define-live-shader fixture-body-fragment-specification
    (:stage :fragment
     :inputs ((world-position :vec3 :location 0
                              :quantity quantities:world-position
                              :unit quantities:cell)
              (mesh-normal :vec3 :location 1
                           :quantity quantities:world-orientation :unit :one)
              (current-clip :vec4 :location 2)
              (previous-clip :vec4 :location 3)
              (shadow-sample :vec3 :location 4
                             :quantity quantities:shadow-coordinate :unit :one)
              (local-coordinates :vec4 :location 5)
              (part-info :vec4 :location 6 :interpolation :flat)
              (side-tone :vec3 :location 7 :interpolation :flat)
              (top-tone :vec3 :location 8 :interpolation :flat)
              (voxel-light :vec3 :location 9 :interpolation :flat))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 30))
                 (shadow-map :depth-texture-2d :binding 4)
                 (shadow-sampler :sampler :binding 5)))
  (let* ((normal (normalize (representation mesh-normal)))
         (world (representation world-position))
         (sun (representation (swizzle sun-vector :xyz)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (visibility
           (soft-shadow-visibility
            shadow-map shadow-sampler shadow-sample normal sun
            (representation shadow-control)
            shadow-row-x shadow-row-y shadow-row-z))
         (part (swizzle part-info :x))
         (seed (swizzle part-info :y))
         (local (swizzle local-coordinates :xyz))
         (tone (if (< part 6.5)
                   (fixture-part-tone part local (swizzle local-coordinates :w)
                                      normal seed side-tone top-tone)
                   (fixture-object-tone part local side-tone top-tone
                                        (swizzle atmosphere-parameters :x))))
         ;; Where a body meets the ground its foot gathers shade.
         (contact (mix 0.55 1.0 (smoothstep 0.0 0.22 (swizzle local :z))))
         (metal (or (< (abs (- part 1.0)) 0.5) (< (abs (- part 9.0)) 0.5)))
         (roughness (if (> part 11.5) 0.18
                        (if metal 0.28 0.85)))
         (eye (representation camera-position))
         (forward (representation (swizzle camera-forward :xyz)))
         (divisor (swizzle (representation render-parameters) :z))
         (view (normalize (mix (- forward) (- (swizzle eye :xyz) world)
                               divisor)))
         (radiance
           (+ (if metal
                  (* tone (* contact
                             (dot (representation (swizzle sky-color-vector :xyz))
                                  (vec3 0.2126 0.7152 0.0722))))
                  (paper-surface-radiance
                   (* tone contact) normal world visibility sun sun-color
                   (representation (swizzle sky-color-vector :xyz))
                   (representation (swizzle ground-color-vector :xyz))))
              (* sun-color
                 (* visibility (material-specular normal view sun roughness)))
              (* tone (if (< (abs (- part 8.0)) 0.5) 1.0
                          (if (> part 12.5) 1.8 0.0)))
              (* (* tone contact)
                 (* voxel-light-gain (* voxel-light voxel-light)))))
         (final
           (atmospheric-surface
            radiance world eye divisor
            (representation character-parameters)
            sun (representation fog-color-vector)
            (representation zenith-color-vector)
            atmosphere-parameters)))
    (set-output color-output (vec4 final 1.0))
    (set-output motion-output
                (mesh-temporal-motion previous-clip current-clip))))
