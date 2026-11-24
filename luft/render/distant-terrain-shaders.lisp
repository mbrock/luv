(in-package #:luft.render.shaders)

;;; Distant terrain: terraced voxel columns rendered by instanced vertex
;;; pulling. Each 16-byte uvec4 record describes one axis-aligned column top or
;;; cliff skirt quad in world cells, addressed directly into the renderer's
;;; shared material-descriptors buffer and shaded with the same sun, cloud
;;; shadows, material character, and aerial perspective as near terrain.

(define-live-shader distant-terrain-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position :invariant t)
               (world-position-output :vec3 :location 0
                                      :quantity quantities:world-position
                                      :unit quantities:cell)
               (mesh-normal-output :vec3 :location 1 :interpolation :flat
                                   :quantity quantities:world-orientation :unit :one)
               (current-clip-output :vec4 :location 2)
               (previous-clip-output :vec4 :location 3)
               (material-tone-output :vec3 :location 4 :interpolation :flat)
               (material-info-output :vec4 :location 5 :interpolation :flat)
               (top-tone-output :vec3 :location 6 :interpolation :flat))
     :resources ((distant-quads :storage-buffer :binding 0 :element :uvec4)
                 (material-descriptors :storage-buffer :binding 1 :element :uvec4)
                 (camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 23))))
  (let* ((quad (buffer-element distant-quads instance-index))
         (x0 (- (float (ldb (byte 24 0) (swizzle quad :x))) 8388608.0))
         (material-code (ldb (byte 8 24) (swizzle quad :x)))
         (y0 (- (float (ldb (byte 24 0) (swizzle quad :y))) 8388608.0))
         (top-code (ldb (byte 8 24) (swizzle quad :y)))
         (z0 (- (float (ldb (byte 12 0) (swizzle quad :z))) 1024.0))
         (z1 (- (float (ldb (byte 12 12) (swizzle quad :z))) 1024.0))
         (ao (/ (float (ldb (byte 8 24) (swizzle quad :z))) 255.0))
         (face (ldb (byte 8 0) (swizzle quad :w)))
         (width (float (ldb (byte 8 8) (swizzle quad :w))))
         (depth (float (ldb (byte 8 16) (swizzle quad :w))))
         (u (float (ldb (byte 1 0) vertex-index)))
         (v (float (ldb (byte 1 1) vertex-index)))
         (z-coord (mix z0 z1 v))
         (top-p (= face (uint 0.0)))
         (east-p (= face (uint 1.0)))
         (west-p (= face (uint 2.0)))
         (north-p (= face (uint 3.0)))
         (wx (if top-p (+ x0 (* u width))
                 (if east-p (+ x0 width)
                     (if west-p x0
                         (if north-p (+ x0 (* (- 1.0 u) width))
                             (+ x0 (* u width)))))))
         (wy (if top-p (+ y0 (* v depth))
                 (if east-p (+ y0 (* u depth))
                     (if west-p (+ y0 (* (- 1.0 u) depth))
                         (if north-p (+ y0 depth) y0)))))
         (wz (if top-p z1 z-coord))
         (nx (if east-p 1.0 (if west-p -1.0 0.0)))
         (ny (if north-p 1.0 (if top-p 0.0 (if east-p 0.0 (if west-p 0.0 -1.0)))))
         (nz (if top-p 1.0 0.0))
         (world (assume-quantity (vec3 wx wy wz)
                                 :quantity quantities:world-position
                                 :unit quantities:cell))
         (normal (assume-quantity (vec3 nx ny nz)
                                  :quantity quantities:world-orientation
                                  :unit :one))
         (surface-row (buffer-element material-descriptors (+ material-code material-code)))
         (packed-tone (if top-p (swizzle surface-row :x) (swizzle surface-row :y)))
         (material-tone (unpack-terrain-tone packed-tone))
         (relief (float (ldb #.(luft.terrain::terrain-descriptor-byte :relief-code)
                             (swizzle surface-row :w))))
         (top-row (buffer-element material-descriptors (+ top-code top-code)))
         (top-tone (unpack-terrain-tone (swizzle top-row :x)))
         (top-relief (float (ldb #.(luft.terrain::terrain-descriptor-byte :relief-code)
                                 (swizzle top-row :w))))
         (top-is-grass (if (< (abs (- top-relief 1.0)) 0.5) 1.0 0.0))
         (divisor (swizzle (representation render-parameters) :z))
         (clip (mesh-view-clip world camera-position camera-right camera-up
                               camera-forward camera-projection divisor))
         (previous (mesh-view-clip world previous-camera-position
                                   previous-camera-right previous-camera-up
                                   previous-camera-forward
                                   previous-camera-projection divisor))
         (jitter (representation (swizzle temporal-parameters :xy))))
    (set-output clip-position
                (vec4 (+ (swizzle clip :x) (* (swizzle jitter :x) (swizzle clip :w)))
                      (+ (swizzle clip :y) (* (swizzle jitter :y) (swizzle clip :w)))
                      (swizzle clip :z) (swizzle clip :w)))
    (set-output world-position-output world)
    (set-output mesh-normal-output normal)
    (set-output current-clip-output clip)
    (set-output previous-clip-output previous)
    (set-output material-tone-output material-tone)
    (set-output material-info-output (vec4 relief ao z1 top-is-grass))
    (set-output top-tone-output top-tone)))

(define-live-shader distant-terrain-fragment-specification
    (:stage :fragment
     :inputs ((world-position :vec3 :location 0
                              :quantity quantities:world-position
                              :unit quantities:cell)
              (mesh-normal :vec3 :location 1 :interpolation :flat
                           :quantity quantities:world-orientation :unit :one)
              (current-clip :vec4 :location 2)
              (previous-clip :vec4 :location 3)
              (material-tone :vec3 :location 4 :interpolation :flat)
              (material-info :vec4 :location 5 :interpolation :flat)
              (top-tone :vec3 :location 6 :interpolation :flat))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 34))))
  (let* ((normal (normalize (representation mesh-normal)))
         (world (representation world-position))
         (sun (representation (swizzle sun-vector :xyz)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (visibility
           (terrain-cloud-visibility
            world sun (swizzle atmosphere-parameters :x)
            (swizzle (representation horizon-color-vector) :w)
            (swizzle (representation sun-color-vector) :w)))
         (relief (swizzle material-info :x))
         (ao (swizzle material-info :y))
         (top-z (swizzle material-info :z))
         (top-is-grass (swizzle material-info :w))
         (eye (representation camera-position))
         (forward (representation (swizzle camera-forward :xyz)))
         (divisor (swizzle (representation render-parameters) :z))
         (to-eye (- (swizzle eye :xyz) world))
         (distance (mix 20.0 (sqrt (dot to-eye to-eye)) divisor))
         ;; Grass-topped cliff sides carry a narrow green lip at the summit,
         ;; matching the near terrain's side-face overhang (#HYBFRT).
         (fringe (if (< (abs (swizzle normal :z)) 0.5)
                     (* top-is-grass
                        (smoothstep (- top-z 1.15) (- top-z 0.15)
                                    (swizzle world :z)))
                     0.0))
         (base-tone (mix material-tone top-tone fringe))
         (tone (terrain-material-character base-tone relief world normal distance))
         (sky (* ao (representation (swizzle sky-color-vector :xyz))))
         (ground (* ao (representation (swizzle ground-color-vector :xyz))))
         (radiance (paper-surface-radiance
                    tone normal world visibility sun sun-color sky ground))
         (final
           (atmospheric-surface
            radiance world
            eye forward divisor
            (representation character-parameters)
            sun (representation fog-color-vector)
            (representation zenith-color-vector)
            atmosphere-parameters)))
    (set-output color-output (vec4 final 1.0))
    (set-output motion-output
                (mesh-temporal-motion previous-clip current-clip))))
