(in-package #:luft.render.shaders)

;;; Shared attachment transforms, the opaque body and its shadow, then the
;;; flame field and its vertex/fragment entry points. All read the frame ABI
;;; in torch-frame.lisp; composition and GPU ownership live in torch-drawing.

(define-shader-function torch-frame-bitangent (normal tangent)
  "Derive B=NxT, so T/B/N is right-handed for every realized frame."
  (interpret
   (vec3 (- (* (swizzle normal :y) (swizzle tangent :z))
            (* (swizzle normal :z) (swizzle tangent :y)))
         (- (* (swizzle normal :z) (swizzle tangent :x))
            (* (swizzle normal :x) (swizzle tangent :z)))
         (- (* (swizzle normal :x) (swizzle tangent :y))
            (* (swizzle normal :y) (swizzle tangent :x))))
   :quantity quantities:world-direction :unit :one))

(define-shader-function torch-frame-world-position
    (local-position origin normal tangent scale)
  (let* ((bitangent (torch-frame-bitangent normal tangent))
         ;; SCALE names the canonical-to-world transform.  Applying it to a
         ;; canonical coordinate realizes an ordinary world distance.
         (world-scale
           (assume-quantity (representation scale)
                            :quantity quantities:world-distance
                            :unit quantities:cell)))
    (+ origin
       (interpret
        (* world-scale
           (+ (* tangent (swizzle local-position :x))
              (* bitangent (swizzle local-position :y))
              (* normal (swizzle local-position :z))))
        :quantity quantities:world-position :unit quantities:cell
        :character :difference))))

(define-shader-function torch-frame-world-normal
    (local-normal normal tangent)
  (let* ((bitangent (torch-frame-bitangent normal tangent)))
    (normalize
     (+ (* tangent (swizzle local-normal :x))
        (* bitangent (swizzle local-normal :y))
        (* normal (swizzle local-normal :z))))))

(define-live-shader torch-body-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (world-position-output :vec3 :location 0
                                      :quantity quantities:world-position
                                      :unit quantities:cell)
               (mesh-normal-output :vec3 :location 1 :interpolation :flat
                                   :quantity quantities:world-orientation
                                   :unit :one)
               (current-clip-output :vec4 :location 4)
               (previous-clip-output :vec4 :location 5)
               (shadow-sample-output :vec3 :location 6
                                     :quantity quantities:shadow-coordinate
                                     :unit :one)
               (voxel-light-output :vec3 :location 9))
     :resources
     ((torch-frames :storage-buffer :binding 0 :element :vec4)
      (torch-body-vertices :storage-buffer :binding 1 :element :vec4)
      (camera-state :uniform-block :binding 2
       :members #.(scene-uniform-prefix 23))))
  (let* ((frame-base
           (* instance-index
              (uint #.luft.render::+torch-flame-instance-row-count+)))
         (origin-row (buffer-element torch-frames frame-base))
         (frame-normal-row
           (buffer-element torch-frames (+ frame-base (uint 1.0))))
         (tangent-row
           (buffer-element torch-frames (+ frame-base (uint 2.0))))
         ;; Storage buffers deliberately expose raw Vec4 rows.  These four
         ;; assumptions are the shader side of the host product declaration;
         ;; categorical SEED and FLAGS stay raw.
         (origin
           (assume-quantity (swizzle origin-row :xyz)
                            :quantity quantities:world-position
                            :unit quantities:cell))
         (normal
           (assume-quantity (swizzle frame-normal-row :xyz)
                            :quantity quantities:world-direction :unit :one))
         (tangent
           (assume-quantity (swizzle tangent-row :xyz)
                            :quantity quantities:world-direction :unit :one))
         (scale
           (assume-quantity (swizzle tangent-row :w)
                            :quantity quantities:spatial-scale
                            :unit quantities:cell))
         (frame-flags (uint (swizzle frame-normal-row :w)))
         (packed-light
           (ldb (byte #.luft.render::+torch-body-light-bit-count+ 0)
                frame-flags))
         (voxel-light
           (/ (vec3 (float (ldb (byte 4 0) packed-light))
                    (float (ldb (byte 4 4) packed-light))
                    (float (ldb (byte 4 8) packed-light)))
              15.0))
         (vertex-base
           (* vertex-index
              (uint #.luft.render::+torch-body-vertex-row-count+)))
         (local-position-row
           (buffer-element torch-body-vertices vertex-base))
         (local-normal-row
           (buffer-element torch-body-vertices
                           (+ vertex-base (uint 1.0))))
         (local-position
           (assume-quantity (swizzle local-position-row :xyz) :unit :one))
         (local-normal
           (assume-quantity (swizzle local-normal-row :xyz) :unit :one))
         (world-position
           (torch-frame-world-position
            local-position origin normal tangent scale))
         ;; A unit lighting normal is also a valid member of the mesh
         ;; interface's broader set of (possibly diagonal) orientation
         ;; witnesses.  Reclassifying it here changes no representation.
         (world-normal
           (assume-quantity
            (representation
             (torch-frame-world-normal local-normal normal tangent))
            :quantity quantities:world-orientation :unit :one))
         (current-clip
           (mesh-view-clip world-position camera-position camera-right
                           camera-up camera-forward camera-projection
                           (swizzle (representation render-parameters) :z)))
         (previous-clip
           (mesh-view-clip world-position previous-camera-position
                           previous-camera-right previous-camera-up
                           previous-camera-forward previous-camera-projection
                           (swizzle (representation render-parameters) :z)))
         (light-clip
           (light-clip-position world-position shadow-row-x shadow-row-y
                                shadow-row-z shadow-row-w))
         ;; Homogeneous clip coordinates are a representation-only projection
         ;; result, so erase the checked normalized jitter at this boundary.
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
    (set-output voxel-light-output voxel-light)))

(define-live-shader torch-body-shadow-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position))
     :resources
     ((torch-frames :storage-buffer :binding 0 :element :vec4)
      (torch-body-vertices :storage-buffer :binding 1 :element :vec4)
      (camera-state :uniform-block :binding 2
       :members #.(scene-uniform-prefix 23))))
  (let* ((frame-base
           (* instance-index
              (uint #.luft.render::+torch-flame-instance-row-count+)))
         (origin-row (buffer-element torch-frames frame-base))
         (frame-normal-row
           (buffer-element torch-frames (+ frame-base (uint 1.0))))
         (tangent-row
           (buffer-element torch-frames (+ frame-base (uint 2.0))))
         (origin
           (assume-quantity (swizzle origin-row :xyz)
                            :quantity quantities:world-position
                            :unit quantities:cell))
         (normal
           (assume-quantity (swizzle frame-normal-row :xyz)
                            :quantity quantities:world-direction :unit :one))
         (tangent
           (assume-quantity (swizzle tangent-row :xyz)
                            :quantity quantities:world-direction :unit :one))
         (scale
           (assume-quantity (swizzle tangent-row :w)
                            :quantity quantities:spatial-scale
                            :unit quantities:cell))
         (vertex-base
           (* vertex-index
              (uint #.luft.render::+torch-body-vertex-row-count+)))
         (local-position-row
           (buffer-element torch-body-vertices vertex-base))
         (world-position
           (torch-frame-world-position
            (assume-quantity (swizzle local-position-row :xyz) :unit :one)
            origin normal tangent scale)))
    (set-output clip-position
                (light-clip-position world-position
                                     shadow-row-x shadow-row-y
                                     shadow-row-z shadow-row-w))))

(define-live-shader torch-body-fragment-specification
    (:stage :fragment
     :inputs ((world-position :vec3 :location 0
                              :quantity quantities:world-position
                              :unit quantities:cell)
              (mesh-normal :vec3 :location 1 :interpolation :flat
                           :quantity quantities:world-orientation :unit :one)
              (current-clip :vec4 :location 4)
              (previous-clip :vec4 :location 5)
              (shadow-sample :vec3 :location 6
                             :quantity quantities:shadow-coordinate :unit :one)
              (voxel-light :vec3 :location 9))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 23))
                 (shadow-map :depth-texture-2d :binding 4)
                 (shadow-sampler :sampler :binding 5)))
  (let* ((normal (normalize (representation mesh-normal)))
         (sun (representation (swizzle sun-vector :xyz)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (sky (representation (swizzle sky-color-vector :xyz)))
         (ground (representation (swizzle ground-color-vector :xyz)))
         (facing (max 0.0 (dot normal sun)))
         (visibility
           (soft-shadow-visibility
            shadow-map shadow-sampler shadow-sample normal sun
            (representation shadow-control)))
         (upness (swizzle normal :z))
         (sky-weight (+ 0.5 (* 0.5 upness)))
         (ambient (+ (* ground (- 1.0 sky-weight)) (* sky sky-weight)))
         (bronze (vec3 0.47 0.17 0.04))
         (local-light (* 0.45 (* voxel-light voxel-light)))
         (radiance
           (* bronze
              (+ (* ambient 0.72)
                 (* sun-color (* visibility facing))
                 local-light)))
         (camera-delta
           (representation
            (- world-position (swizzle camera-position :xyz))))
         (distance (sqrt (dot camera-delta camera-delta)))
         (fog (smoothstep 165.0 300.0 distance)))
    (set-output color-output (vec4 (mix radiance sky fog) 1.0))
    (set-output motion-output
                (mesh-temporal-motion previous-clip current-clip))))

(define-shader-function torch-flame-field
    (point origin normal tangent seed scale time)
  "Return signed distance, density, heat, and radius at POINT."
  (let* ((bitangent (torch-frame-bitangent normal tangent))
         (world-up-projection
           (- (quantity (vec3 0.0 0.0 1.0)
                        :quantity quantities:world-direction :unit :one)
              (interpret
               (* normal (swizzle normal :z))
               :quantity quantities:world-direction :unit :one)))
         (gravity-strength
           (assume-quantity
            (sqrt
             (representation
              (max (dot world-up-projection world-up-projection) 0.0)))
            :unit :one))
         (gravity-direction
           ;; A horizontal surface has no projected gravity direction.  Keep
           ;; the zero fallback as a dimensionless displacement coefficient,
           ;; rather than falsely calling it a unit world direction.
           (assume-quantity
            (if (> gravity-strength 1e-6)
                (representation
                 (/ world-up-projection gravity-strength))
                (vec3 0.0 0.0 0.0))
            :unit :one :character :difference))
         ;; A frame scale is a transform coefficient.  Once applied to this
         ;; canonical effect it realizes distances in the world lattice.
         (world-scale
           (assume-quantity (representation scale)
                            :quantity quantities:world-distance
                            :unit quantities:cell))
         (flame-length
           (* #.luft.render::+torch-flame-length+ world-scale))
         (wick
           (+ origin
              (interpret
               (* normal
                  (* #.luft.render::+torch-flame-wick-offset+ world-scale))
               :quantity quantities:world-position :unit quantities:cell
               :character :difference)))
         (offset (- point wick))
         (axial (/ (dot offset normal) flame-length))
         (height (clamp axial 0.0 1.0))
         (height-squared (* height height))
         (sway-u
           (* world-scale 0.105 height-squared
              (assume-quantity
               (sin (+ (* time 5.1) (* seed 19.7)
                       (* (representation height) 5.3)))
               :unit :one)))
         (sway-v
           (* world-scale 0.075 height-squared
              (assume-quantity
               (sin (+ (* time 6.7) (* seed 31.1)
                       (* (representation height) 7.1)))
               :unit :one)))
         (gravity-bend
           (* world-scale #.luft.render::+torch-flame-wall-bend+
              gravity-strength height-squared))
         (center
           (+ wick
              (interpret (* normal (* flame-length height))
                         :quantity quantities:world-position
                         :unit quantities:cell :character :difference)
              (interpret (* tangent sway-u)
                         :quantity quantities:world-position
                         :unit quantities:cell :character :difference)
              (interpret (* bitangent sway-v)
                         :quantity quantities:world-position
                         :unit quantities:cell :character :difference)
              (interpret (* gravity-direction gravity-bend)
                         :quantity quantities:world-position
                         :unit quantities:cell :character :difference)))
         (radial
           (assume-quantity
            (sqrt
             (max
              (representation
               (dot (- point center) (- point center)))
              1e-12))
            :quantity quantities:world-distance :unit quantities:cell))
         (bulge (smoothstep 0.0 0.22 height))
         (radius
           (* world-scale (- 1.0 height) (+ 0.055 (* 0.13 bulge))))
         (signed-distance (- radial radius))
         (begin (smoothstep -0.02 0.08 axial))
         (end (- 1.0 (smoothstep 0.78 1.04 axial)))
         (inside
           (- 1.0
              (smoothstep -0.025 0.035 (representation signed-distance))))
         (wave
           (+ 0.5
              (* 0.5
                 (sin (+ (* (representation (swizzle point :x)) 17.1)
                         (* (representation (swizzle point :y)) 13.7)
                         (* (representation (swizzle point :z)) 19.3)
                         (* time -8.1) (* seed 23.9))))))
         (fine
           (+ 0.5
              (* 0.5
                 (sin (+ (* (representation (swizzle point :x)) -31.7)
                         (* (representation (swizzle point :y)) 27.3)
                         (* (representation (swizzle point :z)) 23.1)
                         (* time 11.3) (* seed 7.7))))))
         (density (* (representation begin) (representation end) inside
                     (+ 0.68 (* 0.22 wave) (* 0.10 fine))))
         (centrality
           (clamp
            (/ (- signed-distance)
               (max radius
                    (quantity 0.001
                              :quantity quantities:world-distance
                              :unit quantities:cell)))
            0.0 1.0))
         (heat (clamp (+ 0.20 (* 0.95 centrality) (* -0.32 height))
                      0.0 1.0)))
    ;; The return Vec4 is intentionally a heterogeneous private
    ;; representation: distance, density, heat, and radius are unpacked by
    ;; name at the only call site.
    (vec4 (representation signed-distance) density
          (representation heat) (representation radius))))

(define-live-shader torch-flame-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (proxy-world-position-output :vec3 :location 0
                                            :quantity quantities:world-position
                                            :unit quantities:cell)
               (origin-output :vec3 :location 1 :interpolation :flat
                              :quantity quantities:world-position
                              :unit quantities:cell)
               (normal-output :vec3 :location 2 :interpolation :flat
                              :quantity quantities:world-direction :unit :one)
               (tangent-output :vec3 :location 3 :interpolation :flat
                               :quantity quantities:world-direction :unit :one)
               (frame-parameters-output :vec2 :location 4
                                        :interpolation :flat)
               (current-clip-output :vec4 :location 5))
     :resources
     ((flame-instances :storage-buffer :binding 0 :element :vec4)
      (camera-state :uniform-block :binding 1
       :members #.(scene-uniform-prefix 6))))
  (let* ((base-row (* instance-index (uint 3.0)))
         (origin-row (buffer-element flame-instances base-row))
         (normal-row
           (buffer-element flame-instances (+ base-row (uint 1.0))))
         (tangent-row
           (buffer-element flame-instances (+ base-row (uint 2.0))))
         (origin
           (assume-quantity (swizzle origin-row :xyz)
                            :quantity quantities:world-position
                            :unit quantities:cell))
         (seed (swizzle origin-row :w))
         (normal
           (assume-quantity (swizzle normal-row :xyz)
                            :quantity quantities:world-direction :unit :one))
         (tangent
           (assume-quantity (swizzle tangent-row :xyz)
                            :quantity quantities:world-direction :unit :one))
         (scale
           (assume-quantity (swizzle tangent-row :w)
                            :quantity quantities:spatial-scale
                            :unit quantities:cell))
         (world-scale
           (assume-quantity (representation scale)
                            :quantity quantities:world-distance
                            :unit quantities:cell))
         (proxy-radius
           (* #.luft.render::+torch-flame-proxy-radius+ world-scale))
         (volume-center
           (+ origin
              (interpret
               (* normal
                  (* world-scale
                     (assume-quantity
                      (+ #.luft.render::+torch-flame-wick-offset+
                         (* #.luft.render::+torch-flame-length+ 0.5))
                      :unit :one)))
               :quantity quantities:world-position :unit quantities:cell
               :character :difference)))
         (index (float vertex-index))
         (right-corner (if (= index 2.0) 1.0
                           (if (= index 3.0) 1.0
                               (if (= index 5.0) 1.0 0.0))))
         (bottom-corner (if (= index 1.0) 1.0
                            (if (= index 4.0) 1.0
                                (if (= index 5.0) 1.0 0.0))))
         (corner (vec2 (- (* right-corner 2.0) 1.0)
                       (- (* bottom-corner 2.0) 1.0)))
         (proxy-world-position
           (+ (- volume-center
                 (interpret
                  (* (swizzle camera-forward :xyz) proxy-radius)
                  :quantity quantities:world-position :unit quantities:cell
                  :character :difference))
              (interpret
               (* (swizzle camera-right :xyz)
                  (* (assume-quantity (swizzle corner :x) :unit :one)
                     proxy-radius))
               :quantity quantities:world-position :unit quantities:cell
               :character :difference)
              (interpret
               (* (swizzle camera-up :xyz)
                  (* (assume-quantity (swizzle corner :y) :unit :one)
                     proxy-radius))
               :quantity quantities:world-position :unit quantities:cell
               :character :difference)))
         (current-clip
           (mesh-view-clip proxy-world-position camera-position camera-right
                           camera-up camera-forward camera-projection
                           (swizzle (representation render-parameters) :z))))
    ;; Procedural radiance is a post-temporal composite.  Rasterize the stable,
    ;; unjittered proxy and never author a motion/history footprint for it.
    (set-output clip-position current-clip)
    (set-output proxy-world-position-output proxy-world-position)
    (set-output origin-output origin)
    (set-output normal-output normal)
    (set-output tangent-output tangent)
    ;; The packed varying remains heterogeneous: SEED is categorical while
    ;; SCALE is re-assumed from its Y lane by the fragment stage.
    (set-output frame-parameters-output (vec2 seed (representation scale)))
    (set-output current-clip-output current-clip)))

(define-live-shader torch-flame-fragment-specification
    (:stage :fragment
     :inputs ((proxy-world-position :vec3 :location 0
                                    :quantity quantities:world-position
                                    :unit quantities:cell)
              (origin :vec3 :location 1 :interpolation :flat
                      :quantity quantities:world-position
                      :unit quantities:cell)
              (normal :vec3 :location 2 :interpolation :flat
                      :quantity quantities:world-direction :unit :one)
              (tangent :vec3 :location 3 :interpolation :flat
                       :quantity quantities:world-direction :unit :one)
              (frame-parameters :vec2 :location 4 :interpolation :flat)
              (current-clip :vec4 :location 5))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((camera-state :uniform-block :binding 1
       :members #.(scene-uniform-prefix 13))
      (effect-state :uniform-block :binding 2
       :members
       ((flame-effect-parameters :vec4
         :components
         ((:x :quantity quantities:elapsed-time :unit :second)
          (:yzw :quantity quantities:scene-radiance :unit :one)))))
      (opaque-depth :depth-texture-2d :binding 3)
      (depth-sampler :sampler :binding 4)))
  (let* ((ray (if (< (swizzle (representation render-parameters) :z) 0.5)
                  (normalize (swizzle camera-forward :xyz))
                  (assume-quantity
                   (normalize
                    (representation
                     (- proxy-world-position
                        (swizzle camera-position :xyz))))
                   :quantity quantities:world-direction :unit :one)))
         ;; Procedural phase constants remain representation-level numbers;
         ;; elapsed seconds are erased only at that explicit animation seam.
         (time (representation (swizzle flame-effect-parameters :x)))
         (authored-radiance (swizzle flame-effect-parameters :yzw))
         (seed (swizzle frame-parameters :x))
         (scale
           (assume-quantity (swizzle frame-parameters :y)
                            :quantity quantities:spatial-scale
                            :unit quantities:cell))
         (world-scale
           (assume-quantity (representation scale)
                            :quantity quantities:world-distance
                            :unit quantities:cell))
         (full-path-length
           (* 2.0 #.luft.render::+torch-flame-proxy-radius+ world-scale))
         ;; Opaque depth was rendered with the current projection jitter while
         ;; this post-temporal proxy is deliberately stable.  Four nearest
         ;; taps conservatively choose the closest covered internal pixel at an
         ;; edge, avoiding bright half-flames leaking through a bevel silhouette.
         (depth-uv
           (+ (representation (mesh-clip-uv current-clip))
              (* (representation (swizzle temporal-parameters :xy)) 0.5)))
         (half-texel
           (* (representation (swizzle inspection-parameters :zw)) 0.5))
         (depth-a
           (swizzle
            (sample opaque-depth depth-sampler (+ depth-uv half-texel)) :x))
         (depth-b
           (swizzle
            (sample opaque-depth depth-sampler (- depth-uv half-texel)) :x))
         (depth-c
           (swizzle
            (sample opaque-depth depth-sampler
                    (+ depth-uv
                       (vec2 (swizzle half-texel :x)
                             (- (swizzle half-texel :y))))) :x))
         (depth-d
           (swizzle
            (sample opaque-depth depth-sampler
                    (+ depth-uv
                       (vec2 (- (swizzle half-texel :x))
                             (swizzle half-texel :y)))) :x))
         (scene-depth (min (min depth-a depth-b) (min depth-c depth-d)))
         (opaque-view-depth
           (assume-quantity
            (view-depth scene-depth camera-projection
                        (swizzle (representation render-parameters) :z))
            :unit quantities:cell))
         (proxy-view-depth
           (dot (- proxy-world-position (swizzle camera-position :xyz))
                (swizzle camera-forward :xyz)))
         (ray-view-rate
           (max (dot ray (swizzle camera-forward :xyz)) 1e-5))
         (path-length
           (interpret
            (clamp
             (/ (- opaque-view-depth proxy-view-depth) ray-view-rate)
             (quantity 0.0 :unit quantities:cell)
             (assume-quantity (representation full-path-length)
                              :unit quantities:cell))
            :quantity quantities:world-distance :unit quantities:cell))
         (step-length (/ path-length
                         (assume-quantity
                          (float #.luft.render::+torch-flame-sample-count+)
                          :unit :one)))
         (integrated
           (counted-fold
               (sample (float #.luft.render::+torch-flame-sample-count+)
                state (vec4 0.0 0.0 0.0 0.0))
             (let* ((travel
                      (* (assume-quantity (+ sample 0.5) :unit :one)
                         step-length))
                    (point
                      (+ proxy-world-position
                         (interpret
                          (* ray travel)
                          :quantity quantities:world-position
                          :unit quantities:cell :character :difference)))
                    (field (torch-flame-field
                            point origin normal tangent seed scale time))
                    (density (swizzle field :y))
                    (heat (swizzle field :z))
                    (sample-alpha
                      (- 1.0
                         (exp (- (* density
                                    #.luft.render::+torch-flame-extinction+
                                    (representation step-length))))))
                    (transmittance (- 1.0 (swizzle state :w)))
                    (weight (* transmittance sample-alpha))
                    (radiance-scale
                      (+ #.luft.render::+torch-flame-cool-radiance-scale+
                         (* heat
                            #.luft.render::+torch-flame-heat-radiance-gain+)))
                    (emission
                      (* authored-radiance
                         (assume-quantity radiance-scale :unit :one))))
               ;; Fold state is the heterogeneous packed representation of
               ;; radiance XYZ plus opacity W; erase only at that packing seam.
               (vec4 (+ (swizzle state :xyz)
                        (* (representation emission) weight))
                     (+ (swizzle state :w) weight))))))
    (set-output color-output integrated)))
