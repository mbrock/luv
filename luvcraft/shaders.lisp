;;; The visible block-world materials as typed mathematical specifications.
;;;
;;; These shader methods are luvcraft's hot-replaceable source of truth: the
;;; live pipelines watch their CLOS role/stage coordinates through MOP
;;; dependents and rebuild on redefinition.  The crosshair vertex stage at the
;;; end is a literal SPIR-V module rather than a mathematical specification.

(in-package #:luv.spir-v)

;;; Every stage which reads the frame environment declares the same uniform
;;; block at binding 2: identical member order and offsets are an ABI
;;; requirement, so the member list is written once and spliced at read time.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *frame-uniform-members*
    '((camera-vector :vec4)     ; camera position, w unused
      (right-vector :vec4)
      (up-vector :vec4)
      (forward-vector :vec4)
      (projection-vector :vec4) ; x scale, y scale, z scale, z offset
      (fog-vector :vec4)        ; fog near, fog far, unused, unused
      (sun-vector :vec4)        ; sun direction, day factor
      (sun-color-vector :vec4)  ; sun colour, angular width
      (zenith-vector :vec4)     ; zenith colour, w unused
      (horizon-vector :vec4)    ; horizon colour, w unused
      (ambient-vector :vec4)    ; ambient colour, exposure
      (fog-color-vector :vec4)  ; fog colour, w shadow diagnostic selector
      (shadow-control-vector :vec4) ; texel u/v, base bias, slope bias
      (shadow-filter-vector :vec4) ; depth span, world/texel, min/max radius
      (shadow-row-x :vec4)      ; light-space clip x from world position
      (shadow-row-y :vec4)      ; light-space clip y from world position
      (shadow-row-z :vec4)      ; light-space depth from world position
      (shadow-row-w :vec4))     ; light-space homogeneous w
    "The one frame-environment uniform layout shared by all scene stages."))

(define-shader-method shader-specification-for
    block-world-vertex-specification
    ((role (eql :block-surface)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((world-position :vec3 :location 0)
              (uv-shade-input :vec3 :location 1)
              (normal-input :vec3 :location 2)
              (light-input :vec3 :location 3))
     :outputs ((clip-position :vec4 :built-in :position)
               (uv-shade-output :vec3 :location 0)
               (normal-output :vec3 :location 1)
               (fog-output :float :location 2)
               (light-output :vec3 :location 3)
               (shadow-uv-output :vec2 :location 4)
               (shadow-depth-output :float :location 5))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((camera (swizzle camera-vector :xyz))
         (right (swizzle right-vector :xyz))
         (up (swizzle up-vector :xyz))
         (forward (swizzle forward-vector :xyz))
         (relative (- world-position camera))
         (view-x (dot relative right))
         (view-y (dot relative up))
         (view-z (dot relative forward))
         ;; Fog has explicit near/far semantics: no attenuation before near,
         ;; full fog at far, quadratic shaping between, clamped where the
         ;; scene extends past either edge.
         (fog-near (swizzle fog-vector :x))
         (fog-far (swizzle fog-vector :y))
         (fog-span (- fog-far fog-near))
         (fog-progress (clamp (/ (- view-z fog-near) fog-span) 0.0 1.0))
         (fog-amount (* fog-progress fog-progress))
         (x-scale (swizzle projection-vector :x))
         (y-scale (swizzle projection-vector :y))
         (z-scale (swizzle projection-vector :z))
         (z-offset (swizzle projection-vector :w))
         (clip-x (* view-x x-scale))
         (clip-y (- (* view-y y-scale)))
         (clip-z (+ (* view-z z-scale) z-offset))
         (clip (vec4 clip-x clip-y clip-z view-z))
         (world (vec4 world-position 1.0))
         (shadow-x (dot shadow-row-x world))
         (shadow-y (dot shadow-row-y world))
         (shadow-z (dot shadow-row-z world))
         (shadow-w (dot shadow-row-w world))
         (shadow-ndc-x (/ shadow-x shadow-w))
         (shadow-ndc-y (/ shadow-y shadow-w))
         (shadow-ndc-z (/ shadow-z shadow-w))
         (shadow-u (+ (* shadow-ndc-x 0.5) 0.5))
         (shadow-v (+ (* shadow-ndc-y 0.5) 0.5)))
    (set-output clip-position clip)
    (set-output uv-shade-output uv-shade-input)
    (set-output normal-output normal-input)
    (set-output fog-output fog-amount)
    (set-output light-output light-input)
    (set-output shadow-uv-output (vec2 shadow-u shadow-v))
    (set-output shadow-depth-output shadow-ndc-z)))

(defun block-world-vertex-specification ()
  (shader-specification-for :block-surface :vertex))

(defun block-world-camera-uniform-block ()
  "The frame uniform block exactly as the vertex specification declares it."
  (find-if (lambda (resource) (typep resource 'shader-uniform-block))
           (shader-specification-resources
            (block-world-vertex-specification))))

(defun block-world-vertex-lowering ()
  "Compile the camera transform and retain expression-to-SSA provenance."
  (compile-shader-specification (block-world-vertex-specification)))

(defun block-world-vertex-module ()
  (shader-lowering-module (block-world-vertex-lowering)))

(defun block-world-vertex-shader ()
  (assemble-spir-v-module (block-world-vertex-module)))

(define-shader-method shader-specification-for
    block-world-fragment-specification
    ((role (eql :block-surface)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((uv-shade-input :vec3 :location 0)
              (normal-input :vec3 :location 1)
              (fog-input :float :location 2)
              (light-input :vec3 :location 3)
              (shadow-uv-input :vec2 :location 4)
              (shadow-depth-input :float :location 5))
     :outputs ((color-output :vec4 :location 0))
     :resources ((block-atlas :texture-2d :set 0 :binding 0)
                 (block-sampler :sampler :set 0 :binding 1)
                 (frame-state :uniform-block :set 0 :binding 2
                              :members #.*frame-uniform-members*)
                 (shadow-map :depth-texture-2d :set 0 :binding 3)
                 (shadow-sampler :sampler :set 0 :binding 4)
                 (shadow-comparison-sampler :sampler :set 0 :binding 5)))
  (let* ((uv-shade uv-shade-input)
         (uv
           (interpret (swizzle uv-shade :xy)
                      :quantity :texture-uv :affine-p t))
         (ao
           (interpret (swizzle uv-shade :z)
                      :quantity :ambient-occlusion))
         (normal
           (interpret normal-input :quantity :world-direction))
         (sun-direction
           (interpret (swizzle sun-vector :xyz)
                      :quantity :world-direction))
         (n-dot-l (max 0.0 (dot normal sun-direction)))
         (shadow-coordinate
           (interpret shadow-uv-input
                      :quantity :shadow-uv :affine-p t))
         (shadow-u (swizzle shadow-coordinate :x))
         (shadow-v (swizzle shadow-coordinate :y))
         (shadow-in-bounds
           (* (step (interpret 0.0 :quantity :shadow-uv :affine-p t)
                    shadow-u)
              (step shadow-u
                    (interpret 1.0 :quantity :shadow-uv :affine-p t))
              (step (interpret 0.0 :quantity :shadow-uv :affine-p t)
                    shadow-v)
              (step shadow-v
                    (interpret 1.0 :quantity :shadow-uv :affine-p t))))
         (shadow-texel-size
           (interpret (swizzle shadow-control-vector :xy)
                      :quantity :shadow-uv))
         (shadow-base-bias
           (interpret (swizzle shadow-control-vector :z)
                      :quantity :shadow-depth))
         (shadow-slope-bias
           (interpret (swizzle shadow-control-vector :w)
                      :quantity :shadow-depth))
         (shadow-bias
           (+ shadow-base-bias (* shadow-slope-bias (- 1.0 n-dot-l))))
         ;; A PCF tap displaced across a receiver plane must compare against
         ;; that plane's depth at the tap, not the center fragment depth.
         ;; With the orthographic shadow rows, the analytical UV depth gradient
         ;; is the face normal expressed in the light basis and scaled by the
         ;; world-span/depth-span ratio.  Its denominator is bounded only near
         ;; grazing incidence, where direct sun is negligible.  This prevents
         ;; wide kernels from reading the receiver's own slope as an occluder.
         (shadow-right
           (interpret (normalize (swizzle shadow-row-x :xyz))
                      :quantity :world-direction))
         (shadow-up
           (interpret (normalize (swizzle shadow-row-y :xyz))
                      :quantity :world-direction))
         (shadow-forward
           (interpret (normalize (swizzle shadow-row-z :xyz))
                      :quantity :world-direction))
         (shadow-normal-forward
           (min -0.05 (dot normal shadow-forward)))
         (shadow-depth-span
           (interpret (swizzle shadow-filter-vector :x)
                      :quantity :world-distance
                      :dimension :length :unit :metre))
         (shadow-world-units-per-texel
           (interpret (swizzle shadow-filter-vector :y)
                      :quantity :world-distance
                      :dimension :length :unit :metre))
         (shadow-world-span
           (interpret
            (/ shadow-world-units-per-texel (swizzle shadow-texel-size :x))
            :quantity :world-distance
            :dimension :length :unit :metre))
         (shadow-span-ratio (/ shadow-world-span shadow-depth-span))
         (shadow-depth-gradient
           (interpret
            (vec2
             (- (* (/ (dot normal shadow-right) shadow-normal-forward)
                   shadow-span-ratio))
             (- (* (/ (dot normal shadow-up) shadow-normal-forward)
                   shadow-span-ratio)))
            :quantity :shadow-depth-gradient))
         (shadow-center-depth
           (swizzle
            (interpret
             (sample shadow-map shadow-sampler shadow-coordinate)
             :quantity :shadow-depth :affine-p t)
            :x))
         (receiver-depth
           (interpret shadow-depth-input
                      :quantity :shadow-depth :affine-p t))
         (shadow-blocker-separation
           (max (interpret 0.0 :quantity :shadow-depth)
                (- (- receiver-depth shadow-bias)
                   shadow-center-depth)))
         (shadow-minimum-radius
           (interpret (swizzle shadow-filter-vector :z)
                      :quantity :shadow-filter-radius))
         (shadow-maximum-radius
           (interpret (swizzle shadow-filter-vector :w)
                      :quantity :shadow-filter-radius))
         (sun-angular-width
           (interpret (swizzle sun-color-vector :w)
                      :quantity :sun-angular-width))
         (shadow-penumbra-world-radius
           (interpret
            (* (* shadow-blocker-separation shadow-depth-span)
               sun-angular-width)
            :quantity :world-distance
            :dimension :length :unit :metre))
         (shadow-filter-radius
           (clamp
            (+ shadow-minimum-radius
               (interpret
                (/ shadow-penumbra-world-radius
                   shadow-world-units-per-texel)
                :quantity :shadow-filter-radius))
            shadow-minimum-radius shadow-maximum-radius))
         (shadow-sample
           (shadow-visibility
            shadow-map shadow-comparison-sampler
            shadow-coordinate receiver-depth shadow-depth-gradient
            shadow-texel-size shadow-bias shadow-filter-radius))
         (direct-shadow (mix 1.0 shadow-sample shadow-in-bounds))
         ;; The mesh carries normalized raw light readings; every response
         ;; curve and balance below is an art parameter editable live
         ;; without remeshing the world.
         (sky-input
           (interpret (swizzle light-input :x)
                      :quantity :sky-light-level))
         (block-input
           (interpret (swizzle light-input :y)
                      :quantity :block-light-level))
         (emission-input
           (interpret (swizzle light-input :z)
                      :quantity :material-emission))
         (sky-level (* sky-input sky-input))
         (block-level (* block-input block-input))
         (day-factor
           (interpret (swizzle sun-vector :w)
                      :quantity :day-factor))
         ;; Lateral skylight gives ambient visibility but not a hard sun
         ;; beam; the shadow map gates only the direct solar term.
         (sun-visibility
           (smoothstep
            (interpret 0.90 :quantity :sky-light-level)
            (interpret 1.0 :quantity :sky-light-level)
            sky-input))
         (ambient
           (interpret (swizzle ambient-vector :xyz)
                      :quantity :linear-rgb))
         (sun-color
           (interpret (swizzle sun-color-vector :xyz)
                      :quantity :linear-rgb))
         ;; A small floor keeps unlit geometry barely readable rather than
         ;; a void; caves stay dark for the right reason.
         (sky-light
           (interpret
            (* ambient (+ 0.06 (* 1.34 sky-level)) ao)
            :quantity :linear-rgb))
         (sun-light
           (interpret
            (* sun-color
               (* n-dot-l sun-visibility day-factor direct-shadow))
            :quantity :linear-rgb))
         (torch-color
           (interpret (vec3 1.0 0.82 0.58) :quantity :linear-rgb))
         (local-light (* torch-color block-level))
         (albedo
           (interpret
            (swizzle (sample block-atlas block-sampler uv) :rgb)
            :quantity :linear-rgb))
         (reflected
           (interpret
            (* albedo (+ sky-light sun-light local-light))
            :quantity :linear-rgb))
         (radiance
           (+ reflected
              (interpret (* albedo emission-input)
                         :quantity :linear-rgb)))
         (fog-color
           (interpret (swizzle fog-color-vector :xyz)
                      :quantity :linear-rgb))
         (fog-amount
           (interpret fog-input :quantity :fog-amount))
         (fogged (mix radiance fog-color fog-amount))
         (normal-rgba
           (interpret (vec4 fogged 1.0) :quantity :linear-rgba))
         (shadow-diagnostic
           (interpret (swizzle fog-color-vector :w)
                      :quantity :shadow-diagnostic))
         (shadow-rgba
           (interpret
            (vec4 (vec3 direct-shadow direct-shadow direct-shadow) 1.0)
            :quantity :linear-rgba))
         (rgba (mix normal-rgba shadow-rgba shadow-diagnostic)))
    (set-output color-output rgba)))

(defun block-world-fragment-specification ()
  (shader-specification-for :block-surface :fragment))

(defun block-world-fragment-lowering ()
  "Compile the block material and retain its expression-to-SSA provenance."
  (compile-shader-specification (block-world-fragment-specification)))

(defun block-world-fragment-module ()
  "Lower the readable block material to luv's structured SPIR-V module."
  (shader-lowering-module (block-world-fragment-lowering)))

(defun block-world-fragment-shader ()
  (assemble-spir-v-module (block-world-fragment-module)))

;;; The sky is a fullscreen triangle drawn before block geometry with depth
;;; writes disabled.  Its vertex stage reconstructs a per-corner view ray
;;; from the camera basis; its fragment stage is pure image mathematics over
;;; the interpolated ray and the frame environment lanes.

(define-shader-method shader-specification-for
    block-world-sky-vertex-specification
    ((role (eql :sky)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((corner-position :vec3 :location 0))
     :outputs ((clip-position :vec4 :built-in :position)
               (ray-output :vec3 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((corner corner-position)
         (x (swizzle corner :x))
         (y (swizzle corner :y))
         (z (swizzle corner :z))
         (right (swizzle right-vector :xyz))
         (up (swizzle up-vector :xyz))
         (forward (swizzle forward-vector :xyz))
         (x-scale (swizzle projection-vector :x))
         (y-scale (swizzle projection-vector :y))
         ;; Invert the block vertex projection: clip x,y back to view-space
         ;; slopes, including its y flip, so the ray agrees with the world.
         (view-x (/ x x-scale))
         (view-y (- (/ y y-scale)))
         (ray (+ (* right view-x) (* up view-y) forward))
         (clip (vec4 x y z 1.0)))
    (set-output clip-position clip)
    (set-output ray-output ray)))

(defun block-world-sky-vertex-specification ()
  (shader-specification-for :sky :vertex))

(defun block-world-sky-vertex-module ()
  (shader-lowering-module
   (compile-shader-specification (block-world-sky-vertex-specification))))

(defun block-world-sky-vertex-shader ()
  (assemble-spir-v-module (block-world-sky-vertex-module)))

(define-shader-method shader-specification-for
    block-world-sky-fragment-specification
    ((role (eql :sky)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((ray-input :vec3 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((unit (normalize ray-input))
         (elevation (swizzle unit :y))
         (zenith (swizzle zenith-vector :xyz))
         (horizon (swizzle horizon-vector :xyz))
         ;; A soft vertical gradient with a wide horizon band; the band sits
         ;; slightly below level so distant terrain meets the fog colour.
         (height (smoothstep -0.04 0.45 elevation))
         (base (mix horizon zenith height))
         (sun-direction (swizzle sun-vector :xyz))
         (day-factor (swizzle sun-vector :w))
         (sun-color (swizzle sun-color-vector :xyz))
         (sun-width (swizzle sun-color-vector :w))
         (alignment (max 0.0 (dot unit sun-direction)))
         (disc-outer (- 1.0 (* sun-width 3.0)))
         (disc-inner (- 1.0 sun-width))
         (disc (smoothstep disc-outer disc-inner alignment))
         (glow (* (expt alignment 24.0) 0.35))
         (sun-radiance (* sun-color (+ disc glow) day-factor))
         (rgb (+ base sun-radiance))
         (rgba (vec4 rgb 1.0)))
    (set-output color-output rgba)))

(defun block-world-sky-fragment-specification ()
  (shader-specification-for :sky :fragment))

(defun block-world-sky-fragment-module ()
  (shader-lowering-module
   (compile-shader-specification (block-world-sky-fragment-specification))))

(defun block-world-sky-fragment-shader ()
  (assemble-spir-v-module (block-world-sky-fragment-module)))

;;; The shadow-map pass renders the same block mesh into stored light-space
;;; depth.  The block fragment material samples that product with explicit
;;; percentage-closer filtering and receiver bias above.

(define-shader-method shader-specification-for
    block-world-shadow-vertex-specification
    ((role (eql :block-shadow)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((world-position :vec3 :location 0))
     :outputs ((clip-position :vec4 :built-in :position))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((world (vec4 world-position 1.0))
         (clip-x (dot shadow-row-x world))
         (clip-y (dot shadow-row-y world))
         (clip-z (dot shadow-row-z world))
         (clip-w (dot shadow-row-w world))
         (clip (vec4 clip-x clip-y clip-z clip-w)))
    (set-output clip-position clip)))

(defun block-world-shadow-vertex-specification ()
  (shader-specification-for :block-shadow :vertex))

(defun block-world-shadow-vertex-module ()
  (shader-lowering-module
   (compile-shader-specification (block-world-shadow-vertex-specification))))

(defun block-world-shadow-vertex-shader ()
  (assemble-spir-v-module (block-world-shadow-vertex-module)))

;;; The crosshair is deliberately another tiny mathematical material rather
;;; than a magic fixed-function colour.  Its vertex half remains in SHADER.LISP
;;; because the expression language does not yet pretend that integer vertex
;;; indexing and position built-ins are ordinary fragment mathematics.

(define-shader-method shader-specification-for
    block-world-crosshair-fragment-specification
    ((role (eql :block-crosshair)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((ink-input :vec3 :location 0))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((ink ink-input)
         (rgba (vec4 ink 1.0)))
    (set-output color-output rgba)))

(defun block-world-crosshair-fragment-specification ()
  (shader-specification-for :block-crosshair :fragment))

(defun block-world-crosshair-fragment-module ()
  (shader-lowering-module
   (compile-shader-specification
    (block-world-crosshair-fragment-specification))))

(defun block-world-crosshair-fragment-shader ()
  (assemble-spir-v-module (block-world-crosshair-fragment-module)))


(defun block-world-crosshair-vertex-module ()
  "Pass screen-space crosshair vertices and their ink to the fragment stage."
  (make-instance
   'spir-v-module
   :entry-points
   (list (make-instance
          'spir-v-entry-point :execution-model 'vertex
          :function '%main
          :interfaces '(%screen-position %ink %position %ink-output)))
   :annotations
   '((decorate %screen-position location 0)
     (decorate %ink location 1)
     (decorate %position built-in (enum built-in position))
     (decorate %ink-output location 0))
   :global-declarations
   '((%void type-void)
     (%float type-float 32)
     (%vec3 type-vector %float 3)
     (%vec4 type-vector %float 4)
     (%input-vec3-pointer type-pointer input %vec3)
     (%output-vec3-pointer type-pointer output %vec3)
     (%output-vec4-pointer type-pointer output %vec4)
     (%function-type type-function %void)
     (%one constant %float 1.0)
     (%screen-position variable %input-vec3-pointer input)
     (%ink variable %input-vec3-pointer input)
     (%position variable %output-vec4-pointer output)
     (%ink-output variable %output-vec3-pointer output))
   :function-definitions
   (list
    (make-instance
     'spir-v-function-definition
     :result-id '%main :return-type '%void
     :function-type '%function-type
     :basic-blocks
     (list
      (make-instance
       'spir-v-basic-block :label '%entry
       :instructions
       '((%screen load %vec3 %screen-position)
         (%ink-value load %vec3 %ink)
         (%screen-x composite-extract %float %screen 0)
         (%screen-y composite-extract %float %screen 1)
         (%screen-z composite-extract %float %screen 2)
         (%clip-position composite-construct
                         %vec4 %screen-x %screen-y %screen-z %one)
         (store %position %clip-position)
         (store %ink-output %ink-value)
         (return))))))))

(defun block-world-crosshair-vertex-shader ()
  (assemble-spir-v-module (block-world-crosshair-vertex-module)))
