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
      (fog-color-vector :vec4)) ; fog colour, w unused
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
               (light-output :vec3 :location 3))
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
         (clip (vec4 clip-x clip-y clip-z view-z)))
    (set-output clip-position clip)
    (set-output uv-shade-output uv-shade-input)
    (set-output normal-output normal-input)
    (set-output fog-output fog-amount)
    (set-output light-output light-input)))

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
              (light-input :vec3 :location 3))
     :outputs ((color-output :vec4 :location 0))
     :resources ((block-atlas :texture-2d :set 0 :binding 0)
                 (block-sampler :sampler :set 0 :binding 1)
                 (frame-state :uniform-block :set 0 :binding 2
                              :members #.*frame-uniform-members*)))
  (let* ((uv-shade uv-shade-input)
         (uv (swizzle uv-shade :xy))
         (ao (swizzle uv-shade :z))
         (normal normal-input)
         ;; The mesh carries normalized raw light readings; every response
         ;; curve and balance below is an art parameter editable live
         ;; without remeshing the world.
         (sky-input (swizzle light-input :x))
         (block-input (swizzle light-input :y))
         (emission-input (swizzle light-input :z))
         (sky-level (* sky-input sky-input))
         (block-level (* block-input block-input))
         (sun-direction (swizzle sun-vector :xyz))
         (day-factor (swizzle sun-vector :w))
         (n-dot-l (max 0.0 (dot normal sun-direction)))
         ;; Lateral skylight gives ambient visibility but not a hard sun
         ;; beam; the moving sun stays unshadowed until a real shadow pass.
         (sun-visibility (smoothstep 0.90 1.0 sky-input))
         (ambient (swizzle ambient-vector :xyz))
         (sun-color (swizzle sun-color-vector :xyz))
         ;; A small floor keeps unlit geometry barely readable rather than
         ;; a void; caves stay dark for the right reason.
         (sky-light (* ambient (+ 0.06 (* 1.34 sky-level)) ao))
         (sun-light (* sun-color (* n-dot-l sun-visibility day-factor)))
         (torch-color (vec3 1.0 0.82 0.58))
         (local-light (* torch-color block-level))
         (albedo (swizzle (sample block-atlas block-sampler uv) :rgb))
         (reflected (* albedo (+ sky-light sun-light local-light)))
         (radiance (+ reflected (* albedo emission-input)))
         (fog-color (swizzle fog-color-vector :xyz))
         (fogged (mix radiance fog-color fog-input))
         (rgba (vec4 fogged 1.0)))
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
