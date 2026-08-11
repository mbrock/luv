;;; The visible block-world material as a typed mathematical specification.

(in-package #:luv.spir-v)

(define-shader-method shader-specification-for
    block-world-fragment-specification
    ((role (eql :block-surface)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((uv-shade-input :vec3 :location 0)
              (normal-input :vec3 :location 1)
              (fog-input :vec4 :location 2))
     :outputs ((color-output :vec4 :location 0))
     :resources ((block-atlas :texture-2d :set 0 :binding 0)
                 (block-sampler :sampler :set 0 :binding 1)))
  (let* ((uv-shade uv-shade-input)
         (uv (swizzle uv-shade :xy))
         (ao (swizzle uv-shade :z))
         (normal normal-input)
         (sun (vec3 0.30 0.86 0.40))
         (hemisphere (* (+ (dot normal sun) 1.0) 0.5))
         ;; A scalar light made every face the same hue and let snow flatten
         ;; into the fog.  Interpolate between cool open-sky fill and a warm
         ;; sun instead: face direction becomes legible without losing the
         ;; texture atlas or the mesh-baked ambient occlusion.
         (shade-light (vec3 0.48 0.58 0.76))
         (sun-light (vec3 1.02 0.96 0.82))
         (directional-light (mix shade-light sun-light hemisphere))
         (light (* directional-light ao))
         (albedo (swizzle (sample block-atlas block-sampler uv) :rgb))
         (fog-state fog-input)
         (sky (swizzle fog-state :rgb))
         (fog (swizzle fog-state :w))
         (lit (* albedo light))
         (fogged (mix sky lit fog))
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
