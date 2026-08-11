;;; The visible block-world material as a typed mathematical specification.

(in-package #:luv.spir-v)

(define-shader block-world-fragment-specification
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
         (illumination (+ 0.42 (* hemisphere 0.58)))
         (light (* illumination ao))
         (albedo (swizzle (sample block-atlas block-sampler uv) :rgb))
         (fog-state fog-input)
         (sky (swizzle fog-state :rgb))
         (fog (swizzle fog-state :w))
         (lit (* albedo light))
         (fogged (mix sky lit fog))
         (rgba (vec4 fogged 1.0)))
    (set-output color-output rgba)))

(defun block-world-fragment-lowering ()
  "Compile the block material and retain its expression-to-SSA provenance."
  (compile-shader-specification (block-world-fragment-specification)))

(defun block-world-fragment-module ()
  "Lower the readable block material to luv's structured SPIR-V module."
  (shader-lowering-module (block-world-fragment-lowering)))

(defun block-world-fragment-shader ()
  (assemble-spir-v-module (block-world-fragment-module)))

