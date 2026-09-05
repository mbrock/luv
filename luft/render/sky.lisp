(in-package #:luft.render)

;;; The atmosphere draws world radiance before geometry. Temporal rendering
;;; also needs the sky's motion, but no resident meshes or shadow inputs.

(defclass sky-drawing (pipeline-scene-drawing) ())

(defun make-sky-drawing (device target-formats sample-count)
  (make-pipeline-scene-drawing
   'sky-drawing device :label "luft HDR sky"
   :vertex (shaders:present-vertex-specification)
   :fragment (if (rest target-formats)
                 (shaders:sky-temporal-fragment-specification)
                 (shaders:sky-fragment-specification))
   :targets (mapcar (lambda (format) `(:format ,format)) target-formats)
   :sample-count sample-count :depth-compare :always :vertex-count 3))

(defmethod make-scene-drawing-binding
    ((drawing sky-drawing) device camera-buffer shadow-view shadow-sampler)
  (declare (ignore shadow-view shadow-sampler))
  (make-program-binding (scene-drawing-program drawing) device
                        :camera-state camera-buffer))

(in-package #:luft.render.shaders)

(define-shader-function sky-view-ray
    (ndc camera-right camera-up camera-forward camera-projection divisor)
  (let* ((perspective-ray
           (normalize
            (+ (swizzle camera-forward :xyz)
               (* (swizzle camera-right :xyz)
                  (assume-quantity
                   (/ (swizzle ndc :x)
                      (swizzle camera-projection :x))
                   :unit :one))
               (* (swizzle camera-up :xyz)
                  (assume-quantity
                   (/ (- (swizzle ndc :y))
                      (swizzle camera-projection :y))
                   :unit :one)))))
         (isometric-ray
           (normalize
            (+ (swizzle camera-forward :xyz)
               (* (swizzle camera-up :xyz)
                  (assume-quantity
                   (* (- (swizzle ndc :y)) 0.38) :unit :one))))))
    (mix isometric-ray perspective-ray
         (assume-quantity divisor :unit :one))))

(define-shader-function painted-sky-radiance
    (ndc camera-right camera-up camera-forward camera-projection divisor
     sun-vector sun-color-vector sky-color-vector)
  "Return view-stable late-afternoon HDR radiance before exposure or grading."
  (let* ((ray-direction
           (sky-view-ray ndc camera-right camera-up camera-forward
                         camera-projection divisor))
         ;; Cloud and watercolor shaping are procedural image mathematics;
         ;; the result re-enters the semantic ladder as scene radiance.
         (ray (representation ray-direction))
         (height (swizzle ray :z))
         (upness (clamp height 0.0 1.0))
         (horizon-weight (smoothstep -0.10 0.42 height))
         (base-sky
           (representation (swizzle sky-color-vector :xyz)))
         (horizon (vec3 0.58 0.78 1.06))
         (zenith (* base-sky (vec3 0.34 0.58 0.94)))
         (horizon-haze
           (- 1.0 (smoothstep 0.01 0.26 (abs height))))
         (atmosphere
           (mix (mix horizon zenith horizon-weight)
                (vec3 1.12 0.68 0.38)
                (* horizon-haze 0.13)))
         ;; Broad directional noise makes sparse watercolor cloud banks.  The
         ;; envelope keeps them above the haze and below the clear zenith.
         (cloud-point
           (vec3 (* (swizzle ray :x) 5.0)
                 (* (swizzle ray :y) 5.0)
                 (* height 12.0)))
         (cloud-coarse (paper-noise (+ cloud-point (vec3 3.1 11.7 5.3))))
         (cloud-fine
           (paper-noise
            (+ (* cloud-point (vec3 2.3 2.3 1.4))
               (vec3 17.9 2.7 31.1))))
         (cloud-shape
           (smoothstep 0.52 0.72 (+ (* cloud-coarse 0.72)
                                    (* cloud-fine 0.28))))
         (cloud-height
           (* (smoothstep 0.05 0.20 upness)
              (- 1.0 (smoothstep 0.58 0.90 upness))))
         (sun
           (representation (normalize (swizzle sun-vector :xyz))))
         (sun-facing (max 0.0 (dot ray sun)))
         (sun-halo (smoothstep 0.965 0.9992 sun-facing))
         (sun-disc (smoothstep 0.99925 0.99982 sun-facing))
         (cloud-light
           (mix (vec3 0.76 0.88 1.10)
                (vec3 1.34 0.78 0.42)
                (smoothstep 0.72 0.98 sun-facing)))
         (clouded
           (mix atmosphere cloud-light (* cloud-shape cloud-height 0.26)))
         (radiance
           (+ clouded
              (* (representation (swizzle sun-color-vector :xyz))
                 (+ (* sun-halo 0.10) (* sun-disc 2.8))))))
    (assume-quantity radiance
                     :quantity quantities:scene-radiance :unit :one)))

(define-live-shader sky-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((camera-state :uniform-block :binding 0
                  :members #.(scene-uniform-prefix 17))))
  (let* ((radiance
           (painted-sky-radiance
            ndc camera-right camera-up camera-forward camera-projection
            (swizzle (representation render-parameters) :z)
            sun-vector sun-color-vector
            sky-color-vector)))
    (set-output color-output (vec4 (representation radiance) 1.0))))

(define-live-shader sky-temporal-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 0
                  :members #.(scene-uniform-prefix 17))))
  (let* ((divisor (swizzle (representation render-parameters) :z))
         ;; The fullscreen triangle itself cannot move. Reconstruct the ray at
         ;; the same jittered sample location as geometry, as the original
         ;; Vulkan resolve did, and derive motion from that unjittered address.
         (sample-ndc
           (- ndc
              (representation (swizzle temporal-parameters :xy))))
         (ray (sky-view-ray sample-ndc camera-right camera-up camera-forward
                            camera-projection divisor))
         (radiance
           (painted-sky-radiance
            sample-ndc camera-right camera-up camera-forward camera-projection
            divisor
            sun-vector sun-color-vector sky-color-vector))
         (previous-z
           (representation
            (dot ray (swizzle previous-camera-forward :xyz))))
         (previous-clip
           (vec4 (* (representation
                     (dot ray (swizzle previous-camera-right :xyz)))
                    (swizzle previous-camera-projection :x))
                 (- (* (representation
                        (dot ray (swizzle previous-camera-up :xyz)))
                       (swizzle previous-camera-projection :y)))
                 0.0
                 (mix 1.0 previous-z divisor)))
         (current-clip
           (vec4 (swizzle sample-ndc :x) (swizzle sample-ndc :y) 0.0 1.0)))
    (set-output color-output (vec4 (representation radiance) 1.0))
    (set-output motion-output
                (mesh-temporal-motion previous-clip current-clip))))
