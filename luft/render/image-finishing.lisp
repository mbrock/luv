(in-package #:luft.render)

;;; Copy reconstructed radiance into the HDR composite, run the lens chain
;;; over it, then grade that composite for the display after flames and
;;; exposure. The frame specifies that order. Target generations own images
;;; and their borrowed bindings.

(defclass image-finishing (gpu-resource-owner)
  ((composite-program :accessor finishing-composite-program)
   (bloom-bright-program :accessor finishing-bloom-bright-program)
   (bloom-horizontal-program :accessor finishing-bloom-horizontal-program)
   (bloom-vertical-program :accessor finishing-bloom-vertical-program)
   (sun-shaft-program :accessor finishing-sun-shaft-program)
   (present-program :accessor finishing-present-program)
   (sampler :accessor finishing-sampler)))

(defgeneric make-composite-binding (finishing device scene))
(defgeneric make-lens-binding (finishing device stage source camera)
  (:documentation
   "Bind one lens-chain STAGE (:bright, :horizontal, :vertical, or :shafts)
to its SOURCE view and the frame's CAMERA uniform."))
(defgeneric make-presentation-binding (finishing device scene depth camera bloom shafts))
(defgeneric encode-composite (finishing pass binding))
(defgeneric encode-lens-stage (finishing stage pass binding))
(defgeneric encode-presentation (finishing pass binding))

(defmethod make-composite-binding ((finishing image-finishing) device scene)
  (make-program-binding (finishing-composite-program finishing) device
                        :scene scene :scene-sampler (finishing-sampler finishing)))

(defun finishing-lens-program (finishing stage)
  (ecase stage
    (:bright (finishing-bloom-bright-program finishing))
    (:horizontal (finishing-bloom-horizontal-program finishing))
    (:vertical (finishing-bloom-vertical-program finishing))
    (:shafts (finishing-sun-shaft-program finishing))))

(defmethod make-lens-binding ((finishing image-finishing) device stage source camera)
  (make-program-binding (finishing-lens-program finishing stage) device
                        :source source :source-sampler (finishing-sampler finishing)
                        :camera-state camera))

(defmethod make-presentation-binding
    ((finishing image-finishing) device scene depth camera bloom shafts)
  (make-program-binding (finishing-present-program finishing) device
                        :scene scene :scene-sampler (finishing-sampler finishing)
                        :scene-depth depth :camera-state camera
                        :bloom bloom :shafts shafts))

(defmethod encode-composite ((finishing image-finishing) pass binding)
  (encode-program (finishing-composite-program finishing) pass binding
                  (make-gpu-draw-command :vertex-count 3)))

(defmethod encode-lens-stage ((finishing image-finishing) stage pass binding)
  (encode-program (finishing-lens-program finishing stage) pass binding
                  (make-gpu-draw-command :vertex-count 3)))

(defmethod encode-presentation ((finishing image-finishing) pass binding)
  (encode-program (finishing-present-program finishing) pass binding
                  (make-gpu-draw-command :vertex-count 3)))

(defconstant +bloom-divisor+ 4
  "The lens chain runs at a quarter of the output extent on each axis.")

(defun bloom-extent (extent)
  "The reduced lens-chain extent for output EXTENT, at least one texel."
  (mapcar (lambda (dimension) (max 1 (floor dimension +bloom-divisor+))) extent))

(defun make-image-finishing (device color-format)
  (let ((finishing (make-instance 'image-finishing)))
    (with-gpu-construction (finishing)
      (flet ((program (label fragment format)
               (own-gpu-object
                finishing
                (make-drawing-program
                 device :label label
                 :vertex (shaders:present-vertex-specification)
                 :fragment fragment
                 :targets `((:format ,format))))))
        (setf (finishing-sampler finishing)
              (own-gpu-resource finishing device
                                (make-sampler-descriptor :label "luft image filtering"
                                                         :mag-filter :linear :min-filter :linear))
              (finishing-composite-program finishing)
              (program "luft HDR composite copy"
                       (shaders::hdr-copy-fragment-specification) :rgba16-float)
              (finishing-bloom-bright-program finishing)
              (program "luft bloom bright pass"
                       (shaders::bloom-bright-fragment-specification) :rgba16-float)
              (finishing-bloom-horizontal-program finishing)
              (program "luft bloom horizontal blur"
                       (shaders::bloom-horizontal-fragment-specification) :rgba16-float)
              (finishing-bloom-vertical-program finishing)
              (program "luft bloom vertical blur"
                       (shaders::bloom-vertical-fragment-specification) :rgba16-float)
              (finishing-sun-shaft-program finishing)
              (program "luft sun shafts"
                       (shaders::sun-shaft-fragment-specification) :rgba16-float)
              (finishing-present-program finishing)
              (program "luft HDR presentation"
                       (shaders:present-fragment-specification) color-format))))))

(in-package #:luft.render.shaders)

;;; The lens chain, after Luvcraft's (luvcraft/render.lisp, "The lens
;;; chain"): a bright pass into a quarter-resolution image, a separable
;;; thirteen-tap gaussian run twice, and a radial gather toward the sun.  It
;;; works in exposed units so its contribution keeps step with exposure, and
;;; reads the finished HDR composite, so MetalFX's reconstruction, torch
;;; flames, and the sun disc all feed it alike.

(define-live-shader bloom-bright-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((source :texture-2d :binding 0 :sample-transfer :identity)
                 (source-sampler :sampler :binding 1)
                 (camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 31))))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         ;; Four full-resolution taps per chain texel, one output texel off
         ;; each diagonal: a single tap would alias exactly the small bright
         ;; highlights this pass exists to smear.
         (texel (representation (swizzle lens-extent :zw)))
         (dx (* texel (vec2 1.0 0.0)))
         (dy (* texel (vec2 0.0 1.0)))
         (box
           (* (+ (swizzle (sample source source-sampler (+ uv (+ dx dy))) :xyz)
                 (swizzle (sample source source-sampler (+ uv (- dx dy))) :xyz)
                 (swizzle (sample source source-sampler (- uv (- dx dy))) :xyz)
                 (swizzle (sample source source-sampler (- uv (+ dx dy))) :xyz))
              0.25))
         (exposure (representation (swizzle sky-color-vector :w)))
         (radiance (* box exposure))
         (threshold (swizzle lens-parameters :y))
         (luminance (dot radiance (vec3 0.2126 0.7152 0.0722)))
         (knee (smoothstep threshold (+ threshold 0.75) luminance)))
    ;; Clamp a single sun-disc texel so it cannot dominate the whole blur.
    (set-output color-output (vec4 (min (* radiance knee) (vec3 24.0 24.0 24.0))
                                   1.0))))

;;; A thirteen-tap gaussian expressed as seven linearly filtered samples:
;;; each offset lands between the two texels whose weights it combines.  The
;;; two directions are separate programs rather than a uniform lane, and the
;;; shader language does not pass textures to functions, so each spells out
;;; exactly the code it runs.

(define-live-shader bloom-horizontal-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((source :texture-2d :binding 0 :sample-transfer :identity)
                 (source-sampler :sampler :binding 1)
                 (camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 31))))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         (span (vec2 (swizzle (representation lens-extent) :x) 0.0))
         (near-offset (* span 1.4585))
         (mid-offset (* span 3.4038))
         (far-offset (* span 5.3510))
         (blurred
           (+ (* (swizzle (sample source source-sampler uv) :xyz) 0.1370)
              (* (+ (swizzle (sample source source-sampler (+ uv near-offset)) :xyz)
                    (swizzle (sample source source-sampler (- uv near-offset)) :xyz))
                 0.2393)
              (* (+ (swizzle (sample source source-sampler (+ uv mid-offset)) :xyz)
                    (swizzle (sample source source-sampler (- uv mid-offset)) :xyz))
                 0.1394)
              (* (+ (swizzle (sample source source-sampler (+ uv far-offset)) :xyz)
                    (swizzle (sample source source-sampler (- uv far-offset)) :xyz))
                 0.0527))))
    (set-output color-output (vec4 blurred 1.0))))

(define-live-shader bloom-vertical-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((source :texture-2d :binding 0 :sample-transfer :identity)
                 (source-sampler :sampler :binding 1)
                 (camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 31))))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         (span (vec2 0.0 (swizzle (representation lens-extent) :y)))
         (near-offset (* span 1.4585))
         (mid-offset (* span 3.4038))
         (far-offset (* span 5.3510))
         (blurred
           (+ (* (swizzle (sample source source-sampler uv) :xyz) 0.1370)
              (* (+ (swizzle (sample source source-sampler (+ uv near-offset)) :xyz)
                    (swizzle (sample source source-sampler (- uv near-offset)) :xyz))
                 0.2393)
              (* (+ (swizzle (sample source source-sampler (+ uv mid-offset)) :xyz)
                    (swizzle (sample source source-sampler (- uv mid-offset)) :xyz))
                 0.1394)
              (* (+ (swizzle (sample source source-sampler (+ uv far-offset)) :xyz)
                    (swizzle (sample source source-sampler (- uv far-offset)) :xyz))
                 0.0527))))
    (set-output color-output (vec4 blurred 1.0))))

(define-shader-function sun-screen-position
    (sun right up forward projection divisor day-factor)
  "Return the sun's presentation UV and how strongly it counts as on screen.

The weight fades the solar lens effects out as the disc leaves the frame or
falls behind the camera, so turning the head does not pop the shafts.  An
isometric camera has no vanishing point to gather toward, so its weight is
zero."
  (let* ((view-z (dot sun forward))
         (safe-z (max view-z 0.02))
         (clip-x (/ (* (dot sun right) (swizzle projection :x)) safe-z))
         (clip-y (- (/ (* (dot sun up) (swizzle projection :y)) safe-z)))
         (edge (max (abs clip-x) (abs clip-y)))
         (weight (* (- 1.0 (smoothstep 0.80 1.45 edge))
                    (* (smoothstep 0.02 0.30 view-z)
                       (* day-factor divisor)))))
    (vec3 (+ 0.5 (* 0.5 clip-x)) (+ 0.5 (* 0.5 clip-y)) weight)))

;;; Crepuscular rays as a screen-space gather: march the blurred bright image
;;; toward the solar disc and accumulate what is still lit.  Terrain that
;;; occludes the sun is dark in the bright image, so the rays break into
;;; beams around ridges and pillars for free.

(define-live-shader sun-shaft-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((source :texture-2d :binding 0 :sample-transfer :identity)
                 (source-sampler :sampler :binding 1)
                 (camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 31))))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         (sun-screen
           (sun-screen-position
            (representation (swizzle sun-vector :xyz))
            (representation (swizzle camera-right :xyz))
            (representation (swizzle camera-up :xyz))
            (representation (swizzle camera-forward :xyz))
            camera-projection
            (swizzle (representation render-parameters) :z)
            (swizzle (representation zenith-color-vector) :w)))
         (decay (swizzle lens-parameters :w))
         (delta (* (- (swizzle sun-screen :xy) uv) 0.03125))
         (gathered
           (counted-fold (tap 32.0 total (vec3 0.0 0.0 0.0))
             (+ total
                (* (swizzle (sample source source-sampler (+ uv (* delta tap)))
                            :xyz)
                   (expt decay tap))))))
    (set-output color-output
                (vec4 (* gathered (* (swizzle sun-screen :z) 0.03125)) 1.0))))
