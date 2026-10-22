;;; Temporal reconstruction for the block world: a neighbourhood pass at the
;;; scene's size, then a resolve into the image presentation reads.
;;;
;;; The block world's materials write no motion.  Camera motion is recovered
;;; here instead, from depth: each scene texel is unprojected through this
;;; frame's camera and projected again through the last one.  Geometry that
;;; moves by itself (critters, particles, bodies) is left to the history
;;; clamp.  First-person geometry is screen-locked, not world-placed, so the
;;; renderer copies the world-only colour before drawing it, and wherever the
;;; finished scene differs from that copy the motion is zero.  See #C4ED2V.

(in-package #:luvcraft.shaders)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *temporal-uniform-members*
    '((camera-now :vec4)       ; camera position, history weight
      (right-now :vec4)        ; unjittered basis, w unused
      (up-now :vec4)
      (forward-now :vec4)
      (projection-now :vec4)   ; x scale, y scale, z scale, z offset
      (camera-then :vec4)      ; previous camera position, w unused
      (right-then :vec4)
      (up-then :vec4)
      (forward-then :vec4)
      (projection-then :vec4)
      (temporal-jitter :vec4)  ; clip-space jitter x, y, unused, unused
      (temporal-extent :vec4)) ; scene width, height, output width, height
    "The uniform layout shared by the neighbourhood and resolve stages."))

(define-shader-function temporal-rgb-to-ycocg (rgb)
  "Put RGB into a luminance/chroma space whose box clips history usefully."
  (vec3 (+ (* (swizzle rgb :x) 0.25)
           (* (swizzle rgb :y) 0.50)
           (* (swizzle rgb :z) 0.25))
        (* 0.5 (- (swizzle rgb :x) (swizzle rgb :z)))
        (+ (* (swizzle rgb :x) -0.25)
           (* (swizzle rgb :y) 0.50)
           (* (swizzle rgb :z) -0.25))))

(define-shader-function temporal-ycocg-to-rgb (value)
  "Invert TEMPORAL-RGB-TO-YCOCG."
  (vec3 (+ (swizzle value :x) (swizzle value :y)
           (- (swizzle value :z)))
        (+ (swizzle value :x) (swizzle value :z))
        (+ (swizzle value :x) (- (swizzle value :y))
           (- (swizzle value :z)))))

(define-shader-abstraction temporal-texel (centre dx dy size)
  `(uvec2 (uint (clamp (+ (swizzle ,centre :x) ,dx) 0.0
                       (- (swizzle ,size :x) 1.0)))
          (uint (clamp (+ (swizzle ,centre :y) ,dy) 0.0
                       (- (swizzle ,size :y) 1.0)))))

(define-shader-abstraction temporal-neighbourhood (extreme scene centre size)
  (let ((taps (loop for dy in '(-1.0 0.0 1.0)
                    append (loop for dx in '(-1.0 0.0 1.0)
                                 collect `(temporal-rgb-to-ycocg
                                           (swizzle
                                            (texel-load ,scene
                                                        (temporal-texel ,centre
                                                                        ,dx ,dy
                                                                        ,size))
                                            :xyz))))))
    (reduce (lambda (a b) `(,extreme ,a ,b)) taps)))

(define-shader-function temporal-nearness (scene world depth)
  "Order a texel for velocity dilation: screen-locked first, then nearest.

Depth runs from zero at the near plane to one at the far plane; a texel whose
finished colour differs from the world-only copy was drawn after the world, by
first-person geometry, and sorts before all of it."
  (let* ((difference (abs (- (swizzle scene :xyz) (swizzle world :xyz)))))
    (if (> (max (swizzle difference :x)
                (max (swizzle difference :y) (swizzle difference :z)))
           0.0)
        -1.0
        (swizzle depth :x))))

(define-shader-function temporal-nearer (a b)
  "Keep whichever (dx, dy, nearness) tap sorts first."
  (mix a b (step (swizzle b :z) (swizzle a :z))))

(define-shader-abstraction temporal-nearest-tap (scene world depth centre size)
  (let ((taps (loop for dy in '(-1.0 0.0 1.0)
                    append (loop for dx in '(-1.0 0.0 1.0)
                                 collect `(vec3 ,dx ,dy
                                                (temporal-nearness
                                                 (texel-load ,scene
                                                             (temporal-texel ,centre ,dx ,dy ,size))
                                                 (texel-load ,world
                                                             (temporal-texel ,centre ,dx ,dy ,size))
                                                 (texel-load ,depth
                                                             (temporal-texel ,centre ,dx ,dy ,size))))))))
    (reduce (lambda (a b) `(temporal-nearer ,a ,b)) taps)))

(define-shader-abstraction temporal-project-then (relative)
  ;; The previous camera's UV for a point RELATIVE to that camera.  Inline,
  ;; since the camera lanes are the calling stage's uniform members.
  `(let* ((z (max (dot ,relative (swizzle forward-then :xyz)) 0.0001))
          (x (/ (* (dot ,relative (swizzle right-then :xyz))
                   (swizzle projection-then :x))
                z))
          (y (- (/ (* (dot ,relative (swizzle up-then :xyz))
                      (swizzle projection-then :y))
                   z))))
     (* (+ (vec2 x y) (vec2 1.0 1.0)) 0.5)))

(define-shader-method shader-specification-for
    temporal-neighbourhood-fragment-specification
    ((role (eql :temporal-neighbourhood)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((uv-input :vec2 :location 0))
     :outputs ((low-output :vec4 :location 0)
               (high-output :vec4 :location 1))
     :resources
     ((scene-color :texture-2d :set 0 :binding 0
                   :sample-transfer :identity)
      (world-color :texture-2d :set 0 :binding 1
                   :sample-transfer :identity)
      (scene-depth :depth-texture-2d :set 0 :binding 2)
      (temporal-state :uniform-block :set 0 :binding 3
                      :members #.*temporal-uniform-members*)))
  (let* ((size (swizzle temporal-extent :xy))
         (centre (floor (* uv-input size)))
         (tap (temporal-nearest-tap scene-color world-color scene-depth
                                    centre size))
         (pixel (+ centre (swizzle tap :xy)))
         (locked (step (swizzle tap :z) -0.5))
         (depth (swizzle tap :z))
         ;; The tap's unjittered clip position and its view-space slopes.
         (clip (- (- (* (/ (+ pixel (vec2 0.5 0.5)) size) 2.0)
                     (vec2 1.0 1.0))
                  (swizzle temporal-jitter :xy)))
         (ray (+ (* (swizzle right-now :xyz)
                    (/ (swizzle clip :x) (swizzle projection-now :x)))
                 (* (swizzle up-now :xyz)
                    (- (/ (swizzle clip :y) (swizzle projection-now :y))))
                 (swizzle forward-now :xyz)))
         ;; The sky writes no depth: it is a direction, which only the
         ;; camera's turning moves.
         (far (step 0.99999 depth))
         (distance (/ (swizzle projection-now :w)
                      (- (min depth 0.99999) (swizzle projection-now :z))))
         (relative
           (mix (+ (* ray distance)
                   (- (swizzle camera-now :xyz) (swizzle camera-then :xyz)))
                ray far))
         (now (* (+ clip (vec2 1.0 1.0)) 0.5))
         (motion (* (- (temporal-project-then relative) now)
                    (- 1.0 locked)))
         (low (temporal-neighbourhood min scene-color centre size))
         (high (temporal-neighbourhood max scene-color centre size)))
    (set-output low-output (vec4 low (swizzle motion :x)))
    (set-output high-output (vec4 high (swizzle motion :y)))))

(defun temporal-neighbourhood-fragment-specification ()
  (shader-specification-for :temporal-neighbourhood :fragment))

(define-shader-abstraction temporal-catmull-rom
    (texture sampler w0 w12 w3 at0 at12 at3)
  ;; Bicubic Catmull-Rom from five bilinear taps; the four corner taps carry
  ;; little weight and are dropped.  A plain bilinear fetch would blur
  ;; history a little more on every frame it is reprojected.
  (flet ((tap (x y wx wy)
           `(* (swizzle (sample ,texture ,sampler
                                (vec2 (swizzle ,x :x) (swizzle ,y :y)))
                        :xyz)
               (* (swizzle ,wx :x) (swizzle ,wy :y)))))
    `(/ (+ ,(tap at12 at0 w12 w0) ,(tap at0 at12 w0 w12)
           ,(tap at12 at12 w12 w12) ,(tap at3 at12 w3 w12)
           ,(tap at12 at3 w12 w3))
        (+ (* (swizzle ,w12 :x) (swizzle ,w0 :y))
           (* (swizzle ,w0 :x) (swizzle ,w12 :y))
           (* (swizzle ,w12 :x) (swizzle ,w12 :y))
           (* (swizzle ,w3 :x) (swizzle ,w12 :y))
           (* (swizzle ,w12 :x) (swizzle ,w3 :y))))))

(define-shader-method shader-specification-for
    temporal-resolve-fragment-specification
    ((role (eql :temporal-resolve)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((uv-input :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((scene-color :texture-2d :set 0 :binding 0
                   :sample-transfer :identity)
      (bounds-low :texture-2d :set 0 :binding 1
                  :sample-transfer :identity)
      (bounds-high :texture-2d :set 0 :binding 2
                   :sample-transfer :identity)
      (history :texture-2d :set 0 :binding 3
               :sample-transfer :identity)
      (temporal-sampler :sampler :set 0 :binding 4)
      (temporal-state :uniform-block :set 0 :binding 5
                      :members #.*temporal-uniform-members*)))
  (let* ((size (swizzle temporal-extent :xy))
         (output-size (swizzle temporal-extent :zw))
         ;; The scene was rasterized displaced by half its clip jitter, so the
         ;; unjittered estimate of this output pixel lies there.
         (here (+ uv-input (* (swizzle temporal-jitter :xy) 0.5)))
         (fresh (sample scene-color temporal-sampler here))
         (centre (temporal-texel (floor (* here size)) 0.0 0.0 size))
         (below (texel-load bounds-low centre))
         (above (texel-load bounds-high centre))
         (motion (vec2 (swizzle below :w) (swizzle above :w)))
         (history-uv (+ uv-input motion))
         (inside
           (* (* (step 0.0 (swizzle history-uv :x))
                 (step (swizzle history-uv :x) 1.0))
              (* (step 0.0 (swizzle history-uv :y))
                 (step (swizzle history-uv :y) 1.0))))
         (position (* history-uv output-size))
         (middle (+ (floor (- position (vec2 0.5 0.5))) (vec2 0.5 0.5)))
         (f (- position middle))
         (w0 (* f (+ (vec2 -0.5 -0.5) (* f (- (vec2 1.0 1.0) (* f 0.5))))))
         (w1 (+ (vec2 1.0 1.0) (* f (* f (+ (vec2 -2.5 -2.5) (* f 1.5))))))
         (w2 (* f (+ (vec2 0.5 0.5) (* f (- (vec2 2.0 2.0) (* f 1.5))))))
         (w3 (* f (* f (- (* f 0.5) (vec2 0.5 0.5)))))
         (w12 (+ w1 w2))
         (at0 (/ (- middle (vec2 1.0 1.0)) output-size))
         (at3 (/ (+ middle (vec2 2.0 2.0)) output-size))
         (at12 (/ (+ middle (/ w2 w12)) output-size))
         (old (temporal-rgb-to-ycocg
               (temporal-catmull-rom history temporal-sampler
                                     w0 w12 w3 at0 at12 at3)))
         (clipped (clamp old (swizzle below :xyz) (swizzle above :xyz)))
         (speed (clamp (* (sqrt (dot motion motion)) 48.0) 0.0 1.0))
         ;; CAMERA-NOW.W is the history share for a still pixel, and zero
         ;; when there is no history to share.
         (weight (* (swizzle camera-now :w) inside
                    (- 1.0 (* speed 0.35))))
         (resolved (mix (swizzle fresh :xyz)
                        (temporal-ycocg-to-rgb clipped)
                        weight)))
    (set-output color-output (vec4 resolved (swizzle fresh :w)))))

(defun temporal-resolve-fragment-specification ()
  (shader-specification-for :temporal-resolve :fragment))

(defun temporal-uniform-block ()
  "The temporal uniform block exactly as the resolve stage declares it."
  (find-if (lambda (resource) (typep resource 'shader-uniform-block))
           (shader-specification-resources
            (temporal-resolve-fragment-specification))))
