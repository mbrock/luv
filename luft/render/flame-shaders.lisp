(in-package #:luft.render)

;;; Animated torch-flame substrate
;;;
;;; A torch remains a semantic oriented face attachment.  This file compiles
;;; that sparse identity to exactly one UVec4 while leaving renderer residency,
;;; upload, and draw ownership to the caller.  Its CPU functions are deliberately
;;; scalar references for the shader field and integral rather than a second
;;; retained representation.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +torch-flame-instance-word-count+ 4)
  (defconstant +torch-flame-sample-count+ 9)
  (defconstant +torch-flame-cell-size+ 8.0f0)
  (defconstant +torch-flame-wick-offset+ 0.5f0)
  (defconstant +torch-flame-length+ 0.42f0)
  (defconstant +torch-flame-proxy-radius+ 0.38f0)
  (defconstant +torch-flame-wall-bend+ 0.055f0)
  (defconstant +torch-flame-extinction+ 6.4f0))

(deftype torch-flame-orientation-code () '(integer 0 5))
(deftype torch-flame-instance-words ()
  '(simple-array (unsigned-byte 32) (4)))

(defun torch-flame-orientation-code (axis side)
  "Return the stable three-bit code for oriented AXIS/SIDE."
  (check-type axis luft:axis)
  (check-type side luft:side)
  (+ (* 2 (ecase axis (:x 0) (:y 1) (:z 2)))
     (if (eq side :high) 1 0)))

(defun torch-flame-orientation-axis (code)
  (check-type code torch-flame-orientation-code)
  (ecase (ash code -1) (0 :x) (1 :y) (2 :z)))

(defun torch-flame-orientation-side (code)
  (check-type code torch-flame-orientation-code)
  (if (oddp code) :high :low))

(defun torch-flame-orientation-normal (code)
  "Return CODE's exact outward normal as three integer values."
  (check-type code torch-flame-orientation-code)
  (ecase code
    (0 (values -1 0 0))
    (1 (values 1 0 0))
    (2 (values 0 -1 0))
    (3 (values 0 1 0))
    (4 (values 0 0 -1))
    (5 (values 0 0 1))))

(defun %torch-flame-face-orientation-code (face)
  (multiple-value-bind (nx ny nz) (luft:face-oriented-normal face)
    (cond ((minusp nx) 0) ((plusp nx) 1)
          ((minusp ny) 2) ((plusp ny) 3)
          ((minusp nz) 4) ((plusp nz) 5)
          (t (error "Torch face ~S has no oriented normal." face)))))

(defun pack-torch-flame-attachment (face)
  "Pack oriented FACE as center ticks XYZ plus one three-bit orientation code.

The result is exactly one GPU UVec4.  Reserved bits in W are zero; animation
phase is derived from XYZ so immutable scene publication needs no per-frame
instance rewrite."
  (unless (= 2 (luft:site-dimension face))
    (error "A torch flame needs an oriented face site, not ~S." face))
  (let ((words (make-array +torch-flame-instance-word-count+
                           :element-type '(unsigned-byte 32))))
    (loop for coordinate in
          (list (luft:site-x face) (luft:site-y face) (luft:site-z face))
          for axis-number below 3
          do (setf (aref words axis-number)
                   (+ (* 8 coordinate)
                      (if (logbitp axis-number (luft:site-extent face)) 4 0))))
    (setf (aref words 3) (%torch-flame-face-orientation-code face))
    words))

(defun unpack-torch-flame-attachment (words &optional (offset 0))
  "Return center ticks, AXIS, and SIDE from one packed attachment in WORDS."
  (check-type words (array (unsigned-byte 32) (*)))
  (check-type offset (integer 0 *))
  (unless (<= (+ offset +torch-flame-instance-word-count+) (length words))
    (error "Torch flame UVec4 at ~D exceeds a ~D-word array."
           offset (length words)))
  (let ((code (aref words (+ offset 3))))
    (unless (typep code 'torch-flame-orientation-code)
      (error "Invalid packed torch-flame orientation word ~D." code))
    (values (aref words offset)
            (aref words (+ offset 1))
            (aref words (+ offset 2))
            (torch-flame-orientation-axis code)
            (torch-flame-orientation-side code))))

(defun torch-flame-effect-uniform-data (time &optional (previous-time time))
  "Return one float32 Vec4 holding current time, previous time, and two zeros.

The caller owns both clock values; this interface never consults wall time, so
captures and CPU/GPU comparisons can replay the flame field exactly."
  (check-type time real)
  (check-type previous-time real)
  (make-array 4 :element-type 'single-float
                :initial-contents
                (list (coerce time 'single-float)
                      (coerce previous-time 'single-float) 0.0f0 0.0f0)))

(declaim (inline %torch-flame-clamp %torch-flame-smoothstep
                 %torch-flame-fract))

(defun %torch-flame-clamp (value low high)
  (max low (min high value)))

(defun %torch-flame-smoothstep (low high value)
  (let ((amount (%torch-flame-clamp (/ (- value low) (- high low)) 0.0 1.0)))
    (* amount amount (- 3.0 (* 2.0 amount)))))

(defun %torch-flame-fract (value)
  (- value (floor value)))

(defun %torch-flame-reference-frame (code)
  (multiple-value-bind (nx ny nz) (torch-flame-orientation-normal code)
    (if (not (zerop nz))
        (values nx ny nz 1.0 0.0 0.0 0.0 1.0 0.0 0.0)
        (values nx ny nz ny (- nx) 0.0 0.0 0.0 1.0 1.0))))

(defun %torch-flame-reference-seed (tick-x tick-y tick-z)
  (%torch-flame-fract
   (* (sin (+ (* tick-x 0.01731) (* tick-y 0.01173) (* tick-z 0.02357)))
      43758.5453)))

(defun %torch-flame-reference-field
    (instance point-x point-y point-z time)
  (let* ((tick-x (aref instance 0))
         (tick-y (aref instance 1))
         (tick-z (aref instance 2))
         (code (aref instance 3))
         (face-x (/ tick-x +torch-flame-cell-size+))
         (face-y (/ tick-y +torch-flame-cell-size+))
         (face-z (/ tick-z +torch-flame-cell-size+))
         (seed (%torch-flame-reference-seed tick-x tick-y tick-z)))
    (check-type code torch-flame-orientation-code)
    (multiple-value-bind (nx ny nz ux uy uz vx vy vz wallness)
        (%torch-flame-reference-frame code)
      (let* ((wick-x (+ face-x (* nx +torch-flame-wick-offset+)))
             (wick-y (+ face-y (* ny +torch-flame-wick-offset+)))
             (wick-z (+ face-z (* nz +torch-flame-wick-offset+)))
             (qx (- point-x wick-x))
             (qy (- point-y wick-y))
             (qz (- point-z wick-z))
             (axial (/ (+ (* qx nx) (* qy ny) (* qz nz))
                       +torch-flame-length+))
             (height (%torch-flame-clamp axial 0.0 1.0))
             (height-squared (* height height))
             (sway-u (* 0.105 height-squared
                        (sin (+ (* time 5.1) (* seed 19.7)
                                (* height 5.3)))))
             (sway-v (+ (* 0.075 height-squared
                           (sin (+ (* time 6.7) (* seed 31.1)
                                   (* height 7.1))))
                        (* +torch-flame-wall-bend+ wallness height-squared)))
             (center-x (+ wick-x (* nx +torch-flame-length+ height)
                          (* ux sway-u) (* vx sway-v)))
             (center-y (+ wick-y (* ny +torch-flame-length+ height)
                          (* uy sway-u) (* vy sway-v)))
             (center-z (+ wick-z (* nz +torch-flame-length+ height)
                          (* uz sway-u) (* vz sway-v)))
             (rx (- point-x center-x))
             (ry (- point-y center-y))
             (rz (- point-z center-z))
             (radial (sqrt (+ (* rx rx) (* ry ry) (* rz rz))))
             (bulge (%torch-flame-smoothstep 0.0 0.22 height))
             (radius (* (- 1.0 height) (+ 0.055 (* 0.13 bulge))))
             (signed-distance (- radial radius))
             (begin (%torch-flame-smoothstep -0.02 0.08 axial))
             (end (- 1.0 (%torch-flame-smoothstep 0.78 1.04 axial)))
             (inside (- 1.0 (%torch-flame-smoothstep
                             -0.025 0.035 signed-distance)))
             (wave (+ 0.5 (* 0.5
                             (sin (+ (* point-x 17.1) (* point-y 13.7)
                                     (* point-z 19.3) (* time -8.1)
                                     (* seed 23.9))))))
             (fine (+ 0.5 (* 0.5
                             (sin (+ (* point-x -31.7) (* point-y 27.3)
                                     (* point-z 23.1) (* time 11.3)
                                     (* seed 7.7))))))
             (density (* begin end inside
                         (+ 0.68 (* 0.22 wave) (* 0.10 fine))))
             (centrality
               (%torch-flame-clamp
                (/ (- signed-distance) (max radius 0.001)) 0.0 1.0))
             (heat (%torch-flame-clamp
                    (+ 0.20 (* 0.95 centrality) (* -0.32 height)) 0.0 1.0)))
        (values signed-distance density heat)))))

(defun torch-flame-reference-signed-distance
    (instance point-x point-y point-z time)
  "Evaluate the scalar flame envelope used by the GPU at one world point."
  (nth-value 0 (%torch-flame-reference-field
                instance point-x point-y point-z time)))

(defun torch-flame-reference-density
    (instance point-x point-y point-z time)
  "Evaluate the animated scalar extinction density at one world point."
  (nth-value 1 (%torch-flame-reference-field
                instance point-x point-y point-z time)))

(defun torch-flame-reference-integrate-ray
    (instance origin-x origin-y origin-z ray-x ray-y ray-z time)
  "Return the fixed-sample premultiplied HDR RGBA flame integral along RAY."
  (let* ((ray-length (sqrt (+ (* ray-x ray-x) (* ray-y ray-y)
                              (* ray-z ray-z)))))
    (when (zerop ray-length)
      (error "A torch-flame reference ray must have nonzero length."))
    (let* ((ray-x (/ ray-x ray-length))
           (ray-y (/ ray-y ray-length))
           (ray-z (/ ray-z ray-length))
           (path-length (* 2.0 +torch-flame-proxy-radius+))
           (step-length (/ path-length +torch-flame-sample-count+))
           (red 0.0) (green 0.0) (blue 0.0) (alpha 0.0))
      (dotimes (sample +torch-flame-sample-count+)
        (let ((travel (* (+ sample 0.5) step-length)))
          (multiple-value-bind (distance density heat)
              (%torch-flame-reference-field
               instance
               (+ origin-x (* ray-x travel))
               (+ origin-y (* ray-y travel))
               (+ origin-z (* ray-z travel)) time)
            (declare (ignore distance))
            (let* ((sample-alpha
                     (- 1.0 (exp (- (* density +torch-flame-extinction+
                                        step-length)))))
                   (transmittance (- 1.0 alpha))
                   (weight (* transmittance sample-alpha))
                   (emission-red (+ 2.6 (* heat 3.6)))
                   (emission-green (+ 0.16 (* heat 3.04)))
                   (emission-blue (+ 0.018 (* heat 0.702))))
              (incf red (* weight emission-red))
              (incf green (* weight emission-green))
              (incf blue (* weight emission-blue))
              (incf alpha weight)))))
      (values red green blue alpha))))

(in-package #:luft.render.shaders)

(define-shader-function torch-flame-orientation-normal (orientation)
  (if (= orientation (uint 0.0))
      (vec3 -1.0 0.0 0.0)
      (if (= orientation (uint 1.0))
          (vec3 1.0 0.0 0.0)
          (if (= orientation (uint 2.0))
              (vec3 0.0 -1.0 0.0)
              (if (= orientation (uint 3.0))
                  (vec3 0.0 1.0 0.0)
                  (if (= orientation (uint 4.0))
                      (vec3 0.0 0.0 -1.0)
                      (vec3 0.0 0.0 1.0)))))))

(define-shader-function torch-flame-tangent-u (normal)
  (if (> (abs (swizzle normal :z)) 0.5)
      (vec3 1.0 0.0 0.0)
      (vec3 (swizzle normal :y) (- (swizzle normal :x)) 0.0)))

(define-shader-function torch-flame-tangent-v (normal)
  (if (> (abs (swizzle normal :z)) 0.5)
      (vec3 0.0 1.0 0.0)
      (vec3 0.0 0.0 1.0)))

(define-shader-function torch-flame-seed (ticks)
  (fract
   (* (sin (+ (* (swizzle ticks :x) 0.01731)
              (* (swizzle ticks :y) 0.01173)
              (* (swizzle ticks :z) 0.02357)))
      43758.5453)))

(define-shader-function torch-flame-field
    (point face-center normal seed time)
  "Return signed distance, density, heat, and radius at POINT."
  (let* ((tangent-u (torch-flame-tangent-u normal))
         (tangent-v (torch-flame-tangent-v normal))
         (wallness (- 1.0 (abs (swizzle normal :z))))
         (wick (+ face-center
                  (* normal #.luft.render::+torch-flame-wick-offset+)))
         (offset (- point wick))
         (axial (/ (dot offset normal)
                   #.luft.render::+torch-flame-length+))
         (height (clamp axial 0.0 1.0))
         (height-squared (* height height))
         (sway-u
           (* 0.105 height-squared
              (sin (+ (* time 5.1) (* seed 19.7) (* height 5.3)))))
         (sway-v
           (+ (* 0.075 height-squared
                 (sin (+ (* time 6.7) (* seed 31.1) (* height 7.1))))
              (* #.luft.render::+torch-flame-wall-bend+
                 wallness height-squared)))
         (center (+ wick
                    (* normal (* #.luft.render::+torch-flame-length+ height))
                    (* tangent-u sway-u) (* tangent-v sway-v)))
         (radial (sqrt (max (dot (- point center) (- point center)) 1e-12)))
         (bulge (smoothstep 0.0 0.22 height))
         (radius (* (- 1.0 height) (+ 0.055 (* 0.13 bulge))))
         (signed-distance (- radial radius))
         (begin (smoothstep -0.02 0.08 axial))
         (end (- 1.0 (smoothstep 0.78 1.04 axial)))
         (inside (- 1.0 (smoothstep -0.025 0.035 signed-distance)))
         (wave
           (+ 0.5
              (* 0.5
                 (sin (+ (* (swizzle point :x) 17.1)
                         (* (swizzle point :y) 13.7)
                         (* (swizzle point :z) 19.3)
                         (* time -8.1) (* seed 23.9))))))
         (fine
           (+ 0.5
              (* 0.5
                 (sin (+ (* (swizzle point :x) -31.7)
                         (* (swizzle point :y) 27.3)
                         (* (swizzle point :z) 23.1)
                         (* time 11.3) (* seed 7.7))))))
         (density (* begin end inside
                     (+ 0.68 (* 0.22 wave) (* 0.10 fine))))
         (centrality
           (clamp (/ (- signed-distance) (max radius 0.001)) 0.0 1.0))
         (heat (clamp (+ 0.20 (* 0.95 centrality) (* -0.32 height))
                      0.0 1.0)))
    (vec4 signed-distance density heat radius)))

(define-live-shader torch-flame-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (proxy-world-position-output :vec3 :location 0)
               (face-center-output :vec3 :location 1 :interpolation :flat)
               (normal-output :vec3 :location 2 :interpolation :flat)
               (seed-output :float :location 3 :interpolation :flat)
               (current-clip-output :vec4 :location 4)
               (previous-clip-output :vec4 :location 5))
     :resources
     ((flame-instances :storage-buffer :binding 0 :element :uvec4)
      (camera-state :uniform-block :binding 1
       :members ((camera-position :vec4)
                 (camera-right :vec4)
                 (camera-up :vec4)
                 (camera-forward :vec4)
                 (camera-projection :vec4)
                 (render-parameters :vec4)
                 (previous-camera-position :vec4)
                 (previous-camera-right :vec4)
                 (previous-camera-up :vec4)
                 (previous-camera-forward :vec4)
                 (previous-camera-projection :vec4)
                 (temporal-parameters :vec4)))))
  (let* ((instance (buffer-element flame-instances instance-index))
         (ticks (vec3 (float (swizzle instance :x))
                      (float (swizzle instance :y))
                      (float (swizzle instance :z))))
         (orientation (ldb (byte 3 0) (swizzle instance :w)))
         (normal (torch-flame-orientation-normal orientation))
         (face-center (/ ticks #.luft.render::+torch-flame-cell-size+))
         (volume-center
           (+ face-center
              (* normal
                 (+ #.luft.render::+torch-flame-wick-offset+
                    (* #.luft.render::+torch-flame-length+ 0.5)))))
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
                 (* (swizzle camera-forward :xyz)
                    #.luft.render::+torch-flame-proxy-radius+))
              (* (swizzle camera-right :xyz)
                 (* (swizzle corner :x)
                    #.luft.render::+torch-flame-proxy-radius+))
              (* (swizzle camera-up :xyz)
                 (* (swizzle corner :y)
                    #.luft.render::+torch-flame-proxy-radius+))))
         (previous-proxy-world-position
           (+ (- volume-center
                 (* (swizzle previous-camera-forward :xyz)
                    #.luft.render::+torch-flame-proxy-radius+))
              (* (swizzle previous-camera-right :xyz)
                 (* (swizzle corner :x)
                    #.luft.render::+torch-flame-proxy-radius+))
              (* (swizzle previous-camera-up :xyz)
                 (* (swizzle corner :y)
                    #.luft.render::+torch-flame-proxy-radius+))))
         (current-clip
           (mesh-view-clip proxy-world-position camera-position camera-right
                           camera-up camera-forward camera-projection
                           (swizzle render-parameters :z)))
         (previous-clip
           (mesh-view-clip previous-proxy-world-position
                           previous-camera-position previous-camera-right
                           previous-camera-up previous-camera-forward
                           previous-camera-projection
                           (swizzle render-parameters :z)))
         (jitter (swizzle temporal-parameters :xy)))
    (set-output clip-position
                (vec4 (+ (swizzle current-clip :x)
                         (* (swizzle jitter :x) (swizzle current-clip :w)))
                      (+ (swizzle current-clip :y)
                         (* (swizzle jitter :y) (swizzle current-clip :w)))
                      (swizzle current-clip :z)
                      (swizzle current-clip :w)))
    (set-output proxy-world-position-output proxy-world-position)
    (set-output face-center-output face-center)
    (set-output normal-output normal)
    (set-output seed-output (torch-flame-seed ticks))
    (set-output current-clip-output current-clip)
    (set-output previous-clip-output previous-clip)))

(define-live-shader torch-flame-fragment-specification
    (:stage :fragment
     :inputs ((proxy-world-position :vec3 :location 0)
              (face-center :vec3 :location 1 :interpolation :flat)
              (normal :vec3 :location 2 :interpolation :flat)
              (seed :float :location 3 :interpolation :flat)
              (current-clip :vec4 :location 4)
              (previous-clip :vec4 :location 5))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources
     ((camera-state :uniform-block :binding 1
       :members ((camera-position :vec4)
                 (camera-right :vec4)
                 (camera-up :vec4)
                 (camera-forward :vec4)
                 (camera-projection :vec4)
                 (render-parameters :vec4)))
      (effect-state :uniform-block :binding 2
       :members ((flame-effect-parameters :vec4)))))
  (let* ((ray (if (< (swizzle render-parameters :z) 0.5)
                  (normalize (swizzle camera-forward :xyz))
                  (normalize (- proxy-world-position
                                (swizzle camera-position :xyz)))))
         (time (swizzle flame-effect-parameters :x))
         (path-length (* 2.0 #.luft.render::+torch-flame-proxy-radius+))
         (step-length (/ path-length
                         (float #.luft.render::+torch-flame-sample-count+)))
         (integrated
           (counted-fold
               (sample (float #.luft.render::+torch-flame-sample-count+)
                state (vec4 0.0 0.0 0.0 0.0))
             (let* ((travel (* (+ sample 0.5) step-length))
                    (point (+ proxy-world-position (* ray travel)))
                    (field (torch-flame-field
                            point face-center normal seed time))
                    (density (swizzle field :y))
                    (heat (swizzle field :z))
                    (sample-alpha
                      (- 1.0
                         (exp (- (* density
                                    #.luft.render::+torch-flame-extinction+
                                    step-length)))))
                    (transmittance (- 1.0 (swizzle state :w)))
                    (weight (* transmittance sample-alpha))
                    (emission
                      (mix (vec3 2.6 0.16 0.018)
                           (vec3 6.2 3.2 0.72) heat)))
               (vec4 (+ (swizzle state :xyz) (* emission weight))
                     (+ (swizzle state :w) weight))))))
    (set-output color-output integrated)
    (set-output motion-output
                (mesh-temporal-motion previous-clip current-clip))))
