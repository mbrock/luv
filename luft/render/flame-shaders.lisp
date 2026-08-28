(in-package #:luft.render)

;;; Animated torch-flame substrate
;;;
;;; A torch remains a sparse semantic attachment, while its rendered body and
;;; flame share one frame realized against the final surface.  The GPU boundary
;;; is deliberately plain: three Vec4 rows containing origin/seed,
;;; normal/flags, and tangent/scale.  Its CPU functions are scalar references
;;; for the shader field and integral rather than a second retained
;;; representation.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %torch-flame-product-extent-from-members (members)
    "Derive and check the dense scalar extent of product MEMBERS."
    (let ((positions
            (sort
             (loop for member in members append (copy-list (second member)))
             #'<)))
      (unless positions
        (error "A torch product must declare at least one lane."))
      (let ((extent (1+ (car (last positions)))))
        (unless (equal positions (loop for lane below extent collect lane))
          (error "Torch product lanes are not a dense, unique interval: ~S"
                 positions))
        extent)))

  (defun torch-flame-frame-product-members ()
    "Return the canonical semantic and categorical frame-lane declaration."
    '((origin (0 1 2)
       (:quantity quantities:world-position
        :unit quantities:cell :tensor-order 1))
      (seed (3) nil)
      (normal (4 5 6)
       (:quantity quantities:world-direction :unit :one :tensor-order 1))
      (packed-flags (7) nil)
      (tangent (8 9 10)
       (:quantity quantities:world-direction :unit :one :tensor-order 1))
      (scale (11)
       (:quantity quantities:spatial-scale :unit quantities:cell))))

  (defun torch-flame-effect-product-members ()
    "Return the canonical semantic effect-uniform lane declaration."
    '((elapsed-time (0)
       (:quantity quantities:elapsed-time :unit :second))
      (radiance (1 2 3)
       (:quantity quantities:scene-radiance :unit :one :tensor-order 1))))

  (defconstant +torch-flame-instance-scalar-count+
    (%torch-flame-product-extent-from-members
     (torch-flame-frame-product-members)))
  (assert (zerop (mod +torch-flame-instance-scalar-count+ 4)))
  (defconstant +torch-flame-instance-row-count+
    (/ +torch-flame-instance-scalar-count+ 4))
  (defconstant +torch-flame-effect-scalar-count+
    (%torch-flame-product-extent-from-members
     (torch-flame-effect-product-members)))
  (defconstant +torch-flame-sample-count+ 9)
  (defconstant +torch-flame-wick-offset+ 0.5f0)
  (defconstant +torch-flame-length+ 0.42f0)
  (defconstant +torch-flame-proxy-radius+ 0.38f0)
  (defconstant +torch-flame-wall-bend+ 0.055f0)
  (defconstant +torch-flame-extinction+ 6.4f0)
  ;; The authored material supplies hue and baseline HDR strength.  Heat is a
  ;; dimensionless scalar response, not a second hidden RGB palette.
  (defconstant +torch-flame-cool-radiance-scale+ 1.0f0)
  (defconstant +torch-flame-heat-radiance-gain+ 2.0f0)
  (defconstant +torch-flame-frame-tolerance+ 2.0f-4)
  (defconstant +torch-flame-maximum-flags+ #.(1- (ash 1 24)))
  ;; The exact float lane carries sampled light beside the realized frame, so
  ;; flame and body publication remain one immutable transaction.
  (defconstant +torch-body-light-bit-count+ 12)
  (defconstant +torch-body-vertex-row-count+ 2)
  (defconstant +torch-body-vertex-scalar-count+ 8)
  (defconstant +torch-body-side-count+ 8))

(deftype torch-flame-instance-data ()
  `(simple-array single-float (,+torch-flame-instance-scalar-count+)))

(deftype torch-flame-effect-data ()
  `(simple-array single-float (,+torch-flame-effect-scalar-count+)))

(defun make-torch-flame-frame-product-layout ()
  "Describe one origin/normal/tangent torch frame in its twelve float lanes.

SEED at lane 3 and packed FLAGS at lane 7 are categorical representations,
not quantities.  SCALE is a non-negative spatial amount measured in cells;
it maps the canonical torch body's dimensionless coordinates into the world."
  (let ((members (torch-flame-frame-product-members)))
    (luv.arithmetic:make-quantity-layout
     (%torch-flame-product-extent-from-members members)
     (loop for member in members
           for positions = (second member)
           for options = (third member)
           when options
             collect
             (luv.arithmetic:make-quantity-projection
              positions
              (luv.arithmetic:make-declared-quantity-specification options))))))

(defun make-torch-flame-effect-product-layout ()
  "Describe elapsed seconds and relative scene-linear HDR RGB in one Vec4."
  (let ((members (torch-flame-effect-product-members)))
    (luv.arithmetic:make-quantity-layout
     (%torch-flame-product-extent-from-members members)
     (loop for member in members
           for positions = (second member)
           for options = (third member)
           collect
           (luv.arithmetic:make-quantity-projection
            positions
            (luv.arithmetic:make-declared-quantity-specification options))))))

(defmethod luv.arithmetic:value-declaration-for
    ((name (eql 'torch-flame-frame-data)))
  (declare (ignore name))
  (load-time-value
   (let ((representation-type
           `(simple-array single-float
                          (,+torch-flame-instance-scalar-count+))))
     (luv.arithmetic:make-represented-value-declaration
      :representation-type representation-type
      :quantity-layout (make-torch-flame-frame-product-layout)
      :source-form
      `(torch-flame-frame-data
        :type ,representation-type
        :product ,(torch-flame-frame-product-members))))))

(defmethod luv.arithmetic:value-declaration-for
    ((name (eql 'torch-flame-effect-uniform-data)))
  (declare (ignore name))
  (load-time-value
   (let ((representation-type
           `(simple-array single-float
                          (,+torch-flame-effect-scalar-count+))))
     (luv.arithmetic:make-represented-value-declaration
      :representation-type representation-type
      :quantity-layout (make-torch-flame-effect-product-layout)
      :source-form
      `(torch-flame-effect-uniform-data
        :type ,representation-type
        :product ,(torch-flame-effect-product-members))))))

(defun torch-flame-frame-declaration ()
  (or (luv.arithmetic:value-declaration-for 'torch-flame-frame-data)
      (error "The torch-flame frame has no represented-value declaration.")))

(defun torch-flame-effect-declaration ()
  (or (luv.arithmetic:value-declaration-for
       'torch-flame-effect-uniform-data)
      (error "The torch-flame effect has no represented-value declaration.")))

(defun torch-flame-frame-product-extent ()
  (luv.arithmetic:quantity-layout-extent
   (luv.arithmetic:declaration-quantity-layout
    (torch-flame-frame-declaration))))

(defun torch-flame-effect-product-extent ()
  (luv.arithmetic:quantity-layout-extent
   (luv.arithmetic:declaration-quantity-layout
    (torch-flame-effect-declaration))))

(defun torch-flame-effect-byte-size ()
  "Return the byte extent owned by the declared torch effect product."
  (* 4 (torch-flame-effect-product-extent)))

(declaim (inline %torch-flame-finite-single-float-p))

(defun %torch-flame-finite-single-float-p (value)
  (and (= value value)
       (<= (abs value) most-positive-single-float)))

(defun ensure-torch-flame-effect-representation (data)
  "Require DATA to realize the declared, finite torch-effect product."
  (let* ((declaration (torch-flame-effect-declaration))
         (layout (luv.arithmetic:declaration-quantity-layout declaration))
         (extent (luv.arithmetic:quantity-layout-extent layout)))
    (unless (typep data
                   (luv.arithmetic:declaration-representation-type
                    declaration))
      (error "Torch effect data ~S does not satisfy represented type ~S."
             (type-of data)
             (luv.arithmetic:declaration-representation-type declaration)))
    (unless (= (length data) extent)
      (error "Torch effect data has ~D lanes, not its declared product extent."
             (length data)))
    (loop for index below extent
          for value = (aref data index)
          unless (%torch-flame-finite-single-float-p value)
            do (error "Torch effect scalar ~D is not finite: ~S."
                      index value))
    ;; Non-negativity is semantic declaration data, not another handwritten
    ;; copy of the elapsed-time and scene-radiance lane map.
    (dolist (projection (luv.arithmetic:quantity-layout-projections layout))
      (when (luv.arithmetic:quantity-specification-non-negative-p
             (luv.arithmetic:quantity-projection-specification projection))
        (dolist (position
                 (luv.arithmetic:quantity-projection-positions projection))
          (when (minusp (aref data position))
            (error "Torch effect quantity at lane ~D is negative: ~S."
                   position (aref data position))))))
    data))

(defun pack-torch-body-frame-flags (packed-voxel-light)
  "Pack RGB4 voxel light into a torch frame's FLAGS lane."
  (check-type packed-voxel-light (unsigned-byte 12))
  packed-voxel-light)

(defun unpack-torch-body-frame-flags (flags)
  "Return the packed RGB4 voxel light encoded by FLAGS."
  (unless (and (realp flags)
               (= flags (floor flags))
               (<= 0 flags +torch-flame-maximum-flags+))
    (error "Torch body flags are not an exact 24-bit nonnegative integer: ~S."
           flags))
  (ldb (byte +torch-body-light-bit-count+ 0) (floor flags)))

(defun validate-torch-flame-frame (data &optional (offset 0))
  "Validate one three-Vec4 torch frame in DATA starting at OFFSET.

The represented-value declaration supplies the scalar type and product extent.
The normal and tangent must additionally be finite, unit length, and mutually
orthogonal.  FLAGS is an exactly represented nonnegative integer in the normal
row's W lane, SEED is in [0,1), and SCALE is strictly positive.  Return DATA."
  (check-type data (array single-float (*)))
  (check-type offset (integer 0 *))
  (let ((extent (torch-flame-frame-product-extent)))
    (unless (<= (+ offset extent) (length data))
      (error "Torch frame at ~D exceeds a ~D-scalar array."
             offset (length data)))
    ;; The exact one-frame case must satisfy the declaration's physical type.
    ;; Larger population buffers validate the same declared product by window.
    (when (and (zerop offset) (= (length data) extent))
      (let ((declaration (torch-flame-frame-declaration)))
        (unless (typep data
                       (luv.arithmetic:declaration-representation-type
                        declaration))
          (error "Torch frame data ~S does not satisfy represented type ~S."
                 (type-of data)
                 (luv.arithmetic:declaration-representation-type
                  declaration)))))
    (loop for index from offset below (+ offset extent)
          for value = (aref data index)
          unless (%torch-flame-finite-single-float-p value)
            do (error "Torch frame scalar ~D is not finite: ~S." index value)))
  (let* ((seed (aref data (+ offset 3)))
         (nx (aref data (+ offset 4)))
         (ny (aref data (+ offset 5)))
         (nz (aref data (+ offset 6)))
         (flags (aref data (+ offset 7)))
         (tx (aref data (+ offset 8)))
         (ty (aref data (+ offset 9)))
         (tz (aref data (+ offset 10)))
         (scale (aref data (+ offset 11)))
         (normal-length-squared (+ (* nx nx) (* ny ny) (* nz nz)))
         (tangent-length-squared (+ (* tx tx) (* ty ty) (* tz tz)))
         (normal-tangent-dot (+ (* nx tx) (* ny ty) (* nz tz))))
    (unless (and (<= 0.0f0 seed) (< seed 1.0f0))
      (error "Torch frame seed must be in [0,1), not ~S." seed))
    (unless (<= (abs (- normal-length-squared 1.0f0))
                +torch-flame-frame-tolerance+)
      (error "Torch frame normal is not unit length: (~S ~S ~S)."
             nx ny nz))
    (unless (<= (abs (- tangent-length-squared 1.0f0))
                +torch-flame-frame-tolerance+)
      (error "Torch frame tangent is not unit length: (~S ~S ~S)."
             tx ty tz))
    (unless (<= (abs normal-tangent-dot) +torch-flame-frame-tolerance+)
      (error "Torch frame normal and tangent are not orthogonal: ~S."
             normal-tangent-dot))
    (unless (and (<= 0.0f0 flags (coerce +torch-flame-maximum-flags+
                                          'single-float))
                 (= flags (floor flags)))
      (error "Torch frame flags are not an exact 24-bit nonnegative integer: ~S."
             flags))
    (unless (plusp scale)
      (error "Torch frame scale must be positive, not ~S." scale)))
  data)

(defun pack-torch-flame-frame
    (origin-x origin-y origin-z seed
     normal-x normal-y normal-z flags
     tangent-x tangent-y tangent-z scale)
  "Pack and validate one arbitrary realized torch frame as three Vec4 rows."
  (let ((data
          (make-array
           +torch-flame-instance-scalar-count+ :element-type 'single-float
           :initial-contents
           (mapcar (lambda (value) (coerce value 'single-float))
                   (list origin-x origin-y origin-z seed
                         normal-x normal-y normal-z flags
                         tangent-x tangent-y tangent-z scale)))))
    (validate-torch-flame-frame data)
    data))

(defun unpack-torch-flame-frame (data &optional (offset 0))
  "Return origin, seed, normal, flags, tangent, and scale for one frame."
  (validate-torch-flame-frame data offset)
  (values-list
   (loop for index from offset below (+ offset +torch-flame-instance-scalar-count+)
         collect (aref data index))))

(defun %torch-flame-reference-seed (tick-x tick-y tick-z)
  (let ((value
          (* (sin (+ (* tick-x 0.01731) (* tick-y 0.01173)
                     (* tick-z 0.02357)))
             43758.5453)))
    (- value (floor value))))

(defun torch-flame-face-seed (face)
  "Return FACE's stable animation seed without realizing a surface frame."
  (unless (= 2 (luft:site-dimension face))
    (error "A torch seed needs an oriented face site, not ~S." face))
  (let ((ticks
          (loop for coordinate in
                (list (luft:site-x face)
                      (luft:site-y face)
                      (luft:site-z face))
                for axis-number below 3
                collect (+ (* 8 coordinate)
                           (if (logbitp axis-number (luft:site-extent face))
                               4 0)))))
    (%torch-flame-reference-seed
     (first ticks) (second ticks) (third ticks))))

(defun %torch-flame-authored-hdr-radiance ()
  "Return the flame material's authored linear HDR radiance as three values."
  (destructuring-bind (red green blue)
      (material-kind-base-tone *torch-flame-material*)
    (let ((strength
            (material-kind-surface-emission *torch-flame-material*)))
      (unless (and (realp strength) (not (minusp strength))
                   (every (lambda (channel)
                            (and (realp channel) (not (minusp channel))))
                          (list red green blue)))
        (error "Torch flame radiance must be nonnegative, not tone ~S at ~S."
               (list red green blue) strength))
      (values (* red strength) (* green strength) (* blue strength)))))

(defun torch-flame-effect-uniform-data (time)
  "Return one float32 Vec4 holding current time and authored linear HDR RGB.

The RGB lanes are the flame material's base tone times its surface emission.
The caller owns the clock; this interface never consults wall time, so captures
and CPU/GPU comparisons can replay the flame effect exactly."
  (check-type time real)
  (multiple-value-bind (red green blue)
      (%torch-flame-authored-hdr-radiance)
    (ensure-torch-flame-effect-representation
     (make-array 4 :element-type 'single-float
                   :initial-contents
                   (mapcar (lambda (value) (coerce value 'single-float))
                           (list time red green blue))))))

(declaim (inline %torch-flame-clamp %torch-flame-smoothstep
                 %torch-flame-fract))

(defun %torch-flame-clamp (value low high)
  (max low (min high value)))

(defun %torch-flame-smoothstep (low high value)
  (let ((amount (%torch-flame-clamp (/ (- value low) (- high low)) 0.0 1.0)))
    (* amount amount (- 3.0 (* 2.0 amount)))))

(defun %torch-flame-fract (value)
  (- value (floor value)))

(defun %torch-flame-reference-field
    (instance point-x point-y point-z time)
  (validate-torch-flame-frame instance)
  (let* ((origin-x (aref instance 0))
         (origin-y (aref instance 1))
         (origin-z (aref instance 2))
         (seed (aref instance 3))
         (nx (aref instance 4))
         (ny (aref instance 5))
         (nz (aref instance 6))
         (tx (aref instance 8))
         (ty (aref instance 9))
         (tz (aref instance 10))
         (scale (aref instance 11))
         ;; B = N x T, hence T x B = N for the validated orthonormal pair.
         (bx (- (* ny tz) (* nz ty)))
         (by (- (* nz tx) (* nx tz)))
         (bz (- (* nx ty) (* ny tx)))
         ;; Project world up into the actual surface tangent plane.  Its length
         ;; continuously replaces the old axis-specific wallness switch.
         (gravity-x (* (- nz) nx))
         (gravity-y (* (- nz) ny))
         (gravity-z (- 1.0 (* nz nz)))
         (gravity-strength
           (sqrt (max 0.0 (+ (* gravity-x gravity-x)
                             (* gravity-y gravity-y)
                             (* gravity-z gravity-z)))))
         (gravity-divisor (max gravity-strength 1.0e-6))
         (gravity-x (/ gravity-x gravity-divisor))
         (gravity-y (/ gravity-y gravity-divisor))
         (gravity-z (/ gravity-z gravity-divisor))
         (wick-distance (* +torch-flame-wick-offset+ scale))
         (flame-length (* +torch-flame-length+ scale))
         (wick-x (+ origin-x (* nx wick-distance)))
         (wick-y (+ origin-y (* ny wick-distance)))
         (wick-z (+ origin-z (* nz wick-distance)))
         (qx (- point-x wick-x))
         (qy (- point-y wick-y))
         (qz (- point-z wick-z))
         (axial (/ (+ (* qx nx) (* qy ny) (* qz nz)) flame-length))
         (height (%torch-flame-clamp axial 0.0 1.0))
         (height-squared (* height height))
         (sway-u (* scale 0.105 height-squared
                    (sin (+ (* time 5.1) (* seed 19.7) (* height 5.3)))))
         (sway-v (* scale 0.075 height-squared
                    (sin (+ (* time 6.7) (* seed 31.1) (* height 7.1)))))
         (gravity-bend (* scale +torch-flame-wall-bend+ gravity-strength
                          height-squared))
         (center-x (+ wick-x (* nx flame-length height)
                      (* tx sway-u) (* bx sway-v)
                      (* gravity-x gravity-bend)))
         (center-y (+ wick-y (* ny flame-length height)
                      (* ty sway-u) (* by sway-v)
                      (* gravity-y gravity-bend)))
         (center-z (+ wick-z (* nz flame-length height)
                      (* tz sway-u) (* bz sway-v)
                      (* gravity-z gravity-bend)))
         (rx (- point-x center-x))
         (ry (- point-y center-y))
         (rz (- point-z center-z))
         (radial (sqrt (+ (* rx rx) (* ry ry) (* rz rz))))
         (bulge (%torch-flame-smoothstep 0.0 0.22 height))
         (radius (* scale (- 1.0 height) (+ 0.055 (* 0.13 bulge))))
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
    (values signed-distance density heat)))

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
    (instance origin-x origin-y origin-z ray-x ray-y ray-z time
     &key maximum-path-length)
  "Return the fixed-sample premultiplied HDR RGBA flame integral along RAY.

MAXIMUM-PATH-LENGTH, when supplied, clips the proxy chord to the visible
distance in front of opaque scene depth.  It is clamped to the canonical proxy
chord exactly as the fragment shader does, so zero is a deterministic empty
integral and callers can compare full, partial, and fully occluded rays."
  (validate-torch-flame-frame instance)
  (let* ((ray-length (sqrt (+ (* ray-x ray-x) (* ray-y ray-y)
                              (* ray-z ray-z)))))
    (when (zerop ray-length)
      (error "A torch-flame reference ray must have nonzero length."))
    (let* ((ray-x (/ ray-x ray-length))
           (ray-y (/ ray-y ray-length))
           (ray-z (/ ray-z ray-length))
           (full-path-length
             (* 2.0 +torch-flame-proxy-radius+ (aref instance 11)))
           (path-length
             (if maximum-path-length
                 (%torch-flame-clamp maximum-path-length
                                     0.0 full-path-length)
                 full-path-length))
           (step-length (/ path-length +torch-flame-sample-count+))
           (red 0.0) (green 0.0) (blue 0.0) (alpha 0.0))
      (multiple-value-bind (authored-red authored-green authored-blue)
          (%torch-flame-authored-hdr-radiance)
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
                     (radiance-scale
                       (+ +torch-flame-cool-radiance-scale+
                          (* heat +torch-flame-heat-radiance-gain+))))
                (incf red (* weight authored-red radiance-scale))
                (incf green (* weight authored-green radiance-scale))
                (incf blue (* weight authored-blue radiance-scale))
                (incf alpha weight))))))
      (values red green blue alpha))))

;;; Canonical torch body
;;;
;;; This is deliberately an expanded triangle stream rather than another
;;; semantic mesh.  Each vertex is two Vec4 rows containing local position and
;;; flat local normal; W is padding.  The attachment frame is the only
;;; world-space placement representation.

(defun %torch-body-point (radius angle height)
  (vector (coerce (* radius (cos angle)) 'single-float)
          (coerce (* radius (sin angle)) 'single-float)
          (coerce height 'single-float)))

(defun %torch-body-emit-triangle
    (data point-a point-b point-c expected-normal)
  (let* ((ab-x (- (aref point-b 0) (aref point-a 0)))
         (ab-y (- (aref point-b 1) (aref point-a 1)))
         (ab-z (- (aref point-b 2) (aref point-a 2)))
         (ac-x (- (aref point-c 0) (aref point-a 0)))
         (ac-y (- (aref point-c 1) (aref point-a 1)))
         (ac-z (- (aref point-c 2) (aref point-a 2)))
         (normal-x (- (* ab-y ac-z) (* ab-z ac-y)))
         (normal-y (- (* ab-z ac-x) (* ab-x ac-z)))
         (normal-z (- (* ab-x ac-y) (* ab-y ac-x)))
         (facing (+ (* normal-x (aref expected-normal 0))
                    (* normal-y (aref expected-normal 1))
                    (* normal-z (aref expected-normal 2)))))
    (when (minusp facing)
      (rotatef point-b point-c)
      (setf normal-x (- normal-x)
            normal-y (- normal-y)
            normal-z (- normal-z)))
    (let ((normal-length
            (sqrt (+ (* normal-x normal-x)
                     (* normal-y normal-y)
                     (* normal-z normal-z)))))
      (when (zerop normal-length)
        (error "Degenerate canonical torch-body triangle: ~S ~S ~S."
               point-a point-b point-c))
      (setf normal-x (/ normal-x normal-length)
            normal-y (/ normal-y normal-length)
            normal-z (/ normal-z normal-length))
      (loop for point in (list point-a point-b point-c)
            do (vector-push-extend (aref point 0) data)
               (vector-push-extend (aref point 1) data)
               (vector-push-extend (aref point 2) data)
               (vector-push-extend 0.0f0 data)
               (vector-push-extend (coerce normal-x 'single-float) data)
               (vector-push-extend (coerce normal-y 'single-float) data)
               (vector-push-extend (coerce normal-z 'single-float) data)
               (vector-push-extend 0.0f0 data)))))

(defun %make-torch-body-vertex-data ()
  (let ((data
          (make-array 128 :element-type 'single-float
                           :adjustable t :fill-pointer 0))
        (bottom-radius 0.145f0)
        (socket-height 0.16f0)
        (socket-radius 0.078f0)
        (shaft-height 0.50f0)
        (shaft-radius 0.055f0)
        (bottom-centre #(0.0f0 0.0f0 0.0f0))
        (top-centre #(0.0f0 0.0f0 0.50f0)))
    (labels ((angle (index)
               (* 2.0f0 (coerce pi 'single-float)
                  (/ (mod index +torch-body-side-count+)
                     +torch-body-side-count+)))
             (ring-point (radius height index)
               (%torch-body-point radius (angle index) height))
             (radial-normal (index axial)
               (let ((middle (angle (+ index 0.5f0))))
                 (vector (coerce (cos middle) 'single-float)
                         (coerce (sin middle) 'single-float)
                         (coerce axial 'single-float))))
             (emit-frustum-side
                 (index lower-radius lower-height upper-radius upper-height)
               (let* ((next (1+ index))
                      (lower-a
                        (ring-point lower-radius lower-height index))
                      (lower-b
                        (ring-point lower-radius lower-height next))
                      (upper-a
                        (ring-point upper-radius upper-height index))
                      (upper-b
                        (ring-point upper-radius upper-height next))
                      (axial
                        (/ (- lower-radius upper-radius)
                           (- upper-height lower-height)))
                      (expected (radial-normal index axial)))
                 (%torch-body-emit-triangle
                  data lower-a lower-b upper-b expected)
                 (%torch-body-emit-triangle
                  data lower-a upper-b upper-a expected))))
      (dotimes (side +torch-body-side-count+)
        (let ((bottom-a (ring-point bottom-radius 0.0f0 side))
              (bottom-b (ring-point bottom-radius 0.0f0 (1+ side))))
          (%torch-body-emit-triangle
           data bottom-centre bottom-b bottom-a #(0.0f0 0.0f0 -1.0f0)))
        (emit-frustum-side
         side bottom-radius 0.0f0 socket-radius socket-height)
        (emit-frustum-side
         side socket-radius socket-height shaft-radius shaft-height)
        (let ((top-a (ring-point shaft-radius shaft-height side))
              (top-b (ring-point shaft-radius shaft-height (1+ side))))
          (%torch-body-emit-triangle
           data top-centre top-a top-b #(0.0f0 0.0f0 1.0f0)))))
    (make-array (length data) :element-type 'single-float
                              :initial-contents data)))

(defparameter *torch-body-vertex-data* (%make-torch-body-vertex-data))

(defun torch-body-vertex-count ()
  "Return the canonical expanded triangle vertex count for one torch body."
  (/ (length *torch-body-vertex-data*)
     +torch-body-vertex-scalar-count+))

(defun torch-body-vertex-data ()
  "Return a fresh float32 copy of the canonical two-Vec4-per-vertex body."
  (copy-seq *torch-body-vertex-data*))

(defun torch-body-reference-vertex (frame vertex-index)
  "Transform one canonical body vertex through arbitrary realized FRAME.

Return world position and normalized world normal.  This is the scalar oracle
for both body vertex shaders."
  (validate-torch-flame-frame frame)
  (check-type vertex-index (integer 0 *))
  (unless (< vertex-index (torch-body-vertex-count))
    (error "Torch body vertex ~D exceeds the ~D-vertex canonical body."
           vertex-index (torch-body-vertex-count)))
  (let* ((offset (* vertex-index +torch-body-vertex-scalar-count+))
         (local-x (aref *torch-body-vertex-data* offset))
         (local-y (aref *torch-body-vertex-data* (+ offset 1)))
         (local-z (aref *torch-body-vertex-data* (+ offset 2)))
         (local-normal-x (aref *torch-body-vertex-data* (+ offset 4)))
         (local-normal-y (aref *torch-body-vertex-data* (+ offset 5)))
         (local-normal-z (aref *torch-body-vertex-data* (+ offset 6)))
         (origin-x (aref frame 0))
         (origin-y (aref frame 1))
         (origin-z (aref frame 2))
         (normal-x (aref frame 4))
         (normal-y (aref frame 5))
         (normal-z (aref frame 6))
         (tangent-x (aref frame 8))
         (tangent-y (aref frame 9))
         (tangent-z (aref frame 10))
         (scale (aref frame 11))
         ;; B=NxT, so local X/Y/Z map to a right-handed T/B/N frame.
         (bitangent-x (- (* normal-y tangent-z)
                         (* normal-z tangent-y)))
         (bitangent-y (- (* normal-z tangent-x)
                         (* normal-x tangent-z)))
         (bitangent-z (- (* normal-x tangent-y)
                         (* normal-y tangent-x)))
         (world-x
           (+ origin-x
              (* scale (+ (* local-x tangent-x)
                          (* local-y bitangent-x)
                          (* local-z normal-x)))))
         (world-y
           (+ origin-y
              (* scale (+ (* local-x tangent-y)
                          (* local-y bitangent-y)
                          (* local-z normal-y)))))
         (world-z
           (+ origin-z
              (* scale (+ (* local-x tangent-z)
                          (* local-y bitangent-z)
                          (* local-z normal-z)))))
         (world-normal-x
           (+ (* local-normal-x tangent-x)
              (* local-normal-y bitangent-x)
              (* local-normal-z normal-x)))
         (world-normal-y
           (+ (* local-normal-x tangent-y)
              (* local-normal-y bitangent-y)
              (* local-normal-z normal-y)))
         (world-normal-z
           (+ (* local-normal-x tangent-z)
              (* local-normal-y bitangent-z)
              (* local-normal-z normal-z)))
         (world-normal-length
           (sqrt (+ (* world-normal-x world-normal-x)
                    (* world-normal-y world-normal-y)
                    (* world-normal-z world-normal-z)))))
    (values world-x world-y world-z
            (/ world-normal-x world-normal-length)
            (/ world-normal-y world-normal-length)
            (/ world-normal-z world-normal-length))))

(in-package #:luft.render.shaders)

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

(define-live-shader torch-flame-composite-copy-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((scene :texture-2d :binding 0 :sample-transfer :identity)
                 (scene-sampler :sampler :binding 1)))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5))))
    (set-output color-output (sample scene scene-sampler uv))))

;;; Production module manifest ----------------------------------------------

(defparameter *production-shader-specifications*
  '(mesh-vertex-specification
    star-fragment-specification
    shadow-vertex-specification
    player-sdf-vertex-specification
    player-sdf-fragment-specification
    lattice-point-vertex-specification
    lattice-point-fragment-specification
    present-vertex-specification
    present-fragment-specification
    sky-fragment-specification
    sky-temporal-fragment-specification
    exposure-probe-fragment-specification
    temporal-resolve-fragment-specification
    torch-body-vertex-specification
    torch-body-fragment-specification
    torch-body-shadow-vertex-specification
    torch-flame-vertex-specification
    torch-flame-fragment-specification
    torch-flame-composite-copy-fragment-specification)
  "Every production LUFT shader specification, in stable emission order.")

(defun write-production-spir-v (&optional (directory #p"build/"))
  "Assemble every production LUFT shader into DIRECTORY as luft-*.spv.

This is the byte-identity oracle for shader-source migrations: emit before,
emit after, and compare hashes.  Returns the written pathnames."
  (loop for name in *production-shader-specifications*
        for specification = (funcall name)
        for path = (merge-pathnames
                    (format nil "luft-~(~a~).spv" name) directory)
        do (luv.spir-v:write-spir-v
            (luv.spir-v:assemble-shader-specification specification)
            path)
        collect path))
