(in-package #:luft.render)

;;; Torch attachment frames and animated effect inputs
;;;
;;; Bodies and flames consume the same three Vec4 rows: origin/seed,
;;; normal/flags, and tangent/scale. This file defines and validates that ABI
;;; and the per-frame effect uniform. Geometry, CPU reference calculations,
;;; and shaders live in torch-body, torch-reference, and torch-shaders.

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
    (math:make-quantity-layout
     (%torch-flame-product-extent-from-members members)
     (loop for member in members
           for positions = (second member)
           for options = (third member)
           when options
             collect
             (math:make-quantity-projection
              positions
              (math:make-declared-quantity-specification options))))))

(defun make-torch-flame-effect-product-layout ()
  "Describe elapsed seconds and relative scene-linear HDR RGB in one Vec4."
  (let ((members (torch-flame-effect-product-members)))
    (math:make-quantity-layout
     (%torch-flame-product-extent-from-members members)
     (loop for member in members
           for positions = (second member)
           for options = (third member)
           collect
           (math:make-quantity-projection
            positions
            (math:make-declared-quantity-specification options))))))

(defmethod math:value-declaration-for
    ((name (eql 'torch-flame-frame-data)))
  (declare (ignore name))
  (load-time-value
   (let ((representation-type
           `(simple-array single-float
                          (,+torch-flame-instance-scalar-count+))))
     (math:make-represented-value-declaration
      :representation-type representation-type
      :quantity-layout (make-torch-flame-frame-product-layout)
      :source-form
      `(torch-flame-frame-data
        :type ,representation-type
        :product ,(torch-flame-frame-product-members))))))

(defmethod math:value-declaration-for
    ((name (eql 'torch-flame-effect-uniform-data)))
  (declare (ignore name))
  (load-time-value
   (let ((representation-type
           `(simple-array single-float
                          (,+torch-flame-effect-scalar-count+))))
     (math:make-represented-value-declaration
      :representation-type representation-type
      :quantity-layout (make-torch-flame-effect-product-layout)
      :source-form
      `(torch-flame-effect-uniform-data
        :type ,representation-type
        :product ,(torch-flame-effect-product-members))))))

(defun torch-flame-frame-declaration ()
  (or (math:value-declaration-for 'torch-flame-frame-data)
      (error "The torch-flame frame has no represented-value declaration.")))

(defun torch-flame-effect-declaration ()
  (or (math:value-declaration-for
       'torch-flame-effect-uniform-data)
      (error "The torch-flame effect has no represented-value declaration.")))

(defun torch-flame-frame-product-extent ()
  (math:quantity-layout-extent
   (math:declaration-quantity-layout
    (torch-flame-frame-declaration))))

(defun torch-flame-effect-product-extent ()
  (math:quantity-layout-extent
   (math:declaration-quantity-layout
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
         (layout (math:declaration-quantity-layout declaration))
         (extent (math:quantity-layout-extent layout)))
    (unless (typep data
                   (math:declaration-representation-type
                    declaration))
      (error "Torch effect data ~S does not satisfy represented type ~S."
             (type-of data)
             (math:declaration-representation-type declaration)))
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
    (dolist (projection (math:quantity-layout-projections layout))
      (when (math:quantity-specification-non-negative-p
             (math:quantity-projection-specification projection))
        (dolist (position
                 (math:quantity-projection-positions projection))
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
                       (math:declaration-representation-type
                        declaration))
          (error "Torch frame data ~S does not satisfy represented type ~S."
                 (type-of data)
                 (math:declaration-representation-type
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
