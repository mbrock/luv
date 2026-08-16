;;; Luvcraft's dense voxel fields and packed product declarations.

(in-package #:luv)

(luv.world.fields:define-voxel-field :sky-light
  :site-kind :voxel-cell
  :value-type (unsigned-byte 8)
  :quantity (:quantity :sky-propagation-level :unit :one)
  :missing-value :unavailable
  :legal-values (integer 0 15)
  :representation :u8-levels)

(luv.world.fields:define-voxel-field :block-light
  :site-kind :voxel-cell
  :value-type (unsigned-byte 8)
  :quantity (:quantity :block-propagation-level :unit :one)
  :missing-value :unavailable
  :legal-values (integer 0 15)
  :representation :u8-levels)

(defun make-block-mesh-vertex-product-layout ()
  (flet ((projection (positions options)
           (luv.arithmetic:make-quantity-projection
            positions
            (luv.arithmetic:make-declared-quantity-specification options))))
    (luv.arithmetic:make-quantity-layout
     12
     (list
      (projection '(0 1 2)
                  '(:quantity :world-position :unit :cell
                    :tensor-order 1 :affine-p t))
      (projection '(3 4)
                  '(:quantity :texture-uv :unit :one
                    :tensor-order 1 :affine-p t))
      (projection '(5) '(:quantity :ambient-occlusion :unit :one))
      (projection '(6 7 8)
                  '(:quantity :world-direction :unit :one :tensor-order 1))
      (projection '(9) '(:quantity :sky-light-level :unit :one))
      (projection '(10) '(:quantity :block-light-level :unit :one))
      (projection '(11) '(:quantity :material-emission :unit :one))))))

(defmethod luv.arithmetic:value-declaration-for
    ((name (eql :block-mesh-vertices)))
  (declare (ignore name))
  (load-time-value
   (luv.arithmetic:make-represented-value-declaration
    :representation-type '(vector single-float)
    :quantity-layout
    (luv.arithmetic:make-repeated-quantity-layout
     (make-block-mesh-vertex-product-layout) :stride 12)
    :source-form
    '(block-mesh-vertices
      :type (vector single-float)
      :repeated-product
      ((0 1 2) world-position
       (3 4) texture-uv
       (5) ambient-occlusion
       (6 7 8) world-direction
       (9) sky-light-level
       (10) block-light-level
       (11) material-emission)
      :stride 12))))

(defun make-frame-uniform-product-layout ()
  "Describe the semantic lanes in luvcraft's fixed 72-float frame block."
  (flet ((projection (positions options)
           (luv.arithmetic:make-quantity-projection
            positions
            (luv.arithmetic:make-declared-quantity-specification options))))
    (luv.arithmetic:make-quantity-layout
     72
     (list
      (projection '(0 1 2)
                  '(:quantity :world-position :unit :cell
                    :tensor-order 1 :affine-p t))
      (projection '(4 5 6)
                  '(:quantity :world-direction :unit :one :tensor-order 1))
      (projection '(8 9 10)
                  '(:quantity :world-direction :unit :one :tensor-order 1))
      (projection '(12 13 14)
                  '(:quantity :world-direction :unit :one :tensor-order 1))
      (projection '(16) '(:quantity :projection-scale :unit :one))
      (projection '(17) '(:quantity :projection-scale :unit :one))
      (projection '(18) '(:quantity :projection-scale :unit :one))
      (projection '(19) '(:quantity :view-distance :unit :cell))
      (projection '(20) '(:quantity :view-distance :unit :cell))
      (projection '(21) '(:quantity :view-distance :unit :cell))
      (projection '(24 25 26)
                  '(:quantity :world-direction :unit :one :tensor-order 1))
      (projection '(27) '(:quantity :day-factor :unit :one))
      (projection '(28 29 30)
                  '(:quantity :linear-rgb :unit :one :tensor-order 1))
      (projection '(31) '(:quantity :sun-disc-coordinate :unit :one))
      (projection '(32 33 34)
                  '(:quantity :linear-rgb :unit :one :tensor-order 1))
      (projection '(36 37 38)
                  '(:quantity :linear-rgb :unit :one :tensor-order 1))
      (projection '(40 41 42)
                  '(:quantity :linear-rgb :unit :one :tensor-order 1))
      (projection '(44 45 46)
                  '(:quantity :linear-rgb :unit :one :tensor-order 1))
      (projection '(47) '(:quantity :shadow-diagnostic :unit :one))
      (projection '(48 49)
                  '(:quantity :shadow-uv :unit :one :tensor-order 1))
      (projection '(50) '(:quantity :shadow-depth :unit :one))
      (projection '(51) '(:quantity :shadow-depth :unit :one))
      (projection '(52) '(:quantity :world-distance :unit :cell))
      (projection '(53) '(:quantity :world-distance :unit :cell))
      (projection '(54) '(:quantity :shadow-filter-radius :unit :one))
      (projection '(55) '(:quantity :shadow-filter-radius :unit :one))))))

(defun make-camera-uniform-product-layout ()
  "Return the five-vec4 camera prefix of the full frame product."
  (let ((frame (make-frame-uniform-product-layout)))
    (luv.arithmetic:make-quantity-layout
     20
     (remove-if
      (lambda (projection)
        (some (lambda (position) (>= position 20))
              (luv.arithmetic:quantity-projection-positions projection)))
      (luv.arithmetic:quantity-layout-projections frame)))))

(defmethod luv.arithmetic:value-declaration-for
    ((name (eql :camera-uniform-data)))
  (declare (ignore name))
  (load-time-value
   (luv.arithmetic:make-represented-value-declaration
    :representation-type '(simple-array single-float (20))
    :quantity-layout (make-camera-uniform-product-layout)
    :source-form
    '(camera-uniform-data
      :type (simple-array single-float (20))
      :prefix-of :frame-uniform-data))))

(defmethod luv.arithmetic:value-declaration-for
    ((name (eql :frame-uniform-data)))
  (declare (ignore name))
  (load-time-value
   (luv.arithmetic:make-represented-value-declaration
    :representation-type '(simple-array single-float (72))
    :quantity-layout (make-frame-uniform-product-layout)
    :source-form
    '(frame-uniform-data
      :type (simple-array single-float (72))
      :product
      ((0 1 2) world-position
       (4 5 6) right-direction
       (8 9 10) up-direction
       (12 13 14) forward-direction
       (16 17 18) projection-scales
       (19 20 21) view-distances
       (24 25 26) sun-direction
       (27) day-factor
       (28 29 30) sun-color
       (31) sun-disc-coordinate
       (32 33 34) zenith-color
       (36 37 38) horizon-color
       (40 41 42) ambient-color
       (44 45 46) fog-color
       (47) shadow-diagnostic
       (48 49) shadow-uv
       (50 51) shadow-depth
       (52 53) shadow-world-distances
       (54 55) shadow-filter-radii
       (56 57 58 59 60 61 62 63
        64 65 66 67 68 69 70 71) representation-only-shadow-rows)))))
