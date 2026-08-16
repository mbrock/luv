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
                    :tensor-order 1))
      (projection '(3 4)
                  '(:quantity :texture-uv :unit :one
                    :tensor-order 1))
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
                    :tensor-order 1))
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
                  '(:quantity :shadow-uv :unit :one :tensor-order 1
                    :character :difference))
      (projection '(50) '(:quantity :shadow-depth :unit :one
                           :character :difference))
      (projection '(51) '(:quantity :shadow-depth :unit :one
                           :character :difference))
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

(defun shader-input-product-layout (specification)
  "Flatten location-ordered shader inputs into one packed product layout."
  (let ((offset 0) (projections nil))
    (dolist (input
             (sort (copy-list
                    (spv:shader-specification-inputs specification))
                   #'< :key #'spv:shader-interface-location))
      (let* ((width
               (spv:shader-type-component-count
                (luv.arithmetic:declaration-representation-type input)))
             (whole
               (luv.arithmetic:declaration-quantity-specification input))
             (layout
               (luv.arithmetic:declaration-quantity-layout input)))
        (when whole
          (push
           (luv.arithmetic:make-quantity-projection
            (loop for position below width collect (+ offset position))
            whole)
           projections))
        (when layout
          (dolist (projection
                   (luv.arithmetic:quantity-layout-projections layout))
            (push
             (luv.arithmetic:make-quantity-projection
              (mapcar
               (lambda (position) (+ offset position))
               (luv.arithmetic:quantity-projection-positions projection))
              (luv.arithmetic:quantity-projection-specification projection))
             projections)))
        (incf offset width)))
    (luv.arithmetic:make-quantity-layout offset (nreverse projections))))

(defun make-sky-vertex-product-layout ()
  (luv.arithmetic:make-quantity-layout
   3
   (list
    (luv.arithmetic:make-quantity-projection
     '(0 1 2)
     (luv.arithmetic:make-declared-quantity-specification
      '(:quantity :clip-coordinate :unit :one :tensor-order 1))))))

(defun make-crosshair-vertex-product-layout ()
  (luv.arithmetic:make-quantity-layout
   6
   (list
    (luv.arithmetic:make-quantity-projection
     '(0 1 2)
     (luv.arithmetic:make-declared-quantity-specification
      '(:quantity :clip-coordinate :unit :one :tensor-order 1)))
    (luv.arithmetic:make-quantity-projection
     '(3 4 5)
     (luv.arithmetic:make-declared-quantity-specification
      '(:quantity :linear-rgb :unit :one :tensor-order 1))))))

(defmethod luv.arithmetic:value-declaration-for
    ((name (eql :sky-vertices)))
  (declare (ignore name))
  (load-time-value
   (luv.arithmetic:make-represented-value-declaration
    :representation-type '(simple-array single-float (9))
    :quantity-layout
    (luv.arithmetic:make-repeated-quantity-layout
     (make-sky-vertex-product-layout) :stride 3)
    :source-form
    '(sky-vertices :type (simple-array single-float (9))
      :repeated-product ((0 1 2) clip-coordinate) :stride 3))))

(defmethod luv.arithmetic:value-declaration-for
    ((name (eql :crosshair-vertices)))
  (declare (ignore name))
  (load-time-value
   (luv.arithmetic:make-represented-value-declaration
    :representation-type '(vector single-float)
    :quantity-layout
    (luv.arithmetic:make-repeated-quantity-layout
     (make-crosshair-vertex-product-layout) :stride 6)
    :source-form
    '(crosshair-vertices :type (vector single-float)
      :repeated-product
      ((0 1 2) clip-coordinate (3 4 5) absolute-linear-rgb)
      :stride 6))))

(defun ensure-vertex-product-contract
    (vertices declaration-name vertex-count shader-specification)
  "Validate raw VERTICES against one declared CPU and shader input product."
  (let* ((declaration
           (luv.arithmetic:value-declaration-for declaration-name))
         (layout (luv.arithmetic:declaration-quantity-layout declaration)))
    (unless (typep
             vertices
             (luv.arithmetic:declaration-representation-type declaration))
      (error "Vertex data ~S does not satisfy ~S for ~S."
             (type-of vertices)
             (luv.arithmetic:declaration-representation-type declaration)
             declaration-name))
    (unless (typep layout 'luv.arithmetic:repeated-quantity-layout)
      (error "Vertex declaration ~S is not a repeated product."
             declaration-name))
    (let ((stride
            (luv.arithmetic:repeated-quantity-layout-stride layout)))
      (unless (= (length vertices) (* vertex-count stride))
        (error "Vertex data ~S has ~D lanes for ~D vertices at stride ~D."
               declaration-name (length vertices) vertex-count stride)))
    (unless
        (luv.arithmetic:quantity-layout=
         (luv.arithmetic:repeated-quantity-layout-element-layout layout)
         (shader-input-product-layout shader-specification))
      (error "Vertex semantic ABI mismatch for ~S against shader ~S."
             declaration-name
             (spv:shader-object-source-form shader-specification)))
    vertices))
