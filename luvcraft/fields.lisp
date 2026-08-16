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
