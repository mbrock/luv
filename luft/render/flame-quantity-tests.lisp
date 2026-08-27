(in-package #:luft.render.tests)

(defun torch-quantity-projection (layout positions)
  (or (luv.arithmetic:project-quantity-layout layout positions)
      (error "Torch product has no quantity projection at ~S." positions)))

(defun shader-declaration-named (name declarations)
  (or (find name declarations :key #'luv.shader:shader-object-name :test #'eq)
      (error "Shader has no declaration named ~S." name)))

(deftest torch-frame-is-a-declared-product-with-categorical-holes
  (let* ((declaration
           (luv.arithmetic:value-declaration-for
            'luft.render::torch-flame-frame-data))
         (layout (luv.arithmetic:declaration-quantity-layout declaration))
         (frame
           (render:pack-torch-flame-frame
            2.0f0 3.0f0 4.0f0 0.25f0
            0.0f0 0.0f0 1.0f0 17
            1.0f0 0.0f0 0.0f0 1.5f0)))
    (ok (typep frame
               (luv.arithmetic:declaration-representation-type declaration)))
    (ok (= render:+torch-flame-instance-scalar-count+
           (luv.arithmetic:quantity-layout-extent layout)))
    (let ((origin (torch-quantity-projection layout '(0 1 2)))
          (normal (torch-quantity-projection layout '(4 5 6)))
          (tangent (torch-quantity-projection layout '(8 9 10)))
          (scale (torch-quantity-projection layout '(11))))
      (ok (eq luft.render.quantities:world-position
              (luv.arithmetic:quantity-specification-name origin)))
      (ok (eq :point
              (luv.arithmetic:quantity-specification-character origin)))
      (ok (eq luft.render.quantities:world-direction
              (luv.arithmetic:quantity-specification-name normal)))
      (ok (luv.arithmetic:quantity-specification= normal tangent))
      (ok (eq luft.render.quantities:spatial-scale
              (luv.arithmetic:quantity-specification-name scale)))
      (ok (luv.arithmetic:unit-expression=
           luft.render.quantities:cell
           (luv.arithmetic:quantity-specification-unit scale))))
    ;; Random phase and packed material/light bits are categorical words.
    (ok (null (luv.arithmetic:project-quantity-layout layout '(3))))
    (ok (null (luv.arithmetic:project-quantity-layout layout '(7))))
    ;; The represented product supplies physical type and extent.  The
    ;; refinement validator still owns finiteness, range, and frame geometry.
    (ok (signals
         (render:validate-torch-flame-frame
          (make-array 12 :element-type 'single-float
                         :adjustable t :initial-element 0.0f0))
         'error))
    (ok (signals
         (render:pack-torch-flame-frame
          0 0 0 1.0 0 0 1 0 1 0 0 1)
         'error))
    (ok (signals
         (render:pack-torch-flame-frame
          0 0 0 0.0 0 0 2 0 1 0 0 1)
         'error))
    (ok (signals
         (render:pack-torch-flame-frame
          0 0 0 0.0 0 0 1 0 0 0 1 0)
         'error))))

(deftest torch-effect-product-matches-its-shader-uniform-member
  (let* ((declaration
           (luv.arithmetic:value-declaration-for
            'render:torch-flame-effect-uniform-data))
         (host-layout
           (luv.arithmetic:declaration-quantity-layout declaration))
         (specification
           (luft.render.shaders:torch-flame-fragment-specification))
         (effect-block
           (find 2 (luv.shader:shader-specification-resources specification)
                 :key #'luv.shader:shader-resource-binding))
         (member
           (shader-declaration-named
            'luft.render.shaders::flame-effect-parameters
            (luv.shader:shader-uniform-block-members effect-block)))
         (shader-layout
           (luv.arithmetic:declaration-quantity-layout member))
         (data (render:torch-flame-effect-uniform-data 2.5f0)))
    (ok (typep data
               (luv.arithmetic:declaration-representation-type declaration)))
    (ok (= 4 (luv.arithmetic:quantity-layout-extent host-layout)))
    (ok (= 16 (luft.render::torch-flame-effect-byte-size)))
    (ok (luv.arithmetic:quantity-layout= host-layout shader-layout))
    (ok (eq luft.render.quantities:elapsed-time
            (luv.arithmetic:quantity-specification-name
             (torch-quantity-projection host-layout '(0)))))
    (ok (eq luft.render.quantities:scene-radiance
            (luv.arithmetic:quantity-specification-name
             (torch-quantity-projection host-layout '(1 2 3)))))
    (ok (signals
         (luft.render::ensure-torch-flame-effect-representation
          (make-array 4 :element-type 'double-float :initial-element 0.0d0))
         'error))
    (ok (signals (render:torch-flame-effect-uniform-data -0.25f0) 'error))
    (ok (signals
         (luft.render::ensure-torch-flame-effect-representation
          (make-array 4 :element-type 'single-float
                        :initial-contents '(0.0f0 -0.1f0 0.0f0 0.0f0)))
         'error))
    (ok (signals
         (luft.render::ensure-torch-flame-effect-representation
          (make-array
           4 :element-type 'single-float
             :initial-contents
             (list (sb-kernel:make-single-float #x7fc00000)
                   0.0f0 0.0f0 0.0f0)))
         'error))))

(deftest torch-frame-storage-assumptions-reach-matching-varyings
  (let* ((host-layout
           (luv.arithmetic:declaration-quantity-layout
            (luv.arithmetic:value-declaration-for
             'luft.render::torch-flame-frame-data)))
         (vertex (luft.render.shaders:torch-flame-vertex-specification))
         (fragment (luft.render.shaders:torch-flame-fragment-specification))
         (vertex-outputs (luv.shader:shader-specification-outputs vertex))
         (fragment-inputs (luv.shader:shader-specification-inputs fragment)))
    (dolist (claim
             `((luft.render.shaders::proxy-world-position-output
                luft.render.shaders::proxy-world-position (0 1 2))
               (luft.render.shaders::origin-output
                luft.render.shaders::origin (0 1 2))
               (luft.render.shaders::normal-output
                luft.render.shaders::normal (4 5 6))
               (luft.render.shaders::tangent-output
                luft.render.shaders::tangent (8 9 10))))
      (destructuring-bind (output-name input-name host-positions) claim
        (let ((output (shader-declaration-named output-name vertex-outputs))
              (input (shader-declaration-named input-name fragment-inputs))
              (host (torch-quantity-projection host-layout host-positions)))
          (ok (luv.arithmetic:quantity-specification=
               host (luv.arithmetic:declaration-quantity-specification output)))
          (ok (luv.arithmetic:quantity-specification=
               (luv.arithmetic:declaration-quantity-specification output)
               (luv.arithmetic:declaration-quantity-specification input))))))))

(deftest canonical-and-torch-meshes-share-quantity-carrying-varyings
  (let* ((mesh (luft.render.shaders:mesh-vertex-specification))
         (torch (luft.render.shaders:torch-body-vertex-specification))
         (fragment (luft.render.shaders:mesh-fragment-specification)))
    (dolist (claim
             '((luft.render.shaders::world-position-output
                luft.render.shaders::world-position)
               (luft.render.shaders::mesh-normal-output
                luft.render.shaders::mesh-normal)
               (luft.render.shaders::shadow-sample-output
                luft.render.shaders::shadow-sample)))
      (destructuring-bind (output-name input-name) claim
        (let* ((mesh-output
                 (shader-declaration-named
                  output-name (luv.shader:shader-specification-outputs mesh)))
               (torch-output
                 (shader-declaration-named
                  output-name (luv.shader:shader-specification-outputs torch)))
               (fragment-input
                 (shader-declaration-named
                  input-name (luv.shader:shader-specification-inputs fragment)))
               (expected
                 (luv.arithmetic:declaration-quantity-specification
                  fragment-input)))
          (ok expected)
          (ok (luv.arithmetic:quantity-specification=
               expected
               (luv.arithmetic:declaration-quantity-specification
                mesh-output)))
          (ok (luv.arithmetic:quantity-specification=
               expected
               (luv.arithmetic:declaration-quantity-specification
                torch-output))))))))
