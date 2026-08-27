(in-package #:luft.render.tests)

(defun scene-frame-prefix-layout (layout extent)
  "Return the projections wholly contained in the first EXTENT lanes."
  (luv.arithmetic:make-quantity-layout
   extent
   (remove-if-not
    (lambda (projection)
      (every (lambda (position) (< position extent))
             (luv.arithmetic:quantity-projection-positions projection)))
    (luv.arithmetic:quantity-layout-projections layout))))

(defun scene-frame-block-p (resource)
  (and (typep resource 'luv.shader:shader-uniform-block)
       (let ((first
               (first (luv.shader:shader-uniform-block-members resource))))
         (and first
              (string= "CAMERA-POSITION"
                       (symbol-name (luv.shader:shader-object-name first)))))))

(defun shader-binding-named (name specification)
  (find name (luv.shader:shader-specification-bindings specification)
        :key #'luv.shader:shader-object-name
        :test (lambda (left right)
                (string= (symbol-name left) (symbol-name right)))))

(define-test scene-frame-is-one-declared-108-float-product
  (let* ((declaration
           (luv.arithmetic:value-declaration-for
            'luft.render::camera-uniform-data))
         (layout
           (luv.arithmetic:declaration-quantity-layout declaration)))
    (true (eq declaration
              (luv.arithmetic:value-declaration-for
               'luft.render::camera-uniform-data)))
    (true (equal '(simple-array single-float (108))
                 (luv.arithmetic:declaration-representation-type declaration)))
    (true (= 108 (luv.arithmetic:quantity-layout-extent layout)))
    (true (= 432 (luft.render.shaders::scene-uniform-byte-size)))
    (flet ((quantity-at (positions)
             (luv.arithmetic:project-quantity-layout layout positions)))
      (true (eq luft.render.quantities:world-position
                (luv.arithmetic:quantity-specification-name
                 (quantity-at '(0 1 2)))))
      (true (eq :point
                (luv.arithmetic:quantity-specification-character
                 (quantity-at '(0 1 2)))))
      (true (eq luft.render.quantities:temporal-jitter
                (luv.arithmetic:quantity-specification-name
                 (quantity-at '(44 45)))))
      (true (eq luft.render.quantities:bevel-proportion
                (luv.arithmetic:quantity-specification-name
                 (quantity-at '(20)))))
      (true (eq luft.render.quantities:construction-line-strength
                (luv.arithmetic:quantity-specification-name
                 (quantity-at '(21)))))
      (true (eq luft.render.quantities:inspection-ink-strength
                (luv.arithmetic:quantity-specification-name
                 (quantity-at '(23)))))
      (true (eq luft.render.quantities:texture-coordinate
                (luv.arithmetic:quantity-specification-name
                 (quantity-at '(48 49)))))
      (true (eq luft.render.quantities:gait-phase
                (luv.arithmetic:quantity-specification-name
                 (quantity-at '(55)))))
      (true (luv.arithmetic:quantity-specification=
             (quantity-at '(55)) (quantity-at '(95))))
      (true (eq luft.render.quantities:scene-radiance
                (luv.arithmetic:quantity-specification-name
                 (quantity-at '(64 65 66)))))
      (true (eq luft.render.quantities:exposure
                (luv.arithmetic:quantity-specification-name
                 (quantity-at '(67)))))
      (true (eq luft.render.quantities:world-distance
                (luv.arithmetic:quantity-specification-name
                 (quantity-at '(103)))))
      (true (eq luft.render.quantities:horizontal-direction
                (luv.arithmetic:quantity-specification-name
                 (quantity-at '(96 97)))))
      (true (eq luft.render.quantities:horizontal-x-direction
                (luv.arithmetic:quantity-specification-name
                 (quantity-at '(98)))))
      (true (eq luft.render.quantities:spell-flash
                (luv.arithmetic:quantity-specification-name
                 (quantity-at '(99)))))
      ;; Projection rows, categorical controls, history flags, and the dense
      ;; shadow matrix deliberately remain representation.
      (dolist (position '(3 16 17 18 19 22 27 31 35 39
                          40 41 42 43 59 63 71 72 73 74 75
                          76 77 78 79 80 81
                          82 83 84 85 86 87))
        (true (null (quantity-at (list position))))))))

(define-test every-scene-shader-prefix-matches-the-host-product
  (let* ((host
           (luv.arithmetic:declaration-quantity-layout
            (luv.arithmetic:value-declaration-for
             'luft.render::camera-uniform-data)))
         (blocks nil))
    (dolist (name
             luft.render.shaders:*production-shader-specifications*)
      (let ((specification (funcall name)))
        (dolist (resource
                 (luv.shader:shader-specification-resources specification))
          (when (scene-frame-block-p resource)
            (push resource blocks)))))
    (true (= 14 (length blocks)))
    (dolist (block blocks)
      (let* ((member-count
               (length (luv.shader:shader-uniform-block-members block)))
             (members (luv.shader:shader-uniform-block-members block))
             (expected
               (luft.render.shaders::scene-uniform-prefix member-count))
             (extent (* member-count 4))
             (shader-layout
               (luft.render.shaders::shader-uniform-product-layout block)))
        (true (<= member-count 27))
        ;; Layout equality covers the semantic projections; names and offsets
        ;; additionally protect the raw rows whose order is still ABI.
        (loop for member in members
              for source-member in expected
              for index from 0
              do (true (eq (first source-member)
                           (luv.shader:shader-object-name member)))
                 (true (eq :vec4 (second source-member)))
                 (true (= (* index 16)
                          (luv.shader:shader-uniform-member-offset member))))
        (true (= extent
                 (/ (luv.shader:shader-uniform-block-byte-size block) 4)))
        (true (luv.arithmetic:quantity-layout=
               (scene-frame-prefix-layout host extent) shader-layout))))))

(define-test presentation-ladder-retains-named-quantities-at-its-boundaries
  (flet ((binding-quantity (name specification)
           (let ((binding (shader-binding-named name specification)))
             (true binding)
             (and binding
                  (luv.shader:shader-expression-quantity-specification
                   (luv.shader:shader-binding-expression binding))))))
    (let* ((probe
             (luft.render.shaders:exposure-probe-fragment-specification))
           (present
             (luft.render.shaders:present-fragment-specification))
           (average (binding-quantity 'average probe))
           (luminance (binding-quantity 'luminance probe))
           (exposure (binding-quantity 'auto-exposure present))
           (radiance (binding-quantity 'radiance present))
           (exposed (binding-quantity 'exposed-radiance present))
           (presented (binding-quantity 'presented present)))
      (true (eq luft.render.quantities:scene-radiance
                (luv.arithmetic:quantity-specification-name average)))
      (true (eq luft.render.quantities:scene-luminance
                (luv.arithmetic:quantity-specification-name luminance)))
      (true (eq luft.render.quantities:exposure
                (luv.arithmetic:quantity-specification-name exposure)))
      (dolist (specification (list radiance exposed))
        (true (eq luft.render.quantities:scene-radiance
                  (luv.arithmetic:quantity-specification-name specification))))
      (true (eq luft.render.quantities:presented-color
               (luv.arithmetic:quantity-specification-name presented))))))

(define-test declared-frame-products-own-their-upload-buffer-sizes
  (multiple-value-bind (renderer device)
      (make-renderer-target-probe nil)
    (declare (ignore device))
    (let ((state nil))
      (unwind-protect
           (progn
             (setf state (luft.render::make-renderer-frame-state renderer))
             (true (= (luft.render.shaders::scene-uniform-byte-size)
                      (luv::buffer-descriptor-size
                       (flame-resource-probe-descriptor
                        (luft.render::renderer-frame-state-camera-buffer
                         state)))))
             (true (= (luft.render::torch-flame-effect-byte-size)
                      (luv::buffer-descriptor-size
                       (flame-resource-probe-descriptor
                        (luft.render::renderer-frame-state-flame-effect-buffer
                         state))))))
        (when state
          (luft.render::destroy-renderer-frame-state state))))))
