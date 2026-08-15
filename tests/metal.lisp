(defpackage #:luv/metal/tests
  (:use #:cl #:rove #:luv)
  (:local-nicknames (#:objc #:luv.objective-c)
                    (#:metal #:luv.metal)))

(in-package #:luv/metal/tests)

(objc:define-objective-c-message make-test-metal-layer
    ("new" :object :ownership :owned :class "CAMetalLayer"))

(defun install-metal-live-probe-vertex ()
  (eval
   '(luv.spir-v:define-shader-method
        luv.spir-v:shader-specification-for
        metal-live-probe-vertex-specification
        ((role (eql :metal-live-probe)) (stage (eql :vertex)))
        (:stage :vertex
         :inputs ((position :vec3 :location 0))
         :outputs ((clip-position :vec4 :built-in :position)))
      (let* ((clip (luv.spir-v:vec4 position 1.0)))
        (luv.spir-v:set-output clip-position clip)))))

(defun install-metal-live-probe-fragment (red &key invalid-p)
  (eval
   `(luv.spir-v:define-shader-method
        luv.spir-v:shader-specification-for
        metal-live-probe-fragment-specification
        ((role (eql :metal-live-probe)) (stage (eql :fragment)))
        (:stage ,(if invalid-p :compute :fragment)
         :outputs ((color :vec4 :location 0)))
      (let* ((rgba (luv.spir-v:vec4 ,red 0.25 0.75 1.0)))
        (luv.spir-v:set-output color rgba)))))

(deftest metal-messages-retain-structure-abi
  (let ((size
          (objc:objective-c-message-description
           'metal::%set-layer-drawable-size))
        (clear
          (objc:objective-c-message-description
           'metal::%set-color-attachment-clear-color))
        (argument-buffer
          (objc:objective-c-message-description
           'metal:set-metal-argument-table-buffer))
        (draw
          (objc:objective-c-message-description
           'metal:draw-metal-primitives)))
    (ok (equal (getf size :selector) "setDrawableSize:"))
    (ok (equal (second (second (getf size :argument-types)))
               '(:struct metal::cg-size)))
    (ok (equal (getf clear :selector) "setClearColor:"))
    (ok (equal (second (second (getf clear :argument-types)))
               '(:struct metal::mtl-clear-color)))
    (ok (equal (getf argument-buffer :selector)
               "setAddress:attributeStride:atIndex:"))
    (ok (equal (getf draw :selector)
               "drawPrimitives:vertexStart:vertexCount:instanceCount:baseInstance:"))))

(deftest unchecked-messages-preserve-by-value-structure-abi
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (layer
          (make-test-metal-layer (objc:find-objective-c-class "CAMetalLayer")))
      (objc:with-unchecked-objective-c-messages ()
        (metal:set-layer-drawable-size layer 641 359)
        (multiple-value-bind (width height)
            (metal:layer-drawable-size layer)
          (ok (= width 641.0d0))
          (ok (= height 359.0d0)))))))

(deftest canvas-presentation-policy-is-explicit-and-provider-specific
  (ok (equal (luv::sdl-presentation-window-flags :vulkan)
             '(:vulkan :resizable :hidden)))
  (ok (equal (luv::sdl-presentation-window-flags :metal)
             '(:metal :high-pixel-density :resizable :hidden)))
  (let ((canvas (make-sdl-canvas :presentation-api :vulkan)))
    (ok (signals
         (make-canvas-context canvas (make-instance 'metal-gpu-provider))
         'canvas-error))))

(deftest metal-provider-owns-a-real-metal-4-queue
  (let ((device
          (request-gpu-device (make-instance 'metal-gpu-provider))))
    (unwind-protect
         (progn
           (ok (typep device 'metal-gpu-device))
           (ok (typep (device-queue device) 'metal-gpu-queue))
           (ok (equal
                (objc:objective-c-object-protocol-name
                 (luv::metal-native-object (device-queue device)))
                "MTL4CommandQueue"))
           (ok (equal
                (objc:objective-c-object-protocol-name
                 (luv::metal-device-residency-set device))
                "MTLResidencySet"))
           (ok (equal
                (objc:objective-c-object-protocol-name
                 (luv::metal-queue-completion-event (device-queue device)))
                "MTLSharedEvent")))
      (destroy device))))

(deftest metal-device-owns-a-real-metal-4-compiler
  (let ((device
          (request-gpu-device (make-instance 'metal-gpu-provider))))
    (unwind-protect
         (ok (equal
              (objc:objective-c-object-protocol-name
               (metal-device-compiler device))
              "MTL4Compiler"))
      (destroy device))))

(deftest luvcraft-shader-compiles-in-memory-on-the-metal-device
  (let ((specification
          (luv.spir-v:block-world-fragment-specification))
        (device
          (request-gpu-device (make-instance 'metal-gpu-provider)))
        (module nil))
    (unwind-protect
         (progn
           (setf module
                 (create
                  device
                  (make-shader-module-descriptor
                   :label "block fragment Metal library"
                   :language :mathematical
                   :code specification)))
           (ok (typep module 'metal-gpu-shader-module))
           (ok (eq
                specification
                (luv.msl:msl-document-specification
                 (metal-shader-module-document module))))
           (ok (string= (metal-shader-module-entry-point module)
                        "block_world_fragment_specification"))
           (ok (= (metal-shader-module-function-type module)
                  metal:+function-type-fragment+))
           (ok (equal
                (objc:objective-c-object-protocol-name
                 (luv::metal-native-object module))
                "MTLLibrary")))
      (when module (destroy module))
      (destroy device))))

(deftest luvcraft-vertex-and-fragment-link-as-a-metal-4-render-pipeline
  (let ((device
          (request-gpu-device (make-instance 'metal-gpu-provider)))
        (vertex-module nil)
        (fragment-module nil)
        (pipeline nil))
    (unwind-protect
         (progn
           (setf vertex-module
                 (create
                  device
                  (make-shader-module-descriptor
                   :label "block vertex Metal library"
                   :language :mathematical
                   :code (luv.spir-v:block-world-vertex-specification)))
                 fragment-module
                 (create
                  device
                  (make-shader-module-descriptor
                   :label "block fragment Metal library"
                   :language :mathematical
                   :code (luv.spir-v:block-world-fragment-specification)))
                 pipeline
                 (create
                  device
                  (make-render-pipeline-descriptor
                   :label "block world Metal 4 pipeline"
                   :layout nil
                   :vertex
                   `(:module ,vertex-module
                     :buffers
                     ((:array-stride 48
                       :attributes
                       ((:shader-location 0 :offset 0 :format :float32x3)
                        (:shader-location 1 :offset 12 :format :float32x3)
                        (:shader-location 2 :offset 24 :format :float32x3)
                        (:shader-location 3 :offset 36 :format :float32x3)))))
                   :fragment
                   `(:module ,fragment-module
                     :targets ((:format :bgra8-unorm)))
                   :primitive '(:topology :triangle-list)
                   :depth-stencil
                   '(:format :depth32-float
                     :depth-write-enabled t
                     :depth-compare :less))))
           (ok (typep pipeline 'metal-gpu-render-pipeline))
           (ok (equal
                (objc:objective-c-object-protocol-name
                 (luv::metal-native-object pipeline))
                "MTLRenderPipelineState"))
           (ok (equal
                (objc:objective-c-object-protocol-name
                 (metal-render-pipeline-depth-stencil-state pipeline))
                "MTLDepthStencilState"))
           (ok (= 1 (length
                     (metal-render-pipeline-vertex-buffers pipeline)))))
      (when pipeline (destroy pipeline))
      (when fragment-module (destroy fragment-module))
      (when vertex-module (destroy vertex-module))
      (destroy device))))

(deftest failed-metal-library-keeps-the-device-compiler-usable
  (let* ((device
           (request-gpu-device (make-instance 'metal-gpu-provider)))
         (specification
           (luv.spir-v:block-world-fragment-specification))
         (bad-document (luv.msl:compile-msl specification))
         (module nil)
         (failure nil))
    (setf (luv.msl:msl-document-source bad-document)
          "this is deliberately not Metal source")
    (unwind-protect
         (progn
           (handler-case
               (create
                device
                (make-shader-module-descriptor
                 :language :msl :code bad-document))
             (luv::metal-gpu-error (condition)
               (setf failure condition)))
           (ok failure)
           (ok (eq (luv::metal-gpu-error-reason failure)
                   :library-compilation-failed))
           (ok (stringp
                (getf (luv::metal-gpu-error-details failure) :diagnostic)))
           (setf module
                 (create
                  device
                  (make-shader-module-descriptor
                   :language :mathematical :code specification)))
           (ok (typep module 'metal-gpu-shader-module)))
      (when module (destroy module))
      (destroy device))))

(deftest live-metal-pipeline-retains-last-good-and-recovers
  (install-metal-live-probe-vertex)
  (install-metal-live-probe-fragment 0.25)
  (let ((device
          (request-gpu-device (make-instance 'metal-gpu-provider)))
        (artifact nil))
    (unwind-protect
         (progn
           (setf artifact
                 (luv::make-live-shader-pipeline
                  :role :metal-live-probe
                  :vertex-role :metal-live-probe
                  :label "live Metal pipeline probe"
                  :device device :layout nil
                  :vertex-buffers
                  '((:array-stride 12
                     :attributes
                     ((:shader-location 0 :offset 0 :format :float32x3))))
                  :target-format :bgra8-unorm
                  :primitive '(:topology :triangle-list)
                  :depth-stencil nil))
           (let ((first-pipeline
                   (luv::live-shader-pipeline-native-pipeline artifact)))
             (ok (typep first-pipeline 'metal-gpu-render-pipeline))
             (ok (eq :installed (live-shader-pipeline-status artifact)))
             (install-metal-live-probe-fragment 0.5 :invalid-p t)
             (luv::refresh-live-shader-pipeline artifact)
             (ok (eq :failed (live-shader-pipeline-status artifact)))
             (ok (eq first-pipeline
                     (luv::live-shader-pipeline-native-pipeline artifact)))
             (ok (not (luv::metal-object-destroyed-p first-pipeline)))
             (install-metal-live-probe-fragment 0.75)
             (luv::refresh-live-shader-pipeline artifact)
             (let ((replacement
                     (luv::live-shader-pipeline-native-pipeline artifact)))
               (ok (eq :installed (live-shader-pipeline-status artifact)))
               (ok (= 1 (live-shader-pipeline-installed-revision artifact)))
               (ok (not (eq first-pipeline replacement)))
               (ok (luv::metal-object-destroyed-p first-pipeline))
               (ok (typep replacement 'metal-gpu-render-pipeline)))))
      (install-metal-live-probe-fragment 0.25)
      (when artifact
        (luv::release-live-shader-pipeline artifact))
      (destroy device))))

(deftest metal-buffer-populates-a-native-metal-4-argument-table
  (let ((device
          (request-gpu-device (make-instance 'metal-gpu-provider)))
        (buffer nil)
        (argument-table nil))
    (unwind-protect
         (progn
           (setf buffer
                 (create
                  device
                  (make-buffer-descriptor
                   :label "argument table probe"
                   :size 36 :usage '(:vertex :copy-dst))))
           (write-buffer
            buffer
            (make-array
             9 :element-type 'single-float
             :initial-contents
             '(-0.65 -0.55 0.0
                0.65 -0.55 0.0
                0.0 0.70 0.0)))
           (multiple-value-bind (table diagnostic)
               (metal:new-metal-4-argument-table
                (luv::metal-native-object device) 1
                :label "argument table probe" :attribute-strides-p t)
             (ok (null diagnostic))
             (setf argument-table table))
           (metal:set-metal-argument-table-buffer
            argument-table
            (metal:metal-buffer-gpu-address
             (luv::metal-native-object buffer))
            12 0)
           (ok (typep buffer 'metal-gpu-buffer))
           (ok (plusp
                (metal:metal-buffer-gpu-address
                 (luv::metal-native-object buffer))))
           (ok (equal
                (objc:objective-c-object-protocol-name argument-table)
                "MTL4ArgumentTable"))
           (ok (= 36 (length (read-buffer buffer)))))
      (when argument-table
        (objc:release-objective-c-object argument-table))
      (when buffer (destroy buffer))
      (destroy device))))

(deftest metal-queue-reclaims-command-memory-at-its-shared-event-frontier
  (let* ((device
           (request-gpu-device (make-instance 'metal-gpu-provider)))
         (queue (device-queue device))
         (allocator nil)
         (command-buffer nil)
         (submitted-p nil))
    (unwind-protect
         (progn
           (setf allocator
                 (metal:new-command-allocator
                  (luv::metal-native-object device))
                 command-buffer
                 (metal:new-command-buffer
                  (luv::metal-native-object device)))
           (metal:begin-command-buffer command-buffer allocator)
           (metal:end-command-buffer command-buffer)
           ;; SUBMIT-METAL-COMMAND-BUFFER consumes both owned objects.
           (setf submitted-p t)
           (ok (= 1
                  (luv::submit-metal-command-buffer
                   queue command-buffer allocator)))
           (ok (= 1 (length (luv::metal-queue-pending-submissions queue))))
           (ok (not (objc:objective-c-object-released-p command-buffer)))
           (ok (not (objc:objective-c-object-released-p allocator)))
           (submitted-work-done queue)
           (ok (null (luv::metal-queue-pending-submissions queue)))
           (ok (objc:objective-c-object-released-p command-buffer))
           (ok (objc:objective-c-object-released-p allocator)))
      (unless submitted-p
        (when command-buffer
          (objc:release-objective-c-object command-buffer))
        (when allocator
          (objc:release-objective-c-object allocator)))
      (destroy device))))

(deftest metal-submission-signals-its-frontier-when-presentation-raises
  (let* ((device
           (request-gpu-device (make-instance 'metal-gpu-provider)))
         (queue (device-queue device))
         (allocator nil)
         (command-buffer nil)
         (submitted-p nil))
    (unwind-protect
         (progn
           (setf allocator
                 (metal:new-command-allocator
                  (luv::metal-native-object device))
                 command-buffer
                 (metal:new-command-buffer
                  (luv::metal-native-object device)))
           (metal:begin-command-buffer command-buffer allocator)
           (metal:end-command-buffer command-buffer)
           (setf submitted-p t)
           (ok (signals
                (luv::submit-metal-command-buffer
                 queue command-buffer allocator
                 :after-commit (lambda () (error "presentation probe")))
                'simple-error))
           (ok (= 1 (length (luv::metal-queue-pending-submissions queue))))
           (submitted-work-done queue)
           (ok (null (luv::metal-queue-pending-submissions queue)))
           (ok (objc:objective-c-object-released-p command-buffer))
           (ok (objc:objective-c-object-released-p allocator)))
      (unless submitted-p
        (when command-buffer
          (objc:release-objective-c-object command-buffer))
        (when allocator
          (objc:release-objective-c-object allocator)))
      (destroy device))))
