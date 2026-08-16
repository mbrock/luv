(in-package #:luvcraft.tests)

(deftest slug-formats-have-exact-portable-and-metal-storage
  (ok (= 4 (texture-format-bytes-per-texel :rg16-uint)))
  (ok (= 8 (texture-format-bytes-per-texel :rgba16-float)))
  (ok (= metal:+pixel-format-rg16-uint+
         (luv::metal-resource-pixel-format
          :rg16-uint
          (make-texture-descriptor :format :rg16-uint))))
  (ok (= metal:+pixel-format-rgba16-float+
         (luv::metal-resource-pixel-format
          :rgba16-float
          (make-texture-descriptor :format :rgba16-float)))))

(deftest metal-slug-textures-accept-their-exact-packed-words
  (let* ((device
           (request-gpu-device (make-instance 'metal-gpu-provider)))
         (queue (device-queue device))
         (band-texture nil)
         (curve-texture nil))
    (unwind-protect
         (progn
           (setf band-texture
                 (create
                  device
                  (make-texture-descriptor
                   :size '(2 1) :dimensions :2d :format :rg16-uint
                   :usage '(:texture-binding :copy-dst)))
                 curve-texture
                 (create
                  device
                  (make-texture-descriptor
                   :size '(2 1) :dimensions :2d :format :rgba16-float
                   :usage '(:texture-binding :copy-dst))))
           (write-texture
            queue (make-texture-copy :texture band-texture)
            (make-array '(1 2) :element-type '(unsigned-byte 32)
                               :initial-contents '((#x00020001 #x00040003)))
            (make-texture-data-layout :bytes-per-row 8 :rows-per-image 1)
            '(2 1))
           (write-texture
            queue (make-texture-copy :texture curve-texture)
            (make-array
             '(1 2) :element-type '(unsigned-byte 64)
                    :initial-contents
                    '((#x3c00380034003000 #x40003c0038003400)))
            (make-texture-data-layout :bytes-per-row 16 :rows-per-image 1)
            '(2 1))
           (ok (typep band-texture 'luv::metal-gpu-texture))
           (ok (typep curve-texture 'luv::metal-gpu-texture)))
      (when curve-texture (destroy curve-texture))
      (when band-texture (destroy band-texture))
      (destroy device))))

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
  (ok (eq :vulkan
          (luv::sdl-presentation-api-for
           (make-instance 'vulkan-gpu-provider))))
  (ok (eq :metal
          (luv::sdl-presentation-api-for
           (make-instance 'metal-gpu-provider))))
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
          (luvcraft.shaders:block-world-fragment-specification))
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
                   :code (luvcraft.shaders:block-world-vertex-specification)))
                 fragment-module
                 (create
                  device
                  (make-shader-module-descriptor
                   :label "block fragment Metal library"
                   :language :mathematical
                   :code (luvcraft.shaders:block-world-fragment-specification)))
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
                     :targets
                     ((:format :bgra8-unorm
                       :blend :premultiplied-alpha)))
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
           (luvcraft.shaders:block-world-fragment-specification))
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
                 (luvcraft::make-live-shader-pipeline
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
                   (luvcraft::live-shader-pipeline-native-pipeline artifact)))
             (ok (typep first-pipeline 'metal-gpu-render-pipeline))
             (ok (eq :installed (live-shader-pipeline-status artifact)))
             (install-metal-live-probe-fragment 0.5 :invalid-p t)
             (luvcraft::refresh-live-shader-pipeline artifact)
             (ok (eq :failed (live-shader-pipeline-status artifact)))
             (ok (eq first-pipeline
                     (luvcraft::live-shader-pipeline-native-pipeline artifact)))
             (ok (not (luv::metal-object-destroyed-p first-pipeline)))
             (install-metal-live-probe-fragment 0.75)
             (luvcraft::refresh-live-shader-pipeline artifact)
             (let ((replacement
                     (luvcraft::live-shader-pipeline-native-pipeline artifact)))
               (ok (eq :installed (live-shader-pipeline-status artifact)))
               (ok (= 1 (live-shader-pipeline-installed-revision artifact)))
               (ok (not (eq first-pipeline replacement)))
               (ok (luv::metal-object-destroyed-p first-pipeline))
               (ok (typep replacement 'metal-gpu-render-pipeline)))))
      (install-metal-live-probe-fragment 0.25)
      (when artifact
        (luvcraft::release-live-shader-pipeline artifact))
      (destroy device))))

(deftest live-metal-pipeline-replacement-retires-after-its-in-flight-draw
  (install-metal-live-probe-vertex)
  (install-metal-live-probe-fragment 0.25)
  (let* ((device
           (request-gpu-device (make-instance 'metal-gpu-provider)))
         (queue (device-queue device))
         (artifact nil)
         (texture nil)
         (vertices nil)
         (encoder nil)
         (commands nil)
         (old-native-pipeline nil))
    (unwind-protect
         (progn
           (setf artifact
                 (luvcraft::make-live-shader-pipeline
                  :role :metal-live-probe
                  :vertex-role :metal-live-probe
                  :label "in-flight Metal pipeline probe"
                  :device device :layout nil
                  :vertex-buffers
                  '((:array-stride 12
                     :attributes
                     ((:shader-location 0 :offset 0 :format :float32x3))))
                  :target-format :bgra8-unorm
                  :primitive '(:topology :triangle-list)
                  :depth-stencil nil)
                 texture
                 (create
                  device
                  (make-texture-descriptor
                   :label "in-flight pipeline target"
                   :size '(16 16) :dimensions :2d :format :bgra8-unorm
                   :usage '(:render-attachment)))
                 vertices
                 (create
                  device
                  (make-buffer-descriptor
                   :label "in-flight pipeline vertices"
                   :size 36 :usage '(:vertex :copy-dst))))
           (write-buffer
            vertices
            (make-array
             9 :element-type 'single-float
             :initial-contents
             '(-0.7 -0.6 0.0 0.7 -0.6 0.0 0.0 0.7 0.0)))
           (let ((old-pipeline
                   (luvcraft::live-shader-pipeline-native-pipeline artifact)))
             (setf old-native-pipeline (luv::metal-native-object old-pipeline)
                   encoder (create device (make-command-encoder-descriptor)))
             (let ((pass
                     (begin-render-pass
                      encoder
                      (make-render-pass-descriptor
                       :color-attachments
                       (list (list :view texture :load-op :clear
                                   :store-op :store
                                   :clear-value #(0.0 0.0 0.0 1.0)))))))
               (set-pipeline pass old-pipeline)
               (set-vertex-buffer pass 0 vertices)
               (draw pass 3)
               (end-pass pass))
             (setf commands (finish encoder))
             (submit queue commands)
             (install-metal-live-probe-fragment 0.75)
             (luvcraft::refresh-live-shader-pipeline artifact)
             (ok (luv::metal-object-destroyed-p old-pipeline))
             (ok (not (eq old-pipeline
                          (luvcraft::live-shader-pipeline-native-pipeline artifact))))
             (submitted-work-done queue)
             (ok (objc:objective-c-object-released-p old-native-pipeline))))
      (install-metal-live-probe-fragment 0.25)
      (when commands (destroy commands))
      (when encoder (destroy encoder))
      (when vertices (destroy vertices))
      (when texture (destroy texture))
      (when artifact (luvcraft::release-live-shader-pipeline artifact))
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

(deftest metal-finish-produces-one-shot-portable-work-with-dependencies
  (let* ((device
           (request-gpu-device (make-instance 'metal-gpu-provider)))
         (queue (device-queue device))
         (texture nil)
         (encoder nil)
         (command-buffer nil)
         (native-texture nil)
         (native-command-buffer nil)
         (allocator nil))
    (unwind-protect
         (progn
           (setf texture
                 (create
                  device
                  (make-texture-descriptor
                   :label "portable Metal work dependency"
                   :size '(8 8) :dimensions :2d :format :rgba8-unorm
                   :usage '(:render-attachment)))
                 native-texture (luv::metal-native-object texture)
                 encoder
                 (create device (make-command-encoder-descriptor
                                 :label "portable Metal work")))
           (encode encoder
                   (make-gpu-clear-texture-command
                    :texture texture :color #(0.25 0.5 0.75 1.0)))
           (setf command-buffer (finish encoder)
                 native-command-buffer
                 (luv::metal-native-object command-buffer)
                 allocator
                 (luv::metal-command-buffer-allocator command-buffer))
           (ok (typep encoder 'metal-gpu-command-encoder))
           (ok (typep command-buffer 'metal-gpu-command-buffer))
           (ok (member texture
                       (luv::metal-command-buffer-resources command-buffer)))
           (ok (= 1 (submit queue command-buffer)))
           (ok (eq :submitted
                   (luv::metal-command-buffer-state command-buffer)))
           (ok (= 1 (luv::metal-object-last-submission texture)))
           (ok (signals (submit queue command-buffer)
                        'gpu-invalid-state-error))
           (ok (= 1 (length (luv::metal-queue-pending-submissions queue))))
           ;; Logical invalidation is immediate. Native retirement follows the
           ;; shared-event completion frontier without blocking DESTROY.
           (destroy command-buffer)
           (setf command-buffer nil)
           (destroy texture)
           (setf texture nil)
           (submitted-work-done queue)
           (ok (null (luv::metal-queue-pending-submissions queue)))
           (ok (objc:objective-c-object-released-p native-command-buffer))
           (ok (objc:objective-c-object-released-p allocator))
           (ok (objc:objective-c-object-released-p native-texture)))
      (when command-buffer (destroy command-buffer))
      (when encoder (destroy encoder))
      (when texture (destroy texture))
      (destroy device))))

(deftest metal-submission-signals-its-frontier-when-presentation-raises
  (let* ((device
           (request-gpu-device (make-instance 'metal-gpu-provider)))
         (queue (device-queue device))
         (encoder nil)
         (command-buffer nil)
         (native-command-buffer nil)
         (allocator nil))
    (unwind-protect
         (progn
           (setf encoder
                 (create device (make-command-encoder-descriptor))
                 command-buffer (finish encoder)
                 native-command-buffer
                 (luv::metal-native-object command-buffer)
                 allocator
                 (luv::metal-command-buffer-allocator command-buffer))
           (ok (signals
                (luv::submit-metal-command-buffers
                 queue (vector command-buffer)
                 :after-commit (lambda () (error "presentation probe")))
                'simple-error))
           (ok (= 1 (length (luv::metal-queue-pending-submissions queue))))
           (destroy command-buffer)
           (setf command-buffer nil)
           (submitted-work-done queue)
           (ok (null (luv::metal-queue-pending-submissions queue)))
           (ok (objc:objective-c-object-released-p native-command-buffer))
           (ok (objc:objective-c-object-released-p allocator)))
      (when command-buffer (destroy command-buffer))
      (when encoder (destroy encoder))
      (destroy device))))

(deftest luvcraft-metal-frame-resources-follow-the-drawable-pool
  (call-with-sdl-main-thread
   (lambda ()
     (let ((session nil))
       (unwind-protect
            (progn
              (setf session
                    (start-luvcraft
                     :provider (make-instance 'metal-gpu-provider)
                     :world (luvcraft::make-gazetteer-shadow-yard-world)
                     :residency-radius 0
                     :visible-p nil :frames-per-second nil
                     :width 160 :height 100))
              (wait-for-luvcraft-products session :minimum 1)
              (let ((resources-before
                      (length (luvcraft::luvcraft-session-resources session))))
                (dotimes (index 8)
                  (luvcraft::render-luvcraft-frame
                   session (* index (/ 1d0 60d0))))
                (submitted-work-done
                 (device-queue (luvcraft::luvcraft-session-device session)))
                (let ((state-count
                        (hash-table-count
                         (luvcraft::luvcraft-session-frame-states session))))
                  (ok (<= 1 state-count 3))
                  (ok (= (length (luvcraft::luvcraft-session-resources session))
                         (+ resources-before (* 3 state-count)))))))
         (when session
           (stop-luvcraft session)))))))
