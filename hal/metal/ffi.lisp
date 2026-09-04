;;;; The Metal 4 and CAMetalLayer vocabulary needed by the first frame.
;;;;
;;;; The two small structures cross as typed foreign storage through either
;;;; the default NSInvocation bridge or direct libffi calls in an explicitly
;;;; unchecked scope.  They stay declarations in the same message system as
;;;; scalar and object calls; no per-selector shim is needed.

(in-package #:luv.metal)

(cffi:defcstruct cg-size
  (width :double)
  (height :double))

(cffi:defcstruct mtl-clear-color
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(cffi:defcstruct mtl-resource-id
  (value :uint64))

(cffi:defcstruct mtl-origin
  (x :uint64)
  (y :uint64)
  (z :uint64))

(cffi:defcstruct mtl-size
  (width :uint64)
  (height :uint64)
  (depth :uint64))

(cffi:defcstruct mtl-scissor-rect
  (x :uint64)
  (y :uint64)
  (width :uint64)
  (height :uint64))

;; MTLStages and MTL4VisibilityOptions used by explicit Metal 4 barriers.
(defconstant +stage-fragment+ (ash 1 1))
(defconstant +stage-blit+ (ash 1 28))
(defconstant +visibility-device+ (ash 1 0))

(cffi:defcstruct mtl-region
  ;; MTLRegion is two adjacent three-NSUInteger structures.  Flat fields keep
  ;; the identical ABI while remaining directly assignable through CFFI.
  (x :uint64)
  (y :uint64)
  (z :uint64)
  (width :uint64)
  (height :uint64)
  (depth :uint64))

(defconstant +pixel-format-bgra8-unorm+ 80)
(defconstant +pixel-format-bgra8-unorm-srgb+ 81)
(defconstant +pixel-format-rgba8-unorm+ 70)
(defconstant +pixel-format-rgba8-unorm-srgb+ 71)
(defconstant +pixel-format-r16-float+ 25)
(defconstant +pixel-format-r8-unorm+ 10)
(defconstant +pixel-format-rg8-unorm+ 30)
(defconstant +pixel-format-rg16-uint+ 63)
(defconstant +pixel-format-rg16-float+ 65)
(defconstant +pixel-format-rgba16-float+ 115)
(defconstant +pixel-format-depth32-float+ 252)
(defconstant +texture-type-2d+ 2)
(defconstant +texture-type-2d-multisample+ 4)
(defconstant +texture-usage-shader-read+ (ash 1 0))
(defconstant +texture-usage-shader-write+ (ash 1 1))
(defconstant +texture-usage-render-target+ (ash 1 2))
(defconstant +storage-mode-shared+ 0)
(defconstant +storage-mode-private+ 2)
(defconstant +load-action-load+ 1)
(defconstant +load-action-clear+ 2)
(defconstant +store-action-dont-care+ 0)
(defconstant +store-action-store+ 1)
(defconstant +store-action-multisample-resolve+ 2)
(defconstant +multisample-depth-resolve-filter-sample-zero+ 0)
(defconstant +language-version-4-0+ (ash 4 16))
(defconstant +function-type-vertex+ 1)
(defconstant +function-type-fragment+ 2)
(defconstant +function-type-mesh+ 7)
(defconstant +function-type-object+ 8)
(defconstant +vertex-format-float2+ 29)
(defconstant +vertex-format-float3+ 30)
(defconstant +vertex-format-float4+ 31)
(defconstant +vertex-step-function-per-vertex+ 1)
(defconstant +vertex-step-function-per-instance+ 2)
(defconstant +primitive-topology-class-triangle+ 3)
(defconstant +primitive-type-triangle+ 3)
(defconstant +primitive-type-triangle-strip+ 4)
(defconstant +index-type-uint16+ 0)
(defconstant +index-type-uint32+ 1)
(defconstant +blend-state-enabled+ 1)
(defconstant +blend-factor-one+ 1)
(defconstant +blend-factor-one-minus-source-alpha+ 5)
(defconstant +blend-operation-add+ 0)
(defconstant +render-stage-vertex+ (ash 1 0))
(defconstant +render-stage-fragment+ (ash 1 1))
(defconstant +render-stage-object+ (ash 1 3))
(defconstant +render-stage-mesh+ (ash 1 4))
(defconstant +compare-function-never+ 0)
(defconstant +compare-function-less+ 1)
(defconstant +compare-function-equal+ 2)
(defconstant +compare-function-less-equal+ 3)
(defconstant +compare-function-greater+ 4)
(defconstant +compare-function-not-equal+ 5)
(defconstant +compare-function-greater-equal+ 6)
(defconstant +compare-function-always+ 7)
(defconstant +sampler-min-mag-filter-nearest+ 0)
(defconstant +sampler-min-mag-filter-linear+ 1)
(defconstant +sampler-mip-filter-not-mipmapped+ 0)
(defconstant +sampler-mip-filter-nearest+ 1)
(defconstant +sampler-mip-filter-linear+ 2)
(defconstant +sampler-address-mode-clamp-to-edge+ 0)
(defconstant +sampler-address-mode-repeat+ 2)

;;; Device and Metal 4 submission.

(objc:define-objective-c-message metal-device-supports-texture-sample-count-p
    ("supportsTextureSampleCount:" :uint8)
  (count :uint64))

(objc:define-objective-c-message new-metal-4-command-queue
    ("newMTL4CommandQueue" :object :ownership :owned
     :class "MTL4CommandQueue"))

(objc:define-objective-c-message new-metal-shared-event
    ("newSharedEvent" :object :ownership :owned :class "MTLSharedEvent"))

(objc:define-objective-c-message wait-for-metal-shared-event
    ("waitUntilSignaledValue:timeoutMS:" :uint8)
  (value :uint64)
  (timeout-milliseconds :uint64))

(objc:define-objective-c-message metal-shared-event-signaled-value
    ("signaledValue" :uint64))

(objc:define-objective-c-message signal-metal-event
    ("signalEvent:value:" :void)
  (event :object)
  (value :uint64))

;;; Device resources and Metal 4 binding infrastructure.

(objc:define-objective-c-message new-metal-buffer
    ("newBufferWithLength:options:" :object :ownership :owned
     :class "MTLBuffer")
  (length :uint64)
  (options :uint64))

(objc:define-objective-c-message metal-buffer-contents
    ("contents" :pointer))

(objc:define-objective-c-message metal-buffer-gpu-address
    ("gpuAddress" :uint64))

(objc:define-objective-c-message %new-metal-texture-descriptor
    ("new" :object :ownership :owned :class "MTLTextureDescriptor"))

(objc:define-objective-c-message %set-metal-texture-type
    ("setTextureType:" :void)
  (type :uint64))

(objc:define-objective-c-message %set-metal-texture-pixel-format
    ("setPixelFormat:" :void)
  (format :uint64))

(objc:define-objective-c-message %set-metal-texture-width
    ("setWidth:" :void)
  (width :uint64))

(objc:define-objective-c-message %set-metal-texture-height
    ("setHeight:" :void)
  (height :uint64))

(objc:define-objective-c-message %set-metal-texture-sample-count
    ("setSampleCount:" :void)
  (count :uint64))

(objc:define-objective-c-message %set-metal-texture-storage-mode
    ("setStorageMode:" :void)
  (mode :uint64))

(objc:define-objective-c-message %set-metal-texture-usage
    ("setUsage:" :void)
  (usage :uint64))

(objc:define-objective-c-message %new-metal-texture
    ("newTextureWithDescriptor:" :object :ownership :owned :class "MTLTexture")
  (descriptor :object))

(objc:define-objective-c-message metal-texture-resource-id
    ("gpuResourceID" (:struct mtl-resource-id)))

(objc:define-objective-c-message %replace-metal-texture-region
    ("replaceRegion:mipmapLevel:withBytes:bytesPerRow:" :void)
  (region (:struct mtl-region))
  (mipmap-level :uint64)
  (bytes :pointer)
  (bytes-per-row :uint64))

(defun replace-metal-texture-region
    (texture width height bytes bytes-per-row)
  "Replace the complete base level of a two-dimensional Metal texture."
  (%replace-metal-texture-region
   texture
   (list 'x 0 'y 0 'z 0 'width width 'height height 'depth 1)
   0 bytes bytes-per-row))

(objc:define-objective-c-message %new-metal-sampler-descriptor
    ("new" :object :ownership :owned :class "MTLSamplerDescriptor"))

(objc:define-objective-c-message %set-metal-sampler-min-filter
    ("setMinFilter:" :void)
  (filter :uint64))

(objc:define-objective-c-message %set-metal-sampler-mag-filter
    ("setMagFilter:" :void)
  (filter :uint64))

(objc:define-objective-c-message %set-metal-sampler-mip-filter
    ("setMipFilter:" :void)
  (filter :uint64))

(objc:define-objective-c-message %set-metal-sampler-address-mode-s
    ("setSAddressMode:" :void)
  (mode :uint64))

(objc:define-objective-c-message %set-metal-sampler-address-mode-t
    ("setTAddressMode:" :void)
  (mode :uint64))

(objc:define-objective-c-message %set-metal-sampler-address-mode-r
    ("setRAddressMode:" :void)
  (mode :uint64))

(objc:define-objective-c-message %set-metal-sampler-compare-function
    ("setCompareFunction:" :void)
  (function :uint64))

(objc:define-objective-c-message %new-metal-sampler
    ("newSamplerStateWithDescriptor:" :object :ownership :owned
     :class "MTLSamplerState")
  (descriptor :object))

(objc:define-objective-c-message metal-sampler-resource-id
    ("gpuResourceID" (:struct mtl-resource-id)))

(defun new-metal-texture
    (device width height pixel-format usage &key (storage-mode +storage-mode-private+)
                                                (sample-count 1) label)
  "Create one owned two-dimensional Metal texture."
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (descriptor
          (%new-metal-texture-descriptor
           (objc:find-objective-c-class "MTLTextureDescriptor")))
      (%set-metal-texture-type
       descriptor (if (= sample-count 1)
                      +texture-type-2d+
                      +texture-type-2d-multisample+))
      (%set-metal-texture-pixel-format descriptor pixel-format)
      (%set-metal-texture-width descriptor width)
      (%set-metal-texture-height descriptor height)
      (%set-metal-texture-sample-count descriptor sample-count)
      (%set-metal-texture-storage-mode descriptor storage-mode)
      (%set-metal-texture-usage descriptor usage)
      (let ((texture (%new-metal-texture device descriptor)))
        (when (and texture label)
          (%set-object-label texture (objc:lisp-string-to-objective-c label)))
        texture))))

(defun new-metal-sampler
    (device min-filter mag-filter mip-filter address-mode-s address-mode-t
     address-mode-r compare-function &key label)
  "Create one owned Metal sampler state."
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (descriptor
          (%new-metal-sampler-descriptor
           (objc:find-objective-c-class "MTLSamplerDescriptor")))
      (%set-metal-sampler-min-filter descriptor min-filter)
      (%set-metal-sampler-mag-filter descriptor mag-filter)
      (%set-metal-sampler-mip-filter descriptor mip-filter)
      (%set-metal-sampler-address-mode-s descriptor address-mode-s)
      (%set-metal-sampler-address-mode-t descriptor address-mode-t)
      (%set-metal-sampler-address-mode-r descriptor address-mode-r)
      (%set-metal-sampler-compare-function descriptor compare-function)
      (when label
        (%set-object-label descriptor (objc:lisp-string-to-objective-c label)))
      (%new-metal-sampler device descriptor))))

(objc:define-objective-c-message %new-metal-residency-set-descriptor
    ("new" :object :ownership :owned :class "MTLResidencySetDescriptor"))

(objc:define-objective-c-message %set-residency-set-initial-capacity
    ("setInitialCapacity:" :void)
  (capacity :uint64))

(objc:define-objective-c-message %new-metal-residency-set
    ("newResidencySetWithDescriptor:error:" :object :ownership :owned
     :class "MTLResidencySet")
  (descriptor :object)
  (error :pointer))

(objc:define-objective-c-message add-metal-residency-allocation
    ("addAllocation:" :void)
  (allocation :object))

(objc:define-objective-c-message remove-metal-residency-allocation
    ("removeAllocation:" :void)
  (allocation :object))

(objc:define-objective-c-message commit-metal-residency-set
    ("commit" :void))

(objc:define-objective-c-message add-metal-queue-residency-set
    ("addResidencySet:" :void)
  (residency-set :object))

(objc:define-objective-c-message remove-metal-queue-residency-set
    ("removeResidencySet:" :void)
  (residency-set :object))

(objc:define-objective-c-message %new-metal-4-argument-table-descriptor
    ("new" :object :ownership :owned :class "MTL4ArgumentTableDescriptor"))

(objc:define-objective-c-message %set-argument-table-max-buffer-count
    ("setMaxBufferBindCount:" :void)
  (count :uint64))

(objc:define-objective-c-message %set-argument-table-max-texture-count
    ("setMaxTextureBindCount:" :void)
  (count :uint64))

(objc:define-objective-c-message %set-argument-table-max-sampler-count
    ("setMaxSamplerStateBindCount:" :void)
  (count :uint64))

(objc:define-objective-c-message %set-argument-table-initialize-bindings
    ("setInitializeBindings:" :void)
  (enabled :uint8))

(objc:define-objective-c-message %set-argument-table-support-attribute-strides
    ("setSupportAttributeStrides:" :void)
  (enabled :uint8))

(objc:define-objective-c-message %new-metal-4-argument-table
    ("newArgumentTableWithDescriptor:error:" :object :ownership :owned
     :class "MTL4ArgumentTable")
  (descriptor :object)
  (error :pointer))

(objc:define-objective-c-message set-metal-argument-table-buffer
    ("setAddress:attributeStride:atIndex:" :void)
  (address :uint64)
  (attribute-stride :uint64)
  (index :uint64))

(objc:define-objective-c-message set-metal-argument-table-address
    ("setAddress:atIndex:" :void)
  (address :uint64)
  (index :uint64))

(objc:define-objective-c-message set-metal-argument-table-texture
    ("setTexture:atIndex:" :void)
  (resource-id (:struct mtl-resource-id))
  (index :uint64))

(objc:define-objective-c-message set-metal-argument-table-sampler
    ("setSamplerState:atIndex:" :void)
  (resource-id (:struct mtl-resource-id))
  (index :uint64))

(objc:define-objective-c-message %new-metal-4-compiler-descriptor
    ("new" :object :ownership :owned :class "MTL4CompilerDescriptor"))

(objc:define-objective-c-message %new-metal-4-library-descriptor
    ("new" :object :ownership :owned :class "MTL4LibraryDescriptor"))

(objc:define-objective-c-message %new-metal-compile-options
    ("new" :object :ownership :owned :class "MTLCompileOptions"))

(objc:define-objective-c-message %set-object-label
    ("setLabel:" :void)
  (label :object))

(objc:define-objective-c-message %set-library-source
    ("setSource:" :void)
  (source :object))

(objc:define-objective-c-message %set-library-name
    ("setName:" :void)
  (name :object))

(objc:define-objective-c-message %set-library-options
    ("setOptions:" :void)
  (options :object))

(objc:define-objective-c-message %set-language-version
    ("setLanguageVersion:" :void)
  (version :uint64))

(objc:define-objective-c-message %new-metal-4-compiler
    ("newCompilerWithDescriptor:error:" :object :ownership :owned
     :class "MTL4Compiler")
  (descriptor :object)
  (error :pointer))

(objc:define-objective-c-message %new-metal-4-library
    ("newLibraryWithDescriptor:error:" :object :ownership :owned
     :class "MTLLibrary")
  (descriptor :object)
  (error :pointer))

(objc:define-objective-c-message new-metal-library-function
    ("newFunctionWithName:" :object :ownership :owned :class "MTLFunction")
  (name :object))

(objc:define-objective-c-message metal-function-type
    ("functionType" :uint64))

(defun objective-c-error-pointer-description (storage)
  (let ((pointer (cffi:mem-ref storage :pointer)))
    (unless (cffi:null-pointer-p pointer)
      (objc:objective-c-error-description
       (objc:wrap-objective-c-object pointer :ownership :borrowed
                                    :protocol-name "NSError")))))

(defun new-metal-residency-set (device &key label (initial-capacity 64))
  "Create an owned residency set for long-lived Metal allocations."
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (descriptor
          (%new-metal-residency-set-descriptor
           (objc:find-objective-c-class "MTLResidencySetDescriptor")))
      (%set-residency-set-initial-capacity descriptor initial-capacity)
      (when label
        (%set-object-label descriptor (objc:lisp-string-to-objective-c label)))
      (cffi:with-foreign-object (error :pointer)
        (setf (cffi:mem-ref error :pointer) (cffi:null-pointer))
        (let ((residency-set (%new-metal-residency-set device descriptor error)))
          (values residency-set
                  (objective-c-error-pointer-description error)))))))

(defun new-metal-4-argument-table
    (device max-buffer-count
     &key (max-texture-count 0) (max-sampler-count 0) label
       (attribute-strides-p nil))
  "Create an owned Metal 4 argument table with a buffer binding range."
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (descriptor
          (%new-metal-4-argument-table-descriptor
           (objc:find-objective-c-class "MTL4ArgumentTableDescriptor")))
      (%set-argument-table-max-buffer-count descriptor max-buffer-count)
      (%set-argument-table-max-texture-count descriptor max-texture-count)
      (%set-argument-table-max-sampler-count descriptor max-sampler-count)
      (%set-argument-table-initialize-bindings descriptor 1)
      (%set-argument-table-support-attribute-strides
       descriptor (if attribute-strides-p 1 0))
      (when label
        (%set-object-label descriptor (objc:lisp-string-to-objective-c label)))
      (cffi:with-foreign-object (error :pointer)
        (setf (cffi:mem-ref error :pointer) (cffi:null-pointer))
        (let ((table (%new-metal-4-argument-table device descriptor error)))
          (values table (objective-c-error-pointer-description error)))))))

(defun new-metal-4-compiler (device &key label)
  "Create one synchronous Metal 4 compiler owned by DEVICE.

Return the owned compiler and NIL on success, or NIL and a copied NSError
description on native rejection."
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (descriptor
          (%new-metal-4-compiler-descriptor
           (objc:find-objective-c-class "MTL4CompilerDescriptor")))
      (when label
        (%set-object-label descriptor (objc:lisp-string-to-objective-c label)))
      (cffi:with-foreign-object (error :pointer)
        (setf (cffi:mem-ref error :pointer) (cffi:null-pointer))
        (let ((compiler (%new-metal-4-compiler device descriptor error)))
          (values compiler (objective-c-error-pointer-description error)))))))

;;; MetalFX temporal reconstruction on a Metal 4 command buffer.

(objc:define-objective-c-message %new-temporal-scaler-descriptor
    ("new" :object :ownership :owned
     :class "MTLFXTemporalScalerDescriptor"))

(objc:define-objective-c-message %temporal-scaler-supports-metal-4-fx
    ("supportsMetal4FX:" :uint8)
  (device :object))

(objc:define-objective-c-message %set-temporal-color-format
    ("setColorTextureFormat:" :void)
  (format :uint64))

(objc:define-objective-c-message %set-temporal-depth-format
    ("setDepthTextureFormat:" :void)
  (format :uint64))

(objc:define-objective-c-message %set-temporal-motion-format
    ("setMotionTextureFormat:" :void)
  (format :uint64))

(objc:define-objective-c-message %set-temporal-output-format
    ("setOutputTextureFormat:" :void)
  (format :uint64))

(objc:define-objective-c-message %set-temporal-input-width
    ("setInputWidth:" :void)
  (width :uint64))

(objc:define-objective-c-message %set-temporal-input-height
    ("setInputHeight:" :void)
  (height :uint64))

(objc:define-objective-c-message %set-temporal-output-width
    ("setOutputWidth:" :void)
  (width :uint64))

(objc:define-objective-c-message %set-temporal-output-height
    ("setOutputHeight:" :void)
  (height :uint64))

(objc:define-objective-c-message %set-temporal-auto-exposure-enabled
    ("setAutoExposureEnabled:" :void)
  (enabled :uint8))

(objc:define-objective-c-message %set-temporal-synchronous-initialization
    ("setRequiresSynchronousInitialization:" :void)
  (enabled :uint8))

(objc:define-objective-c-message %new-temporal-scaler-with-compiler
    ("newTemporalScalerWithDevice:compiler:" :object :ownership :owned
     :class "MTL4FXTemporalScaler")
  (device :object)
  (compiler :object))

(objc:define-objective-c-message new-metal-fence
    ("newFence" :object :ownership :owned :class "MTLFence"))

(objc:define-objective-c-message %set-temporal-fence
    ("setFence:" :void)
  (fence :object))

(objc:define-objective-c-message %temporal-color-texture-usage
    ("colorTextureUsage" :uint64))

(objc:define-objective-c-message %temporal-depth-texture-usage
    ("depthTextureUsage" :uint64))

(objc:define-objective-c-message %temporal-motion-texture-usage
    ("motionTextureUsage" :uint64))

(objc:define-objective-c-message %temporal-output-texture-usage
    ("outputTextureUsage" :uint64))

(objc:define-objective-c-message %set-temporal-color-texture
    ("setColorTexture:" :void)
  (texture :object))

(objc:define-objective-c-message %set-temporal-depth-texture
    ("setDepthTexture:" :void)
  (texture :object))

(objc:define-objective-c-message %set-temporal-motion-texture
    ("setMotionTexture:" :void)
  (texture :object))

(objc:define-objective-c-message %set-temporal-exposure-texture
    ("setExposureTexture:" :void)
  (texture :object))

(objc:define-objective-c-message %set-temporal-output-texture
    ("setOutputTexture:" :void)
  (texture :object))

(objc:define-objective-c-message %set-temporal-input-content-width
    ("setInputContentWidth:" :void)
  (width :uint64))

(objc:define-objective-c-message %set-temporal-input-content-height
    ("setInputContentHeight:" :void)
  (height :uint64))

(objc:define-objective-c-message %set-temporal-pre-exposure
    ("setPreExposure:" :void)
  (exposure :float))

(objc:define-objective-c-message %set-temporal-jitter-x
    ("setJitterOffsetX:" :void)
  (offset :float))

(objc:define-objective-c-message %set-temporal-jitter-y
    ("setJitterOffsetY:" :void)
  (offset :float))

(objc:define-objective-c-message %set-temporal-motion-scale-x
    ("setMotionVectorScaleX:" :void)
  (scale :float))

(objc:define-objective-c-message %set-temporal-motion-scale-y
    ("setMotionVectorScaleY:" :void)
  (scale :float))

(objc:define-objective-c-message %set-temporal-reset
    ("setReset:" :void)
  (reset :uint8))

(objc:define-objective-c-message %set-temporal-depth-reversed
    ("setDepthReversed:" :void)
  (reversed :uint8))

(objc:define-objective-c-message encode-metal-temporal-scaler
    ("encodeToCommandBuffer:" :void)
  (command-buffer :object))

(objc:define-objective-c-message update-metal-fence
    ("updateFence:afterEncoderStages:" :void)
  (fence :object)
  (stages :uint64))

(objc:define-objective-c-message wait-for-metal-fence
    ("waitForFence:beforeEncoderStages:" :void)
  (fence :object)
  (stages :uint64))

(defun new-metal-4-temporal-scaler
    (device compiler color-format depth-format motion-format output-format
     input-width input-height output-width output-height)
  "Create an owned Metal4FX temporal scaler and its owned synchronization fence."
  (objc:with-autorelease-pool ()
    (let ((class (objc:find-objective-c-class
                  "MTLFXTemporalScalerDescriptor")))
      (when (plusp (%temporal-scaler-supports-metal-4-fx class device))
        (objc:with-owned-objective-c-object
            (descriptor (%new-temporal-scaler-descriptor class))
          (%set-temporal-color-format descriptor color-format)
          (%set-temporal-depth-format descriptor depth-format)
          (%set-temporal-motion-format descriptor motion-format)
          (%set-temporal-output-format descriptor output-format)
          (%set-temporal-input-width descriptor input-width)
          (%set-temporal-input-height descriptor input-height)
          (%set-temporal-output-width descriptor output-width)
          (%set-temporal-output-height descriptor output-height)
          (%set-temporal-auto-exposure-enabled descriptor 0)
          (%set-temporal-synchronous-initialization descriptor 1)
          (let ((scaler (%new-temporal-scaler-with-compiler
                         descriptor device compiler)))
            (when scaler
              (let ((fence (new-metal-fence device)))
                (if fence
                    (progn
                      (%set-temporal-fence scaler fence)
                      (values scaler fence))
                    (progn
                      (objc:release-objective-c-object scaler)
                      (values nil nil)))))))))))

(defun metal-temporal-scaler-texture-usages (scaler)
  "Return SCALER's required color, depth, motion, and output usage masks."
  (values (%temporal-color-texture-usage scaler)
          (%temporal-depth-texture-usage scaler)
          (%temporal-motion-texture-usage scaler)
          (%temporal-output-texture-usage scaler)))

(defun configure-metal-temporal-scaler
    (scaler color depth motion exposure output width height jitter-x jitter-y
     reset-p)
  "Bind one native frame to SCALER before encoding it."
  (%set-temporal-color-texture scaler color)
  (%set-temporal-depth-texture scaler depth)
  (%set-temporal-motion-texture scaler motion)
  (%set-temporal-exposure-texture scaler exposure)
  (%set-temporal-output-texture scaler output)
  (%set-temporal-input-content-width scaler width)
  (%set-temporal-input-content-height scaler height)
  (%set-temporal-pre-exposure scaler 1.0)
  (%set-temporal-jitter-x scaler (coerce jitter-x 'single-float))
  (%set-temporal-jitter-y scaler (coerce jitter-y 'single-float))
  ;; Luft writes normalized current-to-previous motion.  MetalFX consumes
  ;; pixels after applying these once-per-frame extent scales.
  (%set-temporal-motion-scale-x scaler (coerce width 'single-float))
  (%set-temporal-motion-scale-y scaler (coerce height 'single-float))
  (%set-temporal-depth-reversed scaler 0)
  (%set-temporal-reset scaler (if reset-p 1 0))
  scaler)

(defun clear-metal-temporal-scaler (scaler)
  "Release every object property retained by SCALER."
  (%set-temporal-color-texture scaler nil)
  (%set-temporal-depth-texture scaler nil)
  (%set-temporal-motion-texture scaler nil)
  (%set-temporal-exposure-texture scaler nil)
  (%set-temporal-output-texture scaler nil)
  (%set-temporal-fence scaler nil)
  scaler)

(defun compile-metal-4-library (compiler source &key name)
  "Synchronously compile SOURCE as Metal 4 and return an owned MTLLibrary.

The second value is NIL on success or a copied NSError description on native
rejection.  Source and names cross only as in-memory NSString objects."
  (check-type source string)
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (options
          (%new-metal-compile-options
           (objc:find-objective-c-class "MTLCompileOptions")))
      (%set-language-version options +language-version-4-0+)
      (objc:with-owned-objective-c-object
          (descriptor
            (%new-metal-4-library-descriptor
             (objc:find-objective-c-class "MTL4LibraryDescriptor")))
        (%set-library-source
         descriptor (objc:lisp-string-to-objective-c source))
        (%set-library-options descriptor options)
        (when name
          (%set-library-name
           descriptor (objc:lisp-string-to-objective-c name)))
        (cffi:with-foreign-object (error :pointer)
          (setf (cffi:mem-ref error :pointer) (cffi:null-pointer))
          (let ((library (%new-metal-4-library compiler descriptor error)))
            (values library
                    (objective-c-error-pointer-description error))))))))

;;; Metal 4 render pipeline compilation.

(objc:define-objective-c-message %new-metal-4-render-pipeline-descriptor
    ("new" :object :ownership :owned
     :class "MTL4RenderPipelineDescriptor"))

(objc:define-objective-c-message %new-metal-4-mesh-render-pipeline-descriptor
    ("new" :object :ownership :owned
     :class "MTL4MeshRenderPipelineDescriptor"))

(objc:define-objective-c-message %new-metal-4-library-function-descriptor
    ("new" :object :ownership :owned
     :class "MTL4LibraryFunctionDescriptor"))

(objc:define-objective-c-message %new-metal-vertex-descriptor
    ("new" :object :ownership :owned :class "MTLVertexDescriptor"))

(objc:define-objective-c-message %set-function-library
    ("setLibrary:" :void)
  (library :object))

(objc:define-objective-c-message %set-function-name
    ("setName:" :void)
  (name :object))

(objc:define-objective-c-message %set-vertex-function-descriptor
    ("setVertexFunctionDescriptor:" :void)
  (descriptor :object))

(objc:define-objective-c-message %set-fragment-function-descriptor
    ("setFragmentFunctionDescriptor:" :void)
  (descriptor :object))

(objc:define-objective-c-message %set-object-function-descriptor
    ("setObjectFunctionDescriptor:" :void)
  (descriptor :object))

(objc:define-objective-c-message %set-mesh-function-descriptor
    ("setMeshFunctionDescriptor:" :void)
  (descriptor :object))

(objc:define-objective-c-message %set-max-object-threads
    ("setMaxTotalThreadsPerObjectThreadgroup:" :void)
  (count :uint64))

(objc:define-objective-c-message %set-max-mesh-threads
    ("setMaxTotalThreadsPerMeshThreadgroup:" :void)
  (count :uint64))

(objc:define-objective-c-message %set-max-mesh-workgroups
    ("setMaxTotalThreadgroupsPerMeshGrid:" :void)
  (count :uint64))

(objc:define-objective-c-message %set-pipeline-vertex-descriptor
    ("setVertexDescriptor:" :void)
  (descriptor :object))

(objc:define-objective-c-message %set-pipeline-raster-sample-count
    ("setRasterSampleCount:" :void)
  (count :uint64))

(objc:define-objective-c-message %set-input-primitive-topology
    ("setInputPrimitiveTopology:" :void)
  (topology :uint64))

(objc:define-objective-c-message %render-pipeline-color-attachments
    ("colorAttachments" :object :ownership :borrowed
     :class "MTL4RenderPipelineColorAttachmentDescriptorArray"))

(objc:define-objective-c-message %render-pipeline-color-attachment-at
    ("objectAtIndexedSubscript:" :object :ownership :borrowed
     :class "MTL4RenderPipelineColorAttachmentDescriptor")
  (index :uint64))

(objc:define-objective-c-message %set-render-pipeline-pixel-format
    ("setPixelFormat:" :void)
  (format :uint64))

(objc:define-objective-c-message %set-render-pipeline-blending-state
    ("setBlendingState:" :void)
  (state :uint64))

(objc:define-objective-c-message %set-source-rgb-blend-factor
    ("setSourceRGBBlendFactor:" :void)
  (factor :uint64))

(objc:define-objective-c-message %set-destination-rgb-blend-factor
    ("setDestinationRGBBlendFactor:" :void)
  (factor :uint64))

(objc:define-objective-c-message %set-rgb-blend-operation
    ("setRgbBlendOperation:" :void)
  (operation :uint64))

(objc:define-objective-c-message %set-source-alpha-blend-factor
    ("setSourceAlphaBlendFactor:" :void)
  (factor :uint64))

(objc:define-objective-c-message %set-destination-alpha-blend-factor
    ("setDestinationAlphaBlendFactor:" :void)
  (factor :uint64))

(objc:define-objective-c-message %set-alpha-blend-operation
    ("setAlphaBlendOperation:" :void)
  (operation :uint64))

(objc:define-objective-c-message %vertex-descriptor-layouts
    ("layouts" :object :ownership :borrowed
     :class "MTLVertexBufferLayoutDescriptorArray"))

(objc:define-objective-c-message %vertex-layout-at
    ("objectAtIndexedSubscript:" :object :ownership :borrowed
     :class "MTLVertexBufferLayoutDescriptor")
  (index :uint64))

(objc:define-objective-c-message %set-vertex-layout-stride
    ("setStride:" :void)
  (stride :uint64))

(objc:define-objective-c-message %set-vertex-layout-step-function
    ("setStepFunction:" :void)
  (step-function :uint64))

(objc:define-objective-c-message %vertex-descriptor-attributes
    ("attributes" :object :ownership :borrowed
     :class "MTLVertexAttributeDescriptorArray"))

(objc:define-objective-c-message %vertex-attribute-at
    ("objectAtIndexedSubscript:" :object :ownership :borrowed
     :class "MTLVertexAttributeDescriptor")
  (index :uint64))

(objc:define-objective-c-message %set-vertex-attribute-format
    ("setFormat:" :void)
  (format :uint64))

(objc:define-objective-c-message %set-vertex-attribute-offset
    ("setOffset:" :void)
  (offset :uint64))

(objc:define-objective-c-message %set-vertex-attribute-buffer-index
    ("setBufferIndex:" :void)
  (buffer-index :uint64))

(objc:define-objective-c-message %new-metal-4-render-pipeline-state
    ("newRenderPipelineStateWithDescriptor:compilerTaskOptions:error:"
     :object :ownership :owned :class "MTLRenderPipelineState")
  (descriptor :object)
  (compiler-task-options :object)
  (error :pointer))

(objc:define-objective-c-message %new-metal-depth-stencil-descriptor
    ("new" :object :ownership :owned :class "MTLDepthStencilDescriptor"))

(objc:define-objective-c-message %set-depth-compare-function
    ("setDepthCompareFunction:" :void)
  (function :uint64))

(objc:define-objective-c-message %set-depth-write-enabled
    ("setDepthWriteEnabled:" :void)
  (enabled :uint8))

(objc:define-objective-c-message %new-depth-stencil-state
    ("newDepthStencilStateWithDescriptor:"
     :object :ownership :owned :class "MTLDepthStencilState")
  (descriptor :object))

(defun configure-metal-vertex-descriptor (descriptor vertex-buffers)
  (let ((layouts (%vertex-descriptor-layouts descriptor))
        (attributes (%vertex-descriptor-attributes descriptor)))
    (dolist (buffer vertex-buffers)
      (let* ((binding (getf buffer :binding))
             (layout (%vertex-layout-at layouts binding)))
        (%set-vertex-layout-stride layout (getf buffer :array-stride))
        (%set-vertex-layout-step-function
         layout
         (ecase (getf buffer :step-mode)
           (:vertex +vertex-step-function-per-vertex+)
           (:instance +vertex-step-function-per-instance+)))
        (dolist (attribute (getf buffer :attributes))
          (let ((native-attribute
                  (%vertex-attribute-at
                   attributes (getf attribute :shader-location))))
            (%set-vertex-attribute-format
             native-attribute
             (ecase (getf attribute :format)
               (:float32x2 +vertex-format-float2+)
               (:float32x3 +vertex-format-float3+)
               (:float32x4 +vertex-format-float4+)))
            (%set-vertex-attribute-offset
             native-attribute (getf attribute :offset))
            (%set-vertex-attribute-buffer-index native-attribute binding))))))
  descriptor)

(defun compile-metal-4-render-pipeline
    (compiler vertex-library vertex-name fragment-library fragment-name
     vertex-buffers color-formats topology
     &key depth-format blends (sample-count 1) label)
  "Synchronously link Metal libraries into an owned Metal 4 pipeline state."
  (declare (ignore depth-format))
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (vertex-function
          (%new-metal-4-library-function-descriptor
           (objc:find-objective-c-class "MTL4LibraryFunctionDescriptor")))
      (%set-function-library vertex-function vertex-library)
      (%set-function-name
       vertex-function (objc:lisp-string-to-objective-c vertex-name))
      (let ((fragment-function nil))
        (unwind-protect
             (progn
               (when fragment-library
                 (setf fragment-function
                       (%new-metal-4-library-function-descriptor
                        (objc:find-objective-c-class
                         "MTL4LibraryFunctionDescriptor")))
                 (%set-function-library fragment-function fragment-library)
                 (%set-function-name
                  fragment-function
                  (objc:lisp-string-to-objective-c fragment-name)))
               (objc:with-owned-objective-c-object
                   (vertex-descriptor
                     (%new-metal-vertex-descriptor
                      (objc:find-objective-c-class "MTLVertexDescriptor")))
                 (configure-metal-vertex-descriptor
                  vertex-descriptor vertex-buffers)
                 (objc:with-owned-objective-c-object
                     (descriptor
                       (%new-metal-4-render-pipeline-descriptor
                        (objc:find-objective-c-class
                         "MTL4RenderPipelineDescriptor")))
                   (when label
                     (%set-object-label
                      descriptor (objc:lisp-string-to-objective-c label)))
                   (%set-vertex-function-descriptor descriptor vertex-function)
                   (when fragment-function
                     (%set-fragment-function-descriptor
                      descriptor fragment-function))
                   (%set-pipeline-vertex-descriptor
                    descriptor vertex-descriptor)
                   (%set-input-primitive-topology descriptor topology)
                   (%set-pipeline-raster-sample-count
                    descriptor sample-count)
                   (loop with attachments =
                           (%render-pipeline-color-attachments descriptor)
                         for color-format in color-formats
                         for blend in blends
                         for index from 0
                         for attachment =
                           (%render-pipeline-color-attachment-at
                            attachments index)
                         do (%set-render-pipeline-pixel-format
                             attachment color-format)
                            (when blend
                              (%set-render-pipeline-blending-state
                               attachment +blend-state-enabled+)
                              (%set-source-rgb-blend-factor
                               attachment +blend-factor-one+)
                              (%set-destination-rgb-blend-factor
                               attachment +blend-factor-one-minus-source-alpha+)
                              (%set-rgb-blend-operation
                               attachment +blend-operation-add+)
                              (%set-source-alpha-blend-factor
                               attachment +blend-factor-one+)
                              (%set-destination-alpha-blend-factor
                               attachment +blend-factor-one-minus-source-alpha+)
                              (%set-alpha-blend-operation
                               attachment +blend-operation-add+)))
                   (cffi:with-foreign-object (error :pointer)
                     (setf (cffi:mem-ref error :pointer) (cffi:null-pointer))
                     (let ((pipeline
                             (%new-metal-4-render-pipeline-state
                              compiler descriptor nil error)))
                       (values
                        pipeline
                        (objective-c-error-pointer-description error)))))))
          (when fragment-function
            (objc:release-objective-c-object fragment-function)))))))

(defun compile-metal-4-mesh-render-pipeline
    (compiler object-library object-name object-workgroup-size
     mesh-library mesh-name mesh-workgroup-size
     fragment-library fragment-name color-format max-mesh-workgroups
     &key blend (sample-count 1) label)
  "Synchronously link object, mesh, and fragment libraries into Metal 4."
  (objc:with-autorelease-pool ()
    (let ((object-function nil)
          (mesh-function nil)
          (fragment-function nil))
      (unwind-protect
           (progn
             (when object-library
               (setf object-function
                     (%new-metal-4-library-function-descriptor
                      (objc:find-objective-c-class
                       "MTL4LibraryFunctionDescriptor")))
               (%set-function-library object-function object-library)
               (%set-function-name
                object-function (objc:lisp-string-to-objective-c object-name)))
             (setf mesh-function
                   (%new-metal-4-library-function-descriptor
                    (objc:find-objective-c-class
                     "MTL4LibraryFunctionDescriptor")))
             (%set-function-library mesh-function mesh-library)
             (%set-function-name
              mesh-function (objc:lisp-string-to-objective-c mesh-name))
             (when fragment-library
               (setf fragment-function
                     (%new-metal-4-library-function-descriptor
                      (objc:find-objective-c-class
                       "MTL4LibraryFunctionDescriptor")))
               (%set-function-library fragment-function fragment-library)
               (%set-function-name
                fragment-function
                (objc:lisp-string-to-objective-c fragment-name)))
             (objc:with-owned-objective-c-object
                 (descriptor
                   (%new-metal-4-mesh-render-pipeline-descriptor
                    (objc:find-objective-c-class
                     "MTL4MeshRenderPipelineDescriptor")))
               (when label
                 (%set-object-label
                  descriptor (objc:lisp-string-to-objective-c label)))
               (when object-function
                 (%set-object-function-descriptor descriptor object-function)
                 (%set-max-object-threads
                  descriptor (reduce #'* object-workgroup-size)))
               (%set-mesh-function-descriptor descriptor mesh-function)
               (%set-max-mesh-threads
                descriptor (reduce #'* mesh-workgroup-size))
               (%set-max-mesh-workgroups descriptor max-mesh-workgroups)
               (%set-pipeline-raster-sample-count descriptor sample-count)
               (when fragment-function
                 (%set-fragment-function-descriptor descriptor fragment-function))
               (when color-format
                 (let ((attachment
                         (%render-pipeline-color-attachment-at
                          (%render-pipeline-color-attachments descriptor) 0)))
                   (%set-render-pipeline-pixel-format attachment color-format)
                   (when blend
                     (%set-render-pipeline-blending-state
                      attachment +blend-state-enabled+)
                     (%set-source-rgb-blend-factor
                      attachment +blend-factor-one+)
                     (%set-destination-rgb-blend-factor
                      attachment +blend-factor-one-minus-source-alpha+)
                     (%set-rgb-blend-operation attachment +blend-operation-add+)
                     (%set-source-alpha-blend-factor
                      attachment +blend-factor-one+)
                     (%set-destination-alpha-blend-factor
                      attachment +blend-factor-one-minus-source-alpha+)
                     (%set-alpha-blend-operation
                      attachment +blend-operation-add+))))
               (cffi:with-foreign-object (error :pointer)
                 (setf (cffi:mem-ref error :pointer) (cffi:null-pointer))
                 (let ((pipeline
                         (%new-metal-4-render-pipeline-state
                          compiler descriptor nil error)))
                   (values
                    pipeline
                    (objective-c-error-pointer-description error))))))
        (when fragment-function
          (objc:release-objective-c-object fragment-function))
        (when mesh-function
          (objc:release-objective-c-object mesh-function))
        (when object-function
          (objc:release-objective-c-object object-function))))))

(defun new-metal-depth-stencil-state
    (device compare-function depth-write-enabled &key label)
  "Create an owned MTLDepthStencilState for a Metal render pipeline."
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (descriptor
          (%new-metal-depth-stencil-descriptor
           (objc:find-objective-c-class "MTLDepthStencilDescriptor")))
      (when label
        (%set-object-label
         descriptor (objc:lisp-string-to-objective-c label)))
      (%set-depth-compare-function descriptor compare-function)
      (%set-depth-write-enabled descriptor (if depth-write-enabled 1 0))
      (%new-depth-stencil-state device descriptor))))

(objc:define-objective-c-message new-command-allocator
    ("newCommandAllocator" :object :ownership :owned
     :class "MTL4CommandAllocator"))

(objc:define-objective-c-message new-command-buffer
    ("newCommandBuffer" :object :ownership :owned
     :class "MTL4CommandBuffer"))

(objc:define-objective-c-message begin-command-buffer
    ("beginCommandBufferWithAllocator:" :void)
  (allocator :object))

(objc:define-objective-c-message end-command-buffer
    ("endCommandBuffer" :void))

(objc:define-objective-c-message render-command-encoder
    ("renderCommandEncoderWithDescriptor:" :object :ownership :borrowed
     :class "MTL4RenderCommandEncoder")
  (descriptor :object))

(objc:define-objective-c-message compute-command-encoder
    ("computeCommandEncoder" :object :ownership :borrowed
     :class "MTL4ComputeCommandEncoder"))

(objc:define-objective-c-message set-metal-render-pipeline
    ("setRenderPipelineState:" :void)
  (pipeline :object))

(objc:define-objective-c-message set-metal-depth-stencil-state
    ("setDepthStencilState:" :void)
  (depth-stencil-state :object))

(objc:define-objective-c-message set-metal-render-argument-table
    ("setArgumentTable:atStages:" :void)
  (argument-table :object)
  (stages :uint64))

(objc:define-objective-c-message set-metal-scissor-rect
    ("setScissorRect:" :void)
  (rectangle (:struct mtl-scissor-rect)))

(objc:define-objective-c-message draw-metal-primitives
    ("drawPrimitives:vertexStart:vertexCount:instanceCount:baseInstance:" :void)
  (primitive-type :uint64)
  (vertex-start :uint64)
  (vertex-count :uint64)
  (instance-count :uint64)
  (base-instance :uint64))

(objc:define-objective-c-message draw-metal-indexed-primitives
    ("drawIndexedPrimitives:indexCount:indexType:indexBuffer:indexBufferLength:instanceCount:baseVertex:baseInstance:"
     :void)
  (primitive-type :uint64)
  (index-count :uint64)
  (index-type :uint64)
  (index-buffer :uint64)
  (index-buffer-length :uint64)
  (instance-count :uint64)
  (base-vertex :int64)
  (base-instance :uint64))

(objc:define-objective-c-message draw-metal-mesh-threadgroups
    ("drawMeshThreadgroups:threadsPerObjectThreadgroup:threadsPerMeshThreadgroup:"
     :void)
  (threadgroups-per-grid (:struct mtl-size))
  (threads-per-object-threadgroup (:struct mtl-size))
  (threads-per-mesh-threadgroup (:struct mtl-size)))

(objc:define-objective-c-message copy-metal-texture
    ("copyFromTexture:toTexture:" :void)
  (source :object)
  (destination :object))

(objc:define-objective-c-message %copy-metal-texture-to-buffer
    ("copyFromTexture:sourceSlice:sourceLevel:sourceOrigin:sourceSize:toBuffer:destinationOffset:destinationBytesPerRow:destinationBytesPerImage:"
     :void)
  (source :object)
  (source-slice :uint64)
  (source-level :uint64)
  (source-origin (:struct mtl-origin))
  (source-size (:struct mtl-size))
  (destination :object)
  (destination-offset :uint64)
  (destination-bytes-per-row :uint64)
  (destination-bytes-per-image :uint64))

(defun copy-metal-texture-to-buffer
    (encoder source width height destination bytes-per-row)
  "Copy a complete two-dimensional Metal texture into a buffer."
  (%copy-metal-texture-to-buffer
   encoder source 0 0
   (list 'x 0 'y 0 'z 0)
   (list 'width width 'height height 'depth 1)
   destination 0 bytes-per-row 0))

(objc:define-objective-c-message end-encoding
    ("endEncoding" :void))

(objc:define-objective-c-message barrier-after-queue-stages
    ("barrierAfterQueueStages:beforeStages:visibilityOptions:" :void)
  (after-queue-stages :uint64)
  (before-stages :uint64)
  (visibility-options :uint64))

(objc:define-objective-c-message wait-for-drawable
    ("waitForDrawable:" :void)
  (drawable :object))

(objc:define-objective-c-message %commit-command-buffers
    ("commit:count:" :void)
  (command-buffers :pointer)
  (count :uint64))

(objc:define-objective-c-message signal-drawable
    ("signalDrawable:" :void)
  (drawable :object))

(objc:define-objective-c-message present-drawable
    ("present" :void))

(defun commit-command-buffers (queue command-buffers)
  "Commit a non-empty vector of ended Metal 4 command buffers to QUEUE."
  (unless (and (vectorp command-buffers) (plusp (length command-buffers)))
    (error "Expected a non-empty vector of Metal command buffers, got ~S."
           command-buffers))
  (cffi:with-foreign-object
      (native-command-buffers :pointer (length command-buffers))
    (loop for command-buffer across command-buffers
          for index from 0
          do (setf (cffi:mem-aref native-command-buffers :pointer index)
                   (objc:objective-c-pointer command-buffer)))
    (%commit-command-buffers
     queue native-command-buffers (length command-buffers)))
  (values))

(defun commit-command-buffer (queue command-buffer)
  "Commit one ended Metal 4 command buffer to QUEUE."
  (commit-command-buffers queue (vector command-buffer))
  (values))

;;; SDL's borrowed CAMetalLayer and its current drawable.

(objc:define-objective-c-message set-layer-device
    ("setDevice:" :void)
  (device :object))

(objc:define-objective-c-message set-layer-pixel-format
    ("setPixelFormat:" :void)
  (pixel-format :uint64))

(objc:define-objective-c-message set-layer-framebuffer-only
    ("setFramebufferOnly:" :void)
  (enabled :uint8))

(objc:define-objective-c-message layer-pixel-format
    ("pixelFormat" :uint64))

(objc:define-objective-c-message %set-layer-drawable-size
    ("setDrawableSize:" :void)
  (size (:struct cg-size)))

(objc:define-objective-c-message %layer-drawable-size
    ("drawableSize" (:struct cg-size)))

(objc:define-objective-c-message next-drawable
    ("nextDrawable" :object :ownership :borrowed :class "CAMetalDrawable"))

(objc:define-objective-c-message drawable-texture
    ("texture" :object :ownership :borrowed :class "MTLTexture"))

(defun set-layer-drawable-size (layer width height)
  "Set LAYER's drawable extent in physical pixels."
  (%set-layer-drawable-size
   layer (list 'width (coerce width 'double-float)
               'height (coerce height 'double-float)))
  (values))

(defun layer-drawable-size (layer)
  "Return LAYER's physical drawable width and height as two values."
  (let ((size (%layer-drawable-size layer)))
    (values (getf size 'width) (getf size 'height))))

;;; Empty render pass used as a deterministic clear.

(objc:define-objective-c-message %new-render-pass-descriptor
    ("new" :object :ownership :owned
     :class "MTL4RenderPassDescriptor"))

(objc:define-objective-c-message %render-pass-color-attachments
    ("colorAttachments" :object :ownership :borrowed
     :class "MTLRenderPassColorAttachmentDescriptorArray"))

(objc:define-objective-c-message %color-attachment-at
    ("objectAtIndexedSubscript:" :object :ownership :borrowed
     :class "MTLRenderPassColorAttachmentDescriptor")
  (index :uint64))

(objc:define-objective-c-message %set-color-attachment-texture
    ("setTexture:" :void)
  (texture :object))

(objc:define-objective-c-message %set-color-attachment-resolve-texture
    ("setResolveTexture:" :void)
  (texture :object))

(objc:define-objective-c-message %set-color-attachment-load-action
    ("setLoadAction:" :void)
  (action :uint64))

(objc:define-objective-c-message %set-color-attachment-store-action
    ("setStoreAction:" :void)
  (action :uint64))

(objc:define-objective-c-message %set-color-attachment-clear-color
    ("setClearColor:" :void)
  (color (:struct mtl-clear-color)))

(objc:define-objective-c-message %render-pass-depth-attachment
    ("depthAttachment" :object :ownership :borrowed
     :class "MTLRenderPassDepthAttachmentDescriptor"))

(objc:define-objective-c-message %set-depth-attachment-clear-depth
    ("setClearDepth:" :void)
  (depth :double))

(objc:define-objective-c-message %set-depth-resolve-filter
    ("setDepthResolveFilter:" :void)
  (filter :uint64))

(defun configure-metal-pass-color-attachment
    (descriptor index texture color clear-p store-p &optional resolve-texture)
  (when texture
    (let* ((attachments (%render-pass-color-attachments descriptor))
           (attachment (%color-attachment-at attachments index)))
      (%set-color-attachment-texture attachment texture)
      (when resolve-texture
        (%set-color-attachment-resolve-texture attachment resolve-texture))
      (%set-color-attachment-load-action
       attachment (if clear-p +load-action-clear+ +load-action-load+))
      (%set-color-attachment-store-action
       attachment (cond (resolve-texture
                         +store-action-multisample-resolve+)
                        (store-p +store-action-store+)
                        (t +store-action-dont-care+)))
      (when clear-p
        (destructuring-bind (red green blue alpha) (coerce color 'list)
          (%set-color-attachment-clear-color
           attachment
           (list 'red (coerce red 'double-float)
                 'green (coerce green 'double-float)
                 'blue (coerce blue 'double-float)
                 'alpha (coerce alpha 'double-float))))))))

(defun configure-metal-pass-depth-attachment
    (descriptor texture clear-depth clear-p store-p &optional resolve-texture)
  (when texture
    (let ((attachment (%render-pass-depth-attachment descriptor)))
      (%set-color-attachment-texture attachment texture)
      (when resolve-texture
        (%set-color-attachment-resolve-texture attachment resolve-texture)
        (%set-depth-resolve-filter
         attachment +multisample-depth-resolve-filter-sample-zero+))
      (%set-color-attachment-load-action
       attachment (if clear-p +load-action-clear+ +load-action-load+))
      (%set-color-attachment-store-action
       attachment (cond (resolve-texture
                         +store-action-multisample-resolve+)
                        (store-p +store-action-store+)
                        (t +store-action-dont-care+)))
      (when clear-p
        (%set-depth-attachment-clear-depth
         attachment (coerce clear-depth 'double-float))))))

(defun new-render-command-encoder
    (command-buffer &key color-attachments color-texture
                          (color #(0.0 0.0 0.0 1.0))
                          (color-clear-p t) (color-store-p t)
                          depth-texture (clear-depth 1.0)
                          (depth-clear-p t) (depth-store-p nil)
                          depth-resolve-texture)
  "Begin one Metal 4 render pass with optional color and depth attachments."
  (objc:with-owned-objective-c-object
      (descriptor
        (%new-render-pass-descriptor
         (objc:find-objective-c-class "MTL4RenderPassDescriptor")))
    (if color-attachments
        (loop for (texture attachment-color clear-p store-p resolve-texture)
                in color-attachments
              for index from 0
              do (configure-metal-pass-color-attachment
                  descriptor index texture attachment-color clear-p store-p
                  resolve-texture))
        (configure-metal-pass-color-attachment
         descriptor 0 color-texture color color-clear-p color-store-p))
    (configure-metal-pass-depth-attachment
     descriptor depth-texture clear-depth depth-clear-p depth-store-p
     depth-resolve-texture)
    (render-command-encoder command-buffer descriptor)))

(defun new-color-render-command-encoder
    (command-buffer texture color &key (clear-p t))
  "Begin one Metal 4 color pass and return its borrowed render encoder."
  (new-render-command-encoder
   command-buffer :color-texture texture :color color
   :color-clear-p clear-p :color-store-p t))

(defun encode-clear-pass (command-buffer texture color)
  "Encode one empty Metal 4 render pass which clears TEXTURE to COLOR."
  (let ((encoder
          (new-color-render-command-encoder
           command-buffer texture color :clear-p t)))
    (unless encoder
      (error "Metal did not create a render command encoder."))
    (end-encoding encoder)
    encoder))
