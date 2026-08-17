;;;; The Vulkan ABI that luv's backend currently speaks.
;;;;
;;;; This is intentionally ordinary, incomplete CFFI treaty text: constants,
;;;; symbolic enum/bitfield pieces, structs, and raw entry points.  New Vulkan
;;;; declarations belong here only when a concrete backend operation needs them.

(in-package #:luv.vulkan)

(defparameter +portability-enumeration-extension-name+
  "VK_KHR_portability_enumeration")

(defparameter +swapchain-extension-name+ "VK_KHR_swapchain")

(defparameter +debug-utils-extension-name+ "VK_EXT_debug_utils")

;;; Symbolic pieces of the Vulkan vocabulary we currently speak.

(cffi:defcenum (result :int32)
  (:success 0)
  (:incomplete 5)
  (:suboptimal-khr 1000001003))

(cffi:defcenum (structure-type :uint32)
  (:application-info 0)
  (:instance-create-info 1)
  (:device-queue-create-info 2)
  (:device-create-info 3)
  (:memory-allocate-info 5)
  (:buffer-create-info 12)
  (:semaphore-create-info 9)
  (:image-create-info 14)
  (:image-view-create-info 15)
  (:shader-module-create-info 16)
  (:pipeline-shader-stage-create-info 18)
  (:pipeline-vertex-input-state-create-info 19)
  (:pipeline-input-assembly-state-create-info 20)
  (:pipeline-viewport-state-create-info 22)
  (:pipeline-rasterization-state-create-info 23)
  (:pipeline-multisample-state-create-info 24)
  (:pipeline-depth-stencil-state-create-info 25)
  (:pipeline-color-blend-state-create-info 26)
  (:pipeline-dynamic-state-create-info 27)
  (:graphics-pipeline-create-info 28)
  (:compute-pipeline-create-info 29)
  (:pipeline-layout-create-info 30)
  (:sampler-create-info 31)
  (:descriptor-set-layout-create-info 32)
  (:descriptor-pool-create-info 33)
  (:descriptor-set-allocate-info 34)
  (:write-descriptor-set 35)
  (:framebuffer-create-info 37)
  (:render-pass-create-info 38)
  (:command-pool-create-info 39)
  (:command-buffer-allocate-info 40)
  (:command-buffer-begin-info 42)
  (:render-pass-begin-info 43)
  (:image-memory-barrier 45)
  (:physical-device-timeline-semaphore-features 1000207000)
  (:semaphore-type-create-info 1000207002)
  (:semaphore-wait-info 1000207004)
  (:submit-info-2 1000314004)
  (:semaphore-submit-info 1000314005)
  (:command-buffer-submit-info 1000314006)
  (:physical-device-synchronization-2-features 1000314007)
  (:swapchain-create-info-khr 1000001000)
  (:present-info-khr 1000001001)
  (:debug-utils-messenger-callback-data-ext 1000128003)
  (:debug-utils-messenger-create-info-ext 1000128004))

(cffi:defcenum (image-type :uint32)
  (:1d 0)
  (:2d 1)
  (:3d 2))

(cffi:defcenum (format :uint32 :allow-undeclared-values t)
  (:r8g8b8a8-unorm 37)
  (:r8g8b8a8-srgb 43)
  (:b8g8r8a8-unorm 44)
  (:b8g8r8a8-srgb 50)
  (:r16g16-uint 81)
  (:r16g16b16a16-sfloat 97)
  (:r32g32b32-sfloat 106)
  (:d32-sfloat 126))

(cffi:defcenum (image-tiling :uint32)
  (:optimal 0)
  (:linear 1))

(cffi:defcenum (sharing-mode :uint32)
  (:exclusive 0)
  (:concurrent 1))

(cffi:defcenum (image-layout :uint32)
  (:undefined 0)
  (:general 1)
  (:color-attachment-optimal 2)
  (:depth-stencil-attachment-optimal 3)
  (:shader-read-only-optimal 5)
  (:transfer-src-optimal 6)
  (:transfer-dst-optimal 7)
  (:present-src-khr 1000001002))

(cffi:defcenum (sample-count :uint32)
  (:1 1))

(cffi:defcenum (image-view-type :uint32)
  (:1d 0)
  (:2d 1)
  (:3d 2))

(cffi:defcenum (component-swizzle :uint32)
  (:identity 0))

(cffi:defcenum (descriptor-type :uint32)
  (:sampler 0)
  (:combined-image-sampler 1)
  (:sampled-image 2)
  (:storage-image 3)
  (:uniform-buffer 6))

(cffi:defcenum (filter :uint32)
  (:nearest 0)
  (:linear 1))

(cffi:defcenum (sampler-mipmap-mode :uint32)
  (:nearest 0)
  (:linear 1))

(cffi:defcenum (sampler-address-mode :uint32)
  (:repeat 0)
  (:mirrored-repeat 1)
  (:clamp-to-edge 2))

(cffi:defcenum (compare-op :uint32)
  (:never 0)
  (:less 1)
  (:equal 2)
  (:less-or-equal 3)
  (:greater 4)
  (:not-equal 5)
  (:greater-or-equal 6)
  (:always 7))

(cffi:defcenum (stencil-op :uint32)
  (:keep 0)
  (:zero 1)
  (:replace 2)
  (:increment-and-clamp 3)
  (:decrement-and-clamp 4)
  (:invert 5)
  (:increment-and-wrap 6)
  (:decrement-and-wrap 7))

(cffi:defcenum (vertex-input-rate :uint32)
  (:vertex 0)
  (:instance 1))

(cffi:defcenum (border-color :uint32)
  (:float-transparent-black 0))

(cffi:defcenum (primitive-topology :uint32)
  (:triangle-list 3)
  (:triangle-strip 4))

(cffi:defcenum (polygon-mode :uint32)
  (:fill 0))

(cffi:defcenum (front-face :uint32)
  (:counter-clockwise 1)
  (:clockwise 0))

(cffi:defcenum (blend-factor :uint32)
  (:zero 0)
  (:one 1)
  (:one-minus-src-alpha 7))

(cffi:defcenum (blend-op :uint32)
  (:add 0))

(cffi:defcenum (logic-op :uint32)
  (:copy 3))

(cffi:defcenum (attachment-load-op :uint32)
  (:load 0)
  (:clear 1)
  (:dont-care 2))

(cffi:defcenum (attachment-store-op :uint32)
  (:store 0)
  (:dont-care 1))

(cffi:defcenum (subpass-contents :uint32)
  (:inline 0))

(cffi:defcenum (dynamic-state :uint32)
  (:viewport 0)
  (:scissor 1))

(cffi:defcenum (pipeline-bind-point :uint32)
  (:graphics 0)
  (:compute 1))

(cffi:defcenum (command-buffer-level :uint32)
  (:primary 0)
  (:secondary 1))

(cffi:defcenum (color-space :uint32 :allow-undeclared-values t)
  (:srgb-nonlinear-khr 0))

(cffi:defcenum (present-mode :uint32 :allow-undeclared-values t)
  (:immediate-khr 0)
  (:mailbox-khr 1)
  (:fifo-khr 2)
  (:fifo-relaxed-khr 3))

(cffi:defcenum (surface-transform :uint32)
  (:identity #x1)
  (:rotate-90 #x2)
  (:rotate-180 #x4)
  (:rotate-270 #x8)
  (:horizontal-mirror #x10)
  (:horizontal-mirror-rotate-90 #x20)
  (:horizontal-mirror-rotate-180 #x40)
  (:horizontal-mirror-rotate-270 #x80)
  (:inherit #x100))

(cffi:defcenum (composite-alpha :uint32)
  (:opaque #x1)
  (:pre-multiplied #x2)
  (:post-multiplied #x4)
  (:inherit #x8))

(cffi:defbitfield (instance-create-flags :uint32)
  (:enumerate-portability #x1))

(cffi:defbitfield (debug-utils-message-severity-flags :uint32)
  (:verbose #x1)
  (:info #x10)
  (:warning #x100)
  (:error #x1000))

(cffi:defbitfield (debug-utils-message-type-flags :uint32)
  (:general #x1)
  (:validation #x2)
  (:performance #x4)
  (:device-address-binding #x8))

(cffi:defbitfield (queue-flags :uint32)
  (:graphics #x1)
  (:compute #x2)
  (:transfer #x4)
  (:sparse-binding #x8))

(cffi:defbitfield (image-usage-flags :uint32)
  (:transfer-src #x1)
  (:transfer-dst #x2)
  (:sampled #x4)
  (:storage #x8)
  (:color-attachment #x10)
  (:depth-stencil-attachment #x20))

(cffi:defbitfield (buffer-usage-flags :uint32)
  (:transfer-src #x1)
  (:transfer-dst #x2)
  (:uniform #x10)
  (:vertex #x80))

(cffi:defbitfield (shader-stage-flags :uint32)
  (:vertex #x1)
  (:fragment #x10)
  (:compute #x20))

(cffi:defbitfield (cull-mode-flags :uint32)
  (:front #x1)
  (:back #x2))

(cffi:defbitfield (color-component-flags :uint32)
  (:r #x1) (:g #x2) (:b #x4) (:a #x8))

(cffi:defbitfield (memory-property-flags :uint32)
  (:device-local #x1)
  (:host-visible #x2)
  (:host-coherent #x4)
  (:host-cached #x8)
  (:lazily-allocated #x10)
  (:protected #x20))

(cffi:defbitfield (command-pool-create-flags :uint32)
  (:transient #x1)
  (:reset-command-buffer #x2)
  (:protected #x4))

(cffi:defcenum (semaphore-type :uint32)
  (:binary 0)
  (:timeline 1))

(cffi:defbitfield (command-buffer-usage-flags :uint32)
  (:one-time-submit #x1)
  (:render-pass-continue #x2)
  (:simultaneous-use #x4))

(cffi:defbitfield (image-aspect-flags :uint32)
  (:color #x1)
  (:depth #x2)
  (:stencil #x4))

(cffi:defbitfield (access-flags :uint32)
  (:depth-stencil-attachment-read #x200)
  (:depth-stencil-attachment-write #x400)
  (:color-attachment-read #x80)
  (:color-attachment-write #x100)
  (:shader-read #x20)
  (:shader-write #x40)
  (:transfer-read #x800)
  (:transfer-write #x1000))

(cffi:defbitfield (pipeline-stage-flags :uint32)
  (:top-of-pipe #x1)
  (:vertex-shader #x8)
  (:early-fragment-tests #x100)
  (:late-fragment-tests #x200)
  (:fragment-shader #x80)
  (:color-attachment-output #x400)
  (:compute-shader #x800)
  (:transfer #x1000)
  (:bottom-of-pipe #x2000))

;; Synchronization2 stage masks are 64 bits wide.  The stages luv uses all
;; keep their Vulkan 1.0 bit positions.
(cffi:defbitfield (pipeline-stage-flags-2 :uint64)
  (:top-of-pipe #x1)
  (:vertex-shader #x8)
  (:early-fragment-tests #x100)
  (:late-fragment-tests #x200)
  (:fragment-shader #x80)
  (:color-attachment-output #x400)
  (:compute-shader #x800)
  (:transfer #x1000)
  (:bottom-of-pipe #x2000)
  (:all-commands #x10000))

(cffi:defbitfield (dependency-flags :uint32)
  (:by-region #x1))

(cffi:defbitfield (surface-transform-flags :uint32)
  (:identity #x1)
  (:rotate-90 #x2)
  (:rotate-180 #x4)
  (:rotate-270 #x8)
  (:horizontal-mirror #x10)
  (:horizontal-mirror-rotate-90 #x20)
  (:horizontal-mirror-rotate-180 #x40)
  (:horizontal-mirror-rotate-270 #x80)
  (:inherit #x100))

(cffi:defbitfield (composite-alpha-flags :uint32)
  (:opaque #x1)
  (:pre-multiplied #x2)
  (:post-multiplied #x4)
  (:inherit #x8))

(defconstant +queue-family-ignored+ #xffffffff)
(defvkstruct extension-properties ()
  (extension-name (:array :char 256))
  (spec-version :uint32))

(defvkstruct application-info (:s-type :application-info)
  (p-application-name :pointer)
  (application-version :uint32)
  (p-engine-name :pointer)
  (engine-version :uint32)
  (api-version :uint32))

(defvkstruct instance-create-info (:s-type :instance-create-info)
  (flags instance-create-flags)
  (p-application-info :pointer)
  (enabled-layer-count :uint32)
  (pp-enabled-layer-names :pointer)
  (enabled-extension-count :uint32)
  (pp-enabled-extension-names :pointer))

(defvkstruct debug-utils-messenger-create-info-ext
    (:s-type :debug-utils-messenger-create-info-ext)
  (flags :uint32)
  (message-severity debug-utils-message-severity-flags)
  (message-type debug-utils-message-type-flags)
  (pfn-user-callback :pointer)
  (p-user-data :pointer))

(defvkstruct debug-utils-messenger-callback-data-ext
    (:s-type :debug-utils-messenger-callback-data-ext)
  (flags :uint32)
  (p-message-id-name :pointer)
  (message-id-number :int32)
  (p-message :pointer)
  (queue-label-count :uint32)
  (p-queue-labels :pointer)
  (command-buffer-label-count :uint32)
  (p-command-buffer-labels :pointer)
  (object-count :uint32)
  (p-objects :pointer))

(defvkstruct extent-3d ()
  (width :uint32)
  (height :uint32)
  (depth :uint32))

(defvkstruct extent-2d ()
  (width :uint32)
  (height :uint32))

(defvkstruct offset-2d ()
  (x :int32)
  (y :int32))

(defvkstruct rect-2d ()
  (offset (:struct offset-2d))
  (extent (:struct extent-2d)))

(defvkstruct queue-family-properties ()
  (queue-flags queue-flags)
  (queue-count :uint32)
  (timestamp-valid-bits :uint32)
  (min-image-transfer-granularity (:struct extent-3d)))

;; VkPhysicalDeviceFeatures is an untagged, fixed-layout Vulkan 1.0 struct.
;; Keep the complete treaty here even though luv currently requires only
;; SHADER-INT64: passing a shortened struct as P-ENABLED-FEATURES would let the
;; driver read beyond our allocation.
(defvkstruct physical-device-features ()
  (robust-buffer-access :uint32)
  (full-draw-index-uint32 :uint32)
  (image-cube-array :uint32)
  (independent-blend :uint32)
  (geometry-shader :uint32)
  (tessellation-shader :uint32)
  (sample-rate-shading :uint32)
  (dual-src-blend :uint32)
  (logic-op :uint32)
  (multi-draw-indirect :uint32)
  (draw-indirect-first-instance :uint32)
  (depth-clamp :uint32)
  (depth-bias-clamp :uint32)
  (fill-mode-non-solid :uint32)
  (depth-bounds :uint32)
  (wide-lines :uint32)
  (large-points :uint32)
  (alpha-to-one :uint32)
  (multi-viewport :uint32)
  (sampler-anisotropy :uint32)
  (texture-compression-etc2 :uint32)
  (texture-compression-astc-ldr :uint32)
  (texture-compression-bc :uint32)
  (occlusion-query-precise :uint32)
  (pipeline-statistics-query :uint32)
  (vertex-pipeline-stores-and-atomics :uint32)
  (fragment-stores-and-atomics :uint32)
  (shader-tessellation-and-geometry-point-size :uint32)
  (shader-image-gather-extended :uint32)
  (shader-storage-image-extended-formats :uint32)
  (shader-storage-image-multisample :uint32)
  (shader-storage-image-read-without-format :uint32)
  (shader-storage-image-write-without-format :uint32)
  (shader-uniform-buffer-array-dynamic-indexing :uint32)
  (shader-sampled-image-array-dynamic-indexing :uint32)
  (shader-storage-buffer-array-dynamic-indexing :uint32)
  (shader-storage-image-array-dynamic-indexing :uint32)
  (shader-clip-distance :uint32)
  (shader-cull-distance :uint32)
  (shader-float64 :uint32)
  (shader-int64 :uint32)
  (shader-int16 :uint32)
  (shader-resource-residency :uint32)
  (shader-resource-min-lod :uint32)
  (sparse-binding :uint32)
  (sparse-residency-buffer :uint32)
  (sparse-residency-image-2d :uint32)
  (sparse-residency-image-3d :uint32)
  (sparse-residency-2-samples :uint32)
  (sparse-residency-4-samples :uint32)
  (sparse-residency-8-samples :uint32)
  (sparse-residency-16-samples :uint32)
  (sparse-residency-aliased :uint32)
  (variable-multisample-rate :uint32)
  (inherited-queries :uint32))

(defvkstruct device-queue-create-info (:s-type :device-queue-create-info)
  (flags :uint32)
  (queue-family-index :uint32)
  (queue-count :uint32)
  (p-queue-priorities :pointer))

(defvkstruct device-create-info (:s-type :device-create-info)
  (flags :uint32)
  (queue-create-info-count :uint32)
  (p-queue-create-infos :pointer)
  (enabled-layer-count :uint32)
  (pp-enabled-layer-names :pointer)
  (enabled-extension-count :uint32)
  (pp-enabled-extension-names :pointer)
  (p-enabled-features :pointer))

(defvkstruct memory-type ()
  (property-flags memory-property-flags)
  (heap-index :uint32))

(defvkstruct memory-heap ()
  (size :uint64)
  (flags :uint32))

(defvkstruct physical-device-memory-properties ()
  (memory-type-count :uint32)
  (memory-types (:array (:struct memory-type) 32))
  (memory-heap-count :uint32)
  (memory-heaps (:array (:struct memory-heap) 16)))

(defvkstruct memory-allocate-info (:s-type :memory-allocate-info)
  (allocation-size :uint64)
  (memory-type-index :uint32))

(defvkstruct memory-requirements ()
  (size :uint64)
  (alignment :uint64)
  (memory-type-bits :uint32))

(defvkstruct buffer-create-info (:s-type :buffer-create-info)
  (flags :uint32)
  (size :uint64)
  (usage buffer-usage-flags)
  (sharing-mode sharing-mode)
  (queue-family-index-count :uint32)
  (p-queue-family-indices :pointer))

(defvkstruct image-create-info (:s-type :image-create-info)
  (flags :uint32)
  (image-type image-type)
  (format format)
  (extent (:struct extent-3d))
  (mip-levels :uint32)
  (array-layers :uint32)
  (samples sample-count)
  (tiling image-tiling)
  (usage image-usage-flags)
  (sharing-mode sharing-mode)
  (queue-family-index-count :uint32)
  (p-queue-family-indices :pointer)
  (initial-layout image-layout))

(defvkstruct offset-3d ()
  (x :int32)
  (y :int32)
  (z :int32))

(defvkstruct image-subresource-layers ()
  (aspect-mask image-aspect-flags)
  (mip-level :uint32)
  (base-array-layer :uint32)
  (layer-count :uint32))

(defvkstruct image-subresource-range ()
  (aspect-mask image-aspect-flags)
  (base-mip-level :uint32)
  (level-count :uint32)
  (base-array-layer :uint32)
  (layer-count :uint32))

(defvkstruct component-mapping ()
  (r component-swizzle)
  (g component-swizzle)
  (b component-swizzle)
  (a component-swizzle))

(defvkstruct image-view-create-info (:s-type :image-view-create-info)
  (flags :uint32)
  (image :pointer)
  (view-type image-view-type)
  (format format)
  (components (:struct component-mapping))
  (subresource-range (:struct image-subresource-range)))

(defvkstruct shader-module-create-info (:s-type :shader-module-create-info)
  (flags :uint32)
  (code-size :size)
  (p-code :pointer))

(defvkstruct pipeline-shader-stage-create-info
    (:s-type :pipeline-shader-stage-create-info)
  (flags :uint32)
  (stage shader-stage-flags)
  (module :pointer)
  (p-name :pointer)
  (p-specialization-info :pointer))

(defvkstruct compute-pipeline-create-info
    (:s-type :compute-pipeline-create-info)
  (flags :uint32)
  (stage (:struct pipeline-shader-stage-create-info))
  (layout :pointer)
  (base-pipeline-handle :pointer)
  (base-pipeline-index :int32))

(defvkstruct pipeline-vertex-input-state-create-info
    (:s-type :pipeline-vertex-input-state-create-info)
  (flags :uint32)
  (vertex-binding-description-count :uint32)
  (p-vertex-binding-descriptions :pointer)
  (vertex-attribute-description-count :uint32)
  (p-vertex-attribute-descriptions :pointer))

(defvkstruct vertex-input-binding-description ()
  (binding :uint32)
  (stride :uint32)
  (input-rate vertex-input-rate))

(defvkstruct vertex-input-attribute-description ()
  (location :uint32)
  (binding :uint32)
  (format format)
  (offset :uint32))

(defvkstruct pipeline-input-assembly-state-create-info
    (:s-type :pipeline-input-assembly-state-create-info)
  (flags :uint32)
  (topology primitive-topology)
  (primitive-restart-enable :uint32))

(defvkstruct pipeline-viewport-state-create-info
    (:s-type :pipeline-viewport-state-create-info)
  (flags :uint32)
  (viewport-count :uint32)
  (p-viewports :pointer)
  (scissor-count :uint32)
  (p-scissors :pointer))

(defvkstruct pipeline-rasterization-state-create-info
    (:s-type :pipeline-rasterization-state-create-info)
  (flags :uint32)
  (depth-clamp-enable :uint32)
  (rasterizer-discard-enable :uint32)
  (polygon-mode polygon-mode)
  (cull-mode cull-mode-flags)
  (front-face front-face)
  (depth-bias-enable :uint32)
  (depth-bias-constant-factor :float)
  (depth-bias-clamp :float)
  (depth-bias-slope-factor :float)
  (line-width :float))

(defvkstruct pipeline-multisample-state-create-info
    (:s-type :pipeline-multisample-state-create-info)
  (flags :uint32)
  (rasterization-samples sample-count)
  (sample-shading-enable :uint32)
  (min-sample-shading :float)
  (p-sample-mask :pointer)
  (alpha-to-coverage-enable :uint32)
  (alpha-to-one-enable :uint32))

(defvkstruct stencil-op-state ()
  (fail-op stencil-op)
  (pass-op stencil-op)
  (depth-fail-op stencil-op)
  (compare-op compare-op)
  (compare-mask :uint32)
  (write-mask :uint32)
  (reference :uint32))

(defvkstruct pipeline-depth-stencil-state-create-info
    (:s-type :pipeline-depth-stencil-state-create-info)
  (flags :uint32)
  (depth-test-enable :uint32)
  (depth-write-enable :uint32)
  (depth-compare-op compare-op)
  (depth-bounds-test-enable :uint32)
  (stencil-test-enable :uint32)
  (front (:struct stencil-op-state))
  (back (:struct stencil-op-state))
  (min-depth-bounds :float)
  (max-depth-bounds :float))

(defvkstruct pipeline-color-blend-attachment-state ()
  (blend-enable :uint32)
  (src-color-blend-factor blend-factor)
  (dst-color-blend-factor blend-factor)
  (color-blend-op blend-op)
  (src-alpha-blend-factor blend-factor)
  (dst-alpha-blend-factor blend-factor)
  (alpha-blend-op blend-op)
  (color-write-mask color-component-flags))

(defvkstruct pipeline-color-blend-state-create-info
    (:s-type :pipeline-color-blend-state-create-info)
  (flags :uint32)
  (logic-op-enable :uint32)
  (logic-op logic-op)
  (attachment-count :uint32)
  (p-attachments :pointer)
  (blend-constants (:array :float 4)))

(defvkstruct pipeline-dynamic-state-create-info
    (:s-type :pipeline-dynamic-state-create-info)
  (flags :uint32)
  (dynamic-state-count :uint32)
  (p-dynamic-states :pointer))

(defvkstruct graphics-pipeline-create-info
    (:s-type :graphics-pipeline-create-info)
  (flags :uint32)
  (stage-count :uint32)
  (p-stages :pointer)
  (p-vertex-input-state :pointer)
  (p-input-assembly-state :pointer)
  (p-tessellation-state :pointer)
  (p-viewport-state :pointer)
  (p-rasterization-state :pointer)
  (p-multisample-state :pointer)
  (p-depth-stencil-state :pointer)
  (p-color-blend-state :pointer)
  (p-dynamic-state :pointer)
  (layout :pointer)
  (render-pass :pointer)
  (subpass :uint32)
  (base-pipeline-handle :pointer)
  (base-pipeline-index :int32))

(defvkstruct pipeline-layout-create-info
    (:s-type :pipeline-layout-create-info)
  (flags :uint32)
  (set-layout-count :uint32)
  (p-set-layouts :pointer)
  (push-constant-range-count :uint32)
  (p-push-constant-ranges :pointer))

(defvkstruct sampler-create-info (:s-type :sampler-create-info)
  (flags :uint32)
  (mag-filter filter)
  (min-filter filter)
  (mipmap-mode sampler-mipmap-mode)
  (address-mode-u sampler-address-mode)
  (address-mode-v sampler-address-mode)
  (address-mode-w sampler-address-mode)
  (mip-lod-bias :float)
  (anisotropy-enable :uint32)
  (max-anisotropy :float)
  (compare-enable :uint32)
  (compare-op compare-op)
  (min-lod :float)
  (max-lod :float)
  (border-color border-color)
  (unnormalized-coordinates :uint32))

(defvkstruct descriptor-set-layout-binding ()
  (binding :uint32)
  (descriptor-type descriptor-type)
  (descriptor-count :uint32)
  (stage-flags shader-stage-flags)
  (p-immutable-samplers :pointer))

(defvkstruct descriptor-set-layout-create-info
    (:s-type :descriptor-set-layout-create-info)
  (flags :uint32)
  (binding-count :uint32)
  (p-bindings :pointer))

(defvkstruct descriptor-pool-size ()
  (type descriptor-type)
  (descriptor-count :uint32))

(defvkstruct descriptor-pool-create-info
    (:s-type :descriptor-pool-create-info)
  (flags :uint32)
  (max-sets :uint32)
  (pool-size-count :uint32)
  (p-pool-sizes :pointer))

(defvkstruct descriptor-set-allocate-info
    (:s-type :descriptor-set-allocate-info)
  (descriptor-pool :pointer)
  (descriptor-set-count :uint32)
  (p-set-layouts :pointer))

(defvkstruct descriptor-image-info ()
  (sampler :pointer)
  (image-view :pointer)
  (image-layout image-layout))

(defvkstruct descriptor-buffer-info ()
  (buffer :pointer)
  (offset :uint64)
  (range :uint64))

(defvkstruct write-descriptor-set (:s-type :write-descriptor-set)
  (dst-set :pointer)
  (dst-binding :uint32)
  (dst-array-element :uint32)
  (descriptor-count :uint32)
  (descriptor-type descriptor-type)
  (p-image-info :pointer)
  (p-buffer-info :pointer)
  (p-texel-buffer-view :pointer))

(defvkstruct attachment-description ()
  (flags :uint32)
  (format format)
  (samples sample-count)
  (load-op attachment-load-op)
  (store-op attachment-store-op)
  (stencil-load-op attachment-load-op)
  (stencil-store-op attachment-store-op)
  (initial-layout image-layout)
  (final-layout image-layout))

(defvkstruct attachment-reference ()
  (attachment :uint32)
  (layout image-layout))

(defvkstruct subpass-description ()
  (flags :uint32)
  (pipeline-bind-point pipeline-bind-point)
  (input-attachment-count :uint32)
  (p-input-attachments :pointer)
  (color-attachment-count :uint32)
  (p-color-attachments :pointer)
  (p-resolve-attachments :pointer)
  (p-depth-stencil-attachment :pointer)
  (preserve-attachment-count :uint32)
  (p-preserve-attachments :pointer))

(defvkstruct render-pass-create-info (:s-type :render-pass-create-info)
  (flags :uint32)
  (attachment-count :uint32)
  (p-attachments :pointer)
  (subpass-count :uint32)
  (p-subpasses :pointer)
  (dependency-count :uint32)
  (p-dependencies :pointer))

(defvkstruct framebuffer-create-info (:s-type :framebuffer-create-info)
  (flags :uint32)
  (render-pass :pointer)
  (attachment-count :uint32)
  (p-attachments :pointer)
  (width :uint32)
  (height :uint32)
  (layers :uint32))

(defvkstruct viewport ()
  (x :float) (y :float) (width :float) (height :float)
  (min-depth :float) (max-depth :float))

(defvkstruct image-memory-barrier (:s-type :image-memory-barrier)
  (src-access-mask access-flags)
  (dst-access-mask access-flags)
  (old-layout image-layout)
  (new-layout image-layout)
  (src-queue-family-index :uint32)
  (dst-queue-family-index :uint32)
  (image :pointer)
  (subresource-range (:struct image-subresource-range)))

(defvkstruct image-copy ()
  (src-subresource (:struct image-subresource-layers))
  (src-offset (:struct offset-3d))
  (dst-subresource (:struct image-subresource-layers))
  (dst-offset (:struct offset-3d))
  (extent (:struct extent-3d)))

(defvkstruct buffer-image-copy ()
  (buffer-offset :uint64)
  (buffer-row-length :uint32)
  (buffer-image-height :uint32)
  (image-subresource (:struct image-subresource-layers))
  (image-offset (:struct offset-3d))
  (image-extent (:struct extent-3d)))

(defvkstruct command-pool-create-info (:s-type :command-pool-create-info)
  (flags command-pool-create-flags)
  (queue-family-index :uint32))

(defvkstruct command-buffer-allocate-info
    (:s-type :command-buffer-allocate-info)
  (command-pool :pointer)
  (level command-buffer-level)
  (command-buffer-count :uint32))

(defvkstruct command-buffer-begin-info (:s-type :command-buffer-begin-info)
  (flags command-buffer-usage-flags)
  (p-inheritance-info :pointer))

(cffi:defcunion clear-color-value
  (float-32 (:array :float 4))
  (int-32 (:array :int32 4))
  (uint-32 (:array :uint32 4)))

(defvkstruct clear-depth-stencil-value ()
  (depth :float)
  (stencil :uint32))

(cffi:defcunion clear-value
  (color (:union clear-color-value))
  (depth-stencil (:struct clear-depth-stencil-value)))

(defvkstruct render-pass-begin-info (:s-type :render-pass-begin-info)
  (render-pass :pointer)
  (framebuffer :pointer)
  (render-area (:struct rect-2d))
  (clear-value-count :uint32)
  (p-clear-values :pointer))

(defvkstruct semaphore-submit-info (:s-type :semaphore-submit-info)
  (semaphore :pointer)
  (value :uint64)
  (stage-mask pipeline-stage-flags-2)
  (device-index :uint32))

(defvkstruct command-buffer-submit-info (:s-type :command-buffer-submit-info)
  (command-buffer :pointer)
  (device-mask :uint32))

(defvkstruct submit-info-2 (:s-type :submit-info-2)
  (flags :uint32)
  (wait-semaphore-info-count :uint32)
  (p-wait-semaphore-infos :pointer)
  (command-buffer-info-count :uint32)
  (p-command-buffer-infos :pointer)
  (signal-semaphore-info-count :uint32)
  (p-signal-semaphore-infos :pointer))

(defvkstruct semaphore-create-info (:s-type :semaphore-create-info)
  (flags :uint32))

(defvkstruct semaphore-type-create-info (:s-type :semaphore-type-create-info)
  (semaphore-type semaphore-type)
  (initial-value :uint64))

(defvkstruct semaphore-wait-info (:s-type :semaphore-wait-info)
  (flags :uint32)
  (semaphore-count :uint32)
  (p-semaphores :pointer)
  (p-values :pointer))

(defvkstruct physical-device-timeline-semaphore-features
    (:s-type :physical-device-timeline-semaphore-features)
  (timeline-semaphore :uint32))

(defvkstruct physical-device-synchronization-2-features
    (:s-type :physical-device-synchronization-2-features)
  (synchronization-2 :uint32))

(defvkstruct surface-capabilities ()
  (min-image-count :uint32)
  (max-image-count :uint32)
  (current-extent (:struct extent-2d))
  (min-image-extent (:struct extent-2d))
  (max-image-extent (:struct extent-2d))
  (max-image-array-layers :uint32)
  (supported-transforms surface-transform-flags)
  (current-transform surface-transform)
  (supported-composite-alpha composite-alpha-flags)
  (supported-usage-flags image-usage-flags))

(defvkstruct surface-format ()
  (format format)
  (color-space color-space))

(defvkstruct swapchain-create-info (:s-type :swapchain-create-info-khr)
  (flags :uint32)
  (surface :pointer)
  (min-image-count :uint32)
  (image-format format)
  (image-color-space color-space)
  (image-extent (:struct extent-2d))
  (image-array-layers :uint32)
  (image-usage image-usage-flags)
  (image-sharing-mode sharing-mode)
  (queue-family-index-count :uint32)
  (p-queue-family-indices :pointer)
  (pre-transform surface-transform)
  (composite-alpha composite-alpha)
  (present-mode present-mode)
  (clipped :uint32)
  (old-swapchain :pointer))

(defvkstruct present-info (:s-type :present-info-khr)
  (wait-semaphore-count :uint32)
  (p-wait-semaphores :pointer)
  (swapchain-count :uint32)
  (p-swapchains :pointer)
  (p-image-indices :pointer)
  (p-results :pointer))

;;; Raw calls.  DEFVKFUN preserves the CFFI declaration while making the
;;; complete host-to-driver boundary available as structured Lisp data.

(defvkfun "vkEnumerateInstanceExtensionProperties"
    checked-result
  (layer-name :pointer)
  (property-count :pointer)
  (properties :pointer))

(defvkfun "vkCreateInstance"
    checked-result
  (create-info :pointer)
  (allocator :pointer)
  (instance :pointer))

(defvkfun "vkDestroyInstance"
    :void
  (instance :pointer)
  (allocator :pointer))

(defvkfun "vkGetInstanceProcAddr"
    :pointer
  (instance :pointer)
  (name :string))

(defvkproc "vkCreateDebugUtilsMessengerEXT"
    checked-result
  (instance :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (messenger :pointer))

(defvkproc "vkDestroyDebugUtilsMessengerEXT"
    :void
  (instance :pointer)
  (messenger :pointer)
  (allocator :pointer))

(defvkfun "vkEnumeratePhysicalDevices"
    checked-result
  (instance :pointer)
  (device-count :pointer)
  (devices :pointer))

(defvkfun "vkGetPhysicalDeviceQueueFamilyProperties"
    :void
  (physical-device :pointer)
  (property-count :pointer)
  (properties :pointer))

(defvkfun "vkGetPhysicalDeviceFeatures"
    :void
  (physical-device :pointer)
  (features :pointer))

(defvkfun "vkCreateDevice"
    checked-result
  (physical-device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (device :pointer))

(defvkfun "vkDestroyDevice"
    :void
  (device :pointer)
  (allocator :pointer))

(defvkfun "vkGetDeviceQueue"
    :void
  (device :pointer)
  (queue-family-index :uint32)
  (queue-index :uint32)
  (queue :pointer))

(defvkfun "vkDeviceWaitIdle"
    checked-result
  (device :pointer))

(defvkfun "vkGetPhysicalDeviceMemoryProperties"
    :void
  (physical-device :pointer)
  (properties :pointer))

(defvkfun "vkCreateImage"
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (image :pointer))

(defvkfun "vkDestroyImage"
    :void
  (device :pointer)
  (image :pointer)
  (allocator :pointer))

(defvkfun "vkGetImageMemoryRequirements"
    :void
  (device :pointer)
  (image :pointer)
  (requirements :pointer))

(defvkfun "vkCreateBuffer"
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (buffer :pointer))

(defvkfun "vkDestroyBuffer"
    :void
  (device :pointer)
  (buffer :pointer)
  (allocator :pointer))

(defvkfun "vkGetBufferMemoryRequirements"
    :void
  (device :pointer)
  (buffer :pointer)
  (requirements :pointer))

(defvkfun "vkAllocateMemory"
    checked-result
  (device :pointer)
  (allocate-info :pointer)
  (allocator :pointer)
  (memory :pointer))

(defvkfun "vkFreeMemory"
    :void
  (device :pointer)
  (memory :pointer)
  (allocator :pointer))

(defvkfun "vkBindImageMemory"
    checked-result
  (device :pointer)
  (image :pointer)
  (memory :pointer)
  (offset :uint64))

(defvkfun "vkBindBufferMemory"
    checked-result
  (device :pointer)
  (buffer :pointer)
  (memory :pointer)
  (offset :uint64))

(defvkfun "vkMapMemory"
    checked-result
  (device :pointer)
  (memory :pointer)
  (offset :uint64)
  (size :uint64)
  (flags :uint32)
  (data :pointer))

(defvkfun "vkUnmapMemory"
    :void
  (device :pointer)
  (memory :pointer))

(defvkfun "vkCreateImageView"
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (view :pointer))

(defvkfun "vkDestroyImageView"
    :void
  (device :pointer)
  (view :pointer)
  (allocator :pointer))

(defvkfun "vkCreateShaderModule"
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (shader-module :pointer))

(defvkfun "vkDestroyShaderModule"
    :void
  (device :pointer)
  (shader-module :pointer)
  (allocator :pointer))

(defvkfun "vkCreateDescriptorSetLayout"
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (layout :pointer))

(defvkfun "vkDestroyDescriptorSetLayout"
    :void
  (device :pointer)
  (layout :pointer)
  (allocator :pointer))

(defvkfun "vkCreatePipelineLayout"
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (layout :pointer))

(defvkfun "vkDestroyPipelineLayout"
    :void
  (device :pointer)
  (layout :pointer)
  (allocator :pointer))

(defvkfun "vkCreateComputePipelines"
    checked-result
  (device :pointer)
  (pipeline-cache :pointer)
  (create-info-count :uint32)
  (create-infos :pointer)
  (allocator :pointer)
  (pipelines :pointer))

(defvkfun "vkCreateGraphicsPipelines"
    checked-result
  (device :pointer)
  (pipeline-cache :pointer)
  (create-info-count :uint32)
  (create-infos :pointer)
  (allocator :pointer)
  (pipelines :pointer))

(defvkfun "vkDestroyPipeline"
    :void
  (device :pointer)
  (pipeline :pointer)
  (allocator :pointer))

(defvkfun "vkCreateSampler"
    checked-result
  (device :pointer) (create-info :pointer) (allocator :pointer)
  (sampler :pointer))

(defvkfun "vkDestroySampler"
    :void
  (device :pointer) (sampler :pointer) (allocator :pointer))

(defvkfun "vkCreateRenderPass"
    checked-result
  (device :pointer) (create-info :pointer) (allocator :pointer)
  (render-pass :pointer))

(defvkfun "vkDestroyRenderPass"
    :void
  (device :pointer) (render-pass :pointer) (allocator :pointer))

(defvkfun "vkCreateFramebuffer"
    checked-result
  (device :pointer) (create-info :pointer) (allocator :pointer)
  (framebuffer :pointer))

(defvkfun "vkDestroyFramebuffer"
    :void
  (device :pointer) (framebuffer :pointer) (allocator :pointer))

(defvkfun "vkCreateDescriptorPool"
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (pool :pointer))

(defvkfun "vkDestroyDescriptorPool"
    :void
  (device :pointer)
  (pool :pointer)
  (allocator :pointer))

(defvkfun "vkAllocateDescriptorSets"
    checked-result
  (device :pointer)
  (allocate-info :pointer)
  (sets :pointer))

(defvkfun "vkUpdateDescriptorSets"
    :void
  (device :pointer)
  (write-count :uint32)
  (writes :pointer)
  (copy-count :uint32)
  (copies :pointer))

(defvkfun "vkCreateCommandPool"
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (command-pool :pointer))

(defvkfun "vkDestroyCommandPool"
    :void
  (device :pointer)
  (command-pool :pointer)
  (allocator :pointer))

(defvkfun "vkAllocateCommandBuffers"
    checked-result
  (device :pointer)
  (allocate-info :pointer)
  (command-buffers :pointer))

(defvkfun "vkBeginCommandBuffer"
    checked-result
  (command-buffer :pointer)
  (begin-info :pointer))

(defvkfun "vkEndCommandBuffer"
    checked-result
  (command-buffer :pointer))

(defvkfun "vkCmdPipelineBarrier"
    :void
  (command-buffer :pointer)
  (src-stage-mask pipeline-stage-flags)
  (dst-stage-mask pipeline-stage-flags)
  (dependencies dependency-flags)
  (memory-barrier-count :uint32)
  (memory-barriers :pointer)
  (buffer-memory-barrier-count :uint32)
  (buffer-memory-barriers :pointer)
  (image-memory-barrier-count :uint32)
  (image-memory-barriers :pointer))

(defvkfun "vkCmdClearColorImage"
    :void
  (command-buffer :pointer)
  (image :pointer)
  (layout image-layout)
  (color :pointer)
  (range-count :uint32)
  (ranges :pointer))

(defvkfun "vkCmdCopyImage"
    :void
  (command-buffer :pointer)
  (source :pointer)
  (source-layout image-layout)
  (destination :pointer)
  (destination-layout image-layout)
  (region-count :uint32)
  (regions :pointer))

(defvkfun "vkCmdCopyBufferToImage"
    :void
  (command-buffer :pointer)
  (source-buffer :pointer)
  (destination-image :pointer)
  (destination-layout image-layout)
  (region-count :uint32)
  (regions :pointer))

(defvkfun "vkCmdCopyImageToBuffer"
    :void
  (command-buffer :pointer)
  (source-image :pointer)
  (source-layout image-layout)
  (destination-buffer :pointer)
  (region-count :uint32)
  (regions :pointer))

(defvkfun "vkCmdBindPipeline"
    :void
  (command-buffer :pointer)
  (bind-point pipeline-bind-point)
  (pipeline :pointer))

(defvkfun "vkCmdBindDescriptorSets"
    :void
  (command-buffer :pointer)
  (bind-point pipeline-bind-point)
  (layout :pointer)
  (first-set :uint32)
  (set-count :uint32)
  (sets :pointer)
  (dynamic-offset-count :uint32)
  (dynamic-offsets :pointer))

(defvkfun "vkCmdBindVertexBuffers"
    :void
  (command-buffer :pointer)
  (first-binding :uint32)
  (binding-count :uint32)
  (buffers :pointer)
  (offsets :pointer))

(defvkfun "vkCmdDispatch"
    :void
  (command-buffer :pointer)
  (group-count-x :uint32)
  (group-count-y :uint32)
  (group-count-z :uint32))

(defvkfun "vkCmdBeginRenderPass"
    :void
  (command-buffer :pointer) (begin-info :pointer)
  (contents subpass-contents))

(defvkfun "vkCmdEndRenderPass"
    :void
  (command-buffer :pointer))

(defvkfun "vkCmdSetViewport"
    :void
  (command-buffer :pointer) (first-viewport :uint32)
  (viewport-count :uint32) (viewports :pointer))

(defvkfun "vkCmdSetScissor"
    :void
  (command-buffer :pointer) (first-scissor :uint32)
  (scissor-count :uint32) (scissors :pointer))

(defvkfun "vkCmdDraw"
    :void
  (command-buffer :pointer) (vertex-count :uint32)
  (instance-count :uint32) (first-vertex :uint32)
  (first-instance :uint32))

(defvkfun "vkQueueSubmit2"
    checked-result
  (queue :pointer)
  (submit-count :uint32)
  (submits :pointer)
  (fence :pointer))

(defvkfun "vkQueueWaitIdle"
    checked-result
  (queue :pointer))

(defvkfun "vkGetPhysicalDeviceSurfaceSupportKHR"
    checked-result
  (physical-device :pointer)
  (queue-family-index :uint32)
  (surface :pointer)
  (supported :pointer))

(defvkfun "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
    checked-result
  (physical-device :pointer)
  (surface :pointer)
  (capabilities :pointer))

(defvkfun "vkGetPhysicalDeviceSurfaceFormatsKHR"
    checked-result
  (physical-device :pointer)
  (surface :pointer)
  (format-count :pointer)
  (formats :pointer))

(defvkfun "vkGetPhysicalDeviceSurfacePresentModesKHR"
    checked-result
  (physical-device :pointer)
  (surface :pointer)
  (mode-count :pointer)
  (modes :pointer))

(defvkfun "vkCreateSwapchainKHR"
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (swapchain :pointer))

(defvkfun "vkDestroySwapchainKHR"
    :void
  (device :pointer)
  (swapchain :pointer)
  (allocator :pointer))

(defvkfun "vkGetSwapchainImagesKHR"
    checked-result
  (device :pointer)
  (swapchain :pointer)
  (image-count :pointer)
  (images :pointer))

(defvkfun "vkCreateSemaphore"
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (semaphore :pointer))

(defvkfun "vkDestroySemaphore"
    :void
  (device :pointer)
  (semaphore :pointer)
  (allocator :pointer))

(defvkfun "vkGetSemaphoreCounterValue"
    checked-result
  (device :pointer)
  (semaphore :pointer)
  (value :pointer))

(defvkfun "vkWaitSemaphores"
    checked-result
  (device :pointer)
  (wait-info :pointer)
  (timeout :uint64))

(defvkfun "vkAcquireNextImageKHR"
    checked-result
  (device :pointer)
  (swapchain :pointer)
  (timeout :uint64)
  (semaphore :pointer)
  (fence :pointer)
  (image-index :pointer))

(defvkfun "vkQueuePresentKHR"
    checked-result
  (queue :pointer)
  (present-info :pointer))
