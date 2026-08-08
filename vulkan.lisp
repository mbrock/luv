;;;; The Vulkan ABI that luv's WebGPU-shaped backend actually uses.
;;;;
;;;; This is intentionally an ordinary, incomplete CFFI binding.  New Vulkan
;;;; declarations belong here only when a concrete GPU backend operation needs
;;;; them.

(in-package #:luv.vulkan)

(defparameter +portability-enumeration-extension-name+
  "VK_KHR_portability_enumeration")

(defparameter +swapchain-extension-name+ "VK_KHR_swapchain")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library vulkan-loader
    (:darwin (:or "libvulkan.1.dylib" "libvulkan.dylib"))
    (:unix (:or "libvulkan.so.1" "libvulkan.so"))
    (:windows "vulkan-1.dll")))

(cffi:use-foreign-library vulkan-loader)

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
  (:submit-info 4)
  (:memory-allocate-info 5)
  (:buffer-create-info 12)
  (:semaphore-create-info 9)
  (:image-create-info 14)
  (:image-view-create-info 15)
  (:shader-module-create-info 16)
  (:pipeline-shader-stage-create-info 18)
  (:compute-pipeline-create-info 29)
  (:pipeline-layout-create-info 30)
  (:descriptor-set-layout-create-info 32)
  (:descriptor-pool-create-info 33)
  (:descriptor-set-allocate-info 34)
  (:write-descriptor-set 35)
  (:command-pool-create-info 39)
  (:command-buffer-allocate-info 40)
  (:command-buffer-begin-info 42)
  (:image-memory-barrier 45)
  (:swapchain-create-info-khr 1000001000)
  (:present-info-khr 1000001001))

(cffi:defcenum (image-type :uint32)
  (:1d 0)
  (:2d 1)
  (:3d 2))

(cffi:defcenum (format :uint32 :allow-undeclared-values t)
  (:r8g8b8a8-unorm 37)
  (:r8g8b8a8-srgb 43)
  (:b8g8r8a8-unorm 44)
  (:b8g8r8a8-srgb 50))

(cffi:defcenum (image-tiling :uint32)
  (:optimal 0)
  (:linear 1))

(cffi:defcenum (sharing-mode :uint32)
  (:exclusive 0)
  (:concurrent 1))

(cffi:defcenum (image-layout :uint32)
  (:undefined 0)
  (:general 1)
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
  (:storage-image 3))

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

(cffi:defbitfield (queue-flags :uint32)
  (:graphics #x1)
  (:compute #x2)
  (:transfer #x4)
  (:sparse-binding #x8))

(cffi:defbitfield (image-usage-flags :uint32)
  (:transfer-src #x1)
  (:transfer-dst #x2)
  (:storage #x8))

(cffi:defbitfield (buffer-usage-flags :uint32)
  (:transfer-src #x1)
  (:transfer-dst #x2))

(cffi:defbitfield (shader-stage-flags :uint32)
  (:compute #x20))

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

(cffi:defbitfield (command-buffer-usage-flags :uint32)
  (:one-time-submit #x1)
  (:render-pass-continue #x2)
  (:simultaneous-use #x4))

(cffi:defbitfield (image-aspect-flags :uint32)
  (:color #x1)
  (:depth #x2)
  (:stencil #x4))

(cffi:defbitfield (access-flags :uint32)
  (:shader-read #x20)
  (:shader-write #x40)
  (:transfer-read #x800)
  (:transfer-write #x1000))

(cffi:defbitfield (pipeline-stage-flags :uint32)
  (:top-of-pipe #x1)
  (:compute-shader #x800)
  (:transfer #x1000)
  (:bottom-of-pipe #x2000))

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

;;; Conditions and result translation.

(define-condition vulkan-call-error (luv:gpu-error)
  ((result
    :initarg :result
    :reader vulkan-call-error-result))
  (:report
   (lambda (condition stream)
     (format stream "Vulkan call ~S failed with VkResult ~D."
             (luv:gpu-error-operation condition)
             (vulkan-call-error-result condition)))))

(defvar *vulkan-operation* :unknown-vulkan-operation)
(defvar *accepted-results* '(:success))

(cffi:define-foreign-type checked-result-type ()
  ()
  (:actual-type :int32)
  (:simple-parser checked-result))

(defmethod cffi:translate-from-foreign
    (value (type checked-result-type))
  (declare (ignore type))
  (let ((result (cffi:foreign-enum-keyword 'result value :errorp nil)))
    (unless (member result *accepted-results*)
      (error 'vulkan-call-error
             :operation *vulkan-operation*
             :result value))
    result))

(defmacro with-vulkan-results ((operation &rest accepted-results) &body body)
  `(let ((*vulkan-operation* ,operation)
         (*accepted-results* ',(or accepted-results '(:success))))
     ,@body))

;;; Struct declarations remain explicit treaty text.  DEFVKSTRUCT supplies the
;;; standard tagged-struct header and retains the declaration as Lisp data for
;;; increasingly capable fillers later.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *struct-descriptions* (make-hash-table)))

(defmacro defvkstruct (name (&key s-type) &body slots)
  (let ((all-slots
          (append (when s-type
                    '((s-type structure-type)
                      (p-next :pointer)))
                  slots)))
    `(progn
       (cffi:defcstruct ,name ,@all-slots)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *struct-descriptions*)
               ',(list :s-type s-type :slots (mapcar #'first all-slots)))))))

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

(defvkstruct extent-3d ()
  (width :uint32)
  (height :uint32)
  (depth :uint32))

(defvkstruct extent-2d ()
  (width :uint32)
  (height :uint32))

(defvkstruct queue-family-properties ()
  (queue-flags queue-flags)
  (queue-count :uint32)
  (timestamp-valid-bits :uint32)
  (min-image-transfer-granularity (:struct extent-3d)))

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

(defvkstruct pipeline-layout-create-info
    (:s-type :pipeline-layout-create-info)
  (flags :uint32)
  (set-layout-count :uint32)
  (p-set-layouts :pointer)
  (push-constant-range-count :uint32)
  (p-push-constant-ranges :pointer))

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

(defvkstruct write-descriptor-set (:s-type :write-descriptor-set)
  (dst-set :pointer)
  (dst-binding :uint32)
  (dst-array-element :uint32)
  (descriptor-count :uint32)
  (descriptor-type descriptor-type)
  (p-image-info :pointer)
  (p-buffer-info :pointer)
  (p-texel-buffer-view :pointer))

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

(defvkstruct submit-info (:s-type :submit-info)
  (wait-semaphore-count :uint32)
  (p-wait-semaphores :pointer)
  (p-wait-dst-stage-mask :pointer)
  (command-buffer-count :uint32)
  (p-command-buffers :pointer)
  (signal-semaphore-count :uint32)
  (p-signal-semaphores :pointer))

(defvkstruct semaphore-create-info (:s-type :semaphore-create-info)
  (flags :uint32))

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

(defun clear-foreign-object (pointer type &optional (count 1))
  (loop for index below (* count (cffi:foreign-type-size type))
        do (setf (cffi:mem-aref pointer :uint8 index) 0))
  pointer)

(defun fill-vk (pointer type &rest fields)
  (let* ((description
           (or (gethash type *struct-descriptions*)
               (error "Unknown Vulkan struct ~S." type)))
         (foreign-type `(:struct ,type))
         (slots (getf description :slots)))
    (clear-foreign-object pointer foreign-type)
    (let ((s-type (getf description :s-type)))
      (when s-type
        (setf (cffi:foreign-slot-value pointer foreign-type 's-type) s-type
              (cffi:foreign-slot-value pointer foreign-type 'p-next)
              (cffi:null-pointer))))
    (loop for (field value) on fields by #'cddr
          for slot = (find (symbol-name field) slots
                           :key #'symbol-name :test #'string=)
          unless slot
            do (error "~S is not a slot of Vulkan struct ~S." field type)
          do (setf (cffi:foreign-slot-value pointer foreign-type slot) value))
    pointer))

(defmacro with-vk ((variable type &rest fields) &body body)
  `(cffi:with-foreign-object (,variable '(:struct ,type))
     (fill-vk ,variable ',type ,@fields)
     ,@body))

;;; Arguments which own temporary foreign storage.

(cffi:define-foreign-type string-list-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser string-list))

(defmethod cffi:translate-to-foreign
    (strings (type string-list-type))
  (declare (ignore type))
  (if (null strings)
      (values (cffi:null-pointer) nil)
      (let ((pointers nil)
            (array nil))
        (unwind-protect
             (progn
               (dolist (string strings)
                 (push (cffi:foreign-string-alloc string) pointers))
               (setf pointers (nreverse pointers)
                     array (cffi:foreign-alloc
                            :pointer :count (length pointers)))
               (loop for pointer in pointers
                     for index from 0
                     do (setf (cffi:mem-aref array :pointer index)
                              pointer))
               (values array pointers))
          (unless array
            (mapc #'cffi:foreign-string-free pointers))))))

(defmethod cffi:free-translated-object
    (pointer (type string-list-type) strings)
  (declare (ignore type))
  (mapc #'cffi:foreign-string-free strings)
  (unless (cffi:null-pointer-p pointer)
    (cffi:foreign-free pointer)))

(defmacro with-translated-values (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind (variable value type) (first bindings)
        (let ((parameter (gensym "PARAMETER")))
          `(multiple-value-bind (,variable ,parameter)
               (cffi:convert-to-foreign ,value ',type)
             (unwind-protect
                  (with-translated-values ,(rest bindings) ,@body)
               (cffi:free-converted-object
                ,variable ',type ,parameter)))))))

(defmacro with-foreign-array ((pointer type values) &body body)
  (let ((items (gensym "ITEMS"))
        (index (gensym "INDEX"))
        (item (gensym "ITEM")))
    `(let ((,items ,values))
       (if (zerop (length ,items))
           (let ((,pointer (cffi:null-pointer)))
             ,@body)
           (cffi:with-foreign-object (,pointer ',type (length ,items))
             (loop for ,index below (length ,items)
                   for ,item = (elt ,items ,index)
                   do (setf (cffi:mem-aref ,pointer ',type ,index) ,item))
             ,@body)))))

;;; Raw calls.  Their declarations now express translation and checking.

(cffi:defcfun ("vkEnumerateInstanceExtensionProperties"
               %enumerate-instance-extension-properties
               :library vulkan-loader)
    checked-result
  (layer-name :pointer)
  (property-count :pointer)
  (properties :pointer))

(cffi:defcfun ("vkCreateInstance" %create-instance :library vulkan-loader)
    checked-result
  (create-info :pointer)
  (allocator :pointer)
  (instance :pointer))

(cffi:defcfun ("vkDestroyInstance" %destroy-instance :library vulkan-loader)
    :void
  (instance :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkEnumeratePhysicalDevices"
               %enumerate-physical-devices
               :library vulkan-loader)
    checked-result
  (instance :pointer)
  (device-count :pointer)
  (devices :pointer))

(cffi:defcfun ("vkGetPhysicalDeviceQueueFamilyProperties"
               %get-physical-device-queue-family-properties
               :library vulkan-loader)
    :void
  (physical-device :pointer)
  (property-count :pointer)
  (properties :pointer))

(cffi:defcfun ("vkCreateDevice" %create-device :library vulkan-loader)
    checked-result
  (physical-device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (device :pointer))

(cffi:defcfun ("vkDestroyDevice" %destroy-device :library vulkan-loader)
    :void
  (device :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkGetDeviceQueue" %get-device-queue :library vulkan-loader)
    :void
  (device :pointer)
  (queue-family-index :uint32)
  (queue-index :uint32)
  (queue :pointer))

(cffi:defcfun ("vkDeviceWaitIdle" %device-wait-idle :library vulkan-loader)
    checked-result
  (device :pointer))

(cffi:defcfun ("vkGetPhysicalDeviceMemoryProperties"
               %get-physical-device-memory-properties
               :library vulkan-loader)
    :void
  (physical-device :pointer)
  (properties :pointer))

(cffi:defcfun ("vkCreateImage" %create-image :library vulkan-loader)
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (image :pointer))

(cffi:defcfun ("vkDestroyImage" %destroy-image :library vulkan-loader)
    :void
  (device :pointer)
  (image :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkGetImageMemoryRequirements"
               %get-image-memory-requirements
               :library vulkan-loader)
    :void
  (device :pointer)
  (image :pointer)
  (requirements :pointer))

(cffi:defcfun ("vkCreateBuffer" %create-buffer :library vulkan-loader)
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (buffer :pointer))

(cffi:defcfun ("vkDestroyBuffer" %destroy-buffer :library vulkan-loader)
    :void
  (device :pointer)
  (buffer :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkGetBufferMemoryRequirements"
               %get-buffer-memory-requirements
               :library vulkan-loader)
    :void
  (device :pointer)
  (buffer :pointer)
  (requirements :pointer))

(cffi:defcfun ("vkAllocateMemory" %allocate-memory :library vulkan-loader)
    checked-result
  (device :pointer)
  (allocate-info :pointer)
  (allocator :pointer)
  (memory :pointer))

(cffi:defcfun ("vkFreeMemory" %free-memory :library vulkan-loader)
    :void
  (device :pointer)
  (memory :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkBindImageMemory" %bind-image-memory :library vulkan-loader)
    checked-result
  (device :pointer)
  (image :pointer)
  (memory :pointer)
  (offset :uint64))

(cffi:defcfun ("vkBindBufferMemory" %bind-buffer-memory :library vulkan-loader)
    checked-result
  (device :pointer)
  (buffer :pointer)
  (memory :pointer)
  (offset :uint64))

(cffi:defcfun ("vkMapMemory" %map-memory :library vulkan-loader)
    checked-result
  (device :pointer)
  (memory :pointer)
  (offset :uint64)
  (size :uint64)
  (flags :uint32)
  (data :pointer))

(cffi:defcfun ("vkUnmapMemory" %unmap-memory :library vulkan-loader)
    :void
  (device :pointer)
  (memory :pointer))

(cffi:defcfun ("vkCreateImageView" %create-image-view
               :library vulkan-loader)
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (view :pointer))

(cffi:defcfun ("vkDestroyImageView" %destroy-image-view
               :library vulkan-loader)
    :void
  (device :pointer)
  (view :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkCreateShaderModule" %create-shader-module
               :library vulkan-loader)
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (shader-module :pointer))

(cffi:defcfun ("vkDestroyShaderModule" %destroy-shader-module
               :library vulkan-loader)
    :void
  (device :pointer)
  (shader-module :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkCreateDescriptorSetLayout" %create-descriptor-set-layout
               :library vulkan-loader)
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (layout :pointer))

(cffi:defcfun ("vkDestroyDescriptorSetLayout" %destroy-descriptor-set-layout
               :library vulkan-loader)
    :void
  (device :pointer)
  (layout :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkCreatePipelineLayout" %create-pipeline-layout
               :library vulkan-loader)
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (layout :pointer))

(cffi:defcfun ("vkDestroyPipelineLayout" %destroy-pipeline-layout
               :library vulkan-loader)
    :void
  (device :pointer)
  (layout :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkCreateComputePipelines" %create-compute-pipelines
               :library vulkan-loader)
    checked-result
  (device :pointer)
  (pipeline-cache :pointer)
  (create-info-count :uint32)
  (create-infos :pointer)
  (allocator :pointer)
  (pipelines :pointer))

(cffi:defcfun ("vkDestroyPipeline" %destroy-pipeline
               :library vulkan-loader)
    :void
  (device :pointer)
  (pipeline :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkCreateDescriptorPool" %create-descriptor-pool
               :library vulkan-loader)
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (pool :pointer))

(cffi:defcfun ("vkDestroyDescriptorPool" %destroy-descriptor-pool
               :library vulkan-loader)
    :void
  (device :pointer)
  (pool :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkAllocateDescriptorSets" %allocate-descriptor-sets
               :library vulkan-loader)
    checked-result
  (device :pointer)
  (allocate-info :pointer)
  (sets :pointer))

(cffi:defcfun ("vkUpdateDescriptorSets" %update-descriptor-sets
               :library vulkan-loader)
    :void
  (device :pointer)
  (write-count :uint32)
  (writes :pointer)
  (copy-count :uint32)
  (copies :pointer))

(cffi:defcfun ("vkCreateCommandPool" %create-command-pool
               :library vulkan-loader)
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (command-pool :pointer))

(cffi:defcfun ("vkDestroyCommandPool" %destroy-command-pool
               :library vulkan-loader)
    :void
  (device :pointer)
  (command-pool :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkAllocateCommandBuffers" %allocate-command-buffers
               :library vulkan-loader)
    checked-result
  (device :pointer)
  (allocate-info :pointer)
  (command-buffers :pointer))

(cffi:defcfun ("vkBeginCommandBuffer" %begin-command-buffer
               :library vulkan-loader)
    checked-result
  (command-buffer :pointer)
  (begin-info :pointer))

(cffi:defcfun ("vkEndCommandBuffer" %end-command-buffer
               :library vulkan-loader)
    checked-result
  (command-buffer :pointer))

(cffi:defcfun ("vkCmdPipelineBarrier" %cmd-pipeline-barrier
               :library vulkan-loader)
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

(cffi:defcfun ("vkCmdClearColorImage" %cmd-clear-color-image
               :library vulkan-loader)
    :void
  (command-buffer :pointer)
  (image :pointer)
  (layout image-layout)
  (color :pointer)
  (range-count :uint32)
  (ranges :pointer))

(cffi:defcfun ("vkCmdCopyImage" %cmd-copy-image :library vulkan-loader)
    :void
  (command-buffer :pointer)
  (source :pointer)
  (source-layout image-layout)
  (destination :pointer)
  (destination-layout image-layout)
  (region-count :uint32)
  (regions :pointer))

(cffi:defcfun ("vkCmdCopyBufferToImage" %cmd-copy-buffer-to-image
               :library vulkan-loader)
    :void
  (command-buffer :pointer)
  (source-buffer :pointer)
  (destination-image :pointer)
  (destination-layout image-layout)
  (region-count :uint32)
  (regions :pointer))

(cffi:defcfun ("vkCmdBindPipeline" %cmd-bind-pipeline
               :library vulkan-loader)
    :void
  (command-buffer :pointer)
  (bind-point pipeline-bind-point)
  (pipeline :pointer))

(cffi:defcfun ("vkCmdBindDescriptorSets" %cmd-bind-descriptor-sets
               :library vulkan-loader)
    :void
  (command-buffer :pointer)
  (bind-point pipeline-bind-point)
  (layout :pointer)
  (first-set :uint32)
  (set-count :uint32)
  (sets :pointer)
  (dynamic-offset-count :uint32)
  (dynamic-offsets :pointer))

(cffi:defcfun ("vkCmdDispatch" %cmd-dispatch :library vulkan-loader)
    :void
  (command-buffer :pointer)
  (group-count-x :uint32)
  (group-count-y :uint32)
  (group-count-z :uint32))

(cffi:defcfun ("vkQueueSubmit" %queue-submit :library vulkan-loader)
    checked-result
  (queue :pointer)
  (submit-count :uint32)
  (submits :pointer)
  (fence :pointer))

(cffi:defcfun ("vkQueueWaitIdle" %queue-wait-idle :library vulkan-loader)
    checked-result
  (queue :pointer))

(cffi:defcfun ("vkGetPhysicalDeviceSurfaceSupportKHR"
               %get-physical-device-surface-support
               :library vulkan-loader)
    checked-result
  (physical-device :pointer)
  (queue-family-index :uint32)
  (surface :pointer)
  (supported :pointer))

(cffi:defcfun ("vkGetPhysicalDeviceSurfaceCapabilitiesKHR"
               %get-physical-device-surface-capabilities
               :library vulkan-loader)
    checked-result
  (physical-device :pointer)
  (surface :pointer)
  (capabilities :pointer))

(cffi:defcfun ("vkGetPhysicalDeviceSurfaceFormatsKHR"
               %get-physical-device-surface-formats
               :library vulkan-loader)
    checked-result
  (physical-device :pointer)
  (surface :pointer)
  (format-count :pointer)
  (formats :pointer))

(cffi:defcfun ("vkGetPhysicalDeviceSurfacePresentModesKHR"
               %get-physical-device-surface-present-modes
               :library vulkan-loader)
    checked-result
  (physical-device :pointer)
  (surface :pointer)
  (mode-count :pointer)
  (modes :pointer))

(cffi:defcfun ("vkCreateSwapchainKHR" %create-swapchain
               :library vulkan-loader)
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (swapchain :pointer))

(cffi:defcfun ("vkDestroySwapchainKHR" %destroy-swapchain
               :library vulkan-loader)
    :void
  (device :pointer)
  (swapchain :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkGetSwapchainImagesKHR" %get-swapchain-images
               :library vulkan-loader)
    checked-result
  (device :pointer)
  (swapchain :pointer)
  (image-count :pointer)
  (images :pointer))

(cffi:defcfun ("vkCreateSemaphore" %create-semaphore
               :library vulkan-loader)
    checked-result
  (device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (semaphore :pointer))

(cffi:defcfun ("vkDestroySemaphore" %destroy-semaphore
               :library vulkan-loader)
    :void
  (device :pointer)
  (semaphore :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkAcquireNextImageKHR" %acquire-next-image
               :library vulkan-loader)
    checked-result
  (device :pointer)
  (swapchain :pointer)
  (timeout :uint64)
  (semaphore :pointer)
  (fence :pointer)
  (image-index :pointer))

(cffi:defcfun ("vkQueuePresentKHR" %queue-present
               :library vulkan-loader)
    checked-result
  (queue :pointer)
  (present-info :pointer))

;;; The three ordinary Vulkan call shapes used so far.

(defmacro define-enumerator
    (name lambda-list call &key element extractor
                                 (operation (intern (symbol-name name) :keyword)))
  (let ((count (gensym "COUNT"))
        (capacity (gensym "CAPACITY"))
        (items (gensym "ITEMS"))
        (result (gensym "RESULT"))
        (index (gensym "INDEX")))
    `(defun ,name ,lambda-list
       (cffi:with-foreign-object (,count :uint32)
         (setf (cffi:mem-ref ,count :uint32) 0)
         (with-vulkan-results (,operation :success :incomplete)
           (,(first call) ,@(rest call) ,count (cffi:null-pointer)))
         (loop
           for ,capacity = (cffi:mem-ref ,count :uint32)
           when (zerop ,capacity) return nil
           do (cffi:with-foreign-object (,items ',element ,capacity)
                (clear-foreign-object ,items ',element ,capacity)
                (let ((,result
                        (with-vulkan-results
                            (,operation :success :incomplete)
                          (,(first call) ,@(rest call) ,count ,items))))
                  (unless (eq ,result :incomplete)
                    (return-from ,name
                      (loop for ,index below (cffi:mem-ref ,count :uint32)
                            collect
                            ,(if extractor
                                 `(,extractor ,items ,index)
                                 `(cffi:mem-aref
                                   ,items ',element ,index))))))))))))

(defmacro define-creator
    (name lambda-list call &key (element :pointer) checked
                                (operation (intern (symbol-name name) :keyword)))
  (let ((output (gensym "OUTPUT")))
    `(defun ,name ,lambda-list
       (cffi:with-foreign-object (,output ',element)
         ,(if checked
              `(with-vulkan-results (,operation)
                 (,(first call) ,@(rest call) ,output))
              `(,(first call) ,@(rest call) ,output))
         (cffi:mem-ref ,output ',element)))))

(defun extension-property-name (properties index)
  (let ((property
          (cffi:mem-aptr
           properties '(:struct extension-properties) index)))
    (cffi:foreign-string-to-lisp
     (cffi:foreign-slot-pointer
      property '(:struct extension-properties) 'extension-name))))

(define-enumerator enumerate-instance-extension-names ()
  (%enumerate-instance-extension-properties (cffi:null-pointer))
  :element (:struct extension-properties)
  :extractor extension-property-name
  :operation :enumerate-instance-extension-properties)

(define-enumerator enumerate-physical-devices (instance)
  (%enumerate-physical-devices instance)
  :element :pointer)

(define-creator create-instance-handle (create-info)
  (%create-instance create-info (cffi:null-pointer))
  :checked t
  :operation :create-instance)

(define-creator create-device-handle (physical-device create-info)
  (%create-device physical-device create-info (cffi:null-pointer))
  :checked t
  :operation :create-device)

(define-creator get-device-queue-handle
    (device queue-family-index queue-index)
  (%get-device-queue device queue-family-index queue-index))

(define-creator create-image-handle (device create-info)
  (%create-image device create-info (cffi:null-pointer))
  :checked t
  :operation :create-image)

(define-creator create-buffer-handle (device create-info)
  (%create-buffer device create-info (cffi:null-pointer))
  :checked t
  :operation :create-buffer)

(define-creator allocate-memory-handle (device allocate-info)
  (%allocate-memory device allocate-info (cffi:null-pointer))
  :checked t
  :operation :allocate-memory)

(define-creator create-image-view-handle (device create-info)
  (%create-image-view device create-info (cffi:null-pointer))
  :checked t
  :operation :create-image-view)

(define-creator create-shader-module-handle (device create-info)
  (%create-shader-module device create-info (cffi:null-pointer))
  :checked t
  :operation :create-shader-module)

(define-creator create-descriptor-set-layout-handle (device create-info)
  (%create-descriptor-set-layout device create-info (cffi:null-pointer))
  :checked t
  :operation :create-descriptor-set-layout)

(define-creator create-pipeline-layout-handle (device create-info)
  (%create-pipeline-layout device create-info (cffi:null-pointer))
  :checked t
  :operation :create-pipeline-layout)

(define-creator create-compute-pipeline-handle (device create-info)
  (%create-compute-pipelines device (cffi:null-pointer) 1 create-info
                             (cffi:null-pointer))
  :checked t
  :operation :create-compute-pipeline)

(define-creator create-descriptor-pool-handle (device create-info)
  (%create-descriptor-pool device create-info (cffi:null-pointer))
  :checked t
  :operation :create-descriptor-pool)

(define-creator allocate-descriptor-set-handle (device allocate-info)
  (%allocate-descriptor-sets device allocate-info)
  :checked t
  :operation :allocate-descriptor-set)

(define-creator create-command-pool-handle (device create-info)
  (%create-command-pool device create-info (cffi:null-pointer))
  :checked t
  :operation :create-command-pool)

(define-creator allocate-command-buffer-handle (device allocate-info)
  (%allocate-command-buffers device allocate-info)
  :checked t
  :operation :allocate-command-buffer)

(define-creator create-swapchain-handle (device create-info)
  (%create-swapchain device create-info (cffi:null-pointer))
  :checked t
  :operation :create-swapchain)

(define-creator create-semaphore-handle (device create-info)
  (%create-semaphore device create-info (cffi:null-pointer))
  :checked t
  :operation :create-semaphore)

;;; Public, Lisp-shaped operations.

(defstruct queue-family
  (flags nil :type list)
  (count 0 :type (unsigned-byte 32)))

(defstruct physical-memory-type
  (flags nil :type list)
  (heap-index 0 :type (unsigned-byte 32)))

(defstruct image-memory-requirements
  (size 0 :type (unsigned-byte 64))
  (alignment 0 :type (unsigned-byte 64))
  (memory-type-bits 0 :type (unsigned-byte 32)))

(defstruct buffer-memory-requirements
  (size 0 :type (unsigned-byte 64))
  (alignment 0 :type (unsigned-byte 64))
  (memory-type-bits 0 :type (unsigned-byte 32)))

(defstruct presentation-capabilities
  (min-image-count 0 :type (unsigned-byte 32))
  (max-image-count 0 :type (unsigned-byte 32))
  current-extent
  min-image-extent
  max-image-extent
  current-transform
  (composite-alpha nil :type list)
  (usage nil :type list))

(defstruct presentation-format
  format
  color-space)

(defun make-version (major minor patch)
  (logior (ash major 22) (ash minor 12) patch))

(defun create-instance
    (&key
       (application-name "luv")
       ((:application-version application-version-value)
        (make-version 0 0 1))
       (engine-name "luv")
       ((:engine-version engine-version-value) (make-version 0 0 1))
       ((:api-version api-version-value) (make-version 1 0 0))
       flags
       enabled-extension-names)
  (with-translated-values
      ((application-name-pointer application-name :string)
       (engine-name-pointer engine-name :string)
       (extension-names enabled-extension-names string-list))
    (with-vk (application-info application-info
              :p-application-name application-name-pointer
              :application-version application-version-value
              :p-engine-name engine-name-pointer
              :engine-version engine-version-value
              :api-version api-version-value)
      (with-vk (create-info instance-create-info
                :flags flags
                :p-application-info application-info
                :enabled-layer-count 0
                :pp-enabled-layer-names (cffi:null-pointer)
                :enabled-extension-count (length enabled-extension-names)
                :pp-enabled-extension-names extension-names)
        (create-instance-handle create-info)))))

(defun destroy-instance (instance)
  (%destroy-instance instance (cffi:null-pointer))
  (values))

(defun physical-device-queue-families (physical-device)
  (cffi:with-foreign-object (count :uint32)
    (setf (cffi:mem-ref count :uint32) 0)
    (%get-physical-device-queue-family-properties
     physical-device count (cffi:null-pointer))
    (let ((capacity (cffi:mem-ref count :uint32)))
      (if (zerop capacity)
          nil
          (cffi:with-foreign-object
              (properties '(:struct queue-family-properties) capacity)
            (clear-foreign-object
             properties '(:struct queue-family-properties) capacity)
            (%get-physical-device-queue-family-properties
             physical-device count properties)
            (loop for index below
                    (min capacity (cffi:mem-ref count :uint32))
                  for property =
                    (cffi:mem-aptr
                     properties '(:struct queue-family-properties) index)
                  collect
                  (cffi:with-foreign-slots
                      ((queue-flags queue-count)
                       property
                       (:struct queue-family-properties))
                    (make-queue-family
                     :flags queue-flags
                     :count queue-count))))))))

(defun create-device
    (physical-device family-index &key enabled-extension-names)
  (with-translated-values
      ((extension-names enabled-extension-names string-list))
    (cffi:with-foreign-object (queue-priority :float)
      (setf (cffi:mem-ref queue-priority :float) 1.0)
      (with-vk (queue-info device-queue-create-info
                :flags 0
                :queue-family-index family-index
                :queue-count 1
                :p-queue-priorities queue-priority)
        (with-vk (create-info device-create-info
                  :flags 0
                  :queue-create-info-count 1
                  :p-queue-create-infos queue-info
                  :enabled-layer-count 0
                  :pp-enabled-layer-names (cffi:null-pointer)
                  :enabled-extension-count (length enabled-extension-names)
                  :pp-enabled-extension-names extension-names
                  :p-enabled-features (cffi:null-pointer))
          (create-device-handle physical-device create-info))))))

(defun destroy-device (device)
  (%destroy-device device (cffi:null-pointer))
  (values))

(defun get-device-queue (device queue-family-index &optional (queue-index 0))
  (get-device-queue-handle device queue-family-index queue-index))

(defun device-wait-idle (device)
  (with-vulkan-results (:device-wait-idle)
    (%device-wait-idle device))
  (values))

(defun physical-device-memory-types (physical-device)
  (cffi:with-foreign-object
      (properties '(:struct physical-device-memory-properties))
    (clear-foreign-object properties
                          '(:struct physical-device-memory-properties))
    (%get-physical-device-memory-properties physical-device properties)
    (let ((count
            (cffi:foreign-slot-value
             properties '(:struct physical-device-memory-properties)
             'memory-type-count))
          (types
            (cffi:foreign-slot-pointer
             properties '(:struct physical-device-memory-properties)
             'memory-types)))
      (loop for index below count
            for memory-type =
              (cffi:mem-aptr types '(:struct memory-type) index)
            collect
            (cffi:with-foreign-slots
                ((property-flags heap-index)
                 memory-type (:struct memory-type))
              (make-physical-memory-type
               :flags property-flags
               :heap-index heap-index))))))

(defun create-image
    (device &key type format width height (depth 1) usage
                 (mip-levels 1) (array-layers 1) (samples :1)
                 (tiling :optimal) (sharing-mode :exclusive)
                 (initial-layout :undefined))
  (with-vk (create-info image-create-info
            :flags 0
            :image-type type
            :format format
            :mip-levels mip-levels
            :array-layers array-layers
            :samples samples
            :tiling tiling
            :usage usage
            :sharing-mode sharing-mode
            :queue-family-index-count 0
            :p-queue-family-indices (cffi:null-pointer)
            :initial-layout initial-layout)
    (fill-vk
     (cffi:foreign-slot-pointer
      create-info '(:struct image-create-info) 'extent)
     'extent-3d
     :width width :height height :depth depth)
    (create-image-handle device create-info)))

(defun destroy-image (device image)
  (%destroy-image device image (cffi:null-pointer))
  (values))

(defun get-image-memory-requirements (device image)
  (cffi:with-foreign-object (requirements '(:struct memory-requirements))
    (clear-foreign-object requirements '(:struct memory-requirements))
    (%get-image-memory-requirements device image requirements)
    (cffi:with-foreign-slots
        ((size alignment memory-type-bits)
         requirements (:struct memory-requirements))
      (make-image-memory-requirements
       :size size
       :alignment alignment
       :memory-type-bits memory-type-bits))))

(defun create-buffer (device size usage)
  (with-vk (create-info buffer-create-info
            :flags 0
            :size size
            :usage usage
            :sharing-mode :exclusive
            :queue-family-index-count 0
            :p-queue-family-indices (cffi:null-pointer))
    (create-buffer-handle device create-info)))

(defun destroy-buffer (device buffer)
  (%destroy-buffer device buffer (cffi:null-pointer))
  (values))

(defun get-buffer-memory-requirements (device buffer)
  (cffi:with-foreign-object (requirements '(:struct memory-requirements))
    (clear-foreign-object requirements '(:struct memory-requirements))
    (%get-buffer-memory-requirements device buffer requirements)
    (cffi:with-foreign-slots
        ((size alignment memory-type-bits)
         requirements (:struct memory-requirements))
      (make-buffer-memory-requirements
       :size size
       :alignment alignment
       :memory-type-bits memory-type-bits))))

(defun allocate-memory (device size memory-type-index)
  (with-vk (allocate-info memory-allocate-info
            :allocation-size size
            :memory-type-index memory-type-index)
    (allocate-memory-handle device allocate-info)))

(defun free-memory (device memory)
  (%free-memory device memory (cffi:null-pointer))
  (values))

(defun bind-image-memory (device image memory &optional (offset 0))
  (with-vulkan-results (:bind-image-memory)
    (%bind-image-memory device image memory offset))
  (values))

(defun bind-buffer-memory (device buffer memory &optional (offset 0))
  (with-vulkan-results (:bind-buffer-memory)
    (%bind-buffer-memory device buffer memory offset))
  (values))

(defun map-memory (device memory size &optional (offset 0))
  (cffi:with-foreign-object (data :pointer)
    (with-vulkan-results (:map-memory)
      (%map-memory device memory offset size 0 data))
    (cffi:mem-ref data :pointer)))

(defun unmap-memory (device memory)
  (%unmap-memory device memory)
  (values))

(defun create-image-view (device image format &key (view-type :2d))
  (with-vk (create-info image-view-create-info
            :flags 0
            :image image
            :view-type view-type
            :format format)
    (fill-vk
     (cffi:foreign-slot-pointer
      create-info '(:struct image-view-create-info) 'components)
     'component-mapping
     :r :identity :g :identity :b :identity :a :identity)
    (fill-color-subresource-range
     (cffi:foreign-slot-pointer
      create-info '(:struct image-view-create-info) 'subresource-range))
    (create-image-view-handle device create-info)))

(defun destroy-image-view (device view)
  (%destroy-image-view device view (cffi:null-pointer))
  (values))

(defun create-shader-module (device words)
  (with-foreign-array (code :uint32 words)
    (with-vk (create-info shader-module-create-info
              :flags 0
              :code-size (* 4 (length words))
              :p-code code)
      (create-shader-module-handle device create-info))))

(defun destroy-shader-module (device shader-module)
  (%destroy-shader-module device shader-module (cffi:null-pointer))
  (values))

(defun create-storage-image-descriptor-set-layout (device &key (binding 0))
  (with-vk (layout-binding descriptor-set-layout-binding
            :binding binding
            :descriptor-type :storage-image
            :descriptor-count 1
            :stage-flags '(:compute)
            :p-immutable-samplers (cffi:null-pointer))
    (with-vk (create-info descriptor-set-layout-create-info
              :flags 0
              :binding-count 1
              :p-bindings layout-binding)
      (create-descriptor-set-layout-handle device create-info))))

(defun destroy-descriptor-set-layout (device layout)
  (%destroy-descriptor-set-layout device layout (cffi:null-pointer))
  (values))

(defun create-pipeline-layout (device set-layouts)
  (with-foreign-array (layouts :pointer set-layouts)
    (with-vk (create-info pipeline-layout-create-info
              :flags 0
              :set-layout-count (length set-layouts)
              :p-set-layouts layouts
              :push-constant-range-count 0
              :p-push-constant-ranges (cffi:null-pointer))
      (create-pipeline-layout-handle device create-info))))

(defun destroy-pipeline-layout (device layout)
  (%destroy-pipeline-layout device layout (cffi:null-pointer))
  (values))

(defun create-compute-pipeline
    (device shader-module layout &key (entry-point "main"))
  (cffi:with-foreign-string (entry-point-pointer entry-point)
    (with-vk (create-info compute-pipeline-create-info
              :flags 0
              :layout layout
              :base-pipeline-handle (cffi:null-pointer)
              :base-pipeline-index -1)
      (fill-vk
       (cffi:foreign-slot-pointer
        create-info '(:struct compute-pipeline-create-info) 'stage)
       'pipeline-shader-stage-create-info
       :flags 0
       :stage '(:compute)
       :module shader-module
       :p-name entry-point-pointer
       :p-specialization-info (cffi:null-pointer))
      (create-compute-pipeline-handle device create-info))))

(defun destroy-pipeline (device pipeline)
  (%destroy-pipeline device pipeline (cffi:null-pointer))
  (values))

(defun create-storage-image-descriptor-pool (device &key (max-sets 1))
  (with-vk (pool-size descriptor-pool-size
            :type :storage-image
            :descriptor-count max-sets)
    (with-vk (create-info descriptor-pool-create-info
              :flags 0
              :max-sets max-sets
              :pool-size-count 1
              :p-pool-sizes pool-size)
      (create-descriptor-pool-handle device create-info))))

(defun destroy-descriptor-pool (device pool)
  (%destroy-descriptor-pool device pool (cffi:null-pointer))
  (values))

(defun allocate-descriptor-set (device pool layout)
  (with-foreign-array (layouts :pointer (vector layout))
    (with-vk (allocate-info descriptor-set-allocate-info
              :descriptor-pool pool
              :descriptor-set-count 1
              :p-set-layouts layouts)
      (allocate-descriptor-set-handle device allocate-info))))

(defun update-storage-image-descriptor
    (device descriptor-set image-view &key (binding 0))
  (with-vk (image-info descriptor-image-info
            :sampler (cffi:null-pointer)
            :image-view image-view
            :image-layout :general)
    (with-vk (write write-descriptor-set
              :dst-set descriptor-set
              :dst-binding binding
              :dst-array-element 0
              :descriptor-count 1
              :descriptor-type :storage-image
              :p-image-info image-info
              :p-buffer-info (cffi:null-pointer)
              :p-texel-buffer-view (cffi:null-pointer))
      (%update-descriptor-sets
       device 1 write 0 (cffi:null-pointer))))
  (values))

(defun create-command-pool (device queue-family-index &key flags)
  (with-vk (create-info command-pool-create-info
            :flags flags
            :queue-family-index queue-family-index)
    (create-command-pool-handle device create-info)))

(defun destroy-command-pool (device command-pool)
  (%destroy-command-pool device command-pool (cffi:null-pointer))
  (values))

(defun allocate-command-buffer
    (device command-pool &key (level :primary))
  (with-vk (allocate-info command-buffer-allocate-info
            :command-pool command-pool
            :level level
            :command-buffer-count 1)
    (allocate-command-buffer-handle device allocate-info)))

(defun begin-command-buffer (command-buffer &key flags)
  (with-vk (begin-info command-buffer-begin-info
            :flags flags
            :p-inheritance-info (cffi:null-pointer))
    (with-vulkan-results (:begin-command-buffer)
      (%begin-command-buffer command-buffer begin-info)))
  command-buffer)

(defun end-command-buffer (command-buffer)
  (with-vulkan-results (:end-command-buffer)
    (%end-command-buffer command-buffer))
  command-buffer)

(defun fill-color-subresource-range (range)
  (fill-vk range 'image-subresource-range
           :aspect-mask '(:color)
           :base-mip-level 0
           :level-count 1
           :base-array-layer 0
           :layer-count 1))

(defun cmd-transition-image
    (command-buffer image old-layout new-layout
     src-access dst-access src-stage dst-stage)
  (with-vk (barrier image-memory-barrier
            :src-access-mask src-access
            :dst-access-mask dst-access
            :old-layout old-layout
            :new-layout new-layout
            :src-queue-family-index +queue-family-ignored+
            :dst-queue-family-index +queue-family-ignored+
            :image image)
    (fill-color-subresource-range
     (cffi:foreign-slot-pointer
      barrier '(:struct image-memory-barrier) 'subresource-range))
    (%cmd-pipeline-barrier
     command-buffer src-stage dst-stage nil
     0 (cffi:null-pointer)
     0 (cffi:null-pointer)
     1 barrier))
  (values))

(defun cmd-clear-color-image (command-buffer image layout color)
  (with-vk (range image-subresource-range)
    (fill-color-subresource-range range)
    (cffi:with-foreign-object (foreign-color '(:union clear-color-value))
      (clear-foreign-object foreign-color '(:union clear-color-value))
      (let ((components
              (cffi:foreign-slot-pointer
               foreign-color '(:union clear-color-value) 'float-32)))
        (loop for component across color
              for index from 0 below 4
              do (setf (cffi:mem-aref components :float index) component)))
      (%cmd-clear-color-image
       command-buffer image layout foreign-color 1 range)))
  (values))

(defun fill-color-subresource-layers (layers)
  (fill-vk layers 'image-subresource-layers
           :aspect-mask '(:color)
           :mip-level 0
           :base-array-layer 0
           :layer-count 1))

(defun cmd-copy-image
    (command-buffer source source-layout destination destination-layout
     width height &optional (depth 1))
  (with-vk (region image-copy)
    (dolist (slot '(src-subresource dst-subresource))
      (fill-color-subresource-layers
       (cffi:foreign-slot-pointer region '(:struct image-copy) slot)))
    (dolist (slot '(src-offset dst-offset))
      (fill-vk
       (cffi:foreign-slot-pointer region '(:struct image-copy) slot)
       'offset-3d :x 0 :y 0 :z 0))
    (fill-vk
     (cffi:foreign-slot-pointer region '(:struct image-copy) 'extent)
     'extent-3d :width width :height height :depth depth)
    (%cmd-copy-image
     command-buffer source source-layout destination destination-layout
     1 region))
  (values))

(defun cmd-copy-buffer-to-image
    (command-buffer buffer image layout width height
     &key (buffer-offset 0) (buffer-row-length 0)
          (buffer-image-height 0) (x 0) (y 0) (depth 1))
  (with-vk (region buffer-image-copy
            :buffer-offset buffer-offset
            :buffer-row-length buffer-row-length
            :buffer-image-height buffer-image-height)
    (fill-color-subresource-layers
     (cffi:foreign-slot-pointer
      region '(:struct buffer-image-copy) 'image-subresource))
    (fill-vk
     (cffi:foreign-slot-pointer
      region '(:struct buffer-image-copy) 'image-offset)
     'offset-3d :x x :y y :z 0)
    (fill-vk
     (cffi:foreign-slot-pointer
      region '(:struct buffer-image-copy) 'image-extent)
     'extent-3d :width width :height height :depth depth)
    (%cmd-copy-buffer-to-image
     command-buffer buffer image layout 1 region))
  (values))

(defun cmd-bind-compute-pipeline (command-buffer pipeline)
  (%cmd-bind-pipeline command-buffer :compute pipeline)
  (values))

(defun cmd-bind-compute-descriptor-set
    (command-buffer pipeline-layout descriptor-set)
  (with-foreign-array (sets :pointer (vector descriptor-set))
    (%cmd-bind-descriptor-sets
     command-buffer :compute pipeline-layout 0 1 sets
     0 (cffi:null-pointer)))
  (values))

(defun cmd-dispatch (command-buffer x y &optional (z 1))
  (%cmd-dispatch command-buffer x y z)
  (values))

(defun submit-command-buffers
    (queue buffers &key (wait-semaphores #()) (wait-stages #())
                        (signal-semaphores #()))
  (unless (= (length wait-semaphores) (length wait-stages))
    (error "Each wait semaphore needs one destination stage."))
  (with-foreign-array (command-buffers :pointer buffers)
    (with-foreign-array (waits :pointer wait-semaphores)
      (with-foreign-array (stages pipeline-stage-flags wait-stages)
        (with-foreign-array (signals :pointer signal-semaphores)
          (with-vk (submit submit-info
                    :wait-semaphore-count (length wait-semaphores)
                    :p-wait-semaphores waits
                    :p-wait-dst-stage-mask stages
                    :command-buffer-count (length buffers)
                    :p-command-buffers command-buffers
                    :signal-semaphore-count (length signal-semaphores)
                    :p-signal-semaphores signals)
            (with-vulkan-results (:queue-submit)
              (%queue-submit queue 1 submit (cffi:null-pointer))))))))
  (values))

(defun submit-command-buffer (queue command-buffer)
  (submit-command-buffers queue (vector command-buffer)))

(defun queue-wait-idle (queue)
  (with-vulkan-results (:queue-wait-idle)
    (%queue-wait-idle queue))
  (values))

(defun surface-supported-p (physical-device queue-family-index surface)
  (cffi:with-foreign-object (supported :uint32)
    (setf (cffi:mem-ref supported :uint32) 0)
    (with-vulkan-results (:get-physical-device-surface-support)
      (%get-physical-device-surface-support
       physical-device queue-family-index surface supported))
    (not (zerop (cffi:mem-ref supported :uint32)))))

(defun read-extent-2d (pointer)
  (cffi:with-foreign-slots
      ((width height) pointer (:struct extent-2d))
    (list width height)))

(defun get-surface-capabilities (physical-device surface)
  (cffi:with-foreign-object (capabilities '(:struct surface-capabilities))
    (clear-foreign-object capabilities '(:struct surface-capabilities))
    (with-vulkan-results (:get-physical-device-surface-capabilities)
      (%get-physical-device-surface-capabilities
       physical-device surface capabilities))
    (cffi:with-foreign-slots
        ((min-image-count max-image-count current-transform
          supported-composite-alpha supported-usage-flags)
         capabilities (:struct surface-capabilities))
      (make-presentation-capabilities
       :min-image-count min-image-count
       :max-image-count max-image-count
       :current-extent
       (read-extent-2d
        (cffi:foreign-slot-pointer
         capabilities '(:struct surface-capabilities) 'current-extent))
       :min-image-extent
       (read-extent-2d
        (cffi:foreign-slot-pointer
         capabilities '(:struct surface-capabilities) 'min-image-extent))
       :max-image-extent
       (read-extent-2d
        (cffi:foreign-slot-pointer
         capabilities '(:struct surface-capabilities) 'max-image-extent))
       :current-transform current-transform
       :composite-alpha supported-composite-alpha
       :usage supported-usage-flags))))

(defun extract-surface-format (formats index)
  (let ((format
          (cffi:mem-aptr formats '(:struct surface-format) index)))
    (cffi:with-foreign-slots
        ((format color-space) format (:struct surface-format))
      (make-presentation-format :format format :color-space color-space))))

(define-enumerator get-surface-formats (physical-device surface)
  (%get-physical-device-surface-formats physical-device surface)
  :element (:struct surface-format)
  :extractor extract-surface-format)

(define-enumerator get-surface-present-modes (physical-device surface)
  (%get-physical-device-surface-present-modes physical-device surface)
  :element present-mode)

(define-enumerator get-swapchain-images (device swapchain)
  (%get-swapchain-images device swapchain)
  :element :pointer)

(defun create-swapchain
    (device surface format color-space extent
     &key min-image-count (usage '(:transfer-dst))
       (pre-transform :identity) (composite-alpha :opaque)
       (present-mode :fifo-khr) old-swapchain)
  (with-vk (create-info swapchain-create-info
            :flags 0
            :surface surface
            :min-image-count min-image-count
            :image-format format
            :image-color-space color-space
            :image-array-layers 1
            :image-usage usage
            :image-sharing-mode :exclusive
            :queue-family-index-count 0
            :p-queue-family-indices (cffi:null-pointer)
            :pre-transform pre-transform
            :composite-alpha composite-alpha
            :present-mode present-mode
            :clipped 1
            :old-swapchain (or old-swapchain (cffi:null-pointer)))
    (fill-vk
     (cffi:foreign-slot-pointer
      create-info '(:struct swapchain-create-info) 'image-extent)
     'extent-2d :width (first extent) :height (second extent))
    (create-swapchain-handle device create-info)))

(defun destroy-swapchain (device swapchain)
  (%destroy-swapchain device swapchain (cffi:null-pointer))
  (values))

(defun create-semaphore (device)
  (with-vk (create-info semaphore-create-info :flags 0)
    (create-semaphore-handle device create-info)))

(defun destroy-semaphore (device semaphore)
  (%destroy-semaphore device semaphore (cffi:null-pointer))
  (values))

(defun acquire-next-image
    (device swapchain semaphore &key (timeout #xffffffffffffffff))
  (cffi:with-foreign-object (index :uint32)
    (let ((result
            (with-vulkan-results
                (:acquire-next-image :success :suboptimal-khr)
              (%acquire-next-image
               device swapchain timeout semaphore (cffi:null-pointer) index))))
      (values (cffi:mem-ref index :uint32) result))))

(defun present (queue swapchain image-index &key (wait-semaphores #()))
  (with-foreign-array (waits :pointer wait-semaphores)
    (with-foreign-array (swapchains :pointer (vector swapchain))
      (with-foreign-array (indices :uint32 (vector image-index))
        (with-vk (present-info present-info
                  :wait-semaphore-count (length wait-semaphores)
                  :p-wait-semaphores waits
                  :swapchain-count 1
                  :p-swapchains swapchains
                  :p-image-indices indices
                  :p-results (cffi:null-pointer))
          (with-vulkan-results (:present :success :suboptimal-khr)
            (%queue-present queue present-info)))))))
