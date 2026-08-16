;;;; Lisp-shaped Vulkan operations built on the owned ABI.
;;;;
;;;; The raw `vk:` entry points and structs stay in defs.lisp; this file gives
;;;; the GPU and canvas backends the small, direct `lvk:` vocabulary they use.

(in-package #:luv.vulkan)

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
  (vk:enumerate-instance-extension-properties (cffi:null-pointer))
  :element (:struct extension-properties)
  :extractor extension-property-name
  :operation :enumerate-instance-extension-properties)

(define-enumerator enumerate-physical-devices (instance)
  (vk:enumerate-physical-devices instance)
  :element :pointer)

(define-creator create-instance-handle (create-info)
  (vk:create-instance create-info (cffi:null-pointer))
  :checked t
  :operation :create-instance)

(define-creator create-device-handle (physical-device create-info)
  (vk:create-device physical-device create-info (cffi:null-pointer))
  :checked t
  :operation :create-device)

(define-creator get-device-queue-handle
    (device queue-family-index queue-index)
  (vk:get-device-queue device queue-family-index queue-index))

(define-creator create-image-handle (device create-info)
  (vk:create-image device create-info (cffi:null-pointer))
  :checked t
  :operation :create-image)

(define-creator create-buffer-handle (device create-info)
  (vk:create-buffer device create-info (cffi:null-pointer))
  :checked t
  :operation :create-buffer)

(define-creator allocate-memory-handle (device allocate-info)
  (vk:allocate-memory device allocate-info (cffi:null-pointer))
  :checked t
  :operation :allocate-memory)

(define-creator create-image-view-handle (device create-info)
  (vk:create-image-view device create-info (cffi:null-pointer))
  :checked t
  :operation :create-image-view)

(define-creator create-shader-module-handle (device create-info)
  (vk:create-shader-module device create-info (cffi:null-pointer))
  :checked t
  :operation :create-shader-module)

(define-creator create-descriptor-set-layout-handle (device create-info)
  (vk:create-descriptor-set-layout device create-info (cffi:null-pointer))
  :checked t
  :operation :create-descriptor-set-layout)

(define-creator create-pipeline-layout-handle (device create-info)
  (vk:create-pipeline-layout device create-info (cffi:null-pointer))
  :checked t
  :operation :create-pipeline-layout)

(define-creator create-compute-pipeline-handle (device create-info)
  (vk:create-compute-pipelines device (cffi:null-pointer) 1 create-info
                             (cffi:null-pointer))
  :checked t
  :operation :create-compute-pipeline)

(define-creator create-graphics-pipeline-handle (device create-info)
  (vk:create-graphics-pipelines device (cffi:null-pointer) 1 create-info
                              (cffi:null-pointer))
  :checked t
  :operation :create-graphics-pipeline)

(define-creator create-sampler-handle (device create-info)
  (vk:create-sampler device create-info (cffi:null-pointer))
  :checked t
  :operation :create-sampler)

(define-creator create-render-pass-handle (device create-info)
  (vk:create-render-pass device create-info (cffi:null-pointer))
  :checked t
  :operation :create-render-pass)

(define-creator create-framebuffer-handle (device create-info)
  (vk:create-framebuffer device create-info (cffi:null-pointer))
  :checked t
  :operation :create-framebuffer)

(define-creator create-descriptor-pool-handle (device create-info)
  (vk:create-descriptor-pool device create-info (cffi:null-pointer))
  :checked t
  :operation :create-descriptor-pool)

(define-creator allocate-descriptor-set-handle (device allocate-info)
  (vk:allocate-descriptor-sets device allocate-info)
  :checked t
  :operation :allocate-descriptor-set)

(define-creator create-command-pool-handle (device create-info)
  (vk:create-command-pool device create-info (cffi:null-pointer))
  :checked t
  :operation :create-command-pool)

(define-creator allocate-command-buffer-handle (device allocate-info)
  (vk:allocate-command-buffers device allocate-info)
  :checked t
  :operation :allocate-command-buffer)

(define-creator create-swapchain-handle (device create-info)
  (vk:create-swapchain-khr device create-info (cffi:null-pointer))
  :checked t
  :operation :create-swapchain)

(define-creator create-semaphore-handle (device create-info)
  (vk:create-semaphore device create-info (cffi:null-pointer))
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
       ;; Luv assumes modern Vulkan.  Timeline semaphores and
       ;; synchronization2 are mandatory core features at this version.
       ((:api-version api-version-value) (make-version 1 4 0))
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
  (vk:destroy-instance instance (cffi:null-pointer))
  (values))

(defstruct (debug-message (:constructor %make-debug-message))
  severity
  types
  id-name
  id-number
  text)

(defstruct (debug-messenger (:constructor %make-debug-messenger))
  instance
  handle
  user-data
  (destroyed-p nil))

(defvar *debug-messenger-callbacks* (make-hash-table :test #'eql)
  "Callbacks keyed by the address passed through VkDebugUtils user data.")

(defun nullable-foreign-string (pointer)
  (unless (cffi:null-pointer-p pointer)
    (cffi:foreign-string-to-lisp pointer)))

(cffi:defcallback dispatch-debug-utils-message :uint32
    ((severity debug-utils-message-severity-flags)
     (types debug-utils-message-type-flags)
     (callback-data :pointer)
     (user-data :pointer))
  ;; Never unwind through a Vulkan driver.  Callback failures are reported and
  ;; Vulkan is always told not to abort the call which produced the message.
  (handler-case
      (let ((callback
              (gethash (cffi:pointer-address user-data)
                       *debug-messenger-callbacks*)))
        (when callback
          (cffi:with-foreign-slots
              ((p-message-id-name message-id-number p-message)
               callback-data
               (:struct debug-utils-messenger-callback-data-ext))
            (funcall callback
                     (%make-debug-message
                      :severity severity
                      :types types
                      :id-name (nullable-foreign-string p-message-id-name)
                      :id-number message-id-number
                      :text (nullable-foreign-string p-message))))))
    (serious-condition (condition)
      (ignore-errors
        (format *error-output*
                "~&Vulkan debug callback failed: ~A~%" condition))))
  0)

(defun instance-procedure (instance name)
  (let ((procedure (vk:get-instance-proc-addr instance name)))
    (when (cffi:null-pointer-p procedure)
      (error "Vulkan instance procedure ~A is unavailable." name))
    procedure))

(defun install-debug-messenger
    (instance callback
     &key
       (severities '(:warning :error))
       (types '(:general :validation :performance)))
  "Install CALLBACK for INSTANCE and return an owned DEBUG-MESSENGER.

CALLBACK receives one DEBUG-MESSAGE.  VK_EXT_debug_utils must have been
enabled when INSTANCE was created.  Keep the returned messenger alive and
destroy it before destroying INSTANCE."
  (check-type callback (or function symbol))
  (let ((user-data (cffi:foreign-alloc :uint8))
        (handle nil)
        (completed-p nil))
    (setf (gethash (cffi:pointer-address user-data)
                   *debug-messenger-callbacks*)
          callback)
    (unwind-protect
         (with-vk (create-info debug-utils-messenger-create-info-ext
                   :flags 0
                   :message-severity severities
                   :message-type types
                   :pfn-user-callback
                   (cffi:callback dispatch-debug-utils-message)
                   :p-user-data user-data)
           (cffi:with-foreign-object (output :pointer)
             (setf (cffi:mem-ref output :pointer) (cffi:null-pointer))
             (with-vulkan-results (:create-debug-utils-messenger-ext)
               (vk:create-debug-utils-messenger-ext
                instance create-info (cffi:null-pointer) output))
             (setf handle (cffi:mem-ref output :pointer)
                   completed-p t)
             (%make-debug-messenger
              :instance instance :handle handle :user-data user-data)))
      (unless completed-p
        (remhash (cffi:pointer-address user-data)
                 *debug-messenger-callbacks*)
        (cffi:foreign-free user-data)))))

(defun destroy-debug-messenger (messenger)
  "Destroy MESSENGER and release its Lisp callback.  Safe to call twice."
  (unless (debug-messenger-destroyed-p messenger)
    (unwind-protect
         (vk:destroy-debug-utils-messenger-ext
          (debug-messenger-instance messenger)
          (debug-messenger-handle messenger)
          (cffi:null-pointer))
      (remhash (cffi:pointer-address (debug-messenger-user-data messenger))
               *debug-messenger-callbacks*)
      (cffi:foreign-free (debug-messenger-user-data messenger))
      (setf (debug-messenger-destroyed-p messenger) t)))
  (values))

(defun physical-device-queue-families (physical-device)
  (cffi:with-foreign-object (count :uint32)
    (setf (cffi:mem-ref count :uint32) 0)
    (vk:get-physical-device-queue-family-properties
     physical-device count (cffi:null-pointer))
    (let ((capacity (cffi:mem-ref count :uint32)))
      (if (zerop capacity)
          nil
          (cffi:with-foreign-object
              (properties '(:struct queue-family-properties) capacity)
            (clear-foreign-object
             properties '(:struct queue-family-properties) capacity)
            (vk:get-physical-device-queue-family-properties
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
        (with-vk (timeline-features
                  physical-device-timeline-semaphore-features
                  :timeline-semaphore 1)
          (with-vk (synchronization-features
                    physical-device-synchronization-2-features
                    :p-next timeline-features
                    :synchronization-2 1)
            (with-vk (create-info device-create-info
                      :p-next synchronization-features
                      :flags 0
                      :queue-create-info-count 1
                      :p-queue-create-infos queue-info
                      :enabled-layer-count 0
                      :pp-enabled-layer-names (cffi:null-pointer)
                      :enabled-extension-count (length enabled-extension-names)
                      :pp-enabled-extension-names extension-names
                      :p-enabled-features (cffi:null-pointer))
              (create-device-handle physical-device create-info))))))))

(defun destroy-device (device)
  (vk:destroy-device device (cffi:null-pointer))
  (values))

(defun get-device-queue (device queue-family-index &optional (queue-index 0))
  (get-device-queue-handle device queue-family-index queue-index))

(defun device-wait-idle (device)
  (with-vulkan-results (:device-wait-idle)
    (vk:device-wait-idle device))
  (values))

(defun physical-device-memory-types (physical-device)
  (cffi:with-foreign-object
      (properties '(:struct physical-device-memory-properties))
    (clear-foreign-object properties
                          '(:struct physical-device-memory-properties))
    (vk:get-physical-device-memory-properties physical-device properties)
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
  (vk:destroy-image device image (cffi:null-pointer))
  (values))

(defun get-image-memory-requirements (device image)
  (cffi:with-foreign-object (requirements '(:struct memory-requirements))
    (clear-foreign-object requirements '(:struct memory-requirements))
    (vk:get-image-memory-requirements device image requirements)
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
  (vk:destroy-buffer device buffer (cffi:null-pointer))
  (values))

(defun get-buffer-memory-requirements (device buffer)
  (cffi:with-foreign-object (requirements '(:struct memory-requirements))
    (clear-foreign-object requirements '(:struct memory-requirements))
    (vk:get-buffer-memory-requirements device buffer requirements)
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
  (vk:free-memory device memory (cffi:null-pointer))
  (values))

(defun bind-image-memory (device image memory &optional (offset 0))
  (with-vulkan-results (:bind-image-memory)
    (vk:bind-image-memory device image memory offset))
  (values))

(defun bind-buffer-memory (device buffer memory &optional (offset 0))
  (with-vulkan-results (:bind-buffer-memory)
    (vk:bind-buffer-memory device buffer memory offset))
  (values))

(defun map-memory (device memory size &optional (offset 0))
  (cffi:with-foreign-object (data :pointer)
    (with-vulkan-results (:map-memory)
      (vk:map-memory device memory offset size 0 data))
    (cffi:mem-ref data :pointer)))

(defun unmap-memory (device memory)
  (vk:unmap-memory device memory)
  (values))

(defun create-image-view
    (device image format &key (view-type :2d) (aspect :color))
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
    (fill-image-subresource-range
     (cffi:foreign-slot-pointer
      create-info '(:struct image-view-create-info) 'subresource-range)
     aspect)
    (create-image-view-handle device create-info)))

(defun destroy-image-view (device view)
  (vk:destroy-image-view device view (cffi:null-pointer))
  (values))

(defun create-shader-module (device words)
  (with-foreign-array (code :uint32 words)
    (with-vk (create-info shader-module-create-info
              :flags 0
              :code-size (* 4 (length words))
              :p-code code)
      (create-shader-module-handle device create-info))))

(defun destroy-shader-module (device shader-module)
  (vk:destroy-shader-module device shader-module (cffi:null-pointer))
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

(defun create-uniform-buffer-descriptor-set-layout
    (device &key (binding 0) (stages '(:vertex)))
  (with-vk (layout-binding descriptor-set-layout-binding
            :binding binding
            :descriptor-type :uniform-buffer
            :descriptor-count 1
            :stage-flags stages
            :p-immutable-samplers (cffi:null-pointer))
    (with-vk (create-info descriptor-set-layout-create-info
              :flags 0 :binding-count 1 :p-bindings layout-binding)
      (create-descriptor-set-layout-handle device create-info))))

(defun texture-sampler-uniform-descriptor-type (type)
  (ecase type
    (:texture :sampled-image)
    (:sampler :sampler)
    (:uniform-buffer :uniform-buffer)))

(defun texture-sampler-uniform-descriptor-stages (entry)
  (or (getf entry :stages)
      (ecase (getf entry :type)
        ((:texture :sampler :uniform-buffer) '(:vertex :fragment)))))

(defun create-texture-sampler-uniform-descriptor-set-layout
    (device entries)
  (let ((count (length entries)))
    (cffi:with-foreign-object
        (bindings '(:struct descriptor-set-layout-binding) count)
      (loop for entry in entries
            for index from 0
            do (fill-vk
                (cffi:mem-aptr
                 bindings '(:struct descriptor-set-layout-binding) index)
                'descriptor-set-layout-binding
                :binding (getf entry :binding)
                :descriptor-type
                (texture-sampler-uniform-descriptor-type
                 (getf entry :type))
                :descriptor-count 1
                :stage-flags
                (texture-sampler-uniform-descriptor-stages entry)
                :p-immutable-samplers (cffi:null-pointer)))
      (with-vk (create-info descriptor-set-layout-create-info
                :flags 0 :binding-count count :p-bindings bindings)
        (create-descriptor-set-layout-handle device create-info)))))

(defun destroy-descriptor-set-layout (device layout)
  (vk:destroy-descriptor-set-layout device layout (cffi:null-pointer))
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
  (vk:destroy-pipeline-layout device layout (cffi:null-pointer))
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

(defun create-sampler
    (device &key (mag-filter :linear) (min-filter :linear)
                 (mipmap-mode :nearest)
                 (address-mode-u :clamp-to-edge)
                 (address-mode-v :clamp-to-edge)
                 (address-mode-w :clamp-to-edge)
                 compare)
  (with-vk (create-info sampler-create-info
            :flags 0
            :mag-filter mag-filter :min-filter min-filter
            :mipmap-mode mipmap-mode
            :address-mode-u address-mode-u
            :address-mode-v address-mode-v
            :address-mode-w address-mode-w
            :mip-lod-bias 0.0
            :anisotropy-enable 0 :max-anisotropy 1.0
            :compare-enable (if compare 1 0) :compare-op (or compare :always)
            :min-lod 0.0 :max-lod 0.0
            :border-color :float-transparent-black
            :unnormalized-coordinates 0)
    (create-sampler-handle device create-info)))

(defun destroy-sampler (device sampler)
  (vk:destroy-sampler device sampler (cffi:null-pointer))
  (values))

(defun vulkan-attachment-store-op (store-op)
  (ecase store-op
    (:store :store)
    (:discard :dont-care)))

(defun create-color-render-pass
    (device format &key depth-format (depth-store-op :discard))
  (let ((attachment-count (if depth-format 2 1)))
    (cffi:with-foreign-object
        (attachments '(:struct attachment-description) attachment-count)
      (fill-vk
       (cffi:mem-aptr attachments '(:struct attachment-description) 0)
       'attachment-description
       :flags 0 :format format :samples :1
       :load-op :clear :store-op :store
       :stencil-load-op :dont-care :stencil-store-op :dont-care
       :initial-layout :color-attachment-optimal
       :final-layout :color-attachment-optimal)
      (when depth-format
        (fill-vk
         (cffi:mem-aptr attachments '(:struct attachment-description) 1)
         'attachment-description
         :flags 0 :format depth-format :samples :1
         :load-op :clear
         :store-op (vulkan-attachment-store-op depth-store-op)
         :stencil-load-op :dont-care :stencil-store-op :dont-care
         :initial-layout :depth-stencil-attachment-optimal
         :final-layout :depth-stencil-attachment-optimal))
      (with-vk (color-reference attachment-reference
                :attachment 0 :layout :color-attachment-optimal)
        (labels ((create-with-depth-reference (depth-reference)
                   (with-vk (subpass subpass-description
                             :flags 0 :pipeline-bind-point :graphics
                             :input-attachment-count 0
                             :p-input-attachments (cffi:null-pointer)
                             :color-attachment-count 1
                             :p-color-attachments color-reference
                             :p-resolve-attachments (cffi:null-pointer)
                             :p-depth-stencil-attachment depth-reference
                             :preserve-attachment-count 0
                             :p-preserve-attachments (cffi:null-pointer))
                     (with-vk (create-info render-pass-create-info
                               :flags 0 :attachment-count attachment-count
                               :p-attachments attachments
                               :subpass-count 1 :p-subpasses subpass
                               :dependency-count 0
                               :p-dependencies (cffi:null-pointer))
                       (create-render-pass-handle device create-info)))))
          (if depth-format
              (with-vk (depth-reference attachment-reference
                        :attachment 1
                        :layout :depth-stencil-attachment-optimal)
                (create-with-depth-reference depth-reference))
              (create-with-depth-reference (cffi:null-pointer))))))))

(defun create-depth-render-pass
    (device depth-format &key (depth-store-op :store))
  (cffi:with-foreign-object
      (attachments '(:struct attachment-description) 1)
    (fill-vk
     attachments 'attachment-description
     :flags 0 :format depth-format :samples :1
     :load-op :clear
     :store-op (vulkan-attachment-store-op depth-store-op)
     :stencil-load-op :dont-care :stencil-store-op :dont-care
     :initial-layout :depth-stencil-attachment-optimal
     :final-layout :depth-stencil-attachment-optimal)
    (with-vk (depth-reference attachment-reference
              :attachment 0 :layout :depth-stencil-attachment-optimal)
      (with-vk (subpass subpass-description
                :flags 0 :pipeline-bind-point :graphics
                :input-attachment-count 0
                :p-input-attachments (cffi:null-pointer)
                :color-attachment-count 0
                :p-color-attachments (cffi:null-pointer)
                :p-resolve-attachments (cffi:null-pointer)
                :p-depth-stencil-attachment depth-reference
                :preserve-attachment-count 0
                :p-preserve-attachments (cffi:null-pointer))
        (with-vk (create-info render-pass-create-info
                  :flags 0 :attachment-count 1 :p-attachments attachments
                  :subpass-count 1 :p-subpasses subpass
                  :dependency-count 0 :p-dependencies (cffi:null-pointer))
          (create-render-pass-handle device create-info))))))

(defun destroy-render-pass (device render-pass)
  (vk:destroy-render-pass device render-pass (cffi:null-pointer))
  (values))

(defun create-framebuffer
    (device render-pass image-view width height &key depth-view)
  (let ((attachment-vector
          (cond ((and image-view depth-view) (vector image-view depth-view))
                (image-view (vector image-view))
                (depth-view (vector depth-view))
                (t (error "A framebuffer needs at least one attachment.")))))
    (with-foreign-array (attachments :pointer attachment-vector)
      (with-vk (create-info framebuffer-create-info
                :flags 0 :render-pass render-pass
                :attachment-count (length attachment-vector)
                :p-attachments attachments
                :width width :height height :layers 1)
        (create-framebuffer-handle device create-info)))))

(defun destroy-framebuffer (device framebuffer)
  (vk:destroy-framebuffer device framebuffer (cffi:null-pointer))
  (values))

(defun call-with-vertex-input-descriptions (vertex-buffers function)
  (if (null vertex-buffers)
      (funcall function 0 (cffi:null-pointer) 0 (cffi:null-pointer))
      (let ((attribute-count
              (loop for buffer in vertex-buffers
                    sum (length (getf buffer :attributes)))))
        (cffi:with-foreign-object
            (bindings '(:struct vertex-input-binding-description)
                      (length vertex-buffers))
          (cffi:with-foreign-object
              (attributes '(:struct vertex-input-attribute-description)
                          attribute-count)
            (loop with attribute-index = 0
                  for buffer in vertex-buffers
                  for default-binding from 0
                  for binding = (or (getf buffer :binding) default-binding)
                  do (fill-vk
                      (cffi:mem-aptr
                       bindings '(:struct vertex-input-binding-description)
                       default-binding)
                      'vertex-input-binding-description
                      :binding binding
                      :stride (getf buffer :array-stride)
                      :input-rate (or (getf buffer :step-mode) :vertex))
                     (dolist (attribute (getf buffer :attributes))
                       (fill-vk
                        (cffi:mem-aptr
                         attributes
                         '(:struct vertex-input-attribute-description)
                         attribute-index)
                        'vertex-input-attribute-description
                        :location (getf attribute :shader-location)
                        :binding binding
                        :format (getf attribute :format)
                        :offset (getf attribute :offset))
                       (incf attribute-index)))
            (funcall function (length vertex-buffers) bindings
                     attribute-count attributes))))))

(defun call-with-depth-stencil-state (depth-compare depth-write-enabled function)
  (if depth-compare
      (with-vk (state pipeline-depth-stencil-state-create-info
                :flags 0
                :depth-test-enable 1
                :depth-write-enable (if depth-write-enabled 1 0)
                :depth-compare-op depth-compare
                :depth-bounds-test-enable 0
                :stencil-test-enable 0
                :min-depth-bounds 0.0
                :max-depth-bounds 1.0)
        (funcall function state))
      (funcall function (cffi:null-pointer))))

(defun create-graphics-pipeline
    (device vertex-module fragment-module layout render-pass
     &key (vertex-entry-point "main") (fragment-entry-point "main")
          (topology :triangle-strip) vertex-buffers
          depth-compare depth-write-enabled blend)
  (labels
      ((create-with-shader-names (vertex-name fragment-name)
         (let ((stage-count (if fragment-module 2 1)))
           (cffi:with-foreign-object
               (stages '(:struct pipeline-shader-stage-create-info)
                       stage-count)
             (fill-vk
              (cffi:mem-aptr stages
                             '(:struct pipeline-shader-stage-create-info) 0)
              'pipeline-shader-stage-create-info
              :flags 0 :stage '(:vertex) :module vertex-module
              :p-name vertex-name
              :p-specialization-info (cffi:null-pointer))
             (when fragment-module
               (fill-vk
                (cffi:mem-aptr stages
                               '(:struct pipeline-shader-stage-create-info) 1)
                'pipeline-shader-stage-create-info
                :flags 0 :stage '(:fragment) :module fragment-module
                :p-name fragment-name
                :p-specialization-info (cffi:null-pointer)))
             (call-with-vertex-input-descriptions
              vertex-buffers
              (lambda (binding-count bindings attribute-count attributes)
                (with-vk (vertex-input
                          pipeline-vertex-input-state-create-info
                          :flags 0
                          :vertex-binding-description-count binding-count
                          :p-vertex-binding-descriptions bindings
                          :vertex-attribute-description-count attribute-count
                          :p-vertex-attribute-descriptions attributes)
                  (with-vk (input-assembly
                            pipeline-input-assembly-state-create-info
                            :flags 0 :topology topology
                            :primitive-restart-enable 0)
                    (with-vk (viewport-state
                              pipeline-viewport-state-create-info
                              :flags 0 :viewport-count 1
                              :p-viewports (cffi:null-pointer)
                              :scissor-count 1
                              :p-scissors (cffi:null-pointer))
                      (with-vk (rasterization
                                pipeline-rasterization-state-create-info
                                :flags 0 :depth-clamp-enable 0
                                :rasterizer-discard-enable 0
                                :polygon-mode :fill
                                :cull-mode nil
                                :front-face :counter-clockwise
                                :depth-bias-enable 0
                                :depth-bias-constant-factor 0.0
                                :depth-bias-clamp 0.0
                                :depth-bias-slope-factor 0.0
                                :line-width 1.0)
                        (with-vk (multisample
                                  pipeline-multisample-state-create-info
                                  :flags 0 :rasterization-samples :1
                                  :sample-shading-enable 0
                                  :min-sample-shading 0.0
                                  :p-sample-mask (cffi:null-pointer)
                                  :alpha-to-coverage-enable 0
                                  :alpha-to-one-enable 0)
                          (labels
                              ((create-with-blend (attachment-count
                                                   attachments)
                                 (with-vk
                                     (blend
                                      pipeline-color-blend-state-create-info
                                      :flags 0 :logic-op-enable 0
                                      :logic-op :copy
                                      :attachment-count attachment-count
                                      :p-attachments attachments)
                                   (with-foreign-array
                                       (dynamic-states dynamic-state
                                                       #(:viewport :scissor))
                                     (with-vk
                                         (dynamic
                                          pipeline-dynamic-state-create-info
                                          :flags 0
                                          :dynamic-state-count 2
                                          :p-dynamic-states dynamic-states)
                                       (call-with-depth-stencil-state
                                        depth-compare depth-write-enabled
                                        (lambda (depth-state)
                                          (with-vk
                                              (create-info
                                               graphics-pipeline-create-info
                                               :flags 0
                                               :stage-count stage-count
                                               :p-stages stages
                                               :p-vertex-input-state
                                               vertex-input
                                               :p-input-assembly-state
                                               input-assembly
                                               :p-tessellation-state
                                               (cffi:null-pointer)
                                               :p-viewport-state
                                               viewport-state
                                               :p-rasterization-state
                                               rasterization
                                               :p-multisample-state
                                               multisample
                                               :p-depth-stencil-state
                                               depth-state
                                               :p-color-blend-state blend
                                               :p-dynamic-state dynamic
                                               :layout layout
                                               :render-pass render-pass
                                               :subpass 0
                                               :base-pipeline-handle
                                               (cffi:null-pointer)
                                               :base-pipeline-index -1)
                                            (create-graphics-pipeline-handle
                                             device create-info)))))))))
                            (if fragment-module
                                (with-vk
                                    (blend-attachment
                                     pipeline-color-blend-attachment-state
                                     :blend-enable (if blend 1 0)
                                     :src-color-blend-factor :one
                                     :dst-color-blend-factor
                                     (if blend :one-minus-src-alpha :zero)
                                     :color-blend-op :add
                                     :src-alpha-blend-factor :one
                                     :dst-alpha-blend-factor
                                     (if blend :one-minus-src-alpha :zero)
                                     :alpha-blend-op :add
                                     :color-write-mask '(:r :g :b :a))
                                  (create-with-blend 1 blend-attachment))
                                (create-with-blend 0
                                                   (cffi:null-pointer)))))))))))))))
    (cffi:with-foreign-string (vertex-name vertex-entry-point)
      (if fragment-module
          (cffi:with-foreign-string (fragment-name fragment-entry-point)
            (create-with-shader-names vertex-name fragment-name))
          (create-with-shader-names vertex-name (cffi:null-pointer))))))

(defun destroy-pipeline (device pipeline)
  (vk:destroy-pipeline device pipeline (cffi:null-pointer))
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

(defun create-uniform-buffer-descriptor-pool (device &key (max-sets 1))
  (with-vk (pool-size descriptor-pool-size
            :type :uniform-buffer
            :descriptor-count max-sets)
    (with-vk (create-info descriptor-pool-create-info
              :flags 0 :max-sets max-sets
              :pool-size-count 1 :p-pool-sizes pool-size)
      (create-descriptor-pool-handle device create-info))))

(defun texture-sampler-uniform-pool-counts (entries)
  (values (count :texture entries :key (lambda (entry) (getf entry :type)))
          (count :sampler entries :key (lambda (entry) (getf entry :type)))
          (count :uniform-buffer entries
                 :key (lambda (entry) (getf entry :type)))))

(defun create-texture-sampler-uniform-descriptor-pool
    (device entries &key (max-sets 1))
  (multiple-value-bind (texture-count sampler-count uniform-count)
      (texture-sampler-uniform-pool-counts entries)
    (let ((sizes (append
                  (when (plusp texture-count)
                    (list (list :sampled-image texture-count)))
                  (when (plusp sampler-count)
                    (list (list :sampler sampler-count)))
                  (when (plusp uniform-count)
                    (list (list :uniform-buffer uniform-count))))))
      (cffi:with-foreign-object
          (pool-sizes '(:struct descriptor-pool-size) (length sizes))
        (loop for (type count) in sizes
              for index from 0
              do (fill-vk
                  (cffi:mem-aptr
                   pool-sizes '(:struct descriptor-pool-size) index)
                  'descriptor-pool-size
                  :type type :descriptor-count (* count max-sets)))
        (with-vk (create-info descriptor-pool-create-info
                  :flags 0 :max-sets max-sets
                  :pool-size-count (length sizes)
                  :p-pool-sizes pool-sizes)
          (create-descriptor-pool-handle device create-info))))))

(defun destroy-descriptor-pool (device pool)
  (vk:destroy-descriptor-pool device pool (cffi:null-pointer))
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
      (vk:update-descriptor-sets
       device 1 write 0 (cffi:null-pointer))))
  (values))

(defun update-uniform-buffer-descriptor
    (device descriptor-set buffer buffer-size &key (binding 0))
  (with-vk (buffer-info descriptor-buffer-info
            :buffer buffer :offset 0 :range buffer-size)
    (with-vk (write write-descriptor-set
              :dst-set descriptor-set :dst-binding binding
              :dst-array-element 0 :descriptor-count 1
              :descriptor-type :uniform-buffer
              :p-image-info (cffi:null-pointer)
              :p-buffer-info buffer-info
              :p-texel-buffer-view (cffi:null-pointer))
      (vk:update-descriptor-sets
       device 1 write 0 (cffi:null-pointer))))
  (values))

(defun update-texture-sampler-uniform-descriptors
    (device descriptor-set entries)
  (let ((image-count
          (count-if (lambda (entry)
                      (member (getf entry :type) '(:texture :sampler)))
                    entries))
        (buffer-count
          (count :uniform-buffer entries
                 :key (lambda (entry) (getf entry :type))))
        (write-count (length entries)))
    (cffi:with-foreign-object
        (image-infos '(:struct descriptor-image-info) (max 1 image-count))
      (cffi:with-foreign-object
          (buffer-infos '(:struct descriptor-buffer-info)
                        (max 1 buffer-count))
        (cffi:with-foreign-object
            (writes '(:struct write-descriptor-set) write-count)
          (loop with image-index = 0
                with buffer-index = 0
                for entry in entries
                for write-index from 0
                for type = (getf entry :type)
                for write = (cffi:mem-aptr
                             writes '(:struct write-descriptor-set)
                             write-index)
                do
                   (ecase type
                     (:texture
                      (let ((image-info
                              (cffi:mem-aptr
                               image-infos
                               '(:struct descriptor-image-info)
                               image-index)))
                        (fill-vk image-info 'descriptor-image-info
                                 :sampler (cffi:null-pointer)
                                 :image-view (getf entry :image-view)
                                 :image-layout
                                 :shader-read-only-optimal)
                        (fill-vk
                         write 'write-descriptor-set
                         :dst-set descriptor-set
                         :dst-binding (getf entry :binding)
                         :dst-array-element 0
                         :descriptor-count 1
                         :descriptor-type :sampled-image
                         :p-image-info image-info
                         :p-buffer-info (cffi:null-pointer)
                         :p-texel-buffer-view (cffi:null-pointer))
                        (incf image-index)))
                     (:sampler
                      (let ((image-info
                              (cffi:mem-aptr
                               image-infos
                               '(:struct descriptor-image-info)
                               image-index)))
                        (fill-vk image-info 'descriptor-image-info
                                 :sampler (getf entry :sampler)
                                 :image-view (cffi:null-pointer)
                                 :image-layout :undefined)
                        (fill-vk
                         write 'write-descriptor-set
                         :dst-set descriptor-set
                         :dst-binding (getf entry :binding)
                         :dst-array-element 0
                         :descriptor-count 1
                         :descriptor-type :sampler
                         :p-image-info image-info
                         :p-buffer-info (cffi:null-pointer)
                         :p-texel-buffer-view (cffi:null-pointer))
                        (incf image-index)))
                     (:uniform-buffer
                      (let ((buffer-info
                              (cffi:mem-aptr
                               buffer-infos
                               '(:struct descriptor-buffer-info)
                               buffer-index)))
                        (fill-vk buffer-info 'descriptor-buffer-info
                                 :buffer (getf entry :buffer)
                                 :offset 0
                                 :range (getf entry :buffer-size))
                        (fill-vk
                         write 'write-descriptor-set
                         :dst-set descriptor-set
                         :dst-binding (getf entry :binding)
                         :dst-array-element 0
                         :descriptor-count 1
                         :descriptor-type :uniform-buffer
                         :p-image-info (cffi:null-pointer)
                         :p-buffer-info buffer-info
                         :p-texel-buffer-view (cffi:null-pointer))
                        (incf buffer-index)))))
          (vk:update-descriptor-sets
           device write-count writes 0 (cffi:null-pointer))))))
  (values))

(defun create-command-pool (device queue-family-index &key flags)
  (with-vk (create-info command-pool-create-info
            :flags flags
            :queue-family-index queue-family-index)
    (create-command-pool-handle device create-info)))

(defun destroy-command-pool (device command-pool)
  (vk:destroy-command-pool device command-pool (cffi:null-pointer))
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
      (vk:begin-command-buffer command-buffer begin-info)))
  command-buffer)

(defun end-command-buffer (command-buffer)
  (with-vulkan-results (:end-command-buffer)
    (vk:end-command-buffer command-buffer))
  command-buffer)

(defun fill-image-subresource-range (range aspect)
  (fill-vk range 'image-subresource-range
           :aspect-mask (list aspect)
           :base-mip-level 0
           :level-count 1
           :base-array-layer 0
           :layer-count 1))

(defun fill-color-subresource-range (range)
  (fill-image-subresource-range range :color))

(defun cmd-transition-image
    (command-buffer image old-layout new-layout
     src-access dst-access src-stage dst-stage &key (aspect :color))
  (with-vk (barrier image-memory-barrier
            :src-access-mask src-access
            :dst-access-mask dst-access
            :old-layout old-layout
            :new-layout new-layout
            :src-queue-family-index +queue-family-ignored+
            :dst-queue-family-index +queue-family-ignored+
            :image image)
    (fill-image-subresource-range
     (cffi:foreign-slot-pointer
      barrier '(:struct image-memory-barrier) 'subresource-range)
     aspect)
    (vk:cmd-pipeline-barrier
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
      (vk:cmd-clear-color-image
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
    (vk:cmd-copy-image
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
    (vk:cmd-copy-buffer-to-image
     command-buffer buffer image layout 1 region))
  (values))

(defun cmd-copy-image-to-buffer
    (command-buffer image layout buffer width height &optional (depth 1))
  (with-vk (region buffer-image-copy
            :buffer-offset 0
            :buffer-row-length 0
            :buffer-image-height 0)
    (fill-color-subresource-layers
     (cffi:foreign-slot-pointer
      region '(:struct buffer-image-copy) 'image-subresource))
    (fill-vk
     (cffi:foreign-slot-pointer
      region '(:struct buffer-image-copy) 'image-offset)
     'offset-3d :x 0 :y 0 :z 0)
    (fill-vk
     (cffi:foreign-slot-pointer
      region '(:struct buffer-image-copy) 'image-extent)
     'extent-3d :width width :height height :depth depth)
    (vk:cmd-copy-image-to-buffer
     command-buffer image layout buffer 1 region))
  (values))

(defun cmd-bind-compute-pipeline (command-buffer pipeline)
  (vk:cmd-bind-pipeline command-buffer :compute pipeline)
  (values))

(defun cmd-bind-graphics-pipeline (command-buffer pipeline)
  (vk:cmd-bind-pipeline command-buffer :graphics pipeline)
  (values))

(defun cmd-bind-compute-descriptor-set
    (command-buffer pipeline-layout descriptor-set)
  (with-foreign-array (sets :pointer (vector descriptor-set))
    (vk:cmd-bind-descriptor-sets
     command-buffer :compute pipeline-layout 0 1 sets
     0 (cffi:null-pointer)))
  (values))

(defun cmd-bind-graphics-descriptor-set
    (command-buffer pipeline-layout descriptor-set)
  (with-foreign-array (sets :pointer (vector descriptor-set))
    (vk:cmd-bind-descriptor-sets
     command-buffer :graphics pipeline-layout 0 1 sets
     0 (cffi:null-pointer)))
  (values))

(defun cmd-bind-vertex-buffer
    (command-buffer binding buffer &optional (offset 0))
  (with-foreign-array (buffers :pointer (vector buffer))
    (with-foreign-array (offsets :uint64 (vector offset))
      (vk:cmd-bind-vertex-buffers
       command-buffer binding 1 buffers offsets)))
  (values))

(defun cmd-dispatch (command-buffer x y &optional (z 1))
  (vk:cmd-dispatch command-buffer x y z)
  (values))

(defun cmd-begin-color-render-pass
    (command-buffer render-pass framebuffer width height clear-color
     &key depth-clear-value)
  (let ((clear-count (if depth-clear-value 2 1)))
    (cffi:with-foreign-object (clears '(:union clear-value) clear-count)
      (clear-foreign-object clears '(:union clear-value) clear-count)
      (let* ((color
               (cffi:foreign-slot-pointer
                (cffi:mem-aptr clears '(:union clear-value) 0)
                '(:union clear-value) 'color))
             (components
               (cffi:foreign-slot-pointer
                color '(:union clear-color-value) 'float-32)))
        (loop for component across clear-color
              for index below 4
              do (setf (cffi:mem-aref components :float index) component)))
      (when depth-clear-value
        (fill-vk
         (cffi:foreign-slot-pointer
          (cffi:mem-aptr clears '(:union clear-value) 1)
          '(:union clear-value) 'depth-stencil)
         'clear-depth-stencil-value
         :depth (coerce depth-clear-value 'single-float)
         :stencil 0))
      (with-vk (begin-info render-pass-begin-info
                :render-pass render-pass :framebuffer framebuffer
                :clear-value-count clear-count :p-clear-values clears)
        (let ((area
                (cffi:foreign-slot-pointer
                 begin-info '(:struct render-pass-begin-info) 'render-area)))
          (fill-vk
           (cffi:foreign-slot-pointer area '(:struct rect-2d) 'offset)
           'offset-2d :x 0 :y 0)
          (fill-vk
           (cffi:foreign-slot-pointer area '(:struct rect-2d) 'extent)
           'extent-2d :width width :height height))
        (vk:cmd-begin-render-pass command-buffer begin-info :inline))))
  (values))

(defun cmd-begin-depth-render-pass
    (command-buffer render-pass framebuffer width height depth-clear-value)
  (cffi:with-foreign-object (clears '(:union clear-value) 1)
    (clear-foreign-object clears '(:union clear-value) 1)
    (fill-vk
     (cffi:foreign-slot-pointer
      clears '(:union clear-value) 'depth-stencil)
     'clear-depth-stencil-value
     :depth (coerce depth-clear-value 'single-float)
     :stencil 0)
    (with-vk (begin-info render-pass-begin-info
              :render-pass render-pass :framebuffer framebuffer
              :clear-value-count 1 :p-clear-values clears)
      (let ((area
              (cffi:foreign-slot-pointer
               begin-info '(:struct render-pass-begin-info) 'render-area)))
        (fill-vk
         (cffi:foreign-slot-pointer area '(:struct rect-2d) 'offset)
         'offset-2d :x 0 :y 0)
        (fill-vk
         (cffi:foreign-slot-pointer area '(:struct rect-2d) 'extent)
         'extent-2d :width width :height height))
      (vk:cmd-begin-render-pass command-buffer begin-info :inline)))
  (values))

(defun cmd-set-viewport-and-scissor (command-buffer width height)
  (with-vk (viewport viewport
            :x 0.0 :y 0.0
            :width (coerce width 'single-float)
            :height (coerce height 'single-float)
            :min-depth 0.0 :max-depth 1.0)
    (vk:cmd-set-viewport command-buffer 0 1 viewport))
  (with-vk (scissor rect-2d)
    (fill-vk
     (cffi:foreign-slot-pointer scissor '(:struct rect-2d) 'offset)
     'offset-2d :x 0 :y 0)
    (fill-vk
     (cffi:foreign-slot-pointer scissor '(:struct rect-2d) 'extent)
     'extent-2d :width width :height height)
    (vk:cmd-set-scissor command-buffer 0 1 scissor))
  (values))

(defun cmd-set-scissor (command-buffer x y width height)
  (with-vk (scissor rect-2d)
    (fill-vk
     (cffi:foreign-slot-pointer scissor '(:struct rect-2d) 'offset)
     'offset-2d :x x :y y)
    (fill-vk
     (cffi:foreign-slot-pointer scissor '(:struct rect-2d) 'extent)
     'extent-2d :width width :height height)
    (vk:cmd-set-scissor command-buffer 0 1 scissor))
  (values))

(defun cmd-end-render-pass (command-buffer)
  (vk:cmd-end-render-pass command-buffer)
  (values))

(defun cmd-draw
    (command-buffer vertex-count &optional (instance-count 1)
                                        (first-vertex 0) (first-instance 0))
  (vk:cmd-draw command-buffer vertex-count instance-count
             first-vertex first-instance)
  (values))

(defun fill-semaphore-submit-infos (pointer entries)
  "Fill POINTER, an array of VkSemaphoreSubmitInfo, from ENTRIES.

Each entry is a list (SEMAPHORE STAGES &optional VALUE).  STAGES is a
PIPELINE-STAGE-FLAGS-2 keyword list.  VALUE is meaningful only when
SEMAPHORE is a timeline semaphore."
  (loop for index below (length entries)
        for entry = (elt entries index)
        do (destructuring-bind (semaphore stages &optional (value 0)) entry
             (fill-vk
              (cffi:mem-aptr pointer '(:struct semaphore-submit-info) index)
              'semaphore-submit-info
              :semaphore semaphore
              :value value
              :stage-mask stages
              :device-index 0)))
  pointer)

(defun submit-command-buffers
    (queue buffers &key (wait-semaphores #()) (signal-semaphores #()))
  "Submit BUFFERS through vkQueueSubmit2.

WAIT-SEMAPHORES and SIGNAL-SEMAPHORES are sequences of semaphore submit
entries as understood by FILL-SEMAPHORE-SUBMIT-INFOS."
  (let ((buffer-count (length buffers))
        (wait-count (length wait-semaphores))
        (signal-count (length signal-semaphores)))
    (cffi:with-foreign-objects
        ((buffer-infos '(:struct command-buffer-submit-info)
                       (max buffer-count 1))
         (wait-infos '(:struct semaphore-submit-info) (max wait-count 1))
         (signal-infos '(:struct semaphore-submit-info) (max signal-count 1)))
      (loop for index below buffer-count
            do (fill-vk
                (cffi:mem-aptr
                 buffer-infos '(:struct command-buffer-submit-info) index)
                'command-buffer-submit-info
                :command-buffer (elt buffers index)
                :device-mask 0))
      (fill-semaphore-submit-infos wait-infos wait-semaphores)
      (fill-semaphore-submit-infos signal-infos signal-semaphores)
      (with-vk (submit submit-info-2
                :flags 0
                :wait-semaphore-info-count wait-count
                :p-wait-semaphore-infos wait-infos
                :command-buffer-info-count buffer-count
                :p-command-buffer-infos buffer-infos
                :signal-semaphore-info-count signal-count
                :p-signal-semaphore-infos signal-infos)
        (with-vulkan-results (:queue-submit-2)
          (vk:queue-submit2 queue 1 submit (cffi:null-pointer))))))
  (values))

(defun submit-command-buffer (queue command-buffer)
  (submit-command-buffers queue (vector command-buffer)))

(defun queue-wait-idle (queue)
  (with-vulkan-results (:queue-wait-idle)
    (vk:queue-wait-idle queue))
  (values))

(defun surface-supported-p (physical-device queue-family-index surface)
  (cffi:with-foreign-object (supported :uint32)
    (setf (cffi:mem-ref supported :uint32) 0)
    (with-vulkan-results (:get-physical-device-surface-support)
      (vk:get-physical-device-surface-support-khr
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
      (vk:get-physical-device-surface-capabilities-khr
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
  (vk:get-physical-device-surface-formats-khr physical-device surface)
  :element (:struct surface-format)
  :extractor extract-surface-format)

(define-enumerator get-surface-present-modes (physical-device surface)
  (vk:get-physical-device-surface-present-modes-khr physical-device surface)
  :element present-mode)

(define-enumerator get-swapchain-images (device swapchain)
  (vk:get-swapchain-images-khr device swapchain)
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
  (vk:destroy-swapchain-khr device swapchain (cffi:null-pointer))
  (values))

(defun create-semaphore (device)
  (with-vk (create-info semaphore-create-info :flags 0)
    (create-semaphore-handle device create-info)))

(defun create-timeline-semaphore (device &key (initial-value 0))
  "Create a timeline semaphore whose 64-bit counter starts at INITIAL-VALUE."
  (with-vk (type-info semaphore-type-create-info
            :semaphore-type :timeline
            :initial-value initial-value)
    (with-vk (create-info semaphore-create-info
              :p-next type-info
              :flags 0)
      (create-semaphore-handle device create-info))))

(defun destroy-semaphore (device semaphore)
  (vk:destroy-semaphore device semaphore (cffi:null-pointer))
  (values))

(defun semaphore-counter-value (device semaphore)
  "Return the current 64-bit counter of a timeline SEMAPHORE."
  (cffi:with-foreign-object (value :uint64)
    (with-vulkan-results (:get-semaphore-counter-value)
      (vk:get-semaphore-counter-value device semaphore value))
    (cffi:mem-ref value :uint64)))

(defun wait-semaphore-value
    (device semaphore value &key (timeout #xffffffffffffffff))
  "Block until timeline SEMAPHORE's counter reaches at least VALUE."
  (with-foreign-array (semaphores :pointer (vector semaphore))
    (cffi:with-foreign-object (values :uint64)
      (setf (cffi:mem-ref values :uint64) value)
      (with-vk (wait-info semaphore-wait-info
                :flags 0
                :semaphore-count 1
                :p-semaphores semaphores
                :p-values values)
        (with-vulkan-results (:wait-semaphores)
          (vk:wait-semaphores device wait-info timeout)))))
  (values))

(defun acquire-next-image
    (device swapchain semaphore &key (timeout #xffffffffffffffff))
  (cffi:with-foreign-object (index :uint32)
    (let ((result
            (with-vulkan-results
                (:acquire-next-image :success :suboptimal-khr)
              (vk:acquire-next-image-khr
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
            (vk:queue-present-khr queue present-info)))))))
