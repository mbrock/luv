(in-package #:luv)

(defmacro with-vulkan-gpu-driver-environment (&body body)
  "Run BODY with the floating-point environment expected by native drivers."
  #+darwin
  `(float-features:with-float-traps-masked t ,@body)
  #-darwin
  `(progn ,@body))

(define-condition vulkan-gpu-error (gpu-error)
  ((reason
    :initarg :reason
    :reader vulkan-gpu-error-reason)
   (details
    :initarg :details
    :initform nil
    :reader vulkan-gpu-error-details))
  (:report
   (lambda (condition stream)
     (case (vulkan-gpu-error-reason condition)
       (:no-physical-device
        (format stream "Vulkan found no physical devices."))
       (:no-graphics-queue
        (format stream
                "The Vulkan physical device ~S exposes no graphics queue."
                (vulkan-gpu-error-details condition)))
       (:no-compatible-memory
        (format stream
                "The Vulkan device has no compatible memory type for ~S."
                (vulkan-gpu-error-details condition)))
       (otherwise
        (format stream "Vulkan GPU operation ~S failed: ~S~@[ (~S)~]"
                (gpu-error-operation condition)
                (vulkan-gpu-error-reason condition)
                (vulkan-gpu-error-details condition)))))))

(defun vulkan-gpu-instance-options ()
  "Return the extensions and flags for a portable Vulkan instance."
  (let* ((available (lvk:enumerate-instance-extension-names))
         (portability-extension
           lvk:+portability-enumeration-extension-name+)
         (portability-p
           (member portability-extension available :test #'string=)))
    (values (and portability-p (list portability-extension))
            (if portability-p
                '(:enumerate-portability)
                nil))))

(defclass vulkan-gpu-provider (gpu-provider)
  ((application-name
    :initarg :application-name
    :initform "luv gpu"
    :reader vulkan-provider-application-name)))

(unless *gpu-provider*
  (setf *gpu-provider* (make-instance 'vulkan-gpu-provider)))

(defclass vulkan-gpu-object ()
  ((handle
    :initarg :handle
    :reader vulkan-handle)
   (destroyed-p
    :initform nil
    :accessor vulkan-object-destroyed-p)))

(defclass vulkan-gpu-device (gpu-device vulkan-gpu-object)
  ((instance
    :initarg :instance
    :reader vulkan-device-instance)
   (physical-device
    :initarg :physical-device
    :reader vulkan-device-physical-device)
   (queue-family
    :initarg :queue-family
    :reader vulkan-device-queue-family)
   (queue
    :initform nil
    :accessor vulkan-device-queue)))

(defclass vulkan-gpu-queue (gpu-queue vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-queue-device)
   (family
    :initarg :family
    :reader vulkan-queue-family)))

(defclass vulkan-gpu-texture (gpu-texture vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-texture-device)
   (memory
    :initarg :memory
    :initform nil
    :reader vulkan-texture-memory)
   (owned-p
    :initarg :owned-p
    :initform t
    :reader vulkan-texture-owned-p)
   (vk-format
    :initarg :vk-format
    :reader vulkan-texture-vk-format)
   (layout
    :initform :undefined
    :accessor vulkan-texture-layout)))

(defclass vulkan-gpu-texture-view (gpu-texture-view vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-texture-view-device)))

(defclass vulkan-gpu-shader-module (gpu-shader-module vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-shader-module-device)))

(defclass vulkan-gpu-bind-group-layout
    (gpu-bind-group-layout vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-bind-group-layout-device)
   (binding
    :initarg :binding
    :reader vulkan-bind-group-layout-binding)))

(defclass vulkan-gpu-compute-pipeline
    (gpu-compute-pipeline vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-compute-pipeline-device)
   (layout
    :initarg :layout
    :reader vulkan-compute-pipeline-bind-group-layout)
   (pipeline-layout
    :initarg :pipeline-layout
    :reader vulkan-compute-pipeline-layout)))

(defclass vulkan-gpu-bind-group (gpu-bind-group vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-bind-group-device)
   (layout
    :initarg :layout
    :reader vulkan-bind-group-layout)
   (texture-view
    :initarg :texture-view
    :reader vulkan-bind-group-texture-view)
   (descriptor-pool
    :initarg :descriptor-pool
    :reader vulkan-bind-group-descriptor-pool)))

(defclass vulkan-gpu-command-encoder (gpu-command-encoder)
  ((device
    :initarg :device
    :reader vulkan-command-encoder-device)
   (command-pool
    :initarg :command-pool
    :accessor vulkan-command-encoder-command-pool)
   (command-buffer
    :initarg :command-buffer
    :reader vulkan-command-encoder-command-buffer)
   (initial-texture-layouts
    :initform (make-hash-table :test #'eq)
    :reader vulkan-command-encoder-initial-texture-layouts)
   (texture-layouts
    :initform (make-hash-table :test #'eq)
    :reader vulkan-command-encoder-texture-layouts)
   (textures
    :initform (make-hash-table :test #'eq)
    :reader vulkan-command-encoder-textures)
   (active-pass
    :initform nil
    :accessor vulkan-command-encoder-active-pass)
   (state
    :initform :recording
    :accessor vulkan-command-encoder-state)))

(defclass vulkan-gpu-compute-pass-encoder (gpu-compute-pass-encoder)
  ((encoder
    :initarg :encoder
    :reader vulkan-compute-pass-command-encoder)
   (pipeline
    :initform nil
    :accessor vulkan-compute-pass-pipeline)
   (bind-group
    :initform nil
    :accessor vulkan-compute-pass-bind-group)
   (state
    :initform :recording
    :accessor vulkan-compute-pass-state)))

(defclass vulkan-gpu-command-buffer (gpu-command-buffer vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-command-buffer-device)
   (command-pool
    :initarg :command-pool
    :reader vulkan-command-buffer-command-pool)
   (initial-texture-layouts
    :initarg :initial-texture-layouts
    :reader vulkan-command-buffer-initial-texture-layouts)
   (final-texture-layouts
    :initarg :final-texture-layouts
    :reader vulkan-command-buffer-final-texture-layouts)
   (textures
    :initarg :textures
    :reader vulkan-command-buffer-textures)
   (state
    :initform :ready
    :accessor vulkan-command-buffer-state)))

(defun ensure-live-vulkan-object (object operation)
  (when (vulkan-object-destroyed-p object)
    (error 'gpu-object-destroyed-error
           :object object
           :operation operation))
  object)

(defun first-vulkan-graphics-queue-family (physical-device)
  "Return the first graphics-and-compute queue exposed by PHYSICAL-DEVICE."
  (or (loop for properties in
              (lvk:physical-device-queue-families physical-device)
            for index from 0
            when (and (plusp (lvk:queue-family-count properties))
                      (member :graphics (lvk:queue-family-flags properties))
                      (member :compute (lvk:queue-family-flags properties)))
              return index)
      (error 'vulkan-gpu-error
             :operation :request-device
             :reason :no-graphics-queue
             :details physical-device)))

(defun make-vulkan-gpu-device
    (instance physical-device queue-family descriptor
     &key enabled-extension-names)
  "Create GPU wrappers for an already selected Vulkan device and queue."
  (let ((native-device
          (lvk:create-device
           physical-device queue-family
           :enabled-extension-names enabled-extension-names)))
    (handler-case
        (let* ((native-queue
                 (lvk:get-device-queue native-device queue-family))
               (device
                 (make-instance
                  'vulkan-gpu-device
                  :label (gpu-descriptor-label descriptor)
                  :handle native-device
                  :instance instance
                  :physical-device physical-device
                  :queue-family queue-family))
               (queue
                 (make-instance
                  'vulkan-gpu-queue
                  :label "default queue"
                  :handle native-queue
                  :device device
                  :family queue-family)))
          (setf (vulkan-device-queue device) queue)
          device)
      (error (condition)
        (lvk:destroy-device native-device)
        (error condition)))))

(defun make-borrowed-vulkan-texture
    (device image size format vk-format &key (usage '(:copy-dst)))
  "Wrap an externally owned Vulkan IMAGE as a GPU texture."
  (make-instance
   'vulkan-gpu-texture
   :label "borrowed swapchain texture"
   :size (list (first size) (second size) 1)
   :usage usage
   :dimensions :2d
   :format format
   :handle image
   :device device
   :vk-format vk-format
   :owned-p nil))

(defun check-vulkan-device-descriptor (descriptor)
  "Reject WebGPU requirements the initial Vulkan backend cannot honor yet."
  (unless (typep descriptor 'device-descriptor)
    (error 'gpu-request-error
           :operation :request-device
           :descriptor descriptor
           :reason :invalid-descriptor
           :details descriptor))
  (when (device-descriptor-required-features descriptor)
    (error 'gpu-request-error
           :operation :request-device
           :descriptor descriptor
           :reason :unsupported-features
           :details (device-descriptor-required-features descriptor)))
  (when (device-descriptor-required-limits descriptor)
    (error 'gpu-request-error
           :operation :request-device
           :descriptor descriptor
           :reason :unsupported-limits
           :details (device-descriptor-required-limits descriptor))))

(defun reject-gpu-request (descriptor reason &optional details)
  (error 'gpu-request-error
         :operation :create
         :descriptor descriptor
         :reason reason
         :details details))

(defun normalize-vulkan-texture-size (descriptor)
  (let* ((size (texture-descriptor-size descriptor))
         (components
           (typecase size
             (list size)
             (vector (coerce size 'list))
             (otherwise nil))))
    (unless (and (member (length components) '(2 3))
                 (every (lambda (value)
                          (and (integerp value) (plusp value)))
                        components)
                 (or (= 2 (length components))
                     (= 1 (third components))))
      (reject-gpu-request descriptor :invalid-texture-size size))
    (list (first components) (second components) 1)))

(defun normalize-vulkan-texture-usage (descriptor)
  (let* ((usage (texture-descriptor-usage descriptor))
         (usages
           (typecase usage
             (keyword (list usage))
             (list usage)
             (vector (coerce usage 'list))
             (otherwise nil))))
    (unless (and usages
                 (every (lambda (value)
                          (member value
                                  '(:copy-src :copy-dst :storage-binding)))
                        usages))
      (reject-gpu-request descriptor :unsupported-texture-usage usage))
    (remove-duplicates usages)))

(defun vulkan-texture-format (descriptor)
  (or (cdr (assoc (texture-descriptor-format descriptor)
                  '((:rgba8-unorm . :r8g8b8a8-unorm)
                    (:rgba8-unorm-srgb . :r8g8b8a8-srgb)
                    (:bgra8-unorm . :b8g8r8a8-unorm)
                    (:bgra8-unorm-srgb . :b8g8r8a8-srgb))))
      (reject-gpu-request
       descriptor :unsupported-texture-format
       (texture-descriptor-format descriptor))))

(defun vulkan-image-usage (usages)
  (mapcar (lambda (usage)
            (ecase usage
              (:copy-src :transfer-src)
              (:copy-dst :transfer-dst)
              (:storage-binding :storage)))
          usages))

(defun compatible-vulkan-memory-type-p (memory-type-bits index)
  (not (zerop (logand memory-type-bits (ash 1 index)))))

(defun find-vulkan-texture-memory-type (device memory-requirements)
  (let ((memory-types
          (lvk:physical-device-memory-types
           (vulkan-device-physical-device device)))
        (memory-type-bits
          (lvk:image-memory-requirements-memory-type-bits
           memory-requirements)))
    (or (loop for memory-type in memory-types
              for index from 0
              when (and (compatible-vulkan-memory-type-p
                        memory-type-bits index)
                        (member :device-local
                                (lvk:physical-memory-type-flags memory-type)))
                return index)
        (loop for memory-type in memory-types
              for index from 0
              when (compatible-vulkan-memory-type-p memory-type-bits index)
                return index)
        (error 'vulkan-gpu-error
               :operation :create-texture
               :reason :no-compatible-memory
               :details memory-requirements))))

(defmethod request-gpu-device
    ((provider vulkan-gpu-provider) &optional descriptor)
  "Create an owned Vulkan instance, logical device, and graphics queue."
  (with-vulkan-gpu-driver-environment
    (let ((descriptor (or descriptor (make-device-descriptor))))
      (check-vulkan-device-descriptor descriptor)
      (let ((instance nil)
            (native-device nil)
            (completed-p nil))
        (unwind-protect
             (multiple-value-bind (extensions flags)
                 (vulkan-gpu-instance-options)
               (setf instance
                     (lvk:create-instance
                      :application-name
                      (vulkan-provider-application-name provider)
                      :flags flags
                      :enabled-extension-names extensions))
               (let* ((physical-device
                        (or (first
                             (lvk:enumerate-physical-devices instance))
                            (error 'vulkan-gpu-error
                                   :operation :request-device
                                   :reason :no-physical-device)))
                      (queue-family
                        (first-vulkan-graphics-queue-family physical-device)))
                 (let ((device
                         (make-vulkan-gpu-device
                          instance physical-device queue-family descriptor)))
                   (setf native-device (vulkan-handle device)
                         completed-p t)
                   device)))
          (unless completed-p
            (when native-device
              (lvk:destroy-device native-device))
            (when instance
              (lvk:destroy-instance instance))))))))

(defmethod device-queue ((device vulkan-gpu-device))
  (ensure-live-vulkan-object device :device-queue)
  (vulkan-device-queue device))

(defmethod create
    ((device vulkan-gpu-device) (descriptor texture-descriptor))
  "Create one owned, single-mip Vulkan 2D texture and bind its memory."
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-texture)
    (unless (eq :2d (texture-descriptor-dimensions descriptor))
      (reject-gpu-request
       descriptor :unsupported-texture-dimensions
       (texture-descriptor-dimensions descriptor)))
    (let* ((size (normalize-vulkan-texture-size descriptor))
           (usages (normalize-vulkan-texture-usage descriptor))
           (format (vulkan-texture-format descriptor))
           (native-device (vulkan-handle device))
           (image nil)
           (memory nil)
           (completed-p nil))
      (unwind-protect
           (progn
             (setf image
                   (lvk:create-image
                    native-device
                    :type :2d
                     :format format
                     :width (first size)
                     :height (second size)
                     :mip-levels 1
                     :array-layers 1
                     :samples :1
                     :tiling :optimal
                     :usage (vulkan-image-usage usages)
                     :sharing-mode :exclusive
                     :initial-layout :undefined))
             (let* ((requirements
                      (lvk:get-image-memory-requirements native-device image))
                    (memory-type
                      (find-vulkan-texture-memory-type device requirements)))
               (setf memory
                     (lvk:allocate-memory
                      native-device
                      (lvk:image-memory-requirements-size requirements)
                      memory-type))
               (lvk:bind-image-memory native-device image memory)
               (let ((texture
                       (make-instance
                        'vulkan-gpu-texture
                        :label (gpu-descriptor-label descriptor)
                        :size size
                        :usage usages
                        :dimensions :2d
                        :format (texture-descriptor-format descriptor)
                        :handle image
                        :device device
                        :memory memory
                        :vk-format format)))
                 (setf completed-p t)
                 texture)))
        (unless completed-p
          (when image
            (lvk:destroy-image native-device image))
          (when memory
            (lvk:free-memory native-device memory)))))))

(defun ensure-vulkan-object-device (object actual-device expected-device
                                    operation)
  (ensure-live-vulkan-object object operation)
  (unless (eq actual-device expected-device)
    (error 'gpu-device-mismatch-error
           :object object
           :operation operation
           :expected-device expected-device
           :actual-device actual-device))
  object)

(defmethod create
    ((device vulkan-gpu-device) (descriptor texture-view-descriptor))
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-texture-view)
    (let ((texture (texture-view-descriptor-texture descriptor)))
      (unless (typep texture 'vulkan-gpu-texture)
        (reject-gpu-request descriptor :incompatible-texture-backend texture))
      (ensure-vulkan-object-device
       texture (vulkan-texture-device texture) device :create-texture-view)
      (make-instance
       'vulkan-gpu-texture-view
       :label (gpu-descriptor-label descriptor)
       :handle (lvk:create-image-view
                (vulkan-handle device)
                (vulkan-handle texture)
                (vulkan-texture-vk-format texture))
       :device device
       :texture texture))))

(defmethod create
    ((device vulkan-gpu-device) (descriptor shader-module-descriptor))
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-shader-module)
    (let ((code (shader-module-descriptor-code descriptor)))
      (unless (and (vectorp code)
                   (plusp (length code))
                   (every (lambda (word)
                            (typep word '(unsigned-byte 32)))
                          code))
        (reject-gpu-request descriptor :invalid-spir-v code))
      (make-instance
       'vulkan-gpu-shader-module
       :label (gpu-descriptor-label descriptor)
       :handle (lvk:create-shader-module (vulkan-handle device) code)
       :device device))))

(defun storage-texture-layout-entry (descriptor)
  (let ((entries (bind-group-layout-descriptor-entries descriptor)))
    (unless (and (listp entries) (= 1 (length entries))
                 (listp (first entries))
                 (eq :storage-texture (getf (first entries) :type))
                 (typep (getf (first entries) :binding)
                        '(unsigned-byte 32)))
      (reject-gpu-request descriptor :unsupported-bind-group-layout entries))
    (first entries)))

(defmethod create
    ((device vulkan-gpu-device)
     (descriptor bind-group-layout-descriptor))
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-bind-group-layout)
    (let* ((entry (storage-texture-layout-entry descriptor))
           (binding (getf entry :binding)))
      (make-instance
       'vulkan-gpu-bind-group-layout
       :label (gpu-descriptor-label descriptor)
       :handle (lvk:create-storage-image-descriptor-set-layout
                (vulkan-handle device) :binding binding)
       :device device
       :binding binding))))

(defmethod create
    ((device vulkan-gpu-device) (descriptor compute-pipeline-descriptor))
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-compute-pipeline)
    (let ((module (compute-pipeline-descriptor-module descriptor))
          (layout (compute-pipeline-descriptor-layout descriptor))
          (entry-point (compute-pipeline-descriptor-entry-point descriptor)))
      (unless (typep module 'vulkan-gpu-shader-module)
        (reject-gpu-request descriptor :incompatible-shader-module module))
      (unless (typep layout 'vulkan-gpu-bind-group-layout)
        (reject-gpu-request descriptor :incompatible-bind-group-layout layout))
      (ensure-vulkan-object-device
       module (vulkan-shader-module-device module) device
       :create-compute-pipeline)
      (ensure-vulkan-object-device
       layout (vulkan-bind-group-layout-device layout) device
       :create-compute-pipeline)
      (unless (stringp entry-point)
        (reject-gpu-request descriptor :invalid-entry-point entry-point))
      (let ((pipeline-layout
              (lvk:create-pipeline-layout
               (vulkan-handle device) (vector (vulkan-handle layout))))
            (pipeline nil)
            (completed-p nil))
        (unwind-protect
             (progn
               (setf pipeline
                     (lvk:create-compute-pipeline
                      (vulkan-handle device)
                      (vulkan-handle module)
                      pipeline-layout
                      :entry-point entry-point)
                     completed-p t)
               (make-instance
                'vulkan-gpu-compute-pipeline
                :label (gpu-descriptor-label descriptor)
                :handle pipeline
                :device device
                :layout layout
                :pipeline-layout pipeline-layout))
          (unless completed-p
            (lvk:destroy-pipeline-layout
             (vulkan-handle device) pipeline-layout)))))))

(defun storage-texture-bind-group-entry (descriptor layout)
  (let ((entries (bind-group-descriptor-entries descriptor)))
    (unless (and (listp entries) (= 1 (length entries))
                 (listp (first entries))
                 (= (or (getf (first entries) :binding) -1)
                    (vulkan-bind-group-layout-binding layout))
                 (typep (getf (first entries) :resource)
                        'vulkan-gpu-texture-view))
      (reject-gpu-request descriptor :unsupported-bind-group entries))
    (first entries)))

(defmethod create
    ((device vulkan-gpu-device) (descriptor bind-group-descriptor))
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-bind-group)
    (let ((layout (bind-group-descriptor-layout descriptor)))
      (unless (typep layout 'vulkan-gpu-bind-group-layout)
        (reject-gpu-request descriptor :incompatible-bind-group-layout layout))
      (ensure-vulkan-object-device
       layout (vulkan-bind-group-layout-device layout) device
       :create-bind-group)
      (let* ((entry (storage-texture-bind-group-entry descriptor layout))
             (view (getf entry :resource))
             (texture (gpu-texture-view-texture view)))
        (ensure-vulkan-object-device
         view (vulkan-texture-view-device view) device :create-bind-group)
        (unless (member :storage-binding (gpu-texture-usage texture))
          (error 'gpu-usage-error
                 :object texture
                 :operation :create-bind-group
                 :required-usage :storage-binding
                 :actual-usage (gpu-texture-usage texture)))
        (let ((pool
                (lvk:create-storage-image-descriptor-pool
                 (vulkan-handle device)))
              (set nil)
              (completed-p nil))
          (unwind-protect
               (progn
                 (setf set
                       (lvk:allocate-descriptor-set
                        (vulkan-handle device) pool (vulkan-handle layout)))
                 (lvk:update-storage-image-descriptor
                  (vulkan-handle device) set (vulkan-handle view)
                  :binding (vulkan-bind-group-layout-binding layout))
                 (setf completed-p t)
                 (make-instance
                  'vulkan-gpu-bind-group
                  :label (gpu-descriptor-label descriptor)
                  :handle set
                  :device device
                  :layout layout
                  :texture-view view
                  :descriptor-pool pool))
            (unless completed-p
              (lvk:destroy-descriptor-pool
               (vulkan-handle device) pool))))))))

(defmethod create
    ((device vulkan-gpu-device) (descriptor command-encoder-descriptor))
  "Allocate and begin one Vulkan primary command buffer."
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-command-encoder)
    (let ((command-pool nil)
          (completed-p nil))
      (unwind-protect
           (progn
             (setf command-pool
                   (lvk:create-command-pool
                    (vulkan-handle device)
                    (vulkan-device-queue-family device)
                    :flags '(:transient)))
             (let ((command-buffer
                     (lvk:allocate-command-buffer
                      (vulkan-handle device) command-pool)))
               (lvk:begin-command-buffer command-buffer)
               (setf completed-p t)
               (make-instance
                'vulkan-gpu-command-encoder
                :label (gpu-descriptor-label descriptor)
                :device device
                :command-pool command-pool
                :command-buffer command-buffer)))
        (unless completed-p
          (when command-pool
            (lvk:destroy-command-pool
             (vulkan-handle device) command-pool)))))))

(defun ensure-vulkan-command-encoder-state (encoder operation)
  (unless (eq :recording (vulkan-command-encoder-state encoder))
    (error 'gpu-invalid-state-error
           :object encoder
           :operation operation
           :state (vulkan-command-encoder-state encoder)
           :expected-state :recording)))

(defun ensure-no-active-vulkan-pass (encoder operation)
  (when (vulkan-command-encoder-active-pass encoder)
    (error 'gpu-invalid-state-error
           :object encoder
           :operation operation
           :state :compute-pass
           :expected-state :between-passes)))

(defun ensure-vulkan-texture-for-command
    (encoder texture command required-usage)
  (unless (typep texture 'vulkan-gpu-texture)
    (reject-gpu-request command :incompatible-texture-backend texture))
  (ensure-live-vulkan-object texture :encode)
  (unless (eq (vulkan-command-encoder-device encoder)
              (vulkan-texture-device texture))
    (error 'gpu-device-mismatch-error
           :object texture
           :operation :encode
           :expected-device (vulkan-command-encoder-device encoder)
           :actual-device (vulkan-texture-device texture)))
  (unless (member required-usage (gpu-texture-usage texture))
    (error 'gpu-usage-error
           :object texture
           :operation :encode
           :required-usage required-usage
           :actual-usage (gpu-texture-usage texture)))
  (setf (gethash texture (vulkan-command-encoder-textures encoder)) t)
  texture)

(defun vulkan-layout-access-and-stage (layout)
  (ecase layout
    (:undefined
     (values nil (list :top-of-pipe)))
    (:general
     (values (list :shader-read :shader-write)
             (list :compute-shader)))
    (:transfer-src-optimal
     (values (list :transfer-read) (list :transfer)))
    (:transfer-dst-optimal
     (values (list :transfer-write) (list :transfer)))
    (:present-src-khr
     (values nil (list :bottom-of-pipe)))))

(defun vulkan-encoder-texture-layout (encoder texture)
  (multiple-value-bind (layout present-p)
      (gethash texture (vulkan-command-encoder-texture-layouts encoder))
    (if present-p
        layout
        (let ((layout (vulkan-texture-layout texture)))
          (setf (gethash texture
                         (vulkan-command-encoder-initial-texture-layouts
                          encoder))
                layout
                (gethash texture
                         (vulkan-command-encoder-texture-layouts encoder))
                layout)
          layout))))

(defun transition-vulkan-texture (encoder texture new-layout)
  (let ((old-layout (vulkan-encoder-texture-layout encoder texture)))
    (unless (eq old-layout new-layout)
      (multiple-value-bind (src-access src-stage)
          (vulkan-layout-access-and-stage old-layout)
        (multiple-value-bind (dst-access dst-stage)
            (vulkan-layout-access-and-stage new-layout)
          (lvk:cmd-transition-image
           (vulkan-command-encoder-command-buffer encoder)
           (vulkan-handle texture)
           old-layout new-layout
           src-access dst-access src-stage dst-stage)))
      (setf (gethash texture
                     (vulkan-command-encoder-texture-layouts encoder))
            new-layout)))
  texture)

(defun normalize-vulkan-clear-color (command)
  (let* ((color (gpu-clear-texture-command-color command))
         (components
           (typecase color
             (list color)
             (vector (coerce color 'list))
             (otherwise nil))))
    (unless (and (= 4 (length components))
                 (every #'realp components))
      (reject-gpu-request command :invalid-clear-color color))
    (map 'vector
         (lambda (component) (coerce component 'single-float))
         components)))

(defmethod encode
    ((encoder vulkan-gpu-command-encoder)
     (command gpu-clear-texture-command))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-command-encoder-state encoder :encode)
    (ensure-no-active-vulkan-pass encoder :encode)
    (let ((color (normalize-vulkan-clear-color command))
          (texture
            (ensure-vulkan-texture-for-command
             encoder
             (gpu-clear-texture-command-texture command)
             command
             :copy-dst)))
      (transition-vulkan-texture encoder texture :transfer-dst-optimal)
      (lvk:cmd-clear-color-image
       (vulkan-command-encoder-command-buffer encoder)
       (vulkan-handle texture)
       :transfer-dst-optimal
       color)))
  encoder)

(defun ensure-compatible-vulkan-copy (command source destination)
  (when (eq source destination)
    (reject-gpu-request command :same-copy-source-and-destination source))
  (unless (and (equal (gpu-texture-size source)
                      (gpu-texture-size destination))
               ;; Vulkan permits image copies between size-compatible color
               ;; formats.  Every format in this initial vocabulary is one
               ;; four-byte color texel, including RGBA storage -> BGRA
               ;; swapchain copies on Cocoa.
               (member (gpu-texture-format source)
                       '(:rgba8-unorm :rgba8-unorm-srgb
                         :bgra8-unorm :bgra8-unorm-srgb))
               (member (gpu-texture-format destination)
                       '(:rgba8-unorm :rgba8-unorm-srgb
                         :bgra8-unorm :bgra8-unorm-srgb)))
    (reject-gpu-request
     command :incompatible-copy
     (list :source-size (gpu-texture-size source)
           :destination-size (gpu-texture-size destination)
           :source-format (gpu-texture-format source)
           :destination-format (gpu-texture-format destination)))))

(defmethod encode
    ((encoder vulkan-gpu-command-encoder)
     (command gpu-copy-texture-command))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-command-encoder-state encoder :encode)
    (ensure-no-active-vulkan-pass encoder :encode)
    (let ((source
            (ensure-vulkan-texture-for-command
             encoder
             (gpu-copy-texture-command-source command)
             command
             :copy-src))
          (destination
            (ensure-vulkan-texture-for-command
             encoder
             (gpu-copy-texture-command-destination command)
             command
             :copy-dst)))
      (ensure-compatible-vulkan-copy command source destination)
      (transition-vulkan-texture encoder source :transfer-src-optimal)
      (transition-vulkan-texture encoder destination :transfer-dst-optimal)
      (lvk:cmd-copy-image
       (vulkan-command-encoder-command-buffer encoder)
       (vulkan-handle source) :transfer-src-optimal
       (vulkan-handle destination) :transfer-dst-optimal
       (first (gpu-texture-size source))
       (second (gpu-texture-size source)))))
  encoder)

(defmethod begin-compute-pass
    ((encoder vulkan-gpu-command-encoder) &optional descriptor)
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-command-encoder-state encoder :begin-compute-pass)
    (ensure-no-active-vulkan-pass encoder :begin-compute-pass)
    (when descriptor
      (reject-gpu-request descriptor :unsupported-compute-pass-descriptor))
    (let ((pass
            (make-instance
             'vulkan-gpu-compute-pass-encoder
             :encoder encoder)))
      (setf (vulkan-command-encoder-active-pass encoder) pass)
      pass)))

(defun ensure-vulkan-compute-pass-state (pass operation)
  (unless (eq :recording (vulkan-compute-pass-state pass))
    (error 'gpu-invalid-state-error
           :object pass
           :operation operation
           :state (vulkan-compute-pass-state pass)
           :expected-state :recording))
  (let ((encoder (vulkan-compute-pass-command-encoder pass)))
    (unless (eq pass (vulkan-command-encoder-active-pass encoder))
      (error 'gpu-invalid-state-error
             :object pass
             :operation operation
             :state :detached
             :expected-state :active)))
  pass)

(defmethod set-pipeline
    ((pass vulkan-gpu-compute-pass-encoder)
     (pipeline vulkan-gpu-compute-pipeline))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-compute-pass-state pass :set-pipeline)
    (let* ((encoder (vulkan-compute-pass-command-encoder pass))
           (device (vulkan-command-encoder-device encoder)))
      (ensure-vulkan-object-device
       pipeline (vulkan-compute-pipeline-device pipeline) device
       :set-pipeline)
      (lvk:cmd-bind-compute-pipeline
       (vulkan-command-encoder-command-buffer encoder)
       (vulkan-handle pipeline))
      (setf (vulkan-compute-pass-pipeline pass) pipeline)))
  pass)

(defmethod set-bind-group
    ((pass vulkan-gpu-compute-pass-encoder) index
     (bind-group vulkan-gpu-bind-group))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-compute-pass-state pass :set-bind-group)
    (unless (zerop index)
      (reject-gpu-request bind-group :unsupported-bind-group-index index))
    (let* ((encoder (vulkan-compute-pass-command-encoder pass))
           (device (vulkan-command-encoder-device encoder))
           (pipeline (or (vulkan-compute-pass-pipeline pass)
                         (error 'gpu-invalid-state-error
                                :object pass
                                :operation :set-bind-group
                                :state :no-pipeline
                                :expected-state :pipeline-bound))))
      (ensure-vulkan-object-device
       bind-group (vulkan-bind-group-device bind-group) device
       :set-bind-group)
      (unless (eq (vulkan-bind-group-layout bind-group)
                  (vulkan-compute-pipeline-bind-group-layout pipeline))
        (reject-gpu-request bind-group :incompatible-pipeline-layout pipeline))
      (let ((texture
              (gpu-texture-view-texture
               (vulkan-bind-group-texture-view bind-group))))
        (ensure-vulkan-texture-for-command
         encoder texture pass :storage-binding)
        (transition-vulkan-texture encoder texture :general))
      (lvk:cmd-bind-compute-descriptor-set
       (vulkan-command-encoder-command-buffer encoder)
       (vulkan-compute-pipeline-layout pipeline)
       (vulkan-handle bind-group))
      (setf (vulkan-compute-pass-bind-group pass) bind-group)))
  pass)

(defmethod dispatch-workgroups
    ((pass vulkan-gpu-compute-pass-encoder) x &optional (y 1) (z 1))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-compute-pass-state pass :dispatch-workgroups)
    (unless (and (vulkan-compute-pass-pipeline pass)
                 (vulkan-compute-pass-bind-group pass))
      (error 'gpu-invalid-state-error
             :object pass
             :operation :dispatch-workgroups
             :state :incomplete-bindings
             :expected-state :pipeline-and-bind-group-bound))
    (unless (every (lambda (value)
                     (typep value '(unsigned-byte 32)))
                   (list x y z))
      (reject-gpu-request pass :invalid-workgroup-count (list x y z)))
    (lvk:cmd-dispatch
     (vulkan-command-encoder-command-buffer
      (vulkan-compute-pass-command-encoder pass))
     x y z))
  pass)

(defmethod end-pass ((pass vulkan-gpu-compute-pass-encoder))
  (ensure-vulkan-compute-pass-state pass :end-pass)
  (let ((encoder (vulkan-compute-pass-command-encoder pass)))
    (setf (vulkan-command-encoder-active-pass encoder) nil
          (vulkan-compute-pass-state pass) :ended))
  (values))

(defun hash-table-alist (table)
  (loop for key being the hash-keys of table using (hash-value value)
        collect (cons key value)))

(defun hash-table-keys (table)
  (loop for key being the hash-keys of table collect key))

(defmethod finish ((encoder vulkan-gpu-command-encoder))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-command-encoder-state encoder :finish)
    (ensure-no-active-vulkan-pass encoder :finish)
    (let ((device (vulkan-command-encoder-device encoder))
          (command-buffer (vulkan-command-encoder-command-buffer encoder))
          (command-pool (vulkan-command-encoder-command-pool encoder)))
      (ensure-live-vulkan-object device :finish)
      (lvk:end-command-buffer command-buffer)
      (setf (vulkan-command-encoder-state encoder) :finished
            (vulkan-command-encoder-command-pool encoder) nil)
      (make-instance
       'vulkan-gpu-command-buffer
       :label (gpu-object-label encoder)
       :handle command-buffer
       :device device
       :command-pool command-pool
       :initial-texture-layouts
       (hash-table-alist
        (vulkan-command-encoder-initial-texture-layouts encoder))
       :final-texture-layouts
       (hash-table-alist (vulkan-command-encoder-texture-layouts encoder))
       :textures
       (hash-table-keys (vulkan-command-encoder-textures encoder))))))

(defun check-vulkan-command-buffer-for-submit (queue command-buffer)
  (ensure-live-vulkan-object command-buffer :submit)
  (unless (eq (vulkan-queue-device queue)
              (vulkan-command-buffer-device command-buffer))
    (error 'gpu-device-mismatch-error
           :object command-buffer
           :operation :submit
           :expected-device (vulkan-queue-device queue)
           :actual-device (vulkan-command-buffer-device command-buffer)))
  (unless (eq :ready (vulkan-command-buffer-state command-buffer))
    (error 'gpu-invalid-state-error
           :object command-buffer
           :operation :submit
           :state (vulkan-command-buffer-state command-buffer)
           :expected-state :ready))
  (dolist (texture (vulkan-command-buffer-textures command-buffer))
    (ensure-live-vulkan-object texture :submit)
    (unless (eq (vulkan-queue-device queue)
                (vulkan-texture-device texture))
      (error 'gpu-device-mismatch-error
             :object texture
             :operation :submit
             :expected-device (vulkan-queue-device queue)
             :actual-device (vulkan-texture-device texture)))))

(defun vulkan-submitted-texture-layouts (command-buffers)
  "Validate encoded layout assumptions and return the post-batch layouts."
  (let ((layouts (make-hash-table :test #'eq)))
    (labels ((current-layout (texture)
               (multiple-value-bind (layout present-p)
                   (gethash texture layouts)
                 (if present-p layout (vulkan-texture-layout texture)))))
      (loop for command-buffer across command-buffers
            do (dolist (entry
                         (vulkan-command-buffer-initial-texture-layouts
                          command-buffer))
                 (let* ((texture (car entry))
                        (expected-layout (cdr entry))
                        (actual-layout (current-layout texture)))
                   (unless (eq actual-layout expected-layout)
                     (error 'gpu-invalid-state-error
                            :object texture
                            :operation :submit
                            :state actual-layout
                            :expected-state expected-layout))))
               (dolist (entry
                         (vulkan-command-buffer-final-texture-layouts
                          command-buffer))
                 (setf (gethash (car entry) layouts) (cdr entry)))))
    layouts))

(defmethod submit
    ((queue vulkan-gpu-queue) (command-buffer vulkan-gpu-command-buffer))
  (submit queue (vector command-buffer)))

(defun submit-vulkan-command-buffers
    (queue command-buffers &key (wait-semaphores #())
                                (wait-stages #())
                                (signal-semaphores #()))
  "Submit one WebGPU-style batch and synchronously establish its completion.

The initial backend waits for the queue so command-buffer ownership remains
simple.  A later in-flight frame implementation can replace this wait with
fences while preserving the public submission operation."
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object queue :submit)
    (loop for command-buffer across command-buffers
          do (check-vulkan-command-buffer-for-submit queue command-buffer))
    (let ((texture-layouts
            (vulkan-submitted-texture-layouts command-buffers)))
      (when (plusp (length command-buffers))
        (lvk:submit-command-buffers
         (vulkan-handle queue)
         (map 'vector #'vulkan-handle command-buffers)
         :wait-semaphores wait-semaphores
         :wait-stages wait-stages
         :signal-semaphores signal-semaphores)
        (lvk:queue-wait-idle (vulkan-handle queue))
        (loop for command-buffer across command-buffers
              do (setf (vulkan-command-buffer-state command-buffer)
                       :submitted))
        (maphash (lambda (texture layout)
                   (setf (vulkan-texture-layout texture) layout))
                 texture-layouts))))
  (values))

(defmethod submit ((queue vulkan-gpu-queue) (command-buffers vector))
  "Submit one WebGPU-style batch and synchronously establish its completion."
  (submit-vulkan-command-buffers queue command-buffers))

(defmethod destroy ((encoder vulkan-gpu-command-encoder))
  (with-vulkan-gpu-driver-environment
    (when (eq :recording (vulkan-command-encoder-state encoder))
      (let ((device (vulkan-command-encoder-device encoder))
            (command-pool (vulkan-command-encoder-command-pool encoder)))
        (when (and command-pool
                   (not (vulkan-object-destroyed-p device)))
          (lvk:destroy-command-pool
           (vulkan-handle device) command-pool)))
      (setf (vulkan-command-encoder-command-pool encoder) nil)))
  (setf (vulkan-command-encoder-state encoder) :destroyed)
  (values))

(defmethod destroy ((command-buffer vulkan-gpu-command-buffer))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p command-buffer)
      (let ((device (vulkan-command-buffer-device command-buffer)))
        (unless (vulkan-object-destroyed-p device)
          (lvk:destroy-command-pool
           (vulkan-handle device)
           (vulkan-command-buffer-command-pool command-buffer))))
      (setf (vulkan-object-destroyed-p command-buffer) t
            (vulkan-command-buffer-state command-buffer) :destroyed)))
  (values))

(defmethod destroy ((bind-group vulkan-gpu-bind-group))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p bind-group)
      (let ((device (vulkan-bind-group-device bind-group)))
        (unless (vulkan-object-destroyed-p device)
          (lvk:destroy-descriptor-pool
           (vulkan-handle device)
           (vulkan-bind-group-descriptor-pool bind-group))))
      (setf (vulkan-object-destroyed-p bind-group) t)))
  (values))

(defmethod destroy ((pipeline vulkan-gpu-compute-pipeline))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p pipeline)
      (let ((device (vulkan-compute-pipeline-device pipeline)))
        (unless (vulkan-object-destroyed-p device)
          (lvk:destroy-pipeline (vulkan-handle device)
                                (vulkan-handle pipeline))
          (lvk:destroy-pipeline-layout
           (vulkan-handle device)
           (vulkan-compute-pipeline-layout pipeline))))
      (setf (vulkan-object-destroyed-p pipeline) t)))
  (values))

(defmethod destroy ((layout vulkan-gpu-bind-group-layout))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p layout)
      (let ((device (vulkan-bind-group-layout-device layout)))
        (unless (vulkan-object-destroyed-p device)
          (lvk:destroy-descriptor-set-layout
           (vulkan-handle device) (vulkan-handle layout))))
      (setf (vulkan-object-destroyed-p layout) t)))
  (values))

(defmethod destroy ((module vulkan-gpu-shader-module))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p module)
      (let ((device (vulkan-shader-module-device module)))
        (unless (vulkan-object-destroyed-p device)
          (lvk:destroy-shader-module
           (vulkan-handle device) (vulkan-handle module))))
      (setf (vulkan-object-destroyed-p module) t)))
  (values))

(defmethod destroy ((view vulkan-gpu-texture-view))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p view)
      (let ((device (vulkan-texture-view-device view)))
        (unless (vulkan-object-destroyed-p device)
          (lvk:destroy-image-view
           (vulkan-handle device) (vulkan-handle view))))
      (setf (vulkan-object-destroyed-p view) t)))
  (values))

(defmethod destroy ((texture vulkan-gpu-texture))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p texture)
      (let ((device (vulkan-texture-device texture)))
        (when (and (vulkan-texture-owned-p texture)
                   (not (vulkan-object-destroyed-p device)))
          (lvk:destroy-image (vulkan-handle device) (vulkan-handle texture))
          (lvk:free-memory
           (vulkan-handle device) (vulkan-texture-memory texture))))
      (setf (vulkan-object-destroyed-p texture) t)))
  (values))

(defmethod destroy ((device vulkan-gpu-device))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p device)
      (let ((queue (vulkan-device-queue device)))
        (unwind-protect
             (lvk:device-wait-idle (vulkan-handle device))
          (unwind-protect
               (lvk:destroy-device (vulkan-handle device))
            (lvk:destroy-instance (vulkan-device-instance device))
            (setf (vulkan-object-destroyed-p device) t)
            (when queue
              (setf (vulkan-object-destroyed-p queue) t)))))))
  (values))
