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
   (legacy-vk-handle
    :initarg :legacy-vk-handle
    :reader vulkan-device-legacy-vk-handle)
   (legacy-vk-physical-device
    :initarg :legacy-vk-physical-device
    :reader vulkan-device-legacy-vk-physical-device)
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
    :reader vulkan-queue-family)
   (legacy-vk-handle
    :initarg :legacy-vk-handle
    :reader vulkan-queue-legacy-vk-handle)))

(defclass vulkan-gpu-texture (gpu-texture vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-texture-device)
   (memory
    :initarg :memory
    :reader vulkan-texture-memory)
   (vk-format
    :initarg :vk-format
    :reader vulkan-texture-vk-format)
   (layout
    :initform :undefined
    :accessor vulkan-texture-layout)))

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
   (state
    :initform :recording
    :accessor vulkan-command-encoder-state)))

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
  "Return the first graphics-capable queue family exposed by PHYSICAL-DEVICE."
  (or (loop for properties in
              (lvk:physical-device-queue-families physical-device)
            for index from 0
            when (and (plusp (lvk:queue-family-count properties))
                      (member :graphics (lvk:queue-family-flags properties)))
              return index)
      (error 'vulkan-gpu-error
             :operation :request-device
             :reason :no-graphics-queue
             :details physical-device)))

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
                          (member value '(:copy-src :copy-dst)))
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
              (:copy-dst :transfer-dst)))
          usages))

(defun compatible-vulkan-memory-type-p (memory-type-bits index)
  (not (zerop (logand memory-type-bits (ash 1 index)))))

(defun find-vulkan-texture-memory-type (device memory-requirements)
  (let ((memory-types
          (vk:memory-types
           (vk:get-physical-device-memory-properties
            (vulkan-device-legacy-vk-physical-device device))))
        (memory-type-bits (vk:memory-type-bits memory-requirements)))
    (or (loop for memory-type in memory-types
              for index from 0
              when (and (compatible-vulkan-memory-type-p
                         memory-type-bits index)
                        (member :device-local
                                (vk:property-flags memory-type)))
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
                 (setf native-device
                       (lvk:create-device physical-device queue-family))
                 (let* ((native-queue
                          (lvk:get-device-queue native-device queue-family))
                        (device
                          (make-instance
                           'vulkan-gpu-device
                           :label (gpu-descriptor-label descriptor)
                           :handle native-device
                           :instance instance
                           :physical-device physical-device
                           :legacy-vk-handle
                           (vk:make-device-wrapper native-device)
                           :legacy-vk-physical-device
                           (vk:make-physical-device-wrapper physical-device)
                           :queue-family queue-family))
                        (queue
                          (make-instance
                           'vulkan-gpu-queue
                           :label "default queue"
                           :handle native-queue
                           :device device
                           :family queue-family
                           :legacy-vk-handle
                           (vk:make-queue-wrapper native-queue))))
                   (setf (vulkan-device-queue device) queue
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
           (native-device (vulkan-device-legacy-vk-handle device))
           (image nil)
           (memory nil)
           (completed-p nil))
      (unwind-protect
           (progn
             (setf image
                   (vk:create-image
                    native-device
                    (vk:make-image-create-info
                     :image-type :2d
                     :format format
                     :extent (vk:make-extent-3d
                              :width (first size)
                              :height (second size)
                              :depth 1)
                     :mip-levels 1
                     :array-layers 1
                     :samples :1
                     :tiling :optimal
                     :usage (vulkan-image-usage usages)
                     :sharing-mode :exclusive
                     :initial-layout :undefined)))
             (let* ((requirements
                      (vk:get-image-memory-requirements native-device image))
                    (memory-type
                      (find-vulkan-texture-memory-type device requirements)))
               (setf memory
                     (vk:allocate-memory
                      native-device
                      (vk:make-memory-allocate-info
                       :allocation-size (vk:size requirements)
                       :memory-type-index memory-type)))
               (vk:bind-image-memory native-device image memory 0)
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
            (vk:destroy-image native-device image))
          (when memory
            (vk:free-memory native-device memory)))))))

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
                   (vk:create-command-pool
                    (vulkan-device-legacy-vk-handle device)
                    (vk:make-command-pool-create-info
                     :flags (list :transient)
                     :queue-family-index (vulkan-device-queue-family device))))
             (let ((command-buffer
                     (first
                      (vk:allocate-command-buffers
                       (vulkan-device-legacy-vk-handle device)
                       (vk:make-command-buffer-allocate-info
                        :command-pool command-pool
                        :level :primary
                        :command-buffer-count 1)))))
               (vk:begin-command-buffer
                command-buffer (vk:make-command-buffer-begin-info))
               (setf completed-p t)
               (make-instance
                'vulkan-gpu-command-encoder
                :label (gpu-descriptor-label descriptor)
                :device device
                :command-pool command-pool
                :command-buffer command-buffer)))
        (unless completed-p
          (when command-pool
            (vk:destroy-command-pool
             (vulkan-device-legacy-vk-handle device) command-pool)))))))

(defun ensure-vulkan-command-encoder-state (encoder operation)
  (unless (eq :recording (vulkan-command-encoder-state encoder))
    (error 'gpu-invalid-state-error
           :object encoder
           :operation operation
           :state (vulkan-command-encoder-state encoder)
           :expected-state :recording)))

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

(defun vulkan-texture-subresource-range ()
  (vk:make-image-subresource-range
   :aspect-mask (list :color)
   :base-mip-level 0
   :level-count 1
   :base-array-layer 0
   :layer-count 1))

(defun vulkan-layout-access-and-stage (layout)
  (ecase layout
    (:undefined
     (values nil (list :top-of-pipe)))
    (:transfer-src-optimal
     (values (list :transfer-read) (list :transfer)))
    (:transfer-dst-optimal
     (values (list :transfer-write) (list :transfer)))))

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
          (vk:cmd-pipeline-barrier
           (vulkan-command-encoder-command-buffer encoder)
           nil nil
           (list
            (vk:make-image-memory-barrier
             :src-access-mask src-access
             :dst-access-mask dst-access
             :old-layout old-layout
             :new-layout new-layout
             :src-queue-family-index vk:+queue-family-ignored+
             :dst-queue-family-index vk:+queue-family-ignored+
             :image (vulkan-handle texture)
             :subresource-range (vulkan-texture-subresource-range)))
           src-stage dst-stage)))
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
    (let ((color (normalize-vulkan-clear-color command))
          (texture
            (ensure-vulkan-texture-for-command
             encoder
             (gpu-clear-texture-command-texture command)
             command
             :copy-dst)))
      (transition-vulkan-texture encoder texture :transfer-dst-optimal)
      (vk:cmd-clear-color-image
       (vulkan-command-encoder-command-buffer encoder)
       (vulkan-handle texture)
       :transfer-dst-optimal
       (vk:make-clear-color-value :float-32 color)
       (list (vulkan-texture-subresource-range)))))
  encoder)

(defun ensure-compatible-vulkan-copy (command source destination)
  (when (eq source destination)
    (reject-gpu-request command :same-copy-source-and-destination source))
  (unless (and (equal (gpu-texture-size source)
                      (gpu-texture-size destination))
               (eq (gpu-texture-format source)
                   (gpu-texture-format destination)))
    (reject-gpu-request
     command :incompatible-copy
     (list :source-size (gpu-texture-size source)
           :destination-size (gpu-texture-size destination)
           :source-format (gpu-texture-format source)
           :destination-format (gpu-texture-format destination)))))

(defun vulkan-texture-copy-region (texture)
  (let ((size (gpu-texture-size texture))
        (layers
          (vk:make-image-subresource-layers
           :aspect-mask (list :color)
           :mip-level 0
           :base-array-layer 0
           :layer-count 1)))
    (vk:make-image-copy
     :src-subresource layers
     :dst-subresource layers
     :extent (vk:make-extent-3d
              :width (first size)
              :height (second size)
              :depth 1))))

(defmethod encode
    ((encoder vulkan-gpu-command-encoder)
     (command gpu-copy-texture-command))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-command-encoder-state encoder :encode)
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
      (vk:cmd-copy-image
       (vulkan-command-encoder-command-buffer encoder)
       (vulkan-handle source) :transfer-src-optimal
       (vulkan-handle destination) :transfer-dst-optimal
       (list (vulkan-texture-copy-region source)))))
  encoder)

(defun hash-table-alist (table)
  (loop for key being the hash-keys of table using (hash-value value)
        collect (cons key value)))

(defun hash-table-keys (table)
  (loop for key being the hash-keys of table collect key))

(defmethod finish ((encoder vulkan-gpu-command-encoder))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-command-encoder-state encoder :finish)
    (let ((device (vulkan-command-encoder-device encoder))
          (command-buffer (vulkan-command-encoder-command-buffer encoder))
          (command-pool (vulkan-command-encoder-command-pool encoder)))
      (ensure-live-vulkan-object device :finish)
      (vk:end-command-buffer command-buffer)
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

(defmethod submit ((queue vulkan-gpu-queue) (command-buffers vector))
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
        (vk:queue-submit
         (vulkan-queue-legacy-vk-handle queue)
         (list
          (vk:make-submit-info
           :command-buffers
           (loop for command-buffer across command-buffers
                 collect (vulkan-handle command-buffer)))))
        (vk:queue-wait-idle (vulkan-queue-legacy-vk-handle queue))
        (loop for command-buffer across command-buffers
              do (setf (vulkan-command-buffer-state command-buffer)
                       :submitted))
        (maphash (lambda (texture layout)
                   (setf (vulkan-texture-layout texture) layout))
                 texture-layouts))))
  (values))

(defmethod destroy ((encoder vulkan-gpu-command-encoder))
  (with-vulkan-gpu-driver-environment
    (when (eq :recording (vulkan-command-encoder-state encoder))
      (let ((device (vulkan-command-encoder-device encoder))
            (command-pool (vulkan-command-encoder-command-pool encoder)))
        (when (and command-pool
                   (not (vulkan-object-destroyed-p device)))
          (vk:destroy-command-pool
           (vulkan-device-legacy-vk-handle device) command-pool)))
      (setf (vulkan-command-encoder-command-pool encoder) nil)))
  (setf (vulkan-command-encoder-state encoder) :destroyed)
  (values))

(defmethod destroy ((command-buffer vulkan-gpu-command-buffer))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p command-buffer)
      (let ((device (vulkan-command-buffer-device command-buffer)))
        (unless (vulkan-object-destroyed-p device)
          (vk:destroy-command-pool
           (vulkan-device-legacy-vk-handle device)
           (vulkan-command-buffer-command-pool command-buffer))))
      (setf (vulkan-object-destroyed-p command-buffer) t
            (vulkan-command-buffer-state command-buffer) :destroyed)))
  (values))

(defmethod destroy ((texture vulkan-gpu-texture))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p texture)
      (let ((device (vulkan-texture-device texture)))
        (unless (vulkan-object-destroyed-p device)
          (vk:destroy-image
           (vulkan-device-legacy-vk-handle device)
           (vulkan-handle texture))
          (vk:free-memory
           (vulkan-device-legacy-vk-handle device)
           (vulkan-texture-memory texture))))
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
