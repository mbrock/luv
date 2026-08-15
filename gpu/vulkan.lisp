(in-package #:luv)

(defmacro with-vulkan-gpu-driver-environment (&body body)
  "Run BODY with the floating-point environment expected by native drivers."
  #+sbcl
  `(sb-int:with-float-traps-masked
       (:invalid :divide-by-zero :overflow :underflow :inexact)
     ,@body)
  #+(and darwin (not sbcl))
  `(float-features:with-float-traps-masked t ,@body)
  #-(or sbcl darwin)
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

(defun portable-vulkan-gpu-instance-options ()
  "Return optional extensions and flags useful to luv's Vulkan instance."
  (let* ((available (lvk:enumerate-instance-extension-names))
         (portability-extension
           lvk:+portability-enumeration-extension-name+)
         (debug-utils-extension lvk:+debug-utils-extension-name+)
         (portability-p
           (member portability-extension available :test #'string=))
         (debug-utils-p
           (member debug-utils-extension available :test #'string=)))
    (values (append (and portability-p (list portability-extension))
                    (and debug-utils-p (list debug-utils-extension)))
            (if portability-p
                '(:enumerate-portability)
                nil))))

(defclass vulkan-gpu-provider (gpu-provider)
  ((application-name
    :initarg :application-name
    :initform "luv gpu"
    :reader vulkan-provider-application-name)
   (debug-callback
    :initarg :debug-callback
    :initform nil
    :reader vulkan-provider-debug-callback)
   (debug-severities
    :initarg :debug-severities
    :initform '(:warning :error)
    :reader vulkan-provider-debug-severities)
   (debug-types
    :initarg :debug-types
    :initform '(:general :validation :performance)
    :reader vulkan-provider-debug-types)))

(defgeneric vulkan-provider-instance-options (provider)
  (:documentation "Return Vulkan instance extensions and flags for PROVIDER."))

(defmethod vulkan-provider-instance-options ((provider vulkan-gpu-provider))
  (declare (ignore provider))
  (portable-vulkan-gpu-instance-options))

(defgeneric vulkan-gpu-device-extension-names (provider)
  (:documentation "Return device extensions enabled by PROVIDER."))

(defmethod vulkan-gpu-device-extension-names ((provider vulkan-gpu-provider))
  (declare (ignore provider))
  nil)

(unless *gpu-provider*
  (setf *gpu-provider* (make-instance 'vulkan-gpu-provider)))

(defclass vulkan-gpu-object ()
  ((handle
    :initarg :handle
    :reader vulkan-handle)
   (destroyed-p
    :initform nil
    :accessor vulkan-object-destroyed-p)
   (last-submission
    :initform 0
    :accessor vulkan-object-last-submission
    :documentation "Index of the newest queue submission using this object.
Zero means it has never been submitted.")))

(defgeneric vulkan-native-teardown-closure (object)
  (:method ((object t)) nil)
  (:documentation "Return a thunk performing OBJECT's native teardown, or
NIL when OBJECT owns nothing to tear down.

The closure captures only extracted native handles and the device wrapper,
never OBJECT itself, so it can outlive OBJECT as its leak finalizer.
Capturing the device wrapper also orders finalization: a device stays
reachable until every child's pending finalizer has run."))

(defmacro with-vulkan-queue-teardown
    ((device-object device-var) &body body)
  "Run native teardown BODY with DEVICE-VAR bound to DEVICE-OBJECT's
native handle, skipping it entirely once the device is destroyed.

The queue's recursive lock serializes teardown — including teardown on
the finalizer thread — against submission and device destruction."
  (let ((object (gensym "OBJECT"))
        (run (gensym "RUN"))
        (queue (gensym "QUEUE")))
    `(let ((,object ,device-object))
       (flet ((,run ()
                (unless (vulkan-object-destroyed-p ,object)
                  (let ((,device-var (vulkan-handle ,object)))
                    (declare (ignorable ,device-var))
                    ,@body))))
         (let ((,queue (vulkan-device-queue ,object)))
           (if ,queue
               (sb-thread:with-recursive-lock ((vulkan-queue-lock ,queue))
                 (,run))
               (,run)))))))

(defmethod initialize-instance :after ((object vulkan-gpu-object) &key)
  ;; Explicit DESTROY cancels this finalizer.  If the object is instead
  ;; reclaimed by the collector, the leak is a warned discipline failure
  ;; and the native resources are freed as a safety net.  Anything the
  ;; queue still retains through a live submission record is reachable,
  ;; so a collected wrapper is always past the completion frontier.
  (let ((closer (vulkan-native-teardown-closure object)))
    (when closer
      (let ((resource-class (class-name (class-of object)))
            (label (gpu-object-label object)))
        (sb-ext:finalize
         object
         (lambda ()
           (note-gpu-resource-leak resource-class label)
           (with-vulkan-gpu-driver-environment
             (funcall closer))))))))

(defclass vulkan-gpu-device (gpu-device vulkan-gpu-object)
  ((instance
    :initarg :instance
    :reader vulkan-device-instance)
   (instance-extension-names
    :initarg :instance-extension-names
    :initform nil
    :reader vulkan-device-instance-extension-names)
   (debug-messenger
    :initarg :debug-messenger
    :initform nil
    :reader vulkan-device-debug-messenger)
   (device-extension-names
    :initarg :device-extension-names
    :initform nil
    :reader vulkan-device-extension-names)
   (physical-device
    :initarg :physical-device
    :reader vulkan-device-physical-device)
   (queue-family
    :initarg :queue-family
    :reader vulkan-device-queue-family)
   (queue
    :initform nil
    :accessor vulkan-device-queue)
   (render-passes
    :initform (make-hash-table :test #'equal)
    :reader vulkan-device-render-passes)))

(defclass vulkan-gpu-queue (gpu-queue vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-queue-device)
   (family
    :initarg :family
    :reader vulkan-queue-family)
   (timeline
    :initarg :timeline
    :reader vulkan-queue-timeline
    :documentation "Timeline semaphore signaled with each submission index.")
   (submission-counter
    :initform 0
    :accessor vulkan-queue-submission-counter)
   (live-submissions
    :initform '()
    :accessor vulkan-queue-live-submissions
    :documentation "Submission records not yet passed by the frontier,
oldest first.")
   (lock
    :initform (sb-thread:make-mutex :name "vulkan gpu queue")
    :reader vulkan-queue-lock
    :documentation "Guards the counter, live records, deferred destroys,
and scheduled texture layouts across the canvas and REPL threads.")))

(defstruct vulkan-gpu-submission
  "One queue submission awaiting completion, retaining what the GPU may use."
  (index 0 :type (unsigned-byte 64))
  (command-buffers #() :type vector)
  (resources '() :type list)
  (pending-destroys '() :type list))

(defclass vulkan-gpu-buffer (gpu-buffer vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-buffer-device)
   (memory
    :initarg :memory
    :reader vulkan-buffer-memory)
   (mapped
    :initarg :mapped
    :reader vulkan-buffer-mapped)))

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

(defclass vulkan-gpu-sampler (gpu-sampler vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-sampler-device)))

(defclass vulkan-gpu-bind-group-layout
    (gpu-bind-group-layout vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-bind-group-layout-device)
   (binding
    :initarg :binding
    :initform nil
    :reader vulkan-bind-group-layout-binding)
   (entries
    :initarg :entries
    :initform nil
    :reader vulkan-bind-group-layout-entries)))

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

(defclass vulkan-gpu-render-pipeline
    (gpu-render-pipeline vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-render-pipeline-device)
   (layout
    :initarg :layout
    :reader vulkan-render-pipeline-bind-group-layout)
   (pipeline-layout
    :initarg :pipeline-layout
    :reader vulkan-render-pipeline-layout)
   (render-pass
    :initarg :render-pass
    :reader vulkan-render-pipeline-render-pass)
   (vertex-buffers
    :initarg :vertex-buffers
    :initform nil
    :reader vulkan-render-pipeline-vertex-buffers)
   (depth-format
    :initarg :depth-format
    :initform nil
    :reader vulkan-render-pipeline-depth-format)))

(defclass vulkan-gpu-bind-group (gpu-bind-group vulkan-gpu-object)
  ((device
    :initarg :device
    :reader vulkan-bind-group-device)
   (layout
    :initarg :layout
    :reader vulkan-bind-group-layout)
   (texture-views
    :initarg :texture-views
    :initform nil
    :reader vulkan-bind-group-texture-views)
   (samplers
    :initarg :samplers
    :initform nil
    :reader vulkan-bind-group-samplers)
   (buffers
    :initarg :buffers
    :initform nil
    :reader vulkan-bind-group-buffers)
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
   (resources
    :initform (make-hash-table :test #'eq)
    :reader vulkan-command-encoder-resources
    :documentation "Every GPU object wrapper the recorded commands depend
on, including the tracked textures.")
   (native-resource-box
    :initform (list nil)
    :reader vulkan-command-encoder-native-resource-box
    :documentation "One-element list holding the tagged native resources,
shared with the encoder's leak finalizer so mid-recording growth stays
visible to it.")
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

(defclass vulkan-gpu-render-pass-encoder (gpu-render-pass-encoder)
  ((encoder
    :initarg :encoder
    :reader vulkan-render-pass-command-encoder)
   (framebuffer
    :initarg :framebuffer
    :reader vulkan-render-pass-framebuffer)
   (target
    :initarg :target
    :reader vulkan-render-pass-target)
   (depth-target
    :initarg :depth-target
    :initform nil
    :reader vulkan-render-pass-depth-target)
   (depth-store-op
    :initarg :depth-store-op
    :initform nil
    :reader vulkan-render-pass-depth-store-op)
   (pipeline
    :initform nil
    :accessor vulkan-render-pass-pipeline)
   (bind-group
    :initform nil
    :accessor vulkan-render-pass-bind-group)
   (vertex-buffers
    :initform (make-hash-table)
    :reader vulkan-render-pass-vertex-buffers)
   (state
    :initform :recording
    :accessor vulkan-render-pass-state)))

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
   (resources
    :initarg :resources
    :initform nil
    :reader vulkan-command-buffer-resources)
   (native-resources
    :initarg :native-resources
    :initform nil
    :reader vulkan-command-buffer-native-resources)
   (state
    :initform :ready
    :accessor vulkan-command-buffer-state)))

(defun vulkan-command-encoder-native-resources (encoder)
  (first (vulkan-command-encoder-native-resource-box encoder)))

(defun (setf vulkan-command-encoder-native-resources) (value encoder)
  (setf (first (vulkan-command-encoder-native-resource-box encoder)) value))

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

(defun install-vulkan-device-leak-finalizer (device)
  "Arrange to warn about and reclaim DEVICE if it is collected undestroyed.

The finalizer captures only native handles: capturing the queue wrapper
would keep DEVICE reachable through its own back reference forever.
Because every child resource's teardown closure captures the device
wrapper, this finalizer cannot run before theirs have."
  (let ((label (gpu-object-label device))
        (native-device (vulkan-handle device))
        (instance (vulkan-device-instance device))
        (messenger (vulkan-device-debug-messenger device))
        (timeline (vulkan-queue-timeline (vulkan-device-queue device)))
        (render-passes (vulkan-device-render-passes device)))
    (sb-ext:finalize
     device
     (lambda ()
       (note-gpu-resource-leak 'vulkan-gpu-device label)
       (with-vulkan-gpu-driver-environment
         (ignore-errors (lvk:device-wait-idle native-device))
         (maphash (lambda (format render-pass)
                    (declare (ignore format))
                    (lvk:destroy-render-pass native-device render-pass))
                  render-passes)
         (lvk:destroy-semaphore native-device timeline)
         (lvk:destroy-device native-device)
         (when messenger
           (ignore-errors (lvk:destroy-debug-messenger messenger)))
         (lvk:destroy-instance instance)))))
  device)

(defun make-vulkan-gpu-device
    (instance physical-device queue-family descriptor
     &key debug-messenger instance-extension-names enabled-extension-names)
  "Create GPU wrappers for an already selected Vulkan device and queue."
  (let ((native-device
          (lvk:create-device
           physical-device queue-family
           :enabled-extension-names enabled-extension-names))
        (timeline nil))
    (handler-case
        (let* ((native-queue
                 (lvk:get-device-queue native-device queue-family))
               (device
                 (make-instance
                  'vulkan-gpu-device
                  :label (gpu-descriptor-label descriptor)
                  :handle native-device
                  :instance instance
                  :debug-messenger debug-messenger
                  :instance-extension-names instance-extension-names
                  :device-extension-names enabled-extension-names
                  :physical-device physical-device
                  :queue-family queue-family))
               (queue
                 (progn
                   (setf timeline
                         (lvk:create-timeline-semaphore native-device))
                   (make-instance
                    'vulkan-gpu-queue
                    :label "default queue"
                    :handle native-queue
                    :device device
                    :family queue-family
                    :timeline timeline))))
          (setf (vulkan-device-queue device) queue)
          (install-vulkan-device-leak-finalizer device)
          device)
      (error (condition)
        (when timeline
          (lvk:destroy-semaphore native-device timeline))
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
                                  '(:copy-src :copy-dst :storage-binding
                                    :texture-binding :render-attachment)))
                        usages))
      (reject-gpu-request descriptor :unsupported-texture-usage usage))
    (remove-duplicates usages)))

(defun vulkan-gpu-format (format descriptor)
  (or (cdr (assoc format
                  '((:rgba8-unorm . :r8g8b8a8-unorm)
                    (:rgba8-unorm-srgb . :r8g8b8a8-srgb)
                    (:bgra8-unorm . :b8g8r8a8-unorm)
                    (:bgra8-unorm-srgb . :b8g8r8a8-srgb)
                    (:depth32-float . :d32-sfloat))))
      (reject-gpu-request
       descriptor :unsupported-texture-format
       format)))

(defun vulkan-texture-format (descriptor)
  (vulkan-gpu-format (texture-descriptor-format descriptor) descriptor))

(defun vulkan-depth-format-p (format)
  (eq format :depth32-float))

(defun vulkan-texture-aspect (texture)
  (if (vulkan-depth-format-p (gpu-texture-format texture)) :depth :color))

(defun vulkan-image-usage (usages format)
  (mapcar (lambda (usage)
            (ecase usage
              (:copy-src :transfer-src)
              (:copy-dst :transfer-dst)
              (:texture-binding :sampled)
              (:render-attachment
               (if (vulkan-depth-format-p format)
                   :depth-stencil-attachment
                   :color-attachment))
              (:storage-binding :storage)))
          usages))

(defun vulkan-buffer-usage (usages)
  (mapcar (lambda (usage)
            (ecase usage
              (:uniform :uniform)
              (:vertex :vertex)
              (:copy-dst :transfer-dst)))
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

(defun find-vulkan-upload-memory-type
    (device memory-requirements &optional (operation :write-texture))
  (let ((memory-types
          (lvk:physical-device-memory-types
           (vulkan-device-physical-device device)))
        (memory-type-bits
          (lvk:buffer-memory-requirements-memory-type-bits
           memory-requirements)))
    (or (loop for memory-type in memory-types
              for index from 0
              for flags = (lvk:physical-memory-type-flags memory-type)
              when (and (compatible-vulkan-memory-type-p
                         memory-type-bits index)
                        (member :host-visible flags)
                        (member :host-coherent flags))
                return index)
        (error 'vulkan-gpu-error
               :operation operation
               :reason :no-compatible-memory
               :details memory-requirements))))

(defmethod request-gpu-device
    ((provider vulkan-gpu-provider) &optional descriptor)
  "Create an owned Vulkan instance, logical device, and graphics queue."
  (with-vulkan-gpu-driver-environment
    (let ((descriptor (or descriptor (make-device-descriptor))))
      (check-vulkan-device-descriptor descriptor)
      (let ((instance nil)
            (debug-messenger nil)
            (native-device nil)
            (completed-p nil))
        (unwind-protect
             (multiple-value-bind (extensions flags)
                 (vulkan-provider-instance-options provider)
               (setf instance
                     (lvk:create-instance
                      :application-name
                      (vulkan-provider-application-name provider)
                      :flags flags
                      :enabled-extension-names extensions))
               (when (vulkan-provider-debug-callback provider)
                 (unless (member lvk:+debug-utils-extension-name+
                                 extensions :test #'string=)
                   (error 'vulkan-gpu-error
                          :operation :request-device
                          :reason :missing-debug-utils-extension))
                 (setf debug-messenger
                       (lvk:install-debug-messenger
                        instance
                        (vulkan-provider-debug-callback provider)
                        :severities
                        (vulkan-provider-debug-severities provider)
                        :types (vulkan-provider-debug-types provider))))
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
                          instance physical-device queue-family descriptor
                          :debug-messenger debug-messenger
                          :instance-extension-names extensions
                          :enabled-extension-names
                          (vulkan-gpu-device-extension-names provider))))
                   (setf native-device (vulkan-handle device)
                         completed-p t)
                   device)))
          (unless completed-p
            (unwind-protect
                 (when native-device
                   (lvk:destroy-device native-device))
              (unwind-protect
                   (when debug-messenger
                     (lvk:destroy-debug-messenger debug-messenger))
                (when instance
                  (lvk:destroy-instance instance))))))))))

(defmethod device-queue ((device vulkan-gpu-device))
  (ensure-live-vulkan-object device :device-queue)
  (vulkan-device-queue device))

(defmethod create
    ((device vulkan-gpu-device) (descriptor buffer-descriptor))
  "Create one persistently mapped, host-coherent uploadable buffer."
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-buffer)
    (let* ((size (buffer-descriptor-size descriptor))
           (raw-usage (buffer-descriptor-usage descriptor))
           (usage (typecase raw-usage
                    (keyword (list raw-usage))
                    (list (remove-duplicates raw-usage))
                    (vector (remove-duplicates (coerce raw-usage 'list)))
                    (otherwise nil))))
      (unless (and (typep size '(unsigned-byte 64)) (plusp size))
        (reject-gpu-request descriptor :invalid-buffer-size size))
      (unless (and usage
                   (every (lambda (value)
                            (member value '(:uniform :vertex :copy-dst)))
                          usage))
        (reject-gpu-request descriptor :unsupported-buffer-usage raw-usage))
      (let ((native-device (vulkan-handle device))
            (buffer nil)
            (memory nil)
            (mapped nil)
            (completed-p nil))
        (unwind-protect
             (progn
               (setf buffer
                     (lvk:create-buffer native-device size
                                        (vulkan-buffer-usage usage)))
               (let* ((requirements
                        (lvk:get-buffer-memory-requirements
                         native-device buffer))
                      (memory-type
                        (find-vulkan-upload-memory-type
                         device requirements :create-buffer)))
                 (setf memory
                       (lvk:allocate-memory
                        native-device
                        (lvk:buffer-memory-requirements-size requirements)
                        memory-type))
                 (lvk:bind-buffer-memory native-device buffer memory)
                 (setf mapped (lvk:map-memory native-device memory size)))
               (let ((object
                       (make-instance
                        'vulkan-gpu-buffer
                        :label (gpu-descriptor-label descriptor)
                        :size size :usage usage :handle buffer
                        :device device :memory memory :mapped mapped)))
                 (setf completed-p t)
                 object))
          (unless completed-p
            (when mapped (lvk:unmap-memory native-device memory))
            (when buffer (lvk:destroy-buffer native-device buffer))
            (when memory (lvk:free-memory native-device memory))))))))

(defmethod write-buffer
    ((buffer vulkan-gpu-buffer) data &key (offset 0))
  "Copy a one-dimensional single-float array into mapped coherent memory."
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object buffer :write-buffer)
    (unless (and (arrayp data) (= 1 (array-rank data))
                 (nth-value 0
                   (subtypep (array-element-type data) 'single-float)))
      (reject-gpu-request buffer :unsupported-buffer-data data))
    (unless (and (typep offset '(unsigned-byte 64))
                 (zerop (mod offset 4))
                 (<= (+ offset (* 4 (length data)))
                     (gpu-buffer-size buffer)))
      (reject-gpu-request buffer :buffer-write-out-of-bounds
                          (list :offset offset :length (* 4 (length data)))))
    (let ((destination
            (cffi:inc-pointer (vulkan-buffer-mapped buffer) offset)))
      (dotimes (index (length data))
        (setf (cffi:mem-aref destination :float index)
              (aref data index)))))
  buffer)

(defmethod read-buffer
    ((buffer vulkan-gpu-buffer) &key (offset 0) size)
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object buffer :read-buffer)
    (let ((size (or size (- (gpu-buffer-size buffer) offset))))
      (unless (and (typep offset '(unsigned-byte 64))
                   (typep size '(unsigned-byte 64))
                   (<= (+ offset size) (gpu-buffer-size buffer)))
        (reject-gpu-request buffer :buffer-read-out-of-bounds
                            (list :offset offset :size size)))
      (submitted-work-done
       (device-queue (vulkan-buffer-device buffer)))
      (let ((bytes (make-array size :element-type '(unsigned-byte 8)))
            (source (cffi:inc-pointer
                     (vulkan-buffer-mapped buffer) offset)))
        (dotimes (index size bytes)
          (setf (aref bytes index)
                (cffi:mem-aref source :uint8 index)))))))

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
                     :usage (vulkan-image-usage
                             usages (texture-descriptor-format descriptor))
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
                (vulkan-texture-vk-format texture)
                :aspect (vulkan-texture-aspect texture))
       :device device
       :texture texture))))

(defmethod create
    ((device vulkan-gpu-device) (descriptor sampler-descriptor))
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-sampler)
    (make-instance
     'vulkan-gpu-sampler
     :label (gpu-descriptor-label descriptor)
     :handle
     (lvk:create-sampler
      (vulkan-handle device)
      :mag-filter (sampler-descriptor-mag-filter descriptor)
      :min-filter (sampler-descriptor-min-filter descriptor)
      :mipmap-mode (sampler-descriptor-mipmap-filter descriptor)
      :address-mode-u (sampler-descriptor-address-mode-u descriptor)
      :address-mode-v (sampler-descriptor-address-mode-v descriptor)
      :address-mode-w (sampler-descriptor-address-mode-w descriptor)
      :compare (sampler-descriptor-compare descriptor))
     :device device)))

(defmethod create
    ((device vulkan-gpu-device) (descriptor shader-module-descriptor))
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-shader-module)
    (let ((code
            (case (shader-module-descriptor-language descriptor)
              (:spir-v (shader-module-descriptor-code descriptor))
              (:mathematical
               (let ((specification
                       (shader-module-descriptor-code descriptor)))
                 (unless (typep specification
                                'luv.spir-v:shader-specification)
                   (reject-gpu-request
                    descriptor :invalid-mathematical-shader specification))
                 (luv.spir-v:assemble-shader-specification specification)))
              (otherwise
               (reject-gpu-request
                descriptor :unsupported-shader-language
                (shader-module-descriptor-language descriptor))))))
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

(defun uniform-buffer-layout-entry (descriptor)
  (let ((entries (bind-group-layout-descriptor-entries descriptor)))
    (unless (and (listp entries) (= 1 (length entries))
                 (listp (first entries))
                 (eq :uniform-buffer (getf (first entries) :type))
                 (typep (getf (first entries) :binding)
                        '(unsigned-byte 32)))
      (reject-gpu-request descriptor :unsupported-bind-group-layout entries))
    (first entries)))

(defun sampled-texture-sampler-layout-entries (descriptor)
  (let* ((entries (bind-group-layout-descriptor-entries descriptor))
         (bindings (mapcar (lambda (entry) (getf entry :binding)) entries)))
    (unless (and (listp entries) (plusp (length entries))
                 (every (lambda (entry)
                          (and (listp entry)
                               (member (getf entry :type)
                                       '(:texture :sampler :uniform-buffer))
                               (typep (getf entry :binding)
                                      '(unsigned-byte 32))))
                        entries)
                 (find :texture entries
                       :key (lambda (entry) (getf entry :type)))
                 (find :sampler entries
                       :key (lambda (entry) (getf entry :type)))
                 (= (length bindings)
                    (length (remove-duplicates bindings))))
      (reject-gpu-request descriptor :unsupported-bind-group-layout entries))
    entries))

(defmethod create
    ((device vulkan-gpu-device)
     (descriptor bind-group-layout-descriptor))
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-bind-group-layout)
    (let ((entries (bind-group-layout-descriptor-entries descriptor)))
      (cond
        ((and (= 1 (length entries))
              (eq :storage-texture (getf (first entries) :type)))
         (let* ((entry (storage-texture-layout-entry descriptor))
                (binding (getf entry :binding)))
           (make-instance
            'vulkan-gpu-bind-group-layout
            :label (gpu-descriptor-label descriptor)
            :handle (lvk:create-storage-image-descriptor-set-layout
                     (vulkan-handle device) :binding binding)
            :device device :binding binding :entries entries)))
        ((and (= 1 (length entries))
              (eq :uniform-buffer (getf (first entries) :type)))
         (let* ((entry (uniform-buffer-layout-entry descriptor))
                (binding (getf entry :binding)))
           (make-instance
            'vulkan-gpu-bind-group-layout
            :label (gpu-descriptor-label descriptor)
            :handle (lvk:create-uniform-buffer-descriptor-set-layout
                     (vulkan-handle device) :binding binding)
            :device device :entries entries)))
        (t
         (let ((entries (sampled-texture-sampler-layout-entries descriptor)))
           (make-instance
            'vulkan-gpu-bind-group-layout
            :label (gpu-descriptor-label descriptor)
            :handle
            (lvk:create-texture-sampler-uniform-descriptor-set-layout
             (vulkan-handle device) entries)
            :device device :entries entries)))))))

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

(defun vulkan-render-pass-for-format
    (device gpu-format descriptor &optional depth-format
            (depth-store-op :discard))
  (let ((key (list gpu-format depth-format depth-store-op)))
    (or (gethash key (vulkan-device-render-passes device))
        (setf (gethash key (vulkan-device-render-passes device))
              (if gpu-format
                  (lvk:create-color-render-pass
                   (vulkan-handle device)
                   (vulkan-gpu-format gpu-format descriptor)
                   :depth-format
                   (and depth-format
                        (vulkan-gpu-format depth-format descriptor))
                   :depth-store-op depth-store-op)
                  (lvk:create-depth-render-pass
                   (vulkan-handle device)
                   (vulkan-gpu-format depth-format descriptor)
                   :depth-store-op depth-store-op))))))

(defun normalize-vulkan-vertex-buffers (descriptor buffers)
  (unless (listp buffers)
    (reject-gpu-request descriptor :invalid-vertex-buffers buffers))
  (loop for buffer in buffers
        for binding from 0
        for stride = (getf buffer :array-stride)
        for step-mode = (or (getf buffer :step-mode) :vertex)
        for attributes = (getf buffer :attributes)
        unless (and (typep stride '(unsigned-byte 32)) (plusp stride)
                    (member step-mode '(:vertex :instance))
                    (listp attributes) attributes
                    (every (lambda (attribute)
                             (and (typep (getf attribute :shader-location)
                                         '(unsigned-byte 32))
                                  (typep (getf attribute :offset)
                                         '(unsigned-byte 32))
                                  (eq :float32x3 (getf attribute :format))))
                           attributes))
          do (reject-gpu-request descriptor :invalid-vertex-buffer buffer)
        collect
        (list :binding binding :array-stride stride :step-mode step-mode
              :attributes
              (mapcar (lambda (attribute)
                        (list :shader-location
                              (getf attribute :shader-location)
                              :offset (getf attribute :offset)
                              :format :r32g32b32-sfloat))
                      attributes))))

(defmethod create
    ((device vulkan-gpu-device) (descriptor render-pipeline-descriptor))
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-render-pipeline)
    (let* ((layout (render-pipeline-descriptor-layout descriptor))
           (vertex (render-pipeline-descriptor-vertex descriptor))
           (fragment (render-pipeline-descriptor-fragment descriptor))
           (vertex-module (getf vertex :module))
           (vertex-buffers
             (normalize-vulkan-vertex-buffers
              descriptor (or (getf vertex :buffers) nil)))
           (fragment-module (getf fragment :module))
           (targets (getf fragment :targets))
           (format (getf (first targets) :format))
           (depth-stencil
             (render-pipeline-descriptor-depth-stencil descriptor))
           (depth-format (and depth-stencil (getf depth-stencil :format)))
           (depth-compare (and depth-stencil
                               (getf depth-stencil :depth-compare)))
           (depth-write-enabled
             (and depth-stencil
                  (getf depth-stencil :depth-write-enabled)))
           (depth-store-op
             (and depth-stencil
                  (or (getf depth-stencil :depth-store-op) :discard)))
           (topology
             (or (getf (render-pipeline-descriptor-primitive descriptor)
                       :topology)
                 :triangle-list)))
      (unless (and (typep layout 'vulkan-gpu-bind-group-layout)
                   (typep vertex-module 'vulkan-gpu-shader-module)
                   (or (and (typep fragment-module
                                    'vulkan-gpu-shader-module)
                            (= 1 (length targets))
                            format)
                       (and (null fragment-module)
                            (null targets)
                            depth-stencil))
                   (or (null depth-stencil)
                       (and (eq depth-format :depth32-float)
                            (member depth-compare
                                    '(:never :less :equal :less-or-equal
                                      :greater :not-equal :greater-or-equal
                                      :always))))
                   (member topology '(:triangle-list :triangle-strip)))
        (reject-gpu-request descriptor :unsupported-render-pipeline))
      (dolist (object (remove nil (list layout vertex-module fragment-module)))
        (ensure-vulkan-object-device
         object
         (etypecase object
           (vulkan-gpu-bind-group-layout
            (vulkan-bind-group-layout-device object))
           (vulkan-gpu-shader-module
            (vulkan-shader-module-device object)))
         device :create-render-pipeline))
      (let* ((render-pass
               (vulkan-render-pass-for-format
                device format descriptor depth-format depth-store-op))
             (pipeline-layout
               (lvk:create-pipeline-layout
                (vulkan-handle device) (vector (vulkan-handle layout))))
             (pipeline nil)
             (completed-p nil))
        (unwind-protect
             (progn
               (setf pipeline
                     (lvk:create-graphics-pipeline
                      (vulkan-handle device)
                      (vulkan-handle vertex-module)
                      (and fragment-module (vulkan-handle fragment-module))
                      pipeline-layout render-pass
                      :vertex-entry-point (or (getf vertex :entry-point) "main")
                      :fragment-entry-point
                      (or (getf fragment :entry-point) "main")
                      :topology topology
                      :vertex-buffers vertex-buffers
                      :depth-compare depth-compare
                      :depth-write-enabled depth-write-enabled)
                     completed-p t)
               (make-instance
                'vulkan-gpu-render-pipeline
                :label (gpu-descriptor-label descriptor)
                :handle pipeline :device device :layout layout
                :pipeline-layout pipeline-layout :render-pass render-pass
                :vertex-buffers vertex-buffers :depth-format depth-format))
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

(defun sampled-texture-sampler-bind-group-entries (descriptor layout)
  (let* ((entries (bind-group-descriptor-entries descriptor))
         (layout-entries (vulkan-bind-group-layout-entries layout)))
    (unless (= (length layout-entries) (length entries))
      (reject-gpu-request descriptor :unsupported-bind-group entries))
    (loop for layout-entry in layout-entries
          for entry = (find (getf layout-entry :binding) entries
                            :key (lambda (entry) (getf entry :binding)))
          unless (and entry
                      (ecase (getf layout-entry :type)
                        (:texture
                         (typep (getf entry :resource)
                                'vulkan-gpu-texture-view))
                        (:sampler
                         (typep (getf entry :resource)
                                'vulkan-gpu-sampler))
                        (:uniform-buffer
                         (typep (getf entry :resource)
                                'vulkan-gpu-buffer))))
            do (reject-gpu-request descriptor :unsupported-bind-group entries)
          collect (list layout-entry entry))))

(defun uniform-buffer-bind-group-entry (descriptor layout)
  (let* ((entries (bind-group-descriptor-entries descriptor))
         (layout-entry (first (vulkan-bind-group-layout-entries layout)))
         (entry (first entries)))
    (unless (and (= 1 (length entries))
                 (= (or (getf entry :binding) -1)
                    (getf layout-entry :binding))
                 (typep (getf entry :resource) 'vulkan-gpu-buffer))
      (reject-gpu-request descriptor :unsupported-bind-group entries))
    entry))

(defun create-vulkan-uniform-bind-group (device descriptor layout)
  (let* ((entry (uniform-buffer-bind-group-entry descriptor layout))
         (buffer (getf entry :resource))
         (binding (getf (first (vulkan-bind-group-layout-entries layout))
                        :binding)))
    (ensure-vulkan-object-device
     buffer (vulkan-buffer-device buffer) device :create-bind-group)
    (unless (member :uniform (gpu-buffer-usage buffer))
      (error 'gpu-usage-error
             :object buffer :operation :create-bind-group
             :required-usage :uniform
             :actual-usage (gpu-buffer-usage buffer)))
    (let ((pool (lvk:create-uniform-buffer-descriptor-pool
                 (vulkan-handle device)))
          (set nil)
          (completed-p nil))
      (unwind-protect
           (progn
             (setf set (lvk:allocate-descriptor-set
                        (vulkan-handle device) pool (vulkan-handle layout)))
             (lvk:update-uniform-buffer-descriptor
              (vulkan-handle device) set (vulkan-handle buffer)
              (gpu-buffer-size buffer) :binding binding)
             (setf completed-p t)
             (make-instance
              'vulkan-gpu-bind-group
              :label (gpu-descriptor-label descriptor)
              :handle set :device device :layout layout
              :buffers (list buffer)
              :descriptor-pool pool))
        (unless completed-p
          (lvk:destroy-descriptor-pool (vulkan-handle device) pool))))))

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
      (if (vulkan-bind-group-layout-binding layout)
          (let* ((entry (storage-texture-bind-group-entry descriptor layout))
                 (view (getf entry :resource))
                 (texture (gpu-texture-view-texture view)))
            (ensure-vulkan-object-device
             view (vulkan-texture-view-device view) device :create-bind-group)
            (unless (member :storage-binding (gpu-texture-usage texture))
              (error 'gpu-usage-error
                     :object texture :operation :create-bind-group
                     :required-usage :storage-binding
                     :actual-usage (gpu-texture-usage texture)))
            (let ((pool (lvk:create-storage-image-descriptor-pool
                         (vulkan-handle device)))
                  (set nil) (completed-p nil))
              (unwind-protect
                   (progn
                     (setf set (lvk:allocate-descriptor-set
                                (vulkan-handle device) pool
                                (vulkan-handle layout)))
                     (lvk:update-storage-image-descriptor
                      (vulkan-handle device) set (vulkan-handle view)
                      :binding (vulkan-bind-group-layout-binding layout))
                     (setf completed-p t)
                     (make-instance
                      'vulkan-gpu-bind-group
                      :label (gpu-descriptor-label descriptor)
                      :handle set :device device :layout layout
                      :texture-views (list view)
                      :descriptor-pool pool))
                (unless completed-p
                  (lvk:destroy-descriptor-pool
                   (vulkan-handle device) pool)))))
          (if (and (= 1 (length (vulkan-bind-group-layout-entries layout)))
                   (eq :uniform-buffer
                       (getf (first (vulkan-bind-group-layout-entries layout))
                             :type)))
              (create-vulkan-uniform-bind-group device descriptor layout)
              (let* ((pairs
                       (sampled-texture-sampler-bind-group-entries
                        descriptor layout))
                     (views nil)
                     (samplers nil)
                     (buffers nil)
                     (descriptor-entries nil))
                (dolist (pair pairs)
                  (destructuring-bind (layout-entry entry) pair
                    (let ((resource (getf entry :resource)))
                      (ecase (getf layout-entry :type)
                        (:texture
                         (ensure-vulkan-object-device
                          resource (vulkan-texture-view-device resource)
                          device :create-bind-group)
                         (let ((texture (gpu-texture-view-texture resource)))
                           (unless (member :texture-binding
                                           (gpu-texture-usage texture))
                             (error 'gpu-usage-error
                                    :object texture
                                    :operation :create-bind-group
                                    :required-usage :texture-binding
                                    :actual-usage
                                    (gpu-texture-usage texture))))
                         (push resource views)
                         (push `(:binding ,(getf layout-entry :binding)
                                 :type :texture
                                 :image-view ,(vulkan-handle resource))
                               descriptor-entries))
                        (:sampler
                         (ensure-vulkan-object-device
                          resource (vulkan-sampler-device resource)
                          device :create-bind-group)
                         (push resource samplers)
                         (push `(:binding ,(getf layout-entry :binding)
                                 :type :sampler
                                 :sampler ,(vulkan-handle resource))
                               descriptor-entries))
                        (:uniform-buffer
                         (ensure-vulkan-object-device
                          resource (vulkan-buffer-device resource)
                          device :create-bind-group)
                         (unless (member :uniform (gpu-buffer-usage resource))
                           (error 'gpu-usage-error
                                  :object resource
                                  :operation :create-bind-group
                                  :required-usage :uniform
                                  :actual-usage (gpu-buffer-usage resource)))
                         (push resource buffers)
                         (push `(:binding ,(getf layout-entry :binding)
                                 :type :uniform-buffer
                                 :buffer ,(vulkan-handle resource)
                                 :buffer-size ,(gpu-buffer-size resource))
                               descriptor-entries))))))
                (setf views (nreverse views)
                      samplers (nreverse samplers)
                      buffers (nreverse buffers)
                      descriptor-entries (nreverse descriptor-entries))
                (let ((pool
                        (lvk:create-texture-sampler-uniform-descriptor-pool
                         (vulkan-handle device) descriptor-entries))
                      (set nil) (completed-p nil))
                  (unwind-protect
                       (progn
                         (setf set
                               (lvk:allocate-descriptor-set
                                (vulkan-handle device) pool
                                (vulkan-handle layout)))
                         (lvk:update-texture-sampler-uniform-descriptors
                          (vulkan-handle device) set descriptor-entries)
                         (setf completed-p t)
                         (make-instance
                          'vulkan-gpu-bind-group
                          :label (gpu-descriptor-label descriptor)
                          :handle set :device device :layout layout
                          :texture-views views
                          :samplers samplers
                          :buffers buffers
                          :descriptor-pool pool))
                    (unless completed-p
                      (lvk:destroy-descriptor-pool
                       (vulkan-handle device) pool))))))))))

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
               (install-vulkan-encoder-leak-finalizer
                (make-instance
                 'vulkan-gpu-command-encoder
                 :label (gpu-descriptor-label descriptor)
                 :device device
                 :command-pool command-pool
                 :command-buffer command-buffer))))
        (unless completed-p
          (when command-pool
            (lvk:destroy-command-pool
             (vulkan-handle device) command-pool)))))))

(defun install-vulkan-encoder-leak-finalizer (encoder)
  "Arrange to warn about and reclaim ENCODER if it is collected while it
still owns its command pool.  FINISH and DESTROY transfer or release that
ownership and cancel this finalizer."
  (let ((device (vulkan-command-encoder-device encoder))
        (command-pool (vulkan-command-encoder-command-pool encoder))
        (box (vulkan-command-encoder-native-resource-box encoder))
        (label (gpu-object-label encoder)))
    (sb-ext:finalize
     encoder
     (lambda ()
       (note-gpu-resource-leak 'vulkan-gpu-command-encoder label)
       (with-vulkan-gpu-driver-environment
         (with-vulkan-queue-teardown (device native-device)
           (lvk:destroy-command-pool native-device command-pool)
           (dolist (resource (first box))
             (destroy-vulkan-command-native-resource
              native-device resource)))))))
  encoder)

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

(defun retain-vulkan-resource (encoder resource)
  "Record RESOURCE as a dependency of ENCODER's future submission."
  (when resource
    (setf (gethash resource (vulkan-command-encoder-resources encoder)) t))
  resource)

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
  (retain-vulkan-resource encoder texture)
  texture)

(defun vulkan-layout-access-and-stage (layout)
  (ecase layout
    (:undefined
     (values nil (list :top-of-pipe)))
    (:general
     (values (list :shader-read :shader-write)
             (list :compute-shader)))
    (:shader-read-only-optimal
     (values (list :shader-read)
             (list :vertex-shader :fragment-shader)))
    (:color-attachment-optimal
     (values (list :color-attachment-read :color-attachment-write)
             (list :color-attachment-output)))
    (:depth-stencil-attachment-optimal
     (values (list :depth-stencil-attachment-read
                   :depth-stencil-attachment-write)
             (list :early-fragment-tests :late-fragment-tests)))
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
           src-access dst-access src-stage dst-stage
           :aspect (vulkan-texture-aspect texture))))
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

(defmethod encode
    ((encoder vulkan-gpu-command-encoder)
     (command gpu-copy-texture-to-buffer-command))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-command-encoder-state encoder :encode)
    (ensure-no-active-vulkan-pass encoder :encode)
    (let* ((source
             (ensure-vulkan-texture-for-command
              encoder (gpu-copy-texture-to-buffer-command-source command)
              command :copy-src))
           (destination
             (gpu-copy-texture-to-buffer-command-destination command))
           (device (vulkan-command-encoder-device encoder))
           (size (gpu-texture-size source))
           (required-size (* 4 (first size) (second size))))
      (unless (and (typep destination 'vulkan-gpu-buffer)
                   (member :copy-dst (gpu-buffer-usage destination))
                   (<= required-size (gpu-buffer-size destination))
                   (member (gpu-texture-format source)
                           '(:rgba8-unorm :rgba8-unorm-srgb
                             :bgra8-unorm :bgra8-unorm-srgb)))
        (reject-gpu-request
         command :unsupported-texture-to-buffer-copy
         (list :source source :destination destination)))
      (ensure-vulkan-object-device
       destination (vulkan-buffer-device destination) device
       :copy-texture-to-buffer)
      (transition-vulkan-texture encoder source :transfer-src-optimal)
      (lvk:cmd-copy-image-to-buffer
       (vulkan-command-encoder-command-buffer encoder)
       (vulkan-handle source) :transfer-src-optimal
       (vulkan-handle destination) (first size) (second size))
      (retain-vulkan-resource encoder destination)))
  encoder)

(defun reject-texture-write (destination reason &optional details)
  (error 'gpu-request-error
         :operation :write-texture
         :descriptor destination
         :reason reason
         :details details))

(defun texture-write-components (value expected-length destination reason)
  (let ((components
          (typecase value
            (list value)
            (vector (coerce value 'list))
            (otherwise nil))))
    (unless (and (member (length components) expected-length)
                 (every (lambda (component)
                          (typep component '(unsigned-byte 32)))
                        components))
      (reject-texture-write destination reason value))
    components))

(defun check-vulkan-texture-write (device command)
  (let ((destination (gpu-write-texture-command-destination command))
        (data (gpu-write-texture-command-data command))
        (data-layout (gpu-write-texture-command-data-layout command))
        (size (gpu-write-texture-command-size command)))
    (unless (typep destination 'texture-copy)
      (reject-texture-write destination :invalid-texture-copy destination))
    (unless (typep data-layout 'texture-data-layout)
      (reject-texture-write destination :invalid-data-layout data-layout))
    (let* ((texture (texture-copy-texture destination))
         (origin
           (texture-write-components
            (texture-copy-origin destination) '(2 3) destination
            :invalid-origin))
         (extent
           (texture-write-components
            size '(2 3) destination :invalid-write-size)))
      (when (= 2 (length origin))
        (setf origin (append origin '(0))))
      (when (= 2 (length extent))
        (setf extent (append extent '(1))))
      (unless (and (typep texture 'vulkan-gpu-texture)
                   (= 0 (texture-copy-mip-level destination))
                   (eq :all (texture-copy-aspect destination))
                   (= 0 (third origin))
                   (= 1 (third extent)))
        (reject-texture-write destination :unsupported-texture-copy destination))
      (ensure-live-vulkan-object device :write-texture)
      (ensure-vulkan-object-device
       texture (vulkan-texture-device texture) device
       :write-texture)
      (unless (member :copy-dst (gpu-texture-usage texture))
        (error 'gpu-usage-error
               :object texture :operation :write-texture
               :required-usage :copy-dst
               :actual-usage (gpu-texture-usage texture)))
      (unless (member (gpu-texture-format texture)
                      '(:rgba8-unorm :rgba8-unorm-srgb
                        :bgra8-unorm :bgra8-unorm-srgb))
        (reject-texture-write
         destination :unsupported-texture-format
         (gpu-texture-format texture)))
      (unless (and (plusp (first extent)) (plusp (second extent))
                   (<= (+ (first origin) (first extent))
                       (first (gpu-texture-size texture)))
                   (<= (+ (second origin) (second extent))
                       (second (gpu-texture-size texture))))
        (reject-texture-write destination :write-out-of-bounds
                              (list :origin origin :size extent)))
      (unless (and (arrayp data)
                   (= 2 (array-rank data))
                   (nth-value 0
                     (subtypep (array-element-type data)
                               '(unsigned-byte 32)))
                   (>= (array-dimension data 0) (second extent))
                   (>= (array-dimension data 1) (first extent)))
        (reject-texture-write destination :unsupported-texture-data data))
      (let* ((width (first extent))
             (height (second extent))
             (offset (texture-data-layout-offset data-layout))
             (bytes-per-row
               (or (texture-data-layout-bytes-per-row data-layout)
                   (* width 4)))
             (rows-per-image
               (or (texture-data-layout-rows-per-image data-layout) height)))
        (unless (and (typep offset '(unsigned-byte 64))
                     (zerop (mod offset 4))
                     (typep bytes-per-row '(unsigned-byte 32))
                     (>= bytes-per-row (* width 4))
                     (zerop (mod bytes-per-row 4))
                     (typep rows-per-image '(unsigned-byte 32))
                     (>= rows-per-image height))
          (reject-texture-write destination :invalid-data-layout data-layout))
        (values texture origin extent offset bytes-per-row rows-per-image
                data destination)))))

(defun copy-texture-words-to-mapped-memory
    (data pointer width height offset bytes-per-row)
  (dotimes (row height)
    (let ((destination
            (cffi:inc-pointer pointer (+ offset (* row bytes-per-row)))))
      (dotimes (column width)
        (setf (cffi:mem-aref destination :uint32 column)
              (row-major-aref
               data (+ (* row (array-dimension data 1)) column)))))))

(defun record-vulkan-texture-write (encoder command)
  "Lower one queue texture write through a private Vulkan command encoder."
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-command-encoder-state encoder :encode)
    (ensure-no-active-vulkan-pass encoder :encode)
    (multiple-value-bind
          (texture origin extent offset bytes-per-row rows-per-image
                   data destination)
        (check-vulkan-texture-write
         (vulkan-command-encoder-device encoder) command)
      (declare (ignore rows-per-image destination))
      (let* ((device (vulkan-command-encoder-device encoder))
             (native-device (vulkan-handle device))
             (width (first extent))
             (height (second extent))
             (data-size (+ offset (* (1- height) bytes-per-row)
                           (* width 4)))
             (buffer nil)
             (memory nil)
             (mapped nil)
             (retained-p nil))
        (unwind-protect
             (progn
               (setf buffer
                     (lvk:create-buffer
                      native-device data-size '(:transfer-src)))
               (let* ((requirements
                        (lvk:get-buffer-memory-requirements
                         native-device buffer))
                      (memory-type
                        (find-vulkan-upload-memory-type device requirements)))
                 (setf memory
                       (lvk:allocate-memory
                        native-device
                        (lvk:buffer-memory-requirements-size requirements)
                        memory-type))
                 (lvk:bind-buffer-memory native-device buffer memory)
                 (setf mapped (lvk:map-memory native-device memory data-size))
                 (copy-texture-words-to-mapped-memory
                  data mapped width height offset bytes-per-row)
                 (lvk:unmap-memory native-device memory)
                 (setf mapped nil))
               (ensure-vulkan-texture-for-command
                encoder texture command :copy-dst)
               (transition-vulkan-texture
                encoder texture :transfer-dst-optimal)
               (lvk:cmd-copy-buffer-to-image
                (vulkan-command-encoder-command-buffer encoder)
                buffer (vulkan-handle texture) :transfer-dst-optimal
                width height
                :buffer-offset offset
                :buffer-row-length (/ bytes-per-row 4)
                :buffer-image-height height
                :x (first origin) :y (second origin))
               (push (list :upload-buffer buffer memory)
                     (vulkan-command-encoder-native-resources encoder))
               (setf retained-p t
                     buffer nil
                     memory nil))
          (when mapped (lvk:unmap-memory native-device memory))
          (unless retained-p
            (when buffer (lvk:destroy-buffer native-device buffer))
            (when memory (lvk:free-memory native-device memory)))))))
  encoder)

(defmethod enqueue
    ((queue vulkan-gpu-queue) (command gpu-write-texture-command))
  "Issue one WebGPU queue write using Vulkan's portable staging path.

The host data is copied into coherent staging memory before returning.  The
staging buffer is retained by the private command buffer until submission has
completed.  Vulkan 1.4 host image copies may provide a more direct optional
lowering later without changing this queue-level operation."
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object queue :enqueue)
    (let ((encoder nil)
          (commands nil))
      (unwind-protect
           (progn
             (setf encoder
                   (create (vulkan-queue-device queue)
                           (make-command-encoder-descriptor)))
             (record-vulkan-texture-write encoder command)
             (setf commands (finish encoder))
             (submit queue commands)
             queue)
        (when commands (destroy commands))
        (when encoder (destroy encoder))))))

(defun normalize-render-pass-color (descriptor color)
  (let ((components
          (typecase color
            (list color)
            (vector (coerce color 'list))
            (otherwise nil))))
    (unless (and (= 4 (length components)) (every #'realp components))
      (reject-gpu-request descriptor :invalid-clear-color color))
    (map 'vector (lambda (value) (coerce value 'single-float)) components)))

(defun normalize-render-pass-depth (descriptor depth)
  (unless (and (realp depth) (<= 0 depth 1))
    (reject-gpu-request descriptor :invalid-clear-depth depth))
  (coerce depth 'single-float))

(defmethod begin-render-pass
    ((encoder vulkan-gpu-command-encoder)
     (descriptor render-pass-descriptor))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-command-encoder-state encoder :begin-render-pass)
    (ensure-no-active-vulkan-pass encoder :begin-render-pass)
    (let* ((attachments (render-pass-descriptor-color-attachments descriptor))
           (attachment (and (= 1 (length attachments)) (first attachments)))
           (view (and attachment (getf attachment :view)))
           (target (and (typep view 'vulkan-gpu-texture-view)
                        (gpu-texture-view-texture view)))
           (depth-attachment
             (render-pass-descriptor-depth-stencil-attachment descriptor))
           (depth-view (and depth-attachment
                            (getf depth-attachment :view)))
           (depth-target
             (and (typep depth-view 'vulkan-gpu-texture-view)
                  (gpu-texture-view-texture depth-view)))
           (clear-color
             (and attachment
                  (normalize-render-pass-color
                   descriptor
                   (or (getf attachment :clear-value) #(0 0 0 1)))))
           (clear-depth
             (and depth-attachment
                  (normalize-render-pass-depth
                   descriptor
                   (or (getf depth-attachment :depth-clear-value) 1.0))))
           (depth-store-op
             (and depth-attachment
                  (getf depth-attachment :depth-store-op))))
      (unless (and (or (and attachment target
                            (eq :clear (getf attachment :load-op))
                            (eq :store (getf attachment :store-op)))
                       (and (null attachments) (null attachment)))
                   (or (null depth-attachment)
                       (and depth-target
                            (eq :depth32-float
                                (gpu-texture-format depth-target))
                            (eq :clear
                                (getf depth-attachment :depth-load-op))
                            (member depth-store-op '(:discard :store)))))
        (reject-gpu-request descriptor :unsupported-render-pass))
      (let* ((device (vulkan-command-encoder-device encoder))
             (size (gpu-texture-size (or target depth-target)))
             (render-pass
               (vulkan-render-pass-for-format
                device (and target (gpu-texture-format target)) descriptor
                (and depth-target (gpu-texture-format depth-target))
                (or depth-store-op :discard)))
             (framebuffer nil)
             (completed-p nil))
        (when target
          (ensure-vulkan-object-device
           view (vulkan-texture-view-device view) device :begin-render-pass))
        (when depth-target
          (unless (or (null target)
                      (equal size (gpu-texture-size depth-target)))
            (reject-gpu-request descriptor :mismatched-depth-size
                                (gpu-texture-size depth-target)))
          (ensure-vulkan-object-device
           depth-view (vulkan-texture-view-device depth-view)
           device :begin-render-pass)
          (retain-vulkan-resource encoder depth-view)
          (ensure-vulkan-texture-for-command
           encoder depth-target descriptor :render-attachment)
          (transition-vulkan-texture
           encoder depth-target :depth-stencil-attachment-optimal))
        (when target
          (retain-vulkan-resource encoder view)
          (ensure-vulkan-texture-for-command
           encoder target descriptor :render-attachment)
          (transition-vulkan-texture
           encoder target :color-attachment-optimal))
        (unwind-protect
             (progn
               (setf framebuffer
                     (lvk:create-framebuffer
                      (vulkan-handle device) render-pass
                      (and view (vulkan-handle view))
                      (first size) (second size)
                      :depth-view (and depth-view
                                       (vulkan-handle depth-view))))
               (if target
                   (lvk:cmd-begin-color-render-pass
                    (vulkan-command-encoder-command-buffer encoder)
                    render-pass framebuffer (first size) (second size)
                    clear-color :depth-clear-value clear-depth)
                   (lvk:cmd-begin-depth-render-pass
                    (vulkan-command-encoder-command-buffer encoder)
                    render-pass framebuffer (first size) (second size)
                    clear-depth))
               (lvk:cmd-set-viewport-and-scissor
                (vulkan-command-encoder-command-buffer encoder)
                (first size) (second size))
               (push (list :framebuffer framebuffer)
                     (vulkan-command-encoder-native-resources encoder))
               (let ((pass
                       (make-instance
                        'vulkan-gpu-render-pass-encoder
                        :encoder encoder :framebuffer framebuffer
                        :target target :depth-target depth-target
                        :depth-store-op depth-store-op)))
                 (setf (vulkan-command-encoder-active-pass encoder) pass
                       completed-p t)
                 pass))
          (unless completed-p
            (when framebuffer
              (lvk:destroy-framebuffer
               (vulkan-handle device) framebuffer))))))))

(defun ensure-vulkan-render-pass-state (pass operation)
  (unless (eq :recording (vulkan-render-pass-state pass))
    (error 'gpu-invalid-state-error
           :object pass :operation operation
           :state (vulkan-render-pass-state pass)
           :expected-state :recording))
  (let ((encoder (vulkan-render-pass-command-encoder pass)))
    (unless (eq pass (vulkan-command-encoder-active-pass encoder))
      (error 'gpu-invalid-state-error
             :object pass :operation operation
             :state :detached :expected-state :active)))
  pass)

(defmethod encode
    ((pass vulkan-gpu-render-pass-encoder)
     (command gpu-set-pipeline-command))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-render-pass-state pass :set-pipeline)
    (let* ((pipeline (gpu-set-pipeline-command-pipeline command))
           (encoder (vulkan-render-pass-command-encoder pass))
           (device (vulkan-command-encoder-device encoder)))
      (unless (typep pipeline 'vulkan-gpu-render-pipeline)
        (reject-gpu-request command :incompatible-render-pipeline pipeline))
      (ensure-vulkan-object-device
       pipeline (vulkan-render-pipeline-device pipeline) device
       :set-pipeline)
      (unless (eq (vulkan-render-pipeline-render-pass pipeline)
                  (vulkan-render-pass-for-format
                   device
                   (and (vulkan-render-pass-target pass)
                        (gpu-texture-format
                         (vulkan-render-pass-target pass)))
                   pass
                   (and (vulkan-render-pass-depth-target pass)
                        (gpu-texture-format
                         (vulkan-render-pass-depth-target pass)))
                   (or (vulkan-render-pass-depth-store-op pass)
                       :discard)))
        (reject-gpu-request pipeline :incompatible-render-pass pass))
      (lvk:cmd-bind-graphics-pipeline
       (vulkan-command-encoder-command-buffer encoder)
       (vulkan-handle pipeline))
      (retain-vulkan-resource encoder pipeline)
      (setf (vulkan-render-pass-pipeline pass) pipeline)))
  pass)

(defmethod encode
    ((pass vulkan-gpu-render-pass-encoder)
     (command gpu-set-bind-group-command))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-render-pass-state pass :set-bind-group)
    (let ((index (gpu-set-bind-group-command-index command))
          (bind-group (gpu-set-bind-group-command-bind-group command)))
      (unless (zerop index)
        (reject-gpu-request command :unsupported-bind-group-index index))
      (unless (typep bind-group 'vulkan-gpu-bind-group)
        (reject-gpu-request command :incompatible-bind-group bind-group))
      (let* ((encoder (vulkan-render-pass-command-encoder pass))
           (device (vulkan-command-encoder-device encoder))
           (pipeline (or (vulkan-render-pass-pipeline pass)
                         (error 'gpu-invalid-state-error
                                :object pass :operation :set-bind-group
                                :state :no-pipeline
                                :expected-state :pipeline-bound))))
        (ensure-vulkan-object-device
         bind-group (vulkan-bind-group-device bind-group) device
         :set-bind-group)
        (unless (eq (vulkan-bind-group-layout bind-group)
                    (vulkan-render-pipeline-bind-group-layout pipeline))
          (reject-gpu-request bind-group :incompatible-pipeline-layout pipeline))
        (dolist (buffer (vulkan-bind-group-buffers bind-group))
          (ensure-vulkan-object-device
           buffer (vulkan-buffer-device buffer)
           device :set-bind-group))
        (dolist (texture-view (vulkan-bind-group-texture-views bind-group))
          (let ((texture (gpu-texture-view-texture texture-view)))
            (ensure-vulkan-texture-for-command
             encoder texture pass :texture-binding)
            (transition-vulkan-texture
             encoder texture :shader-read-only-optimal)))
        (lvk:cmd-bind-graphics-descriptor-set
         (vulkan-command-encoder-command-buffer encoder)
         (vulkan-render-pipeline-layout pipeline)
         (vulkan-handle bind-group))
        (retain-vulkan-resource encoder bind-group)
        (dolist (texture-view (vulkan-bind-group-texture-views bind-group))
          (retain-vulkan-resource encoder texture-view))
        (dolist (sampler (vulkan-bind-group-samplers bind-group))
          (retain-vulkan-resource encoder sampler))
        (dolist (buffer (vulkan-bind-group-buffers bind-group))
          (retain-vulkan-resource encoder buffer))
        (setf (vulkan-render-pass-bind-group pass) bind-group))))
  pass)

(defmethod encode
    ((pass vulkan-gpu-render-pass-encoder)
     (command gpu-set-vertex-buffer-command))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-render-pass-state pass :set-vertex-buffer)
    (let* ((slot (gpu-set-vertex-buffer-command-slot command))
           (buffer (gpu-set-vertex-buffer-command-buffer command))
           (offset (gpu-set-vertex-buffer-command-offset command))
           (encoder (vulkan-render-pass-command-encoder pass))
           (device (vulkan-command-encoder-device encoder)))
      (unless (and (typep slot '(unsigned-byte 32))
                   (typep buffer 'vulkan-gpu-buffer)
                   (typep offset '(unsigned-byte 64))
                   (zerop (mod offset 4))
                   (< offset (gpu-buffer-size buffer)))
        (reject-gpu-request command :invalid-vertex-buffer-binding
                            (list slot buffer offset)))
      (ensure-vulkan-object-device
       buffer (vulkan-buffer-device buffer) device :set-vertex-buffer)
      (unless (member :vertex (gpu-buffer-usage buffer))
        (error 'gpu-usage-error
               :object buffer :operation :set-vertex-buffer
               :required-usage :vertex
               :actual-usage (gpu-buffer-usage buffer)))
      (lvk:cmd-bind-vertex-buffer
       (vulkan-command-encoder-command-buffer encoder)
       slot (vulkan-handle buffer) offset)
      (retain-vulkan-resource encoder buffer)
      (setf (gethash slot (vulkan-render-pass-vertex-buffers pass)) buffer)))
  pass)

(defmethod encode
    ((pass vulkan-gpu-render-pass-encoder)
     (command gpu-draw-command))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-render-pass-state pass :draw)
    (unless (and (vulkan-render-pass-pipeline pass)
                 (vulkan-render-pass-bind-group pass)
                 (every (lambda (description)
                          (gethash (getf description :binding)
                                   (vulkan-render-pass-vertex-buffers pass)))
                        (vulkan-render-pipeline-vertex-buffers
                         (vulkan-render-pass-pipeline pass))))
      (error 'gpu-invalid-state-error
             :object pass :operation :draw
             :state :incomplete-bindings
             :expected-state :pipeline-bind-group-and-vertex-buffers-bound))
    (let ((vertex-count (gpu-draw-command-vertex-count command))
          (instance-count (gpu-draw-command-instance-count command))
          (first-vertex (gpu-draw-command-first-vertex command))
          (first-instance (gpu-draw-command-first-instance command)))
      (unless (every (lambda (value) (typep value '(unsigned-byte 32)))
                     (list vertex-count instance-count
                           first-vertex first-instance))
        (reject-gpu-request
         command :invalid-draw-arguments
         (list vertex-count instance-count first-vertex first-instance)))
      (lvk:cmd-draw
       (vulkan-command-encoder-command-buffer
        (vulkan-render-pass-command-encoder pass))
       vertex-count instance-count first-vertex first-instance)))
  pass)

(defmethod end-pass ((pass vulkan-gpu-render-pass-encoder))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-render-pass-state pass :end-pass)
    (let ((encoder (vulkan-render-pass-command-encoder pass)))
      (lvk:cmd-end-render-pass
       (vulkan-command-encoder-command-buffer encoder))
      (setf (vulkan-command-encoder-active-pass encoder) nil
            (vulkan-render-pass-state pass) :ended)))
  (values))

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

(defmethod encode
    ((pass vulkan-gpu-compute-pass-encoder)
     (command gpu-set-pipeline-command))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-compute-pass-state pass :set-pipeline)
    (let* ((pipeline (gpu-set-pipeline-command-pipeline command))
           (encoder (vulkan-compute-pass-command-encoder pass))
           (device (vulkan-command-encoder-device encoder)))
      (unless (typep pipeline 'vulkan-gpu-compute-pipeline)
        (reject-gpu-request command :incompatible-compute-pipeline pipeline))
      (ensure-vulkan-object-device
       pipeline (vulkan-compute-pipeline-device pipeline) device
       :set-pipeline)
      (lvk:cmd-bind-compute-pipeline
       (vulkan-command-encoder-command-buffer encoder)
       (vulkan-handle pipeline))
      (retain-vulkan-resource encoder pipeline)
      (setf (vulkan-compute-pass-pipeline pass) pipeline)))
  pass)

(defmethod encode
    ((pass vulkan-gpu-compute-pass-encoder)
     (command gpu-set-bind-group-command))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-compute-pass-state pass :set-bind-group)
    (let ((index (gpu-set-bind-group-command-index command))
          (bind-group (gpu-set-bind-group-command-bind-group command)))
      (unless (zerop index)
        (reject-gpu-request command :unsupported-bind-group-index index))
      (unless (typep bind-group 'vulkan-gpu-bind-group)
        (reject-gpu-request command :incompatible-bind-group bind-group))
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
        (dolist (buffer (vulkan-bind-group-buffers bind-group))
          (ensure-vulkan-object-device
           buffer (vulkan-buffer-device buffer)
           device :set-bind-group))
        (dolist (texture-view (vulkan-bind-group-texture-views bind-group))
          (let ((texture (gpu-texture-view-texture texture-view)))
            (ensure-vulkan-texture-for-command
             encoder texture pass :storage-binding)
            (transition-vulkan-texture encoder texture :general)))
        (lvk:cmd-bind-compute-descriptor-set
         (vulkan-command-encoder-command-buffer encoder)
         (vulkan-compute-pipeline-layout pipeline)
         (vulkan-handle bind-group))
        (retain-vulkan-resource encoder bind-group)
        (dolist (texture-view (vulkan-bind-group-texture-views bind-group))
          (retain-vulkan-resource encoder texture-view))
        (dolist (sampler (vulkan-bind-group-samplers bind-group))
          (retain-vulkan-resource encoder sampler))
        (dolist (buffer (vulkan-bind-group-buffers bind-group))
          (retain-vulkan-resource encoder buffer))
        (setf (vulkan-compute-pass-bind-group pass) bind-group))))
  pass)

(defmethod encode
    ((pass vulkan-gpu-compute-pass-encoder)
     (command gpu-dispatch-workgroups-command))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-compute-pass-state pass :dispatch-workgroups)
    (unless (and (vulkan-compute-pass-pipeline pass)
                 (vulkan-compute-pass-bind-group pass))
      (error 'gpu-invalid-state-error
             :object pass
             :operation :dispatch-workgroups
             :state :incomplete-bindings
             :expected-state :pipeline-and-bind-group-bound))
    (let ((x (gpu-dispatch-workgroups-command-x command))
          (y (gpu-dispatch-workgroups-command-y command))
          (z (gpu-dispatch-workgroups-command-z command)))
      (unless (every (lambda (value)
                       (typep value '(unsigned-byte 32)))
                     (list x y z))
        (reject-gpu-request command :invalid-workgroup-count (list x y z)))
      (lvk:cmd-dispatch
       (vulkan-command-encoder-command-buffer
        (vulkan-compute-pass-command-encoder pass))
       x y z)))
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
      ;; Ownership of the pool and native resources moves to the command
      ;; buffer, whose own finalizer takes over leak protection.
      (sb-ext:cancel-finalization encoder)
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
       (hash-table-keys (vulkan-command-encoder-textures encoder))
       :resources
       (hash-table-keys (vulkan-command-encoder-resources encoder))
       :native-resources
       (prog1 (vulkan-command-encoder-native-resources encoder)
         (setf (vulkan-command-encoder-native-resources encoder) nil))))))

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

(defun vulkan-queue-completed-frontier (queue)
  "Return the index of the newest submission the GPU has fully completed."
  (lvk:semaphore-counter-value
   (vulkan-handle (vulkan-queue-device queue))
   (vulkan-queue-timeline queue)))

(defun wait-for-vulkan-submission (queue index)
  "Block until QUEUE's completion frontier reaches INDEX."
  (lvk:wait-semaphore-value
   (vulkan-handle (vulkan-queue-device queue))
   (vulkan-queue-timeline queue)
   index)
  (values))

(defun maintain-vulkan-queue (queue)
  "Retire live submissions the completion frontier has passed.

Retiring a submission drops the queue's references to everything it
retained and performs the native teardown of resources whose DESTROY was
deferred while this submission was still in flight."
  (with-vulkan-gpu-driver-environment
    (sb-thread:with-recursive-lock ((vulkan-queue-lock queue))
      (let ((frontier (vulkan-queue-completed-frontier queue)))
        (loop while (and (vulkan-queue-live-submissions queue)
                         (<= (vulkan-gpu-submission-index
                              (first (vulkan-queue-live-submissions queue)))
                             frontier))
              do (let ((record (pop (vulkan-queue-live-submissions queue))))
                   (dolist (resource
                            (vulkan-gpu-submission-pending-destroys record))
                     (destroy-vulkan-native resource))))
        frontier))))

(defgeneric destroy-vulkan-native (object)
  (:documentation "Perform OBJECT's native Vulkan teardown unconditionally.

Called either directly by DESTROY when nothing submitted still uses
OBJECT, or by queue maintenance once the completion frontier passes its
last use.  Callers are responsible for the logical destroyed mark."))

(defun vulkan-destroy-or-defer (resource device)
  "Destroy RESOURCE's native objects now, or once its last use completes.

The public DESTROY has already marked RESOURCE destroyed, so no new
submissions can mention it; this decides only when the native teardown is
provably safe."
  (let ((queue (vulkan-device-queue device))
        (last-use (vulkan-object-last-submission resource)))
    (if (or (null queue)
            (vulkan-object-destroyed-p device)
            (zerop last-use))
        (destroy-vulkan-native resource)
        (sb-thread:with-recursive-lock ((vulkan-queue-lock queue))
          (let ((record
                  (and (> last-use (vulkan-queue-completed-frontier queue))
                       (find last-use (vulkan-queue-live-submissions queue)
                             :key #'vulkan-gpu-submission-index))))
            (if record
                (push resource
                      (vulkan-gpu-submission-pending-destroys record))
                (destroy-vulkan-native resource))))))
  (values))

(defun submit-vulkan-command-buffers
    (queue command-buffers &key (wait-semaphores #())
                                (signal-semaphores #())
                                wait-for-completion)
  "Submit one WebGPU-style batch and track it on the queue's frontier.

WAIT-SEMAPHORES and SIGNAL-SEMAPHORES are LVK semaphore submit entries of
the form (SEMAPHORE STAGES &optional VALUE).  Every submission additionally
signals the queue's timeline semaphore with a fresh submission index, which
is returned.  The submission record retains the command buffers and every
resource they captured until the frontier passes the index, so callers may
destroy any of them immediately after this returns."
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object queue :submit)
    (let ((index nil))
      (sb-thread:with-recursive-lock ((vulkan-queue-lock queue))
        (loop for command-buffer across command-buffers
              do (check-vulkan-command-buffer-for-submit
                  queue command-buffer))
        (let ((texture-layouts
                (vulkan-submitted-texture-layouts command-buffers)))
          (when (plusp (length command-buffers))
            (setf index (1+ (vulkan-queue-submission-counter queue)))
            (lvk:submit-command-buffers
             (vulkan-handle queue)
             (map 'vector #'vulkan-handle command-buffers)
             :wait-semaphores wait-semaphores
             :signal-semaphores
             (concatenate
              'vector signal-semaphores
              (vector (list (vulkan-queue-timeline queue)
                            '(:all-commands)
                            index))))
            (setf (vulkan-queue-submission-counter queue) index)
            (let ((resources '()))
              (loop for command-buffer across command-buffers
                    do (setf (vulkan-command-buffer-state command-buffer)
                             :submitted
                             (vulkan-object-last-submission command-buffer)
                             index)
                       (dolist (resource
                                (vulkan-command-buffer-resources
                                 command-buffer))
                         (setf (vulkan-object-last-submission resource)
                               index)
                         (push resource resources)))
              (maphash (lambda (texture layout)
                         (setf (vulkan-texture-layout texture) layout))
                       texture-layouts)
              (setf (vulkan-queue-live-submissions queue)
                    (nconc (vulkan-queue-live-submissions queue)
                           (list (make-vulkan-gpu-submission
                                  :index index
                                  :command-buffers command-buffers
                                  :resources resources))))))))
      (when index
        (when wait-for-completion
          (wait-for-vulkan-submission queue index))
        (maintain-vulkan-queue queue))
      index)))

(defmethod submit ((queue vulkan-gpu-queue) (command-buffers vector))
  "Schedule one WebGPU-style batch and return its submission index."
  (submit-vulkan-command-buffers queue command-buffers))

(defmethod submitted-work-done ((queue vulkan-gpu-queue))
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object queue :submitted-work-done)
    (let ((counter
            (sb-thread:with-recursive-lock ((vulkan-queue-lock queue))
              (vulkan-queue-submission-counter queue))))
      (when (plusp counter)
        (wait-for-vulkan-submission queue counter))
      (maintain-vulkan-queue queue)))
  (values))

(defun destroy-vulkan-command-native-resource (native-device resource)
  (ecase (first resource)
    (:framebuffer
     (lvk:destroy-framebuffer native-device (second resource)))
    (:upload-buffer
     (lvk:destroy-buffer native-device (second resource))
     (lvk:free-memory native-device (third resource))))
  (values))

(defun destroy-vulkan-command-native-resources (device resources)
  (unless (vulkan-object-destroyed-p device)
    (dolist (resource resources)
      (destroy-vulkan-command-native-resource
       (vulkan-handle device) resource)))
  (values))

(defmethod destroy ((encoder vulkan-gpu-command-encoder))
  (with-vulkan-gpu-driver-environment
    (sb-ext:cancel-finalization encoder)
    (when (eq :recording (vulkan-command-encoder-state encoder))
      (let ((device (vulkan-command-encoder-device encoder))
            (command-pool (vulkan-command-encoder-command-pool encoder)))
        (when (and command-pool
                   (not (vulkan-object-destroyed-p device)))
          (lvk:destroy-command-pool
           (vulkan-handle device) command-pool))
        (destroy-vulkan-command-native-resources
         device (vulkan-command-encoder-native-resources encoder))
        (setf (vulkan-command-encoder-native-resources encoder) nil)
        (setf (vulkan-command-encoder-command-pool encoder) nil)))
    (setf (vulkan-command-encoder-state encoder) :destroyed))
  (values))

(defmacro define-vulkan-resource-destroy
    ((class variable) device-form bindings &body native-teardown)
  "Define DESTROY, DESTROY-VULKAN-NATIVE, and the teardown closure for CLASS.

DESTROY marks the wrapper destroyed immediately, cancels its leak
finalizer, then either performs NATIVE-TEARDOWN now or defers it until
the completion frontier passes the object's last submitted use.

BINDINGS extract every native handle NATIVE-TEARDOWN needs, so the
teardown closure captures raw handles and the device wrapper rather than
VARIABLE itself and can therefore serve as the wrapper's leak finalizer.
NATIVE-TEARDOWN runs only while the device is alive, under the queue
teardown lock, with DEVICE anaphorically bound to the native handle."
  (let ((device-object (gensym "DEVICE-OBJECT")))
    `(progn
       (defmethod vulkan-native-teardown-closure ((,variable ,class))
         (let ((,device-object ,device-form)
               ,@bindings)
           (lambda ()
             (with-vulkan-queue-teardown (,device-object device)
               ,@native-teardown))))
       (defmethod destroy-vulkan-native ((,variable ,class))
         (funcall (vulkan-native-teardown-closure ,variable)))
       (defmethod destroy ((,variable ,class))
         (with-vulkan-gpu-driver-environment
           (unless (vulkan-object-destroyed-p ,variable)
             (setf (vulkan-object-destroyed-p ,variable) t)
             (sb-ext:cancel-finalization ,variable)
             (vulkan-destroy-or-defer ,variable ,device-form)))
         (values))
       ',class)))

(define-vulkan-resource-destroy
    (vulkan-gpu-command-buffer command-buffer)
    (vulkan-command-buffer-device command-buffer)
    ((command-pool (vulkan-command-buffer-command-pool command-buffer))
     (resources (vulkan-command-buffer-native-resources command-buffer)))
  (lvk:destroy-command-pool device command-pool)
  (dolist (resource resources)
    (destroy-vulkan-command-native-resource device resource)))

(defmethod destroy :after ((command-buffer vulkan-gpu-command-buffer))
  (setf (vulkan-command-buffer-state command-buffer) :destroyed))

(define-vulkan-resource-destroy (vulkan-gpu-bind-group bind-group)
    (vulkan-bind-group-device bind-group)
    ((descriptor-pool (vulkan-bind-group-descriptor-pool bind-group)))
  (lvk:destroy-descriptor-pool device descriptor-pool))

(define-vulkan-resource-destroy (vulkan-gpu-compute-pipeline pipeline)
    (vulkan-compute-pipeline-device pipeline)
    ((handle (vulkan-handle pipeline))
     (pipeline-layout (vulkan-compute-pipeline-layout pipeline)))
  (lvk:destroy-pipeline device handle)
  (lvk:destroy-pipeline-layout device pipeline-layout))

(define-vulkan-resource-destroy (vulkan-gpu-render-pipeline pipeline)
    (vulkan-render-pipeline-device pipeline)
    ((handle (vulkan-handle pipeline))
     (pipeline-layout (vulkan-render-pipeline-layout pipeline)))
  (lvk:destroy-pipeline device handle)
  (lvk:destroy-pipeline-layout device pipeline-layout))

(define-vulkan-resource-destroy (vulkan-gpu-sampler sampler)
    (vulkan-sampler-device sampler)
    ((handle (vulkan-handle sampler)))
  (lvk:destroy-sampler device handle))

(define-vulkan-resource-destroy (vulkan-gpu-buffer buffer)
    (vulkan-buffer-device buffer)
    ((handle (vulkan-handle buffer))
     (memory (vulkan-buffer-memory buffer)))
  (lvk:unmap-memory device memory)
  (lvk:destroy-buffer device handle)
  (lvk:free-memory device memory))

(define-vulkan-resource-destroy (vulkan-gpu-bind-group-layout layout)
    (vulkan-bind-group-layout-device layout)
    ((handle (vulkan-handle layout)))
  (lvk:destroy-descriptor-set-layout device handle))

(define-vulkan-resource-destroy (vulkan-gpu-shader-module module)
    (vulkan-shader-module-device module)
    ((handle (vulkan-handle module)))
  (lvk:destroy-shader-module device handle))

(define-vulkan-resource-destroy (vulkan-gpu-texture-view view)
    (vulkan-texture-view-device view)
    ((handle (vulkan-handle view)))
  (lvk:destroy-image-view device handle))

(define-vulkan-resource-destroy (vulkan-gpu-texture texture)
    (vulkan-texture-device texture)
    ((handle (vulkan-handle texture))
     (memory (vulkan-texture-memory texture))
     (owned-p (vulkan-texture-owned-p texture)))
  (when owned-p
    (lvk:destroy-image device handle)
    (lvk:free-memory device memory)))

(defmethod destroy ((device vulkan-gpu-device))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p device)
      (sb-ext:cancel-finalization device)
      (let ((queue (vulkan-device-queue device)))
        ;; The queue lock serializes this against finalizer-thread
        ;; teardown of collected children, which checks the device's
        ;; destroyed mark under the same lock.
        (sb-thread:with-recursive-lock ((vulkan-queue-lock queue))
          (unwind-protect
             (progn
               (lvk:device-wait-idle (vulkan-handle device))
               (when queue
                 ;; After the idle wait the frontier has reached the
                 ;; counter, so this retires every record and performs
                 ;; all deferred native destruction.
                 (maintain-vulkan-queue queue)
                 (lvk:destroy-semaphore
                  (vulkan-handle device) (vulkan-queue-timeline queue)))
               (maphash
                (lambda (format render-pass)
                  (declare (ignore format))
                  (lvk:destroy-render-pass
                   (vulkan-handle device) render-pass))
                (vulkan-device-render-passes device))
               (clrhash (vulkan-device-render-passes device)))
          (unwind-protect
               (lvk:destroy-device (vulkan-handle device))
            (unwind-protect
                 (when (vulkan-device-debug-messenger device)
                   (lvk:destroy-debug-messenger
                    (vulkan-device-debug-messenger device)))
              (lvk:destroy-instance (vulkan-device-instance device))
              (setf (vulkan-object-destroyed-p device) t)
              (when queue
                (setf (vulkan-object-destroyed-p queue) t)))))))))
  (values))
