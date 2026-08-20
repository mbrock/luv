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

(defun wrap-vulkan-gpu-driver-teardown (teardown)
  "Return a durable TEARDOWN which restores the Vulkan driver environment.

Queue maintenance already establishes this environment, but process-global
finalizer retirement can replay a failed closure from an arbitrary thread."
  (check-type teardown function)
  (lambda ()
    (with-vulkan-gpu-driver-environment
      (funcall teardown))))

(defun retire-vulkan-leaked-native-owner
    (resource-class label device owner teardown)
  "Transfer leaked native ownership before signaling the discipline warning."
  (unwind-protect
       (with-vulkan-gpu-driver-environment
         (retire-gpu-finalizer-native-owner device owner teardown))
    ;; Custody is already durable even if a handler promotes this warning or
    ;; leaves it by THROW, ERROR, or an interactive debugger restart.
    (note-gpu-resource-leak resource-class label)))

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

;; Vulkan is luv's portable default.  On Darwin, leave the choice open until
;; the native Metal module has loaded so Apple hosts do not accidentally run
;; through MoltenVK merely because the Vulkan backend appears first in ASDF.
#-darwin
(unless *gpu-provider*
  (setf *gpu-provider* (make-instance 'vulkan-gpu-provider)))

(defclass vulkan-gpu-object ()
  ((handle
    :initarg :handle
    :reader vulkan-handle)
   (destroyed-p
    :initform nil
    :accessor vulkan-object-destroyed-p)
   (retirement-teardown
    :initform nil
    :accessor vulkan-object-retirement-teardown
    :documentation "Progress-tracked native teardown closure, if any.")
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
                (unless (or (vulkan-object-destroyed-p ,object)
                            (vulkan-device-native-retired-p ,object))
                  (let ((,device-var (vulkan-handle ,object)))
                    (declare (ignorable ,device-var))
                    ,@body))))
         (let ((,queue (vulkan-device-queue ,object)))
           (if ,queue
               (sb-thread:with-recursive-lock ((vulkan-queue-lock ,queue))
                 (,run))
               (,run)))))))

(defgeneric vulkan-finalizer-device (object)
  (:method ((object t))
    (declare (ignore object))
    nil)
  (:documentation
   "Return the live device queue which may durably own OBJECT's finalizer."))

(defmethod initialize-instance :after ((object vulkan-gpu-object) &key)
  ;; Explicit DESTROY cancels this finalizer.  If the object is instead
  ;; reclaimed by the collector, the leak is a warned discipline failure
  ;; and the native resources are freed as a safety net.  Anything the
  ;; queue still retains through a live submission record is reachable,
  ;; so a collected wrapper is always past the completion frontier.
  (let ((closer (vulkan-native-teardown-closure object))
        (device (vulkan-finalizer-device object)))
    (setf (vulkan-object-retirement-teardown object) closer)
    (when closer
      (let* ((resource-class (class-name (class-of object)))
             (label (gpu-object-label object))
             (owner (list resource-class :label label)))
        (sb-ext:finalize
         object
         (lambda ()
           (retire-vulkan-leaked-native-owner
            resource-class label device owner
            (wrap-vulkan-gpu-driver-teardown closer))))))))

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
   (video-queue-family
    :initarg :video-queue-family :initform nil
    :reader vulkan-device-video-queue-family)
   (queue
    :initform nil
    :accessor vulkan-device-queue)
   (retiring-p
    :initform nil
    :accessor vulkan-device-retiring-p
    :documentation "True once the idle-and-ledger admission barrier closes.")
   (native-device-retired-box
    :initform (list nil)
    :reader vulkan-device-native-retired-box
    :documentation
    "Shared phase flag set immediately after vkDestroyDevice succeeds.")
   (destroy-admission
    :initform nil
    :accessor vulkan-device-destroy-admission)
   (destroy-teardown
    :initform nil
    :accessor vulkan-device-destroy-teardown)
   (finalizer-teardown
    :initform nil
    :accessor vulkan-device-finalizer-teardown
    :documentation
    "Driver-wrapped fallback sharing DESTROY's native progress sequence.")
   (render-passes
    :initform (make-hash-table :test #'equal)
    :reader vulkan-device-render-passes)))

(defun vulkan-device-native-retired-p (device)
  (car (vulkan-device-native-retired-box device)))

(defun (setf vulkan-device-native-retired-p) (value device)
  (setf (car (vulkan-device-native-retired-box device)) value))

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
   (external-semaphore-states
    :initform '()
    :accessor vulkan-queue-external-semaphore-states
    :documentation "Live handle-generation states shared by adopted textures.")
   (retirement-ledger
    :initform (make-gpu-retirement-ledger)
    :reader vulkan-queue-retirement-ledger
    :documentation "Native ownership transferred by logical DESTROY.")
   (lock
    :initform (sb-thread:make-mutex :name "vulkan gpu queue")
    :reader vulkan-queue-lock
    :documentation "Guards the counter, live records, deferred destroys,
and scheduled texture layouts across the canvas and REPL threads.")))

(defgeneric vulkan-admission-closed-p (object)
  (:method ((object t)) nil))

(defmethod vulkan-admission-closed-p ((device vulkan-gpu-device))
  (vulkan-device-retiring-p device))

(defmethod vulkan-admission-closed-p ((queue vulkan-gpu-queue))
  (vulkan-device-retiring-p (vulkan-queue-device queue)))

(defstruct vulkan-gpu-submission
  "One queue submission awaiting completion, retaining what the GPU may use."
  (index 0 :type (unsigned-byte 64))
  (command-buffers #() :type vector)
  (resources '() :type list)
  post-submit-publication)

(defstruct vulkan-external-submission-group
  "Textures sharing one external timeline semaphore in a submission."
  semaphore
  (current-value 0 :type (unsigned-byte 64))
  (textures '() :type list))

(defstruct vulkan-external-semaphore-state
  "Generation-safe shared high-water state for one retained native timeline."
  semaphore
  (value 0 :type (unsigned-byte 64))
  (references 0 :type (unsigned-byte 64)))

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
   (external-owner
    :initarg :external-owner
    :initform nil
    :reader vulkan-texture-external-owner)
   (aspect
    :initarg :aspect
    :initform nil
    :reader vulkan-texture-explicit-aspect)
   (external-semaphore
    :initarg :external-semaphore :initform nil
    :reader vulkan-texture-external-semaphore)
   (external-semaphore-value
    :initarg :external-semaphore-value :initform 0
    :accessor vulkan-texture-private-external-semaphore-value)
   (external-semaphore-state
    :initarg :external-semaphore-state :initform nil
    :reader vulkan-texture-external-semaphore-state)
   (external-submitted
    :initarg :external-submitted :initform nil
   :reader vulkan-texture-external-submitted)
   (vk-format
    :initarg :vk-format
    :reader vulkan-texture-vk-format)
   (layout
    :initarg :layout
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
   (target-formats
    :initarg :target-formats
    :initform nil
    :reader vulkan-render-pipeline-target-formats)
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
   (retirement-teardown
    :initform nil
    :accessor vulkan-command-encoder-retirement-teardown)
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
   (targets
    :initarg :targets
    :initform nil
    :reader vulkan-render-pass-targets)
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

(defmethod vulkan-finalizer-device ((object vulkan-gpu-buffer))
  (vulkan-buffer-device object))

(defmethod vulkan-finalizer-device ((object vulkan-gpu-texture))
  (vulkan-texture-device object))

(defmethod vulkan-finalizer-device ((object vulkan-gpu-texture-view))
  (vulkan-texture-view-device object))

(defmethod vulkan-finalizer-device ((object vulkan-gpu-shader-module))
  (vulkan-shader-module-device object))

(defmethod vulkan-finalizer-device ((object vulkan-gpu-sampler))
  (vulkan-sampler-device object))

(defmethod vulkan-finalizer-device ((object vulkan-gpu-bind-group-layout))
  (vulkan-bind-group-layout-device object))

(defmethod vulkan-finalizer-device ((object vulkan-gpu-compute-pipeline))
  (vulkan-compute-pipeline-device object))

(defmethod vulkan-finalizer-device ((object vulkan-gpu-render-pipeline))
  (vulkan-render-pipeline-device object))

(defmethod vulkan-finalizer-device ((object vulkan-gpu-bind-group))
  (vulkan-bind-group-device object))

(defmethod vulkan-finalizer-device ((object vulkan-gpu-command-buffer))
  (vulkan-command-buffer-device object))

(defun vulkan-command-encoder-native-resources (encoder)
  (first (vulkan-command-encoder-native-resource-box encoder)))

(defun (setf vulkan-command-encoder-native-resources) (value encoder)
  (setf (first (vulkan-command-encoder-native-resource-box encoder)) value))

(defun ensure-live-vulkan-object (object operation)
  (when (or (vulkan-object-destroyed-p object)
            (vulkan-admission-closed-p object))
    (error 'gpu-object-destroyed-error
           :object object
           :operation operation))
  object)

(defun call-with-live-vulkan-device-queue (device operation thunk)
  "Serialize admitted device-native work against DEVICE destruction."
  (let ((queue (vulkan-device-queue device)))
    (if queue
        (sb-thread:with-recursive-lock ((vulkan-queue-lock queue))
          (ensure-live-vulkan-object device operation)
          (funcall thunk))
        (progn
          (ensure-live-vulkan-object device operation)
          (funcall thunk)))))

(defmacro with-live-vulkan-device-queue ((device operation) &body body)
  `(call-with-live-vulkan-device-queue
    ,device ,operation (lambda () ,@body)))

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

(defun first-vulkan-video-decode-queue-family (physical-device)
  (loop for properties in (lvk:physical-device-queue-families physical-device)
        for index from 0
        when (and (plusp (lvk:queue-family-count properties))
                  (member :video-decode (lvk:queue-family-flags properties)))
          return index))

(defparameter *vulkan-video-device-extensions*
  '("VK_KHR_video_queue" "VK_KHR_video_decode_queue"
    "VK_KHR_video_decode_h264" "VK_KHR_video_decode_h265")
  "Optional extensions which let FFmpeg decode on luv's VkDevice.")

(defparameter *vulkan-presentation-timing-device-extensions*
  (list lvk:+present-timing-extension-name+
        lvk:+present-id-2-extension-name+)
  "Optional extensions used to observe a swapchain's real display timeline.")

(defun available-vulkan-video-device-extensions (physical-device)
  (let ((available (lvk:enumerate-device-extension-names physical-device)))
    (remove-if-not (lambda (name) (member name available :test #'string=))
                   *vulkan-video-device-extensions*)))

(defun available-vulkan-presentation-timing-device-extensions
    (physical-device)
  (let ((available (lvk:enumerate-device-extension-names physical-device)))
    (remove-if-not
     (lambda (name) (member name available :test #'string=))
     *vulkan-presentation-timing-device-extensions*)))

(defun install-vulkan-device-leak-finalizer (device)
  "Arrange to warn about and reclaim DEVICE if it is collected undestroyed.

The finalizer captures the same progress-tracked native teardown as explicit
DESTROY, but not the device or queue wrappers.  A failed explicit teardown can
therefore be abandoned without replaying native calls which already returned."
  (multiple-value-bind (native-teardown finalizer-teardown)
      (ensure-vulkan-device-retirement-teardowns
       device (vulkan-device-queue device))
    (declare (ignore native-teardown))
    (let ((label (gpu-object-label device)))
      (sb-ext:finalize
       device
       (lambda ()
         (retire-vulkan-leaked-native-owner
          'vulkan-gpu-device label nil
          (list 'vulkan-gpu-device :label label)
          finalizer-teardown)))))
  device)

(defun make-vulkan-gpu-device
    (instance physical-device queue-family descriptor
     &key debug-messenger instance-extension-names enabled-extension-names
          video-queue-family)
  "Create GPU wrappers for an already selected Vulkan device and queue."
  (let ((native-device
          (lvk:create-device
           physical-device queue-family
           :enabled-extension-names enabled-extension-names
           :additional-family-indices (and video-queue-family
                                           (list video-queue-family))))
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
                  :queue-family queue-family
                  :video-queue-family video-queue-family))
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
    (device image size format vk-format &key (usage '(:copy-dst)) owner aspect
                                             (layout :undefined) semaphore
                                             (semaphore-value 0) submitted)
  "Wrap an externally owned Vulkan IMAGE as a GPU texture."
  (let* ((queue (vulkan-device-queue device))
         (semaphore-state
           (and semaphore
                (retain-vulkan-external-semaphore-state
                 queue semaphore semaphore-value)))
         (completed-p nil))
    (unwind-protect
         (prog1
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
              :external-owner owner
              :aspect aspect
              :layout layout
              :external-semaphore semaphore
              :external-semaphore-value semaphore-value
              :external-semaphore-state semaphore-state
              :external-submitted submitted
              :owned-p nil)
           (setf completed-p t))
      (unless completed-p
        (when semaphore-state
          (release-vulkan-external-semaphore-state
           queue semaphore-state))))))

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

(defun vulkan-gpu-format (format descriptor)
  (or (cdr (assoc format
                  '((:r16-float . :r16-sfloat)
                    (:rgba8-unorm . :r8g8b8a8-unorm)
                    (:r8-unorm . :r8-unorm)
                    (:rg8-unorm . :r8g8-unorm)
                    (:rgba8-unorm-srgb . :r8g8b8a8-srgb)
                    (:bgra8-unorm . :b8g8r8a8-unorm)
                    (:bgra8-unorm-srgb . :b8g8r8a8-srgb)
                    (:rg16-uint . :r16g16-uint)
                    (:rg16-float . :r16g16-sfloat)
                    (:rgba16-float . :r16g16b16a16-sfloat)
                    (:depth32-float . :d32-sfloat))))
      (reject-gpu-request
       descriptor :unsupported-texture-format
       format)))

(defun vulkan-texture-format (descriptor)
  (vulkan-gpu-format (texture-descriptor-format descriptor) descriptor))

(defun vulkan-depth-format-p (format)
  (eq format :depth32-float))

(defun vulkan-texture-aspect (texture)
  (or (vulkan-texture-explicit-aspect texture)
      (if (vulkan-depth-format-p (gpu-texture-format texture)) :depth :color)))

(defmethod adopt-native-texture
    ((device vulkan-gpu-device) native owner (descriptor texture-descriptor))
  "Wrap one plane view of an AVVkFrame image without taking image ownership."
  (ensure-live-vulkan-object device :adopt-native-texture)
  (unless (and (listp native) (getf native :image) owner)
    (reject-gpu-request descriptor :invalid-native-texture native))
  (with-live-vulkan-device-queue (device :adopt-native-texture)
    (make-borrowed-vulkan-texture
     device (getf native :image) (texture-descriptor-size descriptor)
     (texture-descriptor-format descriptor)
     (or (getf native :format) (vulkan-texture-format descriptor))
     :usage (texture-descriptor-usage descriptor)
     :owner owner :aspect (getf native :aspect)
     :layout (or (getf native :layout) :general)
     :semaphore (getf native :semaphore)
     :semaphore-value (or (getf native :semaphore-value) 0)
     :submitted (getf native :submitted))))

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
              (:storage :storage)
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
               ;; A provider handed an explicit callback insists on the
               ;; extension; the default messenger only asks for one when
               ;; the instance already has it, so an ordinary run installs
               ;; it and simply never hears anything.  Nothing speaks
               ;; through it but a validation layer someone loaded.
               (let* ((requested (vulkan-provider-debug-callback provider))
                      (callback (or requested
                                    (and *vulkan-validation-enabled-p*
                                         #'note-vulkan-debug-message)))
                      (available-p
                        (member lvk:+debug-utils-extension-name+
                                extensions :test #'string=)))
                 (when (and requested (not available-p))
                   (error 'vulkan-gpu-error
                          :operation :request-device
                          :reason :missing-debug-utils-extension))
                 (when (and callback available-p)
                   (setf debug-messenger
                         (lvk:install-debug-messenger
                          instance callback
                          :severities
                          (vulkan-provider-debug-severities provider)
                          :types (vulkan-provider-debug-types provider)))))
               (let* ((physical-device
                        (or (first
                             (lvk:enumerate-physical-devices instance))
                            (error 'vulkan-gpu-error
                                   :operation :request-device
                                   :reason :no-physical-device)))
                      (queue-family
                        (first-vulkan-graphics-queue-family physical-device))
                      (video-queue-family
                        (first-vulkan-video-decode-queue-family
                         physical-device)))
                 (let ((device
                         (make-vulkan-gpu-device
                          instance physical-device queue-family descriptor
                          :debug-messenger debug-messenger
                          :instance-extension-names extensions
                          :video-queue-family video-queue-family
                          :enabled-extension-names
                          (remove-duplicates
                           (append
                            (vulkan-gpu-device-extension-names provider)
                            (when (lvk:physical-device-mesh-shader-p
                                   physical-device)
                              (list lvk:+mesh-shader-extension-name+))
                            (available-vulkan-presentation-timing-device-extensions
                             physical-device)
                            (available-vulkan-video-device-extensions
                             physical-device))
                           :test #'string=))))
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
           (usage (buffer-descriptor-usage descriptor)))
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
  "Copy a one-dimensional numeric array into mapped coherent memory.

DATA holds single-floats or unsigned 8-, 32-, or 64-bit integers; OFFSET is
aligned to the element size.  A storage buffer of packed sites arrives here
as its own element type rather than as reinterpreted floats."
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object buffer :write-buffer)
    (multiple-value-bind (foreign-type element-size)
        (buffer-data-foreign-type data)
      (unless foreign-type
        (reject-gpu-request buffer :unsupported-buffer-data data))
      (unless (and (typep offset '(unsigned-byte 64))
                   (zerop (mod offset element-size))
                   (<= (+ offset (* element-size (length data)))
                       (gpu-buffer-size buffer)))
        (reject-gpu-request
         buffer :buffer-write-out-of-bounds
         (list :offset offset :length (* element-size (length data)))))
      ;; One memcpy from the pinned storage vector: the element types
      ;; BUFFER-DATA-FOREIGN-TYPE admits are all stored unboxed, so the
      ;; bytes in the Lisp vector are the bytes the GPU wants.
      (let ((destination (vulkan-buffer-mapped buffer)))
        (sb-kernel:with-array-data ((vector data) (start 0) (end (length data)))
          (sb-sys:with-pinned-objects (vector)
            (sb-kernel:system-area-ub8-copy
             (sb-sys:vector-sap vector) (* start element-size)
             destination offset
             (* element-size (- end start))))))))
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
      ;; One memcpy into the pinned result vector, mirroring WRITE-BUFFER:
      ;; a byte-at-a-time loop over a whole frame's readback is hundreds of
      ;; milliseconds; this is the difference between a film and a slideshow.
      (let ((bytes (make-array size :element-type '(unsigned-byte 8)))
            (source (cffi:inc-pointer
                     (vulkan-buffer-mapped buffer) offset)))
        (sb-sys:with-pinned-objects (bytes)
          (sb-kernel:system-area-ub8-copy
           source 0 (sb-sys:vector-sap bytes) 0 size))
        bytes))))

(defmethod create
    ((device vulkan-gpu-device) (descriptor texture-descriptor))
  "Create one owned, single-mip Vulkan 2D texture and bind its memory."
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-texture)
    (let* ((size (texture-descriptor-size descriptor))
           (usages (texture-descriptor-usage descriptor))
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
            (with-cpu-trace-zone (:vulkan/shader/prepare-spir-v)
              (case (shader-module-descriptor-language descriptor)
                (:spir-v (shader-module-descriptor-code descriptor))
                (:mathematical
                 (let ((specification
                         (shader-module-descriptor-code descriptor)))
                   (unless (typep specification
                                  'luv.shader:shader-specification)
                     (reject-gpu-request
                      descriptor :invalid-mathematical-shader specification))
                   (luv.spir-v:assemble-shader-specification specification)))
                (otherwise
                 (reject-gpu-request
                  descriptor :unsupported-shader-language
                  (shader-module-descriptor-language descriptor)))))))
      (unless (and (vectorp code)
                   (plusp (length code))
                   (every (lambda (word)
                            (typep word '(unsigned-byte 32)))
                          code))
        (reject-gpu-request descriptor :invalid-spir-v code))
      ;; This forced line separates pure Lisp/SPIR-V preparation from the
      ;; first driver call if the latter takes the process or device down.
      (log-event :vulkan "prepared shader module ~A (~:D SPIR-V words)"
                 (or (gpu-descriptor-label descriptor) "unlabelled")
                 (length code))
      (make-instance
       'vulkan-gpu-shader-module
       :label (gpu-descriptor-label descriptor)
       :handle (with-cpu-trace-zone (:vulkan/shader/create-module)
                 (lvk:create-shader-module (vulkan-handle device) code))
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

(defun texture-sampler-uniform-layout-entries (descriptor)
  (let* ((entries (bind-group-layout-descriptor-entries descriptor))
         (bindings (mapcar (lambda (entry) (getf entry :binding)) entries)))
    (unless (and (listp entries) (plusp (length entries))
                 (every (lambda (entry)
                          (and (listp entry)
                               (member (getf entry :type)
                                       '(:texture :sampler :uniform-buffer
                                         :storage-buffer))
                               (typep (getf entry :binding)
                                      '(unsigned-byte 32))))
                        entries)
                 (= (length bindings)
                    (length (remove-duplicates bindings))))
      (reject-gpu-request descriptor :unsupported-bind-group-layout entries))
    entries))

(defun vulkan-descriptor-layout-stages (device entries)
  "Give each of ENTRIES the stages that may read it on DEVICE.

A descriptor must name every stage that reads it, and a mesh pipeline reads
from stages that did not exist when :VERTEX and :FRAGMENT were the whole
graphics vocabulary.  Naming the task and mesh stages is only legal once
VK_EXT_mesh_shader is enabled, so ask the device first."
  (let ((stages (if (lvk:physical-device-mesh-shader-p
                     (vulkan-device-physical-device device))
                    '(:vertex :fragment :task-ext :mesh-ext)
                    '(:vertex :fragment))))
    (mapcar (lambda (entry)
              (if (getf entry :stages)
                  entry
                  (append entry (list :stages stages))))
            entries)))

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
         (let ((entries (vulkan-descriptor-layout-stages
                         device
                         (texture-sampler-uniform-layout-entries descriptor))))
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
      (with-live-vulkan-device-queue (device :create-compute-pipeline)
        (ensure-vulkan-object-device
         module (vulkan-shader-module-device module) device
         :create-compute-pipeline)
        (ensure-vulkan-object-device
         layout (vulkan-bind-group-layout-device layout) device
         :create-compute-pipeline)
        (let ((pipeline-layout
                (lvk:create-pipeline-layout
                 (vulkan-handle device) (vector (vulkan-handle layout))))
              (pipeline nil)
              (completed-p nil))
          (unwind-protect
               (let ((wrapper nil))
                 (setf pipeline
                       (lvk:create-compute-pipeline
                        (vulkan-handle device)
                        (vulkan-handle module)
                        pipeline-layout
                        :entry-point entry-point)
                       wrapper
                       (make-instance
                        'vulkan-gpu-compute-pipeline
                        :label (gpu-descriptor-label descriptor)
                        :handle pipeline
                        :device device
                        :layout layout
                        :pipeline-layout pipeline-layout)
                       completed-p t)
                 wrapper)
            (unless completed-p
              (unwind-protect
                   (when pipeline
                     (lvk:destroy-pipeline
                      (vulkan-handle device) pipeline))
                (lvk:destroy-pipeline-layout
                 (vulkan-handle device) pipeline-layout)))))))))

(defun vulkan-render-pass-for-format
    (device gpu-formats descriptor &optional depth-format
            (depth-store-op :discard))
  "Return the cached render pass for GPU-FORMATS and optional DEPTH-FORMAT.

GPU-FORMATS is a list in fragment-output location order.  A single keyword
is accepted for callers predating multiple render targets."
  (with-live-vulkan-device-queue (device :create-render-pass)
    (let* ((gpu-formats (if (listp gpu-formats)
                            gpu-formats
                            (and gpu-formats (list gpu-formats))))
           (key (list gpu-formats depth-format depth-store-op)))
      (or (gethash key (vulkan-device-render-passes device))
          (setf (gethash key (vulkan-device-render-passes device))
                (if gpu-formats
                    (lvk:create-color-render-pass
                     (vulkan-handle device)
                     (map 'vector
                          (lambda (format)
                            (vulkan-gpu-format format descriptor))
                          gpu-formats)
                     :depth-format
                     (and depth-format
                          (vulkan-gpu-format depth-format descriptor))
                     :depth-store-op depth-store-op)
                    (lvk:create-depth-render-pass
                     (vulkan-handle device)
                     (vulkan-gpu-format depth-format descriptor)
                     :depth-store-op depth-store-op)))))))

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
                                  (member (getf attribute :format)
                                          '(:float32x2 :float32x3
                                            :float32x4))))
                           attributes))
          do (reject-gpu-request descriptor :invalid-vertex-buffer buffer)
        collect
        (list :binding binding :array-stride stride :step-mode step-mode
              :attributes
              (mapcar (lambda (attribute)
                        (list :shader-location
                              (getf attribute :shader-location)
                              :offset (getf attribute :offset)
                              :format
                              (ecase (vertex-attribute-format-component-count
                                      (getf attribute :format))
                                (2 :r32g32-sfloat)
                                (3 :r32g32b32-sfloat)
                                (4 :r32g32b32a32-sfloat))))
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
           (formats (mapcar (lambda (target) (getf target :format)) targets))
           (blends (mapcar (lambda (target) (getf target :blend)) targets))
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
                            (listp targets) targets
                            (every #'identity formats)
                            (every (lambda (blend)
                                     (member blend
                                             '(nil :premultiplied-alpha)))
                                   blends))
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
      (with-live-vulkan-device-queue (device :create-render-pipeline)
        (dolist (object
                 (remove nil (list layout vertex-module fragment-module)))
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
                  device formats descriptor depth-format
                  (or depth-store-op :discard)))
               (pipeline-layout
                 (lvk:create-pipeline-layout
                  (vulkan-handle device) (vector (vulkan-handle layout))))
               (pipeline nil)
               (completed-p nil))
          (unwind-protect
               (let ((wrapper nil))
                 (setf pipeline
                       (lvk:create-graphics-pipeline
                        (vulkan-handle device)
                        (vulkan-handle vertex-module)
                        (and fragment-module (vulkan-handle fragment-module))
                        pipeline-layout render-pass
                        :vertex-entry-point
                        (or (getf vertex :entry-point) "main")
                        :fragment-entry-point
                        (or (getf fragment :entry-point) "main")
                        :topology topology
                        :vertex-buffers vertex-buffers
                        :depth-compare depth-compare
                        :depth-write-enabled depth-write-enabled
                        :blends blends)
                       wrapper
                       (make-instance
                        'vulkan-gpu-render-pipeline
                        :label (gpu-descriptor-label descriptor)
                        :handle pipeline :device device :layout layout
                        :pipeline-layout pipeline-layout :render-pass render-pass
                        :vertex-buffers vertex-buffers :target-formats formats
                        :depth-format depth-format)
                       completed-p t)
                 wrapper)
            (unless completed-p
              (unwind-protect
                   (when pipeline
                     (lvk:destroy-pipeline
                      (vulkan-handle device) pipeline))
                (lvk:destroy-pipeline-layout
                 (vulkan-handle device) pipeline-layout)))))))))

(defmethod create
    ((device vulkan-gpu-device) (descriptor mesh-render-pipeline-descriptor))
  "Link task, mesh, and fragment modules into a VK_EXT_mesh_shader pipeline.

The result is an ordinary VULKAN-GPU-RENDER-PIPELINE with no vertex buffers:
what distinguishes it is that its draw is a workgroup dispatch, so binding and
render-pass compatibility need no separate vocabulary."
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-mesh-render-pipeline)
    (let* ((layout (mesh-render-pipeline-descriptor-layout descriptor))
           (task (mesh-render-pipeline-descriptor-task descriptor))
           (mesh (mesh-render-pipeline-descriptor-mesh descriptor))
           (fragment (mesh-render-pipeline-descriptor-fragment descriptor))
           (task-module (getf task :module))
           (mesh-module (getf mesh :module))
           (fragment-module (getf fragment :module))
           (targets (getf fragment :targets))
           (formats (mapcar (lambda (target) (getf target :format)) targets))
           (blends (mapcar (lambda (target) (getf target :blend)) targets))
           (depth-stencil
             (mesh-render-pipeline-descriptor-depth-stencil descriptor))
           (depth-format (and depth-stencil (getf depth-stencil :format)))
           (depth-compare (and depth-stencil
                               (getf depth-stencil :depth-compare)))
           (depth-write-enabled
             (and depth-stencil
                  (getf depth-stencil :depth-write-enabled)))
           (depth-store-op
             (and depth-stencil
                  (or (getf depth-stencil :depth-store-op) :discard))))
      (unless (lvk:physical-device-mesh-shader-p
               (vulkan-device-physical-device device))
        (error 'gpu-request-error
               :operation :create-mesh-render-pipeline
               :descriptor descriptor
               :reason :vulkan-mesh-shader-runtime-unavailable))
      (unless (and (typep layout 'vulkan-gpu-bind-group-layout)
                   (typep mesh-module 'vulkan-gpu-shader-module)
                   (or (null task-module)
                       (typep task-module 'vulkan-gpu-shader-module))
                   (or (and (typep fragment-module 'vulkan-gpu-shader-module)
                            (listp targets) targets
                            (every #'identity formats)
                            (every (lambda (blend)
                                     (member blend
                                             '(nil :premultiplied-alpha)))
                                   blends))
                       (and (null fragment-module)
                            (null targets)
                            depth-stencil))
                   (or (null depth-stencil)
                       (and (eq depth-format :depth32-float)
                            (member depth-compare
                                    '(:never :less :equal :less-or-equal
                                      :greater :not-equal :greater-or-equal
                                      :always)))))
        (reject-gpu-request descriptor :unsupported-mesh-render-pipeline))
      (dolist (object (remove nil (list layout task-module mesh-module
                                        fragment-module)))
        (ensure-vulkan-object-device
         object
         (etypecase object
           (vulkan-gpu-bind-group-layout
            (vulkan-bind-group-layout-device object))
           (vulkan-gpu-shader-module
            (vulkan-shader-module-device object)))
         device :create-mesh-render-pipeline))
      (with-live-vulkan-device-queue (device :create-mesh-render-pipeline)
        (dolist (object (remove nil (list layout task-module mesh-module
                                          fragment-module)))
          (ensure-vulkan-object-device
           object
           (etypecase object
             (vulkan-gpu-bind-group-layout
              (vulkan-bind-group-layout-device object))
             (vulkan-gpu-shader-module
              (vulkan-shader-module-device object)))
           device :create-mesh-render-pipeline))
        (let* ((render-pass
                 (with-cpu-trace-zone (:vulkan/pipeline/render-pass)
                   (vulkan-render-pass-for-format
                    device formats descriptor depth-format
                    (or depth-store-op :discard))))
               (pipeline-layout
                 (with-cpu-trace-zone (:vulkan/pipeline/create-layout)
                   (lvk:create-pipeline-layout
                    (vulkan-handle device) (vector (vulkan-handle layout)))))
               (pipeline nil)
               (completed-p nil))
          (unwind-protect
               (let ((wrapper nil))
                 (setf pipeline
                       (with-cpu-trace-zone (:vulkan/pipeline/create-mesh)
                         (lvk:create-mesh-graphics-pipeline
                          (vulkan-handle device)
                          (vulkan-handle mesh-module)
                          (and fragment-module (vulkan-handle fragment-module))
                          pipeline-layout render-pass
                          :task-module (and task-module
                                            (vulkan-handle task-module))
                          :task-entry-point
                          (or (getf task :entry-point) "main")
                          :mesh-entry-point
                          (or (getf mesh :entry-point) "main")
                          :fragment-entry-point
                          (or (getf fragment :entry-point) "main")
                          :depth-compare depth-compare
                          :depth-write-enabled depth-write-enabled
                          :blends blends))
                       wrapper
                       (make-instance
                        'vulkan-gpu-render-pipeline
                        :label (gpu-descriptor-label descriptor)
                        :handle pipeline :device device :layout layout
                        :pipeline-layout pipeline-layout :render-pass render-pass
                        :vertex-buffers nil :target-formats formats
                        :depth-format depth-format)
                       completed-p t)
                 wrapper)
            (unless completed-p
              (unwind-protect
                   (when pipeline
                     (lvk:destroy-pipeline
                      (vulkan-handle device) pipeline))
                (lvk:destroy-pipeline-layout
                 (vulkan-handle device) pipeline-layout)))))))))

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
                        ((:uniform-buffer :storage-buffer)
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
             (prog1
                 (make-instance
                  'vulkan-gpu-bind-group
                  :label (gpu-descriptor-label descriptor)
                  :handle set :device device :layout layout
                  :buffers (list buffer)
                  :descriptor-pool pool)
               (setf completed-p t)))
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
                     (prog1
                         (make-instance
                          'vulkan-gpu-bind-group
                          :label (gpu-descriptor-label descriptor)
                          :handle set :device device :layout layout
                          :texture-views (list view)
                          :descriptor-pool pool)
                       (setf completed-p t)))
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
                        ((:uniform-buffer :storage-buffer)
                         (ensure-vulkan-object-device
                          resource (vulkan-buffer-device resource)
                          device :create-bind-group)
                         (let ((required
                                 (if (eq :storage-buffer
                                         (getf layout-entry :type))
                                     :storage
                                     :uniform)))
                           (unless (member required
                                           (gpu-buffer-usage resource))
                             (error 'gpu-usage-error
                                    :object resource
                                    :operation :create-bind-group
                                    :required-usage required
                                    :actual-usage
                                    (gpu-buffer-usage resource))))
                         (push resource buffers)
                         (push `(:binding ,(getf layout-entry :binding)
                                 :type ,(getf layout-entry :type)
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
                         (prog1
                             (make-instance
                              'vulkan-gpu-bind-group
                              :label (gpu-descriptor-label descriptor)
                              :handle set :device device :layout layout
                              :texture-views views
                              :samplers samplers
                              :buffers buffers
                              :descriptor-pool pool)
                           (setf completed-p t)))
                    (unless completed-p
                      (lvk:destroy-descriptor-pool
                       (vulkan-handle device) pool))))))))))

(defmethod create
    ((device vulkan-gpu-device) (descriptor command-encoder-descriptor))
  "Allocate and begin one Vulkan primary command buffer."
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object device :create-command-encoder)
    (with-live-vulkan-device-queue (device :create-command-encoder)
      (let ((command-pool nil)
            (completed-p nil))
        (unwind-protect
             (progn
               (setf command-pool
                     (lvk:create-command-pool
                      (vulkan-handle device)
                      (vulkan-device-queue-family device)
                      :flags '(:transient)))
               (let* ((command-buffer
                        (lvk:allocate-command-buffer
                         (vulkan-handle device) command-pool))
                      (encoder nil))
                 (lvk:begin-command-buffer command-buffer)
                 (setf encoder
                       (make-instance
                        'vulkan-gpu-command-encoder
                        :label (gpu-descriptor-label descriptor)
                        :device device
                        :command-pool command-pool
                        :command-buffer command-buffer)
                       encoder (install-vulkan-encoder-leak-finalizer encoder)
                       completed-p t)
                 encoder))
          (unless completed-p
            (when command-pool
              (lvk:destroy-command-pool
               (vulkan-handle device) command-pool))))))))

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
       (retire-vulkan-leaked-native-owner
        'vulkan-gpu-command-encoder label device
        (list 'vulkan-gpu-command-encoder :label label)
        (wrap-vulkan-gpu-driver-teardown
         (make-vulkan-command-retirement-teardown
          device command-pool (first box)))))))
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

(defmethod encode
    ((encoder vulkan-gpu-command-encoder)
     (command gpu-prepare-texture-command))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-command-encoder-state encoder :encode)
    (ensure-no-active-vulkan-pass encoder :encode)
    (let* ((usage (gpu-prepare-texture-command-usage command))
           (texture
             (ensure-vulkan-texture-for-command
              encoder (gpu-prepare-texture-command-texture command)
              command usage)))
      (ecase usage
        (:texture-binding
         (transition-vulkan-texture
          encoder texture :shader-read-only-optimal)))))
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
                      '(:r16-float :rgba8-unorm :rgba8-unorm-srgb
                        :bgra8-unorm :bgra8-unorm-srgb
                        :rg16-uint :rg16-float :rgba16-float))
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
      (let* ((bytes-per-texel
               (texture-format-bytes-per-texel
                (gpu-texture-format texture)))
             (element-type
               (texture-format-upload-element-type
                (gpu-texture-format texture))))
        (unless (and (arrayp data)
                     (= 2 (array-rank data))
                     (nth-value 0
                       (subtypep (array-element-type data) element-type))
                     (>= (array-dimension data 0) (second extent))
                     (>= (array-dimension data 1) (first extent)))
          (reject-texture-write destination :unsupported-texture-data data))
        (let* ((width (first extent))
               (height (second extent))
               (offset (texture-data-layout-offset data-layout))
               (bytes-per-row
                 (or (texture-data-layout-bytes-per-row data-layout)
                     (* width bytes-per-texel)))
               (rows-per-image
                 (or (texture-data-layout-rows-per-image data-layout) height)))
          (unless (and (typep offset '(unsigned-byte 64))
                       (zerop (mod offset bytes-per-texel))
                       (typep bytes-per-row '(unsigned-byte 32))
                       (>= bytes-per-row (* width bytes-per-texel))
                       (zerop (mod bytes-per-row bytes-per-texel))
                       (typep rows-per-image '(unsigned-byte 32))
                       (>= rows-per-image height))
            (reject-texture-write destination :invalid-data-layout data-layout))
          (values texture origin extent offset bytes-per-row rows-per-image
                  bytes-per-texel data destination))))))

(defun copy-texture-words-to-mapped-memory
    (data pointer width height offset bytes-per-row bytes-per-texel)
  (let ((foreign-type (ecase bytes-per-texel
                        (2 :uint16) (4 :uint32) (8 :uint64))))
    (dotimes (row height)
      (let ((destination
              (cffi:inc-pointer pointer (+ offset (* row bytes-per-row)))))
        (dotimes (column width)
          (setf (cffi:mem-aref destination foreign-type column)
                (row-major-aref
                 data (+ (* row (array-dimension data 1)) column))))))))

(defun record-vulkan-texture-write (encoder command)
  "Lower one queue texture write through a private Vulkan command encoder."
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-command-encoder-state encoder :encode)
    (ensure-no-active-vulkan-pass encoder :encode)
    (multiple-value-bind
          (texture origin extent offset bytes-per-row rows-per-image
                   bytes-per-texel data destination)
        (check-vulkan-texture-write
         (vulkan-command-encoder-device encoder) command)
      (declare (ignore rows-per-image destination))
      (let* ((device (vulkan-command-encoder-device encoder))
             (native-device (vulkan-handle device))
             (width (first extent))
             (height (second extent))
             (data-size (+ offset (* (1- height) bytes-per-row)
                           (* width bytes-per-texel)))
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
                  data mapped width height offset bytes-per-row bytes-per-texel)
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
                :buffer-row-length (/ bytes-per-row bytes-per-texel)
                :buffer-image-height height
                :x (first origin) :y (second origin))
               ;; Leave the texture where a reader can use it.  An upload
               ;; that stops at :TRANSFER-DST-OPTIMAL makes whoever samples
               ;; the texture next responsible for the transition, and the
               ;; place that notices is SET-BIND-GROUP -- inside a render
               ;; pass, where a layout transition is not allowed at all.
               ;; The upload is a whole operation, so it ends in the layout
               ;; an uploaded texture is for; every other consumer can
               ;; transition out of this one legally, outside a pass.
               (transition-vulkan-texture
                encoder texture :shader-read-only-optimal)
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
           (views (mapcar (lambda (attachment) (getf attachment :view))
                          attachments))
           (targets
             (mapcar (lambda (view)
                       (and (typep view 'vulkan-gpu-texture-view)
                            (gpu-texture-view-texture view)))
                     views))
           (depth-attachment
             (render-pass-descriptor-depth-stencil-attachment descriptor))
           (depth-view (and depth-attachment
                            (getf depth-attachment :view)))
           (depth-target
             (and (typep depth-view 'vulkan-gpu-texture-view)
                  (gpu-texture-view-texture depth-view)))
           (clear-colors
             (mapcar (lambda (attachment)
                       (normalize-render-pass-color
                        descriptor
                        (or (getf attachment :clear-value) #(0 0 0 1))))
                     attachments))
           (clear-depth
             (and depth-attachment
                  (normalize-render-pass-depth
                   descriptor
                   (or (getf depth-attachment :depth-clear-value) 1.0))))
           (depth-store-op
             (and depth-attachment
                  (getf depth-attachment :depth-store-op))))
      (unless (and (listp attachments)
                   (every (lambda (attachment target)
                            (and target
                                 (eq :clear (getf attachment :load-op))
                                 (eq :store (getf attachment :store-op))))
                          attachments targets)
                   (or (null depth-attachment)
                       (and depth-target
                            (eq :depth32-float
                                (gpu-texture-format depth-target))
                            (eq :clear
                                (getf depth-attachment :depth-load-op))
                            (member depth-store-op '(:discard :store))))
                   (or targets depth-target))
        (reject-gpu-request descriptor :unsupported-render-pass))
      (let* ((device (vulkan-command-encoder-device encoder))
             (size (gpu-texture-size (or (first targets) depth-target))))
        (with-live-vulkan-device-queue (device :begin-render-pass)
          (let* ((render-pass
                   (vulkan-render-pass-for-format
                    device (mapcar #'gpu-texture-format targets) descriptor
                    (and depth-target (gpu-texture-format depth-target))
                    (or depth-store-op :discard)))
                 (framebuffer nil)
                 (completed-p nil))
            (loop for view in views
                  for target in targets
                  do (unless (equal size (gpu-texture-size target))
                       (reject-gpu-request
                        descriptor :mismatched-color-size
                        (gpu-texture-size target)))
                     (ensure-vulkan-object-device
                      view (vulkan-texture-view-device view)
                      device :begin-render-pass)
                     (retain-vulkan-resource encoder view)
                     (ensure-vulkan-texture-for-command
                      encoder target descriptor :render-attachment)
                     (transition-vulkan-texture
                      encoder target :color-attachment-optimal))
            (when depth-target
              (unless (or (null targets)
                          (equal size (gpu-texture-size depth-target)))
                (reject-gpu-request descriptor :mismatched-depth-size
                                    (gpu-texture-size depth-target)))
              (ensure-vulkan-object-device
               depth-view (vulkan-texture-view-device depth-view)
               device :begin-render-pass))
            (when depth-target
              (retain-vulkan-resource encoder depth-view)
              (ensure-vulkan-texture-for-command
               encoder depth-target descriptor :render-attachment)
              (transition-vulkan-texture
               encoder depth-target :depth-stencil-attachment-optimal))
            (unwind-protect
                 (progn
                   (setf framebuffer
                         (lvk:create-framebuffer
                          (vulkan-handle device) render-pass
                          (mapcar #'vulkan-handle views)
                          (first size) (second size)
                          :depth-view (and depth-view
                                           (vulkan-handle depth-view))))
                   (if targets
                       (lvk:cmd-begin-color-render-pass
                        (vulkan-command-encoder-command-buffer encoder)
                        render-pass framebuffer (first size) (second size)
                        clear-colors :depth-clear-value clear-depth)
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
                            :targets targets :depth-target depth-target
                            :depth-store-op depth-store-op)))
                     (setf (vulkan-command-encoder-active-pass encoder) pass
                           completed-p t)
                     pass))
              (unless completed-p
                (when framebuffer
                  (lvk:destroy-framebuffer
                   (vulkan-handle device) framebuffer))))))))))

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
      ;; Vulkan render-pass compatibility is governed by attachment formats
      ;; and sample counts, not load/store operations.  Our textures are all
      ;; single-sampled, so compare the two formats directly instead of the
      ;; cached native render-pass handle (whose key also carries STORE-OP).
      (unless
          (and
           (equal (vulkan-render-pipeline-target-formats pipeline)
                  (mapcar #'gpu-texture-format
                          (vulkan-render-pass-targets pass)))
           (eq (vulkan-render-pipeline-depth-format pipeline)
               (and (vulkan-render-pass-depth-target pass)
                    (gpu-texture-format
                     (vulkan-render-pass-depth-target pass)))))
        (reject-gpu-request
         pipeline :incompatible-render-pass
         (list :pipeline-label (gpu-object-label pipeline)
               :pipeline-target-formats
               (vulkan-render-pipeline-target-formats pipeline)
               :pipeline-depth-format
               (vulkan-render-pipeline-depth-format pipeline)
               :target-formats
               (mapcar #'gpu-texture-format
                       (vulkan-render-pass-targets pass))
               :depth-format
               (and (vulkan-render-pass-depth-target pass)
                    (gpu-texture-format
                     (vulkan-render-pass-depth-target pass)))
               :depth-store-op (vulkan-render-pass-depth-store-op pass))))
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
     (command gpu-set-scissor-command))
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-render-pass-state pass :set-scissor)
    (let ((x (gpu-set-scissor-command-x command))
          (y (gpu-set-scissor-command-y command))
          (width (gpu-set-scissor-command-width command))
          (height (gpu-set-scissor-command-height command)))
      (unless (and (typep x '(unsigned-byte 31))
                   (typep y '(unsigned-byte 31))
                   (typep width '(unsigned-byte 32))
                   (typep height '(unsigned-byte 32)))
        (reject-gpu-request
         command :invalid-scissor-rectangle (list x y width height)))
      (lvk:cmd-set-scissor
       (vulkan-command-encoder-command-buffer
        (vulkan-render-pass-command-encoder pass))
       x y width height)))
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

(defmethod encode
    ((pass vulkan-gpu-render-pass-encoder)
     (command gpu-draw-mesh-command))
  "Dispatch task or mesh workgroups instead of drawing vertices."
  (with-vulkan-gpu-driver-environment
    (ensure-vulkan-render-pass-state pass :draw-mesh-workgroups)
    (unless (and (vulkan-render-pass-pipeline pass)
                 (vulkan-render-pass-bind-group pass))
      (error 'gpu-invalid-state-error
             :object pass :operation :draw-mesh-workgroups
             :state :incomplete-bindings
             :expected-state :pipeline-and-bind-group-bound))
    (let ((x (gpu-draw-mesh-command-x command))
          (y (gpu-draw-mesh-command-y command))
          (z (gpu-draw-mesh-command-z command)))
      (unless (every (lambda (value) (typep value '(unsigned-byte 32)))
                     (list x y z))
        (reject-gpu-request command :invalid-draw-arguments (list x y z)))
      (let ((encoder (vulkan-render-pass-command-encoder pass)))
        (lvk:cmd-draw-mesh-tasks
         (vulkan-handle (vulkan-command-encoder-device encoder))
         (vulkan-command-encoder-command-buffer encoder)
         x y z))))
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

(defun make-vulkan-finished-command-buffer
    (encoder device command-buffer command-pool native-resources)
  "Publish ENCODER's ended native ownership as one command-buffer wrapper."
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
   :native-resources native-resources))

(defmethod finish ((encoder vulkan-gpu-command-encoder))
  (with-vulkan-gpu-driver-environment
    (unless (member (vulkan-command-encoder-state encoder)
                    '(:recording :ended))
      (error 'gpu-invalid-state-error
             :object encoder :operation :finish
             :state (vulkan-command-encoder-state encoder)
             :expected-state :recording-or-ended))
    (when (member (vulkan-command-encoder-state encoder)
                  '(:recording :ended))
      (ensure-no-active-vulkan-pass encoder :finish))
    (let ((device (vulkan-command-encoder-device encoder)))
      (with-live-vulkan-device-queue (device :finish)
        (unless (member (vulkan-command-encoder-state encoder)
                        '(:recording :ended))
          (error 'gpu-invalid-state-error
                 :object encoder :operation :finish
                 :state (vulkan-command-encoder-state encoder)
                 :expected-state :recording-or-ended))
        (when (eq :recording (vulkan-command-encoder-state encoder))
          (ensure-no-active-vulkan-pass encoder :finish))
        (let ((command-buffer
                (vulkan-command-encoder-command-buffer encoder))
              (command-pool
                (vulkan-command-encoder-command-pool encoder))
              (native-resources
                (copy-list
                 (vulkan-command-encoder-native-resources encoder))))
          (when (eq :recording (vulkan-command-encoder-state encoder))
            (lvk:end-command-buffer command-buffer)
            ;; The encoder and its finalizer retain ownership in :ENDED until a
            ;; fully initialized command-buffer wrapper takes over.
            (setf (vulkan-command-encoder-state encoder) :ended))
          (let ((wrapper
                  (make-vulkan-finished-command-buffer
                   encoder device command-buffer command-pool
                   native-resources)))
            (sb-ext:cancel-finalization encoder)
            (setf (vulkan-command-encoder-state encoder) :finished
                  (vulkan-command-encoder-command-pool encoder) nil
                  (vulkan-command-encoder-native-resources encoder) nil)
            wrapper))))))

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

(defun same-vulkan-native-handle-p (first second)
  "Compare native handles even when CFFI wrapped one address twice."
  (if (and (cffi:pointerp first) (cffi:pointerp second))
      (cffi:pointer-eq first second)
      (eql first second)))

(defun vulkan-texture-external-semaphore-value (texture)
  (let ((state (vulkan-texture-external-semaphore-state texture)))
    (if state
        (vulkan-external-semaphore-state-value state)
        (vulkan-texture-private-external-semaphore-value texture))))

(defun (setf vulkan-texture-external-semaphore-value) (value texture)
  (let ((state (vulkan-texture-external-semaphore-state texture)))
    (if state
        (setf (vulkan-external-semaphore-state-value state) value)
        (setf (vulkan-texture-private-external-semaphore-value texture)
              value)))
  value)

(defun find-vulkan-external-semaphore-state (queue semaphore)
  (find semaphore (vulkan-queue-external-semaphore-states queue)
        :key #'vulkan-external-semaphore-state-semaphore
        :test #'same-vulkan-native-handle-p))

(defun retain-vulkan-external-semaphore-state
    (queue semaphore initial-value)
  "Share one high-water state for the retained native semaphore generation."
  (flet ((retain ()
           (let ((state
                   (or (and queue
                            (find-vulkan-external-semaphore-state
                             queue semaphore))
                       (make-vulkan-external-semaphore-state
                        :semaphore semaphore :value initial-value))))
             (setf (vulkan-external-semaphore-state-value state)
                   (max (vulkan-external-semaphore-state-value state)
                        initial-value))
             (incf (vulkan-external-semaphore-state-references state))
             (when (and queue
                        (not (member
                              state
                              (vulkan-queue-external-semaphore-states queue)
                              :test #'eq)))
               (push state (vulkan-queue-external-semaphore-states queue)))
             state)))
    (if queue
        (sb-thread:with-recursive-lock ((vulkan-queue-lock queue))
          (retain))
        (retain))))

(defun release-vulkan-external-semaphore-state (queue state)
  "Release one adopted texture reference and forget an exhausted generation."
  (flet ((release ()
           (when (plusp
                  (vulkan-external-semaphore-state-references state))
             (decf (vulkan-external-semaphore-state-references state)))
           (when (and queue
                      (zerop
                       (vulkan-external-semaphore-state-references state)))
             (setf (vulkan-queue-external-semaphore-states queue)
                   (delete state
                           (vulkan-queue-external-semaphore-states queue)
                           :test #'eq)))))
    (if queue
        (sb-thread:with-recursive-lock ((vulkan-queue-lock queue))
          (release))
        (release)))
  (values))

(defun vulkan-external-submission-groups (texture-layouts)
  "Group submitted external textures by semaphore and retain the newest wait."
  (let ((groups '()))
    (maphash
     (lambda (texture layout)
       (declare (ignore layout))
       (let ((semaphore (vulkan-texture-external-semaphore texture)))
         (when semaphore
           (let ((group
                   (find semaphore groups
                         :key #'vulkan-external-submission-group-semaphore
                         :test #'same-vulkan-native-handle-p)))
             (if group
                 (setf
                  (vulkan-external-submission-group-current-value group)
                  (max
                   (vulkan-external-submission-group-current-value group)
                   (vulkan-texture-external-semaphore-value texture))
                  (vulkan-external-submission-group-textures group)
                  (cons texture
                        (vulkan-external-submission-group-textures group)))
                 (push
                  (make-vulkan-external-submission-group
                   :semaphore semaphore
                   :current-value
                   (vulkan-texture-external-semaphore-value texture)
                   :textures (list texture))
                  groups))))))
     texture-layouts)
    (dolist (group groups)
      (setf (vulkan-external-submission-group-textures group)
            (nreverse
             (vulkan-external-submission-group-textures group))))
    (nreverse groups)))

(defun vulkan-external-submission-next-value (group)
  (let ((current
          (vulkan-external-submission-group-current-value group)))
    (when (= current (1- (expt 2 64)))
      (error 'vulkan-gpu-error :operation :submit
             :reason :external-semaphore-value-exhausted
             :details current))
    (1+ current)))

(defun make-vulkan-external-callback-batch (callbacks)
  "Return an attempt-all, retry-only-failures external callback obligation."
  (let ((pending (copy-list callbacks)))
    (lambda ()
      (let ((retained '())
            (failures '()))
        (dolist (entry pending)
          (destructuring-bind (texture callback layout value) entry
            (handler-case
                (funcall callback layout value)
              (serious-condition (cause)
                (push entry retained)
                (push (cons texture cause) failures)))))
        (setf pending (nreverse retained))
        (when failures
          (error 'vulkan-gpu-error :operation :submit
                 :reason :external-submission-callbacks-failed
                 :details (nreverse failures)))))))

(defun make-vulkan-post-submit-publication (texture-layouts groups)
  "Build the durable HAL-state and external-owner publication obligation."
  (let ((callbacks '()))
    (dolist (group groups)
      (let ((value (vulkan-external-submission-next-value group)))
        (dolist (texture
                 (vulkan-external-submission-group-textures group))
          (let ((callback (vulkan-texture-external-submitted texture)))
            (when callback
              (push (list texture callback
                          (gethash texture texture-layouts) value)
                    callbacks))))))
    (apply
     #'make-gpu-retirement-sequence
     (append
      (list
       (lambda ()
         ;; Every HAL wrapper advances before any owner callback runs.  A
         ;; failing plane therefore cannot leave its siblings on stale values.
         (maphash
          (lambda (texture layout)
            (setf (vulkan-texture-layout texture) layout))
          texture-layouts)
         (dolist (group groups)
           (let ((value (vulkan-external-submission-next-value group)))
             (dolist (texture
                      (vulkan-external-submission-group-textures group))
               (setf (vulkan-texture-external-semaphore-value texture)
                     value))))))
      (when callbacks
        (list
         (make-vulkan-external-callback-batch (nreverse callbacks))))))))

(defun complete-vulkan-submission-publication (submission)
  "Attempt SUBMISSION's durable post-commit publication once."
  (let ((publication
          (vulkan-gpu-submission-post-submit-publication submission)))
    (when publication
      (funcall publication)
      (setf (vulkan-gpu-submission-post-submit-publication submission) nil)
      t)))

(defun complete-vulkan-queue-submission-publications (queue)
  "Complete the live FIFO prefix of post-commit owner publications."
  (let ((progress-p nil))
    (dolist (submission (vulkan-queue-live-submissions queue) progress-p)
      (when (complete-vulkan-submission-publication submission)
        (setf progress-p t)))))

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

(defun vulkan-retirement-custodian-quiescent-p (queue)
  "Whether QUEUE owns no submitted, publishing, or retirement work."
  (let ((ledger (vulkan-queue-retirement-ledger queue)))
    (and (null (vulkan-queue-live-submissions queue))
         (null (gpu-retirement-ledger-active-batch ledger))
         (null (gpu-retirement-ledger-entries ledger)))))

(defun maybe-release-vulkan-retirement-custodian (queue)
  "Unroot QUEUE only after backend quiescence was proved under its lock."
  (when (vulkan-retirement-custodian-quiescent-p queue)
    (release-gpu-retirement-ledger-custodian
     (vulkan-queue-retirement-ledger queue) queue)
    t))

(defun maintain-vulkan-queue (queue)
  "Retire live submissions the completion frontier has passed.

Retiring a submission drops the queue's references to everything it
retained.  The independent retirement ledger then attempts every native
teardown which that frontier makes safe; failures remain queue-owned."
  (with-vulkan-gpu-driver-environment
    (sb-thread:with-recursive-lock ((vulkan-queue-lock queue))
      ;; External-owner publication is part of the live record.  It must finish
      ;; before a completed record can release those owners or retirement can
      ;; advance beyond it.
      (complete-vulkan-queue-submission-publications queue)
      (let ((frontier (vulkan-queue-completed-frontier queue)))
        (loop while (and (vulkan-queue-live-submissions queue)
                         (<= (vulkan-gpu-submission-index
                              (first (vulkan-queue-live-submissions queue)))
                             frontier))
              do (pop (vulkan-queue-live-submissions queue)))
        (maintain-gpu-retirement-ledger
         (vulkan-queue-retirement-ledger queue) frontier
         :operation :maintain-vulkan-queue)
        (maybe-release-vulkan-retirement-custodian queue)
        frontier))))

(defmethod service-gpu-retirement-custodian ((queue vulkan-gpu-queue))
  "Service QUEUE from the process custodian registry without caller access."
  (with-vulkan-gpu-driver-environment
    (sb-thread:with-recursive-lock ((vulkan-queue-lock queue))
      (let* ((device (vulkan-queue-device queue))
             (ledger (vulkan-queue-retirement-ledger queue))
             (before (+ (length (vulkan-queue-live-submissions queue))
                        (length (gpu-retirement-ledger-active-batch ledger))
                        (length (gpu-retirement-ledger-entries ledger)))))
        (cond
          ((or (vulkan-object-destroyed-p queue)
               (vulkan-object-destroyed-p device)
               (vulkan-device-retiring-p device)
               (vulkan-device-native-retired-p device))
           ;; Never query a frontier through a retiring or retired VkDevice.
           ;; Device teardown has already enforced the ledger/submission
           ;; barrier; a stale empty root may now be removed without FFI.
           (maybe-release-vulkan-retirement-custodian queue))
          (t
           (maintain-vulkan-queue queue)
           (let ((after
                   (+ (length (vulkan-queue-live-submissions queue))
                      (length (gpu-retirement-ledger-active-batch ledger))
                      (length (gpu-retirement-ledger-entries ledger)))))
             (or (< after before)
                 (zerop after)))))))))

(defun live-vulkan-retirement-queue-p (queue device)
  (and queue
       (eq queue (vulkan-device-queue device))
       (not (vulkan-object-destroyed-p device))
       (not (vulkan-device-retiring-p device))
       (not (vulkan-device-native-retired-p device))
       (not (vulkan-object-destroyed-p queue))))

(defun retire-vulkan-native-owner
    (resource device ready-after teardown invalidate operation)
  "Transfer one native owner after revalidating its queue under the lock."
  (labels ((retire-directly (&optional queue-snapshot)
             (perform-gpu-retirement-directly
              resource
              (lambda ()
                (unless (or (vulkan-object-destroyed-p device)
                            (vulkan-device-native-retired-p device))
                  (when (and queue-snapshot
                             (or (not (eq queue-snapshot
                                          (vulkan-device-queue device)))
                                 (vulkan-object-destroyed-p queue-snapshot)))
                    (error "The Vulkan retirement queue is no longer live.")))
                (funcall teardown))
              invalidate
              :operation operation)))
    (let ((queue (vulkan-device-queue device)))
      (if queue
          (sb-thread:with-recursive-lock ((vulkan-queue-lock queue))
            ;; Device teardown uses this same lock.  Revalidate and choose the
            ;; queue or direct path without a race window between them.
            (if (live-vulkan-retirement-queue-p queue device)
                (progn
                  (transfer-gpu-retirement
                   (vulkan-queue-retirement-ledger queue)
                   resource ready-after teardown invalidate queue)
                  (maintain-vulkan-queue queue))
                (retire-directly queue)))
          (retire-directly))))
  (values))

(defmethod retire-gpu-native-owner
    ((device vulkan-gpu-device) owner teardown invalidate)
  (retire-vulkan-native-owner
   owner device 0 teardown invalidate :retire-gpu-native-owner))

(defun vulkan-destroy-or-defer (resource device invalidate)
  "Transfer RESOURCE's native ownership, then logically invalidate it.

A live queue owns the retirement before INVALIDATE marks the wrapper and
cancels its finalizer.  Queue maintenance immediately attempts anything
already safe and retains failures.  Without a live queue, native teardown
must succeed before INVALIDATE is called."
  (let ((teardown
          (or (vulkan-object-retirement-teardown resource)
              (setf (vulkan-object-retirement-teardown resource)
                    (vulkan-native-teardown-closure resource)))))
    (retire-vulkan-native-owner
     resource device (vulkan-object-last-submission resource)
     teardown invalidate :destroy-vulkan-resource)))

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
        ;; The first check can race while waiting for this lock.  Device
        ;; teardown closes admission under the same lock, so this is the
        ;; authoritative check before any queue or device FFI.
        (ensure-live-vulkan-object queue :submit)
        ;; A failed owner publication from an earlier native commit is a FIFO
        ;; admission barrier.  Retry it before scheduling dependent work.
        (complete-vulkan-queue-submission-publications queue)
        (loop for command-buffer across command-buffers
              do (check-vulkan-command-buffer-for-submit
                  queue command-buffer))
        (let* ((texture-layouts
                 (vulkan-submitted-texture-layouts command-buffers))
               (external-groups
                 (vulkan-external-submission-groups texture-layouts)))
          (when (plusp (length command-buffers))
            (setf index (1+ (vulkan-queue-submission-counter queue)))
            (let* ((resources
                     (loop for command-buffer across command-buffers
                           append
                           (copy-list
                            (vulkan-command-buffer-resources
                             command-buffer))))
                   (submission
                     (make-vulkan-gpu-submission
                      :index index
                      :command-buffers command-buffers
                      :resources resources
                      :post-submit-publication
                      (make-vulkan-post-submit-publication
                       texture-layouts external-groups)))
                   ;; Allocate the list cell before native commit.  Once Vulkan
                   ;; accepts the batch, publication below performs no external
                   ;; callback before this durable queue record exists.
                   (submission-cell (list submission)))
              (lvk:submit-command-buffers
               (vulkan-handle queue)
               (map 'vector #'vulkan-handle command-buffers)
               :wait-semaphores
               (concatenate
                'vector wait-semaphores
                (map
                 'vector
                 (lambda (group)
                   (list
                    (vulkan-external-submission-group-semaphore group)
                    '(:all-commands)
                    (vulkan-external-submission-group-current-value group)))
                 external-groups))
               :signal-semaphores
               (concatenate
                'vector signal-semaphores
                (map
                 'vector
                 (lambda (group)
                   (list
                    (vulkan-external-submission-group-semaphore group)
                    '(:all-commands)
                    (vulkan-external-submission-next-value group)))
                 external-groups)
                (vector (list (vulkan-queue-timeline queue)
                              '(:all-commands)
                              index))))
              ;; Native commit has happened.  Publish every counter, wrapper
              ;; state, dependency, and owner callback obligation before any
              ;; fallible user callback can regain control.
              (setf (vulkan-queue-submission-counter queue) index)
              (loop for command-buffer across command-buffers
                    do (setf (vulkan-command-buffer-state command-buffer)
                             :submitted
                             (vulkan-object-last-submission command-buffer)
                             index))
              (dolist (resource resources)
                (setf (vulkan-object-last-submission resource) index))
              (setf (vulkan-queue-live-submissions queue)
                    (nconc (vulkan-queue-live-submissions queue)
                           submission-cell))
              (retain-gpu-retirement-ledger-custodian
               (vulkan-queue-retirement-ledger queue) queue)
              ;; Success clears the obligation slot.  Failure propagates only
              ;; after the rooted live record has retained its retry progress.
              (complete-vulkan-submission-publication submission))))
        (when index
          (when wait-for-completion
            (wait-for-vulkan-submission queue index))
          (maintain-vulkan-queue queue)))
      index)))

(defmethod submit ((queue vulkan-gpu-queue) (command-buffers vector))
  "Schedule one WebGPU-style batch and return its submission index."
  (submit-vulkan-command-buffers queue command-buffers))

(defmethod submitted-work-done ((queue vulkan-gpu-queue))
  (with-vulkan-gpu-driver-environment
    (ensure-live-vulkan-object queue :submitted-work-done)
    (sb-thread:with-recursive-lock ((vulkan-queue-lock queue))
      (ensure-live-vulkan-object queue :submitted-work-done)
      (let ((counter (vulkan-queue-submission-counter queue)))
        (when (plusp counter)
          (wait-for-vulkan-submission queue counter))
        (maintain-vulkan-queue queue))))
  (values))

(defun make-vulkan-device-retirement-step (device function)
  (lambda ()
    (with-vulkan-queue-teardown (device native-device)
      (funcall function native-device))))

(defun vulkan-command-native-resource-retirement-steps (device resource)
  "Flatten one tagged command resource into one-native-call retry steps."
  (ecase (first resource)
    (:framebuffer
     (let ((framebuffer (second resource)))
       (list
        (make-vulkan-device-retirement-step
         device
         (lambda (native-device)
           (lvk:destroy-framebuffer native-device framebuffer))))))
    (:upload-buffer
     (let ((buffer (second resource))
           (memory (third resource)))
       (list
        (make-vulkan-device-retirement-step
         device
         (lambda (native-device)
           (lvk:destroy-buffer native-device buffer)))
        (make-vulkan-device-retirement-step
         device
         (lambda (native-device)
           (lvk:free-memory native-device memory))))))))

(defun make-vulkan-command-retirement-teardown
    (device command-pool resources)
  "Build one persistent, progress-tracked command ownership teardown."
  (apply
   #'make-gpu-retirement-sequence
   (append
    (when command-pool
      (list
       (make-vulkan-device-retirement-step
        device
        (lambda (native-device)
          (lvk:destroy-command-pool native-device command-pool)))))
    (loop for resource in resources
          append
          (vulkan-command-native-resource-retirement-steps
           device resource)))))

(defmethod destroy ((encoder vulkan-gpu-command-encoder))
  (with-vulkan-gpu-driver-environment
    (when (member (vulkan-command-encoder-state encoder)
                  '(:recording :ended))
      (let ((device (vulkan-command-encoder-device encoder))
            (command-pool (vulkan-command-encoder-command-pool encoder))
            (resources
              (copy-list
               (vulkan-command-encoder-native-resources encoder))))
        (flet ((invalidate ()
                 (setf (vulkan-command-encoder-native-resources encoder) nil
                       (vulkan-command-encoder-command-pool encoder) nil
                       (vulkan-command-encoder-state encoder) :destroyed)
                 (sb-ext:cancel-finalization encoder)))
          (let ((teardown
                  (or (vulkan-command-encoder-retirement-teardown encoder)
                      (setf
                       (vulkan-command-encoder-retirement-teardown encoder)
                       (make-vulkan-command-retirement-teardown
                        device command-pool resources)))))
            (retire-vulkan-native-owner
             encoder device 0 teardown #'invalidate
             :destroy-vulkan-command-encoder)))))
    (unless (eq :destroyed (vulkan-command-encoder-state encoder))
      (setf (vulkan-command-encoder-state encoder) :destroyed)))
  (values))

(defmacro define-vulkan-resource-destroy
    ((class variable) device-form bindings &body native-teardown)
  "Define DESTROY and the queueable native teardown closure for CLASS.

DESTROY first transfers NATIVE-TEARDOWN to the queue, then marks the wrapper
destroyed and cancels its leak finalizer.  Without a live queue, it marks and
cancels only after native teardown succeeds.

BINDINGS extract every native handle NATIVE-TEARDOWN needs, so the
teardown closure captures raw handles and the device wrapper rather than
VARIABLE itself and can therefore serve as the wrapper's leak finalizer.
NATIVE-TEARDOWN runs only while the device is alive, under the queue
  teardown lock, with DEVICE anaphorically bound to the native handle."
  (let ((device-object (gensym "DEVICE-OBJECT")))
    `(progn
       (defmethod vulkan-native-teardown-closure ((,variable ,class))
         (let* ((,device-object ,device-form)
                ,@bindings)
           (make-gpu-retirement-sequence
            ,@(loop for form in native-teardown
                    collect
                    `(lambda ()
                       (with-vulkan-queue-teardown
                           (,device-object device)
                         ,form))))))
       (defmethod destroy ((,variable ,class))
         (with-vulkan-gpu-driver-environment
           (unless (vulkan-object-destroyed-p ,variable)
             (vulkan-destroy-or-defer
              ,variable ,device-form
              (lambda ()
                (setf (vulkan-object-destroyed-p ,variable) t)
                (sb-ext:cancel-finalization ,variable)))))
         (values))
       ',class)))

(define-vulkan-resource-destroy
    (vulkan-gpu-command-buffer command-buffer)
    (vulkan-command-buffer-device command-buffer)
    ((command-pool (vulkan-command-buffer-command-pool command-buffer))
     (resources (vulkan-command-buffer-native-resources command-buffer))
     (teardown
       (make-vulkan-command-retirement-teardown
        (vulkan-command-buffer-device command-buffer)
        command-pool resources)))
  (funcall teardown))

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

(defmethod vulkan-native-teardown-closure ((texture vulkan-gpu-texture))
  (let ((device (vulkan-texture-device texture))
        (queue (vulkan-device-queue (vulkan-texture-device texture)))
        (handle (vulkan-handle texture))
        (memory (vulkan-texture-memory texture))
        (owned-p (vulkan-texture-owned-p texture))
        (external-owner (vulkan-texture-external-owner texture))
        (semaphore-state
          (vulkan-texture-external-semaphore-state texture)))
    (apply
     #'make-gpu-retirement-sequence
     (append
      (when owned-p
        (list
         (make-vulkan-device-retirement-step
          device
          (lambda (native-device)
            (lvk:destroy-image native-device handle)))
         (make-vulkan-device-retirement-step
          device
          (lambda (native-device)
            (lvk:free-memory native-device memory)))))
      (when external-owner
        ;; This owner is not a VkDevice child.  Keep it outside the guarded
        ;; device steps so a finalizer after vkDestroyDevice still releases it.
        (list (lambda () (funcall external-owner))))
      (when semaphore-state
        (list
         (lambda ()
           (release-vulkan-external-semaphore-state
            queue semaphore-state))))))))

(defmethod destroy ((texture vulkan-gpu-texture))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p texture)
      (vulkan-destroy-or-defer
       texture (vulkan-texture-device texture)
       (lambda ()
         (setf (vulkan-object-destroyed-p texture) t)
         (sb-ext:cancel-finalization texture)))))
  (values))

(defun make-vulkan-device-destroy-admission (device queue)
  "Return a persistent idle-and-ledger barrier for DEVICE destruction."
  (let ((waited-submission nil))
    (lambda ()
      (unless (vulkan-device-retiring-p device)
        (let ((submission (if queue
                              (vulkan-queue-submission-counter queue)
                              0)))
          ;; A ledger failure leaves admission open.  Preserve a completed
          ;; wait across retry, but renew it if another submission arrived.
          (unless (eql submission waited-submission)
            (lvk:device-wait-idle (vulkan-handle device))
            (setf waited-submission submission))
          (when queue
            (maintain-vulkan-queue queue)
            (ensure-gpu-retirement-ledger-empty
             (vulkan-queue-retirement-ledger queue)
             :operation :destroy-vulkan-device))
          ;; The caller holds QUEUE's lock, so the waited generation cannot
          ;; change between this barrier and closing admission.
          (setf (vulkan-device-retiring-p device) t))))))

(defun make-vulkan-render-pass-retirement-teardown
    (native-device render-pass-table)
  "Return a lazy, retryable drain of RENDER-PASS-TABLE.

The snapshot happens only when teardown begins, after explicit destruction
has closed device admission.  Native destruction and hash bookkeeping have
separate progress flags, so neither a late-created pass nor a Lisp-side error
can make a successful native call repeat."
  (let ((pending nil)
        (snapshotted-p nil)
        (current-native-retired-p nil))
    (lambda ()
      (unless snapshotted-p
        (setf pending
              (loop for format being the hash-keys of render-pass-table
                    using (hash-value render-pass)
                    collect (cons format render-pass))
              snapshotted-p t))
      (loop while pending
            for entry = (first pending)
            do (unless current-native-retired-p
                 (lvk:destroy-render-pass native-device (cdr entry))
                 (setf current-native-retired-p t))
               (remhash (car entry) render-pass-table)
               (setf current-native-retired-p nil)
               (pop pending))
      (values))))

(defun make-vulkan-device-destroy-teardown (device queue)
  "Return a native teardown which does not retain DEVICE or QUEUE."
  (let ((native-device (vulkan-handle device))
        (timeline (and queue (vulkan-queue-timeline queue)))
        (render-pass-table (vulkan-device-render-passes device))
        (native-retired-box (vulkan-device-native-retired-box device))
        (debug-messenger (vulkan-device-debug-messenger device))
        (instance (vulkan-device-instance device)))
    (apply
     #'make-gpu-retirement-sequence
     (append
      (when timeline
        (list
         (lambda ()
           (lvk:destroy-semaphore native-device timeline))))
      (list
       (make-vulkan-render-pass-retirement-teardown
        native-device render-pass-table))
      (list
       (lambda ()
         (lvk:destroy-device native-device)
         ;; Finalizers must stop issuing device-level calls immediately, even
         ;; if a later instance-level owner fails and overall DESTROY retries.
         (setf (car native-retired-box) t)))
      (when debug-messenger
        (list
         (lambda ()
           (lvk:destroy-debug-messenger debug-messenger))))
      (list
       (lambda ()
         (lvk:destroy-instance instance)))))))

(defun ensure-vulkan-device-retirement-teardowns (device queue)
  "Return DEVICE's shared explicit and leak-finalizer teardown closures."
  (let ((native-teardown
          (or (vulkan-device-destroy-teardown device)
              (setf (vulkan-device-destroy-teardown device)
                    (make-vulkan-device-destroy-teardown device queue)))))
    (values
     native-teardown
     (or (vulkan-device-finalizer-teardown device)
         (setf
          (vulkan-device-finalizer-teardown device)
          (let ((native-device (vulkan-handle device))
                (native-retired-box
                  (vulkan-device-native-retired-box device)))
            (wrap-vulkan-gpu-driver-teardown
             (make-gpu-retirement-sequence
              (lambda ()
                (unless (car native-retired-box)
                  (lvk:device-wait-idle native-device)))
              native-teardown))))))))

(defmethod destroy ((device vulkan-gpu-device))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p device)
      (let ((queue (vulkan-device-queue device)))
        (flet ((tear-down-device ()
                 (funcall
                  (or (vulkan-device-destroy-admission device)
                      (setf (vulkan-device-destroy-admission device)
                            (make-vulkan-device-destroy-admission
                             device queue))))
                 ;; Admission is closed after the first successful barrier.
                 ;; Recheck on every partial-teardown retry so an invariant
                 ;; violation can never be silently skipped.
                 (when queue
                   (ensure-gpu-retirement-ledger-empty
                    (vulkan-queue-retirement-ledger queue)
                    :operation :destroy-vulkan-device))
                 (multiple-value-bind (native-teardown finalizer-teardown)
                     (ensure-vulkan-device-retirement-teardowns device queue)
                   (declare (ignore finalizer-teardown))
                   (funcall native-teardown))
                 ;; Logical publication is deliberately outside the shared
                 ;; native sequence so its leak finalizer never captures the
                 ;; DEVICE or QUEUE wrappers through their back reference.
                 (sb-ext:cancel-finalization device)
                 (setf (vulkan-object-destroyed-p device) t)
                 (when queue
                   (setf (vulkan-object-destroyed-p queue) t))))
          ;; This Lisp lock outlives every native queue/device phase and stays
          ;; usable across failures in the persistent teardown sequence.
          (if queue
              (sb-thread:with-recursive-lock ((vulkan-queue-lock queue))
                (tear-down-device))
              (tear-down-device))))))
  (values))
