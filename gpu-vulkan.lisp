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
       (otherwise
        (format stream "Vulkan GPU operation ~S failed: ~S~@[ (~S)~]"
                (gpu-error-operation condition)
                (vulkan-gpu-error-reason condition)
                (vulkan-gpu-error-details condition)))))))

(defun vulkan-gpu-instance-create-info (application-name)
  "Create a portable, presentation-independent Vulkan instance description."
  (let* ((available
           (mapcar #'vk:extension-name
                   (vk:enumerate-instance-extension-properties)))
         (portability-extension
           vk:+khr-portability-enumeration-extension-name+)
         (portability-p
           (member portability-extension available :test #'string=)))
    (vk:make-instance-create-info
     :flags (and portability-p (list :enumerate-portability))
     :application-info
     (vk:make-application-info
      :application-name application-name
      :application-version (vk:make-version 0 0 1)
      :engine-name "luv"
      :engine-version (vk:make-version 0 0 1)
      :api-version vk:+api-version-1-0+)
     :enabled-extension-names
     (and portability-p (list portability-extension)))))

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
              (vk:get-physical-device-queue-family-properties physical-device)
            for index from 0
            when (and (plusp (vk:queue-count properties))
                      (member :graphics (vk:queue-flags properties)))
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

(defun make-vulkan-device-create-info (queue-family)
  (vk:make-device-create-info
   :queue-create-infos
   (list (vk:make-device-queue-create-info
          :queue-family-index queue-family
          :queue-priorities (list 1.0)))))

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
             (progn
               (setf instance
                     (vk:create-instance
                      (vulkan-gpu-instance-create-info
                       (vulkan-provider-application-name provider))))
               (let* ((physical-device
                        (or (first (vk:enumerate-physical-devices instance))
                            (error 'vulkan-gpu-error
                                   :operation :request-device
                                   :reason :no-physical-device)))
                      (queue-family
                        (first-vulkan-graphics-queue-family physical-device)))
                 (setf native-device
                       (vk:create-device
                        physical-device
                        (make-vulkan-device-create-info queue-family)))
                 (let* ((device
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
                           :handle (vk:get-device-queue
                                    native-device queue-family 0)
                           :device device
                           :family queue-family)))
                   (setf (vulkan-device-queue device) queue
                         completed-p t)
                   device)))
          (unless completed-p
            (when native-device
              (vk:destroy-device native-device))
            (when instance
              (vk:destroy-instance instance))))))))

(defmethod device-queue ((device vulkan-gpu-device))
  (ensure-live-vulkan-object device :device-queue)
  (vulkan-device-queue device))

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
                    (vulkan-handle device)
                    (vk:make-command-pool-create-info
                     :flags (list :transient)
                     :queue-family-index (vulkan-device-queue-family device))))
             (let ((command-buffer
                     (first
                      (vk:allocate-command-buffers
                       (vulkan-handle device)
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
             (vulkan-handle device) command-pool)))))))

(defmethod finish ((encoder vulkan-gpu-command-encoder))
  (with-vulkan-gpu-driver-environment
    (unless (eq :recording (vulkan-command-encoder-state encoder))
      (error 'gpu-invalid-state-error
             :object encoder
             :operation :finish
             :state (vulkan-command-encoder-state encoder)
             :expected-state :recording))
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
       :command-pool command-pool))))

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
           :expected-state :ready)))

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
    (when (plusp (length command-buffers))
      (vk:queue-submit
       (vulkan-handle queue)
       (list
        (vk:make-submit-info
         :command-buffers
         (loop for command-buffer across command-buffers
               collect (vulkan-handle command-buffer)))))
      (vk:queue-wait-idle (vulkan-handle queue))
      (loop for command-buffer across command-buffers
            do (setf (vulkan-command-buffer-state command-buffer) :submitted))))
  (values))

(defmethod destroy ((encoder vulkan-gpu-command-encoder))
  (with-vulkan-gpu-driver-environment
    (when (eq :recording (vulkan-command-encoder-state encoder))
      (let ((device (vulkan-command-encoder-device encoder))
            (command-pool (vulkan-command-encoder-command-pool encoder)))
        (when (and command-pool
                   (not (vulkan-object-destroyed-p device)))
          (vk:destroy-command-pool (vulkan-handle device) command-pool)))
      (setf (vulkan-command-encoder-command-pool encoder) nil)))
  (setf (vulkan-command-encoder-state encoder) :destroyed)
  (values))

(defmethod destroy ((command-buffer vulkan-gpu-command-buffer))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p command-buffer)
      (let ((device (vulkan-command-buffer-device command-buffer)))
        (unless (vulkan-object-destroyed-p device)
          (vk:destroy-command-pool
           (vulkan-handle device)
           (vulkan-command-buffer-command-pool command-buffer))))
      (setf (vulkan-object-destroyed-p command-buffer) t
            (vulkan-command-buffer-state command-buffer) :destroyed)))
  (values))

(defmethod destroy ((device vulkan-gpu-device))
  (with-vulkan-gpu-driver-environment
    (unless (vulkan-object-destroyed-p device)
      (let ((queue (vulkan-device-queue device)))
        (unwind-protect
             (vk:device-wait-idle (vulkan-handle device))
          (unwind-protect
               (vk:destroy-device (vulkan-handle device))
            (vk:destroy-instance (vulkan-device-instance device))
            (setf (vulkan-object-destroyed-p device) t)
            (when queue
              (setf (vulkan-object-destroyed-p queue) t)))))))
  (values))
