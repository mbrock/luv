(defpackage #:luv
  (:use #:cl)
  (:export #:*instance*
           #:*physical-device*
           #:*surface*
           #:*device*
           #:*swapchain*
           #:*queue*
           #:probe
           #:headless-probe
           #:open-headless
           #:close-window
           #:window-open-p
           #:render-color))

(in-package #:luv)

(defvar *instance* nil)
(defvar *physical-device* nil)
(defvar *surface* nil)
(defvar *device* nil)
(defvar *swapchain* nil)
(defvar *queue* nil)
(defvar *swapchain-images* nil)
(defvar *queue-family* nil)
(defvar *surface-format* nil)
(defvar *swapchain-extent* nil)
(defvar *context-thread* nil)
(defvar *context-close-requested* nil)
(defvar *context-ready* nil)
(defvar *context-running* nil)
(defvar *context-startup-error* nil)
(defvar *render-lock* (sb-thread:make-mutex :name "luv headless render lock"))
(defvar *render-request-lock*
  (sb-thread:make-mutex :name "luv headless render request lock"))
(defvar *render-requests* nil)

(defstruct render-request
  red
  green
  blue
  alpha
  (completion (sb-thread:make-semaphore :count 0) :read-only t)
  error)

;; The vendored vk release reports positive VkResult success codes as ordinary
;; conditions.  Return the keyword directly so high-level wrappers keep working.
(defmethod cffi:translate-from-foreign :around
    ((value integer) (type %vk::checked-result))
  (if (plusp value)
      (cffi:foreign-enum-keyword '%vk:result value)
      (call-next-method)))

(defun format-api-version (version)
  (format nil "~D.~D.~D"
          (vk:api-version-major version)
          (vk:api-version-minor version)
          (vk:api-version-patch version)))

(defun physical-device-info (device)
  (let ((properties (vk:get-physical-device-properties device)))
    (list :name (vk:device-name properties)
          :type (vk:device-type properties)
          :api-version (format-api-version (vk:api-version properties))
          :vendor-id (vk:vendor-id properties)
          :device-id (vk:device-id properties))))

(defun available-instance-extension-names ()
  (mapcar #'vk:extension-name
          (vk:enumerate-instance-extension-properties)))

(defun luv-instance-create-info (application-name extensions)
  (let* ((available (available-instance-extension-names))
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
     (if portability-p
         (adjoin portability-extension extensions :test #'string=)
         extensions))))

(defun headless-vulkan-instance-extensions ()
  (let ((extensions (list vk:+khr-surface-extension-name+
                          vk:+ext-headless-surface-extension-name+)))
    (dolist (extension extensions)
      (unless (member extension (available-instance-extension-names)
                      :test #'string=)
        (error "The Vulkan loader does not advertise ~A." extension)))
    extensions))

(defun probe (&optional (stream *standard-output*))
  (let ((loader-version (vk:enumerate-instance-version))
        (create-info (luv-instance-create-info "luv" nil)))
    (vk-utils:with-instance (instance create-info)
      (let ((devices (mapcar #'physical-device-info
                             (vk:enumerate-physical-devices instance))))
        (format stream "Vulkan loader API: ~A~%"
                (format-api-version loader-version))
        (format stream "Physical devices: ~D~%" (length devices))
        (loop for device in devices
              for index from 0
              do (format stream "  [~D] ~A (~A, API ~A)~%"
                         index
                         (getf device :name)
                         (getf device :type)
                         (getf device :api-version)))
        (list :loader-api-version (format-api-version loader-version)
              :physical-devices devices)))))

(defun surface-capabilities-info (capabilities)
  (let* ((extent (vk:current-extent capabilities))
         (width (vk:width extent))
         (height (vk:height extent)))
    (list :min-image-count (vk:min-image-count capabilities)
          :max-image-count (vk:max-image-count capabilities)
          :current-extent (if (and (= width #xffffffff)
                                   (= height #xffffffff))
                              :variable
                              (list width height)))))

(defun headless-probe (&optional (stream *standard-output*))
  (let* ((extensions (headless-vulkan-instance-extensions))
         (create-info
           (luv-instance-create-info "luv headless probe" extensions)))
    (vk-utils:with-instance (instance create-info)
      (vk-utils:with-headless-surface-ext
          (surface instance (vk:make-headless-surface-create-info-ext))
        (let* ((device (first (vk:enumerate-physical-devices instance)))
               (capabilities
                 (and device
                      (vk:get-physical-device-surface-capabilities-khr
                       device surface)))
               (formats
                 (and device
                      (vk:get-physical-device-surface-formats-khr
                       device surface)))
               (present-modes
                 (and device
                      (vk:get-physical-device-surface-present-modes-khr
                       device surface)))
               (present-queues
                 (and device
                      (loop for index below
                              (length
                               (vk:get-physical-device-queue-family-properties
                                device))
                            when (vk:get-physical-device-surface-support-khr
                                  device index surface)
                              collect index)))
               (result
                 (list
                  :instance-extensions extensions
                  :device (and device (physical-device-info device))
                  :capabilities
                  (and capabilities
                       (surface-capabilities-info capabilities))
                  :formats
                  (mapcar (lambda (surface-format)
                            (list (vk:format surface-format)
                                  (vk:color-space surface-format)))
                          formats)
                  :present-modes present-modes
                  :present-queue-families present-queues)))
          (format stream "Headless Vulkan extensions: ~{~A~^, ~}~%"
                  extensions)
          (format stream "Physical device: ~A~%"
                  (getf (getf result :device) :name))
          (format stream "Surface formats: ~D; present modes: ~{~A~^, ~}~%"
                  (length formats) present-modes)
          (format stream "Present-capable queue families: ~{~D~^, ~}~%"
                  present-queues)
          result)))))

(defun graphics-present-queue-family ()
  (or (loop for properties in
              (vk:get-physical-device-queue-family-properties *physical-device*)
            for index from 0
            when (and (member :graphics (vk:queue-flags properties))
                      (vk:get-physical-device-surface-support-khr
                       *physical-device* index *surface*))
              return index)
      (error "No Vulkan queue family supports both graphics and presentation.")))

(defun preferred-surface-format ()
  (let ((formats
          (vk:get-physical-device-surface-formats-khr
           *physical-device* *surface*)))
    (or (find-if (lambda (surface-format)
                   (and (eq :b8g8r8a8-srgb (vk:format surface-format))
                        (eq :srgb-nonlinear-khr
                            (vk:color-space surface-format))))
                 formats)
        (first formats)
        (error "The headless surface exposes no image formats."))))

(defun clamp-to-range (value minimum maximum)
  (max minimum (min value maximum)))

(defun swapchain-extent (capabilities width height)
  (let ((current (vk:current-extent capabilities)))
    (if (/= #xffffffff (vk:width current))
        current
        (let ((minimum (vk:min-image-extent capabilities))
              (maximum (vk:max-image-extent capabilities)))
          (vk:make-extent-2d
           :width (clamp-to-range width
                                  (vk:width minimum)
                                  (vk:width maximum))
           :height (clamp-to-range height
                                   (vk:height minimum)
                                   (vk:height maximum)))))))

(defun desired-swapchain-image-count (capabilities)
  (let ((desired (1+ (vk:min-image-count capabilities)))
        (maximum (vk:max-image-count capabilities)))
    (if (plusp maximum)
        (min desired maximum)
        desired)))

(defun headless-swapchain-create-info (surface-format width height)
  (let* ((capabilities
           (vk:get-physical-device-surface-capabilities-khr
            *physical-device* *surface*))
         (extent (swapchain-extent capabilities width height)))
    (unless (member :transfer-dst (vk:supported-usage-flags capabilities))
      (error "The headless surface cannot be cleared as a transfer destination."))
    (values
     (vk:make-swapchain-create-info-khr
      :surface *surface*
      :min-image-count (desired-swapchain-image-count capabilities)
      :image-format (vk:format surface-format)
      :image-color-space (vk:color-space surface-format)
      :image-extent extent
      :image-array-layers 1
      :image-usage (list :transfer-dst)
      :image-sharing-mode :exclusive
      :pre-transform (vk:current-transform capabilities)
      :composite-alpha
      (if (member :opaque (vk:supported-composite-alpha capabilities))
          :opaque
          (first (vk:supported-composite-alpha capabilities)))
      :present-mode :fifo-khr
      :clipped t)
     extent)))

(defun color-subresource-range ()
  (vk:make-image-subresource-range
   :aspect-mask (list :color)
   :base-mip-level 0
   :level-count 1
   :base-array-layer 0
   :layer-count 1))

(defun transition-swapchain-image
    (command-buffer image range old-layout new-layout
     src-access dst-access src-stage dst-stage)
  (vk:cmd-pipeline-barrier
   command-buffer nil nil
   (list
    (vk:make-image-memory-barrier
     :src-access-mask src-access
     :dst-access-mask dst-access
     :old-layout old-layout
     :new-layout new-layout
     :src-queue-family-index vk:+queue-family-ignored+
     :dst-queue-family-index vk:+queue-family-ignored+
     :image image
     :subresource-range range))
   src-stage dst-stage))

(defun record-color-clear (command-buffer image color)
  (let ((range (color-subresource-range)))
    (vk:begin-command-buffer
     command-buffer
     (vk:make-command-buffer-begin-info :flags (list :one-time-submit)))
    (transition-swapchain-image
     command-buffer image range
     :undefined :transfer-dst-optimal
     nil (list :transfer-write)
     (list :top-of-pipe) (list :transfer))
    (vk:cmd-clear-color-image
     command-buffer image :transfer-dst-optimal
     (vk:make-clear-color-value :float-32 color)
     (list range))
    (transition-swapchain-image
     command-buffer image range
     :transfer-dst-optimal :present-src-khr
     (list :transfer-write) nil
     (list :transfer) (list :bottom-of-pipe))
    (vk:end-command-buffer command-buffer)))

(defun active-context-p ()
  (and *instance* *physical-device* *surface*
       *device* *swapchain* *queue*))

(defun %render-color (red green blue alpha)
  (sb-thread:with-mutex (*render-lock*)
    (unless (active-context-p)
      (error "No luv headless context is open. Call LUV:OPEN-HEADLESS first."))
    (let ((color
            (map 'vector
                 (lambda (component) (coerce component 'single-float))
                 (list red green blue alpha)))
          (pool-create-info
            (vk:make-command-pool-create-info
             :queue-family-index *queue-family*)))
      (vk-utils:with-command-pool
          (command-pool *device* pool-create-info)
        (vk-utils:with-command-buffers
            (command-buffers *device*
             (vk:make-command-buffer-allocate-info
              :command-pool command-pool
              :level :primary
              :command-buffer-count 1))
          (vk-utils:with-semaphore
              (image-ready *device* (vk:make-semaphore-create-info))
            (vk-utils:with-semaphore
                (render-done *device* (vk:make-semaphore-create-info))
              (let* ((image-index
                       (vk:acquire-next-image-khr
                        *device* *swapchain* #xffffffffffffffff image-ready))
                     (command-buffer (first command-buffers)))
                (record-color-clear
                 command-buffer (nth image-index *swapchain-images*) color)
                (vk:queue-submit
                 *queue*
                 (list
                  (vk:make-submit-info
                   :wait-semaphores (list image-ready)
                   :wait-dst-stage-mask (list :transfer)
                   :command-buffers (list command-buffer)
                   :signal-semaphores (list render-done))))
                (vk:queue-present-khr
                 *queue*
                 (vk:make-present-info-khr
                  :wait-semaphores (list render-done)
                  :swapchains (list *swapchain*)
                  :image-indices (list image-index)))
                (vk:queue-wait-idle *queue*)))))))
    (values red green blue alpha)))

(defun finish-render-request (request error)
  (setf (render-request-error request) error)
  (sb-thread:signal-semaphore (render-request-completion request)))

(defun take-render-requests ()
  (sb-thread:with-mutex (*render-request-lock*)
    (prog1 *render-requests*
      (setf *render-requests* nil))))

(defun process-render-requests ()
  (dolist (request (take-render-requests))
    (handler-case
        (progn
          (%render-color
           (render-request-red request)
           (render-request-green request)
           (render-request-blue request)
           (render-request-alpha request))
          (finish-render-request request nil))
      (error (condition)
        (finish-render-request request condition)))))

(defun fail-pending-render-requests (condition)
  (dolist (request (take-render-requests))
    (finish-render-request request condition)))

(defun submit-render-request (red green blue alpha)
  (unless (active-context-p)
    (error "No luv headless context is open. Call LUV:OPEN-HEADLESS first."))
  (let ((request (make-render-request
                  :red red :green green :blue blue :alpha alpha)))
    (sb-thread:with-mutex (*render-request-lock*)
      (setf *render-requests* (nconc *render-requests* (list request))))
    (sb-thread:wait-on-semaphore (render-request-completion request))
    (when (render-request-error request)
      (error (render-request-error request)))
    (values red green blue alpha)))

(defun render-color (red green blue &optional (alpha 1.0))
  (if (eq *context-thread* sb-thread:*current-thread*)
      (%render-color red green blue alpha)
      (submit-render-request red green blue alpha)))

(defun wait-for-close (&optional duration)
  (let ((deadline
          (and duration
               (+ (get-internal-real-time)
                  (* duration internal-time-units-per-second)))))
    (loop
      (process-render-requests)
      (when *context-close-requested*
        (return))
      (sleep 0.01)
      (when (and deadline (>= (get-internal-real-time) deadline))
        (return)))))

(defun run-headless-context
    (&key (width 800) (height 600) duration (stream *standard-output*))
  (let* ((extensions (headless-vulkan-instance-extensions))
         (instance-create-info
           (luv-instance-create-info "luv headless" extensions)))
    (vk-utils:with-instance (instance instance-create-info)
      (setf *instance* instance)
      (unwind-protect
           (vk-utils:with-headless-surface-ext
               (surface instance (vk:make-headless-surface-create-info-ext))
             (setf *surface* surface
                   *physical-device*
                   (first (vk:enumerate-physical-devices instance)))
             (unless *physical-device*
               (error "Vulkan found no physical devices."))
             (let* ((queue-family (graphics-present-queue-family))
                    (surface-format (preferred-surface-format))
                    (device-create-info
                      (vk:make-device-create-info
                       :queue-create-infos
                       (list (vk:make-device-queue-create-info
                              :queue-family-index queue-family
                              :queue-priorities (list 1.0)))
                       :enabled-extension-names
                       (list vk:+khr-swapchain-extension-name+))))
               (vk-utils:with-device
                   (device *physical-device* device-create-info)
                 (setf *device* device
                       *queue-family* queue-family
                       *surface-format* surface-format)
                 (unwind-protect
                      (multiple-value-bind (swapchain-create-info extent)
                          (headless-swapchain-create-info
                           surface-format width height)
                        (vk-utils:with-swapchain-khr
                            (swapchain device swapchain-create-info)
                          (setf *swapchain* swapchain
                                *queue* (vk:get-device-queue
                                         device queue-family 0)
                                *swapchain-images*
                                (vk:get-swapchain-images-khr
                                 device swapchain)
                                *swapchain-extent* extent)
                          (unwind-protect
                               (progn
                                 (%render-color 1.0 1.0 0.0 1.0)
                                 (setf *context-ready* t)
                                 (format stream
                                         "Luv headless surface: ~Dx~D, ~A, queue family ~D.~%"
                                         (vk:width extent) (vk:height extent)
                                         (vk:format surface-format)
                                         queue-family)
                                 (wait-for-close duration))
                            (sb-thread:with-mutex (*render-lock*)
                              (fail-pending-render-requests
                               (make-condition
                                'simple-error
                                :format-control
                                "The luv headless context closed before rendering."))
                              (setf *swapchain* nil
                                    *queue* nil
                                    *swapchain-images* nil
                                    *swapchain-extent* nil)))))
                   (sb-thread:with-mutex (*render-lock*)
                     (setf *device* nil
                           *queue-family* nil
                           *surface-format* nil)))))
        (setf *physical-device* nil
              *surface* nil
              *instance* nil))))))

(defun context-thread-main (arguments)
  (unwind-protect
       (handler-case
           (apply #'run-headless-context arguments)
         (error (condition)
           (setf *context-startup-error* condition)))
    (setf *context-ready* nil
          *context-running* nil)))

(defun open-headless
    (&key (width 800) (height 600) duration (stream *standard-output*))
  (when *context-running*
    (error "A luv headless context is already open."))
  (setf *context-close-requested* nil
        *context-ready* nil
        *context-startup-error* nil
        *context-running* t)
  (let ((arguments
          (list :width width :height height
                :duration duration :stream stream)))
    (setf *context-thread*
          (sb-thread:make-thread
           (lambda () (context-thread-main arguments))
           :name "luv headless context")))
  (loop repeat 3000
        when (window-open-p)
          return t
        when *context-startup-error*
          do (error *context-startup-error*)
        unless *context-running*
          do (error "The luv headless context stopped during startup.")
        do (sleep 0.01)
        finally (error "Timed out while opening the luv headless context.")))

(defun window-open-p ()
  (and *context-ready* *context-running* (active-context-p)))

(defun close-window ()
  (let ((thread *context-thread*))
    (when *context-running*
      (setf *context-close-requested* t)
      (when (and thread
                 (sb-thread:thread-alive-p thread)
                 (not (eq thread sb-thread:*current-thread*)))
        (sb-thread:join-thread thread)))
    (setf *context-thread* nil
          *context-close-requested* nil
          *context-ready* nil
          *context-running* nil))
  (values))
