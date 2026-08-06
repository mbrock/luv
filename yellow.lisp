(in-package #:luv)

(defun graphics-present-queue-family (physical-device surface)
  "Find one queue family that can both clear and present SURFACE."
  (or (loop for properties in
              (vk:get-physical-device-queue-family-properties physical-device)
            for index from 0
            when (and (member :graphics (vk:queue-flags properties))
                      (vk:get-physical-device-surface-support-khr
                       physical-device index surface))
              return index)
      (error "No Vulkan queue family supports both graphics and presentation.")))

(defun preferred-surface-format (physical-device surface)
  "Choose a familiar sRGB swapchain format, falling back to the first one."
  (let ((formats
          (vk:get-physical-device-surface-formats-khr physical-device surface)))
    (or (find-if (lambda (surface-format)
                   (and (eq :b8g8r8a8-srgb (vk:format surface-format))
                        (eq :srgb-nonlinear-khr
                            (vk:color-space surface-format))))
                 formats)
        (first formats)
        (error "The Vulkan surface exposes no image formats."))))

(defun clamp-to-range (value minimum maximum)
  (max minimum (min value maximum)))

(defun swapchain-extent (window capabilities)
  "Choose the configured surface extent, or clamp SDL's pixel size to its range."
  (let ((current (vk:current-extent capabilities)))
    (if (/= #xffffffff (vk:width current))
        current
        (multiple-value-bind (success width height)
            (sdl3:get-window-size-in-pixels window)
          (unless success
            (error "SDL could not report the window's pixel size: ~A"
                   (sdl3:get-error)))
          (let ((minimum (vk:min-image-extent capabilities))
                (maximum (vk:max-image-extent capabilities)))
            (vk:make-extent-2d
             :width (clamp-to-range width
                                    (vk:width minimum)
                                    (vk:width maximum))
             :height (clamp-to-range height
                                     (vk:height minimum)
                                     (vk:height maximum))))))))

(defun desired-swapchain-image-count (capabilities)
  "Request one more image than the surface minimum, respecting a finite maximum."
  (let ((desired (1+ (vk:min-image-count capabilities)))
        (maximum (vk:max-image-count capabilities)))
    (if (plusp maximum)
        (min desired maximum)
        desired)))

(defun yellow-swapchain-create-info
    (window physical-device surface surface-format)
  (let* ((capabilities
           (vk:get-physical-device-surface-capabilities-khr
            physical-device surface))
         (extent (swapchain-extent window capabilities)))
    (unless (member :transfer-dst (vk:supported-usage-flags capabilities))
      (error "The Vulkan surface cannot be cleared as a transfer destination."))
    (values
     (vk:make-swapchain-create-info-khr
      :surface surface
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

(defun record-yellow-clear (command-buffer image)
  "Record a one-shot command buffer that makes IMAGE solid yellow."
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
     (vk:make-clear-color-value :float-32 #(1.0 1.0 0.0 1.0))
     (list range))
    (transition-swapchain-image
     command-buffer image range
     :transfer-dst-optimal :present-src-khr
     (list :transfer-write) nil
     (list :transfer) (list :bottom-of-pipe))
    (vk:end-command-buffer command-buffer)))

(defun closing-event-p (event-type)
  (member event-type '(:quit :window-close-requested)))

(defun wait-for-window-close (&optional duration)
  "Dispatch SDL events until the window closes or DURATION seconds elapse."
  (let ((deadline
          (and duration
               (+ (get-internal-real-time)
                  (* duration internal-time-units-per-second)))))
    (loop
      (multiple-value-bind (event event-type)
          (sdl3:wait-event-timeout* 50)
        (declare (ignore event))
        (when (closing-event-p event-type)
          (return)))
      (when (and deadline (>= (get-internal-real-time) deadline))
        (return)))))

(defun present-yellow
    (window physical-device surface &key duration (stream *standard-output*))
  "Create a device and swapchain, present yellow once, then run the event loop."
  (let* ((queue-family
           (graphics-present-queue-family physical-device surface))
         (surface-format
           (preferred-surface-format physical-device surface))
         (device-create-info
           (vk:make-device-create-info
            :queue-create-infos
            (list (vk:make-device-queue-create-info
                   :queue-family-index queue-family
                   :queue-priorities (list 1.0)))
            :enabled-extension-names
            (list vk:+khr-swapchain-extension-name+))))
    (vk-utils:with-device (device physical-device device-create-info)
      (multiple-value-bind (swapchain-create-info extent)
          (yellow-swapchain-create-info
           window physical-device surface surface-format)
        (vk-utils:with-swapchain-khr
            (swapchain device swapchain-create-info)
          (let* ((queue (vk:get-device-queue device queue-family 0))
                 (images (vk:get-swapchain-images-khr device swapchain))
                 (pool-create-info
                   (vk:make-command-pool-create-info
                    :queue-family-index queue-family)))
            (vk-utils:with-command-pool
                (command-pool device pool-create-info)
              (vk-utils:with-command-buffers
                  (command-buffers device
                   (vk:make-command-buffer-allocate-info
                    :command-pool command-pool
                    :level :primary
                    :command-buffer-count 1))
                (vk-utils:with-semaphore
                    (image-ready device (vk:make-semaphore-create-info))
                  (vk-utils:with-semaphore
                      (render-done device (vk:make-semaphore-create-info))
                    (let* ((image-index
                             (vk:acquire-next-image-khr
                              device swapchain #xffffffffffffffff image-ready))
                           (command-buffer (first command-buffers)))
                      (record-yellow-clear
                       command-buffer (nth image-index images))
                      (vk:queue-submit
                       queue
                       (list
                        (vk:make-submit-info
                         :wait-semaphores (list image-ready)
                         :wait-dst-stage-mask (list :transfer)
                         :command-buffers (list command-buffer)
                         :signal-semaphores (list render-done))))
                      (vk:queue-present-khr
                       queue
                       (vk:make-present-info-khr
                        :wait-semaphores (list render-done)
                        :swapchains (list swapchain)
                        :image-indices (list image-index)))
                      (vk:queue-wait-idle queue)
                      (format stream
                              "Yellow Vulkan window: ~Dx~D, ~A, queue family ~D.~%"
                              (vk:width extent) (vk:height extent)
                              (vk:format surface-format) queue-family)
                      (format stream "Close the window to return to Lisp.~%")
                      (wait-for-window-close duration))))))))))))

(defun yellow-window
    (&key (width 800) (height 600) duration (stream *standard-output*))
  "Open a native SDL Vulkan window and keep a yellow swapchain image visible.

When DURATION is NIL, run until the window is closed. A numeric duration is
useful for automated smoke tests. This first presentation spike intentionally
draws only once; swapchain recreation on resize comes later."
  (unless (sdl3:init :video)
    (error "SDL video initialization failed: ~A" (sdl3:get-error)))
  (unwind-protect
       (let ((window
               (sdl3:create-window
                "luv — Vulkan yellow" width height '(:vulkan :resizable))))
         (when (cffi:null-pointer-p window)
           (error "SDL window creation failed: ~A" (sdl3:get-error)))
         (unwind-protect
              (let* ((extensions (sdl-vulkan-instance-extensions))
                     (instance-create-info
                       (vk:make-instance-create-info
                        :application-info
                        (vk:make-application-info
                         :application-name "luv"
                         :application-version (vk:make-version 0 0 1)
                         :engine-name "luv"
                         :engine-version (vk:make-version 0 0 1)
                         :api-version vk:+api-version-1-0+)
                        :enabled-extension-names extensions)))
                (vk-utils:with-instance (instance instance-create-info)
                  (multiple-value-bind (raw-surface surface)
                      (create-sdl-vulkan-surface window instance)
                    (unwind-protect
                         (let ((physical-device
                                 (first
                                  (vk:enumerate-physical-devices instance))))
                           (unless physical-device
                             (error "Vulkan found no physical devices."))
                           (format stream "SDL video driver: ~A~%"
                                   (sdl3:get-current-video-driver))
                           (present-yellow
                            window physical-device surface
                            :duration duration :stream stream))
                      (sdl3:vulkan-destroy-surface
                       (vk:raw-handle instance)
                       raw-surface
                       (cffi:null-pointer))))))
           (sdl3:destroy-window window)))
    (sdl3:quit))
  (values))

(defun main ()
  "Open the yellow SDL-backed Vulkan window until it is closed."
  (yellow-window))
