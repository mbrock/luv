(in-package #:luv)

(defun graphics-present-queue-family ()
  "Find one queue family that can both clear and present `*surface*'."
  (or (loop for properties in
              (vk:get-physical-device-queue-family-properties *physical-device*)
            for index from 0
            when (and (member :graphics (vk:queue-flags properties))
                      (vk:get-physical-device-surface-support-khr
                       *physical-device* index *surface*))
              return index)
      (error "No Vulkan queue family supports both graphics and presentation.")))

(defun preferred-surface-format ()
  "Choose a familiar sRGB swapchain format, falling back to the first one."
  (let ((formats
          (vk:get-physical-device-surface-formats-khr
           *physical-device* *surface*)))
    (or (find-if (lambda (surface-format)
                   (and (eq :b8g8r8a8-srgb (vk:format surface-format))
                        (eq :srgb-nonlinear-khr
                            (vk:color-space surface-format))))
                 formats)
        (first formats)
        (error "The Vulkan surface exposes no image formats."))))

(defun clamp-to-range (value minimum maximum)
  (max minimum (min value maximum)))

(defun swapchain-extent (capabilities)
  "Choose the configured surface extent, or clamp SDL's pixel size to its range."
  (let ((current (vk:current-extent capabilities)))
    (if (/= #xffffffff (vk:width current))
        current
        (let ((minimum (vk:min-image-extent capabilities))
              (maximum (vk:max-image-extent capabilities)))
          (flet ((clamped-extent (width height)
                   (vk:make-extent-2d
                    :width (clamp-to-range width
                                           (vk:width minimum)
                                           (vk:width maximum))
                    :height (clamp-to-range height
                                            (vk:height minimum)
                                            (vk:height maximum)))))
            (if *window*
                (multiple-value-bind (success width height)
                    (sdl3:get-window-size-in-pixels *window*)
                  (unless success
                    (error "SDL could not report the window's pixel size: ~A"
                           (sdl3:get-error)))
                  (clamped-extent width height))
                (clamped-extent (vk:width *headless-extent*)
                                (vk:height *headless-extent*))))))))

(defun desired-swapchain-image-count (capabilities)
  "Request one more image than the surface minimum, respecting a finite maximum."
  (let ((desired (1+ (vk:min-image-count capabilities)))
        (maximum (vk:max-image-count capabilities)))
    (if (plusp maximum)
        (min desired maximum)
        desired)))

(defun yellow-swapchain-create-info (surface-format)
  (let* ((capabilities
           (vk:get-physical-device-surface-capabilities-khr
            *physical-device* *surface*))
         (extent (swapchain-extent capabilities)))
    (unless (member :transfer-dst (vk:supported-usage-flags capabilities))
      (error "The Vulkan surface cannot be cleared as a transfer destination."))
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
  "Record a one-shot command buffer that clears IMAGE to COLOR."
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

(defun closing-event-p (event-type)
  (member event-type '(:quit :window-close-requested)))

(defun wait-for-window-close (&optional duration)
  "Dispatch SDL events until the window closes or DURATION seconds elapse."
  (let ((deadline
          (and duration
               (+ (get-internal-real-time)
                  (* duration internal-time-units-per-second)))))
    (loop
      (process-render-requests)
      (when *window-close-requested*
        (return))
      #+darwin
      (multiple-value-bind (event event-type)
          (sdl3:poll-event*)
        (declare (ignore event))
        (when (closing-event-p event-type)
          (return)))
      #+darwin
      (sleep 0.01)
      #-darwin
      (multiple-value-bind (event event-type)
          (sdl3:wait-event-timeout* 50)
        (declare (ignore event))
        (when (closing-event-p event-type)
          (return)))
      (when (and deadline (>= (get-internal-real-time) deadline))
        (return)))))

(defun wait-for-headless-close (&optional duration)
  "Service queued renders without relying on an SDL event source."
  (let ((deadline
          (and duration
               (+ (get-internal-real-time)
                  (* duration internal-time-units-per-second)))))
    (loop
      (process-render-requests)
      (when *window-close-requested*
        (return))
      (sleep 0.01)
      (when (and deadline (>= (get-internal-real-time) deadline))
        (return)))))

(defun active-context-p ()
  (and (or *window* *headless-extent*)
       *instance* *physical-device* *surface*
       *device* *swapchain* *queue*))

(defun %render-color (red green blue alpha)
  (with-native-graphics-environment
    (sb-thread:with-mutex (*render-lock*)
      (unless (active-context-p)
        (error "No luv window is open. Call LUV:OPEN-WINDOW first."))
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
      (values red green blue alpha))))

(defun submit-render-request (red green blue alpha)
  "Ask the native window thread to render, then return its result."
  (unless (active-context-p)
    (error "No luv window is open. Call LUV:OPEN-WINDOW first."))
  (let ((request (make-render-request
                  :red red :green green :blue blue :alpha alpha)))
    (sb-thread:with-mutex (*render-request-lock*)
      (setf *render-requests* (nconc *render-requests* (list request))))
    (sb-thread:wait-on-semaphore (render-request-completion request))
    (when (render-request-error request)
      (error (render-request-error request)))
    (values red green blue alpha)))

(defun finish-render-request (request error)
  (setf (render-request-error request) error)
  (sb-thread:signal-semaphore (render-request-completion request)))

(defun take-render-requests ()
  (sb-thread:with-mutex (*render-request-lock*)
    (prog1 *render-requests*
      (setf *render-requests* nil))))

(defun process-render-requests ()
  "Render every request currently waiting for the native window thread."
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

(defun render-color (red green blue &optional (alpha 1.0))
  "Clear the ambient swapchain to RED, GREEN, BLUE, and ALPHA, then present it."
  #+darwin
  (if (trivial-main-thread:main-thread-p)
      (%render-color red green blue alpha)
      (submit-render-request red green blue alpha))
  #-darwin
  (%render-color red green blue alpha))

(defun run-window-context (&key duration (stream *standard-output*))
  "Own the logical device and swapchain until the ambient window closes."
  (let* ((queue-family
           (graphics-present-queue-family))
         (surface-format
           (preferred-surface-format))
         (device-create-info
           (vk:make-device-create-info
            :queue-create-infos
            (list (vk:make-device-queue-create-info
                   :queue-family-index queue-family
                   :queue-priorities (list 1.0)))
            :enabled-extension-names
            (list vk:+khr-swapchain-extension-name+))))
    (vk-utils:with-device (device *physical-device* device-create-info)
      (setf *device* device
            *queue-family* queue-family
            *surface-format* surface-format)
      (unwind-protect
           (multiple-value-bind (swapchain-create-info extent)
               (yellow-swapchain-create-info surface-format)
             (vk-utils:with-swapchain-khr
                 (swapchain device swapchain-create-info)
               (setf *swapchain* swapchain
                     *queue* (vk:get-device-queue device queue-family 0)
                     *swapchain-images*
                     (vk:get-swapchain-images-khr device swapchain)
                     *swapchain-extent* extent)
               (unwind-protect
                    (progn
                      (%render-color 1.0 1.0 0.0 1.0)
                      (setf *window-context-ready* t)
                      (format stream
                              "Luv ~A: ~Dx~D, ~A, queue family ~D.~%"
                              (if *window* "window" "headless surface")
                              (vk:width extent) (vk:height extent)
                              (vk:format surface-format) queue-family)
                      (format stream
                              "Try (luv:render-color 1.0 0.0 1.0), or close the context.~%")
                      (if *window*
                          (wait-for-window-close duration)
                          (wait-for-headless-close duration)))
                 (sb-thread:with-mutex (*render-lock*)
                   (fail-pending-render-requests
                    (make-condition
                     'simple-error
                     :format-control
                     "The luv window closed before rendering."))
                   (setf *swapchain* nil
                         *queue* nil
                         *swapchain-images* nil
                         *swapchain-extent* nil)))))
        (sb-thread:with-mutex (*render-lock*)
          (setf *device* nil
                *queue-family* nil
                *surface-format* nil
                *headless-extent* nil))))))

(defun run-window
    (&key (width 800) (height 600) duration (stream *standard-output*))
  "Own the SDL/Vulkan context and event loop on the window thread."
  (with-native-graphics-environment
    (unless (sdl3:init :video)
      (error "SDL video initialization failed: ~A" (sdl3:get-error)))
    (unwind-protect
       (let ((window
               (sdl3:create-window
                "luv — Vulkan yellow" width height '(:vulkan :resizable))))
         (when (cffi:null-pointer-p window)
           (error "SDL window creation failed: ~A" (sdl3:get-error)))
         (setf *window* window)
         (unwind-protect
              (let* ((extensions (sdl-vulkan-instance-extensions))
                     (instance-create-info
                       (luv-instance-create-info "luv window" extensions)))
                (vk-utils:with-instance (instance instance-create-info)
                  (setf *instance* instance)
                  (multiple-value-bind (raw-surface surface)
                      (create-sdl-vulkan-surface *window* instance)
                    (setf *surface* surface)
                    (unwind-protect
                         (progn
                           (setf *physical-device*
                                 (first (vk:enumerate-physical-devices instance)))
                           (unless *physical-device*
                             (error "Vulkan found no physical devices."))
                           (format stream "SDL video driver: ~A~%"
                                   (sdl3:get-current-video-driver))
                           (run-window-context
                            :duration duration :stream stream))
                      (setf *physical-device* nil)
                      (unwind-protect
                           (sdl3:vulkan-destroy-surface
                            (vk:raw-handle instance)
                            raw-surface
                            (cffi:null-pointer))
                        (setf *surface* nil))))))
              (setf *instance* nil))
           (unwind-protect
                (sdl3:destroy-window *window*)
             (setf *window* nil)))
      (sdl3:quit)))
  (values))

(defun run-headless
    (&key (width 800) (height 600) duration (stream *standard-output*))
  "Own a VK_EXT_headless_surface context without creating an SDL window."
  (with-native-graphics-environment
    (let* ((extensions (headless-vulkan-instance-extensions))
           (instance-create-info
             (luv-instance-create-info "luv headless" extensions)))
      (vk-utils:with-instance (instance instance-create-info)
        (setf *instance* instance)
        (unwind-protect
             (vk-utils:with-headless-surface-ext
                 (surface instance (vk:make-headless-surface-create-info-ext))
               (setf *surface* surface
                     *headless-extent*
                     (vk:make-extent-2d :width width :height height))
               (unwind-protect
                    (progn
                      (setf *physical-device*
                            (first (vk:enumerate-physical-devices instance)))
                      (unless *physical-device*
                        (error "Vulkan found no physical devices."))
                      (run-window-context :duration duration :stream stream))
                 (setf *physical-device* nil
                       *surface* nil)))
          (setf *instance* nil)))))
  (values))

(defun window-open-p ()
  "Return true while luv's ambient window context is ready for rendering."
  (and *window-context-ready*
       *window-context-running*
       (active-context-p)))

(defun window-thread-main (arguments)
  (unwind-protect
       (handler-case
           (apply #'run-window arguments)
         (error (condition)
           (setf *window-startup-error* condition)))
    (setf *window-context-ready* nil
          *window-context-running* nil)))

(defun headless-thread-main (arguments)
  (unwind-protect
       (handler-case
           (apply #'run-headless arguments)
         (error (condition)
           (setf *window-startup-error* condition)))
    (setf *window-context-ready* nil
          *window-context-running* nil)))

(defun start-window-context (arguments)
  "Start the platform-owned window context and remember its owner thread."
  #+darwin
  (progn
    (setf *window-thread* (trivial-main-thread:main-thread))
    ;; Always dispatch from a worker. If OPEN-WINDOW itself was called on
    ;; thread zero, trivial-main-thread can then move its continuation aside
    ;; before giving Cocoa the real process main thread.
    (sb-thread:make-thread
     (lambda ()
       (trivial-main-thread:call-in-main-thread
        (lambda () (window-thread-main arguments))))
     :name "luv Cocoa main-thread dispatcher"))
  #-darwin
  (setf *window-thread*
        (sb-thread:make-thread
         (lambda () (window-thread-main arguments))
         :name "luv window context")))

(defun wait-for-window-context-stop ()
  (loop repeat 3000
        unless *window-context-running* do (return)
        do (sleep 0.01)
        finally (error "Timed out while closing the luv window.")))

(defun open-window
    (&key (width 800) (height 600) duration (stream *standard-output*))
  "Open an ambient SDL/Vulkan context, present yellow, and return its window."
  (when *window-context-running*
    (error "A luv window is already open."))
  (setf *window-close-requested* nil
        *window-context-ready* nil
        *window-startup-error* nil
        *window-context-running* t)
  (let ((arguments
          (list :width width :height height
                :duration duration :stream stream)))
    (start-window-context arguments))
  (loop repeat 3000
        when (window-open-p)
          return *window*
        when *window-startup-error*
          do (error *window-startup-error*)
        unless *window-context-running*
          do (error "The luv window thread stopped during startup.")
        do (sleep 0.01)
        finally (error "Timed out while opening the luv window.")))

(defun open-headless
    (&key (width 800) (height 600) duration (stream *standard-output*))
  "Open an ambient headless Vulkan context, present yellow, and return T."
  (when *window-context-running*
    (error "A luv context is already open."))
  (setf *window-close-requested* nil
        *window-context-ready* nil
        *window-startup-error* nil
        *window-context-running* t)
  (let ((arguments
          (list :width width :height height
                :duration duration :stream stream)))
    #+darwin
    (progn
      (setf *window-thread* (trivial-main-thread:main-thread))
      (sb-thread:make-thread
       (lambda ()
         (trivial-main-thread:call-in-main-thread
          (lambda () (headless-thread-main arguments))))
       :name "luv Cocoa headless dispatcher"))
    #-darwin
    (setf *window-thread*
          (sb-thread:make-thread
           (lambda () (headless-thread-main arguments))
           :name "luv headless context")))
  (loop repeat 3000
        when (window-open-p)
          return t
        when *window-startup-error*
          do (error *window-startup-error*)
        unless *window-context-running*
          do (error "The luv headless context stopped during startup.")
        do (sleep 0.01)
        finally (error "Timed out while opening the luv headless context.")))

(defun close-window ()
  "Ask the ambient window to close, wait for teardown, and return no values."
  (let ((thread *window-thread*))
    (declare (ignorable thread))
    (when *window-context-running*
      (setf *window-close-requested* t)
      #-darwin
      (when (and thread
                 (sb-thread:thread-alive-p thread)
                 (not (eq thread sb-thread:*current-thread*)))
        (sb-thread:join-thread thread))
      #+darwin
      (wait-for-window-context-stop))
    (setf *window-thread* nil
          *window-close-requested* nil
          *window-context-ready* nil
          *window-context-running* nil))
  (values))

(defun yellow-window (&rest arguments)
  "Compatibility entry point for `open-window'."
  (apply #'open-window arguments))

(defun main ()
  "Open the ambient SDL-backed Vulkan window and return immediately."
  (open-window))
