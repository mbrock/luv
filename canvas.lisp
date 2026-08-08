(in-package #:luv)

(define-condition canvas-error (gpu-error)
  ((canvas :initarg :canvas :reader canvas-error-canvas)
   (reason :initarg :reason :reader canvas-error-reason)
   (details :initarg :details :initform nil :reader canvas-error-details))
  (:report
   (lambda (condition stream)
     (format stream "Canvas operation ~S failed: ~S~@[ (~A)~]."
             (gpu-error-operation condition)
             (canvas-error-reason condition)
             (canvas-error-details condition)))))

(defclass canvas () ())

(defclass canvas-context ()
  ((canvas :initarg :canvas :reader context-canvas)))

(defclass sdl-canvas (canvas)
  ((title :initarg :title :initform "luv canvas" :reader canvas-title)
   (width :initarg :width :initform 800 :reader canvas-width)
   (height :initarg :height :initform 600 :reader canvas-height)
   (context :initform nil :accessor canvas-context)))

(defclass vulkan-canvas-context (canvas-context)
  ((window :initform nil :accessor canvas-window)
   (device :initform nil :accessor canvas-device)
   (surface :initform nil :accessor canvas-surface)
   (swapchain :initform nil :accessor canvas-swapchain)
   (textures :initform #() :accessor canvas-textures)
   (extent :initform nil :accessor canvas-extent)
   (format :initform nil :accessor canvas-format)
   (image-ready :initform nil :accessor canvas-image-ready)
   (render-done :initform nil :accessor canvas-render-done)
   (current-texture :initform nil :accessor canvas-current-texture)
   (state :initform :new :accessor canvas-state)
   (startup-error :initform nil :accessor canvas-startup-error)
   (close-requested-p :initform nil :accessor canvas-close-requested-p)
   (thread :initform nil :accessor canvas-thread)
   (request-lock
    :initform (sb-thread:make-mutex :name "luv canvas request lock")
    :reader canvas-request-lock)
   (requests :initform nil :accessor canvas-requests)))

(defstruct canvas-request
  function
  (completion (sb-thread:make-semaphore :count 0) :read-only t)
  values
  error)

(defgeneric get-current-texture (canvas-context)
  (:documentation "Return the borrowed texture acquired for the active frame."))

(defmethod get-current-texture ((context vulkan-canvas-context))
  (or (canvas-current-texture context)
      (error 'canvas-error :canvas (context-canvas context)
             :operation :get-current-texture :reason :outside-frame)))

(defun canvas-vulkan-instance-extensions ()
  (cffi:with-foreign-object (count :uint32)
    (let ((names (sdl3:vulkan-get-instance-extensions count)))
      (when (cffi:null-pointer-p names)
        (error "SDL could not report Vulkan instance extensions: ~A"
               (sdl3:get-error)))
      (loop for index below (cffi:mem-ref count :uint32)
            collect (cffi:foreign-string-to-lisp
                     (cffi:mem-aref names :pointer index))))))

(defun create-canvas-surface (window instance)
  (cffi:with-foreign-object (surface :pointer)
    (unless (sdl3:vulkan-create-surface
             window instance (cffi:null-pointer) surface)
      (error "SDL could not create a Vulkan surface: ~A" (sdl3:get-error)))
    (cffi:mem-ref surface :pointer)))

(defun canvas-physical-device-and-queue-family (instance surface)
  (or (loop for physical-device in (lvk:enumerate-physical-devices instance)
            do (loop for properties in
                       (lvk:physical-device-queue-families physical-device)
                     for index from 0
                     when (and (plusp (lvk:queue-family-count properties))
                               (member :graphics
                                       (lvk:queue-family-flags properties))
                               (lvk:surface-supported-p
                                physical-device index surface))
                       do (return-from canvas-physical-device-and-queue-family
                            (values physical-device index))))
      (error 'canvas-error :canvas nil :operation :open
             :reason :no-presentation-queue)))

(defun preferred-canvas-format (physical-device surface)
  (let ((formats (lvk:get-surface-formats physical-device surface)))
    (or (find-if
         (lambda (format)
           (and (eq :b8g8r8a8-srgb
                    (lvk:presentation-format-format format))
                (eq :srgb-nonlinear-khr
                    (lvk:presentation-format-color-space format))))
         formats)
        (first formats)
        (error "The canvas surface exposes no image formats."))))

(defun clamp-canvas-extent (value minimum maximum)
  (max minimum (min value maximum)))

(defun choose-canvas-extent (window capabilities)
  (let ((current (lvk:presentation-capabilities-current-extent capabilities)))
    (if (/= #xffffffff (first current))
        current
        (multiple-value-bind (success width height)
            (sdl3:get-window-size-in-pixels window)
          (unless success
            (error "SDL could not report the canvas pixel size: ~A"
                   (sdl3:get-error)))
          (let ((minimum
                  (lvk:presentation-capabilities-min-image-extent capabilities))
                (maximum
                  (lvk:presentation-capabilities-max-image-extent capabilities)))
            (list (clamp-canvas-extent
                   width (first minimum) (first maximum))
                  (clamp-canvas-extent
                   height (second minimum) (second maximum))))))))

(defun choose-canvas-image-count (capabilities)
  (let ((desired
          (1+ (lvk:presentation-capabilities-min-image-count capabilities)))
        (maximum
          (lvk:presentation-capabilities-max-image-count capabilities)))
    (if (plusp maximum) (min desired maximum) desired)))

(defun canvas-gpu-format (vk-format)
  (or (cdr (assoc vk-format
                  '((:r8g8b8a8-unorm . :rgba8-unorm)
                    (:r8g8b8a8-srgb . :rgba8-unorm-srgb)
                    (:b8g8r8a8-unorm . :bgra8-unorm)
                    (:b8g8r8a8-srgb . :bgra8-unorm-srgb))))
      (error "The initial canvas cannot expose Vulkan format ~S." vk-format)))

(defun configure-vulkan-canvas
    (context instance surface physical-device queue-family)
  (let* ((canvas (context-canvas context))
         (capabilities
           (lvk:get-surface-capabilities physical-device surface))
         (format (preferred-canvas-format physical-device surface))
         (vk-format (lvk:presentation-format-format format))
         (color-space (lvk:presentation-format-color-space format))
         (extent (choose-canvas-extent (canvas-window context) capabilities))
         (composite-alpha
           (or (find :opaque
                     (lvk:presentation-capabilities-composite-alpha
                      capabilities))
               (first (lvk:presentation-capabilities-composite-alpha
                       capabilities)))))
    (unless (member :transfer-dst
                    (lvk:presentation-capabilities-usage capabilities))
      (error 'canvas-error :canvas canvas :operation :open
             :reason :unsupported-surface-usage :details :copy-dst))
    (let* ((device
               (make-vulkan-gpu-device
                instance physical-device queue-family
                (make-device-descriptor :label (canvas-title canvas))
                :enabled-extension-names
                (list lvk:+swapchain-extension-name+)))
             (native-device (vulkan-handle device))
             (swapchain nil))
        (setf (canvas-device context) device)
        (setf swapchain
              (lvk:create-swapchain
               native-device surface vk-format color-space extent
               :min-image-count (choose-canvas-image-count capabilities)
               :usage '(:transfer-dst)
               :pre-transform
               (lvk:presentation-capabilities-current-transform capabilities)
               :composite-alpha composite-alpha
               :present-mode :fifo-khr))
        (setf (canvas-swapchain context) swapchain
              (canvas-extent context) extent
              (canvas-format context) (canvas-gpu-format vk-format)
              (canvas-textures context)
              (map 'vector
                   (lambda (image)
                     (make-borrowed-vulkan-texture
                      device image extent (canvas-gpu-format vk-format)
                      vk-format))
                   (lvk:get-swapchain-images native-device swapchain))
              (canvas-image-ready context) (lvk:create-semaphore native-device)
              (canvas-render-done context) (lvk:create-semaphore native-device)))))

(defun destroy-vulkan-canvas (context instance)
  (let ((device (canvas-device context)))
    (when device
      (ignore-errors (lvk:device-wait-idle (vulkan-handle device)))
      (dolist (texture (coerce (canvas-textures context) 'list))
        (destroy texture))
      (when (canvas-image-ready context)
        (lvk:destroy-semaphore
         (vulkan-handle device) (canvas-image-ready context)))
      (when (canvas-render-done context)
        (lvk:destroy-semaphore
         (vulkan-handle device) (canvas-render-done context)))
      (when (canvas-swapchain context)
        (lvk:destroy-swapchain
         (vulkan-handle device) (canvas-swapchain context))))
    (when (canvas-surface context)
      (sdl3:vulkan-destroy-surface
       instance (canvas-surface context) (cffi:null-pointer)))
    (if device
        (destroy device)
        (when instance (lvk:destroy-instance instance)))))

(defun take-canvas-requests (context)
  (sb-thread:with-mutex ((canvas-request-lock context))
    (prog1 (canvas-requests context)
      (setf (canvas-requests context) nil))))

(defun process-canvas-requests (context)
  (dolist (request (take-canvas-requests context))
    (handler-case
        (setf (canvas-request-values request)
              (multiple-value-list
               (funcall (canvas-request-function request))))
      (error (condition)
        (setf (canvas-request-error request) condition)))
    (sb-thread:signal-semaphore (canvas-request-completion request))))

(defun fail-canvas-requests (context condition)
  (dolist (request (take-canvas-requests context))
    (setf (canvas-request-error request) condition)
    (sb-thread:signal-semaphore (canvas-request-completion request))))

(defun canvas-event-loop (context)
  (loop until (canvas-close-requested-p context)
        do (process-canvas-requests context)
           (multiple-value-bind (event event-type) (sdl3:poll-event*)
             (declare (ignore event))
             (when (member event-type '(:quit :window-close-requested))
               (setf (canvas-close-requested-p context) t)))
           (sleep 0.005)))

(defun run-vulkan-canvas (context)
  (let ((instance nil))
    (with-vulkan-gpu-driver-environment
      (unwind-protect
           (handler-case
               (progn
                 (unless (sdl3:init :video)
                   (error "SDL video initialization failed: ~A"
                          (sdl3:get-error)))
                 (let* ((canvas (context-canvas context))
                        (window
                          (sdl3:create-window
                           (canvas-title canvas)
                           (canvas-width canvas) (canvas-height canvas)
                           '(:vulkan :resizable))))
                   (when (cffi:null-pointer-p window)
                     (error "SDL window creation failed: ~A" (sdl3:get-error)))
                   (setf (canvas-window context) window)
                   (multiple-value-bind (portable-extensions flags)
                       (vulkan-gpu-instance-options)
                     (setf instance
                           (lvk:create-instance
                            :application-name (canvas-title canvas)
                            :flags flags
                            :enabled-extension-names
                            (remove-duplicates
                             (append (canvas-vulkan-instance-extensions)
                                     portable-extensions)
                             :test #'string=)))
                     (let ((surface
                             (create-canvas-surface window instance)))
                       (setf (canvas-surface context) surface)
                       (multiple-value-bind (physical-device queue-family)
                           (canvas-physical-device-and-queue-family
                            instance surface)
                         (configure-vulkan-canvas
                          context instance surface
                          physical-device queue-family))))
                   (setf (canvas-state context) :ready)
                   (canvas-event-loop context)))
             (error (condition)
               (setf (canvas-startup-error context) condition)))
        (setf (canvas-state context) :closing)
        (ignore-errors (destroy-vulkan-canvas context instance))
        (when (canvas-window context)
          (sdl3:destroy-window (canvas-window context)))
        (sdl3:quit)
        (fail-canvas-requests
         context
         (make-condition 'canvas-error :canvas (context-canvas context)
                         :operation :frame :reason :canvas-closed))
        (setf (canvas-state context) :closed)))))

(defun start-vulkan-canvas-thread (context)
  #+darwin
  (progn
    (setf (canvas-thread context) (trivial-main-thread:main-thread))
    (sb-thread:make-thread
     (lambda ()
       (trivial-main-thread:call-in-main-thread
        (lambda () (run-vulkan-canvas context))))
     :name "luv canvas Cocoa dispatcher"))
  #-darwin
  (setf (canvas-thread context)
        (sb-thread:make-thread
         (lambda () (run-vulkan-canvas context))
         :name "luv canvas event loop")))

(defun open-canvas (&key (title "luv canvas") (width 800) (height 600))
  "Open an SDL canvas and its surface-compatible Vulkan device."
  (let* ((canvas (make-instance 'sdl-canvas
                                :title title :width width :height height))
         (context (make-instance 'vulkan-canvas-context :canvas canvas)))
    (setf (canvas-context canvas) context
          (canvas-state context) :starting)
    (start-vulkan-canvas-thread context)
    (loop repeat 6000
          when (eq :ready (canvas-state context)) do (return canvas)
          when (canvas-startup-error context)
            do (error (canvas-startup-error context))
          when (eq :closed (canvas-state context))
            do (error 'canvas-error :canvas canvas :operation :open
                      :reason :closed-during-startup)
          do (sleep 0.005)
          finally (error 'canvas-error :canvas canvas :operation :open
                         :reason :startup-timeout))))

(defun call-on-canvas-thread (context function)
  #+darwin
  (when (trivial-main-thread:main-thread-p)
    (return-from call-on-canvas-thread (funcall function)))
  #-darwin
  (when (eq sb-thread:*current-thread* (canvas-thread context))
    (return-from call-on-canvas-thread (funcall function)))
  (unless (eq :ready (canvas-state context))
    (error 'canvas-error :canvas (context-canvas context)
           :operation :frame :reason :not-ready
           :details (canvas-state context)))
  (let ((request (make-canvas-request :function function)))
    (sb-thread:with-mutex ((canvas-request-lock context))
      (setf (canvas-requests context)
            (nconc (canvas-requests context) (list request))))
    (sb-thread:wait-on-semaphore (canvas-request-completion request))
    (when (canvas-request-error request)
      (error (canvas-request-error request)))
    (values-list (canvas-request-values request))))

(defun %present-canvas-frame (context function)
  (let* ((device (canvas-device context))
         (queue (device-queue device))
         (encoder nil)
         (commands nil))
    (multiple-value-bind (image-index acquire-result)
        (lvk:acquire-next-image
         (vulkan-handle device) (canvas-swapchain context)
         (canvas-image-ready context))
      (declare (ignore acquire-result))
      (let ((texture (aref (canvas-textures context) image-index)))
        (unwind-protect
             (progn
               (setf encoder
                     (create device (make-command-encoder-descriptor))
                     (canvas-current-texture context) texture)
               (funcall function texture encoder)
               (transition-vulkan-texture encoder texture :present-src-khr)
               (setf commands (finish encoder))
               (submit-vulkan-command-buffers
                queue (vector commands)
                :wait-semaphores (vector (canvas-image-ready context))
                :wait-stages (vector '(:transfer))
                :signal-semaphores (vector (canvas-render-done context)))
               (lvk:present
                (vulkan-handle queue) (canvas-swapchain context) image-index
                :wait-semaphores (vector (canvas-render-done context)))
               (lvk:queue-wait-idle (vulkan-handle queue))
               texture)
          (setf (canvas-current-texture context) nil)
          (when commands (destroy commands))
          (when encoder (destroy encoder)))))))

(defun present-canvas-frame (canvas function)
  "Acquire one texture, call FUNCTION with texture and encoder, and present."
  (let ((context (canvas-context canvas)))
    (call-on-canvas-thread
     context (lambda () (%present-canvas-frame context function)))))

(defun render-canvas-color (canvas red green blue &optional (alpha 1.0))
  "Clear and present one canvas frame through the new GPU texture API."
  (present-canvas-frame
   canvas
   (lambda (texture encoder)
     (encode encoder
             (make-gpu-clear-texture-command
              :texture texture
              :color (vector red green blue alpha))))))

(defun close-canvas (canvas)
  "Ask CANVAS's native event loop to tear down all presentation objects."
  (let ((context (canvas-context canvas)))
    (setf (canvas-close-requested-p context) t)
    (loop repeat 6000
          when (eq :closed (canvas-state context)) do (return (values))
          do (sleep 0.005)
          finally (error 'canvas-error :canvas canvas :operation :close
                         :reason :shutdown-timeout))))
