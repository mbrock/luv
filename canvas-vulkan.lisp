;;; Vulkan presentation contexts and their SDL bridge.

(in-package #:luv)

(defclass vulkan-canvas-context (canvas-context)
  ((canvas
    :initarg :canvas
    :reader context-canvas)
   (provider
    :initarg :provider
    :reader vulkan-canvas-provider)
   (instance
    :initarg :instance
    :reader vulkan-canvas-instance)
   (physical-device
    :initarg :physical-device
    :reader vulkan-canvas-physical-device)
   (device
    :initarg :device
    :reader canvas-device)
   (surface
    :initarg :surface
    :reader vulkan-canvas-surface)
   (configuration
    :initform nil
    :accessor canvas-context-configuration)
   (swapchain
    :initform nil
    :accessor vulkan-canvas-swapchain)
   (textures
    :initform #()
    :accessor vulkan-canvas-textures)
   (extent
    :initform nil
    :accessor canvas-extent)
   (format
    :initform nil
    :accessor canvas-format)
   (render-done
    :initform #()
    :accessor vulkan-canvas-render-done)
   (frame-slots
    :initform #()
    :accessor vulkan-canvas-frame-slots)
   (next-frame-slot
    :initform 0
    :accessor vulkan-canvas-next-frame-slot)
   (current-texture
    :initform nil
    :accessor vulkan-canvas-current-texture)
   (state
    :initform :unconfigured
    :accessor canvas-context-state)))

(defmethod context-device ((context vulkan-canvas-context))
  (canvas-device context))

(defclass vulkan-canvas-frame-slot ()
  ((fence
    :initarg :fence
    :reader vulkan-frame-slot-fence)
   (image-ready
    :initarg :image-ready
    :reader vulkan-frame-slot-image-ready)
   (commands
    :initform nil
    :accessor vulkan-frame-slot-commands)))

(defun make-vulkan-canvas-frame-slot (native-device)
  (let ((fence nil)
        (image-ready nil)
        (completed-p nil))
    (unwind-protect
         (let ((slot nil))
           (setf fence (lvk:create-fence native-device :signaled t)
                 image-ready (lvk:create-semaphore native-device)
                 slot
                 (make-instance
                  'vulkan-canvas-frame-slot
                  :fence fence :image-ready image-ready)
                 completed-p t)
           slot)
      (unless completed-p
        (when image-ready
          (lvk:destroy-semaphore native-device image-ready))
        (when fence
          (lvk:destroy-fence native-device fence))))))

(defun make-vulkan-canvas-frame-slots (native-device count)
  (let ((slots (make-array count :initial-element nil))
        (completed-p nil))
    (unwind-protect
         (progn
           (dotimes (index count)
             (setf (aref slots index)
                   (make-vulkan-canvas-frame-slot native-device)))
           (setf completed-p t)
           slots)
      (unless completed-p
        (loop for slot across slots
              when slot
                do (lvk:destroy-semaphore
                    native-device (vulkan-frame-slot-image-ready slot))
                   (lvk:destroy-fence
                    native-device (vulkan-frame-slot-fence slot)))))))

(defun make-vulkan-semaphores (native-device count)
  (let ((semaphores (make-array count :initial-element nil))
        (completed-p nil))
    (unwind-protect
         (progn
           (dotimes (index count)
             (setf (aref semaphores index)
                   (lvk:create-semaphore native-device)))
           (setf completed-p t)
           semaphores)
      (unless completed-p
        (loop for semaphore across semaphores
              when semaphore
                do (lvk:destroy-semaphore native-device semaphore))))))

(defun destroy-vulkan-canvas-frame-slot (native-device slot)
  (when (vulkan-frame-slot-commands slot)
    (destroy (vulkan-frame-slot-commands slot))
    (setf (vulkan-frame-slot-commands slot) nil))
  (lvk:destroy-semaphore
   native-device (vulkan-frame-slot-image-ready slot))
  (lvk:destroy-fence native-device (vulkan-frame-slot-fence slot))
  (values))

(defun recycle-vulkan-canvas-frame-slot (native-device slot)
  (when (vulkan-frame-slot-commands slot)
    (lvk:wait-for-fence native-device (vulkan-frame-slot-fence slot))
    (destroy (vulkan-frame-slot-commands slot))
    (setf (vulkan-frame-slot-commands slot) nil))
  (lvk:reset-fence native-device (vulkan-frame-slot-fence slot))
  slot)

(defun sdl-canvas-vulkan-instance-extensions (canvas provider)
  (declare (ignore canvas provider))
  (cffi:with-foreign-object (count :uint32)
    (let ((names (sdl3:vulkan-get-instance-extensions count)))
      (when (cffi:null-pointer-p names)
        (error "SDL could not report Vulkan instance extensions: ~A"
               (sdl3:get-error)))
      (loop for index below (cffi:mem-ref count :uint32)
            collect (cffi:foreign-string-to-lisp
                     (cffi:mem-aref names :pointer index))))))

(defun create-sdl-vulkan-canvas-surface (canvas provider instance)
  (declare (ignore provider))
  (cffi:with-foreign-object (surface :pointer)
    (unless (sdl3:vulkan-create-surface
             (sdl-canvas-window canvas)
             instance (cffi:null-pointer) surface)
      (error "SDL could not create a Vulkan surface: ~A" (sdl3:get-error)))
    (cffi:mem-ref surface :pointer)))

(defun destroy-sdl-vulkan-canvas-surface
    (canvas provider instance surface)
  (declare (ignore canvas provider))
  (sdl3:vulkan-destroy-surface instance surface (cffi:null-pointer))
  (values))

(defun canvas-physical-device-and-queue-family (instance surface canvas)
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
      (error 'canvas-error :canvas canvas :operation :make-context
             :reason :no-presentation-queue)))

(defun gpu-canvas-format-to-vulkan (format)
  (or (cdr (assoc format
                  '((:rgba8-unorm . :r8g8b8a8-unorm)
                    (:rgba8-unorm-srgb . :r8g8b8a8-srgb)
                    (:bgra8-unorm . :b8g8r8a8-unorm)
                    (:bgra8-unorm-srgb . :b8g8r8a8-srgb))))
      (error 'canvas-error :operation :configure
             :reason :unsupported-format :details format)))

(defun vulkan-canvas-format-to-gpu (format)
  (or (cdr (assoc format
                  '((:r8g8b8a8-unorm . :rgba8-unorm)
                    (:r8g8b8a8-srgb . :rgba8-unorm-srgb)
                    (:b8g8r8a8-unorm . :bgra8-unorm)
                    (:b8g8r8a8-srgb . :bgra8-unorm-srgb))))
      (error 'canvas-error :operation :configure
             :reason :unsupported-format :details format)))

(defun choose-vulkan-canvas-format (context requested-format)
  (let* ((physical-device (vulkan-canvas-physical-device context))
         (surface (vulkan-canvas-surface context))
         (formats (lvk:get-surface-formats physical-device surface))
         (requested-vulkan-format
           (and requested-format
                (gpu-canvas-format-to-vulkan requested-format))))
    (if requested-vulkan-format
        (or (find-if
             (lambda (format)
               (eq requested-vulkan-format
                   (lvk:presentation-format-format format)))
             formats)
            (error 'canvas-error :canvas (context-canvas context)
                   :operation :configure :reason :unsupported-surface-format
                   :details requested-format))
        (or (find-if
             (lambda (format)
               (and (eq :b8g8r8a8-srgb
                        (lvk:presentation-format-format format))
                    (eq :srgb-nonlinear-khr
                        (lvk:presentation-format-color-space format))))
             formats)
            (first formats)
            (error 'canvas-error :canvas (context-canvas context)
                   :operation :configure :reason :no-surface-formats)))))

(defun clamp-canvas-extent (value minimum maximum)
  (max minimum (min value maximum)))

(defun choose-vulkan-canvas-extent (context capabilities)
  (let ((current (lvk:presentation-capabilities-current-extent capabilities)))
    (if (/= #xffffffff (first current))
        current
        (multiple-value-bind (width height)
            (canvas-size (context-canvas context))
          (let ((minimum
                  (lvk:presentation-capabilities-min-image-extent capabilities))
                (maximum
                  (lvk:presentation-capabilities-max-image-extent capabilities)))
            (list (clamp-canvas-extent
                   width (first minimum) (first maximum))
                  (clamp-canvas-extent
                   height (second minimum) (second maximum))))))))

(defun choose-vulkan-canvas-image-count (capabilities)
  (let ((desired
          (1+ (lvk:presentation-capabilities-min-image-count capabilities)))
        (maximum
          (lvk:presentation-capabilities-max-image-count capabilities)))
    (if (plusp maximum) (min desired maximum) desired)))

(defun ensure-vulkan-canvas-state (context operation expected-state)
  (unless (member (canvas-context-state context)
                  (if (listp expected-state)
                      expected-state
                      (list expected-state)))
    (error 'canvas-state-error
           :canvas (context-canvas context)
           :operation operation :reason :invalid-context-state
           :state (canvas-context-state context)
           :expected-state expected-state)))

(defmethod configure-canvas-context
    ((context vulkan-canvas-context) configuration)
  (unless (typep configuration 'canvas-configuration)
    (error 'canvas-error :canvas (context-canvas context)
           :operation :configure :reason :invalid-configuration
           :details configuration))
  (when (eq :configured (canvas-context-state context))
    (unconfigure-canvas-context context))
  (ensure-vulkan-canvas-state context :configure :unconfigured)
  (let ((configured-device (canvas-configuration-device configuration)))
    (when (and configured-device
               (not (eq configured-device (context-device context))))
      (error 'canvas-error :canvas (context-canvas context)
             :operation :configure :reason :device-mismatch
             :details configured-device)))
  (unless (equal '(:copy-dst) (canvas-configuration-usage configuration))
    (error 'canvas-error :canvas (context-canvas context)
           :operation :configure :reason :unsupported-usage
           :details (canvas-configuration-usage configuration)))
  (with-vulkan-gpu-driver-environment
    (let* ((native-device (vulkan-handle (context-device context)))
           (physical-device (vulkan-canvas-physical-device context))
           (surface (vulkan-canvas-surface context))
           (capabilities
             (lvk:get-surface-capabilities physical-device surface))
           (surface-format
             (choose-vulkan-canvas-format
              context (canvas-configuration-format configuration)))
           (vk-format (lvk:presentation-format-format surface-format))
           (gpu-format (vulkan-canvas-format-to-gpu vk-format))
           (color-space
             (lvk:presentation-format-color-space surface-format))
           (extent (choose-vulkan-canvas-extent context capabilities))
           (composite-alpha
             (or (find :opaque
                       (lvk:presentation-capabilities-composite-alpha
                        capabilities))
                 (first (lvk:presentation-capabilities-composite-alpha
                         capabilities))))
           (swapchain nil)
           (textures #())
           (frame-slots #())
           (render-done #())
           (completed-p nil))
      (unless (member :transfer-dst
                      (lvk:presentation-capabilities-usage capabilities))
        (error 'canvas-error :canvas (context-canvas context)
               :operation :configure
               :reason :unsupported-surface-usage :details :copy-dst))
      (unwind-protect
           (progn
             (setf swapchain
                   (lvk:create-swapchain
                    native-device surface vk-format color-space extent
                    :min-image-count
                    (choose-vulkan-canvas-image-count capabilities)
                    :usage '(:transfer-dst)
                    :pre-transform
                    (lvk:presentation-capabilities-current-transform
                     capabilities)
                    :composite-alpha composite-alpha
                    :present-mode :fifo-khr)
                   textures
                   (map 'vector
                        (lambda (image)
                          (make-borrowed-vulkan-texture
                           (context-device context) image extent
                           gpu-format vk-format))
                        (lvk:get-swapchain-images native-device swapchain))
                   frame-slots
                   (make-vulkan-canvas-frame-slots
                    native-device (min 2 (length textures)))
                   render-done
                   (make-vulkan-semaphores native-device (length textures)))
             (setf (vulkan-canvas-swapchain context) swapchain
                   (vulkan-canvas-textures context) textures
                   (canvas-extent context) extent
                   (canvas-format context) gpu-format
                   (vulkan-canvas-render-done context) render-done
                   (vulkan-canvas-frame-slots context) frame-slots
                   (vulkan-canvas-next-frame-slot context) 0
                   (canvas-context-configuration context)
                   (make-canvas-configuration
                    :device (context-device context)
                    :format gpu-format
                    :usage (canvas-configuration-usage configuration))
                   (canvas-context-state context) :configured
                   completed-p t)
             context)
        (unless completed-p
          (loop for semaphore across render-done
                when semaphore
                  do (lvk:destroy-semaphore native-device semaphore))
          (loop for slot across frame-slots
                when slot
                  do (destroy-vulkan-canvas-frame-slot native-device slot))
          (loop for texture across textures do (destroy texture))
          (when swapchain
            (lvk:destroy-swapchain native-device swapchain)))))))

(defmethod unconfigure-canvas-context ((context vulkan-canvas-context))
  (when (eq :in-frame (canvas-context-state context))
    (ensure-vulkan-canvas-state context :unconfigure :configured))
  (unless (member (canvas-context-state context) '(:unconfigured :destroyed))
    (with-vulkan-gpu-driver-environment
      (let ((native-device (vulkan-handle (context-device context))))
        (lvk:device-wait-idle native-device)
        (loop for slot across (vulkan-canvas-frame-slots context)
              do (destroy-vulkan-canvas-frame-slot native-device slot))
        (loop for texture across (vulkan-canvas-textures context)
              do (destroy texture))
        (loop for semaphore across (vulkan-canvas-render-done context)
              do (lvk:destroy-semaphore native-device semaphore))
        (when (vulkan-canvas-swapchain context)
          (lvk:destroy-swapchain
           native-device (vulkan-canvas-swapchain context)))))
    (setf (vulkan-canvas-swapchain context) nil
          (vulkan-canvas-textures context) #()
          (vulkan-canvas-render-done context) #()
          (vulkan-canvas-frame-slots context) #()
          (vulkan-canvas-next-frame-slot context) 0
          (vulkan-canvas-current-texture context) nil
          (canvas-context-configuration context) nil
          (canvas-extent context) nil
          (canvas-format context) nil
          (canvas-context-state context) :unconfigured))
  (values))

(defmethod destroy-canvas-context ((context vulkan-canvas-context))
  (unless (eq :destroyed (canvas-context-state context))
    (unconfigure-canvas-context context)
    (destroy-sdl-vulkan-canvas-surface
     (context-canvas context)
     (vulkan-canvas-provider context)
     (vulkan-canvas-instance context)
     (vulkan-canvas-surface context))
    (destroy (context-device context))
    (setf (canvas-context-state context) :destroyed)
    (when (eq context (canvas-context (context-canvas context)))
      (setf (canvas-context (context-canvas context)) nil)))
  (values))

(defmethod make-canvas-context
    ((canvas sdl-canvas) (provider vulkan-gpu-provider)
     &optional configuration)
  (when (canvas-context canvas)
    (error 'canvas-error :canvas canvas :operation :make-context
           :reason :context-already-exists))
  (let ((requested
          (or configuration (make-canvas-configuration))))
    (when (canvas-configuration-device requested)
      (error 'canvas-error :canvas canvas :operation :make-context
             :reason :external-device-not-supported
             :details (canvas-configuration-device requested)))
    (call-on-sdl-canvas-thread
     canvas
     (lambda ()
       (with-vulkan-gpu-driver-environment
         (let ((instance nil)
               (surface nil)
               (device nil)
               (context nil)
               (completed-p nil))
           (unwind-protect
                (progn
                  (multiple-value-bind (portable-extensions flags)
                      (vulkan-gpu-instance-options)
                    (setf instance
                          (lvk:create-instance
                           :application-name (canvas-title canvas)
                           :flags flags
                           :enabled-extension-names
                           (remove-duplicates
                            (append
                             (sdl-canvas-vulkan-instance-extensions
                              canvas provider)
                             portable-extensions)
                            :test #'string=))))
                  (setf surface
                        (create-sdl-vulkan-canvas-surface
                         canvas provider instance))
                  (multiple-value-bind (physical-device queue-family)
                      (canvas-physical-device-and-queue-family
                       instance surface canvas)
                    (setf device
                          (make-vulkan-gpu-device
                           instance physical-device queue-family
                           (make-device-descriptor :label (canvas-title canvas))
                           :enabled-extension-names
                           (list lvk:+swapchain-extension-name+))
                          context
                          (make-instance
                           'vulkan-canvas-context
                           :canvas canvas :provider provider
                           :instance instance :surface surface
                           :physical-device physical-device :device device)
                          (canvas-context canvas) context)
                    (configure-canvas-context
                     context
                     (make-canvas-configuration
                      :device device
                      :format (canvas-configuration-format requested)
                      :usage (canvas-configuration-usage requested)))
                    (setf completed-p t)
                    context))
             (unless completed-p
               (setf (canvas-context canvas) nil)
               (cond
                 (context
                  (ignore-errors (destroy-canvas-context context)))
                 (t
                  (when surface
                    (destroy-sdl-vulkan-canvas-surface
                     canvas provider instance surface))
                  (if device
                      (destroy device)
                      (when instance (lvk:destroy-instance instance)))))))))))))

(defmethod get-current-texture ((context vulkan-canvas-context))
  (or (vulkan-canvas-current-texture context)
      (error 'canvas-state-error
             :canvas (context-canvas context)
             :operation :get-current-texture :reason :outside-frame
             :state (canvas-context-state context) :expected-state :in-frame)))

(defun call-with-vulkan-canvas-frame (context function)
  (ensure-vulkan-canvas-state context :frame :configured)
  (let* ((device (context-device context))
         (native-device (vulkan-handle device))
         (queue (device-queue device))
         (slot-index (vulkan-canvas-next-frame-slot context))
         (slot (aref (vulkan-canvas-frame-slots context) slot-index))
         (encoder nil)
         (commands nil))
    (recycle-vulkan-canvas-frame-slot native-device slot)
    (multiple-value-bind (image-index acquire-result)
        (lvk:acquire-next-image
         native-device (vulkan-canvas-swapchain context)
         (vulkan-frame-slot-image-ready slot))
      (declare (ignore acquire-result))
      (let ((texture (aref (vulkan-canvas-textures context) image-index))
            (render-done
              (aref (vulkan-canvas-render-done context) image-index)))
        (unwind-protect
             (progn
               (setf encoder
                     (create device (make-command-encoder-descriptor))
                     (vulkan-canvas-current-texture context) texture
                     (canvas-context-state context) :in-frame)
               (funcall function texture encoder)
               (transition-vulkan-texture encoder texture :present-src-khr)
               (setf commands (finish encoder))
               (submit-vulkan-command-buffers
                queue (vector commands)
                :wait-semaphores
                (vector (vulkan-frame-slot-image-ready slot))
                :wait-stages (vector '(:transfer))
                :signal-semaphores
                (vector render-done)
                :completion-fence (vulkan-frame-slot-fence slot)
                :wait-for-completion nil)
               (setf (vulkan-frame-slot-commands slot) commands
                     commands nil)
               (lvk:present
                (vulkan-handle queue)
                (vulkan-canvas-swapchain context) image-index
                :wait-semaphores
                (vector render-done))
               (setf (vulkan-canvas-next-frame-slot context)
                     (mod (1+ slot-index)
                          (length (vulkan-canvas-frame-slots context))))
               texture)
          (setf (vulkan-canvas-current-texture context) nil
                (canvas-context-state context) :configured)
          (when commands (destroy commands))
          (when encoder (destroy encoder)))))))

(defmethod call-with-canvas-frame
    ((context vulkan-canvas-context) function)
  (call-on-sdl-canvas-thread
   (context-canvas context)
   (lambda ()
     (call-with-vulkan-canvas-frame context function))))
