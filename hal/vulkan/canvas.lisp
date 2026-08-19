;;; Vulkan presentation contexts and their SDL bridge.

(in-package #:luv)

(defmethod sdl-presentation-api-for ((provider vulkan-gpu-provider))
  (declare (ignore provider))
  :vulkan)

(defclass vulkan-canvas-context (canvas-context)
  ((canvas
    :initarg :canvas
    :reader context-canvas)
   (provider
    :initarg :provider
    :reader vulkan-canvas-provider)
   (instance
    :initarg :instance
    :initform nil
    :accessor vulkan-canvas-instance)
   (physical-device
    :initarg :physical-device
    :initform nil
    :accessor vulkan-canvas-physical-device)
   (device
    :initarg :device
    :initform nil
    :accessor canvas-device)
   (surface
    :initarg :surface
    :initform nil
    :accessor vulkan-canvas-surface)
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
   (window-size
    :initform nil
    :accessor vulkan-canvas-window-size
    :documentation
    "The window's pixel size when this swapchain was built.

Compared against the window rather than against EXTENT, which comes from
the surface's own idea of its current extent: a platform where the two
disagree would otherwise ask for a rebuild on every single frame.")
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
  ((image-ready
    :initarg :image-ready
    :reader vulkan-frame-slot-image-ready)
   (submission-index
    :initform nil
    :accessor vulkan-frame-slot-submission-index
    :documentation "Queue submission index of this slot's frame in flight.")
   (commands
    :initform nil
    :accessor vulkan-frame-slot-commands)))

(defun make-vulkan-canvas-frame-slot (native-device)
  (make-instance
   'vulkan-canvas-frame-slot
   :image-ready (lvk:create-semaphore native-device)))

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
                    native-device (vulkan-frame-slot-image-ready slot)))))))

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
  (values))

(defun recycle-vulkan-canvas-frame-slot (queue slot)
  "Wait for SLOT's frame to pass the queue frontier, then release it."
  (when (vulkan-frame-slot-commands slot)
    (wait-for-vulkan-submission
     queue (vulkan-frame-slot-submission-index slot))
    (destroy (vulkan-frame-slot-commands slot))
    (setf (vulkan-frame-slot-commands slot) nil
          (vulkan-frame-slot-submission-index slot) nil))
  slot)

(defun sdl-vulkan-instance-extensions ()
  (cffi:with-foreign-object (count :uint32)
    (let ((names (sdl3:vulkan-get-instance-extensions count)))
      (when (cffi:null-pointer-p names)
        (error "SDL could not report Vulkan instance extensions: ~A"
               (sdl3:get-error)))
      (loop for index below (cffi:mem-ref count :uint32)
            collect (cffi:foreign-string-to-lisp
                     (cffi:mem-aref names :pointer index))))))

(defun sdl-vulkan-presentation-ready-p ()
  (member :video (sdl3:was-init :video)))

(defmethod vulkan-provider-instance-options :around
    ((provider vulkan-gpu-provider))
  (multiple-value-bind (extensions flags) (call-next-method)
    (values (if (sdl-vulkan-presentation-ready-p)
                (remove-duplicates
                 (append (sdl-vulkan-instance-extensions) extensions)
                 :test #'string=)
                extensions)
            flags)))

(defmethod vulkan-gpu-device-extension-names :around
    ((provider vulkan-gpu-provider))
  (declare (ignore provider))
  (let ((extensions (call-next-method)))
    (if (sdl-vulkan-presentation-ready-p)
        (remove-duplicates
         (cons lvk:+swapchain-extension-name+ extensions)
         :test #'string=)
        extensions)))

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

(defun bind-vulkan-canvas-device (context device)
  "Attach unconfigured CONTEXT to DEVICE and create its native SDL surface."
  (unless (typep device 'vulkan-gpu-device)
    (error 'canvas-error :canvas (context-canvas context)
           :operation :configure :reason :unsupported-device
           :details device))
  (ensure-live-vulkan-object device :configure-canvas)
  (let ((bound-device (context-device context)))
    (when (and bound-device (not (eq bound-device device)))
      (error 'canvas-error :canvas (context-canvas context)
             :operation :configure :reason :device-mismatch
             :details device)))
  (unless (context-device context)
    (let* ((required-instance-extensions (sdl-vulkan-instance-extensions))
           (missing-instance-extensions
             (set-difference
              required-instance-extensions
              (vulkan-device-instance-extension-names device)
              :test #'string=)))
      (when missing-instance-extensions
        (error 'canvas-error :canvas (context-canvas context)
               :operation :configure :reason :missing-instance-extensions
               :details missing-instance-extensions)))
    (unless (member lvk:+swapchain-extension-name+
                    (vulkan-device-extension-names device)
                    :test #'string=)
      (error 'canvas-error :canvas (context-canvas context)
             :operation :configure :reason :missing-swapchain-extension
             :details lvk:+swapchain-extension-name+))
    (let* ((instance (vulkan-device-instance device))
           (physical-device (vulkan-device-physical-device device))
           (queue-family (vulkan-device-queue-family device))
           (surface nil)
           (completed-p nil))
      (unwind-protect
           (progn
             (setf surface
                   (create-sdl-vulkan-canvas-surface
                    (context-canvas context)
                    (vulkan-canvas-provider context)
                    instance))
             (unless (lvk:surface-supported-p
                      physical-device queue-family surface)
               (error 'canvas-error :canvas (context-canvas context)
                      :operation :configure
                      :reason :device-cannot-present-to-surface
                      :details device))
             (setf (vulkan-canvas-instance context) instance
                   (vulkan-canvas-physical-device context) physical-device
                   (canvas-device context) device
                   (vulkan-canvas-surface context) surface
                   completed-p t))
        (unless completed-p
          (when surface
            (destroy-sdl-vulkan-canvas-surface
             (context-canvas context)
             (vulkan-canvas-provider context)
             instance surface))))))
  context)

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

(defun configure-vulkan-canvas-context (context configuration)
  (unless (typep configuration 'canvas-configuration)
    (error 'canvas-error :canvas (context-canvas context)
           :operation :configure :reason :invalid-configuration
           :details configuration))
  (let ((configured-device (canvas-configuration-device configuration)))
    (unless configured-device
      (error 'canvas-error :canvas (context-canvas context)
             :operation :configure :reason :device-required))
    (when (eq :configured (canvas-context-state context))
      (unconfigure-canvas-context context))
    (ensure-vulkan-canvas-state context :configure :unconfigured)
    (bind-vulkan-canvas-device context configured-device))
  (unless (and (canvas-configuration-usage configuration)
               (every (lambda (usage)
                        (member usage
                                '(:copy-src :copy-dst :render-attachment)))
                      (canvas-configuration-usage configuration)))
    (error 'canvas-error :canvas (context-canvas context)
           :operation :configure :reason :unsupported-usage
           :details (canvas-configuration-usage configuration)))
  (with-vulkan-gpu-driver-environment
    (let* ((usage (canvas-configuration-usage configuration))
           (native-usage
             (mapcar (lambda (value)
                       (ecase value
                         (:copy-src :transfer-src)
                         (:copy-dst :transfer-dst)
                         (:render-attachment :color-attachment)))
                     usage))
           (native-device (vulkan-handle (context-device context)))
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
      (unless (every (lambda (value)
                       (member value
                               (lvk:presentation-capabilities-usage
                                capabilities)))
                     native-usage)
        (error 'canvas-error :canvas (context-canvas context)
               :operation :configure
               :reason :unsupported-surface-usage :details usage))
      (unwind-protect
           (progn
             (setf swapchain
                   (lvk:create-swapchain
                    native-device surface vk-format color-space extent
                    :min-image-count
                    (choose-vulkan-canvas-image-count capabilities)
                    :usage native-usage
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
                           gpu-format vk-format :usage usage))
                        (lvk:get-swapchain-images native-device swapchain))
                   frame-slots
                   (make-vulkan-canvas-frame-slots
                    native-device (min 2 (length textures)))
                   render-done
                   (make-vulkan-semaphores native-device (length textures)))
             (setf (vulkan-canvas-swapchain context) swapchain
                   (vulkan-canvas-textures context) textures
                   (canvas-extent context) extent
                   (vulkan-canvas-window-size context)
                   (multiple-value-list
                    (canvas-size (context-canvas context)))
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

(defmethod configure-canvas-context
    ((context vulkan-canvas-context) configuration)
  (call-on-sdl-canvas-thread
   (context-canvas context)
   (lambda ()
     (configure-vulkan-canvas-context context configuration))))

(defun unconfigure-vulkan-canvas-context (context)
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
          (vulkan-canvas-window-size context) nil
          (vulkan-canvas-render-done context) #()
          (vulkan-canvas-frame-slots context) #()
          (vulkan-canvas-next-frame-slot context) 0
          (vulkan-canvas-current-texture context) nil
          (canvas-context-configuration context) nil
          (canvas-extent context) nil
          (canvas-format context) nil
          (canvas-context-state context) :unconfigured))
  (values))

(defmethod unconfigure-canvas-context ((context vulkan-canvas-context))
  (unconfigure-vulkan-canvas-context context))

(defmethod destroy-canvas-context ((context vulkan-canvas-context))
  (unless (eq :destroyed (canvas-context-state context))
    (unconfigure-canvas-context context)
    (when (vulkan-canvas-surface context)
      (destroy-sdl-vulkan-canvas-surface
       (context-canvas context)
       (vulkan-canvas-provider context)
       (vulkan-canvas-instance context)
       (vulkan-canvas-surface context)))
    (setf (vulkan-canvas-surface context) nil
          (canvas-context-state context) :destroyed)
    (when (eq context (canvas-context (context-canvas context)))
      (setf (canvas-context (context-canvas context)) nil)))
  (values))

(defmethod make-canvas-context
    ((canvas sdl-canvas) (provider vulkan-gpu-provider)
     &optional configuration)
  (when (canvas-context canvas)
    (error 'canvas-error :canvas canvas :operation :make-context
           :reason :context-already-exists))
  (call-on-sdl-canvas-thread
   canvas
   (lambda ()
     (let ((context
             (make-instance 'vulkan-canvas-context
                            :canvas canvas :provider provider)))
       (setf (canvas-context canvas) context)
       (handler-case
           (progn
             (when configuration
               (configure-canvas-context context configuration))
             context)
         (error (condition)
           (ignore-errors (destroy-canvas-context context))
           (error condition)))))))

(defmethod get-current-texture ((context vulkan-canvas-context))
  (or (vulkan-canvas-current-texture context)
      (error 'canvas-state-error
             :canvas (context-canvas context)
             :operation :get-current-texture :reason :outside-frame
             :state (canvas-context-state context) :expected-state :in-frame)))

;;; A swapchain is only ever right for one window size.  Resizing the window
;;; leaves the images, their views, and the surface's own extent describing a
;;; window that no longer exists, and Vulkan says so by refusing to hand out
;;; another image.  Rebuilding is the whole answer, and it is cheap enough to
;;; do from inside the frame that discovered the problem.

(defun rebuild-vulkan-canvas-swapchain (context)
  "Replace CONTEXT's swapchain with one built for the window's present size.

The configuration is the one the application already asked for: only the
extent, the images, and the per-image synchronization are made again.
Returns NIL when the window has no pixels to present to at all, which is an
ordinary state for a minimized window and not an error."
  (let ((configuration (canvas-context-configuration context)))
    (unless configuration
      (error 'canvas-state-error
             :canvas (context-canvas context)
             :operation :rebuild-swapchain :reason :invalid-context-state
             :state (canvas-context-state context)
             :expected-state :configured))
    (multiple-value-bind (width height) (canvas-size (context-canvas context))
      (when (or (zerop width) (zerop height))
        (return-from rebuild-vulkan-canvas-swapchain nil)))
    (unconfigure-vulkan-canvas-context context)
    (configure-vulkan-canvas-context context configuration)
    (log-event :vulkan "rebuilt the swapchain for ~{~D~^x~}"
               (canvas-extent context))
    t))

(defun ensure-vulkan-canvas-swapchain (context)
  "Make the swapchain agree with the window before a frame asks for an image.

Returns NIL when this frame cannot be presented, which the caller skips."
  (let ((size (multiple-value-list (canvas-size (context-canvas context)))))
    (cond ((or (zerop (first size)) (zerop (second size))) nil)
          ((equal size (vulkan-canvas-window-size context)) t)
          (t (rebuild-vulkan-canvas-swapchain context)))))

(defun acquire-vulkan-canvas-image (context)
  "Acquire the next presentable image, rebuilding a stale swapchain once.

Returns the frame slot, its index, and the image index, or NIL when the
surface still cannot supply an image and this frame should be skipped."
  (let ((device (context-device context))
        (queue (device-queue (context-device context))))
    (dotimes (attempt 2)
      (let* ((slot-index (vulkan-canvas-next-frame-slot context))
             (slot (aref (vulkan-canvas-frame-slots context) slot-index)))
        (with-cpu-trace-zone (:vulkan/recycle-frame-slot)
          (recycle-vulkan-canvas-frame-slot queue slot))
        (multiple-value-bind (image-index result)
            (with-cpu-trace-zone (:canvas/acquire-drawable)
              (lvk:acquire-next-image
               (vulkan-handle device) (vulkan-canvas-swapchain context)
               (vulkan-frame-slot-image-ready slot)))
          (if (eq result :error-out-of-date-khr)
              ;; The semaphore is left unsignalled by a refused acquisition,
              ;; so the rebuilt swapchain's own slots start clean.
              (unless (rebuild-vulkan-canvas-swapchain context)
                (return-from acquire-vulkan-canvas-image nil))
              (return-from acquire-vulkan-canvas-image
                (values slot slot-index image-index))))))
    nil))

(defun %call-with-vulkan-canvas-frame (context function)
  (with-cpu-trace-zone (:canvas/frame)
    (ensure-vulkan-canvas-state context :frame :configured)
    (unless (ensure-vulkan-canvas-swapchain context)
      (return-from %call-with-vulkan-canvas-frame nil))
    (let* ((device (context-device context))
           (queue (device-queue device))
           (encoder nil)
           (commands nil))
      (multiple-value-bind (slot slot-index image-index)
          (acquire-vulkan-canvas-image context)
        (unless slot
          (return-from %call-with-vulkan-canvas-frame nil))
        (let ((texture (aref (vulkan-canvas-textures context) image-index))
              (render-done
                (aref (vulkan-canvas-render-done context) image-index)))
          (unwind-protect
               (progn
                 (setf encoder
                       (create device (make-command-encoder-descriptor))
                       (vulkan-canvas-current-texture context) texture
                       (canvas-context-state context) :in-frame)
                 (with-cpu-trace-zone (:gpu/encode)
                   (funcall function texture encoder))
                 (with-cpu-trace-zone (:gpu/finish-encoding)
                   (transition-vulkan-texture
                    encoder texture :present-src-khr)
                   (setf commands (finish encoder)))
                 (let ((submission-index
                         (with-cpu-trace-zone (:gpu/submit)
                           (submit-vulkan-command-buffers
                            queue (vector commands)
                            :wait-semaphores
                            (vector
                             (list
                              (vulkan-frame-slot-image-ready slot)
                              (mapcar
                               (lambda (usage)
                                 (ecase usage
                                   (:copy-dst :transfer)
                                   (:render-attachment
                                    :color-attachment-output)))
                               (canvas-configuration-usage
                                (canvas-context-configuration context)))))
                            :signal-semaphores
                            (vector (list render-done '(:all-commands)))
                            :wait-for-completion nil))))
                   (setf (vulkan-frame-slot-commands slot) commands
                         (vulkan-frame-slot-submission-index slot)
                         submission-index
                         commands nil))
                 (when (eq :error-out-of-date-khr
                           (with-cpu-trace-zone (:canvas/present)
                             (lvk:present
                              (vulkan-handle queue)
                              (vulkan-canvas-swapchain context) image-index
                              :wait-semaphores
                              (vector render-done))))
                   ;; The image was presented to a surface that has already
                   ;; moved on.  Forgetting the size this swapchain was built
                   ;; for is what makes the next frame rebuild it, even when
                   ;; the window itself reports the same pixels as before.
                   (setf (vulkan-canvas-window-size context) nil))
                 (setf (vulkan-canvas-next-frame-slot context)
                       (mod (1+ slot-index)
                            (length (vulkan-canvas-frame-slots context))))
                 texture)
            (setf (vulkan-canvas-current-texture context) nil
                  (canvas-context-state context) :configured)
            (when commands (destroy commands))
            (when encoder (destroy encoder))))))))

(defun call-with-vulkan-canvas-frame (context function)
  "Run one frame, then account for whatever the validation layer said.

The end of the frame is the safe point: every Vulkan call it made has
returned, so a complaint can become a condition without unwinding through a
driver mid-call.  GUARDING-SDL-CANVAS catches it, retains it with the
backtrace the callback took while the offending frames were still on the
stack, and parks the canvas -- the same treatment as any other frame that
failed, and visible from ./sly as a reported failure rather than a line
somebody has to go looking for."
  (with-vulkan-validation (:frame)
    (%call-with-vulkan-canvas-frame context function)))

(defmethod call-with-canvas-frame
    ((context vulkan-canvas-context) function)
  (call-on-sdl-canvas-thread
   (context-canvas context)
   (lambda ()
     (call-with-vulkan-canvas-frame context function))))
