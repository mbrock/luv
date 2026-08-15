;;;; Metal 4 presentation as a relationship between an SDL canvas and device.

(in-package #:luv)

(defmethod sdl-presentation-api-for ((provider metal-gpu-provider))
  (declare (ignore provider))
  :metal)

(defclass metal-canvas-context (canvas-context)
  ((canvas :initarg :canvas :reader context-canvas)
   (provider :initarg :provider :reader metal-canvas-provider)
   (view :initarg :view :accessor metal-canvas-view)
   (layer :initarg :layer :reader metal-canvas-layer)
   (device :initform nil :accessor canvas-device)
   (configuration :initform nil :accessor canvas-context-configuration)
   (extent :initform nil :accessor canvas-extent)
   (format :initform nil :accessor canvas-format)
   (current-texture :initform nil :accessor metal-canvas-current-texture)
   (state :initform :unconfigured :accessor canvas-context-state)))

(defmethod context-device ((context metal-canvas-context))
  (canvas-device context))

(defun ensure-metal-canvas-state (context operation expected-state)
  (unless (member (canvas-context-state context)
                  (if (listp expected-state)
                      expected-state
                      (list expected-state)))
    (error 'canvas-state-error
           :canvas (context-canvas context)
           :operation operation :reason :invalid-context-state
           :state (canvas-context-state context)
           :expected-state expected-state)))

(defun metal-pixel-format (format)
  (case format
    ((nil :bgra8-unorm-srgb) luv.metal:+pixel-format-bgra8-unorm-srgb+)
    (:bgra8-unorm luv.metal:+pixel-format-bgra8-unorm+)
    (otherwise
     (error 'canvas-error :operation :configure
            :reason :unsupported-format :details format))))

(defun gpu-metal-pixel-format (format)
  (case format
    (#.luv.metal:+pixel-format-bgra8-unorm+ :bgra8-unorm)
    (#.luv.metal:+pixel-format-bgra8-unorm-srgb+ :bgra8-unorm-srgb)
    (otherwise
     (error 'canvas-error :operation :configure
            :reason :unsupported-native-format :details format))))

(defun synchronize-metal-canvas-drawable-size (context)
  "Make the layer follow SDL's physical-pixel extent, including Retina scale."
  (multiple-value-bind (width height) (canvas-size (context-canvas context))
    (luv.metal:set-layer-drawable-size
     (metal-canvas-layer context) width height)
    (multiple-value-bind (layer-width layer-height)
        (luv.metal:layer-drawable-size (metal-canvas-layer context))
      (unless (and (= width (round layer-width))
                   (= height (round layer-height)))
        (error 'canvas-error :canvas (context-canvas context)
               :operation :drawable-size :reason :native-size-mismatch
               :details (list :sdl (list width height)
                              :metal (list layer-width layer-height))))
      (setf (canvas-extent context) (list width height)))))

(defun configure-metal-canvas-context (context configuration)
  (unless (typep configuration 'canvas-configuration)
    (error 'canvas-error :canvas (context-canvas context)
           :operation :configure :reason :invalid-configuration
           :details configuration))
  (let ((device (canvas-configuration-device configuration)))
    (unless (typep device 'metal-gpu-device)
      (error 'canvas-error :canvas (context-canvas context)
             :operation :configure :reason :metal-device-required
             :details device))
    (ensure-live-metal-object device :configure-canvas)
    (when (eq :configured (canvas-context-state context))
      (unconfigure-canvas-context context))
    (ensure-metal-canvas-state context :configure :unconfigured)
    (unless (equal '(:copy-dst) (canvas-configuration-usage configuration))
      (error 'canvas-error :canvas (context-canvas context)
             :operation :configure :reason :unsupported-usage
             :details (canvas-configuration-usage configuration)))
    (let* ((layer (metal-canvas-layer context))
           (native-format
             (metal-pixel-format (canvas-configuration-format configuration))))
      (luv.metal:set-layer-device layer (metal-native-object device))
      (luv.metal:set-layer-pixel-format layer native-format)
      ;; Luvcraft renders to an owned color texture and copies the complete
      ;; frame into the drawable, so drawable textures are not framebuffer-only.
      (luv.metal:set-layer-framebuffer-only layer 0)
      (synchronize-metal-canvas-drawable-size context)
      (let ((format
              (gpu-metal-pixel-format (luv.metal:layer-pixel-format layer))))
        (setf (canvas-device context) device
              (canvas-format context) format
              (canvas-context-configuration context)
              (make-canvas-configuration
               :device device :format format
               :usage (canvas-configuration-usage configuration))
              (canvas-context-state context) :configured)))
    context))

(defmethod configure-canvas-context
    ((context metal-canvas-context) configuration)
  (call-on-sdl-canvas-thread
   (context-canvas context)
   (lambda () (configure-metal-canvas-context context configuration))))

(defun unconfigure-metal-canvas-context (context)
  (when (eq :in-frame (canvas-context-state context))
    (ensure-metal-canvas-state context :unconfigure :configured))
  (unless (member (canvas-context-state context) '(:unconfigured :destroyed))
    (luv.metal:set-layer-device (metal-canvas-layer context) nil)
    (setf (metal-canvas-current-texture context) nil
          (canvas-context-configuration context) nil
          (canvas-device context) nil
          (canvas-extent context) nil
          (canvas-format context) nil
          (canvas-context-state context) :unconfigured))
  (values))

(defmethod unconfigure-canvas-context ((context metal-canvas-context))
  (let ((canvas (context-canvas context)))
    (if (eq :open (canvas-state canvas))
        (call-on-sdl-canvas-thread
         canvas (lambda () (unconfigure-metal-canvas-context context)))
        (unconfigure-metal-canvas-context context))))

(defun destroy-metal-canvas-context (context)
  (unless (eq :destroyed (canvas-context-state context))
    (unconfigure-metal-canvas-context context)
    (when (metal-canvas-view context)
      (sdl3:metal-destroy-view (metal-canvas-view context))
      (setf (metal-canvas-view context) nil))
    (setf (canvas-context-state context) :destroyed)
    (when (eq context (canvas-context (context-canvas context)))
      (setf (canvas-context (context-canvas context)) nil)))
  (values))

(defmethod destroy-canvas-context ((context metal-canvas-context))
  (let ((canvas (context-canvas context)))
    (if (and (eq :open (canvas-state canvas))
             (not (sdl-canvas-native-thread-p canvas)))
        (call-on-sdl-canvas-thread
         canvas (lambda () (destroy-metal-canvas-context context)))
        (destroy-metal-canvas-context context))))

(defmethod make-canvas-context
    ((canvas sdl-canvas) (provider metal-gpu-provider)
     &optional configuration)
  (unless (eq :metal (sdl-canvas-presentation-api canvas))
    (error 'canvas-error :canvas canvas :operation :make-context
           :reason :presentation-api-mismatch
           :details (list :canvas (sdl-canvas-presentation-api canvas)
                          :provider :metal)))
  (when (canvas-context canvas)
    (error 'canvas-error :canvas canvas :operation :make-context
           :reason :context-already-exists))
  (call-on-sdl-canvas-thread
   canvas
   (lambda ()
     (let ((view (sdl3:metal-create-view (sdl-canvas-window canvas))))
       (when (cffi:null-pointer-p view)
         (error 'canvas-error :canvas canvas :operation :make-context
                :reason :metal-view-failed :details (sdl3:get-error)))
       (let ((context nil))
         (handler-case
             (let ((layer (sdl3:metal-get-layer view)))
               (when (cffi:null-pointer-p layer)
                 (error 'canvas-error :canvas canvas :operation :make-context
                        :reason :metal-layer-failed :details (sdl3:get-error)))
               (setf context
                     (make-instance
                      'metal-canvas-context
                      :canvas canvas :provider provider :view view
                      :layer
                      (luv.objective-c:wrap-objective-c-object
                       layer :ownership :borrowed :protocol-name "CAMetalLayer"))
                     (canvas-context canvas) context)
               (when configuration
                 (configure-metal-canvas-context context configuration))
               context)
           (error (condition)
             (if context
                 (ignore-errors (destroy-metal-canvas-context context))
                 (sdl3:metal-destroy-view view))
             (error condition))))))))

(defmethod get-current-texture ((context metal-canvas-context))
  (or (metal-canvas-current-texture context)
      (error 'canvas-state-error
             :canvas (context-canvas context)
             :operation :get-current-texture :reason :outside-frame
             :state (canvas-context-state context) :expected-state :in-frame)))

(defmethod canvas-frame-resource-key
    ((context metal-canvas-context) (surface-texture metal-gpu-texture))
  (declare (ignore context))
  ;; CAMetalLayer cycles a bounded native drawable pool, but each NEXTDRAWABLE
  ;; call is represented by a fresh borrowed Lisp texture wrapper.  Metal 4's
  ;; resource identity survives those wrappers and becomes reusable only when
  ;; the layer makes that drawable available again.
  (getf
   (luv.metal:metal-texture-resource-id
    (metal-native-object surface-texture))
   'luv.metal::value))

(defun call-with-metal-canvas-frame (context function)
  (with-cpu-trace-zone (:canvas/frame)
    (ensure-metal-canvas-state context :frame :configured)
    (luv.objective-c:with-autorelease-pool ()
      ;; All selectors and resource relationships below have already crossed
      ;; the Lisp validation boundary.  Keep the inspectable exception bridge
      ;; for setup and diagnosis, but do established per-frame traffic as
      ;; direct objc_msgSend calls on the native thread that actually encodes.
      (luv.objective-c:with-unchecked-objective-c-messages ()
        (with-cpu-trace-zone (:metal/synchronize-drawable)
          (synchronize-metal-canvas-drawable-size context))
        (let* ((device (context-device context))
               (queue (device-queue device))
               (native-queue (metal-native-object queue))
               (drawable
                 (with-cpu-trace-zone (:canvas/acquire-drawable)
                   (luv.metal:next-drawable
                    (metal-canvas-layer context)))))
          (unless drawable
            (error 'canvas-error :canvas (context-canvas context)
                   :operation :frame :reason :no-metal-drawable))
          (let ((encoder nil)
                (command-buffer nil)
                (texture nil))
            (with-cpu-trace-zone (:metal/allocate-frame-resources)
              (setf encoder
                    (create
                     device
                     (make-command-encoder-descriptor
                      :label "Metal canvas frame"))))
            (unwind-protect
                 (let ((native-texture
                         (luv.metal:drawable-texture drawable)))
                   (setf texture
                         (make-instance
                          'metal-gpu-texture
                          :device device :native-object native-texture
                          :owned-p nil
                          :size (canvas-extent context)
                          :dimensions :2d :format (canvas-format context)
                          :usage (canvas-configuration-usage
                                  (canvas-context-configuration context))))
                   (setf (metal-canvas-current-texture context) texture
                         (canvas-context-state context) :in-frame)
                   (with-cpu-trace-zone (:gpu/encode)
                     (funcall function texture encoder))
                   (when (metal-encoder-active-pass encoder)
                     (error 'canvas-error :canvas (context-canvas context)
                            :operation :frame
                            :reason :metal-pass-left-open))
                   (with-cpu-trace-zone (:gpu/finish-encoding)
                     (setf command-buffer (finish encoder)))
                   (with-cpu-trace-zone (:metal/wait-for-drawable)
                     (luv.metal:wait-for-drawable native-queue drawable))
                   (with-cpu-trace-zone (:gpu/submit)
                     (submit-metal-command-buffers
                      queue (vector command-buffer)
                      :after-commit
                      (lambda ()
                        (luv.metal:signal-drawable native-queue drawable)
                        (luv.metal:present-drawable drawable))))
                   texture)
              (setf (metal-canvas-current-texture context) nil
                    (canvas-context-state context) :configured)
              (when command-buffer
                (destroy command-buffer))
              (when encoder
                (destroy encoder))
              (when texture
                (destroy texture)))))))))

(defmethod call-with-canvas-frame
    ((context metal-canvas-context) function)
  (call-on-sdl-canvas-thread
   (context-canvas context)
   (lambda () (call-with-metal-canvas-frame context function))))

(defun metal-submission-selectors (trace)
  (mapcar
   (lambda (event)
     (getf
      (luv.objective-c:objective-c-message-event-description event)
      :selector))
   (luv.objective-c:objective-c-trace-events trace)))

(defparameter +metal-clear-submission-sequence+
  '("beginCommandBufferWithAllocator:"
    "setClearColor:"
    "renderCommandEncoderWithDescriptor:"
    "endEncoding"
    "endCommandBuffer"
    "waitForDrawable:"
    "commit:count:"
    "signalDrawable:"
    "present"
    "signalEvent:value:"))

(defun validate-metal-clear-submission (selectors)
  "Require the Metal 4 drawable operations in their semantic order."
  (let ((tail selectors))
    (dolist (required +metal-clear-submission-sequence+)
      (setf tail (member required tail :test #'string=))
      (unless tail
        (error 'canvas-error :operation :frame
               :reason :incomplete-metal-4-submission
               :details (list :missing required :selectors selectors)))
      (setf tail (rest tail))))
  selectors)

(defun collect-metal-clear-evidence (canvas device context color)
  (multiple-value-bind (logical-width logical-height)
      (canvas-logical-size canvas)
    (multiple-value-bind (pixel-width pixel-height)
        (canvas-size canvas)
      (multiple-value-bind (layer-width layer-height)
          (luv.metal:layer-drawable-size (metal-canvas-layer context))
        (let (trace)
          ;; Objective-C sends happen on the Cocoa thread. Establish the
          ;; dynamic tracing runtime there rather than on this caller thread.
          (request-canvas-frame
           canvas
           (lambda (timestamp)
             (declare (ignore timestamp))
             (luv.objective-c:with-objective-c-trace (active-trace)
               (setf trace active-trace)
               (apply #'render-canvas-color context (coerce color 'list)))))
          (let ((selectors (metal-submission-selectors trace)))
            (validate-metal-clear-submission selectors)
            (list
             :device
             (luv.objective-c:objective-c-string
              (luv.metal:device-name (metal-native-object device)))
             :logical-size (list logical-width logical-height)
             :pixel-size (list pixel-width pixel-height)
             :drawable-size (list (round layer-width) (round layer-height))
             :pixel-format (canvas-format context)
             :clear-color (coerce color 'list)
             :submission-selectors selectors
             :presented t)))))))

(defun probe-sdl-metal-clear
    (&key (width 320) (height 200) (visible-p nil)
      (color #(0.80d0 0.10d0 0.70d0 1.0d0)))
  "Present one Metal 4 clear and return bounded SDL/layer/submission evidence."
  (let ((canvas
          (make-sdl-canvas
           :title "luv Metal 4 clear" :width width :height height
           :visible-p visible-p :presentation-api :metal))
        (provider (make-instance 'metal-gpu-provider))
        (device nil)
        (context nil)
        (evidence nil))
    (unwind-protect
         (progn
           (open-canvas canvas)
           (setf device (request-gpu-device provider)
                 context
                 (make-canvas-context
                  canvas provider (make-canvas-configuration :device device))
                 evidence
                 (collect-metal-clear-evidence
                  canvas device context color)))
      (when (member (canvas-state canvas) '(:opening :open))
        (close-canvas canvas))
      (when device
        (destroy device)))
    (setf (getf evidence :canvas-state) (canvas-state canvas)
          (getf evidence :context-state) (canvas-context-state context)
          (getf evidence :view-destroyed)
          (null (metal-canvas-view context))
          (getf evidence :window-destroyed)
          (null (sdl-canvas-window canvas))
          (getf evidence :device-destroyed)
          (metal-object-destroyed-p device))
    evidence))
