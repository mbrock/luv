;;;; Metal 4 presentation as a relationship between an SDL canvas and device.

(in-package #:luv)

(defmethod sdl-presentation-api-for ((provider metal-gpu-provider))
  (declare (ignore provider))
  :metal)

(defconstant +metal-canvas-frames-in-flight+ 3
  "Frames a Metal canvas may have recorded but not finished on the GPU.")

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
   (residency-queue :initform nil :accessor metal-canvas-residency-queue
                    :documentation
                    "The queue holding the layer's drawable residency set.")
   (frame-serial :initform 0 :accessor metal-canvas-frame-serial)
   (slot-submissions
    :initform (make-array +metal-canvas-frames-in-flight+ :initial-element 0)
    :reader metal-canvas-slot-submissions
    :documentation
    "Each frame slot's last queue submission: reusing the slot waits on it.")
   (state :initform :unconfigured :accessor canvas-context-state)))

;;; Frame slots and late drawables.
;;;
;;; A CAMetalLayer lends three drawables, and a drawable stays lent from the
;;; moment it is acquired until the compositor has shown the one after it.
;;; Acquired at the top of a frame, it is held through the frame's simulation
;;; and encoding and the whole of its GPU work as well, so three of them did
;;; not cover 60 Hz once a frame took 14 ms: luvcraft fullscreen ran at 47.
;;;
;;; So a frame's resources no longer belong to whichever drawable it got.
;;; Each frame takes the next of three slots, and the slot's previous frame
;;; must have finished on the GPU before it is reused; that wait is the proof
;;; a frame resource cache relies on.  The frame's surface is a stand-in
;;; which knows its size and format, and fetches a drawable only when
;;; something first needs the real texture.  An application rendering into
;;; the surface fetches it at once, as before.  One which renders elsewhere
;;; and copies into the surface last -- luvcraft -- fetches it only for that
;;; copy, and everything recorded until then is handed to the GPU first, so
;;; the frame's work is under way while the drawable is awaited and the
;;; drawable is held for the copy alone.  Moppe's NHAL makes the same split.

(defmethod context-device ((context metal-canvas-context))
  (canvas-device context))

(defclass metal-canvas-surface-texture (metal-gpu-texture)
  ((context :initarg :context :reader metal-surface-context)
   (slot :initarg :slot :reader metal-surface-slot)
   (drawable :initform nil :accessor metal-surface-drawable)
   ;; :PENDING until something needs the native texture, then :ACQUIRED, or
   ;; :UNPRESENTABLE when the drawable is not the size the frame was built
   ;; for.  :CLOSED once the frame is over: nothing fetches a drawable then.
   (status :initform :pending :accessor metal-surface-status))
  (:documentation
   "A frame's presentation surface, which fetches its drawable on first use."))

(defun realize-metal-canvas-surface (surface &optional encoder)
  "Fetch SURFACE's drawable unless it has one, and say whether it is usable.

With ENCODER, whatever it has recorded is submitted first, so the GPU works
on the frame while this thread waits for a drawable."
  (when (eq :pending (metal-surface-status surface))
    (when encoder
      (with-cpu-trace-zone (:metal/flush-before-drawable)
        (flush-metal-command-encoder encoder)))
    (let* ((context (metal-surface-context surface))
           (drawable (with-cpu-trace-zone (:canvas/acquire-drawable)
                       (luv.metal:next-drawable (metal-canvas-layer context)))))
      (unless drawable
        (error 'canvas-error :canvas (context-canvas context)
               :operation :frame :reason :no-metal-drawable))
      (let ((native (luv.metal:drawable-texture drawable)))
        (setf (metal-surface-drawable surface) drawable
              (slot-value surface 'native-object) native
              (metal-surface-status surface)
              ;; A fullscreen transition can leave an old-size drawable in
              ;; the layer's pool after SDL reports the new size.  Such a
              ;; frame is simply not shown.
              (if (equal (list (metal-drawable-texture-width native)
                               (metal-drawable-texture-height native))
                         (subseq (gpu-texture-size surface) 0 2))
                  :acquired
                  :unpresentable)))))
  (eq :acquired (metal-surface-status surface)))

(defmethod metal-native-object ((surface metal-canvas-surface-texture))
  (when (eq :pending (metal-surface-status surface))
    (realize-metal-canvas-surface surface))
  (call-next-method))

(defmethod prepare-metal-copy-destination
    ((surface metal-canvas-surface-texture) encoder)
  (realize-metal-canvas-surface surface encoder))

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
    (unless (and (canvas-configuration-usage configuration)
                 (every (lambda (usage)
                          (member usage
                                  '(:copy-src :copy-dst :render-attachment)))
                        (canvas-configuration-usage configuration)))
      (error 'canvas-error :canvas (context-canvas context)
             :operation :configure :reason :unsupported-usage
             :details (canvas-configuration-usage configuration)))
    (let* ((layer (metal-canvas-layer context))
           (native-format
             (metal-pixel-format (canvas-configuration-format configuration))))
      (luv.metal:set-layer-device layer (metal-native-object device))
      ;; A Metal 4 queue makes nothing resident on its own, drawables
      ;; included; the layer keeps them in a set of its own for the queue to
      ;; hold.  Without it a composited window still happens to show its
      ;; frames, but full screen scans drawables out directly and freezes on
      ;; whichever two or three it last managed to show.
      (let ((queue (metal-native-object (device-queue device))))
        (luv.metal:add-metal-queue-residency-set
         queue (luv.metal:layer-residency-set layer))
        (setf (metal-canvas-residency-queue context) queue))
      (luv.metal:set-layer-pixel-format layer native-format)
      ;; A render-only drawable can remain framebuffer-only. Copies need the
      ;; broader CAMetalLayer texture contract used by captures and luvcraft.
      (luv.metal:set-layer-framebuffer-only
       layer
       (if (equal '(:render-attachment)
                  (canvas-configuration-usage configuration))
           1
           0))
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
    (let ((queue (shiftf (metal-canvas-residency-queue context) nil)))
      (when queue
        (luv.metal:remove-metal-queue-residency-set
         queue (luv.metal:layer-residency-set (metal-canvas-layer context)))))
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
    ((context metal-canvas-context)
     (surface-texture metal-canvas-surface-texture))
  (declare (ignore context))
  ;; The frame slot, not the drawable: the slot's fence is what proves its
  ;; previous frame's resources are free, and asking which drawable this is
  ;; would fetch one early.
  (metal-surface-slot surface-texture))

(luv.objective-c:define-objective-c-message metal-drawable-texture-width
    ("width" :uint64))
(luv.objective-c:define-objective-c-message metal-drawable-texture-height
    ("height" :uint64))

(defun begin-metal-canvas-frame-slot (context)
  "Advance CONTEXT to its next frame slot once that slot's last frame is done."
  (let* ((serial (incf (metal-canvas-frame-serial context)))
         (slot (mod serial +metal-canvas-frames-in-flight+))
         (value (aref (metal-canvas-slot-submissions context) slot))
         (queue (device-queue (context-device context))))
    (when (plusp value)
      (with-cpu-trace-zone (:metal/wait-for-frame-slot)
        (flush-metal-queue-completion-signal queue)
        (unless (plusp
                 (luv.metal:wait-for-metal-shared-event
                  (metal-queue-completion-event queue) value 2000))
          (error 'canvas-error :canvas (context-canvas context)
                 :operation :frame :reason :frame-slot-timeout
                 :details (list :slot slot :submission value)))))
    slot))

(zdefun (call-with-metal-canvas-frame :zone :canvas/frame) (context function)
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
               (slot (begin-metal-canvas-frame-slot context))
               (encoder nil)
               (command-buffer nil)
               (texture nil))
          (with-cpu-trace-zone (:metal/allocate-frame-resources)
            (setf encoder
                  (create
                   device
                   (make-command-encoder-descriptor
                    :label "Metal canvas frame"))))
          (unwind-protect
               (progn
                 (setf texture
                       (make-instance
                        'metal-canvas-surface-texture
                        :context context :slot slot
                        :device device :native-object nil
                        :owned-p nil
                        :size (canonical-texture-extent
                               (canvas-extent context) context :canvas-frame)
                        :dimensions :2d :format (canvas-format context)
                        :usage (canvas-configuration-usage
                                (canvas-context-configuration context))))
                 (setf (metal-canvas-current-texture context) texture
                       (canvas-context-state context) :in-frame)
                 (with-cpu-trace-zone (:gpu/encode)
                   (let ((presentation-time
                           (canvas-presentation-time
                            (context-canvas context))))
                     (call-with-canvas-time
                      (context-canvas context) presentation-time
                      (lambda ()
                        (funcall function texture encoder
                                 presentation-time)))))
                 (when (metal-encoder-active-pass encoder)
                   (error 'canvas-error :canvas (context-canvas context)
                          :operation :frame
                          :reason :metal-pass-left-open))
                 (with-cpu-trace-zone (:gpu/finish-encoding)
                   (setf command-buffer (finish encoder)))
                 (let ((drawable (and (eq :acquired
                                          (metal-surface-status texture))
                                      (metal-surface-drawable texture))))
                   (when drawable
                     (with-cpu-trace-zone (:metal/wait-for-drawable)
                       (luv.metal:wait-for-drawable native-queue drawable)))
                   (with-cpu-trace-zone (:gpu/submit)
                     (setf (aref (metal-canvas-slot-submissions context) slot)
                           (submit-metal-command-buffers
                            queue (vector command-buffer)
                            :after-commit
                            (and drawable
                                 (lambda ()
                                   (luv.metal:signal-drawable
                                    native-queue drawable)
                                   (luv.metal:present-drawable drawable)))))))
                 texture)
            (setf (metal-canvas-current-texture context) nil
                  (canvas-context-state context) :configured)
            (when command-buffer
              (destroy command-buffer))
            (when encoder
              (destroy encoder))
            (when texture
              (setf (metal-surface-status texture) :closed)
              (destroy texture)))))))

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
