;;; Portable canvas and presentation protocols.
;;;
;;; A CANVAS is a native place with a lifetime, size, event source, and frame
;;; clock.  A CANVAS-CONTEXT is a configured relationship between that place
;;; and a GPU implementation.  Platform hosts implement the former protocol;
;;; presentation backends implement the latter.

(in-package #:luv)

(define-condition canvas-error (error)
  ((canvas
    :initarg :canvas
    :initform nil
    :reader canvas-error-canvas)
   (operation
    :initarg :operation
    :initform nil
    :reader canvas-error-operation)
   (reason
    :initarg :reason
    :reader canvas-error-reason)
   (details
    :initarg :details
    :initform nil
    :reader canvas-error-details))
  (:report
   (lambda (condition stream)
     (format stream "Canvas operation ~S failed: ~S~@[ (~A)~]."
             (canvas-error-operation condition)
             (canvas-error-reason condition)
             (canvas-error-details condition)))))

(define-condition canvas-state-error (canvas-error)
  ((state
    :initarg :state
    :reader canvas-state-error-state)
   (expected-state
    :initarg :expected-state
    :reader canvas-state-error-expected-state))
  (:report
   (lambda (condition stream)
     (format stream "Cannot perform ~S on ~S in canvas state ~S; expected ~S."
             (canvas-error-operation condition)
             (canvas-error-canvas condition)
             (canvas-state-error-state condition)
             (canvas-state-error-expected-state condition)))))

(defclass canvas-clock () ()
  (:documentation "A policy object deciding when a canvas should run frames."))

(defclass demand-clock (canvas-clock) ()
  (:documentation "A clock whose frames happen only when explicitly requested."))

(defclass cadence-clock (canvas-clock)
  ((frames-per-second
    :initarg :frames-per-second
    :initform 60
    :reader clock-frames-per-second)
   (frame-function
    :initarg :frame-function
    :reader clock-frame-function)
   (next-frame-time
    :initform nil
    :accessor cadence-clock-next-frame-time))
  (:documentation "A clock which calls a frame function at a regular cadence."))

(defmethod initialize-instance :after ((clock cadence-clock) &key)
  (unless (and (realp (clock-frames-per-second clock))
               (plusp (clock-frames-per-second clock)))
    (error "FRAMES-PER-SECOND must be a positive real number."))
  (unless (functionp (clock-frame-function clock))
    (error "FRAME-FUNCTION must be a function.")))

(defun make-demand-clock ()
  "Construct a clock for explicitly requested frames."
  (make-instance 'demand-clock))

(defun make-cadence-clock (frame-function &key (frames-per-second 60))
  "Construct a clock which calls FRAME-FUNCTION with canvas and timestamp."
  (make-instance 'cadence-clock
                 :frame-function frame-function
                 :frames-per-second frames-per-second))

(defgeneric clock-wait-timeout (clock timestamp)
  (:documentation
   "Return milliseconds until CLOCK is due, or NIL to wait indefinitely."))

(defgeneric service-canvas-clock (clock canvas timestamp)
  (:documentation "Run any frame CLOCK has made due at TIMESTAMP."))

(defmethod clock-wait-timeout ((clock demand-clock) timestamp)
  (declare (ignore clock timestamp))
  nil)

(defmethod service-canvas-clock ((clock demand-clock) canvas timestamp)
  (declare (ignore clock canvas timestamp))
  nil)

(defmethod clock-wait-timeout ((clock cadence-clock) timestamp)
  (let ((next (cadence-clock-next-frame-time clock)))
    (if (or (null next) (<= next timestamp))
        0
        ;; SDL's event timeout has millisecond resolution.  Rounding upward
        ;; makes a 60 Hz deadline (16.667 ms) late by construction; on a
        ;; 120 Hz display that can miss the intended presentation refresh.
        ;; Wake on the last whole millisecond before the deadline and let the
        ;; event loop poll through the sub-millisecond remainder.
        (floor (* 1000 (- next timestamp))))))

(defmethod service-canvas-clock ((clock cadence-clock) canvas timestamp)
  (let ((next (cadence-clock-next-frame-time clock)))
    (when (or (null next) (<= next timestamp))
      ;; Deliberately do not accumulate missed frames.  A cadence is a pacing
      ;; policy, not a demand to replay time spent in a debugger.
      (setf (cadence-clock-next-frame-time clock)
            (let ((interval (/ 1.0d0 (clock-frames-per-second clock))))
              (if next
                  ;; Preserve the established phase after an ordinary late
                  ;; wakeup or a long pause, while skipping every missed frame.
                  (+ next
                     (* interval
                        (1+ (floor (/ (- timestamp next) interval)))))
                  (+ timestamp interval))))
      (funcall (clock-frame-function clock) canvas timestamp))))

(defclass canvas ()
  ((clock
    :initarg :clock
    :initform (make-demand-clock)
    :accessor canvas-clock)
   (event-handler
    :initarg :event-handler
    :initform nil
    :accessor canvas-event-handler))
  (:documentation "A native destination with a lifetime and frame clock."))

(defmethod (setf canvas-clock) :before (clock (canvas canvas))
  (declare (ignore canvas))
  (unless (typep clock 'canvas-clock)
    (error 'type-error :datum clock :expected-type 'canvas-clock)))

(defclass canvas-context () ()
  (:documentation "A GPU presentation relationship configured for a canvas."))

(defgeneric canvas-frame-resource-key (context surface-texture)
  (:documentation
   "Return the stable presentation slot key for SURFACE-TEXTURE in CONTEXT.

Applications use this to retain per-frame resources without assuming that a
backend returns the same Lisp wrapper every time it revisits a native drawable."))

(defmethod canvas-frame-resource-key
    ((context canvas-context) (surface-texture gpu-texture))
  (declare (ignore context))
  surface-texture)

;;; Portable input vocabulary. Native backends translate into these objects;
;;; consumers never need to know the SDL event ABI.

(defclass canvas-event-handler () ()
  (:documentation "Protocol class for objects receiving canvas events."))

(defclass canvas-event ()
  ((timestamp
    :initarg :timestamp
    :reader canvas-event-timestamp)))

(defclass canvas-pointer-event (canvas-event)
  ((x :initarg :x :reader canvas-pointer-event-x)
   (y :initarg :y :reader canvas-pointer-event-y)))

(defclass canvas-pointer-motion-event (canvas-pointer-event)
  ((delta-x :initarg :delta-x :initform 0.0 :reader canvas-pointer-event-delta-x)
   (delta-y :initarg :delta-y :initform 0.0 :reader canvas-pointer-event-delta-y)))
(defclass canvas-pointer-enter-event (canvas-pointer-motion-event) ())
(defclass canvas-pointer-exit-event (canvas-pointer-motion-event) ())

(defclass canvas-pointer-button-event (canvas-pointer-event)
  ((button :initarg :button :reader canvas-pointer-event-button)
   (clicks :initarg :clicks :initform 1 :reader canvas-pointer-event-clicks)))

(defclass canvas-pointer-button-press-event (canvas-pointer-button-event) ())
(defclass canvas-pointer-button-release-event (canvas-pointer-button-event) ())

(defclass canvas-pointer-wheel-event (canvas-pointer-event)
  ((scroll-x :initarg :scroll-x :initform 0.0
             :reader canvas-pointer-event-scroll-x)
   (scroll-y :initarg :scroll-y :initform 0.0
             :reader canvas-pointer-event-scroll-y))
  (:documentation
   "A scroll, carrying where the pointer was and how far the wheel turned.

The amounts are in wheel notches rather than pixels, positive up and right,
already corrected for a natural-scrolling platform -- what the window system
says the user asked for, not what the hardware reported."))

(defclass canvas-key-event (canvas-event)
  ((key-name
    :initarg :key-name
    :reader canvas-key-event-key-name)
   (modifiers
    :initarg :modifiers
    :initform nil
    :reader canvas-key-event-modifiers)
   (character
    :initarg :character
    :initform nil
    :reader canvas-key-event-character)
   (unshifted-character
    :initarg :unshifted-character
    :initform nil
    :reader canvas-key-event-unshifted-character)
   (repeat-p
    :initarg :repeat-p
    :initform nil
    :reader canvas-key-event-repeat-p))
  (:documentation
   "A portable physical-key event with logical modifiers and layout text."))

(defclass canvas-key-press-event (canvas-key-event) ())
(defclass canvas-key-release-event (canvas-key-event) ())

(defclass canvas-window-event (canvas-event) ())
(defclass canvas-window-size-event (canvas-window-event)
  ((width :initarg :width :reader canvas-window-event-width)
   (height :initarg :height :reader canvas-window-event-height)))
(defclass canvas-window-resized-event (canvas-window-size-event) ())
(defclass canvas-window-pixel-size-changed-event
    (canvas-window-size-event) ())
(defclass canvas-window-focus-gained-event (canvas-window-event) ())
(defclass canvas-window-focus-lost-event (canvas-window-event) ())
(defclass canvas-window-close-request-event (canvas-window-event) ())

(defgeneric handle-canvas-event (handler canvas event)
  (:documentation "Deliver portable EVENT from CANVAS to HANDLER."))

(defmethod handle-canvas-event ((handler null) (canvas canvas) event)
  (declare (ignore handler canvas event))
  nil)

(defmethod handle-canvas-event ((handler function) (canvas canvas) event)
  (funcall handler canvas event))

(defun dispatch-canvas-event (canvas event)
  (handle-canvas-event (canvas-event-handler canvas) canvas event))

(defstruct canvas-configuration
  "The small portable portion of a canvas presentation configuration."
  device
  format
  (usage '(:copy-dst)))

;;; Native-place protocol.

(defgeneric open-canvas (canvas)
  (:documentation "Realize CANVAS in its native window system."))

(defgeneric close-canvas (canvas)
  (:documentation "Close CANVAS and all presentation contexts attached to it."))

(defgeneric canvas-title (canvas)
  (:documentation "Return CANVAS's native title."))

(defgeneric canvas-size (canvas)
  (:documentation "Return CANVAS's drawable width and height as two values."))

(defgeneric canvas-logical-size (canvas)
  (:documentation "Return CANVAS's logical width and height as two values."))

(defgeneric canvas-position (canvas)
  (:documentation "Return CANVAS's native x and y position as two values."))

(defgeneric canvas-visible-p (canvas)
  (:documentation "Return whether CANVAS is intended to be visible."))

(defgeneric set-canvas-relative-pointer-mode (canvas enabled)
  (:documentation
   "Capture or release relative pointer motion for CANVAS."))

(defgeneric show-canvas (canvas)
  (:documentation "Make an open CANVAS visible."))

(defgeneric hide-canvas (canvas)
  (:documentation "Hide CANVAS without destroying its native resources."))

(defgeneric move-canvas (canvas x y)
  (:documentation "Move CANVAS to native position X, Y."))

(defgeneric resize-canvas (canvas width height)
  (:documentation "Resize CANVAS to WIDTH by HEIGHT logical units."))

(defgeneric raise-canvas (canvas)
  (:documentation "Request that the native host raise CANVAS."))

(defgeneric minimize-canvas (canvas)
  (:documentation "Request that the native host minimize CANVAS."))

(defgeneric restore-canvas (canvas)
  (:documentation "Restore a minimized or maximized CANVAS."))

(defgeneric canvas-state (canvas)
  (:documentation "Return the native lifecycle state of CANVAS."))

(defgeneric canvas-context (canvas)
  (:documentation "Return CANVAS's presentation context, or NIL."))

(defgeneric request-canvas-frame (canvas function)
  (:documentation
   "Run FUNCTION with a timestamp on CANVAS's native frame/event thread.

The initial native implementation is synchronous: the caller waits for the
function's values.  The protocol leaves room for a real frame scheduler."))

;;; Presentation-relationship protocol.

(defgeneric make-canvas-context (canvas gpu-provider &optional configuration)
  (:documentation
   "Create a GPU presentation relationship between CANVAS and GPU-PROVIDER.
When CONFIGURATION is omitted, return the context unconfigured."))

(defgeneric context-canvas (context)
  (:documentation "Return the native canvas presented by CONTEXT."))

(defgeneric context-device (context)
  (:documentation
   "Return the GPU device used by CONTEXT, or NIL before first configuration."))

(defgeneric configure-canvas-context (context configuration)
  (:documentation "Configure or reconfigure CONTEXT for presentation."))

(defgeneric unconfigure-canvas-context (context)
  (:documentation "Release CONTEXT's current presentation configuration."))

(defgeneric destroy-canvas-context (context)
  (:documentation "Destroy CONTEXT and its backend relationship."))

(defgeneric get-current-texture (context)
  (:documentation
   "Return the borrowed GPU texture current during a canvas frame."))

(defgeneric call-with-canvas-frame (context function)
  (:documentation
   "Acquire a frame texture, call FUNCTION with texture and encoder, and
complete presentation.  FUNCTION runs on the canvas's native frame thread."))

(defun present-canvas-frame (context function)
  "Schedule and present one frame through CONTEXT."
  (request-canvas-frame
   (context-canvas context)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (call-with-canvas-frame context function))))

(defun render-canvas-color (context red green blue &optional (alpha 1.0))
  "Clear and present one frame through CONTEXT."
  (present-canvas-frame
   context
   (lambda (texture encoder)
     (encode encoder
             (make-gpu-clear-texture-command
              :texture texture
              :color (vector red green blue alpha))))))
