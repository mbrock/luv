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
        (ceiling (* 1000 (- next timestamp))))))

(defmethod service-canvas-clock ((clock cadence-clock) canvas timestamp)
  (let ((next (cadence-clock-next-frame-time clock)))
    (when (or (null next) (<= next timestamp))
      ;; Deliberately do not accumulate missed frames.  A cadence is a pacing
      ;; policy, not a demand to replay time spent in a debugger.
      (setf (cadence-clock-next-frame-time clock)
            (+ timestamp (/ 1.0d0 (clock-frames-per-second clock))))
      (funcall (clock-frame-function clock) canvas timestamp))))

(defclass canvas ()
  ((clock
    :initarg :clock
    :initform (make-demand-clock)
    :accessor canvas-clock))
  (:documentation "A native destination with a lifetime and frame clock."))

(defmethod (setf canvas-clock) :before (clock (canvas canvas))
  (declare (ignore canvas))
  (unless (typep clock 'canvas-clock)
    (error 'type-error :datum clock :expected-type 'canvas-clock)))

(defclass canvas-context () ()
  (:documentation "A GPU presentation relationship configured for a canvas."))

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

(defgeneric canvas-size (canvas)
  (:documentation "Return CANVAS's drawable width and height as two values."))

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
   "Create a GPU presentation relationship between CANVAS and GPU-PROVIDER."))

(defgeneric context-canvas (context)
  (:documentation "Return the native canvas presented by CONTEXT."))

(defgeneric context-device (context)
  (:documentation "Return the GPU device used by CONTEXT."))

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
