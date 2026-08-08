;; Let's define something roughly like a WebGPU style API.
;;
;; Then we can implement it for our Vulkan bindings.
;;
;; Later we can implement it for Metal, etc.
;;
;; This API itself is agnostic of presenting, swapchains, etc.
;;
;; The base classes omit parent pointer slots.
;; We might, like e.g. McCLIM, also define STANDARD-GPU-QUEUE and so on.

(in-package #:luv)

(define-condition gpu-error (error)
  ((operation
    :initarg :operation
    :initform nil
    :reader gpu-error-operation))
  (:documentation "Base condition for errors exposed by the luv GPU API."))

(define-condition gpu-request-error (gpu-error)
  ((descriptor
    :initarg :descriptor
    :reader gpu-request-error-descriptor)
   (reason
    :initarg :reason
    :reader gpu-request-error-reason)
   (details
    :initarg :details
    :initform nil
    :reader gpu-request-error-details))
  (:report
   (lambda (condition stream)
     (case (gpu-request-error-reason condition)
       (:invalid-descriptor
        (format stream "Expected a GPU descriptor, got ~S."
                (gpu-request-error-details condition)))
       (:unsupported-features
        (format stream "Required GPU features are not implemented yet: ~S"
                (gpu-request-error-details condition)))
       (:unsupported-limits
        (format stream "Required GPU limits are not implemented yet: ~S"
                (gpu-request-error-details condition)))
       (otherwise
        (format stream "GPU request failed~@[ during ~S~]: ~S~@[ (~S)~]"
                (gpu-error-operation condition)
                (gpu-request-error-reason condition)
                (gpu-request-error-details condition)))))))

(define-condition gpu-object-error (gpu-error)
  ((object
    :initarg :object
    :reader gpu-object-error-object))
  (:documentation "Base condition for an operation rejected by a GPU object."))

(define-condition gpu-object-destroyed-error (gpu-object-error) ()
  (:report
   (lambda (condition stream)
     (format stream "~S has already been destroyed~@[ during ~S~]."
             (gpu-object-error-object condition)
             (gpu-error-operation condition)))))

(define-condition gpu-invalid-state-error (gpu-object-error)
  ((state
    :initarg :state
    :reader gpu-invalid-state-error-state)
   (expected-state
    :initarg :expected-state
    :reader gpu-invalid-state-error-expected-state))
  (:report
   (lambda (condition stream)
     (format stream
             "Cannot perform ~S on ~S in state ~S; expected ~S."
             (gpu-error-operation condition)
             (gpu-object-error-object condition)
             (gpu-invalid-state-error-state condition)
             (gpu-invalid-state-error-expected-state condition)))))

(define-condition gpu-device-mismatch-error (gpu-object-error)
  ((expected-device
    :initarg :expected-device
    :reader gpu-device-mismatch-error-expected-device)
   (actual-device
    :initarg :actual-device
    :reader gpu-device-mismatch-error-actual-device))
  (:report
   (lambda (condition stream)
     (format stream "~S belongs to ~S, not the device ~S required by ~S."
             (gpu-object-error-object condition)
             (gpu-device-mismatch-error-actual-device condition)
             (gpu-device-mismatch-error-expected-device condition)
             (gpu-error-operation condition)))))

(define-condition gpu-usage-error (gpu-object-error)
  ((required-usage
    :initarg :required-usage
    :reader gpu-usage-error-required-usage)
   (actual-usage
    :initarg :actual-usage
    :reader gpu-usage-error-actual-usage))
  (:report
   (lambda (condition stream)
     (format stream "~S requires usage ~S for ~S, but was created with ~S."
             (gpu-object-error-object condition)
             (gpu-usage-error-required-usage condition)
             (gpu-error-operation condition)
             (gpu-usage-error-actual-usage condition)))))

(defclass gpu-provider () ()
  (:documentation "Instances of GPU-PROVIDER subclasses are platform-specific
factories for requesting GPU-DEVICE instances."))

(defvar *gpu-provider* nil
  "If you're lucky, someone has bound this to a working GPU-PROVIDER.")

(defclass gpu-object ()
  ((label :initarg :label
          :initform nil
          :accessor gpu-object-label))
  (:documentation "Base class for instantiated GPU resources."))

(defclass gpu-device (gpu-object) ())

(defclass gpu-queue (gpu-object) ())
(defclass gpu-buffer (gpu-object) ())
(defclass gpu-texture (gpu-object)
  ((size
    :initarg :size
    :reader gpu-texture-size)
   (usage
    :initarg :usage
    :reader gpu-texture-usage)
   (dimensions
    :initarg :dimensions
    :reader gpu-texture-dimensions)
   (format
    :initarg :format
    :reader gpu-texture-format)))
(defclass gpu-texture-view (gpu-object) ())

(defclass gpu-command-buffer (gpu-object) ())

(defclass gpu-command-encoder (gpu-object) ())
(defclass gpu-render-pass-encoder (gpu-command-encoder) ())
(defclass gpu-compute-pass-encoder (gpu-command-encoder) ())

(defclass gpu-bind-group (gpu-object) ())
(defclass gpu-bind-group-layout (gpu-object) ())

(defclass gpu-pipeline (gpu-object) ())
(defclass gpu-render-pipeline (gpu-pipeline) ())
(defclass gpu-compute-pipeline (gpu-pipeline) ())

(defclass gpu-shader-module (gpu-object) ())

(defgeneric request-gpu-device (provider &optional descriptor))

(defgeneric device-queue (device)
  (:documentation "Return the default queue belonging to DEVICE."))

(defgeneric create (device descriptor)
  (:documentation "Asks the DEVICE for a handle to newly created instance
of some object fulfilling the DESCRIPTOR."))

(defgeneric encode (encoder command)
  (:documentation "Asks the ENCODER to append the COMMAND to its work
sequence."))

(defgeneric finish (encoder)
  (:documentation "Asks the ENCODER to seal its work sequence."))

(defgeneric submit (queue work)
  (:documentation "Submit some command buffers to the QUEUE."))

(defgeneric destroy (handle)
  (:documentation "Destroy the GPU object denoted by HANDLE."))

(defmethod submit (queue (buffers vector))
  "Platforms can override this for more efficient batch submission."
  (loop for buffer across buffers
        do (submit queue buffer)))

(defgeneric begin-render-pass (encoder descriptor))
(defgeneric begin-compute-pass (encoder &optional descriptor))
(defgeneric end-pass (pass-encoder))

(defstruct gpu-descriptor (label nil))

(defstruct (device-descriptor (:include gpu-descriptor))
  required-features required-limits)

(defstruct (buffer-descriptor (:include gpu-descriptor))
  size usage)

(defstruct (texture-descriptor (:include gpu-descriptor))
  size usage dimensions format)

(defstruct (texture-view-descriptor (:include gpu-descriptor))
  )

(defstruct (bind-group-layout-descriptor (:include gpu-descriptor))
  entries)

(defstruct (bind-group-descriptor (:include gpu-descriptor))
  layout entries resource)

(defstruct (render-pipeline-descriptor (:include gpu-descriptor))
  layout vertex fragment)

(defstruct (compute-pipeline-descriptor (:include gpu-descriptor))
  layout module)

(defstruct (command-encoder-descriptor (:include gpu-descriptor)))

(defstruct (shader-module-descriptor (:include gpu-descriptor))
  code)

(defstruct gpu-command)

(defstruct (gpu-draw-command (:include gpu-command))
  vertex-count
  instance-count
  first-vertex
  first-instance)

(defstruct (gpu-set-viewport-command (:include gpu-command))
  x 
  y
  width
  height
  min-depth
  max-depth)

(defstruct (gpu-clear-texture-command (:include gpu-command))
  texture
  color)

(defstruct (gpu-copy-texture-command (:include gpu-command))
  source
  destination)
