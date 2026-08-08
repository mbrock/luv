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

(defclass gpu-provider () ()
  (:documentation "Instances of GPU-PROVIDER subclasses are platform-specific
factories for requesting GPU-DEVICE instances."))

(defvar *gpu-provider* nil
  "If you're lucky, someone has bound this to a working GPU-PROVIDER.")

(defclass gpu-object () 
  (label)
  (:documentation "Base class for instantiated GPU resources."))

(defclass gpu-device (gpu-object) ())

(defclass gpu-queue (gpu-object) ())
(defclass gpu-buffer (gpu-object) ())
(defclass gpu-texture (gpu-object) ())
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
