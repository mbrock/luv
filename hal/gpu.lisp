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

(define-condition gpu-resource-leaked (warning)
  ((resource-class
    :initarg :resource-class
    :reader gpu-resource-leaked-class)
   (label
    :initarg :label
    :initform nil
    :reader gpu-resource-leaked-label))
  (:report
   (lambda (condition stream)
     (format stream
             "Leaked GPU ~A~@[ labeled ~S~]: it was reclaimed by the ~
garbage collector instead of being destroyed explicitly."
             (gpu-resource-leaked-class condition)
             (gpu-resource-leaked-label condition))))
  (:documentation "Signaled from the finalizer when a live GPU object is
collected without DESTROY.  The native resources are still reclaimed, but
explicit destruction is the expected discipline."))

(defvar *leaked-gpu-resources* '()
  "GPU-RESOURCE-LEAKED conditions recorded for objects the collector had
to reclaim.  Inspect or clear this from the REPL to audit leak hygiene.")

(defun note-gpu-resource-leak (resource-class label)
  "Record and signal one GPU-RESOURCE-LEAKED warning."
  (let ((condition (make-condition 'gpu-resource-leaked
                                   :resource-class resource-class
                                   :label label)))
    (push condition *leaked-gpu-resources*)
    (warn condition))
  (values))

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
(defclass gpu-buffer (gpu-object)
  ((size
    :initarg :size
    :reader gpu-buffer-size)
   (usage
    :initarg :usage
    :reader gpu-buffer-usage)))
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
(defclass gpu-texture-view (gpu-object)
  ((texture
    :initarg :texture
    :reader gpu-texture-view-texture)))
(defclass gpu-sampler (gpu-object) ())

(defclass gpu-command-buffer (gpu-object) ()
  (:documentation
   "Finished one-shot work accepted by a GPU queue's SUBMIT operation."))

(defclass gpu-encoder (gpu-object) ()
  (:documentation "Abstract receiver for recorded GPU commands."))

(defclass gpu-command-encoder (gpu-encoder) ())
(defclass gpu-render-pass-encoder (gpu-encoder) ())
(defclass gpu-compute-pass-encoder (gpu-encoder) ())

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
  (:documentation "Record an inspectable GPU COMMAND onto ENCODER."))

(defgeneric enqueue (queue command)
  (:documentation "Issue a queue-scoped GPU COMMAND onto QUEUE."))

(defgeneric finish (encoder)
  (:documentation
   "Seal ENCODER and return one finished, one-shot GPU command buffer."))

(defgeneric submit (queue work)
  (:documentation "Schedule some command buffers on the QUEUE.

Submission is asynchronous: returning does not mean the GPU has finished
the work, only that the implementation retains everything the work depends
on until it completes.  Use SUBMITTED-WORK-DONE to wait."))

(defgeneric submitted-work-done (queue)
  (:documentation "Block until all work submitted to QUEUE so far has
completed on the GPU."))

(defgeneric write-buffer (buffer data &key offset)
  (:documentation "Copy host DATA into BUFFER starting at byte OFFSET."))

(defgeneric read-buffer (buffer &key offset size)
  (:documentation
   "Wait for BUFFER's device queue and copy mapped bytes back to the host."))

(defgeneric destroy (handle)
  (:documentation
   "Logically invalidate HANDLE immediately.

Native teardown may be deferred until submitted work which captured HANDLE
has completed."))

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

(defun texture-format-bytes-per-texel (format)
  "Return the exact storage size of one texel in portable FORMAT."
  (ecase format
    ((:rgba8-unorm :rgba8-unorm-srgb
      :bgra8-unorm :bgra8-unorm-srgb
      :depth32-float :rg16-uint)
     4)
    (:rgba16-float 8)))

(defun texture-format-upload-element-type (format)
  "The packed array element type accepted by WRITE-TEXTURE for FORMAT."
  (ecase (texture-format-bytes-per-texel format)
    (4 '(unsigned-byte 32))
    (8 '(unsigned-byte 64))))

(defun texture-format-sample-transfer (format)
  "The colour transfer a sampled texture FORMAT applies before shader math.

This describes representation decoding, not a quantity.  Alpha remains
linear for the sRGB formats; the transfer names their RGB-channel behavior."
  (if (member format '(:rgba8-unorm-srgb :bgra8-unorm-srgb))
      :srgb-to-linear
      :identity))

(defstruct (texture-view-descriptor (:include gpu-descriptor))
  texture)

(defstruct (sampler-descriptor (:include gpu-descriptor))
  (address-mode-u :clamp-to-edge)
  (address-mode-v :clamp-to-edge)
  (address-mode-w :clamp-to-edge)
  (mag-filter :linear)
  (min-filter :linear)
  (mipmap-filter :nearest)
  compare)

(defstruct texture-copy
  texture
  (mip-level 0)
  (origin '(0 0 0))
  (aspect :all))

(defstruct texture-data-layout
  (offset 0)
  bytes-per-row
  rows-per-image)

(defstruct (bind-group-layout-descriptor (:include gpu-descriptor))
  entries)

(defstruct (bind-group-descriptor (:include gpu-descriptor))
  layout entries)

(defstruct (render-pipeline-descriptor (:include gpu-descriptor))
  "A render pipeline.  Each fragment target may name :BLEND
:PREMULTIPLIED-ALPHA; omitted blending retains opaque replacement semantics."
  layout vertex fragment (primitive '(:topology :triangle-list)) depth-stencil)

(defstruct (render-pass-descriptor (:include gpu-descriptor))
  color-attachments depth-stencil-attachment)

(defstruct (compute-pipeline-descriptor (:include gpu-descriptor))
  layout module (entry-point "main"))

(defstruct (command-encoder-descriptor (:include gpu-descriptor)))

(defstruct (shader-module-descriptor (:include gpu-descriptor))
  code
  (language :spir-v))

(defstruct gpu-command)

(defstruct (gpu-queue-command (:include gpu-command)))

(defstruct (gpu-command-encoder-command (:include gpu-command)))

(defstruct (gpu-pass-command (:include gpu-command)))

(defstruct (gpu-render-pass-command (:include gpu-pass-command)))

(defstruct (gpu-compute-pass-command (:include gpu-pass-command)))

(defstruct (gpu-draw-command (:include gpu-render-pass-command))
  vertex-count
  (instance-count 1)
  (first-vertex 0)
  (first-instance 0))

(defstruct (gpu-set-pipeline-command (:include gpu-pass-command))
  pipeline)

(defstruct (gpu-set-bind-group-command (:include gpu-pass-command))
  (index 0)
  bind-group)

(defstruct (gpu-set-vertex-buffer-command (:include gpu-render-pass-command))
  (slot 0)
  buffer
  (offset 0))

(defstruct (gpu-dispatch-workgroups-command
            (:include gpu-compute-pass-command))
  x
  (y 1)
  (z 1))

(defstruct (gpu-set-viewport-command (:include gpu-render-pass-command))
  x
  y
  width
  height
  min-depth
  max-depth)

(defstruct (gpu-set-scissor-command (:include gpu-render-pass-command))
  x y width height)

(defstruct (gpu-clear-texture-command
            (:include gpu-command-encoder-command))
  texture
  color)

(defstruct (gpu-copy-texture-command
            (:include gpu-command-encoder-command))
  source
  destination)

(defstruct (gpu-copy-texture-to-buffer-command
            (:include gpu-command-encoder-command))
  source
  destination)

(defstruct (gpu-write-texture-command (:include gpu-queue-command))
  destination
  data
  data-layout
  size)

(defstruct (gpu-prepare-texture-command
             (:include gpu-command-encoder-command))
  texture
  usage)

(defmethod encode ((encoder gpu-encoder) (command gpu-command))
  (error 'gpu-request-error
         :operation :encode
         :descriptor command
         :reason :unsupported-command-for-encoder
         :details (list :encoder (class-name (class-of encoder))
                        :command (type-of command))))

(defmethod enqueue ((queue gpu-queue) (command gpu-queue-command))
  (error 'gpu-request-error
         :operation :enqueue
         :descriptor command
         :reason :unsupported-queue-command
         :details (list :queue (class-name (class-of queue))
                        :command (type-of command))))

;;; These WebGPU-flavored verbs are intentionally just REPL conveniences.
;;; Command objects are the protocol: backends specialize ENCODE for recorded
;;; encoder commands and ENQUEUE for immediate queue commands.

(defun set-pipeline (pass-encoder pipeline)
  (encode pass-encoder
          (make-gpu-set-pipeline-command :pipeline pipeline)))

(defun set-bind-group (pass-encoder index bind-group)
  (encode pass-encoder
          (make-gpu-set-bind-group-command
           :index index :bind-group bind-group)))

(defun set-vertex-buffer (pass-encoder slot buffer &key (offset 0))
  (encode pass-encoder
          (make-gpu-set-vertex-buffer-command
           :slot slot :buffer buffer :offset offset)))

(defun set-scissor-rect (pass-encoder x y width height)
  (encode pass-encoder
          (make-gpu-set-scissor-command
           :x x :y y :width width :height height)))

(defun dispatch-workgroups (pass-encoder x &optional (y 1) (z 1))
  (encode pass-encoder
          (make-gpu-dispatch-workgroups-command :x x :y y :z z)))

(defun draw (pass-encoder vertex-count
             &optional (instance-count 1) (first-vertex 0) (first-instance 0))
  (encode pass-encoder
          (make-gpu-draw-command
           :vertex-count vertex-count :instance-count instance-count
           :first-vertex first-vertex :first-instance first-instance)))

(defun write-texture (queue destination data data-layout size)
  "Issue one WebGPU-style convenience upload onto QUEUE."
  (enqueue queue
           (make-gpu-write-texture-command
            :destination destination :data data
            :data-layout data-layout :size size)))

(defun prepare-texture (encoder texture usage)
  "Prepare TEXTURE for semantic USAGE in ENCODER's following commands.

The backend owns any layout transition, hazard barrier, or validation needed
to realize the usage.  Application code does not dispatch on the backend.
#T5MQO0"
  (encode encoder
          (make-gpu-prepare-texture-command
           :texture texture :usage usage)))
