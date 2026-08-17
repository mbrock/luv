;;;; The Metal 4 implementation of luv's portable GPU protocol.

(in-package #:luv)

(define-condition metal-gpu-error (gpu-error)
  ((reason :initarg :reason :reader metal-gpu-error-reason)
   (details :initarg :details :initform nil :reader metal-gpu-error-details))
  (:report
   (lambda (condition stream)
     (format stream "Metal GPU operation ~S failed: ~S~@[ (~S)~]."
             (gpu-error-operation condition)
             (metal-gpu-error-reason condition)
             (metal-gpu-error-details condition)))))

(defclass metal-gpu-provider (gpu-provider) ()
  (:documentation "A provider for the system's preferred Metal device."))

;; Metal is the native default on Apple hosts.  Preserve an explicit provider
;; installed by an embedding application before luv finishes loading.
(unless *gpu-provider*
  (setf *gpu-provider* (make-instance 'metal-gpu-provider)))

(defclass metal-gpu-object ()
  ((native-object :initarg :native-object :reader metal-native-object)
   (destroyed-p :initform nil :accessor metal-object-destroyed-p)
   (last-submission
    :initform 0
    :accessor metal-object-last-submission
    :documentation "Newest queue submission which may still use this object.")))

(defclass metal-gpu-device (gpu-device metal-gpu-object)
  ((queue :initform nil :accessor metal-device-queue)
   (compiler :initarg :compiler :reader metal-device-compiler)
   (residency-set :initarg :residency-set
                  :reader metal-device-residency-set)))

(defstruct metal-submission
  value command-buffers resources pending-destroys)

(defclass metal-gpu-queue (gpu-queue metal-gpu-object)
  ((device :initarg :device :reader metal-queue-device)
   (completion-event :initarg :completion-event
                     :reader metal-queue-completion-event)
   (submitted-value :initform 0 :accessor metal-queue-submitted-value)
   (pending-submissions :initform nil
                        :accessor metal-queue-pending-submissions)
   (lock :initform (sb-thread:make-mutex
                    :name "luv Metal submission queue")
         :reader metal-queue-lock)))

(defclass metal-gpu-buffer (gpu-buffer metal-gpu-object)
  ((device :initarg :device :reader metal-buffer-device)
   (mapped :initarg :mapped :reader metal-buffer-mapped)))

(defclass metal-gpu-texture (gpu-texture metal-gpu-object)
  ((device :initarg :device :reader metal-texture-device)
   (owned-p :initarg :owned-p :initform t :reader metal-texture-owned-p)))

(defclass metal-gpu-texture-view (gpu-texture-view metal-gpu-object)
  ((device :initarg :device :reader metal-texture-view-device)))

(defclass metal-gpu-sampler (gpu-sampler metal-gpu-object)
  ((device :initarg :device :reader metal-sampler-device)))

(defclass metal-gpu-bind-group-layout (gpu-bind-group-layout metal-gpu-object)
  ((device :initarg :device :reader metal-bind-group-layout-device)
   (entries :initarg :entries :reader metal-bind-group-layout-entries)))

(defclass metal-gpu-bind-group (gpu-bind-group metal-gpu-object)
  ((device :initarg :device :reader metal-bind-group-device)
   (layout :initarg :layout :reader metal-bind-group-layout)
   (entries :initarg :entries :reader metal-bind-group-entries)))

(defclass metal-gpu-shader-module (gpu-shader-module metal-gpu-object)
  ((device :initarg :device :reader metal-shader-module-device)
   (document :initarg :document :reader metal-shader-module-document)
   (entry-point :initarg :entry-point :reader metal-shader-module-entry-point)
   (function-type :initarg :function-type
                  :reader metal-shader-module-function-type))
  (:documentation "A device-compiled MSL library retaining its source document."))

(defclass metal-gpu-render-pipeline (gpu-render-pipeline metal-gpu-object)
  ((device :initarg :device :reader metal-render-pipeline-device)
   (layout :initarg :layout :reader metal-render-pipeline-layout)
   (vertex-buffers :initarg :vertex-buffers
                   :reader metal-render-pipeline-vertex-buffers)
   (depth-format :initarg :depth-format
                 :reader metal-render-pipeline-depth-format)
   (primitive-topology :initarg :primitive-topology
                       :reader metal-render-pipeline-primitive-topology)
   (fragment-p :initarg :fragment-p
               :reader metal-render-pipeline-fragment-p)
   (depth-stencil-state :initarg :depth-stencil-state
                        :reader metal-render-pipeline-depth-stencil-state))
  (:documentation
   "A linked Metal 4 render pipeline and its draw-time depth state."))

(defclass metal-gpu-mesh-render-pipeline (metal-gpu-render-pipeline)
  ((task-workgroup-size
    :initarg :task-workgroup-size
    :reader metal-mesh-pipeline-task-workgroup-size)
   (mesh-workgroup-size
    :initarg :mesh-workgroup-size
    :reader metal-mesh-pipeline-mesh-workgroup-size))
  (:documentation
   "A linked Metal 4 object/mesh pipeline with its dispatch geometry."))

(defclass metal-gpu-command-encoder (gpu-command-encoder)
  ((device :initarg :device :reader metal-command-encoder-device)
   (allocator :initarg :allocator :accessor metal-encoder-allocator)
   (command-buffer :initarg :command-buffer
                   :accessor metal-encoder-command-buffer)
   (resources :initform (make-hash-table :test #'eq)
              :reader metal-encoder-resources)
   (active-pass :initform nil :accessor metal-encoder-active-pass)
   (pending-consumer-barrier
    :initform nil :accessor metal-encoder-pending-consumer-barrier)
   (state :initform :recording :accessor metal-encoder-state)
   (encoded-p :initform nil :accessor metal-encoder-encoded-p))
  (:documentation
   "A general Metal 4 command encoder which owns recording memory until FINISH."))

(defclass metal-gpu-command-buffer (gpu-command-buffer metal-gpu-object)
  ((device :initarg :device :reader metal-command-buffer-device)
   (allocator :initarg :allocator :reader metal-command-buffer-allocator)
   (resources :initarg :resources :reader metal-command-buffer-resources)
   (state :initform :ready :accessor metal-command-buffer-state))
  (:documentation
   "One finished, one-shot Metal 4 command buffer and its recorded dependencies."))

(defclass metal-render-pass-encoder (gpu-render-pass-encoder)
  ((owner :initarg :owner :reader metal-render-pass-owner)
   (native-encoder :initarg :native-encoder
                   :reader metal-render-pass-native-encoder)
   (pipeline :initform nil :accessor metal-render-pass-pipeline)
   (argument-table :initform nil
                   :accessor metal-render-pass-argument-table)
   (vertex-bindings :initform (make-hash-table)
                    :reader metal-render-pass-vertex-bindings)
   (bind-group :initform nil :accessor metal-render-pass-bind-group)
   (state :initform :encoding :accessor metal-render-pass-state))
  (:documentation
   "A Metal 4 render encoder whose resources arrive through argument tables.

The first vertex-stage realization is the executable mechanism described by
#348B7B; it deliberately has no legacy individual-resource setter path."))

(defun ensure-live-metal-object (object operation)
  (when (metal-object-destroyed-p object)
    (error 'gpu-object-destroyed-error :object object :operation operation))
  object)

(defun check-metal-device-descriptor (descriptor)
  (unless (typep descriptor 'device-descriptor)
    (error 'gpu-request-error :operation :request-device
           :descriptor descriptor :reason :invalid-descriptor))
  (when (or (device-descriptor-required-features descriptor)
            (device-descriptor-required-limits descriptor))
    (error 'gpu-request-error :operation :request-device
           :descriptor descriptor :reason :unsupported-requirements)))

(defmethod request-gpu-device
    ((provider metal-gpu-provider) &optional descriptor)
  (declare (ignore provider))
  (let ((descriptor (or descriptor (make-device-descriptor))))
    (check-metal-device-descriptor descriptor)
    (let ((native-device (luv.metal:make-system-default-device)))
      (unless native-device
        (error 'metal-gpu-error :operation :request-device
               :reason :no-system-device))
      (handler-case
          (let ((native-queue nil)
                (native-compiler nil)
                (native-residency-set nil)
                (native-completion-event nil))
            (unwind-protect
                 (progn
                   (setf native-queue
                         (luv.metal:new-metal-4-command-queue native-device))
                   (unless native-queue
                     (error 'metal-gpu-error :operation :request-device
                            :reason :metal-4-unavailable))
                   (multiple-value-bind (compiler diagnostic)
                       (luv.metal:new-metal-4-compiler
                        native-device :label "luv Metal 4 compiler")
                     (unless compiler
                       (error 'metal-gpu-error :operation :request-device
                              :reason :metal-4-compiler-unavailable
                              :details diagnostic))
                     (setf native-compiler compiler))
                   (multiple-value-bind (residency-set diagnostic)
                       (luv.metal:new-metal-residency-set
                        native-device :label "luv Metal 4 resources")
                     (unless residency-set
                       (error 'metal-gpu-error :operation :request-device
                              :reason :residency-set-creation-failed
                              :details diagnostic))
                     (setf native-residency-set residency-set))
                   (setf native-completion-event
                         (luv.metal:new-metal-shared-event native-device))
                   (unless native-completion-event
                     (error 'metal-gpu-error :operation :request-device
                            :reason :completion-event-creation-failed))
                   (luv.metal:add-metal-queue-residency-set
                    native-queue native-residency-set)
                   (let* ((device
                            (make-instance
                             'metal-gpu-device
                             :label (gpu-descriptor-label descriptor)
                             :native-object native-device
                             :compiler native-compiler
                             :residency-set native-residency-set))
                          (queue
                            (make-instance
                             'metal-gpu-queue
                             :label "default Metal 4 queue"
                             :native-object native-queue
                             :device device
                             :completion-event native-completion-event)))
                     (setf (metal-device-queue device) queue
                           native-queue nil
                           native-compiler nil
                           native-residency-set nil
                           native-completion-event nil)
                     device))
              (when native-completion-event
                (luv.objective-c:release-objective-c-object
                 native-completion-event))
              (when native-residency-set
                (luv.objective-c:release-objective-c-object
                 native-residency-set))
              (when native-compiler
                (luv.objective-c:release-objective-c-object native-compiler))
              (when native-queue
                (luv.objective-c:release-objective-c-object native-queue))))
        (error (condition)
          (unless (luv.objective-c:objective-c-object-released-p native-device)
            (luv.objective-c:release-objective-c-object native-device))
          (error condition))))))

(defmethod device-queue ((device metal-gpu-device))
  (ensure-live-metal-object device :device-queue)
  (metal-device-queue device))

(defun ensure-metal-command-encoder-state (encoder operation)
  (unless (eq :recording (metal-encoder-state encoder))
    (error 'gpu-invalid-state-error :object encoder :operation operation
           :state (metal-encoder-state encoder) :expected-state :recording))
  encoder)

(defun ensure-no-active-metal-pass (encoder operation)
  (when (metal-encoder-active-pass encoder)
    (error 'gpu-invalid-state-error :object encoder :operation operation
           :state :pass-active :expected-state :between-passes))
  encoder)

(defun retain-metal-resource (encoder resource)
  "Retain RESOURCE as a dependency of ENCODER's eventual command buffer."
  (setf (gethash resource (metal-encoder-resources encoder)) t)
  resource)

(defun metal-encoder-resource-list (encoder)
  (loop for resource being the hash-keys of (metal-encoder-resources encoder)
        collect resource))

(defmethod create
    ((device metal-gpu-device) (descriptor command-encoder-descriptor))
  "Allocate and begin one general Metal 4 command buffer."
  (ensure-live-metal-object device :create-command-encoder)
  (let ((allocator nil)
        (command-buffer nil)
        (completed-p nil))
    (unwind-protect
         (progn
           (setf allocator
                 (luv.metal:new-command-allocator (metal-native-object device))
                 command-buffer
                 (luv.metal:new-command-buffer (metal-native-object device)))
           (unless (and allocator command-buffer)
             (error 'metal-gpu-error :operation :create-command-encoder
                    :reason :command-resource-creation-failed))
           (luv.metal:begin-command-buffer command-buffer allocator)
           (let ((encoder
                   (make-instance
                    'metal-gpu-command-encoder
                    :label (gpu-descriptor-label descriptor)
                    :device device :allocator allocator
                    :command-buffer command-buffer)))
             (setf allocator nil command-buffer nil completed-p t)
             encoder))
      (unless completed-p
        (when command-buffer
          (luv.objective-c:release-objective-c-object command-buffer))
        (when allocator
          (luv.objective-c:release-objective-c-object allocator))))))

(defmethod finish ((encoder metal-gpu-command-encoder))
  "End recording and transfer native memory and dependencies to one-shot work."
  (ensure-metal-command-encoder-state encoder :finish)
  (ensure-no-active-metal-pass encoder :finish)
  (let ((command-buffer (metal-encoder-command-buffer encoder))
        (allocator (metal-encoder-allocator encoder)))
    (luv.metal:end-command-buffer command-buffer)
    (setf (metal-encoder-state encoder) :finished
          (metal-encoder-command-buffer encoder) nil
          (metal-encoder-allocator encoder) nil)
    (make-instance
     'metal-gpu-command-buffer
     :label (gpu-object-label encoder)
     :native-object command-buffer :allocator allocator
     :device (metal-command-encoder-device encoder)
     :resources (metal-encoder-resource-list encoder))))

(defgeneric destroy-metal-native (object)
  (:documentation
   "Release OBJECT's native Metal ownership after its last use is complete."))

(defun metal-queue-completed-frontier (queue)
  (luv.metal:metal-shared-event-signaled-value
   (metal-queue-completion-event queue)))

(defun maintain-metal-queue (queue)
  "Retire the completed submission prefix and its deferred native releases."
  (sb-thread:with-recursive-lock ((metal-queue-lock queue))
    (let ((frontier (metal-queue-completed-frontier queue)))
      (loop while (and (metal-queue-pending-submissions queue)
                       (<= (metal-submission-value
                            (first (metal-queue-pending-submissions queue)))
                           frontier))
            do (let ((submission
                       (pop (metal-queue-pending-submissions queue))))
                 (dolist (resource
                          (metal-submission-pending-destroys submission))
                   (destroy-metal-native resource))))
      frontier)))

(defun metal-destroy-or-defer (resource device)
  "Release RESOURCE now or attach its native teardown to its last submission."
  (let ((queue (metal-device-queue device))
        (last-use (metal-object-last-submission resource)))
    (if (or (null queue)
            (metal-object-destroyed-p device)
            (zerop last-use))
        (destroy-metal-native resource)
        (sb-thread:with-recursive-lock ((metal-queue-lock queue))
          (let ((submission
                  (and (> last-use (metal-queue-completed-frontier queue))
                       (find last-use (metal-queue-pending-submissions queue)
                             :key #'metal-submission-value))))
            (if submission
                (pushnew resource
                         (metal-submission-pending-destroys submission)
                         :test #'eq)
                (destroy-metal-native resource))))))
  (values))

(defun check-metal-command-buffer-for-submit (queue command-buffer)
  (unless (typep command-buffer 'metal-gpu-command-buffer)
    (error 'gpu-request-error :operation :submit
           :descriptor command-buffer :reason :invalid-command-buffer))
  (ensure-live-metal-object command-buffer :submit)
  (unless (eq (metal-queue-device queue)
              (metal-command-buffer-device command-buffer))
    (error 'gpu-device-mismatch-error
           :object command-buffer :operation :submit
           :expected-device (metal-queue-device queue)
           :actual-device (metal-command-buffer-device command-buffer)))
  (unless (eq :ready (metal-command-buffer-state command-buffer))
    (error 'gpu-invalid-state-error
           :object command-buffer :operation :submit
           :state (metal-command-buffer-state command-buffer)
           :expected-state :ready))
  (dolist (resource (metal-command-buffer-resources command-buffer))
    (ensure-live-metal-object resource :submit))
  command-buffer)

(defun collect-metal-submission-resources (command-buffers)
  (remove-duplicates
   (loop for command-buffer across command-buffers
         append (metal-command-buffer-resources command-buffer))
   :test #'eq))

(defmethod submitted-work-done ((queue metal-gpu-queue))
  "Wait for the Metal 4 shared-event frontier most recently submitted."
  (ensure-live-metal-object queue :submitted-work-done)
  (let ((value
          (sb-thread:with-recursive-lock ((metal-queue-lock queue))
            (metal-queue-submitted-value queue))))
    (when (plusp value)
      (unless (plusp
               (luv.metal:wait-for-metal-shared-event
                (metal-queue-completion-event queue) value 30000))
        (error 'metal-gpu-error :operation :submitted-work-done
               :reason :completion-timeout :details value))))
  (maintain-metal-queue queue)
  (values))

(defun submit-metal-command-buffers
    (queue command-buffers &key after-commit)
  "Commit finished Metal work and retain its dependencies to QUEUE's frontier.

AFTER-COMMIT performs the drawable signal and presentation handshake before
the completion event is enqueued.  Presentation is the only caller of that
backend-local extension; ordinary callers use SUBMIT.  #T9K4RC"
  (ensure-live-metal-object queue :submit)
  (unless (vectorp command-buffers)
    (error 'gpu-request-error :operation :submit
           :descriptor command-buffers :reason :invalid-command-buffers))
  (when (zerop (length command-buffers))
    (return-from submit-metal-command-buffers nil))
  (sb-thread:with-recursive-lock ((metal-queue-lock queue))
    (maintain-metal-queue queue)
    (loop for command-buffer across command-buffers
          do (check-metal-command-buffer-for-submit queue command-buffer))
    (let* ((resources (collect-metal-submission-resources command-buffers))
           (native-command-buffers
             (map 'vector #'metal-native-object command-buffers)))
      (luv.metal:commit-command-buffers
       (metal-native-object queue) native-command-buffers)
      (let ((value (incf (metal-queue-submitted-value queue))))
        (loop for command-buffer across command-buffers
              do (setf (metal-command-buffer-state command-buffer) :submitted
                       (metal-object-last-submission command-buffer) value))
        (dolist (resource resources)
          (setf (metal-object-last-submission resource) value))
        ;; Register queue ownership before presentation can fail.
        (setf (metal-queue-pending-submissions queue)
              (nconc (metal-queue-pending-submissions queue)
                     (list (make-metal-submission
                            :value value
                            :command-buffers command-buffers
                            :resources resources))))
        (unwind-protect
             (when after-commit
               (funcall after-commit))
          (luv.metal:signal-metal-event
           (metal-native-object queue)
           (metal-queue-completion-event queue) value))
        value))))

(defmethod destroy ((encoder metal-gpu-command-encoder))
  (when (eq :recording (metal-encoder-state encoder))
    (when (metal-encoder-active-pass encoder)
      (ignore-errors (end-pass (metal-encoder-active-pass encoder))))
    (when (metal-encoder-command-buffer encoder)
      (ignore-errors
        (luv.metal:end-command-buffer (metal-encoder-command-buffer encoder)))
      (luv.objective-c:release-objective-c-object
       (metal-encoder-command-buffer encoder)))
    (when (metal-encoder-allocator encoder)
      (luv.objective-c:release-objective-c-object
       (metal-encoder-allocator encoder)))
    (setf (metal-encoder-command-buffer encoder) nil
          (metal-encoder-allocator encoder) nil))
  (setf (metal-encoder-state encoder) :destroyed)
  (values))

(defmethod destroy-metal-native ((command-buffer metal-gpu-command-buffer))
  (luv.objective-c:release-objective-c-object
   (metal-native-object command-buffer))
  (luv.objective-c:release-objective-c-object
   (metal-command-buffer-allocator command-buffer))
  (values))

(defmethod destroy ((command-buffer metal-gpu-command-buffer))
  (unless (metal-object-destroyed-p command-buffer)
    (setf (metal-object-destroyed-p command-buffer) t
          (metal-command-buffer-state command-buffer) :destroyed)
    (metal-destroy-or-defer
     command-buffer (metal-command-buffer-device command-buffer)))
  (values))

(defmethod submit
    ((queue metal-gpu-queue) (command-buffer metal-gpu-command-buffer))
  (submit-metal-command-buffers queue (vector command-buffer)))

(defmethod submit ((queue metal-gpu-queue) (command-buffers vector))
  (submit-metal-command-buffers queue command-buffers))

(defmethod destroy ((device metal-gpu-device))
  (unless (metal-object-destroyed-p device)
    (let ((queue (metal-device-queue device)))
      (when (and queue (not (metal-object-destroyed-p queue)))
        (submitted-work-done queue)))
    (luv.objective-c:release-objective-c-object
     (metal-device-compiler device))
    (let ((queue (metal-device-queue device)))
      (when (and queue (not (metal-object-destroyed-p queue)))
        (luv.metal:remove-metal-queue-residency-set
         (metal-native-object queue) (metal-device-residency-set device))
        (luv.objective-c:release-objective-c-object
         (metal-queue-completion-event queue))
        (luv.objective-c:release-objective-c-object
         (metal-native-object queue))
        (setf (metal-object-destroyed-p queue) t)))
    (luv.objective-c:release-objective-c-object
     (metal-device-residency-set device))
    (luv.objective-c:release-objective-c-object (metal-native-object device))
    (setf (metal-object-destroyed-p device) t))
  (values))

(defun normalize-metal-buffer-usage (descriptor)
  (let* ((raw (buffer-descriptor-usage descriptor))
         (usage
           (typecase raw
             (keyword (list raw))
             (list (remove-duplicates raw))
             (vector (remove-duplicates (coerce raw 'list)))
             (otherwise nil))))
    (unless (and usage
                 (every (lambda (value)
                          (member value '(:uniform :vertex :copy-dst)))
                        usage))
      (reject-metal-gpu-request descriptor :unsupported-buffer-usage raw))
    usage))

(defmethod create
    ((device metal-gpu-device) (descriptor buffer-descriptor))
  "Create one shared Metal buffer and add its allocation to device residency."
  (ensure-live-metal-object device :create-buffer)
  (let ((size (buffer-descriptor-size descriptor)))
    (unless (and (typep size '(unsigned-byte 64)) (plusp size))
      (reject-metal-gpu-request descriptor :invalid-buffer-size size))
    (let* ((usage (normalize-metal-buffer-usage descriptor))
           (native-buffer
             (luv.metal:new-metal-buffer (metal-native-object device) size 0)))
      (unless native-buffer
        (error 'metal-gpu-error :operation :create-buffer
               :reason :buffer-creation-failed :details size))
      (let ((resident-p nil)
            (completed-p nil))
        (unwind-protect
             (let ((mapped (luv.metal:metal-buffer-contents native-buffer)))
               (when (cffi:null-pointer-p mapped)
                 (error 'metal-gpu-error :operation :create-buffer
                        :reason :buffer-not-cpu-visible))
               (luv.metal:add-metal-residency-allocation
                (metal-device-residency-set device) native-buffer)
               (setf resident-p t)
               (luv.metal:commit-metal-residency-set
                (metal-device-residency-set device))
               (let ((buffer
                       (make-instance
                        'metal-gpu-buffer
                        :label (gpu-descriptor-label descriptor)
                        :size size :usage usage :device device
                        :native-object native-buffer :mapped mapped)))
                 (setf completed-p t)
                 buffer))
          (unless completed-p
            (when resident-p
              (luv.metal:remove-metal-residency-allocation
               (metal-device-residency-set device) native-buffer)
              (luv.metal:commit-metal-residency-set
               (metal-device-residency-set device)))
            (luv.objective-c:release-objective-c-object native-buffer)))))))

(defmethod write-buffer
    ((buffer metal-gpu-buffer) data &key (offset 0))
  "Copy a one-dimensional single-float array into shared Metal memory."
  (ensure-live-metal-object buffer :write-buffer)
  (unless (and (arrayp data) (= 1 (array-rank data))
               (nth-value 0
                 (subtypep (array-element-type data) 'single-float)))
    (reject-metal-gpu-request buffer :unsupported-buffer-data data))
  (unless (and (typep offset '(unsigned-byte 64))
               (zerop (mod offset 4))
               (<= (+ offset (* 4 (length data)))
                   (gpu-buffer-size buffer)))
    (reject-metal-gpu-request
     buffer :buffer-write-out-of-bounds
     (list :offset offset :length (* 4 (length data)))))
  (let ((destination (cffi:inc-pointer (metal-buffer-mapped buffer) offset)))
    (dotimes (index (length data))
      (setf (cffi:mem-aref destination :float index) (aref data index))))
  buffer)

(defmethod read-buffer
    ((buffer metal-gpu-buffer) &key (offset 0) size)
  (ensure-live-metal-object buffer :read-buffer)
  (let ((size (or size (- (gpu-buffer-size buffer) offset))))
    (unless (and (typep offset '(unsigned-byte 64))
                 (typep size '(unsigned-byte 64))
                 (<= (+ offset size) (gpu-buffer-size buffer)))
      (reject-metal-gpu-request
       buffer :buffer-read-out-of-bounds (list :offset offset :size size)))
    (submitted-work-done (device-queue (metal-buffer-device buffer)))
    (let ((bytes (make-array size :element-type '(unsigned-byte 8)))
          (source (cffi:inc-pointer (metal-buffer-mapped buffer) offset)))
      (dotimes (index size bytes)
        (setf (aref bytes index) (cffi:mem-aref source :uint8 index))))))

(defmethod destroy-metal-native ((buffer metal-gpu-buffer))
  (let* ((device (metal-buffer-device buffer))
         (residency-set (metal-device-residency-set device)))
    (luv.metal:remove-metal-residency-allocation
     residency-set (metal-native-object buffer))
    (luv.metal:commit-metal-residency-set residency-set)
    (luv.objective-c:release-objective-c-object
     (metal-native-object buffer)))
  (values))

(defmethod destroy ((buffer metal-gpu-buffer))
  (unless (metal-object-destroyed-p buffer)
    (setf (metal-object-destroyed-p buffer) t)
    (metal-destroy-or-defer buffer (metal-buffer-device buffer)))
  (values))

(defun normalize-metal-texture-size (descriptor)
  (let ((size (texture-descriptor-size descriptor)))
    (unless (and (listp size) (= 2 (length size))
                 (every (lambda (value)
                          (typep value '(integer 1 *)))
                        size))
      (reject-metal-gpu-request descriptor :invalid-texture-size size))
    size))

(defun normalize-metal-texture-usage (descriptor)
  (let* ((raw (texture-descriptor-usage descriptor))
         (usage (typecase raw
                  (keyword (list raw))
                  (list (remove-duplicates raw))
                  (vector (remove-duplicates (coerce raw 'list)))
                  (otherwise nil))))
    (unless (and usage
                 (every (lambda (value)
                          (member value '(:render-attachment :texture-binding
                                          :copy-src :copy-dst)))
                        usage))
      (reject-metal-gpu-request descriptor :unsupported-texture-usage raw))
    usage))

(defun metal-resource-pixel-format (format descriptor)
  (case format
    (:rgba8-unorm luv.metal:+pixel-format-rgba8-unorm+)
    (:rgba8-unorm-srgb luv.metal:+pixel-format-rgba8-unorm-srgb+)
    (:bgra8-unorm luv.metal:+pixel-format-bgra8-unorm+)
    (:bgra8-unorm-srgb luv.metal:+pixel-format-bgra8-unorm-srgb+)
    (:rg16-uint luv.metal:+pixel-format-rg16-uint+)
    (:rgba16-float luv.metal:+pixel-format-rgba16-float+)
    (:depth32-float luv.metal:+pixel-format-depth32-float+)
    (otherwise
     (reject-metal-gpu-request descriptor :unsupported-texture-format format))))

(defun metal-native-texture-usage (usage)
  (logior (if (member :texture-binding usage)
              luv.metal:+texture-usage-shader-read+ 0)
          (if (member :render-attachment usage)
              luv.metal:+texture-usage-render-target+ 0)))

(defmethod create
    ((device metal-gpu-device) (descriptor texture-descriptor))
  "Create one resident two-dimensional Metal texture."
  (ensure-live-metal-object device :create-texture)
  (unless (eq :2d (texture-descriptor-dimensions descriptor))
    (reject-metal-gpu-request
     descriptor :unsupported-texture-dimensions
     (texture-descriptor-dimensions descriptor)))
  (let* ((size (normalize-metal-texture-size descriptor))
         (usage (normalize-metal-texture-usage descriptor))
         (native
           (luv.metal:new-metal-texture
            (metal-native-object device) (first size) (second size)
            (metal-resource-pixel-format
             (texture-descriptor-format descriptor) descriptor)
            (metal-native-texture-usage usage)
            :storage-mode
            (if (member :copy-dst usage)
                luv.metal:+storage-mode-shared+
                luv.metal:+storage-mode-private+)
            :label (gpu-descriptor-label descriptor))))
    (unless native
      (error 'metal-gpu-error :operation :create-texture
             :reason :texture-creation-failed :details descriptor))
    (let ((resident-p nil) (completed-p nil))
      (unwind-protect
           (progn
             (luv.metal:add-metal-residency-allocation
              (metal-device-residency-set device) native)
             (setf resident-p t)
             (luv.metal:commit-metal-residency-set
              (metal-device-residency-set device))
             (let ((texture
                     (make-instance
                      'metal-gpu-texture
                      :label (gpu-descriptor-label descriptor)
                      :device device :native-object native :owned-p t
                      :size size :usage usage :dimensions :2d
                      :format (texture-descriptor-format descriptor))))
               (setf completed-p t)
               texture))
        (unless completed-p
          (when resident-p
            (luv.metal:remove-metal-residency-allocation
             (metal-device-residency-set device) native)
            (luv.metal:commit-metal-residency-set
             (metal-device-residency-set device)))
          (luv.objective-c:release-objective-c-object native))))))

(defmethod create
    ((device metal-gpu-device) (descriptor texture-view-descriptor))
  (ensure-live-metal-object device :create-texture-view)
  (let ((texture (texture-view-descriptor-texture descriptor)))
    (unless (typep texture 'metal-gpu-texture)
      (reject-metal-gpu-request descriptor :incompatible-texture texture))
    (ensure-metal-object-device
     texture (metal-texture-device texture) device :create-texture-view)
    ;; The first Metal vocabulary exposes only complete single-mip views, so
    ;; the view is a semantic wrapper over the same native texture.
    (make-instance
     'metal-gpu-texture-view
     :label (gpu-descriptor-label descriptor)
     :device device :texture texture
     :native-object (metal-native-object texture))))

(defun metal-sampler-filter (filter descriptor)
  (declare (ignore descriptor))
  (ecase filter
    (:nearest luv.metal:+sampler-min-mag-filter-nearest+)
    (:linear luv.metal:+sampler-min-mag-filter-linear+)))

(defun metal-sampler-mip-filter (filter descriptor)
  (declare (ignore descriptor))
  (ecase filter
    (:nearest luv.metal:+sampler-mip-filter-nearest+)
    (:linear luv.metal:+sampler-mip-filter-linear+)))

(defun metal-sampler-address-mode (mode descriptor)
  (declare (ignore descriptor))
  (ecase mode
    (:clamp-to-edge luv.metal:+sampler-address-mode-clamp-to-edge+)
    (:repeat luv.metal:+sampler-address-mode-repeat+)))

(defmethod create
    ((device metal-gpu-device) (descriptor sampler-descriptor))
  (ensure-live-metal-object device :create-sampler)
  (let ((native
          (luv.metal:new-metal-sampler
           (metal-native-object device)
           (metal-sampler-filter
            (sampler-descriptor-min-filter descriptor) descriptor)
           (metal-sampler-filter
            (sampler-descriptor-mag-filter descriptor) descriptor)
           (metal-sampler-mip-filter
            (sampler-descriptor-mipmap-filter descriptor) descriptor)
           (metal-sampler-address-mode
            (sampler-descriptor-address-mode-u descriptor) descriptor)
           (metal-sampler-address-mode
            (sampler-descriptor-address-mode-v descriptor) descriptor)
           (metal-sampler-address-mode
            (sampler-descriptor-address-mode-w descriptor) descriptor)
           (metal-compare-function
            (or (sampler-descriptor-compare descriptor) :never))
           :label (gpu-descriptor-label descriptor))))
    (unless native
      (error 'metal-gpu-error :operation :create-sampler
             :reason :sampler-creation-failed :details descriptor))
    (make-instance
     'metal-gpu-sampler :label (gpu-descriptor-label descriptor)
     :device device :native-object native)))

(defun normalize-metal-bind-group-layout-entries (descriptor)
  (let* ((entries (bind-group-layout-descriptor-entries descriptor))
         (bindings (mapcar (lambda (entry) (getf entry :binding)) entries)))
    (unless (and (listp entries) entries
                 (every (lambda (entry)
                          (and (listp entry)
                               (typep (getf entry :binding)
                                      '(unsigned-byte 32))
                               (member (getf entry :type)
                                       '(:texture :sampler :uniform-buffer))))
                        entries)
                 (= (length bindings) (length (remove-duplicates bindings))))
      (reject-metal-gpu-request descriptor :unsupported-bind-group-layout
                                entries))
    entries))

(defmethod create
    ((device metal-gpu-device) (descriptor bind-group-layout-descriptor))
  (ensure-live-metal-object device :create-bind-group-layout)
  (make-instance
   'metal-gpu-bind-group-layout
   :label (gpu-descriptor-label descriptor) :device device
   :native-object nil
   :entries (normalize-metal-bind-group-layout-entries descriptor)))

(defun validate-metal-bind-group-entries (device descriptor layout)
  (let ((entries (bind-group-descriptor-entries descriptor))
        (layout-entries (metal-bind-group-layout-entries layout)))
    (unless (= (length entries) (length layout-entries))
      (reject-metal-gpu-request descriptor :incomplete-bind-group entries))
    (dolist (layout-entry layout-entries)
      (let* ((binding (getf layout-entry :binding))
             (entry (find binding entries
                          :key (lambda (candidate)
                                 (getf candidate :binding))))
             (resource (and entry (getf entry :resource))))
        (unless (and entry
                     (ecase (getf layout-entry :type)
                       (:texture (typep resource 'metal-gpu-texture-view))
                       (:sampler (typep resource 'metal-gpu-sampler))
                       (:uniform-buffer
                        (and (typep resource 'metal-gpu-buffer)
                             (member :uniform (gpu-buffer-usage resource))))))
          (reject-metal-gpu-request descriptor :invalid-bind-group-entry
                                    layout-entry))
        (ensure-metal-object-device
         resource
         (etypecase resource
           (metal-gpu-texture-view (metal-texture-view-device resource))
           (metal-gpu-sampler (metal-sampler-device resource))
           (metal-gpu-buffer (metal-buffer-device resource)))
         device :create-bind-group)))
    entries))

(defmethod create
    ((device metal-gpu-device) (descriptor bind-group-descriptor))
  (ensure-live-metal-object device :create-bind-group)
  (let ((layout (bind-group-descriptor-layout descriptor)))
    (unless (typep layout 'metal-gpu-bind-group-layout)
      (reject-metal-gpu-request descriptor :incompatible-bind-group-layout
                                layout))
    (ensure-metal-object-device
     layout (metal-bind-group-layout-device layout) device :create-bind-group)
    (make-instance
     'metal-gpu-bind-group
     :label (gpu-descriptor-label descriptor) :device device
     :layout layout :native-object nil
     :entries (validate-metal-bind-group-entries
               device descriptor layout))))

(defmethod enqueue
    ((queue metal-gpu-queue) (command gpu-write-texture-command))
  "Upload one tightly represented byte image into a shared Metal texture."
  (ensure-live-metal-object queue :write-texture)
  (let* ((copy (gpu-write-texture-command-destination command))
         (texture (texture-copy-texture copy))
         (layout (gpu-write-texture-command-data-layout command))
         (size (gpu-write-texture-command-size command))
         (data (gpu-write-texture-command-data command))
         (offset (texture-data-layout-offset layout))
         (bytes-per-row (texture-data-layout-bytes-per-row layout))
         (bytes-per-texel
           (and (typep texture 'metal-gpu-texture)
                (texture-format-bytes-per-texel
                 (gpu-texture-format texture))))
         (element-type
           (and (typep texture 'metal-gpu-texture)
                (texture-format-upload-element-type
                 (gpu-texture-format texture))))
         (foreign-type (case bytes-per-texel (4 :uint32) (8 :uint64))))
    (unless (and (typep texture 'metal-gpu-texture)
                 (eq (metal-texture-device texture)
                     (metal-queue-device queue))
                 (member :copy-dst (gpu-texture-usage texture))
                 (zerop (texture-copy-mip-level copy))
                 (equal '(0 0 0) (texture-copy-origin copy))
                 (equal size (gpu-texture-size texture))
                 (arrayp data) (= 2 (array-rank data))
                 (nth-value 0
                   (subtypep (array-element-type data) element-type))
                 (= (array-dimension data 0) (second size))
                 (= (array-dimension data 1) (first size))
                 (typep offset '(unsigned-byte 64))
                 (zerop (mod offset bytes-per-texel))
                 (typep bytes-per-row '(integer 1 *))
                 (>= bytes-per-row (* bytes-per-texel (first size)))
                 (zerop (mod bytes-per-row bytes-per-texel)))
      (reject-metal-gpu-request command :unsupported-texture-upload))
    (cffi:with-foreign-object
        (storage :uint8 (+ offset (* bytes-per-row (second size))))
      (dotimes (row (second size))
        (let ((destination
                (cffi:inc-pointer storage (+ offset (* row bytes-per-row)))))
          (dotimes (column (first size))
            (setf (cffi:mem-aref destination foreign-type column)
                  (row-major-aref
                   data (+ (* row (array-dimension data 1)) column))))))
      (luv.metal:replace-metal-texture-region
       (metal-native-object texture) (first size) (second size)
       (cffi:inc-pointer storage offset) bytes-per-row)))
  command)

(defmethod destroy-metal-native ((texture metal-gpu-texture))
  (when (metal-texture-owned-p texture)
    (let* ((device (metal-texture-device texture))
           (residency-set (metal-device-residency-set device)))
      (luv.metal:remove-metal-residency-allocation
       residency-set (metal-native-object texture))
      (luv.metal:commit-metal-residency-set residency-set)
      (luv.objective-c:release-objective-c-object
       (metal-native-object texture))))
  (values))

(defmethod destroy ((texture metal-gpu-texture))
  (unless (metal-object-destroyed-p texture)
    (setf (metal-object-destroyed-p texture) t)
    (metal-destroy-or-defer texture (metal-texture-device texture)))
  (values))

(defmethod destroy ((view metal-gpu-texture-view))
  (setf (metal-object-destroyed-p view) t)
  (values))

(defmethod destroy ((sampler metal-gpu-sampler))
  (unless (metal-object-destroyed-p sampler)
    (setf (metal-object-destroyed-p sampler) t)
    (metal-destroy-or-defer sampler (metal-sampler-device sampler)))
  (values))

(defmethod destroy-metal-native ((sampler metal-gpu-sampler))
  (luv.objective-c:release-objective-c-object
   (metal-native-object sampler))
  (values))

(defmethod destroy ((layout metal-gpu-bind-group-layout))
  (setf (metal-object-destroyed-p layout) t)
  (values))

(defmethod destroy ((bind-group metal-gpu-bind-group))
  (setf (metal-object-destroyed-p bind-group) t)
  (values))

(defun metal-document-for-shader-module (descriptor)
  (let ((code (shader-module-descriptor-code descriptor))
        (language (shader-module-descriptor-language descriptor)))
    (case language
      (:mathematical
       (unless (typep code 'luv.spir-v:shader-specification)
         (error 'gpu-request-error
                :operation :create-shader-module
                :descriptor descriptor
                :reason :invalid-mathematical-shader
                :details code))
       (luv.msl:compile-msl code))
      (:msl
       (unless (typep code 'luv.msl:msl-document)
         (error 'gpu-request-error
                :operation :create-shader-module
                :descriptor descriptor
                :reason :invalid-msl-document
                :details code))
       code)
      (otherwise
       (error 'gpu-request-error
              :operation :create-shader-module
              :descriptor descriptor
              :reason :unsupported-shader-language
              :details language)))))

(defun expected-metal-function-type (stage)
  (ecase stage
    (:vertex luv.metal:+function-type-vertex+)
    (:fragment luv.metal:+function-type-fragment+)
    (:task luv.metal:+function-type-object+)
    (:mesh luv.metal:+function-type-mesh+)))

(defmethod create
    ((device metal-gpu-device) (descriptor shader-module-descriptor))
  "Lower a mathematical shader directly to MSL and compile it on DEVICE.

The complete MSL document remains attached to the returned module so native
diagnostics and graph provenance stay inspectable.  This is the device-owned
compiler boundary of #58IDSR."
  (ensure-live-metal-object device :create-shader-module)
  (let* ((document (metal-document-for-shader-module descriptor))
         (source (luv.msl:msl-document-source document))
         (entry-point
           (luv.msl:msl-entry-point-name
            (luv.msl:msl-document-entry-point document)))
         (stage
           (luv.msl:msl-entry-point-stage
            (luv.msl:msl-document-entry-point document)))
         (expected-type (expected-metal-function-type stage)))
    (multiple-value-bind (library diagnostic)
        (luv.metal:compile-metal-4-library
         (metal-device-compiler device) source
         :name (or (gpu-descriptor-label descriptor) entry-point))
      (unless library
        (error 'metal-gpu-error :operation :create-shader-module
               :reason :library-compilation-failed
               :details (list :diagnostic diagnostic :document document)))
      (let ((completed-p nil))
        (unwind-protect
             (luv.objective-c:with-autorelease-pool ()
               (let ((function
                       (luv.metal:new-metal-library-function
                        library
                        (luv.objective-c:lisp-string-to-objective-c
                         entry-point))))
                 (unless function
                   (error 'metal-gpu-error :operation :create-shader-module
                          :reason :entry-point-not-found
                          :details (list :entry-point entry-point
                                         :document document)))
                 (unwind-protect
                      (let ((actual-type
                              (luv.metal:metal-function-type function)))
                        (unless (= actual-type expected-type)
                          (error 'metal-gpu-error
                                 :operation :create-shader-module
                                 :reason :entry-point-stage-mismatch
                                 :details (list :entry-point entry-point
                                                :expected expected-type
                                                :actual actual-type)))
                        (let ((module
                                (make-instance
                                 'metal-gpu-shader-module
                                 :label (gpu-descriptor-label descriptor)
                                 :native-object library :device device
                                 :document document :entry-point entry-point
                                 :function-type actual-type)))
                          (setf completed-p t)
                          module))
                   (luv.objective-c:release-objective-c-object function))))
          (unless completed-p
            (luv.objective-c:release-objective-c-object library)))))))

(defmethod destroy ((module metal-gpu-shader-module))
  (unless (metal-object-destroyed-p module)
    (setf (metal-object-destroyed-p module) t)
    (metal-destroy-or-defer module (metal-shader-module-device module)))
  (values))

(defmethod destroy-metal-native ((module metal-gpu-shader-module))
  (luv.objective-c:release-objective-c-object
   (metal-native-object module))
  (values))

(defun reject-metal-gpu-request (descriptor reason &optional details)
  (error 'gpu-request-error
         :operation :create :descriptor descriptor
         :reason reason :details details))

(defun ensure-metal-object-device
    (object actual-device expected-device operation)
  (ensure-live-metal-object object operation)
  (unless (eq actual-device expected-device)
    (error 'gpu-device-mismatch-error
           :object object :operation operation
           :expected-device expected-device :actual-device actual-device))
  object)

(defun normalize-metal-vertex-buffers (descriptor buffers)
  (unless (listp buffers)
    (reject-metal-gpu-request descriptor :invalid-vertex-buffers buffers))
  (loop for buffer in buffers
        for binding from 0
        for stride = (getf buffer :array-stride)
        for step-mode = (or (getf buffer :step-mode) :vertex)
        for attributes = (getf buffer :attributes)
        unless (and (typep stride '(unsigned-byte 32)) (plusp stride)
                    (member step-mode '(:vertex :instance))
                    (listp attributes) attributes
                    (every (lambda (attribute)
                             (and (typep (getf attribute :shader-location)
                                         '(unsigned-byte 32))
                                  (typep (getf attribute :offset)
                                         '(unsigned-byte 32))
                                  (eq :float32x3
                                      (getf attribute :format))))
                           attributes))
          do (reject-metal-gpu-request
              descriptor :invalid-vertex-buffer buffer)
        collect
        (list :binding binding :array-stride stride :step-mode step-mode
              :attributes attributes)))

(defun metal-render-pipeline-pixel-format (format descriptor)
  (and format (metal-resource-pixel-format format descriptor)))

(defun metal-compare-function (function)
  (ecase function
    (:never luv.metal:+compare-function-never+)
    (:less luv.metal:+compare-function-less+)
    (:equal luv.metal:+compare-function-equal+)
    (:less-or-equal luv.metal:+compare-function-less-equal+)
    (:greater luv.metal:+compare-function-greater+)
    (:not-equal luv.metal:+compare-function-not-equal+)
    (:greater-or-equal luv.metal:+compare-function-greater-equal+)
    (:always luv.metal:+compare-function-always+)))

(defmethod create
    ((device metal-gpu-device) (descriptor render-pipeline-descriptor))
  "Link device-owned vertex and fragment modules into a Metal 4 pipeline."
  (ensure-live-metal-object device :create-render-pipeline)
  (let* ((layout (render-pipeline-descriptor-layout descriptor))
         (vertex (render-pipeline-descriptor-vertex descriptor))
         (fragment (render-pipeline-descriptor-fragment descriptor))
         (vertex-module (getf vertex :module))
         (fragment-module (getf fragment :module))
         (vertex-entry-point
           (and vertex-module
                (or (getf vertex :entry-point)
                    (metal-shader-module-entry-point vertex-module))))
         (fragment-entry-point
           (and fragment-module
                (or (getf fragment :entry-point)
                    (metal-shader-module-entry-point fragment-module))))
         (vertex-buffers
           (normalize-metal-vertex-buffers
            descriptor (or (getf vertex :buffers) nil)))
         (targets (getf fragment :targets))
         (format (and (= (length targets) 1)
                      (getf (first targets) :format)))
         (blend (getf (first targets) :blend))
         (primitive (render-pipeline-descriptor-primitive descriptor))
         (topology (or (getf primitive :topology) :triangle-list))
         (depth-stencil
           (render-pipeline-descriptor-depth-stencil descriptor))
         (depth-format (and depth-stencil (getf depth-stencil :format)))
         (depth-compare
           (and depth-stencil (getf depth-stencil :depth-compare)))
         (depth-write-enabled
           (and depth-stencil (getf depth-stencil :depth-write-enabled))))
    (unless (and (or (null layout)
                     (typep layout 'metal-gpu-bind-group-layout))
                 (typep vertex-module 'metal-gpu-shader-module)
                 (= (metal-shader-module-function-type vertex-module)
                    luv.metal:+function-type-vertex+)
                 (string= vertex-entry-point
                          (metal-shader-module-entry-point vertex-module))
                 (or (and (typep fragment-module 'metal-gpu-shader-module)
                          (= (metal-shader-module-function-type fragment-module)
                             luv.metal:+function-type-fragment+)
                          (string= fragment-entry-point
                                   (metal-shader-module-entry-point
                                    fragment-module))
                          format
                          (member blend '(nil :premultiplied-alpha)))
                     (and (null fragment-module) (null format) depth-stencil))
                 (member topology '(:triangle-list :triangle-strip))
                 (or (null depth-stencil)
                     (and (eq depth-format :depth32-float)
                          (member depth-compare
                                  '(:never :less :equal :less-or-equal
                                    :greater :not-equal :greater-or-equal
                                    :always)))))
      (reject-metal-gpu-request
       descriptor :unsupported-metal-render-pipeline
       (list :layout layout :topology topology :depth-stencil depth-stencil)))
    (ensure-metal-object-device
     vertex-module (metal-shader-module-device vertex-module) device
     :create-render-pipeline)
    (when layout
      (ensure-metal-object-device
       layout (metal-bind-group-layout-device layout) device
       :create-render-pipeline))
    (when fragment-module
      (ensure-metal-object-device
       fragment-module (metal-shader-module-device fragment-module) device
       :create-render-pipeline))
    (let ((pipeline-state nil)
          (depth-state nil)
          (completed-p nil))
      (unwind-protect
           (progn
             (multiple-value-bind (pipeline diagnostic)
                 (luv.metal:compile-metal-4-render-pipeline
                  (metal-device-compiler device)
                  (metal-native-object vertex-module) vertex-entry-point
                  (and fragment-module (metal-native-object fragment-module))
                  fragment-entry-point
                  vertex-buffers
                  (metal-render-pipeline-pixel-format format descriptor)
                  luv.metal:+primitive-topology-class-triangle+
                  :depth-format
                  (and depth-format
                       (metal-resource-pixel-format depth-format descriptor))
                  :blend blend
                  :label (gpu-descriptor-label descriptor))
               (unless pipeline
                 (error 'metal-gpu-error
                        :operation :create-render-pipeline
                        :reason :pipeline-compilation-failed
                        :details diagnostic))
               (setf pipeline-state pipeline))
             (when depth-stencil
               (setf depth-state
                     (luv.metal:new-metal-depth-stencil-state
                      (metal-native-object device)
                      (metal-compare-function depth-compare)
                      depth-write-enabled
                      :label (and (gpu-descriptor-label descriptor)
                                  (format nil "~A depth state"
                                          (gpu-descriptor-label descriptor)))))
               (unless depth-state
                 (error 'metal-gpu-error
                        :operation :create-render-pipeline
                        :reason :depth-state-creation-failed)))
             (let ((pipeline
                     (make-instance
                      'metal-gpu-render-pipeline
                      :label (gpu-descriptor-label descriptor)
                      :native-object pipeline-state :device device
                      :layout layout :vertex-buffers vertex-buffers
                      :primitive-topology topology
                      :fragment-p (not (null fragment-module))
                      :depth-format depth-format
                      :depth-stencil-state depth-state)))
               (setf completed-p t)
               pipeline))
        (unless completed-p
          (when depth-state
            (luv.objective-c:release-objective-c-object depth-state))
          (when pipeline-state
            (luv.objective-c:release-objective-c-object pipeline-state)))))))

(defun metal-shader-module-workgroup-size (module)
  (luv.spir-v:shader-specification-workgroup-size
   (luv.msl:msl-document-specification
    (metal-shader-module-document module))))

(defmethod create
    ((device metal-gpu-device) (descriptor mesh-render-pipeline-descriptor))
  "Link task, mesh, and fragment modules into a Metal 4 mesh pipeline."
  (ensure-live-metal-object device :create-mesh-render-pipeline)
  (let* ((layout (mesh-render-pipeline-descriptor-layout descriptor))
         (task (mesh-render-pipeline-descriptor-task descriptor))
         (mesh (mesh-render-pipeline-descriptor-mesh descriptor))
         (fragment (mesh-render-pipeline-descriptor-fragment descriptor))
         (task-module (getf task :module))
         (mesh-module (getf mesh :module))
         (fragment-module (getf fragment :module))
         (task-entry-point
           (and task-module
                (or (getf task :entry-point)
                    (metal-shader-module-entry-point task-module))))
         (mesh-entry-point
           (and mesh-module
                (or (getf mesh :entry-point)
                    (metal-shader-module-entry-point mesh-module))))
         (fragment-entry-point
           (and fragment-module
                (or (getf fragment :entry-point)
                    (metal-shader-module-entry-point fragment-module))))
         (targets (getf fragment :targets))
         (format (and (= (length targets) 1)
                      (getf (first targets) :format)))
         (blend (getf (first targets) :blend))
         (max-mesh-workgroups
           (mesh-render-pipeline-descriptor-max-mesh-workgroups descriptor))
         (depth-stencil
           (mesh-render-pipeline-descriptor-depth-stencil descriptor)))
    (unless (and (or (null layout)
                     (typep layout 'metal-gpu-bind-group-layout))
                 (or (null task-module)
                     (and (typep task-module 'metal-gpu-shader-module)
                          (= (metal-shader-module-function-type task-module)
                             luv.metal:+function-type-object+)
                          (string= task-entry-point
                                   (metal-shader-module-entry-point
                                    task-module))))
                 (typep mesh-module 'metal-gpu-shader-module)
                 (= (metal-shader-module-function-type mesh-module)
                    luv.metal:+function-type-mesh+)
                 (string= mesh-entry-point
                          (metal-shader-module-entry-point mesh-module))
                 (typep fragment-module 'metal-gpu-shader-module)
                 (= (metal-shader-module-function-type fragment-module)
                    luv.metal:+function-type-fragment+)
                 (string= fragment-entry-point
                          (metal-shader-module-entry-point fragment-module))
                 format
                 (member blend '(nil :premultiplied-alpha))
                 (typep max-mesh-workgroups '(integer 1 #.most-positive-fixnum))
                 (null depth-stencil))
      (reject-metal-gpu-request
       descriptor :unsupported-metal-mesh-render-pipeline
       (list :layout layout :depth-stencil depth-stencil
             :max-mesh-workgroups max-mesh-workgroups)))
    (when layout
      (ensure-metal-object-device
       layout (metal-bind-group-layout-device layout) device
       :create-mesh-render-pipeline))
    (dolist (module (remove nil
                            (list task-module mesh-module fragment-module)))
      (ensure-metal-object-device
       module (metal-shader-module-device module) device
       :create-mesh-render-pipeline))
    (let* ((task-workgroup-size
             (and task-module
                  (metal-shader-module-workgroup-size task-module)))
           (mesh-workgroup-size
             (metal-shader-module-workgroup-size mesh-module))
           (pipeline-state nil)
           (completed-p nil))
      (unwind-protect
           (progn
             (multiple-value-bind (pipeline diagnostic)
                 (luv.metal:compile-metal-4-mesh-render-pipeline
                  (metal-device-compiler device)
                  (and task-module (metal-native-object task-module))
                  task-entry-point task-workgroup-size
                  (metal-native-object mesh-module) mesh-entry-point
                  mesh-workgroup-size
                  (metal-native-object fragment-module) fragment-entry-point
                  (metal-render-pipeline-pixel-format format descriptor)
                  max-mesh-workgroups
                  :blend blend :label (gpu-descriptor-label descriptor))
               (unless pipeline
                 (error 'metal-gpu-error
                        :operation :create-mesh-render-pipeline
                        :reason :pipeline-compilation-failed
                        :details diagnostic))
               (setf pipeline-state pipeline))
             (let ((pipeline
                     (make-instance
                      'metal-gpu-mesh-render-pipeline
                      :label (gpu-descriptor-label descriptor)
                      :native-object pipeline-state :device device :layout layout
                      :vertex-buffers nil :primitive-topology :triangle-list
                      :fragment-p t :depth-format nil :depth-stencil-state nil
                      :task-workgroup-size task-workgroup-size
                      :mesh-workgroup-size mesh-workgroup-size)))
               (setf completed-p t)
               pipeline))
        (unless completed-p
          (when pipeline-state
            (luv.objective-c:release-objective-c-object pipeline-state)))))))

(defmethod destroy ((pipeline metal-gpu-render-pipeline))
  (unless (metal-object-destroyed-p pipeline)
    (setf (metal-object-destroyed-p pipeline) t)
    (metal-destroy-or-defer pipeline (metal-render-pipeline-device pipeline)))
  (values))

(defmethod destroy-metal-native ((pipeline metal-gpu-render-pipeline))
  (let ((depth-state (metal-render-pipeline-depth-stencil-state pipeline)))
    (when depth-state
      (luv.objective-c:release-objective-c-object depth-state)))
  (luv.objective-c:release-objective-c-object
   (metal-native-object pipeline))
  (values))

(defun probe-metal-shader-library (specification)
  "Compile SPECIFICATION through a fresh Metal device and return bounded evidence."
  (let ((device nil)
        (module nil)
        (evidence nil))
    (unwind-protect
         (progn
           (setf device (request-gpu-device (make-instance 'metal-gpu-provider))
                 module
                 (create
                  device
                  (make-shader-module-descriptor
                   :label "luvcraft MSL probe"
                   :language :mathematical :code specification))
                 evidence
                 (list
                  :device
                  (luv.objective-c:objective-c-string
                   (luv.metal:device-name (metal-native-object device)))
                  :compiler
                  (luv.objective-c:objective-c-object-protocol-name
                   (metal-device-compiler device))
                  :library
                  (luv.objective-c:objective-c-object-protocol-name
                   (metal-native-object module))
                  :stage
                  (luv.msl:msl-entry-point-stage
                   (luv.msl:msl-document-entry-point
                    (metal-shader-module-document module)))
                  :entry-point (metal-shader-module-entry-point module)
                  :source-length
                  (length
                   (luv.msl:msl-document-source
                    (metal-shader-module-document module)))))
           evidence)
      (when module (destroy module))
      (when device (destroy device)))))

(defun probe-metal-render-pipeline
    (vertex-specification fragment-specification vertex-buffers
     &key (target-format :bgra8-unorm)
       (primitive '(:topology :triangle-list))
       (depth-stencil '(:format :depth32-float
                        :depth-write-enabled t
                        :depth-compare :less)))
  "Link two mathematical shaders on a fresh Metal device and return evidence."
  (let ((device nil)
        (vertex-module nil)
        (fragment-module nil)
        (pipeline nil))
    (unwind-protect
         (progn
           (setf device
                 (request-gpu-device (make-instance 'metal-gpu-provider))
                 vertex-module
                 (create
                  device
                  (make-shader-module-descriptor
                   :label "Metal pipeline probe vertex"
                   :language :mathematical :code vertex-specification))
                 fragment-module
                 (create
                  device
                  (make-shader-module-descriptor
                   :label "Metal pipeline probe fragment"
                   :language :mathematical :code fragment-specification))
                 pipeline
                 (create
                  device
                  (make-render-pipeline-descriptor
                   :label "Metal 4 render pipeline probe"
                   :layout nil
                   :vertex `(:module ,vertex-module
                             :buffers ,vertex-buffers)
                   :fragment `(:module ,fragment-module
                               :targets ((:format ,target-format)))
                   :primitive primitive :depth-stencil depth-stencil)))
           (list
            :device
            (luv.objective-c:objective-c-string
             (luv.metal:device-name (metal-native-object device)))
            :vertex-entry-point
            (metal-shader-module-entry-point vertex-module)
            :fragment-entry-point
            (metal-shader-module-entry-point fragment-module)
            :pipeline
            (luv.objective-c:objective-c-object-protocol-name
             (metal-native-object pipeline))
            :depth-state
            (and (metal-render-pipeline-depth-stencil-state pipeline)
                 (luv.objective-c:objective-c-object-protocol-name
                  (metal-render-pipeline-depth-stencil-state pipeline)))
            :vertex-buffers
            (metal-render-pipeline-vertex-buffers pipeline)))
      (when pipeline (destroy pipeline))
      (when fragment-module (destroy fragment-module))
      (when vertex-module (destroy vertex-module))
      (when device (destroy device)))))

(defun ensure-metal-render-pass-state (pass operation)
  (unless (eq :encoding (metal-render-pass-state pass))
    (error 'gpu-invalid-state-error :object pass :operation operation
           :state (metal-render-pass-state pass) :expected-state :encoding))
  pass)

(defun metal-attachment-texture (view)
  (etypecase view
    (metal-gpu-texture view)
    (metal-gpu-texture-view (gpu-texture-view-texture view))))

(defun normalize-metal-color-attachment (device descriptor attachment)
  (when attachment
    (let* ((view (getf attachment :view))
           (texture (and (typep view '(or metal-gpu-texture
                                          metal-gpu-texture-view))
                         (metal-attachment-texture view)))
           (load-op (or (getf attachment :load-op) :clear))
           (store-op (or (getf attachment :store-op) :store))
           (clear-value
             (or (getf attachment :clear-value) #(0.0 0.0 0.0 1.0))))
      (unless (and texture
                   (member :render-attachment (gpu-texture-usage texture))
                   (member load-op '(:clear :load))
                   (member store-op '(:store :discard))
                   (= 4 (length clear-value))
                   (every #'realp clear-value))
        (reject-metal-gpu-request
         descriptor :unsupported-metal-color-attachment attachment))
      (ensure-metal-object-device
       texture (metal-texture-device texture) device :begin-render-pass)
      (list texture load-op store-op clear-value))))

(defun normalize-metal-depth-attachment (device descriptor attachment)
  (when attachment
    (let* ((view (getf attachment :view))
           (texture (and (typep view '(or metal-gpu-texture
                                          metal-gpu-texture-view))
                         (metal-attachment-texture view)))
           (load-op (or (getf attachment :depth-load-op) :clear))
           (store-op (or (getf attachment :depth-store-op) :discard))
           (clear-depth (or (getf attachment :depth-clear-value) 1.0)))
      (unless (and texture
                   (eq :depth32-float (gpu-texture-format texture))
                   (member :render-attachment (gpu-texture-usage texture))
                   (member load-op '(:clear :load))
                   (member store-op '(:store :discard))
                   (realp clear-depth) (<= 0 clear-depth 1))
        (reject-metal-gpu-request
         descriptor :unsupported-metal-depth-attachment attachment))
      (ensure-metal-object-device
       texture (metal-texture-device texture) device :begin-render-pass)
      (list texture load-op store-op clear-depth))))

(defmethod begin-render-pass
    ((encoder metal-gpu-command-encoder) descriptor)
  "Begin a Metal 4 color, depth, or color-and-depth pass."
  (ensure-metal-command-encoder-state encoder :begin-render-pass)
  (ensure-no-active-metal-pass encoder :begin-render-pass)
  (let* ((attachments (render-pass-descriptor-color-attachments descriptor))
         (depth-attachment
           (render-pass-descriptor-depth-stencil-attachment descriptor))
         (device (metal-command-encoder-device encoder)))
    (unless (and (listp attachments) (<= (length attachments) 1)
                 (or attachments depth-attachment))
      (reject-metal-gpu-request
       descriptor :unsupported-metal-render-pass
       (list :color-attachments attachments
             :depth-stencil depth-attachment)))
    (let* ((color (normalize-metal-color-attachment
                   device descriptor (first attachments)))
           (depth (normalize-metal-depth-attachment
                   device descriptor depth-attachment)))
      (when color
        (retain-metal-resource encoder (first color))
        (let ((view (getf (first attachments) :view)))
          (when (typep view 'metal-gpu-texture-view)
            (retain-metal-resource encoder view))))
      (when depth
        (retain-metal-resource encoder (first depth))
        (let ((view (getf depth-attachment :view)))
          (when (typep view 'metal-gpu-texture-view)
            (retain-metal-resource encoder view))))
      (when (and color depth
                 (not (equal (gpu-texture-size (first color))
                             (gpu-texture-size (first depth)))))
        (reject-metal-gpu-request descriptor :mismatched-depth-size
                                  (gpu-texture-size (first depth))))
      (let ((native-encoder
              (luv.metal:new-render-command-encoder
               (metal-encoder-command-buffer encoder)
               :color-texture
               (and color (metal-native-object (first color)))
               :color (and color (fourth color))
               :color-clear-p (and color (eq :clear (second color)))
               :color-store-p (and color (eq :store (third color)))
               :depth-texture
               (and depth (metal-native-object (first depth)))
               :clear-depth (and depth (fourth depth))
               :depth-clear-p (and depth (eq :clear (second depth)))
               :depth-store-p (and depth (eq :store (third depth))))))
        (unless native-encoder
          (error 'metal-gpu-error :operation :begin-render-pass
                 :reason :render-encoder-creation-failed))
        (let ((barrier (metal-encoder-pending-consumer-barrier encoder)))
          (when barrier
            (destructuring-bind
                (after-queue-stages before-stages visibility-options)
                barrier
              (luv.metal:barrier-after-queue-stages
               native-encoder after-queue-stages before-stages
               visibility-options))
            (setf (metal-encoder-pending-consumer-barrier encoder) nil)))
        (let ((pass
                (make-instance
                 'metal-render-pass-encoder
                 :owner encoder :native-encoder native-encoder
                 :label (gpu-descriptor-label descriptor))))
          (setf (metal-encoder-active-pass encoder) pass)
          pass)))))

(defmethod encode
    ((encoder metal-gpu-command-encoder)
     (command gpu-prepare-texture-command))
  (ensure-metal-command-encoder-state encoder :prepare-texture)
  (ensure-no-active-metal-pass encoder :prepare-texture)
  (let ((texture (gpu-prepare-texture-command-texture command))
        (usage (gpu-prepare-texture-command-usage command))
        (device (metal-command-encoder-device encoder)))
    (unless (and (typep texture 'metal-gpu-texture)
                 (member usage (gpu-texture-usage texture))
                 (eq usage :texture-binding))
      (reject-metal-gpu-request
       command :unsupported-texture-preparation
       (list :texture texture :usage usage)))
    (ensure-live-metal-object texture :prepare-texture)
    (ensure-metal-object-device
     texture (metal-texture-device texture) device :prepare-texture)
    ;; Metal 4 queues do not perform ordinary MTLResource hazard tracking.
    ;; Coalesce every texture produced by the preceding pass into the one
    ;; producer-to-fragment barrier installed on the following encoder.  The
    ;; command encoder retains each concrete resource independently below.
    (unless (metal-encoder-pending-consumer-barrier encoder)
      (setf (metal-encoder-pending-consumer-barrier encoder)
            (list luv.metal:+stage-fragment+
                  luv.metal:+stage-fragment+
                  luv.metal:+visibility-device+)))
    (retain-metal-resource encoder texture))
  encoder)

(defun release-metal-render-pass-argument-table (pass)
  (let ((table (metal-render-pass-argument-table pass)))
    (when table
      ;; MTL4RenderCommandEncoder snapshots table contents at each draw.
      (luv.objective-c:release-objective-c-object table)
      (setf (metal-render-pass-argument-table pass) nil))))

(defun metal-layout-binding-count (layout type)
  (let ((bindings
          (loop for entry in (and layout
                                  (metal-bind-group-layout-entries layout))
                when (eq type (getf entry :type))
                  collect (getf entry :binding))))
    (if bindings (1+ (reduce #'max bindings)) 0)))

(defun configure-metal-pass-bind-group (pass bind-group)
  (let* ((pipeline (metal-render-pass-pipeline pass))
         (layout (and pipeline (metal-render-pipeline-layout pipeline)))
         (table (metal-render-pass-argument-table pass))
         (owner (metal-render-pass-owner pass)))
    (unless pipeline
      (error 'gpu-invalid-state-error :object pass :operation :set-bind-group
             :state :no-pipeline :expected-state :pipeline-bound))
    (unless (and (typep bind-group 'metal-gpu-bind-group)
                 (eq layout (metal-bind-group-layout bind-group)))
      (reject-metal-gpu-request bind-group :incompatible-pipeline-layout
                                pipeline))
    (dolist (layout-entry (metal-bind-group-layout-entries layout))
      (let* ((binding (getf layout-entry :binding))
             (entry (find binding (metal-bind-group-entries bind-group)
                          :key (lambda (candidate)
                                 (getf candidate :binding))))
             (resource (getf entry :resource)))
        (retain-metal-resource owner resource)
        (when (typep resource 'metal-gpu-texture-view)
          (retain-metal-resource owner (gpu-texture-view-texture resource)))
        (ecase (getf layout-entry :type)
          (:uniform-buffer
           (luv.metal:set-metal-argument-table-address
            table
            (luv.metal:metal-buffer-gpu-address
             (metal-native-object resource))
            binding))
          (:texture
           (luv.metal:set-metal-argument-table-texture
            table
            (luv.metal:metal-texture-resource-id
             (metal-native-object
              (gpu-texture-view-texture resource)))
            binding))
          (:sampler
           (luv.metal:set-metal-argument-table-sampler
            table
            (luv.metal:metal-sampler-resource-id
             (metal-native-object resource))
            binding))))))
  (retain-metal-resource (metal-render-pass-owner pass) bind-group)
  (setf (metal-render-pass-bind-group pass) bind-group)
  bind-group)

(defmethod encode
    ((pass metal-render-pass-encoder) (command gpu-set-pipeline-command))
  (ensure-metal-render-pass-state pass :set-pipeline)
  (let* ((pipeline (gpu-set-pipeline-command-pipeline command))
         (owner (metal-render-pass-owner pass))
         (device (metal-command-encoder-device owner)))
    (unless (typep pipeline 'metal-gpu-render-pipeline)
      (reject-metal-gpu-request command :incompatible-pipeline pipeline))
    (ensure-metal-object-device
     pipeline (metal-render-pipeline-device pipeline) device :set-pipeline)
    (retain-metal-resource owner pipeline)
    (release-metal-render-pass-argument-table pass)
    (clrhash (metal-render-pass-vertex-bindings pass))
    (let ((vertex-buffers (metal-render-pipeline-vertex-buffers pipeline)))
      (let* ((layout (metal-render-pipeline-layout pipeline))
             (buffer-count
               (max (if vertex-buffers
                        (1+ (reduce #'max vertex-buffers
                                    :key (lambda (buffer)
                                           (getf buffer :binding))))
                        0)
                    (metal-layout-binding-count layout :uniform-buffer)))
             (texture-count (metal-layout-binding-count layout :texture))
             (sampler-count (metal-layout-binding-count layout :sampler)))
        (when (plusp (+ buffer-count texture-count sampler-count))
          (multiple-value-bind (table diagnostic)
              (luv.metal:new-metal-4-argument-table
               (metal-native-object device)
               buffer-count
               :max-texture-count texture-count
               :max-sampler-count sampler-count
               :label (format nil "~A render arguments"
                              (or (gpu-object-label pipeline) "Metal pipeline"))
               :attribute-strides-p t)
            (unless table
              (error 'metal-gpu-error :operation :set-pipeline
                     :reason :argument-table-creation-failed
                     :details diagnostic))
            (setf (metal-render-pass-argument-table pass) table)))))
    (luv.metal:set-metal-render-pipeline
     (metal-render-pass-native-encoder pass) (metal-native-object pipeline))
    (luv.metal:set-metal-depth-stencil-state
     (metal-render-pass-native-encoder pass)
     (metal-render-pipeline-depth-stencil-state pipeline))
    (setf (metal-render-pass-pipeline pass) pipeline)
    (let ((bind-group (metal-render-pass-bind-group pass)))
      (when bind-group
        (if (eq (metal-render-pipeline-layout pipeline)
                (metal-bind-group-layout bind-group))
            (configure-metal-pass-bind-group pass bind-group)
            (setf (metal-render-pass-bind-group pass) nil))))
    command))

(defmethod encode
    ((pass metal-render-pass-encoder)
     (command gpu-set-bind-group-command))
  (ensure-metal-render-pass-state pass :set-bind-group)
  (unless (zerop (gpu-set-bind-group-command-index command))
    (reject-metal-gpu-request command :unsupported-bind-group-index
                              (gpu-set-bind-group-command-index command)))
  (let ((pipeline (metal-render-pass-pipeline pass))
        (bind-group (gpu-set-bind-group-command-bind-group command)))
    (unless pipeline
      (error 'gpu-invalid-state-error :object pass :operation :set-bind-group
             :state :no-pipeline :expected-state :pipeline-bound))
    (unless (typep bind-group 'metal-gpu-bind-group)
      (reject-metal-gpu-request command :incompatible-bind-group bind-group))
    (ensure-metal-object-device
     bind-group (metal-bind-group-device bind-group)
     (metal-render-pipeline-device pipeline)
     :set-bind-group)
    (configure-metal-pass-bind-group pass bind-group))
  command)

(defun metal-pipeline-vertex-buffer-at (pipeline slot)
  (find slot (metal-render-pipeline-vertex-buffers pipeline)
        :key (lambda (buffer) (getf buffer :binding))))

(defmethod encode
    ((pass metal-render-pass-encoder)
     (command gpu-set-vertex-buffer-command))
  (ensure-metal-render-pass-state pass :set-vertex-buffer)
  (let* ((pipeline (metal-render-pass-pipeline pass))
         (slot (gpu-set-vertex-buffer-command-slot command))
         (buffer (gpu-set-vertex-buffer-command-buffer command))
         (offset (gpu-set-vertex-buffer-command-offset command))
         (layout (and pipeline
                      (metal-pipeline-vertex-buffer-at pipeline slot))))
    (unless pipeline
      (error 'gpu-invalid-state-error :object pass :operation :set-vertex-buffer
             :state :no-pipeline :expected-state :pipeline-bound))
    (unless layout
      (reject-metal-gpu-request command :unsupported-vertex-buffer-slot slot))
    (unless (typep buffer 'metal-gpu-buffer)
      (reject-metal-gpu-request command :incompatible-vertex-buffer buffer))
    (ensure-metal-object-device
     buffer (metal-buffer-device buffer)
     (metal-render-pipeline-device pipeline) :set-vertex-buffer)
    (unless (member :vertex (gpu-buffer-usage buffer))
      (reject-metal-gpu-request command :buffer-missing-vertex-usage buffer))
    (unless (and (typep offset '(unsigned-byte 64))
                 (zerop (mod offset 4))
                 (< offset (gpu-buffer-size buffer)))
      (reject-metal-gpu-request command :invalid-vertex-buffer-offset offset))
    (luv.metal:set-metal-argument-table-buffer
     (metal-render-pass-argument-table pass)
     (+ (luv.metal:metal-buffer-gpu-address (metal-native-object buffer))
        offset)
     (getf layout :array-stride) slot)
    (retain-metal-resource (metal-render-pass-owner pass) buffer)
    (setf (gethash slot (metal-render-pass-vertex-bindings pass)) buffer)
    command))

(defmethod encode
    ((pass metal-render-pass-encoder)
     (command gpu-set-scissor-command))
  (ensure-metal-render-pass-state pass :set-scissor)
  (let ((values
          (list (gpu-set-scissor-command-x command)
                (gpu-set-scissor-command-y command)
                (gpu-set-scissor-command-width command)
                (gpu-set-scissor-command-height command))))
    (unless (every (lambda (value) (typep value '(unsigned-byte 64))) values)
      (reject-metal-gpu-request command :invalid-scissor-rectangle values))
    (destructuring-bind (x y width height) values
      (luv.metal:set-metal-scissor-rect
       (metal-render-pass-native-encoder pass)
       (list 'luv.metal::x x 'luv.metal::y y
             'luv.metal::width width 'luv.metal::height height))))
  command)

(defun metal-primitive-type (pipeline)
  (ecase (metal-render-pipeline-primitive-topology pipeline)
    (:triangle-list luv.metal:+primitive-type-triangle+)
    (:triangle-strip luv.metal:+primitive-type-triangle-strip+)))

(defmethod encode
    ((pass metal-render-pass-encoder) (command gpu-draw-command))
  (ensure-metal-render-pass-state pass :draw)
  (let ((pipeline (metal-render-pass-pipeline pass)))
    (unless pipeline
      (error 'gpu-invalid-state-error :object pass :operation :draw
             :state :no-pipeline :expected-state :pipeline-bound))
    (when (typep pipeline 'metal-gpu-mesh-render-pipeline)
      (reject-metal-gpu-request command :vertex-draw-with-mesh-pipeline))
    (when (and (metal-render-pipeline-layout pipeline)
               (null (metal-render-pass-bind-group pass)))
      (error 'gpu-invalid-state-error :object pass :operation :draw
             :state :bind-group-missing
             :expected-state :pipeline-bind-group-and-vertex-buffers-bound))
    (dolist (layout (metal-render-pipeline-vertex-buffers pipeline))
      (unless (gethash (getf layout :binding)
                       (metal-render-pass-vertex-bindings pass))
        (error 'gpu-invalid-state-error :object pass :operation :draw
               :state :vertex-buffer-missing
               :expected-state :all-vertex-buffers-bound)))
    (let ((vertex-count (gpu-draw-command-vertex-count command))
          (instance-count (gpu-draw-command-instance-count command))
          (first-vertex (gpu-draw-command-first-vertex command))
          (first-instance (gpu-draw-command-first-instance command)))
      (unless (and (typep vertex-count '(integer 1 *))
                   (typep instance-count '(integer 1 *))
                   (typep first-vertex '(unsigned-byte 64))
                   (typep first-instance '(unsigned-byte 64)))
        (reject-metal-gpu-request command :invalid-draw-range))
      (when (metal-render-pass-argument-table pass)
        (luv.metal:set-metal-render-argument-table
         (metal-render-pass-native-encoder pass)
         (metal-render-pass-argument-table pass)
         (logior luv.metal:+render-stage-vertex+
                 (if (metal-render-pipeline-fragment-p pipeline)
                     luv.metal:+render-stage-fragment+
                     0))))
      (luv.metal:draw-metal-primitives
       (metal-render-pass-native-encoder pass)
       (metal-primitive-type pipeline) first-vertex vertex-count
       instance-count first-instance)
      command)))

(defun metal-size-value (size)
  (destructuring-bind (width height depth) size
    (list 'luv.metal::width width
          'luv.metal::height height
          'luv.metal::depth depth)))

(defmethod encode
    ((pass metal-render-pass-encoder) (command gpu-draw-mesh-command))
  (ensure-metal-render-pass-state pass :draw-mesh)
  (let ((pipeline (metal-render-pass-pipeline pass)))
    (unless pipeline
      (error 'gpu-invalid-state-error :object pass :operation :draw-mesh
             :state :no-pipeline :expected-state :pipeline-bound))
    (unless (typep pipeline 'metal-gpu-mesh-render-pipeline)
      (reject-metal-gpu-request command :mesh-draw-with-vertex-pipeline))
    (when (and (metal-render-pipeline-layout pipeline)
               (null (metal-render-pass-bind-group pass)))
      (error 'gpu-invalid-state-error :object pass :operation :draw-mesh
             :state :bind-group-missing
             :expected-state :pipeline-and-bind-group-bound))
    (let ((counts (list (gpu-draw-mesh-command-x command)
                        (gpu-draw-mesh-command-y command)
                        (gpu-draw-mesh-command-z command))))
      (unless (every (lambda (value) (typep value '(integer 1 *))) counts)
        (reject-metal-gpu-request command :invalid-mesh-draw-range counts))
      (when (metal-render-pass-argument-table pass)
        (luv.metal:set-metal-render-argument-table
         (metal-render-pass-native-encoder pass)
         (metal-render-pass-argument-table pass)
         (logior
          (if (metal-mesh-pipeline-task-workgroup-size pipeline)
              luv.metal:+render-stage-object+
              0)
          luv.metal:+render-stage-mesh+
          luv.metal:+render-stage-fragment+)))
      (luv.metal:draw-metal-mesh-threadgroups
       (metal-render-pass-native-encoder pass)
       (metal-size-value counts)
       (metal-size-value
        (or (metal-mesh-pipeline-task-workgroup-size pipeline) '(1 1 1)))
       (metal-size-value
        (metal-mesh-pipeline-mesh-workgroup-size pipeline)))
      command)))

(defmethod end-pass ((pass metal-render-pass-encoder))
  (ensure-metal-render-pass-state pass :end-pass)
  (let ((owner (metal-render-pass-owner pass)))
    (luv.metal:end-encoding (metal-render-pass-native-encoder pass))
    (release-metal-render-pass-argument-table pass)
    (setf (metal-render-pass-state pass) :ended
          (metal-encoder-active-pass owner) nil
          (metal-encoder-encoded-p owner) t))
  (values))

(defmethod encode
    ((encoder metal-gpu-command-encoder)
     (command gpu-clear-texture-command))
  (ensure-metal-command-encoder-state encoder :encode)
  (ensure-no-active-metal-pass encoder :encode)
  (let ((texture (gpu-clear-texture-command-texture command))
        (color (gpu-clear-texture-command-color command)))
    (unless (typep texture 'metal-gpu-texture)
      (error 'gpu-request-error :operation :encode :descriptor command
             :reason :foreign-texture))
    (ensure-live-metal-object texture :encode)
    (ensure-metal-object-device
     texture (metal-texture-device texture)
     (metal-command-encoder-device encoder) :encode)
    (unless (member :render-attachment (gpu-texture-usage texture))
      (reject-metal-gpu-request command :texture-missing-render-usage texture))
    (unless (and (= (length color) 4)
                 (every #'realp color))
      (error 'gpu-request-error :operation :encode :descriptor command
             :reason :invalid-clear-color :details color))
    (luv.metal:encode-clear-pass
     (metal-encoder-command-buffer encoder)
     (metal-native-object texture)
     color)
    (retain-metal-resource encoder texture)
    (setf (metal-encoder-encoded-p encoder) t)
    command))

(defmethod encode
    ((encoder metal-gpu-command-encoder)
     (command gpu-copy-texture-command))
  (ensure-metal-command-encoder-state encoder :copy-texture)
  (ensure-no-active-metal-pass encoder :copy-texture)
  (let ((source (gpu-copy-texture-command-source command))
        (destination (gpu-copy-texture-command-destination command)))
    (unless (and (typep source 'metal-gpu-texture)
                 (typep destination 'metal-gpu-texture)
                 (equal (gpu-texture-size source)
                        (gpu-texture-size destination))
                 (eq (gpu-texture-format source)
                     (gpu-texture-format destination))
                 (member :copy-src (gpu-texture-usage source))
                 (member :copy-dst (gpu-texture-usage destination)))
      (reject-metal-gpu-request command :incompatible-copy
                                (list source destination)))
    (let ((device (metal-command-encoder-device encoder)))
      (ensure-metal-object-device
       source (metal-texture-device source) device :copy-texture)
      (ensure-metal-object-device
       destination (metal-texture-device destination) device :copy-texture))
    (let ((native-encoder
            (luv.metal:compute-command-encoder
             (metal-encoder-command-buffer encoder))))
      (unless native-encoder
        (error 'metal-gpu-error :operation :copy-texture
               :reason :compute-encoder-creation-failed))
      ;; MTL4CommandQueue ignores ordinary resource hazard tracking.  Make the
      ;; render-target writes visible before this encoder's blit-stage read.
      (luv.metal:barrier-after-queue-stages
       native-encoder luv.metal:+stage-fragment+ luv.metal:+stage-blit+
       luv.metal:+visibility-device+)
      (luv.metal:copy-metal-texture
       native-encoder (metal-native-object source)
       (metal-native-object destination))
      (luv.metal:end-encoding native-encoder))
    (retain-metal-resource encoder source)
    (retain-metal-resource encoder destination)
    (setf (metal-encoder-encoded-p encoder) t)
    command))

(defmethod encode
    ((encoder metal-gpu-command-encoder)
     (command gpu-copy-texture-to-buffer-command))
  (ensure-metal-command-encoder-state encoder :copy-texture-to-buffer)
  (ensure-no-active-metal-pass encoder :copy-texture-to-buffer)
  (let* ((source (gpu-copy-texture-to-buffer-command-source command))
         (destination
           (gpu-copy-texture-to-buffer-command-destination command))
         (size (and (typep source 'metal-gpu-texture)
                    (gpu-texture-size source)))
         (bytes-per-row (and size (* 4 (first size)))))
    (unless (and size
                 (member :copy-src (gpu-texture-usage source))
                 (typep destination 'metal-gpu-buffer)
                 (member :copy-dst (gpu-buffer-usage destination))
                 (member (gpu-texture-format source)
                         '(:rgba8-unorm :rgba8-unorm-srgb
                           :bgra8-unorm :bgra8-unorm-srgb))
                 (<= (* bytes-per-row (second size))
                     (gpu-buffer-size destination)))
      (reject-metal-gpu-request command :unsupported-texture-readback))
    (let ((device (metal-command-encoder-device encoder)))
      (ensure-metal-object-device
       source (metal-texture-device source) device :copy-texture-to-buffer)
      (ensure-metal-object-device
       destination (metal-buffer-device destination) device
       :copy-texture-to-buffer))
    (let ((native-encoder
            (luv.metal:compute-command-encoder
             (metal-encoder-command-buffer encoder))))
      (unless native-encoder
        (error 'metal-gpu-error :operation :copy-texture-to-buffer
               :reason :compute-encoder-creation-failed))
      (luv.metal:barrier-after-queue-stages
       native-encoder luv.metal:+stage-fragment+ luv.metal:+stage-blit+
       luv.metal:+visibility-device+)
      (luv.metal:copy-metal-texture-to-buffer
       native-encoder (metal-native-object source)
       (first size) (second size)
       (metal-native-object destination) bytes-per-row)
      (luv.metal:end-encoding native-encoder))
    (retain-metal-resource encoder source)
    (retain-metal-resource encoder destination)
    (setf (metal-encoder-encoded-p encoder) t)
    command))
