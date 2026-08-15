;;;; The deliberately small Metal implementation needed by canvas clearing.

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

(defclass metal-gpu-object ()
  ((native-object :initarg :native-object :reader metal-native-object)
   (destroyed-p :initform nil :accessor metal-object-destroyed-p)))

(defclass metal-gpu-device (gpu-device metal-gpu-object)
  ((queue :initform nil :accessor metal-device-queue)
   (compiler :initarg :compiler :reader metal-device-compiler)
   (residency-set :initarg :residency-set
                  :reader metal-device-residency-set)))

(defstruct metal-submission
  value command-buffer allocator)

(defclass metal-gpu-queue (gpu-queue metal-gpu-object)
  ((device :initarg :device :reader metal-queue-device)
   (completion-event :initarg :completion-event
                     :reader metal-queue-completion-event)
   (submitted-value :initform 0 :accessor metal-queue-submitted-value)
   (pending-submissions :initform nil
                        :accessor metal-queue-pending-submissions)))

(defclass metal-gpu-buffer (gpu-buffer metal-gpu-object)
  ((device :initarg :device :reader metal-buffer-device)
   (mapped :initarg :mapped :reader metal-buffer-mapped)))

(defclass metal-gpu-texture (gpu-texture metal-gpu-object)
  ((device :initarg :device :reader metal-texture-device)))

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
   (depth-stencil-state :initarg :depth-stencil-state
                        :reader metal-render-pipeline-depth-stencil-state))
  (:documentation
   "A linked Metal 4 render pipeline and its draw-time depth state."))

(defclass metal-frame-command-encoder (gpu-command-encoder)
  ((context :initarg :context :reader metal-encoder-context)
   (texture :initarg :texture :reader metal-encoder-texture)
   (command-buffer :initarg :command-buffer
                   :reader metal-encoder-command-buffer)
   (active-pass :initform nil :accessor metal-encoder-active-pass)
   (encoded-p :initform nil :accessor metal-encoder-encoded-p)))

(defclass metal-render-pass-encoder (gpu-render-pass-encoder)
  ((owner :initarg :owner :reader metal-render-pass-owner)
   (native-encoder :initarg :native-encoder
                   :reader metal-render-pass-native-encoder)
   (pipeline :initform nil :accessor metal-render-pass-pipeline)
   (argument-table :initform nil
                   :accessor metal-render-pass-argument-table)
   (vertex-bindings :initform (make-hash-table)
                    :reader metal-render-pass-vertex-bindings)
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

(defun reclaim-completed-metal-submissions (queue)
  "Release command memory whose Metal 4 shared-event value has completed."
  (let ((completed
          (luv.metal:metal-shared-event-signaled-value
           (metal-queue-completion-event queue)))
        (pending nil))
    (dolist (submission (metal-queue-pending-submissions queue))
      (if (<= (metal-submission-value submission) completed)
          (progn
            (luv.objective-c:release-objective-c-object
             (metal-submission-command-buffer submission))
            (luv.objective-c:release-objective-c-object
             (metal-submission-allocator submission)))
          (push submission pending)))
    (setf (metal-queue-pending-submissions queue) (nreverse pending)))
  (values))

(defmethod submitted-work-done ((queue metal-gpu-queue))
  "Wait for the Metal 4 shared-event frontier most recently submitted."
  (ensure-live-metal-object queue :submitted-work-done)
  (let ((value (metal-queue-submitted-value queue)))
    (when (plusp value)
      (unless (plusp
               (luv.metal:wait-for-metal-shared-event
                (metal-queue-completion-event queue) value 30000))
        (error 'metal-gpu-error :operation :submitted-work-done
               :reason :completion-timeout :details value))))
  (reclaim-completed-metal-submissions queue)
  (values))

(defun submit-metal-command-buffer
    (queue command-buffer allocator &key after-commit)
  "Consume, commit, and retain Metal 4 command memory to QUEUE's frontier.

AFTER-COMMIT performs the drawable signal and presentation handshake before
the completion event is enqueued.  This is the allocator-lifetime proof in
#PH57K5."
  (let ((committed-p nil))
    (handler-case
        (progn
          (ensure-live-metal-object queue :submit)
          (reclaim-completed-metal-submissions queue)
          (luv.metal:commit-command-buffer
           (metal-native-object queue) command-buffer)
          (setf committed-p t)
          (let ((value (incf (metal-queue-submitted-value queue))))
            ;; Register ownership before presentation.  If presentation raises,
            ;; the unwind still queues the completion signal and the queue keeps
            ;; the allocator alive until that signal crosses the frontier.
            (push (make-metal-submission
                   :value value :command-buffer command-buffer
                   :allocator allocator)
                  (metal-queue-pending-submissions queue))
            (unwind-protect
                 (when after-commit
                   (funcall after-commit))
              (luv.metal:signal-metal-event
               (metal-native-object queue)
               (metal-queue-completion-event queue) value))
            value))
      (error (condition)
        ;; Before commit there is no GPU ownership to retire.  After commit the
        ;; pending submission above is the only safe owner of these objects.
        (unless committed-p
          (luv.objective-c:release-objective-c-object command-buffer)
          (luv.objective-c:release-objective-c-object allocator))
        (error condition)))))

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

(defmethod destroy ((buffer metal-gpu-buffer))
  (unless (metal-object-destroyed-p buffer)
    (let* ((device (metal-buffer-device buffer))
           (residency-set (metal-device-residency-set device)))
      ;; This is deliberately conservative until Metal gets the same deferred
      ;; physical-retirement queue as Vulkan: logical destroy waits at the
      ;; shared-event frontier before changing residency or releasing storage.
      (submitted-work-done (device-queue device))
      (luv.metal:remove-metal-residency-allocation
       residency-set (metal-native-object buffer))
      (luv.metal:commit-metal-residency-set residency-set)
      (luv.objective-c:release-objective-c-object
       (metal-native-object buffer))
      (setf (metal-object-destroyed-p buffer) t)))
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
    (:fragment luv.metal:+function-type-fragment+)))

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
    (luv.objective-c:release-objective-c-object
     (metal-native-object module))
    (setf (metal-object-destroyed-p module) t))
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
  (ecase format
    (:bgra8-unorm luv.metal:+pixel-format-bgra8-unorm+)
    (:bgra8-unorm-srgb luv.metal:+pixel-format-bgra8-unorm-srgb+)
    ((nil)
     (reject-metal-gpu-request descriptor :missing-color-format))))

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
         (primitive (render-pipeline-descriptor-primitive descriptor))
         (topology (or (getf primitive :topology) :triangle-list))
         (depth-stencil
           (render-pipeline-descriptor-depth-stencil descriptor))
         (depth-format (and depth-stencil (getf depth-stencil :format)))
         (depth-compare
           (and depth-stencil (getf depth-stencil :depth-compare)))
         (depth-write-enabled
           (and depth-stencil (getf depth-stencil :depth-write-enabled))))
    (unless (and (null layout)
                 (typep vertex-module 'metal-gpu-shader-module)
                 (typep fragment-module 'metal-gpu-shader-module)
                 (= (metal-shader-module-function-type vertex-module)
                    luv.metal:+function-type-vertex+)
                 (= (metal-shader-module-function-type fragment-module)
                    luv.metal:+function-type-fragment+)
                 (string= vertex-entry-point
                          (metal-shader-module-entry-point vertex-module))
                 (string= fragment-entry-point
                          (metal-shader-module-entry-point fragment-module))
                 format
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
    (ensure-metal-object-device
     fragment-module (metal-shader-module-device fragment-module) device
     :create-render-pipeline)
    (let ((pipeline-state nil)
          (depth-state nil)
          (completed-p nil))
      (unwind-protect
           (progn
             (multiple-value-bind (pipeline diagnostic)
                 (luv.metal:compile-metal-4-render-pipeline
                  (metal-device-compiler device)
                  (metal-native-object vertex-module) vertex-entry-point
                  (metal-native-object fragment-module) fragment-entry-point
                  vertex-buffers
                  (metal-render-pipeline-pixel-format format descriptor)
                  luv.metal:+primitive-topology-class-triangle+
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
                      :depth-format depth-format
                      :depth-stencil-state depth-state)))
               (setf completed-p t)
               pipeline))
        (unless completed-p
          (when depth-state
            (luv.objective-c:release-objective-c-object depth-state))
          (when pipeline-state
            (luv.objective-c:release-objective-c-object pipeline-state)))))))

(defmethod destroy ((pipeline metal-gpu-render-pipeline))
  (unless (metal-object-destroyed-p pipeline)
    (submitted-work-done
     (device-queue (metal-render-pipeline-device pipeline)))
    (let ((depth-state (metal-render-pipeline-depth-stencil-state pipeline)))
      (when depth-state
        (luv.objective-c:release-objective-c-object depth-state)))
    (luv.objective-c:release-objective-c-object
     (metal-native-object pipeline))
    (setf (metal-object-destroyed-p pipeline) t))
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

(defun metal-color-pass-attachment (encoder descriptor)
  (let ((attachments (render-pass-descriptor-color-attachments descriptor))
        (depth (render-pass-descriptor-depth-stencil-attachment descriptor)))
    (unless (and (= 1 (length attachments)) (null depth))
      (reject-metal-gpu-request
       descriptor :unsupported-metal-render-pass
       (list :color-attachments attachments :depth-stencil depth)))
    (let* ((attachment (first attachments))
           (texture (getf attachment :view))
           (load-op (or (getf attachment :load-op) :clear))
           (store-op (or (getf attachment :store-op) :store))
           (clear-value
             (or (getf attachment :clear-value) #(0.0 0.0 0.0 1.0))))
      (unless (and (typep texture 'metal-gpu-texture)
                   (eq texture (metal-encoder-texture encoder))
                   (member load-op '(:clear :load))
                   (eq store-op :store)
                   (= 4 (length clear-value))
                   (every #'realp clear-value))
        (reject-metal-gpu-request
         descriptor :unsupported-metal-color-attachment attachment))
      (values texture load-op clear-value))))

(defmethod begin-render-pass
    ((encoder metal-frame-command-encoder) descriptor)
  "Begin a Metal 4 color pass on the current drawable texture."
  (when (metal-encoder-active-pass encoder)
    (error 'gpu-invalid-state-error :object encoder :operation :begin-render-pass
           :state :pass-active :expected-state :between-passes))
  (multiple-value-bind (texture load-op clear-value)
      (metal-color-pass-attachment encoder descriptor)
    (let ((native-encoder
            (luv.metal:new-color-render-command-encoder
             (metal-encoder-command-buffer encoder)
             (metal-native-object texture) clear-value
             :clear-p (eq load-op :clear))))
      (unless native-encoder
        (error 'metal-gpu-error :operation :begin-render-pass
               :reason :render-encoder-creation-failed))
      (let ((pass
              (make-instance
               'metal-render-pass-encoder
               :owner encoder :native-encoder native-encoder
               :label (gpu-descriptor-label descriptor))))
        (setf (metal-encoder-active-pass encoder) pass)
        pass))))

(defun release-metal-render-pass-argument-table (pass)
  (let ((table (metal-render-pass-argument-table pass)))
    (when table
      ;; MTL4RenderCommandEncoder snapshots table contents at each draw.
      (luv.objective-c:release-objective-c-object table)
      (setf (metal-render-pass-argument-table pass) nil))))

(defmethod encode
    ((pass metal-render-pass-encoder) (command gpu-set-pipeline-command))
  (ensure-metal-render-pass-state pass :set-pipeline)
  (let* ((pipeline (gpu-set-pipeline-command-pipeline command))
         (owner (metal-render-pass-owner pass))
         (device (metal-texture-device (metal-encoder-texture owner))))
    (unless (typep pipeline 'metal-gpu-render-pipeline)
      (reject-metal-gpu-request command :incompatible-pipeline pipeline))
    (ensure-metal-object-device
     pipeline (metal-render-pipeline-device pipeline) device :set-pipeline)
    (release-metal-render-pass-argument-table pass)
    (clrhash (metal-render-pass-vertex-bindings pass))
    (let ((vertex-buffers (metal-render-pipeline-vertex-buffers pipeline)))
      (when vertex-buffers
        (multiple-value-bind (table diagnostic)
            (luv.metal:new-metal-4-argument-table
             (metal-native-object device)
             (1+ (reduce #'max vertex-buffers
                         :key (lambda (buffer) (getf buffer :binding))))
             :label (format nil "~A vertex arguments"
                            (or (gpu-object-label pipeline) "Metal pipeline"))
             :attribute-strides-p t)
          (unless table
            (error 'metal-gpu-error :operation :set-pipeline
                   :reason :argument-table-creation-failed
                   :details diagnostic))
          (setf (metal-render-pass-argument-table pass) table))))
    (luv.metal:set-metal-render-pipeline
     (metal-render-pass-native-encoder pass) (metal-native-object pipeline))
    (let ((depth-state
            (metal-render-pipeline-depth-stencil-state pipeline)))
      (when depth-state
        (luv.metal:set-metal-depth-stencil-state
         (metal-render-pass-native-encoder pass) depth-state)))
    (setf (metal-render-pass-pipeline pass) pipeline)
    command))

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
    (setf (gethash slot (metal-render-pass-vertex-bindings pass)) buffer)
    command))

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
         luv.metal:+render-stage-vertex+))
      (luv.metal:draw-metal-primitives
       (metal-render-pass-native-encoder pass)
       (metal-primitive-type pipeline) first-vertex vertex-count
       instance-count first-instance)
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
    ((encoder metal-frame-command-encoder)
     (command gpu-clear-texture-command))
  (when (or (metal-encoder-encoded-p encoder)
            (metal-encoder-active-pass encoder))
    (error 'gpu-invalid-state-error :object encoder :operation :encode
           :state :clear-encoded :expected-state :empty))
  (let ((texture (gpu-clear-texture-command-texture command))
        (color (gpu-clear-texture-command-color command)))
    (unless (typep texture 'metal-gpu-texture)
      (error 'gpu-request-error :operation :encode :descriptor command
             :reason :foreign-texture))
    (ensure-live-metal-object texture :encode)
    (unless (eq texture (metal-encoder-texture encoder))
      (error 'gpu-invalid-state-error :object texture :operation :encode
             :state :outside-frame :expected-state :current-frame))
    (unless (and (= (length color) 4)
                 (every #'realp color))
      (error 'gpu-request-error :operation :encode :descriptor command
             :reason :invalid-clear-color :details color))
    (luv.metal:encode-clear-pass
     (metal-encoder-command-buffer encoder)
     (metal-native-object texture)
     color)
    (setf (metal-encoder-encoded-p encoder) t)
    command))
