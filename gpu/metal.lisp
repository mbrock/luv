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
   (compiler :initarg :compiler :reader metal-device-compiler)))

(defclass metal-gpu-queue (gpu-queue metal-gpu-object)
  ((device :initarg :device :reader metal-queue-device)))

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
   (depth-stencil-state :initarg :depth-stencil-state
                        :reader metal-render-pipeline-depth-stencil-state))
  (:documentation
   "A linked Metal 4 render pipeline and its draw-time depth state."))

(defclass metal-frame-command-encoder (gpu-command-encoder)
  ((context :initarg :context :reader metal-encoder-context)
   (texture :initarg :texture :reader metal-encoder-texture)
   (command-buffer :initarg :command-buffer
                   :reader metal-encoder-command-buffer)
   (encoded-p :initform nil :accessor metal-encoder-encoded-p)))

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
          (let ((native-queue
                  (luv.metal:new-metal-4-command-queue native-device))
                (native-compiler nil))
            (unless native-queue
              (error 'metal-gpu-error :operation :request-device
                     :reason :metal-4-unavailable))
            (unwind-protect
                 (multiple-value-bind (compiler diagnostic)
                     (luv.metal:new-metal-4-compiler
                      native-device :label "luv Metal 4 compiler")
                   (unless compiler
                     (error 'metal-gpu-error :operation :request-device
                            :reason :metal-4-compiler-unavailable
                            :details diagnostic))
                   (setf native-compiler compiler)
                   (let* ((device
                            (make-instance
                             'metal-gpu-device
                             :label (gpu-descriptor-label descriptor)
                             :native-object native-device
                             :compiler native-compiler))
                          (queue
                            (make-instance
                             'metal-gpu-queue
                             :label "default Metal 4 queue"
                             :native-object native-queue
                             :device device)))
                     (setf (metal-device-queue device) queue
                           native-queue nil
                           native-compiler nil)
                     device))
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

(defmethod destroy ((device metal-gpu-device))
  (unless (metal-object-destroyed-p device)
    (luv.objective-c:release-objective-c-object
     (metal-device-compiler device))
    (let ((queue (metal-device-queue device)))
      (when (and queue (not (metal-object-destroyed-p queue)))
        (luv.objective-c:release-objective-c-object
         (metal-native-object queue))
        (setf (metal-object-destroyed-p queue) t)))
    (luv.objective-c:release-objective-c-object (metal-native-object device))
    (setf (metal-object-destroyed-p device) t))
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

(defmethod encode
    ((encoder metal-frame-command-encoder)
     (command gpu-clear-texture-command))
  (when (metal-encoder-encoded-p encoder)
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
