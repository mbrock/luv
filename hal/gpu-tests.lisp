(in-package #:luv.tests)

(define-test temporal-motion-format-has-two-half-float-lanes
  (true (= 4 (luv:texture-format-bytes-per-texel :rg16-float))))

(defclass frame-resource-key-context (luv:canvas-context) ())

(defmethod luv:canvas-frame-resource-key
    ((context frame-resource-key-context) surface)
  (declare (ignore context))
  (car surface))

(define-test canvas-frame-resources-follow-stable-presentation-slots
  (let ((cache (luv:make-canvas-frame-resource-cache))
        (context (make-instance 'frame-resource-key-context))
        (constructions 0))
    (flet ((construct (key surface)
             (incf constructions)
             (list key (cdr surface))))
      (multiple-value-bind (first created-p key)
          (luv:canvas-frame-resource cache context '(7 . :first) #'construct)
        (true created-p)
        (true (= 7 key))
        (true (equal '(7 :first) first))
        (multiple-value-bind (again created-again-p)
            (luv:canvas-frame-resource cache context '(7 . :new-wrapper)
                                       #'construct)
          (true (eq first again))
          (true (not created-again-p))))
      (true (= 1 constructions))
      (true (= 1 (luv:canvas-frame-resource-count cache))))))

(define-test canvas-frame-resource-release-is-retryable
  (let* ((entries (make-hash-table :test #'eql))
         (cache (luv:make-canvas-frame-resource-cache :entries entries))
         (context (make-instance 'frame-resource-key-context))
         (fail-p t)
         (releases 0))
    (luv:canvas-frame-resource
     cache context '(3 . :surface) (lambda (key surface)
                                    (declare (ignore key surface))
                                    :owned))
    (flet ((release (value)
             (true (eq :owned value))
             (incf releases)
             (when fail-p (error "injected frame resource release failure"))))
      (fail (luv:evict-canvas-frame-resource-key cache 3 #'release)
            'simple-error)
      (true (= 1 (luv:canvas-frame-resource-count cache)))
      (setf fail-p nil)
      (luv:destroy-canvas-frame-resource-cache cache #'release)
      (true (= 2 releases))
      (true (zerop (luv:canvas-frame-resource-count cache)))
      (fail
       (luv:canvas-frame-resource cache context '(3 . :surface)
                                  (lambda (&rest arguments)
                                    (declare (ignore arguments)) :new))
       'simple-error))))

(define-test buffer-uploads-preserve-sixteen-bit-storage
  (multiple-value-bind (foreign-type element-size)
      (luv::buffer-data-foreign-type
       (make-array 3 :element-type '(unsigned-byte 16)))
    (true (eq :uint16 foreign-type))
    (true (= 2 element-size))))

(defclass descriptor-probe-device (luv:gpu-device)
  ((operation :initform nil :accessor descriptor-probe-operation)
   (descriptor :initform nil :accessor descriptor-probe-descriptor)
   (native :initform nil :accessor descriptor-probe-native)
   (owner :initform nil :accessor descriptor-probe-owner)))

(defmethod luv:create
    ((device descriptor-probe-device) (descriptor luv::buffer-descriptor))
  (setf (descriptor-probe-operation device) :create-buffer
        (descriptor-probe-descriptor device) descriptor)
  descriptor)

(defmethod luv:create
    ((device descriptor-probe-device) (descriptor luv::texture-descriptor))
  (setf (descriptor-probe-operation device) :create-texture
        (descriptor-probe-descriptor device) descriptor)
  descriptor)

(defmethod luv:adopt-native-texture
    ((device descriptor-probe-device) native owner
     (descriptor luv::texture-descriptor))
  (setf (descriptor-probe-operation device) :adopt-texture
        (descriptor-probe-descriptor device) descriptor
        (descriptor-probe-native device) native
        (descriptor-probe-owner device) owner)
  descriptor)

(defun gpu-request-reason (thunk)
  (handler-case
      (progn (funcall thunk) :no-error)
    (luv:gpu-request-error (condition)
      (luv:gpu-request-error-reason condition))))

(defun gpu-request-condition (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (luv:gpu-request-error (condition) condition)))

(define-test portable-buffer-descriptors-reach-devices-in-one-canonical-shape
  (let* ((device (make-instance 'descriptor-probe-device))
         (usage (vector :vertex :index :copy-dst :vertex))
         (source
           (luv:make-buffer-descriptor
            :label "probe buffer" :size 64 :usage usage))
         (canonical (luv:create device source)))
    (true (not (eq source canonical)))
    (true (eq :create-buffer (descriptor-probe-operation device)))
    (true (= 64 (luv::buffer-descriptor-size canonical)))
    (true (equal '(:vertex :index :copy-dst)
                 (luv::buffer-descriptor-usage canonical)))
    (true (eq usage (luv::buffer-descriptor-usage source)))
    (true (string= "probe buffer" (luv::gpu-descriptor-label canonical)))))

(define-test portable-texture-descriptors-canonicalize-create-and-adoption
  (dolist (size '((32 16) (32 16 1) #(32 16) #(32 16 1)))
    (let* ((device (make-instance 'descriptor-probe-device))
           (source
             (luv:make-texture-descriptor
              :label "probe texture" :size size :dimensions :2d
              :format :rgba8-unorm :usage :texture-binding
              :sample-count 1))
           (canonical (luv:create device source)))
      (true (equal '(32 16 1) (luv::texture-descriptor-size canonical)))
      (true (equal '(:texture-binding)
                   (luv::texture-descriptor-usage canonical)))
      (true (= 1 (luv::texture-descriptor-sample-count canonical)))
      (true (eq size (luv::texture-descriptor-size source)))))
  (let* ((device (make-instance 'descriptor-probe-device))
         (native (list :native))
         (owner (list :owner))
         (source
           (luv:make-texture-descriptor
            :size #(20 10) :dimensions :2d :format :r8-unorm
            :usage #(:texture-binding :storage-binding :texture-binding)))
         (canonical
           (luv:adopt-native-texture device native owner source)))
    (true (eq :adopt-texture (descriptor-probe-operation device)))
    (true (eq native (descriptor-probe-native device)))
    (true (eq owner (descriptor-probe-owner device)))
    (true (equal '(20 10 1) (luv::texture-descriptor-size canonical)))
    (true (equal '(:texture-binding :storage-binding)
                 (luv::texture-descriptor-usage canonical)))))

(define-test multisample-textures-are-render-only-and-use-portable-counts
  (let ((device (make-instance 'descriptor-probe-device)))
    (dolist (sample-count '(2 4 8))
      (let ((canonical
              (luv:create
               device
               (luv:make-texture-descriptor
                :size '(32 16) :format :rgba8-unorm
                :dimensions :2d :usage :render-attachment
                :sample-count sample-count))))
        (true (= sample-count
                 (luv::texture-descriptor-sample-count canonical)))))
    (dolist (sample-count '(0 3 16))
      (true (eq :invalid-texture-sample-count
                (gpu-request-reason
                 (lambda ()
                   (luv:create
                    device
                    (luv:make-texture-descriptor
                     :size '(32 16) :format :rgba8-unorm
                     :dimensions :2d :usage :render-attachment
                     :sample-count sample-count)))))))
    (true (eq :invalid-multisample-texture-usage
              (gpu-request-reason
               (lambda ()
                 (luv:create
                  device
                  (luv:make-texture-descriptor
                   :size '(32 16) :format :rgba8-unorm
                   :dimensions :2d
                   :usage '(:render-attachment :texture-binding)
                   :sample-count 4))))))))

(define-test portable-descriptor-errors-do-not-depend-on-a-backend
  (let ((device (make-instance 'descriptor-probe-device)))
    (dolist (size '(nil 0 -1 #.(1+ (expt 2 64))))
      (true (eq :invalid-buffer-size
                (gpu-request-reason
                 (lambda ()
                   (luv:create
                    device (luv:make-buffer-descriptor
                            :size size :usage :vertex)))))))
    (dolist (usage '(nil :indirect (:vertex :indirect)
                     #(:copy-dst :map-read)))
      (true (eq :invalid-buffer-usage
                (gpu-request-reason
                 (lambda ()
                   (luv:create
                    device (luv:make-buffer-descriptor
                            :size 4 :usage usage)))))))
    (dolist (size '(nil (16) (16 8 2) (16 0) #(16 -1)))
      (true (eq :invalid-texture-size
                (gpu-request-reason
                 (lambda ()
                   (luv:create
                    device (luv:make-texture-descriptor
                            :size size :dimensions :2d :format :r8-unorm
                            :usage :texture-binding)))))))
    (dolist (usage '(nil :present (:copy-dst :present) #(present)))
      (true (eq :invalid-texture-usage
                (gpu-request-reason
                 (lambda ()
                   (luv:create
                    device (luv:make-texture-descriptor
                            :size '(16 8) :dimensions :2d :format :r8-unorm
                            :usage usage)))))))
    (true (eq :invalid-texture-dimensions
              (gpu-request-reason
               (lambda ()
                 (luv:create
                  device (luv:make-texture-descriptor
                          :size '(16 8) :dimensions :3d :format :r8-unorm
                          :usage :texture-binding))))))))

(define-test portable-descriptor-errors-retain-source-and-operation
  (let* ((device (make-instance 'descriptor-probe-device))
         (source
           (luv:make-buffer-descriptor :size 0 :usage :vertex))
         (condition
           (gpu-request-condition (lambda () (luv:create device source)))))
    (true (eq :create (luv:gpu-error-operation condition)))
    (true (eq source (luv:gpu-request-error-descriptor condition)))
    (true (eql 0 (luv:gpu-request-error-details condition))))
  (let ((device (make-instance 'descriptor-probe-device))
        (dotted-usage (cons :vertex :copy-dst))
        (circular-size (list 16 8)))
    (setf (cddr circular-size) circular-size)
    (true (eq :invalid-buffer-usage
              (gpu-request-reason
               (lambda ()
                 (luv:create
                  device (luv:make-buffer-descriptor
                          :size 4 :usage dotted-usage))))))
    (true (eq :invalid-texture-size
              (gpu-request-reason
               (lambda ()
                 (luv:create
                  device (luv:make-texture-descriptor
                          :size circular-size :dimensions :2d :format :r8-unorm
                          :usage :texture-binding))))))))

(define-test native-retirement-preserves-a-strict-fifo-retry-barrier
  (let ((ledger (luv::make-gpu-retirement-ledger))
        (events '())
        (fail-view-p t)
        (warning nil))
    (luv::enqueue-gpu-retirement
     ledger :view 0
     (lambda ()
       (setf events (nconc events (list :view)))
       (when fail-view-p
         (setf fail-view-p nil)
         (error "view retirement failed"))))
    (luv::enqueue-gpu-retirement
     ledger :texture 0
     (lambda ()
       (setf events (nconc events (list :texture)))))
    (luv::enqueue-gpu-retirement
     ledger :future 2
     (lambda ()
       (setf events (nconc events (list :future)))))
    (handler-bind
        ((luv::gpu-native-retirement-warning
           (lambda (condition)
             (setf warning condition)
             (muffle-warning condition))))
      (luv::maintain-gpu-retirement-ledger ledger 0 :operation :test))
    ;; The failed view is an ownership barrier: its texture and every later
    ;; entry remain untouched until the view succeeds.
    (true (equal '(:view) events))
    (true (typep warning 'luv:gpu-native-retirement-warning))
    (true (typep warning 'luv:gpu-native-retirement-condition))
    (true (eq :test (luv:gpu-native-retirement-operation warning)))
    (true (= 1 (length (luv:gpu-native-retirement-failures warning))))
    (true (eq :view
              (luv:gpu-native-retirement-failure-resource
               (first (luv:gpu-native-retirement-failures warning)))))
    (true (= 1
             (luv:gpu-native-retirement-failure-attempts
              (first (luv:gpu-native-retirement-failures warning)))))
    (true (search "view retirement failed"
                  (princ-to-string
                   (luv:gpu-native-retirement-failure-cause
                    (first (luv:gpu-native-retirement-failures warning))))))
    (true (search "retained the failed resource and its FIFO successors"
                  (princ-to-string warning)))
    (true (equal '(:view :texture :future)
                 (mapcar #'luv::gpu-retirement-entry-resource
                         (luv::gpu-retirement-ledger-entries ledger))))
    (fail
     (luv::ensure-gpu-retirement-ledger-empty
      ledger :operation :test-device-destroy)
     'luv::gpu-native-retirement-error)
    (setf events nil)
    (luv::maintain-gpu-retirement-ledger ledger 2 :operation :test-retry)
    (true (equal '(:view :texture :future) events))
    (true (null (luv::gpu-retirement-ledger-entries ledger)))
    (true (eq ledger (luv::ensure-gpu-retirement-ledger-empty ledger)))))

(define-test native-retirement-detaches-callback-enqueues-without-losing-order
  (let ((ledger (luv::make-gpu-retirement-ledger))
        (events '())
        (fail-second-p t))
    (luv::enqueue-gpu-retirement
     ledger :first 0
     (lambda ()
       (push :first events)
       ;; Model a release callback which recursively destroys another owner.
       (luv::enqueue-gpu-retirement
        ledger :callback-child 0
        (lambda () (push :callback-child events)))
       ;; Recursive maintenance cannot overtake the outer detached batch, and
       ;; device teardown cannot mistake that detached batch for emptiness.
       (luv::maintain-gpu-retirement-ledger ledger 0)
       (fail
        (luv::ensure-gpu-retirement-ledger-empty ledger)
        'luv:gpu-native-retirement-error)))
    (luv::enqueue-gpu-retirement
     ledger :second 0
     (lambda ()
       (push :second events)
       (when fail-second-p
         (setf fail-second-p nil)
         (error "second retirement failed"))))
    (handler-bind
        ((luv::gpu-native-retirement-warning #'muffle-warning))
      (luv::maintain-gpu-retirement-ledger ledger 0))
    (true (equal '(:second :first) events))
    ;; The older failure stays before work enqueued by a callback, and the
    ;; callback child is deliberately not visited in the same maintenance pass.
    (true (equal '(:second :callback-child)
                 (mapcar #'luv::gpu-retirement-entry-resource
                         (luv::gpu-retirement-ledger-entries ledger))))
    (setf events nil)
    (luv::maintain-gpu-retirement-ledger ledger 0)
    (true (equal '(:callback-child :second) events))
    (true (null (luv::gpu-retirement-ledger-entries ledger)))))

(define-test native-retirement-sequences-resume-after-the-last-successful-step
  (let ((ledger (luv::make-gpu-retirement-ledger))
        (first-attempts 0)
        (second-attempts 0)
        (third-attempts 0)
        (follower-attempts 0)
        (fail-second-p t))
    (luv::enqueue-gpu-retirement
     ledger :multi-step 0
     (luv::make-gpu-retirement-sequence
      (lambda () (incf first-attempts))
      (lambda ()
        (incf second-attempts)
        (when fail-second-p
          (setf fail-second-p nil)
          (error "injected second-step failure")))
      (lambda () (incf third-attempts))))
    (luv::enqueue-gpu-retirement
     ledger :follower 0 (lambda () (incf follower-attempts)))
    (handler-bind
        ((luv:gpu-native-retirement-warning #'muffle-warning))
      (luv::maintain-gpu-retirement-ledger ledger 0))
    (true (= 1 first-attempts))
    (true (= 1 second-attempts))
    (true (zerop third-attempts))
    (true (zerop follower-attempts))
    (luv::maintain-gpu-retirement-ledger ledger 0)
    ;; Retry resumes at step two; the successful destructive step is not run
    ;; twice, and the follower runs only after the whole owner retires.
    (true (= 1 first-attempts))
    (true (= 2 second-attempts))
    (true (= 1 third-attempts))
    (true (= 1 follower-attempts))
    (true (null (luv::gpu-retirement-ledger-entries ledger)))))

(define-test finalizer-native-retirement-remains-durably-retryable
  (let ((luv::*gpu-finalizer-retirement-ledger*
          (luv::make-gpu-retirement-ledger))
        (attempts 0)
        (fail-p t))
    (handler-bind
        ((luv:gpu-native-retirement-warning #'muffle-warning))
      (luv::retire-gpu-finalizer-native-owner
       nil :leaked-owner
       (lambda ()
         (incf attempts)
         (when fail-p
           (setf fail-p nil)
           (error "injected finalizer teardown failure")))))
    (true (= 1 attempts))
    (true (equal '(:leaked-owner)
                 (mapcar #'luv::gpu-retirement-entry-resource
                         (luv::gpu-retirement-ledger-entries
                          luv::*gpu-finalizer-retirement-ledger*))))
    (luv::maintain-gpu-finalizer-retirements)
    (true (= 2 attempts))
    (true (null
           (luv::gpu-retirement-ledger-entries
            luv::*gpu-finalizer-retirement-ledger*)))))

(define-test native-retirement-transfer-precedes-invalidation
  (let ((ledger (luv::make-gpu-retirement-ledger))
        (invalidated-p nil))
    (luv::transfer-gpu-retirement
     ledger :resource 0 (lambda () nil)
     (lambda ()
       (true (eq :resource
                 (luv::gpu-retirement-entry-resource
                  (first (luv::gpu-retirement-ledger-entries ledger)))))
       (setf invalidated-p t)))
    (true invalidated-p))
  (let ((invalidated-p nil)
        (condition nil))
    (handler-case
        (luv::perform-gpu-retirement-directly
         :resource
         (lambda () (error "native teardown failed"))
         (lambda () (setf invalidated-p t)))
      (luv:gpu-native-retirement-error (error)
        (setf condition error)))
    (true (typep condition 'luv:gpu-native-retirement-error))
    (true (eq :destroy (luv:gpu-native-retirement-operation condition)))
    (true (= 1 (length (luv:gpu-native-retirement-failures condition))))
    (true (search "native teardown failed" (princ-to-string condition)))
    (true (not invalidated-p))))

(define-test native-retirement-worker-start-failure-cannot-split-custody
  (let* ((luv::*gpu-retirement-ledger-custodians*
           (make-hash-table :test #'eq))
         (luv::*gpu-retirement-custodian-service-thread* nil)
         (luv::*gpu-retirement-custodian-service-enabled-p* t)
         (ledger (luv::make-gpu-retirement-ledger))
         (custodian (list :fake-queue))
         (invalidated-p nil)
         (spawn-symbol
           'luv::spawn-gpu-retirement-custodian-service-thread)
         (original-spawn (symbol-function spawn-symbol)))
    (unwind-protect
         (progn
           (setf (symbol-function spawn-symbol)
                 (lambda () (error "injected worker creation failure")))
           (luv::transfer-gpu-retirement
            ledger :owner 0 (lambda () nil)
            (lambda () (setf invalidated-p t)) custodian)
           (true invalidated-p)
           (true (= 1 (length
                       (luv::gpu-retirement-ledger-entries ledger))))
           (true (eq custodian
                     (gethash
                      ledger luv::*gpu-retirement-ledger-custodians*)))
           (true (search
                  "injected worker creation failure"
                  (princ-to-string
                   luv::*gpu-retirement-custodian-service-start-error*))))
      (setf (symbol-function spawn-symbol) original-spawn)
      (luv::release-gpu-retirement-ledger-custodian
       ledger custodian))))

(define-test native-retirement-service-worker-is-ephemeral
  (let ((thread (luv::spawn-gpu-retirement-custodian-service-thread)))
    (true (sb-thread:thread-ephemeral-p thread))
    (sb-thread:join-thread thread))
  (let ((creators
          (loop repeat 4
                collect
                (sb-thread:make-thread
                 (lambda ()
                   (loop repeat 4
                         always
                         (let ((thread
                                 (luv::spawn-gpu-retirement-custodian-service-thread)))
                           (prog1
                               (sb-thread:thread-ephemeral-p thread)
                             (sb-thread:join-thread thread)))))))))
    (true (every #'identity
                 (mapcar #'sb-thread:join-thread creators)))))
