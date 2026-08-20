(in-package #:luv.tests)

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

(deftest portable-buffer-descriptors-reach-devices-in-one-canonical-shape
  (let* ((device (make-instance 'descriptor-probe-device))
         (usage (vector :vertex :copy-dst :vertex))
         (source
           (luv:make-buffer-descriptor
            :label "probe buffer" :size 64 :usage usage))
         (canonical (luv:create device source)))
    (ok (not (eq source canonical)))
    (ok (eq :create-buffer (descriptor-probe-operation device)))
    (ok (= 64 (luv::buffer-descriptor-size canonical)))
    (ok (equal '(:vertex :copy-dst)
               (luv::buffer-descriptor-usage canonical)))
    (ok (eq usage (luv::buffer-descriptor-usage source)))
    (ok (string= "probe buffer" (luv::gpu-descriptor-label canonical)))))

(deftest portable-texture-descriptors-canonicalize-create-and-adoption
  (dolist (size '((32 16) (32 16 1) #(32 16) #(32 16 1)))
    (let* ((device (make-instance 'descriptor-probe-device))
           (source
             (luv:make-texture-descriptor
              :label "probe texture" :size size :dimensions :2d
              :format :rgba8-unorm :usage :texture-binding))
           (canonical (luv:create device source)))
      (ok (equal '(32 16 1) (luv::texture-descriptor-size canonical)))
      (ok (equal '(:texture-binding)
                 (luv::texture-descriptor-usage canonical)))
      (ok (eq size (luv::texture-descriptor-size source)))))
  (let* ((device (make-instance 'descriptor-probe-device))
         (native (list :native))
         (owner (list :owner))
         (source
           (luv:make-texture-descriptor
            :size #(20 10) :dimensions :2d :format :r8-unorm
            :usage #(:texture-binding :storage-binding :texture-binding)))
         (canonical
           (luv:adopt-native-texture device native owner source)))
    (ok (eq :adopt-texture (descriptor-probe-operation device)))
    (ok (eq native (descriptor-probe-native device)))
    (ok (eq owner (descriptor-probe-owner device)))
    (ok (equal '(20 10 1) (luv::texture-descriptor-size canonical)))
    (ok (equal '(:texture-binding :storage-binding)
               (luv::texture-descriptor-usage canonical)))))

(deftest portable-descriptor-errors-do-not-depend-on-a-backend
  (let ((device (make-instance 'descriptor-probe-device)))
    (dolist (size '(nil 0 -1 #.(1+ (expt 2 64))))
      (ok (eq :invalid-buffer-size
              (gpu-request-reason
               (lambda ()
                 (luv:create
                  device (luv:make-buffer-descriptor
                          :size size :usage :vertex)))))))
    (dolist (usage '(nil :index (:vertex :index) #(:copy-dst :index)))
      (ok (eq :invalid-buffer-usage
              (gpu-request-reason
               (lambda ()
                 (luv:create
                  device (luv:make-buffer-descriptor
                          :size 4 :usage usage)))))))
    (dolist (size '(nil (16) (16 8 2) (16 0) #(16 -1)))
      (ok (eq :invalid-texture-size
              (gpu-request-reason
               (lambda ()
                 (luv:create
                  device (luv:make-texture-descriptor
                          :size size :dimensions :2d :format :r8-unorm
                          :usage :texture-binding)))))))
    (dolist (usage '(nil :present (:copy-dst :present) #(present)))
      (ok (eq :invalid-texture-usage
              (gpu-request-reason
               (lambda ()
                 (luv:create
                  device (luv:make-texture-descriptor
                          :size '(16 8) :dimensions :2d :format :r8-unorm
                          :usage usage)))))))
    (ok (eq :invalid-texture-dimensions
            (gpu-request-reason
             (lambda ()
               (luv:create
                device (luv:make-texture-descriptor
                        :size '(16 8) :dimensions :3d :format :r8-unorm
                        :usage :texture-binding))))))))

(deftest portable-descriptor-errors-retain-source-and-operation
  (let* ((device (make-instance 'descriptor-probe-device))
         (source
           (luv:make-buffer-descriptor :size 0 :usage :vertex))
         (condition
           (gpu-request-condition (lambda () (luv:create device source)))))
    (ok (eq :create (luv:gpu-error-operation condition)))
    (ok (eq source (luv:gpu-request-error-descriptor condition)))
    (ok (eql 0 (luv:gpu-request-error-details condition))))
  (let ((device (make-instance 'descriptor-probe-device))
        (dotted-usage (cons :vertex :copy-dst))
        (circular-size (list 16 8)))
    (setf (cddr circular-size) circular-size)
    (ok (eq :invalid-buffer-usage
            (gpu-request-reason
             (lambda ()
               (luv:create
                device (luv:make-buffer-descriptor
                        :size 4 :usage dotted-usage))))))
    (ok (eq :invalid-texture-size
            (gpu-request-reason
             (lambda ()
               (luv:create
                device (luv:make-texture-descriptor
                        :size circular-size :dimensions :2d :format :r8-unorm
                        :usage :texture-binding))))))))

(deftest native-retirement-preserves-a-strict-fifo-retry-barrier
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
    (ok (equal '(:view) events))
    (ok (typep warning 'luv:gpu-native-retirement-warning))
    (ok (typep warning 'luv:gpu-native-retirement-condition))
    (ok (eq :test (luv:gpu-native-retirement-operation warning)))
    (ok (= 1 (length (luv:gpu-native-retirement-failures warning))))
    (ok (eq :view
            (luv:gpu-native-retirement-failure-resource
             (first (luv:gpu-native-retirement-failures warning)))))
    (ok (= 1
           (luv:gpu-native-retirement-failure-attempts
            (first (luv:gpu-native-retirement-failures warning)))))
    (ok (search "view retirement failed"
                (princ-to-string
                 (luv:gpu-native-retirement-failure-cause
                  (first (luv:gpu-native-retirement-failures warning))))))
    (ok (search "retained the failed resource and its FIFO successors"
                (princ-to-string warning)))
    (ok (equal '(:view :texture :future)
               (mapcar #'luv::gpu-retirement-entry-resource
                       (luv::gpu-retirement-ledger-entries ledger))))
    (ok (signals
         (luv::ensure-gpu-retirement-ledger-empty
          ledger :operation :test-device-destroy)
         'luv::gpu-native-retirement-error))
    (setf events nil)
    (luv::maintain-gpu-retirement-ledger ledger 2 :operation :test-retry)
    (ok (equal '(:view :texture :future) events))
    (ok (null (luv::gpu-retirement-ledger-entries ledger)))
    (ok (eq ledger (luv::ensure-gpu-retirement-ledger-empty ledger)))))

(deftest native-retirement-detaches-callback-enqueues-without-losing-order
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
       (ok (signals
            (luv::ensure-gpu-retirement-ledger-empty ledger)
            'luv:gpu-native-retirement-error))))
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
    (ok (equal '(:second :first) events))
    ;; The older failure stays before work enqueued by a callback, and the
    ;; callback child is deliberately not visited in the same maintenance pass.
    (ok (equal '(:second :callback-child)
               (mapcar #'luv::gpu-retirement-entry-resource
                       (luv::gpu-retirement-ledger-entries ledger))))
    (setf events nil)
    (luv::maintain-gpu-retirement-ledger ledger 0)
    (ok (equal '(:callback-child :second) events))
    (ok (null (luv::gpu-retirement-ledger-entries ledger)))))

(deftest native-retirement-sequences-resume-after-the-last-successful-step
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
    (ok (= 1 first-attempts))
    (ok (= 1 second-attempts))
    (ok (zerop third-attempts))
    (ok (zerop follower-attempts))
    (luv::maintain-gpu-retirement-ledger ledger 0)
    ;; Retry resumes at step two; the successful destructive step is not run
    ;; twice, and the follower runs only after the whole owner retires.
    (ok (= 1 first-attempts))
    (ok (= 2 second-attempts))
    (ok (= 1 third-attempts))
    (ok (= 1 follower-attempts))
    (ok (null (luv::gpu-retirement-ledger-entries ledger)))))

(deftest finalizer-native-retirement-remains-durably-retryable
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
    (ok (= 1 attempts))
    (ok (equal '(:leaked-owner)
               (mapcar #'luv::gpu-retirement-entry-resource
                       (luv::gpu-retirement-ledger-entries
                        luv::*gpu-finalizer-retirement-ledger*))))
    (luv::maintain-gpu-finalizer-retirements)
    (ok (= 2 attempts))
    (ok (null
         (luv::gpu-retirement-ledger-entries
          luv::*gpu-finalizer-retirement-ledger*)))))

(deftest native-retirement-transfer-precedes-invalidation
  (let ((ledger (luv::make-gpu-retirement-ledger))
        (invalidated-p nil))
    (luv::transfer-gpu-retirement
     ledger :resource 0 (lambda () nil)
     (lambda ()
       (ok (eq :resource
               (luv::gpu-retirement-entry-resource
                (first (luv::gpu-retirement-ledger-entries ledger)))))
       (setf invalidated-p t)))
    (ok invalidated-p))
  (let ((invalidated-p nil)
        (condition nil))
    (handler-case
        (luv::perform-gpu-retirement-directly
         :resource
         (lambda () (error "native teardown failed"))
         (lambda () (setf invalidated-p t)))
      (luv:gpu-native-retirement-error (error)
        (setf condition error)))
    (ok (typep condition 'luv:gpu-native-retirement-error))
    (ok (eq :destroy (luv:gpu-native-retirement-operation condition)))
    (ok (= 1 (length (luv:gpu-native-retirement-failures condition))))
    (ok (search "native teardown failed" (princ-to-string condition)))
    (ok (not invalidated-p))))

(deftest native-retirement-worker-start-failure-cannot-split-custody
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
           (ok invalidated-p)
           (ok (= 1 (length
                     (luv::gpu-retirement-ledger-entries ledger))))
           (ok (eq custodian
                   (gethash
                    ledger luv::*gpu-retirement-ledger-custodians*)))
           (ok (search
                "injected worker creation failure"
                (princ-to-string
                 luv::*gpu-retirement-custodian-service-start-error*))))
      (setf (symbol-function spawn-symbol) original-spawn)
      (luv::release-gpu-retirement-ledger-custodian
       ledger custodian))))

(deftest native-retirement-service-worker-is-ephemeral
  (let ((thread (luv::spawn-gpu-retirement-custodian-service-thread)))
    (ok (sb-thread:thread-ephemeral-p thread))
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
    (ok (every #'identity
               (mapcar #'sb-thread:join-thread creators)))))
