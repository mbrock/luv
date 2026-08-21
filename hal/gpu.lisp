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

(defstruct gpu-retirement-entry
  "One native teardown durably owned by a GPU queue.

READY-AFTER is the queue completion frontier which makes TEARDOWN safe.
RESOURCE is retained for diagnosis and, for current backends, also supplies
the native handles used by TEARDOWN."
  resource
  (ready-after 0 :type (unsigned-byte 64))
  teardown
  (attempts 0 :type fixnum)
  last-error)

(defstruct (gpu-native-retirement-failure
            (:constructor make-gpu-native-retirement-failure
                (&key resource cause attempts)))
  (resource nil :read-only t)
  (cause nil :read-only t)
  (attempts 0 :type fixnum :read-only t))

(define-condition gpu-native-retirement-condition ()
  ((operation
    :initarg :operation
    :reader gpu-native-retirement-operation)
   (failures
    :initarg :failures
    :reader gpu-native-retirement-failures))
  (:documentation
   "Structured evidence that native GPU ownership could not be retired."))

(define-condition gpu-native-retirement-warning
    (gpu-native-retirement-condition warning) ()
  (:report
   (lambda (condition stream)
     (let ((failures (gpu-native-retirement-failures condition)))
       (format stream
               "~D native GPU retirement~:P failed during ~S; ~
the retirement ledger retained the failed resource and its FIFO successors ~
for retry."
               (length failures)
               (gpu-native-retirement-operation condition))))))

(define-condition gpu-native-retirement-error
    (gpu-native-retirement-condition error) ()
  (:report
   (lambda (condition stream)
     (let* ((failures (gpu-native-retirement-failures condition))
            (first (first failures)))
       (format stream
               "Cannot complete ~S: ~D native GPU retirement~:P remain"
               (gpu-native-retirement-operation condition)
               (length failures))
       (when first
         (format stream " (first failure for ~S: ~A)"
                 (gpu-native-retirement-failure-resource first)
                 (gpu-native-retirement-failure-cause first)))
       (write-char #\. stream)))))

(defstruct gpu-retirement-ledger
  "A queue-owned list of native teardowns, including failed attempts."
  (entries '() :type list)
  tail
  (active-batch '() :type list))

(defvar *gpu-retirement-ledger-custodians* (make-hash-table :test #'eq)
  "Process roots for queues with native retirement or submitted work.")

(defvar *gpu-retirement-ledger-custodian-lock*
  (sb-thread:make-mutex :name "luv GPU retirement custodians")
  "Serializes durable queue roots without entering backend queue locks.")

(defvar *gpu-retirement-custodian-service-wake*
  (sb-thread:make-waitqueue :name "luv GPU retirement custodian service")
  "Wakes the lazy process-wide retirement custodian service thread.")

(defvar *gpu-retirement-custodian-service-thread* nil
  "The lazy process-wide retirement custodian service thread, or NIL.")

(defvar *gpu-retirement-custodian-service-start-error* nil
  "Newest best-effort worker start error; queue custody remains unaffected.")

(defvar *gpu-retirement-custodian-process-exiting-p* nil
  "True once SBCL has begun orderly process shutdown.")

(defvar *gpu-retirement-custodian-service-enabled-p* t
  "Whether retaining a queue custodian starts eventual background service.")

(defvar *gpu-retirement-custodian-service-failures*
  (make-hash-table :test #'eq)
  "Consecutive service failures per custodian, for bounded diagnostics.")

(defparameter *gpu-retirement-custodian-service-minimum-delay* 0.01)
(defparameter *gpu-retirement-custodian-service-maximum-delay* 1.0)

(defun note-gpu-retirement-custodian-process-exit ()
  ;; SBCL terminates and joins ephemeral threads while holding its private
  ;; thread-creation lock.  Tell the worker's unwind cleanup not to fight that
  ;; orderly shutdown by trying to restart itself under the same lock.
  (setf *gpu-retirement-custodian-process-exiting-p* t))

(pushnew 'note-gpu-retirement-custodian-process-exit sb-ext:*exit-hooks*)

(defgeneric service-gpu-retirement-custodian (custodian)
  (:documentation
   "Perform one safe eventual-retirement service pass for CUSTODIAN.

Backend methods revalidate native liveness while holding their queue lock and
return true when the pass made progress.  They must never acquire a queue lock
while the process-wide custodian registry lock is held."))

(defmethod service-gpu-retirement-custodian ((custodian t))
  (declare (ignore custodian))
  nil)

(defun gpu-retirement-custodian-snapshot ()
  "Return a strong snapshot without retaining the registry lock."
  (sb-thread:with-mutex (*gpu-retirement-ledger-custodian-lock*)
    (loop for custodian being the hash-values
            of *gpu-retirement-ledger-custodians*
          collect custodian)))

(defun note-gpu-retirement-custodian-service-failure (custodian cause)
  "Record CAUSE and warn only on the first consecutive service failure."
  (let ((attempts
          (sb-thread:with-mutex (*gpu-retirement-ledger-custodian-lock*)
            ;; A device teardown can prove quiescence after SERVICE releases
            ;; its queue lock but before this handler runs.  Do not resurrect
            ;; the custodian through the diagnostic table in that race.
            (when (loop for registered being the hash-values
                          of *gpu-retirement-ledger-custodians*
                        thereis (eq registered custodian))
              (incf (gethash custodian
                             *gpu-retirement-custodian-service-failures*
                             0))))))
    (when (eql attempts 1)
      ;; A hostile warning handler must not kill the service worker.  The queue
      ;; remains rooted before, during, and after this diagnostic.
      (handler-case
          (warn 'gpu-native-retirement-warning
                :operation :service-gpu-retirement-custodian
                :failures
                (list
                 (make-gpu-native-retirement-failure
                  :resource custodian :cause cause :attempts attempts)))
        (serious-condition () nil))))
  nil)

(defun service-gpu-retirement-custodians-once ()
  "Service one registry snapshot without holding the registry lock.

Returns true when any backend reports progress.  Errors are diagnosed once per
consecutive failure and never prevent later custodians from being serviced."
  (let ((progress-p nil))
    (dolist (custodian (gpu-retirement-custodian-snapshot) progress-p)
      (handler-case
          (let ((progress (service-gpu-retirement-custodian custodian)))
            (when progress
              (setf progress-p t))
            (sb-thread:with-mutex
                (*gpu-retirement-ledger-custodian-lock*)
              (remhash custodian
                       *gpu-retirement-custodian-service-failures*)))
        (serious-condition (cause)
          (note-gpu-retirement-custodian-service-failure
           custodian cause))))))

(declaim (ftype (function () *) run-gpu-retirement-custodian-service))

(defun spawn-gpu-retirement-custodian-service-thread ()
  ;; This process custodian is runtime infrastructure, like SBCL's finalizer
  ;; and timer workers: durable ownership lives in the registry, while the
  ;; worker must not keep a noninteractive Lisp alive after its main thread
  ;; exits (especially when a failed test deliberately leaves work pending).
  ;; MAKE-SYSTEM-THREAD is private because callers must serialize it with
  ;; SBCL's thread-creation mutex; START-THREAD intentionally omits that lock
  ;; for ephemeral threads under the assumption that it is already held.
  (sb-thread::call-with-system-mutex
   (lambda ()
     (sb-thread::make-system-thread
      "luv GPU retirement service"
      #'run-gpu-retirement-custodian-service
      nil nil))
   sb-thread::*make-thread-lock*))

(defun start-gpu-retirement-custodian-service-locked ()
  "Start the singleton service worker.  The caller holds the registry lock."
  (when (and (not *gpu-retirement-custodian-process-exiting-p*)
             *gpu-retirement-custodian-service-enabled-p*
             (plusp (hash-table-count
                     *gpu-retirement-ledger-custodians*))
             (not (and *gpu-retirement-custodian-service-thread*
                       (sb-thread:thread-alive-p
                        *gpu-retirement-custodian-service-thread*))))
    (setf *gpu-retirement-custodian-service-thread*
          (spawn-gpu-retirement-custodian-service-thread)
          *gpu-retirement-custodian-service-start-error* nil))
  *gpu-retirement-custodian-service-thread*)

(defun try-start-gpu-retirement-custodian-service-locked ()
  "Best-effort worker start which cannot revoke already published custody."
  (handler-case
      (start-gpu-retirement-custodian-service-locked)
    (serious-condition (cause)
      (setf *gpu-retirement-custodian-service-start-error* cause)
      nil)))

(defun run-gpu-retirement-custodian-service ()
  "Eventually service rooted queues, exiting atomically when none remain."
  (let ((delay *gpu-retirement-custodian-service-minimum-delay*)
        (current sb-thread:*current-thread*))
    (unwind-protect
         (loop
           (let ((progress-p
                   (service-gpu-retirement-custodians-once)))
             (sb-thread:with-mutex
                 (*gpu-retirement-ledger-custodian-lock*)
               (when (zerop (hash-table-count
                             *gpu-retirement-ledger-custodians*))
                 (when (eq current
                           *gpu-retirement-custodian-service-thread*)
                   (setf *gpu-retirement-custodian-service-thread* nil))
                 (return))
               ;; CONDITION-WAIT releases the registry lock.  No queue lock is
               ;; ever taken while this process-wide lock is held.
               (sb-thread:condition-wait
                *gpu-retirement-custodian-service-wake*
                *gpu-retirement-ledger-custodian-lock*
                :timeout delay))
             (setf delay
                   (if progress-p
                       *gpu-retirement-custodian-service-minimum-delay*
                       (min *gpu-retirement-custodian-service-maximum-delay*
                            (* 2 delay))))))
      (sb-thread:with-mutex (*gpu-retirement-ledger-custodian-lock*)
        (when (eq current *gpu-retirement-custodian-service-thread*)
          (setf *gpu-retirement-custodian-service-thread* nil))
        ;; An unexpected worker exit must not strand a newly retained queue.
        (unless *gpu-retirement-custodian-process-exiting-p*
          (try-start-gpu-retirement-custodian-service-locked))))))

(defun retain-gpu-retirement-ledger-custodian (ledger custodian)
  "Root CUSTODIAN until its ledger and backend submissions are quiescent."
  (sb-thread:with-mutex (*gpu-retirement-ledger-custodian-lock*)
    (setf (gethash ledger *gpu-retirement-ledger-custodians*) custodian)
    ;; The hash publication is the ownership transfer.  Thread creation and
    ;; wakeup are liveness accelerators and must not leave the public wrapper
    ;; valid after its teardown was already enqueued.
    (try-start-gpu-retirement-custodian-service-locked)
    (handler-case
        (sb-thread:condition-notify
         *gpu-retirement-custodian-service-wake*)
      (serious-condition (cause)
        (setf *gpu-retirement-custodian-service-start-error* cause))))
  ledger)

(defun release-gpu-retirement-ledger-custodian (ledger custodian)
  "Drop CUSTODIAN's root after backend quiescence was proved under its lock."
  (sb-thread:with-mutex (*gpu-retirement-ledger-custodian-lock*)
    (when (eq custodian
              (gethash ledger *gpu-retirement-ledger-custodians*))
      (remhash ledger *gpu-retirement-ledger-custodians*)
      (remhash custodian *gpu-retirement-custodian-service-failures*)
      (sb-thread:condition-notify *gpu-retirement-custodian-service-wake*)))
  ledger)

(defun enqueue-gpu-retirement
    (ledger resource ready-after teardown)
  "Transfer RESOURCE's native teardown to LEDGER and return its entry.

The caller holds its queue lock.  This append must happen before the public
wrapper is marked destroyed or its leak finalizer is cancelled."
  (check-type ready-after (unsigned-byte 64))
  (check-type teardown function)
  (let* ((entry (make-gpu-retirement-entry
                 :resource resource
                 :ready-after ready-after
                 :teardown teardown))
         (cell (list entry)))
    (if (gpu-retirement-ledger-tail ledger)
        (setf (cdr (gpu-retirement-ledger-tail ledger)) cell
              (gpu-retirement-ledger-tail ledger) cell)
        (setf (gpu-retirement-ledger-entries ledger) cell
              (gpu-retirement-ledger-tail ledger) cell))
    entry))

(defun transfer-gpu-retirement
    (ledger resource ready-after teardown invalidate &optional custodian)
  "Durably enqueue native ownership, then logically invalidate RESOURCE.

INVALIDATE is called only after LEDGER owns the complete teardown.  Queue
implementations call this while holding the lock which also guards LEDGER."
  (let ((entry
          (enqueue-gpu-retirement
           ledger resource ready-after teardown)))
    (when custodian
      (retain-gpu-retirement-ledger-custodian ledger custodian))
    (funcall invalidate)
    entry))

(defun gpu-retirement-failure-for-entry (entry)
  (make-gpu-native-retirement-failure
   :resource (gpu-retirement-entry-resource entry)
   :cause (gpu-retirement-entry-last-error entry)
   :attempts (gpu-retirement-entry-attempts entry)))

(defun gpu-retirement-ledger-failures (ledger)
  "Describe every currently retained failed entry in LEDGER."
  (loop for entry in (gpu-retirement-ledger-entries ledger)
        when (gpu-retirement-entry-last-error entry)
          collect (gpu-retirement-failure-for-entry entry)))

(defun maintain-gpu-retirement-ledger
    (ledger completed-frontier &key (operation :maintain-queue))
  "Attempt the eligible FIFO prefix in LEDGER at COMPLETED-FRONTIER.

Only a successful eligible prefix leaves the ledger.  The first ineligible or
failed entry is a FIFO barrier: it and the entire unattempted suffix remain
ahead of ownership transferred recursively by teardown callbacks."
  (check-type completed-frontier (unsigned-byte 64))
  ;; A teardown callback can recursively destroy another resource and enter
  ;; queue maintenance through the same recursive backend lock.  The outer
  ;; batch owns the ordering frontier; leave newly queued work for its next
  ;; pass rather than letting recursive maintenance overtake it.
  (when (gpu-retirement-ledger-active-batch ledger)
    (return-from maintain-gpu-retirement-ledger (values ledger nil)))
  (let ((pending (gpu-retirement-ledger-entries ledger))
        (pending-tail (gpu-retirement-ledger-tail ledger))
        (retained-head nil)
        (merged-p nil)
        (failures '()))
    ;; Teardown callbacks run under a backend's recursive queue lock and may
    ;; themselves destroy another resource.  Detach this maintenance batch so
    ;; such transfers enter a fresh queue and cannot be visited or overwritten
    ;; by the traversal below.
    (setf (gpu-retirement-ledger-entries ledger) nil
          (gpu-retirement-ledger-tail ledger) nil
          (gpu-retirement-ledger-active-batch ledger) pending)
    (labels ((merge-retained-batch ()
               ;; The blocked suffix retains its existing cons cells and
               ;; original tail, then precedes callback-enqueued ownership.
               (when retained-head
                 (setf (cdr pending-tail)
                       (gpu-retirement-ledger-entries ledger)
                       (gpu-retirement-ledger-entries ledger) retained-head
                       (gpu-retirement-ledger-tail ledger)
                       (or (gpu-retirement-ledger-tail ledger) pending-tail)))
               (setf merged-p t)))
      (unwind-protect
           (progn
             (loop while pending
                   for entry = (first pending)
                   do (cond
                        ((> (gpu-retirement-entry-ready-after entry)
                            completed-frontier)
                         (setf retained-head pending)
                         (loop-finish))
                        (t
                         (incf (gpu-retirement-entry-attempts entry))
                         (handler-case
                             (progn
                               (funcall
                                (gpu-retirement-entry-teardown entry))
                               (setf (gpu-retirement-entry-last-error entry) nil
                                     pending (rest pending)
                                     (gpu-retirement-ledger-active-batch ledger)
                                     pending))
                           (error (cause)
                             (setf (gpu-retirement-entry-last-error entry) cause
                                   retained-head pending
                                   failures
                                   (list
                                    (make-gpu-native-retirement-failure
                                     :resource
                                     (gpu-retirement-entry-resource entry)
                                     :cause cause
                                     :attempts
                                     (gpu-retirement-entry-attempts entry))))
                             (loop-finish))))))
             (merge-retained-batch))
        ;; Preserve the detached suffix even across an unexpected non-local
        ;; exit from a callback.  Backend locks still delimit all mutation.
        (unless merged-p
          (setf retained-head
                (or retained-head
                    (gpu-retirement-ledger-active-batch ledger)))
          (merge-retained-batch))
        (setf (gpu-retirement-ledger-active-batch ledger) nil)))
    ;; Eventual service polls independently of callers.  Surface the first
    ;; failed attempt, but do not emit one warning per poll forever; the ledger
    ;; retains the cause and attempt count for explicit inspection/teardown.
    (when (and failures
               (some (lambda (failure)
                       (= 1 (gpu-native-retirement-failure-attempts failure)))
                     failures))
      (warn 'gpu-native-retirement-warning
            :operation operation :failures failures))
    (values ledger failures)))

(defun perform-gpu-retirement-directly
    (resource teardown invalidate &key (operation :destroy))
  "Perform TEARDOWN without a live queue, then invalidate RESOURCE.

An error leaves the wrapper and its leak finalizer live and is re-signaled as
a structured GPU-NATIVE-RETIREMENT-ERROR retaining the original cause."
  (let ((entry (make-gpu-retirement-entry
                :resource resource :teardown teardown)))
    (handler-case
        (progn
          (incf (gpu-retirement-entry-attempts entry))
          (funcall teardown))
      (error (cause)
        (setf (gpu-retirement-entry-last-error entry) cause)
        (error 'gpu-native-retirement-error
               :operation operation
               :failures
               (list (make-gpu-native-retirement-failure
                      :resource resource :cause cause
                      :attempts (gpu-retirement-entry-attempts entry))))))
    (funcall invalidate)
    resource))

(defun ensure-gpu-retirement-ledger-empty
    (ledger &key (operation :destroy-device))
  "Refuse owner teardown while LEDGER still owns any native resources."
  (let ((entries
          (append (copy-list (gpu-retirement-ledger-active-batch ledger))
                  (gpu-retirement-ledger-entries ledger))))
    (when entries
      (error 'gpu-native-retirement-error
             :operation operation
             :failures
             (loop for entry in entries
                   collect (gpu-retirement-failure-for-entry entry)))))
  ledger)

(defun make-gpu-retirement-sequence (&rest steps)
  "Return a retryable closure over ordered, individually idempotent STEPS.

Each zero-argument step is removed only after it returns normally.  A retry
therefore resumes at the failing native call without repeating any earlier
destructive call which already succeeded."
  (dolist (step steps)
    (check-type step function))
  (let ((remaining (copy-list steps)))
    (lambda ()
      (loop while remaining
            do (funcall (first remaining))
               (pop remaining))
      (values))))

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

(defclass gpu-temporal-scaler (gpu-object)
  ((input-size
    :initarg :input-size
    :reader gpu-temporal-scaler-input-size)
   (output-size
    :initarg :output-size
    :reader gpu-temporal-scaler-output-size)
   (color-usage
    :initarg :color-usage
    :reader gpu-temporal-scaler-color-usage)
   (depth-usage
    :initarg :depth-usage
    :reader gpu-temporal-scaler-depth-usage)
   (motion-usage
    :initarg :motion-usage
    :reader gpu-temporal-scaler-motion-usage)
   (output-usage
    :initarg :output-usage
    :reader gpu-temporal-scaler-output-usage))
  (:documentation
   "A retained temporal reconstruction owner and its exact texture contract."))

(defgeneric retire-gpu-native-owner (device owner teardown invalidate)
  (:documentation
   "Transfer OWNER's native teardown to DEVICE, then invalidate OWNER.

Backend methods may transfer ownership to a durable device queue before
INVALIDATE and attempt it immediately.  The default has no such queue: native
TEARDOWN must return successfully before INVALIDATE is called."))

(defmethod retire-gpu-native-owner
    ((device t) owner teardown invalidate)
  (declare (ignore device))
  (perform-gpu-retirement-directly
   owner teardown invalidate :operation :retire-gpu-native-owner))

(defvar *gpu-finalizer-retirement-ledger* (make-gpu-retirement-ledger)
  "Process-local durable ownership for native teardown abandoned by GC.")

(defvar *gpu-finalizer-retirement-lock*
  (sb-thread:make-mutex :name "luv GPU finalizer retirement")
  "Serializes the process-local finalizer retirement ledger.")

(defun maintain-gpu-finalizer-retirements ()
  "Retry the process-local FIFO of native ownership recovered by finalizers."
  (sb-thread:with-recursive-lock (*gpu-finalizer-retirement-lock*)
    (maintain-gpu-retirement-ledger
     *gpu-finalizer-retirement-ledger* 0
     :operation :maintain-gpu-finalizer-retirements)))

(defun retire-gpu-finalizer-native-owner (device owner teardown)
  "Durably route a finalizer's OWNER through DEVICE or the fallback ledger.

The fallback ledger takes ownership before any attempt.  Its retryable routing
step records when a backend queue has accepted OWNER, so a warning promoted to
an error cannot cause a later retry to transfer the same native owner twice."
  (check-type teardown function)
  (let ((routed-p nil))
    (sb-thread:with-recursive-lock (*gpu-finalizer-retirement-lock*)
      (transfer-gpu-retirement
       *gpu-finalizer-retirement-ledger* owner 0
       (lambda ()
         (unless routed-p
           (retire-gpu-native-owner
            device owner teardown (lambda () (setf routed-p t)))))
       (lambda () nil))
      (maintain-gpu-finalizer-retirements)))
  (values))

(defgeneric request-gpu-device (provider &optional descriptor))

(defgeneric device-queue (device)
  (:documentation "Return the default queue belonging to DEVICE."))

(defgeneric create (device descriptor)
  (:documentation "Asks the DEVICE for a handle to newly created instance
of some object fulfilling the DESCRIPTOR."))

(defgeneric adopt-native-texture (device native-object owner descriptor)
  (:documentation "Wrap a platform texture and its retained OWNER in the HAL."))

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

(defparameter +portable-buffer-usages+
  '(:uniform :storage :vertex :index :copy-dst))

(defparameter +portable-texture-usages+
  '(:copy-src :copy-dst :storage-binding :texture-binding
    :render-attachment))

(defun reject-portable-gpu-descriptor
    (operation descriptor reason details)
  (error 'gpu-request-error
         :operation operation
         :descriptor descriptor
         :reason reason
         :details details))

(defun portable-sequence-list (value)
  "Copy a proper list or vector VALUE, returning NIL for malformed lists."
  (typecase value
    (vector (coerce value 'list))
    (list
     (handler-case
         (and (list-length value) (copy-list value))
       (type-error () nil)))
    (otherwise nil)))

(defun canonical-gpu-usage-list
    (usage permitted descriptor reason operation)
  "Return portable USAGE syntax as one stable, duplicate-free keyword list."
  (let ((usages
          (if (keywordp usage)
              (list usage)
              (portable-sequence-list usage))))
    (unless (and usages
                 (every (lambda (value) (member value permitted)) usages))
      (reject-portable-gpu-descriptor
       operation descriptor reason usage))
    ;; Preserve the caller's first occurrence: usage order is not semantic,
    ;; but stable normalization makes descriptors pleasant to inspect.
    (remove-duplicates usages :test #'eq :from-end t)))

(defun canonical-texture-extent
    (size descriptor &optional (operation :create))
  "Return the current portable two-dimensional SIZE contract as (W H 1)."
  (let ((components (portable-sequence-list size)))
    (unless (and (member (length components) '(2 3))
                 (every (lambda (value)
                          (and (integerp value) (plusp value)))
                        components)
                 (or (= 2 (length components))
                     (= 1 (third components))))
      (reject-portable-gpu-descriptor
       operation descriptor :invalid-texture-size size))
    (list (first components) (second components) 1)))

(defun canonical-buffer-descriptor
    (descriptor &optional (operation :create))
  "Copy DESCRIPTOR into the one structural shape every backend receives."
  (let ((size (buffer-descriptor-size descriptor)))
    (unless (and (typep size '(unsigned-byte 64)) (plusp size))
      (reject-portable-gpu-descriptor
       operation descriptor :invalid-buffer-size size))
    (let ((canonical (copy-buffer-descriptor descriptor)))
      (setf (buffer-descriptor-usage canonical)
            (canonical-gpu-usage-list
             (buffer-descriptor-usage descriptor)
             +portable-buffer-usages+ descriptor :invalid-buffer-usage
             operation))
      canonical)))

(defun canonical-texture-descriptor
    (descriptor &optional (operation :create))
  "Copy DESCRIPTOR into the portable two-dimensional texture contract."
  (unless (eq :2d (texture-descriptor-dimensions descriptor))
    (reject-portable-gpu-descriptor
     operation descriptor :invalid-texture-dimensions
     (texture-descriptor-dimensions descriptor)))
  (let ((canonical (copy-texture-descriptor descriptor)))
    (setf (texture-descriptor-size canonical)
          (canonical-texture-extent
           (texture-descriptor-size descriptor) descriptor operation)
          (texture-descriptor-usage canonical)
          (canonical-gpu-usage-list
           (texture-descriptor-usage descriptor)
           +portable-texture-usages+ descriptor :invalid-texture-usage
           operation))
    canonical))

(defmethod create :around
    ((device gpu-device) (descriptor buffer-descriptor))
  (call-next-method device (canonical-buffer-descriptor descriptor)))

(defmethod create :around
    ((device gpu-device) (descriptor texture-descriptor))
  (call-next-method device (canonical-texture-descriptor descriptor)))

(defmethod adopt-native-texture :around
    ((device gpu-device) native owner (descriptor texture-descriptor))
  (call-next-method
   device native owner
   (canonical-texture-descriptor descriptor :adopt-native-texture)))

(defun buffer-data-foreign-type (data)
  "Return the CFFI element type and byte size for a one-dimensional DATA
array of single-floats or unsigned 8-, 16-, 32-, or 64-bit integers."
  (let ((element-type (and (arrayp data) (= 1 (array-rank data))
                           (array-element-type data))))
    (cond ((null element-type) nil)
          ((subtypep element-type 'single-float) (values :float 4))
          ((subtypep element-type '(unsigned-byte 8)) (values :uint8 1))
          ((subtypep element-type '(unsigned-byte 16)) (values :uint16 2))
          ((subtypep element-type '(unsigned-byte 32)) (values :uint32 4))
          ((subtypep element-type '(unsigned-byte 64)) (values :uint64 8))
          (t nil))))

(defun texture-format-bytes-per-texel (format)
  "Return the exact storage size of one texel in portable FORMAT."
  (ecase format
    (:r16-float 2)
    ((:rgba8-unorm :rgba8-unorm-srgb
      :bgra8-unorm :bgra8-unorm-srgb
      :depth32-float :rg16-uint :rg16-float)
     4)
    (:rgba16-float 8)))

(defun vertex-attribute-format-component-count (format)
  "Return the scalar lane count of a portable vertex attribute FORMAT.

The vocabulary is deliberately small and float-only: it names what the
mesh and instance products this project actually writes contain, and every
backend is expected to accept all of it."
  (ecase format
    (:float32x2 2)
    (:float32x3 3)
    (:float32x4 4)))

(defun texture-format-upload-element-type (format)
  "The packed array element type accepted by WRITE-TEXTURE for FORMAT."
  (ecase (texture-format-bytes-per-texel format)
    (2 '(unsigned-byte 16))
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

(defstruct (mesh-render-pipeline-descriptor (:include gpu-descriptor))
  "A task/mesh render pipeline.

TASK may be NIL for a direct mesh dispatch.  MAX-MESH-WORKGROUPS states the
largest task-to-mesh amplification admitted by one task workgroup."
  layout task mesh fragment (max-mesh-workgroups 1) depth-stencil)

(defstruct (render-pass-descriptor (:include gpu-descriptor))
  color-attachments depth-stencil-attachment)

(defstruct (compute-pipeline-descriptor (:include gpu-descriptor))
  layout module (entry-point "main"))

(defstruct (command-encoder-descriptor (:include gpu-descriptor)))

(defstruct (temporal-scaler-descriptor (:include gpu-descriptor))
  input-size
  output-size
  (color-format :rgba16-float)
  (depth-format :depth32-float)
  (motion-format :rg16-float)
  (output-format :rgba16-float))

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

(defstruct (gpu-draw-indexed-command (:include gpu-render-pass-command))
  index-buffer
  (index-format :uint16)
  index-count
  (instance-count 1)
  (first-index 0)
  (base-vertex 0)
  (first-instance 0))

(defstruct (gpu-draw-mesh-command (:include gpu-render-pass-command))
  x
  (y 1)
  (z 1))

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

(defstruct (gpu-temporal-scale-command
             (:include gpu-command-encoder-command))
  scaler
  color
  depth
  motion
  output
  jitter
  reset-p)

(defstruct (gpu-signal-temporal-scaler-command
             (:include gpu-render-pass-command))
  scaler)

(defstruct (gpu-wait-temporal-scaler-command
             (:include gpu-render-pass-command))
  scaler)

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

(defun draw-indexed (pass-encoder index-buffer index-format index-count
                     &optional (instance-count 1) (first-index 0)
                               (base-vertex 0) (first-instance 0))
  "Draw indexed instances from INDEX-BUFFER.

INDEX-FORMAT is :UINT16 or :UINT32.  The buffer is carried by the command so
recorded work owns the complete indexed-draw dependency at one boundary."
  (encode pass-encoder
          (make-gpu-draw-indexed-command
           :index-buffer index-buffer :index-format index-format
           :index-count index-count :instance-count instance-count
           :first-index first-index :base-vertex base-vertex
           :first-instance first-instance)))

(defun draw-mesh-workgroups (pass-encoder x &optional (y 1) (z 1))
  (encode pass-encoder
          (make-gpu-draw-mesh-command :x x :y y :z z)))

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

(defun encode-temporal-scale
    (encoder scaler color depth motion output jitter reset-p)
  "Encode one temporal reconstruction from COLOR, DEPTH, and MOTION.

JITTER is the current sample offset in input-pixel units.  Motion values are
current-to-previous displacements in normalized input-texture coordinates;
the scaler's input extent binds their conversion to pixels once per frame."
  (encode encoder
          (make-gpu-temporal-scale-command
           :scaler scaler :color color :depth depth :motion motion
           :output output :jitter jitter :reset-p reset-p)))

(defun signal-temporal-scaler-inputs (pass scaler)
  "Publish PASS's attachment writes to SCALER's private synchronization."
  (encode pass (make-gpu-signal-temporal-scaler-command :scaler scaler)))

(defun wait-temporal-scaler-output (pass scaler)
  "Make PASS's fragment reads wait for SCALER's reconstructed output."
  (encode pass (make-gpu-wait-temporal-scaler-command :scaler scaler)))
