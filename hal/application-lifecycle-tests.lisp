(in-package #:luv.tests)

(define-test release-report-attempts-every-step-in-source-order
  (let ((events nil)
        (condition nil))
    (handler-case
        (luv:with-release-report
          (luv:releasing :first
            (setf events (append events '(:first)))
            (error "first release failure"))
          (luv:releasing :second
            (setf events (append events '(:second))))
          (luv:releasing :third
            (setf events (append events '(:third)))
            (error "third release failure")))
      (luv:release-error (failure)
        (setf condition failure)))
    (true (equal '(:first :second :third) events))
    (true (typep condition 'luv:release-error))
    (true (equal '(:first :third)
                 (mapcar #'luv:release-failure-name
                         (luv:release-error-failures condition))))
    (true (every (lambda (failure)
                   (typep (luv:release-failure-condition failure) 'error))
                 (luv:release-error-failures condition)))))

(define-test release-unwind-preserves-primary-and-reports-all-cleanup-failures
  (let ((primary (make-condition 'simple-error
                                 :format-control "primary body failure"))
        (caught nil)
        (warning nil)
        (events nil))
    (handler-bind
        ((luv:release-warning
           (lambda (condition)
             (setf warning condition)
             (muffle-warning condition))))
      (handler-case
          (luv:unwind-protect-releasing
              (error primary)
            (luv:releasing :one
              (setf events (append events '(:one)))
              (error "cleanup one failed"))
            (luv:releasing :two
              (setf events (append events '(:two))))
            (luv:releasing :three
              (setf events (append events '(:three)))
              (error "cleanup three failed")))
        (error (condition)
          (setf caught condition))))
    (true (eq primary caught))
    (true (equal '(:one :two :three) events))
    (true (typep warning 'luv:release-warning))
    (true (eq primary (luv:release-warning-primary-condition warning)))
    (true (equal '(:one :three)
                 (mapcar #'luv:release-failure-name
                         (luv:release-warning-failures warning))))))

(define-test normal-release-unwind-signals-cleanup-failure
  (let ((condition nil))
    (handler-case
        (luv:unwind-protect-releasing
            :body-value
          (luv:releasing :cleanup
            (error "normal cleanup failed")))
      (luv:release-error (failure)
        (setf condition failure)))
    (true (typep condition 'luv:release-error))
    (true (equal '(:cleanup)
                 (mapcar #'luv:release-failure-name
                         (luv:release-error-failures condition))))))

(define-test contained-release-failure-is-not-mislabeled-as-primary
  (let ((warning nil))
    (handler-bind
        ((luv:release-warning
           (lambda (condition)
             (setf warning condition)
             (muffle-warning condition))))
      (luv:with-release-warnings
        (luv:releasing :contained
          (error "contained release failure"))))
    (true (typep warning 'luv:release-warning))
    (true (null (luv:release-warning-primary-condition warning)))
    (true (equal '(:contained)
                 (mapcar #'luv:release-failure-name
                         (luv:release-warning-failures warning))))))

(define-test attachment-publication-finishes-before-a-racing-stop-can-begin
  (let* ((controller (luv:make-stop-controller :name "attachment race"))
         (publication-entered (sb-thread:make-semaphore :count 0))
         (continue-publication (sb-thread:make-semaphore :count 0))
         (stop-finished (sb-thread:make-semaphore :count 0))
         (events nil)
         (publication-thread
           (sb-thread:make-thread
            (lambda ()
              (luv:call-with-running-stop-controller
               controller
               (lambda ()
                 (push :publication-entered events)
                 (sb-thread:signal-semaphore publication-entered)
                 (unless (sb-thread:wait-on-semaphore
                          continue-publication :timeout 1.0)
                   (error "The attachment publication was not released."))
                 (push :publication-finished events)
                 :attached))))))
    (unless (sb-thread:wait-on-semaphore publication-entered :timeout 1.0)
      (error "The attachment publication did not begin."))
    (let ((stop-thread
            (sb-thread:make-thread
             (lambda ()
               (unwind-protect
                    (luv:call-with-stop-controller
                     controller
                     (lambda () (push :stop events)))
                 (sb-thread:signal-semaphore stop-finished))))))
      (sb-thread:signal-semaphore continue-publication)
      (true (eq :attached (sb-thread:join-thread publication-thread)))
      (unless (sb-thread:wait-on-semaphore stop-finished :timeout 1.0)
        (error "The racing stop did not finish."))
      (sb-thread:join-thread stop-thread))
    (true (equal '(:publication-entered :publication-finished :stop)
                 (nreverse events)))
    (true (eq :stopped (luv:stop-controller-state controller)))))

(define-test stopping-controller-rejects-publication-without-running-it
  (let* ((controller (luv:make-stop-controller :name "closed attachment"))
         (stop-entered (sb-thread:make-semaphore :count 0))
         (continue-stop (sb-thread:make-semaphore :count 0))
         (attachment (list :attachment))
         (called-p nil)
         (condition nil)
         (stop-thread
           (sb-thread:make-thread
            (lambda ()
              (luv:call-with-stop-controller
               controller
               (lambda ()
                 (sb-thread:signal-semaphore stop-entered)
                 (unless (sb-thread:wait-on-semaphore continue-stop :timeout 1.0)
                   (error "The attachment rejection stop was not released."))))))))
    (unless (sb-thread:wait-on-semaphore stop-entered :timeout 1.0)
      (error "The attachment rejection stop did not begin."))
    (unwind-protect
         (handler-case
             (luv:call-with-running-stop-controller
              controller (lambda () (setf called-p t))
              :attachment attachment)
           (luv:application-attachment-closed (failure)
             (setf condition failure)))
      (sb-thread:signal-semaphore continue-stop)
      (sb-thread:join-thread stop-thread))
    (true (not called-p))
    (true (typep condition 'luv:application-attachment-closed))
    (true (eq controller
              (luv:application-attachment-closed-controller condition)))
    (true (eq attachment
              (luv:application-attachment-closed-attachment condition)))
    (true (eq :stopping
              (luv:application-attachment-closed-state condition)))))

(define-test terminal-duplicate-attachment-is-an-idempotent-success
  (let* ((controller (luv:make-stop-controller :name "duplicate attachment"))
         (attachment (list :attachment))
         (publication-calls 0)
         (membership-calls 0))
    (luv:call-with-stop-controller controller (lambda () nil))
    (true (eq attachment
              (luv:call-with-running-stop-controller
               controller (lambda () (incf publication-calls))
               :attachment attachment
               :already-attached-p
               (lambda ()
                 (incf membership-calls)
                 t))))
    (true (zerop publication-calls))
    (true (= 1 membership-calls))))

(define-test attachment-rejection-is-signalled-outside-the-controller-lock
  (let* ((controller (luv:make-stop-controller :name "reentrant rejection"))
         (condition nil)
         (observed-state nil))
    (luv:call-with-stop-controller controller (lambda () nil))
    (handler-case
        (luv:call-with-running-stop-controller
         controller (lambda () :wrong) :attachment :late)
      (luv:application-attachment-closed (failure)
        (setf condition failure
              ;; This takes the same mutex.  It can complete only when the
              ;; semantic rejection was signalled after leaving the gate.
              observed-state (luv:stop-controller-state controller))))
    (true (typep condition 'luv:application-attachment-closed))
    (true (eq :stopped observed-state))))

(define-test stop-controller-runs-one-owner-and-publishes-values
  (let* ((controller (luv:make-stop-controller :name "concurrency probe"))
         (started (sb-thread:make-semaphore :count 0))
         (continue (sb-thread:make-semaphore :count 0))
         (calls 0)
         (owner
           (sb-thread:make-thread
            (lambda ()
              (multiple-value-list
               (luv:call-with-stop-controller
                controller
                (lambda ()
                  (incf calls)
                  (sb-thread:signal-semaphore started)
                  (unless (sb-thread:wait-on-semaphore continue :timeout 1.0)
                    (error "stop-controller test owner was not released"))
                  (values :stopped 42))))))))
    (unless (sb-thread:wait-on-semaphore started :timeout 1.0)
      (error "stop-controller test owner did not start"))
    (let ((waiter
            (sb-thread:make-thread
             (lambda ()
               (multiple-value-list
                (luv:call-with-stop-controller
                 controller
                 (lambda ()
                   (error "a concurrent stop body ran twice"))))))))
      (sb-thread:signal-semaphore continue)
      (true (equal '(:stopped 42) (sb-thread:join-thread owner)))
      (true (equal '(:stopped 42) (sb-thread:join-thread waiter))))
    (true (equal '(:stopped 42)
                 (multiple-value-list
                  (luv:call-with-stop-controller
                   controller
                   (lambda () (error "a completed stop body ran twice"))))))
    (true (= 1 calls))
    (true (eq :stopped (luv:stop-controller-state controller)))
    (true (equal '(:stopped 42)
                 (luv:stop-controller-result-values controller)))
    (true (null (luv:stop-controller-condition controller)))))

(define-test stop-controller-publishes-one-failure-to-every-caller
  (let* ((controller (luv:make-stop-controller :name "failure probe"))
         (primary (make-condition 'simple-error
                                  :format-control "teardown failed"))
         (calls 0)
         (first
           (handler-case
               (luv:call-with-stop-controller
                controller
                (lambda ()
                  (incf calls)
                  (error primary)))
             (error (condition) condition)))
         (second
           (handler-case
               (luv:call-with-stop-controller
                controller
                (lambda ()
                  (incf calls)
                  :wrong-result))
             (error (condition) condition))))
    (true (eq primary first))
    (true (eq primary second))
    (true (eq primary (luv:stop-controller-condition controller)))
    (true (= 1 calls))))

(define-test requested-stop-publishes-its-exact-failure
  (let* ((controller (luv:make-stop-controller :name "async failure probe"))
         (primary (make-condition 'simple-error
                                  :format-control "async teardown failed"))
         (calls 0))
    (multiple-value-bind (began-p worker)
        (luv:request-controlled-stop
         controller
         (lambda ()
           (incf calls)
           (error primary))
         :thread-name "stop-controller failing test worker")
      (true began-p)
      (true (typep worker 'sb-thread:thread))
      (sb-thread:join-thread worker))
    (let ((published
            (handler-case
                (luv:wait-for-controlled-stop controller)
              (error (condition) condition))))
      (true (eq primary published)))
    (true (eq primary (luv:stop-controller-condition controller)))
    (true (= 1 calls))
    (multiple-value-bind (began-p worker)
        (luv:request-controlled-stop controller (lambda () :wrong))
      (true (not began-p))
      (true (null worker)))))

(define-test canvas-thread-guard-rejects-blocking-but-allows-request
  (let* ((forbidden-thread sb-thread:*current-thread*)
         (controller
           (luv:make-stop-controller
            :name "canvas-thread probe"
            :blocking-thread-p
            (lambda ()
              (eq sb-thread:*current-thread* forbidden-thread))))
         (calls 0))
    (fail
     (luv:call-with-stop-controller controller (lambda () :wrong))
     'luv:stop-controller-blocking-thread-error)
    (true (eq :running (luv:stop-controller-state controller)))
    (multiple-value-bind (began-p worker)
        (luv:request-controlled-stop
         controller (lambda () (incf calls) (values :done 7))
         :thread-name "stop-controller test worker")
      (true began-p)
      (true (typep worker 'sb-thread:thread))
      (sb-thread:join-thread worker))
    ;; Reading an already-published result never waits and is safe here.
    (true (equal '(:done 7)
                 (multiple-value-list
                  (luv:wait-for-controlled-stop controller))))
    (true (= 1 calls))
    (multiple-value-bind (began-p worker)
        (luv:request-controlled-stop controller (lambda () :wrong))
      (true (not began-p))
      (true (null worker)))))
