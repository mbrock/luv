(defpackage #:luv.tracy.capture.tests
  (:use #:cl #:rove))

(in-package #:luv.tracy.capture.tests)

(defclass scripted-tracy-process ()
  ((role :initarg :role :reader scripted-process-role)
   (pathname :initarg :pathname :initform nil
             :reader scripted-process-pathname)
   (lock
    :initform (sb-thread:make-mutex :name "scripted Tracy process")
    :reader scripted-process-lock)
   (completion
    :initform (sb-thread:make-semaphore :name "scripted Tracy completion")
    :reader scripted-process-completion)
   (alive-p :initform t :accessor scripted-process-alive-p)
   (exit-code :initarg :exit-code :initform 0
              :accessor scripted-process-exit-code)
   (wait-error :initarg :wait-error :initform nil
               :reader scripted-process-wait-error)
   (interrupts :initform 0 :accessor scripted-process-interrupts)
   (terminations :initform 0 :accessor scripted-process-terminations)
   (closes :initform 0 :accessor scripted-process-closes)))

(defclass scripted-tracy-runtime
    (luv.tracy.capture:tracy-capture-runtime)
  ((lock
    :initform (sb-thread:make-mutex :name "scripted Tracy runtime")
    :reader scripted-runtime-lock)
   (now
    :initarg :now
    :initform (encode-universal-time 5 4 3 2 1 2026 0)
    :accessor scripted-runtime-now)
   (connected-p
    :initarg :connected-p :initform nil
    :accessor scripted-runtime-connected-p)
   (prepare-error
    :initarg :prepare-error :initform nil
    :accessor scripted-runtime-prepare-error)
   (prepare-count
    :initform 0 :accessor scripted-runtime-prepare-count)
   (launch-error-role
    :initarg :launch-error-role :initform nil
    :accessor scripted-runtime-launch-error-role)
   (capture-launch-gate
    :initarg :capture-launch-gate :initform nil
    :accessor scripted-runtime-capture-launch-gate)
   (interrupt-gate
    :initarg :interrupt-gate :initform nil
    :accessor scripted-runtime-interrupt-gate)
   (interrupt-failures
    :initarg :interrupt-failures :initform 0
    :accessor scripted-runtime-interrupt-failures)
   (auxiliary-launch-gate
    :initarg :auxiliary-launch-gate :initform nil
    :accessor scripted-runtime-auxiliary-launch-gate)
   (capture-wait-error
    :initarg :capture-wait-error :initform nil
    :accessor scripted-runtime-capture-wait-error)
   (capture-exit-code
    :initarg :capture-exit-code :initform 0
    :accessor scripted-runtime-capture-exit-code)
   (write-output-p
    :initarg :write-output-p :initform t
    :accessor scripted-runtime-write-output-p)
   (existing-pathnames
    :initform (make-hash-table :test #'equal)
    :reader scripted-runtime-existing-pathnames)
   (launches
    :initform nil :accessor scripted-runtime-launches)
   (processes
    :initform nil :accessor scripted-runtime-processes)))

(defmethod luv.tracy.capture:tracy-clock-now
    ((runtime scripted-tracy-runtime))
  (scripted-runtime-now runtime))

(defmethod luv.tracy.capture:tracy-path-exists-p
    ((runtime scripted-tracy-runtime) pathname)
  (sb-thread:with-mutex ((scripted-runtime-lock runtime))
    (not (null
          (gethash (namestring pathname)
                   (scripted-runtime-existing-pathnames runtime))))))

(defmethod luv.tracy.capture:prepare-tracy-client
    ((runtime scripted-tracy-runtime) application-name)
  (declare (ignore application-name))
  (sb-thread:with-mutex ((scripted-runtime-lock runtime))
    (incf (scripted-runtime-prepare-count runtime)))
  (when (scripted-runtime-prepare-error runtime)
    (error (scripted-runtime-prepare-error runtime)))
  t)

(defmethod luv.tracy.capture:tracy-viewer-connected-p
    ((runtime scripted-tracy-runtime))
  (scripted-runtime-connected-p runtime))

(defmethod luv.tracy.capture:resolve-tracy-program
    ((runtime scripted-tracy-runtime) role)
  (declare (ignore runtime))
  (ecase role
    (:capture #P"/scripted/tracy-capture")
    (:profiler #P"/scripted/tracy-profiler")
    (:reveal #P"/scripted/reveal")))

(defmethod luv.tracy.capture:launch-tracy-process
    ((runtime scripted-tracy-runtime) role program arguments)
  (let ((gate
          (if (eq role :capture)
              (scripted-runtime-capture-launch-gate runtime)
              (scripted-runtime-auxiliary-launch-gate runtime))))
    (when gate
      (unless (sb-thread:wait-on-semaphore gate :timeout 1.0)
        (error "Scripted Tracy launch gate timed out."))))
  (when (eq role (scripted-runtime-launch-error-role runtime))
    (error "Scripted Tracy ~(~A~) launch failed." role))
  (let ((process
          (make-instance
           'scripted-tracy-process
           :role role
           :pathname (and (eq role :capture)
                          (pathname (second arguments)))
           :exit-code (if (eq role :capture)
                          (scripted-runtime-capture-exit-code runtime)
                          0)
           :wait-error (and (eq role :capture)
                            (scripted-runtime-capture-wait-error runtime)))))
    (sb-thread:with-mutex ((scripted-runtime-lock runtime))
      (push (list role program (copy-list arguments))
            (scripted-runtime-launches runtime))
      (push process (scripted-runtime-processes runtime)))
    (unless (eq role :capture)
      (setf (scripted-process-alive-p process) nil)
      (sb-thread:signal-semaphore
       (scripted-process-completion process)))
    process))

(defmethod luv.tracy.capture:tracy-process-alive-p
    ((runtime scripted-tracy-runtime) (process scripted-tracy-process))
  (declare (ignore runtime))
  (sb-thread:with-mutex ((scripted-process-lock process))
    (scripted-process-alive-p process)))

(defmethod luv.tracy.capture:interrupt-tracy-process
    ((runtime scripted-tracy-runtime) (process scripted-tracy-process))
  (let ((gate (scripted-runtime-interrupt-gate runtime)))
    (when gate
      (unless (sb-thread:wait-on-semaphore gate :timeout 1.0)
        (error "Scripted Tracy interrupt gate timed out."))))
  (sb-thread:with-mutex ((scripted-process-lock process))
    (incf (scripted-process-interrupts process))
    (when (plusp (scripted-runtime-interrupt-failures runtime))
      (decf (scripted-runtime-interrupt-failures runtime))
      (error "Scripted Tracy interrupt failed."))
    (when (scripted-process-alive-p process)
      (setf (scripted-process-alive-p process) nil)
      (when (and (scripted-runtime-write-output-p runtime)
                 (scripted-process-pathname process))
        (sb-thread:with-mutex ((scripted-runtime-lock runtime))
          (setf (gethash
                 (namestring (scripted-process-pathname process))
                 (scripted-runtime-existing-pathnames runtime))
                t)))
      (sb-thread:signal-semaphore
       (scripted-process-completion process))))
  t)

(defmethod luv.tracy.capture:terminate-tracy-process
    ((runtime scripted-tracy-runtime) (process scripted-tracy-process))
  (declare (ignore runtime))
  (sb-thread:with-mutex ((scripted-process-lock process))
    (incf (scripted-process-terminations process))
    (setf (scripted-process-alive-p process) nil)
    (sb-thread:signal-semaphore
     (scripted-process-completion process)))
  t)

(defmethod luv.tracy.capture:wait-tracy-process
    ((runtime scripted-tracy-runtime) (process scripted-tracy-process))
  (declare (ignore runtime))
  (when (scripted-process-wait-error process)
    (error (scripted-process-wait-error process)))
  (unless (sb-thread:wait-on-semaphore
           (scripted-process-completion process) :timeout 1.0)
    (error "Scripted Tracy process did not finish."))
  process)

(defmethod luv.tracy.capture:tracy-process-exit-code
    ((runtime scripted-tracy-runtime) (process scripted-tracy-process))
  (declare (ignore runtime))
  (scripted-process-exit-code process))

(defmethod luv.tracy.capture:close-tracy-process
    ((runtime scripted-tracy-runtime) (process scripted-tracy-process))
  (declare (ignore runtime))
  (sb-thread:with-mutex ((scripted-process-lock process))
    (incf (scripted-process-closes process)))
  t)

(defun make-scripted-controller (runtime &key (graceful-stop-seconds 5.0d0))
  (luv.tracy.capture:make-tracy-capture-controller
   :application-name "Test Game"
   :directory (uiop:temporary-directory)
   :runtime runtime
   :graceful-stop-seconds graceful-stop-seconds
   :open-on-completion-p nil))

(defun wait-for-tracy-test (predicate &key (timeout 1.0))
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        when (funcall predicate) return t
        when (>= (get-internal-real-time) deadline) return nil
        do (sleep 1/1000)))

(defun scripted-capture-process (runtime)
  (sb-thread:with-mutex ((scripted-runtime-lock runtime))
    (find :capture (scripted-runtime-processes runtime)
          :key #'scripted-process-role)))

(defun scripted-launches (runtime)
  (sb-thread:with-mutex ((scripted-runtime-lock runtime))
    (reverse (copy-tree (scripted-runtime-launches runtime)))))

(deftest tracy-capture-launch-is-off-caller-and-uses-the-exact-program
  (let* ((gate (sb-thread:make-semaphore :name "Tracy launch gate"))
         (runtime
           (make-instance 'scripted-tracy-runtime
                          :capture-launch-gate gate))
         (controller (make-scripted-controller runtime))
         (started (get-internal-real-time))
         (pathname (luv.tracy.capture:start-tracy-capture controller))
         (elapsed
           (/ (- (get-internal-real-time) started)
              (coerce internal-time-units-per-second 'double-float))))
    (ok (< elapsed 0.2d0) "START does not wait for the injected launcher")
    (ok (eq :starting
            (luv.tracy.capture:tracy-capture-state controller)))
    (ok (search "test-game-20260102-030405Z"
                (namestring pathname)))
    (sb-thread:signal-semaphore gate)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :recording
               (luv.tracy.capture:tracy-capture-state controller)))))
    (destructuring-bind (role program arguments)
        (first (scripted-launches runtime))
      (ok (eq :capture role))
      (ok (equal #P"/scripted/tracy-capture" program))
      (ok (equal (list "-o" (namestring pathname)
                       "-a" "127.0.0.1")
                 arguments)))
    (luv.tracy.capture:stop-tracy-capture controller)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :idle
               (luv.tracy.capture:tracy-capture-state controller)))))))

(deftest tracy-path-reservation-skips-an-existing-capture-at-the-same-clock
  (let* ((runtime (make-instance 'scripted-tracy-runtime))
         (controller (make-scripted-controller runtime))
         (first (luv.tracy.capture:start-tracy-capture controller)))
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :recording
               (luv.tracy.capture:tracy-capture-state controller)))))
    (luv.tracy.capture:stop-tracy-capture controller)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :idle
               (luv.tracy.capture:tracy-capture-state controller)))))
    ;; Revisit serial zero at the identical injected clock.  The first exact
    ;; candidate now exists, so reservation must advance rather than spin or
    ;; overwrite it.
    (sb-thread:with-mutex
        ((luv.tracy.capture::tracy-capture-lock controller))
      (setf (luv.tracy.capture::tracy-capture-serial controller) 0))
    (let* ((started (get-internal-real-time))
           (second (luv.tracy.capture:start-tracy-capture controller))
           (elapsed
             (/ (- (get-internal-real-time) started)
                (coerce internal-time-units-per-second 'double-float))))
      (ok (< elapsed 0.2d0))
      (ok (not (equal first second)))
      (ok (search "-1.tracy" (namestring second))))
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :recording
               (luv.tracy.capture:tracy-capture-state controller)))))
    (luv.tracy.capture:stop-tracy-capture controller)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :idle
               (luv.tracy.capture:tracy-capture-state controller)))))))

(deftest concurrent-tracy-stops-emit-one-interrupt-and-one-close
  (let* ((runtime (make-instance 'scripted-tracy-runtime))
         (controller (make-scripted-controller runtime))
         (pathname (luv.tracy.capture:start-tracy-capture controller)))
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :recording
               (luv.tracy.capture:tracy-capture-state controller)))))
    (let ((threads
            (loop repeat 12
                  collect
                  (sb-thread:make-thread
                   (lambda ()
                     (luv.tracy.capture:stop-tracy-capture controller))))))
      (dolist (thread threads)
        (sb-thread:join-thread thread)))
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :idle
               (luv.tracy.capture:tracy-capture-state controller)))))
    (let ((process (scripted-capture-process runtime)))
      (ok (= 1 (scripted-process-interrupts process)))
      (ok (= 1 (scripted-process-closes process))))
    (ok (equal pathname
               (luv.tracy.capture:tracy-capture-last-completed-pathname
                controller)))
    (ok (null (luv.tracy.capture:tracy-capture-diagnostics controller)))))

(deftest capture-owner-retries-transient-interrupt-failures
  (let* ((runtime
           (make-instance 'scripted-tracy-runtime
                          :interrupt-failures 2))
         (controller (make-scripted-controller runtime)))
    (luv.tracy.capture:start-tracy-capture controller)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :recording
               (luv.tracy.capture:tracy-capture-state controller)))))
    (luv.tracy.capture:stop-tracy-capture controller)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :idle
               (luv.tracy.capture:tracy-capture-state controller)))))
    (let ((process (scripted-capture-process runtime)))
      (ok (= 3 (scripted-process-interrupts process)))
      (ok (= 0 (scripted-process-terminations process)))
      (ok (= 1 (scripted-process-closes process))))
    (ok (equal '(:stop :stop)
               (mapcar
                #'luv.tracy.capture:tracy-capture-diagnostic-operation
                (luv.tracy.capture:tracy-capture-diagnostics controller))))))

(deftest persistent-interrupt-failure-cannot-orphan-a-released-capture
  (let* ((runtime
           (make-instance 'scripted-tracy-runtime
                          :interrupt-failures 20))
         (controller (make-scripted-controller runtime)))
    (luv.tracy.capture:start-tracy-capture controller)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :recording
               (luv.tracy.capture:tracy-capture-state controller)))))
    ;; Model an application adapter dropping its last reference immediately.
    ;; The generation-owning closure still retries and finally terminates its
    ;; exact child before releasing the Lisp process handle.
    (luv.tracy.capture:release-tracy-capture-controller controller)
    (ok (eq :released
            (luv.tracy.capture:tracy-capture-state controller)))
    (ok (wait-for-tracy-test
         (lambda ()
           (let ((process (scripted-capture-process runtime)))
             (and process
                  (= 1 (scripted-process-terminations process))
                  (= 1 (scripted-process-closes process)))))))
    (let ((process (scripted-capture-process runtime)))
      (ok (= 3 (scripted-process-interrupts process)))
      (ng (scripted-process-alive-p process)))
    (ok (eq :released
            (luv.tracy.capture:tracy-capture-state controller)))
    (ok (member :terminate
                (mapcar
                 #'luv.tracy.capture:tracy-capture-diagnostic-operation
                 (luv.tracy.capture:tracy-capture-diagnostics controller))))))

(deftest tracy-stop-does-not-wait-for-interrupt-or-finalization
  (let* ((gate (sb-thread:make-semaphore :name "Tracy interrupt gate"))
         (runtime
           (make-instance 'scripted-tracy-runtime :interrupt-gate gate))
         (controller (make-scripted-controller runtime)))
    (luv.tracy.capture:start-tracy-capture controller)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :recording
               (luv.tracy.capture:tracy-capture-state controller)))))
    (let ((started (get-internal-real-time)))
      (luv.tracy.capture:stop-tracy-capture controller)
      (ok (< (/ (- (get-internal-real-time) started)
                (coerce internal-time-units-per-second 'double-float))
             0.2d0)))
    (ok (eq :stopping
            (luv.tracy.capture:tracy-capture-state controller)))
    (sb-thread:signal-semaphore gate)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :idle
               (luv.tracy.capture:tracy-capture-state controller)))))))

(deftest failed-tracy-launch-is-contained-and-controller-recovers
  (let* ((runtime
           (make-instance 'scripted-tracy-runtime
                          :launch-error-role :capture))
         (controller (make-scripted-controller runtime)))
    (luv.tracy.capture:start-tracy-capture controller)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :idle
               (luv.tracy.capture:tracy-capture-state controller)))))
    (let ((diagnostic
            (luv.tracy.capture:tracy-capture-last-diagnostic controller)))
      (ok (eq :start
              (luv.tracy.capture:tracy-capture-diagnostic-operation
               diagnostic)))
      (ok (typep
           (luv.tracy.capture:tracy-capture-diagnostic-condition diagnostic)
           'error)))
    (setf (scripted-runtime-launch-error-role runtime) nil)
    (luv.tracy.capture:start-tracy-capture controller)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :recording
               (luv.tracy.capture:tracy-capture-state controller)))))
    (luv.tracy.capture:stop-tracy-capture controller)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :idle
               (luv.tracy.capture:tracy-capture-state controller)))))))

(deftest failed-tracy-finalizer-is-contained-and-closes-the-handle
  (let* ((runtime
           (make-instance
            'scripted-tracy-runtime
            :capture-wait-error
            (make-condition 'simple-error
                            :format-control "scripted wait failure")))
         (controller (make-scripted-controller runtime)))
    (luv.tracy.capture:start-tracy-capture controller)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :recording
               (luv.tracy.capture:tracy-capture-state controller)))))
    ;; The owner observes exit before it performs the injected failing reap.
    ;; Request a normal stop so this test exercises that finalization edge
    ;; rather than leaving the scripted capture intentionally alive.
    (luv.tracy.capture:stop-tracy-capture controller)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :idle
               (luv.tracy.capture:tracy-capture-state controller)))))
    (let ((process (scripted-capture-process runtime))
          (diagnostic
            (luv.tracy.capture:tracy-capture-last-diagnostic controller)))
      (ok (= 1 (scripted-process-closes process)))
      (ok (eq :finalize
              (luv.tracy.capture:tracy-capture-diagnostic-operation
               diagnostic))))))

(deftest tracy-profiler-open-is-off-caller-and-failure-is-diagnostic
  (let* ((runtime (make-instance 'scripted-tracy-runtime))
         (controller (make-scripted-controller runtime))
         (pathname (luv.tracy.capture:start-tracy-capture controller)))
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :recording
               (luv.tracy.capture:tracy-capture-state controller)))))
    (luv.tracy.capture:stop-tracy-capture controller)
    (ok (wait-for-tracy-test
         (lambda ()
           (eq :idle
               (luv.tracy.capture:tracy-capture-state controller)))))
    (let ((gate (sb-thread:make-semaphore :name "Tracy profiler gate")))
      (setf (scripted-runtime-auxiliary-launch-gate runtime) gate
            (scripted-runtime-launch-error-role runtime) :profiler)
      (let ((started (get-internal-real-time)))
        (ok (equal pathname
                   (luv.tracy.capture:open-tracy-capture controller)))
        (ok (< (/ (- (get-internal-real-time) started)
                  (coerce internal-time-units-per-second 'double-float))
               0.2d0)))
      (sb-thread:signal-semaphore gate)
      (ok (wait-for-tracy-test
           (lambda ()
             (let ((diagnostic
                     (luv.tracy.capture:tracy-capture-last-diagnostic
                      controller)))
               (and diagnostic
                    (eq :open
                        (luv.tracy.capture:tracy-capture-diagnostic-operation
                         diagnostic))))))))))

(deftest tracy-controller-release-is-immediate-terminal-and-idempotent
  (let* ((gate (sb-thread:make-semaphore :name "Tracy release launch gate"))
         (runtime
           (make-instance 'scripted-tracy-runtime
                          :capture-launch-gate gate))
         (controller (make-scripted-controller runtime)))
    (luv.tracy.capture:start-tracy-capture controller)
    (let ((started (get-internal-real-time)))
      (luv.tracy.capture:release-tracy-capture-controller controller)
      (luv.tracy.capture:release-tracy-capture-controller controller)
      (ok (< (/ (- (get-internal-real-time) started)
                (coerce internal-time-units-per-second 'double-float))
             0.2d0)))
    (ok (eq :released
            (luv.tracy.capture:tracy-capture-state controller)))
    (sb-thread:signal-semaphore gate)
    (ok (wait-for-tracy-test
         (lambda ()
           (let ((process (scripted-capture-process runtime)))
             (and process
                  (= 1 (scripted-process-interrupts process))
                  (= 1 (scripted-process-closes process)))))))
    (ok (eq :released
            (luv.tracy.capture:tracy-capture-state controller)))))
