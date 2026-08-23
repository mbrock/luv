;;; Honest, one-owner application teardown.

(in-package #:luv)

;;; Named release aggregation.

(defstruct (release-failure
             (:constructor %make-release-failure (name condition)))
  "One named release step which signalled CONDITION."
  name
  condition)

(defvar *release-failures* nil
  "Failures collected by RELEASING in the current release scope.")

(defvar *collecting-release-failures-p* nil)

(defvar *releasing-depth* 0
  "Dynamic nesting depth of named release steps.")

(define-condition release-error (error)
  ((failures :initarg :failures :reader release-error-failures))
  (:documentation "All named failures from a completed release sequence.")
  (:report
   (lambda (condition stream)
     (let ((failures (release-error-failures condition)))
       (format stream "~D release step~:P failed.~:{~2%~A:~%  ~A~}"
               (length failures)
               (mapcar
                (lambda (failure)
                  (list (release-failure-name failure)
                        (release-failure-condition failure)))
                failures))))))

(define-condition release-warning (warning)
  ((failures :initarg :failures :reader release-warning-failures)
   (primary-condition
    :initarg :primary-condition :initform nil
    :reader release-warning-primary-condition))
  (:documentation
   "Named release failures reported without replacing an active transfer.")
  (:report
   (lambda (condition stream)
     (let ((failures (release-warning-failures condition))
           (primary (release-warning-primary-condition condition)))
       (format stream
               "~D release step~:P failed while unwinding~@[ ~A~].~
                ~:{~2%~A:~%  ~A~}"
               (length failures) primary
               (mapcar
                (lambda (failure)
                  (list (release-failure-name failure)
                        (release-failure-condition failure)))
                failures))))))

(defun call-releasing (name function)
  "Call FUNCTION as release step NAME, containing and recording any error."
  (check-type function function)
  (unless *collecting-release-failures-p*
    (error "RELEASING must run within a LUV release scope."))
  (let ((*releasing-depth* (1+ *releasing-depth*)))
    (handler-case (funcall function)
      (error (condition)
        (push (%make-release-failure name condition) *release-failures*)
        nil))))

(defmacro releasing (name &body body)
  "Run BODY as one named, contained release step."
  `(call-releasing ,name (lambda () ,@body)))

(defun report-release-warning (failures primary-condition)
  "Warn about FAILURES without allowing warning policy to mask an unwind."
  (when failures
    (handler-case
        (warn 'release-warning
              :failures failures :primary-condition primary-condition)
      ;; A global warning policy is allowed to promote warnings to errors, but
      ;; cleanup reporting still cannot replace the condition already leaving.
      (error (reporting-error)
        (format *error-output*
                "Release failures could not be warned normally: ~A~%"
                reporting-error)
        (finish-output *error-output*)))))

(defun call-with-release-report (function)
  "Call FUNCTION, then signal one RELEASE-ERROR for all named failures.

An uncontained error from FUNCTION remains primary.  Any failures recorded
before that error are reported as a RELEASE-WARNING while it unwinds."
  (check-type function function)
  (let ((entry-releasing-depth *releasing-depth*)
        (*release-failures* nil)
        (*collecting-release-failures-p* t)
        (completed-p nil)
        (primary-condition nil)
        (result-values nil))
    (handler-bind
        ((error
           (lambda (condition)
             (unless (or completed-p
                         (> *releasing-depth* entry-releasing-depth)
                         primary-condition)
               (setf primary-condition condition)))))
      (unwind-protect
           (progn
             (setf result-values (multiple-value-list (funcall function))
                   completed-p t))
        (unless completed-p
          (report-release-warning
           (reverse *release-failures*) primary-condition))))
    (when *release-failures*
      (error 'release-error :failures (reverse *release-failures*)))
    (values-list result-values)))

(defmacro with-release-report (&body body)
  "Run BODY and signal one RELEASE-ERROR after every named step ran."
  `(call-with-release-report (lambda () ,@body)))

(defun call-with-release-warnings (function &optional primary-condition)
  "Call FUNCTION and warn once about all of its named release failures."
  (check-type function function)
  (let ((entry-releasing-depth *releasing-depth*)
        (*release-failures* nil)
        (*collecting-release-failures-p* t)
        (completed-p nil)
        (escaping-condition primary-condition)
        (result-values nil))
    (handler-bind
        ((error
           (lambda (condition)
             (unless (or completed-p
                         (> *releasing-depth* entry-releasing-depth)
                         escaping-condition)
               (setf escaping-condition condition)))))
      (unwind-protect
           (progn
             (setf result-values (multiple-value-list (funcall function))
                   completed-p t))
        (report-release-warning
         (reverse *release-failures*) escaping-condition)))
    (values-list result-values)))

(defmacro with-release-warnings (&body body)
  "Run BODY and non-fatally report every named release failure."
  `(call-with-release-warnings (lambda () ,@body)))

(defun call-with-release-unwind (body-function cleanup-function)
  "Call BODY-FUNCTION and always run the named steps in CLEANUP-FUNCTION.

If the body finishes, cleanup failures become one RELEASE-ERROR.  If the body
leaves by an error or another nonlocal transfer, cleanup failures are reported
without replacing that primary transfer.  Every cleanup step expressed with
RELEASING is attempted in either case."
  (check-type body-function function)
  (check-type cleanup-function function)
  (let ((entry-releasing-depth *releasing-depth*)
        (completed-p nil)
        (primary-condition nil)
        (result-values nil))
    (handler-bind
        ((error
           (lambda (condition)
             (unless (or completed-p
                         (> *releasing-depth* entry-releasing-depth)
                         primary-condition)
               (setf primary-condition condition)))))
      (unwind-protect
           (progn
             (setf result-values
                   (multiple-value-list (funcall body-function))
                   completed-p t))
        (if completed-p
            (call-with-release-report cleanup-function)
            (call-with-release-warnings
             cleanup-function primary-condition))))
    (values-list result-values)))

(defmacro unwind-protect-releasing (protected-form &body cleanup-forms)
  "UNWIND-PROTECT with named, exhaustive, primary-preserving cleanup."
  `(call-with-release-unwind
    (lambda () ,protected-form)
    (lambda () ,@cleanup-forms)))

;;; One-owner stop publication.

(define-condition stop-controller-error (error)
  ((controller :initarg :controller :reader stop-error-controller))
  (:documentation "Base condition for invalid stop-controller operations."))

(define-condition stop-controller-blocking-thread-error
    (stop-controller-error)
  ((operation :initarg :operation :reader stop-error-operation))
  (:report
   (lambda (condition stream)
     (format stream
             "Cannot ~A ~A from its nonblocking owner thread; request the ~
              stop beside that thread instead."
             (stop-error-operation condition)
             (stop-controller-name
              (stop-error-controller condition))))))

(define-condition recursive-stop-error (stop-controller-error) ()
  (:report
   (lambda (condition stream)
     (format stream "The teardown owner recursively stopped ~A."
             (stop-controller-name
              (stop-error-controller condition))))))

(define-condition stop-not-started-error (stop-controller-error) ()
  (:report
   (lambda (condition stream)
     (format stream "No stop has been requested for ~A."
             (stop-controller-name
              (stop-error-controller condition))))))

(define-condition stop-operation-aborted (stop-controller-error) ()
  (:report
   (lambda (condition stream)
     (format stream "The teardown owner left ~A without publishing a result."
              (stop-controller-name
               (stop-error-controller condition))))))

(define-condition application-attachment-closed (error)
  ((controller
    :initarg :controller
    :reader application-attachment-closed-controller)
   (attachment
    :initarg :attachment
    :reader application-attachment-closed-attachment)
   (state
    :initarg :state
    :reader application-attachment-closed-state))
  (:documentation
   "An application attachment was offered after terminal teardown began.")
  (:report
   (lambda (condition stream)
     (format stream "Cannot attach ~S to ~A while its stop controller is ~(~A~)."
             (application-attachment-closed-attachment condition)
             (stop-controller-name
              (application-attachment-closed-controller condition))
             (application-attachment-closed-state condition)))))

(defclass stop-controller ()
  ((name :initarg :name :initform "application" :reader stop-controller-name)
   (blocking-thread-p
    :initarg :blocking-thread-p :initform (constantly nil)
    :reader stop-controller-blocking-thread-p)
   (state :initform :running :accessor %stop-controller-state)
   (owner :initform nil :accessor %stop-controller-owner)
   (result-values :initform nil :accessor %stop-controller-result-values)
   (condition :initform nil :accessor %stop-controller-condition)
   (lock :initform (sb-thread:make-mutex :name "LUV stop controller")
         :reader stop-controller-lock)
   (ready :initform (sb-thread:make-waitqueue
                     :name "LUV stop controller ready")
          :reader stop-controller-ready))
  (:documentation
   "A one-shot publication boundary around exactly one teardown execution.

The controller is a separate object owned by an application.  One caller
changes RUNNING to STOPPING and executes the teardown; every other blocking
caller waits for the same result or condition.  STOPPED is terminal, so no
resource-owning body can run twice."))

(defun make-stop-controller (&key (name "application") blocking-thread-p)
  "Make a one-shot stop controller named NAME.

BLOCKING-THREAD-P, when supplied, is a quick predicate which is true on a
thread that may request a stop but must never own or wait for one."
  (when blocking-thread-p
    (check-type blocking-thread-p function))
  (make-instance 'stop-controller
                 :name name
                 :blocking-thread-p (or blocking-thread-p (constantly nil))))

(defun make-canvas-stop-controller (canvas &key (name "canvas application"))
  "Make a stop controller whose blocking path rejects CANVAS's native thread."
  (make-stop-controller
   :name name
   :blocking-thread-p (lambda () (canvas-thread-p canvas))))

(defun stop-controller-state (controller)
  "Return CONTROLLER's synchronized RUNNING, STOPPING, or STOPPED state."
  (sb-thread:with-mutex ((stop-controller-lock controller))
    (%stop-controller-state controller)))

(defun stop-controller-condition (controller)
  "Return CONTROLLER's published failure condition, or NIL."
  (sb-thread:with-mutex ((stop-controller-lock controller))
    (%stop-controller-condition controller)))

(defun stop-controller-result-values (controller)
  "Return a fresh list of CONTROLLER's published normal values."
  (sb-thread:with-mutex ((stop-controller-lock controller))
    (copy-list (%stop-controller-result-values controller))))

(defun call-with-running-stop-controller
    (controller function &key attachment already-attached-p)
  "Call quick publication FUNCTION iff CONTROLLER is still running.

The controller lock remains held across FUNCTION, making publication atomic
with the RUNNING to STOPPING transition.  FUNCTION and ALREADY-ATTACHED-P must
therefore neither block nor reenter CONTROLLER.  A true ALREADY-ATTACHED-P is
an idempotent success even during teardown and returns ATTACHMENT without
republishing it.  Otherwise, once stop has begun, signal
APPLICATION-ATTACHMENT-CLOSED outside the lock so the caller can release its
unpublished ATTACHMENT without deadlocking lifecycle inspection."
  (check-type controller stop-controller)
  (check-type function function)
  (when already-attached-p (check-type already-attached-p function))
  (let ((state nil)
        (called-p nil)
        (result-values nil))
    (sb-thread:with-mutex ((stop-controller-lock controller))
      (setf state (%stop-controller-state controller))
      (cond ((eq :running state)
             (setf result-values (multiple-value-list (funcall function))
                   called-p t))
            ((and already-attached-p (funcall already-attached-p))
             (setf result-values (list attachment)
                   called-p t))))
    (unless called-p
      (error 'application-attachment-closed
             :controller controller :attachment attachment :state state))
    (values-list result-values)))

(defun ensure-stop-blocking-allowed (controller operation)
  (when (funcall (stop-controller-blocking-thread-p controller))
    (error 'stop-controller-blocking-thread-error
           :controller controller :operation operation)))

(defun publish-stop-outcome (controller result-values condition)
  (sb-thread:with-mutex ((stop-controller-lock controller))
    (setf (%stop-controller-result-values controller) result-values
          (%stop-controller-condition controller) condition
          (%stop-controller-owner controller) nil
          (%stop-controller-state controller) :stopped)
    (sb-thread:condition-broadcast (stop-controller-ready controller)))
  (values))

(defun return-stop-outcome (result-values condition)
  (if condition
      (error condition)
      (values-list result-values)))

(defun run-stop-owner (controller function)
  (let ((result-values nil)
        (condition nil)
        (completed-p nil))
    (unwind-protect
         (handler-case
             (progn
               (setf result-values (multiple-value-list (funcall function))
                     completed-p t))
           (error (failure)
             (setf condition failure
                   completed-p t)))
      (unless completed-p
        (setf condition
              (make-condition 'stop-operation-aborted
                              :controller controller)))
      (publish-stop-outcome controller result-values condition))
    (return-stop-outcome result-values condition)))

(defun call-with-stop-controller (controller function)
  "Run FUNCTION as CONTROLLER's sole teardown, or observe its published result.

A concurrent caller waits while the owner runs.  A later caller receives the
same values or signals the same condition without running FUNCTION again."
  (check-type controller stop-controller)
  (check-type function function)
  (let ((owner-p nil)
        (result-values nil)
        (condition nil))
    (sb-thread:with-mutex ((stop-controller-lock controller))
      (loop
        (case (%stop-controller-state controller)
          (:running
           (ensure-stop-blocking-allowed controller :stop)
           (setf (%stop-controller-state controller) :stopping
                 (%stop-controller-owner controller) sb-thread:*current-thread*
                 owner-p t)
           (return))
          (:stopping
           (when (eq (%stop-controller-owner controller)
                     sb-thread:*current-thread*)
             (error 'recursive-stop-error :controller controller))
           (ensure-stop-blocking-allowed controller :wait-for-stop)
           (sb-thread:condition-wait
            (stop-controller-ready controller)
            (stop-controller-lock controller)))
          (:stopped
           (setf result-values
                 (copy-list (%stop-controller-result-values controller))
                 condition (%stop-controller-condition controller))
           (return))
          (otherwise
           (error "Invalid state ~S in ~A."
                  (%stop-controller-state controller)
                  (stop-controller-name controller))))))
    (if owner-p
        (run-stop-owner controller function)
        (return-stop-outcome result-values condition))))

(defun wait-for-controlled-stop (controller)
  "Wait for CONTROLLER's existing owner and return its published outcome."
  (check-type controller stop-controller)
  (let ((result-values nil)
        (condition nil))
    (sb-thread:with-mutex ((stop-controller-lock controller))
      (loop
        (case (%stop-controller-state controller)
          (:running
           (error 'stop-not-started-error :controller controller))
          (:stopping
           (when (eq (%stop-controller-owner controller)
                     sb-thread:*current-thread*)
             (error 'recursive-stop-error :controller controller))
           (ensure-stop-blocking-allowed controller :wait-for-stop)
           (sb-thread:condition-wait
            (stop-controller-ready controller)
            (stop-controller-lock controller)))
          (:stopped
           (setf result-values
                 (copy-list (%stop-controller-result-values controller))
                 condition (%stop-controller-condition controller))
           (return)))))
    (return-stop-outcome result-values condition)))

(defun request-controlled-stop
    (controller function &key thread-name)
  "Request FUNCTION as CONTROLLER's teardown on one new worker thread.

Return true and the worker when this call reserved ownership, or NIL and NIL
when a stop was already requested or published.  The worker contains any
failure after publishing it; WAIT-FOR-CONTROLLED-STOP and synchronous callers
receive that exact condition."
  (check-type controller stop-controller)
  (check-type function function)
  (let ((ticket (list :reserved-stop-owner))
        (begin-p nil))
    (sb-thread:with-mutex ((stop-controller-lock controller))
      (when (eq :running (%stop-controller-state controller))
        (setf (%stop-controller-state controller) :stopping
              (%stop-controller-owner controller) ticket
              begin-p t)))
    (if (not begin-p)
        (values nil nil)
        (handler-case
            (let ((thread
                    (sb-thread:make-thread
                     (lambda ()
                       (sb-thread:with-mutex
                           ((stop-controller-lock controller))
                         (when (eq ticket (%stop-controller-owner controller))
                           (setf (%stop-controller-owner controller)
                                 sb-thread:*current-thread*)))
                       ;; The published condition is the asynchronous API;
                       ;; an unhandled worker error would only enter a debugger.
                       (handler-case (run-stop-owner controller function)
                         (error () nil)))
                     :name (or thread-name
                               (format nil "stop ~A"
                                       (stop-controller-name controller))))))
              (values t thread))
          (error (condition)
            (publish-stop-outcome controller nil condition)
            (error condition))))))
