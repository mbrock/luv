;;; A deliberately small owner/worker production boundary.

(in-package #:luv.production)

(math:define-quantity :production-duration :kind :duration
  :non-negative-p t)

(defclass production-request ()
  ((key :initarg :key :reader production-request-key)
   (priority :initarg :priority :initform 0 :reader production-request-priority)
   (ticket :initform nil :accessor production-request-ticket))
  (:documentation
   "An inspectable immutable unit of CPU work after it is submitted."))

(defgeneric perform-production-request (request)
  (:documentation
   "Compute REQUEST on the production worker and return a transferable value."))

(defclass production-result ()
  ((request :initarg :request :reader production-result-request)
   (value :initarg :value :initform nil :reader production-result-value)
   (condition :initarg :condition :initform nil
              :reader production-result-condition)
   (elapsed-seconds :initarg :elapsed-seconds
                    :type double-float
                    :quantity (:quantity :production-duration :unit :second)
                    :reader production-result-elapsed-seconds))
  (:metaclass luv.arithmetic.records:quantity-class))

(defclass single-worker-production-system ()
  ((name :initarg :name :reader production-system-name)
   (request-mailbox :reader production-system-request-mailbox)
   (result-mailbox :reader production-system-result-mailbox)
   (lock :reader production-system-lock)
   ;; DESIRED is a latest-value map, not a history queue. Scheduling the same
   ;; semantic key replaces work which has not started yet.
   (desired :reader production-system-desired)
   (wake-p :initform nil :accessor production-system-wake-p)
   (active-request :initform nil :accessor production-system-active-request)
   (next-ticket :initform 0 :accessor production-system-next-ticket)
   (running-p :initform t :accessor production-system-running-p)
   (thread :initform nil :accessor production-system-thread)))

(defun production-system-request-better-p (left right)
  (or (< (production-request-priority left)
         (production-request-priority right))
      (and (= (production-request-priority left)
              (production-request-priority right))
           (< (production-request-ticket left)
              (production-request-ticket right)))))

(zdefun (take-production-system-request :zone :production/take-request) (system)
  (sb-thread:with-mutex ((production-system-lock system))
    (setf (production-system-wake-p system) nil)
    (let ((best nil))
      (maphash
       (lambda (key request)
         (declare (ignore key))
         (when (or (null best)
                   (production-system-request-better-p request best))
           (setf best request)))
       (production-system-desired system))
      (when best
        (remhash (production-request-key best)
                 (production-system-desired system))
        (setf (production-system-active-request system) best))
      best)))

(defun wake-production-system-if-needed (system)
  (let ((wake-p nil))
    (sb-thread:with-mutex ((production-system-lock system))
      (when (and (production-system-running-p system)
                 (plusp (hash-table-count
                         (production-system-desired system)))
                 ;; One completed value is the publication backpressure
                 ;; frontier. The owner must receive it before the worker
                 ;; begins another request, so result memory stays bounded
                 ;; even when rendering pauses.
                 (sb-concurrency:mailbox-empty-p
                  (production-system-result-mailbox system))
                 (null (production-system-active-request system))
                 (not (production-system-wake-p system)))
        (setf (production-system-wake-p system) t
              wake-p t)))
    (when wake-p
      (sb-concurrency:send-message
       (production-system-request-mailbox system) :work))))

(defun run-production-system (system)
  (name-tracy-thread (production-system-name system))
  (loop
    (multiple-value-bind (message received-p)
        (sb-concurrency:receive-message
         (production-system-request-mailbox system))
      (declare (ignore received-p))
      (ecase message
        (:stop (return))
        (:work
         (let ((request (take-production-system-request system)))
           (when request
             (let* ((start (get-internal-real-time))
                    (value nil)
                    (condition nil))
               (with-cpu-trace-zone (:production/perform)
                 (handler-case
                     (setf value (perform-production-request request))
                   (error (caught) (setf condition caught))))
               (sb-concurrency:send-message
                (production-system-result-mailbox system)
                (make-instance
                 'production-result
                 :request request :value value :condition condition
                 :elapsed-seconds
                 (/ (- (get-internal-real-time) start)
                    (coerce internal-time-units-per-second 'double-float))))
               (sb-thread:with-mutex ((production-system-lock system))
                 (setf (production-system-active-request system) nil)))))
         (wake-production-system-if-needed system))))))

(defun make-single-worker-production-system
    (&key (name "luv single CPU producer"))
  "Make one sleeping SB-CONCURRENCY mailbox worker.

There is intentionally no pool yet. One worker proves the ownership and
publication protocol, prevents CPU oversubscription, and keeps completion
order intelligible while taking expensive work out of an owner callback."
  (let ((system (make-instance 'single-worker-production-system :name name)))
    (setf (slot-value system 'request-mailbox)
          (sb-concurrency:make-mailbox :name (format nil "~A requests" name))
          (slot-value system 'result-mailbox)
          (sb-concurrency:make-mailbox :name (format nil "~A results" name))
          (slot-value system 'lock)
          (sb-thread:make-mutex :name (format nil "~A state" name))
          (slot-value system 'desired)
          (make-hash-table :test #'equal)
          (production-system-thread system)
          (sb-thread:make-thread
           (lambda () (run-production-system system)) :name name))
    system))

(zdefun (schedule-production-request :zone :production/schedule)
    (system request)
  "Schedule the latest REQUEST for its semantic key and return its ticket."
  (check-type system single-worker-production-system)
  (check-type request production-request)
  (sb-thread:with-mutex ((production-system-lock system))
    (unless (production-system-running-p system)
      (error "Production system ~A has stopped."
             (production-system-name system)))
    (setf (production-request-ticket request)
          (incf (production-system-next-ticket system))
          (gethash (production-request-key request)
                   (production-system-desired system))
          request))
  (wake-production-system-if-needed system)
  (production-request-ticket request))

(defun production-request-pending-p (system key)
  "Whether KEY is desired or presently executing."
  (sb-thread:with-mutex ((production-system-lock system))
    (or (nth-value 1 (gethash key (production-system-desired system)))
        (let ((active (production-system-active-request system)))
          (and active (equal key (production-request-key active)))))))

(defun cancel-production-request (system key)
  "Cancel work for KEY if it has not begun; active results remain discardable."
  (sb-thread:with-mutex ((production-system-lock system))
    (remhash key (production-system-desired system))))

(defun receive-production-result-no-hang (system)
  (multiple-value-bind (result present-p)
      (sb-concurrency:receive-message-no-hang
       (production-system-result-mailbox system))
    (when present-p
      (wake-production-system-if-needed system))
    (values result present-p)))

(defun production-system-completed-count (system)
  "Return the number of completed results waiting for their owner."
  (sb-concurrency:mailbox-count (production-system-result-mailbox system)))

(defun production-system-pending-count (system)
  (sb-thread:with-mutex ((production-system-lock system))
    (+ (hash-table-count (production-system-desired system))
       (if (production-system-active-request system) 1 0)
       (production-system-completed-count system))))

(defun stop-production-system (system &key (timeout 10.0))
  "Cooperatively stop SYSTEM after its active request, then join its worker."
  (check-type system single-worker-production-system)
  (let ((stop-p nil))
    (sb-thread:with-mutex ((production-system-lock system))
      (when (production-system-running-p system)
        (setf (production-system-running-p system) nil
              stop-p t)
        (clrhash (production-system-desired system))))
    (when stop-p
      (sb-concurrency:send-message
       (production-system-request-mailbox system) :stop)
      (multiple-value-bind (value state)
          (sb-thread:join-thread (production-system-thread system)
                                 :timeout timeout :default :timeout)
        (declare (ignore value))
        (when (eq state :timeout)
          (error "Production worker ~A did not stop within ~,2F seconds."
                 (production-system-name system) timeout)))))
  nil)
