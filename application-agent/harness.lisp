(in-package #:luv.application-agent)

;;; One agent owns one provider connection and one FIFO mailbox worker.  ASK
;;; only publishes a TURN to that worker.  Provider turns, tool settlement,
;;; observer callbacks, and provider close therefore never execute on the
;;; application's canvas thread or race one another on the provider socket.

(define-condition application-agent-released (error)
  ((agent :initarg :agent :reader released-application-agent))
  (:report (lambda (condition stream)
             (format stream "Application agent ~S is released."
                     (released-application-agent condition)))))

(define-condition agent-observer-failure (warning)
  ((observer :initarg :observer :reader agent-observer-failure-observer)
   (kind :initarg :kind :reader agent-observer-failure-kind)
   (object :initarg :object :reader agent-observer-failure-object)
   (cause :initarg :cause :reader agent-observer-failure-cause))
  (:report (lambda (condition stream)
             (format stream "Agent observer ~S failed during ~S: ~A"
                     (agent-observer-failure-observer condition)
                     (agent-observer-failure-kind condition)
                     (agent-observer-failure-cause condition)))))

(defconstant +observer-failure-limit+ 16)

(defclass application-agent (openai:agent)
  ((application :initarg :application :accessor %application-agent-application)
   (%turns :initform '() :accessor %application-agent-turns)
   (%current-turn :initform nil :accessor %application-agent-current-turn)
   (handles :initform (make-handle-table) :reader application-agent-handles)
   (observers :initform '() :accessor application-agent-observers)
   (observer-failures :initform '()
                      :accessor %application-agent-observer-failures)
   (state :initform :open :accessor %application-agent-state)
   (state-lock :initform
               (sb-thread:make-mutex :name "application agent state")
               :reader application-agent-state-lock)
   (worker-mailbox :initform
                   (sb-concurrency:make-mailbox
                    :name "application agent provider work")
                   :reader application-agent-worker-mailbox)
   (worker-thread :initform nil :accessor %application-agent-worker-thread)
   (worker-finished-p :initform nil
                      :accessor %application-agent-worker-finished-p)
   (close-thread :initform nil :accessor %application-agent-close-thread)
   (close-finished-p :initform nil
                     :accessor %application-agent-close-finished-p)
   (close-error :initform nil :accessor %application-agent-close-error)
   (close-ready :initform
                (sb-thread:make-waitqueue
                 :name "application agent provider closed")
                :reader application-agent-close-ready)
   (turn-function :initarg :turn-function :initform #'openai:agent-turn
                  :reader application-agent-turn-function)
   (close-function :initarg :close-function :initform #'openai:close-agent
                   :reader application-agent-close-function)
   (close-failure-function
    :initarg :close-failure-function :initform #'warn
    :reader application-agent-close-failure-function)
   (observer-failure-function
    :initarg :observer-failure-function :initform #'warn
    :reader application-agent-observer-failure-function))
  (:documentation
   "Provider agent plus application owner, transcript, observers, and lifecycle."))

(defmethod initialize-instance :after ((agent application-agent) &key)
  (unless (%application-agent-application agent)
    (error ":APPLICATION is required.")))

(defvar *current-agent* nil
  "The APPLICATION-AGENT whose CLIM command is executing dynamically.")

(defun application-agent-state (agent)
  (sb-thread:with-mutex ((application-agent-state-lock agent))
    (%application-agent-state agent)))

(defun application-agent-application (agent)
  "Return AGENT's application while attached, or NIL after release."
  (sb-thread:with-mutex ((application-agent-state-lock agent))
    (%application-agent-application agent)))

(defun application-agent-open-p (agent)
  (eq :open (application-agent-state agent)))

(defun application-agent-worker-thread (agent)
  (sb-thread:with-mutex ((application-agent-state-lock agent))
    (%application-agent-worker-thread agent)))

(defun application-agent-close-thread (agent)
  (sb-thread:with-mutex ((application-agent-state-lock agent))
    (%application-agent-close-thread agent)))

(defun application-agent-close-finished-p (agent)
  (sb-thread:with-mutex ((application-agent-state-lock agent))
    (%application-agent-close-finished-p agent)))

(defun application-agent-close-error (agent)
  (sb-thread:with-mutex ((application-agent-state-lock agent))
    (%application-agent-close-error agent)))

(defun application-agent-release-finished-p (agent)
  "Whether both AGENT's close request and FIFO worker have terminated."
  (sb-thread:with-mutex ((application-agent-state-lock agent))
    (and (%application-agent-close-finished-p agent)
         (%application-agent-worker-finished-p agent))))

(defun application-agent-turns (agent)
  "Return a stable newest-first snapshot of AGENT's retained turns."
  (sb-thread:with-mutex ((application-agent-state-lock agent))
    (copy-list (%application-agent-turns agent))))

(defun application-agent-current-turn (agent)
  (sb-thread:with-mutex ((application-agent-state-lock agent))
    (%application-agent-current-turn agent)))

(defun application-agent-observer-failures (agent)
  (sb-thread:with-mutex ((application-agent-state-lock agent))
    (copy-list (%application-agent-observer-failures agent))))

(defun add-agent-observer (agent function)
  "Attach FUNCTION once while AGENT is open."
  (check-type function function)
  (sb-thread:with-mutex ((application-agent-state-lock agent))
    (unless (eq :open (%application-agent-state agent))
      (error 'application-agent-released :agent agent))
    (pushnew function (application-agent-observers agent) :test #'eq))
  function)

(defun remove-agent-observer (agent function)
  "Detach FUNCTION if present; safe during notification and after release."
  (sb-thread:with-mutex ((application-agent-state-lock agent))
    (let ((present-p
            (member function (application-agent-observers agent) :test #'eq)))
      (setf (application-agent-observers agent)
            (remove function (application-agent-observers agent) :test #'eq))
      (not (null present-p)))))

(defun retain-observer-failure (agent failure)
  (sb-thread:with-mutex ((application-agent-state-lock agent))
    (when (eq :open (%application-agent-state agent))
      (push failure (%application-agent-observer-failures agent))
      (when (> (length (%application-agent-observer-failures agent))
               +observer-failure-limit+)
        (setf (cdr (nthcdr (1- +observer-failure-limit+)
                           (%application-agent-observer-failures agent)))
              nil)))))

(defun notify-agent-observers (agent kind object)
  "Notify an atomic observer snapshot, containing every failure independently."
  (let ((observers
          (sb-thread:with-mutex ((application-agent-state-lock agent))
            (copy-list (application-agent-observers agent)))))
    (dolist (observer observers)
      (handler-case (funcall observer agent kind object)
        (error (cause)
          (let ((failure
                  (make-condition 'agent-observer-failure
                                  :observer observer :kind kind
                                  :object object :cause cause)))
            (retain-observer-failure agent failure)
            ;; Reporting is diagnostic, not another observer.  A WARN handler
            ;; that promotes warnings to errors (or any custom reporter that
            ;; fails) must not prevent the remaining observers from running.
            (handler-case
                (funcall (application-agent-observer-failure-function agent)
                         failure)
              (error () nil)))))))
  object)

(defun note-tool-call (agent call)
  (let ((turn (application-agent-current-turn agent)))
    (when turn
      (push-turn-call turn call)
      (setf (turn-status turn) :working)))
  (notify-agent-observers agent :call-started call))

(defun note-tool-call-finished (agent call)
  (notify-agent-observers agent :call-finished call))

(defun event-key-name (key)
  (remove-if-not #'alphanumericp (string key)))

(defun event-value (event key)
  (cdr (find key event :key #'car
             :test (lambda (wanted actual)
                     (string-equal (event-key-name wanted)
                                   (event-key-name actual))))))

(defmethod openai:handle-agent-event ((agent application-agent) event)
  (let ((turn (application-agent-current-turn agent)))
    (when (and turn (listp event) (not (keywordp (first event))))
      (let ((type (event-value event :type)))
        (when (stringp type)
          (cond
            ((member type '("response.reasoning_summary_text.delta"
                            "response.reasoning_text.delta")
                     :test #'string=)
             (setf (turn-thought turn)
                   (concatenate 'string (turn-thought turn)
                                (or (event-value event :delta) "")))
             (notify-agent-observers agent :thought turn))
            ((string= type "response.reasoning_summary_part.added")
             (unless (string= (turn-thought turn) "")
               (setf (turn-thought turn)
                     (concatenate 'string (turn-thought turn)
                                  (string #\Newline) (string #\Newline)))))
            ((string= type "response.output_text.delta")
             (setf (turn-text turn)
                   (concatenate 'string (turn-text turn)
                                (or (event-value event :delta) "")))
             (notify-agent-observers agent :text turn))))))))

(defun run-turn (agent turn)
  "Run TURN to completion in AGENT's sole provider-owning worker."
  (setf (turn-started turn) (get-internal-real-time)
        (turn-status turn) :thinking)
  (handler-case
      (progn
        (sb-thread:with-mutex ((application-agent-state-lock agent))
          (unless (eq :open (%application-agent-state agent))
            (error 'application-agent-released :agent agent))
          (setf (%application-agent-current-turn agent) turn))
        (notify-agent-observers agent :turn-started turn)
        (let ((*handles* (application-agent-handles agent)))
          (setf (turn-response turn)
                (funcall (application-agent-turn-function agent)
                         agent (turn-prompt turn)))
          (let ((text
                  (and (turn-response turn)
                       (openai:agent-response-text (turn-response turn)))))
            (when (and text (> (length text) (length (turn-text turn))))
              (setf (turn-text turn) text)))
          (setf (turn-status turn) :done)))
    (error (condition)
      (setf (turn-error turn) condition
            (turn-status turn) :failed)))
  (finish-turn turn)
  (notify-agent-observers agent :turn-finished turn)
  turn)

(defun reject-released-turn (agent turn)
  (setf (turn-started turn) (get-internal-real-time)
        (turn-error turn)
        (make-condition 'application-agent-released :agent agent)
        (turn-status turn) :failed)
  (finish-turn turn))

(defun report-application-agent-close-failure (agent condition)
  (handler-case
      (funcall (application-agent-close-failure-function agent) condition)
    (error () nil)))

(defun close-application-agent-in-lifecycle-thread (agent)
  "Promptly initiate AGENT's terminal provider close, containing failures.

OPENAI:CLOSE-AGENT only publishes a WebSocket close frame.  It is deliberately
allowed to overlap the sole worker's active receive so the socket's :CLOSED
event can wake NEXT-AGENT-EVENT.  Ordinary provider turns remain confined to
the FIFO worker, and this lifecycle path is created exactly once."
  (let ((failure nil))
    (handler-case
        (funcall (application-agent-close-function agent) agent)
      (error (condition)
        (setf failure condition)
        (report-application-agent-close-failure agent condition)))
    (sb-thread:with-mutex ((application-agent-state-lock agent))
      (setf (%application-agent-close-error agent) failure
            (%application-agent-close-finished-p agent) t)
      (sb-thread:condition-broadcast (application-agent-close-ready agent))))
  agent)

(defun application-agent-worker-loop (agent)
  "Own AGENT's provider turns in mailbox FIFO order until release."
  (unwind-protect
       (loop
         (multiple-value-bind (message received-p)
             (sb-concurrency:receive-message
              (application-agent-worker-mailbox agent))
           (declare (ignore received-p))
           (ecase (first message)
             (:turn
              (let ((turn (second message)))
                (if (application-agent-open-p agent)
                    (run-turn agent turn)
                    (reject-released-turn agent turn))))
             (:release
              (return agent)))))
    (sb-thread:with-mutex ((application-agent-state-lock agent))
      (setf (%application-agent-worker-finished-p agent) t)
      (sb-thread:condition-broadcast (application-agent-close-ready agent)))))

(defun ensure-application-agent-worker (agent)
  "Return AGENT's sole worker.  Caller holds AGENT's state lock."
  (let ((worker (%application-agent-worker-thread agent)))
    (if (and worker (sb-thread:thread-alive-p worker))
        worker
        (setf (%application-agent-worker-finished-p agent) nil
              (%application-agent-worker-thread agent)
              (sb-thread:make-thread
               (lambda () (application-agent-worker-loop agent))
               :name "application agent provider worker")))))

(defun ask (text &key agent)
  "Publish one provider turn to AGENT's FIFO worker and return immediately."
  (unless agent
    (error ":AGENT is required by the application-neutral ASK."))
  (check-type text string)
  (let ((turn (make-instance 'turn :prompt text)))
    (sb-thread:with-mutex ((application-agent-state-lock agent))
      (unless (eq :open (%application-agent-state agent))
        (error 'application-agent-released :agent agent))
      (setf (turn-thread turn) (ensure-application-agent-worker agent))
      (push turn (%application-agent-turns agent))
      ;; Mailbox publication shares the state lock with transcript publication,
      ;; so concurrent callers have one deterministic application order.
      (sb-concurrency:send-message
       (application-agent-worker-mailbox agent) (list :turn turn)))
    turn))

(defun ask-and-wait (text &key agent (timeout 120) (print-p t))
  "ASK and wait up to TIMEOUT seconds on the caller, never the canvas thread."
  (let ((turn (ask text :agent agent)))
    (wait-for-turn turn :timeout timeout)
    (when print-p (print-transcript turn))
    turn))

(defun wait-for-application-agent-release (agent &key timeout)
  "Wait for AGENT's close path and FIFO worker, returning NIL on timeout.

Release state and application detachment do not wait for this join point.  A
finite TIMEOUT therefore remains safe even when a provider-specific close
function stalls or fails to wake its active receive."
  (let ((deadline
          (and timeout
               (+ (get-internal-real-time)
                  (round (* timeout internal-time-units-per-second))))))
    (sb-thread:with-mutex ((application-agent-state-lock agent))
      (loop until (and (%application-agent-close-finished-p agent)
                       (%application-agent-worker-finished-p agent))
            do (let ((remaining
                       (and deadline
                            (/ (- deadline (get-internal-real-time))
                               (float internal-time-units-per-second 1.0)))))
                 (when (and remaining (not (plusp remaining)))
                   (return-from wait-for-application-agent-release nil))
                 (unless (sb-thread:condition-wait
                          (application-agent-close-ready agent)
                          (application-agent-state-lock agent)
                          :timeout remaining)
                   (return-from wait-for-application-agent-release nil))))))
  agent)

(defun release-application-agent (agent)
  "Detach AGENT and initiate its provider close exactly once.

Terminal state is visible before this function returns.  A dedicated lifecycle
thread promptly sends the provider's non-destructive close frame so an active
receive can wake; it never executes on the caller or application canvas.  The
sole FIFO worker rejects queued turns and exits after the active turn settles.
Concurrent callers return NIL; the teardown owner returns true.
WAIT-FOR-APPLICATION-AGENT-RELEASE is the explicit bounded join point."
  (let ((release-p nil)
        (startup-failure nil))
    (sb-thread:with-mutex ((application-agent-state-lock agent))
      (when (eq :open (%application-agent-state agent))
        (ensure-application-agent-worker agent)
        (setf (%application-agent-state agent) :released
              (%application-agent-application agent) nil
              (%application-agent-current-turn agent) nil
              (application-agent-observers agent) '()
              (%application-agent-observer-failures agent) '()
              release-p t)
        (sb-concurrency:send-message
         (application-agent-worker-mailbox agent) (list :release))
        (handler-case
            (setf (%application-agent-close-thread agent)
                  (sb-thread:make-thread
                   (lambda ()
                     (close-application-agent-in-lifecycle-thread agent))
                   :name "application agent provider closer"))
          (error (condition)
            (setf startup-failure condition
                  (%application-agent-close-error agent) condition
                  (%application-agent-close-finished-p agent) t)
            (sb-thread:condition-broadcast
             (application-agent-close-ready agent))))))
    (when release-p
      (release-handle-table (application-agent-handles agent)))
    (when startup-failure
      (report-application-agent-close-failure agent startup-failure))
    release-p))
