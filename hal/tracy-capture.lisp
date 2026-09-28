;;; Concurrent, application-neutral ownership of Tracy capture subprocesses.
;;;
;;; Public operations only publish intent and return.  Program lookup, launch,
;;; graceful interruption, process waiting, trace validation, GUI launch, and
;;; process-handle close all happen on explicitly owned background threads.

(in-package #:luv.tracy.capture)

(defparameter *tracy-capture-program-environment* "LUV_TRACY_CAPTURE")
(defparameter *tracy-profiler-program-environment* "LUV_TRACY_PROFILER")

(defparameter *tracy-capture-poll-seconds* 1/50
  "How often the capture owner observes process exit and stop intent.")

(defparameter *tracy-capture-interrupt-attempts* 3
  "Graceful-stop failures allowed before the owner forcibly terminates.")

(defclass tracy-capture-runtime () ()
  (:documentation
   "The platform edge of a TRACY-CAPTURE-CONTROLLER.

Applications normally use NATIVE-TRACY-CAPTURE-RUNTIME.  Tests and unusual
hosts can specialize this protocol without replacing the controller's state
machine."))

(defclass native-tracy-capture-runtime (tracy-capture-runtime) ()
  (:documentation "The exact SBCL and luv Tracy process implementation."))

(defgeneric tracy-clock-now (runtime)
  (:documentation "Return the current universal time for names and diagnostics."))

(defgeneric tracy-path-exists-p (runtime pathname)
  (:documentation "Whether PATHNAME names an existing capture output."))

(defgeneric prepare-tracy-client (runtime application-name)
  (:documentation
   "Ensure the in-process Tracy client is running for APPLICATION-NAME."))

(defgeneric tracy-viewer-connected-p (runtime)
  (:documentation "Whether a Tracy consumer is already attached."))

(defgeneric resolve-tracy-program (runtime role)
  (:documentation
   "Return the exact executable pathname for ROLE.

ROLE is one of :CAPTURE, :PROFILER, or :REVEAL.  Implementations must not
silently fall back to a different tool version through PATH lookup."))

(defgeneric launch-tracy-process (runtime role program arguments)
  (:documentation
   "Launch PROGRAM for ROLE with exact string ARGUMENTS and return a handle."))

(defgeneric tracy-process-alive-p (runtime process)
  (:documentation "Whether PROCESS is still alive."))

(defgeneric interrupt-tracy-process (runtime process)
  (:documentation
   "Request PROCESS's graceful Tracy capture stop without waiting for it."))

(defgeneric terminate-tracy-process (runtime process)
  (:documentation
   "Forcibly terminate PROCESS after graceful capture shutdown failed.

This is the terminal orphan-prevention edge.  A native implementation may
discard the unfinished trace, but must make eventual process exit more likely
than another graceful request would."))

(defgeneric wait-tracy-process (runtime process)
  (:documentation "Wait for PROCESS on a controller-owned worker thread."))

(defgeneric tracy-process-exit-code (runtime process)
  (:documentation "Return PROCESS's exit code after WAIT-TRACY-PROCESS."))

(defgeneric close-tracy-process (runtime process)
  (:documentation "Release the Lisp-side handle for a finished PROCESS."))

(defmethod tracy-clock-now ((runtime native-tracy-capture-runtime))
  (declare (ignore runtime))
  (get-universal-time))

(defmethod tracy-path-exists-p
    ((runtime native-tracy-capture-runtime) pathname)
  (declare (ignore runtime))
  (not (null (probe-file pathname))))

(defmethod prepare-tracy-client
    ((runtime native-tracy-capture-runtime) application-name)
  (declare (ignore runtime))
  (luv:start-tracy :application-name application-name)
  ;; START-TRACY initially calls its caller "main".  This caller is a
  ;; deliberately separate control lane; applications name their actual
  ;; canvas lane when they next observe a connected viewer.
  (luv:name-tracy-thread "Tracy capture control")
  t)

(defmethod tracy-viewer-connected-p
    ((runtime native-tracy-capture-runtime))
  (declare (ignore runtime))
  (luv:tracy-connected-p))

(defun configured-tracy-program (variable description)
  (let ((configured (uiop:getenv variable)))
    (unless (and configured (plusp (length configured)))
      (error "No Tracy ~A is configured in ~A. Enter the luv Tracy ~
              environment (nix develop .#tracy) or configure that variable ~
              with one exact executable path."
             description variable))
    (let ((pathname (pathname configured)))
      (unless (uiop:absolute-pathname-p pathname)
        (error "~A must name one exact absolute executable, got ~S."
               variable configured))
      (or (probe-file pathname)
          (error "The Tracy ~A configured by ~A does not exist: ~A"
                 description variable pathname)))))

(defmethod resolve-tracy-program
    ((runtime native-tracy-capture-runtime) role)
  (declare (ignore runtime))
  (ecase role
    (:capture
     (configured-tracy-program
      *tracy-capture-program-environment* "capture tool"))
    (:profiler
     (configured-tracy-program
      *tracy-profiler-program-environment* "profiler GUI"))
    (:reveal
     #+darwin
     (or (probe-file #P"/usr/bin/open")
         (error "macOS capture reveal requires /usr/bin/open."))
     #-darwin
     (error "Capture reveal has no exact native program on this host."))))

(defmethod launch-tracy-process
    ((runtime native-tracy-capture-runtime) role program arguments)
  (declare (ignore runtime role))
  (sb-ext:run-program
   (namestring program) arguments
   :search nil :input nil :output nil :error nil :wait nil))

(defmethod tracy-process-alive-p
    ((runtime native-tracy-capture-runtime) process)
  (declare (ignore runtime))
  (sb-ext:process-alive-p process))

(defmethod interrupt-tracy-process
    ((runtime native-tracy-capture-runtime) process)
  (declare (ignore runtime))
  ;; SIGINT is tracy-capture's graceful disconnect-and-save operation.
  (when (sb-ext:process-alive-p process)
    (sb-ext:process-kill process sb-posix:sigint))
  t)

(defmethod terminate-tracy-process
    ((runtime native-tracy-capture-runtime) process)
  (declare (ignore runtime))
  (when (sb-ext:process-alive-p process)
    (sb-ext:process-kill process sb-posix:sigkill))
  t)

(defmethod wait-tracy-process
    ((runtime native-tracy-capture-runtime) process)
  (declare (ignore runtime))
  (sb-ext:process-wait process))

(defmethod tracy-process-exit-code
    ((runtime native-tracy-capture-runtime) process)
  (declare (ignore runtime))
  (sb-ext:process-exit-code process))

(defmethod close-tracy-process
    ((runtime native-tracy-capture-runtime) process)
  (declare (ignore runtime))
  (sb-ext:process-close process))

(defstruct (tracy-capture-diagnostic
             (:constructor make-tracy-capture-diagnostic
                 (&key operation state pathname condition timestamp)))
  "One contained asynchronous failure, suitable for an inspector or HUD."
  operation
  state
  pathname
  condition
  timestamp)

(defvar *tracy-capture-controller-counter* 0)
(defvar *tracy-capture-controller-counter-lock*
  (sb-thread:make-mutex :name "Tracy capture controller identities"))

(defun next-tracy-capture-controller-id ()
  (sb-thread:with-mutex (*tracy-capture-controller-counter-lock*)
    (incf *tracy-capture-controller-counter*)))

(defclass tracy-capture-controller ()
  ((application-name
    :initarg :application-name
    :reader tracy-capture-application-name)
   (directory
    :initarg :directory
    :reader tracy-capture-directory)
   (runtime
    :initarg :runtime
    :reader tracy-capture-runtime)
   (open-on-completion-p
    :initarg :open-on-completion-p
    :reader tracy-capture-open-on-completion-p)
   (identity
    :initarg :identity
    :reader tracy-capture-identity)
   (lock
    :initform (sb-thread:make-mutex :name "Tracy capture semantic state")
    :reader tracy-capture-lock)
   (state
    :initform :idle
    :accessor %tracy-capture-state)
   (generation
    :initform 0
    :accessor tracy-capture-generation)
   (serial
    :initform 0
    :accessor tracy-capture-serial)
   (pathname
    :initform nil
    :accessor %tracy-capture-pathname)
   (process
    :initform nil
    :accessor tracy-capture-process)
   (stop-requested-p
    :initform nil
    :accessor tracy-capture-stop-requested-p)
   (stop-sent-p
    :initform nil
    :accessor tracy-capture-stop-sent-p)
   (stop-requested-at
    :initform nil
    :accessor tracy-capture-stop-requested-at)
   (stop-failure-count
    :initform 0
    :accessor tracy-capture-stop-failure-count)
   (termination-sent-p
    :initform nil
    :accessor tracy-capture-termination-sent-p)
   (graceful-stop-seconds
    :initarg :graceful-stop-seconds
    :initform 5.0d0
    :reader tracy-capture-graceful-stop-seconds)
   (last-completed-pathname
    :initform nil
    :accessor %tracy-capture-last-completed-pathname)
   (diagnostics
    :initform nil
    :accessor %tracy-capture-diagnostics))
  (:documentation
   "One concurrent Tracy capture owner.

The public state sequence is :IDLE -> :STARTING -> :RECORDING -> :STOPPING
-> :FINALIZING -> :IDLE, with :RELEASED terminal.  A single generation owns
at most one capture subprocess.  No public operation waits for a subprocess."))

(defun make-tracy-capture-controller
    (&key application-name directory
          (runtime (make-instance 'native-tracy-capture-runtime))
          (open-on-completion-p t)
          (graceful-stop-seconds 5.0d0))
  "Make an idle controller whose trace files live beneath DIRECTORY."
  (check-type application-name string)
  (unless (plusp (length application-name))
    (error "A Tracy capture controller needs a nonempty application name."))
  (unless directory
    (error "A Tracy capture controller needs an output directory."))
  (check-type graceful-stop-seconds (real 0))
  (make-instance
   'tracy-capture-controller
   :application-name application-name
   :directory (uiop:ensure-directory-pathname directory)
   :runtime runtime
   :open-on-completion-p (not (null open-on-completion-p))
   :graceful-stop-seconds
   (coerce graceful-stop-seconds 'double-float)
   :identity (next-tracy-capture-controller-id)))

(defun tracy-capture-state (controller)
  "Return CONTROLLER's semantic state without exposing its lock."
  (sb-thread:with-mutex ((tracy-capture-lock controller))
    (%tracy-capture-state controller)))

(defmethod print-object ((controller tracy-capture-controller) stream)
  (print-unreadable-object (controller stream :type t :identity t)
    (format stream "~A ~(~A~)"
            (tracy-capture-application-name controller)
            (tracy-capture-state controller))))

(defun tracy-capture-controller-released-p (controller)
  (eq :released (tracy-capture-state controller)))

(defun tracy-capture-active-p (controller)
  "Whether a capture is starting, recording, stopping, or finalizing."
  (not (null
        (member (tracy-capture-state controller)
                '(:starting :recording :stopping :finalizing)))))

(defun tracy-capture-pathname (controller)
  "Return the current generation's reserved output pathname, if any."
  (sb-thread:with-mutex ((tracy-capture-lock controller))
    (%tracy-capture-pathname controller)))

(defun tracy-capture-last-completed-pathname (controller)
  "Return the most recent successfully frozen trace pathname, if any."
  (sb-thread:with-mutex ((tracy-capture-lock controller))
    (%tracy-capture-last-completed-pathname controller)))

(defun tracy-capture-diagnostics (controller)
  "Return an oldest-first snapshot of contained asynchronous failures."
  (sb-thread:with-mutex ((tracy-capture-lock controller))
    (reverse (copy-list (%tracy-capture-diagnostics controller)))))

(defun tracy-capture-last-diagnostic (controller)
  "Return CONTROLLER's newest contained asynchronous failure."
  (sb-thread:with-mutex ((tracy-capture-lock controller))
    (first (%tracy-capture-diagnostics controller))))

(defun diagnostic-time (controller)
  (or (ignore-errors
        (tracy-clock-now (tracy-capture-runtime controller)))
      (get-universal-time)))

(defun %record-tracy-capture-diagnostic
    (controller operation condition &optional pathname state)
  "Record a diagnostic while CONTROLLER's lock is already held."
  (push (make-tracy-capture-diagnostic
         :operation operation
         :state (or state (%tracy-capture-state controller))
         :pathname pathname
         :condition condition
         :timestamp (diagnostic-time controller))
        (%tracy-capture-diagnostics controller)))

(defun record-tracy-capture-diagnostic
    (controller operation condition &optional pathname state)
  (sb-thread:with-mutex ((tracy-capture-lock controller))
    (%record-tracy-capture-diagnostic
     controller operation condition pathname state)))

(defun safe-capture-name (name)
  (string-downcase
   (with-output-to-string (stream)
     (loop for character across name
           do (write-char
               (if (alphanumericp character) character #\-)
               stream)))))

(defun reserve-tracy-capture-pathname (controller)
  "Reserve a unique semantic name while CONTROLLER's lock is held."
  (let* ((runtime (tracy-capture-runtime controller))
         (now (tracy-clock-now runtime)))
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time now 0)
      (ensure-directories-exist (tracy-capture-directory controller))
      (loop
        for serial = (prog1 (tracy-capture-serial controller)
                       (incf (tracy-capture-serial controller)))
        for pathname =
          (merge-pathnames
           (format nil
                   "~A-~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0DZ-p~D-c~D-~D.tracy"
                   (safe-capture-name
                    (tracy-capture-application-name controller))
                   year month date hour minute second
                   (sb-posix:getpid)
                   (tracy-capture-identity controller)
                   serial)
           (tracy-capture-directory controller))
        unless (tracy-path-exists-p runtime pathname)
          return pathname))))

(defun generation-current-p (controller generation)
  (= generation (tracy-capture-generation controller)))

(defun finish-tracy-capture-generation (controller generation)
  "Clear GENERATION's ownership while CONTROLLER's lock is already held."
  (when (generation-current-p controller generation)
    (setf (tracy-capture-process controller) nil
          (%tracy-capture-pathname controller) nil
          (tracy-capture-stop-requested-p controller) nil
          (tracy-capture-stop-sent-p controller) nil
          (tracy-capture-stop-requested-at controller) nil
          (tracy-capture-stop-failure-count controller) 0
          (tracy-capture-termination-sent-p controller) nil)
    (unless (eq :released (%tracy-capture-state controller))
      (setf (%tracy-capture-state controller) :idle))))

(defun spawn-tracy-controller-thread
    (controller operation pathname function)
  "Spawn FUNCTION, containing thread creation and unhandled worker failures."
  (handler-case
      (sb-thread:make-thread
       (lambda ()
         (handler-case (funcall function)
           (error (condition)
             (record-tracy-capture-diagnostic
              controller operation condition pathname))))
       :name (format nil "~A Tracy ~(~A~)"
                     (tracy-capture-application-name controller)
                     operation))
    (error (condition)
      (record-tracy-capture-diagnostic
       controller operation condition pathname)
      nil)))

(defun capture-process-arguments (pathname)
  (list "-o" (namestring pathname) "-a" "127.0.0.1"))

(defun auxiliary-process-arguments (role pathname)
  (ecase role
    (:profiler (list (namestring pathname)))
    (:reveal
     #+darwin (list "-R" (namestring pathname))
     #-darwin (list (namestring pathname)))))

(defun run-tracy-auxiliary-process (controller operation role pathname)
  "Launch, reap, and close one non-capture helper on this worker thread."
  (let* ((runtime (tracy-capture-runtime controller))
         (process nil))
    (unwind-protect
         (progn
           (unless (tracy-path-exists-p runtime pathname)
             (error "No completed Tracy capture exists at ~A." pathname))
           (let ((program (resolve-tracy-program runtime role)))
             (setf process
                   (launch-tracy-process
                    runtime role program
                    (auxiliary-process-arguments role pathname))))
           (wait-tracy-process runtime process)
           (let ((exit-code (tracy-process-exit-code runtime process)))
             (unless (eql 0 exit-code)
               (error "Tracy ~(~A~) process exited with code ~S for ~A."
                      role exit-code pathname))))
      (when process
        (handler-case (close-tracy-process runtime process)
          (error (condition)
            (record-tracy-capture-diagnostic
             controller operation condition pathname)))))))

(defun schedule-tracy-auxiliary-process
    (controller operation role pathname)
  (spawn-tracy-controller-thread
   controller operation pathname
   (lambda ()
     (run-tracy-auxiliary-process
      controller operation role pathname))))

(defun open-tracy-capture (controller &optional pathname)
  "Open PATHNAME, or the last completed trace, without waiting for its GUI."
  (let ((target
          (or pathname
              (tracy-capture-last-completed-pathname controller))))
    (when target
      (schedule-tracy-auxiliary-process
       controller :open :profiler target))
    target))

(defun reveal-tracy-capture (controller &optional pathname)
  "Reveal PATHNAME, or the last completed trace, without waiting for Finder."
  (let ((target
          (or pathname
              (tracy-capture-last-completed-pathname controller))))
    (when target
      (schedule-tracy-auxiliary-process
       controller :reveal :reveal target))
    target))

(defun tracy-stop-elapsed-seconds (controller)
  (let ((started (tracy-capture-stop-requested-at controller)))
    (when started
      (/ (- (get-internal-real-time) started)
         (coerce internal-time-units-per-second 'double-float)))))

(defun claim-tracy-capture-stop-action
    (controller generation process)
  "Claim the next stop action for GENERATION while its owner remains alive."
  (sb-thread:with-mutex ((tracy-capture-lock controller))
    (when (and (generation-current-p controller generation)
               (eq process (tracy-capture-process controller))
               (tracy-capture-stop-requested-p controller))
      (cond
        ((tracy-capture-termination-sent-p controller) nil)
        ((or (>= (tracy-capture-stop-failure-count controller)
                 *tracy-capture-interrupt-attempts*)
             (and (tracy-capture-stop-sent-p controller)
                  (>= (or (tracy-stop-elapsed-seconds controller) 0d0)
                      (tracy-capture-graceful-stop-seconds controller))))
         (setf (tracy-capture-termination-sent-p controller) t)
         :terminate)
        ((not (tracy-capture-stop-sent-p controller))
         (setf (tracy-capture-stop-sent-p controller) t)
         :interrupt)))))

(defun perform-tracy-capture-stop-action
    (controller generation process pathname action)
  "Perform ACTION on the generation-owning worker, retrying contained errors."
  (let ((runtime (tracy-capture-runtime controller)))
    (ecase action
      (:interrupt
       (handler-case
           (interrupt-tracy-process runtime process)
         (error (condition)
           (sb-thread:with-mutex ((tracy-capture-lock controller))
             (when (and (generation-current-p controller generation)
                        (eq process (tracy-capture-process controller)))
               (setf (tracy-capture-stop-sent-p controller) nil)
               (incf (tracy-capture-stop-failure-count controller))
               (%record-tracy-capture-diagnostic
                controller :stop condition pathname))))))
      (:terminate
       (record-tracy-capture-diagnostic
        controller :terminate
        (make-condition
         'simple-error
         :format-control
         "Graceful Tracy shutdown did not settle; forcibly terminating ~A."
         :format-arguments (list pathname))
        pathname)
       (handler-case
           (terminate-tracy-process runtime process)
         (error (condition)
           (sb-thread:with-mutex ((tracy-capture-lock controller))
             (when (and (generation-current-p controller generation)
                        (eq process (tracy-capture-process controller)))
               ;; A failed terminal signal remains owned and is retried on the
               ;; next bounded observation rather than becoming an orphan.
               (setf (tracy-capture-termination-sent-p controller) nil)
               (%record-tracy-capture-diagnostic
                controller :terminate condition pathname)))))))))

(defun service-tracy-capture-stop
    (controller generation process pathname)
  "Let the capture owner service one published stop intent without a new thread."
  (let ((action
          (claim-tracy-capture-stop-action
           controller generation process)))
    (when action
      (perform-tracy-capture-stop-action
       controller generation process pathname action))))

(defun wait-for-owned-tracy-process
    (controller generation pathname process)
  "Observe PROCESS on its owner worker, servicing stop and escalation intents."
  (let ((runtime (tracy-capture-runtime controller)))
    (loop while (tracy-process-alive-p runtime process)
          do (service-tracy-capture-stop
              controller generation process pathname)
             (when (tracy-process-alive-p runtime process)
               (sleep *tracy-capture-poll-seconds*)))
    ;; Reap exactly once after the nonblocking observations report exit.
    (wait-tracy-process runtime process)))

(defun finalize-tracy-capture
    (controller generation pathname process)
  "Wait, validate, publish, and close one exact capture generation."
  (let* ((runtime (tracy-capture-runtime controller))
         (completed-p nil)
         (open-p nil))
    (unwind-protect
         (handler-case
             (progn
               (wait-for-owned-tracy-process
                controller generation pathname process)
               (sb-thread:with-mutex ((tracy-capture-lock controller))
                 (when (and (generation-current-p controller generation)
                            (not (eq :released
                                     (%tracy-capture-state controller))))
                   (setf (%tracy-capture-state controller) :finalizing)))
               (let ((exit-code (tracy-process-exit-code runtime process)))
                 (unless (eql 0 exit-code)
                   (error "tracy-capture exited with code ~S for ~A."
                          exit-code pathname)))
               (unless (tracy-path-exists-p runtime pathname)
                 (error "tracy-capture exited successfully without writing ~A."
                        pathname))
               (sb-thread:with-mutex ((tracy-capture-lock controller))
                 (when (generation-current-p controller generation)
                   (setf (%tracy-capture-last-completed-pathname controller)
                         pathname
                         completed-p t
                         open-p
                         (and (tracy-capture-open-on-completion-p controller)
                              (not (eq :released
                                       (%tracy-capture-state controller))))))))
           (error (condition)
             (record-tracy-capture-diagnostic
              controller :finalize condition pathname)
             ;; Closing a live Lisp process handle does not terminate its OS
             ;; child.  A finalizer failure therefore makes one terminal
             ;; attempt before the handle becomes unreachable.
             (handler-case
                 (when (tracy-process-alive-p runtime process)
                   (terminate-tracy-process runtime process))
               (error (termination-condition)
                 (record-tracy-capture-diagnostic
                  controller :terminate termination-condition pathname)))))
      (handler-case (close-tracy-process runtime process)
        (error (condition)
          (record-tracy-capture-diagnostic
           controller :close condition pathname)))
      (sb-thread:with-mutex ((tracy-capture-lock controller))
        (finish-tracy-capture-generation controller generation)))
    (when (and completed-p open-p)
      (open-tracy-capture controller pathname))))

(defun run-tracy-capture-generation (controller generation pathname)
  "Launch and own GENERATION entirely on its background worker."
  (let* ((runtime (tracy-capture-runtime controller))
         (process nil)
         (installed-p nil)
         (stop-now-p nil))
    (handler-case
        (progn
          (prepare-tracy-client
           runtime (tracy-capture-application-name controller))
          (when (tracy-viewer-connected-p runtime)
            (error "A Tracy viewer is already connected; stop it before starting a capture."))
          (let ((program (resolve-tracy-program runtime :capture)))
            (setf process
                  (launch-tracy-process
                   runtime :capture program
                   (capture-process-arguments pathname))))
          (sb-thread:with-mutex ((tracy-capture-lock controller))
            (when (generation-current-p controller generation)
              (setf (tracy-capture-process controller) process
                    installed-p t
                    stop-now-p
                    (or (tracy-capture-stop-requested-p controller)
                        (eq :released (%tracy-capture-state controller))))
              (unless (or stop-now-p
                          (eq :released (%tracy-capture-state controller)))
                (setf (%tracy-capture-state controller) :recording))))
          (unless installed-p
            ;; A stale launch is never allowed to become an orphan.
            (when (tracy-process-alive-p runtime process)
              (interrupt-tracy-process runtime process)))
          ;; When STOP-NOW-P is true, the generation owner observes the
          ;; already-published intent immediately in its bounded wait loop.  A
          ;; second worker is neither needed nor able to lose that signal.
          (finalize-tracy-capture
           controller generation pathname process))
      (error (condition)
        (when process
          (handler-case
              (when (tracy-process-alive-p runtime process)
                (terminate-tracy-process runtime process))
            (error (termination-condition)
              (record-tracy-capture-diagnostic
               controller :terminate termination-condition pathname)))
          (handler-case (close-tracy-process runtime process)
            (error (close-condition)
              (record-tracy-capture-diagnostic
               controller :close close-condition pathname))))
        (sb-thread:with-mutex ((tracy-capture-lock controller))
          (%record-tracy-capture-diagnostic
           controller :start condition pathname)
          (finish-tracy-capture-generation controller generation))))))

(defun begin-tracy-capture (controller)
  "Publish a new generation and return its pathname and worker closure."
  (sb-thread:with-mutex ((tracy-capture-lock controller))
    (ecase (%tracy-capture-state controller)
      (:idle
       (let ((pathname (reserve-tracy-capture-pathname controller))
             (generation (incf (tracy-capture-generation controller))))
         (setf (%tracy-capture-state controller) :starting
               (%tracy-capture-pathname controller) pathname
               (tracy-capture-process controller) nil
               (tracy-capture-stop-requested-p controller) nil
               (tracy-capture-stop-sent-p controller) nil
               (tracy-capture-stop-requested-at controller) nil
               (tracy-capture-stop-failure-count controller) 0
               (tracy-capture-termination-sent-p controller) nil)
         (values pathname generation)))
      ((:starting :recording :stopping :finalizing)
       (values (%tracy-capture-pathname controller) nil))
      (:released
       (error "The Tracy capture controller for ~A has been released."
              (tracy-capture-application-name controller))))))

(defun start-tracy-capture (controller)
  "Start one capture asynchronously, or return the current capture pathname."
  (multiple-value-bind (pathname generation)
      (begin-tracy-capture controller)
    (when generation
      (unless
          (spawn-tracy-controller-thread
           controller :start pathname
           (lambda ()
             (run-tracy-capture-generation
              controller generation pathname)))
        (sb-thread:with-mutex ((tracy-capture-lock controller))
          (when (generation-current-p controller generation)
            (finish-tracy-capture-generation controller generation)))))
    pathname))

(defun note-tracy-capture-stop-requested (controller)
  "Publish the first stop timestamp while CONTROLLER's lock is held."
  (setf (tracy-capture-stop-requested-p controller) t)
  (unless (tracy-capture-stop-requested-at controller)
    (setf (tracy-capture-stop-requested-at controller)
          (get-internal-real-time))))

(defun request-tracy-capture-stop (controller release-p)
  "Atomically publish stop intent for the generation-owning worker."
  (sb-thread:with-mutex ((tracy-capture-lock controller))
    (let ((pathname (%tracy-capture-pathname controller)))
      (case (%tracy-capture-state controller)
        (:idle
         (when release-p
           (setf (%tracy-capture-state controller) :released)))
        (:starting
         (note-tracy-capture-stop-requested controller)
         (setf (%tracy-capture-state controller)
               (if release-p :released :stopping)))
        (:recording
         (note-tracy-capture-stop-requested controller)
         (setf (%tracy-capture-state controller)
               (if release-p :released :stopping)))
        ((:stopping :finalizing)
         (when release-p
           (setf (%tracy-capture-state controller) :released)))
        (:released
         ;; Terminal idempotence needs no public retry call: the generation
         ;; owner retains the controller and automatically retries itself.
         nil))
      pathname)))

(defun stop-tracy-capture (controller)
  "Request one graceful stop and return immediately.

Concurrent and repeated calls publish only one interrupt for a generation."
  (request-tracy-capture-stop controller nil))

(defun toggle-tracy-capture (controller)
  "Atomically start from idle or stop the one busy generation."
  (let ((pathname nil)
        (generation nil)
        (start-p nil))
    (sb-thread:with-mutex ((tracy-capture-lock controller))
      (case (%tracy-capture-state controller)
        (:idle
         (setf pathname (reserve-tracy-capture-pathname controller)
               generation (incf (tracy-capture-generation controller))
               (%tracy-capture-state controller) :starting
               (%tracy-capture-pathname controller) pathname
               (tracy-capture-process controller) nil
               (tracy-capture-stop-requested-p controller) nil
               (tracy-capture-stop-sent-p controller) nil
               (tracy-capture-stop-requested-at controller) nil
               (tracy-capture-stop-failure-count controller) 0
               (tracy-capture-termination-sent-p controller) nil
               start-p t))
        ((:starting :recording)
         (setf pathname (%tracy-capture-pathname controller)
               generation (tracy-capture-generation controller)
               (%tracy-capture-state controller) :stopping)
         (note-tracy-capture-stop-requested controller))
        ((:stopping :finalizing)
         (setf pathname (%tracy-capture-pathname controller)))
        (:released
         (error "The Tracy capture controller for ~A has been released."
                (tracy-capture-application-name controller)))))
    (when start-p
      (unless
          (spawn-tracy-controller-thread
           controller :start pathname
           (lambda ()
             (run-tracy-capture-generation
              controller generation pathname)))
        (sb-thread:with-mutex ((tracy-capture-lock controller))
          (when (generation-current-p controller generation)
            (finish-tracy-capture-generation controller generation)))))
    pathname))

(defun release-tracy-capture-controller (controller)
  "Detach CONTROLLER immediately and asynchronously stop its owned capture.

This operation is terminal and idempotent.  It never waits for capture
finalization or for a profiler window."
  (request-tracy-capture-stop controller t)
  nil)
