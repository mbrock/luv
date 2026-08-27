(in-package #:luft.render.tests)

(defvar *instrument-test-events* nil)

(defstruct (instrument-owner-request
             (:constructor make-instrument-owner-request (function)))
  function
  (completion (sb-thread:make-semaphore :count 0) :read-only t)
  values
  condition)

(defclass instrument-owner-canvas ()
  ((state :initform :open :accessor instrument-owner-canvas-state)
   (mailbox
    :initform (sb-concurrency:make-mailbox
               :name "LUFT instrument owner requests")
    :reader instrument-owner-canvas-mailbox)
   (thread :reader instrument-owner-canvas-thread)
   (request-lock
    :initform (sb-thread:make-mutex
               :name "LUFT instrument owner request count")
    :reader instrument-owner-canvas-request-lock)
   (request-count :initform 0 :accessor instrument-owner-canvas-request-count)
   (request-enqueued
    :initform (sb-thread:make-semaphore :count 0)
    :reader instrument-owner-canvas-request-enqueued)
   (reject-requests-p
    :initform nil :accessor instrument-owner-canvas-reject-requests-p)
   (stopped
    :initform (sb-thread:make-semaphore :count 0)
    :reader instrument-owner-canvas-stopped)))

(defun run-instrument-owner-canvas (canvas)
  (unwind-protect
       (loop for request = (sb-concurrency:receive-message
                            (instrument-owner-canvas-mailbox canvas))
             until (eq request :stop)
             do (handler-case
                    (setf (instrument-owner-request-values request)
                          (multiple-value-list
                           (funcall (instrument-owner-request-function request)
                                    0.0d0)))
                  (error (condition)
                    (setf (instrument-owner-request-condition request)
                          condition)))
                (sb-thread:signal-semaphore
                 (instrument-owner-request-completion request)))
    (sb-thread:signal-semaphore (instrument-owner-canvas-stopped canvas))))

(defmethod initialize-instance :after ((canvas instrument-owner-canvas) &key)
  (setf (slot-value canvas 'thread)
        (sb-thread:make-thread
         (lambda () (run-instrument-owner-canvas canvas))
         :name "LUFT instrument native owner")))

(defmethod luv:canvas-state ((canvas instrument-owner-canvas))
  (instrument-owner-canvas-state canvas))

(defmethod luv:canvas-thread-p ((canvas instrument-owner-canvas))
  (and (slot-boundp canvas 'thread)
       (eq sb-thread:*current-thread*
           (instrument-owner-canvas-thread canvas))))

(defmethod luv:request-canvas-frame
    ((canvas instrument-owner-canvas) function)
  (when (luv:canvas-thread-p canvas)
    (return-from luv:request-canvas-frame (funcall function 0.0d0)))
  (when (instrument-owner-canvas-reject-requests-p canvas)
    (error "The instrument owner rejected the request before its callback."))
  (unless (eq :open (instrument-owner-canvas-state canvas))
    (error "The instrument owner canvas is not open."))
  (let ((request (make-instrument-owner-request function)))
    (sb-thread:with-mutex ((instrument-owner-canvas-request-lock canvas))
      (incf (instrument-owner-canvas-request-count canvas)))
    (sb-concurrency:send-message
     (instrument-owner-canvas-mailbox canvas) request)
    (sb-thread:signal-semaphore
     (instrument-owner-canvas-request-enqueued canvas))
    (unless (sb-thread:wait-on-semaphore
             (instrument-owner-request-completion request) :timeout 2.0)
      (error "The instrument owner did not service a frame request."))
    (when (instrument-owner-request-condition request)
      (error (instrument-owner-request-condition request)))
    (values-list (instrument-owner-request-values request))))

(defun close-instrument-owner-canvas (canvas)
  (unless (eq :closed (instrument-owner-canvas-state canvas))
    (setf (instrument-owner-canvas-state canvas) :closed)
    (sb-concurrency:send-message
     (instrument-owner-canvas-mailbox canvas) :stop)
    (unless (sb-thread:wait-on-semaphore
             (instrument-owner-canvas-stopped canvas) :timeout 2.0)
      (error "The instrument owner did not stop."))
    (sb-thread:join-thread (instrument-owner-canvas-thread canvas)))
  canvas)

(defun wait-for-instrument-owner-request (canvas)
  (unless (sb-thread:wait-on-semaphore
           (instrument-owner-canvas-request-enqueued canvas) :timeout 1.0)
    (error "The expected instrument owner request was not enqueued.")))

(defclass instrument-test-probe ()
  ((name :initarg :name :reader instrument-test-name)
   (priority :initarg :priority :reader instrument-test-priority)
   (handled-p :initarg :handled-p :initform nil
              :reader instrument-test-handled-p)
   (release-count :initform 0 :accessor instrument-test-release-count)
   (released-on-owner-p
    :initform nil :accessor instrument-test-released-on-owner-p)
   (fail-release-p :initarg :fail-release-p :initform nil
                   :reader instrument-test-fail-release-p)))

(defmethod render::viewer-instrument-priority
    ((instrument instrument-test-probe))
  (instrument-test-priority instrument))

(defmethod render::refresh-viewer-instrument
    ((instrument instrument-test-probe) viewer)
  (declare (ignore viewer))
  (push (list :refresh (instrument-test-name instrument))
        *instrument-test-events*)
  instrument)

(defmethod render::encode-viewer-instrument
    ((instrument instrument-test-probe)
     viewer pass surface-texture physical-extent)
  (declare (ignore viewer pass surface-texture))
  (push (list :encode (instrument-test-name instrument) physical-extent)
        *instrument-test-events*)
  instrument)

(defmethod render::handle-viewer-instrument-event
    ((instrument instrument-test-probe) viewer canvas event)
  (declare (ignore viewer canvas event))
  (push (list :event (instrument-test-name instrument))
        *instrument-test-events*)
  (instrument-test-handled-p instrument))

(defmethod render::release-viewer-instrument
    ((instrument instrument-test-probe) viewer)
  (let ((canvas (render::viewer-instrument-canvas viewer)))
    (setf (instrument-test-released-on-owner-p instrument)
          (and canvas (luv:canvas-thread-p canvas))))
  (incf (instrument-test-release-count instrument))
  (push (list :release (instrument-test-name instrument))
        *instrument-test-events*)
  (when (instrument-test-fail-release-p instrument)
    (error "Deliberate instrument release failure.")))

(define-test stopped-viewers-consume-and-reject-late-instruments
  (let* ((canvas (make-instance 'instrument-owner-canvas))
         (viewer (clim:make-application-frame 'render:viewer :canvas canvas))
         (owned (make-instance 'instrument-test-probe
                               :name :owned :priority 10))
         (late (make-instance 'instrument-test-probe
                              :name :late :priority 10
                              :fail-release-p t))
         (condition nil))
    (unwind-protect
         (progn
           (render::add-viewer-instrument viewer owned)
           (render::add-viewer-instrument viewer owned)
           (true (= 1 (length (render::viewer-instruments viewer))))
           (luv:call-with-stop-controller
            (render::viewer-stop-controller viewer) (lambda () nil))
           (setf (instrument-owner-canvas-reject-requests-p canvas) t)
           (true (eq owned (render::add-viewer-instrument viewer owned)))
           (true (zerop (instrument-test-release-count owned)))
           (true (= 1 (length (render::viewer-instruments viewer))))
           (setf (instrument-owner-canvas-reject-requests-p canvas) nil)
           (true (render::remove-viewer-instrument viewer owned))
           (handler-bind ((luv:release-warning #'muffle-warning))
             (handler-case (render::add-viewer-instrument viewer late)
               (luv:application-attachment-closed (failure)
                 (setf condition failure))))
           (true (typep condition 'luv:application-attachment-closed))
           (true (eq late
                     (luv:application-attachment-closed-attachment condition)))
           (true (eq :stopped
                     (luv:application-attachment-closed-state condition)))
           (true (= 1 (instrument-test-release-count owned)))
           (true (= 1 (instrument-test-release-count late)))
           (true (instrument-test-released-on-owner-p late))
           (true (null (render::viewer-instruments viewer))))
      (ignore-errors (render::release-viewer-instruments viewer))
      (close-instrument-owner-canvas canvas))))

(define-test instrument-add-versus-stop-never-repopulates-the-terminal-registry
  ;; This is a bounded integration regression, not a scheduler stress test.
  ;; Four fresh simultaneous starts exercise both legal outcomes without making
  ;; routine renderer work pay for sixteen canvas/thread setup cycles.
  (dotimes (iteration 4)
    (declare (ignore iteration))
    (let* ((canvas (make-instance 'instrument-owner-canvas))
           (viewer (clim:make-application-frame 'render:viewer :canvas canvas))
           (instrument (make-instance 'instrument-test-probe
                                      :name :racing :priority 10))
           (start (sb-thread:make-semaphore :count 0))
           (finished (sb-thread:make-semaphore :count 0))
           (add-condition nil)
           (stop-condition nil)
           (add-thread nil)
           (stop-thread nil))
      (unwind-protect
           (progn
             (setf add-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (progn
                             (sb-thread:wait-on-semaphore start)
                             (handler-case
                                 (render::add-viewer-instrument
                                  viewer instrument)
                               (error (failure)
                                 (setf add-condition failure))))
                        (sb-thread:signal-semaphore finished)))
                    :name "LUFT racing instrument add")
                   stop-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (progn
                             (sb-thread:wait-on-semaphore start)
                             (handler-case
                                 (luv:call-with-stop-controller
                                  (render::viewer-stop-controller viewer)
                                  (lambda ()
                                    (render::release-viewer-instruments viewer)))
                               (error (failure)
                                 (setf stop-condition failure))))
                        (sb-thread:signal-semaphore finished)))
                    :name "LUFT racing instrument stop"))
             (sb-thread:signal-semaphore start)
             (sb-thread:signal-semaphore start)
             (unless (and (sb-thread:wait-on-semaphore finished :timeout 2.0)
                          (sb-thread:wait-on-semaphore finished :timeout 2.0))
               (error "The LUFT attachment race did not settle."))
             (sb-thread:join-thread add-thread)
             (setf add-thread nil)
             (sb-thread:join-thread stop-thread)
             (setf stop-thread nil)
             (true (null stop-condition))
             (true (or (null add-condition)
                       (typep add-condition
                              'luv:application-attachment-closed)))
             (true (eq :stopped
                       (luv:stop-controller-state
                        (render::viewer-stop-controller viewer))))
             (true (null (render::viewer-instruments viewer)))
             (true (= 1 (instrument-test-release-count instrument)))
             (true (instrument-test-released-on-owner-p instrument)))
        (sb-thread:signal-semaphore start)
        (sb-thread:signal-semaphore start)
        (when add-thread (sb-thread:join-thread add-thread))
        (when stop-thread (sb-thread:join-thread stop-thread))
        (ignore-errors (render::release-viewer-instruments viewer))
        (close-instrument-owner-canvas canvas)))))

(defclass instrument-test-renderer-replacement ()
  ((replacement :initarg :replacement
                :reader instrument-test-renderer-replacement)))

(defmethod render::refresh-viewer-instrument
    ((instrument instrument-test-renderer-replacement) viewer)
  ;; This is the semantic shape of a metabar operation which rebuilds and
  ;; publishes the renderer cohort at the frame boundary.
  (setf (render::viewer-renderer viewer)
        (instrument-test-renderer-replacement instrument))
  instrument)

(defclass inspector-preparation-probe ()
  ((revisions :initform nil
              :accessor inspector-preparation-probe-revisions)))

(defmethod mcluv::prepare-mirror-compositor-revision
    ((probe inspector-preparation-probe)
     (mirror mcluv:luv-gpu-mirror) revision)
  (declare (ignore mirror))
  (push revision (inspector-preparation-probe-revisions probe)))

(define-test a-static-inspector-prepares-before-the-renderer-is-borrowed
  (let* ((viewer
           (clim:make-application-frame
            'render:viewer :renderer :installed-cohort :inspector-p t))
         (mirror
           (make-instance 'mcluv:luv-gpu-mirror
                          :sheet nil :target nil :context nil))
         (probe (make-instance 'inspector-preparation-probe))
         (revision
           (mcluv::make-gpu-mirror-prepared-revision
            mirror nil #() #() #() #() #() #())))
    (unwind-protect
         (progn
           (mcluv::publish-gpu-mirror-prepared-revision mirror revision)
           (setf (mcluv:mirror-compositor mirror) probe
                 (render::viewer-inspector-mirror viewer) mirror)
           (true (eq :installed-cohort
                     (render::prepare-viewer-frame-renderer viewer)))
           (true (equal (list revision)
                        (inspector-preparation-probe-revisions probe))))
      (render::release-viewer-instruments viewer))))

(define-test a-frame-borrows-the-renderer-after-instrument-publication
  (let* ((viewer
           (clim:make-application-frame
            'render:viewer :renderer :retired-cohort))
         (instrument
           (make-instance 'instrument-test-renderer-replacement
                          :replacement :installed-cohort)))
    (unwind-protect
         (progn
           (render::add-viewer-instrument viewer instrument)
           (true (eq :installed-cohort
                     (render::prepare-viewer-frame-renderer viewer)))
           (true (eq :installed-cohort (render::viewer-renderer viewer))))
      (render::release-viewer-instruments viewer))))

(define-test viewer-instruments-have-explicit-input-paint-and-release-order
  (let* ((viewer (gensym "VIEWER"))
         (low (make-instance 'instrument-test-probe
                             :name :low :priority 10))
         (modal (make-instance 'instrument-test-probe
                               :name :modal :priority 100 :handled-p t))
         (*instrument-test-events* nil))
    (unwind-protect
         (progn
           (render::add-viewer-instrument viewer low)
           (render::add-viewer-instrument viewer modal)
           (render::add-viewer-instrument viewer modal)
           (true (equal '(:modal :low)
                        (mapcar #'instrument-test-name
                                (render::viewer-instruments viewer))))
           (render::refresh-viewer-instruments viewer)
           (true (equal '((:refresh :modal) (:refresh :low))
                        (nreverse *instrument-test-events*)))
           (setf *instrument-test-events* nil)
           (render::encode-viewer-instruments
            viewer :pass :surface '(2200 1600))
           (true (equal '((:encode :low (2200 1600))
                          (:encode :modal (2200 1600)))
                        (nreverse *instrument-test-events*)))
           (setf *instrument-test-events* nil)
           (true (render::dispatch-viewer-instrument-event
                  viewer nil (make-instance 'luv:canvas-event :timestamp 0)))
           (true (equal '((:event :modal)) *instrument-test-events*))
           (setf *instrument-test-events* nil)
           (true (render::remove-viewer-instrument viewer modal))
           (true (equal '((:release :modal)) *instrument-test-events*))
           (true (equal (list low) (render::viewer-instruments viewer))))
      (render::release-viewer-instruments viewer))
    (true (equal '((:release :low) (:release :modal))
                 *instrument-test-events*))
    (true (null (render::viewer-instruments viewer)))))

(define-test off-thread-instrument-removal-waits-for-the-borrowing-frame
  (let* ((canvas (make-instance 'instrument-owner-canvas))
         (viewer (clim:make-application-frame 'render:viewer :canvas canvas))
         (instrument (make-instance 'instrument-test-probe
                                    :name :owned :priority 10))
         (borrowed (sb-thread:make-semaphore :count 0))
         (continue (sb-thread:make-semaphore :count 0))
         (frame-finished (sb-thread:make-semaphore :count 0))
         (remove-finished (sb-thread:make-semaphore :count 0))
         (snapshot-kept-p nil)
         (remove-result nil)
         (frame-condition nil)
         (remove-condition nil)
         (frame-thread nil)
         (remove-thread nil))
    (unwind-protect
         (progn
           (render::add-viewer-instrument viewer instrument)
           (wait-for-instrument-owner-request canvas)
           (setf frame-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (luv:request-canvas-frame
                         canvas
                         (lambda (timestamp)
                           (declare (ignore timestamp))
                           (let ((snapshot
                                   (render::viewer-instruments viewer)))
                             (sb-thread:signal-semaphore borrowed)
                             (unless (sb-thread:wait-on-semaphore
                                      continue :timeout 1.0)
                               (error "The borrowing frame was not released."))
                             (setf snapshot-kept-p
                                   (eq instrument (first snapshot))))))
                      (error (condition)
                        (setf frame-condition condition)))
                    (sb-thread:signal-semaphore frame-finished))
                  :name "LUFT borrowing frame"))
           (wait-for-instrument-owner-request canvas)
           (unless (sb-thread:wait-on-semaphore borrowed :timeout 1.0)
             (error "The instrument frame did not borrow its snapshot."))
           (setf remove-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (setf remove-result
                              (render::remove-viewer-instrument
                               viewer instrument))
                      (error (condition)
                        (setf remove-condition condition)))
                    (sb-thread:signal-semaphore remove-finished))
                  :name "LUFT off-thread instrument removal"))
           (wait-for-instrument-owner-request canvas)
           (true (not (sb-thread:wait-on-semaphore
                       remove-finished :timeout 0.02))
                 "remove remains synchronous while the frame owns its snapshot")
           (true (zerop (instrument-test-release-count instrument))
                 "the borrowed instrument has not been released")
           (sb-thread:signal-semaphore continue)
           (unless (sb-thread:wait-on-semaphore frame-finished :timeout 1.0)
             (error "The borrowing frame did not finish."))
           (unless (sb-thread:wait-on-semaphore remove-finished :timeout 1.0)
             (error "The queued instrument removal did not finish."))
           (sb-thread:join-thread frame-thread)
           (setf frame-thread nil)
           (sb-thread:join-thread remove-thread)
           (setf remove-thread nil)
           (true (null frame-condition))
           (true (null remove-condition))
           (true snapshot-kept-p)
           (true remove-result)
           (true (= 1 (instrument-test-release-count instrument)))
           (true (instrument-test-released-on-owner-p instrument))
           (true (null (render::viewer-instruments viewer))))
      (sb-thread:signal-semaphore continue)
      (when frame-thread (sb-thread:join-thread frame-thread))
      (when remove-thread (sb-thread:join-thread remove-thread))
      (ignore-errors (render::release-viewer-instruments viewer))
      (close-instrument-owner-canvas canvas))))

(define-test instrument-release-errors-return-through-the-frame-boundary
  (let* ((canvas (make-instance 'instrument-owner-canvas))
         (viewer (clim:make-application-frame 'render:viewer :canvas canvas))
         (instrument (make-instance 'instrument-test-probe
                                    :name :failing :priority 10
                                    :fail-release-p t))
         (condition nil))
    (unwind-protect
         (progn
           (render::add-viewer-instrument viewer instrument)
           (wait-for-instrument-owner-request canvas)
           (handler-case
               (render::remove-viewer-instrument viewer instrument)
             (error (failure)
               (setf condition failure)))
           (wait-for-instrument-owner-request canvas)
           (true (typep condition 'error))
           (true (= 1 (instrument-test-release-count instrument)))
           (true (instrument-test-released-on-owner-p instrument))
           (true (null (render::viewer-instruments viewer))))
      (ignore-errors (render::release-viewer-instruments viewer))
      (close-instrument-owner-canvas canvas))))

(define-test luft-m-x-is-an-application-command-available-with-pointer-released
  (let* ((viewer (clim:make-application-frame 'render:viewer))
         (entries
           (mcluv:command-menu-entries-for-tables
            (mcluv:command-menu-tables-for viewer)
            :owner-frame viewer)))
    (true (equal '(render::com-execute-command)
                 (render::viewer-key-command
                  viewer (key-press :x :character #\x :modifiers '(:meta)))))
    (true (= 1 (count 'render::com-execute-command entries
                      :key #'mcluv:command-menu-entry-command-name)))
    (true (find "Execute Command" entries :test #'string=
                :key #'mcluv:command-menu-entry-label))
    (true (find "Toggle Lobby Panel" entries :test #'string=
                :key #'mcluv:command-menu-entry-label))
    (dolist (label '("Edit World" "Leave World Edit Mode"
                     "Undo World Edit" "Redo World Edit"))
      (true (find label entries :test #'string=
                  :key #'mcluv:command-menu-entry-label)))
    ;; The shared palette does not advertise a command it cannot yet prompt.
    (true (null (find 'render::com-start-moving entries
                      :key #'mcluv:command-menu-entry-command-name)))))
