(in-package #:luvcraft.tests)

(defmacro define-test-with-libghostty (name &body body)
  `(define-test ,name
     (if (uiop:getenv "LUV_GHOSTTY_LIBRARY")
         (progn ,@body)
         (skip "libghostty-vt unavailable" (true nil)))))

(defclass recording-command-encoder (gpu-command-encoder)
  ((commands :initform nil :accessor recording-command-encoder-commands)))

(defmethod encode ((encoder recording-command-encoder) command)
  (push command (recording-command-encoder-commands encoder))
  encoder)

(defstruct (overlay-owner-request
             (:constructor make-overlay-owner-request (function)))
  function
  (completion (sb-thread:make-semaphore :count 0) :read-only t)
  values
  condition)

(defclass overlay-owner-canvas ()
  ((state :initform :open :accessor overlay-owner-canvas-state)
   (mailbox
    :initform (sb-concurrency:make-mailbox
               :name "Luvcraft overlay owner requests")
    :reader overlay-owner-canvas-mailbox)
   (thread :reader overlay-owner-canvas-thread)
   (request-enqueued
    :initform (sb-thread:make-semaphore :count 0)
    :reader overlay-owner-canvas-request-enqueued)
   (reject-requests-p
    :initform nil :accessor overlay-owner-canvas-reject-requests-p)
   (stopped
    :initform (sb-thread:make-semaphore :count 0)
    :reader overlay-owner-canvas-stopped)))

(defun run-overlay-owner-canvas (canvas)
  (unwind-protect
       (loop for request = (sb-concurrency:receive-message
                            (overlay-owner-canvas-mailbox canvas))
             until (eq request :stop)
             do (handler-case
                    (setf (overlay-owner-request-values request)
                          (multiple-value-list
                           (funcall (overlay-owner-request-function request)
                                    0.0d0)))
                  (error (condition)
                    (setf (overlay-owner-request-condition request)
                          condition)))
                (sb-thread:signal-semaphore
                 (overlay-owner-request-completion request)))
    (sb-thread:signal-semaphore (overlay-owner-canvas-stopped canvas))))

(defmethod initialize-instance :after ((canvas overlay-owner-canvas) &key)
  (setf (slot-value canvas 'thread)
        (sb-thread:make-thread
         (lambda () (run-overlay-owner-canvas canvas))
         :name "Luvcraft overlay native owner")))

(defmethod canvas-state ((canvas overlay-owner-canvas))
  (overlay-owner-canvas-state canvas))

(defmethod canvas-thread-p ((canvas overlay-owner-canvas))
  (and (slot-boundp canvas 'thread)
       (eq sb-thread:*current-thread* (overlay-owner-canvas-thread canvas))))

(defmethod request-canvas-frame ((canvas overlay-owner-canvas) function)
  (when (canvas-thread-p canvas)
    (return-from request-canvas-frame (funcall function 0.0d0)))
  (when (overlay-owner-canvas-reject-requests-p canvas)
    (error "The overlay owner rejected the request before its callback."))
  (unless (eq :open (overlay-owner-canvas-state canvas))
    (error "The overlay owner canvas is not open."))
  (let ((request (make-overlay-owner-request function)))
    (sb-concurrency:send-message
     (overlay-owner-canvas-mailbox canvas) request)
    (sb-thread:signal-semaphore
     (overlay-owner-canvas-request-enqueued canvas))
    (unless (sb-thread:wait-on-semaphore
             (overlay-owner-request-completion request) :timeout 2.0)
      (error "The overlay owner did not service a frame request."))
    (when (overlay-owner-request-condition request)
      (error (overlay-owner-request-condition request)))
    (values-list (overlay-owner-request-values request))))

(defun close-overlay-owner-canvas (canvas)
  (unless (eq :closed (overlay-owner-canvas-state canvas))
    (setf (overlay-owner-canvas-state canvas) :closed)
    (sb-concurrency:send-message
     (overlay-owner-canvas-mailbox canvas) :stop)
    (unless (sb-thread:wait-on-semaphore
             (overlay-owner-canvas-stopped canvas) :timeout 2.0)
      (error "The overlay owner did not stop."))
    (sb-thread:join-thread (overlay-owner-canvas-thread canvas)))
  canvas)

(defun wait-for-overlay-owner-request (canvas)
  (unless (sb-thread:wait-on-semaphore
           (overlay-owner-canvas-request-enqueued canvas) :timeout 1.0)
    (error "The expected overlay owner request was not enqueued.")))

(defclass owned-overlay-probe ()
  ((canvas :initarg :canvas :reader owned-overlay-canvas)
   (release-count :initform 0 :accessor owned-overlay-release-count)
   (released-on-owner-p :initform nil
                        :accessor owned-overlay-released-on-owner-p)
   (fail-release-p :initarg :fail-release-p :initform nil
                   :reader owned-overlay-fail-release-p)))

(defmethod release-luvcraft-overlay ((overlay owned-overlay-probe))
  (setf (owned-overlay-released-on-owner-p overlay)
        (canvas-thread-p (owned-overlay-canvas overlay)))
  (incf (owned-overlay-release-count overlay))
  (when (owned-overlay-fail-release-p overlay)
    (error "Deliberate overlay release failure.")))

(defclass capture-frame-key-context (canvas-context) ())

(defmethod canvas-frame-resource-key
    ((context capture-frame-key-context) target)
  (declare (ignore context))
  (list :capture-target target))

(defclass capture-frame-key-eviction-probe ()
  ((keys :initform nil :accessor capture-frame-key-eviction-probe-keys)
   (canvas :initarg :canvas :reader capture-frame-key-eviction-probe-canvas)
   (owner-thread-p :initform nil
                   :accessor capture-frame-key-eviction-probe-owner-thread-p)))

(defmethod evict-luvcraft-overlay-frame-key
    ((probe capture-frame-key-eviction-probe) frame-key)
  (push frame-key (capture-frame-key-eviction-probe-keys probe))
  (setf (capture-frame-key-eviction-probe-owner-thread-p probe)
        (canvas-thread-p (capture-frame-key-eviction-probe-canvas probe)))
  probe)

(define-test capture-cleanup-evicts-overlay-frame-keys-on-the-canvas-thread
  (let* ((canvas (make-instance 'overlay-owner-canvas))
         (context (make-instance 'capture-frame-key-context))
         (session (make-instance 'luvcraft-session
                                 :canvas canvas :context context))
         (first (make-instance 'capture-frame-key-eviction-probe
                               :canvas canvas))
         (second (make-instance 'capture-frame-key-eviction-probe
                                :canvas canvas))
         (terminal
           (allocate-instance (find-class 'terminal-display)))
         (target (list :offscreen-texture))
         (capture (make-instance 'application-capture
                                 :application session :kind :screenshot)))
    (unwind-protect
         (progn
           ;; The second direct presentation stands behind a terminal-display
           ;; owner, like the Telegram and film-browser wall modes do.
           (setf (terminal-display-mode-overlay terminal) second
                 (luvcraft-session-overlays session) (list first terminal)
                 (capture-target capture) target)
           (cleanup-capture session capture)
           (dolist (probe (list first second))
             (true (equal (list (list :capture-target target))
                          (capture-frame-key-eviction-probe-keys probe)))
             (true (capture-frame-key-eviction-probe-owner-thread-p probe))))
      (close-overlay-owner-canvas canvas))))

(define-test stopped-luvcraft-sessions-consume-and-reject-late-overlays
  (let* ((canvas (make-instance 'overlay-owner-canvas))
         (session (make-instance 'luvcraft-session :canvas canvas))
         (owned (make-instance 'owned-overlay-probe :canvas canvas))
         (late (make-instance 'owned-overlay-probe
                              :canvas canvas :fail-release-p t))
         (condition nil))
    (unwind-protect
         (progn
           (add-luvcraft-overlay session owned)
           (add-luvcraft-overlay session owned)
           (true (= 1 (length (luvcraft-session-overlays session))))
           (call-with-stop-controller
            (luvcraft::luvcraft-session-stop-controller session)
            (lambda () nil))
           ;; A duplicate remains idempotent even if the closing native canvas
           ;; would reject a callback before the attachment gate can inspect it.
           (setf (overlay-owner-canvas-reject-requests-p canvas) t)
           (true (eq owned (add-luvcraft-overlay session owned)))
           (true (zerop (owned-overlay-release-count owned)))
           (true (= 1 (length (luvcraft-session-overlays session))))
           (setf (overlay-owner-canvas-reject-requests-p canvas) nil)
           ;; Removal remains legal after the terminal transition so teardown
           ;; and constructor rollback never lose their release path.
           (true (eq owned (remove-luvcraft-overlay session owned)))
           (handler-bind ((release-warning #'muffle-warning))
             (handler-case (add-luvcraft-overlay session late)
               (application-attachment-closed (failure)
                 (setf condition failure))))
           (true (typep condition 'application-attachment-closed))
           (true (eq late
                     (application-attachment-closed-attachment condition)))
           (true (eq :stopped
                     (application-attachment-closed-state condition)))
           (true (= 1 (owned-overlay-release-count owned)))
           ;; The rejected release failed deliberately, but ADD invoked it once
           ;; and preserved the semantic rejection as the primary condition.
           (true (= 1 (owned-overlay-release-count late)))
           (true (owned-overlay-released-on-owner-p late))
           (true (null (luvcraft-session-overlays session))))
      (close-overlay-owner-canvas canvas))))

(define-test overlay-add-versus-stop-never-repopulates-the-terminal-registry
  (dotimes (iteration 16)
    (let* ((canvas (make-instance 'overlay-owner-canvas))
           (session (make-instance 'luvcraft-session :canvas canvas))
           (overlay (make-instance 'owned-overlay-probe :canvas canvas))
           (start (sb-thread:make-semaphore :count 0))
           (finished (sb-thread:make-semaphore :count 0))
           (add-condition nil)
           (stop-condition nil)
           (add-thread nil)
           (stop-thread nil))
      (declare (ignore iteration))
      (unwind-protect
           (progn
             (setf add-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (progn
                             (sb-thread:wait-on-semaphore start)
                             (handler-case
                                 (add-luvcraft-overlay session overlay)
                               (error (failure)
                                 (setf add-condition failure))))
                        (sb-thread:signal-semaphore finished)))
                    :name "Luvcraft racing overlay add")
                   stop-thread
                   (sb-thread:make-thread
                    (lambda ()
                      (unwind-protect
                           (progn
                             (sb-thread:wait-on-semaphore start)
                             (handler-case
                                 (call-with-stop-controller
                                  (luvcraft::luvcraft-session-stop-controller
                                   session)
                                  (lambda ()
                                    (dolist
                                        (attached
                                         (copy-list
                                          (luvcraft-session-overlays session)))
                                      (remove-luvcraft-overlay
                                       session attached))))
                               (error (failure)
                                 (setf stop-condition failure))))
                        (sb-thread:signal-semaphore finished)))
                    :name "Luvcraft racing overlay stop"))
             (sb-thread:signal-semaphore start)
             (sb-thread:signal-semaphore start)
             (unless (and (sb-thread:wait-on-semaphore finished :timeout 2.0)
                          (sb-thread:wait-on-semaphore finished :timeout 2.0))
               (error "The Luvcraft attachment race did not settle."))
             (sb-thread:join-thread add-thread)
             (setf add-thread nil)
             (sb-thread:join-thread stop-thread)
             (setf stop-thread nil)
             (true (null stop-condition))
             (true (or (null add-condition)
                       (typep add-condition 'application-attachment-closed)))
             (true (eq :stopped
                       (stop-controller-state
                        (luvcraft::luvcraft-session-stop-controller session))))
             (true (null (luvcraft-session-overlays session)))
             (true (= 1 (owned-overlay-release-count overlay)))
             (true (owned-overlay-released-on-owner-p overlay)))
        (sb-thread:signal-semaphore start)
        (sb-thread:signal-semaphore start)
        (when add-thread (sb-thread:join-thread add-thread))
        (when stop-thread (sb-thread:join-thread stop-thread))
        (dolist (attached (copy-list (luvcraft-session-overlays session)))
          (ignore-errors (remove-luvcraft-overlay session attached)))
        (close-overlay-owner-canvas canvas)))))

(define-test tracy-controller-release-detaches-before-the-next-session
  (let ((first
          (luv.tracy.capture:make-tracy-capture-controller
           :application-name "Luvcraft test"
           :directory (uiop:temporary-directory)
           :open-on-completion-p nil)))
    (unwind-protect
         (progn
           (sb-thread:with-mutex
               (luvcraft::*luvcraft-tracy-capture-controller-lock*)
             (setf luvcraft::*luvcraft-tracy-capture-controller* first))
           (true (luvcraft:release-luvcraft-tracy-capture-controller))
           (true (null luvcraft::*luvcraft-tracy-capture-controller*))
           (true (luv.tracy.capture:tracy-capture-controller-released-p first))
           (let ((second
                   (luvcraft::ensure-luvcraft-tracy-capture-controller)))
             (true (not (eq first second)))
             (true (not
                    (luv.tracy.capture:tracy-capture-controller-released-p
                     second)))))
      (luvcraft:release-luvcraft-tracy-capture-controller))))

(defclass recording-chunk-window ()
  ((locations :initform nil :accessor recording-window-locations)))

(defmethod locate-chunk-window-site
    ((window recording-chunk-window) x y z)
  (push (list x y z) (recording-window-locations window))
  (values window 37 :available))

(defclass recording-modal-focus ()
  ((score :initarg :score :initform nil :reader recording-focus-score)
   (transitions :initform nil :accessor recording-focus-transitions)
   (events :initform nil :accessor recording-focus-events)))

(defmethod luvcraft-focus-score
    ((focus recording-modal-focus) (session luvcraft-session))
  (recording-focus-score focus))

(defmethod luvcraft-focus-entered
    ((focus recording-modal-focus) (session luvcraft-session))
  (push :entered (recording-focus-transitions focus)))

(defmethod luvcraft-focus-left
    ((focus recording-modal-focus) (session luvcraft-session))
  (push :left (recording-focus-transitions focus)))

(defmethod handle-luvcraft-focus-event
    ((focus recording-modal-focus) (session luvcraft-session) canvas event)
  (declare (ignore session canvas))
  (push event (recording-focus-events focus)))

(define-test overlays-default-to-the-depth-bearing-scene-stage
  (true (eq :scene
            (luvcraft-overlay-stage
             (make-instance 'recording-modal-focus)))))

(define-test player-body-renders-after-the-world-in-the-viewmodel-stage
  (true (eq :viewmodel
            (luvcraft-overlay-stage (make-instance 'player-body)))))

(define-test modal-focus-suspends-player-input-and-owns-events
  (let ((session (make-instance 'luvcraft-session))
        (first (make-instance 'recording-modal-focus))
        (second (make-instance 'recording-modal-focus))
        (event (make-instance 'canvas-key-press-event
                              :timestamp 0 :key-name :w)))
    (setf (movement-urging-p (luvcraft-session-movement-intent session)
                             :forward)
          t
          (movement-intent-jump-requested-p
           (luvcraft-session-movement-intent session))
          t)
    (true (eq first (focus-luvcraft-session session first)))
    (true (eq first (luvcraft-session-modal-focus session)))
    (true (movement-intent-still-p (luvcraft-session-movement-intent session)))
    ;; Interpreting keys belongs to the layer above; what the core promises is
    ;; that a focused object is offered the event and that nothing else sees it.
    (true (dispatch-luvcraft-focus-event session nil event))
    (true (equal (list event) (recording-focus-events first)))
    (true (movement-intent-still-p (luvcraft-session-movement-intent session)))
    (focus-luvcraft-session session second)
    (true (equal '(:left :entered) (recording-focus-transitions first)))
    (true (equal '(:entered) (recording-focus-transitions second)))
    (add-luvcraft-overlay session second)
    (remove-luvcraft-overlay session second :release-p nil)
    (true (null (luvcraft-session-modal-focus session)))
    (true (equal '(:left :entered) (recording-focus-transitions second)))
    (true (not (dispatch-luvcraft-focus-event session nil event)))))

(define-test off-thread-overlay-removal-waits-for-the-borrowing-frame
  (let* ((canvas (make-instance 'overlay-owner-canvas))
         (session (make-instance 'luvcraft-session :canvas canvas))
         (overlay (make-instance 'owned-overlay-probe :canvas canvas))
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
           (add-luvcraft-overlay session overlay)
           (wait-for-overlay-owner-request canvas)
           (setf frame-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (request-canvas-frame
                         canvas
                         (lambda (timestamp)
                           (declare (ignore timestamp))
                           (let ((snapshot
                                   (copy-list
                                    (luvcraft-session-overlays session))))
                             (sb-thread:signal-semaphore borrowed)
                             (unless (sb-thread:wait-on-semaphore
                                      continue :timeout 1.0)
                               (error "The borrowing frame was not released."))
                             (setf snapshot-kept-p
                                   (eq overlay (first snapshot))))))
                      (error (condition)
                        (setf frame-condition condition)))
                    (sb-thread:signal-semaphore frame-finished))
                  :name "Luvcraft borrowing frame"))
           (wait-for-overlay-owner-request canvas)
           (unless (sb-thread:wait-on-semaphore borrowed :timeout 1.0)
             (error "The overlay frame did not borrow its snapshot."))
           (setf remove-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (setf remove-result
                              (remove-luvcraft-overlay session overlay))
                      (error (condition)
                        (setf remove-condition condition)))
                    (sb-thread:signal-semaphore remove-finished))
                  :name "Luvcraft off-thread overlay removal"))
           (wait-for-overlay-owner-request canvas)
           (true (not (sb-thread:wait-on-semaphore
                       remove-finished :timeout 0.02))
                 "remove remains synchronous while the frame owns its snapshot")
           (true (zerop (owned-overlay-release-count overlay))
                 "the borrowed overlay has not been released")
           (sb-thread:signal-semaphore continue)
           (unless (sb-thread:wait-on-semaphore frame-finished :timeout 1.0)
             (error "The borrowing frame did not finish."))
           (unless (sb-thread:wait-on-semaphore remove-finished :timeout 1.0)
             (error "The queued overlay removal did not finish."))
           (sb-thread:join-thread frame-thread)
           (setf frame-thread nil)
           (sb-thread:join-thread remove-thread)
           (setf remove-thread nil)
           (true (null frame-condition))
           (true (null remove-condition))
           (true snapshot-kept-p)
           (true (eq overlay remove-result))
           (true (= 1 (owned-overlay-release-count overlay)))
           (true (owned-overlay-released-on-owner-p overlay))
           (true (null (luvcraft-session-overlays session))))
      (sb-thread:signal-semaphore continue)
      (when frame-thread (sb-thread:join-thread frame-thread))
      (when remove-thread (sb-thread:join-thread remove-thread))
      (close-overlay-owner-canvas canvas))))

(define-test mid-frame-overlay-fuse-detaches-without-releasing
  (let* ((canvas (make-instance 'overlay-owner-canvas))
         (session (make-instance 'luvcraft-session :canvas canvas))
         (overlay (make-instance 'owned-overlay-probe :canvas canvas)))
    (unwind-protect
         (progn
           (add-luvcraft-overlay session overlay)
           (wait-for-overlay-owner-request canvas)
           (request-canvas-frame
            canvas
            (lambda (timestamp)
              (declare (ignore timestamp))
              (remove-luvcraft-overlay session overlay :release-p nil)))
           (wait-for-overlay-owner-request canvas)
           (true (zerop (owned-overlay-release-count overlay)))
           (true (null (luvcraft-session-overlays session))))
      (close-overlay-owner-canvas canvas))))

(define-test overlay-release-errors-return-through-the-frame-boundary
  (let* ((canvas (make-instance 'overlay-owner-canvas))
         (session (make-instance 'luvcraft-session :canvas canvas))
         (overlay (make-instance 'owned-overlay-probe
                                 :canvas canvas :fail-release-p t))
         (condition nil))
    (unwind-protect
         (progn
           (add-luvcraft-overlay session overlay)
           (wait-for-overlay-owner-request canvas)
           (handler-case
               (remove-luvcraft-overlay session overlay)
             (error (failure)
               (setf condition failure)))
           (wait-for-overlay-owner-request canvas)
           (true (typep condition 'error))
           (true (= 1 (owned-overlay-release-count overlay)))
           (true (owned-overlay-released-on-owner-p overlay))
           (true (null (luvcraft-session-overlays session))))
      (close-overlay-owner-canvas canvas))))

(define-test overlay-teardown-detaches-all-and-aggregates-release-failures
  (let* ((canvas (make-instance 'overlay-owner-canvas))
         (session (make-instance 'luvcraft-session :canvas canvas))
         (first (make-instance 'owned-overlay-probe
                               :canvas canvas :fail-release-p t))
         (second (make-instance 'owned-overlay-probe :canvas canvas))
         (third (make-instance 'owned-overlay-probe
                               :canvas canvas :fail-release-p t))
         (condition nil))
    (unwind-protect
         (progn
           (dolist (overlay (list first second third))
             (add-luvcraft-overlay session overlay)
             (wait-for-overlay-owner-request canvas))
           (handler-case
               (with-release-report
                 ;; This is the application teardown policy: keep the release
                 ;; report on its owner thread while each individual release
                 ;; crosses, synchronously, to the native canvas owner.
                 (dolist (overlay
                           (copy-list (luvcraft-session-overlays session)))
                   (releasing :overlay
                     (remove-luvcraft-overlay session overlay))))
             (release-error (failure)
               (setf condition failure)))
           (loop repeat 3
                 do (wait-for-overlay-owner-request canvas))
           (true (typep condition 'release-error))
           (true (= 2 (length (release-error-failures condition))))
           (true (every (lambda (failure)
                          (eq :overlay (release-failure-name failure)))
                        (release-error-failures condition)))
           (true (every (lambda (overlay)
                          (= 1 (owned-overlay-release-count overlay)))
                        (list first second third)))
           (true (every #'owned-overlay-released-on-owner-p
                        (list first second third)))
           (true (null (luvcraft-session-overlays session))))
      (close-overlay-owner-canvas canvas))))

(defclass recording-pointer-canvas ()
  ((relative-p :initform nil :accessor recording-canvas-relative-p)))

(defmethod set-canvas-relative-pointer-mode
    ((canvas recording-pointer-canvas) enabled)
  (setf (recording-canvas-relative-p canvas) (not (null enabled))))

(define-test default-residency-radius-is-six-chunks
  (true (= 6 (luvcraft-session-residency-radius
              (make-instance 'luvcraft-session)))))

(define-test focus-borrows-mouse-look-and-hands-it-back
  (let* ((canvas (make-instance 'recording-pointer-canvas))
         (camera (make-instance 'fly-camera :yaw 0.25 :pitch -0.1))
         (session (make-instance 'luvcraft-session :canvas canvas
                                                    :camera camera))
         (first (make-instance 'recording-modal-focus))
         (second (make-instance 'recording-modal-focus)))
    (setf (recording-canvas-relative-p canvas) t
          (luvcraft::luvcraft-session-pointer-captured-p session) t)
    (focus-luvcraft-session session first)
    (true (not (recording-canvas-relative-p canvas))
          "a focused interaction is given an ordinary cursor")
    ;; Moving straight from one interaction to another still owes the capture.
    (focus-luvcraft-session session second)
    (true (not (recording-canvas-relative-p canvas)))
    (setf (camera-yaw camera) 1.0
          (camera-pitch camera) 0.4)
    (unfocus-luvcraft-session session)
    (true (recording-canvas-relative-p canvas)
          "leaving focus puts the player back into mouse look")
    (true (luvcraft::luvcraft-session-pointer-captured-p session))
    (true (null (luvcraft::luvcraft-session-focus-camera-origin session))
          "mouse look cancels the competing cinematic return")
    (true (< (abs (- (camera-yaw camera) 0.25)) 1e-6))
    (setf (camera-yaw camera) 0.7)
    (luvcraft::advance-luvcraft-focus-camera session 0.1d0)
    (true (< (abs (- (camera-yaw camera) 0.7)) 1e-6)
          "a subsequent mouse delta is not dragged back")
    ;; A player who was not in mouse look is not put into it by focusing.
    (setf (luvcraft::luvcraft-session-pointer-captured-p session) nil
          (recording-canvas-relative-p canvas) nil)
    (focus-luvcraft-session session first)
    (unfocus-luvcraft-session session)
    (true (not (recording-canvas-relative-p canvas)))
    (true (not (luvcraft::luvcraft-session-pointer-captured-p session)))))

(define-test orderly-quit-callback-owns-native-close-once
  (let* ((calls 0)
         (session
           (make-instance 'luvcraft-session
                          :quit-function
                          (lambda (stopping-session)
                            (declare (ignore stopping-session))
                            (incf calls))))
         (event (make-instance 'luv:canvas-window-close-request-event
                               :timestamp 0)))
    (true (eq :defer-canvas-close
              (handle-canvas-event session nil event)))
    (true (eq :defer-canvas-close
              (handle-canvas-event session nil event)))
    (luv:wait-for-controlled-stop
     (luvcraft::luvcraft-session-stop-controller session))
    (true (= 1 calls))
    (true (not (luvcraft-session-running-p session)))))

(defclass failing-hand-item () ())

(defmethod luvcraft::hand-item-taken-out
    ((item failing-hand-item) body session)
  (declare (ignore item body session))
  (error "fixture failed to open"))

(define-test failed-hand-item-initialization-is-not-published-in-hand
  (let ((body (make-instance 'player-body))
        (item (make-instance 'failing-hand-item)))
    (fail (luvcraft::take-out-hand-item body nil item))
    (true (null (player-body-hand-item body)))
    (true (member item (luvcraft::player-body-pocket body)))))

(define-test failed-phone-mode-is-neither-cached-nor-left-overlaid
  (let* ((phone (make-instance 'phone))
         (display (list :incomplete-phone-display))
         (removed nil)
         (symbols '(luvcraft::terminal-grid-columns-for-rows
                    luvcraft::phone-font-pathname
                    luvcraft::make-terminal-display
                    luvcraft::attach-terminal-display-shell
                    luvcraft::change-terminal-display-mode
                    luvcraft::remove-luvcraft-overlay))
         (originals (mapcar #'symbol-function symbols)))
    (unwind-protect
         (progn
           (setf (symbol-function (first symbols))
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (values 40 20))
                 (symbol-function (second symbols))
                 (lambda (weight)
                   (declare (ignore weight))
                   #P"fixture.ttf")
                 (symbol-function (third symbols))
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   display)
                 (symbol-function (fourth symbols))
                 (lambda (made-display)
                   (declare (ignore made-display))
                   t)
                 (symbol-function (fifth symbols))
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (error "fixture Telegram startup failed"))
                 (symbol-function (sixth symbols))
                 (lambda (session-display removed-display)
                   (declare (ignore session-display))
                   (setf removed removed-display)))
           (let ((luvcraft::*phone-initial-mode* :telegram))
             (fail (luvcraft::ensure-phone-display phone nil))
             (true (null (phone-display phone)))
             (true (eq display removed))))
      (loop for symbol in symbols
            for function in originals
            do (setf (symbol-function symbol) function)))))

(define-test software-cursor-follows-screen-pointer-coordinates
  (let ((center (luvcraft::make-luvcraft-cursor-vertices 200 100 100 50))
        (top-left
          (luvcraft::make-luvcraft-cursor-vertices 200 100 0 0)))
    (true (= luvcraft::+luvcraft-cursor-vertex-count+ (/ (length center) 5)))
    ;; The Vulkan viewport performs the Y inversion: moving from the centre to
    ;; the top-left subtracts one in both vertex-coordinate axes here.  The
    ;; quad's own corner offset survives the subtraction as rounding, so the
    ;; shift is read to single-float tolerance rather than exactly.
    (flet ((shift (lane)
             (- (aref top-left lane) (aref center lane))))
      (true (< (abs (+ 1.0 (shift 0))) 1e-5))
      (true (< (abs (+ 1.0 (shift 1))) 1e-5)))
    ;; The design-grid lanes describe the arrow, not where it is drawn.
    (true (equalp (subseq center 3 5) (subseq top-left 3 5)))))

(define-test cursor-corners-round-without-moving-the-outline
  ;; The shader rounds by insetting the outline and growing the distance back,
  ;; which only reads as the drawn shape if every inset corner sits exactly one
  ;; radius inside both of the edges that meet there.
  (let* ((radius 1.3)
         (outline (luvcraft.shaders:luvcraft-cursor-outline))
         (inset (luvcraft.shaders::inset-cursor-outline outline radius))
         (count (length outline)))
    (true (= count (length inset)))
    ;; The tip is the hotspot, so the design grid starts there.
    (true (equal '(0.0 0.0) (first outline)))
    (true (equal (luvcraft.shaders:luvcraft-cursor-extent)
                 (list (reduce #'max outline :key #'first)
                       (reduce #'max outline :key #'second))))
    (dotimes (index count)
      (destructuring-bind (corner-x corner-y) (nth index inset)
        (dolist (edge (list (- index 1) index))
          (let* ((from (nth (mod edge count) outline))
                 (to (nth (mod (+ edge 1) count) outline))
                 (normal (luvcraft.shaders::cursor-outline-edge-normal from to))
                 (depth (+ (* (first normal) (- corner-x (first from)))
                           (* (second normal) (- corner-y (second from))))))
            (true (< (abs (+ radius depth)) 1e-4))))))))

(define-test pointer-reports-coalesce-into-latest-frame-state
  (let ((session (make-instance 'luvcraft-session)))
    (setf (luvcraft::luvcraft-session-pointer-dirty-p session) nil)
    (dolist (point '((12.0 18.0) (40.0 55.0) (91.0 73.0)))
      (luvcraft::note-luvcraft-pointer-position
       session
       (make-instance 'luv:canvas-pointer-motion-event
                      :timestamp 0 :x (first point) :y (second point))))
    (true (= 91.0 (luvcraft::luvcraft-session-pointer-x session)))
    (true (= 73.0 (luvcraft::luvcraft-session-pointer-y session)))
    (true (luvcraft::luvcraft-session-pointer-dirty-p session))))

(define-test focusing-a-terminal-frames-the-whole-wall-above-the-hotbar
  (let* ((world (make-block-world :chunk-width 16
                                  :chunk-height 16
                                  :chunk-depth 16))
         (camera
           (make-instance 'fly-camera
                          :position (make-vec3 2.5 3.5 0.5)
                          :yaw 0.0 :pitch 0.0))
         (session
           (make-instance 'luvcraft-session :world world :camera camera)))
    (ensure-world-chunk world 0 0 0)
    (place-terminal-block-rectangle world 2 3 4 :back 3 2)
    (let* ((surface (find-terminal-surface world 2 3 4 :back))
           (display (make-instance 'terminal-display :surface surface))
           (ordinary-position
             (luvcraft::copy-camera-position (camera-position camera)))
           (ordinary (camera-field-of-view camera))
           (ordinary-focal (aref (camera-uniform-data camera 960 640) 17))
           ;; The live hotbar occupies 114 pixels at 1280 high, hence 57 in
           ;; this proportional 960 by 640 framing fixture.
           (target
             (luvcraft::terminal-focus-camera-pose
              surface 960 640 0.0 0.0 0.0 57.0)))
      (add-luvcraft-overlay session display)
      ;; Which key reaches this is the command layer's business; what the wall
      ;; promises is the framing it asks the camera for once it is focused.
      (toggle-luvcraft-session-focus session)
      (true (eq display (luvcraft-session-modal-focus session)))
      (true (luvcraft::luvcraft-session-focus-camera-active-p session))
      ;; The final pose is head-on and every surface corner lies inside the
      ;; six-percent picture margin plus the hotbar's excluded lower region.
      (luvcraft::set-camera-pose camera target)
      (multiple-value-bind (right up forward) (camera-basis camera)
        (let* ((lower-left
                 (luvcraft::terminal-surface-lower-left-point surface))
               (surface-right
                 (luvcraft::voxel-direction-vec3
                  (luvcraft::terminal-face-frame-right
                   (luvcraft::terminal-face-frame
                    (terminal-surface-face surface)))))
               (surface-up
                 (luvcraft::voxel-direction-vec3
                  (luvcraft::terminal-face-frame-up
                   (luvcraft::terminal-face-frame
                    (terminal-surface-face surface)))))
               (focal (/ (tan (/ (camera-field-of-view camera) 2.0))))
               (aspect (/ 960.0 640.0)))
          (flet ((clip (point)
                   (let* ((relative
                            (make-vec3
                             (- (vec3-x point) (camera-x camera))
                             (- (vec3-y point) (camera-y camera))
                             (- (vec3-z point) (camera-z camera))))
                          (depth (vec3-dot relative forward)))
                     (list (/ (* (vec3-dot relative right) focal)
                              (* depth aspect))
                           (- (/ (* (vec3-dot relative up) focal) depth))))))
            (loop for column in '(0.0 3.0) do
              (loop for row in '(0.0 2.0)
                    for point =
                      (luvcraft::terminal-offset-point
                       lower-left surface-right column surface-up row)
                    for projected = (clip point)
                    do (true (<= -0.92001 (first projected) 0.92001))
                       (true (<= -0.88001 (second projected) 0.701885)))))))
      (luvcraft::set-camera-pose
       camera
       (luvcraft::make-camera-pose
        ordinary-position 0.0 0.0 ordinary))
      (luvcraft::advance-camera-focus camera target 0.1d0)
      (let ((focused (camera-field-of-view camera)))
        (true (< focused ordinary))
        (true (> (aref (camera-uniform-data camera 960 640) 17)
                 ordinary-focal))
        (true (> focused
                 luvcraft::+luvcraft-camera-focused-vertical-field-of-view+))
        (unfocus-luvcraft-session session)
        (true (null (luvcraft-session-modal-focus session)))
        (dotimes (iteration 20)
          (declare (ignore iteration))
          (luvcraft::advance-luvcraft-focus-camera session 0.1d0))
        (true (< (abs (- (camera-field-of-view camera) ordinary)) 1e-5))
        (true (< (vec3-length
                  (make-vec3
                   (- (camera-x camera) (vec3-x ordinary-position))
                   (- (camera-y camera) (vec3-y ordinary-position))
                   (- (camera-z camera) (vec3-z ordinary-position))))
                 1e-5))
        (true (not (luvcraft::luvcraft-session-focus-camera-active-p
                    session)))))))

(define-test-with-libghostty focused-terminal-display-sends-keys-to-its-pty
  (luv.ghostty:with-terminal (ghostty-terminal :columns 32 :rows 4)
    (let* ((device
             (luv.terminal:open-pty-device
              ghostty-terminal
              :program "/bin/sh"
              :arguments
              (list "-c"
                    "IFS= read -r line; printf 'focused:%s\r\n' \"$line\"")))
           (display
             (make-instance 'terminal-display
                            :terminal ghostty-terminal :device device))
           (session (make-instance 'luvcraft-session)))
      (unwind-protect
           (progn
             (focus-luvcraft-session session display)
             (dolist (event
                       (list
                        (make-instance 'canvas-key-press-event
                                       :timestamp 0 :key-name :o
                                       :character #\o
                                       :unshifted-character #\o)
                        (make-instance 'canvas-key-press-event
                                       :timestamp 0 :key-name :k
                                       :character #\k
                                       :unshifted-character #\k)
                        (make-instance 'canvas-key-press-event
                                       :timestamp 0 :key-name :return
                                       :character #\Return
                                       :unshifted-character #\Return)))
               (true (dispatch-luvcraft-focus-event session nil event)))
             (true (eq :exited
                       (luv.terminal:wait-for-pty-device device :timeout 3.0)))
             (true (search
                    "focused:ok"
                    (luv.terminal:call-with-pty-device-terminal
                     device #'luv.ghostty:terminal-text))))
        (unfocus-luvcraft-session session)
        (luv.terminal:close-pty-device device)))))

(define-test terminal-film-mode-fits-the-authored-wall-and-returns-to-shell
  (let* ((world (make-block-world :chunk-width 16
                                  :chunk-height 16
                                  :chunk-depth 16))
         (session (make-instance 'luvcraft-session))
         (aspect (/ 16.0 9.0)))
    (ensure-world-chunk world 0 0 0)
    (place-terminal-block-rectangle world 2 3 4 :back 3 2)
    (let* ((surface (find-terminal-surface world 2 3 4 :back))
           (display (make-instance 'terminal-display :surface surface)))
      (change-terminal-display-mode display session :film)
      (true (eq :film (terminal-display-mode display)))
      (multiple-value-bind (origin right up)
          (luvcraft::terminal-film-rectangle surface aspect)
        (declare (ignore origin))
        (let ((width (vec3-length right))
              (height (vec3-length up)))
          (true (< (abs (- (/ width height) aspect)) 1e-5))
          (true (<= width (luvcraft::terminal-surface-physical-width surface)))
          (true (<= height
                    (luvcraft::terminal-surface-physical-height surface)))))
      (change-terminal-display-mode display session :shell)
      (true (eq :shell (terminal-display-mode display))))))

(define-test-with-libghostty terminal-display-pty-output-marks-a-frame-publication-dirty
  (luv.ghostty:with-terminal (terminal :columns 32 :rows 4)
    (let ((display (make-instance 'terminal-display :terminal terminal)))
      (attach-terminal-display-pty
       display
       :program "/bin/sh"
       :arguments (list "-c" "printf 'fresh shell output\\r\\n'"))
      (let ((device (terminal-display-device display)))
        (unwind-protect
             (progn
               (true (eq :exited
                         (luv.terminal:wait-for-pty-device
                          device :timeout 3.0)))
               (true (luvcraft::terminal-display-dirty-p display))
               (true (search
                      "fresh shell output"
                      (luv.terminal:call-with-pty-device-terminal
                       device #'luv.ghostty:terminal-text))))
          (luv.terminal:close-pty-device device))))))

(define-test block-smash-particles-form-a-bounded-textured-burst
  (let ((system (make-instance 'block-particle-system))
        (coordinate (make-world-coordinate 3 5 -2)))
    (smash-block-particles system luvcraft::*dirt-block* coordinate)
    (true (= luvcraft::+block-particle-burst-size+
             (block-particle-count system)))
    (let ((vertices (luvcraft::block-particle-vertices system)))
      (true (= (length vertices)
               (* (block-particle-count system)
                  luvcraft::+block-particle-vertices-per-particle+
                  luvcraft::+block-mesh-floats-per-vertex+)))
      (true (typep vertices '(array single-float (*)))))
    (dotimes (index 20)
      (declare (ignorable index))
      (smash-block-particles system luvcraft::*stone-block* coordinate))
    (true (= luvcraft::+maximum-block-particles+
             (block-particle-count system)))))

(define-test block-smash-particles-rise-fall-and-expire
  (let* ((system (make-instance 'block-particle-system))
         (coordinate (make-world-coordinate 0 0 0)))
    (smash-block-particles system luvcraft::*grass-block* coordinate)
    (let* ((particle (aref (block-particle-system-particles system) 0))
           (initial-y (luvcraft::block-particle-y particle))
           (initial-velocity-y
             (luvcraft::block-particle-velocity-y particle)))
      (luvcraft::advance-block-particles system 0.05)
      (true (> (luvcraft::block-particle-y particle) initial-y))
      (true (< (luvcraft::block-particle-velocity-y particle)
               initial-velocity-y)))
    (luvcraft::advance-block-particles system 1.0)
    (true (zerop (block-particle-count system)))))

(defun make-critter-test-world (&key (surface luvcraft::*grass-block*))
  "A one-chunk world with a flat SURFACE top at y=1, for walking animals over."
  (let ((world (make-block-world)))
    (ensure-world-chunk world 0 0 0)
    (loop for x below 16 do
      (loop for z below 16 do
        (setf (world-block-at world x 0 z) luvcraft::*dirt-block*
              (world-block-at world x 1 z) surface)))
    (relight-block-world world)
    world))

(define-test turtles-stand-on-the-ground-they-walk-over
  (let* ((world (make-critter-test-world))
         (turtle (spawn-critter-at :turtle world 8 2 8 4242)))
    (true (typep turtle 'turtle))
    (true (eq :turtle (critter-species turtle)))
    (dotimes (step 1200)
      (declare (ignorable step))
      (advance-critter turtle world (/ 1d0 60)))
    ;; It rests on the surface it started on, has not fallen through the
    ;; world or climbed it, and has actually gone somewhere.
    (true (luvcraft::critter-grounded-p turtle))
    (true (< (abs (- 2d0 (critter-y turtle))) 1d-3))
    (true (> (+ (abs (- (critter-x turtle) 8.5d0))
                (abs (- (critter-z turtle) 8.5d0)))
             0.5d0))
    ;; And it stays inside the chunk it was spawned in: a turtle walking into
    ;; the world boundary turns away from it rather than leaving.
    (true (<= 0d0 (critter-x turtle) 16d0))
    (true (<= 0d0 (critter-z turtle) 16d0))))

(define-test turtles-wander-the-same-way-from-the-same-seed
  (let ((world (make-critter-test-world))
        (positions '()))
    (dotimes (attempt 2)
      (declare (ignorable attempt))
      (let ((turtle (spawn-critter-at :turtle world 8 2 8 777)))
        (dotimes (step 600)
          (declare (ignorable step))
          (advance-critter turtle world (/ 1d0 60)))
        (push (list (critter-x turtle) (critter-z turtle)
                    (critter-yaw turtle))
              positions)))
    (true (equal (first positions) (second positions)))))

(define-test a-turtle-says-which-ground-it-lives-on
  (let ((meadow (make-critter-test-world))
        (bare (make-critter-test-world :surface luvcraft::*stone-block*)))
    (true (spawn-critter-at :turtle meadow 4 2 4 1))
    (true (spawn-critter-at :turtle
                            (make-critter-test-world
                             :surface luvcraft::*sand-block*)
                            4 2 4 1))
    ;; Stone is not turtle country, an occupied cell is nobody's, and a
    ;; species nothing has claimed is an error rather than a silent absence.
    (true (null (spawn-critter-at :turtle bare 4 2 4 1)))
    (setf (world-block-at meadow 4 2 4) luvcraft::*stone-block*)
    (true (null (spawn-critter-at :turtle meadow 4 2 4 1)))
    (fail (spawn-critter-at :axolotl meadow 4 2 4 1) 'error)))

(define-test a-critter-population-fills-up-and-forgets-what-wanders-off
  (let ((world (make-critter-test-world))
        (population (make-instance 'critter-population :target-count 3)))
    ;; Only the resident chunk offers sites, so the neighbourhood has to be
    ;; visited a few times before it is as full as it wants to be.
    (dotimes (frame 40)
      (declare (ignorable frame))
      (maintain-critter-population population world 8d0 8d0))
    (true (= 3 (critter-count population)))
    (true (every (lambda (critter) (typep critter 'turtle))
                 (critter-population-critters population)))
    (maintain-critter-population population world 900d0 900d0)
    (true (zerop (critter-count population)))))

(define-test a-critter-model-is-a-bounded-stream-of-turned-boxes
  (let* ((world (make-critter-test-world))
         (population (make-instance 'critter-population))
         (turtle (spawn-critter-at :turtle world 8 2 8 5)))
    (setf (critter-yaw turtle) 0.9d0)
    (add-critter population turtle)
    (let ((vertices (critter-vertices population world)))
      (true (typep vertices '(array single-float (*))))
      (true (= (length vertices)
               (* (critter-model-box-count turtle)
                  luvcraft::+critter-vertices-per-box+
                  luvcraft::+block-mesh-floats-per-vertex+)))
      ;; Every vertex of every turned box stays within the animal: the yaw
      ;; rotation moves the model around its own position rather than away
      ;; from it, whatever heading it walks on.
      (true (loop for offset from 0 below (length vertices)
                  by luvcraft::+block-mesh-floats-per-vertex+
                  always
                  (let ((dx (- (aref vertices offset) (critter-x turtle)))
                        (y (aref vertices (+ offset 1)))
                        (dz (- (aref vertices (+ offset 2))
                               (critter-z turtle))))
                    (and (< (sqrt (+ (* dx dx) (* dz dz))) 0.65)
                         (<= -0.001 (- y (critter-y turtle))
                             (critter-height turtle)))))))))

(defun make-critter-riding-session (&key (world (make-critter-test-world)))
  "A headless session with one turtle in front of a player looking at it."
  (let* ((population (make-instance 'critter-population))
         (turtle (spawn-critter-at :turtle world 8 2 8 3))
         (camera (make-instance 'fly-camera
                                :position (make-vec3 8.5d0 2.3d0 5.5d0)
                                :yaw 0d0 :pitch 0d0))
         (player (make-instance 'block-world-player
                                :position (make-vec3 8.5d0 2d0 5.5d0)))
         (session (make-instance 'luvcraft-session
                                 :world world :camera camera :player player
                                 :critters population)))
    (add-critter population turtle)
    (values session turtle player world)))

(define-test looking-at-an-animal-is-looking-past-the-terrain
  (multiple-value-bind (session turtle player world)
      (make-critter-riding-session)
    (declare (ignore player))
    (true (eq turtle (luvcraft-session-targeted-critter session)))
    ;; A wall between the two is decided by which the ray reaches first, not by
    ;; whether the animal is within reach.
    (setf (world-block-at world 8 2 7) luvcraft::*stone-block*)
    (true (null (luvcraft-session-targeted-critter session)))
    (setf (world-block-at world 8 2 7) nil)
    (true (eq turtle (luvcraft-session-targeted-critter session)))
    ;; And an animal beyond the player's reach is out of it.
    (setf (vec3-z (camera-position (luvcraft-session-camera session)))
          -8d0)
    (true (null (luvcraft-session-targeted-critter session)))))

(define-test mounting-a-turtle-carries-the-player-and-reins-the-animal
  (multiple-value-bind (session turtle player world)
      (make-critter-riding-session)
    (let ((ride (toggle-luvcraft-session-focus session)))
      (true (typep ride 'critter-ride))
      (true (eq turtle (critter-ride-critter ride)))
      (true (eq ride (luvcraft-session-modal-focus session)))
      ;; A mount carries the player, so the ordinary controller stands down.
      (true (luvcraft-focus-carries-player-p ride))
      ;; An unridden turtle rests first; a reined one walks.  The rider steers
      ;; with the session's own movement intent, the same one the player's legs
      ;; would have read.
      (setf (movement-urging-p (luvcraft-session-movement-intent session)
                               :forward)
            t)
      (let ((start-z (critter-z turtle)))
        (dotimes (frame 300)
          (declare (ignorable frame))
          (advance-critters (luvcraft-session-critters session) world
                            (/ 1d0 60))
          (advance-luvcraft-focus ride session (/ 1d0 60)))
        (true (not (turtle-resting-p turtle)))
        (true (> (abs (- (critter-z turtle) start-z)) 0.2d0)))
      ;; Wherever it went, the player went too.
      (true (< (abs (- (player-x player) (critter-x turtle))) 1d-6))
      (true (< (abs (- (player-z player) (critter-z turtle))) 1d-6))
      ;; And the camera it asks for is a seat on the animal: over its own
      ;; footprint, above its back, facing the way it faces give or take the
      ;; sway of its gait.
      (let* ((pose (luvcraft-focus-camera-pose ride session))
             (position (luvcraft::camera-pose-position pose))
             (dx (- (vec3-x position) (critter-x turtle)))
             (dz (- (vec3-z position) (critter-z turtle))))
        (true (typep pose 'luvcraft::camera-pose))
        (true (< (sqrt (+ (* dx dx) (* dz dz)))
                 (* 2 (critter-half-width turtle))))
        (true (> (vec3-y position)
                 (+ (critter-y turtle) (critter-height turtle))))
        (true (< (abs (luvcraft::shortest-angle-difference
                       (luvcraft::camera-pose-yaw pose) (critter-yaw turtle)))
                 0.1d0)))
      ;; Shift-TAB is the same toggle: it puts the rider down somewhere their
      ;; own box fits and gives the animal its mind back.
      (true (eq ride (toggle-luvcraft-session-focus session)))
      (true (null (luvcraft-session-modal-focus session)))
      (true (body-position-clear-p player world (player-x player)
                                   (player-y player) (player-z player)))
      (true (turtle-resting-p turtle)))))

(define-test a-ride-ends-when-its-animal-does
  (multiple-value-bind (session turtle player world)
      (make-critter-riding-session)
    (declare (ignore player world))
    (let ((ride (toggle-luvcraft-session-focus session))
          (population (luvcraft-session-critters session)))
      (true (typep ride 'critter-ride))
      (setf (fill-pointer (critter-population-critters population)) 0)
      (advance-luvcraft-focus ride session (/ 1d0 60))
      (true (null (luvcraft-session-modal-focus session)))
      (true (eq turtle (critter-ride-critter ride))))))

(define-test an-animal-nobody-can-ride-is-only-looked-at
  (let ((critter (make-instance 'critter)))
    (true (null (activate-luvcraft-critter critter nil)))
    (true (null (luvcraft-focus-carries-player-p critter)))
    ;; And it ignores a rider's wishes rather than failing to understand them.
    (true (null (urge-critter critter 1d0 1d0 0.1d0)))))

(define-test a-turtle-and-the-player-are-both-bodies
  (let ((turtle (make-instance 'turtle))
        (player (make-instance 'block-world-player)))
    (dolist (body (list turtle player))
      (true (typep (body-position body) 'luvcraft::vec3))
      (true (typep (body-velocity body) 'luvcraft::vec3))
      (true (plusp (body-half-width body)))
      (true (plusp (body-height body)))
      (true (null (body-grounded-p body)))
      (setf (body-grounded-p body) t)
      (true (body-grounded-p body)))))

(define-test vec3-is-imported-from-its-arithmetic-representation-package
  (dolist (package-name '("LUVCRAFT.WORLD" "LUVCRAFT"))
    (multiple-value-bind (symbol status)
        (find-symbol "VEC3" package-name)
      (true (eq symbol 'luv.arithmetic.lisp.vec3:vec3))
      (true (eq status :internal))))
  (true (eq (symbol-package 'vec3)
            (find-package "LUV.ARITHMETIC.LISP.VEC3"))))

(define-test player-storage-publishes-quantities-without-wrapping-values
  (let* ((position (make-vec3 1d0 2d0 3d0))
         (velocity (make-vec3 4d0 5d0 6d0))
         (player (make-instance 'block-world-player
                                :position position :velocity velocity))
         (position-declaration
           (luv.arithmetic.records:record-slot-declaration
            'block-world-player 'luvcraft::position))
         (velocity-declaration
           (luv.arithmetic.records:record-slot-declaration
            'block-world-player 'luvcraft::velocity)))
    (true (eq position (player-position player)))
    (true (eq 'vec3
              (luv.arithmetic:declaration-representation-type
               position-declaration)))
    (true (eq :world-position
              (luv.arithmetic:quantity-specification-name
               (luv.arithmetic:declaration-quantity-specification
                position-declaration))))
    (true (eq :point
              (luv.arithmetic:quantity-specification-character
               (luv.arithmetic:declaration-quantity-specification
                position-declaration))))
    (true (= 1
             (luv.arithmetic:quantity-specification-tensor-order
              (luv.arithmetic:declaration-quantity-specification
               velocity-declaration))))
    (true (null
           (luv.arithmetic.records:record-slot-declaration
            'block-world-player 'luvcraft::grounded-p)))
    (let ((predicted (luvcraft::predict-player-position player 0.5d0)))
      (true (equalp (make-vec3 3d0 4.5d0 6d0) predicted))
      (true (eq position (player-position player)))
      (true (eq velocity (player-velocity player))))
    (true (compiled-function-p luvcraft::*predict-player-position-function*))
    (fail
     (luv.arithmetic.lisp:bind-lisp-arithmetic-realization
      luvcraft::*predict-player-position-realization*
      (list velocity-declaration velocity-declaration
            luvcraft::*player-frame-duration-declaration*)
      :actual-result-declaration position-declaration)
     'luv.arithmetic:declaration-compatibility-error)))

(define-test sky-frame-structure-publishes-quantities-without-changing-layout
  (let* ((sky (sky-frame-parameters
               (make-instance 'sky-clock)
               (make-default-sky-profile)))
         (direction (luvcraft::sky-frame-parameters-sun-direction sky))
         (direction-declaration
           (luv.arithmetic.records:record-slot-declaration
            'luvcraft::sky-frame-parameters 'luvcraft::sun-direction))
         (fog-declaration
           (luv.arithmetic.records:record-slot-declaration
            'luvcraft::sky-frame-parameters 'luvcraft::fog-far)))
    (true (typep sky 'luvcraft::sky-frame-parameters))
    (true (typep direction 'vec3))
    (true (eq :world-direction
              (luv.arithmetic:quantity-specification-name
               (luv.arithmetic:declaration-quantity-specification
                direction-declaration))))
    (true (eq 'single-float
              (luv.arithmetic:declaration-representation-type
               fog-declaration)))
    (true (typep (luvcraft::sky-frame-parameters-fog-far sky) 'single-float))))

(define-test production-fog-law-is-shared-by-shader-and-cpu
  (let* ((sky
           (luvcraft::%make-sky-frame-parameters
            :fog-near 20.0 :fog-far 100.0))
         (definition
           (luv.arithmetic.language:arithmetic-function-definition-for
            'luvcraft.arithmetic:fog-amount-at-view-distance))
         (vertex (luvcraft.shaders:block-world-vertex-specification))
         (calls
           (remove-if-not
            (lambda (expression)
              (and (typep expression
                          'luv.arithmetic.language:arithmetic-function-call)
                   (eq definition
                       (luv.arithmetic.language:arithmetic-function-call-definition
                        expression))))
            (luv.shader:shader-specification-expressions vertex))))
    (true definition)
    (true (= 1 (length calls)))
    (true (= 0.0 (luvcraft::sky-fog-amount-at-distance sky 10.0)))
    (true (= 0.25 (luvcraft::sky-fog-amount-at-distance sky 60.0)))
    (true (= 1.0 (luvcraft::sky-fog-amount-at-distance sky 120.0)))
    (true (compiled-function-p luvcraft::*sky-fog-amount-function*))
    (fail
     (luv.arithmetic.lisp:bind-lisp-arithmetic-realization
      luvcraft::*sky-fog-amount-realization*
      (list
       luvcraft::*sky-fog-view-distance-declaration*
       luvcraft::*sky-fog-amount-declaration*
       (luv.arithmetic.records:record-slot-declaration
        'luvcraft::sky-frame-parameters 'luvcraft::fog-far))
      :actual-result-declaration luvcraft::*sky-fog-amount-declaration*)
     'luv.arithmetic:declaration-compatibility-error)))

(define-test semantic-owner-audit-exposes-camera-sky-material-and-timing-fields
  (dolist (claim
           '((fly-camera luvcraft::yaw :camera-yaw)
             (fly-camera luvcraft::sensitivity :look-sensitivity)
             (sky-clock luvcraft::rate :sky-cycle-rate)
             (sky-clock luvcraft::pinned-day-fraction :day-fraction)
             (luvcraft::sky-keyframe luvcraft::sun-color :linear-rgb)
             (luvcraft::sky-keyframe luvcraft::fog-far :view-distance)
             (block-kind luvcraft::light-opacity :block-light-attenuation-step)
             (block-kind luvcraft::surface-emission :material-emission)
             (luvcraft::luvcraft-frame-sample luvcraft::simulation-seconds
              :simulation-duration)
             (luvcraft::luvcraft-frame-benchmark luvcraft::drain-seconds
              :benchmark-drain-duration)
             (luvcraft::production-result luv.production::elapsed-seconds
              :production-duration)
             (luvcraft::luvcraft-lighting-state luvcraft::last-latency-seconds
              :lighting-reconciliation-duration)
             (luvcraft-session luvcraft::last-frame-time
              :monotonic-frame-time)
             (luvcraft-session luvcraft::physics-accumulator
              :physics-accumulated-duration)))
    (destructuring-bind (record slot quantity) claim
      (let ((declaration
              (luv.arithmetic.records:record-slot-declaration record slot)))
        (true declaration)
        (true (eq quantity
                  (luv.arithmetic:quantity-specification-name
                   (luv.arithmetic:declaration-quantity-specification
                    declaration))))))))

(define-test semantic-owner-audit-exposes-quantity-bearing-constants
  (dolist (claim
           '((luvcraft::+player-physics-step+ :frame-duration double-float)
             (luvcraft::+player-collision-epsilon+ :world-distance double-float)
             (luvcraft::+player-step-height+ :world-distance double-float)
             (luvcraft::+player-terminal-fall-speed+ :world-velocity double-float)
             (luvcraft::+luvcraft-camera-near-distance+ :view-distance single-float)
             (luvcraft::+luvcraft-camera-far-distance+ :view-distance single-float)
             (luvcraft::+luvcraft-camera-vertical-field-of-view+
              :camera-field-of-view single-float)
             (luvcraft::+luvcraft-target-reach+ :ray-distance double-float)
             (luvcraft::+luvcraft-maximum-frame-duration+
              :frame-duration double-float)
             (luvcraft::+luvcraft-shadow-half-extent+ :world-distance single-float)
             (luvcraft::+luvcraft-shadow-depth-radius+ :world-distance single-float)
             (luvcraft::shadow-base-bias :shadow-depth single-float)
             (luvcraft::shadow-slope-bias :shadow-depth single-float)
             (luvcraft::shadow-minimum-filter-radius
              :shadow-filter-radius single-float)
             (luvcraft::shadow-maximum-filter-radius
              :shadow-filter-radius single-float)))
    (destructuring-bind (name quantity representation) claim
      (let ((declaration
              (luv.arithmetic:value-declaration-for name)))
        (true declaration)
        (true (eq representation
                  (luv.arithmetic:declaration-representation-type
                   declaration)))
        (true (eq quantity
                  (luv.arithmetic:quantity-specification-name
                   (luv.arithmetic:declaration-quantity-specification
                    declaration))))))))

(define-test chunk-window-protocol-selects-representation-at-crossings
  (let* ((space (make-voxel-space
                 :chunk-shape
                 (make-chunk-shape :width 2 :height 2 :depth 2)))
         (domain (make-chunk-domain space (make-chunk-coordinate 0 0 0)))
         (window (make-instance 'recording-chunk-window)))
    ;; A local step remains pure domain arithmetic: the window is not asked.
    (multiple-value-bind (offset local crossing materialization availability)
        (continue-chunk-window-site
         window domain (make-local-coordinate 0 0 0) +voxel-positive-x+)
      (true (= offset 1))
      (true (= (local-coordinate-x local) 1))
      (true (null crossing))
      (true (null materialization))
      (true (eq availability :local))
      (true (null (recording-window-locations window))))
    ;; Crossing selects the aggregate once; a fifth window participates by
    ;; adding a method, with no type switch in the continuation operation.
    (multiple-value-bind (offset local crossing materialization availability)
        (continue-chunk-window-site
         window domain (make-local-coordinate 1 0 0) +voxel-positive-x+)
      (true (= offset 37))
      (true (= (local-coordinate-x local) 0))
      (true (eq crossing +voxel-positive-x+))
      (true (eq materialization window))
      (true (eq availability :available))
      (true (equal (recording-window-locations window) '((2 0 0)))))))

(define-test chunk-window-neighbor-iteration-retains-only-explicit-copies
  (let* ((space (make-voxel-space
                 :chunk-shape
                 (make-chunk-shape :width 3 :height 3 :depth 3)))
         (domain (make-chunk-domain space (make-chunk-coordinate 0 0 0)))
         (window (make-instance 'recording-chunk-window))
         (neighbors nil))
    (do-chunk-window-neighbors
        (offset destination crossing direction materialization availability
         window domain (make-local-coordinate 1 1 1)
         *voxel-face-directions*)
      (true (eq availability :local))
      (push (list offset (copy-local-coordinate destination)) neighbors))
    (true (= (length neighbors) 6))
    (dolist (expected (list (make-local-coordinate 0 1 1)
                            (make-local-coordinate 2 1 1)
                            (make-local-coordinate 1 0 1)
                            (make-local-coordinate 1 2 1)
                            (make-local-coordinate 1 1 0)
                            (make-local-coordinate 1 1 2)))
      (true (find expected neighbors :key #'second :test #'equalp)))
    (true (null (recording-window-locations window)))
    (setf neighbors nil)
    (do-chunk-window-neighbors
        (offset destination crossing direction materialization availability
         window domain (make-local-coordinate 0 1 1)
         *voxel-face-directions*)
      (when crossing (push crossing neighbors)))
    (true (equal neighbors (list +voxel-negative-x+)))
    (true (equal (recording-window-locations window) '((-1 1 1))))))

(define-test current-meshing-windows-share-location-availability
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0))
         (neighborhood (luvcraft::make-block-mesh-neighborhood world chunk))
         (snapshot
           (make-block-mesh-snapshot
            world chunk (chunk-mesh-dependency-stamp world chunk))))
    (dolist (window (list world neighborhood snapshot))
      (multiple-value-bind (materialization offset availability)
          (locate-chunk-window-site window 0 0 0)
        (true materialization)
        ;; Offsets belong to each representation: the live/neighborhood
        ;; chunks use local dense order, while the snapshot includes a halo.
        (true (typep offset '(integer 0)))
        (true (eq availability :available)))
      (multiple-value-bind (materialization offset availability)
          (locate-chunk-window-site window 20 0 0)
        (true (null materialization))
        (true (null offset))
        (true (eq availability :unavailable))))))

(define-test voxel-light-fields-retain-distinct-quantity-definitions
  (let* ((sky (luvcraft.world.fields:field-definition-for :sky-light))
         (block (luvcraft.world.fields:field-definition-for :block-light))
         (world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0)))
    (relight-block-world world)
    (let* ((light (block-chunk-light-field chunk))
           (region (luvcraft::capture-light-region world))
           (entry (nth-value 0 (locate-chunk-window-site region 0 0 0)))
           (resident-representation
             (luvcraft.world.fields:materialized-field-representation
              light :sky-light))
           (captured-representation
             (luvcraft.world.fields:materialized-field-representation
              entry :sky-light))
           (block-properties
             (luvcraft::light-region-entry-block-properties entry))
           (snapshot
             (make-block-mesh-snapshot
              world chunk (chunk-mesh-dependency-stamp world chunk))))
      (true (equal '(unsigned-byte 8)
                   (luv.arithmetic:declaration-representation-type sky)))
      (true (equal (luv.arithmetic:declaration-representation-type sky)
                   (luv.arithmetic:declaration-representation-type block)))
      (true (eq :sky-propagation-level
                (luv.arithmetic:quantity-specification-name
                 (luv.arithmetic:declaration-quantity-specification sky))))
      (true (eq :block-propagation-level
                (luv.arithmetic:quantity-specification-name
                 (luv.arithmetic:declaration-quantity-specification block))))
      (fail (luv.arithmetic:ensure-declarations-compatible sky block)
            'luv.arithmetic:declaration-compatibility-error)
      (true (typep resident-representation 'luvcraft::voxel-light-columns))
      (true (typep captured-representation 'luvcraft::voxel-light-columns))
      (true (not (eq resident-representation captured-representation)))
      (true (eq (block-chunk-domain chunk)
                (luvcraft.world.fields:field-representation-domain
                 resident-representation)))
      (true (eq (luvcraft::light-region-entry-domain entry)
                (luvcraft.world.fields:field-representation-domain
                 captured-representation)))
      (true (= (length (luvcraft::light-region-entry-opacity-lut entry))
               (luv.domains:domain-cardinality
                (luvcraft.world.fields:field-representation-domain
                 block-properties))))
      (dolist (lane-and-quantity
                '((luvcraft::propagation-loss
                   :block-light-attenuation-step)
                  (luvcraft::emission-level
                   :block-light-emission-step)))
        (destructuring-bind (lane quantity) lane-and-quantity
          (true (eq quantity
                    (luv.arithmetic:quantity-specification-name
                     (luv.arithmetic:declaration-quantity-specification
                      (luv.arithmetic.records:columnar-row-lane-declaration
                       (luvcraft::block-light-properties-row-declaration
                        block-properties)
                       lane)))))))
      (dolist (claim `((,light :sky-light)
                       (,light :block-light)
                       (,entry :block-content)
                       (,entry :sky-light)
                       (,entry :block-light)
                       (,snapshot :block-content)
                       (,snapshot :sky-light)
                       (,snapshot :block-light)))
        (destructuring-bind (materialization name) claim
          (true (luvcraft.world.fields:materialized-field-current-p
                 materialization name)))))))

(define-test block-meshes-carry-a-repeated-product-matching-the-shader-contract
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0))
         (mesh (mesh-block-chunk (make-instance 'exposed-face-mesher)
                                 world chunk))
         (declaration (block-mesh-vertex-declaration mesh))
         (layout (luv.arithmetic:declaration-quantity-layout declaration))
         (element
           (luv.arithmetic:repeated-quantity-layout-element-layout layout))
         (shader-layout
           (luvcraft::shader-input-product-layout
            (luv.shader:shader-specification-for :block-surface :vertex))))
    (true (eq declaration
              (luv.arithmetic:value-declaration-for :block-mesh-vertices)))
    (true (typep (block-mesh-vertices mesh)
                 (luv.arithmetic:declaration-representation-type declaration)))
    (true (= luvcraft::+block-mesh-floats-per-vertex+
             (luv.arithmetic:repeated-quantity-layout-stride layout)))
    (true (luv.arithmetic:quantity-layout= element shader-layout))
    (true (= (length (block-mesh-vertices mesh))
             (* luvcraft::+block-mesh-floats-per-vertex+
                (block-mesh-vertex-count mesh))))
    (fail
     (make-instance 'block-mesh
                    :vertices (make-array 11 :element-type 'single-float)
                    :vertex-count 1 :face-count 0)
     'error)))

(define-test screen-geometry-products-match-their-shader-contracts
  (dolist (claim
           `((:sky-vertices
              ,(luvcraft::make-block-world-sky-vertices)
              3
              ,(luvcraft.shaders:block-world-sky-vertex-specification))
             (:crosshair-vertices
              ,(luvcraft::make-block-world-crosshair-vertices 960 640)
              ,luvcraft::+block-world-crosshair-vertex-count+
              ,(luvcraft.shaders:block-world-crosshair-vertex-specification))
             (:cursor-vertices
              ,(luvcraft::make-luvcraft-cursor-vertices 960 640 120 90)
              ,luvcraft::+luvcraft-cursor-vertex-count+
              ,(luvcraft.shaders::shader-specification-for :cursor :vertex))))
    (destructuring-bind (name vertices count specification) claim
      (let* ((declaration (luv.arithmetic:value-declaration-for name))
             (layout
               (luv.arithmetic:declaration-quantity-layout declaration)))
        (true (typep vertices
                     (luv.arithmetic:declaration-representation-type
                      declaration)))
        (true (= (length vertices)
                 (* count
                    (luv.arithmetic:repeated-quantity-layout-stride layout))))
        (true
           (luv.arithmetic:quantity-layout=
            (luv.arithmetic:repeated-quantity-layout-element-layout layout)
            (luvcraft::shader-input-product-layout specification)))))))

(define-test world-text-model-carries-atlas-band-and-ink-metadata
  (let* ((center (luv.arithmetic.lisp.vec3:make-vec3 0.0 0.0 10.0))
         (serialized
           (luv.slug::make-slug-serialized-outline
            :horizontal-band-count 7 :vertical-band-count 5))
         (resource
           (luv.slug::make-slug-device-glyph
            :serialized serialized))
         (glyph
           (luv.slug:make-slug-glyph-placement
            :resource resource
            :origin-x 0.0 :origin-y 0.0
            :outline-min-x 0.0 :outline-min-y 0.0
            :outline-max-x 1.0 :outline-max-y 1.0))
         (locations (make-hash-table :test #'eq))
         (atlas (luv.slug:make-slug-glyph-atlas :locations locations))
         (instances nil))
    (setf (gethash resource locations) '(17 29)
          instances
          (luvcraft::make-world-text-instances
           (list glyph) atlas center
           (luv.arithmetic.lisp.vec3:make-vec3 1.0 0.0 0.0)
           (luv.arithmetic.lisp.vec3:make-vec3 0.0 1.0 0.0)
           0.5 0.0 0.0 1.0 1.0))
    (true (= 24 (length instances)))
    (true (= 10.0 (aref instances 2)))
    ;; The quad is the exact outline: the vertex stage dilates it by pixels.
    (true (< (abs (- 0.5 (aref instances 3))) 1e-6))
    (true (< (abs (- 0.5 (aref instances 7))) 1e-6))
    (true (= 7.0 (aref instances 11)))
    (true (= 5.0 (aref instances 14)))
    (true (= 17.0 (aref instances 15)))
    (true (= 29.0 (aref instances 16)))
    ;; Geometry carries only the static em padding (none by default); band
    ;; selection receives the exact bounds used by PACK-SLUG-OUTLINE.  Their
    ;; spare Z lanes carry the default ink components.
    (true (= 0.0 (aref instances 9)))
    (true (= 1.0 (aref instances 12)))
    (let ((luv.slug:*slug-static-padding* 0.035))
      (let ((padded (luvcraft::make-world-text-instances
                     (list glyph) atlas center
                     (luv.arithmetic.lisp.vec3:make-vec3 1.0 0.0 0.0)
                     (luv.arithmetic.lisp.vec3:make-vec3 0.0 1.0 0.0)
                     0.5 0.0 0.0 1.0 1.0)))
        (true (= -0.035 (aref padded 9)))
        (true (= 1.035 (aref padded 12)))))
    (true (equalp #(0.0 0.0 0.32) (subseq instances 18 21)))
    (true (equalp #(1.0 1.0 0.48) (subseq instances 21 24)))
    (true (= 0.96 (aref instances 17)))))

(define-test terminal-grid-domain-is-an-exact-row-major-viewport
  (let ((domain (make-instance 'luvcraft::terminal-grid-domain
                               :columns 80 :rows 24)))
    (true (= 1920 (luv.domains:domain-cardinality domain)))
    (true (= 0 (luvcraft::terminal-grid-offset domain 0 0)))
    (true (= 79 (luvcraft::terminal-grid-offset domain 79 0)))
    (true (= 80 (luvcraft::terminal-grid-offset domain 0 1)))
    (true (= 1919 (luvcraft::terminal-grid-offset domain 79 23)))
    (multiple-value-bind (column row)
        (luvcraft::terminal-grid-coordinate domain 997)
      (true (= 37 column))
      (true (= 12 row)))
    (fail (luvcraft::terminal-grid-offset domain 80 0) 'error)
    (fail (luvcraft::terminal-grid-coordinate domain 1920) 'error)))

(define-test empty-terminal-presentations-have-no-drawable-glyphs
  (let* ((domain (make-instance 'luvcraft::terminal-grid-domain
                                :columns 80 :rows 24))
         (presentation
           (luvcraft::make-terminal-grid-presentation domain "")))
    (true (every (lambda (character) (char= character #\Space))
                 (luvcraft::terminal-grid-presentation-characters
                  presentation)))))

(define-test terminal-block-material-rectangles-become-one-display-surface
  (let ((world (make-block-world :chunk-width 16
                                 :chunk-height 16
                                 :chunk-depth 16)))
    (ensure-world-chunk world 0 0 0)
    ;; :BACK reads left-to-right in world X from a viewer on negative Z.
    (place-terminal-block-rectangle world 2 3 4 :back 3 2)
    (multiple-value-bind (surface status)
        (find-terminal-surface world 3 4 4 :back)
      (true (eq status :rectangle))
      (true (= 3 (terminal-surface-width surface)))
      (true (= 2 (terminal-surface-height surface)))
      (true (= 2 (world-coordinate-x (terminal-surface-origin surface))))
      (true (= 3 (world-coordinate-y (terminal-surface-origin surface))))
      (true (luvcraft::terminal-surface-current-p surface))
      (let ((lower-left
              (luvcraft::terminal-surface-lower-left-point surface 0.0)))
        (true (= 2.0 (vec3-x lower-left)))
        (true (= 3.0 (vec3-y lower-left)))
        (true (= 4.0 (vec3-z lower-left))))
      ;; One missing voxel leaves an L-shaped component.  The retained screen
      ;; becomes invalid and rediscovery refuses to pretend it is rectangular.
      (edit-block-at nil world 3 3 4)
      (true (not (luvcraft::terminal-surface-current-p surface)))
      (multiple-value-bind (split split-status)
          (find-terminal-surface world 2 3 4 :back)
        (true (null split))
        (true (eq split-status :non-rectangular))))))

(define-test terminal-discovery-is-a-compiled-discover-once-frontier-program
  (let ((definition
          (luvcraft.frontier:frontier-program-definition-for
           'luvcraft::terminal-surface-discovery))
        (realization (luvcraft::terminal-discovery-realization)))
    (true (eq :discover-once
              (luvcraft.frontier:frontier-program-definition-family definition)))
    (true (luvcraft.frontier:frontier-program-definition-retain-admissions-p
           definition))
    (true (luvcraft.frontier:frontier-realization-current-p realization))
    (true (functionp
           (luvcraft.frontier:frontier-realization-drain-function realization)))
    (true (functionp
           (luvcraft.frontier:frontier-realization-admit-function realization))))
  ;; A rectangle straddling a chunk seam is discovered as one component whose
  ;; retained admitted sites are exactly its blocks, without any coordinate
  ;; objects retained per member.
  (let ((world (make-block-world :chunk-width 16
                                 :chunk-height 16
                                 :chunk-depth 16)))
    (ensure-world-chunk world 0 0 0)
    (ensure-world-chunk world 1 0 0)
    (place-terminal-block-rectangle world 13 3 4 :back 6 3)
    (multiple-value-bind (execution status)
        (luvcraft::discover-terminal-component
         world 15 4 4 :back luvcraft:*terminal-block* :air)
      (true (eq status :component))
      (true (= 18 (luvcraft.frontier:frontier-execution-visits execution)))
      (true (= 18 (luvcraft.frontier:frontier-site-buffer-length
                   (luvcraft.frontier:frontier-execution-admitted-sites
                    execution))))
      (true (plusp (luvcraft.frontier:frontier-execution-crossings execution)))
      ;; Every popped site exposes its four coplanar relations.
      (true (= (* 4 18)
               (luvcraft.frontier:frontier-execution-relations execution))))
    (multiple-value-bind (surface status)
        (find-terminal-surface world 15 4 4 :back)
      (true (eq status :rectangle))
      (true (= 6 (terminal-surface-width surface)))
      (true (= 3 (terminal-surface-height surface)))
      (true (= 13 (world-coordinate-x (terminal-surface-origin surface)))))
    ;; A covered seed and a wrong material report their own statuses.
    (setf (world-block-at world 15 4 3) luvcraft::*stone-block*)
    (true (eq :covered (nth-value 1 (find-terminal-surface world 15 4 4 :back))))
    (true (eq :not-terminal
              (nth-value 1 (find-terminal-surface world 15 4 3 :back))))))

(define-test terminal-grid-fits-the-unified-surface-not-individual-blocks
  (let* ((world (make-block-world :chunk-width 16
                                  :chunk-height 16
                                  :chunk-depth 16))
         (domain (make-instance 'luvcraft::terminal-grid-domain
                                :columns 80 :rows 24)))
    (ensure-world-chunk world 0 0 0)
    (place-terminal-block-rectangle world 4 6 5 :back 8 5)
    (let ((surface (find-terminal-surface world 8 8 5 :back)))
      (multiple-value-bind (scale left bottom width height)
          (luvcraft::fit-terminal-grid-in-surface
           domain surface 0.6 1.0 0.12 1.0)
        (true (< (abs (- scale (/ 7.76 48.0))) 1e-6))
        (true (< (abs (- width 7.76)) 1e-6))
        (true (< height 5.0))
        (true (< (abs (- left 0.12)) 1e-6))
        (true (> bottom 0.12))
        ;; Eight blocks can carry eighty columns because font fit is a surface
        ;; projection; no terminal-cell count is attached to one voxel.
        (true (= 10 (/ (luvcraft::terminal-grid-domain-columns domain)
                       (terminal-surface-width surface))))))))

(define-test-with-libghostty terminal-display-fixture-really-crosses-ghostty
  (ghostty:with-terminal (terminal :columns 80 :rows 24)
    (ghostty:write-terminal terminal (luvcraft::terminal-display-fixture))
    (let* ((domain (make-instance 'luvcraft::terminal-grid-domain
                                  :columns 80 :rows 24))
           (presentation
             (luvcraft::make-terminal-grid-presentation
              domain (ghostty:terminal-text terminal))))
      (true (char= #\l
                   (luvcraft::terminal-grid-character presentation 2 1)))
      (true (char= #\$
                   (luvcraft::terminal-grid-character presentation 2 5)))
      (true (char= #\┘
                   (luvcraft::terminal-grid-character presentation 79 23)))
      (dotimes (row 24)
        (true (not (char= #\Space
                          (luvcraft::terminal-grid-character
                           presentation 0 row))))
        (true (not (char= #\Space
                          (luvcraft::terminal-grid-character
                           presentation 79 row))))))))

(define-test packed-light-worklists-preserve-order-and-release-entries
  (let* ((world (make-block-world))
         (chunk (luvcraft::ensure-world-chunk world 0 0 0))
         (region (luvcraft::capture-light-region world))
         (entry (gethash (chunk-domain-coordinate (block-chunk-domain chunk))
                         (luvcraft::light-region-entries region)))
         (lifo (luvcraft::make-light-worklist :scheduling :lifo))
         (level (luvcraft::make-light-worklist :scheduling :level)))
    (luvcraft::light-worklist-push lifo entry 1 3)
    (luvcraft::light-worklist-push lifo entry 2 12)
    (let* ((bucket (aref (luvcraft::light-worklist-buckets lifo) 0))
           (entries (luvcraft::light-worklist-bucket-entry-lane bucket)))
      (multiple-value-bind (popped popped-offset popped-level present-p)
          (luvcraft::light-worklist-pop lifo)
        (true present-p)
        (true (eq entry popped))
        (true (= 2 popped-offset))
        (true (= 12 popped-level)))
      (multiple-value-bind (popped popped-offset popped-level present-p)
          (luvcraft::light-worklist-pop lifo)
        (true present-p)
        (true (eq entry popped))
        (true (= 1 popped-offset))
        (true (= 3 popped-level)))
      (true (luvcraft::light-worklist-empty-p lifo))
      (true (loop for index below (array-total-size entries)
                  always (null (row-major-aref entries index))))
      (luvcraft::light-worklist-push lifo entry 4 5)
      (true (eq entries
                (luvcraft::light-worklist-bucket-entry-lane
                 (aref (luvcraft::light-worklist-buckets lifo) 0)))))
    (dolist (item '((1 3) (2 12) (3 12) (4 5)))
      (luvcraft::light-worklist-push level entry (first item) (second item)))
    (dolist (expected '((3 12) (2 12) (4 5) (1 3)))
      (multiple-value-bind (popped popped-offset popped-level present-p)
          (luvcraft::light-worklist-pop level)
        (true present-p)
        (true (eq entry popped))
        (true (= (first expected) popped-offset))
        (true (= (second expected) popped-level))))
    (true (luvcraft::light-worklist-empty-p level))
    (loop for bucket across (luvcraft::light-worklist-buckets level)
          for entries = (luvcraft::light-worklist-bucket-entry-lane bucket)
          do (true (loop for index below (array-total-size entries)
                         always (null (row-major-aref entries index)))))
    (multiple-value-bind (popped offset popped-level present-p)
        (luvcraft::light-worklist-pop level)
      (true (null popped))
      (true (null offset))
      (true (null popped-level))
      (true (null present-p)))))

(zdefun zoned-test-function (value)
  "A small definition used to prove inferred zones preserve function shape."
  (declare (type fixnum value))
  (values (1+ value) (1- value)))

(defclass zoned-test-subject () ())

(zdefmethod zoned-test-method ((subject zoned-test-subject) value)
  (declare (ignore subject))
  (* value 2))

(zdefun (explicit-zoned-test-function
         :zone :test/explicit-definition
         :value value)
    (value)
  value)

(defun tree-occurrences (needle tree)
  (cond ((eq needle tree) 1)
        ((consp tree)
         (+ (tree-occurrences needle (car tree))
            (tree-occurrences needle (cdr tree))))
        (t 0)))

(define-test instrumentation-macros-keep-their-body-singular
  (dolist (form
           '((with-tracy-zone (:test/expansion) compile-marker)
             (with-cpu-trace-zone (:test/expansion) compile-marker)
             (with-luvcraft-frame-timing
                 (nil luvcraft-frame-sample-frame-seconds :test/expansion)
               compile-marker)))
    (true (= 1 (tree-occurrences 'compile-marker (macroexpand-1 form))))))

(define-test concise-zones-preserve-definitions-and-infer-stable-names
  (let ((trace (make-cpu-trace :label "concise zones")))
    (with-cpu-trace (trace)
      (zone (:test/region :value 3)
        (true (= 3 (explicit-zoned-test-function 3))))
      (multiple-value-bind (above below)
          (zoned-test-function 7)
        (true (= 8 above))
        (true (= 6 below)))
      (true (= 10 (zoned-test-method (make-instance 'zoned-test-subject) 5))))
    (true (string=
           "A small definition used to prove inferred zones preserve function shape."
           (documentation 'zoned-test-function 'function)))
    (true (equal '(:test/region
                   :test/explicit-definition
                   "luvcraft.tests/zoned-test-function"
                   "luvcraft.tests/zoned-test-method<luvcraft.tests/zoned-test-subject>")
                 (mapcar #'cpu-trace-zone-name (cpu-trace-zones trace))))))

(define-test cpu-trace-zones-are-nested-reusable-and-bounded
  (let ((trace (make-cpu-trace :label "test")))
    (with-cpu-trace (trace)
      (with-cpu-trace-zone (:outer)
        (with-cpu-trace-zone (:inner)
          (values))))
    (let* ((first-zones (cpu-trace-zones trace))
           (outer (first first-zones))
           (inner (second first-zones)))
      (true (= 2 (length first-zones)))
      (true (eq :outer (cpu-trace-zone-name outer)))
      (true (eq :inner (cpu-trace-zone-name inner)))
      (true (= -1 (cpu-trace-zone-parent-index outer)))
      (true (= 0 (cpu-trace-zone-parent-index inner)))
      (true (>= (cpu-trace-zone-seconds outer)
                (cpu-trace-zone-seconds inner)))
      (true (>= (cpu-trace-zone-bytes-consed outer)
                (cpu-trace-zone-bytes-consed inner)))
      (true (>= (cpu-trace-zone-gc-seconds outer)
                (cpu-trace-zone-gc-seconds inner)))
      (with-cpu-trace (trace)
        (with-cpu-trace-zone (:again)
          (values)))
      (let ((second-zones (cpu-trace-zones trace)))
        (true (= 1 (length second-zones)))
        (true (eq outer (first second-zones)))
        (true (eq :again (cpu-trace-zone-name (first second-zones)))))
      (let ((text (with-output-to-string (stream)
                    (print-cpu-trace trace stream))))
        (true (search "inclusive" text))
        (true (search "allocated" text))
        (true (search "garbage collection" text))
        (true (search "again" text))))))

(define-test runtime-observations-measure-allocation-and-garbage-collection
  (let ((observation (make-runtime-observation))
        (retained nil)
        (old-nursery-size (sb-ext:bytes-consed-between-gcs)))
    (unwind-protect
         (progn
           (setf (sb-ext:bytes-consed-between-gcs) (* 1024 1024))
           ;; Establish the small nursery before observing automatic GC.
           ;; Explicit SB-EXT:GC calls intentionally do not run after-GC hooks.
           (sb-ext:gc :full t)
           (with-runtime-observation (observation)
             (setf retained
                   (loop repeat 64
                         collect (make-array (* 256 1024)
                                             :element-type '(unsigned-byte 8)
                                             :initial-element 17)))))
      (setf (sb-ext:bytes-consed-between-gcs) old-nursery-size))
    (true (= 17 (aref (first retained) 0)))
    (true (>= (runtime-observation-bytes-consed observation)
              (* 16 1024 1024)))
    (true (plusp
           (runtime-observation-garbage-collections observation)))
    (true (>= (runtime-observation-gc-seconds observation) 0d0))
    (true (plusp (runtime-observation-elapsed-seconds observation)))))

(define-test tracy-source-locations-are-interned-per-zone
  ;; Tracy tells zones apart by the address of their source location, and
  ;; recompiling a file re-runs the LOAD-TIME-VALUE that asks for one.  Two
  ;; requests describing the same zone therefore have to answer with the same
  ;; pointer, or a recompile in the middle of a capture would split the zone.
  (let ((first (luv:tracy-source-location "test/zone" :file "tests.lisp"))
        (again (luv:tracy-source-location "test/zone" :file "tests.lisp"))
        (other (luv:tracy-source-location "test/other" :file "tests.lisp")))
    (true (cffi:pointer-eq first again))
    (true (not (cffi:pointer-eq first other)))
    (true (string= "canvas/frame" (luv:tracy-zone-name :canvas/frame)))
    (true (string= "already a name" (luv:tracy-zone-name "already a name")))))

(define-test streaming-trace-quiescence-requires-a-complete-publication-frontier
  (let ((quiet '(:center (0 0) :desired 81 :outstanding 0 :staged 0
                 :products 81 :lighting-dirty-p nil :errors 0)))
    (true (luvcraft::luvcraft-streaming-trace-state-quiescent-p quiet))
    (true (luvcraft::luvcraft-streaming-trace-state-quiescent-p quiet '(0 0)))
    (true (not (luvcraft::luvcraft-streaming-trace-state-quiescent-p
                quiet '(1 0))))
    (dolist (busy '((:outstanding 1) (:staged 1) (:products 80)
                    (:lighting-dirty-p t) (:errors 1)))
      (let ((state (copy-list quiet)))
        (setf (getf state (first busy)) (second busy))
        (true (not (luvcraft::luvcraft-streaming-trace-state-quiescent-p
                    state)))))))

(define-test tracy-zone-contexts-remain-natively-nested
  ;; The shim owns TracyCZoneCtx values at their native size.  Lisp observes
  ;; only the stack discipline that WITH-TRACY-ZONE relies on, so adding fields
  ;; to Tracy's opaque context cannot silently truncate zone-end events again.
  (if (not (luv:tracy-client-available-p))
      (true t "This machine has no Tracy client to check the zone ABI against.")
      (let ((ours (not luv:*tracy*)))
        (unwind-protect
             (progn
               (luv:start-tracy :application-name "luv tests")
               (let ((location (luv::tracy-source-location "test/abi")))
                 (true (zerop (luv::%tracy-zone-depth)))
                 (luv::%tracy-emit-zone-begin location 1)
                 (true (= 1 (luv::%tracy-zone-depth)))
                 (luv::%tracy-emit-zone-begin location 1)
                 (true (= 2 (luv::%tracy-zone-depth)))
                 (luv::%tracy-emit-zone-value 2)
                 (luv::%tracy-emit-zone-end)
                 (true (= 1 (luv::%tracy-zone-depth)))
                 (luv::%tracy-emit-zone-end)
                 (true (zerop (luv::%tracy-zone-depth)))))
          (when ours (luv:stop-tracy))))))

(define-test texture-preparation-is-a-backend-neutral-command
  (let ((encoder (make-instance 'recording-command-encoder)))
    (prepare-texture encoder :shadow-depth :texture-binding)
    (let ((command (first (recording-command-encoder-commands encoder))))
      (true (typep command 'gpu-prepare-texture-command))
      (true (eq :shadow-depth
                (luv::gpu-prepare-texture-command-texture command)))
      (true (eq :texture-binding
                (luv::gpu-prepare-texture-command-usage command))))))

(define-test frame-performance-summary-is-comparison-friendly
  (let ((samples (make-array 4)))
    (dotimes (index 4)
      (let ((sample (luvcraft::make-luvcraft-frame-sample)))
        (setf (luvcraft::luvcraft-frame-sample-frame-seconds sample)
              (/ (1+ index) 1000d0)
              (aref samples index) sample)))
    (let ((benchmark
            (luvcraft::make-luvcraft-frame-benchmark :samples samples)))
      (multiple-value-bind (median p95 mean maximum)
          (luvcraft::luvcraft-frame-metric-summary
           benchmark #'luvcraft::luvcraft-frame-sample-frame-seconds)
        (true (= 2.5d0 median))
        (true (= 4d0 p95))
        (true (= 2.5d0 mean))
        (true (= 4d0 maximum))))))

(define-test streaming-frame-summary-covers-only-the-transition
  (let ((samples (make-array 4)))
    (dotimes (index 4)
      (let ((sample (luvcraft::make-luvcraft-frame-sample)))
        (setf (luvcraft::luvcraft-frame-sample-frame-seconds sample)
              (/ (1+ index) 1000d0)
              (aref samples index) sample)))
    (let* ((benchmark
             (luvcraft::make-luvcraft-frame-benchmark
              :scenario :streaming :samples samples
              :width 960 :height 640
              :presentation-width 1920 :presentation-height 1280
              :completion-seconds 0.004d0
              :entering-chunk-count 9 :settled-frame 1))
           (transition
             (luvcraft::luvcraft-frame-benchmark-transition-samples benchmark))
           (text
             (with-output-to-string (stream)
               (luvcraft:print-luvcraft-frame-benchmark benchmark stream))))
      (true (= 2 (length transition)))
      (true (search "scene: 960x640; presentation: 1920x1280" text))
      (true (search "9 entering chunks, 2 frames" text))
      (true (search "settled: frame 1" text)))))

(defclass gated-production-request (luvcraft::production-request)
  ((gate :initarg :gate :reader gated-production-request-gate)
   (value :initarg :value :reader gated-production-request-value)))

(defmethod luvcraft::perform-production-request ((request gated-production-request))
  (sb-thread:wait-on-semaphore (gated-production-request-gate request))
  (gated-production-request-value request))

(defclass title-canvas ()
  ((title :initarg :title :accessor canvas-title)))

(defun production-system-active-request (system)
  (sb-thread:with-mutex ((luv.production::production-system-lock system))
    (luv.production::production-system-active-request system)))

(defun wait-until (predicate &key (timeout 2.0))
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop until (funcall predicate)
          when (>= (get-internal-real-time) deadline)
            do (return nil)
          do (sleep 0.001)
          finally (return t))))

(define-test little-world-is-deterministic-and-chunked
  (let ((first (make-little-block-world :seed 77))
        (second (make-little-block-world :seed 77)))
    (true (= (length (resident-world-chunks first)) 81))
    (true (typep (block-world-source first) 'little-world-source))
    (true (= (little-world-source-seed (block-world-source first)) 77))
    (true
       (loop for x from -16 below 32
             always
             (loop for z from -16 below 32
                   always
                   (loop for y below 16
                         always
                         (multiple-value-bind (first-block first-status)
                             (world-block-at first x y z)
                           (multiple-value-bind (second-block second-status)
                               (world-block-at second x y z)
                             (and (eq first-status :resident)
                                  (eq second-status :resident)
                                  (eq first-block second-block))))))))
    (multiple-value-bind (block status) (world-block-at first 80 0 0)
      (true (null block))
      (true (eq status :absent))))
  (let* ((source (make-instance 'little-world-source :seed 77))
         (world (make-block-world :source source)))
    (materialize-little-world-chunk source world 0 0)
    (let ((revision (block-world-revision world)))
      (materialize-little-world-chunk source world 0 0)
      (true (= (block-world-revision world) revision)))))

(define-test little-world-edits-survive-rematerialization
  (let* ((world (make-little-block-world :chunk-radius 0 :seed 31))
         (source (block-world-source world)))
    (true (world-block-at world 1 1 1))
    (edit-block-at nil world 1 1 1)
    (multiple-value-bind (block present-p)
        (block-edit-at (little-world-source-edits source) 1 1 1)
      (true present-p)
      (true (null block)))
    (true (= (block-edit-overlay-count (little-world-source-edits source)) 1))
    (rematerialize-little-world-chunk source world 0 0)
    (multiple-value-bind (block status) (world-block-at world 1 1 1)
      (true (eq status :resident))
      (true (null block)))
    ;; Explicit placement into generated air is an overlay value too.
    (edit-block-at luvcraft::*stone-block* world 2 14 2)
    (rematerialize-little-world-chunk source world 0 0)
    (true (eq (world-block-at world 2 14 2) luvcraft::*stone-block*))
    (true (= (block-edit-overlay-count (little-world-source-edits source)) 2))))

(define-test little-world-save-descriptions-round-trip-semantic-state
  (let* ((world (make-empty-little-block-world
                 :chunk-width 12 :chunk-height 20 :chunk-depth 10 :seed 913))
         (source (block-world-source world))
         (camera (make-instance 'fly-camera :yaw 1.25 :pitch -0.35))
         (player (make-instance 'block-world-player
                                :position
                                (make-vec3 -20.5d0 7.25d0 44.0d0))))
    (record-block-edit (little-world-source-edits source)
                       luvcraft::*crystal-block* -19 8 44)
    (record-block-edit (little-world-source-edits source) nil 3 4 -5)
    (let ((description
            (make-luvcraft-save-description
             world :camera camera :player player
             :selected-block luvcraft::*crystal-block*)))
      ;; Stable coordinate order makes saves readable and diffs meaningful.
      (true (equal
             (mapcar (lambda (edit) (getf edit :at))
                     (getf (rest (getf (rest (getf (rest description) :world))
                                      :source))
                           :edits))
             '((-19 8 44) (3 4 -5))))
      (multiple-value-bind (restored resume)
          (restore-luvcraft-save-description description)
        (let* ((restored-space (block-world-space restored))
               (shape (voxel-space-chunk-shape restored-space))
               (restored-source (block-world-source restored)))
          (true (= (chunk-shape-width shape) 12))
          (true (= (chunk-shape-height shape) 20))
          (true (= (chunk-shape-depth shape) 10))
          (true (= (little-world-source-seed restored-source) 913))
          (true (= (block-edit-overlay-count
                    (little-world-source-edits restored-source))
                   2))
          (true (eq (block-edit-at (little-world-source-edits restored-source)
                                   -19 8 44)
                    luvcraft::*crystal-block*))
          (multiple-value-bind (block present-p)
              (block-edit-at (little-world-source-edits restored-source)
                             3 4 -5)
            (true present-p)
            (true (null block)))
          (center-little-world-residency restored-source restored -2 4
                                         :radius 0)
          (multiple-value-bind (block status)
              (world-block-at restored -19 8 44)
            (true (eq status :resident))
            (true (eq block luvcraft::*crystal-block*)))
          (center-little-world-residency restored-source restored 0 -1
                                         :radius 0)
          (multiple-value-bind (block status)
              (world-block-at restored 3 4 -5)
            (true (eq status :resident))
            (true (null block))))
        (multiple-value-bind (restored-camera restored-player selected-block)
            (restore-luvcraft-resume-save-description resume)
          (true (= (camera-yaw restored-camera) 1.25))
          (true (= (camera-pitch restored-camera) -0.35))
          (true (= (player-x restored-player) -20.5d0))
          (true (= (player-y restored-player) 7.25d0))
          (true (= (player-z restored-player) 44.0d0))
          (true (eq selected-block luvcraft::*crystal-block*)))))))

(define-test camera-uniform-coerces-vec3-at-the-gpu-boundary
  (let* ((uniform
          (camera-uniform-data
           (make-instance 'fly-camera
                          :position (make-vec3 8d0 11d0 -6d0)
                          :yaw 1.25d0
                          :pitch -0.35d0)
           1280 720))
         (declaration
           (luv.arithmetic:value-declaration-for :camera-uniform-data)))
    (true (typep uniform '(simple-array single-float (20))))
    (true (typep uniform
                 (luv.arithmetic:declaration-representation-type declaration)))
    (true (= 20
             (luv.arithmetic:quantity-layout-extent
              (luv.arithmetic:declaration-quantity-layout declaration))))
    (true (equalp (subseq uniform 0 4) #(8.0 11.0 -6.0 0.0)))))

(define-test frame-uniform-product-matches-the-live-shader-contract
  (let* ((session
           (make-instance 'luvcraft-session
                          :camera (make-instance 'fly-camera)))
         (data (luvcraft::frame-uniform-data session 1280 720))
         (declaration
           (luv.arithmetic:value-declaration-for :frame-uniform-data))
         (host-layout
           (luv.arithmetic:declaration-quantity-layout declaration))
         (block (luvcraft.shaders:block-world-camera-uniform-block))
         (shader-layout (luvcraft::frame-shader-uniform-product-layout block)))
    (true (eq declaration
              (luv.arithmetic:value-declaration-for :frame-uniform-data)))
    (true (typep data
                 (luv.arithmetic:declaration-representation-type declaration)))
    (true (= 76 (luv.arithmetic:quantity-layout-extent host-layout)))
    (true (luv.arithmetic:quantity-layout= host-layout shader-layout))
    (true (= 304 (luvcraft::block-world-camera-uniform-size session)))
    (true (= (aref data 56)
             (* luvcraft::+block-atlas-tile-size+
                luvcraft::*block-atlas-tile-capacity*)))
    ;; Four dense matrix rows are representation for the declared
    ;; :WORLD-TO-SHADOW map, not sixteen falsely homogeneous quantities.
    (loop for position from 60 below 76
          do (true (null (luv.arithmetic:project-quantity-layout
                          host-layout (list position)))))))

(define-test world-save-validation-rejects-unknown-meaning
  (fail
   (restore-luvcraft-save-description
    '(:luvcraft-world :format-version 99
      :world (:block-world) :resume nil)))
  (fail
   (restore-block-save-description :block '(:name :missing-material)))
  (fail
   (restore-world-source-save-description
   :little-world '(:source-version 99 :seed 1 :edits ()))))

(define-test retired-gnome-world-edits-migrate-to-explicit-air
  (let ((overlay
          (luvcraft::restore-block-edit-overlay
           '((:at (48 6 -82) :value (:block :name :gnome))))))
    (multiple-value-bind (block present-p)
        (block-edit-at overlay 48 6 -82)
      (true present-p)
      (true (null block)))
    (true (equal (luvcraft::block-edit-overlay-save-descriptions overlay)
                 '((:at (48 6 -82) :value (:air)))))))

(define-test asynchronous-world-checkpoints-flush-the-latest-description
  (uiop:with-temporary-file
      (:pathname pathname :prefix "luvcraft-checkpoint-" :suffix ".sexp")
    (let* ((first-world (make-empty-little-block-world :seed 101))
           (latest-world (make-empty-little-block-world :seed 202))
           (writer (make-world-checkpoint-writer pathname)))
      (request-world-checkpoint
       writer (make-luvcraft-save-description first-world))
      (request-world-checkpoint
       writer (make-luvcraft-save-description latest-world))
      (stop-world-checkpoint-writer writer)
      (multiple-value-bind (restored resume) (read-luvcraft-save pathname)
        (true (null resume))
        (true (= (little-world-source-seed (block-world-source restored))
                 202))))))

(define-test little-world-residency-follows-a-bounded-window
  (let* ((world (make-little-block-world :chunk-radius 1 :seed 31))
         (source (block-world-source world)))
    (edit-block-at nil world 1 1 1)
    (multiple-value-bind (entering leaving)
        (center-little-world-residency source world 2 0 :radius 1)
      (true (= (length entering) 6))
      (true (= (length leaving) 6)))
    (true (= (length (resident-world-chunks world)) 9))
    (multiple-value-bind (chunk present-p) (world-chunk-at world 0 0 0)
      (true (null chunk))
      (true (null present-p)))
    (center-little-world-residency source world 0 0 :radius 1)
    (multiple-value-bind (block status) (world-block-at world 1 1 1)
      (true (eq status :resident))
      (true (null block)))
    (let ((revision (block-world-revision world)))
      (multiple-value-bind (entering leaving)
          (center-little-world-residency source world 0 0 :radius 1)
        (true (null entering))
        (true (null leaving)))
      (true (= (block-world-revision world) revision)))))

(define-test block-atlas-and-mesh-vertices-carry-material-readings
  (true (eq :srgb-to-linear
            (texture-format-sample-transfer
             luvcraft::+block-atlas-texture-format+)))
  (true (eq luvcraft::+block-atlas-texture-format+
            (luvcraft::ensure-block-atlas-sample-transfer
             luvcraft::+block-atlas-texture-format+)))
  (true (eq :identity
            (texture-format-sample-transfer
             luvcraft::+block-normal-atlas-texture-format+)))
  (fail
   (luvcraft::ensure-block-atlas-sample-transfer :rgba8-unorm)
   'error)
  (let* ((domain luvcraft:*block-atlas-tile-domain*)
         (tile-count (luvcraft:block-atlas-tile-count domain))
         (atlas (make-block-texture-atlas))
         (normal-atlas (make-block-normal-atlas)))
    (true (equal (array-dimensions atlas)
                 (list 16 (* 16 luvcraft::*block-atlas-tile-capacity*))))
    (true (equal (array-dimensions normal-atlas)
                 (list 16 (* 16 luvcraft::*block-atlas-tile-capacity*))))
    (true (subtypep (array-element-type atlas) '(unsigned-byte 32)))
    (true (subtypep (array-element-type normal-atlas) '(unsigned-byte 32)))
    ;; Painted tiles fill a prefix of the capacity; the headroom past them
    ;; stays zero, waiting for a live image to define a new material into it.
    (true (<= tile-count luvcraft::*block-atlas-tile-capacity*))
    (true (zerop (aref atlas 8 (* 16 tile-count))))
    (true (/= (aref atlas 8 8) (aref atlas 8 (+ 8 (* 3 16)))))
    (true (/= (aref atlas 8 8) (aref atlas 8 (+ 8 (* 9 16)))))
    ;; The colour atlas remains ordinary opaque sRGB material colour.
    (true (loop for tile below tile-count
                always (loop for x below luvcraft::+block-atlas-tile-size+
                             always (loop for y below
                                          luvcraft::+block-atlas-tile-size+
                                          always (= 255
                                                    (ldb (byte 8 24)
                                                         (aref atlas y
                                                               (+ x (* tile 16)))))))))
    (true (loop for tile below tile-count
                always (/= (ldb (byte 8 24)
                                (aref normal-atlas 3 (+ 3 (* tile 16))))
                           (ldb (byte 8 24)
                                (aref normal-atlas 11 (+ 12 (* tile 16)))))))
    ;; The normal materialization is derived from exactly that height field:
    ;; alpha preserves it byte-for-byte, RGB stays unit length within RGBA8
    ;; quantization, and at least one tangent lane responds to relief.
    (true (loop for y below luvcraft::+block-atlas-tile-size+
                always
                (loop for x below (* luvcraft::+block-atlas-tile-size+
                                     tile-count)
                      for tile = (floor x luvcraft::+block-atlas-tile-size+)
                      for local-x = (mod x luvcraft::+block-atlas-tile-size+)
                      always (= (luvcraft::paint-block-atlas-relief
                                 (luvcraft:block-atlas-tile-at-offset tile domain)
                                 local-x y)
                                (ldb (byte 8 24) (aref normal-atlas y x))))))
    (true (loop for y below luvcraft::+block-atlas-tile-size+
                always
                (loop for x below (* luvcraft::+block-atlas-tile-size+
                                     tile-count)
                      for pixel = (aref normal-atlas y x)
                      for nx = (- (/ (ldb (byte 8 0) pixel) 127.5) 1.0)
                      for ny = (- (/ (ldb (byte 8 8) pixel) 127.5) 1.0)
                      for nz = (- (/ (ldb (byte 8 16) pixel) 127.5) 1.0)
                      always (< (abs (- (+ (* nx nx) (* ny ny) (* nz nz))
                                        1.0))
                                0.025))))
    (true (loop for y below luvcraft::+block-atlas-tile-size+
                thereis
                (loop for x below (* luvcraft::+block-atlas-tile-size+
                                     tile-count)
                      for pixel = (aref normal-atlas y x)
                      thereis (or (/= (ldb (byte 8 0) pixel) 128)
                                  (/= (ldb (byte 8 8) pixel) 128))))))
  (flet ((face (name)
           (find name luvcraft::*block-faces* :key #'block-face-name)))
    (true (eq (block-face-tile luvcraft::*grass-block* (face :top)) :grass-top))
    (true (eq (block-face-tile luvcraft::*grass-block* (face :front)) :grass-side))
    (true (eq (block-face-tile luvcraft::*grass-block* (face :bottom)) :dirt))
    (true (eq (block-face-tile luvcraft::*wood-block* (face :top)) :wood-end))
    (true (eq (block-face-tile luvcraft::*sand-block* (face :top)) :sand))
    (true (eq (block-face-tile luvcraft::*snow-block* (face :top)) :snow))
    (true (eq (block-face-tile *crystal-block* (face :top)) :crystal))
    (true (eq (block-face-tile *terminal-block* (face :front)) :terminal))
    (true (eq (block-face-tile luvcraft::*cactus-block* (face :front)) :cactus-side))
    (true (eq (block-face-tile luvcraft::*cactus-block* (face :top)) :cactus-end))
    (true (= (block-light-emission *crystal-block*) 12))
    (true (= (block-surface-emission *crystal-block*) 1.2))
    (true (= (block-surface-emission *terminal-block*) 0.16))
    (true (equal (mapcar #'block-kind-name (placeable-block-kinds))
                 '(:grass :dirt :stone :wood :leaves :sand :snow :crystal
                   :terminal :urbit :gravel :clay :mud :moss :cactus
                   :cobblestone :stone-bricks :bricks :planks :sandstone
                   :slate :tape :fountain :lava-spring :flowers))))
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0)))
    (setf (world-block-at world 0 0 0) luvcraft::*stone-block*)
    (let ((mesh (mesh-block-chunk (make-instance 'exposed-face-mesher)
                                  world chunk)))
      (true (= (length (block-mesh-vertices mesh))
               (* luvcraft::+block-mesh-floats-per-vertex+
                  (block-mesh-vertex-count mesh)))))))

(define-test block-atlas-capacity-is-a-live-materialization-policy
  (let ((old-capacity luvcraft::*block-atlas-tile-capacity*))
    (unwind-protect
         (progn
           (setf luvcraft::*block-atlas-tile-capacity* (1+ old-capacity))
           (true (equal (array-dimensions (make-block-texture-atlas))
                        (list luvcraft::+block-atlas-tile-size+
                              (* luvcraft::+block-atlas-tile-size+
                                 (1+ old-capacity)))))
           (true (equal (array-dimensions (make-block-normal-atlas))
                        (list luvcraft::+block-atlas-tile-size+
                              (* luvcraft::+block-atlas-tile-size+
                                 (1+ old-capacity))))))
      (setf luvcraft::*block-atlas-tile-capacity* old-capacity))))

(define-test little-world-has-readable-biome-materials
  ;; Mountain ranges are a few hundred blocks apart, so the sweep is wide
  ;; enough to cross a valley, a range, and the snow above the tree line.
  (let* ((source (make-instance 'little-world-source :seed 121))
         (height luvcraft::*little-world-height*)
         (materials (make-hash-table :test #'eq))
         (lowest height)
         (highest 0))
    (loop for x from -320 to 320 by 8 do
      (loop for z from -320 to 320 by 8
            for surface = (little-world-surface-height source x z height)
            do (setf lowest (min lowest surface)
                     highest (max highest surface))
               (setf (gethash
                      (little-world-surface-material
                       source x z surface height)
                      materials)
                     t)))
    (true (gethash luvcraft::*grass-block* materials))
    (true (gethash luvcraft::*sand-block* materials))
    (true (gethash luvcraft::*stone-block* materials))
    (true (gethash luvcraft::*snow-block* materials))
    ;; Dramatic relief: the sweep spans more than half the world height.
    (true (> (- highest lowest) (/ height 2)))))

(define-test little-world-meadow-relief-is-the-original-terrain
  ;; Old saves restore as :meadow; their ground must not move.
  (let ((source (make-instance 'little-world-source :seed 121 :relief :meadow)))
    (true (eq (little-world-source-relief source) :meadow))
    (true
       (loop for x from -40 to 40 by 3
             always
             (loop for z from -40 to 40 by 3
                   for surface = (little-world-surface-height source x z 16)
                   always
                   (and (= surface
                           (luvcraft::little-world-meadow-surface-height
                            source x z 16))
                        (= surface
                           (little-world-ground-height source x z 16))
                        (eq (little-world-surface-material
                             source x z surface 16)
                            (luvcraft::little-world-meadow-surface-material
                             source x z surface 16))))))))

(define-test little-world-alpine-terrain-has-caves-with-solid-floors
  (let* ((world (make-little-block-world :chunk-radius 2 :seed 121))
         (source (block-world-source world))
         (height luvcraft::*little-world-height*)
         (cave-cells 0)
         (bad-grounds 0)
         (unsupported 0))
    (loop for x from -32 below 48 do
      (loop for z from -32 below 48
            for surface = (little-world-surface-height source x z height)
            for ground = (little-world-ground-height source x z height)
            ;; Landmarks may stand on the ground, so only the ground itself
            ;; is checked.
            do (unless (and (<= 2 ground surface)
                            (world-block-at world x ground z))
                 (incf bad-grounds))
               ;; Caves are hollows under the surface, never bottomless.
               (loop for y from 2 below surface
                     unless (world-block-at world x y z)
                       do (incf cave-cells)
                          (unless (loop for below from (1- y) downto 0
                                        thereis (world-block-at world x below z))
                            (incf unsupported)))))
    (true (> cave-cells 500))
    (true (= bad-grounds 0))
    (true (= unsupported 0))))

(define-test little-world-spawn-player-stands-on-the-ground
  (let* ((world (make-little-block-world :chunk-radius 1 :seed 121))
         (source (block-world-source world))
         (camera (make-instance 'fly-camera
                                :position (make-vec3 8.3d0 40.0d0 -5.7d0)))
         (player (little-world-spawn-player world camera))
         (ground (little-world-ground-height
                  source 8 -6 luvcraft::*little-world-height*)))
    (true (typep player 'block-world-player))
    (true (= (player-x player) 8.5d0))
    (true (= (player-z player) -5.5d0))
    (true (< ground (player-y player) (+ ground 2)))
    (true (world-block-at world 8 ground -6))
    (true (null (world-block-at world 8 (1+ ground) -6)))
    (true (null (little-world-spawn-player
                 (make-block-world :source nil) camera)))))

(define-test little-world-saves-keep-their-relief
  (let* ((alpine (make-empty-little-block-world :seed 5 :relief :alpine))
         (description (make-luvcraft-save-description alpine))
         (restored (restore-luvcraft-save-description description)))
    (true (eq (little-world-source-relief (block-world-source restored))
              :alpine)))
  ;; A save written before reliefs existed has no :relief entry and is a
  ;; meadow, so its edits stay on the ground they were made on.
  (let ((restored
          (restore-world-source-save-description
           :little-world
           (list :source-version luvcraft::+little-world-source-version+
                 :seed 913
                 :edits '()))))
    (true (eq (little-world-source-relief restored) :meadow))))

(define-test crosshair-and-numbered-materials-are-playable-state
  (let* ((vertices (luvcraft::make-block-world-crosshair-vertices 960 640))
         (canvas (make-instance 'title-canvas :title "luvcraft test"))
         (session (make-instance 'luvcraft-session
                                 :canvas canvas
                                 :title-base "luvcraft test"
                                 :selected-block luvcraft::*stone-block*)))
    (true (= (length vertices)
             (* luvcraft::+block-world-crosshair-vertex-count+ 6)))
    (true (eq (select-luvcraft-block session 1) luvcraft::*grass-block*))
    (true (eq (luvcraft-session-selected-block session) luvcraft::*grass-block*))
    (true (eq (select-luvcraft-block session 7) luvcraft::*snow-block*))
    (true (eq (select-luvcraft-block session 8) *crystal-block*))
    (true (eq (select-luvcraft-block session 9) *terminal-block*))
    (true (search "1–9,0 select" (canvas-title canvas)))
    (true (search "terminal" (canvas-title canvas)))
    ;; The tenth slot is the urbit material, and its chip is the 0 key.
    (true (eq (select-luvcraft-block session 10) luvcraft::*urbit-block*))
    (true (search "[0] urbit" (canvas-title canvas)))
    (true (eq (select-luvcraft-block session 11) luvcraft::*gravel-block*))
    (true (search "[inventory]" (canvas-title canvas)))))

(define-test urbit-wall-boots-a-comet-once-and-resumes-its-pier
  ;; The pier lives under the checkout's build directory, named by the wall.
  (let ((pier (urbit-pier-pathname)))
    (true (search "build/urbit/comet" (namestring pier))))
  ;; A pier vere has not made an .urb in boots as a comet; one it has,
  ;; resumes.  The urbit itself is not run here: booting a comet is a
  ;; networked, minutes-long affair that belongs on a wall, not in a test.
  (let* ((pier (merge-pathnames
                (make-pathname :directory '(:relative "luv-urbit-test-pier"))
                (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (true (equal (list "-c" (namestring pier))
                        (urbit-boot-arguments pier)))
           (ensure-directories-exist (merge-pathnames #P".urb/" pier))
           (true (equal (list (namestring pier))
                        (urbit-boot-arguments pier))))
      (uiop:delete-directory-tree pier :validate t :if-does-not-exist :ignore))))

(define-test block-inventory-supports-creative-and-finite-stacks
  (let* ((creative
           (make-block-inventory :blocks (list luvcraft::*stone-block*)))
         (finite (make-block-inventory :blocks (list luvcraft::*dirt-block*)
                                       :quantity 2))
         (entry
           (block-inventory-entry-for finite luvcraft::*dirt-block*)))
    (true (equal (block-inventory-blocks creative)
                 (list luvcraft::*stone-block*)))
    (true (null (block-inventory-entry-quantity
                 (first (block-inventory-entries creative)))))
    (true (remove-block-from-inventory creative luvcraft::*stone-block* 1000))
    (true (remove-block-from-inventory finite luvcraft::*dirt-block*))
    (true (= 1 (block-inventory-entry-quantity entry)))
    (false (remove-block-from-inventory finite luvcraft::*dirt-block* 2))
    (add-block-to-inventory finite luvcraft::*dirt-block* 4)
    (true (= 5 (block-inventory-entry-quantity entry)))
    (add-block-to-inventory finite *crystal-block* 3)
    (true (equal (block-inventory-blocks finite)
                 (list luvcraft::*dirt-block* *crystal-block*)))
    (true (= 3
             (block-inventory-entry-quantity
              (block-inventory-entry-for finite *crystal-block*))))))

(define-test numbered-selection-follows-the-session-inventory
  (let* ((canvas (make-instance 'title-canvas :title "inventory test"))
         (inventory
           (make-block-inventory
            :blocks (list luvcraft::*wood-block* *crystal-block*)))
         (session
           (make-instance 'luvcraft-session
                          :canvas canvas :inventory inventory
                          :selected-block luvcraft::*wood-block*)))
    (true (eq *crystal-block* (select-luvcraft-block session 2)))
    (true (eq *crystal-block*
              (luvcraft-session-selected-block session)))
    (true (null (select-luvcraft-block session 3)))
    (true (search "1–2 select" (canvas-title canvas)))))

(define-test inventory-and-ten-slot-quickbar-have-independent-extents
  (let* ((extra
           (make-instance 'block-kind :name :test-extra
                          :face-tiles '(:all :stone)
                          :categories '(:building)
                          :display-color '(0.4 0.5 0.6)))
         (base-count (length (placeable-block-kinds)))
         (inventory
           (make-block-inventory
            :blocks (append (placeable-block-kinds) (list extra))))
         (canvas (make-instance 'title-canvas :title "inventory extent test"))
         (session
           (make-instance 'luvcraft-session
                          :canvas canvas :inventory inventory
                          :selected-block luvcraft::*grass-block*)))
    (true (= (1+ base-count) (length (block-inventory-blocks inventory))))
    (true (= 10 (length (block-inventory-quickbar-blocks inventory))))
    ;; The full inventory may select a block with no number key; the title
    ;; makes that distinction visible rather than advertising an eleventh key.
    (true (eq extra (select-luvcraft-block session (1+ base-count))))
    (true (search "[inventory]" (canvas-title canvas)))
    (true (search "1–9,0 select" (canvas-title canvas)))))

(define-test gazetteer-names-semantic-gameplay-views
  (let* ((views (luvcraft-gazetteer-views))
         (names (mapcar #'luvcraft-gazetteer-view-name views)))
    (true (equal names (remove-duplicates names :test #'eq)))
    (dolist (name '(:little-world-noon :little-world-dusk :shadow-forest
                    :glow-floor :crystal-seam :shadow-yard))
      (true (find name names)))
    (let* ((view (find-luvcraft-gazetteer-view "crystal-seam"))
           (world
             (funcall (luvcraft::luvcraft-gazetteer-view-world-factory view))))
      (true (eq (world-block-at world 16 1 8) *crystal-block*))
      (true (= (nth-value 1 (world-light-at world 16 1 8))
               (block-light-emission *crystal-block*)))
      (true (= (nth-value 1 (world-light-at world 15 1 8))
               (1- (block-light-emission *crystal-block*)))))))

(define-test shadow-yard-gazetteer-has-raised-casters-over-receiver
  (let* ((view (find-luvcraft-gazetteer-view "shadow-yard"))
         (world (funcall (luvcraft::luvcraft-gazetteer-view-world-factory view))))
    (true (eq (world-block-at world 7 0 7) luvcraft::*snow-block*))
    (true (eq (world-block-at world 9 1 10) luvcraft::*stone-block*))
    (true (eq (world-block-at world 10 8 10) luvcraft::*stone-block*))
    (true (null (world-block-at world 9 9 10)))
    (true (null (world-block-at world 8 1 4)))
    (true (= (nth-value 0 (world-light-at world 7 1 7)) 15))))

(define-test shadow-projection-ignores-subtexel-camera-translation
  (let* ((clock (make-instance 'sky-clock :pinned-day-fraction 0.42))
         (sky (sky-frame-parameters clock (make-default-sky-profile)))
         (first-camera
           (make-instance 'fly-camera :position (make-vec3 0d0 0d0 0d0)))
         (nearby-camera
           (make-instance 'fly-camera
                          :position (make-vec3 0.01d0 0d0 0.01d0)))
         (farther-camera
           (make-instance 'fly-camera
                          :position (make-vec3 0.25d0 0d0 0.25d0)))
         (anchor nil)
         (first-rows
           (multiple-value-bind (rows new-anchor)
               (luvcraft::shadow-frame-rows first-camera sky anchor)
             (setf anchor new-anchor)
             rows))
         (nearby-rows
           (multiple-value-bind (rows new-anchor)
               (luvcraft::shadow-frame-rows nearby-camera sky anchor)
             (setf anchor new-anchor)
             rows))
         (farther-rows
           (multiple-value-bind (rows new-anchor)
               (luvcraft::shadow-frame-rows farther-camera sky anchor)
             (setf anchor new-anchor)
             rows)))
    ;; The first two rows locate the orthographic footprint.  Translation
    ;; smaller than one 0.0625-world-unit shadow texel cannot move it, to
    ;; within the rounding of the anchor's own walk, which is a millionth
    ;; of a texel against the texel's 1/1024 of the footprint.
    (flet ((same-footprint-p (rows other)
             (every (lambda (a b) (< (abs (- a b)) 1e-9))
                    (subseq rows 0 8) (subseq other 0 8))))
      (true (same-footprint-p first-rows nearby-rows))
      (true (not (same-footprint-p first-rows farther-rows))))))

(define-test shadow-lattice-turns-about-the-camera-not-the-origin
  ;; A frame's texel lattice is the set of world points with integer
  ;; light-space texel coordinates.  When the sun moves, the lattice has to
  ;; turn, and it must do so about the eye: a point near the camera should
  ;; see nearly the same texel coordinate before and after, however far the
  ;; camera stands from the world origin.
  (let* ((profile (make-default-sky-profile))
         (camera (make-instance 'fly-camera
                                :position (make-vec3 700d0 70d0 -300d0)))
         (texels-per-unit (/ luvcraft::+luvcraft-shadow-map-size+
                             (* 2.0 luvcraft::+luvcraft-shadow-half-extent+)))
         (probe (list 702.3 71.1 -301.7)))
    (flet ((probe-texel (day-fraction anchor)
             (multiple-value-bind (rows anchor)
                 (luvcraft::shadow-frame-rows
                  camera
                  (sky-frame-parameters
                   (make-instance 'sky-clock :pinned-day-fraction day-fraction)
                   profile)
                  anchor)
               (values
                (loop for row in (list (subseq rows 0 4) (subseq rows 4 8))
                      collect (* texels-per-unit
                                 luvcraft::+luvcraft-shadow-half-extent+
                                 (+ (nth 3 row)
                                    (loop for index below 3
                                          sum (* (nth index row)
                                                 (nth index probe))))))
                anchor))))
      (multiple-value-bind (before anchor) (probe-texel 0.42 nil)
        (let ((after (probe-texel 0.42003 anchor)))
          ;; One frame of a ten-minute day turns the sun by 1.9e-4 radians;
          ;; a probe three units from the eye may move a thousandth of a
          ;; texel, not the tenth of a texel the origin pivot gave it.
          (loop for b in before for a in after
                do (true (< (abs (- a b)) 0.05))))))))

(define-test shadow-projection-is-continuous-through-old-up-axis-threshold
  (let* ((camera (make-instance 'fly-camera))
         (profile (make-default-sky-profile))
         (before
           (luvcraft::shadow-frame-rows
            camera
            (sky-frame-parameters
             (make-instance 'sky-clock :pinned-day-fraction 0.451)
             profile)))
         (after
           (luvcraft::shadow-frame-rows
            camera
            (sky-frame-parameters
             (make-instance 'sky-clock :pinned-day-fraction 0.453)
             profile)))
         (right-dot
           (loop for index below 3
                 sum (* (nth index before) (nth index after)))))
    ;; Row X has length 1/extent.  Undo that scale before comparing the
    ;; neighboring orientations around the former abs(forward.y)=0.92 switch.
    (true (> (* right-dot
                luvcraft::+luvcraft-shadow-half-extent+
                luvcraft::+luvcraft-shadow-half-extent+)
             0.99))))

(define-test temporal-frame-derivatives-expose-change-and-flicker
  (let ((first #(10 20 30 255 40 50 60 255))
        (second #(13 17 36 255 40 50 60 255))
        (third #(16 14 42 255 43 53 63 255)))
    (multiple-value-bind (difference mean maximum changed)
        (luvcraft::temporal-derivative-rgba second first 10.0)
      (true (equalp difference #(40 40 40 255 0 0 0 255)))
      (true (< (abs (- mean (/ 2.0 255.0))) 1e-6))
      (true (< (abs (- maximum (/ 4.0 255.0))) 1e-6))
      (true (= changed 0.5)))
    (multiple-value-bind (difference mean maximum changed)
        (luvcraft::temporal-derivative-rgba third second 10.0 first)
      (true (equalp difference #(0 0 0 255 30 30 30 255)))
      (true (< (abs (- mean (/ 1.5 255.0))) 1e-6))
      (true (< (abs (- maximum (/ 3.0 255.0))) 1e-6))
      (true (= changed 0.5)))))

(define-test scalar-player-walks-collides-and-jumps
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (camera (make-instance 'fly-camera
                                :position (make-vec3 1.5 4.62 1.5)
                                :yaw 0d0 :pitch 0d0))
         (player (make-instance 'block-world-player
                                :position (make-vec3 1.5d0 3d0 1.5d0)))
         (intent (make-movement-intent)))
    (ensure-world-chunk world 0 0 0)
    (loop for x below 4 do
      (loop for z below 4 do
        (setf (world-block-at world x 0 z) luvcraft::*stone-block*)))
    ;; Gravity settles the body exactly on the block tops.
    (dotimes (step 240)
      (declare (ignorable step))
      (step-block-world-player player world camera intent (/ 1d0 120d0)))
    (true (< (abs (- (player-y player) 1d0)) 1d-5))
    (true (player-grounded-p player))
    (true (< (abs (- (camera-y camera) 2.62d0)) 1d-5))
    ;; A held right input accelerates into, but not through, a two-block wall.
    (setf (world-block-at world 3 1 1) luvcraft::*stone-block*
          (world-block-at world 3 2 1) luvcraft::*stone-block*
          (movement-urging-p intent :right) t)
    (dotimes (step 120)
      (declare (ignorable step))
      (step-block-world-player player world camera intent (/ 1d0 120d0)))
    (true (<= (player-x player) 2.700001d0))
    (true (= (player-velocity-x player) 0d0))
    (true (< (abs (- (player-y player) 1d0)) 1d-5))
    (true (player-grounded-p player))
    (setf (movement-urging-p intent :right) nil)
    ;; Jump is an edge request, not a second form of flying.
    (let ((ground-y (player-y player)))
      (step-block-world-player player world camera intent (/ 1d0 120d0)
                               :jump-p t)
      (true (> (player-y player) ground-y))
      (true (not (player-grounded-p player))))
    (dotimes (step 120)
      (declare (ignorable step))
      (step-block-world-player player world camera intent (/ 1d0 120d0)))
    (true (< (abs (- (player-y player) 1d0)) 1d-5))
    (true (player-grounded-p player))))

(define-test scalar-player-autojumps-a-clear-one-block-ledge
  (let* ((world (make-block-world :chunk-width 8
                                  :chunk-height 4
                                  :chunk-depth 4))
         (camera (make-instance 'fly-camera
                                :position (make-vec3 1.5 2.62 1.5)
                                :yaw 0d0 :pitch 0d0))
         (player (make-instance 'block-world-player
                                :position (make-vec3 1.5d0 1d0 1.5d0)
                                :grounded-p t))
         (intent (make-movement-intent))
         (highest-y (player-y player)))
    (ensure-world-chunk world 0 0 0)
    (loop for x below 8 do
      (loop for z below 4 do
        (setf (world-block-at world x 0 z) luvcraft::*stone-block*)))
    (setf (world-block-at world 3 1 1) luvcraft::*stone-block*
          (movement-urging-p intent :right) t)
    (dotimes (step 120)
      (declare (ignorable step))
      (step-block-world-player player world camera intent (/ 1d0 120d0))
      (setf highest-y (max highest-y (player-y player))))
    (true (> highest-y 2d0))
    (true (> (player-x player) 3.3d0))))

(define-test destinational-body-routes-around-terrain-and-arrives-continuously
  (let* ((world (make-block-world :chunk-width 8
                                  :chunk-height 4
                                  :chunk-depth 8))
         (player (make-instance 'block-world-player
                                :position (make-vec3 1.5d0 1d0 1.5d0)
                                :grounded-p t))
         (action nil))
    (ensure-world-chunk world 0 0 0)
    (loop for x below 8 do
      (loop for z below 8 do
        (setf (world-block-at world x 0 z) luvcraft::*stone-block*)))
    ;; A head-high wall closes the direct line but leaves both sides open.
    (setf (world-block-at world 3 1 1) luvcraft::*stone-block*
          (world-block-at world 3 2 1) luvcraft::*stone-block*
          action (start-body-move-to player world 5 1 1))
    (true (eq :running (body-move-action-status action)))
    (true (> (length (body-move-action-path action)) 4)
          "the planned intent names a route, not a straight velocity")
    (let ((start (body-cell-list player))
          (start-x (player-x player))
          (start-z (player-z player)))
      (advance-body-movement player world (/ 1d0 120d0))
      (true (or (> (abs (- (player-x player) start-x)) 1d-6)
                (> (abs (- (player-z player) start-z)) 1d-6))
            "the physical body starts moving before its public cell changes")
      (true (equal start (body-cell-list player)))
      (true (eq :running (body-move-action-status action))))
    (loop repeat 2399
          while (eq :running (body-move-action-status action))
          do (advance-body-movement player world (/ 1d0 120d0)))
    (true (eq :arrived (body-move-action-status action)))
    (true (equal '(5 1 1) (body-cell-list player)))
    (true (< (abs (- (player-x player) 5.5d0)) 0.12d0))
    (true (< (abs (- (player-z player) 1.5d0)) 0.12d0))
    (true (null (body-movement-action player)))))

(define-test destinational-body-reports-an-unstandable-place-immediately
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (player (make-instance 'block-world-player
                                :position (make-vec3 1.5d0 1d0 1.5d0)
                                :grounded-p t)))
    (ensure-world-chunk world 0 0 0)
    (loop for x below 4 do
      (loop for z below 4 do
        (setf (world-block-at world x 0 z) luvcraft::*stone-block*)))
    (let ((action (start-body-move-to player world 2 3 2)))
      (true (eq :failed (body-move-action-status action)))
      (true (search "not a clear supported cell"
                    (body-move-action-detail action)))
      ;; Waiting after an immediate terminal result is also immediate; the
      ;; completion notification may already be sitting in its mailbox.
      (true (eq action (await-body-move-action action)))
      (true (null (body-movement-action player))))))

(define-test meshing-and-editing-cross-a-chunk-boundary
  (let ((world (make-block-world :chunk-width 2
                                 :chunk-height 2
                                 :chunk-depth 2)))
    (let ((left (ensure-world-chunk world 0 0 0))
          (right (ensure-world-chunk world 1 0 0)))
      (setf (world-block-at world 1 0 0) luvcraft::*stone-block*
            (world-block-at world 2 0 0) luvcraft::*stone-block*)
      (let ((mesher (make-instance 'exposed-face-mesher)))
        (flet ((face-count ()
                 (+ (block-mesh-face-count
                     (mesh-block-chunk mesher world left))
                    (block-mesh-face-count
                     (mesh-block-chunk mesher world right)))))
          (true (= (face-count) 10))
          (let ((revision (block-world-revision world)))
            (setf (world-block-at world 2 0 0) nil)
            (true (= (block-world-revision world) (1+ revision))))
          (true (= (face-count) 6))
          (setf (world-block-at world 2 0 0) luvcraft::*stone-block*)
          (true (= (face-count) 10)))))))

(define-test chunk-mesh-is-exactly-sized
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0))
         (mesher (make-instance 'exposed-face-mesher)))
    (setf (world-block-at world 0 0 0) luvcraft::*stone-block*)
    (let* ((mesh (mesh-block-chunk mesher world chunk))
           (vertices (block-mesh-vertices mesh)))
      (true (= (block-mesh-face-count mesh) 6))
      (true (= (length vertices)
               (* (block-mesh-face-count mesh)
                  luvcraft::+block-mesh-floats-per-face+)))
      (true (= (array-total-size vertices) (length vertices)))
      (true (= 14 luvcraft::+block-mesh-floats-per-vertex+))
      ;; Vertex lanes are atlas-independent: local corner coordinates, the
      ;; tile offset under the atlas mapping, and four base-three edge digits.
      (loop for offset from 0 below (length vertices)
            by luvcraft::+block-mesh-floats-per-vertex+
            do (true (member (aref vertices (+ offset 3))
                             '(0.03125 0.96875)))
               (true (member (aref vertices (+ offset 4))
                             '(0.03125 0.96875)))
               (true (= (aref vertices (+ offset 12))
                        (block-atlas-tile-offset :stone)))
               (true (= (aref vertices (+ offset 13)) 80.0))))))

(define-test immutable-mesh-snapshot-is-bit-identical-to-owner-side-meshing
  (let* ((world (make-little-block-world :chunk-radius 1 :seed 121))
         (chunk (world-chunk-at world 0 0 0))
         (mesher (make-instance 'exposed-face-mesher))
         (stamp (chunk-mesh-dependency-stamp world chunk))
         (snapshot (make-block-mesh-snapshot world chunk stamp))
         (direct (mesh-block-chunk mesher world chunk))
         (copied (mesh-block-snapshot mesher snapshot)))
    (true (equal stamp (block-mesh-snapshot-dependency-stamp snapshot)))
    (let ((halo (luvcraft::block-mesh-snapshot-halo-domain snapshot)))
      (true (= (luv.domains:domain-cardinality halo)
               (length (luvcraft::block-mesh-snapshot-sample-indices snapshot))
               (length (luvcraft::block-mesh-snapshot-sky-samples snapshot))
               (length
                (luvcraft::block-mesh-snapshot-block-light-samples snapshot)))))
    (true (eq (luvcraft.world.fields:field-definition-for :sky-light)
              (luvcraft::block-mesh-snapshot-sky-definition snapshot)))
    (true (eq (luvcraft.world.fields:field-definition-for :block-light)
              (luvcraft::block-mesh-snapshot-block-light-definition snapshot)))
    (true (= (block-mesh-face-count direct) (block-mesh-face-count copied)))
    (true (= (block-mesh-vertex-count direct) (block-mesh-vertex-count copied)))
    (true (equalp (block-mesh-vertices direct) (block-mesh-vertices copied)))
    (setf (world-block-at world 0 0 0) nil)
    (true (equalp (block-mesh-vertices copied)
                  (block-mesh-vertices (mesh-block-snapshot mesher snapshot))))))

(define-test production-system-coalesces-desired-work-and-stops-cooperatively
  (let ((system (luvcraft::make-single-worker-production-system
                 :name "luv production test")))
    (unwind-protect
         (let* ((first
                  (make-instance
                   'luvcraft::little-world-load-request
                   :key '(:load (0 0 0)) :priority 4
                   :seed 1 :demand-token 1
                   :width 8 :height 8 :depth 8))
                (latest
                  (make-instance
                   'luvcraft::little-world-load-request
                   :key '(:load (0 0 0)) :priority 0
                   :seed 2 :demand-token 2
                   :width 8 :height 8 :depth 8)))
           (luvcraft::schedule-production-request system first)
           (luvcraft::schedule-production-request system latest)
           (multiple-value-bind (result present-p)
               (sb-concurrency:receive-message
                (luv.production::production-system-result-mailbox system)
                :timeout 5.0)
             (true present-p)
             (true (null (luvcraft::production-result-condition result)))
             (true (<= (luvcraft::production-system-pending-count system) 2))))
      (luvcraft::stop-production-system system))
    (true (not (sb-thread:thread-alive-p
                (luv.production::production-system-thread system))))))

(define-test production-system-keeps-one-result-behind-its-owner
  (let* ((system (luvcraft::make-single-worker-production-system
                  :name "luv production backpressure test"))
         (first-gate (sb-thread:make-semaphore :count 0))
         (second-gate (sb-thread:make-semaphore :count 0))
         (first (make-instance 'gated-production-request
                               :key :first :gate first-gate :value :first))
         (second (make-instance 'gated-production-request
                                :key :second :gate second-gate :value :second)))
    (unwind-protect
         (progn
           (luvcraft::schedule-production-request system first)
           (true (wait-until
                  (lambda () (eq (production-system-active-request system)
                                 first))))
           ;; Scheduling while FIRST is active must remain desired work, not a
           ;; second queued wake which can run behind an unread first result.
           (luvcraft::schedule-production-request system second)
           (sb-thread:signal-semaphore first-gate)
           (true (wait-until
                  (lambda ()
                    (and (= 1 (sb-concurrency:mailbox-count
                               (luv.production::production-system-result-mailbox
                                system)))
                         (not (eq (production-system-active-request system)
                                  first))))))
           (true (null (production-system-active-request system)))
           (true (= 1 (sb-concurrency:mailbox-count
                       (luv.production::production-system-result-mailbox
                        system))))
           (true (nth-value
                  1 (gethash :second
                             (luv.production::production-system-desired system))))
           (multiple-value-bind (result present-p)
               (luvcraft::receive-production-result-no-hang system)
             (true present-p)
             (true (eq (luvcraft::production-result-value result) :first)))
           (true (wait-until
                  (lambda () (eq (production-system-active-request system)
                                 second))))
           (sb-thread:signal-semaphore second-gate)
           (multiple-value-bind (result present-p)
               (sb-concurrency:receive-message
                (luv.production::production-system-result-mailbox system)
                :timeout 2.0)
             (true present-p)
             (true (eq (luvcraft::production-result-value result) :second))))
      (sb-thread:signal-semaphore first-gate)
      (sb-thread:signal-semaphore second-gate)
      (luvcraft::stop-production-system system))))

(define-test prebuilt-world-remains-desired-for-asynchronous-meshing
  (let* ((world (make-block-world :chunk-width 8
                                  :chunk-height 8
                                  :chunk-depth 8))
         (first (ensure-world-chunk world -1 0 2))
         (second (ensure-world-chunk world 3 0 -4))
         (system (luvcraft::make-single-worker-production-system
                  :name "luv static residency test"))
         (session (make-instance 'luvcraft-session
                              :world world
                              :player (make-instance 'block-world-player
                                                     :position
                                                     (make-vec3 0d0 0d0 0d0))
                              :production-system system)))
    (unwind-protect
         (progn
           (luvcraft::maintain-luvcraft-residency session)
           (true (gethash (luvcraft::block-chunk-key first)
                          (luvcraft-session-desired-chunks session)))
           (true (gethash (luvcraft::block-chunk-key second)
                          (luvcraft-session-desired-chunks session)))
           (remove-world-chunk world -1 0 2)
           (luvcraft::maintain-luvcraft-residency session)
           (true (not (gethash (luvcraft::block-chunk-key first)
                               (luvcraft-session-desired-chunks session))))
           (true (gethash (luvcraft::block-chunk-key second)
                          (luvcraft-session-desired-chunks session))))
      (luvcraft::stop-production-system system))))

(define-test chunk-mesh-products-have-narrow-neighbor-dependencies
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (left (ensure-world-chunk world 0 0 0))
         (right (ensure-world-chunk world 1 0 0))
         (mesher (make-instance 'exposed-face-mesher)))
    (setf (world-block-at world 3 1 1) luvcraft::*stone-block*
          (world-block-at world 4 1 1) luvcraft::*stone-block*)
    (true (= (block-mesh-face-count (mesh-block-chunk mesher world left)) 5))
    (true (= (block-mesh-face-count (mesh-block-chunk mesher world right)) 5))
    (let ((stamp (chunk-mesh-dependency-stamp world left)))
      ;; This changes RIGHT, but not the boundary LEFT's mesh observes.
      (setf (world-block-at world 5 2 2) luvcraft::*stone-block*)
      (true (equal stamp (chunk-mesh-dependency-stamp world left)))
      ;; This touches RIGHT's -X boundary and must invalidate LEFT.
      (setf (world-block-at world 4 2 2) luvcraft::*stone-block*)
      (true (not (equal stamp (chunk-mesh-dependency-stamp world left)))))
    (let ((stamp (chunk-mesh-dependency-stamp world left)))
      (remove-world-chunk world 0 0 0)
      (let ((replacement (ensure-world-chunk world 0 0 0)))
        (true (not (equal stamp
                          (chunk-mesh-dependency-stamp world replacement))))))))

(defun test-luvcraft-chunk-product (chunk stamp)
  (make-instance
   'luvcraft::luvcraft-chunk-product
   :coordinate (chunk-domain-coordinate (block-chunk-domain chunk))
   :dependency-stamp stamp
   :mesh nil :vertex-buffer nil))

(define-test boundary-mesh-replacements-publish-as-one-visible-cohort
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (left (ensure-world-chunk world 0 0 0))
         (right (ensure-world-chunk world 1 0 0))
         (session (make-instance 'luvcraft::luvcraft-session :world world))
         (left-key '(0 0 0))
         (right-key '(1 0 0)))
    (setf (gethash left-key (luvcraft::luvcraft-session-desired-chunks session)) t
          (gethash right-key (luvcraft::luvcraft-session-desired-chunks session)) t
          (world-block-at world 1 0 0) luvcraft::*stone-block*
          (world-block-at world 2 0 0) luvcraft::*stone-block*)
    (let ((old-left
            (test-luvcraft-chunk-product
             left (chunk-mesh-dependency-stamp world left)))
          (old-right
            (test-luvcraft-chunk-product
             right (chunk-mesh-dependency-stamp world right))))
      (setf (gethash left-key (luvcraft::luvcraft-session-chunk-products session))
            old-left
            (gethash right-key (luvcraft::luvcraft-session-chunk-products session))
            old-right)
      ;; Removing RIGHT's boundary block also exposes a face owned by LEFT.
      ;; One completed replacement must leave the whole old pair visible.
      (setf (world-block-at world 2 0 0) nil)
      (let ((new-right
              (test-luvcraft-chunk-product
               right (chunk-mesh-dependency-stamp world right))))
        (setf (gethash right-key
                       (luvcraft::luvcraft-session-staged-chunk-products session))
              new-right)
        (true (zerop (luvcraft::publish-ready-luvcraft-meshes session)))
        (true (eq old-left
                  (gethash left-key
                           (luvcraft::luvcraft-session-chunk-products session))))
        (true (eq old-right
                  (gethash right-key
                           (luvcraft::luvcraft-session-chunk-products session))))
        (let ((new-left
                (test-luvcraft-chunk-product
                 left (chunk-mesh-dependency-stamp world left))))
          (setf (gethash left-key
                         (luvcraft::luvcraft-session-staged-chunk-products session))
                new-left)
          (true (= 2 (luvcraft::publish-ready-luvcraft-meshes session)))
          (true (eq new-left
                    (gethash left-key
                             (luvcraft::luvcraft-session-chunk-products session))))
          (true (eq new-right
                    (gethash right-key
                             (luvcraft::luvcraft-session-chunk-products session)))))))))

(define-test camera-edits-the-resident-lattice
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (camera (make-instance 'fly-camera
                                :position (make-vec3 0.5 1.5 1.5)
                                :yaw (/ pi 2) :pitch 0.0))
         (session (make-instance 'luvcraft-session
                              :world world
                              :camera camera
                              :selected-block luvcraft::*dirt-block*)))
    (ensure-world-chunk world 0 0 0)
    ;; The second stone means placing after removing the first still has a
    ;; solid target beyond the empty adjacent site.
    (setf (world-block-at world 2 1 1) luvcraft::*stone-block*
          (world-block-at world 3 1 1) luvcraft::*stone-block*)
    (multiple-value-bind (coordinate status)
        (edit-luvcraft-block session :remove)
      (true (eq status :edited))
      (true (= (world-coordinate-x coordinate) 2))
      (true (null (world-block-at world 2 1 1))))
    (let ((occupied-session
            (make-instance 'luvcraft-session
                           :world world :camera camera
                           :player (make-instance 'block-world-player
                                                  :position
                                                  (make-vec3 2.5d0 1d0 1.5d0))
                           :selected-block luvcraft::*dirt-block*)))
      (multiple-value-bind (coordinate status)
          (edit-luvcraft-block occupied-session :place)
        (true (null coordinate))
        (true (eq status :blocked))
        (true (null (world-block-at world 2 1 1)))))
    (multiple-value-bind (coordinate status)
        (edit-luvcraft-block session :place)
      (true (eq status :edited))
      (true (= (world-coordinate-x coordinate) 2))
      (true (eq (world-block-at world 2 1 1) luvcraft::*dirt-block*)))))
