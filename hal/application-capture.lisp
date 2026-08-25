;;; GPU-native screenshots and films of live applications.
;;;
;;; The application owns what a frame means.  This file owns the mechanical
;;; capture transaction: one offscreen color target, one readback buffer,
;;; command submission, synchronous readback, PNG/video output, real-time film
;;; pacing, and cleanup.  Nothing here reads a host window or presents a
;;; swapchain drawable.

(in-package #:luv)

(defclass application-capture ()
  ((application
    :initarg :application
    :reader capture-application)
   (kind
    :initarg :kind
    :reader capture-kind)
   (options
    :initarg :options
    :initform nil
    :reader capture-options)
   (label
    :initarg :label
    :initform "application capture"
    :reader capture-label)
   ;; Established once before PREPARE-CAPTURE.  A film deliberately keeps
   ;; this exact relationship and extent for its whole frame sequence.
   (canvas :initform nil :accessor capture-native-canvas)
   (context :initform nil :accessor capture-native-context)
   (device :initform nil :accessor capture-native-device)
   (extent :initform nil :accessor capture-extent)
   (format :initform nil :accessor capture-format)
   ;; Shared resources remain visible to CLEANUP-CAPTURE while still alive.
   ;; This lets an application evict target-keyed views or frame state before
   ;; the target itself is invalidated.
   (target :initform nil :accessor capture-target)
   (readback-buffer :initform nil :accessor capture-readback-buffer)
   (frame-index :initform 0 :accessor capture-frame-index)
   ;; A deliberately untyped application-owned cell for saving pause state or
   ;; another small piece of lifecycle information across the transaction.
   (client-state :initform nil :accessor capture-client-state))
  (:documentation
   "One screenshot or film transaction around an application-owned frame.

The object is control-plane state, allocated once per capture.  TARGET and
READBACK-BUFFER are dense GPU resources owned and destroyed by the shared
capture implementation."))

(define-condition application-capture-busy (error)
  ((application
    :initarg :application
    :reader application-capture-busy-application)
   (active-capture
    :initarg :active-capture
    :reader application-capture-busy-active-capture)
   (requested-capture
    :initarg :requested-capture
    :reader application-capture-busy-requested-capture))
  (:report
   (lambda (condition stream)
     (let ((active (application-capture-busy-active-capture condition))
           (requested
             (application-capture-busy-requested-capture condition)))
       (format stream
               "Cannot begin ~A (~(~A~)); ~A (~(~A~)) already owns ~S."
               (capture-label requested) (capture-kind requested)
               (capture-label active) (capture-kind active)
               (application-capture-busy-application condition))))))

(define-condition application-capture-shutting-down (error)
  ((application
    :initarg :application
    :reader application-capture-shutting-down-application)
   (requested-capture
    :initarg :requested-capture
    :reader application-capture-shutting-down-requested-capture))
  (:report
   (lambda (condition stream)
     (let ((capture
             (application-capture-shutting-down-requested-capture
              condition)))
       (format stream
               "Cannot begin ~A (~(~A~)); ~S has begun shutting down."
               (capture-label capture) (capture-kind capture)
               (application-capture-shutting-down-application condition))))))

(define-condition application-capture-shutdown-blocking-thread-error (error)
  ((application
    :initarg :application
    :reader application-capture-shutdown-blocking-application)
   (active-capture
    :initarg :active-capture
    :reader application-capture-shutdown-blocking-active-capture))
  (:report
   (lambda (condition stream)
     (format stream
             "Cannot wait for ~A (~(~A~)) to finish on ~S's canvas thread."
             (capture-label
              (application-capture-shutdown-blocking-active-capture condition))
             (capture-kind
              (application-capture-shutdown-blocking-active-capture condition))
             (application-capture-shutdown-blocking-application condition)))))

(defclass application-capture-coordinator ()
  ((lock
    :initform (sb-thread:make-mutex :name "application capture reservation")
    :reader application-capture-coordinator-lock)
   (active-capture
    :initform nil
    :accessor application-capture-coordinator-active-capture)
   (state
    :initform :open
    :accessor application-capture-coordinator-state)
   (ready
    :initform (sb-thread:make-waitqueue
               :name "application capture quiescence")
    :reader application-capture-coordinator-ready))
  (:documentation
   "One shutdown-aware semantic reservation lane for an application's captures.

OPEN admits one capture.  SHUTTING-DOWN is terminal: new captures fail and a
non-canvas teardown owner may wait for the existing capture to leave."))

(defvar *application-capture-coordinators*
  (make-hash-table :test #'eq :weakness :key)
  "Weak application identities mapped to their capture reservation lanes.")

(defvar *application-capture-coordinators-lock*
  (sb-thread:make-mutex :name "application capture coordinators"))

(defun application-capture-coordinator-for (application)
  (sb-thread:with-mutex (*application-capture-coordinators-lock*)
    (or (gethash application *application-capture-coordinators*)
        (setf (gethash application *application-capture-coordinators*)
              (make-instance 'application-capture-coordinator)))))

(defun reserve-application-capture (capture)
  "Reserve CAPTURE's application without holding a lock across its work."
  (let* ((application (capture-application capture))
         (coordinator (application-capture-coordinator-for application))
         (active nil)
         (shutting-down-p nil))
    (sb-thread:with-mutex ((application-capture-coordinator-lock coordinator))
      (setf shutting-down-p
            (eq :shutting-down
                (application-capture-coordinator-state coordinator)))
      (unless shutting-down-p
        (setf active
              (application-capture-coordinator-active-capture coordinator))
        (unless active
          (setf (application-capture-coordinator-active-capture coordinator)
                capture))))
    ;; Signal after leaving the semantic mutex so a handler may safely inspect
    ;; the capture identities or choose another application.
    (cond
      (shutting-down-p
       (error 'application-capture-shutting-down
              :application application
              :requested-capture capture))
      (active
       (error 'application-capture-busy
              :application application
              :active-capture active
              :requested-capture capture)))
    coordinator))

(defun release-application-capture (coordinator capture)
  "Release CAPTURE's exact reservation without disturbing a later owner."
  (sb-thread:with-mutex ((application-capture-coordinator-lock coordinator))
    (when (eq capture
              (application-capture-coordinator-active-capture coordinator))
      (setf (application-capture-coordinator-active-capture coordinator) nil)
      (sb-thread:condition-broadcast
       (application-capture-coordinator-ready coordinator))))
  (values))

(defun call-with-application-capture-reserved (function capture)
  (let ((coordinator (reserve-application-capture capture)))
    (unwind-protect
         (funcall function)
      (release-application-capture coordinator capture))))

(defun application-capture-shutdown-p (application)
  "Return true once APPLICATION's capture admission has closed terminally."
  (let ((coordinator (application-capture-coordinator-for application)))
    (sb-thread:with-mutex
        ((application-capture-coordinator-lock coordinator))
      (eq :shutting-down
          (application-capture-coordinator-state coordinator)))))

(defun request-application-capture-shutdown (application)
  "Close APPLICATION's capture admission without waiting for active work.

This is safe on a canvas thread.  Return true when this call closed admission,
or NIL when shutdown had already been requested."
  (let ((coordinator (application-capture-coordinator-for application))
        (began-p nil))
    (sb-thread:with-mutex
        ((application-capture-coordinator-lock coordinator))
      (case (application-capture-coordinator-state coordinator)
        (:open
         (setf (application-capture-coordinator-state coordinator)
               :shutting-down
               began-p t))
        (:shutting-down)
        (otherwise
         (error "Invalid application capture coordinator state ~S."
                (application-capture-coordinator-state coordinator))))
      (when began-p
        (sb-thread:condition-broadcast
         (application-capture-coordinator-ready coordinator))))
    began-p))

(defun call-if-application-captures-open (application function)
  "Atomically call small publication FUNCTION only while admission is open.

FUNCTION runs under APPLICATION's short capture gate lock and must not block or
reenter the capture gate.  Return true when it ran, NIL after shutdown began."
  (check-type function function)
  (let ((coordinator (application-capture-coordinator-for application)))
    (sb-thread:with-mutex
        ((application-capture-coordinator-lock coordinator))
      (when (eq :open (application-capture-coordinator-state coordinator))
        (funcall function)
        t))))

(defun proper-capture-options-p (options)
  (loop for tail = options then (cddr tail)
        while tail
        always (and (consp tail) (consp (cdr tail)))))

(defmethod initialize-instance :after ((capture application-capture) &key)
  (unless (member (capture-kind capture) '(:screenshot :film))
    (error "Unsupported application capture kind ~S."
           (capture-kind capture)))
  (unless (proper-capture-options-p (capture-options capture))
    (error "Application capture options must be a proper property list: ~S."
           (capture-options capture)))
  (unless (stringp (capture-label capture))
    (error 'type-error :datum (capture-label capture)
                       :expected-type 'string)))

(defun capture-option (capture indicator &optional default)
  "Return INDICATOR's value in CAPTURE's application-owned option plist."
  (let ((missing (cons nil nil)))
    (let ((value (getf (capture-options capture) indicator missing)))
      (if (eq value missing) default value))))

(defgeneric capture-canvas (application)
  (:documentation
   "Return APPLICATION's open LUV canvas for a native GPU capture.

The shared implementation derives the presentation context and device through
CANVAS-CONTEXT and CONTEXT-DEVICE; applications do not repeat those hooks."))

(defun quiesce-application-captures (application)
  "Close APPLICATION's capture gate and wait for its active capture to leave.

The caller must run beside the canvas thread while a capture is active.  The
active capture may still need that thread for its final frame and cache cleanup;
waiting on the canvas thread would deadlock it and is rejected explicitly."
  (request-application-capture-shutdown application)
  (let* ((coordinator (application-capture-coordinator-for application))
         (canvas (capture-canvas application))
         (blocking-thread-p (canvas-thread-p canvas)))
    (sb-thread:with-mutex
        ((application-capture-coordinator-lock coordinator))
      (loop
        for active =
        (application-capture-coordinator-active-capture coordinator)
        while active
        do (when blocking-thread-p
             (error 'application-capture-shutdown-blocking-thread-error
                    :application application :active-capture active))
           (sb-thread:condition-wait
            (application-capture-coordinator-ready coordinator)
            (application-capture-coordinator-lock coordinator)))))
  (values))

(defgeneric prepare-capture (application capture)
  (:documentation
   "Prepare APPLICATION for CAPTURE on the calling thread.

This is the place to wait for application-specific readiness and to quiesce a
normal frame loop.  CAPTURE's canvas, context, device, extent, and format are
already established, but its shared GPU resources do not exist yet.
CLEANUP-CAPTURE is called even when this method exits by error."))

(defmethod prepare-capture ((application t) (capture application-capture))
  (declare (ignore application capture))
  (values))

(defgeneric advance-capture-frame (application capture frame-index)
  (:documentation
   "Advance application-owned work before offscreen film frame FRAME-INDEX.

This runs on the canvas's native frame thread after BEFORE-FRAME and immediately
before that frame's GPU encoding.  A streaming application can therefore publish
completed products without racing an encoded but not yet submitted native frame,
or putting its world model or simulation policy into the shared capture loop."))

(defmethod advance-capture-frame
    ((application t) (capture application-capture) frame-index)
  (declare (ignore application capture frame-index))
  (values))

(defgeneric encode-capture-frame
    (application capture encoder target extent)
  (:documentation
   "Encode APPLICATION's complete visible frame into offscreen TARGET.

This method runs on the canvas's native frame thread.  ENCODER is open and
owned by the shared implementation; EXTENT is the fixed two-dimensional
capture extent.  The method must not finish or submit ENCODER and must not
destroy TARGET.  Its primary value is optional application metadata returned
by CAPTURE-APPLICATION-SCREENSHOT."))

(defgeneric cleanup-capture (application capture)
  (:documentation
   "Undo application-owned CAPTURE preparation on the calling thread.

When shared GPU resources were created they are still alive and visible through
CAPTURE-TARGET and CAPTURE-READBACK-BUFFER during this method.  Applications
with target-keyed GPU caches should synchronously evict those entries on their
canvas thread here.  Shared resources are destroyed after this method returns.
The method must not destroy the shared target or readback buffer."))

(defmethod cleanup-capture ((application t) (capture application-capture))
  (declare (ignore application capture))
  (values))

(defun capture-color-format-p (format)
  (member format '(:rgba8-unorm :rgba8-unorm-srgb
                   :bgra8-unorm :bgra8-unorm-srgb)))

(defun normalize-capture-extent (extent)
  (unless (and (typep extent 'sequence)
               (= 2 (length extent))
               (every (lambda (dimension)
                        (typep dimension '(integer 1)))
                      extent))
    (error "Application capture requires a positive 2D extent, got ~S."
           extent))
  (coerce extent 'list))

(defun establish-capture-relationship (capture)
  (let* ((application (capture-application capture))
         (canvas (capture-canvas application)))
    (unless (typep canvas 'canvas)
      (error "CAPTURE-CANVAS returned ~S, not a LUV canvas." canvas))
    (unless (eq :open (canvas-state canvas))
      (error 'canvas-state-error
             :canvas canvas :operation :capture :reason :invalid-state
             :state (canvas-state canvas) :expected-state :open))
    (let ((context (canvas-context canvas)))
      (unless (typep context 'canvas-context)
        (error "Capture canvas ~S has no configured LUV context." canvas))
      (let ((device (context-device context))
            (extent (normalize-capture-extent (canvas-extent context)))
            (format (canvas-format context)))
        (unless (typep device 'gpu-device)
          (error "Capture context ~S has no configured LUV device." context))
        (unless (capture-color-format-p format)
          (error "Application capture requires an RGBA8 or BGRA8 color ~
                  format, got ~S."
                 format))
        (setf (capture-native-canvas capture) canvas
              (capture-native-context capture) context
              (capture-native-device capture) device
              (capture-extent capture) extent
              (capture-format capture) format))))
  capture)

(defun ensure-capture-relationship-current (capture)
  "Reject a resize or context replacement before it becomes a GPU mismatch."
  (let* ((canvas (capture-native-canvas capture))
         (context (canvas-context canvas)))
    (unless (eq context (capture-native-context capture))
      (error "The canvas context changed during ~A."
             (capture-label capture)))
    (unless (eq (context-device context) (capture-native-device capture))
      (error "The canvas device changed during ~A."
             (capture-label capture)))
    (unless (eq (canvas-format context) (capture-format capture))
      (error "The canvas format changed during ~A: ~S became ~S."
             (capture-label capture) (capture-format capture)
             (canvas-format context)))
    (let ((extent (normalize-capture-extent (canvas-extent context))))
      (unless (equal extent (capture-extent capture))
        (error "The canvas extent changed during ~A: ~S became ~S."
               (capture-label capture) (capture-extent capture) extent))))
  capture)

(defun capture-resource-label (capture noun)
  (format nil "~A ~A" (capture-label capture) noun))

(defun make-capture-target (capture)
  (create
   (capture-native-device capture)
   (make-texture-descriptor
    :label (capture-resource-label capture "target")
    :size (capture-extent capture)
    :dimensions :2d
    :format (capture-format capture)
    ;; Luvcraft copies its resolved presentation image into TARGET, while
    ;; LUFT renders directly through a view.  The shared readback needs
    ;; COPY-SRC in either case.
    :usage '(:render-attachment :copy-src :copy-dst))))

(defun make-capture-readback-buffer (capture)
  (destructuring-bind (width height) (capture-extent capture)
    (create
     (capture-native-device capture)
     (make-buffer-descriptor
      :label (capture-resource-label capture "readback")
      :size (* 4 width height)
      :usage '(:copy-dst)))))

(defun render-capture-frame (capture &key advance-p)
  "Render and synchronously read one frame from an established CAPTURE.

When ADVANCE-P is true, advance the application inside the same native canvas
request which encodes and submits the frame."
  (let* ((application (capture-application capture))
         (canvas (capture-native-canvas capture))
         (device (capture-native-device capture))
         (metadata nil))
    (request-canvas-frame
     canvas
     (lambda (timestamp)
       (declare (ignore timestamp))
       (when advance-p
         (advance-capture-frame
          application capture (capture-frame-index capture)))
       (ensure-capture-relationship-current capture)
       (let ((encoder nil)
             (commands nil))
         (unwind-protect-releasing
              (progn
                (setf encoder
                      (create
                       device
                       (make-command-encoder-descriptor
                        :label
                        (capture-resource-label capture "frame encoder"))))
                (setf metadata
                      (encode-capture-frame
                       application capture encoder
                       (capture-target capture) (capture-extent capture)))
                (encode
                 encoder
                 (make-gpu-copy-texture-to-buffer-command
                  :source (capture-target capture)
                 :destination (capture-readback-buffer capture)))
                (setf commands (finish encoder))
                (submit (device-queue device) commands))
           (releasing :capture-frame-command-buffer
             (when commands (destroy commands)))
           (releasing :capture-frame-encoder
             (when encoder (destroy encoder)))))))
    (values (read-buffer (capture-readback-buffer capture)) metadata)))

(defun call-with-capture-target (function capture)
  "Call FUNCTION with CAPTURE while owning its offscreen GPU resources."
  (check-type function function)
  (call-with-application-capture-reserved
   (lambda ()
     (establish-capture-relationship capture)
     (let ((cleanup-p nil))
       (unwind-protect-releasing
            (progn
              ;; CLEANUP-CAPTURE also runs for a partially completed
              ;; preparation.
              (setf cleanup-p t)
              (prepare-capture (capture-application capture) capture)
              (setf (capture-target capture) (make-capture-target capture))
              (setf (capture-readback-buffer capture)
                    (make-capture-readback-buffer capture))
              (funcall function capture))
         ;; Application cleanup runs first because target-keyed views must not
         ;; outlive the target.  Every shared object still gets an attempt, and
         ;; a body failure stays primary if any of these steps also fails.
         (releasing :capture-application-cleanup
           (when cleanup-p
             (cleanup-capture (capture-application capture) capture)))
         (releasing :capture-readback-buffer
           (when (capture-readback-buffer capture)
             (destroy (capture-readback-buffer capture))
             (setf (capture-readback-buffer capture) nil)))
         (releasing :capture-target
           (when (capture-target capture)
             (destroy (capture-target capture))
             (setf (capture-target capture) nil))))))
   capture))

(defun capture-application-screenshot
    (application pathname &key options (label "application screenshot"))
  "Render APPLICATION offscreen and write its native-resolution PNG.

OPTIONS is an application-owned property list interpreted by the capture
protocol methods.  Return PATHNAME, packed pixels, width, height, color format,
and the optional metadata value from ENCODE-CAPTURE-FRAME."
  (let ((capture
          (make-instance 'application-capture
                         :application application
                         :kind :screenshot
                         :options options
                         :label label)))
    (call-with-capture-target
     (lambda (capture)
       (multiple-value-bind (pixels metadata)
           (render-capture-frame capture)
         (destructuring-bind (width height) (capture-extent capture)
           (ensure-directories-exist pathname)
           (write-rgba-png pathname pixels width height
                           (capture-format capture))
           (values pathname pixels width height (capture-format capture)
                   metadata))))
     capture)))

(defun capture-clock-seconds ()
  (/ (get-internal-real-time)
     (float internal-time-units-per-second 1.0d0)))

(defun capture-frame-wait-seconds
    (start-seconds frame-index frame-rate now-seconds)
  "Return the wall-clock wait before FRAME-INDEX's successor deadline."
  (- (+ start-seconds (/ (1+ frame-index) (float frame-rate 1.0d0)))
     now-seconds))

(defun call-with-capture-film-frames
    (writer capture frame-count frame-rate
     &key before-frame progress-function
          (clock-function #'capture-clock-seconds)
          (sleep-function #'sleep))
  "Write FRAME-COUNT real-time-paced offscreen frames through WRITER."
  (check-type writer function)
  (check-type frame-count (integer 1))
  (check-type frame-rate (integer 1))
  (when before-frame (check-type before-frame function))
  (when progress-function (check-type progress-function function))
  (check-type clock-function function)
  (check-type sleep-function function)
  (let ((start (funcall clock-function))
        (application (capture-application capture)))
    (dotimes (frame frame-count)
      ;; A stop owner has closed admission and is waiting beside the canvas
      ;; thread.  Finish at most the current frame, then let cleanup release the
      ;; reservation promptly.  A film always emits its first frame so ffmpeg
      ;; receives a valid stream even when shutdown races its preparation.
      (when (and (plusp frame)
                 (application-capture-shutdown-p application))
        (return))
      (setf (capture-frame-index capture) frame)
      (when before-frame (funcall before-frame frame))
      (funcall writer (render-capture-frame capture :advance-p t))
      (when progress-function
        (funcall progress-function frame frame-count))
      ;; Absolute deadlines prevent encoding variance from accumulating drift.
      (let ((wait
              (capture-frame-wait-seconds
               start frame frame-rate (funcall clock-function))))
        (when (and (plusp wait)
                   (not (application-capture-shutdown-p application)))
          (funcall sleep-function wait)))))
  (values))

(defun capture-application-film
    (application pathname
     &key (seconds 8) (frame-rate 30) before-frame progress-function options
          (label "application film"))
  "Film APPLICATION offscreen into a real-time-paced H.264 MP4.

The application chooses readiness, advancement, frame meaning, and cleanup;
the shared implementation owns the fixed-size target, readback, ffmpeg writer,
and absolute wall-clock pacing.  Return PATHNAME and the encoded frame count."
  (check-type seconds (real 0))
  (check-type frame-rate (integer 1))
  (let* ((frame-count (max 1 (round (* seconds frame-rate))))
         (capture
           (make-instance 'application-capture
                          :application application
                          :kind :film
                          :options options
                          :label label)))
    (call-with-capture-target
     (lambda (capture)
       (destructuring-bind (width height) (capture-extent capture)
         (call-with-video-encoder
          (lambda (writer)
            (call-with-capture-film-frames
             writer capture frame-count frame-rate
             :before-frame before-frame
             :progress-function progress-function))
          pathname width height
          :frame-rate frame-rate :format (capture-format capture))))
     capture)))
