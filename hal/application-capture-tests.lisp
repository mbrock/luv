(in-package #:luv.tests)

;;; A backend-free executable capture transaction.  The probe keeps the GPU
;;; protocol real (descriptors, command encoding, submission, readback, and
;;; destruction) while replacing native resources with small Lisp objects.

(defclass capture-probe-resource ()
  ((destroyed-p :initform nil :accessor capture-probe-destroyed-p)))

(defvar *capture-probe-destroy-events* nil)
(defvar *capture-probe-destroy-failures* nil)

(defclass capture-probe-texture
    (luv:gpu-texture capture-probe-resource)
  ((pixels :initarg :pixels :accessor capture-probe-texture-pixels)))

(defclass capture-probe-buffer
    (luv:gpu-buffer capture-probe-resource)
  ((data :initarg :data :accessor capture-probe-buffer-data)))

(defclass capture-probe-encoder
    (luv:gpu-command-encoder capture-probe-resource)
  ((device :initarg :device :reader capture-probe-encoder-device)
   (commands :initform nil :accessor capture-probe-encoder-commands)))

(defclass capture-probe-command-buffer
    (luv:gpu-command-buffer capture-probe-resource)
  ((commands :initarg :commands :reader capture-probe-command-buffer-commands)))

(defclass capture-probe-queue (luv:gpu-queue) ())

(defclass capture-probe-device (luv:gpu-device)
  ((queue :initform (make-instance 'capture-probe-queue)
          :reader capture-probe-device-queue)
   (resources :initform nil :accessor capture-probe-device-resources)))

(defmethod luv:device-queue ((device capture-probe-device))
  (capture-probe-device-queue device))

(defmethod luv:create
    ((device capture-probe-device) (descriptor luv::texture-descriptor))
  (destructuring-bind (width height depth)
      (luv::texture-descriptor-size descriptor)
    (declare (ignore depth))
    (let ((texture
            (make-instance
             'capture-probe-texture
             :label (luv::gpu-descriptor-label descriptor)
             :size (luv::texture-descriptor-size descriptor)
             :usage (luv::texture-descriptor-usage descriptor)
             :dimensions (luv::texture-descriptor-dimensions descriptor)
             :format (luv::texture-descriptor-format descriptor)
             :pixels (make-array (* 4 width height)
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0))))
      (push texture (capture-probe-device-resources device))
      texture)))

(defmethod luv:create
    ((device capture-probe-device) (descriptor luv::buffer-descriptor))
  (let ((buffer
          (make-instance
           'capture-probe-buffer
           :label (luv::gpu-descriptor-label descriptor)
           :size (luv::buffer-descriptor-size descriptor)
           :usage (luv::buffer-descriptor-usage descriptor)
           :data (make-array (luv::buffer-descriptor-size descriptor)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0))))
    (push buffer (capture-probe-device-resources device))
    buffer))

(defmethod luv:create
    ((device capture-probe-device)
     (descriptor luv::command-encoder-descriptor))
  (let ((encoder
          (make-instance 'capture-probe-encoder
                         :label (luv::gpu-descriptor-label descriptor)
                         :device device)))
    (push encoder (capture-probe-device-resources device))
    encoder))

(defmethod luv:encode
    ((encoder capture-probe-encoder) (command luv::gpu-command))
  (push command (capture-probe-encoder-commands encoder))
  command)

(defmethod luv:finish ((encoder capture-probe-encoder))
  (let ((commands
          (make-instance
           'capture-probe-command-buffer
           :commands (nreverse (capture-probe-encoder-commands encoder)))))
    (push commands
          (capture-probe-device-resources
           (capture-probe-encoder-device encoder)))
    commands))

(defmethod luv:submit
    ((queue capture-probe-queue) (commands capture-probe-command-buffer))
  (declare (ignore queue))
  (dolist (command (capture-probe-command-buffer-commands commands))
    (when (typep command 'luv::gpu-copy-texture-to-buffer-command)
      (replace
       (capture-probe-buffer-data
        (luv::gpu-copy-texture-to-buffer-command-destination command))
       (capture-probe-texture-pixels
        (luv::gpu-copy-texture-to-buffer-command-source command)))))
  commands)

(defmethod luv:read-buffer ((buffer capture-probe-buffer) &key offset size)
  (let* ((offset (or offset 0))
         (size (or size (- (length (capture-probe-buffer-data buffer))
                           offset))))
    (subseq (capture-probe-buffer-data buffer) offset (+ offset size))))

(defun capture-probe-resource-kind (resource)
  (typecase resource
    (capture-probe-texture :target)
    (capture-probe-buffer :readback)
    (capture-probe-encoder :encoder)
    (capture-probe-command-buffer :commands)))

(defmethod luv:destroy ((resource capture-probe-resource))
  (let ((kind (capture-probe-resource-kind resource)))
    (when (boundp '*capture-probe-destroy-events*)
      (setf *capture-probe-destroy-events*
            (append *capture-probe-destroy-events* (list kind))))
    (when (member kind *capture-probe-destroy-failures*)
      (error "Injected capture probe ~A destruction failure." kind)))
  (setf (capture-probe-destroyed-p resource) t)
  (values))

(defclass capture-probe-context (luv:canvas-context)
  ((device :initarg :device :reader capture-probe-context-device)
   (extent :initarg :extent :accessor capture-probe-context-extent)
   (format :initarg :format :accessor capture-probe-context-format)))

(defmethod luv:context-device ((context capture-probe-context))
  (capture-probe-context-device context))

(defmethod luv:canvas-extent ((context capture-probe-context))
  (capture-probe-context-extent context))

(defmethod luv:canvas-format ((context capture-probe-context))
  (capture-probe-context-format context))

(defclass capture-probe-canvas (luv:canvas)
  ((context :initarg :context :reader capture-probe-canvas-context)
   (state :initarg :state :initform :open :accessor capture-probe-canvas-state)
   (native-thread
    :initarg :native-thread :initform nil
    :reader capture-probe-canvas-native-thread)))

(defmethod luv:canvas-context ((canvas capture-probe-canvas))
  (capture-probe-canvas-context canvas))

(defmethod luv:canvas-state ((canvas capture-probe-canvas))
  (capture-probe-canvas-state canvas))

(defmethod luv:canvas-thread-p ((canvas capture-probe-canvas))
  (eq sb-thread:*current-thread*
      (capture-probe-canvas-native-thread canvas)))

(defmethod luv:request-canvas-frame ((canvas capture-probe-canvas) function)
  (declare (ignore canvas))
  (funcall function 0.0d0))

(defclass capture-probe-application ()
  ((canvas :initarg :canvas :reader capture-probe-application-canvas)
   (events :initform nil :accessor capture-probe-application-events)
   (fail-encode-p :initarg :fail-encode-p :initform nil
                  :reader capture-probe-fail-encode-p)
   (fail-cleanup-p :initarg :fail-cleanup-p :initform nil
                   :reader capture-probe-fail-cleanup-p)
   (cleanup-saw-live-target-p :initform nil
                              :accessor capture-probe-cleanup-saw-live-target-p)))

(defclass blocking-capture-probe-application (capture-probe-application)
  ((prepare-entered
    :initform (sb-thread:make-semaphore :name "capture prepare entered")
    :reader blocking-capture-prepare-entered)
   (prepare-release
    :initform (sb-thread:make-semaphore :name "capture prepare release")
    :reader blocking-capture-prepare-release)))

(defun note-capture-probe-event (application event)
  (setf (capture-probe-application-events application)
        (append (capture-probe-application-events application) (list event))))

(defmethod luv:capture-canvas ((application capture-probe-application))
  (capture-probe-application-canvas application))

(defmethod luv:prepare-capture
    ((application capture-probe-application)
     (capture luv:application-capture))
  (note-capture-probe-event application :prepare)
  (setf (luv:capture-client-state capture) :prepared))

(defmethod luv:prepare-capture :after
    ((application blocking-capture-probe-application)
     (capture luv:application-capture))
  (declare (ignore capture))
  (sb-thread:signal-semaphore (blocking-capture-prepare-entered application))
  (unless (sb-thread:wait-on-semaphore
           (blocking-capture-prepare-release application) :timeout 1.0)
    (error "Blocking capture preparation timed out.")))

(defmethod luv:advance-capture-frame
    ((application capture-probe-application)
     (capture luv:application-capture) frame-index)
  (declare (ignore capture))
  (note-capture-probe-event application (list :advance frame-index)))

(defmethod luv:encode-capture-frame
    ((application capture-probe-application)
     (capture luv:application-capture) encoder target extent)
  (declare (ignore encoder extent))
  (let ((frame (luv:capture-frame-index capture)))
    (note-capture-probe-event application (list :encode frame))
    (when (capture-probe-fail-encode-p application)
      (error "Deliberate capture probe encoding failure."))
    (let ((value (+ (luv:capture-option capture :pixel-value 20) frame))
          (pixels (capture-probe-texture-pixels target)))
      (loop for offset below (length pixels) by 4
            do (setf (aref pixels offset) value
                     (aref pixels (+ offset 1)) value
                     (aref pixels (+ offset 2)) value
                     (aref pixels (+ offset 3)) 255)))
    (luv:capture-option capture :metadata)))

(defmethod luv:cleanup-capture
    ((application capture-probe-application)
     (capture luv:application-capture))
  (let ((target (luv:capture-target capture)))
    (setf (capture-probe-cleanup-saw-live-target-p application)
          (and target (not (capture-probe-destroyed-p target)))))
  (note-capture-probe-event application :cleanup)
  (when (capture-probe-fail-cleanup-p application)
    (error "Injected capture probe application cleanup failure.")))

(defun make-capture-probe-application
    (&key fail-encode-p fail-cleanup-p native-thread)
  (let* ((device (make-instance 'capture-probe-device))
         (context (make-instance 'capture-probe-context
                                 :device device :extent '(2 1)
                                 :format :rgba8-unorm))
         (canvas (make-instance 'capture-probe-canvas
                                :context context
                                :native-thread native-thread)))
    (values (make-instance 'capture-probe-application
                           :canvas canvas
                           :fail-encode-p fail-encode-p
                           :fail-cleanup-p fail-cleanup-p)
            device context)))

(defun make-blocking-capture-probe-application (&key native-thread)
  (multiple-value-bind (application device context)
      (make-capture-probe-application :native-thread native-thread)
    (values
     (make-instance 'blocking-capture-probe-application
                    :canvas (capture-probe-application-canvas application))
     device context)))

(deftest application-capture-shutdown-closes-admission-terminally
  (multiple-value-bind (application device)
      (make-capture-probe-application)
    (let* ((requested
             (make-instance 'luv:application-capture
                            :application application :kind :screenshot
                            :label "late screenshot"))
           (published-p nil)
           (condition nil))
      (ok (luv:request-application-capture-shutdown application))
      (ok (luv:application-capture-shutdown-p application))
      (ok (not (luv:request-application-capture-shutdown application)))
      (handler-case
          (luv::call-with-capture-target
           (lambda (capture)
             (declare (ignore capture))
             :wrong)
           requested)
        (luv:application-capture-shutting-down (failure)
          (setf condition failure)))
      (ok condition)
      (ok (eq application
              (luv:application-capture-shutting-down-application condition)))
      (ok (eq requested
              (luv:application-capture-shutting-down-requested-capture
               condition)))
      (ok (not (luv:call-if-application-captures-open
                application (lambda () (setf published-p t)))))
      (ok (not published-p))
      (ok (null (capture-probe-application-events application)))
      (ok (null (capture-probe-device-resources device))))))

(deftest capture-shutdown-waits-off-canvas-and-rejects-late-captures
  (multiple-value-bind (application device)
      (make-blocking-capture-probe-application)
    (let* ((active
             (make-instance 'luv:application-capture
                            :application application :kind :film
                            :label "draining film"))
           (late
             (make-instance 'luv:application-capture
                            :application application :kind :screenshot
                            :label "late screenshot"))
           (active-condition nil)
           (drain-condition nil)
           (drained
             (sb-thread:make-semaphore :name "capture shutdown drained"))
           (active-thread
             (sb-thread:make-thread
              (lambda ()
                (handler-case
                    (luv::call-with-capture-target
                     (lambda (capture)
                       (declare (ignore capture))
                       :finished)
                     active)
                  (error (condition)
                    (setf active-condition condition))))
              :name "capture active during shutdown test")))
      (ok (sb-thread:wait-on-semaphore
           (blocking-capture-prepare-entered application) :timeout 1.0))
      (ok (luv:request-application-capture-shutdown application))
      (ok (signals
           (luv::call-with-capture-target
            (lambda (capture) (declare (ignore capture)) :wrong)
            late)
           'luv:application-capture-shutting-down))
      (let ((drainer
              (sb-thread:make-thread
               (lambda ()
                 (handler-case
                     (luv:quiesce-application-captures application)
                   (error (condition)
                     (setf drain-condition condition)))
                 (sb-thread:signal-semaphore drained))
               :name "application capture shutdown test")))
        ;; The stop owner has closed admission but cannot pass the active
        ;; capture until its preparation and cleanup have both left.
        (ok (not (sb-thread:wait-on-semaphore drained :timeout 0.05)))
        (sb-thread:signal-semaphore
         (blocking-capture-prepare-release application))
        (sb-thread:join-thread active-thread)
        (ok (sb-thread:wait-on-semaphore drained :timeout 1.0))
        (sb-thread:join-thread drainer))
      (ok (null active-condition))
      (ok (null drain-condition))
      (ok (equal '(:prepare :cleanup)
                 (capture-probe-application-events application)))
      (ok (every #'capture-probe-destroyed-p
                 (capture-probe-device-resources device))))))

(deftest canvas-thread-cannot-wait-for-an-active-capture
  (multiple-value-bind (application device)
      (make-blocking-capture-probe-application
       :native-thread sb-thread:*current-thread*)
    (declare (ignore device))
    (let* ((active
             (make-instance 'luv:application-capture
                            :application application :kind :film
                            :label "canvas-blocking film"))
           (active-condition nil)
           (thread
             (sb-thread:make-thread
              (lambda ()
                (handler-case
                    (luv::call-with-capture-target
                     (lambda (capture)
                       (declare (ignore capture))
                       :finished)
                     active)
                  (error (condition)
                    (setf active-condition condition))))
              :name "canvas-blocking capture test")))
      (ok (sb-thread:wait-on-semaphore
           (blocking-capture-prepare-entered application) :timeout 1.0))
      (let ((condition
              (handler-case
                  (progn
                    (luv:quiesce-application-captures application)
                    nil)
                (luv:application-capture-shutdown-blocking-thread-error
                    (failure)
                  failure))))
        (ok condition)
        (ok (eq application
                (luv:application-capture-shutdown-blocking-application
                 condition)))
        (ok (eq active
                (luv:application-capture-shutdown-blocking-active-capture
                 condition))))
      (sb-thread:signal-semaphore
       (blocking-capture-prepare-release application))
      (sb-thread:join-thread thread)
      (ok (null active-condition))
      ;; Once the owner has left, quiescence is immediate even on this thread.
      (ok (null (multiple-value-list
                 (luv:quiesce-application-captures application)))))))

(deftest overlapping-application-captures-fail-before-preparation
  (multiple-value-bind (application device)
      (make-blocking-capture-probe-application)
    (let* ((active
             (make-instance 'luv:application-capture
                            :application application :kind :film
                            :label "active film"))
           (requested
             (make-instance 'luv:application-capture
                            :application application :kind :screenshot
                            :label "overlapping screenshot"))
           (active-result nil)
           (active-condition nil)
           (thread
             (sb-thread:make-thread
              (lambda ()
                (handler-case
                    (setf active-result
                          (luv::call-with-capture-target
                           (lambda (capture)
                             (declare (ignore capture))
                             :finished)
                           active))
                  (error (condition)
                    (setf active-condition condition))))
              :name "active application capture test")))
      (ok (sb-thread:wait-on-semaphore
           (blocking-capture-prepare-entered application) :timeout 1.0))
      (let ((condition
              (handler-case
                  (progn
                    (luv::call-with-capture-target
                     (lambda (capture)
                       (declare (ignore capture))
                       :overlapped)
                     requested)
                    nil)
                (luv:application-capture-busy (condition) condition))))
        (ok condition)
        (ok (eq application
                (luv:application-capture-busy-application condition)))
        (ok (eq active
                (luv:application-capture-busy-active-capture condition)))
        (ok (eq requested
                (luv:application-capture-busy-requested-capture condition))))
      ;; The rejected capture performed no preparation and allocated no GPU
      ;; resource.  Let the exact active owner finish and release its claim.
      (ok (equal '(:prepare)
                 (capture-probe-application-events application)))
      (ok (null (capture-probe-device-resources device)))
      (sb-thread:signal-semaphore
       (blocking-capture-prepare-release application))
      (sb-thread:join-thread thread)
      (ok (null active-condition))
      (ok (eq :finished active-result))
      (ok (equal '(:prepare :cleanup)
                 (capture-probe-application-events application)))
      (ok (every #'capture-probe-destroyed-p
                 (capture-probe-device-resources device))))))

(deftest failed-capture-body-does-not-strand-the-application-reservation
  (multiple-value-bind (application device)
      (make-capture-probe-application)
    (let ((first
            (make-instance 'luv:application-capture
                           :application application :kind :screenshot))
          (second
            (make-instance 'luv:application-capture
                           :application application :kind :screenshot)))
      (ok (signals
           (luv::call-with-capture-target
            (lambda (capture)
              (declare (ignore capture))
              (error "Injected reserved capture body failure."))
            first)
           'error))
      (ok (eq :recovered
              (luv::call-with-capture-target
               (lambda (capture)
                 (declare (ignore capture))
                 :recovered)
               second)))
      (ok (equal '(:prepare :cleanup :prepare :cleanup)
                 (capture-probe-application-events application)))
      (ok (every #'capture-probe-destroyed-p
                 (capture-probe-device-resources device))))))

(deftest application-screenshot-is-one-owned-offscreen-transaction
  (multiple-value-bind (application device)
      (make-capture-probe-application)
    (let* ((directory
             (uiop:ensure-directory-pathname
              (merge-pathnames
               (format nil "luv-application-capture-test-~36R/"
                       (random most-positive-fixnum))
               (uiop:temporary-directory))))
           (pathname (merge-pathnames "frame.png" directory)))
      (unwind-protect
           (multiple-value-bind
                 (written pixels width height format metadata)
               (luv:capture-application-screenshot
                application pathname
                :options '(:pixel-value 37 :metadata :frame-metadata)
                :label "capture probe screenshot")
             (ok (equal pathname written))
             (ok (probe-file pathname))
             (ok (= 2 width))
             (ok (= 1 height))
             (ok (eq :rgba8-unorm format))
             (ok (eq :frame-metadata metadata))
             (ok (equalp #(37 37 37 255 37 37 37 255) pixels))
             (ok (equal '(:prepare (:encode 0) :cleanup)
                        (capture-probe-application-events application)))
             (ok (capture-probe-cleanup-saw-live-target-p application))
             (ok (every #'capture-probe-destroyed-p
                        (capture-probe-device-resources device))))
        (uiop:delete-directory-tree directory :validate t
                                              :if-does-not-exist :ignore)))))

(deftest failed-frame-encoding-still-cleans-up-application-and-gpu-resources
  (multiple-value-bind (application device)
      (make-capture-probe-application :fail-encode-p t)
    (let* ((capture
             (make-instance 'luv:application-capture
                            :application application :kind :screenshot))
           (condition
             (handler-case
                 (progn
                   (luv::call-with-capture-target
                    #'luv::render-capture-frame capture)
                   nil)
               (error (error) error))))
      (ok condition)
      (ok (search "Deliberate capture probe" (princ-to-string condition)))
      (ok (equal '(:prepare (:encode 0) :cleanup)
                 (capture-probe-application-events application)))
      (ok (capture-probe-cleanup-saw-live-target-p application))
      (ok (every #'capture-probe-destroyed-p
                 (capture-probe-device-resources device))))))

(deftest capture-cleanup-failures-cannot-mask-the-frame-failure
  (multiple-value-bind (application device)
      (make-capture-probe-application
       :fail-encode-p t :fail-cleanup-p t)
    (declare (ignore device))
    (let* ((*capture-probe-destroy-events* nil)
           (*capture-probe-destroy-failures* '(:encoder :readback :target))
           (warnings nil)
           (capture
             (make-instance 'luv:application-capture
                            :application application :kind :screenshot))
           (condition
             (handler-bind
                 ((luv:release-warning
                    (lambda (warning)
                      (setf warnings (append warnings (list warning)))
                      (muffle-warning warning))))
               (handler-case
                   (progn
                     (luv::call-with-capture-target
                      #'luv::render-capture-frame capture)
                     nil)
                 (error (error) error)))))
      (ok condition)
      (ok (search "Deliberate capture probe encoding failure"
                  (princ-to-string condition)))
      (ok (equal '(:prepare (:encode 0) :cleanup)
                 (capture-probe-application-events application)))
      (ok (equal '(:encoder :readback :target)
                 *capture-probe-destroy-events*))
      (ok (equal '(:capture-frame-encoder
                   :capture-application-cleanup
                   :capture-readback-buffer
                   :capture-target)
                 (loop for warning in warnings
                       append
                       (mapcar #'luv:release-failure-name
                               (luv:release-warning-failures warning))))))))

(deftest film-frames-preserve-hook-order-and-use-absolute-deadlines
  (multiple-value-bind (application device)
      (make-capture-probe-application)
    (declare (ignore device))
    (let* ((capture
             (make-instance 'luv:application-capture
                            :application application :kind :film
                            :options '(:pixel-value 40)))
           (clock-values '(10.0d0 10.1d0 10.7d0 11.6d0))
           (waits nil))
      (luv::call-with-capture-target
       (lambda (capture)
         (luv::call-with-capture-film-frames
          (lambda (pixels)
            (note-capture-probe-event
             application
             (list :write (luv:capture-frame-index capture)
                   (aref pixels 0))))
          capture 3 2
          :before-frame
          (lambda (frame)
            (note-capture-probe-event application (list :before frame)))
          :progress-function
          (lambda (frame count)
            (note-capture-probe-event application
                                      (list :progress frame count)))
          :clock-function (lambda () (pop clock-values))
          :sleep-function (lambda (seconds) (push seconds waits))))
       capture)
      (ok (equal
           '(:prepare
             (:before 0) (:advance 0) (:encode 0) (:write 0 40)
             (:progress 0 3)
             (:before 1) (:advance 1) (:encode 1) (:write 1 41)
             (:progress 1 3)
             (:before 2) (:advance 2) (:encode 2) (:write 2 42)
             (:progress 2 3)
             :cleanup)
           (capture-probe-application-events application)))
      (ok (= 2 (length waits)))
      (ok (< (abs (- 0.4d0 (second waits))) 1.0d-9))
      (ok (< (abs (- 0.3d0 (first waits))) 1.0d-9)))))

(deftest shutdown-cooperatively-shortens-an-active-film
  (multiple-value-bind (application device)
      (make-capture-probe-application)
    (declare (ignore device))
    (let ((capture
            (make-instance 'luv:application-capture
                           :application application :kind :film))
          (clock-values '(10.0d0 10.1d0))
          (waits nil))
      (luv::call-with-capture-target
       (lambda (capture)
         (luv::call-with-capture-film-frames
          (lambda (pixels)
            (declare (ignore pixels))
            (note-capture-probe-event application :write)
            (luv:request-application-capture-shutdown application))
          capture 20 2
          :clock-function (lambda () (pop clock-values))
          :sleep-function (lambda (seconds) (push seconds waits))))
       capture)
      (ok (equal '(:prepare (:advance 0) (:encode 0) :write :cleanup)
                 (capture-probe-application-events application)))
      (ok (null waits))
      (ok (null clock-values)))))

(deftest capture-rejects-an-extent-change-before-encoding
  (multiple-value-bind (application device context)
      (make-capture-probe-application)
    (let ((capture
            (make-instance 'luv:application-capture
                           :application application :kind :screenshot)))
      (ok
       (signals
        (luv::call-with-capture-target
         (lambda (capture)
           (setf (capture-probe-context-extent context) '(3 1))
           (luv::render-capture-frame capture))
         capture)
        'error))
      (ok (equal '(:prepare :cleanup)
                 (capture-probe-application-events application)))
      (ok (capture-probe-cleanup-saw-live-target-p application))
      (ok (every #'capture-probe-destroyed-p
                 (capture-probe-device-resources device))))))

(deftest capture-rejects-a-format-change-before-encoding
  (multiple-value-bind (application device context)
      (make-capture-probe-application)
    (let ((capture
            (make-instance 'luv:application-capture
                           :application application :kind :screenshot)))
      (ok
       (signals
        (luv::call-with-capture-target
         (lambda (capture)
           (setf (capture-probe-context-format context) :bgra8-unorm)
           (luv::render-capture-frame capture))
         capture)
        'error))
      (ok (equal '(:prepare :cleanup)
                 (capture-probe-application-events application)))
      (ok (capture-probe-cleanup-saw-live-target-p application))
      (ok (every #'capture-probe-destroyed-p
                 (capture-probe-device-resources device))))))
