;;; Vulkan presentation contexts and their SDL bridge.

(in-package #:luv)

(defmethod sdl-presentation-api-for ((provider vulkan-gpu-provider))
  (declare (ignore provider))
  :vulkan)

(defclass vulkan-canvas-context (canvas-context)
  ((canvas
    :initarg :canvas
    :reader context-canvas)
   (provider
    :initarg :provider
    :reader vulkan-canvas-provider)
   (instance
    :initarg :instance
    :initform nil
    :accessor vulkan-canvas-instance)
   (physical-device
    :initarg :physical-device
    :initform nil
    :accessor vulkan-canvas-physical-device)
   (device
    :initarg :device
    :initform nil
    :accessor canvas-device)
   (surface
    :initarg :surface
    :initform nil
    :accessor vulkan-canvas-surface)
   (configuration
    :initform nil
    :accessor canvas-context-configuration)
   (swapchain
    :initform nil
    :accessor vulkan-canvas-swapchain)
   (textures
    :initform #()
    :accessor vulkan-canvas-textures)
   (extent
    :initform nil
    :accessor canvas-extent)
   (window-size
    :initform nil
    :accessor vulkan-canvas-window-size
    :documentation
    "The window's pixel size when this swapchain was built.

Compared against the window rather than against EXTENT, which comes from
the surface's own idea of its current extent: a platform where the two
disagree would otherwise ask for a rebuild on every single frame.")
   (format
    :initform nil
    :accessor canvas-format)
   (render-done
    :initform #()
    :accessor vulkan-canvas-render-done)
   (frame-slots
    :initform #()
    :accessor vulkan-canvas-frame-slots)
   (next-frame-slot
    :initform 0
    :accessor vulkan-canvas-next-frame-slot)
   (presentation-timeline
    :initform nil
    :accessor vulkan-canvas-presentation-timeline)
   (current-texture
    :initform nil
    :accessor vulkan-canvas-current-texture)
   (state
    :initform :unconfigured
    :accessor canvas-context-state)))

(defconstant +vulkan-presentation-timing-capacity+ 4096)
(defconstant +vulkan-presentation-timing-queue-size+ 256)

(defclass vulkan-presentation-timeline ()
  ((status :initform :warming :accessor vulkan-presentation-timeline-status)
   (reason :initform nil :accessor vulkan-presentation-timeline-reason)
   (stage :initarg :stage :reader vulkan-presentation-timeline-stage)
   (absolute-time-p
    :initarg :absolute-time-p :initform nil
    :reader vulkan-presentation-timeline-absolute-time-p)
   (time-domain :initform nil :accessor vulkan-presentation-timeline-time-domain)
   (time-domain-id :initform 0
                   :accessor vulkan-presentation-timeline-time-domain-id)
   (refresh-duration :initform 0
                     :accessor vulkan-presentation-timeline-refresh-duration)
   (refresh-interval :initform 0
                     :accessor vulkan-presentation-timeline-refresh-interval)
   (next-present-id :initform 1
                    :accessor vulkan-presentation-timeline-next-present-id)
   (latest-result-id :initform 0
                     :accessor vulkan-presentation-timeline-latest-result-id)
   (latest-result-nanoseconds
    :initform 0
    :accessor vulkan-presentation-timeline-latest-result-nanoseconds)
   (count :initform 0 :accessor vulkan-presentation-timeline-count)
   (dropped-count :initform 0
                  :accessor vulkan-presentation-timeline-dropped-count)
   (present-ids
    :initform (make-array +vulkan-presentation-timing-capacity+
                          :element-type '(unsigned-byte 64)
                          :initial-element 0)
    :reader vulkan-presentation-timeline-present-ids)
   (predicted-seconds
    :initform (make-array +vulkan-presentation-timing-capacity+
                          :element-type 'double-float :initial-element 0d0)
    :reader vulkan-presentation-timeline-predicted-seconds)
   (submitted-seconds
    :initform (make-array +vulkan-presentation-timing-capacity+
                          :element-type 'double-float :initial-element 0d0)
    :reader vulkan-presentation-timeline-submitted-seconds)
   (target-nanoseconds
    :initform (make-array +vulkan-presentation-timing-capacity+
                          :element-type '(unsigned-byte 64)
                          :initial-element 0)
    :reader vulkan-presentation-timeline-target-nanoseconds)
   (actual-nanoseconds
    :initform (make-array +vulkan-presentation-timing-capacity+
                          :element-type '(unsigned-byte 64)
                          :initial-element 0)
    :reader vulkan-presentation-timeline-actual-nanoseconds)
   (actual-time-domains
    :initform (make-array +vulkan-presentation-timing-capacity+
                          :initial-element nil)
    :reader vulkan-presentation-timeline-actual-time-domains)
   (actual-time-domain-ids
    :initform (make-array +vulkan-presentation-timing-capacity+
                          :element-type '(unsigned-byte 64)
                          :initial-element 0)
    :reader vulkan-presentation-timeline-actual-time-domain-ids))
  (:documentation
   "One swapchain's bounded, presentation-ID-indexed display observations."))

(defstruct presentation-timing-observation
  (present-id 0 :type (unsigned-byte 64))
  (predicted-seconds 0d0 :type double-float)
  (submitted-seconds 0d0 :type double-float)
  (target-nanoseconds 0 :type (unsigned-byte 64))
  (actual-nanoseconds 0 :type (unsigned-byte 64))
  actual-time-domain
  (actual-time-domain-id 0 :type (unsigned-byte 64)))

(defstruct presentation-timing-snapshot
  status reason stage time-domain absolute-time-p
  (time-domain-id 0 :type (unsigned-byte 64))
  (refresh-duration 0 :type (unsigned-byte 64))
  (refresh-interval 0 :type (unsigned-byte 64))
  (dropped-count 0 :type (unsigned-byte 64))
  (observations #() :type vector))

(defun vulkan-presentation-timeline-index (present-id)
  (mod (1- present-id) +vulkan-presentation-timing-capacity+))

(defun note-vulkan-presentation-submission
    (timeline present-id predicted-seconds submitted-seconds
     &optional (target-nanoseconds 0))
  (let ((index (vulkan-presentation-timeline-index present-id)))
    (setf (aref (vulkan-presentation-timeline-present-ids timeline) index)
          present-id
          (aref (vulkan-presentation-timeline-predicted-seconds timeline) index)
          (float predicted-seconds 1d0)
          (aref (vulkan-presentation-timeline-submitted-seconds timeline) index)
          (float submitted-seconds 1d0)
          (aref (vulkan-presentation-timeline-target-nanoseconds timeline) index)
          target-nanoseconds
          (aref (vulkan-presentation-timeline-actual-nanoseconds timeline) index)
          0
          (aref (vulkan-presentation-timeline-actual-time-domains timeline) index)
          nil
          (aref (vulkan-presentation-timeline-actual-time-domain-ids timeline)
                index)
          0
          (vulkan-presentation-timeline-count timeline)
          (min +vulkan-presentation-timing-capacity+
               (1+ (vulkan-presentation-timeline-count timeline)))
          (vulkan-presentation-timeline-next-present-id timeline)
          (1+ present-id)))
  present-id)

(defun note-vulkan-presentation-result
    (timeline present-id actual-nanoseconds time-domain time-domain-id)
  (let ((index (vulkan-presentation-timeline-index present-id)))
    (when (= present-id
             (aref (vulkan-presentation-timeline-present-ids timeline) index))
      (setf (aref (vulkan-presentation-timeline-actual-nanoseconds timeline)
                  index)
            actual-nanoseconds
            (aref (vulkan-presentation-timeline-actual-time-domains timeline)
                  index)
            time-domain
            (aref
             (vulkan-presentation-timeline-actual-time-domain-ids timeline)
             index)
            time-domain-id)
      (when (and (> present-id
                    (vulkan-presentation-timeline-latest-result-id timeline))
                 (eq time-domain
                     (vulkan-presentation-timeline-time-domain timeline))
                 (= time-domain-id
                    (vulkan-presentation-timeline-time-domain-id timeline)))
        (setf (vulkan-presentation-timeline-latest-result-id timeline) present-id
              (vulkan-presentation-timeline-latest-result-nanoseconds timeline)
              actual-nanoseconds))
      t)))

(defun predict-vulkan-presentation-target (timeline fallback-time)
  "Return FALLBACK-TIME and the next native display beat when available.

The selected Vulkan time domain may be opaque and unrelated to Lisp's
monotonic clock.  Keep animation prediction in the host domain and advance
native targets only from observed or already-scheduled native timestamps."
  (let* ((base-id (vulkan-presentation-timeline-latest-result-id timeline))
         (duration (vulkan-presentation-timeline-refresh-duration timeline))
         (previous-id
           (1- (vulkan-presentation-timeline-next-present-id timeline)))
         (previous-index
           (and (plusp previous-id)
                (vulkan-presentation-timeline-index previous-id)))
         (previous-target
           (and previous-index
                (= previous-id
                   (aref (vulkan-presentation-timeline-present-ids timeline)
                         previous-index))
                (aref (vulkan-presentation-timeline-target-nanoseconds timeline)
                      previous-index))))
    (if (and (eq :recording
                 (vulkan-presentation-timeline-status timeline))
             (vulkan-presentation-timeline-absolute-time-p timeline)
             (plusp duration)
             (or (plusp base-id)
                 (and previous-target (plusp previous-target))))
        ;; Feedback seeds the native clock.  After that, requested targets own
        ;; cadence: a late result is evidence of a missed beat, not a new phase
        ;; from which to delay the following frame.
        (let ((native-base
                (if (and previous-target (plusp previous-target))
                    previous-target
                    (vulkan-presentation-timeline-latest-result-nanoseconds
                     timeline))))
          (values fallback-time (+ native-base duration)))
        (values fallback-time nil))))

(defun snapshot-vulkan-presentation-timeline (timeline)
  (let* ((count (vulkan-presentation-timeline-count timeline))
         (next (vulkan-presentation-timeline-next-present-id timeline))
         (first-id (- next count))
         (observations (make-array count)))
    (dotimes (offset count)
      (let* ((present-id (+ first-id offset))
             (index (vulkan-presentation-timeline-index present-id)))
        (setf (aref observations offset)
              (make-presentation-timing-observation
               :present-id present-id
               :predicted-seconds
               (aref (vulkan-presentation-timeline-predicted-seconds timeline)
                     index)
               :submitted-seconds
               (aref (vulkan-presentation-timeline-submitted-seconds timeline)
                     index)
               :target-nanoseconds
               (aref (vulkan-presentation-timeline-target-nanoseconds timeline)
                     index)
               :actual-nanoseconds
               (aref (vulkan-presentation-timeline-actual-nanoseconds timeline)
                     index)
               :actual-time-domain
               (aref (vulkan-presentation-timeline-actual-time-domains timeline)
                     index)
               :actual-time-domain-id
               (aref
                (vulkan-presentation-timeline-actual-time-domain-ids timeline)
                index)))))
    (make-presentation-timing-snapshot
     :status (vulkan-presentation-timeline-status timeline)
     :reason (vulkan-presentation-timeline-reason timeline)
     :stage (vulkan-presentation-timeline-stage timeline)
     :absolute-time-p
     (vulkan-presentation-timeline-absolute-time-p timeline)
     :time-domain (vulkan-presentation-timeline-time-domain timeline)
     :time-domain-id (vulkan-presentation-timeline-time-domain-id timeline)
     :refresh-duration (vulkan-presentation-timeline-refresh-duration timeline)
     :refresh-interval (vulkan-presentation-timeline-refresh-interval timeline)
     :dropped-count (vulkan-presentation-timeline-dropped-count timeline)
     :observations observations)))

(defun vulkan-canvas-presentation-timing-snapshot (context)
  "Return a coherent copy of CONTEXT's bounded presentation observations."
  (check-type context vulkan-canvas-context)
  (call-on-sdl-canvas-thread
   (context-canvas context)
   (lambda ()
     (let ((timeline (vulkan-canvas-presentation-timeline context)))
       (and timeline (snapshot-vulkan-presentation-timeline timeline))))))

(defun completed-presentation-timing-observations (snapshot)
  (remove-if
   (lambda (observation)
     (zerop (presentation-timing-observation-actual-nanoseconds observation)))
   (coerce (presentation-timing-snapshot-observations snapshot) 'list)))

(defun presentation-timing-interval-milliseconds (observations)
  (loop for previous = nil then observation
        for observation in observations
        when (and previous
                  (= (1+ (presentation-timing-observation-present-id previous))
                     (presentation-timing-observation-present-id observation))
                  (eq (presentation-timing-observation-actual-time-domain previous)
                      (presentation-timing-observation-actual-time-domain observation))
                  (= (presentation-timing-observation-actual-time-domain-id previous)
                     (presentation-timing-observation-actual-time-domain-id observation)))
          collect
          (/ (- (presentation-timing-observation-actual-nanoseconds observation)
                (presentation-timing-observation-actual-nanoseconds previous))
             1d6)))

(defun presentation-timing-phase-errors-milliseconds (observations)
  (when observations
    (let ((origin (first observations)))
      (loop for observation in observations
            when (and
                  (eq (presentation-timing-observation-actual-time-domain origin)
                      (presentation-timing-observation-actual-time-domain observation))
                  (= (presentation-timing-observation-actual-time-domain-id origin)
                     (presentation-timing-observation-actual-time-domain-id observation)))
              collect
              (* 1d3
                 (- (/ (- (presentation-timing-observation-actual-nanoseconds
                           observation)
                          (presentation-timing-observation-actual-nanoseconds
                           origin))
                       1d9)
                    (- (presentation-timing-observation-predicted-seconds observation)
                       (presentation-timing-observation-predicted-seconds
                        origin))))))))

(defun presentation-timing-summary (values)
  (when values
    (let* ((count (length values))
           (sorted (sort (copy-seq values) #'<))
           (mean (/ (reduce #'+ values) count))
           (variance
             (/ (reduce #'+ values
                        :key (lambda (value) (expt (- value mean) 2)))
                count)))
      (list :count count
            :minimum (first sorted)
            :mean mean
            :p50 (elt sorted (floor (* 0.50d0 (1- count))))
            :p95 (elt sorted (floor (* 0.95d0 (1- count))))
            :maximum (car (last sorted))
            :standard-deviation (sqrt variance)))))

(defun print-vulkan-canvas-presentation-timing
    (context &optional (stream *standard-output*))
  "Print display cadence and prediction drift observed for CONTEXT."
  (let ((snapshot (vulkan-canvas-presentation-timing-snapshot context)))
    (unless snapshot
      (format stream "No Vulkan presentation timeline exists.~%")
      (return-from print-vulkan-canvas-presentation-timing nil))
    (let* ((observations
             (completed-presentation-timing-observations snapshot))
           (intervals
             (presentation-timing-interval-milliseconds observations))
           (phase-errors
             (presentation-timing-phase-errors-milliseconds observations))
           (interval-summary (presentation-timing-summary intervals))
           (phase-summary (presentation-timing-summary phase-errors))
           (duration (presentation-timing-snapshot-refresh-duration snapshot)))
      (format stream "Vulkan presentation timing: ~A~@[ (~A)~]~%"
              (presentation-timing-snapshot-status snapshot)
              (presentation-timing-snapshot-reason snapshot))
      (format stream "  stage/domain: ~A / ~A [~D]~%"
              (presentation-timing-snapshot-stage snapshot)
              (presentation-timing-snapshot-time-domain snapshot)
              (presentation-timing-snapshot-time-domain-id snapshot))
      (format stream "  absolute display scheduling: ~:[unavailable~;enabled~]~%"
              (presentation-timing-snapshot-absolute-time-p snapshot))
      (when (plusp duration)
        (format stream "  display: ~,6F ms (~,3F Hz), interval granularity ~,6F ms~%"
                (/ duration 1d6) (/ 1d9 duration)
                (/ (presentation-timing-snapshot-refresh-interval snapshot)
                   1d6)))
      (format stream "  frames: ~D submitted, ~D timed, ~D timing drops~%"
              (length (presentation-timing-snapshot-observations snapshot))
              (length observations)
              (presentation-timing-snapshot-dropped-count snapshot))
      (when interval-summary
        (format stream
                "  actual interval ms: mean ~,6F  sd ~,6F  min/p50/p95/max ~,6F / ~,6F / ~,6F / ~,6F~%"
                (getf interval-summary :mean)
                (getf interval-summary :standard-deviation)
                (getf interval-summary :minimum)
                (getf interval-summary :p50)
                (getf interval-summary :p95)
                (getf interval-summary :maximum)))
      (when phase-summary
        (format stream
                "  prediction drift ms: rms ~,6F  min/max ~,6F / ~,6F~%"
                (sqrt (/ (reduce #'+ phase-errors :key (lambda (x) (* x x)))
                         (length phase-errors)))
                (getf phase-summary :minimum)
                (getf phase-summary :maximum)))
      snapshot)))

(defun write-vulkan-canvas-presentation-timing-csv (context pathname)
  "Write CONTEXT's raw presentation observations to PATHNAME."
  (let* ((snapshot (vulkan-canvas-presentation-timing-snapshot context))
         (observations
           (and snapshot
                (coerce (presentation-timing-snapshot-observations snapshot)
                        'list)))
         (completed
           (and snapshot
                (completed-presentation-timing-observations snapshot)))
         (origin (first completed)))
    (unless snapshot
      (error "No Vulkan presentation timeline exists for ~S." context))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
      (format stream
              "present_id,predicted_seconds,submitted_seconds,target_nanoseconds,actual_nanoseconds,time_domain,time_domain_id,actual_interval_ms,prediction_drift_ms,target_error_ms~%")
      (loop with previous = nil
            for observation in observations
            for actual =
              (presentation-timing-observation-actual-nanoseconds observation)
            for target =
              (presentation-timing-observation-target-nanoseconds observation)
            for comparable-p =
              (and origin (plusp actual)
                   (eq (presentation-timing-observation-actual-time-domain origin)
                       (presentation-timing-observation-actual-time-domain
                        observation))
                   (= (presentation-timing-observation-actual-time-domain-id origin)
                      (presentation-timing-observation-actual-time-domain-id
                       observation)))
            for interval =
              (and previous (plusp actual)
                   (plusp
                    (presentation-timing-observation-actual-nanoseconds previous))
                   (= (1+ (presentation-timing-observation-present-id previous))
                      (presentation-timing-observation-present-id observation))
                   (/ (- actual
                         (presentation-timing-observation-actual-nanoseconds
                          previous))
                      1d6))
            for drift =
              (and comparable-p
                   (* 1d3
                      (- (/ (- actual
                               (presentation-timing-observation-actual-nanoseconds
                                origin))
                            1d9)
                         (- (presentation-timing-observation-predicted-seconds
                             observation)
                            (presentation-timing-observation-predicted-seconds
                             origin)))))
            for target-error =
              (and (plusp target)
                   (plusp actual)
                   (eq (presentation-timing-observation-actual-time-domain
                        observation)
                       (presentation-timing-snapshot-time-domain snapshot))
                   (= (presentation-timing-observation-actual-time-domain-id
                       observation)
                      (presentation-timing-snapshot-time-domain-id snapshot))
                   (/ (- actual target) 1d6))
            do (format stream "~D,~,9F,~,9F,~D,~D,~A,~D,~:[~;~,9F~],~:[~;~,9F~],~:[~;~,9F~]~%"
                       (presentation-timing-observation-present-id observation)
                       (presentation-timing-observation-predicted-seconds
                        observation)
                       (presentation-timing-observation-submitted-seconds
                        observation)
                       target
                       actual
                       (or (presentation-timing-observation-actual-time-domain
                            observation)
                           "")
                       (presentation-timing-observation-actual-time-domain-id
                        observation)
                       interval interval drift drift
                       target-error target-error)
               (setf previous observation)))
    pathname))

(defmethod context-device ((context vulkan-canvas-context))
  (canvas-device context))

(defclass vulkan-canvas-frame-slot ()
  ((image-ready
    :initarg :image-ready
    :reader vulkan-frame-slot-image-ready)
   (submission-index
    :initform nil
    :accessor vulkan-frame-slot-submission-index
    :documentation "Queue submission index of this slot's frame in flight.")
   (commands
    :initform nil
    :accessor vulkan-frame-slot-commands)))

(defun make-vulkan-canvas-frame-slot (native-device)
  (make-instance
   'vulkan-canvas-frame-slot
   :image-ready (lvk:create-semaphore native-device)))

(defun make-vulkan-canvas-frame-slots (native-device count)
  (let ((slots (make-array count :initial-element nil))
        (completed-p nil))
    (unwind-protect
         (progn
           (dotimes (index count)
             (setf (aref slots index)
                   (make-vulkan-canvas-frame-slot native-device)))
           (setf completed-p t)
           slots)
      (unless completed-p
        (loop for slot across slots
              when slot
                do (lvk:destroy-semaphore
                    native-device (vulkan-frame-slot-image-ready slot)))))))

(defun make-vulkan-semaphores (native-device count)
  (let ((semaphores (make-array count :initial-element nil))
        (completed-p nil))
    (unwind-protect
         (progn
           (dotimes (index count)
             (setf (aref semaphores index)
                   (lvk:create-semaphore native-device)))
           (setf completed-p t)
           semaphores)
      (unless completed-p
        (loop for semaphore across semaphores
              when semaphore
                do (lvk:destroy-semaphore native-device semaphore))))))

(defun destroy-vulkan-canvas-frame-slot (native-device slot)
  (when (vulkan-frame-slot-commands slot)
    (destroy (vulkan-frame-slot-commands slot))
    (setf (vulkan-frame-slot-commands slot) nil))
  (lvk:destroy-semaphore
   native-device (vulkan-frame-slot-image-ready slot))
  (values))

(defun recycle-vulkan-canvas-frame-slot (queue slot)
  "Wait for SLOT's frame to pass the queue frontier, then release it."
  (when (vulkan-frame-slot-commands slot)
    (wait-for-vulkan-submission
     queue (vulkan-frame-slot-submission-index slot))
    (destroy (vulkan-frame-slot-commands slot))
    (setf (vulkan-frame-slot-commands slot) nil
          (vulkan-frame-slot-submission-index slot) nil))
  slot)

(defun sdl-vulkan-instance-extensions ()
  (cffi:with-foreign-object (count :uint32)
    (let ((names (sdl3:vulkan-get-instance-extensions count)))
      (when (cffi:null-pointer-p names)
        (error "SDL could not report Vulkan instance extensions: ~A"
               (sdl3:get-error)))
      (loop for index below (cffi:mem-ref count :uint32)
            collect (cffi:foreign-string-to-lisp
                     (cffi:mem-aref names :pointer index))))))

(defun sdl-vulkan-presentation-ready-p ()
  (member :video (sdl3:was-init :video)))

(defmethod vulkan-provider-instance-options :around
    ((provider vulkan-gpu-provider))
  (multiple-value-bind (extensions flags) (call-next-method)
    (values (if (sdl-vulkan-presentation-ready-p)
                (let* ((available (lvk:enumerate-instance-extension-names))
                       (surface-capabilities-2
                         lvk:+surface-capabilities-2-extension-name+))
                  (remove-duplicates
                   (append
                    (sdl-vulkan-instance-extensions)
                    (and (member surface-capabilities-2 available
                                 :test #'string=)
                         (list surface-capabilities-2))
                    extensions)
                   :test #'string=))
                extensions)
            flags)))

(defmethod vulkan-gpu-device-extension-names :around
    ((provider vulkan-gpu-provider))
  (declare (ignore provider))
  (let ((extensions (call-next-method)))
    (if (sdl-vulkan-presentation-ready-p)
        (remove-duplicates
         (cons lvk:+swapchain-extension-name+ extensions)
         :test #'string=)
        extensions)))

(defun create-sdl-vulkan-canvas-surface (canvas provider instance)
  (declare (ignore provider))
  (cffi:with-foreign-object (surface :pointer)
    (unless (sdl3:vulkan-create-surface
             (sdl-canvas-window canvas)
             instance (cffi:null-pointer) surface)
      (error "SDL could not create a Vulkan surface: ~A" (sdl3:get-error)))
    (cffi:mem-ref surface :pointer)))

(defun destroy-sdl-vulkan-canvas-surface
    (canvas provider instance surface)
  (declare (ignore canvas provider))
  (sdl3:vulkan-destroy-surface instance surface (cffi:null-pointer))
  (values))

(defun choose-vulkan-presentation-stage (stages)
  (find-if (lambda (stage) (member stage stages))
           '(:image-first-pixel-visible
             :image-first-pixel-out
             :request-dequeued
             :queue-operations-end)))

(defun make-vulkan-canvas-presentation-timeline (context)
  "Describe and, when possible, enable timing observation for CONTEXT."
  (let* ((canvas (context-canvas context))
         (device (context-device context))
         (device-extensions (vulkan-device-extension-names device))
         (instance-extensions
           (vulkan-device-instance-extension-names device))
         (timeline (make-instance 'vulkan-presentation-timeline :stage nil)))
    (labels ((unsupported (reason)
               (setf (vulkan-presentation-timeline-status timeline) :unsupported
                     (vulkan-presentation-timeline-reason timeline) reason)
               timeline))
      (cond
        ((not (sdl-canvas-direct-display-p canvas))
         (unsupported :not-direct-display))
        ((not (member lvk:+surface-capabilities-2-extension-name+
                      instance-extensions :test #'string=))
         (unsupported :missing-surface-capabilities-2))
        ((not (member lvk:+present-timing-extension-name+
                      device-extensions :test #'string=))
         (unsupported :missing-present-timing-extension))
        ((not (member lvk:+present-id-2-extension-name+
                      device-extensions :test #'string=))
         (unsupported :missing-present-id-2-extension))
        (t
         (multiple-value-bind
               (timing-feature-p absolute-p relative-p id-feature-p)
             (lvk:physical-device-presentation-features
              (vulkan-canvas-physical-device context)
              :present-timing-p t :present-id-2-p t)
           (declare (ignore relative-p))
           (unless timing-feature-p
             (return-from make-vulkan-canvas-presentation-timeline
               (unsupported :missing-present-timing-feature)))
           (unless id-feature-p
             (return-from make-vulkan-canvas-presentation-timeline
               (unsupported :missing-present-id-2-feature)))
           (let* ((capabilities
                    (lvk:get-presentation-timing-surface-capabilities
                     (vulkan-canvas-physical-device context)
                     (vulkan-canvas-surface context)))
                  (stage
                    (choose-vulkan-presentation-stage
                     (lvk:presentation-timing-capabilities-stages
                      capabilities))))
             (cond
               ((not (lvk:presentation-timing-capabilities-supported-p
                      capabilities))
                (unsupported :surface-does-not-support-present-timing))
               ((not (lvk:presentation-timing-capabilities-present-id-2-p
                      capabilities))
                (unsupported :surface-does-not-support-present-id-2))
               ((not stage)
                (unsupported :surface-offers-no-presentation-stage))
               (t
                (make-instance
                 'vulkan-presentation-timeline
                 :stage stage
                 :absolute-time-p
                 (and absolute-p
                      (lvk:presentation-timing-capabilities-absolute-time-p
                       capabilities))))))))))))

(defun refresh-vulkan-presentation-timeline
    (timeline native-device swapchain)
  (when (eq :warming (vulkan-presentation-timeline-status timeline))
    (let ((properties
            (lvk:get-swapchain-timing-properties native-device swapchain))
          (domains (lvk:get-swapchain-time-domains native-device swapchain)))
      (when (and properties domains)
        (let ((domain
                (or (assoc :clock-monotonic domains)
                    (assoc :swapchain-local-ext domains)
                    (assoc :present-stage-local-ext domains)
                    (first domains))))
          (setf (vulkan-presentation-timeline-refresh-duration timeline)
                (lvk:swapchain-timing-properties-refresh-duration properties)
                (vulkan-presentation-timeline-refresh-interval timeline)
                (lvk:swapchain-timing-properties-refresh-interval properties)
                (vulkan-presentation-timeline-time-domain timeline) (car domain)
                (vulkan-presentation-timeline-time-domain-id timeline) (cdr domain)
                (vulkan-presentation-timeline-status timeline) :recording
                (vulkan-presentation-timeline-reason timeline) nil)))))
  timeline)

(defun drain-vulkan-presentation-timeline
    (timeline native-device swapchain)
  (when (eq :recording (vulkan-presentation-timeline-status timeline))
    (lvk:map-past-presentation-timings
     native-device swapchain
     (lambda (present-id stage time time-domain time-domain-id)
       (when (eq stage (vulkan-presentation-timeline-stage timeline))
         (note-vulkan-presentation-result
          timeline present-id time time-domain time-domain-id)))))
  timeline)

(defun bind-vulkan-canvas-device (context device)
  "Attach unconfigured CONTEXT to DEVICE and create its native SDL surface."
  (unless (typep device 'vulkan-gpu-device)
    (error 'canvas-error :canvas (context-canvas context)
           :operation :configure :reason :unsupported-device
           :details device))
  (ensure-live-vulkan-object device :configure-canvas)
  (let ((bound-device (context-device context)))
    (when (and bound-device (not (eq bound-device device)))
      (error 'canvas-error :canvas (context-canvas context)
             :operation :configure :reason :device-mismatch
             :details device)))
  (unless (context-device context)
    (let* ((required-instance-extensions (sdl-vulkan-instance-extensions))
           (missing-instance-extensions
             (set-difference
              required-instance-extensions
              (vulkan-device-instance-extension-names device)
              :test #'string=)))
      (when missing-instance-extensions
        (error 'canvas-error :canvas (context-canvas context)
               :operation :configure :reason :missing-instance-extensions
               :details missing-instance-extensions)))
    (unless (member lvk:+swapchain-extension-name+
                    (vulkan-device-extension-names device)
                    :test #'string=)
      (error 'canvas-error :canvas (context-canvas context)
             :operation :configure :reason :missing-swapchain-extension
             :details lvk:+swapchain-extension-name+))
    (let* ((instance (vulkan-device-instance device))
           (physical-device (vulkan-device-physical-device device))
           (queue-family (vulkan-device-queue-family device))
           (surface nil)
           (completed-p nil))
      (unwind-protect
           (progn
             (setf surface
                   (create-sdl-vulkan-canvas-surface
                    (context-canvas context)
                    (vulkan-canvas-provider context)
                    instance))
             (unless (lvk:surface-supported-p
                      physical-device queue-family surface)
               (error 'canvas-error :canvas (context-canvas context)
                      :operation :configure
                      :reason :device-cannot-present-to-surface
                      :details device))
             (setf (vulkan-canvas-instance context) instance
                   (vulkan-canvas-physical-device context) physical-device
                   (canvas-device context) device
                   (vulkan-canvas-surface context) surface
                   completed-p t))
        (unless completed-p
          (when surface
            (destroy-sdl-vulkan-canvas-surface
             (context-canvas context)
             (vulkan-canvas-provider context)
             instance surface))))))
  context)

(defun gpu-canvas-format-to-vulkan (format)
  (or (cdr (assoc format
                  '((:rgba8-unorm . :r8g8b8a8-unorm)
                    (:rgba8-unorm-srgb . :r8g8b8a8-srgb)
                    (:bgra8-unorm . :b8g8r8a8-unorm)
                    (:bgra8-unorm-srgb . :b8g8r8a8-srgb))))
      (error 'canvas-error :operation :configure
             :reason :unsupported-format :details format)))

(defun vulkan-canvas-format-to-gpu (format)
  (or (cdr (assoc format
                  '((:r8g8b8a8-unorm . :rgba8-unorm)
                    (:r8g8b8a8-srgb . :rgba8-unorm-srgb)
                    (:b8g8r8a8-unorm . :bgra8-unorm)
                    (:b8g8r8a8-srgb . :bgra8-unorm-srgb))))
      (error 'canvas-error :operation :configure
             :reason :unsupported-format :details format)))

(defun choose-vulkan-canvas-format (context requested-format)
  (let* ((physical-device (vulkan-canvas-physical-device context))
         (surface (vulkan-canvas-surface context))
         (formats (lvk:get-surface-formats physical-device surface))
         (requested-vulkan-format
           (and requested-format
                (gpu-canvas-format-to-vulkan requested-format))))
    (if requested-vulkan-format
        (or (find-if
             (lambda (format)
               (eq requested-vulkan-format
                   (lvk:presentation-format-format format)))
             formats)
            (error 'canvas-error :canvas (context-canvas context)
                   :operation :configure :reason :unsupported-surface-format
                   :details requested-format))
        (or (find-if
             (lambda (format)
               (and (eq :b8g8r8a8-srgb
                        (lvk:presentation-format-format format))
                    (eq :srgb-nonlinear-khr
                        (lvk:presentation-format-color-space format))))
             formats)
            (first formats)
            (error 'canvas-error :canvas (context-canvas context)
                   :operation :configure :reason :no-surface-formats)))))

(defun clamp-canvas-extent (value minimum maximum)
  (max minimum (min value maximum)))

(defun choose-vulkan-canvas-extent (context capabilities)
  (let ((current (lvk:presentation-capabilities-current-extent capabilities)))
    (if (/= #xffffffff (first current))
        current
        (multiple-value-bind (width height)
            (canvas-size (context-canvas context))
          (let ((minimum
                  (lvk:presentation-capabilities-min-image-extent capabilities))
                (maximum
                  (lvk:presentation-capabilities-max-image-extent capabilities)))
            (list (clamp-canvas-extent
                   width (first minimum) (first maximum))
                  (clamp-canvas-extent
                   height (second minimum) (second maximum))))))))

(defun choose-vulkan-canvas-image-count (context capabilities)
  (let ((desired
          (if (sdl-canvas-direct-display-p (context-canvas context))
              ;; The minimum FIFO chain admits one image being scanned and
              ;; one being prepared.  Another image would be another frame of
              ;; latency and another opportunity for CPU production to run
              ;; ahead of the actual KMS page-flip cadence.
              (lvk:presentation-capabilities-min-image-count capabilities)
              (1+ (lvk:presentation-capabilities-min-image-count
                   capabilities))))
        (maximum
          (lvk:presentation-capabilities-max-image-count capabilities)))
    (if (plusp maximum) (min desired maximum) desired)))

(defun ensure-vulkan-canvas-state (context operation expected-state)
  (unless (member (canvas-context-state context)
                  (if (listp expected-state)
                      expected-state
                      (list expected-state)))
    (error 'canvas-state-error
           :canvas (context-canvas context)
           :operation operation :reason :invalid-context-state
           :state (canvas-context-state context)
           :expected-state expected-state)))

(defun configure-vulkan-canvas-context (context configuration)
  (unless (typep configuration 'canvas-configuration)
    (error 'canvas-error :canvas (context-canvas context)
           :operation :configure :reason :invalid-configuration
           :details configuration))
  (let ((configured-device (canvas-configuration-device configuration)))
    (unless configured-device
      (error 'canvas-error :canvas (context-canvas context)
             :operation :configure :reason :device-required))
    (when (eq :configured (canvas-context-state context))
      (unconfigure-canvas-context context))
    (ensure-vulkan-canvas-state context :configure :unconfigured)
    (bind-vulkan-canvas-device context configured-device))
  (unless (and (canvas-configuration-usage configuration)
               (every (lambda (usage)
                        (member usage
                                '(:copy-src :copy-dst :render-attachment)))
                      (canvas-configuration-usage configuration)))
    (error 'canvas-error :canvas (context-canvas context)
           :operation :configure :reason :unsupported-usage
           :details (canvas-configuration-usage configuration)))
  (with-vulkan-gpu-driver-environment
    (let* ((usage (canvas-configuration-usage configuration))
           (native-usage
             (mapcar (lambda (value)
                       (ecase value
                         (:copy-src :transfer-src)
                         (:copy-dst :transfer-dst)
                         (:render-attachment :color-attachment)))
                     usage))
           (native-device (vulkan-handle (context-device context)))
           (physical-device (vulkan-canvas-physical-device context))
           (surface (vulkan-canvas-surface context))
           (capabilities
             (lvk:get-surface-capabilities physical-device surface))
           (surface-format
             (choose-vulkan-canvas-format
              context (canvas-configuration-format configuration)))
           (vk-format (lvk:presentation-format-format surface-format))
           (gpu-format (vulkan-canvas-format-to-gpu vk-format))
           (color-space
             (lvk:presentation-format-color-space surface-format))
           (extent (choose-vulkan-canvas-extent context capabilities))
           (composite-alpha
             (or (find :opaque
                       (lvk:presentation-capabilities-composite-alpha
                        capabilities))
                 (first (lvk:presentation-capabilities-composite-alpha
                         capabilities))))
           (presentation-timeline
             (make-vulkan-canvas-presentation-timeline context))
           (present-timing-p
             (eq :warming
                 (vulkan-presentation-timeline-status presentation-timeline)))
           (swapchain nil)
           (textures #())
           (frame-slots #())
           (render-done #())
           (completed-p nil))
      (unless (every (lambda (value)
                       (member value
                               (lvk:presentation-capabilities-usage
                                capabilities)))
                     native-usage)
        (error 'canvas-error :canvas (context-canvas context)
               :operation :configure
               :reason :unsupported-surface-usage :details usage))
      (unwind-protect
           (progn
             (setf swapchain
                   (lvk:create-swapchain
                    native-device surface vk-format color-space extent
                    :min-image-count
                    (choose-vulkan-canvas-image-count context capabilities)
                    :usage native-usage
                    :pre-transform
                    (lvk:presentation-capabilities-current-transform
                     capabilities)
                    :composite-alpha composite-alpha
                    :present-mode :fifo-khr
                    :present-timing-p present-timing-p)
                   textures
                   (map 'vector
                        (lambda (image)
                          (make-borrowed-vulkan-texture
                           (context-device context) image extent
                           gpu-format vk-format :usage usage))
                        (lvk:get-swapchain-images native-device swapchain))
                   frame-slots
                   (make-vulkan-canvas-frame-slots
                    native-device
                    (if (sdl-canvas-direct-display-p
                         (context-canvas context))
                        1
                        (min 2 (length textures))))
                   render-done
                   (make-vulkan-semaphores native-device (length textures)))
             (when present-timing-p
               (lvk:set-swapchain-present-timing-queue-size
                native-device swapchain
                +vulkan-presentation-timing-queue-size+)
               (refresh-vulkan-presentation-timeline
                presentation-timeline native-device swapchain))
             (setf (vulkan-canvas-swapchain context) swapchain
                   (vulkan-canvas-textures context) textures
                   (canvas-extent context) extent
                   (vulkan-canvas-window-size context)
                   (multiple-value-list
                    (canvas-size (context-canvas context)))
                   (canvas-format context) gpu-format
                   (vulkan-canvas-render-done context) render-done
                   (vulkan-canvas-frame-slots context) frame-slots
                   (vulkan-canvas-next-frame-slot context) 0
                   (vulkan-canvas-presentation-timeline context)
                   presentation-timeline
                   (canvas-context-configuration context)
                   (make-canvas-configuration
                    :device (context-device context)
                    :format gpu-format
                    :usage (canvas-configuration-usage configuration))
                   (canvas-context-state context) :configured
                   completed-p t)
             context)
        (unless completed-p
          (loop for semaphore across render-done
                when semaphore
                  do (lvk:destroy-semaphore native-device semaphore))
          (loop for slot across frame-slots
                when slot
                  do (destroy-vulkan-canvas-frame-slot native-device slot))
          (loop for texture across textures do (destroy texture))
          (when swapchain
            (lvk:destroy-swapchain native-device swapchain)))))))

(defmethod configure-canvas-context
    ((context vulkan-canvas-context) configuration)
  (call-on-sdl-canvas-thread
   (context-canvas context)
   (lambda ()
     (configure-vulkan-canvas-context context configuration))))

(defun unconfigure-vulkan-canvas-context (context)
  (when (eq :in-frame (canvas-context-state context))
    (ensure-vulkan-canvas-state context :unconfigure :configured))
  (unless (member (canvas-context-state context) '(:unconfigured :destroyed))
    (with-vulkan-gpu-driver-environment
      (let ((native-device (vulkan-handle (context-device context))))
        (lvk:device-wait-idle native-device)
        (loop for slot across (vulkan-canvas-frame-slots context)
              do (destroy-vulkan-canvas-frame-slot native-device slot))
        (loop for texture across (vulkan-canvas-textures context)
              do (destroy texture))
        (loop for semaphore across (vulkan-canvas-render-done context)
              do (lvk:destroy-semaphore native-device semaphore))
        (when (vulkan-canvas-swapchain context)
          (lvk:destroy-swapchain
           native-device (vulkan-canvas-swapchain context)))))
    (setf (vulkan-canvas-swapchain context) nil
          (vulkan-canvas-textures context) #()
          (vulkan-canvas-window-size context) nil
          (vulkan-canvas-render-done context) #()
          (vulkan-canvas-frame-slots context) #()
          (vulkan-canvas-next-frame-slot context) 0
          (vulkan-canvas-presentation-timeline context) nil
          (vulkan-canvas-current-texture context) nil
          (canvas-context-configuration context) nil
          (canvas-extent context) nil
          (canvas-format context) nil
          (canvas-context-state context) :unconfigured))
  (values))

(defmethod unconfigure-canvas-context ((context vulkan-canvas-context))
  (unconfigure-vulkan-canvas-context context))

(defmethod destroy-canvas-context ((context vulkan-canvas-context))
  (unless (eq :destroyed (canvas-context-state context))
    (unconfigure-canvas-context context)
    (when (vulkan-canvas-surface context)
      (destroy-sdl-vulkan-canvas-surface
       (context-canvas context)
       (vulkan-canvas-provider context)
       (vulkan-canvas-instance context)
       (vulkan-canvas-surface context)))
    (setf (vulkan-canvas-surface context) nil
          (canvas-context-state context) :destroyed)
    (when (eq context (canvas-context (context-canvas context)))
      (setf (canvas-context (context-canvas context)) nil)))
  (values))

(defmethod make-canvas-context
    ((canvas sdl-canvas) (provider vulkan-gpu-provider)
     &optional configuration)
  (when (canvas-context canvas)
    (error 'canvas-error :canvas canvas :operation :make-context
           :reason :context-already-exists))
  (call-on-sdl-canvas-thread
   canvas
   (lambda ()
     (let ((context
             (make-instance 'vulkan-canvas-context
                            :canvas canvas :provider provider)))
       (setf (canvas-context canvas) context)
       (handler-case
           (progn
             (when configuration
               (configure-canvas-context context configuration))
             context)
         (error (condition)
           (ignore-errors (destroy-canvas-context context))
           (error condition)))))))

(defmethod get-current-texture ((context vulkan-canvas-context))
  (or (vulkan-canvas-current-texture context)
      (error 'canvas-state-error
             :canvas (context-canvas context)
             :operation :get-current-texture :reason :outside-frame
             :state (canvas-context-state context) :expected-state :in-frame)))

;;; A swapchain is only ever right for one window size.  Resizing the window
;;; leaves the images, their views, and the surface's own extent describing a
;;; window that no longer exists, and Vulkan says so by refusing to hand out
;;; another image.  Rebuilding is the whole answer, and it is cheap enough to
;;; do from inside the frame that discovered the problem.

(defun rebuild-vulkan-canvas-swapchain (context)
  "Replace CONTEXT's swapchain with one built for the window's present size.

The configuration is the one the application already asked for: only the
extent, the images, and the per-image synchronization are made again.
Returns NIL when the window has no pixels to present to at all, which is an
ordinary state for a minimized window and not an error."
  (let ((configuration (canvas-context-configuration context)))
    (unless configuration
      (error 'canvas-state-error
             :canvas (context-canvas context)
             :operation :rebuild-swapchain :reason :invalid-context-state
             :state (canvas-context-state context)
             :expected-state :configured))
    (multiple-value-bind (width height) (canvas-size (context-canvas context))
      (when (or (zerop width) (zerop height))
        (return-from rebuild-vulkan-canvas-swapchain nil)))
    (unconfigure-vulkan-canvas-context context)
    (configure-vulkan-canvas-context context configuration)
    (log-event :vulkan "rebuilt the swapchain for ~{~D~^x~}"
               (canvas-extent context))
    t))

(defun ensure-vulkan-canvas-swapchain (context)
  "Make the swapchain agree with the window before a frame asks for an image.

Returns NIL when this frame cannot be presented, which the caller skips."
  (let ((size (multiple-value-list (canvas-size (context-canvas context)))))
    (cond ((or (zerop (first size)) (zerop (second size))) nil)
          ((equal size (vulkan-canvas-window-size context)) t)
          (t (rebuild-vulkan-canvas-swapchain context)))))

(defun acquire-vulkan-canvas-image (context)
  "Acquire the next presentable image, rebuilding a stale swapchain once.

Returns the frame slot, its index, and the image index, or NIL when the
surface still cannot supply an image and this frame should be skipped."
  (let ((device (context-device context))
        (queue (device-queue (context-device context))))
    (dotimes (attempt 2)
      (let* ((slot-index (vulkan-canvas-next-frame-slot context))
             (slot (aref (vulkan-canvas-frame-slots context) slot-index)))
        (with-cpu-trace-zone (:vulkan/recycle-frame-slot)
          (recycle-vulkan-canvas-frame-slot queue slot))
        (multiple-value-bind (image-index result)
            (with-cpu-trace-zone (:canvas/acquire-drawable)
              (lvk:acquire-next-image
               (vulkan-handle device) (vulkan-canvas-swapchain context)
               (vulkan-frame-slot-image-ready slot)))
          (if (eq result :error-out-of-date-khr)
              ;; The semaphore is left unsignalled by a refused acquisition,
              ;; so the rebuilt swapchain's own slots start clean.
              (unless (rebuild-vulkan-canvas-swapchain context)
                (return-from acquire-vulkan-canvas-image nil))
              (return-from acquire-vulkan-canvas-image
                (values slot slot-index image-index))))))
    nil))

(defun vulkan-canvas-presentation-target (context)
  "Choose the display beat represented by CONTEXT's next frame."
  (let* ((canvas (context-canvas context))
         (timeline (vulkan-canvas-presentation-timeline context))
         (fallback (canvas-presentation-time canvas)))
    (if timeline
        (predict-vulkan-presentation-target
         timeline fallback)
        (values fallback nil))))

(defun present-vulkan-canvas-image
    (context queue image-index render-done predicted-presentation-time
     target-nanoseconds)
  "Present one image and opportunistically collect its display timestamp."
  (let* ((device (context-device context))
         (native-device (vulkan-handle device))
         (swapchain (vulkan-canvas-swapchain context))
         (timeline (vulkan-canvas-presentation-timeline context)))
    (labels ((plain-present ()
               (lvk:present
                (vulkan-handle queue) swapchain image-index
                :wait-semaphores (vector render-done)))
             (timed-present (present-id)
               (lvk:present
                (vulkan-handle queue) swapchain image-index
                :wait-semaphores (vector render-done)
                :present-id present-id
                :present-stage
                (vulkan-presentation-timeline-stage timeline)
                :time-domain-id
                (vulkan-presentation-timeline-time-domain-id timeline)
                :target-time target-nanoseconds
                :target-time-domain-present-stage
                (and target-nanoseconds
                     (eq :present-stage-local-ext
                         (vulkan-presentation-timeline-time-domain timeline))
                     (vulkan-presentation-timeline-stage timeline)))))
      (cond
        ((and timeline
              (eq :recording
                  (vulkan-presentation-timeline-status timeline)))
         (let* ((present-id
                  (vulkan-presentation-timeline-next-present-id timeline))
                (submitted-seconds (monotonic-seconds))
                (result (timed-present present-id)))
           (when (eq result :error-present-timing-queue-full-ext)
             (drain-vulkan-presentation-timeline
              timeline native-device swapchain)
             (setf result (timed-present present-id)))
           (if (eq result :error-present-timing-queue-full-ext)
               (progn
                 (incf (vulkan-presentation-timeline-dropped-count timeline))
                 (plain-present))
               (progn
                 (unless (eq result :error-out-of-date-khr)
                   (note-vulkan-presentation-submission
                    timeline present-id predicted-presentation-time
                    submitted-seconds (or target-nanoseconds 0)))
                 (drain-vulkan-presentation-timeline
                  timeline native-device swapchain)
                 result))))
        (t
         (prog1 (plain-present)
           (when (and timeline
                      (eq :warming
                          (vulkan-presentation-timeline-status timeline)))
             (refresh-vulkan-presentation-timeline
              timeline native-device swapchain))))))))

(zdefun (%call-with-vulkan-canvas-frame :zone :canvas/frame)
    (context function)
  (ensure-vulkan-canvas-state context :frame :configured)
  (unless (ensure-vulkan-canvas-swapchain context)
    (return-from %call-with-vulkan-canvas-frame nil))
  (let* ((device (context-device context))
         (queue (device-queue device))
         (encoder nil)
         (commands nil)
         (predicted-presentation-time nil)
         (target-nanoseconds nil))
    (multiple-value-bind (slot slot-index image-index)
          (acquire-vulkan-canvas-image context)
        (unless slot
          (return-from %call-with-vulkan-canvas-frame nil))
        (let ((texture (aref (vulkan-canvas-textures context) image-index))
              (render-done
                (aref (vulkan-canvas-render-done context) image-index)))
          (unwind-protect
               (progn
                 (setf encoder
                       (create device (make-command-encoder-descriptor))
                       (vulkan-canvas-current-texture context) texture
                       (canvas-context-state context) :in-frame)
                 (let ((timeline
                         (vulkan-canvas-presentation-timeline context)))
                   (when (and timeline
                              (eq :recording
                                  (vulkan-presentation-timeline-status
                                   timeline)))
                     (drain-vulkan-presentation-timeline
                      timeline (vulkan-handle device)
                      (vulkan-canvas-swapchain context))))
                 (with-cpu-trace-zone (:gpu/encode)
                   (multiple-value-bind (presentation-time native-target)
                       (vulkan-canvas-presentation-target context)
                     (setf predicted-presentation-time presentation-time
                           target-nanoseconds native-target)
                     (call-with-canvas-time
                      (context-canvas context) presentation-time
                      (lambda ()
                        (funcall function texture encoder
                                 presentation-time)))))
                 (with-cpu-trace-zone (:gpu/finish-encoding)
                   (transition-vulkan-texture
                    encoder texture :present-src-khr)
                   (setf commands (finish encoder)))
                 (let ((submission-index
                         (with-cpu-trace-zone (:gpu/submit)
                           (submit-vulkan-command-buffers
                            queue (vector commands)
                            :wait-semaphores
                            (vector
                             (list
                              (vulkan-frame-slot-image-ready slot)
                              (mapcar
                               (lambda (usage)
                                 (ecase usage
                                   (:copy-dst :transfer)
                                   (:render-attachment
                                    :color-attachment-output)))
                               (canvas-configuration-usage
                                (canvas-context-configuration context)))))
                            :signal-semaphores
                            (vector (list render-done '(:all-commands)))
                            :wait-for-completion nil))))
                   (setf (vulkan-frame-slot-commands slot) commands
                         (vulkan-frame-slot-submission-index slot)
                         submission-index
                         commands nil))
                 (when (eq :error-out-of-date-khr
                           (with-cpu-trace-zone (:canvas/present)
                             (present-vulkan-canvas-image
                              context queue image-index render-done
                              predicted-presentation-time
                              target-nanoseconds)))
                   ;; The image was presented to a surface that has already
                   ;; moved on.  Forgetting the size this swapchain was built
                   ;; for is what makes the next frame rebuild it, even when
                   ;; the window itself reports the same pixels as before.
                   (setf (vulkan-canvas-window-size context) nil))
                 (setf (vulkan-canvas-next-frame-slot context)
                       (mod (1+ slot-index)
                            (length (vulkan-canvas-frame-slots context))))
                 texture)
            (setf (vulkan-canvas-current-texture context) nil
                  (canvas-context-state context) :configured)
            (when commands (destroy commands))
            (when encoder (destroy encoder)))))))

(defun call-with-vulkan-canvas-frame (context function)
  "Run one frame, then account for whatever the validation layer said.

The end of the frame is the safe point: every Vulkan call it made has
returned, so a complaint can become a condition without unwinding through a
driver mid-call.  GUARDING-SDL-CANVAS catches it, retains it with the
backtrace the callback took while the offending frames were still on the
stack, and parks the canvas -- the same treatment as any other frame that
failed, and visible from ./sly as a reported failure rather than a line
somebody has to go looking for."
  (with-vulkan-validation (:frame)
    (%call-with-vulkan-canvas-frame context function)))

(defmethod call-with-canvas-frame
    ((context vulkan-canvas-context) function)
  (call-on-sdl-canvas-thread
   (context-canvas context)
   (lambda ()
     (call-with-vulkan-canvas-frame context function))))
