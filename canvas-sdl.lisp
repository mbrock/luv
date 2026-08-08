;;; SDL realization of the native canvas protocol.

(in-package #:luv)

(defclass sdl-canvas (canvas)
  ((title
    :initarg :title
    :initform "luv canvas"
    :accessor canvas-title)
   (width
    :initarg :width
    :initform 800
    :accessor canvas-width)
   (height
    :initarg :height
    :initform 600
    :accessor canvas-height)
   (x
    :initarg :x
    :initform nil
    :accessor sdl-canvas-x)
   (y
    :initarg :y
    :initform nil
    :accessor sdl-canvas-y)
   (visible-p
    :initarg :visible-p
    :initform t
    :accessor canvas-visible-p)
   (window
    :initform nil
    :accessor sdl-canvas-window)
   (context
    :initform nil
    :accessor canvas-context)
   (state
    :initform :new
    :accessor canvas-state)
   (startup-error
    :initform nil
    :accessor sdl-canvas-startup-error)
   (startup-completion
    :initform (sb-thread:make-semaphore :count 0)
    :accessor sdl-canvas-startup-completion)
   (shutdown-completion
    :initform (sb-thread:make-semaphore :count 0)
    :accessor sdl-canvas-shutdown-completion)
   (close-requested-p
    :initform nil
    :accessor sdl-canvas-close-requested-p)
   (thread
    :initform nil
    :accessor sdl-canvas-thread)
   (wake-event-type
    :initform nil
    :accessor sdl-canvas-wake-event-type)
   (request-lock
    :initform (sb-thread:make-mutex :name "luv SDL canvas request lock")
    :reader sdl-canvas-request-lock)
   (requests
    :initform nil
    :accessor sdl-canvas-requests)))

(defstruct sdl-canvas-request
  function
  (completion (sb-thread:make-semaphore :count 0) :read-only t)
  values
  error)

(defmacro with-sdl-native-environment (&body body)
  "Run BODY with the floating-point environment expected by native drivers."
  #+darwin
  `(float-features:with-float-traps-masked t ,@body)
  #-darwin
  `(progn ,@body))

(defgeneric prepare-sdl-canvas-host (canvas)
  (:documentation "Prepare the native application host before SDL_Init."))

(defgeneric activate-sdl-canvas-host (canvas)
  (:documentation "Show and activate CANVAS after its SDL window exists."))

(defgeneric deactivate-sdl-canvas-host (canvas)
  (:documentation "Release CANVAS's native application presence after SDL_Quit."))

(defmethod prepare-sdl-canvas-host ((canvas canvas))
  (declare (ignore canvas))
  (unless (sdl3:set-app-metadata "luv" "0.0.1" "com.mbrock.luv")
    (error "SDL application metadata failed: ~A" (sdl3:get-error))))

(defmethod activate-sdl-canvas-host ((canvas sdl-canvas))
  (when (canvas-visible-p canvas)
    (let ((window (sdl-canvas-window canvas)))
      (unless (sdl3:show-window window)
        (error "SDL window show failed: ~A" (sdl3:get-error)))
      ;; Raising is a request to the window manager and may legitimately be
      ;; denied by focus-stealing policy, so it is not an open failure.
      (sdl3:raise-window window))))

(defmethod deactivate-sdl-canvas-host ((canvas canvas))
  (declare (ignore canvas))
  nil)

(defun make-sdl-canvas (&key (title "luv canvas") (width 800) (height 600)
                          x y (visible-p t) (clock (make-demand-clock)))
  "Construct an unrealized SDL canvas."
  (make-instance 'sdl-canvas :title title :width width :height height
                              :x x :y y :visible-p visible-p :clock clock))

(defmethod canvas-size ((canvas sdl-canvas))
  (if (eq :open (canvas-state canvas))
      (call-on-sdl-canvas-thread
       canvas
       (lambda ()
         (multiple-value-bind (success width height)
             (sdl3:get-window-size-in-pixels (sdl-canvas-window canvas))
           (unless success
             (error 'canvas-error :canvas canvas :operation :size
                    :reason :native-size-failed :details (sdl3:get-error)))
           (values width height))))
      (values (canvas-width canvas) (canvas-height canvas))))

(defun sdl-canvas-native-thread-p (canvas)
  #+darwin
  (declare (ignore canvas))
  #+darwin
  (trivial-main-thread:main-thread-p)
  #-darwin
  (eq sb-thread:*current-thread* (sdl-canvas-thread canvas)))

(defun take-sdl-canvas-requests (canvas)
  (sb-thread:with-mutex ((sdl-canvas-request-lock canvas))
    (prog1 (sdl-canvas-requests canvas)
      (setf (sdl-canvas-requests canvas) nil))))

(defun process-sdl-canvas-requests (canvas)
  (dolist (request (take-sdl-canvas-requests canvas))
    (handler-case
        (setf (sdl-canvas-request-values request)
              (multiple-value-list
               (funcall (sdl-canvas-request-function request))))
      (error (condition)
        (setf (sdl-canvas-request-error request) condition)))
    (sb-thread:signal-semaphore (sdl-canvas-request-completion request))))

(defun fail-sdl-canvas-requests (canvas condition)
  (dolist (request (take-sdl-canvas-requests canvas))
    (setf (sdl-canvas-request-error request) condition)
    (sb-thread:signal-semaphore (sdl-canvas-request-completion request))))

(defun wake-sdl-canvas (canvas)
  "Wake CANVAS's native SDL event loop after cross-thread work arrives."
  (let ((event-type (sdl-canvas-wake-event-type canvas)))
    (when event-type
      (cffi:with-foreign-object (event '(:union sdl3:event))
        (dotimes (index (cffi:foreign-type-size '(:union sdl3:event)))
          (setf (cffi:mem-aref event :uint8 index) 0))
        (setf (cffi:mem-ref event :uint32) event-type)
        (sdl3:push-event event)))))

(defmethod (setf canvas-clock) :after (clock (canvas sdl-canvas))
  (when (typep clock 'cadence-clock)
    (setf (cadence-clock-next-frame-time clock) nil))
  (wake-sdl-canvas canvas))

(defun call-on-sdl-canvas-thread (canvas function)
  "Call FUNCTION synchronously on CANVAS's native event thread."
  (when (sdl-canvas-native-thread-p canvas)
    (return-from call-on-sdl-canvas-thread (funcall function)))
  (unless (eq :open (canvas-state canvas))
    (error 'canvas-state-error
           :canvas canvas :operation :dispatch
           :reason :invalid-state
           :state (canvas-state canvas) :expected-state :open))
  (let ((request (make-sdl-canvas-request :function function)))
    (sb-thread:with-mutex ((sdl-canvas-request-lock canvas))
      (setf (sdl-canvas-requests canvas)
            (nconc (sdl-canvas-requests canvas) (list request))))
    (wake-sdl-canvas canvas)
    (sb-thread:wait-on-semaphore (sdl-canvas-request-completion request))
    (when (sdl-canvas-request-error request)
      (error (sdl-canvas-request-error request)))
    (values-list (sdl-canvas-request-values request))))

(defun call-sdl-canvas-window-operation (canvas operation function)
  (unless (eq :open (canvas-state canvas))
    (error 'canvas-state-error
           :canvas canvas :operation operation :reason :invalid-state
           :state (canvas-state canvas) :expected-state :open))
  (call-on-sdl-canvas-thread
   canvas
   (lambda ()
     (unless (funcall function (sdl-canvas-window canvas))
       (error 'canvas-error :canvas canvas :operation operation
              :reason :native-window-operation-failed
              :details (sdl3:get-error)))))
  canvas)

(defmethod (setf canvas-title) (title (canvas sdl-canvas))
  (check-type title string)
  (when (eq :open (canvas-state canvas))
    (call-sdl-canvas-window-operation
     canvas :set-title
     (lambda (window) (sdl3:set-window-title window title))))
  (setf (slot-value canvas 'title) title))

(defmethod canvas-position ((canvas sdl-canvas))
  (if (eq :open (canvas-state canvas))
      (call-on-sdl-canvas-thread
       canvas
       (lambda ()
         (multiple-value-bind (success x y)
             (sdl3:get-window-position (sdl-canvas-window canvas))
           (unless success
             (error 'canvas-error :canvas canvas :operation :position
                    :reason :native-position-failed :details (sdl3:get-error)))
           (values x y))))
      (values (sdl-canvas-x canvas) (sdl-canvas-y canvas))))

(defmethod show-canvas ((canvas sdl-canvas))
  (let ((was-visible-p (canvas-visible-p canvas)))
    (setf (canvas-visible-p canvas) t)
    (when (eq :open (canvas-state canvas))
      (handler-case
          (call-on-sdl-canvas-thread
           canvas
           (lambda () (activate-sdl-canvas-host canvas)))
        (error (condition)
          (setf (canvas-visible-p canvas) was-visible-p)
          (error condition)))))
  canvas)

(defmethod hide-canvas ((canvas sdl-canvas))
  (when (eq :open (canvas-state canvas))
    (call-sdl-canvas-window-operation canvas :hide #'sdl3:hide-window))
  (setf (canvas-visible-p canvas) nil)
  canvas)

(defmethod move-canvas ((canvas sdl-canvas) x y)
  (check-type x integer)
  (check-type y integer)
  (when (eq :open (canvas-state canvas))
    (call-sdl-canvas-window-operation
     canvas :move
     (lambda (window) (sdl3:set-window-position window x y))))
  (setf (sdl-canvas-x canvas) x
        (sdl-canvas-y canvas) y)
  canvas)

(defmethod resize-canvas ((canvas sdl-canvas) width height)
  (check-type width (integer 1))
  (check-type height (integer 1))
  (when (eq :open (canvas-state canvas))
    (call-sdl-canvas-window-operation
     canvas :resize
     (lambda (window) (sdl3:set-window-size window width height))))
  (setf (canvas-width canvas) width
        (canvas-height canvas) height)
  canvas)

(defmethod raise-canvas ((canvas sdl-canvas))
  (call-sdl-canvas-window-operation canvas :raise #'sdl3:raise-window))

(defmethod minimize-canvas ((canvas sdl-canvas))
  (call-sdl-canvas-window-operation canvas :minimize #'sdl3:minimize-window))

(defmethod restore-canvas ((canvas sdl-canvas))
  (call-sdl-canvas-window-operation canvas :restore #'sdl3:restore-window))

(defmethod request-canvas-frame ((canvas sdl-canvas) function)
  (call-on-sdl-canvas-thread
   canvas
   (lambda ()
     (funcall function
              (/ (get-internal-real-time)
                 (coerce internal-time-units-per-second 'double-float))))))

(defun canvas-timestamp ()
  (/ (get-internal-real-time)
     (coerce internal-time-units-per-second 'double-float)))

(defun handle-sdl-canvas-event (canvas event-type)
  (when (or (= event-type
               (cffi:foreign-enum-value 'sdl3::event-type :quit))
            (= event-type
               (cffi:foreign-enum-value 'sdl3::event-type
                                        :window-close-requested)))
    (setf (sdl-canvas-close-requested-p canvas) t)))

(defun poll-sdl-canvas-event-type ()
  ;; cl-sdl3's event-unmarshal quite reasonably uses its static EVENT-TYPE
  ;; enum, but SDL_RegisterEvents returns values not present in that enum.
  ;; The host loop only needs the raw tag.
  (cffi:with-foreign-object (event '(:union sdl3:event))
    (when (sdl3:poll-event event)
      (cffi:mem-ref event :uint32))))

(defun drain-sdl-canvas-events (canvas)
  (loop for event-type = (poll-sdl-canvas-event-type)
        while event-type
        do (handle-sdl-canvas-event canvas event-type)))

(defun wait-for-sdl-canvas-event (canvas timeout)
  (cffi:with-foreign-object (event '(:union sdl3:event))
    (when (if timeout
              (sdl3:wait-event-timeout event timeout)
              (sdl3:wait-event event))
      (handle-sdl-canvas-event canvas (cffi:mem-ref event :uint32))
      (drain-sdl-canvas-events canvas))))

(defun sdl-canvas-event-loop (canvas)
  (loop until (sdl-canvas-close-requested-p canvas)
        do (process-sdl-canvas-requests canvas)
           (let ((timestamp (canvas-timestamp)))
             (service-canvas-clock (canvas-clock canvas) canvas timestamp)
             (unless (sdl-canvas-close-requested-p canvas)
               (wait-for-sdl-canvas-event
                canvas
                (clock-wait-timeout (canvas-clock canvas)
                                    (canvas-timestamp)))))))

(defun run-sdl-canvas (canvas)
  (with-sdl-native-environment
    (let ((sdl-initialized-p nil))
      (unwind-protect
           (handler-case
               (progn
                 (prepare-sdl-canvas-host canvas)
                 (unless (sdl3:init :video)
                   (error "SDL video initialization failed: ~A"
                          (sdl3:get-error)))
                 (setf sdl-initialized-p t)
                 (let ((wake-event-type (sdl3:register-events 1)))
                   (when (zerop wake-event-type)
                     (error "SDL user event registration failed: ~A"
                            (sdl3:get-error)))
                   (setf (sdl-canvas-wake-event-type canvas) wake-event-type))
                 (let ((window
                         (sdl3:create-window
                          (canvas-title canvas)
                          (canvas-width canvas) (canvas-height canvas)
                          '(:vulkan :resizable :hidden))))
                   (when (cffi:null-pointer-p window)
                     (error "SDL window creation failed: ~A" (sdl3:get-error)))
                   (setf (sdl-canvas-window canvas) window)
                   (when (and (sdl-canvas-x canvas) (sdl-canvas-y canvas))
                     (unless (sdl3:set-window-position
                              window
                              (sdl-canvas-x canvas) (sdl-canvas-y canvas))
                       (error "SDL window positioning failed: ~A"
                              (sdl3:get-error))))
                   (activate-sdl-canvas-host canvas)
                   (setf (canvas-state canvas) :open)
                   (sb-thread:signal-semaphore
                    (sdl-canvas-startup-completion canvas))
                   (sdl-canvas-event-loop canvas)))
             (error (condition)
               (setf (sdl-canvas-startup-error canvas) condition)
               (when (eq :opening (canvas-state canvas))
                 (sb-thread:signal-semaphore
                  (sdl-canvas-startup-completion canvas)))))
        (setf (canvas-state canvas) :closing)
        (when (canvas-context canvas)
          (handler-case
              (destroy-canvas-context (canvas-context canvas))
            (error (condition)
              (unless (sdl-canvas-startup-error canvas)
                (setf (sdl-canvas-startup-error canvas) condition))))
          (setf (canvas-context canvas) nil))
        (when (sdl-canvas-window canvas)
          (sdl3:destroy-window (sdl-canvas-window canvas))
          (setf (sdl-canvas-window canvas) nil))
        (when sdl-initialized-p
          (sdl3:quit)
          (handler-case
              (deactivate-sdl-canvas-host canvas)
            (error (condition)
              (unless (sdl-canvas-startup-error canvas)
                (setf (sdl-canvas-startup-error canvas) condition)))))
        (setf (sdl-canvas-wake-event-type canvas) nil)
        (fail-sdl-canvas-requests
         canvas
         (make-condition 'canvas-error :canvas canvas
                         :operation :frame :reason :canvas-closed))
        (setf (canvas-state canvas) :closed)
        (sb-thread:signal-semaphore
         (sdl-canvas-shutdown-completion canvas))))))

(defun start-sdl-canvas-thread (canvas)
  #+darwin
  (progn
    (setf (sdl-canvas-thread canvas) (trivial-main-thread:main-thread))
    (sb-thread:make-thread
     (lambda ()
       (trivial-main-thread:call-in-main-thread
        (lambda () (run-sdl-canvas canvas))))
     :name "luv SDL Cocoa dispatcher"))
  #-darwin
  (setf (sdl-canvas-thread canvas)
        (sb-thread:make-thread
         (lambda () (run-sdl-canvas canvas))
         :name "luv SDL canvas event loop")))

(defmethod open-canvas ((canvas sdl-canvas))
  (unless (member (canvas-state canvas) '(:new :closed))
    (error 'canvas-state-error
           :canvas canvas :operation :open :reason :invalid-state
           :state (canvas-state canvas) :expected-state '(:new :closed)))
  (setf (canvas-state canvas) :opening
        (sdl-canvas-startup-error canvas) nil
        (sdl-canvas-close-requested-p canvas) nil
        (sdl-canvas-startup-completion canvas)
        (sb-thread:make-semaphore :count 0)
        (sdl-canvas-shutdown-completion canvas)
        (sb-thread:make-semaphore :count 0))
  (start-sdl-canvas-thread canvas)
  (sb-thread:wait-on-semaphore (sdl-canvas-startup-completion canvas))
  (cond ((sdl-canvas-startup-error canvas)
         (error (sdl-canvas-startup-error canvas)))
        ((eq :open (canvas-state canvas)) canvas)
        (t
         (error 'canvas-error :canvas canvas :operation :open
                :reason :closed-during-startup))))

(defmethod close-canvas ((canvas sdl-canvas))
  (when (member (canvas-state canvas) '(:opening :open))
    (setf (sdl-canvas-close-requested-p canvas) t)
    (wake-sdl-canvas canvas)
    (unless (sdl-canvas-native-thread-p canvas)
      (sb-thread:wait-on-semaphore
       (sdl-canvas-shutdown-completion canvas))))
  (values))
