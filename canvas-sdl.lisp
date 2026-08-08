;;; SDL realization of the native canvas protocol.

(in-package #:luv)

(defclass sdl-canvas (canvas)
  ((title
    :initarg :title
    :initform "luv canvas"
    :reader canvas-title)
   (width
    :initarg :width
    :initform 800
    :reader canvas-width)
   (height
    :initarg :height
    :initform 600
    :reader canvas-height)
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
   (close-requested-p
    :initform nil
    :accessor sdl-canvas-close-requested-p)
   (thread
    :initform nil
    :accessor sdl-canvas-thread)
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

(defun make-sdl-canvas (&key (title "luv canvas") (width 800) (height 600))
  "Construct an unrealized SDL canvas."
  (make-instance 'sdl-canvas :title title :width width :height height))

(defmethod canvas-size ((canvas sdl-canvas))
  (let ((window (sdl-canvas-window canvas)))
    (if window
        (multiple-value-bind (success width height)
            (sdl3:get-window-size-in-pixels window)
          (unless success
            (error 'canvas-error :canvas canvas :operation :size
                   :reason :native-size-failed :details (sdl3:get-error)))
          (values width height))
        (values (canvas-width canvas) (canvas-height canvas)))))

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
    (sb-thread:wait-on-semaphore (sdl-canvas-request-completion request))
    (when (sdl-canvas-request-error request)
      (error (sdl-canvas-request-error request)))
    (values-list (sdl-canvas-request-values request))))

(defmethod request-canvas-frame ((canvas sdl-canvas) function)
  (call-on-sdl-canvas-thread
   canvas
   (lambda ()
     (funcall function
              (/ (get-internal-real-time)
                 (coerce internal-time-units-per-second 'double-float))))))

(defun sdl-canvas-event-loop (canvas)
  (loop until (sdl-canvas-close-requested-p canvas)
        do (process-sdl-canvas-requests canvas)
           (multiple-value-bind (event event-type) (sdl3:poll-event*)
             (declare (ignore event))
             (when (member event-type '(:quit :window-close-requested))
               (setf (sdl-canvas-close-requested-p canvas) t)))
           (sleep 0.005)))

(defun run-sdl-canvas (canvas)
  (with-sdl-native-environment
    (unwind-protect
         (handler-case
             (progn
               (unless (sdl3:init :video)
                 (error "SDL video initialization failed: ~A"
                        (sdl3:get-error)))
               (let ((window
                       (sdl3:create-window
                        (canvas-title canvas)
                        (canvas-width canvas) (canvas-height canvas)
                        '(:vulkan :resizable))))
                 (when (cffi:null-pointer-p window)
                   (error "SDL window creation failed: ~A" (sdl3:get-error)))
                 (setf (sdl-canvas-window canvas) window
                       (canvas-state canvas) :open)
                 (sdl-canvas-event-loop canvas)))
           (error (condition)
             (setf (sdl-canvas-startup-error canvas) condition)))
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
      (sdl3:quit)
      (fail-sdl-canvas-requests
       canvas
       (make-condition 'canvas-error :canvas canvas
                       :operation :frame :reason :canvas-closed))
      (setf (canvas-state canvas) :closed))))

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
        (sdl-canvas-close-requested-p canvas) nil)
  (start-sdl-canvas-thread canvas)
  (loop repeat 6000
        when (eq :open (canvas-state canvas)) do (return canvas)
        when (sdl-canvas-startup-error canvas)
          do (error (sdl-canvas-startup-error canvas))
        when (eq :closed (canvas-state canvas))
          do (error 'canvas-error :canvas canvas :operation :open
                    :reason :closed-during-startup)
        do (sleep 0.005)
        finally (error 'canvas-error :canvas canvas :operation :open
                       :reason :startup-timeout)))

(defmethod close-canvas ((canvas sdl-canvas))
  (when (member (canvas-state canvas) '(:opening :open))
    (setf (sdl-canvas-close-requested-p canvas) t)
    (unless (sdl-canvas-native-thread-p canvas)
      (loop repeat 6000
            when (eq :closed (canvas-state canvas)) do (return)
            do (sleep 0.005)
            finally
               (error 'canvas-error :canvas canvas :operation :close
                      :reason :shutdown-timeout))))
  (values))
