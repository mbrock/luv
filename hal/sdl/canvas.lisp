;;; SDL realization of the native canvas protocol.

(in-package #:luv)

(defclass sdl-canvas (canvas)
  ((title
    :initarg :title
    :initform "luv canvas"
    :accessor canvas-title)
   ;; NIL width or height means "whatever suits the display this window opens
   ;; on"; the native size is resolved once, at window creation, when SDL can
   ;; finally be asked about the desktop.
   (width
    :initarg :width
    :initform 800
    :accessor canvas-width)
   (height
    :initarg :height
    :initform 600
    :accessor canvas-height)
   (fullscreen-p
    :initarg :fullscreen-p
    :initform nil
    :accessor canvas-fullscreen-p
    :documentation
    "Whether the window occupies its display's borderless fullscreen mode.")
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
   (presentation-api
    :initarg :presentation-api
    :initform :vulkan
    :reader sdl-canvas-presentation-api
    :documentation
    "The native graphics machinery SDL must select when realizing the window.")
   ;; The loop publishes where it is and when it got there.  Everything the
   ;; watchdog knows, and everything ./sly status reports about a window that
   ;; has stopped answering, is read out of these three slots.
   (phase
    :initform :new
    :accessor sdl-canvas-phase)
   (phase-time
    :initform nil
    :accessor sdl-canvas-phase-time)
   (ticks
    :initform 0
    :accessor sdl-canvas-ticks)
   (watchdog
    :initform nil
    :accessor sdl-canvas-watchdog)
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
    :accessor sdl-canvas-requests)
   ;; What has gone wrong on this loop, kept: the newest first, each with the
   ;; backtrace it was caught with.  A frame failure also parks here, in
   ;; FRAME-FAILURE, and holds further frames until it is resumed.
   (failures
    :initform nil
    :accessor sdl-canvas-failures)
   (frame-failure
    :initform nil
    :accessor sdl-canvas-frame-failure)
   (frame-count
    :initform 0
    :accessor sdl-canvas-frame-count)
   (frames-held-p
    :initform nil
    :accessor sdl-canvas-frames-held-p)))

(defstruct sdl-canvas-request
  function
  (completion (sb-thread:make-semaphore :count 0) :read-only t)
  values
  error)

(defmacro with-sdl-native-environment (&body body)
  "Run BODY with the floating-point environment expected by native drivers."
  #+sbcl
  `(sb-int:with-float-traps-masked
       (:invalid :divide-by-zero :overflow :underflow :inexact)
     ,@body)
  #+(and darwin (not sbcl))
  `(float-features:with-float-traps-masked t ,@body)
  #-(or sbcl darwin)
  `(progn ,@body))

(defparameter *runtime-signals-sdl-must-not-take*
  ;; SIGILL, SIGTRAP, SIGBUS, SIGFPE, SIGSEGV.
  '(4 5 7 8 11)
  "The signals SBCL's runtime handles itself, by number.

SDL's KMSDRM backend mutes the console keyboard and registers an emergency
restore handler over every fatal signal, SIGSEGV included.  SBCL takes ordinary
SIGSEGVs as part of running Lisp -- garbage collection's write barrier and the
control stack guard both arrive that way -- and SDL's handler answers one by
re-raising it with PTHREAD_KILL, whose SIGINFO carries no faulting address at
all.  What reaches SBCL is then a memory fault at (UID << 32) | PID: a
CORRUPTION WARNING and a MEMORY-FAULT-ERROR in place of a collection.")

(defconstant +sigaction-size+ 256
  "Room for one struct sigaction, which is 152 bytes on x86-64 Linux.

The bytes are only ever moved between the kernel and this buffer.")

(defun call-with-runtime-signal-handlers-preserved (function)
  "Call FUNCTION, restoring SBCL's own fatal-signal handlers afterwards.

Wrap the SDL calls that may install console-keyboard cleanup handlers: the
muting itself is worth keeping on a virtual console, but the handlers are not
SDL's to take.  SDL_Quit and the process's own exit still restore the keyboard."
  #+linux
  (let ((signals *runtime-signals-sdl-must-not-take*))
    (cffi:with-foreign-object (saved :char (* +sigaction-size+ (length signals)))
      (loop for signal in signals
            for index from 0
            do (cffi:foreign-funcall
                "sigaction" :int signal :pointer (cffi:null-pointer)
                :pointer (cffi:inc-pointer saved (* index +sigaction-size+))
                :int))
      (unwind-protect (funcall function)
        (loop for signal in signals
              for index from 0
              do (cffi:foreign-funcall
                  "sigaction" :int signal
                  :pointer (cffi:inc-pointer saved (* index +sigaction-size+))
                  :pointer (cffi:null-pointer)
                  :int)))))
  #-linux
  (funcall function))

(defmacro with-runtime-signal-handlers-preserved (&body body)
  `(call-with-runtime-signal-handlers-preserved (lambda () ,@body)))

(defun linux-console-tty-p ()
  "Whether this process has a Linux virtual console as its controlling terminal."
  #+linux
  (let ((name (ignore-errors
                (uiop:run-program '("tty") :output :string :error-output nil))))
    (when name
      (let ((name (string-trim '(#\Space #\Tab #\Newline #\Return) name)))
        (and (uiop:string-prefix-p "/dev/tty" name)
             (let ((number (subseq name (length "/dev/tty"))))
               (and (plusp (length number))
                    (every #'digit-char-p number)))))))
  #-linux
  nil)

(defun select-sdl-video-driver ()
  "Choose KMSDRM on a real console, otherwise a safe headless SDL backend.

An explicit SDL_VIDEODRIVER, DISPLAY, or WAYLAND_DISPLAY always wins.  This
must run before SDL video initialization: it covers standalone executables as
well as processes started through the Nix development shell."
  (when (and (null (uiop:getenv "SDL_VIDEODRIVER"))
             (null (uiop:getenv "DISPLAY"))
             (null (uiop:getenv "WAYLAND_DISPLAY")))
    (sb-posix:setenv "SDL_VIDEODRIVER"
                     (if (linux-console-tty-p) "kmsdrm" "offscreen")
                     1)))

(defun call-with-sdl-main-thread (function)
  "Call FUNCTION where synchronous SDL canvas work can use the native main thread.

On Darwin, a batch Lisp normally evaluates its toplevel form on the process
main thread.  Opening a canvas from that continuation would let Cocoa replace
the continuation with its durable event loop.  This boundary moves FUNCTION
to a worker while the calling thread runs TRIVIAL-MAIN-THREAD, then restores
the caller with FUNCTION's values or condition after native teardown.  Calls
from an existing worker (including SLY and standalone program workers) already
have a main-thread host and execute directly."
  (check-type function function)
  #+darwin
  (if (not (trivial-main-thread:main-thread-p))
      (funcall function)
      (let ((completion (sb-thread:make-semaphore :count 0))
            (values nil)
            (failure nil)
            (runner-started-p nil)
            (worker nil))
        (setf worker
              (sb-thread:make-thread
               (lambda ()
                 (unwind-protect
                      (handler-case
                          (progn
                            ;; Establish the runner before FUNCTION can ask a
                            ;; canvas to replace its task with SDL's event loop.
                            (trivial-main-thread:call-in-main-thread
                             (lambda () (values)) :blocking t)
                            (setf runner-started-p t
                                  values (multiple-value-list
                                          (funcall function))))
                        (error (condition)
                          (setf failure condition)))
                   (sb-thread:signal-semaphore completion)
                   (when runner-started-p
                     (trivial-main-thread:stop-main-runner))))
               :name "luv SDL batch operation"))
        ;; The worker's first main-thread call interrupts this wait and enters
        ;; the runner.  STOP-MAIN-RUNNER later restores this continuation.
        (sb-thread:wait-on-semaphore completion)
        (sb-thread:join-thread worker)
        (if failure
            (error failure)
            (values-list values))))
  #-darwin
  (funcall function))

(defgeneric prepare-sdl-canvas-host (canvas)
  (:documentation "Prepare the native application host before SDL_Init."))

(defgeneric activate-sdl-canvas-host (canvas)
  (:documentation "Show and activate CANVAS after its SDL window exists."))

(defgeneric deactivate-sdl-canvas-host (canvas)
  (:documentation "Release CANVAS's native application presence after SDL_Quit."))

(defgeneric claim-sdl-canvas-host (canvas)
  (:documentation
   "Claim any process-global native host required before CANVAS starts."))

(defgeneric release-sdl-canvas-host (canvas)
  (:documentation "Release a host previously claimed for CANVAS."))

(defmethod claim-sdl-canvas-host ((canvas canvas))
  (declare (ignore canvas))
  nil)

(defmethod release-sdl-canvas-host ((canvas canvas))
  (declare (ignore canvas))
  nil)

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

;; cl-sdl3's KEYCODE enum is currently incomplete (and mistypes ]), while SDL
;; keycodes are Unicode values by design. Keep the raw integer at this boundary.
(cffi:defcfun ("SDL_GetKeyFromScancode" raw-sdl-key-from-scancode) :uint32
  (scancode sdl3::scancode)
  (modstate sdl3::keymod)
  (key-event :bool))

(defgeneric sdl-presentation-window-flags (presentation-api)
  (:documentation
   "Return the immutable SDL window flags required by PRESENTATION-API."))

(defgeneric sdl-presentation-api-for (provider)
  (:documentation
   "Return the SDL presentation policy required by GPU PROVIDER."))

(defmethod sdl-presentation-window-flags ((presentation-api (eql :vulkan)))
  (declare (ignore presentation-api))
  ;; On macOS a point already names a sufficiently fine game-rendering pixel.
  ;; Asking SDL for high density doubles both drawable axes on Retina displays,
  ;; quadrupling scene work and making point-sized embedded UI unnecessarily
  ;; small.  Other platforms retain their native high-density drawable.
  (append '(:vulkan)
          #-darwin '(:high-pixel-density)
          '(:resizable :hidden)))

#+darwin
(defmethod sdl-presentation-window-flags ((presentation-api (eql :metal)))
  (declare (ignore presentation-api))
  '(:metal :resizable :hidden))

(defun sdl-canvas-window-flags (canvas)
  "The SDL window flags CANVAS needs: its presentation API plus its own state."
  (let ((flags (sdl-presentation-window-flags
                (sdl-canvas-presentation-api canvas))))
    (if (canvas-fullscreen-p canvas)
        (cons :fullscreen flags)
        flags)))

(defparameter *sdl-default-canvas-fill* 0.8
  "How much of a display's usable area an unsized window asks for.")

(defparameter *sdl-default-canvas-aspect* 1.6
  "The widest shape an unsized window takes before it stops following the
display.  An ultrawide desktop is a place to put several windows, not a
reason to open one 3.6:1 window nobody can look across.")

(defparameter *sdl-fallback-canvas-size* '(1280 800)
  "The window size to open when SDL cannot describe any display.")

(defun sdl-default-canvas-size ()
  "Return a comfortable window size for the primary display, in points.

SDL_GetDisplayUsableBounds already excludes the menu bar and the dock, so a
fraction of it is a window that fits wherever the desktop actually is.  The
height leads and the width follows it at an ordinary aspect, never wider than
the same fraction of the desktop.  The answer is in logical points, which is
what SDL_CreateWindow wants: a Retina window is the same physical size as its
low-density twin and simply resolves finer, so the density belongs in the
drawable rather than in this number."
  (let ((display (sdl3:get-primary-display)))
    (multiple-value-bind (success bounds)
        (if (zerop display)
            (values nil nil)
            (sdl3:get-display-usable-bounds display))
      (if success
          (let* ((height (max 480 (round (* *sdl-default-canvas-fill*
                                            (sdl3:%h bounds)))))
                 (width (max 640 (min (round (* *sdl-default-canvas-fill*
                                                (sdl3:%w bounds)))
                                      (round (* *sdl-default-canvas-aspect*
                                                height))))))
            (values width height))
          (values-list *sdl-fallback-canvas-size*)))))

(defun resolve-kmsdrm-vulkan-canvas-size (canvas)
  "Use the active display mode required by SDL's direct-display Vulkan WSI.

Unlike a desktop window system, KMSDRM presents Vulkan through VK_KHR_display.
SDL can only create that surface at a mode the display actually advertises;
arbitrary window dimensions commonly fail in SDL_Vulkan_CreateSurface."
  (when (and (eq :vulkan (sdl-canvas-presentation-api canvas))
             (string-equal "kmsdrm" (sdl3:get-current-video-driver)))
    (let* ((display (sdl3:get-primary-display))
           (mode (unless (zerop display)
                   (sdl3:get-current-display-mode display))))
      (when mode
        (setf (canvas-width canvas) (sdl3:%w mode)
              (canvas-height canvas) (sdl3:%h mode)
              (canvas-fullscreen-p canvas) t)))))

(defun resolve-sdl-canvas-size (canvas)
  "Fill in whichever of CANVAS's dimensions were left for the display to pick."
  (resolve-kmsdrm-vulkan-canvas-size canvas)
  (unless (and (canvas-width canvas) (canvas-height canvas))
    (multiple-value-bind (width height) (sdl-default-canvas-size)
      (setf (canvas-width canvas) (or (canvas-width canvas) width)
            (canvas-height canvas) (or (canvas-height canvas) height))))
  (values (canvas-width canvas) (canvas-height canvas)))

(defun sdl-canvas-direct-display-p (canvas)
  "Whether CANVAS is hosted directly by SDL's KMSDRM video driver."
  (flet ((direct-p ()
           (string-equal "kmsdrm" (sdl3:get-current-video-driver))))
    (if (sdl-canvas-native-thread-p canvas)
        (direct-p)
        (call-on-sdl-canvas-thread canvas #'direct-p))))

(defun sdl-canvas-refresh-rate (canvas)
  "Return CANVAS's current display refresh rate, or NIL when SDL omits it."
  (flet ((refresh-rate ()
           (let* ((window (sdl-canvas-window canvas))
                  (display
                    (if (and window (not (cffi:null-pointer-p window)))
                        (sdl3:get-display-for-window window)
                        (sdl3:get-primary-display)))
                  (mode (unless (zerop display)
                          (sdl3:get-current-display-mode display))))
             (when mode
               (let ((numerator (sdl3:%refresh-rate-numerator mode))
                     (denominator (sdl3:%refresh-rate-denominator mode))
                     (approximate (sdl3:%refresh-rate mode)))
                 (cond ((and (plusp numerator) (plusp denominator))
                        (/ (float numerator 1d0) denominator))
                       ((plusp approximate)
                        (float approximate 1d0))))))))
    (if (sdl-canvas-native-thread-p canvas)
        (refresh-rate)
        (call-on-sdl-canvas-thread canvas #'refresh-rate))))

(defmethod canvas-presentation-time ((canvas sdl-canvas))
  ;; Drawable acquisition has already happened when this is queried.  A
  ;; cadence frame is intended for its following beat; direct display is
  ;; paced by FIFO image release and uses the panel's real refresh interval.
  (let* ((clock (canvas-clock canvas))
         (rate (cond ((typep clock 'cadence-clock)
                      (clock-frames-per-second clock))
                     ((typep clock 'presentation-clock)
                      (sdl-canvas-refresh-rate canvas))))
         ;; Acquisition may have blocked since the event loop cached its
         ;; coherent NOW.  Prediction starts from a fresh underlying sample,
         ;; then the backend temporarily overrides logical time with it.
         (now (canvas-time-unadjusted canvas)))
    (if rate (+ now (/ 1d0 rate)) now)))

(defun make-sdl-canvas (&key (title "luv canvas") (width 800) (height 600)
                          x y (visible-p t) (fullscreen-p nil)
                          (clock (make-demand-clock))
                          (time (make-lazy-clock))
                          (presentation-api :vulkan))
  "Construct an unrealized SDL canvas.

WIDTH or HEIGHT may be NIL, which asks the display for a size when the window
is finally created."
  (make-instance 'sdl-canvas :title title :width width :height height
                              :x x :y y :visible-p visible-p
                              :clock clock :time time
                              :fullscreen-p fullscreen-p
                              :presentation-api presentation-api))

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

(defmethod canvas-logical-size ((canvas sdl-canvas))
  (if (eq :open (canvas-state canvas))
      (call-on-sdl-canvas-thread
       canvas
       (lambda ()
         (multiple-value-bind (success width height)
             (sdl3:get-window-size (sdl-canvas-window canvas))
           (unless success
             (error 'canvas-error :canvas canvas :operation :logical-size
                    :reason :native-size-failed :details (sdl3:get-error)))
           (values width height))))
      (values (canvas-width canvas) (canvas-height canvas))))

(defmethod set-canvas-relative-pointer-mode ((canvas sdl-canvas) enabled)
  (unless (eq :open (canvas-state canvas))
    (error 'canvas-state-error :canvas canvas :operation :relative-pointer-mode
           :reason :invalid-state :state (canvas-state canvas)
           :expected-state :open))
  (call-on-sdl-canvas-thread
   canvas
   (lambda ()
     (unless (sdl3:set-window-relative-mouse-mode
              (sdl-canvas-window canvas) (not (null enabled)))
       (error 'canvas-error :canvas canvas :operation :relative-pointer-mode
              :reason :native-pointer-mode-failed :details (sdl3:get-error)))
     (not (null enabled)))))

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

(zdefun (process-sdl-canvas-requests :zone :canvas/process-requests) (canvas)
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

(defun sdl-canvas-terminal-error (canvas)
  "Return the native-loop failure callers need, preserving its original type."
  (or (sdl-canvas-startup-error canvas)
      (make-condition 'canvas-error :canvas canvas
                      :operation :frame :reason :canvas-closed)))

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
    ;; Bounded, because the alternative is not "wait a little longer" but
    ;; "hang forever with nothing on screen".  The canvas thread services this
    ;; queue once a frame; if it has stopped doing that, no amount of waiting
    ;; will help and every caller after this one queues behind the same block.
    (unless (sb-thread:wait-on-semaphore
             (sdl-canvas-request-completion request)
             :timeout *canvas-dispatch-timeout*)
      (error 'canvas-dispatch-timeout
             :canvas canvas :operation :dispatch :reason :thread-unresponsive
             :seconds *canvas-dispatch-timeout*))
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
    (call-on-sdl-canvas-thread
     canvas
     (lambda ()
       ;; Top-level placement is only a request: Wayland and some other
       ;; window managers are entitled to reject it.
       (sdl3:set-window-position (sdl-canvas-window canvas) x y))))
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

(defmethod canvas-clipboard-text ((canvas sdl-canvas))
  ;; The clipboard belongs to the video subsystem, which only answers on
  ;; the thread that initialized it.
  (call-on-sdl-canvas-thread
   canvas
   (lambda ()
     (when (sdl3:has-clipboard-text)
       (let ((text (sdl3:get-clipboard-text)))
         (and (plusp (length text)) text))))))

(defmethod set-canvas-fullscreen ((canvas sdl-canvas) enabled)
  (let ((enabled (and enabled t)))
    (when (eq :open (canvas-state canvas))
      (call-sdl-canvas-window-operation
       canvas :fullscreen
       (lambda (window)
         (prog1 (sdl3:set-window-fullscreen window enabled)
           ;; The new extent arrives as an ordinary resize event; letting SDL
           ;; settle here keeps CANVAS-SIZE honest for a caller that looks
           ;; immediately after toggling.
           (sdl3:sync-window window)))))
    (setf (canvas-fullscreen-p canvas) enabled))
  canvas)

(defmethod raise-canvas ((canvas sdl-canvas))
  (call-sdl-canvas-window-operation canvas :raise #'sdl3:raise-window))

(defmethod minimize-canvas ((canvas sdl-canvas))
  (call-sdl-canvas-window-operation canvas :minimize #'sdl3:minimize-window))

(defmethod restore-canvas ((canvas sdl-canvas))
  (call-sdl-canvas-window-operation canvas :restore #'sdl3:restore-window))

(defmethod request-canvas-frame ((canvas sdl-canvas) function)
  ;; Dynamic bindings do not follow a closure onto another thread.  Preserve
  ;; the opt-in trace explicitly so a caller can measure the real native frame
  ;; rather than only the time spent waiting for its request.  #OHNIWM
  (let ((trace *cpu-trace*))
    (call-on-sdl-canvas-thread
     canvas
     (lambda ()
       (let ((*cpu-trace* trace))
         (funcall function (canvas-time canvas)))))))

;;; What went wrong, kept.
;;;
;;; The pump itself must not die of application errors: a frame that throws,
;;; a key handler that throws, a paint that throws.  Each of those runs under
;;; a guard that catches the condition with its backtrace still standing,
;;; retains it on the canvas, logs it, and lets the loop go on servicing the
;;; window.  A failed frame parks the frame clock -- rendering into a broken
;;; world every 16 ms would only bury the evidence -- until someone fixes the
;;; cause and asks for RESUME-CANVAS-FRAMES.  Anyone who changed the world
;;; and wants to know what the next frame made of it can FENCE-CANVAS and
;;; read what was retained since.

(defstruct canvas-failure
  "One error caught on a canvas loop, with everything needed to read it later."
  serial
  canvas
  phase
  universal-time
  tick
  condition
  report
  backtrace)

(defvar *canvas-failure-serial* 0
  "A count of every failure retained on any canvas, so a caller can mark a
moment and later ask what failed after it.")

(defvar *canvas-failure-lock*
  (sb-thread:make-mutex :name "luv canvas failure lock"))

(defparameter *canvas-failure-limit* 12
  "How many failures a canvas keeps; the oldest are forgotten.")

(defparameter *canvas-failure-backtrace-depth* 40
  "How many frames of backtrace a retained failure carries.")

(defvar *open-sdl-canvases* '()
  "Every SDL canvas whose loop is running, so a fence or a hold can find
them without being handed one.")

(defvar *open-sdl-canvases-lock*
  (sb-thread:make-mutex :name "luv open canvases lock"))

(defun open-canvases ()
  "The canvases whose native loops are running right now."
  (sb-thread:with-mutex (*open-sdl-canvases-lock*)
    (copy-list *open-sdl-canvases*)))

(defun note-sdl-canvas-open (canvas)
  (sb-thread:with-mutex (*open-sdl-canvases-lock*)
    (pushnew canvas *open-sdl-canvases*)))

(defun note-sdl-canvas-closed (canvas)
  (sb-thread:with-mutex (*open-sdl-canvases-lock*)
    (setf *open-sdl-canvases* (remove canvas *open-sdl-canvases*))))

(defparameter *canvas-failure-backtrace-floor*
  '("SDL-CANVAS-EVENT-LOOP" "PROCESS-SDL-CANVAS-REQUESTS" "MAIN-RUNNER")
  "Frames at which a retained backtrace stops: below them is the pump and
the image's own toplevel, the same in every failure and never the point.")

(defun capture-backtrace-string (&optional (depth *canvas-failure-backtrace-depth*))
  "The current backtrace as text, from the frame that asked for it down to
the pump: forms abbreviated, the frames of the capture itself left out."
  (let ((text (with-output-to-string (stream)
                (let ((*print-length* 6) (*print-level* 3)
                      (*print-pretty* nil))
                  (sb-debug:print-backtrace :stream stream :count depth
                                            :emergency-best-effort t)))))
    (with-output-to-string (out)
      (with-input-from-string (in text)
        (loop for line = (read-line in nil)
              for index from 0
              while line
              ;; The first line names the thread; then the capture, the
              ;; guard's handler, %SIGNAL, and ERROR itself.
              unless (and (plusp index) (< index 5)
                          (or (search "CAPTURE-BACKTRACE-STRING" line)
                              (search "%SIGNAL" line)
                              (search "(ERROR " line)
                              (search "(LAMBDA (CONDITION)" line)))
                do (write-line line out)
              when (some (lambda (floor) (search floor line))
                         *canvas-failure-backtrace-floor*)
                do (return))))))

(defun retain-canvas-failure (canvas phase condition backtrace)
  "Keep CONDITION, caught in PHASE on CANVAS with BACKTRACE, and log it."
  (let ((failure
          (make-canvas-failure
           :canvas canvas :phase phase
           :universal-time (get-universal-time)
           :tick (sdl-canvas-ticks canvas)
           :condition condition
           :report (handler-case (princ-to-string condition)
                     (error () (format nil "~S" (type-of condition))))
           :backtrace backtrace)))
    (sb-thread:with-mutex (*canvas-failure-lock*)
      (setf (canvas-failure-serial failure) (incf *canvas-failure-serial*))
      (setf (sdl-canvas-failures canvas)
            (subseq (cons failure (sdl-canvas-failures canvas))
                    0 (min *canvas-failure-limit*
                           (1+ (length (sdl-canvas-failures canvas)))))))
    (log-event :canvas "~S failed in ~(~A~): ~A~@[~%~A~]"
               (canvas-title canvas) phase
               (canvas-failure-report failure)
               backtrace)
    failure))

(defun canvas-failures (canvas)
  "CANVAS's retained failures, newest first."
  (sdl-canvas-failures canvas))

(defun canvas-failures-since (serial)
  "Every retained failure on any open canvas newer than SERIAL, oldest first."
  (sort (loop for canvas in (open-canvases)
              append (remove-if-not
                      (lambda (failure)
                        (> (canvas-failure-serial failure) serial))
                      (canvas-failures canvas)))
        #'< :key #'canvas-failure-serial))

(defun canvas-failure-serial-now ()
  "The failure serial as of now: pass it later to CANVAS-FAILURES-SINCE."
  (sb-thread:with-mutex (*canvas-failure-lock*)
    *canvas-failure-serial*))

(defun report-canvas-failure (failure &optional (stream *standard-output*))
  "Print FAILURE for a person: when, where, what, and the backtrace."
  (multiple-value-bind (second minute hour)
      (decode-universal-time (canvas-failure-universal-time failure))
    (format stream "~&~2,'0D:~2,'0D:~2,'0D canvas ~S failed in ~(~A~) ~
                    (loop iteration ~D):~%  ~A~%"
            hour minute second
            (canvas-title (canvas-failure-canvas failure))
            (canvas-failure-phase failure)
            (canvas-failure-tick failure)
            (canvas-failure-report failure))
    (when (canvas-failure-backtrace failure)
      (format stream "~A~%" (canvas-failure-backtrace failure)))))

(defmacro guarding-sdl-canvas ((canvas phase) &body body)
  "Run BODY on CANVAS's loop in PHASE; an error is retained, not raised.
Returns BODY's values, or NIL and the failure as a second value."
  (let ((backtrace (gensym "BACKTRACE"))
        (failure (gensym "FAILURE")))
    `(let ((,backtrace nil) (,failure nil))
       (values
        (handler-case
            (handler-bind ((error
                             (lambda (condition)
                               (declare (ignore condition))
                               (setf ,backtrace (capture-backtrace-string)))))
              ,@body)
          (error (condition)
            (setf ,failure
                  (retain-canvas-failure ,canvas ,phase condition ,backtrace))
            nil))
        ,failure))))

(defun resume-canvas-frames (canvas)
  "Let CANVAS run frames again after a frame failure parked them."
  (setf (sdl-canvas-frame-failure canvas) nil)
  (wake-sdl-canvas canvas)
  canvas)

(defun hold-canvas-frames (canvas)
  "Stop CANVAS running frames or delivering events; the window is still
pumped.  For the duration of a redefinition the loop must not run through."
  (setf (sdl-canvas-frames-held-p canvas) t)
  canvas)

(defun release-canvas-frames (canvas)
  (setf (sdl-canvas-frames-held-p canvas) nil)
  (wake-sdl-canvas canvas)
  canvas)

(defun call-with-canvas-frames-held (function &optional (canvases (open-canvases)))
  "Call FUNCTION with every canvas in CANVASES holding its frames and events."
  (unwind-protect
       (progn
         (dolist (canvas canvases) (hold-canvas-frames canvas))
         ;; A frame already under way finishes; wait for the loops to come
         ;; round to a phase that honours the hold.
         (dolist (canvas canvases)
           (fence-canvas canvas :frames 0 :timeout 1.0))
         (funcall function))
    (dolist (canvas canvases) (release-canvas-frames canvas))))

(defmacro with-canvas-frames-held (() &body body)
  `(call-with-canvas-frames-held (lambda () ,@body)))

(defun fence-canvas (canvas &key (frames 1) (timeout 5.0))
  "Wait until CANVAS has run FRAMES more frames -- or, with no frame clock,
come round its loop twice more -- and return :DONE; :FAILED as soon as a
frame failure parks it; :CLOSED if it closes; :TIMEOUT after TIMEOUT
seconds.  This is how a caller who just changed the world finds out what
the next frame made of the change."
  (let* ((start-frames (sdl-canvas-frame-count canvas))
         (start-ticks (sdl-canvas-ticks canvas))
         (cadence-p (typep (canvas-clock canvas) 'animated-clock))
         (deadline (+ (get-internal-real-time)
                      (* timeout internal-time-units-per-second))))
    (wake-sdl-canvas canvas)
    (loop
      (cond ((sdl-canvas-frame-failure canvas) (return :failed))
            ((not (member (canvas-state canvas) '(:opening :open)))
             (return :closed))
            ((if (and cadence-p (not (sdl-canvas-frames-held-p canvas)))
                 (>= (sdl-canvas-frame-count canvas) (+ start-frames frames))
                 (>= (sdl-canvas-ticks canvas) (+ start-ticks 2)))
             (return :done))
            ((> (get-internal-real-time) deadline) (return :timeout)))
      (sleep 0.002))))

(defun fence-canvases (&key (frames 1) (timeout 5.0))
  "FENCE-CANVAS every open canvas; an alist of canvas to outcome."
  (mapcar (lambda (canvas)
            (cons canvas (fence-canvas canvas :frames frames :timeout timeout)))
          (open-canvases)))

(defun sdl-canvas-window-event-p (canvas event)
  (= (sdl3:%window-id event)
     (sdl3:get-window-id (sdl-canvas-window canvas))))

(defun sdl-canvas-window-id-p (canvas window-id)
  (= window-id (sdl3:get-window-id (sdl-canvas-window canvas))))

(defun sdl-mouse-button-name (button)
  (case button
    (1 :left)
    (2 :middle)
    (3 :right)
    (4 :x1)
    (5 :x2)))

(defun dispatch-sdl-pointer-motion (canvas event class)
  (when (sdl-canvas-window-event-p canvas event)
    (dispatch-canvas-event
     canvas
     (make-instance class
                    :timestamp (sdl3:%timestamp event)
                    :x (sdl3:%x event)
                    :y (sdl3:%y event)
                    :delta-x (sdl3:%xrel event)
                    :delta-y (sdl3:%yrel event)))))

(defun dispatch-sdl-pointer-button (canvas event class)
  (when (sdl-canvas-window-event-p canvas event)
    (let ((button (sdl-mouse-button-name (sdl3:%button event))))
      (when button
        (dispatch-canvas-event
         canvas
         (make-instance class
                        :timestamp (sdl3:%timestamp event)
                        :x (sdl3:%x event)
                        :y (sdl3:%y event)
                        :button button
                        :clicks (sdl3:%clicks event)))))))

(defun dispatch-sdl-pointer-wheel (canvas event)
  "Turn one SDL wheel event into a canvas one.

SDL reports a flipped direction rather than flipped amounts when the platform
is set to natural scrolling, so the sign is corrected here and consumers only
ever see what the user meant."
  (when (sdl-canvas-window-event-p canvas event)
    ;; The struct reader hands the direction back as the enum's keyword
    ;; when it can translate it and as the raw integer when it cannot, so
    ;; both spellings of "flipped" are honoured; anything else is normal.
    (let* ((direction (sdl3:%direction event))
           (sign (if (or (eq direction :flipped)
                         (eql direction
                              (cffi:foreign-enum-value
                               'sdl3::mouse-wheel-direction :flipped)))
                     -1.0
                     1.0)))
      (dispatch-canvas-event
       canvas
       (make-instance 'canvas-pointer-wheel-event
                      :timestamp (sdl3:%timestamp event)
                      :x (sdl3:%mouse-x event)
                      :y (sdl3:%mouse-y event)
                      :scroll-x (* sign (sdl3:%x event))
                      :scroll-y (* sign (sdl3:%y event)))))))

(defun dispatch-sdl-pointer-boundary (canvas event class)
  (when (sdl-canvas-window-event-p canvas event)
    (multiple-value-bind (buttons x y) (sdl3:get-mouse-state)
      (declare (ignore buttons))
      (dispatch-canvas-event
       canvas
       (make-instance class
                      :timestamp (sdl3:%timestamp event)
                      :x x :y y)))))

(defun sdl-scancode-key-name (scancode)
  "Translate SDL's physical key names into luv's portable vocabulary."
  (case scancode
    (:minus (intern "-" "KEYWORD"))
    (:equals (intern "=" "KEYWORD"))
    (:leftbracket (intern "[" "KEYWORD"))
    (:rightbracket (intern "]" "KEYWORD"))
    (:backslash (intern "\\" "KEYWORD"))
    (:semicolon (intern ";" "KEYWORD"))
    (:apostrophe (intern "'" "KEYWORD"))
    (:grave (intern "`" "KEYWORD"))
    (:comma (intern "," "KEYWORD"))
    (:period (intern "." "KEYWORD"))
    (:slash (intern "/" "KEYWORD"))
    (:lctrl :control-left)
    (:lshift :shift-left)
    (:lalt :alt-left)
    (:lgui :super-left)
    (:rctrl :control-right)
    (:rshift :shift-right)
    (:ralt :alt-right)
    (:rgui :super-right)
    (:capslock :caps-lock)
    (:printscreen :print-screen)
    (:scrolllock :scroll-lock)
    (:numlockclear :num-lock)
    (:pageup :page-up)
    (:pagedown :page-down)
    (:application :menu)
    (otherwise scancode)))

(defun sdl-key-modifiers (modifiers)
  "Translate SDL's left/right modifier bitfield into logical modifiers."
  (labels ((present-p (&rest names)
             (some (lambda (name) (member name modifiers)) names)))
    (remove nil
            (list (and (present-p :lshift :rshift) :shift)
                  (and (present-p :lctrl :rctrl :ctrl) :control)
                  (and (present-p :lalt :ralt :alt) :meta)
                  (and (present-p :lgui :rgui :gui) :super)
                  (and (present-p :caps) :caps-lock)
                  (and (present-p :num) :num-lock)))))

;; SDL's KMSDRM backend never learns the console's keymap, so a Dvorak
;; console gets QWERTY characters from SDL_GetKeyFromScancode.  The layout
;; override translates scancodes itself; keys it does not list (digits,
;; space, editing keys) fall through to SDL, whose QWERTY answer matches.
(defparameter +dvorak-scancode-characters+
  '((:q . #\') (:w . #\,) (:e . #\.) (:r . #\p) (:t . #\y) (:y . #\f)
    (:u . #\g) (:i . #\c) (:o . #\r) (:p . #\l)
    (:leftbracket . #\/) (:rightbracket . #\=)
    (:a . #\a) (:s . #\o) (:d . #\e) (:f . #\u) (:g . #\i) (:h . #\d)
    (:j . #\h) (:k . #\t) (:l . #\n) (:semicolon . #\s) (:apostrophe . #\-)
    (:z . #\;) (:x . #\q) (:c . #\j) (:v . #\k) (:b . #\x) (:n . #\b)
    (:m . #\m) (:comma . #\w) (:period . #\v) (:slash . #\z)
    (:minus . #\[) (:equals . #\]))
  "Dvorak characters by physical scancode; unlisted keys match QWERTY.")

(defparameter +us-shifted-characters+
  '((#\1 . #\!) (#\2 . #\@) (#\3 . #\#) (#\4 . #\$) (#\5 . #\%)
    (#\6 . #\^) (#\7 . #\&) (#\8 . #\*) (#\9 . #\() (#\0 . #\))
    (#\' . #\") (#\, . #\<) (#\. . #\>) (#\; . #\:) (#\/ . #\?)
    (#\- . #\_) (#\= . #\+) (#\[ . #\{) (#\] . #\}) (#\` . #\~)
    (#\\ . #\|))
  "The US shift pairs, which Dvorak shares for its printing characters.")

(defvar *canvas-keyboard-layout* :environment
  "Character layout override: :DVORAK, NIL to trust SDL's keymap, or
:ENVIRONMENT to read LUV_KEYBOARD_LAYOUT at first use.")

(defvar *canvas-swap-caps-control* :environment
  "Whether the Caps Lock key acts as Control, as the console's
ctrl:swapcaps option intends: T, NIL, or :ENVIRONMENT to read
LUV_KEYBOARD_SWAP_CAPS_CONTROL at first use.  The physical Control
keys stay Control; nobody wants Caps Lock in a game.")

(defvar *canvas-caps-control-down-p* nil
  "Whether the Caps Lock key, remapped to Control, is currently held.
SDL's modifier state reports Caps Lock as a toggle rather than a held
key, so the swap has to track the key itself.")

(defun canvas-swap-caps-control-p ()
  (when (eq *canvas-swap-caps-control* :environment)
    (setf *canvas-swap-caps-control*
          (let ((value (uiop:getenv "LUV_KEYBOARD_SWAP_CAPS_CONTROL")))
            (and value (not (member value '("" "0") :test #'string=))))))
  *canvas-swap-caps-control*)

(defun sdl-swapped-key-modifiers (modifiers)
  "Logical modifiers for MODIFIERS with the Caps-as-Control swap applied."
  (let ((logical (sdl-key-modifiers modifiers)))
    (if (canvas-swap-caps-control-p)
        (let ((cleaned (remove :caps-lock logical)))
          (if *canvas-caps-control-down-p*
              (adjoin :control cleaned)
              cleaned))
        logical)))

(defun canvas-keyboard-layout ()
  (when (eq *canvas-keyboard-layout* :environment)
    (setf *canvas-keyboard-layout*
          (let ((name (uiop:getenv "LUV_KEYBOARD_LAYOUT")))
            (cond ((null name) nil)
                  ((string-equal name "dvorak") :dvorak)
                  (t (warn "Ignoring unknown LUV_KEYBOARD_LAYOUT ~S." name)
                     nil)))))
  *canvas-keyboard-layout*)

(defun layout-key-character (scancode modifiers)
  "Return SCANCODE's character under the override layout, or NIL to ask SDL."
  (case (canvas-keyboard-layout)
    (:dvorak
     (let ((base (cdr (assoc scancode +dvorak-scancode-characters+))))
       (when base
         (if (intersection '(:lshift :rshift :shift) modifiers)
             (if (alpha-char-p base)
                 (char-upcase base)
                 (or (cdr (assoc base +us-shifted-characters+)) base))
             base))))))

(defun sdl-key-character (scancode modifiers)
  "Return SCANCODE's character under the configured layout and MODIFIERS."
  (or (layout-key-character scancode modifiers)
      (let ((code (raw-sdl-key-from-scancode scancode modifiers nil)))
        (when (or (member code '(8 9 13 27 127))
                  (<= 32 code (1- char-code-limit)))
          (code-char code)))))

(zdefun (dispatch-sdl-key
         :zone :canvas/key-event
         :value (if (eq class 'canvas-key-press-event) 1 0))
    (canvas event class)
  ;; Read fields directly from SDL_Event. Materializing cl-sdl3's
  ;; KEYBOARD-EVENT would translate its KEY slot through the broken enum even
  ;; though luv intentionally derives characters from SCANCODE and MOD.
  (let* ((type '(:struct sdl3:keyboard-event))
         (window-id (cffi:foreign-slot-value event type 'sdl3::%window-id)))
    (when (sdl-canvas-window-id-p canvas window-id)
      (let* ((raw-scancode
               (cffi:foreign-slot-value event type 'sdl3::%scancode))
             (swapped-p (and (eq raw-scancode :capslock)
                             (canvas-swap-caps-control-p)))
             (scancode (if swapped-p :lctrl raw-scancode))
             (modifiers (cffi:foreign-slot-value event type 'sdl3::%mod))
             (key-name (sdl-scancode-key-name scancode)))
        (when swapped-p
          (setf *canvas-caps-control-down-p*
                (eq class 'canvas-key-press-event)))
        (unless (eq key-name :unknown)
          (dispatch-canvas-event
           canvas
           (make-instance class
                          :timestamp
                          (cffi:foreign-slot-value event type 'sdl3::%timestamp)
                          :key-name key-name
                          :modifiers (sdl-swapped-key-modifiers modifiers)
                          :character (sdl-key-character scancode modifiers)
                          :unshifted-character (sdl-key-character scancode nil)
                          :repeat-p
                          (cffi:foreign-slot-value
                           event type 'sdl3::%repeat))))))))

(defun dispatch-sdl-window-event (canvas event class)
  (when (sdl-canvas-window-event-p canvas event)
    (dispatch-canvas-event
     canvas
     (make-instance class :timestamp (sdl3:%timestamp event)))))

(defun dispatch-sdl-window-size-event (canvas event class)
  (when (sdl-canvas-window-event-p canvas event)
    (dispatch-canvas-event
     canvas
     (make-instance class
                    :timestamp (sdl3:%timestamp event)
                    :width (sdl3:%data-1 event)
                    :height (sdl3:%data-2 event)))))

(defun dispatch-sdl-canvas-close-request (canvas timestamp)
  "Offer a native close to CANVAS and apply the default immediate policy.

An application returning :DEFER-CANVAS-CLOSE has begun orderly teardown on
another thread; its eventual CLOSE-CANVAS call ends the loop.  This lets it
release resources layered above the presentation context before SDL destroys
that context.  Every other handler retains the ordinary native close policy."
  (unless (eq :defer-canvas-close
              (dispatch-canvas-event
               canvas
               (make-instance 'canvas-window-close-request-event
                              :timestamp timestamp)))
    (setf (sdl-canvas-close-requested-p canvas) t)))

(defun synchronize-sdl-canvas-logical-size (canvas timestamp)
  "Publish SDL's current logical size when it changed without RESIZED first."
  (multiple-value-bind (width height) (canvas-logical-size canvas)
    (unless (and (= width (canvas-width canvas))
                 (= height (canvas-height canvas)))
      (setf (canvas-width canvas) width
            (canvas-height canvas) height)
      (dispatch-canvas-event
       canvas
       (make-instance 'canvas-window-resized-event
                      :timestamp timestamp :width width :height height)))))

(zdefun (handle-sdl-canvas-event
         :zone :canvas/event
         :value event-type)
    (canvas event event-type)
  ;; Keep the raw event tag: SDL_RegisterEvents may return a value absent from
  ;; cl-sdl3's static enum. Decode only the native events we understand.
  (cond
    ((= event-type (cffi:foreign-enum-value 'sdl3::event-type :quit))
     ;; macOS Command-Q is SDL_EVENT_QUIT rather than a window-close event.
     ;; Give it the identical application teardown path.
     (dispatch-sdl-canvas-close-request
      canvas
      (cffi:foreign-slot-value event '(:struct sdl3:common-event)
                               'sdl3::%timestamp)))
    ((member event-type
             (list
              (cffi:foreign-enum-value 'sdl3::event-type
                                       :window-close-requested)
              (cffi:foreign-enum-value 'sdl3::event-type
                                       :window-resized)
              (cffi:foreign-enum-value 'sdl3::event-type
                                       :window-pixel-size-changed)
              (cffi:foreign-enum-value 'sdl3::event-type
                                       :window-mouse-enter)
              (cffi:foreign-enum-value 'sdl3::event-type
                                       :window-mouse-leave)
              (cffi:foreign-enum-value 'sdl3::event-type
                                       :window-focus-gained)
              (cffi:foreign-enum-value 'sdl3::event-type
                                       :window-focus-lost)))
     (let ((window-event
             (cffi:mem-ref event '(:struct sdl3:window-event))))
       (when (sdl-canvas-window-event-p canvas window-event)
         (cond
           ((= event-type
               (cffi:foreign-enum-value 'sdl3::event-type
                                        :window-close-requested))
            (dispatch-sdl-canvas-close-request
             canvas (sdl3:%timestamp window-event)))
           ((= event-type
               (cffi:foreign-enum-value 'sdl3::event-type
                                        :window-resized))
            (setf (canvas-width canvas) (sdl3:%data-1 window-event)
                  (canvas-height canvas) (sdl3:%data-2 window-event))
            (dispatch-sdl-window-size-event
             canvas window-event 'canvas-window-resized-event))
           ((= event-type
               (cffi:foreign-enum-value 'sdl3::event-type
                                        :window-pixel-size-changed))
            ;; Wayland may report the new pixel extent before the corresponding
            ;; logical resize. Querying here keeps layout ahead of repaint.
            (synchronize-sdl-canvas-logical-size
             canvas (sdl3:%timestamp window-event))
            (dispatch-sdl-window-size-event
             canvas window-event 'canvas-window-pixel-size-changed-event))
           ((= event-type
               (cffi:foreign-enum-value 'sdl3::event-type
                                        :window-mouse-enter))
            (dispatch-sdl-pointer-boundary
             canvas window-event 'canvas-pointer-enter-event))
           ((= event-type
               (cffi:foreign-enum-value 'sdl3::event-type
                                        :window-mouse-leave))
            (dispatch-sdl-pointer-boundary
             canvas window-event 'canvas-pointer-exit-event))
           ((= event-type
               (cffi:foreign-enum-value 'sdl3::event-type
                                        :window-focus-gained))
            (dispatch-sdl-window-event
             canvas window-event 'canvas-window-focus-gained-event))
           (t
            (dispatch-sdl-window-event
             canvas window-event 'canvas-window-focus-lost-event))))))
    ((= event-type
        (cffi:foreign-enum-value 'sdl3::event-type :key-down))
     (dispatch-sdl-key
      canvas event 'canvas-key-press-event))
    ((= event-type
        (cffi:foreign-enum-value 'sdl3::event-type :key-up))
     (dispatch-sdl-key
      canvas event 'canvas-key-release-event))
    ((= event-type
        (cffi:foreign-enum-value 'sdl3::event-type :mouse-motion))
     (dispatch-sdl-pointer-motion
      canvas
      (cffi:mem-ref event '(:struct sdl3:mouse-motion-event))
      'canvas-pointer-motion-event))
    ((= event-type
        (cffi:foreign-enum-value 'sdl3::event-type :mouse-button-down))
     (dispatch-sdl-pointer-button
      canvas
      (cffi:mem-ref event '(:struct sdl3:mouse-button-event))
      'canvas-pointer-button-press-event))
    ((= event-type
        (cffi:foreign-enum-value 'sdl3::event-type :mouse-button-up))
     (dispatch-sdl-pointer-button
      canvas
      (cffi:mem-ref event '(:struct sdl3:mouse-button-event))
      'canvas-pointer-button-release-event))
    ((= event-type
        (cffi:foreign-enum-value 'sdl3::event-type :mouse-wheel))
     (dispatch-sdl-pointer-wheel
      canvas
      (cffi:mem-ref event '(:struct sdl3:mouse-wheel-event))))))

(defun poll-sdl-canvas-event (canvas)
  (cffi:with-foreign-object (event '(:union sdl3:event))
    (when (sdl3:poll-event event)
      (guarding-sdl-canvas (canvas :events)
        (handle-sdl-canvas-event canvas event (cffi:mem-ref event :uint32)))
      t)))

(zdefun (drain-sdl-canvas-events :zone :canvas/drain-events) (canvas)
  (loop while (poll-sdl-canvas-event canvas)))

;;; Where the loop is, published for anyone who wants to know whether it is
;;; still moving.  Two slot writes per phase: this runs on the thread whose
;;; responsiveness is the whole question, so it may not allocate, lock, or
;;; call out.

(defun enter-sdl-canvas-phase (canvas phase)
  "Record that CANVAS's native loop has just entered PHASE."
  ;; The time is written first: a reader that catches the pair mid-update
  ;; then sees the old phase with a fresh clock, and so underestimates a
  ;; stall by one phase rather than inventing one.
  (setf (sdl-canvas-phase-time canvas) (get-internal-real-time)
        (sdl-canvas-phase canvas) phase))

(defun sdl-canvas-phase-seconds (canvas)
  "How long CANVAS has been in its current phase, or NIL before it started."
  (let ((entered (sdl-canvas-phase-time canvas)))
    (when entered
      (/ (float (- (get-internal-real-time) entered) 1d0)
         internal-time-units-per-second))))

(defparameter *canvas-event-wait-slice* 0.25
  "The longest a canvas ever stays inside SDL waiting for an event.

A loop that can block indefinitely cannot be observed: it looks exactly like
a loop that has died, and a demand-clock canvas used to do precisely that by
asking SDL_WaitEvent for no deadline at all.  Every wait is now a step this
long at most, so the loop always comes up for air, always republishes where
it is, and a watchdog can tell a parked canvas from a wedged one by the only
evidence that matters: whether the loop came back.")

(defun wait-for-sdl-canvas-event (canvas timeout)
  (cffi:with-foreign-object (event '(:union sdl3:event))
    (when (sdl3:wait-event-timeout event timeout)
      ;; Handling belongs to its own phase: an event handler runs application
      ;; code -- a keystroke into the terminal wall, a click into an overlay
      ;; -- and is a place the loop can be lost for reasons that have nothing
      ;; to do with waiting.
      (enter-sdl-canvas-phase canvas :events)
      (guarding-sdl-canvas (canvas :events)
        (handle-sdl-canvas-event canvas event (cffi:mem-ref event :uint32)))
      (drain-sdl-canvas-events canvas))))

(defun sdl-canvas-wait-milliseconds (canvas timestamp)
  "How many milliseconds this iteration may spend inside SDL's event wait."
  (let ((slice (max 1 (round (* 1000 *canvas-event-wait-slice*))))
        (requested (clock-wait-timeout (canvas-clock canvas) timestamp)))
    (if requested
        (max 0 (min requested slice))
        slice)))

(zdefun (run-sdl-canvas-frame :zone :canvas/service-frame)
    (canvas timestamp)
  "Service the clock once, under guard.  A frame that fails parks the clock
in FRAME-FAILURE; a frame that runs counts."
  (multiple-value-bind (ran-p failure)
      (guarding-sdl-canvas (canvas :frame)
        (service-canvas-clock (canvas-clock canvas) canvas timestamp))
    (cond (failure
           (setf (sdl-canvas-frame-failure canvas) failure))
          (ran-p
           (incf (sdl-canvas-frame-count canvas))))))

(defun sdl-canvas-event-loop (canvas)
  (loop until (sdl-canvas-close-requested-p canvas)
        do (enter-sdl-canvas-phase canvas :requests)
           (process-sdl-canvas-requests canvas)
           (let ((timestamp (canvas-time canvas))
                 ;; Held or parked, the loop still pumps the window; it only
                 ;; stops running the application through it.
                 (*canvas-events-held-p*
                   (or (sdl-canvas-frames-held-p canvas)
                       (and (sdl-canvas-frame-failure canvas) t))))
             (enter-sdl-canvas-phase canvas :frame)
             (unless *canvas-events-held-p*
               (run-sdl-canvas-frame canvas timestamp))
             (unless (sdl-canvas-close-requested-p canvas)
               (enter-sdl-canvas-phase canvas :waiting)
               (let ((milliseconds
                       (if *canvas-events-held-p*
                           (max 1
                                (round (* 1000 *canvas-event-wait-slice*)))
                           (sdl-canvas-wait-milliseconds
                            canvas (canvas-time canvas)))))
                 ;; The next event or loop turn deserves a fresh real time.
                 ;; If an event handler asks first, its answer remains stable
                 ;; through the following request and frame phases.
                 (clear-canvas-time canvas)
                 (wait-for-sdl-canvas-event canvas milliseconds))))
           (incf (sdl-canvas-ticks canvas))))

;;; The watchdog.
;;;
;;; A window whose loop has stopped is not merely idle: macOS paints the
;;; beachball over it, it answers nothing, and the only way out is finding
;;; the process and killing it by hand.  Every stall reaching the desktop
;;; this way has cost someone a hunt, so the loop is now watched from a
;;; thread that cannot be caught by the same block -- the phases above are
;;; all bounded, so a phase that stops advancing means the loop is gone, and
;;; that is a fact rather than a guess.
;;;
;;; Three deadlines, each doing strictly more than the last: say so, take a
;;; native sample while the evidence still exists, and finally end the
;;; process so that no beachballing window outlives the Lisp that made it.

(defparameter *canvas-watchdog-interval* 0.5
  "How often the watchdog looks at the canvas it is watching.")

(defparameter *canvas-watchdog-warn-seconds* 2.0
  "How long one phase may last before the watchdog starts saying so.

Nothing in a healthy loop takes this long: a frame is milliseconds, a
request is serviced within one, and a wait is capped by
*CANVAS-EVENT-WAIT-SLICE*.  Two seconds is already far past anything that
has ever been legitimate here.")

(defparameter *canvas-watchdog-sample-seconds* 6.0
  "How long a stall lasts before the watchdog samples the process natively.

The sample is the part that is impossible to recover afterwards: once the
image is killed, or restarted by whoever notices the window, the stack that
would have named the deadlock is gone.")

(defparameter *canvas-watchdog-fatal-seconds* 30.0
  "How long a stall lasts before the image ends itself, or NIL to never.

A wedged canvas thread on Darwin is the main thread, so the image is already
unusable: every canvas call times out and the window answers nothing.  Dying
is what keeps the desktop clean, and ./sly start brings back a working
image in seconds.")

(defvar *canvas-watchdog-inhibit-hook* nil
  "A function of no arguments; true means never end the process.

An attached debugger stopped inside a frame callback is a stall by every
measure this file can take, and is also exactly what someone debugging the
render loop wants.  SLY-SERVER.LISP points this at a live Slynk connection
check, so an image someone is holding open only ever gets logged at.")

(defun sdl-canvas-watched-phase-p (phase)
  "Whether PHASE is one the event loop guarantees to leave promptly."
  (member phase '(:requests :frame :waiting :events)))

(defun sdl-canvas-fatal-deadline (canvas)
  "How long CANVAS may stall before the image ends itself, or NIL for never.

Only a canvas with a window on screen is worth dying over: that window is
the thing that beachballs, outlives its Lisp in the user's face, and has to
be hunted down and killed by hand.  A hidden canvas -- a capture, a
benchmark, a smoke test -- is watched and sampled just the same, but a slow
first frame there is somebody's build, not somebody's desktop."
  (and *canvas-watchdog-fatal-seconds*
       (canvas-visible-p canvas)
       *canvas-watchdog-fatal-seconds*))

(defmethod canvas-stalled-seconds ((canvas sdl-canvas))
  (let ((seconds (sdl-canvas-phase-seconds canvas)))
    (when (and seconds
               (sdl-canvas-watched-phase-p (sdl-canvas-phase canvas))
               (> seconds *canvas-watchdog-warn-seconds*))
      seconds)))

(defmethod canvas-health ((canvas sdl-canvas))
  (let ((failure (sdl-canvas-frame-failure canvas)))
    (list :state (canvas-state canvas)
          :phase (sdl-canvas-phase canvas)
          :phase-seconds (sdl-canvas-phase-seconds canvas)
          :ticks (sdl-canvas-ticks canvas)
          :frames (sdl-canvas-frame-count canvas)
          :stalled-p (and (canvas-stalled-seconds canvas) t)
          :held-p (sdl-canvas-frames-held-p canvas)
          :failure-count (length (sdl-canvas-failures canvas))
          :frame-failure
          (and failure
               (list :report (canvas-failure-report failure)
                     :phase (canvas-failure-phase failure)
                     :universal-time (canvas-failure-universal-time failure)
                     :tick (canvas-failure-tick failure))))))

(defun canvas-stall-sample-pathname (canvas)
  (declare (ignore canvas))
  (let ((name (format nil "canvas-stall-~D-~D.txt"
                      (sb-unix:unix-getpid) (get-universal-time))))
    (merge-pathnames
     name
     (or (ignore-errors
          (let ((directory (asdf:system-relative-pathname "luv" "build/")))
            (ensure-directories-exist directory)))
         (uiop:temporary-directory)))))

(defun sample-stalled-canvas-process (canvas seconds)
  "Write a native sample of this process while the stall is still happening."
  #-darwin
  (progn
    (log-event :watchdog
               "no native sampler on this platform; stall of ~,1F s unsampled"
               seconds)
    nil)
  #+darwin
  (let ((pathname (canvas-stall-sample-pathname canvas)))
    (handler-case
        (progn
          (uiop:run-program
           (list "sample" (princ-to-string (sb-unix:unix-getpid))
                 "1" "-file" (namestring pathname))
           :output nil :error-output nil :ignore-error-status t)
          (log-event :watchdog
                     "sampled the process ~,1F s into the stall: ~A"
                     seconds (namestring pathname))
          pathname)
      (error (condition)
        (log-event :watchdog "could not sample the stalled process: ~A"
                   condition)
        nil))))

(defun end-stalled-canvas-process (canvas seconds)
  (log-event
   :watchdog
   "canvas ~S has been in ~S for ~,1F s; ending this image so its window ~
does not outlive it. Bind LUV:*CANVAS-WATCHDOG-FATAL-SECONDS* to NIL to keep ~
a stalled image for inspection."
   (canvas-title canvas) (sdl-canvas-phase canvas) seconds)
  (finish-output *error-output*)
  ;; Unwinding would need the very thread that is stuck, and every ordinary
  ;; teardown path goes through it.  Leave abruptly instead: the window dies
  ;; with the process, which is the entire point.
  (sb-ext:exit :code 70 :abort t))

(defun watch-sdl-canvas (canvas)
  "Watch CANVAS's native loop until it closes, escalating while it is stuck."
  (let ((announced 0.0)
        (sampled-p nil))
    (loop while (member (canvas-state canvas) '(:opening :open))
          do (sleep *canvas-watchdog-interval*)
             (let ((seconds (canvas-stalled-seconds canvas)))
               (cond
                 ((null seconds)
                  (when (plusp announced)
                    (log-event :watchdog "canvas ~S is answering again"
                               (canvas-title canvas))
                    (setf announced 0.0 sampled-p nil)))
                 (t
                  (let ((fatal (sdl-canvas-fatal-deadline canvas)))
                    (when (>= seconds
                              (+ announced *canvas-watchdog-warn-seconds*))
                      (setf announced seconds)
                      (log-event
                       :watchdog
                       "canvas ~S has been in ~S for ~,1F s and is not ~
servicing its window~@[; ending this image in ~,1F s unless it recovers~]"
                       (canvas-title canvas) (sdl-canvas-phase canvas) seconds
                       (when fatal (max 0.0 (- fatal seconds)))))
                    (when (and (not sampled-p)
                               (>= seconds *canvas-watchdog-sample-seconds*))
                      (setf sampled-p t)
                      (sample-stalled-canvas-process canvas seconds))
                    (when (and fatal
                               (>= seconds fatal)
                               (not (and *canvas-watchdog-inhibit-hook*
                                         (ignore-errors
                                          (funcall
                                           *canvas-watchdog-inhibit-hook*)))))
                      (end-stalled-canvas-process canvas seconds)))))))))

(defun start-sdl-canvas-watchdog (canvas)
  (setf (sdl-canvas-watchdog canvas)
        (sb-thread:make-thread
         (lambda ()
           (handler-case (watch-sdl-canvas canvas)
             (error (condition)
               (log-event :watchdog "watchdog for ~S stopped: ~A"
                          (canvas-title canvas) condition))))
         :name "luv canvas watchdog")))

(defun stop-sdl-canvas-watchdog (canvas)
  (let ((thread (sdl-canvas-watchdog canvas)))
    (setf (sdl-canvas-watchdog canvas) nil)
    (when (and thread (sb-thread:thread-alive-p thread))
      ;; The watchdog leaves on its own once the state is no longer open; it
      ;; only ever sleeps for an interval, so this join is bounded by that.
      (ignore-errors
       (sb-thread:join-thread
        thread :timeout (* 4 *canvas-watchdog-interval*) :default nil))))
  (values))

(defun run-sdl-canvas (canvas)
  (with-sdl-native-environment
    (let ((sdl-initialized-p nil))
      (unwind-protect
           (handler-case
               (progn
                 (prepare-sdl-canvas-host canvas)
                 (select-sdl-video-driver)
                 (unless (with-runtime-signal-handlers-preserved
                           (sdl3:init :video))
                   (error "SDL video initialization failed: ~A"
                          (sdl3:get-error)))
                 (setf sdl-initialized-p t)
                 (let ((wake-event-type (sdl3:register-events 1)))
                   (when (zerop wake-event-type)
                     (error "SDL user event registration failed: ~A"
                            (sdl3:get-error)))
                   (setf (sdl-canvas-wake-event-type canvas) wake-event-type))
                 (let ((window
                         (multiple-value-bind (width height)
                             (resolve-sdl-canvas-size canvas)
                           ;; KMSDRM's console keyboard support installs its
                           ;; own fatal-signal handlers here.
                           (with-runtime-signal-handlers-preserved
                             (sdl3:create-window
                              (canvas-title canvas) width height
                              (sdl-canvas-window-flags canvas))))))
                   (when (cffi:null-pointer-p window)
                     (error "SDL window creation failed: ~A" (sdl3:get-error)))
                   (setf (sdl-canvas-window canvas) window)
                   (when (and (sdl-canvas-x canvas) (sdl-canvas-y canvas))
                     ;; Wayland and some other window managers deliberately
                     ;; deny applications control over top-level placement.
                     (sdl3:set-window-position
                      window
                      (sdl-canvas-x canvas) (sdl-canvas-y canvas)))
                   (activate-sdl-canvas-host canvas)
                   (enter-sdl-canvas-phase canvas :requests)
                   (setf (canvas-state canvas) :open)
                   (multiple-value-bind (width height) (canvas-size canvas)
                     (log-event :canvas "opened ~S, ~Dx~D pixels, ~(~A~)"
                                (canvas-title canvas) width height
                                (sdl-canvas-presentation-api canvas)))
                   (start-sdl-canvas-watchdog canvas)
                   (note-sdl-canvas-open canvas)
                   (sb-thread:signal-semaphore
                    (sdl-canvas-startup-completion canvas))
                   (sdl-canvas-event-loop canvas)))
             (error (condition)
               ;; Only what the guards above cannot catch reaches here:
               ;; startup, and the loop's own machinery.
               (log-event :canvas "~S failed outside any guard: ~A"
                          (canvas-title canvas) condition)
               (setf (sdl-canvas-startup-error canvas) condition)
               (when (eq :opening (canvas-state canvas))
                 (sb-thread:signal-semaphore
                  (sdl-canvas-startup-completion canvas)))))
        ;; Teardown is deliberately outside the watchdog's reach: it runs on
        ;; this same thread with the loop already gone, so its phases would
        ;; look like a stall, and killing the image in the middle of
        ;; releasing a device would trade a clean window for a dirty exit.
        (enter-sdl-canvas-phase canvas :closing)
        (setf (canvas-state canvas) :closing)
        (note-sdl-canvas-closed canvas)
        (stop-sdl-canvas-watchdog canvas)
        (log-event :canvas "closing ~S after ~D loop iterations"
                   (canvas-title canvas) (sdl-canvas-ticks canvas))
        (when (canvas-context canvas)
          (handler-case
              (destroy-canvas-context (canvas-context canvas))
            (error (condition)
              (unless (sdl-canvas-startup-error canvas)
                (setf (sdl-canvas-startup-error canvas) condition))))
          (setf (canvas-context canvas) nil))
        (when (sdl-canvas-window canvas)
          (sdl3:destroy-window (sdl-canvas-window canvas))
          (setf (sdl-canvas-window canvas) nil)
          ;; The window is gone from SDL; give the native loop a few turns
          ;; so the desktop learns it too, rather than leaving a ghost that
          ;; beachballs until the process ends.
          (loop repeat 5 do (sdl3:pump-events) (sleep 0.005)))
        (when sdl-initialized-p
          (sdl3:quit)
          (handler-case
              (deactivate-sdl-canvas-host canvas)
            (error (condition)
              (unless (sdl-canvas-startup-error canvas)
                (setf (sdl-canvas-startup-error canvas) condition)))))
        (setf (sdl-canvas-wake-event-type canvas) nil)
        ;; If event dispatch or rendering killed the native loop, wake every
        ;; synchronous caller with that exact condition.  Replacing it with a
        ;; generic :CANVAS-CLOSED made the actionable error available only by
        ;; inspecting SDL-CANVAS-STARTUP-ERROR after the fact.
        (fail-sdl-canvas-requests canvas (sdl-canvas-terminal-error canvas))
        (enter-sdl-canvas-phase canvas :closed)
        (setf (canvas-state canvas) :closed)
        (log-event :canvas "closed ~S" (canvas-title canvas))
        ;; CLOSE-CANVAS's completion is also permission for its caller to open
        ;; a replacement.  Publish native-host release before waking it.
        (release-sdl-canvas-host canvas)
        (sb-thread:signal-semaphore
         (sdl-canvas-shutdown-completion canvas))))))

(defun start-sdl-canvas-thread (canvas)
  #+darwin
  (progn
    ;; Claim synchronously.  CALL-IN-MAIN-THREAD cannot run a second canvas
    ;; while the first one owns SDL's durable Cocoa event loop, so queueing it
    ;; would leave OPEN-CANVAS waiting forever with no possible signaller.
    (claim-sdl-canvas-host canvas)
    (handler-case
        (progn
          (setf (sdl-canvas-thread canvas) (trivial-main-thread:main-thread))
          (sb-thread:make-thread
           (lambda ()
             (handler-case
                 (trivial-main-thread:call-in-main-thread
                  ;; CALL-IN-MAIN-THREAD is intentionally nonblocking here:
                  ;; this dispatcher returns once the work is queued.  Keep
                  ;; host ownership around the queued function itself.
                  (lambda ()
                    (unwind-protect
                         (run-sdl-canvas canvas)
                      (release-sdl-canvas-host canvas))))
               (error (condition)
                 ;; A dispatcher failure happens outside RUN-SDL-CANVAS's
                 ;; lifecycle handler.  Publish it to both waiters rather
                 ;; than leaving OPEN-CANVAS or CLOSE-CANVAS asleep.
                 (release-sdl-canvas-host canvas)
                 (setf (sdl-canvas-startup-error canvas) condition
                       (canvas-state canvas) :closed)
                 (sb-thread:signal-semaphore
                  (sdl-canvas-startup-completion canvas))
                 (sb-thread:signal-semaphore
                  (sdl-canvas-shutdown-completion canvas)))))
           :name "luv SDL Cocoa dispatcher"))
      (error (condition)
        (release-sdl-canvas-host canvas)
        (error condition))))
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
  (handler-case
      (start-sdl-canvas-thread canvas)
    (error (condition)
      ;; Keep a rejected host claim out of the :OPENING state.  In particular,
      ;; callers such as REALIZE-MIRROR may reasonably attempt cleanup after
      ;; OPEN-CANVAS signals; CLOSE-CANVAS must not wait for a thread that was
      ;; never started.
      (setf (sdl-canvas-startup-error canvas) condition
            (canvas-state canvas) :closed)
      (error condition)))
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
