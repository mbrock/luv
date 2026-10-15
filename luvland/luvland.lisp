;;;; Luvland: client windows as quads on a strip the camera travels along.

(in-package #:luvland)

(defvar *luvland* nil "The running Luvland, if any.")

;;; Column-major 4x4 matrices as 16 single-floats, as :MAT4 uniforms read them.

(defun mat4 (&rest columns)
  (let ((matrix (make-array 16 :element-type 'single-float)))
    (loop for value in columns
          for index from 0
          do (setf (aref matrix index) (coerce value 'single-float)))
    matrix))

(defun mat4* (a b)
  (let ((product (make-array 16 :element-type 'single-float :initial-element 0.0)))
    (dotimes (column 4 product)
      (dotimes (row 4)
        (setf (aref product (+ row (* 4 column)))
              (loop for k below 4
                    sum (* (aref a (+ row (* 4 k)))
                           (aref b (+ k (* 4 column))))))))))

(defun perspective (fov-y aspect near far)
  "Right-handed, looking down -Z, depth 0..1, with Vulkan's downward clip Y."
  (let ((focal (/ 1.0 (tan (/ fov-y 2.0)))))
    (mat4 (/ focal aspect) 0 0 0
          0 (- focal) 0 0
          0 0 (/ far (- near far)) -1
          0 0 (/ (* near far) (- near far)) 0)))

(defun translation (x y z)
  (mat4 1 0 0 0  0 1 0 0  0 0 1 0  x y z 1))

;;; A critically damped spring in closed form, as niri's springs are
;;; (#MS2WHR): position is a function of time, so retargeting reads the exact
;;; current position and velocity and starts a new curve from there.

(defparameter *camera-stiffness* 1000.0
  "The author's niri horizontal-view-movement spring, critically damped.")

(defstruct (spring (:constructor make-spring
                       (position &key (stiffness *camera-stiffness*))))
  position
  (velocity 0.0)
  (target position)
  (start 0.0)
  (stiffness *camera-stiffness*))

(defun spring-state (spring time)
  "Position and velocity of SPRING at TIME."
  (let* ((omega (sqrt (spring-stiffness spring)))
         (dt (max 0.0 (- time (spring-start spring))))
         (x0 (- (spring-position spring) (spring-target spring)))
         (v0 (spring-velocity spring))
         (b (+ v0 (* omega x0)))
         (decay (exp (- (* omega dt)))))
    (values (+ (spring-target spring) (* (+ x0 (* b dt)) decay))
            (* decay (- b (* omega (+ x0 (* b dt))))))))

(defun retarget-spring (spring target time)
  (multiple-value-bind (position velocity) (spring-state spring time)
    (setf (spring-position spring) position
          (spring-velocity spring) velocity
          (spring-target spring) target
          (spring-start spring) time)))

;;; The window shader: four strip vertices mapped through one matrix.

;;; Windows and their frames share one vertex shader.  It expands the unit
;;; square by EXTEND pixels on every side and hands the fragment shader the
;;; point in window pixels, so frame, border, shadow, and rounded corners are
;;; all measured in the client's own pixels: exact when the window is shown
;;; one to one.

(defmacro define-window-shader (name stage inputs resources outputs &body body)
  `(shader:define-shader ,name
       (:stage ,stage
        :inputs ,inputs
        :resources ((placement :uniform-block :set 0 :binding 2
                               :members ((model-view-projection :mat4)
                                         (tint :vec4)
                                         ;; width, height, corner radius, extension
                                         (shape :vec4)
                                         ;; border width, shadow blur, shadow alpha, unused
                                         (frame :vec4)
                                         (border-color :vec4)))
                    ,@resources)
        :outputs ,outputs)
     ,@body))

(define-window-shader window-vertex-specification :vertex
    ((vertex-index :uint :built-in :vertex-index))
    ()
    ((clip-position :vec4 :built-in :position)
     (window-pixel :vec2 :location 0))
  (let* ((two (shader:uint 2.0))
         (u (shader:float (mod vertex-index two)))
         (v (shader:float (/ vertex-index two)))
         (width (shader:swizzle shape :x))
         (height (shader:swizzle shape :y))
         (extend (shader:swizzle shape :w))
         (x (- (* u (+ width (* 2.0 extend))) extend))
         (y (- (* v (+ height (* 2.0 extend))) extend)))
    (shader:set-output window-pixel (shader:vec2 x y))
    (shader:set-output clip-position
                       (* model-view-projection
                          (shader:vec4 (/ x width) (/ y height) 0.0 1.0)))))

(shader:define-shader-function rounded-box-distance (point half-size radius)
  "Signed distance from POINT to a box of HALF-SIZE about the origin whose
corners are rounded by RADIUS."
  (let* ((corner (+ (- (shader:vec2 (abs (shader:swizzle point :x))
                                    (abs (shader:swizzle point :y)))
                       half-size)
                    (shader:vec2 radius radius)))
         (outside (shader:vec2 (max (shader:swizzle corner :x) 0.0)
                               (max (shader:swizzle corner :y) 0.0))))
    (- (+ (sqrt (shader:dot outside outside))
          (min (max (shader:swizzle corner :x) (shader:swizzle corner :y)) 0.0))
       radius)))

(define-window-shader window-fragment-specification :fragment
    ((window-pixel :vec2 :location 0))
    ((image :texture-2d :set 0 :binding 0)
     (image-sampler :sampler :set 0 :binding 1))
    ((color-output :vec4 :location 0))
  (let* ((size (shader:swizzle shape :xy))
         (texel (shader:sample image image-sampler (/ window-pixel size)))
         (distance (rounded-box-distance (- window-pixel (* size 0.5))
                                         (* size 0.5)
                                         (shader:swizzle shape :z)))
         (coverage (shader:clamp (- 0.5 distance) 0.0 1.0))
         (color (* (shader:swizzle texel :xyz) (shader:swizzle tint :xyz) coverage)))
    ;; Clients are opaque for now: their alpha is not trusted (XRGB).
    (shader:set-output color-output (shader:vec4 color coverage))))

(define-window-shader frame-fragment-specification :fragment
    ((window-pixel :vec2 :location 0))
    ((image :texture-2d :set 0 :binding 0)
     (image-sampler :sampler :set 0 :binding 1))
    ((color-output :vec4 :location 0))
  (let* ((size (shader:swizzle shape :xy))
         (border (shader:swizzle frame :x))
         (blur (shader:swizzle frame :y))
         (shadow-alpha (shader:swizzle frame :z))
         (point (- window-pixel (* size 0.5)))
         (half-outer (+ (* size 0.5) (shader:vec2 border border)))
         (radius (+ (shader:swizzle shape :z) border))
         (edge (rounded-box-distance point half-outer radius))
         (coverage (shader:clamp (- 0.5 edge) 0.0 1.0))
         ;; The shadow falls a little below, as from a light overhead.
         (lowered (- point (shader:vec2 0.0 (* blur 0.25))))
         (shadow (* shadow-alpha
                    (- 1.0 (shader:smoothstep (* blur -0.5) blur
                                              (rounded-box-distance lowered half-outer radius)))))
         (alpha (+ coverage (* shadow (- 1.0 coverage))))
         (color (* (shader:swizzle border-color :xyz) coverage)))
    (shader:set-output color-output (shader:vec4 color alpha))))

;;; The atelier.

(defclass luvland (luv.workbench:workbench-application luv:canvas-event-handler)
  ((canvas :initarg :canvas :reader luvland-canvas)
   (device :initarg :device :reader luvland-device)
   (context :initarg :context :reader luvland-context)
   (server :initarg :server :reader luvland-server)
   (texture-format :initarg :texture-format :reader luvland-texture-format)
   (sampler :accessor luvland-sampler)
   (layout :accessor luvland-layout)
   (pipeline :accessor luvland-pipeline)
   (frame-pipeline :accessor luvland-frame-pipeline)
   (configured-extent :initform nil :accessor luvland-configured-extent)
   (modules :initform '() :accessor luvland-modules)
   (windows :initform '() :accessor luvland-windows
            :documentation "WINDOW records in strip order, oldest first.")
   (focus :initform nil :accessor luvland-focus)
   (camera :initform (make-spring 0.0) :reader luvland-camera)
   (zoom :initform (make-spring 1.0) :reader luvland-zoom
         :documentation "The camera's distance as a multiple of the one-to-one
distance: 1 shows the focused window exactly, more pulls back over the strip.")
   (frame-states :initform (make-hash-table :test 'eq) :reader luvland-frame-states)
   (graveyard :initform '() :accessor luvland-graveyard
              :documentation "(done-p . thunk) pairs: work to do once the GPU has
finished everything submitted before it was queued.")
   (dead-buffers :initform '() :accessor luvland-dead-buffers
                 :documentation "Destroyed dmabuf buffers whose imports to drop,
pushed by the server thread.")
   (failures :initform '() :accessor luvland-failures)
   (frame-number :initform 0 :accessor luvland-frame-number)
   (processes :initform '() :accessor luvland-processes)
   (suppressed-keys :initform '() :accessor luvland-suppressed-keys)
   (running-p :initform t :accessor luvland-running-p)
   (stop-controller :initarg :stop-controller :reader luvland-stop-controller)
   (command-frame :initform nil :accessor luvland-command-frame
                  :documentation "The headless CLIM frame Luvland's commands run on.")
   (pending-keymaps :initform nil :accessor luvland-pending-keymaps
                    :documentation "Keymaps a key sequence may continue in, or NIL.")
   (key-sequence :initform nil :accessor luvland-key-sequence)
   (prefix-key-name :initform nil :accessor luvland-prefix-key-name)
   (help-sections :initform nil :accessor luvland-help-sections)
   (history :initform '() :accessor luvland-history
            :documentation "Toplevels, most recently focused first.")
   (forwarded-keys :initform '() :accessor luvland-forwarded-keys
                   :documentation "evdev keys pressed in clients and not yet released.")
   (forwarded-buttons :initform '() :accessor luvland-forwarded-buttons)
   (pointer :initform nil :accessor luvland-pointer
            :documentation "The last pointer position, in logical canvas pixels.")
   (software-cursor-p :initarg :software-cursor-p :initform nil
                      :reader luvland-software-cursor-p)
   (cursor-texture :initform nil :accessor luvland-cursor-texture)
   (frame-times :initform '() :accessor luvland-frame-times
                :documentation "Recent frames, newest first, as plists of milliseconds.")))

(defmethod print-object ((luvland luvland) stream)
  (print-unreadable-object (luvland stream :type t)
    (format stream "WAYLAND_DISPLAY=~A ~D window~:P"
            (wl:server-socket-name (luvland-server luvland))
            (length (luvland-windows luvland)))))

(defstruct (window (:constructor make-window (toplevel)))
  toplevel
  texture
  view
  ;; The texture shm snapshots upload into, and its view.
  (shm-texture nil)
  (shm-view nil)
  ;; The dmabuf frame being shown, claimed from the server.
  (held-frame nil)
  (snapshot-serial -1)
  ;; The window's number, the lowest free one when it appeared.
  (number 0)
  ;; The (size states) last sent to the client, to configure only on change.
  (configured nil)
  (width 0)
  (height 0)
  (x 0.0))

(defmethod print-object ((window window) stream)
  (print-unreadable-object (window stream :type t)
    (format stream "~A ~Dx~D" (window-toplevel window)
            (window-width window) (window-height window))))

(defparameter *pixels-per-unit* 1000.0
  "Client pixels per world unit: a 1000-pixel window is one unit wide.")
(defparameter *field-of-view* (* 50 (/ pi 180)))

(defun canvas-extent (luvland)
  (or (luv:canvas-extent (luvland-context luvland)) '(1 1)))

(defun now (luvland)
  "Seconds on luv's monotonic canvas clock, the clock presentation times are
predicted on."
  (declare (ignore luvland))
  (luv::monotonic-seconds))

(defun after-gpu (luvland thunk)
  "Call THUNK on the canvas thread once the GPU has finished all work
submitted so far, which is all work that could still read what THUNK frees."
  (let ((done-p (or (luv:queue-completion-watch (luv:device-queue (luvland-device luvland)))
                    ;; A backend that cannot say: four frames is plenty.
                    (let ((frame (+ (luvland-frame-number luvland) 4)))
                      (lambda () (>= (luvland-frame-number luvland) frame))))))
    (push (cons done-p thunk) (luvland-graveyard luvland))))

(defun retire (luvland resource)
  "Destroy RESOURCE once nothing in flight samples it."
  (when resource
    (after-gpu luvland (lambda () (luv:destroy resource)))))

(defun release-later (luvland frame)
  "Give FRAME's buffer back to its client once nothing in flight reads it."
  (when frame
    (after-gpu luvland
               (lambda ()
                 (wl:call-in-server (lambda () (wl:release-dmabuf-frame frame))
                                    :server (luvland-server luvland) :wait nil)))))

(defun bury-the-dead (luvland)
  (setf (luvland-graveyard luvland)
        (remove-if (lambda (entry)
                     (when (funcall (car entry))
                       (funcall (cdr entry))
                       t))
                   (luvland-graveyard luvland))))

(defun create-pipeline (luvland)
  (let* ((device (luvland-device luvland))
         (format (luv:canvas-format (luvland-context luvland)))
         (layout (luv:create device (luv:make-bind-group-layout-descriptor
                                     :label "Luvland window"
                                     :entries '((:binding 0 :type :texture)
                                                (:binding 1 :type :sampler)
                                                (:binding 2 :type :uniform-buffer)))))
         (modules '()))
    (flet ((module (label specification)
             (let ((module (luv:create device (luv:make-shader-module-descriptor
                                               :label label :language :mathematical
                                               :code specification))))
               (push module modules)
               module)))
      (let ((vertex (module "Luvland window vertex" (window-vertex-specification)))
            (window (module "Luvland window fragment" (window-fragment-specification)))
            (frame (module "Luvland frame fragment" (frame-fragment-specification))))
        (flet ((pipeline (label fragment)
                 (luv:create device (luv:make-render-pipeline-descriptor
                                     :label label
                                     :layout layout
                                     :vertex `(:module ,vertex)
                                     :fragment `(:module ,fragment
                                                 :targets ((:format ,format
                                                            :blend :premultiplied-alpha)))
                                     :primitive '(:topology :triangle-strip)))))
          (setf (luvland-modules luvland) modules
                (luvland-layout luvland) layout
                (luvland-sampler luvland)
                (luv:create device (luv:make-sampler-descriptor
                                    :label "Luvland window sampler"))
                (luvland-pipeline luvland) (pipeline "Luvland windows" window)
                (luvland-frame-pipeline luvland) (pipeline "Luvland frames" frame)))))))

;;; Windows follow the server's toplevels.

(defun sync-windows (luvland)
  "Add windows for newly mapped toplevels, drop vanished ones, and upload
fresh snapshots.  Runs on the canvas thread."
  (let* ((toplevels (remove-if-not #'wl:toplevel-mapped-p
                                   (reverse (wl:server-toplevels (luvland-server luvland)))))
         (kept (remove-if-not (lambda (window)
                                (member (window-toplevel window) toplevels))
                              (luvland-windows luvland))))
    (dolist (window (set-difference (luvland-windows luvland) kept))
      (retire luvland (window-shm-view window))
      (retire luvland (window-shm-texture window))
      (release-later luvland (window-held-frame window)))
    (drop-dead-imports luvland)
    (dolist (toplevel toplevels)
      (unless (find toplevel kept :key #'window-toplevel)
        (let ((window (make-window toplevel)))
          (setf (window-number window)
                (loop for number from 0
                      unless (find number kept :key #'window-number) return number)
                kept (append kept (list window))))
        ;; A new window takes focus, as it would on a desktop.
        (focus-window luvland toplevel)))
    (setf (luvland-windows luvland) kept)
    (unless (and (luvland-focus luvland)
                 (find (luvland-focus luvland) kept :key #'window-toplevel))
      (setf (luvland-focus luvland) (and kept (window-toplevel (first kept))))
      (focus-keyboard-later luvland (luvland-focus luvland)))
    (dolist (window kept)
      (or (take-dmabuf-frame luvland window)
          (upload-snapshot luvland window)))
    (layout-strip luvland)))

(defun show (window texture view width height)
  (setf (window-texture window) texture
        (window-view window) view
        (window-width window) width
        (window-height window) height))

(defun upload-snapshot (luvland window)
  "Copy WINDOW's newest shm commit to its texture, if there is one this
thread has not read yet."
  (wl:call-with-surface-snapshot
   (wl:toplevel-surface (window-toplevel window))
   (lambda (snapshot)
     (let ((width (wl:snapshot-width snapshot))
           (height (wl:snapshot-height snapshot))
           (device (luvland-device luvland))
           (texture (window-shm-texture window)))
       (unless (and texture (equal (list width height) (luv:gpu-texture-size texture)))
         (retire luvland (window-shm-view window))
         (retire luvland texture)
         (setf texture (luv:create device (luv:make-texture-descriptor
                                           :label "Luvland client window"
                                           :size (list width height)
                                           :dimensions :2d
                                           :format (luvland-texture-format luvland)
                                           :usage '(:copy-dst :texture-binding)))
               (window-shm-texture window) texture
               (window-shm-view window) (luv:create device (luv:make-texture-view-descriptor
                                                          :texture texture))))
       (luv:write-texture (luv:device-queue device)
                          (luv:make-texture-copy :texture texture)
                          (wl:snapshot-pixels snapshot)
                          (luv:make-texture-data-layout :bytes-per-row (* 4 width)
                                                        :rows-per-image height)
                          (list width height))
       ;; A client that went back to shm no longer needs its last dmabuf.
       (release-later luvland (shiftf (window-held-frame window) nil))
       (show window texture (window-shm-view window) width height)
       (setf (window-snapshot-serial window) (wl:snapshot-serial snapshot))))))

;;; dmabufs.  Each client buffer is imported once and kept in the buffer's
;;; host data, since clients cycle through two to four of them.

(defun fourcc-texture-format (luvland fourcc)
  (let ((srgb-p (eq (luvland-texture-format luvland) :bgra8-unorm-srgb)))
    (cond
      ((or (= fourcc wl:+drm-format-argb8888+) (= fourcc wl:+drm-format-xrgb8888+))
       (if srgb-p :bgra8-unorm-srgb :bgra8-unorm))
      ((or (= fourcc wl:+drm-format-abgr8888+) (= fourcc wl:+drm-format-xbgr8888+))
       (if srgb-p :rgba8-unorm-srgb :rgba8-unorm)))))

(defun dmabuf-description (device texture-format)
  "What DEVICE can import, in the form LUV.WAYLAND:SERVER's :DMABUF takes,
or NIL when it cannot import dmabufs at all."
  (multiple-value-bind (major minor) (luv:dmabuf-render-node device)
    (when major
      (let ((srgb-p (eq texture-format :bgra8-unorm-srgb))
            (formats '()))
        (loop for (fourcc format) in (list (list wl:+drm-format-argb8888+ :bgra8-unorm)
                                           (list wl:+drm-format-xrgb8888+ :bgra8-unorm)
                                           (list wl:+drm-format-abgr8888+ :rgba8-unorm)
                                           (list wl:+drm-format-xbgr8888+ :rgba8-unorm))
              do (dolist (modifier (luv:dmabuf-modifiers
                                    device (if srgb-p
                                               (intern (format nil "~A-SRGB" format) :keyword)
                                               format)))
                   (push (cons fourcc modifier) formats)))
        (when formats
          (list :main-device (wl:make-dev-t major minor)
                :formats (nreverse formats)))))))

(defun import-buffer (luvland buffer frame)
  "BUFFER's imported (texture view), made now from FRAME's planes if need be."
  (or (wl:dmabuf-host-data buffer)
      (let* ((device (luvland-device luvland))
             (texture (luv:import-dmabuf-texture
                       device
                       (luv:make-texture-descriptor
                        :label "Luvland client dmabuf"
                        :size (list (wl:dmabuf-width buffer) (wl:dmabuf-height buffer))
                        :dimensions :2d
                        :format (fourcc-texture-format luvland (wl:dmabuf-format buffer))
                        :usage '(:texture-binding))
                       :modifier (wl:dmabuf-modifier buffer)
                       :planes (wl:dmabuf-frame-planes frame)))
             (import (list texture (luv:create device (luv:make-texture-view-descriptor
                                                       :texture texture)))))
        (setf (wl:dmabuf-host-data buffer) import)
        ;; Forget the import when the client destroys the buffer.
        (wl:call-in-server
         (lambda ()
           (flet ((dead (buffer) (sb-ext:atomic-push buffer (slot-value luvland 'dead-buffers))))
             (if (wl:resource-live-p buffer)
                 (wl:on-dmabuf-destroyed buffer #'dead)
                 (dead buffer))))
         :server (luvland-server luvland) :wait nil)
        import)))

(defun drop-dead-imports (luvland)
  (loop for buffer = (sb-ext:atomic-pop (slot-value luvland 'dead-buffers))
        while buffer
        do (destructuring-bind (&optional texture view) (wl:dmabuf-host-data buffer)
             (setf (wl:dmabuf-host-data buffer) nil)
             (retire luvland view)
             (retire luvland texture))))

(defun take-dmabuf-frame (luvland window)
  "Show WINDOW's newest dmabuf frame, if there is one to claim; true if so."
  (let ((frame (wl:claim-dmabuf-frame (wl:toplevel-surface (window-toplevel window)))))
    (when frame
      (let ((buffer (wl:dmabuf-frame-buffer frame)))
        (handler-case
            (destructuring-bind (texture view) (import-buffer luvland buffer frame)
              (release-later luvland (shiftf (window-held-frame window) frame))
              (show window texture view (wl:dmabuf-width buffer) (wl:dmabuf-height buffer))
              (setf (window-snapshot-serial window) (wl:dmabuf-frame-serial frame)))
          (error (condition)
            (push (list :import buffer condition) (luvland-failures luvland))
            (wl:call-in-server (lambda () (wl:release-dmabuf-frame frame))
                               :server (luvland-server luvland) :wait nil))))
      t)))

(defparameter *terminal* '("foot" "--override=pad=18x14 center")
  "The terminal Luvland opens as a client, as a program and its arguments.
Windows meet the compositor's frame edge to edge, so foot pads its own grid
in its own background colour rather than setting text against the border.")

(defparameter *window-margin* 28
  "Pixels between the focused window's border and the edge of the view.")
(defparameter *border-width* 2.0)
(defparameter *corner-radius* 10.0)
(defparameter *shadow-blur* 24.0)
(defparameter *shadow-alpha* 0.45)
(defparameter *focused-border-color* '(0.94 0.62 0.34))
(defparameter *border-color* '(0.24 0.25 0.29))

(defun layout-strip (luvland)
  "Place windows left to right along X, each centered on Y = 0, a margin's
width apart so one stands alone in the view when it has focus."
  (let ((x 0.0))
    (dolist (window (luvland-windows luvland))
      (setf (window-x window) x)
      (incf x (/ (+ (window-width window) (* 2 *window-margin*)) *pixels-per-unit*)))))

(defun window-world-size (window)
  (values (/ (window-width window) *pixels-per-unit*)
          (/ (window-height window) *pixels-per-unit*)))

(defun focused-window (luvland)
  (find (luvland-focus luvland) (luvland-windows luvland) :key #'window-toplevel))

(defun native-distance (luvland)
  "The distance at which a window in the plane Z = 0 shows one client pixel
per drawable pixel: the view's height in pixels, in world units, divided by
twice the tangent of half the field of view."
  (destructuring-bind (width height) (canvas-extent luvland)
    (declare (ignore width))
    (/ (/ height *pixels-per-unit*) (* 2 (tan (/ *field-of-view* 2))))))

(defun pixel-alignment (view-pixels window-pixels)
  "Half a pixel, in world units, when centering WINDOW-PIXELS in VIEW-PIXELS
would put its edges between pixels; otherwise zero."
  (if (oddp (- view-pixels window-pixels)) (/ 0.5 *pixels-per-unit*) 0.0))

(defun top-reserve (luvland)
  "Drawable pixels at the top kept for the workbench's status line."
  (if (luvland-workbench luvland)
      (round (* mcluv::+status-bar-height+ (display-scale luvland)))
      0))

(defun display-scale (luvland)
  "Drawable pixels per logical pixel."
  (destructuring-bind (width height) (canvas-extent luvland)
    (declare (ignore height))
    (multiple-value-bind (logical-width logical-height)
        (luv:canvas-logical-size (luvland-canvas luvland))
      (declare (ignore logical-height))
      (/ width (max 1 logical-width)))))

(defun camera-target (luvland window)
  "Where the camera looks to show WINDOW one to one, centred in the view
below the status line, its edges on pixel boundaries."
  (destructuring-bind (width height) (canvas-extent luvland)
    (let ((reserve (top-reserve luvland)))
      (values (+ (window-x window) (/ (window-width window) *pixels-per-unit* 2)
                 (pixel-alignment width (window-width window)))
              ;; Raising the camera lowers the window by half the reserve.
              (+ (/ reserve 2 *pixels-per-unit*)
                 (pixel-alignment (- height reserve) (window-height window)))))))

(defparameter *overview-zoom* 2.4
  "How far the overview pulls the camera back, in one-to-one distances.")

(defun camera-position (luvland time)
  (let ((window (focused-window luvland)))
    (values (spring-state (luvland-camera luvland) time)
            (if window (nth-value 1 (camera-target luvland window)) 0.0)
            (* (native-distance luvland) (spring-state (luvland-zoom luvland) time)))))

(defun overview-p (luvland)
  (/= 1.0 (spring-target (luvland-zoom luvland))))

(defun toggle-overview (luvland)
  "Pull the camera back over the whole strip, as niri's overview does, or
return to showing the focused window one to one."
  (retarget-spring (luvland-zoom luvland)
                   (if (overview-p luvland) 1.0 *overview-zoom*)
                   (now luvland)))

(defun aim-camera (luvland)
  (let ((window (focused-window luvland)))
    (when window
      (let ((target (camera-target luvland window))
            (spring (luvland-camera luvland)))
        (unless (= target (spring-target spring))
          (retarget-spring spring target (now luvland)))))))

(defun view-projection (luvland time &optional (extent (canvas-extent luvland)))
  (destructuring-bind (width height) extent
    (multiple-value-bind (cx cy cz) (camera-position luvland time)
      (mat4* (perspective *field-of-view* (/ width (max 1 height)) 0.05 100.0)
             (translation (- cx) (- cy) (- cz))))))

(defun window-model (window)
  "Map the unit square, V downward, onto WINDOW's place in the world."
  (multiple-value-bind (width height) (window-world-size window)
    (mat4 width 0 0 0
          0 (- height) 0 0
          0 0 1 0
          (window-x window) (/ height 2) 0 1)))

;;; Configuring clients: every window is sized to stand alone in the view,
;;; tiled so clients take the size exactly, and only the focused one is
;;; activated.

(defun fitted-window-size (luvland)
  (destructuring-bind (width height) (canvas-extent luvland)
    (list (max 64 (- width (* 2 *window-margin*)))
          (max 64 (- height (* 2 *window-margin*) (top-reserve luvland))))))

(defun reconfigure-windows (luvland)
  "Send each window the size and states it should have, if they changed."
  (let ((size (fitted-window-size luvland))
        (server (luvland-server luvland)))
    (unless (equal size (wl:server-initial-toplevel-size server))
      (setf (wl:server-initial-toplevel-size server) size))
    (dolist (window (luvland-windows luvland))
      (let* ((toplevel (window-toplevel window))
             (states (append (when (eq toplevel (luvland-focus luvland)) '(:activated))
                             '(:tiled-left :tiled-right :tiled-top :tiled-bottom)))
             (wanted (list size states)))
        (unless (equal wanted (window-configured window))
          (setf (window-configured window) wanted)
          (wl:call-in-server (lambda ()
                               (when (wl:resource-live-p toplevel)
                                 (wl:configure-toplevel toplevel :size size :states states)))
                             :server server :wait nil))))))

;;; Frames.

(defun frame-state (luvland surface-texture)
  "Uniform buffers and bind groups for one swapchain image, by window, and
the image's own view under :TARGET.  A swapchain image is reacquired only
after its last presentation, so its buffers are no longer being read when
this frame rewrites them."
  (or (gethash surface-texture (luvland-frame-states luvland))
      (let ((state (make-hash-table :test 'eq)))
        (setf (gethash :target state)
              (luv:create (luvland-device luvland)
                          (luv:make-texture-view-descriptor :texture surface-texture))
              (gethash surface-texture (luvland-frame-states luvland)) state))))

(defun window-bindings (luvland state window)
  "The uniform buffers and bind groups for drawing WINDOW's frame and its
contents into the swapchain image STATE belongs to."
  (let ((entry (gethash window state)))
    (unless (and entry (eq (getf entry :view) (window-view window)))
      (let ((device (luvland-device luvland)))
        (flet ((buffer (key)
                 (or (getf entry key)
                     (luv:create device (luv:make-buffer-descriptor
                                         :label "Luvland window placement"
                                         :size 128 :usage '(:uniform)))))
               (bind-group (buffer)
                 (luv:create device (luv:make-bind-group-descriptor
                                     :label "Luvland window"
                                     :layout (luvland-layout luvland)
                                     :entries `((:binding 0 :resource ,(window-view window))
                                                (:binding 1 :resource ,(luvland-sampler luvland))
                                                (:binding 2 :resource ,buffer))))))
          (when entry
            (retire luvland (getf entry :frame-group))
            (retire luvland (getf entry :window-group)))
          (let ((frame-buffer (buffer :frame-buffer))
                (window-buffer (buffer :window-buffer)))
            (setf entry (list :view (window-view window)
                              :frame-buffer frame-buffer
                              :frame-group (bind-group frame-buffer)
                              :window-buffer window-buffer
                              :window-group (bind-group window-buffer))
                  (gethash window state) entry)))))
    entry))

(defun placement-uniform (matrix window &key tint extend border-color)
  (let ((data (make-array 32 :element-type 'single-float :initial-element 0.0)))
    (replace data matrix)
    (replace data (map 'vector (lambda (x) (coerce x 'single-float)) tint) :start1 16)
    (setf (aref data 20) (float (window-width window) 1.0)
          (aref data 21) (float (window-height window) 1.0)
          (aref data 22) (float *corner-radius* 1.0)
          (aref data 23) (float extend 1.0)
          (aref data 24) (float *border-width* 1.0)
          (aref data 25) (float *shadow-blur* 1.0)
          (aref data 26) (float *shadow-alpha* 1.0))
    (replace data (map 'vector (lambda (x) (coerce x 'single-float)) border-color)
             :start1 28)
    (setf (aref data 31) 1.0)
    data))

(defun encode-windows (luvland encoder target extent time)
  "Draw every window into TARGET, a texture of EXTENT (width height), as
the world will be at TIME."
  ;; Imported dmabufs belong to their clients' queues until acquired, and an
  ;; acquire cannot happen inside the pass.
  (let ((acquired '()))
    (dolist (window (luvland-windows luvland))
      (let ((texture (window-texture window)))
        (when (and (window-held-frame window) texture
                   (not (member texture acquired)))
          (luv:acquire-external-texture encoder texture)
          (push texture acquired)))))
  (let* ((state (frame-state luvland target))
         (view-projection (view-projection luvland time extent))
         (focus (luvland-focus luvland))
         (windows (remove-if-not #'window-view (luvland-windows luvland)))
         (pass (luv:begin-render-pass
                encoder
                (luv:make-render-pass-descriptor
                 :label "Luvland"
                 :color-attachments `((:view ,(gethash :target state)
                                       :load-op :clear :store-op :store
                                       :clear-value #(0.012 0.014 0.022 1.0)))))))
    (dolist (window windows)
      (let* ((focused (eq (window-toplevel window) focus))
             (entry (window-bindings luvland state window))
             (matrix (mat4* view-projection (window-model window))))
        (luv:write-buffer (getf entry :frame-buffer)
                          (placement-uniform matrix window
                                             :tint '(1 1 1 1)
                                             :extend (+ *border-width* *shadow-blur*)
                                             :border-color (if focused
                                                               *focused-border-color*
                                                               *border-color*)))
        (luv:write-buffer (getf entry :window-buffer)
                          (placement-uniform matrix window
                                             :tint (if focused '(1 1 1 1) '(0.62 0.62 0.68 1))
                                             :extend 0
                                             :border-color '(0 0 0)))))
    ;; Frames first, so no window's shadow falls across a neighbour.
    (luv:set-pipeline pass (luvland-frame-pipeline luvland))
    (dolist (window windows)
      (luv:set-bind-group pass 0 (getf (gethash window state) :frame-group))
      (luv:draw pass 4))
    (luv:set-pipeline pass (luvland-pipeline luvland))
    (dolist (window windows)
      (luv:set-bind-group pass 0 (getf (gethash window state) :window-group))
      (luv:draw pass 4))
    (when (and (luvland-software-cursor-p luvland) (luvland-pointer luvland))
      (encode-cursor luvland pass state extent))
    ;; The McCLIM shell is the last thing drawn, over every window.
    (let ((workbench (luvland-workbench luvland)))
      (when workbench (luv.workbench:encode-workbench workbench pass target)))
    (luv:end-pass pass)))

;;; The software cursor: on a console nothing else draws the pointer.  It is
;;; a frame without a window, a small disc in screen space.

(defparameter *cursor-size* 12.0)
(defparameter *cursor-color* '(0.97 0.95 0.90))

(defun make-placeholder-texture (luvland)
  "A written 1x1 texture for a bind group whose shader never samples it.
The upload leaves it in the layout a pass samples from; an unwritten one
would ask for a layout transition inside the render pass."
  (let* ((device (luvland-device luvland))
         (texture (luv:create device (luv:make-texture-descriptor
                                      :label "Luvland placeholder"
                                      :size '(1 1) :dimensions :2d
                                      :format (luvland-texture-format luvland)
                                      :usage '(:copy-dst :texture-binding)))))
    (luv:write-texture (luv:device-queue device)
                       (luv:make-texture-copy :texture texture)
                       (make-array '(1 1) :element-type '(unsigned-byte 32)
                                          :initial-element 0)
                       (luv:make-texture-data-layout :bytes-per-row 4 :rows-per-image 1)
                       '(1 1))
    texture))

(defun cursor-bindings (luvland state)
  (or (gethash :cursor state)
      (let* ((device (luvland-device luvland))
             (texture (or (luvland-cursor-texture luvland)
                          (setf (luvland-cursor-texture luvland)
                                (make-placeholder-texture luvland))))
             (view (luv:create device (luv:make-texture-view-descriptor :texture texture)))
             (buffer (luv:create device (luv:make-buffer-descriptor
                                         :label "Luvland cursor placement"
                                         :size 128 :usage '(:uniform)))))
        (setf (gethash :cursor state)
              (list :view view :frame-buffer buffer
                    :frame-group (luv:create device (luv:make-bind-group-descriptor
                                                     :label "Luvland cursor"
                                                     :layout (luvland-layout luvland)
                                                     :entries `((:binding 0 :resource ,view)
                                                                (:binding 1 :resource ,(luvland-sampler luvland))
                                                                (:binding 2 :resource ,buffer)))))))))

(defun encode-cursor (luvland pass state extent)
  (destructuring-bind (width height) extent
    (multiple-value-bind (logical-width logical-height)
        (luv:canvas-logical-size (luvland-canvas luvland))
      (destructuring-bind (x y) (luvland-pointer luvland)
        (let* ((scale-x (/ width (max 1 logical-width)))
               (scale-y (/ height (max 1 logical-height)))
               (size *cursor-size*)
               (left (- (* x scale-x) (/ size 2)))
               (top (- (* y scale-y) (/ size 2)))
               ;; Clip space with Y down, as Vulkan's is.
               (matrix (mat4 (/ (* 2 size) width) 0 0 0
                             0 (/ (* 2 size) height) 0 0
                             0 0 1 0
                             (- (/ (* 2 left) width) 1) (- (/ (* 2 top) height) 1) 0 1))
               (entry (cursor-bindings luvland state))
               (data (make-array 32 :element-type 'single-float :initial-element 0.0)))
          (replace data matrix)
          (replace data '(1.0 1.0 1.0 1.0) :start1 16)
          (setf (aref data 20) size (aref data 21) size
                (aref data 22) (/ size 2) (aref data 23) 8.0
                (aref data 24) 0.0 (aref data 25) 6.0 (aref data 26) 0.5)
          (replace data (mapcar (lambda (x) (float x 1.0)) *cursor-color*) :start1 28)
          (setf (aref data 31) 1.0)
          (luv:write-buffer (getf entry :frame-buffer) data)
          (luv:set-pipeline pass (luvland-frame-pipeline luvland))
          (luv:set-bind-group pass 0 (getf entry :frame-group))
          (luv:draw pass 4))))))

(defun milliseconds-since (start)
  (/ (- (get-internal-real-time) start)
     (/ internal-time-units-per-second 1000.0)))

(defun record-frame-time (luvland &rest times)
  (let ((entries (cons (list* :at (now luvland) times) (luvland-frame-times luvland))))
    (setf (luvland-frame-times luvland)
          (if (> (length entries) 240) (subseq entries 0 240) entries))))

(defun render-frame (luvland)
  (let ((start (get-internal-real-time))
        (synced nil)
        (encoded nil))
    (sync-windows luvland)
    (reconfigure-windows luvland)
    ;; A buffer replaced just now was last sampled by the previous frame,
    ;; which has usually finished: give it back before drawing, not a frame
    ;; later, or a client with three swapchain images starves.
    (bury-the-dead luvland)
    (aim-camera luvland)
    ;; The shell samples and prepares outside the pass it is drawn in.
    (let ((workbench (luvland-workbench luvland)))
      (when workbench (luv.workbench:refresh-workbench workbench)))
    (setf synced (milliseconds-since start))
    (luv:call-with-canvas-frame
     (luvland-context luvland)
     (lambda (surface-texture encoder presentation-time)
       ;; Animate to when this frame will be seen, not when it is drawn.
       (let ((begun (get-internal-real-time)))
         (encode-windows luvland encoder surface-texture (canvas-extent luvland)
                         (or presentation-time (now luvland)))
         (setf encoded (milliseconds-since begun)))))
    (record-frame-time luvland :sync synced :encode encoded
                               :total (milliseconds-since start)))
  (incf (luvland-frame-number luvland))
  (bury-the-dead luvland)
  ;; Now is a good time for every client to draw its next frame.
  (wl:call-in-server #'wl:send-frame-callbacks
                     :server (luvland-server luvland) :wait nil))

(defun forget-frame-state (luvland target)
  "Destroy the view, buffers, and bind groups kept for TARGET."
  (let ((state (gethash target (luvland-frame-states luvland))))
    (when state
      (remhash target (luvland-frame-states luvland))
      (loop for key being the hash-keys of state using (hash-value value)
            do (if (eq key :target)
                   (luv:destroy value)
                   (dolist (slot '(:frame-group :window-group :frame-buffer :window-buffer :view))
                     (let ((resource (getf value slot)))
                       ;; A window's view belongs to the window, not this state.
                       (when (and resource (or (not (eq slot :view)) (eq key :cursor)))
                         (luv:destroy resource)))))))))

;;; Screenshots through luv's shared capture transaction.

(defmethod luv:capture-canvas ((luvland luvland))
  (luvland-canvas luvland))

(defmethod luv:encode-capture-frame ((luvland luvland) capture encoder target extent)
  (declare (ignore capture))
  ;; A capture may run while window frames are held; show the newest commits.
  (sync-windows luvland)
  (aim-camera luvland)
  (encode-windows luvland encoder target extent (now luvland))
  nil)

(defmethod luv:cleanup-capture ((luvland luvland) capture)
  (let ((target (luv:capture-target capture)))
    (when target
      (luv:request-canvas-frame (luvland-canvas luvland)
                                (lambda (timestamp)
                                  (declare (ignore timestamp))
                                  (forget-frame-state luvland target))))))

(defun capture-luvland-screenshot (pathname &optional (luvland *luvland*))
  "Render one Luvland frame offscreen into the PNG at PATHNAME."
  (luv:capture-application-screenshot luvland (merge-pathnames pathname)
                                      :label "Luvland screenshot"))

;;; Input.

(defun focus-keyboard-later (luvland toplevel)
  (let ((surface (and toplevel (wl:toplevel-surface toplevel))))
    (wl:call-in-server (lambda () (wl:focus-keyboard surface))
                       :server (luvland-server luvland) :wait nil)))

(defun focus-window (luvland toplevel)
  (setf (luvland-focus luvland) toplevel
        (luvland-history luvland)
        (cons toplevel (remove toplevel (luvland-history luvland))))
  (focus-keyboard-later luvland toplevel))

(defun focus-neighbor (luvland direction &key wrap)
  (let* ((windows (luvland-windows luvland))
         (index (position (luvland-focus luvland) windows :key #'window-toplevel)))
    (when index
      (let* ((target (+ index direction))
             (next (nth (if wrap
                            (mod target (length windows))
                            (max 0 (min (1- (length windows)) target)))
                        windows)))
        (focus-window luvland (window-toplevel next))))))

(defmethod luv:handle-canvas-event ((luvland luvland) canvas (event luv:canvas-key-press-event))
  (declare (ignore canvas))
  (let ((key-name (luv:canvas-key-event-key-name event)))
    (cond
      ((handle-command-key luvland event)
       ;; Its release belongs to Luvland too.
       (pushnew key-name (luvland-suppressed-keys luvland)))
      ((luv:canvas-key-event-repeat-p event))   ; clients repeat keys themselves
      (t (forward-key luvland key-name t)))))

(defmethod luv:handle-canvas-event ((luvland luvland) canvas (event luv:canvas-key-release-event))
  (declare (ignore canvas))
  (let ((key-name (luv:canvas-key-event-key-name event)))
    (if (member key-name (luvland-suppressed-keys luvland))
        (setf (luvland-suppressed-keys luvland)
              (remove key-name (luvland-suppressed-keys luvland)))
        (forward-key luvland key-name nil))))

(defun send-to-server (luvland function)
  (wl:call-in-server function :server (luvland-server luvland) :wait nil))

(defun forward-key (luvland key-name pressed-p)
  (let ((code (key-evdev-code key-name)))
    (when code
      (if pressed-p
          (pushnew code (luvland-forwarded-keys luvland))
          (setf (luvland-forwarded-keys luvland)
                (remove code (luvland-forwarded-keys luvland))))
      (send-to-server luvland (lambda () (wl:send-key code pressed-p))))))

(defun release-forwarded-input (luvland)
  "Release every key and button a client saw pressed, so none sticks while
the shell or a key sequence has the keyboard."
  (let ((keys (shiftf (luvland-forwarded-keys luvland) '()))
        (buttons (shiftf (luvland-forwarded-buttons luvland) '())))
    (when (or keys buttons)
      (send-to-server luvland
                      (lambda ()
                        (dolist (code keys) (wl:send-key code nil))
                        (dolist (code buttons) (wl:send-pointer-button code nil)))))))

(defun pick (luvland x y)
  "The window under logical canvas point X, Y and the surface-local point
where the camera ray through it meets the window's plane."
  (multiple-value-bind (width height) (luv:canvas-logical-size (luvland-canvas luvland))
    (destructuring-bind (pixel-width pixel-height) (canvas-extent luvland)
      (multiple-value-bind (cx cy cz) (camera-position luvland (now luvland))
        (let* ((focal (/ 1.0 (tan (/ *field-of-view* 2))))
               (aspect (/ pixel-width (max 1 pixel-height)))
               (ndc-x (- (* 2 (/ x (max 1 width))) 1))
               (ndc-y (- 1 (* 2 (/ y (max 1 height)))))
               ;; The ray direction in view space, with Z = -1.
               (dx (/ (* ndc-x aspect) focal))
               (dy (/ ndc-y focal))
               ;; Every window lies in the plane Z = 0.
               (distance cz)
               (wx (+ cx (* dx distance)))
               (wy (+ cy (* dy distance))))
          (dolist (window (luvland-windows luvland))
            (multiple-value-bind (ww wh) (window-world-size window)
              (let ((u (/ (- wx (window-x window)) ww))
                    (v (/ (- (/ wh 2) wy) wh)))
                (when (and (<= 0 u 1) (<= 0 v 1))
                  (return (values window
                                  (* u (window-width window))
                                  (* v (window-height window)))))))))))))

(defmethod luv:handle-canvas-event ((luvland luvland) canvas (event luv:canvas-pointer-motion-event))
  (declare (ignore canvas))
  (setf (luvland-pointer luvland)
        (list (luv:canvas-pointer-event-x event) (luv:canvas-pointer-event-y event)))
  (multiple-value-bind (window x y)
      (pick luvland (luv:canvas-pointer-event-x event) (luv:canvas-pointer-event-y event))
    (let ((surface (and window (wl:toplevel-surface (window-toplevel window)))))
      (wl:call-in-server (lambda ()
                           (if (eq surface (wl:pointer-focus (wl::seat)))
                               (when surface (wl:send-pointer-motion x y))
                               (wl:focus-pointer surface (or x 0) (or y 0))))
                         :server (luvland-server luvland) :wait nil))))

(defun pointer-button (luvland event pressed-p)
  (let ((code (button-evdev-code (luv:canvas-pointer-event-button event))))
    (when pressed-p
      ;; Clicking a window focuses it and brings the camera along; in the
      ;; overview it also brings the camera back in to it.
      (let ((window (pick luvland (luv:canvas-pointer-event-x event)
                          (luv:canvas-pointer-event-y event))))
        (when (and window (not (eq (window-toplevel window) (luvland-focus luvland))))
          (focus-window luvland (window-toplevel window)))
        (when (and window (overview-p luvland))
          (toggle-overview luvland))))
    (when code
      (if pressed-p
          (pushnew code (luvland-forwarded-buttons luvland))
          (setf (luvland-forwarded-buttons luvland)
                (remove code (luvland-forwarded-buttons luvland))))
      (send-to-server luvland (lambda () (wl:send-pointer-button code pressed-p))))))

(defmethod luv:handle-canvas-event ((luvland luvland) canvas (event luv:canvas-pointer-button-press-event))
  (declare (ignore canvas))
  (pointer-button luvland event t))

(defmethod luv:handle-canvas-event ((luvland luvland) canvas (event luv:canvas-pointer-button-release-event))
  (declare (ignore canvas))
  (pointer-button luvland event nil))

(defmethod luv:handle-canvas-event ((luvland luvland) canvas (event luv:canvas-pointer-wheel-event))
  (declare (ignore canvas))
  (let ((dx (* 15 (luv:canvas-pointer-event-scroll-x event)))
        (dy (* -15 (luv:canvas-pointer-event-scroll-y event))))
    (wl:call-in-server (lambda () (wl:send-pointer-axis dx dy))
                       :server (luvland-server luvland) :wait nil)))

(defmethod luv:handle-canvas-event ((luvland luvland) canvas (event luv:canvas-window-close-request-event))
  (declare (ignore canvas))
  (request-stop luvland)
  :defer-canvas-close)

(defmethod luv:handle-canvas-event ((luvland luvland) canvas event)
  (declare (ignore canvas event))
  nil)

;;; Clients.

(defun client-environment (luvland)
  (cons (format nil "WAYLAND_DISPLAY=~A" (wl:server-socket-name (luvland-server luvland)))
        (remove-if (lambda (entry)
                     (some (lambda (prefix) (uiop:string-prefix-p prefix entry))
                           '("WAYLAND_DISPLAY=" "DISPLAY=" "WAYLAND_SOCKET=")))
                   (sb-ext:posix-environ))))

(defun spawn-client (luvland program &rest arguments)
  "Run PROGRAM as a client of LUVLAND's server."
  (let ((process (sb-ext:run-program program arguments
                                     :search t :wait nil
                                     :output nil :error nil :input nil
                                     :environment (client-environment luvland))))
    (push process (luvland-processes luvland))
    process))

;;; Lifecycle.

(defun texture-format-for (canvas-format)
  "Clients draw sRGB-encoded pixels; decode them on sampling when the
swapchain encodes on write, so colors pass through unchanged."
  (if (search "SRGB" (symbol-name canvas-format))
      :bgra8-unorm-srgb
      :bgra8-unorm))

;;; The workbench: Luvland is a conforming atelier, so the shared status
;;; line, M-x, legend, prompt, and message window are McCLIM panes drawn by
;;; Luvland's own final pass.

(defmethod luv.workbench:workbench-application-name ((luvland luvland)) "Luvland")
(defmethod luv.workbench:workbench-application-command-frame ((luvland luvland))
  (luvland-command-frame luvland))
(defmethod luv.workbench:workbench-application-canvas ((luvland luvland))
  (luvland-canvas luvland))
(defmethod luv.workbench:workbench-application-context ((luvland luvland))
  (luvland-context luvland))
(defmethod luv.workbench:workbench-application-stop-controller ((luvland luvland))
  (luvland-stop-controller luvland))

(defmethod luv.workbench:suspend-workbench-application-input ((luvland luvland))
  ;; The shell swallows the releases of keys clients already saw pressed.
  (release-forwarded-input luvland)
  (reset-key-state luvland)
  (setf (luvland-suppressed-keys luvland) nil)
  (list :luvland-input-suspension luvland))

(defmethod luv.workbench:resume-workbench-application-input ((luvland luvland) token)
  (declare (ignore token))
  nil)

(defmethod mcluv:command-menu-tables-for ((luvland luvland))
  (list (clim:find-command-table 'luvland-commands)))

(defmethod mcluv:status-bar-application-name ((luvland luvland)) "Luvland")

(defmethod mcluv:status-bar-channels-for ((luvland luvland))
  (append (call-next-method) '(:display :windows :focus)))

(defmethod mcluv:status-bar-channel-label ((channel (eql :display)) (luvland luvland))
  "display")
(defmethod mcluv:status-bar-channel-label ((channel (eql :windows)) (luvland luvland))
  "windows")
(defmethod mcluv:status-bar-channel-label ((channel (eql :focus)) (luvland luvland))
  "focus")

(defmethod mcluv:status-bar-channel-value ((channel (eql :display)) (luvland luvland) bar)
  (declare (ignore bar))
  (wl:server-socket-name (luvland-server luvland)))
(defmethod mcluv:status-bar-channel-value ((channel (eql :windows)) (luvland luvland) bar)
  (declare (ignore bar))
  (length (luvland-windows luvland)))
(defmethod mcluv:status-bar-channel-value ((channel (eql :focus)) (luvland luvland) bar)
  (declare (ignore bar))
  (let ((window (focused-window luvland)))
    (and window (format nil "~D ~A" (window-number window) (window-title window)))))

;;; Lifecycle.

(defun default-software-cursor-p ()
  "On a console nothing else draws the pointer."
  (string-equal (uiop:getenv "SDL_VIDEODRIVER") "kmsdrm"))

(defun make-frame-clock (luvland)
  "A presentation clock on a direct display, where presenting paces frames;
otherwise sixty frames a second."
  (flet ((frame (canvas timestamp)
           (declare (ignore canvas timestamp))
           (render-frame luvland)))
    (if (ignore-errors (luv::sdl-canvas-direct-display-p (luvland-canvas luvland)))
        (luv:make-presentation-clock #'frame)
        (luv:make-cadence-clock #'frame :frames-per-second 60))))

(defun start-luvland (&key (width 1600) (height 900) fullscreen-p
                           (spawn *terminal*) (window-size '(1000 700))
                           (software-cursor-p (default-software-cursor-p))
                           (init-file-p t))
  "Open Luvland: a canvas, a Wayland server on its own socket, the McCLIM
workbench, the init file, and SPAWN, a program run as the first client."
  (when *luvland*
    (error "Luvland is already running: ~A" *luvland*))
  (let ((server nil)
        (canvas nil)
        (device nil)
        (luvland nil))
    (handler-bind ((serious-condition
                     (lambda (condition)
                       (declare (ignore condition))
                       (when canvas (ignore-errors (luv:close-canvas canvas)))
                       (when device (ignore-errors (luv:destroy device)))
                       (when server (wl:stop-server server)))))
      (setf canvas (luv:make-sdl-canvas
                    :title "Luvland" :width width :height height
                    :fullscreen-p fullscreen-p :high-pixel-density-p t
                    :presentation-api (luv:sdl-presentation-api-for luv:*gpu-provider*)))
      (luv:open-canvas canvas)
      (setf device (luv:request-gpu-device luv:*gpu-provider*
                                           (luv:make-device-descriptor :label "Luvland")))
      (let ((context (luv:make-canvas-context
                      canvas luv:*gpu-provider*
                      (luv:make-canvas-configuration
                       :device device :usage '(:render-attachment :copy-src)))))
        (setf server (wl:start-server
                      :initargs `(:initial-toplevel-size ,window-size
                                  ;; On a console the canvas is the display's mode.
                                  :output-size ,(or (luv:canvas-extent context)
                                                    (list width height))
                                  :dmabuf ,(dmabuf-description
                                            device (texture-format-for
                                                    (luv:canvas-format context))))))
        (setf luvland (make-instance 'luvland
                                     :canvas canvas :device device :context context
                                     :server server
                                     :software-cursor-p software-cursor-p
                                     :stop-controller (luv:make-canvas-stop-controller
                                                       canvas :name "Luvland")
                                     :texture-format (texture-format-for
                                                      (luv:canvas-format context))))
        (create-pipeline luvland)
        (setf (luvland-command-frame luvland)
              (clim:make-application-frame 'luvland-frame :luvland luvland))
        ;; The workbench wraps whatever handles events now, so Luvland first.
        (setf (luv:canvas-event-handler canvas) luvland)
        (luv.workbench:start-workbench luvland)
        (setf (luv:canvas-clock canvas) (make-frame-clock luvland))
        (setf *luvland* luvland)
        (when init-file-p
          (luv:request-canvas-frame canvas (lambda (timestamp)
                                             (declare (ignore timestamp))
                                             (load-init-file luvland))))
        (when spawn
          (apply #'spawn-client luvland spawn))
        luvland))))

(defun %stop-luvland (luvland)
  "Release LUVLAND in the workbench's order: quiesce, fence the last frame,
detach the shell, then the application's resources, canvas, and device."
  (setf (luvland-running-p luvland) nil)
  (let ((canvas (luvland-canvas luvland))
        (workbench (luvland-workbench luvland)))
    (when workbench (luv.workbench:quiesce-workbench workbench))
    (when (member (luv:canvas-state canvas) '(:opening :open))
      (setf (luv:canvas-clock canvas) (luv:make-demand-clock))
      (luv:request-canvas-frame canvas (lambda (timestamp) (declare (ignore timestamp)))))
    (setf (luv:canvas-event-handler canvas) nil)
    (when workbench (luv.workbench:stop-workbench workbench))
    (let ((frame (luvland-command-frame luvland)))
      (when (and frame (not (eq :disowned (clim:frame-state frame))))
        (clim:destroy-frame frame)))
    (dolist (process (luvland-processes luvland))
      (when (sb-ext:process-alive-p process)
        (sb-ext:process-kill process 15)))
    (wl:stop-server (luvland-server luvland))
    (when (member (luv:canvas-state canvas) '(:opening :open))
      (luv:close-canvas canvas))
    (luv:destroy (luvland-device luvland)))
  (when (eq luvland *luvland*)
    (setf *luvland* nil))
  (values))

(defun request-stop (luvland)
  "Stop LUVLAND from any thread, the canvas thread included."
  (luv:request-controlled-stop (luvland-stop-controller luvland)
                               (lambda () (%stop-luvland luvland))
                               :thread-name "Luvland stop"))

(defun stop-luvland (&optional (luvland *luvland*))
  "Close LUVLAND's workbench, clients, server, canvas, and device, once."
  (when luvland
    (luv:call-with-stop-controller (luvland-stop-controller luvland)
                                   (lambda () (%stop-luvland luvland))))
  (values))
