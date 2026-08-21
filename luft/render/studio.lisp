(in-package #:luft.render)

(defvar *viewer* nil)

(defparameter *chamfer-width* 0.11
  "The reserved chamfer band, in cells; the shape rule may fill 0 < w < 1/2.

The pre-refoundation atelier used 0.11.  The width scales every displacement
the shape word asks for, so it also scales the disagreement a mixed corner
has with the creases running into it: at 0.20 that disagreement is a visible
gouge, and at 0.11 it is the subtle break it was drawn as.")

(defparameter *wireframe* 0.0
  "How strongly the lattice wireframe is inked over the surface, 0 to 1.

The wires are the 4x4 point grid and each cell's shared diagonal, which is
the way to read a triangulation and exactly the wrong way to judge whether a
crease looks right: an inked crease reads as a fold whether or not it is one.")

(defparameter *projection* :isometric
  "Either :PERSPECTIVE or :ISOMETRIC.

An isometric picture has no vanishing point, so two chamfers the same width
are the same width on screen wherever they sit.  That is what makes it the
projection to judge a shape rule in.")

(defparameter *isometric-height* 12.0
  "How many world units of height an isometric frame spans.")

(defclass fly-camera ()
  ((position :initarg :position :accessor camera-position)
   (yaw :initarg :yaw :initform 0.0 :accessor camera-yaw)
   (pitch :initarg :pitch :initform 0.0 :accessor camera-pitch)
   (field-of-view :initarg :field-of-view :initform (* 70.0 (/ pi 180))
                  :accessor camera-field-of-view)))

(defun make-fly-camera
    (&key (position (vec3:make-vec3 16.954 -0.954 8.970))
          (yaw 2.3561945) (pitch -0.44051066)
          (field-of-view 0.9599311))
  (make-instance 'fly-camera :position position :yaw yaw :pitch pitch
                             :field-of-view field-of-view))

(defun camera-basis (camera)
  (let* ((yaw (camera-yaw camera))
         (pitch (camera-pitch camera))
         (forward (vec3:make-vec3 (* (cos yaw) (cos pitch))
                                  (* (sin yaw) (cos pitch))
                                  (sin pitch)))
         (right (vec3:make-vec3 (sin yaw) (- (cos yaw)) 0.0))
         (up (vec3:vec3-cross right forward)))
    (values right up forward)))

(defun projection-lane (width height field-of-view near far)
  "The four projection coefficients and the homogeneous-divisor selector.

Both projections use the same three rows: clip X and Y are the view
coordinates scaled, and clip Z is an affine function of view depth.  The
perspective divisor is the view depth and the isometric divisor is one, so
the selector is the whole of the difference."
  (let ((aspect (/ (coerce width 'single-float) height)))
    (ecase *projection*
      (:perspective
       (let ((focal (/ (tan (/ field-of-view 2.0)))))
         (values (/ focal aspect) focal
                 (/ far (- far near))
                 (/ (- (* far near)) (- far near))
                 1.0)))
      (:isometric
       (let ((half (/ *isometric-height* 2.0)))
         (values (/ 1.0 (* half aspect)) (/ 1.0 half)
                 (/ 1.0 (- far near))
                 (/ (- near) (- far near))
                 0.0))))))

(defun camera-uniform-data (camera width height)
  (multiple-value-bind (right up forward) (camera-basis camera)
    (let ((near 0.1)
          (far 200.0))
      (flet ((lane (vector fourth)
               (list (vec3:vec3-x vector) (vec3:vec3-y vector)
                     (vec3:vec3-z vector) fourth)))
        (multiple-value-bind (px py pz pw divisor)
            (projection-lane width height (camera-field-of-view camera)
                             near far)
          (make-array
           24 :element-type 'single-float
           :initial-contents
           (mapcar
            (lambda (value) (coerce value 'single-float))
            (append (lane (camera-position camera) 0.0)
                    (lane right 0.0) (lane up 0.0) (lane forward 0.0)
                    (list px py pz pw)
                    (list *chamfer-width* *wireframe* divisor 0.0)))))))))

(defclass viewer (canvas-event-handler)
  ((canvas :initarg :canvas :reader viewer-canvas)
   (context :initarg :context :reader viewer-context)
   (device :initarg :device :reader viewer-device)
   (renderer :initarg :renderer :accessor viewer-renderer)
   (camera :initarg :camera :reader viewer-camera)
   (pressed-keys :initform (make-hash-table :test #'eq)
                 :reader viewer-pressed-keys)
   (pointer-captured-p :initform nil :accessor viewer-pointer-captured-p)
   (last-timestamp :initform nil :accessor viewer-last-timestamp)
   (speed :initarg :speed :initform 12.0 :accessor viewer-speed)
   (sensitivity :initarg :sensitivity :initform 0.0032
                :accessor viewer-sensitivity)
   (running-p :initform t :accessor viewer-running-p)))

(defun viewer-key-down-p (viewer &rest names)
  (some (lambda (name) (gethash name (viewer-pressed-keys viewer))) names))

(defun advance-viewer-camera (viewer timestamp)
  (let* ((last (viewer-last-timestamp viewer))
         (dt (if last (min 0.1 (max 0.0 (- timestamp last))) 0.0))
         (camera (viewer-camera viewer))
         (step (* dt (viewer-speed viewer)
                  (if (viewer-key-down-p viewer :left-shift :right-shift)
                      3.0 1.0))))
    (setf (viewer-last-timestamp viewer) timestamp)
    (multiple-value-bind (right up forward) (camera-basis camera)
      (declare (ignore up))
      (flet ((move (direction amount)
               (let ((position (camera-position camera)))
                 (setf (camera-position camera)
                       (vec3:make-vec3
                        (+ (vec3:vec3-x position)
                           (* amount (vec3:vec3-x direction)))
                        (+ (vec3:vec3-y position)
                           (* amount (vec3:vec3-y direction)))
                        (+ (vec3:vec3-z position)
                           (* amount (vec3:vec3-z direction))))))))
        (when (viewer-key-down-p viewer :w :up) (move forward step))
        (when (viewer-key-down-p viewer :s :down) (move forward (- step)))
        (when (viewer-key-down-p viewer :d :right) (move right step))
        (when (viewer-key-down-p viewer :a :left) (move right (- step)))
        (when (viewer-key-down-p viewer :space :e)
          (move (vec3:make-vec3 0 0 1) step))
        (when (viewer-key-down-p viewer :left-control :q :c)
          (move (vec3:make-vec3 0 0 1) (- step)))))))

(defun render-viewer-frame (viewer timestamp)
  (declare (ignore timestamp))
  (when (viewer-running-p viewer)
    (present-canvas-frame
     (viewer-context viewer)
     (lambda (surface-texture encoder presentation-time)
       (advance-viewer-camera viewer presentation-time)
       (let ((extent (canvas-extent (viewer-context viewer))))
         (encode-renderer-frame
          (viewer-renderer viewer) encoder surface-texture
          extent
          (camera-uniform-data (viewer-camera viewer)
                               (first extent) (second extent))))))))

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-window-close-request-event))
  (declare (ignore canvas event))
  (setf (viewer-running-p viewer) nil)
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-key-press-event))
  (let ((key (canvas-key-event-key-name event)))
    (if (eq :escape key)
        (when (viewer-pointer-captured-p viewer)
          (set-canvas-relative-pointer-mode canvas nil)
          (setf (viewer-pointer-captured-p viewer) nil))
        (setf (gethash key (viewer-pressed-keys viewer)) t)))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-key-release-event))
  (declare (ignore canvas))
  (remhash (canvas-key-event-key-name event) (viewer-pressed-keys viewer))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-button-press-event))
  (when (and (eq :left (canvas-pointer-event-button event))
             (not (viewer-pointer-captured-p viewer)))
    (set-canvas-relative-pointer-mode canvas t)
    (setf (viewer-pointer-captured-p viewer) t))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-motion-event))
  (declare (ignore canvas))
  (when (viewer-pointer-captured-p viewer)
    (let ((camera (viewer-camera viewer))
          (sensitivity (viewer-sensitivity viewer)))
      (decf (camera-yaw camera)
            (* (canvas-pointer-event-delta-x event) sensitivity))
      (setf (camera-pitch camera)
            (max -1.5 (min 1.5
                           (- (camera-pitch camera)
                              (* (canvas-pointer-event-delta-y event)
                                 sensitivity)))))))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-window-focus-lost-event))
  (declare (ignore canvas event))
  (clrhash (viewer-pressed-keys viewer))
  nil)

(defmethod handle-canvas-event ((viewer viewer) canvas event)
  (declare (ignore viewer canvas event))
  nil)

(defun start-viewer (&key
                       (solid (make-gallery-solid))
                       (camera (make-fly-camera))
                       (title "LUFT indexed faces")
                       (width 1100) (height 800)
                       (frames-per-second 60)
                       (provider *gpu-provider*))
  "Open the greenfield indexed-instanced LUFT atelier."
  (let ((canvas
          (make-sdl-canvas
           :title title :width width :height height :visible-p nil
           :presentation-api (sdl-presentation-api-for provider)))
        (device nil)
        (renderer nil)
        (completed-p nil))
    (open-canvas canvas)
    (unwind-protect
         (let* ((device*
                  (setf device
                        (request-gpu-device
                         provider (make-device-descriptor :label title))))
                (context
                  (make-canvas-context
                   canvas provider
                   ;; :copy-src drops the Metal framebuffer-only contract so
                   ;; CAPTURE-VIEWER-FRAME can read the drawable back.
                   (make-canvas-configuration
                    :device device*
                    :usage '(:render-attachment :copy-src))))
                (renderer*
                  (setf renderer
                        (make-renderer
                         device* (make-face-materialization solid)
                         (canvas-format context) (canvas-extent context))))
                (viewer
                  (make-instance 'viewer :canvas canvas :context context
                                         :device device* :renderer renderer*
                                         :camera camera)))
           (setf (canvas-event-handler canvas) viewer)
           (request-canvas-frame
            canvas (lambda (timestamp) (render-viewer-frame viewer timestamp)))
           (show-canvas canvas)
           (setf (canvas-clock canvas)
                 (make-cadence-clock
                  (lambda (native-canvas timestamp)
                    (declare (ignore native-canvas))
                    (render-viewer-frame viewer timestamp))
                  :frames-per-second frames-per-second))
           (setf *viewer* viewer
                 completed-p t)
           viewer)
      (unless completed-p
        (when renderer (destroy-renderer renderer))
        (when (eq :open (canvas-state canvas)) (close-canvas canvas))
        (when device (ignore-errors (destroy device)))))))

(defun capture-viewer-frame (pathname &optional (viewer *viewer*))
  "Render one VIEWER frame on its canvas thread and write it to PATHNAME."
  (let* ((context (viewer-context viewer))
         (extent (canvas-extent context))
         (pathname (merge-pathnames pathname))
         (buffer
           (create (viewer-device viewer)
                   (make-buffer-descriptor
                    :label "luft capture readback"
                    :size (* 4 (first extent) (second extent))
                    :usage '(:copy-dst)))))
    (unwind-protect
         (progn
           (luv::call-on-sdl-canvas-thread
            (viewer-canvas viewer)
            (lambda ()
              (present-canvas-frame
               context
               (lambda (surface-texture encoder presentation-time)
                 (declare (ignore presentation-time))
                 (encode-renderer-frame
                  (viewer-renderer viewer) encoder surface-texture extent
                  (camera-uniform-data (viewer-camera viewer)
                                       (first extent) (second extent)))
                 (encode encoder
                         (make-gpu-copy-texture-to-buffer-command
                          :source surface-texture :destination buffer))))))
           (ensure-directories-exist pathname)
           (write-rgba-png pathname (read-buffer buffer)
                           (first extent) (second extent)
                           (canvas-format context)))
      (destroy buffer))))

(defun refresh-viewer-renderer (&optional (viewer *viewer*)
                                &key (solid (make-gallery-solid)))
  "Rebuild VIEWER's renderer so edited shaders and geometry take effect."
  (when viewer
    (let* ((context (viewer-context viewer))
           (old (viewer-renderer viewer))
           (materialization (make-face-materialization solid)))
      (setf (viewer-running-p viewer) nil)
      (unwind-protect
           (setf (viewer-renderer viewer)
                 (make-renderer (viewer-device viewer) materialization
                                (canvas-format context)
                                (canvas-extent context)))
        (setf (viewer-running-p viewer) t))
      (when old (destroy-renderer old))))
  (values))

(defun stop-viewer (&optional (viewer *viewer*))
  (when viewer
    (setf (viewer-running-p viewer) nil)
    (let ((canvas (viewer-canvas viewer)))
      (when (eq :open (canvas-state canvas))
        (when (viewer-pointer-captured-p viewer)
          (set-canvas-relative-pointer-mode canvas nil)
          (setf (viewer-pointer-captured-p viewer) nil))
        (setf (canvas-clock canvas) (make-demand-clock)))
      (when (viewer-renderer viewer)
        (destroy-renderer (viewer-renderer viewer))
        (setf (viewer-renderer viewer) nil))
      (when (eq :open (canvas-state canvas)) (close-canvas canvas)))
    (ignore-errors (destroy (viewer-device viewer)))
    (when (eq viewer *viewer*) (setf *viewer* nil)))
  (values))

(defun run-standalone-viewer ()
  (let ((viewer (start-viewer)))
    (unwind-protect
         (loop while (viewer-running-p viewer) do (sleep 0.05))
      (stop-viewer viewer))))
