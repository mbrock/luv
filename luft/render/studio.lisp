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

(defparameter *isometric-height* 64.0
  "How many world units of height an isometric frame spans.")

(defclass fly-camera ()
  ((position :initarg :position :accessor camera-position)
   (yaw :initarg :yaw :initform 0.0 :accessor camera-yaw)
   (pitch :initarg :pitch :initform 0.0 :accessor camera-pitch)
   (field-of-view :initarg :field-of-view :initform (* 70.0 (/ pi 180))
                  :accessor camera-field-of-view)))

(defun make-fly-camera
    (&key (position (vec3:make-vec3 70.0 -18.0 50.0))
          (yaw 2.2455373) (pitch -0.5165006)
          (field-of-view 0.9599311))
  (make-instance 'fly-camera :position position :yaw yaw :pitch pitch
                             :field-of-view field-of-view))

(defun reset-viewer-camera (&optional (viewer *viewer*))
  "Return VIEWER to the composed isometric sanctuary view."
  (when viewer
    (let ((camera (viewer-camera viewer)))
      (setf (camera-position camera) (vec3:make-vec3 70.0 -18.0 50.0)
            (camera-yaw camera) 2.2455373
            (camera-pitch camera) -0.5165006
            (camera-field-of-view camera) 0.9599311
            *isometric-height* 64.0)
      (when (viewer-renderer viewer)
        (setf (renderer-history-valid-p (viewer-renderer viewer)) nil))))
  viewer)

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

(defstruct (frame-view (:constructor %make-frame-view))
  "One immutable camera sample shared by geometry and temporal motion."
  position right up forward projection divisor jitter)

(defun halton (index base)
  (loop with fraction = (/ 1.0 base)
        with value = 0.0
        while (plusp index)
        do (incf value (* fraction (mod index base)))
           (setf index (floor index base)
                 fraction (/ fraction base))
        finally (return (coerce value 'single-float))))

(defun temporal-jitter (frame-index width height)
  "Sample the eight-position Halton(2,3) sequence in clip coordinates."
  (let ((sample (1+ (mod frame-index 8))))
    (vector (coerce (/ (* 2.0 (- (halton sample 2) 0.5))
                       (max width 1))
                    'single-float)
            (coerce (/ (* 2.0 (- (halton sample 3) 0.5))
                       (max height 1))
                    'single-float))))

(defun capture-frame-view (camera width height jitter)
  (multiple-value-bind (right up forward) (camera-basis camera)
    (let ((near 0.1)
          (far 200.0))
      (multiple-value-bind (px py pz pw divisor)
          (projection-lane width height (camera-field-of-view camera)
                           near far)
        (%make-frame-view
         :position (let ((position (camera-position camera)))
                     (vec3:make-vec3 (vec3:vec3-x position)
                                     (vec3:vec3-y position)
                                     (vec3:vec3-z position)))
         :right right :up up :forward forward
         :projection (vector px py pz pw)
         :divisor divisor :jitter jitter)))))

(defun camera-uniform-data (view previous)
  (flet ((lane (vector fourth)
           (list (vec3:vec3-x vector) (vec3:vec3-y vector)
                 (vec3:vec3-z vector) fourth)))
    (make-array
     48 :element-type 'single-float
     :initial-contents
     (mapcar
      (lambda (value) (coerce value 'single-float))
      (append (lane (frame-view-position view) 0.0)
              (lane (frame-view-right view) 0.0)
              (lane (frame-view-up view) 0.0)
              (lane (frame-view-forward view) 0.0)
              (coerce (frame-view-projection view) 'list)
              (list *chamfer-width* *wireframe*
                    (frame-view-divisor view) 0.0)
              (lane (frame-view-position previous) 0.0)
              (lane (frame-view-right previous) 0.0)
              (lane (frame-view-up previous) 0.0)
              (lane (frame-view-forward previous) 0.0)
              (coerce (frame-view-projection previous) 'list)
              (list (aref (frame-view-jitter view) 0)
                    (aref (frame-view-jitter view) 1)
                    (frame-view-divisor previous) 0.0))))))

(defun encode-viewer-frame (viewer encoder surface-texture extent)
  (let* ((renderer (viewer-renderer viewer))
         (width (first extent))
         (height (second extent))
         (jitter (if (renderer-temporal-p renderer)
                     (temporal-jitter (renderer-frame-index renderer)
                                      width height)
                     #(0.0 0.0)))
         (view (capture-frame-view (viewer-camera viewer)
                                   width height jitter))
         (previous (or (renderer-previous-view renderer) view)))
    (encode-renderer-frame
     renderer encoder surface-texture extent
     (camera-uniform-data view previous)
     :jitter jitter :view view)))

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
         (step (* dt (viewer-speed viewer))))
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
        ;; W/S dolly along the exact 3D viewing ray.  The wheel alone changes
        ;; isometric scale; Space/Shift remain independent world-Z movement.
        (when (viewer-key-down-p viewer :w :up) (move forward step))
        (when (viewer-key-down-p viewer :s :down) (move forward (- step)))
        (when (viewer-key-down-p viewer :d :right) (move right step))
        (when (viewer-key-down-p viewer :a :left) (move right (- step)))
        (when (viewer-key-down-p viewer :space)
          (move (vec3:make-vec3 0 0 1) step))
        (when (viewer-key-down-p viewer :left-shift :right-shift)
          (move (vec3:make-vec3 0 0 1) (- step)))))))

(defun render-viewer-frame (viewer timestamp)
  (declare (ignore timestamp))
  (when (viewer-running-p viewer)
    (present-canvas-frame
     (viewer-context viewer)
     (lambda (surface-texture encoder presentation-time)
       (advance-viewer-camera viewer presentation-time)
       (let ((extent (canvas-extent (viewer-context viewer))))
         (encode-viewer-frame viewer encoder surface-texture extent))))))

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-window-close-request-event))
  (declare (ignore canvas event))
  (setf (viewer-running-p viewer) nil)
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-key-press-event))
  (let ((key (canvas-key-event-key-name event)))
    (cond ((eq :escape key)
           (when (viewer-pointer-captured-p viewer)
             (set-canvas-relative-pointer-mode canvas nil)
             (setf (viewer-pointer-captured-p viewer) nil)))
          ((eq :r key) (reset-viewer-camera viewer))
          (t (setf (gethash key (viewer-pressed-keys viewer)) t))))
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
    ((viewer viewer) canvas (event canvas-pointer-wheel-event))
  (declare (ignore viewer canvas))
  (when (eq *projection* :isometric)
    (setf *isometric-height*
          (max 6.0 (min 96.0
                        (* *isometric-height*
                           (expt 1.10
                                 (canvas-pointer-event-delta-y event)))))))
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
                       (solid (make-mountain-sanctuary-scene))
                       (camera (make-fly-camera))
                       (title "LUFT mountain sanctuary")
                       (width 1100) (height 800)
                       (frames-per-second 60)
                       (provider *gpu-provider*))
  "Open the greenfield indexed-instanced LUFT atelier."
  (let ((canvas
          (make-sdl-canvas
           :title title :width width :height height :visible-p nil
           :high-pixel-density-p t
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
                 (encode-viewer-frame viewer encoder surface-texture extent)
                 (encode encoder
                         (make-gpu-copy-texture-to-buffer-command
                          :source surface-texture :destination buffer))))))
           (ensure-directories-exist pathname)
           (write-rgba-png pathname (read-buffer buffer)
                           (first extent) (second extent)
                           (canvas-format context)))
      (destroy buffer))))

(defun refresh-viewer-renderer (&optional (viewer *viewer*)
                                &key (solid (make-mountain-sanctuary-scene)))
  "Rebuild VIEWER's renderer so edited shaders and geometry take effect."
  (when viewer
    (luv::call-on-sdl-canvas-thread
     (viewer-canvas viewer)
     (lambda ()
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
         (when old (destroy-renderer old))))))
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
