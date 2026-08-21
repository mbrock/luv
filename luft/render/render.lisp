;;; The greenfield atelier: dense face records drawn as realized patches.
;;;
;;; The CPU pipeline is the foundation's: occupancy -> solid 3-chain ->
;;; oriented surface 2-chain -> per-face shape word -> one 16-byte record
;;; per exposed face (LUFT:MATERIALIZE-SURFACE).  This file uploads that
;;; record array and the permanent 54-entry winding template, and issues one
;;; draw of 54 * FACE-COUNT vertices.  The vertex stage realizes every patch
;;; point from its face record alone; no occupancy travels to the GPU.

(in-package #:luft.render)

;;; ------------------------------------------------------------------------
;;; Worlds
;;;
;;; The foundation's SOLID-WORLD is the mutable occupancy.  These helpers
;;; carve a small demonstration world whose objects exercise every kind of
;;; edge and corner the chamfer classifier can author.

(defparameter *chamfer-width* 0.22d0
  "The reserved chamfer width, 0 < w < 1/2.")

(defun fill-box (world x0 x1 y0 y1 z0 z1 &optional (state t))
  "Set the box of cells [X0..X1] x [Y0..Y1] x [Z0..Z1] to STATE."
  (loop for z from z0 to z1
        do (loop for y from y0 to y1
                 do (loop for x from x0 to x1
                          do (setf (luft:solid-cell-p world x y z) state))))
  world)

(defun make-atelier-world (&key (horizontal-bits 5) (floor-height 2))
  "A floor carrying one exhibit of each crease and corner kind."
  (let* ((domain (luft:make-world-domain :horizontal-bits horizontal-bits))
         (world (luft:make-solid-world domain))
         (period (luft:world-domain-x-period domain))
         (z floor-height))
    ;; The floor.
    (fill-box world 0 (1- period) 0 (1- period) 0 (1- floor-height))
    ;; Convex exhibits resting on the floor.
    (fill-box world 3 3 3 3 z z)                  ; a lone cube
    (fill-box world 7 8 3 3 z z)                  ; a bar
    (fill-box world 12 13 3 3 z z)                ; an L
    (fill-box world 12 12 4 4 z z)
    (fill-box world 17 18 3 4 z z)                ; a 2x2 slab
    (fill-box world 22 23 3 4 z (1+ z))           ; a 2x2x2 block
    (fill-box world 27 27 3 3 z (+ z 3))          ; a pillar
    ;; Floating exhibits whose every corner is pure.
    (fill-box world 3 3 9 9 (+ z 2) (+ z 2))      ; a floating cube
    (fill-box world 7 9 9 9 (+ z 2) (+ z 2))      ; a floating bar
    (fill-box world 12 13 9 10 (+ z 2) (+ z 2))   ; a floating slab
    (fill-box world 17 17 9 9 (+ z 2) (+ z 2))    ; cubes meeting at an edge
    (fill-box world 18 18 10 10 (+ z 2) (+ z 2))
    ;; Concave exhibits cut into or built on the floor.
    (fill-box world 3 3 15 15 (1- z) (1- z) nil)  ; a one-cell pit
    (fill-box world 7 8 15 16 (1- z) (1- z) nil)  ; a 2x2 pit
    (loop for step from 0 below 3                 ; stairs
          do (fill-box world (+ 12 step) (+ 12 step) 15 17 z (+ z step)))
    (fill-box world 17 19 15 15 z (+ z 2))        ; a wall with a ledge
    (fill-box world 17 19 16 16 (+ z 2) (+ z 2))
    (fill-box world 23 25 15 17 z (+ z 1))        ; a block with a notch
    (fill-box world 23 24 15 16 z (+ z 1) nil)
    world))

;;; ------------------------------------------------------------------------
;;; Cameras

(defclass fly-camera ()
  ((position :initarg :position :accessor camera-position)
   (yaw :initarg :yaw :initform 0.0 :accessor camera-yaw)
   (pitch :initarg :pitch :initform 0.0 :accessor camera-pitch)
   (field-of-view :initarg :field-of-view
                  :initform (coerce (* 70.0 (/ pi 180)) 'single-float)
                  :accessor camera-field-of-view)))

(defun make-fly-camera (&key (position (vec3:make-vec3 26.0 -6.0 9.5))
                             (yaw 1.95) (pitch -0.35)
                             (field-of-view (coerce (* 70.0 (/ pi 180))
                                                    'single-float)))
  (make-instance 'fly-camera :position position :yaw yaw :pitch pitch
                             :field-of-view field-of-view))

(defun camera-basis (camera)
  "Return the camera's RIGHT, UP, and FORWARD unit vectors in a Z-up world."
  (let* ((yaw (camera-yaw camera))
         (pitch (camera-pitch camera))
         (forward (vec3:make-vec3 (* (cos yaw) (cos pitch))
                                  (* (sin yaw) (cos pitch))
                                  (sin pitch)))
         (right (vec3:make-vec3 (sin yaw) (- (cos yaw)) 0.0))
         (up (vec3:vec3-cross right forward)))
    (values right up forward)))

(defparameter *near-distance* 0.1)
(defparameter *far-distance* 400.0)

(defun frame-projection (camera width height)
  "Return CAMERA's four projection coefficients for WIDTH by HEIGHT."
  (let* ((near *near-distance*)
         (far *far-distance*)
         (focal (/ (tan (/ (camera-field-of-view camera) 2.0))))
         (aspect (/ (coerce width 'single-float) height)))
    (vector (coerce (/ focal aspect) 'single-float)
            (coerce focal 'single-float)
            (coerce (/ far (- far near)) 'single-float)
            (coerce (/ (- (* far near)) (- far near)) 'single-float))))

;;; ------------------------------------------------------------------------
;;; The frame block

(defparameter *sun-direction* (vec3:make-vec3 0.45 0.3 0.85))
(defparameter *ambient-light* 0.30)
(defparameter *exposure* 1.0)
(defparameter *sky-color* #(0.62 0.72 0.85 1.0))

(defun frame-uniform-data (camera width height)
  "Pack the frame block: 7 vec4s matching SHADERS:*FRAME-UNIFORM-MEMBERS*."
  (multiple-value-bind (right up forward) (camera-basis camera)
    (let* ((position (camera-position camera))
           (projection (frame-projection camera width height))
           (sun (vec3:vec3-normalize *sun-direction*))
           (data (make-array (* 4 (length shaders:*frame-uniform-members*))
                             :element-type 'single-float))
           (index 0))
      (flet ((lane (value)
               (setf (aref data index) (coerce value 'single-float))
               (incf index))
             (vec (v w)
               (dolist (value (list (vec3:vec3-x v) (vec3:vec3-y v)
                                    (vec3:vec3-z v) w))
                 (setf (aref data index) (coerce value 'single-float))
                 (incf index))))
        (vec position 0.0)
        (vec right 0.0)
        (vec up 0.0)
        (vec forward 0.0)
        (map nil #'lane projection)
        (vec sun *ambient-light*)
        (lane *chamfer-width*)
        (lane *exposure*)
        (lane 0.0)
        (lane 0.0))
      data)))

(defun frame-uniform-size ()
  (* 4 4 (length shaders:*frame-uniform-members*)))

;;; ------------------------------------------------------------------------
;;; The renderer

(defclass renderer ()
  ((device :initarg :device :reader renderer-device)
   (owns-device-p :initarg :owns-device-p :initform nil
                  :reader renderer-owns-device-p)
   (world :initarg :world :accessor renderer-world)
   (camera :initarg :camera :accessor renderer-camera)
   (extent :initarg :extent :accessor renderer-extent)
   (color-format :initarg :color-format :reader renderer-color-format)
   (layout :initform nil :accessor renderer-layout)
   (pipeline :initform nil :accessor renderer-pipeline)
   (modules :initform '() :accessor renderer-modules)
   (uniform-buffer :initform nil :accessor renderer-uniform-buffer)
   (template-buffer :initform nil :accessor renderer-template-buffer)
   (faces-buffer :initform nil :accessor renderer-faces-buffer)
   (faces-capacity :initform 0 :accessor renderer-faces-capacity)
   (face-count :initform 0 :accessor renderer-face-count)
   (bind-group :initform nil :accessor renderer-bind-group)
   (color-texture :initform nil :accessor renderer-color-texture)
   (color-view :initform nil :accessor renderer-color-view)
   (depth-texture :initform nil :accessor renderer-depth-texture)
   (depth-view :initform nil :accessor renderer-depth-view)))

(defun template-words ()
  "The positive winding template widened to u32 for the storage buffer."
  (let* ((template luft:+positive-face-indices+)
         (words (make-array (length template)
                            :element-type '(unsigned-byte 32))))
    (map-into words #'identity template)))

(defun create-renderer-targets (renderer)
  (let ((device (renderer-device renderer))
        (extent (renderer-extent renderer)))
    (setf (renderer-color-texture renderer)
          (create device
                  (make-texture-descriptor
                   :label "luft frame color" :size extent :dimensions :2d
                   :format (renderer-color-format renderer)
                   :usage '(:render-attachment :copy-src)))
          (renderer-color-view renderer)
          (create device (make-texture-view-descriptor
                          :texture (renderer-color-texture renderer)))
          (renderer-depth-texture renderer)
          (create device
                  (make-texture-descriptor
                   :label "luft frame depth" :size extent :dimensions :2d
                   :format :depth32-float :usage '(:render-attachment)))
          (renderer-depth-view renderer)
          (create device (make-texture-view-descriptor
                          :texture (renderer-depth-texture renderer))))))

(defun destroy-renderer-targets (renderer)
  (dolist (accessor (list #'renderer-color-view #'renderer-color-texture
                          #'renderer-depth-view #'renderer-depth-texture))
    (let ((resource (funcall accessor renderer)))
      (when resource (ignore-errors (destroy resource)))))
  (setf (renderer-color-texture renderer) nil
        (renderer-color-view renderer) nil
        (renderer-depth-texture renderer) nil
        (renderer-depth-view renderer) nil))

(defun ensure-renderer-extent (renderer extent)
  (unless (equal extent (renderer-extent renderer))
    (log-event :luft "reframing ~{~D~^x~} to ~{~D~^x~}"
               (renderer-extent renderer) extent)
    (destroy-renderer-targets renderer)
    (setf (renderer-extent renderer) extent)
    (create-renderer-targets renderer))
  renderer)

(defun create-renderer-bind-group (renderer)
  (create (renderer-device renderer)
          (make-bind-group-descriptor
           :label "luft patch bindings"
           :layout (renderer-layout renderer)
           :entries
           `((:binding ,shaders:+frame-binding+
              :resource ,(renderer-uniform-buffer renderer))
             (:binding ,shaders:+faces-binding+
              :resource ,(renderer-faces-buffer renderer))
             (:binding ,shaders:+template-binding+
              :resource ,(renderer-template-buffer renderer))))))

(defun upload-world (renderer &optional (world (renderer-world renderer)))
  "Materialize WORLD's surface and publish one coherent face-record buffer.

Returns the number of exposed faces."
  (multiple-value-bind (records faces)
      (luft:materialize-surface world *chamfer-width*)
    (let* ((device (renderer-device renderer))
           (needed (max 16 (* 4 (length records))))
           (grow-p (or (null (renderer-faces-buffer renderer))
                       (> needed (renderer-faces-capacity renderer)))))
      (when grow-p
        (let ((buffer (create device
                              (make-buffer-descriptor
                               :label "luft face records"
                               :size (max needed
                                          (* 2 (renderer-faces-capacity
                                                renderer)))
                               :usage '(:storage))))
              (old-buffer (renderer-faces-buffer renderer))
              (old-group (renderer-bind-group renderer)))
          (setf (renderer-faces-buffer renderer) buffer
                (renderer-faces-capacity renderer) needed)
          (setf (renderer-bind-group renderer)
                (create-renderer-bind-group renderer))
          (when old-group (ignore-errors (destroy old-group)))
          (when old-buffer (ignore-errors (destroy old-buffer)))))
      (when (plusp (length records))
        (write-buffer (renderer-faces-buffer renderer) records))
      (setf (renderer-face-count renderer) (length faces)
            (renderer-world renderer) world)
      (length faces))))

(defun make-renderer (&key world camera device
                           (provider *gpu-provider*)
                           (width 1280) (height 800)
                           (color-format :rgba8-unorm-srgb))
  "Create every GPU object needed to draw WORLD from CAMERA."
  (let* ((owns-device-p (null device))
         (device (or device
                     (request-gpu-device
                      provider
                      (make-device-descriptor :label "luft atelier"))))
         (renderer (make-instance 'renderer
                                  :device device :owns-device-p owns-device-p
                                  :world world
                                  :camera (or camera (make-fly-camera))
                                  :extent (list width height)
                                  :color-format color-format))
         (completed-p nil))
    (unwind-protect
         (progn
           (setf (renderer-uniform-buffer renderer)
                 (create device (make-buffer-descriptor
                                 :label "luft frame block"
                                 :size (frame-uniform-size)
                                 :usage '(:uniform)))
                 (renderer-template-buffer renderer)
                 (create device (make-buffer-descriptor
                                 :label "luft winding template"
                                 :size (* 4 shaders:+patch-vertices-per-face+)
                                 :usage '(:storage)))
                 (renderer-layout renderer)
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft patch layout"
                          :entries
                          `((:binding ,shaders:+frame-binding+
                             :type :uniform-buffer)
                            (:binding ,shaders:+faces-binding+
                             :type :storage-buffer)
                            (:binding ,shaders:+template-binding+
                             :type :storage-buffer)))))
           (write-buffer (renderer-template-buffer renderer) (template-words))
           (let ((vertex (create device
                                 (make-shader-module-descriptor
                                  :label "luft patch vertex"
                                  :language :mathematical
                                  :code (shaders:patch-vertex-shader))))
                 (fragment (create device
                                   (make-shader-module-descriptor
                                    :label "luft patch fragment"
                                    :language :mathematical
                                    :code (shaders:patch-fragment-shader)))))
             (push vertex (renderer-modules renderer))
             (push fragment (renderer-modules renderer))
             (setf (renderer-pipeline renderer)
                   (create device
                           (make-render-pipeline-descriptor
                            :label "luft patch pipeline"
                            :layout (renderer-layout renderer)
                            :vertex `(:module ,vertex)
                            :fragment
                            `(:module ,fragment
                              :targets ((:format ,color-format)))
                            :primitive '(:topology :triangle-list)
                            :depth-stencil
                            '(:format :depth32-float
                              :depth-write-enabled t
                              :depth-compare :less)))))
           (create-renderer-targets renderer)
           (upload-world renderer)
           (setf completed-p t)
           renderer)
      (unless completed-p
        (destroy-renderer renderer)))))

(defun destroy-renderer (renderer)
  "Release every GPU object of RENDERER, and its device when it owns one."
  (destroy-renderer-targets renderer)
  (dolist (accessor (list #'renderer-bind-group #'renderer-faces-buffer
                          #'renderer-template-buffer
                          #'renderer-uniform-buffer #'renderer-pipeline
                          #'renderer-layout))
    (let ((resource (funcall accessor renderer)))
      (when resource (ignore-errors (destroy resource)))))
  (dolist (module (renderer-modules renderer))
    (ignore-errors (destroy module)))
  (setf (renderer-modules renderer) '()
        (renderer-bind-group renderer) nil
        (renderer-faces-buffer renderer) nil
        (renderer-template-buffer renderer) nil
        (renderer-uniform-buffer renderer) nil
        (renderer-pipeline renderer) nil
        (renderer-layout renderer) nil)
  (when (renderer-owns-device-p renderer)
    (ignore-errors (destroy (renderer-device renderer))))
  (values))

;;; ------------------------------------------------------------------------
;;; Encoding one frame

(defun encode-frame (renderer encoder)
  "Encode one frame of RENDERER into ENCODER; return the color texture."
  (destructuring-bind (width height) (renderer-extent renderer)
    (write-buffer (renderer-uniform-buffer renderer)
                  (frame-uniform-data (renderer-camera renderer)
                                      width height)))
  (let ((pass (begin-render-pass
               encoder
               (make-render-pass-descriptor
                :label "luft patch pass"
                :color-attachments
                (list `(:view ,(renderer-color-view renderer)
                        :load-op :clear :store-op :store
                        :clear-value ,*sky-color*))
                :depth-stencil-attachment
                `(:view ,(renderer-depth-view renderer)
                  :depth-load-op :clear
                  :depth-store-op :discard
                  :depth-clear-value 1.0)))))
    (when (plusp (renderer-face-count renderer))
      (set-pipeline pass (renderer-pipeline renderer))
      (set-bind-group pass 0 (renderer-bind-group renderer))
      (draw pass (* shaders:+patch-vertices-per-face+
                    (renderer-face-count renderer))))
    (end-pass pass))
  (renderer-color-texture renderer))

;;; ------------------------------------------------------------------------
;;; Headless capture

(defun render-pixels (renderer)
  "Render one frame headlessly and return its packed pixel bytes."
  (let* ((device (renderer-device renderer))
         (extent (renderer-extent renderer))
         (readback (create device
                           (make-buffer-descriptor
                            :label "luft readback"
                            :size (* 4 (first extent) (second extent))
                            :usage '(:copy-dst))))
         (encoder nil)
         (command-buffer nil))
    (unwind-protect
         (progn
           (setf encoder (create device
                                 (make-command-encoder-descriptor
                                  :label "luft capture frame")))
           (encode-frame renderer encoder)
           (encode encoder
                   (make-gpu-copy-texture-to-buffer-command
                    :source (renderer-color-texture renderer)
                    :destination readback))
           (setf command-buffer (finish encoder))
           (submit (device-queue device) command-buffer)
           (values (read-buffer readback)
                   (first extent) (second extent)
                   (renderer-color-format renderer)))
      (when command-buffer (destroy command-buffer))
      (when encoder (destroy encoder))
      (destroy readback))))

(defun render-to-png (renderer pathname)
  "Render one frame headlessly and write it to PATHNAME as a PNG."
  (multiple-value-bind (pixels width height format) (render-pixels renderer)
    (ensure-directories-exist pathname)
    (write-rgba-png pathname pixels width height format))
  pathname)

(defun capture-atelier-png (pathname &key (width 1280) (height 800)
                                          (world (make-atelier-world))
                                          (camera (make-fly-camera)))
  "Render the atelier world once to PATHNAME and release everything."
  (let ((renderer (make-renderer :world world :camera camera
                                 :width width :height height)))
    (unwind-protect
         (render-to-png renderer pathname)
      (destroy-renderer renderer))))

;;; ------------------------------------------------------------------------
;;; Viewer: a window with a fly camera

(defvar *viewer* nil "The most recently started viewer.")

(defclass viewer (canvas-event-handler)
  ((canvas :initarg :canvas :reader viewer-canvas)
   (context :initarg :context :reader viewer-context)
   (renderer :initarg :renderer :accessor viewer-renderer)
   (pressed-keys :initform (make-hash-table :test #'eq)
                 :reader viewer-pressed-keys)
   (pointer-captured-p :initform nil :accessor viewer-pointer-captured-p)
   (running-p :initform t :accessor viewer-running-p)
   (last-timestamp :initform nil :accessor viewer-last-timestamp)
   (speed :initarg :speed :initform 10.0 :accessor viewer-speed)
   (sensitivity :initarg :sensitivity :initform 0.0032
                :accessor viewer-sensitivity)))

(defun viewer-key-down-p (viewer &rest names)
  (some (lambda (name) (gethash name (viewer-pressed-keys viewer))) names))

(defun advance-viewer-camera (viewer timestamp)
  (let* ((last (viewer-last-timestamp viewer))
         (dt (if last (min 0.1 (max 0.0 (- timestamp last))) 0.0))
         (camera (renderer-camera (viewer-renderer viewer)))
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
  (unless (viewer-running-p viewer)
    (return-from render-viewer-frame nil))
  (present-canvas-frame
   (viewer-context viewer)
   (lambda (surface-texture encoder presentation-time)
     (ensure-renderer-extent (viewer-renderer viewer)
                             (canvas-extent (viewer-context viewer)))
     (advance-viewer-camera viewer presentation-time)
     (let ((color (encode-frame (viewer-renderer viewer) encoder)))
       (encode encoder
               (make-gpu-copy-texture-command
                :source color :destination surface-texture))))))

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-key-press-event))
  (let ((key (canvas-key-event-key-name event)))
    (if (eq key :escape)
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
  (when (and (not (viewer-pointer-captured-p viewer))
             (eq :left (canvas-pointer-event-button event)))
    (set-canvas-relative-pointer-mode canvas t)
    (setf (viewer-pointer-captured-p viewer) t))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-motion-event))
  (declare (ignore canvas))
  (when (viewer-pointer-captured-p viewer)
    (let ((camera (renderer-camera (viewer-renderer viewer)))
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
  (declare (ignore canvas))
  (clrhash (viewer-pressed-keys viewer))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-window-close-request-event))
  (declare (ignore canvas event))
  (setf (viewer-running-p viewer) nil)
  nil)

(defmethod handle-canvas-event ((viewer viewer) canvas event)
  (declare (ignore viewer canvas event))
  nil)

(defun start-viewer (&key (world (make-atelier-world))
                          (camera (make-fly-camera))
                          (title "luft atelier")
                          (width 1280) (height 800)
                          (frames-per-second 60)
                          (provider *gpu-provider*))
  "Open a window flying through WORLD and return the running VIEWER.

Click to capture the pointer; Escape releases it; WASD, Space, and C move."
  (let ((canvas (make-sdl-canvas
                 :title title :width width :height height :visible-p nil
                 :presentation-api (sdl-presentation-api-for provider)))
        (device nil)
        (renderer nil)
        (completed-p nil))
    (open-canvas canvas)
    (unwind-protect
         (let* ((device* (setf device
                               (request-gpu-device
                                provider
                                (make-device-descriptor :label title))))
                (context (make-canvas-context
                          canvas provider
                          (make-canvas-configuration :device device*)))
                (extent (canvas-extent context))
                (renderer* (setf renderer
                                 (make-renderer
                                  :world world :camera camera :device device*
                                  :width (first extent)
                                  :height (second extent)
                                  :color-format (canvas-format context))))
                (viewer (make-instance 'viewer :canvas canvas :context context
                                               :renderer renderer*)))
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
           (setf completed-p t
                 *viewer* viewer)
           viewer)
      (unless completed-p
        (when renderer (destroy-renderer renderer))
        (close-canvas canvas)
        (when device (destroy device))))))

(defun stop-viewer (&optional (viewer *viewer*))
  "Close VIEWER's window and release its renderer and device."
  (when viewer
    (setf (viewer-running-p viewer) nil)
    (let* ((canvas (viewer-canvas viewer))
           (renderer (viewer-renderer viewer))
           (device (and renderer (renderer-device renderer))))
      (when (eq :open (canvas-state canvas))
        (setf (canvas-clock canvas) (make-demand-clock)))
      (when renderer
        (destroy-renderer renderer)
        (setf (viewer-renderer viewer) nil))
      (when (eq :open (canvas-state canvas))
        (close-canvas canvas))
      (when device
        (ignore-errors (destroy device))))
    (when (eq viewer *viewer*)
      (setf *viewer* nil)))
  (values))
