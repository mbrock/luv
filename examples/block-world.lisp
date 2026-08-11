;;; A small CPU-authored block world and a first-person canvas demo.

(in-package #:luv)

;;; Blocks and faces are ordinary CLOS objects so the interesting semantic
;;; choices stay inspectable and redefinable at the REPL.  The result of
;;; meshing is deliberately mundane: interleaved XYZ/RGB single floats.

(defclass block-kind ()
  ((name :initarg :name :reader block-kind-name)
   (color :initarg :color :reader block-kind-color)))

(defclass grass-block-kind (block-kind) ())

(defgeneric block-solid-p (block))
(defgeneric block-face-color (block face))

(defmethod block-solid-p ((block null)) nil)
(defmethod block-solid-p ((block block-kind)) t)

(defclass block-face ()
  ((name :initarg :name :reader block-face-name)
   (neighbor :initarg :neighbor :reader block-face-neighbor)
   (corners :initarg :corners :reader block-face-corners)
   (shade :initarg :shade :reader block-face-shade)))

(defun make-block-face (name neighbor corners shade)
  (make-instance 'block-face :name name :neighbor neighbor
                              :corners corners :shade shade))

(defparameter *block-faces*
  (list
   (make-block-face :left '(-1 0 0)
                    '((0 0 0) (0 0 1) (0 1 1) (0 1 0)) 0.72)
   (make-block-face :right '(1 0 0)
                    '((1 0 1) (1 0 0) (1 1 0) (1 1 1)) 0.84)
   (make-block-face :bottom '(0 -1 0)
                    '((0 0 1) (0 0 0) (1 0 0) (1 0 1)) 0.55)
   (make-block-face :top '(0 1 0)
                    '((0 1 0) (0 1 1) (1 1 1) (1 1 0)) 1.0)
   (make-block-face :back '(0 0 -1)
                    '((1 0 0) (0 0 0) (0 1 0) (1 1 0)) 0.66)
   (make-block-face :front '(0 0 1)
                    '((0 0 1) (1 0 1) (1 1 1) (0 1 1)) 0.9)))

(defmethod block-face-color ((block block-kind) (face block-face))
  (map 'vector
       (lambda (component)
         (coerce (* component (block-face-shade face)) 'single-float))
       (block-kind-color block)))

(defmethod block-face-color ((block grass-block-kind) (face block-face))
  (let ((color (case (block-face-name face)
                 (:top #(0.34 0.72 0.24))
                 (:bottom #(0.42 0.25 0.12))
                 (otherwise #(0.40 0.58 0.20)))))
    (map 'vector
         (lambda (component)
           (coerce (* component (block-face-shade face)) 'single-float))
         color)))

(defparameter *grass-block*
  (make-instance 'grass-block-kind :name :grass
                                   :color #(0.34 0.72 0.24)))
(defparameter *dirt-block*
  (make-instance 'block-kind :name :dirt :color #(0.50 0.31 0.16)))
(defparameter *stone-block*
  (make-instance 'block-kind :name :stone :color #(0.52 0.55 0.58)))
(defparameter *wood-block*
  (make-instance 'block-kind :name :wood :color #(0.48 0.31 0.13)))
(defparameter *leaf-block*
  (make-instance 'block-kind :name :leaves :color #(0.20 0.56 0.23)))

(defclass block-world ()
  ((width :initarg :width :reader block-world-width)
   (height :initarg :height :reader block-world-height)
   (depth :initarg :depth :reader block-world-depth)
   (blocks :initarg :blocks :reader block-world-blocks)))

(defun make-block-world (width height depth)
  (unless (and (every #'plusp (list width height depth))
               (every #'integerp (list width height depth)))
    (error "Block-world dimensions must be positive integers."))
  (make-instance
   'block-world :width width :height height :depth depth
   :blocks (make-array (list width height depth) :initial-element nil)))

(defgeneric block-at (world x y z))
(defgeneric (setf block-at) (block world x y z))

(defun block-coordinate-in-bounds-p (world x y z)
  (and (<= 0 x) (< x (block-world-width world))
       (<= 0 y) (< y (block-world-height world))
       (<= 0 z) (< z (block-world-depth world))))

(defmethod block-at ((world block-world) x y z)
  (when (block-coordinate-in-bounds-p world x y z)
    (aref (block-world-blocks world) x y z)))

(defmethod (setf block-at) (block (world block-world) x y z)
  (unless (block-coordinate-in-bounds-p world x y z)
    (error "Block coordinate (~D ~D ~D) is outside ~Dx~Dx~D."
           x y z (block-world-width world) (block-world-height world)
           (block-world-depth world)))
  (setf (aref (block-world-blocks world) x y z) block))

(defun make-little-block-world (&key (width 16) (height 8) (depth 16))
  "Make a tiny rolling field with rocks and one extremely blocky tree."
  (let ((world (make-block-world width height depth)))
    (dotimes (x width)
      (dotimes (z depth)
        (let ((surface
                (max 1
                     (min (- height 3)
                          (+ 2 (round (+ (* 0.55 (sin (* x 0.62)))
                                         (* 0.45 (cos (* z 0.47))))))))))
          (dotimes (y (1+ surface))
            (setf (block-at world x y z)
                  (cond ((= y surface) *grass-block*)
                        ((zerop y) *stone-block*)
                        (t *dirt-block*)))))))
    ;; A few readable landmarks make motion and scale apparent immediately.
    (loop for (x y z) in '((2 3 4) (3 3 4) (3 4 4)
                           (12 3 10) (12 4 10) (11 3 10))
          when (block-coordinate-in-bounds-p world x y z)
            do (setf (block-at world x y z) *stone-block*))
    (let ((tree-x (min 9 (- width 2)))
          (tree-z (min 7 (- depth 2))))
      (when (and (> tree-x 1) (> tree-z 1) (> height 7))
        (loop for y from 3 to 5
              do (setf (block-at world tree-x y tree-z) *wood-block*))
        (loop for x from (1- tree-x) to (1+ tree-x)
              do (loop for z from (1- tree-z) to (1+ tree-z)
                       do (setf (block-at world x 6 z) *leaf-block*)))
        (setf (block-at world tree-x 7 tree-z) *leaf-block*)))
    world))

(defclass block-mesher () ())
(defclass exposed-face-mesher (block-mesher) ())

(defclass block-mesh ()
  ((vertices :initarg :vertices :reader block-mesh-vertices)
   (vertex-count :initarg :vertex-count :reader block-mesh-vertex-count)
   (face-count :initarg :face-count :reader block-mesh-face-count)))

(defgeneric mesh-block-world (mesher world))
(defgeneric emit-block-face (mesher world vertices block face x y z))

(defun push-block-vertex (vertices position color)
  (dolist (component position)
    (vector-push-extend (coerce component 'single-float) vertices))
  (loop for component across color
        do (vector-push-extend component vertices)))

(defun block-color-variation (x y z)
  (+ 0.93 (* 0.07 (/ (mod (+ (* x 17) (* y 31) (* z 13)) 7) 6.0))))

(defun block-face-corner-occlusion (world face corner x y z)
  (let* ((normal (block-face-neighbor face))
         (axes (loop for component in normal
                     for axis from 0
                     when (zerop component) collect axis))
         (first-axis (first axes))
         (second-axis (second axes))
         (first-sign (if (zerop (nth first-axis corner)) -1 1))
         (second-sign (if (zerop (nth second-axis corner)) -1 1)))
    (labels ((occupied-p (first-step second-step)
               (let ((offset (copy-list normal)))
                 (incf (nth first-axis offset) first-step)
                 (incf (nth second-axis offset) second-step)
                 (block-solid-p
                  (block-at world (+ x (first offset))
                                  (+ y (second offset))
                                  (+ z (third offset)))))))
      (let* ((first-side (occupied-p first-sign 0))
             (second-side (occupied-p 0 second-sign))
             (corner-block (occupied-p first-sign second-sign)))
        (if (and first-side second-side)
            0.56
            (- 1.0 (* 0.14 (count t (list first-side second-side
                                           corner-block)))))))))

(defmethod emit-block-face
    ((mesher exposed-face-mesher) (world block-world) vertices
     (block block-kind)
     (face block-face) x y z)
  (declare (ignore mesher))
  (let* ((corners (block-face-corners face))
         (base-color (block-face-color block face))
         (variation (block-color-variation x y z)))
    (dolist (index '(0 1 2 0 2 3))
      (let* ((corner (nth index corners))
             (shade (* variation
                       (block-face-corner-occlusion
                        world face corner x y z)))
             (color (map 'vector
                         (lambda (component)
                           (coerce (* component shade) 'single-float))
                         base-color)))
        (destructuring-bind (cx cy cz) corner
          (push-block-vertex vertices (list (+ x cx) (+ y cy) (+ z cz))
                             color))))
    vertices))

(defmethod mesh-block-world
    ((mesher exposed-face-mesher) (world block-world))
  (let ((vertices (make-array 0 :element-type 'single-float
                                :adjustable t :fill-pointer 0))
        (face-count 0))
    (dotimes (x (block-world-width world))
      (dotimes (y (block-world-height world))
        (dotimes (z (block-world-depth world))
          (let ((block (block-at world x y z)))
            (when (block-solid-p block)
              (dolist (face *block-faces*)
                (destructuring-bind (dx dy dz) (block-face-neighbor face)
                  (unless (block-solid-p
                           (block-at world (+ x dx) (+ y dy) (+ z dz)))
                    (emit-block-face mesher world vertices block face x y z)
                    (incf face-count)))))))))
    (make-instance 'block-mesh :vertices vertices
                               :vertex-count (* face-count 6)
                               :face-count face-count)))

;;; Camera mathematics stays small and inspectable.  Six vec4 values are
;;; written directly into a std140-compatible uniform block.

(defclass fly-camera ()
  ((x :initarg :x :initform 8.0 :accessor camera-x)
   (y :initarg :y :initform 5.0 :accessor camera-y)
   (z :initarg :z :initform -5.0 :accessor camera-z)
   (yaw :initarg :yaw :initform 0.0 :accessor camera-yaw)
   (pitch :initarg :pitch :initform -0.12 :accessor camera-pitch)
   (speed :initarg :speed :initform 6.0 :accessor camera-speed)
   (sensitivity :initarg :sensitivity :initform 0.0025
                :accessor camera-sensitivity)))

(defun vec3 (x y z)
  (vector (coerce x 'single-float)
          (coerce y 'single-float)
          (coerce z 'single-float)))

(defun vec3-scale (vector scale)
  (vec3 (* (aref vector 0) scale)
        (* (aref vector 1) scale)
        (* (aref vector 2) scale)))

(defun vec3-add (&rest vectors)
  (vec3 (loop for vector in vectors sum (aref vector 0))
        (loop for vector in vectors sum (aref vector 1))
        (loop for vector in vectors sum (aref vector 2))))

(defun vec3-length (vector)
  (sqrt (loop for component across vector sum (* component component))))

(defun vec3-normalize (vector)
  (let ((length (vec3-length vector)))
    (if (plusp length) (vec3-scale vector (/ length)) vector)))

(defgeneric camera-basis (camera))
(defgeneric advance-camera (camera pressed-keys seconds))
(defgeneric camera-uniform-data (camera width height))

(defmethod camera-basis ((camera fly-camera))
  (let* ((yaw (camera-yaw camera))
         (pitch (camera-pitch camera))
         (forward (vec3 (* (sin yaw) (cos pitch))
                        (sin pitch)
                        (* (cos yaw) (cos pitch))))
         (right (vec3 (cos yaw) 0.0 (- (sin yaw))))
         (up (vec3 (- (* (sin pitch) (sin yaw)))
                   (cos pitch)
                   (- (* (sin pitch) (cos yaw))))))
    (values right up forward)))

(defun camera-key-down-p (keys &rest names)
  (some (lambda (name) (gethash name keys)) names))

(defmethod advance-camera
    ((camera fly-camera) pressed-keys seconds)
  (multiple-value-bind (right up forward) (camera-basis camera)
    (declare (ignore up))
    (let* ((forward-amount
             (- (if (camera-key-down-p pressed-keys :w :up) 1.0 0.0)
                (if (camera-key-down-p pressed-keys :s :down) 1.0 0.0)))
           (right-amount
             (- (if (camera-key-down-p pressed-keys :d :right) 1.0 0.0)
                (if (camera-key-down-p pressed-keys :a :left) 1.0 0.0)))
           (up-amount
             (- (if (camera-key-down-p pressed-keys :space) 1.0 0.0)
                (if (camera-key-down-p
                     pressed-keys :shift-left :shift-right) 1.0 0.0)))
           (motion
             (vec3-add (vec3-scale forward forward-amount)
                       (vec3-scale right right-amount)
                       (vec3 0.0 up-amount 0.0)))
           (distance (* (camera-speed camera) seconds))
           (step (vec3-scale (vec3-normalize motion) distance)))
      (incf (camera-x camera) (aref step 0))
      (incf (camera-y camera) (aref step 1))
      (incf (camera-z camera) (aref step 2))))
  camera)

(defmethod camera-uniform-data ((camera fly-camera) width height)
  (multiple-value-bind (right up forward) (camera-basis camera)
    (let* ((near 0.1)
           (far 100.0)
           (focal (/ (tan (/ (* 70.0 (/ pi 180.0)) 2.0))))
           (aspect (/ (coerce width 'single-float) height))
           (projection
             (vec3 (/ focal aspect) focal (/ far (- far near)))))
      (make-array
       24 :element-type 'single-float
       :initial-contents
       (list (coerce (camera-x camera) 'single-float)
             (coerce (camera-y camera) 'single-float)
             (coerce (camera-z camera) 'single-float) 0.0
             (aref right 0) (aref right 1) (aref right 2) 0.0
             (aref up 0) (aref up 1) (aref up 2) 0.0
             (aref forward 0) (aref forward 1) (aref forward 2) 0.0
             (aref projection 0) (aref projection 1) (aref projection 2)
             (coerce (/ (- (* far near)) (- far near))
                     'single-float)
             0.43 0.68 0.92 (coerce (/ far) 'single-float))))))

;;; Running demo.

(defclass cube-world-frame-state ()
  ((uniform-buffer :initarg :uniform-buffer
                   :reader cube-world-frame-uniform-buffer)
   (bind-group :initarg :bind-group :reader cube-world-frame-bind-group)))

(defclass cube-world-demo (canvas-event-handler)
  ((canvas :initarg :canvas :reader cube-world-demo-canvas)
   (device :initarg :device :reader cube-world-demo-device)
   (context :initarg :context :reader cube-world-demo-context)
   (world :initarg :world :reader cube-world-demo-world)
   (mesh :initarg :mesh :reader cube-world-demo-mesh)
   (camera :initarg :camera :reader cube-world-demo-camera)
   (vertex-buffer :initarg :vertex-buffer :reader cube-world-demo-vertex-buffer)
   (color-texture :initarg :color-texture
                  :reader cube-world-demo-color-texture)
   (color-view :initarg :color-view :reader cube-world-demo-color-view)
   (depth-texture :initarg :depth-texture
                  :reader cube-world-demo-depth-texture)
   (depth-view :initarg :depth-view :reader cube-world-demo-depth-view)
   (layout :initarg :layout :reader cube-world-demo-layout)
   (pipeline :initarg :pipeline :reader cube-world-demo-pipeline)
   (frame-states :initform (make-hash-table :test #'eq)
                 :reader cube-world-demo-frame-states)
   (resources :initarg :resources :initform nil
              :accessor cube-world-demo-resources)
   (pressed-keys :initform (make-hash-table :test #'eq)
                 :reader cube-world-demo-pressed-keys)
   (pointer-captured-p :initform nil
                       :accessor cube-world-demo-pointer-captured-p)
   (last-frame-time :initform nil :accessor cube-world-demo-last-frame-time)
   (running-p :initform t :accessor cube-world-demo-running-p)))

(defun remember-cube-world-resource (demo resource)
  (push resource (cube-world-demo-resources demo))
  resource)

(defun cube-world-frame-state (demo surface-texture)
  (or (gethash surface-texture (cube-world-demo-frame-states demo))
      (let ((buffer nil) (bind-group nil) (completed-p nil))
        (unwind-protect
             (progn
               (setf buffer
                     (create
                      (cube-world-demo-device demo)
                      (make-buffer-descriptor
                       :label "block world camera uniform"
                       :size 96 :usage '(:uniform)))
                     bind-group
                     (create
                      (cube-world-demo-device demo)
                      (make-bind-group-descriptor
                       :label "block world frame bindings"
                       :layout (cube-world-demo-layout demo)
                       :entries `((:binding 0 :resource ,buffer)))))
               (remember-cube-world-resource demo buffer)
               (remember-cube-world-resource demo bind-group)
               (let ((state
                       (make-instance
                        'cube-world-frame-state
                        :uniform-buffer buffer :bind-group bind-group)))
                 (setf (gethash surface-texture
                                (cube-world-demo-frame-states demo))
                       state
                       completed-p t)
                 state))
          (unless completed-p
            (when bind-group (destroy bind-group))
            (when buffer (destroy buffer)))))))

(defun encode-cube-world-frame
    (demo surface-texture encoder &key readback-buffer)
  (let* ((extent (canvas-extent (cube-world-demo-context demo)))
         (frame (cube-world-frame-state demo surface-texture)))
    (write-buffer
     (cube-world-frame-uniform-buffer frame)
     (camera-uniform-data
      (cube-world-demo-camera demo) (first extent) (second extent)))
    (let ((pass
            (begin-render-pass
             encoder
             (make-render-pass-descriptor
              :color-attachments
              `((:view ,(cube-world-demo-color-view demo)
                 :load-op :clear :store-op :store
                 :clear-value #(0.43 0.68 0.92 1.0)))
              :depth-stencil-attachment
              `(:view ,(cube-world-demo-depth-view demo)
                :depth-load-op :clear :depth-store-op :discard
                :depth-clear-value 1.0)))))
      (set-pipeline pass (cube-world-demo-pipeline demo))
      (set-bind-group pass 0 (cube-world-frame-bind-group frame))
      (set-vertex-buffer pass 0 (cube-world-demo-vertex-buffer demo))
      (draw pass (block-mesh-vertex-count (cube-world-demo-mesh demo)))
      (end-pass pass))
    (when readback-buffer
      (encode
       encoder
       (make-gpu-copy-texture-to-buffer-command
        :source (cube-world-demo-color-texture demo)
        :destination readback-buffer)))
    (encode
     encoder
     (make-gpu-copy-texture-command
      :source (cube-world-demo-color-texture demo)
      :destination surface-texture))))

(defun render-cube-world-frame (demo timestamp)
  (when (cube-world-demo-running-p demo)
    (let* ((last (cube-world-demo-last-frame-time demo))
           (seconds (if last (min 0.1 (max 0.0 (- timestamp last))) 0.0)))
      (setf (cube-world-demo-last-frame-time demo) timestamp)
      (advance-camera (cube-world-demo-camera demo)
                      (cube-world-demo-pressed-keys demo) seconds)
      (present-canvas-frame
       (cube-world-demo-context demo)
       (lambda (surface-texture encoder)
         (encode-cube-world-frame demo surface-texture encoder))))))

(defun capture-cube-world-screenshot (demo pathname)
  "Render DEMO once, read its real color attachment, and write a PNG."
  (unless (eq :open (canvas-state (cube-world-demo-canvas demo)))
    (error "Cannot capture a closed cube-world demo."))
  (let* ((context (cube-world-demo-context demo))
         (extent (canvas-extent context))
         (buffer
           (create
            (cube-world-demo-device demo)
            (make-buffer-descriptor
             :label "block world screenshot readback"
             :size (* 4 (first extent) (second extent))
             :usage '(:copy-dst)))))
    (unwind-protect
         (progn
           (present-canvas-frame
            context
            (lambda (surface-texture encoder)
              (encode-cube-world-frame
               demo surface-texture encoder :readback-buffer buffer)))
           (write-rgba-png
            pathname (read-buffer buffer)
            (first extent) (second extent) (canvas-format context)))
      (destroy buffer))))

(defmethod handle-canvas-event
    ((demo cube-world-demo) canvas (event canvas-key-press-event))
  (let ((key (canvas-key-event-key-name event)))
    (if (eq key :escape)
        (when (cube-world-demo-pointer-captured-p demo)
          (set-canvas-relative-pointer-mode canvas nil)
          (setf (cube-world-demo-pointer-captured-p demo) nil))
        (setf (gethash key (cube-world-demo-pressed-keys demo)) t)))
  nil)

(defmethod handle-canvas-event
    ((demo cube-world-demo) canvas (event canvas-key-release-event))
  (declare (ignore canvas))
  (remhash (canvas-key-event-key-name event)
           (cube-world-demo-pressed-keys demo))
  nil)

(defmethod handle-canvas-event
    ((demo cube-world-demo) canvas (event canvas-pointer-button-press-event))
  (when (and (eq :left (canvas-pointer-event-button event))
             (not (cube-world-demo-pointer-captured-p demo)))
    (set-canvas-relative-pointer-mode canvas t)
    (setf (cube-world-demo-pointer-captured-p demo) t))
  nil)

(defmethod handle-canvas-event
    ((demo cube-world-demo) canvas (event canvas-pointer-motion-event))
  (declare (ignore canvas))
  (when (cube-world-demo-pointer-captured-p demo)
    (let ((camera (cube-world-demo-camera demo)))
      (incf (camera-yaw camera)
            (* (canvas-pointer-event-delta-x event)
               (camera-sensitivity camera)))
      (setf (camera-pitch camera)
            (max -1.5
                 (min 1.5
                      (- (camera-pitch camera)
                         (* (canvas-pointer-event-delta-y event)
                            (camera-sensitivity camera))))))))
  nil)

(defmethod handle-canvas-event
    ((demo cube-world-demo) canvas (event canvas-window-focus-lost-event))
  (declare (ignore event))
  (clrhash (cube-world-demo-pressed-keys demo))
  (when (cube-world-demo-pointer-captured-p demo)
    (set-canvas-relative-pointer-mode canvas nil)
    (setf (cube-world-demo-pointer-captured-p demo) nil))
  nil)

(defmethod handle-canvas-event
    ((demo cube-world-demo) canvas (event canvas-window-close-request-event))
  (declare (ignore canvas event))
  (setf (cube-world-demo-running-p demo) nil)
  nil)

(defmethod handle-canvas-event
    ((demo cube-world-demo) canvas (event canvas-event))
  (declare (ignore demo canvas event))
  nil)

(defun start-cube-world-demo (&key
                                (title "luv little block world — click, look, fly")
                                (width 960) (height 640)
                                (frames-per-second 60)
                                (world (make-little-block-world))
                                (mesher (make-instance
                                         'exposed-face-mesher))
                                (camera (make-instance 'fly-camera)))
  "Open a little CPU-meshed block world.

Click to capture the pointer, look with the mouse, fly with WASD, rise with
Space, descend with either Shift key, and press Escape to release the pointer."
  (let ((canvas (make-sdl-canvas :title title :width width :height height))
        (device nil) (context nil) (resources nil) (completed-p nil))
    (open-canvas canvas)
    (unwind-protect
         (progn
           (setf device
                 (request-gpu-device
                  *gpu-provider* (make-device-descriptor :label title))
                 context
                 (make-canvas-context
                  canvas *gpu-provider*
                  (make-canvas-configuration :device device)))
           (flet ((keep (resource)
                    (push resource resources)
                    resource))
             (let* ((extent (canvas-extent context))
                  (mesh (mesh-block-world mesher world))
                  (vertex-buffer
                    (keep
                     (create
                      device
                      (make-buffer-descriptor
                       :label "block world vertices"
                       :size (* 4 (length (block-mesh-vertices mesh)))
                       :usage '(:vertex)))))
                  (color-texture
                    (keep
                     (create
                      device
                      (make-texture-descriptor
                       :label "block world color"
                       :size extent :dimensions :2d
                       :format (canvas-format context)
                       :usage '(:render-attachment :copy-src)))))
                  (color-view
                    (keep
                     (create device (make-texture-view-descriptor
                                     :texture color-texture))))
                  (depth-texture
                    (keep
                     (create
                      device
                      (make-texture-descriptor
                       :label "block world depth"
                       :size extent :dimensions :2d :format :depth32-float
                       :usage '(:render-attachment)))))
                  (depth-view
                    (keep
                     (create device (make-texture-view-descriptor
                                     :texture depth-texture))))
                  (vertex-module
                    (keep
                     (create device (make-shader-module-descriptor
                                     :label "block world vertex shader"
                                     :code (spv:block-world-vertex-shader)))))
                  (fragment-module
                    (keep
                     (create device (make-shader-module-descriptor
                                     :label "block world fragment shader"
                                     :code (spv:block-world-fragment-shader)))))
                  (layout
                    (keep
                     (create
                      device
                      (make-bind-group-layout-descriptor
                       :label "block world layout"
                       :entries '((:binding 0 :type :uniform-buffer))))))
                  (pipeline
                    (keep
                     (create
                      device
                      (make-render-pipeline-descriptor
                       :label "block world pipeline"
                       :layout layout
                       :vertex
                       `(:module ,vertex-module
                         :buffers ((:array-stride 24
                                    :attributes
                                    ((:shader-location 0 :offset 0
                                      :format :float32x3)
                                     (:shader-location 1 :offset 12
                                      :format :float32x3)))))
                       :fragment
                       `(:module ,fragment-module
                         :targets ((:format ,(canvas-format context))))
                       :primitive '(:topology :triangle-list)
                       :depth-stencil
                       '(:format :depth32-float
                         :depth-write-enabled t :depth-compare :less)))))
                  (demo
                    (make-instance
                     'cube-world-demo
                     :canvas canvas :device device :context context
                     :world world :mesh mesh :camera camera
                     :vertex-buffer vertex-buffer
                     :color-texture color-texture :color-view color-view
                     :depth-texture depth-texture :depth-view depth-view
                     :layout layout :pipeline pipeline
                     :resources resources)))
             (write-buffer vertex-buffer (block-mesh-vertices mesh))
             (setf (canvas-event-handler canvas) demo
                   (canvas-clock canvas)
                   (make-cadence-clock
                    (lambda (native-canvas timestamp)
                      (declare (ignore native-canvas))
                      (render-cube-world-frame demo timestamp))
                    :frames-per-second frames-per-second)
                   completed-p t)
               demo)))
      (unless completed-p
        (dolist (resource resources)
          (ignore-errors (destroy resource)))
        (close-canvas canvas)
        (when device (destroy device))))))

(defun stop-cube-world-demo (demo)
  "Stop DEMO and explicitly release all of its GPU and canvas resources."
  ;; A native close request may already have set this, but the resources still
  ;; belong to the demo until this explicit teardown.
  (setf (cube-world-demo-running-p demo) nil)
  (let ((canvas (cube-world-demo-canvas demo)))
    (when (eq :open (canvas-state canvas))
      (setf (canvas-clock canvas) (make-demand-clock))
      (when (cube-world-demo-pointer-captured-p demo)
        (ignore-errors (set-canvas-relative-pointer-mode canvas nil))
        (setf (cube-world-demo-pointer-captured-p demo) nil))
      ;; A synchronous no-op after changing the clock is a native-thread
      ;; barrier: an already-running frame has finished before teardown starts.
      (request-canvas-frame canvas (lambda (timestamp)
                                     (declare (ignore timestamp)))))
    (setf (canvas-event-handler canvas) nil)
    (dolist (resource (cube-world-demo-resources demo))
      (destroy resource))
    (setf (cube-world-demo-resources demo) nil)
    (close-canvas canvas))
  (destroy (cube-world-demo-device demo))
  (values))
