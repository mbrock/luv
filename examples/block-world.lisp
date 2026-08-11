;;; A small CPU-authored block world and a first-person canvas demo.

(in-package #:luv)

;;; Blocks and faces are ordinary CLOS objects so the interesting semantic
;;; choices stay inspectable and redefinable at the REPL.  The result of
;;; meshing is deliberately mundane: interleaved position, UV/AO, and normal
;;; triples of single floats.

(defclass block-kind ()
  ((name :initarg :name :reader block-kind-name)
   (face-tiles :initarg :face-tiles :reader block-kind-face-tiles)))

(defgeneric block-solid-p (block))
(defgeneric block-face-tile (block face))

(defmethod block-solid-p ((block null)) nil)
(defmethod block-solid-p ((block block-kind)) t)

(defclass block-face ()
  ((name :initarg :name :reader block-face-name)
   (neighbor :initarg :neighbor :reader block-face-neighbor)
   (corners :initarg :corners :reader block-face-corners)))

(defun make-block-face (name neighbor corners)
  (make-instance 'block-face :name name :neighbor neighbor
                              :corners corners))

(defparameter *block-faces*
  (list
   (make-block-face :left '(-1 0 0)
                    '((0 0 0) (0 0 1) (0 1 1) (0 1 0)))
   (make-block-face :right '(1 0 0)
                    '((1 0 1) (1 0 0) (1 1 0) (1 1 1)))
   (make-block-face :bottom '(0 -1 0)
                    '((0 0 1) (0 0 0) (1 0 0) (1 0 1)))
   (make-block-face :top '(0 1 0)
                    '((0 1 0) (0 1 1) (1 1 1) (1 1 0)))
   (make-block-face :back '(0 0 -1)
                    '((1 0 0) (0 0 0) (0 1 0) (1 1 0)))
   (make-block-face :front '(0 0 1)
                    '((0 0 1) (1 0 1) (1 1 1) (0 1 1)))))

(defmethod block-face-tile ((block block-kind) (face block-face))
  (let ((tiles (block-kind-face-tiles block)))
    (or (getf tiles (block-face-name face))
        (and (member (block-face-name face) '(:left :right :back :front))
             (getf tiles :side))
        (getf tiles :all)
        (error "No texture tile for ~S face ~S."
               (block-kind-name block) (block-face-name face)))))

(defparameter *grass-block*
  (make-instance 'block-kind :name :grass
                             :face-tiles '(:top 0 :side 1 :bottom 2)))
(defparameter *dirt-block*
  (make-instance 'block-kind :name :dirt
                             :face-tiles '(:all 2)))
(defparameter *stone-block*
  (make-instance 'block-kind :name :stone
                             :face-tiles '(:all 3)))
(defparameter *wood-block*
  (make-instance 'block-kind :name :wood
                             :face-tiles '(:top 5 :bottom 5 :side 4)))
(defparameter *leaf-block*
  (make-instance 'block-kind :name :leaves
                             :face-tiles '(:all 6)))

(defconstant +block-atlas-tile-size+ 16)
(defconstant +block-atlas-tile-count+ 7)

(defun block-atlas-byte (value)
  (max 0 (min 255 (round value))))

(defun pack-block-atlas-rgba (red green blue)
  (logior (block-atlas-byte red)
          (ash (block-atlas-byte green) 8)
          (ash (block-atlas-byte blue) 16)
          #xff000000))

(defun block-atlas-variation (x y salt)
  (- (mod (+ (* x 17) (* y 31) (* salt 43) (* x y 7)) 25) 12))

(defun block-atlas-pixel (tile x y)
  (labels ((pixel (red green blue &optional (variation 0))
             (pack-block-atlas-rgba (+ red variation)
                                    (+ green variation)
                                    (+ blue variation))))
    (let ((variation (block-atlas-variation x y tile)))
      (case tile
        (0 (pixel 91 171 68 variation))
        (1 (if (< y 4)
               (pixel 86 158 61 variation)
               (pixel 123 82 48 (round variation 2))))
        (2 (pixel 126 84 49 variation))
        (3 (pixel 126 132 136
                  (+ (round variation 2)
                     (if (zerop (mod (+ (* x 3) (* y 5)) 19)) 20 0))))
        (4 (pixel 116 76 39
                  (+ (round variation 3)
                     (if (zerop (mod x 5)) 18 0))))
        (5 (let* ((dx (- x 7.5))
                  (dy (- y 7.5))
                  (ring (mod (floor (+ (* dx dx) (* dy dy))) 18)))
             (pixel 133 91 49 (- ring 9))))
        (6 (pixel 51 132 58
                  (+ variation (if (evenp (+ x y)) 8 -8))))
        (otherwise (error "Unknown block atlas tile ~D." tile))))))

(defun make-block-texture-atlas ()
  "Return the little world's horizontal RGBA8 atlas as packed pixel words."
  (let* ((width (* +block-atlas-tile-size+ +block-atlas-tile-count+))
         (pixels (make-array (list +block-atlas-tile-size+ width)
                             :element-type '(unsigned-byte 32))))
    (dotimes (y +block-atlas-tile-size+)
      (dotimes (tile +block-atlas-tile-count+)
        (dotimes (x +block-atlas-tile-size+)
          (setf (aref pixels y (+ x (* tile +block-atlas-tile-size+)))
                (block-atlas-pixel tile x y)))))
    pixels))

(defclass little-world-source ()
  ((seed :initarg :seed :initform 121 :reader little-world-source-seed)
   (edits :initarg :edits :initform (make-block-edit-overlay)
          :reader little-world-source-edits)))

(defun little-world-hash (source x z &optional (salt 0))
  "A stable coordinate hash for terrain readings and discrete features."
  (let ((value
          (logand #xffffffff
                  (+ (little-world-source-seed source)
                     (* x 374761393) (* z 668265263) (* salt 2246822519)))))
    (setf value (logand #xffffffff
                        (* (logxor value (ash value -13)) 1274126177)))
    (logand #xffffffff (logxor value (ash value -16)))))

(defun little-world-hash-reading (source x z salt)
  (- (* 2.0d0 (/ (little-world-hash source x z salt) #xffffffff)) 1.0d0))

(defun smooth-little-world-reading (reading)
  (* reading reading (- 3.0d0 (* 2.0d0 reading))))

(defun interpolate-little-world-reading (left right amount)
  (+ left (* (- right left) amount)))

(defun little-world-value-noise (source x z period &optional (salt 0))
  "Sample deterministic smooth value noise at integer world position X,Z."
  (check-type period (real (0) *))
  (let* ((sample-x (/ x (coerce period 'double-float)))
         (sample-z (/ z (coerce period 'double-float)))
         (cell-x (floor sample-x))
         (cell-z (floor sample-z))
         (tx (smooth-little-world-reading (- sample-x cell-x)))
         (tz (smooth-little-world-reading (- sample-z cell-z)))
         (near (interpolate-little-world-reading
                (little-world-hash-reading source cell-x cell-z salt)
                (little-world-hash-reading source (1+ cell-x) cell-z salt)
                tx))
         (far (interpolate-little-world-reading
               (little-world-hash-reading source cell-x (1+ cell-z) salt)
               (little-world-hash-reading
                source (1+ cell-x) (1+ cell-z) salt)
               tx)))
    (interpolate-little-world-reading near far tz)))

(defun little-world-surface-height (source x z height)
  (let ((reading
          (+ 5.0d0
             (* 2.8d0 (little-world-value-noise source x z 64 0))
             (* 1.4d0 (little-world-value-noise source x z 28 1))
             (* 0.65d0 (little-world-value-noise source x z 11 2)))))
    (max 2 (min (- height 6) (round reading)))))

(defgeneric materialize-block-world-chunk
    (source world chunk-x chunk-y chunk-z))
(defgeneric populate-block-world-chunk
    (source world chunk-x chunk-y chunk-z))
(defgeneric apply-block-world-source-edits
    (source world chunk-x chunk-y chunk-z))
(defgeneric edit-block-world-source (source world block x y z))

(defun materialize-little-world-chunk (source world chunk-x chunk-z)
  "Materialize one deterministic terrain chunk at vertical layer zero."
  (check-type source little-world-source)
  (with-world-change-transaction (world)
    (let* ((chunk (ensure-world-chunk world chunk-x 0 chunk-z))
           (shape (voxel-space-chunk-shape (block-world-space world)))
           (width (chunk-shape-width shape))
           (height (chunk-shape-height shape))
           (depth (chunk-shape-depth shape))
           (origin (chunk-domain-origin (block-chunk-domain chunk))))
      (dotimes (local-z depth)
        (dotimes (local-x width)
          (let* ((x (+ (world-coordinate-x origin) local-x))
                 (z (+ (world-coordinate-z origin) local-z))
                 (surface (little-world-surface-height source x z height)))
            (dotimes (y (1+ surface))
              (setf (block-at world x y z)
                    (cond ((= y surface) *grass-block*)
                          ((or (zerop y) (< y (- surface 2))) *stone-block*)
                          (t *dirt-block*)))))))
      chunk)))

(defmethod materialize-block-world-chunk
    ((source little-world-source) (world block-world)
     chunk-x chunk-y chunk-z)
  (unless (zerop chunk-y)
    (error "The little world currently materializes only chunk layer zero."))
  (materialize-little-world-chunk source world chunk-x chunk-z))

(defun populate-little-world-chunk (source world chunk-x chunk-z)
  "Place deterministic, sparse landmarks after neighboring terrain exists."
  (with-world-change-transaction (world)
    (let* ((shape (voxel-space-chunk-shape (block-world-space world)))
           (width (chunk-shape-width shape))
           (height (chunk-shape-height shape))
           (depth (chunk-shape-depth shape))
           (origin-x (* chunk-x width))
           (origin-z (* chunk-z depth))
           (hash (little-world-hash source chunk-x chunk-z))
           (rock-x (+ origin-x 2 (mod hash (- width 4))))
           (rock-z (+ origin-z 2 (mod (ash hash -8) (- depth 4))))
           (rock-y (1+ (little-world-surface-height
                        source rock-x rock-z height))))
      (setf (block-at world rock-x rock-y rock-z) *stone-block*)
      (when (zerop (mod hash 2))
        (setf (block-at world (1+ rock-x) rock-y rock-z) *stone-block*))
      (when (and (< (mod (ash hash -16) 5) 4)
                 (> (little-world-value-noise
                     source rock-x rock-z 48 29)
                    -0.35d0))
        (let* ((tree-x (+ origin-x 3 (mod (ash hash -3) (- width 6))))
               (tree-z (+ origin-z 3 (mod (ash hash -11) (- depth 6))))
               (surface (little-world-surface-height
                         source tree-x tree-z height))
               (crown (+ surface 4)))
          (loop for y from (1+ surface) below crown
                do (setf (block-at world tree-x y tree-z) *wood-block*))
          (loop for x from (1- tree-x) to (1+ tree-x)
                do (loop for z from (1- tree-z) to (1+ tree-z)
                         do (setf (block-at world x crown z) *leaf-block*)))
          (setf (block-at world tree-x (1+ crown) tree-z) *leaf-block*))))))

(defmethod populate-block-world-chunk
    ((source little-world-source) (world block-world)
     chunk-x chunk-y chunk-z)
  (unless (zerop chunk-y)
    (error "The little world currently populates only chunk layer zero."))
  (populate-little-world-chunk source world chunk-x chunk-z))

(defmethod apply-block-world-source-edits
    ((source little-world-source) (world block-world)
     chunk-x chunk-y chunk-z)
  (multiple-value-bind (chunk present-p)
      (world-chunk-at world chunk-x chunk-y chunk-z)
    (unless present-p
      (error "Cannot apply edits to absent chunk (~D ~D ~D)."
             chunk-x chunk-y chunk-z))
    (apply-block-edits-to-chunk (little-world-source-edits source)
                                world chunk)))

(defmethod edit-block-world-source
    ((source little-world-source) (world block-world) block x y z)
  (record-block-edit (little-world-source-edits source) block x y z)
  (setf (block-at world x y z) block))

(defmethod edit-block-world-source
    ((source t) (world block-world) block x y z)
  (setf (block-at world x y z) block))

(defun edit-block-at (block world x y z)
  "Edit one resident site, recording it in WORLD's source when supported."
  (edit-block-world-source (block-world-source world) world block x y z))

(defun rematerialize-little-world-chunk (source world chunk-x chunk-z)
  "Regenerate one chunk from SOURCE, then replay its explicit edits."
  (with-world-change-transaction (world)
    (remove-world-chunk world chunk-x 0 chunk-z)
    (materialize-block-world-chunk source world chunk-x 0 chunk-z)
    (populate-block-world-chunk source world chunk-x 0 chunk-z)
    (apply-block-world-source-edits source world chunk-x 0 chunk-z)))

(defun make-little-block-world (&key (chunk-radius 4)
                                     (chunk-width 16)
                                     (chunk-height 16)
                                     (chunk-depth 16)
                                     (seed 121))
  "Make a deterministic square of resident terrain chunks and landmarks."
  (check-type chunk-radius (integer 0))
  (check-type chunk-width (integer 8))
  (check-type chunk-height (integer 8))
  (check-type chunk-depth (integer 8))
  (let* ((source (make-instance 'little-world-source :seed seed))
         (world (make-block-world :id (list :little-world seed)
                                  :chunk-width chunk-width
                                  :chunk-height chunk-height
                                  :chunk-depth chunk-depth
                                  :source source)))
    (with-world-change-transaction (world)
      ;; Residency is established first so terrain and later features may cross
      ;; chunk boundaries without ever pretending that absent terrain is air.
      (loop for chunk-x from (- chunk-radius) to chunk-radius
            do (loop for chunk-z from (- chunk-radius) to chunk-radius
                     do (ensure-world-chunk world chunk-x 0 chunk-z)))
      (loop for chunk-x from (- chunk-radius) to chunk-radius
            do (loop for chunk-z from (- chunk-radius) to chunk-radius
                     do (materialize-block-world-chunk
                         source world chunk-x 0 chunk-z)))
      (loop for chunk-x from (- chunk-radius) to chunk-radius
            do (loop for chunk-z from (- chunk-radius) to chunk-radius
                     do (populate-block-world-chunk
                         source world chunk-x 0 chunk-z)))
      (loop for chunk-x from (- chunk-radius) to chunk-radius
            do (loop for chunk-z from (- chunk-radius) to chunk-radius
                     do (apply-block-world-source-edits
                         source world chunk-x 0 chunk-z))))
    world))

(defclass block-mesher () ())
(defclass exposed-face-mesher (block-mesher)
  ((absent-neighbor-policy
    :initarg :absent-neighbor-policy
    :initform :air
    :reader exposed-face-mesher-absent-neighbor-policy)))

(defclass block-mesh ()
  ((vertices :initarg :vertices :reader block-mesh-vertices)
   (vertex-count :initarg :vertex-count :reader block-mesh-vertex-count)
   (face-count :initarg :face-count :reader block-mesh-face-count)))

(defgeneric mesh-block-world (mesher world))
(defgeneric mesh-block-chunk (mesher world chunk))
(defgeneric emit-block-face (mesher world vertices block face x y z))

(defun push-block-vertex (vertices position uv shade normal)
  (dolist (component position)
    (vector-push-extend (coerce component 'single-float) vertices))
  (loop for component across uv
        do (vector-push-extend component vertices))
  (vector-push-extend (coerce shade 'single-float) vertices)
  (dolist (component normal)
    (vector-push-extend (coerce component 'single-float) vertices)))

(defun block-color-variation (x y z)
  (+ 0.93 (* 0.07 (/ (mod (+ (* x 17) (* y 31) (* z 13)) 7) 6.0))))

(defun mesher-block-at (mesher world x y z)
  (multiple-value-bind (block status) (block-at world x y z)
    (ecase status
      (:resident block)
      (:absent
       (ecase (exposed-face-mesher-absent-neighbor-policy mesher)
         (:air nil)
         (:solid *stone-block*)
         (:error
          (error "Meshing reached absent terrain at (~D ~D ~D)." x y z)))))))

(defun block-face-corner-occlusion (mesher world face corner x y z)
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
                  (mesher-block-at mesher world
                                   (+ x (first offset))
                                   (+ y (second offset))
                                   (+ z (third offset)))))))
      (let* ((first-side (occupied-p first-sign 0))
             (second-side (occupied-p 0 second-sign))
             (corner-block (occupied-p first-sign second-sign)))
        (if (and first-side second-side)
            0.56
            (- 1.0 (* 0.14 (count t (list first-side second-side
                                           corner-block)))))))))

(defun block-face-local-uv (face corner)
  (case (block-face-name face)
    ((:top :bottom) (values (first corner) (third corner)))
    ((:front :back) (values (first corner) (- 1 (second corner))))
    ((:left :right) (values (third corner) (- 1 (second corner))))
    (otherwise (error "Unknown block face ~S." (block-face-name face)))))

(defun block-face-atlas-uv (block face corner)
  (multiple-value-bind (local-u local-v) (block-face-local-uv face corner)
    (let* ((tile (block-face-tile block face))
           (size +block-atlas-tile-size+)
           (width (* size +block-atlas-tile-count+))
           ;; Half-texel insets make bilinear bleed impossible even if a
           ;; caller swaps the intentionally nearest-filtered sampler.
           (u (/ (+ (* tile size) 0.5 (* local-u (1- size))) width))
           (v (/ (+ 0.5 (* local-v (1- size))) size)))
      (vector (coerce u 'single-float) (coerce v 'single-float)))))

(defmethod emit-block-face
    ((mesher exposed-face-mesher) (world block-world) vertices
     (block block-kind)
     (face block-face) x y z)
  (let* ((corners (block-face-corners face))
         (variation (block-color-variation x y z)))
    (dolist (index '(0 1 2 0 2 3))
      (let* ((corner (nth index corners))
             (shade (* variation
                       (block-face-corner-occlusion
                        mesher world face corner x y z)))
             (uv (block-face-atlas-uv block face corner)))
        (destructuring-bind (cx cy cz) corner
          (push-block-vertex vertices (list (+ x cx) (+ y cy) (+ z cz))
                             uv shade (block-face-neighbor face)))))
    vertices))

(defmethod mesh-block-chunk
    ((mesher exposed-face-mesher) (world block-world) (chunk block-chunk))
  (let ((vertices (make-array 0 :element-type 'single-float
                                :adjustable t :fill-pointer 0))
        (face-count 0))
    (let ((origin (chunk-domain-origin (block-chunk-domain chunk))))
      (map-chunk-blocks
       (lambda (block local-x local-y local-z)
         (when (block-solid-p block)
           (let ((x (+ (world-coordinate-x origin) local-x))
                 (y (+ (world-coordinate-y origin) local-y))
                 (z (+ (world-coordinate-z origin) local-z)))
             (dolist (face *block-faces*)
               (destructuring-bind (dx dy dz) (block-face-neighbor face)
                 (unless (block-solid-p
                          (mesher-block-at mesher world
                                           (+ x dx) (+ y dy) (+ z dz)))
                   (emit-block-face mesher world vertices block face x y z)
                   (incf face-count)))))))
       chunk))
    (make-instance 'block-mesh :vertices vertices
                               :vertex-count (* face-count 6)
                               :face-count face-count)))

(defmethod mesh-block-world
    ((mesher exposed-face-mesher) (world block-world))
  "Make a combined compatibility mesh from independently meshed chunks."
  (let ((vertices (make-array 0 :element-type 'single-float
                                :adjustable t :fill-pointer 0))
        (vertex-count 0)
        (face-count 0))
    (dolist (chunk (resident-world-chunks world))
      (let ((mesh (mesh-block-chunk mesher world chunk)))
        (loop for component across (block-mesh-vertices mesh)
              do (vector-push-extend component vertices))
        (incf vertex-count (block-mesh-vertex-count mesh))
        (incf face-count (block-mesh-face-count mesh))))
    (make-instance 'block-mesh :vertices vertices
                               :vertex-count vertex-count
                               :face-count face-count)))

;;; Camera mathematics stays small and inspectable.  Six vec4 values are
;;; written directly into a std140-compatible uniform block.

(defclass fly-camera ()
  ((x :initarg :x :initform 8.0 :accessor camera-x)
   (y :initarg :y :initform 11.0 :accessor camera-y)
   (z :initarg :z :initform -6.0 :accessor camera-z)
   (yaw :initarg :yaw :initform 0.0 :accessor camera-yaw)
   (pitch :initarg :pitch :initform -0.28 :accessor camera-pitch)
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
           (far 180.0)
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

(defparameter *chunk-neighbor-directions*
  '((-1 0 0) (1 0 0) (0 -1 0) (0 1 0) (0 0 -1) (0 0 1)))

(defclass cube-world-chunk-product ()
  ((coordinate :initarg :coordinate
               :reader cube-world-chunk-product-coordinate)
   (dependency-stamp :initarg :dependency-stamp
                     :reader cube-world-chunk-product-dependency-stamp)
   (mesh :initarg :mesh :reader cube-world-chunk-product-mesh)
   (vertex-buffer :initarg :vertex-buffer
                  :reader cube-world-chunk-product-vertex-buffer)))

(defclass cube-world-demo (canvas-event-handler)
  ((canvas :initarg :canvas :reader cube-world-demo-canvas)
   (device :initarg :device :reader cube-world-demo-device)
   (context :initarg :context :reader cube-world-demo-context)
   (world :initarg :world :reader cube-world-demo-world)
   (mesher :initarg :mesher :reader cube-world-demo-mesher)
   (chunk-products :initform (make-hash-table :test #'equal)
                   :reader cube-world-demo-chunk-products)
   (meshed-world-revision
    :initarg :meshed-world-revision
    :initform -1
    :accessor cube-world-demo-meshed-world-revision)
   (camera :initarg :camera :reader cube-world-demo-camera)
   (selected-block :initarg :selected-block :initform *stone-block*
                   :accessor cube-world-demo-selected-block)
   (atlas-texture :initarg :atlas-texture
                  :reader cube-world-demo-atlas-texture)
   (atlas-view :initarg :atlas-view :reader cube-world-demo-atlas-view)
   (atlas-sampler :initarg :atlas-sampler
                  :reader cube-world-demo-atlas-sampler)
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

(defun block-chunk-key (chunk)
  (let ((coordinate
          (chunk-domain-coordinate (block-chunk-domain chunk))))
    (chunk-key (chunk-coordinate-x coordinate)
               (chunk-coordinate-y coordinate)
               (chunk-coordinate-z coordinate))))

(defun chunk-mesh-dependency-stamp (world chunk)
  "Describe exactly which resident block data CHUNK's exposed mesh observes."
  (let* ((coordinate
           (chunk-domain-coordinate (block-chunk-domain chunk)))
         (x (chunk-coordinate-x coordinate))
         (y (chunk-coordinate-y coordinate))
         (z (chunk-coordinate-z coordinate)))
    (cons
     (list chunk (block-chunk-revision chunk))
     (loop for (dx dy dz) in *chunk-neighbor-directions*
           collect
           (multiple-value-bind (neighbor present-p)
               (world-chunk-at world (+ x dx) (+ y dy) (+ z dz))
             (if present-p
                 ;; Only the neighbor boundary facing this chunk contributes.
                 (list neighbor
                       (block-chunk-boundary-revision
                        neighbor (- dx) (- dy) (- dz)))
                 '(nil)))))))

(defun cube-world-demo-products-in-order (demo)
  (let ((products (cube-world-demo-chunk-products demo)))
    (loop for chunk in (resident-world-chunks (cube-world-demo-world demo))
          for product = (gethash (block-chunk-key chunk) products)
          when product collect product)))

(defun cube-world-demo-mesh (demo)
  "Return a combined, inspectable snapshot of DEMO's chunk meshes."
  (let ((vertices (make-array 0 :element-type 'single-float
                                :adjustable t :fill-pointer 0))
        (vertex-count 0)
        (face-count 0))
    (dolist (product (cube-world-demo-products-in-order demo))
      (let ((mesh (cube-world-chunk-product-mesh product)))
        (loop for component across (block-mesh-vertices mesh)
              do (vector-push-extend component vertices))
        (incf vertex-count (block-mesh-vertex-count mesh))
        (incf face-count (block-mesh-face-count mesh))))
    (make-instance 'block-mesh :vertices vertices
                               :vertex-count vertex-count
                               :face-count face-count)))

(defun destroy-cube-world-chunk-products (demo)
  (maphash
   (lambda (key product)
     (declare (ignore key))
     (destroy (cube-world-chunk-product-vertex-buffer product)))
   (cube-world-demo-chunk-products demo))
  (clrhash (cube-world-demo-chunk-products demo))
  (values))

(defun refresh-cube-world-mesh (demo)
  "Refresh only chunk products invalidated by content or neighbor boundaries.

Replaced and evicted buffers are destroyed immediately at the API level.  The
GPU completion frontier defers their native teardown when an in-flight frame
still owns their last use."
  (let* ((world (cube-world-demo-world demo))
         (products (cube-world-demo-chunk-products demo))
         (resident-keys (make-hash-table :test #'equal)))
    (dolist (chunk (resident-world-chunks world))
      (let* ((key (block-chunk-key chunk))
             (stamp (chunk-mesh-dependency-stamp world chunk))
             (old (gethash key products)))
        (setf (gethash key resident-keys) t)
        (unless (and old
                     (equal stamp
                            (cube-world-chunk-product-dependency-stamp old)))
          (let ((buffer nil) (completed-p nil))
            (unwind-protect
                 (let* ((mesh (mesh-block-chunk
                               (cube-world-demo-mesher demo) world chunk))
                        (vertices (block-mesh-vertices mesh)))
                   (setf buffer
                         (create
                          (cube-world-demo-device demo)
                          (make-buffer-descriptor
                           :label (format nil "block chunk ~{~D~^,~} revision ~D"
                                          key (block-chunk-revision chunk))
                           :size (max 4 (* 4 (length vertices)))
                           :usage '(:vertex))))
                   (write-buffer buffer vertices)
                   (setf (gethash key products)
                         (make-instance
                          'cube-world-chunk-product
                          :coordinate
                          (chunk-domain-coordinate (block-chunk-domain chunk))
                          :dependency-stamp stamp
                          :mesh mesh
                          :vertex-buffer buffer)
                         completed-p t)
                   (when old
                     (destroy
                      (cube-world-chunk-product-vertex-buffer old))))
              (unless completed-p
                (when buffer (destroy buffer))))))))
    (let ((evicted-keys nil))
      (maphash (lambda (key product)
                 (unless (gethash key resident-keys)
                   (destroy (cube-world-chunk-product-vertex-buffer product))
                   (push key evicted-keys)))
               products)
      (dolist (key evicted-keys)
        (remhash key products)))
    (setf (cube-world-demo-meshed-world-revision demo)
          (block-world-revision world))
    (cube-world-demo-products-in-order demo)))

(defun cube-world-demo-target (demo &key (max-distance 8d0))
  "Raycast from DEMO's camera through resident block terrain."
  (let ((camera (cube-world-demo-camera demo)))
    (multiple-value-bind (right up forward) (camera-basis camera)
      (declare (ignore right up))
      (raycast-block-world
       (cube-world-demo-world demo)
       (vector (camera-x camera) (camera-y camera) (camera-z camera))
       forward #'block-solid-p :max-distance max-distance))))

(defun edit-cube-world-block (demo action)
  "Apply ACTION (:REMOVE or :PLACE) along DEMO's centre view ray."
  (multiple-value-bind (hit status) (cube-world-demo-target demo)
    (unless hit
      (return-from edit-cube-world-block (values nil status)))
    (let* ((world (cube-world-demo-world demo))
           (coordinate
             (ecase action
               (:remove (block-ray-hit-coordinate hit))
               (:place (block-ray-hit-adjacent-coordinate hit)))))
      (unless coordinate
        (return-from edit-cube-world-block (values nil :blocked)))
      (let ((x (world-coordinate-x coordinate))
            (y (world-coordinate-y coordinate))
            (z (world-coordinate-z coordinate)))
        (multiple-value-bind (old-block residency) (block-at world x y z)
          (unless (eq residency :resident)
            (return-from edit-cube-world-block (values nil :absent)))
          (ecase action
            (:remove
             (edit-block-at nil world x y z))
            (:place
             (when old-block
               (return-from edit-cube-world-block (values nil :blocked)))
             (edit-block-at
              (cube-world-demo-selected-block demo) world x y z)))
          (values coordinate :edited))))))

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
                       :entries
                       `((:binding 0
                          :resource ,(cube-world-demo-atlas-view demo))
                         (:binding 1
                          :resource ,(cube-world-demo-atlas-sampler demo))
                         (:binding 2 :resource ,buffer)))))
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
  (let* ((products (refresh-cube-world-mesh demo))
         (extent (canvas-extent (cube-world-demo-context demo)))
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
      (dolist (product products)
        (let ((mesh (cube-world-chunk-product-mesh product)))
          (when (plusp (block-mesh-vertex-count mesh))
            (set-vertex-buffer
             pass 0 (cube-world-chunk-product-vertex-buffer product))
            (draw pass (block-mesh-vertex-count mesh)))))
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
           (ensure-directories-exist pathname)
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
  (let ((button (canvas-pointer-event-button event)))
    (cond
      ((not (cube-world-demo-pointer-captured-p demo))
       (when (eq button :left)
         (set-canvas-relative-pointer-mode canvas t)
         (setf (cube-world-demo-pointer-captured-p demo) t)))
      ((eq button :left)
       (edit-cube-world-block demo :remove))
      ((eq button :right)
       (edit-cube-world-block demo :place))))
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
                                (visible-p t)
                                (world (make-little-block-world))
                                (mesher (make-instance
                                         'exposed-face-mesher))
                                (camera (make-instance 'fly-camera)))
  "Open a little CPU-meshed block world.

Click to capture the pointer, look with the mouse, fly with WASD, rise with
Space, and descend with either Shift key.  Once captured, left click removes
the block at the centre of view and right click places the selected block.
Press Escape to release the pointer.

Pass :VISIBLE-P NIL to keep the SDL window hidden while still exercising the
real SDL/Vulkan surface and swapchain path.  Pass :FRAMES-PER-SECOND NIL for a
capture-only demand clock."
  (let ((canvas (make-sdl-canvas :title title :width width :height height
                                 :visible-p visible-p))
        (device nil) (context nil) (resources nil) (demo nil)
        (completed-p nil))
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
                  (atlas-width
                    (* +block-atlas-tile-size+ +block-atlas-tile-count+))
                  (atlas-height +block-atlas-tile-size+)
                  (atlas-data (make-block-texture-atlas))
                  (atlas-texture
                    (keep
                     (create
                      device
                      (make-texture-descriptor
                       :label "block world texture atlas"
                       :size (list atlas-width atlas-height)
                       :dimensions :2d :format :rgba8-unorm-srgb
                       :usage '(:copy-dst :texture-binding)))))
                  (atlas-view
                    (keep
                     (create device (make-texture-view-descriptor
                                     :texture atlas-texture))))
                  (atlas-sampler
                    (keep
                     (create device (make-sampler-descriptor
                                     :label "block world nearest sampler"
                                     :mag-filter :nearest
                                     :min-filter :nearest
                                     :mipmap-filter :nearest))))
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
                       :entries '((:binding 0 :type :texture)
                                  (:binding 1 :type :sampler)
                                  (:binding 2 :type :uniform-buffer))))))
                  (pipeline
                    (keep
                     (create
                      device
                      (make-render-pipeline-descriptor
                       :label "block world pipeline"
                       :layout layout
                       :vertex
                       `(:module ,vertex-module
                         :buffers ((:array-stride 36
                                    :attributes
                                    ((:shader-location 0 :offset 0
                                      :format :float32x3)
                                     (:shader-location 1 :offset 12
                                      :format :float32x3)
                                     (:shader-location 2 :offset 24
                                      :format :float32x3)))))
                       :fragment
                       `(:module ,fragment-module
                         :targets ((:format ,(canvas-format context))))
                       :primitive '(:topology :triangle-list)
                       :depth-stencil
                       '(:format :depth32-float
                         :depth-write-enabled t :depth-compare :less)))))
                  (new-demo
                    (make-instance
                     'cube-world-demo
                     :canvas canvas :device device :context context
                     :world world :mesher mesher
                     :camera camera
                     :atlas-texture atlas-texture :atlas-view atlas-view
                     :atlas-sampler atlas-sampler
                     :color-texture color-texture :color-view color-view
                     :depth-texture depth-texture :depth-view depth-view
                     :layout layout :pipeline pipeline
                     :resources resources)))
             (write-texture
              (device-queue device)
              (make-texture-copy :texture atlas-texture)
              atlas-data
              (make-texture-data-layout
               :bytes-per-row (* atlas-width 4)
               :rows-per-image atlas-height)
              (list atlas-width atlas-height))
             (setf demo new-demo)
             (refresh-cube-world-mesh demo)
             (setf (canvas-event-handler canvas) demo
                   (canvas-clock canvas)
                   (if frames-per-second
                       (make-cadence-clock
                        (lambda (native-canvas timestamp)
                          (declare (ignore native-canvas))
                          (render-cube-world-frame demo timestamp))
                        :frames-per-second frames-per-second)
                       (make-demand-clock))
                   completed-p t)
               demo)))
      (unless completed-p
        (when demo
          (ignore-errors (destroy-cube-world-chunk-products demo)))
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
    (destroy-cube-world-chunk-products demo)
    (dolist (resource (cube-world-demo-resources demo))
      (destroy resource))
    (setf (cube-world-demo-resources demo) nil)
    (close-canvas canvas))
  (destroy (cube-world-demo-device demo))
  (values))

(defun hidden-cube-world-frame-pathname (directory index)
  (merge-pathnames
   (format nil "block-world-~3,'0D.png" index)
   (uiop:ensure-directory-pathname directory)))

(defun capture-hidden-cube-world-screenshot
    (pathname &key
                (title "luv hidden block world")
                (width 960) (height 640)
                (world (make-little-block-world))
                (mesher (make-instance 'exposed-face-mesher))
                (camera (make-instance 'fly-camera)))
  "Open a hidden SDL/Vulkan canvas, render one block-world frame, and save it."
  (let ((demo nil))
    (unwind-protect
         (progn
           (setf demo
                 (start-cube-world-demo
                  :title title :width width :height height
                  :frames-per-second nil :visible-p nil
                  :world world :mesher mesher :camera camera))
           (capture-cube-world-screenshot demo pathname))
      (when demo
        (stop-cube-world-demo demo)))))

(defun capture-hidden-cube-world-frames
    (directory &key
                 (count 6)
                 (title "luv hidden block world")
                 (width 960) (height 640)
                 (yaw-step 0.35)
                 (world (make-little-block-world))
                 (mesher (make-instance 'exposed-face-mesher))
                 (camera (make-instance 'fly-camera)))
  "Capture COUNT hidden block-world frames into DIRECTORY.

Each frame reuses one hidden SDL/Vulkan canvas and advances CAMERA's yaw by
YAW-STEP, returning the pathnames that were written."
  (check-type count (integer 1))
  (check-type yaw-step real)
  (let ((directory (uiop:ensure-directory-pathname directory))
        (demo nil)
        (initial-yaw (camera-yaw camera)))
    (ensure-directories-exist directory)
    (unwind-protect
         (progn
           (setf demo
                 (start-cube-world-demo
                  :title title :width width :height height
                  :frames-per-second nil :visible-p nil
                  :world world :mesher mesher :camera camera))
           (loop for index below count
                 for pathname =
                 (hidden-cube-world-frame-pathname directory index)
                 do (setf (camera-yaw camera)
                          (+ initial-yaw (* yaw-step index)))
                    (capture-cube-world-screenshot demo pathname)
                 collect pathname))
      (when demo
        (stop-cube-world-demo demo)))))
