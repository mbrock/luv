;;; Turning resident block data into exposed-face triangle meshes.
;;;
;;; The result of meshing is deliberately mundane: interleaved position,
;;; UV/shade, and normal triples of single floats.  Meshing can read the live
;;; world, a preresolved 3x3x3 chunk neighborhood, or an immutable
;;; BLOCK-MESH-SNAPSHOT copied for a worker thread; ambient occlusion and
;;; face visibility sample through the same accessor in all three cases.

(in-package #:luv)

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

(defclass block-mesh-snapshot ()
  ((key :initarg :key :reader block-mesh-snapshot-key)
   (dependency-stamp :initarg :dependency-stamp
                     :reader block-mesh-snapshot-dependency-stamp)
   (domain :initarg :domain :reader block-mesh-snapshot-domain)
   (palette :initarg :palette :reader block-mesh-snapshot-palette)
   (target-indices :initarg :target-indices
                   :reader block-mesh-snapshot-target-indices)
   (sample-indices :initarg :sample-indices
                   :reader block-mesh-snapshot-sample-indices)
   (origin-x :initarg :origin-x :reader block-mesh-snapshot-origin-x)
   (origin-y :initarg :origin-y :reader block-mesh-snapshot-origin-y)
   (origin-z :initarg :origin-z :reader block-mesh-snapshot-origin-z)
   (width :initarg :width :reader block-mesh-snapshot-width)
   (height :initarg :height :reader block-mesh-snapshot-height)
   (depth :initarg :depth :reader block-mesh-snapshot-depth))
  (:documentation
   "An immutable dense chunk plus one-cell halo transferred to a CPU worker.

The snapshot owns compact u16 columns, but its small palette contains the same
shared semantic block descriptors used by the world.  Cell identity does not
cross the thread boundary, and the worker never observes live chunk storage."))

(defgeneric mesh-block-world (mesher world))
(defgeneric mesh-block-chunk (mesher world chunk))
(defgeneric mesh-block-snapshot (mesher snapshot))
(defgeneric emit-block-face (mesher world vertices block face x y z))

(defconstant +block-mesh-floats-per-vertex+ 9)
(defconstant +block-mesh-vertices-per-face+ 6)
(defconstant +block-mesh-floats-per-face+
  (* +block-mesh-floats-per-vertex+ +block-mesh-vertices-per-face+))

(declaim (inline push-block-vertex-components))
(defun push-block-vertex-components
    (vertices px py pz u v shade nx ny nz)
  "Append one interleaved vertex without constructing tuple objects."
  (vector-push (coerce px 'single-float) vertices)
  (vector-push (coerce py 'single-float) vertices)
  (vector-push (coerce pz 'single-float) vertices)
  (vector-push (coerce u 'single-float) vertices)
  (vector-push (coerce v 'single-float) vertices)
  (vector-push (coerce shade 'single-float) vertices)
  (vector-push (coerce nx 'single-float) vertices)
  (vector-push (coerce ny 'single-float) vertices)
  (vector-push (coerce nz 'single-float) vertices)
  vertices)

(defun block-color-variation (x y z)
  (+ 0.93 (* 0.07 (/ (mod (+ (* x 17) (* y 31) (* z 13)) 7) 6.0))))

(defstruct (block-mesh-neighborhood
             (:constructor %make-block-mesh-neighborhood))
  "The 3x3x3 resident chunk neighborhood needed by one chunk mesh."
  (origin-x 0 :type integer)
  (origin-y 0 :type integer)
  (origin-z 0 :type integer)
  (width 16 :type (integer 1))
  (height 16 :type (integer 1))
  (depth 16 :type (integer 1))
  (chunks (make-array 27 :initial-element nil) :type simple-vector))

(defun block-mesh-neighborhood-index (dx dy dz)
  (+ (1+ dx) (* 3 (+ (1+ dy) (* 3 (1+ dz))))))

(defun make-block-mesh-neighborhood (world chunk)
  "Resolve once the chunks every visibility and AO sample can reach."
  (let* ((domain (block-chunk-domain chunk))
         (coordinate (chunk-domain-coordinate domain))
         (shape (voxel-space-chunk-shape (block-world-space world)))
         (width (chunk-shape-width shape))
         (height (chunk-shape-height shape))
         (depth (chunk-shape-depth shape))
         (chunk-x (chunk-coordinate-x coordinate))
         (chunk-y (chunk-coordinate-y coordinate))
         (chunk-z (chunk-coordinate-z coordinate))
         (chunks (make-array 27 :initial-element nil)))
    (loop for dz from -1 to 1 do
      (loop for dy from -1 to 1 do
        (loop for dx from -1 to 1 do
          (setf (aref chunks (block-mesh-neighborhood-index dx dy dz))
                (world-chunk-at world
                                (+ chunk-x dx)
                                (+ chunk-y dy)
                                (+ chunk-z dz))))))
    (%make-block-mesh-neighborhood
     :origin-x (* chunk-x width)
     :origin-y (* chunk-y height)
     :origin-z (* chunk-z depth)
     :width width :height height :depth depth :chunks chunks)))

(declaim (inline block-mesh-neighborhood-block-at))
(defun block-mesh-neighborhood-block-at (neighborhood x y z)
  "Read a nearby world site with no coordinate objects or hash-key consing."
  (let ((relative-x (- x (block-mesh-neighborhood-origin-x neighborhood)))
        (relative-y (- y (block-mesh-neighborhood-origin-y neighborhood)))
        (relative-z (- z (block-mesh-neighborhood-origin-z neighborhood))))
    (multiple-value-bind (dx local-x)
        (floor relative-x (block-mesh-neighborhood-width neighborhood))
      (multiple-value-bind (dy local-y)
          (floor relative-y (block-mesh-neighborhood-height neighborhood))
        (multiple-value-bind (dz local-z)
            (floor relative-z (block-mesh-neighborhood-depth neighborhood))
          (if (and (<= -1 dx 1) (<= -1 dy 1) (<= -1 dz 1))
              (let ((chunk
                      (aref (block-mesh-neighborhood-chunks neighborhood)
                            (block-mesh-neighborhood-index dx dy dz))))
                (if chunk
                    (values
                     (block-content-at-offset
                      (block-chunk-content chunk)
                      (+ local-x
                         (* (block-mesh-neighborhood-width neighborhood)
                            (+ local-y
                               (* (block-mesh-neighborhood-height neighborhood)
                                  local-z)))))
                     :resident)
                    (values nil :absent)))
              (values nil :absent)))))))

(declaim (inline block-mesh-snapshot-block-at))
(defun block-mesh-snapshot-block-at (snapshot x y z)
  (let ((sample-x (1+ (- x (block-mesh-snapshot-origin-x snapshot))))
        (sample-y (1+ (- y (block-mesh-snapshot-origin-y snapshot))))
        (sample-z (1+ (- z (block-mesh-snapshot-origin-z snapshot))))
        (sample-width (+ 2 (block-mesh-snapshot-width snapshot)))
        (sample-height (+ 2 (block-mesh-snapshot-height snapshot)))
        (sample-depth (+ 2 (block-mesh-snapshot-depth snapshot))))
    (if (and (<= 0 sample-x) (< sample-x sample-width)
             (<= 0 sample-y) (< sample-y sample-height)
             (<= 0 sample-z) (< sample-z sample-depth))
        (let ((index
                (aref (block-mesh-snapshot-sample-indices snapshot)
                      (+ sample-x
                         (* sample-width
                            (+ sample-y (* sample-height sample-z)))))))
          ;; Zero is absent.  Resident air has its own palette entry.
          (if (zerop index)
              (values nil :absent)
              (values (aref (block-mesh-snapshot-palette snapshot) index)
                      :resident)))
        (values nil :absent))))

(defun mesher-block-at (mesher samples x y z)
  (multiple-value-bind (block status)
      (etypecase samples
        (block-world (describe-block-allocatingly samples x y z))
        (block-mesh-neighborhood
         (block-mesh-neighborhood-block-at samples x y z))
        (block-mesh-snapshot
         (block-mesh-snapshot-block-at samples x y z)))
    (ecase status
      (:resident block)
      (:absent
       (ecase (exposed-face-mesher-absent-neighbor-policy mesher)
         (:air nil)
         (:solid *stone-block*)
         (:error
          (error "Meshing reached absent terrain at (~D ~D ~D)." x y z)))))))

(declaim (inline block-face-corner-occlusion-components))
(defun block-face-corner-occlusion-components
    (mesher samples nx ny nz cx cy cz x y z)
  "Return corner AO using scalar offsets and no temporary axis/offset lists."
  (flet ((occupied-p (ox oy oz)
           (block-solid-p
            (mesher-block-at mesher samples (+ x ox) (+ y oy) (+ z oz)))))
    (multiple-value-bind (first-side second-side corner-block)
        (cond
          ((not (zerop nx))
           (let ((sy (if (zerop cy) -1 1))
                 (sz (if (zerop cz) -1 1)))
             (values (occupied-p nx sy 0)
                     (occupied-p nx 0 sz)
                     (occupied-p nx sy sz))))
          ((not (zerop ny))
           (let ((sx (if (zerop cx) -1 1))
                 (sz (if (zerop cz) -1 1)))
             (values (occupied-p sx ny 0)
                     (occupied-p 0 ny sz)
                     (occupied-p sx ny sz))))
          (t
           (let ((sx (if (zerop cx) -1 1))
                 (sy (if (zerop cy) -1 1)))
             (values (occupied-p sx 0 nz)
                     (occupied-p 0 sy nz)
                     (occupied-p sx sy nz)))))
      (if (and first-side second-side)
          0.56
          (- 1.0
             (* 0.14 (+ (if first-side 1 0)
                        (if second-side 1 0)
                        (if corner-block 1 0))))))))

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

(defun emit-block-face-into
    (mesher samples vertices block face x y z)
  (let* ((corners (block-face-corners face))
         (normal (block-face-neighbor face))
         (nx (first normal))
         (ny (second normal))
         (nz (third normal))
         (tile (block-face-tile block face))
         (size +block-atlas-tile-size+)
         (atlas-width (* size +block-atlas-tile-count+))
         (variation (block-color-variation x y z)))
    (dolist (index '(0 1 2 0 2 3))
      (let* ((corner (nth index corners))
             (cx (first corner))
             (cy (second corner))
             (cz (third corner))
             (shade (* variation
                       (block-face-corner-occlusion-components
                        mesher samples nx ny nz cx cy cz x y z))))
        (multiple-value-bind (local-u local-v)
            (block-face-local-uv face corner)
          (push-block-vertex-components
           vertices (+ x cx) (+ y cy) (+ z cz)
           (/ (+ (* tile size) 0.5 (* local-u (1- size))) atlas-width)
           (/ (+ 0.5 (* local-v (1- size))) size)
           shade nx ny nz))))
    vertices))

(defmethod emit-block-face
    ((mesher exposed-face-mesher) (world block-world) vertices
     (block block-kind) (face block-face) x y z)
  "Compatibility entry point for tools emitting an individual world face."
  (let* ((shape (voxel-space-chunk-shape (block-world-space world)))
         (chunk-x (floor x (chunk-shape-width shape)))
         (chunk-y (floor y (chunk-shape-height shape)))
         (chunk-z (floor z (chunk-shape-depth shape)))
         (chunk (world-chunk-at world chunk-x chunk-y chunk-z)))
    (unless chunk
      (error "Cannot emit a face from absent chunk (~D ~D ~D)."
             chunk-x chunk-y chunk-z))
    (emit-block-face-into
     mesher (make-block-mesh-neighborhood world chunk)
     vertices block face x y z)))

(defun block-storage-face-masks (mesher samples domain palette indices)
  "Return one exposed-face bit mask per site and the exact face count."
  (let* ((shape (voxel-space-chunk-shape (chunk-domain-space domain)))
         (width (chunk-shape-width shape))
         (height (chunk-shape-height shape))
         (depth (chunk-shape-depth shape))
         (origin (chunk-domain-origin domain))
         (origin-x (world-coordinate-x origin))
         (origin-y (world-coordinate-y origin))
         (origin-z (world-coordinate-z origin))
         (masks (make-array (chunk-domain-cardinality domain)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0))
         (face-count 0)
         (offset 0))
    (dotimes (local-z depth)
      (dotimes (local-y height)
        (dotimes (local-x width)
          (when (block-solid-p (aref palette (aref indices offset)))
            (let ((x (+ origin-x local-x))
                  (y (+ origin-y local-y))
                  (z (+ origin-z local-z))
                  (mask 0))
              (loop for face in *block-faces*
                    for bit from 0
                    for normal = (block-face-neighbor face)
                    unless (block-solid-p
                            (mesher-block-at
                             mesher samples
                             (+ x (first normal))
                             (+ y (second normal))
                             (+ z (third normal))))
                      do (setf mask (logior mask (ash 1 bit)))
                         (incf face-count))
              (setf (aref masks offset) mask)))
          (incf offset))))
    (values masks face-count)))

(defun mesh-block-storage (mesher samples domain palette indices)
  "Mesh one immutable or owner-borrowed dense chunk representation."
  (let* ((shape (voxel-space-chunk-shape (chunk-domain-space domain)))
         (width (chunk-shape-width shape))
         (height (chunk-shape-height shape))
         (depth (chunk-shape-depth shape))
         (origin (chunk-domain-origin domain))
         (origin-x (world-coordinate-x origin))
         (origin-y (world-coordinate-y origin))
         (origin-z (world-coordinate-z origin)))
    (multiple-value-bind (masks face-count)
        (block-storage-face-masks mesher samples domain palette indices)
      (let ((vertices
              (make-array (* face-count +block-mesh-floats-per-face+)
                          :element-type 'single-float :fill-pointer 0))
            (offset 0))
        (dotimes (local-z depth)
          (dotimes (local-y height)
            (dotimes (local-x width)
              (let ((mask (aref masks offset)))
                (unless (zerop mask)
                  (let ((block (aref palette (aref indices offset)))
                        (x (+ origin-x local-x))
                        (y (+ origin-y local-y))
                        (z (+ origin-z local-z)))
                    (loop for face in *block-faces*
                          for bit from 0
                          when (logbitp bit mask)
                            do (emit-block-face-into
                                mesher samples vertices block face x y z)))))
              (incf offset))))
        (assert (= (length vertices)
                   (* face-count +block-mesh-floats-per-face+)))
        (make-instance 'block-mesh
                       :vertices vertices
                       :vertex-count (* face-count
                                        +block-mesh-vertices-per-face+)
                       :face-count face-count)))))

(defmethod mesh-block-chunk
    ((mesher exposed-face-mesher) (world block-world) (chunk block-chunk))
  (let ((samples (make-block-mesh-neighborhood world chunk)))
    (with-block-content-storage (domain palette indices) chunk
      (mesh-block-storage mesher samples domain palette indices))))

(defmethod mesh-block-snapshot
    ((mesher exposed-face-mesher) (snapshot block-mesh-snapshot))
  (mesh-block-storage
   mesher snapshot
   (block-mesh-snapshot-domain snapshot)
   (block-mesh-snapshot-palette snapshot)
   (block-mesh-snapshot-target-indices snapshot)))

(defun block-mesh-snapshot-palette-index (block palette indices)
  (or (gethash block indices)
      (let ((index (length palette)))
        (unless (< index #xffff)
          (error "A mesh snapshot cannot contain more than 65534 block states."))
        (vector-push-extend block palette)
        (setf (gethash block indices) index)
        index)))

(defun make-block-mesh-snapshot (world chunk dependency-stamp)
  "Copy CHUNK and its one-cell halo into immutable worker-owned columns."
  (let* ((domain (block-chunk-domain chunk))
         (shape (voxel-space-chunk-shape (chunk-domain-space domain)))
         (width (chunk-shape-width shape))
         (height (chunk-shape-height shape))
         (depth (chunk-shape-depth shape))
         (sample-width (+ width 2))
         (sample-height (+ height 2))
         (sample-depth (+ depth 2))
         (origin (chunk-domain-origin domain))
         (origin-x (world-coordinate-x origin))
         (origin-y (world-coordinate-y origin))
         (origin-z (world-coordinate-z origin))
         ;; Index zero denotes an absent sample.  Index one denotes resident
         ;; air, whose semantic descriptor is also NIL.
         (palette (make-array 2 :adjustable t :fill-pointer 2
                                :initial-contents '(nil nil)))
         (palette-indices (make-hash-table :test #'eq))
         (target-indices
           (make-array (chunk-domain-cardinality domain)
                       :element-type '(unsigned-byte 16)))
         (sample-indices
           (make-array (* sample-width sample-height sample-depth)
                       :element-type '(unsigned-byte 16)))
         (neighborhood (make-block-mesh-neighborhood world chunk)))
    (setf (gethash nil palette-indices) 1)
    (dotimes (sample-z sample-depth)
      (dotimes (sample-y sample-height)
        (dotimes (sample-x sample-width)
          (let ((world-x (+ origin-x (1- sample-x)))
                (world-y (+ origin-y (1- sample-y)))
                (world-z (+ origin-z (1- sample-z))))
            (multiple-value-bind (block status)
                (block-mesh-neighborhood-block-at
                 neighborhood world-x world-y world-z)
              (let ((index
                      (if (eq status :resident)
                          (block-mesh-snapshot-palette-index
                           block palette palette-indices)
                          0)))
                (setf (aref sample-indices
                            (+ sample-x
                               (* sample-width
                                  (+ sample-y (* sample-height sample-z)))))
                      index)
                (when (and (<= 1 sample-x width)
                           (<= 1 sample-y height)
                           (<= 1 sample-z depth))
                  (setf (aref target-indices
                              (+ (1- sample-x)
                                 (* width
                                    (+ (1- sample-y)
                                       (* height (1- sample-z))))))
                        index))))))))
    (make-instance
     'block-mesh-snapshot
     :key (block-chunk-key chunk) :dependency-stamp dependency-stamp
     :domain domain :palette palette :target-indices target-indices
     :sample-indices sample-indices
     :origin-x origin-x :origin-y origin-y :origin-z origin-z
     :width width :height height :depth depth)))

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
