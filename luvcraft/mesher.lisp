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
   ;; Published light copied for the same one-cell halo as block samples.
   (sky-samples :initarg :sky-samples :initform nil
                :reader block-mesh-snapshot-sky-samples)
   (block-light-samples :initarg :block-light-samples :initform nil
                        :reader block-mesh-snapshot-block-light-samples))
  (:documentation
   "An immutable dense chunk plus one-cell halo transferred to a CPU worker.

The snapshot owns compact u16 columns, but its small palette contains the same
shared semantic block descriptors used by the world.  Cell identity does not
cross the thread boundary, and the worker never observes live chunk storage."))

(defgeneric mesh-block-world (mesher world))
(defgeneric mesh-block-chunk (mesher world chunk))
(defgeneric mesh-block-snapshot (mesher snapshot))
(defgeneric emit-block-face (mesher world vertices block face x y z))

(defconstant +block-mesh-floats-per-vertex+ 12)
(defconstant +block-mesh-vertices-per-face+ 6)
(defconstant +block-mesh-floats-per-face+
  (* +block-mesh-floats-per-vertex+ +block-mesh-vertices-per-face+))

(declaim (inline push-block-vertex-components))
(defun push-block-vertex-components
    (vertices px py pz u v shade nx ny nz sky-level block-level emission)
  "Append one interleaved vertex without constructing tuple objects.

The fourth lane carries normalized raw light readings, not an
art-directed bake: shader edits can change the response curve without
remeshing the world."
  (vector-push (coerce px 'single-float) vertices)
  (vector-push (coerce py 'single-float) vertices)
  (vector-push (coerce pz 'single-float) vertices)
  (vector-push (coerce u 'single-float) vertices)
  (vector-push (coerce v 'single-float) vertices)
  (vector-push (coerce shade 'single-float) vertices)
  (vector-push (coerce nx 'single-float) vertices)
  (vector-push (coerce ny 'single-float) vertices)
  (vector-push (coerce nz 'single-float) vertices)
  (vector-push (coerce sky-level 'single-float) vertices)
  (vector-push (coerce block-level 'single-float) vertices)
  (vector-push (coerce emission 'single-float) vertices)
  vertices)

(defun block-color-variation (x y z)
  (+ 0.93 (* 0.07 (/ (mod (+ (* x 17) (* y 31) (* z 13)) 7) 6.0))))

(defstruct (block-mesh-neighborhood
             (:constructor %make-block-mesh-neighborhood))
  "The 3x3x3 resident chunk neighborhood needed by one chunk mesh."
  (domain nil :type (or null chunk-domain))
  (chunks (make-array 27 :initial-element nil) :type simple-vector))

(defun block-mesh-neighborhood-index (dx dy dz)
  (+ (1+ dx) (* 3 (+ (1+ dy) (* 3 (1+ dz))))))

(defun make-block-mesh-neighborhood (world chunk)
  "Resolve once the chunks every visibility and AO sample can reach."
  (let* ((domain (block-chunk-domain chunk))
         (coordinate (chunk-domain-coordinate domain))
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
    (%make-block-mesh-neighborhood :domain domain :chunks chunks)))

(declaim (inline block-mesh-neighborhood-locate))
(defun block-mesh-neighborhood-locate (neighborhood x y z)
  "Resolve one nearby world site to its resident chunk and dense offset.

The 3x3x3 window owns availability; voxel-space and chunk-domain own the
decomposition and storage order beneath it.  See #K3KZTG."
  (let* ((domain (block-mesh-neighborhood-domain neighborhood))
         (space (chunk-domain-space domain))
         (center (chunk-domain-coordinate domain)))
    (multiple-value-bind
          (chunk-x chunk-y chunk-z local-x local-y local-z)
        (voxel-space-decompose-components space x y z)
      (let ((dx (- chunk-x (chunk-coordinate-x center)))
            (dy (- chunk-y (chunk-coordinate-y center)))
            (dz (- chunk-z (chunk-coordinate-z center))))
        (when (and (<= -1 dx 1) (<= -1 dy 1) (<= -1 dz 1))
          (let ((chunk
                  (aref (block-mesh-neighborhood-chunks neighborhood)
                        (block-mesh-neighborhood-index dx dy dz))))
            (when chunk
              (values chunk
                      (chunk-domain-offset-components
                       (block-chunk-domain chunk)
                       local-x local-y local-z)))))))))

(declaim (inline block-mesh-neighborhood-block-at))
(defun block-mesh-neighborhood-block-at (neighborhood x y z)
  "Read a nearby world site with no coordinate objects or hash-key consing."
  (multiple-value-bind (chunk offset)
      (block-mesh-neighborhood-locate neighborhood x y z)
    (if chunk
        (values (block-content-at-offset
                 (block-chunk-content chunk) offset)
                :resident)
        (values nil :absent))))

(declaim (inline block-mesh-halo-offset-components))
(defun block-mesh-halo-offset-components (domain x y z)
  "Return the dense one-cell-halo offset for a world site, or NIL outside."
  (let* ((shape (voxel-space-chunk-shape (chunk-domain-space domain)))
         (sample-width (+ 2 (chunk-shape-width shape)))
         (sample-height (+ 2 (chunk-shape-height shape)))
         (sample-depth (+ 2 (chunk-shape-depth shape))))
    (multiple-value-bind (origin-x origin-y origin-z)
        (chunk-domain-world-components domain 0 0 0)
      (let ((sample-x (1+ (- x origin-x)))
            (sample-y (1+ (- y origin-y)))
            (sample-z (1+ (- z origin-z))))
        (when (and (<= 0 sample-x) (< sample-x sample-width)
                   (<= 0 sample-y) (< sample-y sample-height)
                   (<= 0 sample-z) (< sample-z sample-depth))
          (+ sample-x
             (* sample-width
                (+ sample-y (* sample-height sample-z)))))))))

(declaim (inline block-mesh-snapshot-block-at))
(defun block-mesh-snapshot-block-at (snapshot x y z)
  (let ((offset (block-mesh-halo-offset-components
                 (block-mesh-snapshot-domain snapshot) x y z)))
    (if offset
        (let ((index (aref (block-mesh-snapshot-sample-indices snapshot)
                           offset)))
          ;; Zero is absent.  Resident air has its own palette entry.
          (if (zerop index)
              (values nil :absent)
              (values (aref (block-mesh-snapshot-palette snapshot) index)
                      :resident)))
        (values nil :absent))))

(defgeneric sample-block-at (samples x y z)
  (:documentation
   "Read one world site from a meshing sample source.

Return (VALUES BLOCK STATUS) where STATUS is :RESIDENT or :ABSENT.  Meshing
itself never chooses a representation: the live world, a preresolved chunk
neighborhood, and an immutable worker snapshot each answer through their own
method, and further sample sources only need to add one."))

(defmethod sample-block-at ((samples block-world) x y z)
  (world-block-at samples x y z))

(defmethod sample-block-at ((samples block-mesh-neighborhood) x y z)
  (block-mesh-neighborhood-block-at samples x y z))

(defmethod sample-block-at ((samples block-mesh-snapshot) x y z)
  (block-mesh-snapshot-block-at samples x y z))

(defgeneric sample-light-at (samples x y z)
  (:documentation
   "Read one site's published light from a meshing sample source.

Return (VALUES SKY BLOCK STATUS) with raw 0..15 levels.  STATUS is
:RESIDENT or :ABSENT; an absent or unlit sample answers zeros, and the
corner-averaging rules decide what that means rather than any caller
falling through to BLOCK-SOLID-P."))

(defmethod sample-light-at ((samples block-world) x y z)
  (multiple-value-bind (sky block state) (world-light-at samples x y z)
    (if (eq state :absent)
        (values 0 0 :absent)
        (values sky block :resident))))

(defmethod sample-light-at ((samples block-mesh-neighborhood) x y z)
  (multiple-value-bind (chunk offset)
      (block-mesh-neighborhood-locate samples x y z)
    (if chunk
        (let ((field (block-chunk-light-field chunk)))
          (if field
              (values
               (aref (chunk-light-field-sky-levels field) offset)
               (aref (chunk-light-field-block-levels field) offset)
               :resident)
              (values 0 0 :resident)))
        (values 0 0 :absent))))

(defmethod sample-light-at ((samples block-mesh-snapshot) x y z)
  (let ((offset (block-mesh-halo-offset-components
                 (block-mesh-snapshot-domain samples) x y z)))
    (if offset
        (if (zerop (aref (block-mesh-snapshot-sample-indices samples)
                         offset))
            (values 0 0 :absent)
            (values
             (aref (block-mesh-snapshot-sky-samples samples) offset)
             (aref (block-mesh-snapshot-block-light-samples samples) offset)
             :resident))
        (values 0 0 :absent))))

(defun mesher-block-at (mesher samples x y z)
  (multiple-value-bind (block status) (sample-block-at samples x y z)
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

(declaim (inline block-face-corner-light-components))
(defun block-face-corner-light-components
    (mesher samples nx ny nz cx cy cz x y z)
  "Average the reachable face-adjacent light around one vertex corner.

The four candidate cells mirror the ambient-occlusion neighborhood, but
occupancy and light averaging remain separately named results.  A solid
cell holds no air light; an absent or unlit sample contributes nothing
rather than posing as open sky; and the diagonal cell is unreachable when
both side cells occlude it.  Returns (VALUES SKY-LEVEL BLOCK-LEVEL)
normalized to 0..1."
  (let ((sky-sum 0) (block-sum 0) (count 0))
    (flet ((solid-p (ox oy oz)
             (block-solid-p
              (mesher-block-at mesher samples (+ x ox) (+ y oy) (+ z oz))))
           (consider (ox oy oz)
             (multiple-value-bind (sky block status)
                 (sample-light-at samples (+ x ox) (+ y oy) (+ z oz))
               (when (eq status :resident)
                 (incf sky-sum sky)
                 (incf block-sum block)
                 (incf count)))))
      (multiple-value-bind (s1x s1y s1z s2x s2y s2z dx dy dz)
          (cond ((not (zerop nx))
                 (let ((sy (if (zerop cy) -1 1))
                       (sz (if (zerop cz) -1 1)))
                   (values nx sy 0 nx 0 sz nx sy sz)))
                ((not (zerop ny))
                 (let ((sx (if (zerop cx) -1 1))
                       (sz (if (zerop cz) -1 1)))
                   (values sx ny 0 0 ny sz sx ny sz)))
                (t
                 (let ((sx (if (zerop cx) -1 1))
                       (sy (if (zerop cy) -1 1)))
                   (values sx 0 nz 0 sy nz sx sy nz))))
        (consider nx ny nz)
        (unless (solid-p s1x s1y s1z) (consider s1x s1y s1z))
        (unless (solid-p s2x s2y s2z) (consider s2x s2y s2z))
        (unless (and (solid-p s1x s1y s1z) (solid-p s2x s2y s2z))
          (unless (solid-p dx dy dz) (consider dx dy dz)))))
    (if (plusp count)
        (values (/ sky-sum (* 15.0 count)) (/ block-sum (* 15.0 count)))
        (values 0.0 0.0))))

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
         (nx (voxel-direction-dx normal))
         (ny (voxel-direction-dy normal))
         (nz (voxel-direction-dz normal))
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
        (multiple-value-bind (sky-level block-level)
            (block-face-corner-light-components
             mesher samples nx ny nz cx cy cz x y z)
          (multiple-value-bind (local-u local-v)
              (block-face-local-uv face corner)
            (push-block-vertex-components
             vertices (+ x cx) (+ y cy) (+ z cz)
             (/ (+ (* tile size) 0.5 (* local-u (1- size))) atlas-width)
             (/ (+ 0.5 (* local-v (1- size))) size)
             shade nx ny nz
             sky-level block-level
             (block-surface-emission block))))))
    vertices))

(defmethod emit-block-face
    ((mesher exposed-face-mesher) (world block-world) vertices
     (block block-kind) (face block-face) x y z)
  "Compatibility entry point for tools emitting an individual world face."
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (voxel-space-decompose-components (block-world-space world) x y z)
    (let ((chunk (world-chunk-at world chunk-x chunk-y chunk-z)))
      (unless chunk
        (error "Cannot emit a face from absent chunk (~D ~D ~D)."
               chunk-x chunk-y chunk-z))
      (emit-block-face-into
       mesher (make-block-mesh-neighborhood world chunk)
       vertices block face x y z))))

(defun block-storage-face-masks (mesher samples domain palette indices)
  "Return one exposed-face bit mask per site and the exact face count."
  (let* ((masks (make-array (chunk-domain-cardinality domain)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0))
         (face-count 0))
    (do-chunk-domain-sites (offset local domain)
      (when (block-solid-p (aref palette (aref indices offset)))
        (let* ((coordinate (chunk-domain-world-coordinate domain local))
               (x (world-coordinate-x coordinate))
               (y (world-coordinate-y coordinate))
               (z (world-coordinate-z coordinate))
               (mask 0))
          (declare (dynamic-extent coordinate))
          (loop for face in *block-faces*
                for bit from 0
                for normal = (block-face-neighbor face)
                unless (block-solid-p
                        (mesher-block-at
                         mesher samples
                         (+ x (voxel-direction-dx normal))
                         (+ y (voxel-direction-dy normal))
                         (+ z (voxel-direction-dz normal))))
                  do (setf mask (logior mask (ash 1 bit)))
                     (incf face-count))
          (setf (aref masks offset) mask))))
    (values masks face-count)))

(defun mesh-block-storage (mesher samples domain palette indices)
  "Mesh one immutable or owner-borrowed dense chunk representation."
  (multiple-value-bind (masks face-count)
      (block-storage-face-masks mesher samples domain palette indices)
    (let ((vertices
            (make-array (* face-count +block-mesh-floats-per-face+)
                        :element-type 'single-float :fill-pointer 0)))
      (do-chunk-domain-sites (offset local domain)
        (let ((mask (aref masks offset)))
          (unless (zerop mask)
            (let* ((block (aref palette (aref indices offset)))
                   (coordinate
                     (chunk-domain-world-coordinate domain local))
                   (x (world-coordinate-x coordinate))
                   (y (world-coordinate-y coordinate))
                   (z (world-coordinate-z coordinate)))
              (declare (dynamic-extent coordinate))
              (loop for face in *block-faces*
                    for bit from 0
                    when (logbitp bit mask)
                      do (emit-block-face-into
                          mesher samples vertices block face x y z))))))
      (assert (= (length vertices)
                 (* face-count +block-mesh-floats-per-face+)))
      (make-instance 'block-mesh
                     :vertices vertices
                     :vertex-count (* face-count
                                      +block-mesh-vertices-per-face+)
                     :face-count face-count))))

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
         (sky-samples
           (make-array (* sample-width sample-height sample-depth)
                       :element-type '(unsigned-byte 8)
                       :initial-element 0))
         (block-light-samples
           (make-array (* sample-width sample-height sample-depth)
                       :element-type '(unsigned-byte 8)
                       :initial-element 0))
         (neighborhood (make-block-mesh-neighborhood world chunk)))
    (setf (gethash nil palette-indices) 1)
    (dotimes (sample-z sample-depth)
      (dotimes (sample-y sample-height)
        (dotimes (sample-x sample-width)
          (let ((world-x (+ origin-x (1- sample-x)))
                (world-y (+ origin-y (1- sample-y)))
                (world-z (+ origin-z (1- sample-z))))
            (let ((sample-offset
                    (block-mesh-halo-offset-components
                     domain world-x world-y world-z)))
              (assert sample-offset)
              (multiple-value-bind (block status)
                  (block-mesh-neighborhood-block-at
                   neighborhood world-x world-y world-z)
                (let ((index
                        (if (eq status :resident)
                            (block-mesh-snapshot-palette-index
                             block palette palette-indices)
                            0)))
                  (setf (aref sample-indices sample-offset) index)
                  (when (and (<= 1 sample-x width)
                             (<= 1 sample-y height)
                             (<= 1 sample-z depth))
                    (setf (aref target-indices
                                (chunk-domain-offset-components
                                 domain
                                 (1- sample-x)
                                 (1- sample-y)
                                 (1- sample-z)))
                          index))))
              (multiple-value-bind (sky block-light status)
                  (sample-light-at neighborhood world-x world-y world-z)
                (declare (ignore status))
                (setf (aref sky-samples sample-offset) sky
                      (aref block-light-samples sample-offset)
                      block-light)))))))
    (make-instance
     'block-mesh-snapshot
     :key (block-chunk-key chunk) :dependency-stamp dependency-stamp
     :domain domain :palette palette :target-indices target-indices
     :sample-indices sample-indices
     :sky-samples sky-samples :block-light-samples block-light-samples)))

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
