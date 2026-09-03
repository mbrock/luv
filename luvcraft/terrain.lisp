;;; The little world: deterministic terrain, landmarks, and recorded edits.
;;;
;;; A LITTLE-WORLD-SOURCE is a pure function of its seed.  Terrain heights and
;;; biome materials come from stable value noise, caves from volume noise,
;;; landmarks (rocks and trees) from coordinate hashes, and player edits from
;;; a sparse replayable overlay, so any chunk can be regenerated
;;; bit-identically on any thread.
;;;
;;; Two reliefs share this machinery.  The :MEADOW relief is the original
;;; gentle heightmap and stays bit-identical so saved worlds keep the ground
;;; their edits were built on.  The :ALPINE relief fills whatever vertical
;;; room a chunk offers with mountain ranges, ravines, and cave systems.

(in-package #:luvcraft)

(defparameter *little-world-height* 64
  "The chunk height a new little world is given.

The alpine relief scales with it: lowlands sit near a fifth of the way up,
the snow line a little past half, and the highest ridges stop three cells
short of the sky.")

(defclass little-world-source ()
  ((seed :initarg :seed :initform 121 :reader little-world-source-seed)
   (relief :initarg :relief :initform :alpine
           :type (member :meadow :alpine)
           :reader little-world-source-relief
           :documentation
           "Which terrain family the seed is read through: the original
gentle :MEADOW, or the :ALPINE mountains and caves.")
   (edits :initarg :edits :initform (make-block-edit-overlay)
          :reader little-world-source-edits)))

(defmethod shared-initialize :after
    ((source little-world-source) slot-names &key)
  (declare (ignore slot-names))
  (unless (member (little-world-source-relief source) '(:meadow :alpine))
    (error "A little-world relief must be :MEADOW or :ALPINE, not ~S."
           (little-world-source-relief source))))

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

(defun little-world-hash-3d (source x y z &optional (salt 0))
  "A stable volume hash for cave readings; the 2D hash with a Y term."
  (declare (type fixnum x y z salt))
  (let ((value
          (logand #xffffffff
                  (+ (logand #xffffffff (little-world-source-seed source))
                     (logand #xffffffff (* (logand x #xffffffff) 374761393))
                     (logand #xffffffff (* (logand y #xffffffff) 3266489917))
                     (logand #xffffffff (* (logand z #xffffffff) 668265263))
                     (logand #xffffffff
                             (* (logand salt #xffffffff) 2246822519))))))
    (declare (type (unsigned-byte 32) value))
    (setf value (logand #xffffffff
                        (* (logxor value (ash value -13)) 1274126177)))
    (logand #xffffffff (logxor value (ash value -16)))))

(defun little-world-hash-reading-3d (source x y z salt)
  (- (* 2.0d0 (/ (little-world-hash-3d source x y z salt) #xffffffff))
     1.0d0))

(defun little-world-volume-noise
    (source x y z period vertical-period &optional (salt 0))
  "Sample deterministic smooth value noise at integer world position X,Y,Z.

PERIOD is the horizontal cell size and VERTICAL-PERIOD the vertical one, so
a reading can be stretched flat for tunnels that run rather than plunge."
  (check-type period (real (0) *))
  (check-type vertical-period (real (0) *))
  (let* ((sample-x (/ x (coerce period 'double-float)))
         (sample-y (/ y (coerce vertical-period 'double-float)))
         (sample-z (/ z (coerce period 'double-float)))
         (cell-x (floor sample-x))
         (cell-y (floor sample-y))
         (cell-z (floor sample-z))
         (tx (smooth-little-world-reading (- sample-x cell-x)))
         (ty (smooth-little-world-reading (- sample-y cell-y)))
         (tz (smooth-little-world-reading (- sample-z cell-z))))
    (flet ((corner (dx dy dz)
             (little-world-hash-reading-3d
              source (+ cell-x dx) (+ cell-y dy) (+ cell-z dz) salt))
           (lerp (left right amount)
             (interpolate-little-world-reading left right amount)))
      (lerp (lerp (lerp (corner 0 0 0) (corner 1 0 0) tx)
                  (lerp (corner 0 0 1) (corner 1 0 1) tx)
                  tz)
            (lerp (lerp (corner 0 1 0) (corner 1 1 0) tx)
                  (lerp (corner 0 1 1) (corner 1 1 1) tx)
                  tz)
            ty))))

(defun little-world-smoothstep (edge0 edge1 value)
  "0 at or below EDGE0, 1 at or above EDGE1, smooth in between."
  (smooth-little-world-reading
   (max 0.0d0 (min 1.0d0 (/ (- value edge0) (- edge1 edge0))))))

;;; The meadow relief: the original gentle heightmap.

(defun little-world-meadow-surface-height (source x z height)
  (let ((reading
          (+ 5.5d0
             (* 3.2d0 (little-world-value-noise source x z 64 0))
             (* 1.7d0 (little-world-value-noise source x z 28 1))
             (* 0.8d0 (little-world-value-noise source x z 11 2)))))
    (max 2 (min (- height 4) (round reading)))))

(defun little-world-meadow-surface-material (source x z surface height)
  (let* ((moisture (little-world-value-noise source x z 52 17))
         (slope (little-world-surface-slope source x z surface height)))
    (cond ((>= surface 9) *snow-block*)
          ((or (<= surface 3) (< moisture -0.48d0)) *sand-block*)
          ((>= slope 2) *stone-block*)
          (t *grass-block*))))

;;; The alpine relief: mountain ranges, ravines, and caves.
;;;
;;; Heights are read as fractions of the chunk height so the same seed gives
;;; a 16-cell test world foothills and a 64-cell game world real peaks.

(defparameter *alpine-lowland-level* 0.19d0
  "Where the rolling lowland sits, as a fraction of the world height.")

(defparameter *alpine-tree-line* 0.42d0
  "Above this fraction of the height, ground is bare stone.")

(defparameter *alpine-snow-line* 0.60d0
  "Above this fraction of the height, gentle ground carries snow.")

(defparameter *alpine-beach-level* 0.13d0
  "At or below this fraction of the height, ground is sand.")

(defun little-world-alpine-relief (source x z)
  "The alpine surface at X,Z as a fraction of the world height.

Rolling lowland carries mountain ranges wherever a broad mask says so.  The
peaks are ridged: one minus the magnitude of a noise reading folds its zero
crossings into sharp crest lines, and squaring keeps the flanks below them.
Narrow ravines cut down through everything along the crests of another
ridged reading, so cliffs and gorges appear in the plains as well."
  (let* ((lowland
           (+ *alpine-lowland-level*
              (* 0.050d0 (little-world-value-noise source x z 96 0))
              (* 0.028d0 (little-world-value-noise source x z 28 1))
              (* 0.012d0 (little-world-value-noise source x z 11 2))))
         (range
           (little-world-smoothstep
            0.08d0 0.42d0
            (+ (little-world-value-noise source x z 176 3)
               (* 0.35d0 (little-world-value-noise source x z 72 6)))))
         (ridge (- 1.0d0 (abs (little-world-value-noise source x z 44 4))))
         (crag (- 1.0d0 (abs (little-world-value-noise source x z 18 5))))
         (mountain
           (* range
              (+ 0.10d0
                 (* 0.42d0 ridge ridge)
                 (* 0.12d0 crag)
                 (* 0.03d0 (little-world-value-noise source x z 7 8)))))
         (ravine
           (* 0.12d0
              (little-world-smoothstep
               0.92d0 0.985d0
               (- 1.0d0
                  (abs (little-world-value-noise source x z 90 7)))))))
    (- (+ lowland mountain) ravine)))

(defun little-world-alpine-surface-height (source x z height)
  (max 2 (min (- height 3)
              (round (* height (little-world-alpine-relief source x z))))))

(defun little-world-alpine-surface-material (source x z surface height)
  (let* ((level (/ surface (coerce height 'double-float)))
         (moisture (little-world-value-noise source x z 52 17))
         (slope (little-world-surface-slope source x z surface height)))
    (cond ((>= slope 4) *slate-block*)
          ((and (>= level *alpine-snow-line*) (<= slope 2)) *snow-block*)
          ((>= level *alpine-tree-line*) *stone-block*)
          ((or (<= level *alpine-beach-level*) (< moisture -0.48d0))
           *sand-block*)
          ((>= slope 2) *stone-block*)
          ((> moisture 0.55d0) *moss-block*)
          (t *grass-block*))))

(defparameter *cave-tunnel-period* 26
  "Horizontal cell size of the two readings whose shared zero set is a
tunnel.")

(defparameter *cave-tunnel-vertical-period* 16
  "Vertical cell size of the tunnel readings; smaller than the horizontal
one so tunnels run more than they plunge.")

(defparameter *cave-cavern-period* 30
  "Cell size of the reading whose high values hollow out caverns.")

(defparameter *cave-cavern-threshold* 0.60d0
  "A cavern reading above this is open rock, once the cell is deep enough.")

(defparameter *cave-cavern-depth* 30
  "How far under the surface the cavern threshold has fully relaxed.

Nearer the surface the threshold climbs toward certainty, so shallow
lowland keeps its ground while the rock under a mountain hollows out.")

(defparameter *cave-crystal-chance* 28
  "One in this many cave floors or ceilings grows a crystal.")

(defun little-world-cave-p (source x y z surface)
  "Whether the alpine cell at X,Y,Z below SURFACE is open cave.

Tunnels are the cells near where two independent volume readings both cross
zero, which is a family of winding curves thickened to a few cells; their
radius wanders with a slow horizontal reading and narrows over the last
three cells below the surface, so only a tunnel's core breaks through into
daylight.  Caverns are the high values of a third reading against a threshold
that relaxes with depth, so they open up under mountains rather than under
the plains.  The bottom two layers are never carved."
  (and (>= y 2)
       (<= y surface)
       (let* ((a (little-world-volume-noise
                  source x y z
                  *cave-tunnel-period* *cave-tunnel-vertical-period* 40))
              (b (little-world-volume-noise
                  source x y z
                  *cave-tunnel-period* *cave-tunnel-vertical-period* 41))
              (narrowing (min 1.0d0 (/ (+ (- surface y) 1) 4.0d0)))
              (radius
                (* narrowing narrowing
                   (+ 0.020d0
                      (* 0.012d0
                         (little-world-value-noise source x z 40 43))))))
         (or (< (+ (* a a) (* b b)) radius)
             (let ((depth (- surface y 5)))
               (and (plusp depth)
                    (> (+ (little-world-volume-noise
                           source x y z
                           *cave-cavern-period* *cave-cavern-period* 44)
                          (* 0.35d0
                             (little-world-volume-noise
                              source x y z 12 12 45)))
                       (+ *cave-cavern-threshold*
                          (* 0.30d0
                             (- 1.0d0
                                (min 1.0d0
                                     (/ depth
                                        (coerce *cave-cavern-depth*
                                                'double-float)))))))))))))

(defun little-world-alpine-ground-height (source x z height)
  "The highest Y at X,Z that stays solid once caves are carved."
  (let ((surface (little-world-alpine-surface-height source x z height)))
    (loop for y from surface downto 2
          unless (little-world-cave-p source x y z surface)
            do (return y)
          finally (return 1))))

;;; Reading a relief.

(defun little-world-surface-height (source x z height)
  "The generated surface Y at X,Z before caves, in a world HEIGHT cells tall."
  (ecase (little-world-source-relief source)
    (:meadow (little-world-meadow-surface-height source x z height))
    (:alpine (little-world-alpine-surface-height source x z height))))

(defun little-world-surface-slope (source x z surface height)
  "The largest height difference between SURFACE and its four neighbors."
  (loop for (dx dz) in '((-1 0) (1 0) (0 -1) (0 1))
        maximize
        (abs (- surface
                (little-world-surface-height
                 source (+ x dx) (+ z dz) height)))))

(defun little-world-surface-material (source x z surface height)
  "Choose a visible biome material from height, moisture, and local slope."
  (ecase (little-world-source-relief source)
    (:meadow (little-world-meadow-surface-material source x z surface height))
    (:alpine (little-world-alpine-surface-material source x z surface height))))

(defun little-world-ground-height (source x z height)
  "The highest solid Y at X,Z once caves are carved: where a walker stands."
  (ecase (little-world-source-relief source)
    (:meadow (little-world-meadow-surface-height source x z height))
    (:alpine (little-world-alpine-ground-height source x z height))))

(defun little-world-spawn-player (world camera)
  "A new player standing on the generated ground under CAMERA's X,Z.

A fixed eye height suited the flat meadow; alpine ground under the same
X,Z may be a peak or a ravine floor.  Return NIL when WORLD is not read
from a little-world source."
  (let ((source (block-world-source world)))
    (when (typep source 'little-world-source)
      (let* ((shape (voxel-space-chunk-shape (block-world-space world)))
             (x (floor (camera-x camera)))
             (z (floor (camera-z camera)))
             (ground (little-world-ground-height
                      source x z (chunk-shape-height shape))))
        (make-instance 'block-world-player
                       :position (make-vec3 (+ x 0.5d0)
                                            (+ ground 1.1d0)
                                            (+ z 0.5d0)))))))

(defgeneric materialize-block-world-chunk
    (source world chunk-x chunk-y chunk-z))
(defgeneric populate-block-world-chunk
    (source world chunk-x chunk-y chunk-z))
(defgeneric apply-block-world-source-edits
    (source world chunk-x chunk-y chunk-z))
(defgeneric edit-block-world-source (source world block x y z))

(defun little-world-column-material (source y surface surface-material)
  "The block at depth Y in a column whose top is SURFACE-MATERIAL at SURFACE.

Stone underlies everything; the meadow's top three cells are dirt or sand.
Alpine columns whose surface is already rock or snow stay rock all the way
up, so cliffs and peaks are not capped with dirt."
  (cond ((= y surface) surface-material)
        ((or (zerop y) (< y (- surface 2))) *stone-block*)
        ((eq surface-material *sand-block*) *sand-block*)
        ((and (eq (little-world-source-relief source) :alpine)
              (member surface-material
                      (list *stone-block* *snow-block* *slate-block*)))
         *stone-block*)
        (t *dirt-block*)))

(defun materialize-little-world-chunk (source world chunk-x chunk-z)
  "Materialize one deterministic terrain chunk at vertical layer zero.

Every column is filled from the ground up, then, in the alpine relief,
carved from the bottom up: a carved cell over solid stone may grow a
crystal on that floor, and a solid cell over a carved one may hang a
crystal from that ceiling, so cave systems glow from within."
  (check-type source little-world-source)
  (with-world-change-transaction (world)
    (let* ((chunk (ensure-world-chunk world chunk-x 0 chunk-z))
           (shape (voxel-space-chunk-shape (block-world-space world)))
           (width (chunk-shape-width shape))
           (height (chunk-shape-height shape))
           (depth (chunk-shape-depth shape))
           (origin (chunk-domain-origin (block-chunk-domain chunk)))
           (caves-p (eq (little-world-source-relief source) :alpine)))
      (dotimes (local-z depth)
        (dotimes (local-x width)
          (let* ((x (+ (world-coordinate-x origin) local-x))
                 (z (+ (world-coordinate-z origin) local-z))
                 (surface (little-world-surface-height source x z height))
                 (surface-material
                   (little-world-surface-material
                    source x z surface height)))
            (flet ((offset (y)
                     (+ local-x (* width (+ y (* height local-z))))))
              (dotimes (y (1+ surface))
                (setf (chunk-block-at-offset chunk (offset y))
                      (little-world-column-material
                       source y surface surface-material)))
              (when caves-p
                (let ((below-carved-p nil))
                  (loop for y from 2 to surface
                        for carved-p = (little-world-cave-p
                                        source x y z surface)
                        for crystal-p = (zerop
                                         (mod (little-world-hash-3d
                                               source x y z 46)
                                              *cave-crystal-chance*))
                        do (cond (carved-p
                                  (setf (chunk-block-at-offset
                                         chunk (offset y))
                                        nil)
                                  (when (and crystal-p (not below-carved-p)
                                             (eq (chunk-block-at-offset
                                                  chunk (offset (1- y)))
                                                 *stone-block*))
                                    (setf (chunk-block-at-offset
                                           chunk (offset (1- y)))
                                          *crystal-block*)))
                                 ((and crystal-p below-carved-p
                                       (< y surface))
                                  (setf (chunk-block-at-offset
                                         chunk (offset y))
                                        *crystal-block*)))
                           (setf below-carved-p carved-p))))))))
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
      (setf (world-block-at world rock-x rock-y rock-z) *stone-block*)
      (when (zerop (mod hash 2))
        (setf (world-block-at world (1+ rock-x) rock-y rock-z) *stone-block*))
      (when (and (< (mod (ash hash -16) 5) 4)
                 (> (little-world-value-noise
                     source rock-x rock-z 48 29)
                    -0.35d0))
        (let* ((tree-x (+ origin-x 3 (mod (ash hash -3) (- width 6))))
               (tree-z (+ origin-z 3 (mod (ash hash -11) (- depth 6))))
               (surface (little-world-surface-height
                         source tree-x tree-z height))
               (surface-material
                 (little-world-surface-material
                  source tree-x tree-z surface height))
               (trunk-height (+ 3 (mod (ash hash -23) 2)))
               (crown (+ surface trunk-height)))
          (when (and (eq surface-material *grass-block*)
                     (< (+ crown 2) height))
            (loop for y from (1+ surface) to crown
                  do (setf (world-block-at world tree-x y tree-z) *wood-block*))
            ;; A broad, clipped lower crown and a small bright upper crown
            ;; make silhouettes much less like identical green boxes.
            (loop for x from (- tree-x 2) to (+ tree-x 2) do
              (loop for z from (- tree-z 2) to (+ tree-z 2)
                    when (<= (+ (abs (- x tree-x))
                                (abs (- z tree-z)))
                             3)
                      do (setf (world-block-at world x crown z) *leaf-block*)))
            (loop for x from (1- tree-x) to (1+ tree-x) do
              (loop for z from (1- tree-z) to (1+ tree-z)
                    do (setf (world-block-at world x (1+ crown) z)
                             *leaf-block*)))
            (setf (world-block-at world tree-x (+ crown 2) tree-z)
                  *leaf-block*)))))))

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
  (setf (world-block-at world x y z) block))

(defmethod edit-block-world-source
    ((source t) (world block-world) block x y z)
  (setf (world-block-at world x y z) block))

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

(defun block-chunk-key (chunk)
  (let ((coordinate
          (chunk-domain-coordinate (block-chunk-domain chunk))))
    (chunk-key (chunk-coordinate-x coordinate)
               (chunk-coordinate-y coordinate)
               (chunk-coordinate-z coordinate))))

(defun center-little-world-residency
    (source world center-x center-z &key (radius 4))
  "Materialize a square resident window and evict everything outside it.

Return the entering and leaving chunk-coordinate keys.  Entering chunks are
created as one staged transaction: establish all desired domains, materialize
terrain, populate landmarks, then replay sparse edits."
  (check-type source little-world-source)
  (check-type radius (integer 0))
  (let ((desired (make-hash-table :test #'equal))
        (entering nil)
        (leaving nil))
    (loop for chunk-x from (- center-x radius) to (+ center-x radius) do
      (loop for chunk-z from (- center-z radius) to (+ center-z radius)
            for key = (chunk-key chunk-x 0 chunk-z)
            do (setf (gethash key desired) t)
               (unless (nth-value 1
                                  (world-chunk-at world chunk-x 0 chunk-z))
                 (push key entering))))
    (dolist (chunk (resident-world-chunks world))
      (let ((key (block-chunk-key chunk)))
        (unless (gethash key desired)
          (push key leaving))))
    (setf entering (nreverse entering)
          leaving (nreverse leaving))
    (with-world-change-transaction (world)
      ;; Establish the whole neighborhood first so every later stage observes
      ;; resident air rather than confusing a not-yet-created neighbor with it.
      (maphash (lambda (key present-p)
                 (declare (ignore present-p))
                 (destructuring-bind (x y z) key
                   (ensure-world-chunk world x y z)))
               desired)
      (dolist (key entering)
        (destructuring-bind (x y z) key
          (materialize-block-world-chunk source world x y z)))
      (dolist (key entering)
        (destructuring-bind (x y z) key
          (populate-block-world-chunk source world x y z)))
      (dolist (key entering)
        (destructuring-bind (x y z) key
          (apply-block-world-source-edits source world x y z)))
      (dolist (key leaving)
        (destructuring-bind (x y z) key
          (remove-world-chunk world x y z))))
    (values entering leaving)))

(defun make-little-block-world (&key (chunk-radius 4)
                                     (chunk-width 16)
                                     (chunk-height *little-world-height*)
                                     (chunk-depth 16)
                                     (seed 121)
                                     (relief :alpine))
  "Make a deterministic square of resident terrain chunks and landmarks."
  (check-type chunk-radius (integer 0))
  (check-type chunk-width (integer 8))
  (check-type chunk-height (integer 8))
  (check-type chunk-depth (integer 8))
  (let* ((source (make-instance 'little-world-source
                                :seed seed :relief relief))
         (world (make-block-world :id (list :little-world seed)
                                  :chunk-width chunk-width
                                 :chunk-height chunk-height
                                 :chunk-depth chunk-depth
                                 :source source)))
    (center-little-world-residency source world 0 0 :radius chunk-radius)
    world))

(defun make-empty-little-block-world (&key (chunk-width 16)
                                           (chunk-height *little-world-height*)
                                           (chunk-depth 16)
                                           (seed 121)
                                           (relief :alpine))
  "Make a sourced block world whose resident set is initially empty."
  (make-block-world
   :id (list :little-world seed)
   :chunk-width chunk-width :chunk-height chunk-height :chunk-depth chunk-depth
   :source (make-instance 'little-world-source :seed seed :relief relief)))

(defun little-world-landmarks-for-chunk (source world key)
  "Capture deterministic landmarks whose owned sites lie inside KEY.

This retains the current visual generator while making chunk production
independent: cross-boundary canopy sites are attributed to their destination
chunk and do not require neighboring live mutation."
  (destructuring-bind (chunk-x chunk-y chunk-z) key
    (unless (zerop chunk-y)
      (return-from little-world-landmarks-for-chunk nil))
    (let* ((shape (voxel-space-chunk-shape (block-world-space world)))
           (width (chunk-shape-width shape))
           (height (chunk-shape-height shape))
           (depth (chunk-shape-depth shape))
           (minimum-x (* chunk-x width))
           (minimum-z (* chunk-z depth))
           (maximum-x (+ minimum-x width))
           (maximum-z (+ minimum-z depth))
           (landmarks nil))
      (flet ((emit (block x y z)
               (when (and (<= minimum-x x) (< x maximum-x)
                          (<= minimum-z z) (< z maximum-z)
                          (<= 0 y) (< y height))
                 (push (list block x y z) landmarks))))
        ;; A destination chunk can receive a two-cell canopy overhang from a
        ;; landmark rooted in an immediate neighbor.
        (loop for owner-x from (1- chunk-x) to (1+ chunk-x) do
          (loop for owner-z from (1- chunk-z) to (1+ chunk-z) do
            (let* ((origin-x (* owner-x width))
                   (origin-z (* owner-z depth))
                   (hash (little-world-hash source owner-x owner-z))
                   (rock-x (+ origin-x 2 (mod hash (- width 4))))
                   (rock-z (+ origin-z 2 (mod (ash hash -8) (- depth 4))))
                   (rock-y (1+ (little-world-surface-height
                                source rock-x rock-z height))))
              (emit *stone-block* rock-x rock-y rock-z)
              (when (zerop (mod hash 2))
                (emit *stone-block* (1+ rock-x) rock-y rock-z))
              (when (and (< (mod (ash hash -16) 5) 4)
                         (> (little-world-value-noise
                             source rock-x rock-z 48 29)
                            -0.35d0))
                (let* ((tree-x (+ origin-x 3
                                  (mod (ash hash -3) (- width 6))))
                       (tree-z (+ origin-z 3
                                  (mod (ash hash -11) (- depth 6))))
                       (surface (little-world-surface-height
                                 source tree-x tree-z height))
                       (surface-material
                         (little-world-surface-material
                          source tree-x tree-z surface height))
                       (trunk-height (+ 3 (mod (ash hash -23) 2)))
                       (crown (+ surface trunk-height)))
                  (when (and (eq surface-material *grass-block*)
                             (< (+ crown 2) height))
                    (loop for y from (1+ surface) to crown
                          do (emit *wood-block* tree-x y tree-z))
                    (loop for x from (- tree-x 2) to (+ tree-x 2) do
                      (loop for z from (- tree-z 2) to (+ tree-z 2)
                            when (<= (+ (abs (- x tree-x))
                                        (abs (- z tree-z)))
                                     3)
                              do (emit *leaf-block* x crown z)))
                    (loop for x from (1- tree-x) to (1+ tree-x) do
                      (loop for z from (1- tree-z) to (1+ tree-z)
                            do (emit *leaf-block* x (1+ crown) z)))
                    (emit *leaf-block* tree-x (+ crown 2) tree-z))))))))
      (nreverse landmarks))))
