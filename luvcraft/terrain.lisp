;;; The little world: deterministic terrain, landmarks, and recorded edits.
;;;
;;; A LITTLE-WORLD-SOURCE is a pure function of its seed.  Terrain heights and
;;; biome materials come from stable value noise, landmarks (rocks and trees)
;;; from coordinate hashes, and player edits from a sparse replayable overlay,
;;; so any chunk can be regenerated bit-identically on any thread.

(in-package #:luv)

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
          (+ 5.5d0
             (* 3.2d0 (little-world-value-noise source x z 64 0))
             (* 1.7d0 (little-world-value-noise source x z 28 1))
             (* 0.8d0 (little-world-value-noise source x z 11 2)))))
    (max 2 (min (- height 4) (round reading)))))

(defun little-world-surface-material (source x z surface height)
  "Choose a visible biome material from height, moisture, and local slope."
  (let* ((moisture (little-world-value-noise source x z 52 17))
         (slope
           (loop for (dx dz) in '((-1 0) (1 0) (0 -1) (0 1))
                 maximize
                 (abs (- surface
                         (little-world-surface-height
                          source (+ x dx) (+ z dz) height))))))
    (cond ((>= surface 9) *snow-block*)
          ((or (<= surface 3) (< moisture -0.48d0)) *sand-block*)
          ((>= slope 2) *stone-block*)
          (t *grass-block*))))

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
                 (surface (little-world-surface-height source x z height))
                 (surface-material
                   (little-world-surface-material
                    source x z surface height)))
            (dotimes (y (1+ surface))
              (setf (chunk-block-at-offset
                     chunk
                     (+ local-x (* width (+ y (* height local-z)))))
                    (cond ((= y surface) surface-material)
                          ((or (zerop y) (< y (- surface 2))) *stone-block*)
                          ((eq surface-material *sand-block*) *sand-block*)
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
    (center-little-world-residency source world 0 0 :radius chunk-radius)
    world))

(defun make-empty-little-block-world (&key (chunk-width 16)
                                           (chunk-height 16)
                                           (chunk-depth 16)
                                           (seed 121))
  "Make a sourced block world whose resident set is initially empty."
  (make-block-world
   :id (list :little-world seed)
   :chunk-width chunk-width :chunk-height chunk-height :chunk-depth chunk-depth
   :source (make-instance 'little-world-source :seed seed)))

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
