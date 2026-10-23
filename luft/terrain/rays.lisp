(in-package #:luft.terrain)

;;; A ray is a terrain query, like a body's collision: any game that points
;;; into the world -- a click to walk, a pick, an inspector -- asks the same
;;; question of the cells and gets a face back.

(defun ray-axis-crossings (position direction)
  "Return grid STEP, first crossing distance, and crossing interval."
  (cond ((plusp direction)
         (values 1 (/ (- (1+ (floor position)) position) direction)
                 (/ direction)))
        ((minusp direction)
         (values -1 (/ (- position (floor position)) (- direction))
                 (/ (- direction))))
        (t (values 0 most-positive-single-float most-positive-single-float))))

(defun ray-entry-face (solid cell-x cell-y cell-z axis step)
  "Return the outward face through which a ray entered CELL along AXIS."
  (let* ((domain (luft:fiber-store-domain solid))
         (anchor-x (+ cell-x (if (and (eq axis :x) (minusp step)) 1 0)))
         (anchor-y (+ cell-y (if (and (eq axis :y) (minusp step)) 1 0)))
         (anchor-z (+ cell-z (if (and (eq axis :z) (minusp step)) 1 0)))
         (extent (ecase axis
                   (:x luft:+yz-face-extent+)
                   (:y luft:+xz-face-extent+)
                   (:z luft:+xy-face-extent+)))
         (geometry (luft:make-site domain anchor-x anchor-y anchor-z extent 1))
         (occupancy (lambda (x y z) (inspection-cell-bit solid x y z))))
    (luft:orient-face-outward domain geometry occupancy)))

(defun raycast-face (source origin direction &key (max-distance 600.0))
  "Follow a continuous ray from ORIGIN along DIRECTION into SOURCE's cells.

Return the first outward face it meets, the occupied cell's X, Y, and Z, the
point where it met the face, and the distance travelled, as six values; or
NIL when nothing solid lies within MAX-DISTANCE.  Tied edge and corner
crossings advance together, so a ray never reports a cell it merely touches.
Unknown cells -- outside the domain or not resident -- read as air."
  (let* ((solid (inspection-source-solid source))
         (direction (vec3-normalize direction))
         (x (floor (vec3-x origin)))
         (y (floor (vec3-y origin)))
         (z (floor (vec3-z origin)))
         step-x step-y step-z
         next-x next-y next-z
         delta-x delta-y delta-z
         (distance 0.0)
         (entry-axis nil)
         (entry-step 0))
    (multiple-value-setq (step-x next-x delta-x)
      (ray-axis-crossings (vec3-x origin) (vec3-x direction)))
    (multiple-value-setq (step-y next-y delta-y)
      (ray-axis-crossings (vec3-y origin) (vec3-y direction)))
    (multiple-value-setq (step-z next-z delta-z)
      (ray-axis-crossings (vec3-z origin) (vec3-z direction)))
    (loop
      (when (= 1 (inspection-cell-bit solid x y z))
        (when entry-axis
          (let ((face (ray-entry-face solid x y z entry-axis entry-step)))
            (when face
              (return
                (values face x y z
                        (make-vec3 (+ (vec3-x origin) (* distance (vec3-x direction)))
                                   (+ (vec3-y origin) (* distance (vec3-y direction)))
                                   (+ (vec3-z origin) (* distance (vec3-z direction))))
                        distance))))))
      (let ((next (min next-x next-y next-z)))
        (when (> next max-distance) (return nil))
        (setf distance next
              entry-axis nil)
        ;; Remember one carrier face for tied crossings, choosing the axis
        ;; most aligned with the ray.  All tied cells still advance together.
        (flet ((remember (axis step component)
                 (when (or (null entry-axis)
                           (> (abs component)
                              (abs (vec3-component direction entry-axis))))
                   (setf entry-axis axis entry-step step))))
          (when (<= next-x (+ next 1.0e-6))
            (incf x step-x)
            (incf next-x delta-x)
            (remember :x step-x (vec3-x direction)))
          (when (<= next-y (+ next 1.0e-6))
            (incf y step-y)
            (incf next-y delta-y)
            (remember :y step-y (vec3-y direction)))
          (when (<= next-z (+ next 1.0e-6))
            (incf z step-z)
            (incf next-z delta-z)
            (remember :z step-z (vec3-z direction))))))))

(defun raycast-standing-cell (source origin direction &key (max-distance 600.0))
  "Return the foot cell X, Y, and Z a ray names by striking an upward face.

A walking game's click: only the top of a solid cell promises somewhere to
stand, so a wall, an underside, or the sky answers NIL rather than a
surprising roof."
  (multiple-value-bind (face x y z)
      (raycast-face source origin direction :max-distance max-distance)
    (when (and face
               (= luft:+xy-face-extent+ (luft:site-extent face))
               (luft:site-positive-p face))
      (values x y (1+ z)))))
