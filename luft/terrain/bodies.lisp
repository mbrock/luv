(in-package #:luft.terrain)

;;; Bodies are terrain queries: a box that collides with the fiber store,
;;; where it can stand, and the cells it can walk between.  Characters,
;;; controllers, and the world clock that move bodies belong to a game.

(defconstant +body-default-height+ 1.8)
(defconstant +walking-step-height+ 1)
(defconstant +walking-maximum-drop+ 2)
(defconstant +body-default-radius+ 0.3)
(defconstant +walking-collision-epsilon+ 1.0e-5)
(defparameter *walking-path-horizontal-radius* 80
  "Farthest horizontal cell distance admitted by one click-to-walk route.")
(defparameter *walking-path-vertical-radius* 16
  "Farthest vertical cell distance admitted by one click-to-walk route.")
(defparameter *walking-path-visit-limit* 20000
  "Maximum standable cells considered by one click-to-walk search.")

(defgeneric inspection-source-solid (source)
  (:documentation "Return SOURCE's fiber store for occupancy queries."))

(defmethod inspection-source-solid ((source t)) source)

(defmethod inspection-source-solid ((source scene)) (scene-solid source))

(defmacro with-collision-boundary (() &body body)
  "Run BODY with gameplay boundary policy: the finite horizontal box is a
wall, and a chunk that is not resident is air.

Meshing and other world clients retain explicit OUTSIDE-DOMAIN semantics.
Only physical occupants choose these restarts, so walkers slide along the
box and fall through nothing they cannot see."
  `(handler-bind
       ((luft:outside-domain
          (lambda (condition)
            (declare (ignore condition))
            (invoke-restart 'luft:treat-as-solid)))
        (luft:missing-chunk
          (lambda (condition)
            (declare (ignore condition))
            (invoke-restart 'luft:treat-as-air))))
     ,@body))

(defun collision-cell-occupancy-bit (solid x y z)
  "Read SOLID for gameplay under WITH-COLLISION-BOUNDARY."
  (with-collision-boundary ()
    (luft:fiber-store-cell-bit solid x y z)))

(defun inspection-cell-bit (solid x y z)
  "Read SOLID for picking and inspection: everything unknown is air."
  (handler-bind
      ((luft:outside-domain
         (lambda (condition)
           (declare (ignore condition))
           (invoke-restart 'luft:treat-as-air)))
       (luft:missing-chunk
         (lambda (condition)
           (declare (ignore condition))
           (invoke-restart 'luft:treat-as-air))))
    (luft:fiber-store-cell-bit solid x y z)))

(defclass physical-body ()
  ((height :initarg :height :initform +body-default-height+
           :type real
           :quantity (:quantity quantities:world-distance :unit quantities:cell)
           :accessor body-height)
   (radius :initarg :radius :initform +body-default-radius+ :type real
           :quantity (:quantity quantities:world-distance :unit quantities:cell)
           :accessor body-radius)
   (position :initarg :position :initform (make-vec3 0.0 0.0 0.0) :type vec3
             :quantity (:quantity quantities:world-position
                        :unit quantities:cell :tensor-order 1)
             :accessor body-position)
   (previous-position :initarg :position
                      :type vec3
                      :quantity (:quantity quantities:world-position
                                 :unit quantities:cell :tensor-order 1)
                      :accessor body-previous-position)
   ;; Tick endpoints are not the previous rendered frame's motion history.
   ;; Keep the current endpoint too, to recognize out-of-band pose edits.
   (previous-tick-position :type vec3
                           :quantity (:quantity quantities:world-position
                                      :unit quantities:cell :tensor-order 1)
                           :reader body-previous-tick-position)
   (tick-position :type vec3
                  :quantity (:quantity quantities:world-position
                             :unit quantities:cell :tensor-order 1)
                  :reader body-tick-position)
   (velocity :initarg :velocity :initform (make-vec3 0.0 0.0 0.0) :type vec3
             :quantity (:quantity quantities:world-velocity
                        :unit ((quantities:cell 1) (:second -1)) :tensor-order 1)
             :accessor body-velocity)
   (gravity :initarg :gravity :initform -24.0 :type real
            :quantity (:quantity quantities:world-acceleration
                       :unit ((quantities:cell 1) (:second -2)))
            :accessor body-gravity)
   (grounded-p :initarg :grounded-p :initform nil :accessor body-grounded-p))
  (:metaclass quantity-class)
  (:documentation "A terrain-colliding box, positioned at the centre of its feet."))

(defun body-overlaps-cell-p (body cell)
  "Whether CELL intersects BODY's physical box, including its edges."
  (let* ((position (body-position body))
         (radius (body-radius body))
         (base-z (vec3-z position))
         (cell-z (luft:site-z cell)))
    (and (< (- (vec3-x position) radius) (1+ (luft:site-x cell)))
         (> (+ (vec3-x position) radius) (luft:site-x cell))
         (< (- (vec3-y position) radius) (1+ (luft:site-y cell)))
         (> (+ (vec3-y position) radius) (luft:site-y cell))
         (< cell-z (+ base-z (body-height body)))
         (< base-z (1+ cell-z)))))

(defun reset-body-interpolation (body)
  "Cut both tick endpoints to BODY's physical pose, without changing frame history."
  (let ((position (body-position body)))
    (dolist (slot '(previous-tick-position tick-position))
      (unless (slot-boundp body slot)
        (setf (slot-value body slot) (make-vec3 0.0 0.0 0.0)))
      (let ((sample (slot-value body slot)))
        (setf (vec3-x sample) (vec3-x position)
              (vec3-y sample) (vec3-y position)
              (vec3-z sample) (vec3-z position)))))
  body)

(defmethod initialize-instance :after ((body physical-body) &key)
  ;; INITARG sharing above names the semantic initial value.  Temporal state
  ;; must own a distinct vector before either side is mutated.
  (let ((position (body-position body)))
    (setf (body-previous-position body)
          (make-vec3 (vec3-x position) (vec3-y position) (vec3-z position))))
  (reset-body-interpolation body))

(defmethod update-instance-for-redefined-class :after
    ((body physical-body) added-slots discarded-slots property-list &rest initargs)
  (declare (ignore added-slots discarded-slots property-list initargs))
  (unless (and (slot-boundp body 'previous-tick-position)
               (slot-boundp body 'tick-position))
    (reset-body-interpolation body)))

(defun body-clear-at-p
    (solid x y base-z &optional (height +body-default-height+)
                              (radius +body-default-radius+))
  "Whether the full body fits; touching a cell face is not penetration."
  (with-collision-boundary ()
    (loop for cell-x from (floor (+ (- x radius) +walking-collision-epsilon+))
            below (ceiling (- (+ x radius) +walking-collision-epsilon+))
          always
          (loop for cell-y from (floor (+ (- y radius) +walking-collision-epsilon+))
                  below (ceiling (- (+ y radius) +walking-collision-epsilon+))
                always (luft:fiber-store-column-clear-p
                        solid cell-x cell-y
                        (floor (+ base-z +walking-collision-epsilon+))
                        (ceiling (- (+ base-z height) +walking-collision-epsilon+)))))))

(defun sweep-body-axis (solid position height radius axis amount)
  "Clip an axis displacement against every voxel crossed by the body's box.

Return the allowed displacement and whether contact shortened it. Other axes
retain their positions, so successive sweeps slide along walls and corners."
  (let* ((low (make-vec3 (- (vec3-x position) radius)
                         (- (vec3-y position) radius)
                         (vec3-z position)))
         (high (make-vec3 (+ (vec3-x position) radius)
                          (+ (vec3-y position) radius)
                          (+ (vec3-z position) height)))
         (allowed amount))
    (flet ((start (a)
             (floor (+ (vec3-component low a)
                       (if (eq a axis) (min 0 amount) 0)
                       +walking-collision-epsilon+)))
           (end (a)
             (ceiling (- (+ (vec3-component high a)
                            (if (eq a axis) (max 0 amount) 0))
                         +walking-collision-epsilon+))))
      (loop for x from (start :x) below (end :x) do
        (loop for y from (start :y) below (end :y) do
          (loop for z from (start :z) below (end :z)
                when (= 1 (collision-cell-occupancy-bit solid x y z))
                  do (let ((cell (ecase axis (:x x) (:y y) (:z z))))
                       (cond
                         ((and (plusp amount)
                               (>= cell (- (vec3-component high axis)
                                           +walking-collision-epsilon+)))
                          (setf allowed
                                (min allowed (max 0 (- cell
                                                       (vec3-component high axis))))))
                         ((and (minusp amount)
                               (<= (1+ cell) (+ (vec3-component low axis)
                                                +walking-collision-epsilon+)))
                          (setf allowed
                                (max allowed (min 0 (- (1+ cell)
                                                       (vec3-component low axis))))))))))))
    (values allowed (/= allowed amount))))

(defun body-support-height (body source x y current-base-z)
  "Return a nearby supported base height for a step to X,Y, or NIL.

The controller can climb one cubical step and descend two.  It does not scan
for a remote roof, so a wall cannot teleport the player onto its top."
  (let* ((solid (inspection-source-solid source))
         (cell-x (floor x))
         (cell-y (floor y))
         (base (floor current-base-z)))
    (loop for support-z from (+ base (1- +walking-step-height+))
            downto (- base (1+ +walking-maximum-drop+))
          for candidate-base = (1+ support-z)
          when (and (= 1 (collision-cell-occupancy-bit
                          solid cell-x cell-y support-z))
                    (body-clear-at-p
                     solid x y candidate-base (body-height body) (body-radius body)))
            return (coerce candidate-base 'single-float))))

(defun body-standable-cell-p (body source x y z)
  "Whether the player can stand at the centre of the exact foot cell X,Y,Z."
  (let ((solid (inspection-source-solid source)))
    (and (= 1 (collision-cell-occupancy-bit solid x y (1- z)))
         (body-clear-at-p solid (+ x 0.5) (+ y 0.5) z
                          (body-height body) (body-radius body)))))

(defun nearby-walking-cell-p (cell start)
  (and (<= (max (abs (- (luft:site-x cell) (luft:site-x start)))
                (abs (- (luft:site-y cell) (luft:site-y start))))
           *walking-path-horizontal-radius*)
       (<= (abs (- (luft:site-z cell) (luft:site-z start)))
           *walking-path-vertical-radius*)))

(defun map-walking-cell-neighbors (function body source cell start)
  "Call FUNCTION with each nearby eight-connected neighbor and step cost."
  (let* ((solid (inspection-source-solid source))
         (domain (luft:fiber-store-domain solid))
         (x (luft:site-x cell))
         (y (luft:site-y cell))
         (z (luft:site-z cell)))
    (dolist (offset '((0 1) (1 0) (0 -1) (-1 0)
                       (1 1) (1 -1) (-1 -1) (-1 1)))
      (destructuring-bind (dx dy) offset
        (let* ((next-x (+ x dx))
               (next-y (+ y dy))
               (next-z
                 (body-support-height
                  body source (+ next-x 0.5) (+ next-y 0.5) z)))
          (when (and next-z
                     ;; A diagonal may pass beside a wall, but never through
                     ;; the point where two blocked cardinal cells meet.
                     (or (zerop dx) (zerop dy)
                         (and (body-support-height
                               body source (+ x dx 0.5) (+ y 0.5) z)
                              (body-support-height
                               body source (+ x 0.5) (+ y dy 0.5) z))))
            (handler-case
                (let ((next
                        (luft:make-site
                         domain next-x next-y (round next-z)
                         luft:+cell-extent+ 1)))
                  (when (nearby-walking-cell-p next start)
                    (funcall function next
                             (if (and (/= dx 0) (/= dy 0))
                                 (sqrt 2.0)
                                 1.0))))
              (luft:outside-domain () nil))))))))

(defun walking-route-octile-distance (from to)
  (let ((dx (abs (- (luft:site-x from) (luft:site-x to))))
        (dy (abs (- (luft:site-y from) (luft:site-y to)))))
    (+ (max dx dy) (* (1- (sqrt 2.0)) (min dx dy)))))

(defun walking-route-frontier-push (frontier priority cost cell)
  (vector-push-extend (list priority cost cell) frontier)
  (loop with child = (1- (length frontier))
        while (plusp child)
        for parent = (floor (1- child) 2)
        while (< priority (first (aref frontier parent)))
        do (rotatef (aref frontier child) (aref frontier parent))
           (setf child parent))
  frontier)

(defun walking-route-frontier-pop (frontier)
  (let ((minimum (aref frontier 0))
        (last (vector-pop frontier)))
    (when (plusp (length frontier))
      (setf (aref frontier 0) last)
      (loop with parent = 0
            for left = (1+ (* 2 parent))
            while (< left (length frontier))
            for right = (1+ left)
            for child = (if (and (< right (length frontier))
                                 (< (first (aref frontier right))
                                    (first (aref frontier left))))
                            right left)
            while (< (first (aref frontier child))
                     (first (aref frontier parent)))
            do (rotatef (aref frontier parent) (aref frontier child))
               (setf parent child)))
    minimum))

(defun reconstruct-walking-route (parents start destination)
  (let ((cells nil)
        (cell destination))
    (loop until (= cell start)
          do (push cell cells)
             (setf cell (gethash cell parents)))
    cells))

(defun search-walking-path (body source start destination)
  "Search a bounded, eight-connected octile path for BODY from START.

START and DESTINATION are packed LUFT foot cells.  Return the cells after
START, a status of :RUNNING, :ARRIVED, or :FAILED, a failure detail, and
the number of cells visited, so a refused destination stays inspectable."
  (flet ((result (cells status detail visits)
           (return-from search-walking-path
             (values cells status detail visits))))
    (unless (nearby-walking-cell-p destination start)
      (result nil :failed "destination is outside the local path window" 0))
    (unless (body-standable-cell-p
             body source (luft:site-x destination) (luft:site-y destination)
             (luft:site-z destination))
      (result nil :failed "destination is not a clear supported cell" 0))
    (when (= start destination)
      (result nil :arrived "already there" 0))
    (let ((parents (make-hash-table :test #'eql))
          (costs (make-hash-table :test #'eql))
          (frontier (make-array 64 :adjustable t :fill-pointer 0))
          (visits 0))
      (setf (gethash start costs) 0.0)
      (walking-route-frontier-push
       frontier (walking-route-octile-distance start destination) 0.0 start)
      (loop while (and (plusp (length frontier))
                       (< visits *walking-path-visit-limit*))
            for entry = (walking-route-frontier-pop frontier)
            for cost = (second entry)
            for cell = (third entry)
            when (= cost (gethash cell costs))
              do (incf visits)
                 (when (= cell destination)
                   (result (reconstruct-walking-route parents start destination)
                           :running nil visits))
                 (map-walking-cell-neighbors
                  (lambda (next step-cost)
                    (let ((next-cost (+ cost step-cost)))
                      (when (< next-cost
                               (gethash next costs
                                        most-positive-single-float))
                        (setf (gethash next costs) next-cost
                              (gethash next parents) cell)
                        (walking-route-frontier-push
                         frontier
                         (+ next-cost
                            (walking-route-octile-distance
                             next destination))
                         next-cost next))))
                  body source cell start))
      (result nil :failed "no local walkable path reaches the destination"
              visits))))
