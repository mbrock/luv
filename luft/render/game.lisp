(in-package #:luft.render)

;;; The small semantic game layer over LUFT's packed terrain.
;;;
;;; A player is an inspectable object at the application boundary.  Terrain
;;; queries read chunk fibers, and rendering consumes two dense
;;; vec4 lanes made from this state once per frame.

(defconstant +walking-player-height+ 2.9)
(defconstant +walking-player-step-height+ 1)
(defconstant +walking-player-maximum-drop+ 2)
(defconstant +walking-player-radius+ 0.3)
(defconstant +walking-collision-epsilon+ 1.0e-5)
(defconstant +walking-player-half-step+ 0.75)
(luv.arithmetic:define-quantity-constant +walking-player-speed+ 7.0
  :type real
  :quantity (:quantity quantities:world-velocity
             :unit ((quantities:cell 1) (:second -1))))
(luv.arithmetic:define-quantity-constant +walking-player-gravity+ -24.0
  :type real
  :quantity (:quantity quantities:world-acceleration
             :unit ((quantities:cell 1) (:second -2))))
(luv.arithmetic:define-quantity-constant +walking-player-jump-speed+ 9.0
  :type real
  :quantity (:quantity quantities:world-velocity
             :unit ((quantities:cell 1) (:second -1))))
(defparameter *walking-path-horizontal-radius* 80
  "Farthest horizontal cell distance admitted by one click-to-walk route.")
(defparameter *walking-path-vertical-radius* 16
  "Farthest vertical cell distance admitted by one click-to-walk route.")
(defparameter *walking-path-visit-limit* 20000
  "Maximum standable cells considered by one click-to-walk search.")
(defparameter *walking-route-arrival-radius* 0.09
  "Horizontal distance from a cell centre which completes one waypoint.")
(defconstant +fireball-speed+ 23.0)
(defconstant +fireball-radius+ 0.38)
(defconstant +fireball-cast-angle+ 0.68)

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

(defclass walking-player ()
  ((height :initarg :height :initform +walking-player-height+
           :type real
           :quantity (:quantity quantities:world-distance :unit quantities:cell)
           :accessor walking-player-height)
   (position :initarg :position :type vec3:vec3
             :quantity (:quantity quantities:world-position
                        :unit quantities:cell :tensor-order 1)
             :accessor walking-player-position)
   (previous-position :initarg :position
                      :type vec3:vec3
                      :quantity (:quantity quantities:world-position
                                 :unit quantities:cell :tensor-order 1)
                      :accessor walking-player-previous-position)
   (heading-x :initarg :heading-x :initform 0.0
              :accessor walking-player-heading-x)
   (heading-y :initarg :heading-y :initform 1.0
              :accessor walking-player-heading-y)
   (previous-heading-x :initarg :heading-x :initform 0.0
                       :accessor walking-player-previous-heading-x)
   (previous-heading-y :initarg :heading-y :initform 1.0
                       :accessor walking-player-previous-heading-y)
   (gait :initform 0.0 :accessor walking-player-gait)
   (previous-gait :initform 0.0 :accessor walking-player-previous-gait)
   (speed :initarg :speed :initform +walking-player-speed+
          :type real
          :quantity (:quantity quantities:world-velocity
                     :unit ((quantities:cell 1) (:second -1)))
          :accessor walking-player-speed)
   (vertical-velocity :initform 0.0 :type real
                      :quantity (:quantity quantities:world-velocity
                                 :unit ((quantities:cell 1) (:second -1)))
                      :accessor walking-player-vertical-velocity)
   (grounded-p :initform t :accessor walking-player-grounded-p)
   (route :initform nil :accessor walking-player-route)
   (jump-requested-p :initform nil
                     :accessor walking-player-jump-requested-p)
   (spell-flash :initform 0.0 :accessor walking-player-spell-flash)
   (fireball-position :initform nil
                      :accessor walking-player-fireball-position)
   (previous-fireball-position
    :initform nil :accessor walking-player-previous-fireball-position)
   (fireball-velocity :initform nil
                      :accessor walking-player-fireball-velocity)
   (fireball-distance-remaining
    :initform 0.0 :accessor walking-player-fireball-distance-remaining))
  (:metaclass luv.arithmetic.records:quantity-class)
  (:documentation
   "The continuous, player-owned state of LUFT's walking character.

POSITION is the centre of the character's feet.  Heading and gait are
semantic animation inputs; keys and shader clocks are deliberately absent."))

(luv.arithmetic.lisp:define-lisp-arithmetic-function ballistic-displacement
    ((velocity :quantity quantities:world-velocity
               :unit ((quantities:cell 1) (:second -1)))
     (acceleration :quantity quantities:world-acceleration
                   :unit ((quantities:cell 1) (:second -2)))
     (elapsed :quantity quantities:elapsed-time :unit :second))
  ;; A signed displacement, not a nonnegative WORLD-DISTANCE or a point.
  (luv.arithmetic.language:interpret
   (+ (* velocity elapsed) (* 0.5 acceleration elapsed elapsed))
   :quantity quantities:world-z-position :unit quantities:cell
   :character :difference))

(defparameter *walking-duration-declaration*
  (luv.arithmetic:make-represented-value-declaration
   :representation-type 'real
   :quantity-specification
   (luv.arithmetic:make-declared-quantity-specification
    '(:quantity quantities:elapsed-time :unit :second))))

(defparameter *walking-displacement-realization*
  (luv.arithmetic.lisp:make-lisp-arithmetic-realization
   'ballistic-displacement :parameter-representation-types '(real real real)
   :result-representation-type 'real))

(defparameter *walking-displacement-function*
  (luv.arithmetic.lisp:bind-lisp-arithmetic-realization
   *walking-displacement-realization*
   (list (luv.arithmetic.records:record-slot-declaration
          'walking-player 'vertical-velocity)
         (luv.arithmetic:value-declaration-for '+walking-player-gravity+)
         *walking-duration-declaration*)))

(defclass walking-route ()
  ((start :initarg :start :reader walking-route-start)
   (destination :initarg :destination :reader walking-route-destination)
   (cells :initarg :cells :accessor walking-route-cells)
   (status :initarg :status :initform :running
           :accessor walking-route-status)
   (detail :initarg :detail :initform nil :accessor walking-route-detail)
   (visits :initarg :visits :initform 0 :reader walking-route-visits))
  (:documentation
   "One inspectable discrete intention realized by the continuous player.

START, DESTINATION, and CELLS are packed LUFT cell sites at the character's
foot height.  The route owns no duplicate terrain field; collision remains
authoritative while the player crosses between its cell-centre waypoints."))

(defmethod initialize-instance :after ((player walking-player) &key)
  ;; INITARG sharing above names the semantic initial value.  Temporal state
  ;; must own a distinct vector before either side is mutated.
  (let ((position (walking-player-position player)))
    (setf (walking-player-previous-position player)
          (vec3:make-vec3 (vec3:vec3-x position)
                          (vec3:vec3-y position)
                          (vec3:vec3-z position)))))

(defun make-walking-player
    (&key
       (position
         (vec3:make-vec3 (+ +sanctuary-origin-x+ 29.5)
                         (+ +sanctuary-origin-y+ 24.5) 14.0))
       (heading-x 0.0) (heading-y 1.0) (speed +walking-player-speed+))
  "Make the sanctuary player at the bridge's authored starting point."
  (make-instance 'walking-player :position position
                                 :heading-x heading-x :heading-y heading-y
                                 :speed speed))

(defun begin-walking-player-frame (player)
  "Retain PLAYER's exact pre-step pose for temporal rendering."
  (let ((position (walking-player-position player))
        (previous (walking-player-previous-position player)))
    (setf (vec3:vec3-x previous) (vec3:vec3-x position)
          (vec3:vec3-y previous) (vec3:vec3-y position)
          (vec3:vec3-z previous) (vec3:vec3-z position)
          (walking-player-previous-heading-x player)
          (walking-player-heading-x player)
          (walking-player-previous-heading-y player)
          (walking-player-heading-y player)
          (walking-player-previous-gait player) (walking-player-gait player)))
  player)

(defun walking-player-clear-at-p
    (solid x y base-z &optional (height +walking-player-height+)
                              (radius +walking-player-radius+))
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

(defun sweep-walking-body-axis (solid position height radius axis amount)
  "Clip an axis displacement against every voxel crossed by the body's box.

Return the allowed displacement and whether contact shortened it. Other axes
retain their positions, so successive sweeps slide along walls and corners."
  (let* ((low (vec3:make-vec3 (- (vec3:vec3-x position) radius)
                              (- (vec3:vec3-y position) radius)
                              (vec3:vec3-z position)))
         (high (vec3:make-vec3 (+ (vec3:vec3-x position) radius)
                               (+ (vec3:vec3-y position) radius)
                               (+ (vec3:vec3-z position) height)))
         (allowed amount))
    (flet ((start (a)
             (floor (+ (vec3:vec3-component low a)
                       (if (eq a axis) (min 0 amount) 0)
                       +walking-collision-epsilon+)))
           (end (a)
             (ceiling (- (+ (vec3:vec3-component high a)
                            (if (eq a axis) (max 0 amount) 0))
                         +walking-collision-epsilon+))))
      (loop for x from (start :x) below (end :x) do
        (loop for y from (start :y) below (end :y) do
          (loop for z from (start :z) below (end :z)
                when (= 1 (collision-cell-occupancy-bit solid x y z))
                  do (let ((cell (ecase axis (:x x) (:y y) (:z z))))
                       (cond
                         ((and (plusp amount)
                               (>= cell (- (vec3:vec3-component high axis)
                                           +walking-collision-epsilon+)))
                          (setf allowed
                                (min allowed (max 0 (- cell
                                                       (vec3:vec3-component high axis))))))
                         ((and (minusp amount)
                               (<= (1+ cell) (+ (vec3:vec3-component low axis)
                                                +walking-collision-epsilon+)))
                          (setf allowed
                                (max allowed (min 0 (- (1+ cell)
                                                       (vec3:vec3-component low axis))))))))))))
    (values allowed (/= allowed amount))))

(defun walking-player-support-height
    (source x y current-base-z &optional (height +walking-player-height+))
  "Return a nearby supported base height for a step to X,Y, or NIL.

The controller can climb one cubical step and descend two.  It does not scan
for a remote roof, so a wall cannot teleport the player onto its top."
  (let* ((solid (inspection-source-solid source))
         (cell-x (floor x))
         (cell-y (floor y))
         (base (floor current-base-z)))
    (loop for support-z from (+ base (1- +walking-player-step-height+))
            downto (- base (1+ +walking-player-maximum-drop+))
          for candidate-base = (1+ support-z)
          when (and (= 1 (collision-cell-occupancy-bit
                          solid cell-x cell-y support-z))
                    (walking-player-clear-at-p
                     solid x y candidate-base height))
            return (coerce candidate-base 'single-float))))

(defun walking-player-standable-cell-p (source x y z)
  "Whether the player can stand at the centre of the exact foot cell X,Y,Z."
  (let ((solid (inspection-source-solid source)))
    (and (= 1 (collision-cell-occupancy-bit solid x y (1- z)))
         (walking-player-clear-at-p solid (+ x 0.5) (+ y 0.5) z))))

(defun nearby-walking-cell-p (cell start)
  (and (<= (max (abs (- (luft:site-x cell) (luft:site-x start)))
                (abs (- (luft:site-y cell) (luft:site-y start))))
           *walking-path-horizontal-radius*)
       (<= (abs (- (luft:site-z cell) (luft:site-z start)))
           *walking-path-vertical-radius*)))

(defun map-walking-cell-neighbors (function source cell start)
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
                 (walking-player-support-height
                  source (+ next-x 0.5) (+ next-y 0.5) z)))
          (when (and next-z
                     ;; A diagonal may pass beside a wall, but never through
                     ;; the point where two blocked cardinal cells meet.
                     (or (zerop dx) (zerop dy)
                         (and (walking-player-support-height
                               source (+ x dx 0.5) (+ y 0.5) z)
                              (walking-player-support-height
                               source (+ x 0.5) (+ y dy 0.5) z))))
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

(defun find-walking-route (player source destination)
  "Return a bounded, eight-connected octile route from PLAYER to DESTINATION.

DESTINATION is a packed LUFT foot cell.  A returned failed route retains the
reason and search count, so a click that cannot be honored is inspectable
rather than silently becoming a straight-line collision attempt."
  (let* ((solid (inspection-source-solid source))
         (domain (luft:fiber-store-domain solid))
         (position (walking-player-position player))
         (start
           (luft:make-site
            domain (floor (vec3:vec3-x position))
            (floor (vec3:vec3-y position))
            (floor (vec3:vec3-z position)) luft:+cell-extent+ 1)))
    (labels ((route (cells status detail visits)
               (make-instance 'walking-route
                              :start start :destination destination
                              :cells cells :status status :detail detail
                              :visits visits)))
      (unless (nearby-walking-cell-p destination start)
        (return-from find-walking-route
          (route nil :failed "destination is outside the local path window" 0)))
      (unless (walking-player-standable-cell-p
               source (luft:site-x destination) (luft:site-y destination)
               (luft:site-z destination))
        (return-from find-walking-route
          (route nil :failed "destination is not a clear supported cell" 0)))
      (when (= start destination)
        (return-from find-walking-route
          (route nil :arrived "already there" 0)))
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
                     (return-from find-walking-route
                       (route
                        (reconstruct-walking-route parents start destination)
                        :running nil visits)))
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
                    source cell start))
        (route nil :failed "no local walkable path reaches the destination"
               visits)))))

(defun start-walking-player-route (player source x y z)
  "Replace PLAYER's current intention with a route to foot cell X,Y,Z."
  (let* ((domain (luft:fiber-store-domain (inspection-source-solid source)))
         (destination
           (handler-case
               (luft:make-site domain x y z luft:+cell-extent+ 1)
             (luft:outside-domain () nil)))
         (route
           (if destination
               (find-walking-route player source destination)
               (make-instance
                'walking-route :start nil :destination nil :cells nil
                :status :failed :detail "destination is outside the world"))))
    (setf (walking-player-route player) route)
    route))

(defun cancel-walking-player-route (player &optional (detail "manual movement"))
  "Return movement authority to direct input, retaining the cancelled route."
  (let ((route (walking-player-route player)))
    (when (and route (eq :running (walking-route-status route)))
      (setf (walking-route-status route) :cancelled
            (walking-route-detail route) detail)))
  player)

(defun walking-player-reached-route-cell-p (player cell)
  (let* ((position (walking-player-position player))
         (dx (- (+ (luft:site-x cell) 0.5) (vec3:vec3-x position)))
         (dy (- (+ (luft:site-y cell) 0.5) (vec3:vec3-y position)))
         (horizontal-distance (sqrt (+ (* dx dx) (* dy dy)))))
    (and (< horizontal-distance *walking-route-arrival-radius*)
         (< (abs (- (luft:site-z cell) (vec3:vec3-z position))) 0.16))))

(defun trim-walking-player-route (player)
  (let ((route (walking-player-route player)))
    (when (and route (eq :running (walking-route-status route)))
      (loop while (and (walking-route-cells route)
                       (walking-player-reached-route-cell-p
                        player (first (walking-route-cells route))))
            do (pop (walking-route-cells route)))
      (unless (walking-route-cells route)
        (setf (walking-route-status route) :arrived
              (walking-route-detail route) "destination reached"))))
  player)

(defun walking-player-route-control (player camera)
  "Return camera-relative axes and remaining distance for PLAYER's route."
  (trim-walking-player-route player)
  (let ((route (walking-player-route player)))
    (when (and route (eq :running (walking-route-status route)))
      (let* ((cell (first (walking-route-cells route)))
             (position (walking-player-position player))
             (dx (- (+ (luft:site-x cell) 0.5) (vec3:vec3-x position)))
             (dy (- (+ (luft:site-y cell) 0.5) (vec3:vec3-y position)))
             (distance (sqrt (+ (* dx dx) (* dy dy))))
             (direction-x (/ dx (max distance 1.0e-6)))
             (direction-y (/ dy (max distance 1.0e-6)))
             (yaw (camera-yaw camera)))
        ;; Routes express a desired step; the same jump/gravity controller
        ;; must actually get there instead of snapping to the waypoint height.
        (when (and (walking-player-grounded-p player)
                   (> (luft:site-z cell) (+ (vec3:vec3-z position) 0.1)))
          (request-walking-player-jump player))
        (values (+ (* (cos yaw) direction-x) (* (sin yaw) direction-y))
                (- (* (sin yaw) direction-x) (* (cos yaw) direction-y))
                distance)))))

(defun try-walking-player-axis (player source axis amount)
  "Move up to horizontal contact without changing the player's foot height."
  (let ((position (walking-player-position player)))
    (multiple-value-bind (travel blocked-p)
        (sweep-walking-body-axis
         (inspection-source-solid source) position (walking-player-height player)
         +walking-player-radius+ axis amount)
      (ecase axis
        (:x (incf (vec3:vec3-x position) travel))
        (:y (incf (vec3:vec3-y position) travel)))
      (not blocked-p))))

(defun request-walking-player-jump (player)
  "Request one grounded jump on PLAYER's next simulation step."
  (setf (walking-player-jump-requested-p player) t)
  player)

(defun walking-player-staff-head-position (player)
  "Return the world-space head of PLAYER's staff in its full casting pose."
  (let* ((player-position (walking-player-position player))
         (heading-x (walking-player-heading-x player))
         (heading-y (walking-player-heading-y player))
         (right-x heading-y)
         (right-y (- heading-x))
         ;; The shader pivots the staff around the gripping hand.  Mirror its
         ;; authored head here so the fireball belongs to that visible cast.
         (head-height (- 3.34 1.505))
         (forward (+ 0.195 (* (sin +fireball-cast-angle+) head-height)))
         (height (+ 1.505 (* (cos +fireball-cast-angle+) head-height))))
    (vec3:make-vec3
     (+ (vec3:vec3-x player-position)
        (* right-x 0.640) (* heading-x forward))
     (+ (vec3:vec3-y player-position)
        (* right-y 0.640) (* heading-y forward))
     (+ (vec3:vec3-z player-position) height))))

(defun launch-walking-player-fireball
    (player origin direction &key (distance 64.0))
  "Launch (or replace) PLAYER's fireball from ORIGIN along DIRECTION."
  (let ((position (add-scaled-directions origin direction 0.48)))
    (setf (walking-player-fireball-position player) position
          (walking-player-previous-fireball-position player)
          (vec3:make-vec3 (vec3:vec3-x position) (vec3:vec3-y position)
                          (vec3:vec3-z position))
          (walking-player-fireball-velocity player)
          (vec3:vec3-scale direction +fireball-speed+)
          (walking-player-fireball-distance-remaining player)
          (max 0.0 (- distance 0.48))
          (walking-player-spell-flash player) 1.0))
  player)

(defun cast-walking-player-fireball (player target)
  "Turn PLAYER toward TARGET and launch a fireball from the pivoted staff.

This is intentionally the small semantic boundary for the tech demo.  Input,
rendering, and the temporary kinematic transport do not need to know what a
future spell or weapon action vocabulary will look like."
  (let* ((position (walking-player-position player))
         (dx (- (vec3:vec3-x target) (vec3:vec3-x position)))
         (dy (- (vec3:vec3-y target) (vec3:vec3-y position)))
         (horizontal-distance (sqrt (+ (* dx dx) (* dy dy)))))
    (when (> horizontal-distance 1.0e-4)
      (setf (walking-player-heading-x player) (/ dx horizontal-distance)
            (walking-player-heading-y player) (/ dy horizontal-distance)))
    (cancel-walking-player-route player "casting a fireball")
    (let* ((origin (walking-player-staff-head-position player))
           (direction
             (vec3:make-vec3 (- (vec3:vec3-x target) (vec3:vec3-x origin))
                             (- (vec3:vec3-y target) (vec3:vec3-y origin))
                             (- (vec3:vec3-z target)
                                (vec3:vec3-z origin))))
           (length (vec3:vec3-length direction)))
      ;; A click directly on the staff is still a cast, aimed along the new
      ;; facing instead of asking VEC3-NORMALIZE to invent a direction.
      (when (< length 1.0e-4)
        (setf direction
              (vec3:make-vec3 (walking-player-heading-x player)
                              (walking-player-heading-y player) 0.0)
              length 32.0))
      (launch-walking-player-fireball
       player origin (vec3:vec3-normalize direction) :distance length))))

(defun clear-walking-player-fireball (player)
  (setf (walking-player-fireball-position player) nil
        (walking-player-previous-fireball-position player) nil
        (walking-player-fireball-velocity player) nil
        (walking-player-fireball-distance-remaining player) 0.0)
  player)

(defun advance-walking-player-fireball (player seconds)
  "Advance PLAYER's fireball in a straight line, stopping at its cast target."
  (let ((position (walking-player-fireball-position player)))
    (when position
      (let ((previous (walking-player-previous-fireball-position player)))
        (setf (vec3:vec3-x previous) (vec3:vec3-x position)
              (vec3:vec3-y previous) (vec3:vec3-y position)
              (vec3:vec3-z previous) (vec3:vec3-z position)))
      (let* ((velocity (walking-player-fireball-velocity player))
             (remaining
               (walking-player-fireball-distance-remaining player))
             (travel (min remaining (* +fireball-speed+ seconds)))
             (time (/ travel +fireball-speed+)))
        (incf (vec3:vec3-x position) (* (vec3:vec3-x velocity) time))
        (incf (vec3:vec3-y position) (* (vec3:vec3-y velocity) time))
        (incf (vec3:vec3-z position) (* (vec3:vec3-z velocity) time))
        (decf (walking-player-fireball-distance-remaining player) travel)
        (when (<= (walking-player-fireball-distance-remaining player) 0.0)
          (clear-walking-player-fireball player)))))
  player)

(defun advance-walking-player-vertical (player source seconds)
  "Integrate gravity and sweep the whole body to the first floor or ceiling."
  (let* ((position (walking-player-position player))
         (solid (inspection-source-solid source))
         (height (walking-player-height player))
         (jump-p (walking-player-jump-requested-p player)))
    (setf (walking-player-jump-requested-p player) nil)
    (when (and (walking-player-grounded-p player)
               (walking-player-clear-at-p
                solid (vec3:vec3-x position) (vec3:vec3-y position)
                (- (vec3:vec3-z position) 0.0001) height))
      (setf (walking-player-grounded-p player) nil))
    (when (and jump-p (walking-player-grounded-p player))
      (setf (walking-player-vertical-velocity player) +walking-player-jump-speed+
            (walking-player-grounded-p player) nil))
    (unless (walking-player-grounded-p player)
      (let* ((velocity (walking-player-vertical-velocity player))
             (distance (funcall *walking-displacement-function*
                                velocity +walking-player-gravity+ seconds)))
        (incf (walking-player-vertical-velocity player)
              (* +walking-player-gravity+ seconds))
        (multiple-value-bind (travel blocked-p)
            (sweep-walking-body-axis solid position height +walking-player-radius+
                                     :z distance)
          (incf (vec3:vec3-z position) travel)
          (when blocked-p
            (setf (walking-player-vertical-velocity player) 0.0
                  (walking-player-grounded-p player) (minusp distance)))))))
  player)

(defun step-walking-player
    (player source camera forward right seconds &key maximum-distance)
  "Advance PLAYER from camera-relative movement axes for SECONDS."
  (let ((length (sqrt (+ (* forward forward) (* right right)))))
    (if (plusp length)
        (let* ((forward (/ forward length))
               (right (/ right length))
               (yaw (camera-yaw camera))
               (direction-x (+ (* (cos yaw) forward) (* (sin yaw) right)))
               (direction-y (+ (* (sin yaw) forward) (* (- (cos yaw)) right)))
               (distance (min (* seconds (walking-player-speed player))
                              (or maximum-distance most-positive-single-float)))
               (position (walking-player-position player))
               (before-x (vec3:vec3-x position))
               (before-y (vec3:vec3-y position)))
          (try-walking-player-axis player source :x (* direction-x distance))
          (try-walking-player-axis player source :y (* direction-y distance))
          (let* ((dx (- (vec3:vec3-x position) before-x))
                 (dy (- (vec3:vec3-y position) before-y))
                 (travelled (sqrt (+ (* dx dx) (* dy dy)))))
            (when (plusp travelled)
              (setf (walking-player-heading-x player) (/ dx travelled)
                    (walking-player-heading-y player) (/ dy travelled))
              ;; The shader's authored half-step is 0.75 cells.  Advancing
              ;; gait from actual travelled distance keeps planted feet in
              ;; world space even when collision shortens a requested move.
              (incf (walking-player-gait player)
                    (* pi (/ travelled +walking-player-half-step+))))))
        ;; A released key owes the character a planted-foot pose rather than
        ;; leaving one boot suspended forever at an arbitrary sampled phase.
        (let* ((gait (walking-player-gait player))
               (target (* pi (round (/ gait pi))))
               (difference (- target gait))
               (maximum-change (* 9.0 seconds)))
          (incf (walking-player-gait player)
                (max (- maximum-change)
                     (min maximum-change difference))))))
  (advance-walking-player-vertical player source seconds)
  player)

(defun advance-walking-player
    (player source camera forward right seconds &key maximum-distance)
  "Integrate movement in at most 1/120-second steps, retaining one frame pose."
  (begin-walking-player-frame player)
  ;; A demand world starts with no published collision window. Missing chunks
  ;; normally mean air, but applying that policy before initial publication
  ;; drops the player through the authored spawn while its road is loading.
  ;; Do not accumulate this time or consume queued input. Static empty scenes
  ;; still simulate normally, and resident empty chunks really do mean air.
  (when (and (typep source 'streaming-scene)
             (streaming-scene-source source)
             (zerop (luft:fiber-store-count (scene-solid source))))
    (return-from advance-walking-player player))
  (let* ((steps (max 1 (ceiling (* seconds 120))))
         (dt (/ seconds steps)))
    (dotimes (i steps)
      (step-walking-player player source camera forward right dt
                           :maximum-distance (and maximum-distance
                                                  (/ maximum-distance steps)))))
  (advance-walking-player-fireball player seconds)
  (setf (walking-player-spell-flash player)
        (max 0.0 (- (walking-player-spell-flash player) (* 2.4 seconds))))
  player)

(defun soft-follow-step (current target seconds quiet-radius)
  "Move CURRENT calmly toward TARGET, catching up harder as separation grows.

QUIET-RADIUS is a soft camera zone: motion inside it is treated as local
character movement rather than a reason to reframe.  Outside it, an exact
exponential response makes the result independent of frame rate, while its
rate rises with distance so large discontinuities do not leave the player
behind."
  (let* ((difference (- target current))
         (distance (abs difference))
         (excess (max 0.0 (- distance quiet-radius))))
    (if (zerop excess)
        current
        (let* ((rate (+ 1.8 (* 0.9 excess)))
               (blend (- 1.0 (exp (- (* rate seconds))))))
          (+ current (* (signum difference) excess blend))))))

(defun follow-walking-player (camera player &key (distance 18.0) seconds)
  "Follow PLAYER through a quiet zone with distance-sensitive catch-up."
  (multiple-value-bind (right up forward) (camera-basis camera)
    (declare (ignore right up))
    (let* ((player-position (walking-player-position player))
           (heading-x (walking-player-heading-x player))
           (heading-y (walking-player-heading-y player))
           ;; Showing more of where the traveler is going makes movement legible.
           (aim-x (+ (vec3:vec3-x player-position) (* 2.4 heading-x)))
           (aim-y (+ (vec3:vec3-y player-position) (* 2.4 heading-y)))
           (aim-z (+ (vec3:vec3-z player-position) 1.45))
           (target-x (- aim-x (* distance (vec3:vec3-x forward))))
           (target-y (- aim-y (* distance (vec3:vec3-y forward))))
           (target-z (- aim-z (* distance (vec3:vec3-z forward))))
           (camera-position (camera-position camera)))
      (if (null seconds)
          (setf (vec3:vec3-x camera-position) target-x
                (vec3:vec3-y camera-position) target-y
                (vec3:vec3-z camera-position) target-z)
          (setf (vec3:vec3-x camera-position)
                (soft-follow-step (vec3:vec3-x camera-position)
                                  target-x seconds 0.28)
                (vec3:vec3-y camera-position)
                (soft-follow-step (vec3:vec3-y camera-position)
                                  target-y seconds 0.28)
                ;; Terrain relief is much less important than lateral travel:
                ;; let a whole stair tread pass without bobbing the frame.
                (vec3:vec3-z camera-position)
                (soft-follow-step (vec3:vec3-z camera-position)
                                  target-z seconds 0.85)))))
  camera)

(defun walking-player-render-lanes (player)
  "Return current, previous, and heading vec4 lanes for the GPU boundary."
  (labels ((position-lane (position gait)
             (list (vec3:vec3-x position) (vec3:vec3-y position)
                   (+ (vec3:vec3-z position) 1.48) gait)))
    (values
     (position-lane (walking-player-position player)
                    (walking-player-gait player))
     (position-lane (walking-player-previous-position player)
                    (walking-player-previous-gait player))
     (list (walking-player-heading-x player)
           (walking-player-heading-y player)
           (walking-player-previous-heading-x player)
           (walking-player-spell-flash player))
     (let ((position (walking-player-fireball-position player)))
       (if position
           (list (vec3:vec3-x position) (vec3:vec3-y position)
                 (vec3:vec3-z position) +fireball-radius+)
           '(0.0 0.0 0.0 0.0)))
     (let ((position (walking-player-previous-fireball-position player)))
       (if position
           (list (vec3:vec3-x position) (vec3:vec3-y position)
                 (vec3:vec3-z position) +fireball-radius+)
           '(0.0 0.0 0.0 0.0))))))
