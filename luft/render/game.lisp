(in-package #:luft.render)

;;; The small semantic game layer over LUFT's packed terrain.
;;;
;;; A player is an inspectable object at the application boundary.  Terrain
;;; queries stay direct packed-chain reads, and rendering consumes two dense
;;; vec4 lanes made from this state once per frame.

(defconstant +walking-player-height+ 2.9)
(defconstant +walking-player-step-height+ 1)
(defconstant +walking-player-maximum-drop+ 2)
(defconstant +walking-player-maximum-axis-substep+ 0.5)
(defconstant +walking-player-half-step+ 0.75)
(defconstant +walking-player-speed+ 7.0)
(defconstant +walking-player-gravity+ -24.0)
(defconstant +walking-player-jump-speed+ 9.0)
(defparameter *walking-path-horizontal-radius* 80
  "Farthest horizontal cell distance admitted by one click-to-walk route.")
(defparameter *walking-path-vertical-radius* 16
  "Farthest vertical cell distance admitted by one click-to-walk route.")
(defparameter *walking-path-visit-limit* 20000
  "Maximum standable cells considered by one click-to-walk search.")
(defparameter *walking-route-arrival-radius* 0.09
  "Horizontal distance from a cell centre which completes one waypoint.")
(defconstant +thrown-ball-speed+ 18.0)
(defconstant +thrown-ball-radius+ 0.32)

(defgeneric inspection-source-solid (source)
  (:documentation "Return SOURCE's packed solid chain for sparse queries."))

(defmethod inspection-source-solid ((source t)) source)

(defmethod inspection-source-solid ((source scene)) (scene-solid source))

(defun collision-cell-occupancy-bit (solid x y z)
  "Read SOLID for gameplay, treating its finite horizontal boundary as wall.

Meshing and other world clients retain explicit OUTSIDE-DOMAIN semantics.
Only physical occupants choose this restart, so walkers slide along the box
and projectile terrain publication materializes nearby boundary colliders."
  (handler-bind
      ((luft:outside-domain
         (lambda (condition)
           (declare (ignore condition))
           (invoke-restart 'luft:treat-as-solid))))
    (luft:chain-cell-occupancy-bit solid x y z)))

(defclass walking-player ()
  ((position :initarg :position :accessor walking-player-position)
   (previous-position :initarg :position
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
          :accessor walking-player-speed)
   (vertical-velocity :initform 0.0 :accessor walking-player-vertical-velocity)
   (grounded-p :initform t :accessor walking-player-grounded-p)
   (route :initform nil :accessor walking-player-route)
   (jump-requested-p :initform nil
                     :accessor walking-player-jump-requested-p)
   (spell-flash :initform 0.0 :accessor walking-player-spell-flash)
   (ball-position :initform nil :accessor walking-player-ball-position)
   (previous-ball-position :initform nil
                           :accessor walking-player-previous-ball-position)
   (physics :initform (luvcraft:make-physics-world :terrain nil)
            :reader walking-player-physics)
   (ball-handle :initform nil :accessor walking-player-ball-handle)
   (physics-clock :initform 0.0d0 :accessor walking-player-physics-clock))
  (:documentation
   "The continuous, player-owned state of LUFT's walking character.

POSITION is the centre of the character's feet.  Heading and gait are
semantic animation inputs; keys and shader clocks are deliberately absent."))

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

(defun walking-player-clear-at-p (solid x y base-z)
  "Whether the point-footprint player fits above BASE-Z at X,Y."
  (let ((cell-x (floor x))
        (cell-y (floor y)))
    (loop for z from (floor base-z)
          below (ceiling (+ base-z +walking-player-height+))
          always (zerop (collision-cell-occupancy-bit
                         solid cell-x cell-y z)))))

(defun walking-player-support-height (source x y current-base-z)
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
                     solid x y candidate-base))
            return (coerce candidate-base 'single-float))))

(defun walking-player-standable-cell-p (source x y z)
  "Whether the player can stand at the centre of the exact foot cell X,Y,Z."
  (let ((solid (inspection-source-solid source)))
    (and (= 1 (collision-cell-occupancy-bit solid x y (1- z)))
         (walking-player-clear-at-p solid (+ x 0.5) (+ y 0.5) z))))

(defun nearby-walking-cell-p (cell start)
  (and (<= (+ (abs (- (luft:site-x cell) (luft:site-x start)))
              (abs (- (luft:site-y cell) (luft:site-y start))))
           *walking-path-horizontal-radius*)
       (<= (abs (- (luft:site-z cell) (luft:site-z start)))
           *walking-path-vertical-radius*)))

(defun map-walking-cell-neighbors (function source cell start)
  "Call FUNCTION for four-connected, nearby standable neighbors of CELL."
  (let* ((solid (inspection-source-solid source))
         (domain (luft:chain-domain solid))
         (x (luft:site-x cell))
         (y (luft:site-y cell))
         (z (luft:site-z cell)))
    (dolist (offset '((0 1) (1 0) (0 -1) (-1 0)))
      (destructuring-bind (dx dy) offset
        (let* ((next-x (+ x dx))
               (next-y (+ y dy))
               (next-z
                 (walking-player-support-height
                  source (+ next-x 0.5) (+ next-y 0.5) z)))
          (when next-z
            (handler-case
                (let ((next
                        (luft:make-site
                         domain next-x next-y (round next-z)
                         luft:+cell-extent+ 1)))
                  (when (nearby-walking-cell-p next start)
                    (funcall function next)))
              (luft:outside-domain () nil))))))))

(defun reconstruct-walking-route (parents start destination)
  (let ((cells nil)
        (cell destination))
    (loop until (= cell start)
          do (push cell cells)
             (setf cell (gethash cell parents)))
    cells))

(defun find-walking-route (player source destination)
  "Return a bounded four-connected route from PLAYER to DESTINATION.

DESTINATION is a packed LUFT foot cell.  A returned failed route retains the
reason and search count, so a click that cannot be honored is inspectable
rather than silently becoming a straight-line collision attempt."
  (let* ((solid (inspection-source-solid source))
         (domain (luft:chain-domain solid))
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
            (seen (make-hash-table :test #'eql))
            (queue (make-array 64 :adjustable t :fill-pointer 0))
            (head 0)
            (visits 0))
        (setf (gethash start seen) t)
        (vector-push-extend start queue)
        (loop while (and (< head (length queue))
                         (< visits *walking-path-visit-limit*))
              for cell = (aref queue head)
              do (incf head)
                 (incf visits)
                 (map-walking-cell-neighbors
                  (lambda (next)
                    (unless (gethash next seen)
                      (setf (gethash next seen) t
                            (gethash next parents) cell)
                      (when (= next destination)
                        (return-from find-walking-route
                          (route
                           (reconstruct-walking-route
                            parents start destination)
                           :running nil visits)))
                      (vector-push-extend next queue)))
                  source cell start))
        (route nil :failed "no local walkable path reaches the destination"
               visits)))))

(defun start-walking-player-route (player source x y z)
  "Replace PLAYER's current intention with a route to foot cell X,Y,Z."
  (let* ((domain (luft:chain-domain (inspection-source-solid source)))
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
        (values (+ (* (cos yaw) direction-x) (* (sin yaw) direction-y))
                (- (* (sin yaw) direction-x) (* (cos yaw) direction-y))
                distance)))))

(defun try-walking-player-axis (player source axis amount)
  "Sweep PLAYER along one horizontal AXIS, sliding at blocked boundaries."
  (let* ((position (walking-player-position player))
         ;; No substep spans a whole terrain cell, so an endpoint beyond a
         ;; thin wall can never hide the occupied cell crossed to reach it.
         (step-count
           (max 1 (ceiling (/ (abs amount)
                              +walking-player-maximum-axis-substep+))))
         (step (/ amount step-count))
         (x (vec3:vec3-x position))
         (y (vec3:vec3-y position))
         (z (vec3:vec3-z position))
         (clear-p t))
    ;; Validate the whole axis attempt before publishing it.  Axis separation
    ;; therefore retains its atomic slide-at-a-wall behavior while every cell
    ;; crossed by a long attempt still participates in collision.
    (loop repeat step-count
          while clear-p
          do (incf x (if (eq axis :x) step 0.0))
             (incf y (if (eq axis :y) step 0.0))
             (let ((support (walking-player-support-height source x y z)))
               (if support
                   (setf z support)
                   (setf clear-p nil))))
    (when clear-p
      (setf (vec3:vec3-x position) x
            (vec3:vec3-y position) y
            (vec3:vec3-z position) z)
      t)))

(defun try-walking-player-air-axis (player source axis amount)
  "Sweep one airborne horizontal axis while retaining solid wall collision."
  (let* ((position (walking-player-position player))
         (solid (inspection-source-solid source))
         (step-count
           (max 1 (ceiling (/ (abs amount)
                              +walking-player-maximum-axis-substep+))))
         (step (/ amount step-count))
         (x (vec3:vec3-x position))
         (y (vec3:vec3-y position))
         (clear-p t))
    (loop repeat step-count
          while clear-p
          do (incf x (if (eq axis :x) step 0.0))
             (incf y (if (eq axis :y) step 0.0))
             (unless (walking-player-clear-at-p
                      solid x y (vec3:vec3-z position))
               (setf clear-p nil)))
    (when clear-p
      (setf (vec3:vec3-x position) x
            (vec3:vec3-y position) y)
      t)))

(defun request-walking-player-jump (player)
  "Request one grounded jump on PLAYER's next simulation step."
  (setf (walking-player-jump-requested-p player) t)
  player)

(defun cast-walking-player-spell (player)
  "Ignite the staff orb; the renderer consumes this short cast envelope."
  (setf (walking-player-spell-flash player) 1.0)
  player)

(defun throw-walking-player-ball (player origin direction)
  "Throw (or replace) PLAYER's ball from ORIGIN along DIRECTION."
  (let* ((position (add-scaled-directions origin direction 1.15))
         (physics (walking-player-physics player))
         (old (walking-player-ball-handle player)))
    (when (and old (luvcraft:physics-body-alive-p physics old))
      (luvcraft:destroy-physics-body physics old))
    (setf (walking-player-ball-position player) position
          (walking-player-previous-ball-position player)
          (vec3:make-vec3 (vec3:vec3-x position) (vec3:vec3-y position)
                          (vec3:vec3-z position))
          (walking-player-ball-handle player)
          (luvcraft:spawn-physics-body
           physics
           (vec3:vec3-x position) (vec3:vec3-z position)
           (vec3:vec3-y position)
           :radius +thrown-ball-radius+ :mass 0.85
           :vx (* +thrown-ball-speed+ (vec3:vec3-x direction))
           :vy (* +thrown-ball-speed+ (vec3:vec3-z direction))
           :vz (* +thrown-ball-speed+ (vec3:vec3-y direction))
           :restitution 0.72 :friction 0.58 :rolling-resistance 0.018
           :damping 0.035 :lifetime 8.0)
          (walking-player-spell-flash player) 1.0))
  player)

(defun post-walking-player-ball-terrain (player source)
  "Publish nearby LUFT cells to the Luvcraft solver as static boxes."
  (let* ((physics (walking-player-physics player))
         (position (walking-player-ball-position player))
         (solid (inspection-source-solid source)))
    (luvcraft:clear-physics-boxes physics)
    (when position
      (let ((cx (floor (vec3:vec3-x position)))
            (cy (floor (vec3:vec3-y position)))
            (cz (floor (vec3:vec3-z position))))
        (loop for x from (- cx 2) to (+ cx 2) do
          (loop for y from (- cy 2) to (+ cy 2) do
            (loop for z from (- cz 2) to (+ cz 2)
                  when (plusp (collision-cell-occupancy-bit solid x y z))
                    do (luvcraft:post-physics-box
                        physics x z y (1+ x) (1+ z) (1+ y))))))))
  player)

(defun sync-walking-player-ball (player)
  (let ((physics (walking-player-physics player))
        (handle (walking-player-ball-handle player)))
    (if (and handle (luvcraft:physics-body-alive-p physics handle))
        (multiple-value-bind (x z y)
            (luvcraft:physics-body-position physics handle)
          (let ((position (walking-player-ball-position player)))
            (setf (vec3:vec3-x position) x
                  (vec3:vec3-y position) y
                  (vec3:vec3-z position) z)))
        (setf (walking-player-ball-handle player) nil
              (walking-player-ball-position player) nil
              (walking-player-previous-ball-position player) nil)))
  player)

(defun advance-walking-player-ball (player source seconds)
  (let ((position (walking-player-ball-position player)))
    (when position
      (let ((previous (walking-player-previous-ball-position player)))
        (setf (vec3:vec3-x previous) (vec3:vec3-x position)
              (vec3:vec3-y previous) (vec3:vec3-y position)
              (vec3:vec3-z previous) (vec3:vec3-z position)))
      (incf (walking-player-physics-clock player) seconds)
      (loop repeat 3
            while (>= (walking-player-physics-clock player) (/ 1d0 60d0))
            do (decf (walking-player-physics-clock player) (/ 1d0 60d0))
               (post-walking-player-ball-terrain player source)
               (luvcraft:step-physics-world (walking-player-physics player)
                                            (/ 1.0 60.0)))
      (when (>= (walking-player-physics-clock player) (/ 1d0 60d0))
        (setf (walking-player-physics-clock player) 0d0))
      (sync-walking-player-ball player)))
  player)

(defun advance-walking-player-vertical (player source seconds)
  "Apply Luvcraft-strength gravity to LUFT's Z-up walking controller."
  (let* ((position (walking-player-position player))
         (jump-p (walking-player-jump-requested-p player)))
    (setf (walking-player-jump-requested-p player) nil)
    (when (and jump-p (walking-player-grounded-p player))
      (setf (walking-player-vertical-velocity player)
            +walking-player-jump-speed+
            (walking-player-grounded-p player) nil))
    (unless (walking-player-grounded-p player)
      (incf (walking-player-vertical-velocity player)
            (* +walking-player-gravity+ seconds))
      (incf (vec3:vec3-z position)
            (* (walking-player-vertical-velocity player) seconds)))
    (let ((support (walking-player-support-height
                    source (vec3:vec3-x position) (vec3:vec3-y position)
                    (vec3:vec3-z position))))
      (when (and support
                 (<= (walking-player-vertical-velocity player) 0.0)
                 (<= (vec3:vec3-z position) support))
        (setf (vec3:vec3-z position) support
              (walking-player-vertical-velocity player) 0.0
              (walking-player-grounded-p player) t))))
  player)

(defun advance-walking-player
    (player source camera forward right seconds &key maximum-distance)
  "Advance PLAYER from camera-relative movement axes for SECONDS."
  (begin-walking-player-frame player)
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
          (if (walking-player-grounded-p player)
              (progn
                (try-walking-player-axis player source :x (* direction-x distance))
                (try-walking-player-axis player source :y (* direction-y distance)))
              (progn
                (try-walking-player-air-axis
                 player source :x (* direction-x distance))
                (try-walking-player-air-axis
                 player source :y (* direction-y distance))))
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
  (advance-walking-player-ball player source seconds)
  (setf (walking-player-spell-flash player)
        (max 0.0 (- (walking-player-spell-flash player) (* 2.4 seconds))))
  player)

(defun follow-walking-player (camera player &key (distance 18.0) seconds)
  "Follow PLAYER with a soft look-ahead rather than a rigid camera weld."
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
           (camera-position (camera-position camera))
           (blend (if seconds (min 1.0 (* seconds 8.0)) 1.0)))
      (setf (vec3:vec3-x camera-position)
            (+ (vec3:vec3-x camera-position)
               (* blend (- target-x (vec3:vec3-x camera-position))))
            (vec3:vec3-y camera-position)
            (+ (vec3:vec3-y camera-position)
               (* blend (- target-y (vec3:vec3-y camera-position))))
            (vec3:vec3-z camera-position)
            (+ (vec3:vec3-z camera-position)
               (* blend (- target-z (vec3:vec3-z camera-position)))))))
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
     (let ((position (walking-player-ball-position player)))
       (if position
           (list (vec3:vec3-x position) (vec3:vec3-y position)
                 (vec3:vec3-z position) +thrown-ball-radius+)
           '(0.0 0.0 0.0 0.0)))
     (let ((position (walking-player-previous-ball-position player)))
       (if position
           (list (vec3:vec3-x position) (vec3:vec3-y position)
                 (vec3:vec3-z position) +thrown-ball-radius+)
           '(0.0 0.0 0.0 0.0))))))
