(in-package #:luft.render)

;;; Bodies own physics; characters compose bodies with replaceable intentions.
;;; One world clock steps every registered body, including unattended bodies.

(defconstant +body-default-height+ 1.8)
(defconstant +walking-step-height+ 1)
(defconstant +walking-maximum-drop+ 2)
(defconstant +body-default-radius+ 0.3)
(defconstant +walking-collision-epsilon+ 1.0e-5)
(defconstant +walking-half-step+ 0.75)
(define-quantity-constant +walking-speed+ 7.0
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

(defclass movement-controller () ()
  (:documentation "Supplies desire, never integrates or owns a body's physics."))

(defclass movement-intent (movement-controller)
  ((direction-x :initarg :direction-x :initform 0.0 :accessor intent-direction-x)
   (direction-y :initarg :direction-y :initform 0.0 :accessor intent-direction-y)
   (jump-requested-p :initform nil :accessor intent-jump-requested-p)))

(defclass walking-character ()
  ((body :initarg :body :accessor character-body)
   (controller :initarg :controller :initform (make-instance 'movement-intent)
               :accessor character-controller)
   (heading-x :initarg :heading-x :initform 0.0
              :accessor character-heading-x)
   (heading-y :initarg :heading-y :initform 1.0
              :accessor character-heading-y)
   (previous-heading-x :initarg :heading-x :initform 0.0
                       :accessor character-previous-heading-x)
   (previous-heading-y :initarg :heading-y :initform 1.0
                       :accessor character-previous-heading-y)
   (gait :initform 0.0 :accessor character-gait)
   (previous-gait :initform 0.0 :accessor character-previous-gait)
   (speed :initarg :speed :initform +walking-speed+
          :type real
          :quantity (:quantity quantities:world-velocity
                     :unit ((quantities:cell 1) (:second -1)))
          :accessor character-speed)
   (ground-acceleration :initarg :ground-acceleration :initform 45.0 :type real
                        :quantity (:quantity quantities:world-acceleration
                                   :unit ((quantities:cell 1) (:second -2)))
                        :accessor character-ground-acceleration)
   (air-acceleration :initarg :air-acceleration :initform 14.0 :type real
                     :quantity (:quantity quantities:world-acceleration
                                :unit ((quantities:cell 1) (:second -2)))
                     :accessor character-air-acceleration)
   (jump-speed :initarg :jump-speed :initform 9.0 :type real
               :quantity (:quantity quantities:world-velocity
                          :unit ((quantities:cell 1) (:second -1)))
               :accessor character-jump-speed))
  (:metaclass quantity-class)
  (:documentation
   "Locomotion tuning and semantic animation, composed with a physical body."))

(define-lisp-arithmetic-function ballistic-displacement
    ((velocity :quantity quantities:world-velocity
               :unit ((quantities:cell 1) (:second -1)))
     (acceleration :quantity quantities:world-acceleration
                   :unit ((quantities:cell 1) (:second -2)))
     (elapsed :quantity quantities:elapsed-time :unit :second))
  ;; A signed displacement, not a nonnegative WORLD-DISTANCE or a point.
  (interpret
   (+ (* velocity elapsed) (* 0.5 acceleration elapsed elapsed))
   :quantity quantities:world-z-position :unit quantities:cell
   :character :difference))

(defparameter *walking-duration-declaration*
  (math:make-represented-value-declaration
   :representation-type 'real
   :quantity-specification
   (math:make-declared-quantity-specification
    '(:quantity quantities:elapsed-time :unit :second))))

(defparameter *walking-displacement-realization*
  (make-lisp-arithmetic-realization
   'ballistic-displacement :parameter-representation-types '(real real real)
   :result-representation-type 'real))

(defparameter *body-vertical-velocity-declaration*
  ;; A homogeneous vector's component has the same units, but scalar rank.
  ;; Never bind the scalar ballistic equation to the vec3 storage declaration.
  (let ((quantity (math:declaration-quantity-specification
                   (record-slot-declaration 'physical-body 'velocity))))
    (assert (= 1 (math:quantity-specification-tensor-order quantity)))
    (math:make-represented-value-declaration
     :representation-type 'real
     :quantity-specification
     (math:make-declared-quantity-specification
      (list :quantity (math:quantity-specification-name quantity)
            :unit (math:quantity-specification-unit quantity) :tensor-order 0)))))

(defparameter *walking-displacement-function*
  (bind-lisp-arithmetic-realization
   *walking-displacement-realization*
   (list *body-vertical-velocity-declaration*
         (record-slot-declaration 'physical-body 'gravity)
         *walking-duration-declaration*)))

(defclass walking-route (movement-controller)
  ((start :initarg :start :reader walking-route-start)
   (destination :initarg :destination :reader walking-route-destination)
   (cells :initarg :cells :accessor walking-route-cells)
   (status :initarg :status :initform :running
           :accessor walking-route-status)
   (detail :initarg :detail :initform nil :accessor walking-route-detail)
   (visits :initarg :visits :initform 0 :reader walking-route-visits))
  (:documentation
   "One inspectable discrete intention realized by a continuous character.

START, DESTINATION, and CELLS are packed LUFT cell sites at the character's
foot height.  The route owns no duplicate terrain field; collision remains
authoritative while the character crosses between its cell-centre waypoints."))

(defmethod initialize-instance :after ((body physical-body) &key)
  ;; INITARG sharing above names the semantic initial value.  Temporal state
  ;; must own a distinct vector before either side is mutated.
  (let ((position (body-position body)))
    (setf (body-previous-position body)
          (make-vec3 (vec3-x position) (vec3-y position) (vec3-z position)))))

(defun make-walking-character
    (&key
       (position (make-vec3 0.0 0.0 0.0))
       (heading-x 0.0) (heading-y 1.0) (speed +walking-speed+)
       body (controller (make-instance 'movement-intent)))
  "Make a character, with an independent body unless BODY is supplied."
  (make-instance 'walking-character
                 :body (or body (make-instance 'physical-body :position position))
                 :controller controller :heading-x heading-x :heading-y heading-y
                 :speed speed))

(defun begin-character-frame (player)
  "Retain PLAYER's exact pre-step pose for temporal rendering."
  (setf (character-previous-heading-x player) (character-heading-x player)
        (character-previous-heading-y player) (character-heading-y player)
        (character-previous-gait player) (character-gait player))
  player)

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

(defun find-walking-route (player source destination)
  "Return a bounded, eight-connected octile route from PLAYER to DESTINATION.

DESTINATION is a packed LUFT foot cell.  A returned failed route retains the
reason and search count, so a click that cannot be honored is inspectable
rather than silently becoming a straight-line collision attempt."
  (let* ((solid (inspection-source-solid source))
         (domain (luft:fiber-store-domain solid))
         (body (character-body player))
         (position (body-position body))
         (start
           (luft:make-site
            domain (floor (vec3-x position))
            (floor (vec3-y position))
            (floor (vec3-z position)) luft:+cell-extent+ 1)))
    (labels ((route (cells status detail visits)
               (make-instance 'walking-route
                              :start start :destination destination
                              :cells cells :status status :detail detail
                              :visits visits)))
      (unless (nearby-walking-cell-p destination start)
        (return-from find-walking-route
          (route nil :failed "destination is outside the local path window" 0)))
      (unless (body-standable-cell-p
               body source (luft:site-x destination) (luft:site-y destination)
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
                    body source cell start))
        (route nil :failed "no local walkable path reaches the destination"
               visits)))))

(defun start-character-route (player source x y z)
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
    (cancel-character-route player "replaced by another route")
    (setf (character-controller player) route)
    route))

(defun cancel-character-route (player &optional (detail "manual movement"))
  "Stop route desire, retaining the inspectable cancelled controller.
SET-CHARACTER-MOVEMENT or REQUEST-CHARACTER-JUMP selects direct intent."
  (let ((route (character-controller player)))
    (when (and (typep route 'walking-route)
               (eq :running (walking-route-status route)))
      (setf (walking-route-status route) :cancelled
            (walking-route-detail route) detail)))
  player)

(defun character-reached-route-cell-p (player cell)
  (let* ((position (body-position (character-body player)))
         (dx (- (+ (luft:site-x cell) 0.5) (vec3-x position)))
         (dy (- (+ (luft:site-y cell) 0.5) (vec3-y position)))
         (horizontal-distance (sqrt (+ (* dx dx) (* dy dy)))))
    (and (< horizontal-distance *walking-route-arrival-radius*)
         (< (abs (- (luft:site-z cell) (vec3-z position))) 0.16))))

(defun trim-character-route (player)
  (let ((route (character-controller player)))
    (when (and (typep route 'walking-route)
               (eq :running (walking-route-status route)))
      (loop while (and (walking-route-cells route)
                       (character-reached-route-cell-p
                        player (first (walking-route-cells route))))
            do (pop (walking-route-cells route)))
      (unless (walking-route-cells route)
        (setf (walking-route-status route) :arrived
              (walking-route-detail route) "destination reached"))))
  player)

(defgeneric controller-desire (controller character seconds)
  (:documentation "Return desired horizontal velocity X,Y and a jump request.
Called only on fixed simulation ticks. NIL means no steering, not braking."))

(defmethod controller-desire ((controller null) character seconds)
  (declare (ignore character seconds))
  (values nil nil nil))

(defmethod controller-desire ((controller movement-intent) character seconds)
  (declare (ignore seconds))
  (let* ((x (intent-direction-x controller))
         (y (intent-direction-y controller))
         (scale (/ (character-speed character) (max 1.0 (sqrt (+ (* x x) (* y y))))))
         (jump (intent-jump-requested-p controller)))
    ;; An airborne request remains queued until supported. Loading and zero
    ;; duration never call this method, so cannot lose the request either.
    (when (body-grounded-p (character-body character))
      (setf (intent-jump-requested-p controller) nil))
    (values (* x scale) (* y scale) jump)))

(defmethod controller-desire ((route walking-route) character seconds)
  (trim-character-route character)
  (unless (eq :running (walking-route-status route))
    (return-from controller-desire (values 0.0 0.0 nil)))
  (let* ((cell (first (walking-route-cells route)))
         (body (character-body character))
         (position (body-position body))
         (dx (- (+ (luft:site-x cell) 0.5) (vec3-x position)))
         (dy (- (+ (luft:site-y cell) 0.5) (vec3-y position)))
         (distance (sqrt (+ (* dx dx) (* dy dy))))
         (acceleration (if (body-grounded-p body)
                           (character-ground-acceleration character)
                           (character-air-acceleration character)))
         ;; Approach slowly enough to brake, including one tick of latency.
         ;; Proportional arrival also suppresses repeated waypoint overshoot.
         (speed (min (character-speed character) (* 4 distance)
                     (/ distance seconds)
                     (max 0.0 (- (sqrt (* 2 acceleration distance))
                                 (* acceleration seconds)))))
         (scale (/ speed (max distance 1.0e-6))))
    (values (* dx scale) (* dy scale)
            (> (luft:site-z cell) (+ (vec3-z position) 0.1)))))

(defun direct-character-intent (character)
  (cancel-character-route character)
  (unless (typep (character-controller character) 'movement-intent)
    (setf (character-controller character) (make-instance 'movement-intent)))
  (character-controller character))

(defun set-character-movement (character x y)
  (let ((intent (direct-character-intent character)))
    (setf (intent-direction-x intent) x (intent-direction-y intent) y))
  character)

(defun request-character-jump (character)
  (setf (intent-jump-requested-p (direct-character-intent character)) t)
  character)

(defun refresh-body-support (body solid)
  (let ((position (body-position body)))
    (setf (body-grounded-p body)
          (and (<= (vec3-z (body-velocity body)) 0)
               ;; A falling body must actually sweep into contact, not stop
               ;; a probe's width above the floor. Resting authored spawns
               ;; and already grounded bodies may establish support here.
               (or (body-grounded-p body)
                   (zerop (vec3-z (body-velocity body))))
               (<= (body-gravity body) 0)
               (not (body-clear-at-p
                     solid (vec3-x position) (vec3-y position)
                     (- (vec3-z position) 0.0001)
                     (body-height body) (body-radius body)))))))

(defun steer-character (character seconds)
  (let* ((body (character-body character))
         (velocity (body-velocity body)))
    (multiple-value-bind (x y jump)
        (controller-desire (character-controller character) character seconds)
      (when x
        (let* ((dx (- x (vec3-x velocity)))
               (dy (- y (vec3-y velocity)))
               (distance (sqrt (+ (* dx dx) (* dy dy))))
               (change (* seconds (if (body-grounded-p body)
                                      (character-ground-acceleration character)
                                      (character-air-acceleration character))))
               (scale (min 1.0 (/ change (max distance 1.0e-9)))))
          (incf (vec3-x velocity) (* dx scale))
          (incf (vec3-y velocity) (* dy scale))))
      (when (and jump (body-grounded-p body))
        (setf (vec3-z velocity) (character-jump-speed character)
              (body-grounded-p body) nil)))))

(defun move-body-axis (body solid axis amount)
  "Sweep, retain tangential velocity, and zero only the blocked component."
  (multiple-value-bind (travel blocked)
      (sweep-body-axis solid (body-position body) (body-height body)
                       (body-radius body) axis amount)
    (ecase axis
      (:x (incf (vec3-x (body-position body)) travel))
      (:y (incf (vec3-y (body-position body)) travel))
      (:z (incf (vec3-z (body-position body)) travel)))
    (when blocked
      (ecase axis
        (:x (setf (vec3-x (body-velocity body)) 0.0))
        (:y (setf (vec3-y (body-velocity body)) 0.0))
        (:z (setf (vec3-z (body-velocity body)) 0.0
                  (body-grounded-p body) (minusp amount)))))
    (values travel blocked)))

(defun integrate-body (body solid seconds)
  (let ((velocity (body-velocity body)))
    (move-body-axis body solid :x (* (vec3-x velocity) seconds))
    (move-body-axis body solid :y (* (vec3-y velocity) seconds))
    ;; Check again after horizontal motion: only the trailing edge may still
    ;; touch a ledge. Floor edits are authoritative even for idle occupants.
    (refresh-body-support body solid)
    (if (body-grounded-p body)
        (setf (vec3-z velocity) 0.0)
        (let ((distance (funcall *walking-displacement-function*
                                 (vec3-z velocity) (body-gravity body) seconds)))
          (incf (vec3-z velocity) (* (body-gravity body) seconds))
          (move-body-axis body solid :z distance)))))

(defun update-character-travel (character x y seconds)
  (let* ((position (body-position (character-body character)))
         (dx (- (vec3-x position) x))
         (dy (- (vec3-y position) y))
         (distance (sqrt (+ (* dx dx) (* dy dy)))))
    (if (plusp distance)
        (progn
          (setf (character-heading-x character) (/ dx distance)
                (character-heading-y character) (/ dy distance))
          (incf (character-gait character) (* pi (/ distance +walking-half-step+))))
        (let* ((gait (character-gait character))
               (difference (- (* pi (round (/ gait pi))) gait))
               (change (* 9.0 seconds)))
          (incf (character-gait character) (max (- change) (min change difference))))))
  (trim-character-route character))

(defclass world-simulation ()
  ((source :initarg :source :accessor simulation-source)
   (bodies :initform nil :reader simulation-bodies)
   (characters :initform nil :reader simulation-characters)
   (clock :initform 0 :reader simulation-clock)
   (accumulator :initform 0d0 :reader simulation-accumulator))
  (:documentation
   "Owns terrain, physical occupants, and one clock, independently of a viewer.
Characters supply locomotion; bodies without characters are equally physical."))

(defun make-world-simulation (source)
  (make-instance 'world-simulation :source source))

(defun add-simulation-body (simulation body)
  (pushnew body (slot-value simulation 'bodies) :test #'eq)
  body)

(defun add-simulation-character (simulation character)
  ;; One locomotion authority per body, rather than order-dependent steering.
  (assert (not (find (character-body character) (simulation-characters simulation)
                     :test (lambda (body other)
                             (and (not (eq other character))
                                  (eq body (character-body other)))))))
  (pushnew character (slot-value simulation 'characters) :test #'eq)
  (add-simulation-body simulation (character-body character))
  character)

(defun remove-simulation-character (simulation character)
  (when (member character (simulation-characters simulation) :test #'eq)
    (setf (slot-value simulation 'characters)
          (remove character (simulation-characters simulation) :test #'eq)
          (slot-value simulation 'bodies)
          (remove (character-body character) (simulation-bodies simulation) :test #'eq)))
  character)

(defun advance-world-simulation (simulation seconds)
  "Capture one frame history, then run whole 1/120-second world ticks.
Unspent time is retained; initial authored collision loading time is discarded."
  (check-type seconds (real 0 *))
  (dolist (body (simulation-bodies simulation))
    (let ((position (body-position body)) (previous (body-previous-position body)))
      (setf (vec3-x previous) (vec3-x position)
            (vec3-y previous) (vec3-y position)
            (vec3-z previous) (vec3-z position))))
  (mapc #'begin-character-frame (simulation-characters simulation))
  (let ((source (simulation-source simulation)))
    (when (and (typep source 'streaming-scene)
               (streaming-scene-source source)
               (zerop (luft:fiber-store-count (scene-solid source))))
      (setf (slot-value simulation 'accumulator) 0d0)
      (return-from advance-world-simulation simulation))
    (incf (slot-value simulation 'accumulator) (coerce seconds 'double-float))
    (let ((solid (inspection-source-solid source)) (dt (/ 1d0 120)))
      ;; Tolerate only floating-point dust at exact tick boundaries, not a
      ;; partial physics step. Keep arbitrary frame durations in bounded
      ;; double storage rather than accumulating unbounded rational products.
      (loop while (>= (+ (simulation-accumulator simulation) 1d-12) dt) do
        (let ((poses (mapcar (lambda (character)
                              (let ((p (body-position (character-body character))))
                                (list character (vec3-x p) (vec3-y p))))
                            (simulation-characters simulation))))
          (dolist (body (simulation-bodies simulation)) (refresh-body-support body solid))
          (dolist (character (simulation-characters simulation)) (steer-character character dt))
          (dolist (body (simulation-bodies simulation)) (integrate-body body solid dt))
          (dolist (pose poses) (apply #'update-character-travel (append pose (list dt)))))
        (decf (slot-value simulation 'accumulator) dt)
        (incf (slot-value simulation 'clock) 1/120))
      (when (< (abs (simulation-accumulator simulation)) 1d-12)
        (setf (slot-value simulation 'accumulator) 0d0))))
  simulation)
