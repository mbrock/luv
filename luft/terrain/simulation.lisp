(in-package #:luft.terrain)

;;; Characters compose the terrain's bodies (bodies.lisp) with replaceable
;;; intentions.  One world clock steps every registered body, including
;;; unattended bodies.

(defconstant +walking-half-step+ 0.75)
(define-quantity-constant +walking-speed+ 7.0
  :type real
  :quantity (:quantity quantities:world-velocity
             :unit ((quantities:cell 1) (:second -1))))
(defparameter *walking-route-arrival-radius* 0.09
  "Horizontal distance from a cell centre which completes one waypoint.")

(defclass movement-controller () ()
  (:documentation "Supplies desire, never integrates or owns a body's physics."))

(defclass movement-intent (movement-controller)
  ((direction-x :initarg :direction-x :initform 0.0 :accessor intent-direction-x)
   (direction-y :initarg :direction-y :initform 0.0 :accessor intent-direction-y)
   (jump-requested-p :initform nil :accessor intent-jump-requested-p)
   ;; Only a flying character reads this: +1 rises, -1 sinks.
   (direction-z :initarg :direction-z :initform 0.0
                :accessor intent-direction-z)))

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
               :accessor character-jump-speed)
   ;; Creative flight: no gravity, vertical intent, collision as ever.
   (flying-p :initform nil :accessor character-flying-p))
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

(defun find-walking-route (player source destination)
  "Return PLAYER's walking route to DESTINATION, a packed LUFT foot cell.

A returned failed route retains the reason and search count, so a click that
cannot be honored is inspectable rather than silently becoming a
straight-line collision attempt."
  (let* ((solid (inspection-source-solid source))
         (body (character-body player))
         (position (body-position body))
         (start
           (luft:make-site
            (luft:fiber-store-domain solid) (floor (vec3-x position))
            (floor (vec3-y position))
            (floor (vec3-z position)) luft:+cell-extent+ 1)))
    (multiple-value-bind (cells status detail visits)
        (search-walking-path body source start destination)
      (make-instance 'walking-route
                     :start start :destination destination
                     :cells cells :status status :detail detail
                     :visits visits))))

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

(defparameter *flight-speed-factor* 2.0
  "How much faster a flying character crosses the world than it walks.")

(defparameter *flight-vertical-speed* 7.0
  "Cells per second a flying character rises or sinks at full urge.")

(defparameter *walking-gravity* -24.0
  "The gravity a character returns to when it lands from flight.")

(defun set-character-flying (character flying-p)
  "Take CHARACTER into creative flight or let it fall back to walking."
  (let ((body (character-body character)))
    (setf (character-flying-p character) (and flying-p t)
          (body-gravity body) (if flying-p 0.0 *walking-gravity*)
          (vec3-z (body-velocity body)) 0.0
          (body-grounded-p body) nil))
  (let ((controller (character-controller character)))
    (when (typep controller 'movement-intent)
      (setf (intent-direction-z controller) 0.0
            (intent-jump-requested-p controller) nil)))
  character)

(defun set-character-vertical-urge (character z)
  "Urge a flying CHARACTER up (+1), down (-1), or to hold height (0)."
  (setf (intent-direction-z (direct-character-intent character)) z)
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
      (when (and x (character-flying-p character))
        (setf x (* x *flight-speed-factor*)
              y (* y *flight-speed-factor*)))
      (when (character-flying-p character)
        ;; Flight steers height like position: toward the urged speed, with
        ;; the ground acceleration, and never by jumping.
        (let* ((controller (character-controller character))
               (target (* *flight-vertical-speed*
                          (if (typep controller 'movement-intent)
                              (intent-direction-z controller)
                              0.0)))
               (change (* seconds (character-ground-acceleration character)))
               (dz (- target (vec3-z velocity))))
          (incf (vec3-z velocity) (max (- change) (min change dz)))
          (when (plusp (vec3-z velocity))
            (setf (body-grounded-p body) nil))
          (setf jump nil)))
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
