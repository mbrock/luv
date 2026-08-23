(in-package #:luft.render)

;;; The small semantic game layer over LUFT's packed terrain.
;;;
;;; A player is an inspectable object at the application boundary.  Terrain
;;; queries stay direct packed-chain reads, and rendering consumes two dense
;;; vec4 lanes made from this state once per frame.

(defconstant +walking-player-height+ 2.9)
(defconstant +walking-player-step-height+ 1)
(defconstant +walking-player-maximum-drop+ 2)
(defconstant +walking-player-half-step+ 0.75)
(defconstant +walking-player-speed+ 7.0)
(defconstant +walking-player-gravity+ -24.0)
(defconstant +walking-player-jump-speed+ 9.0)

(defgeneric inspection-source-solid (source)
  (:documentation "Return SOURCE's packed solid chain for sparse queries."))

(defmethod inspection-source-solid ((source t)) source)

(defmethod inspection-source-solid ((source scene)) (scene-solid source))

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
   (jump-requested-p :initform nil
                     :accessor walking-player-jump-requested-p)
   (spell-flash :initform 0.0 :accessor walking-player-spell-flash))
  (:documentation
   "The continuous, player-owned state of LUFT's walking character.

POSITION is the centre of the character's feet.  Heading and gait are
semantic animation inputs; keys and shader clocks are deliberately absent."))

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
          always (zerop (luft:chain-cell-occupancy-bit
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
          when (and (= 1 (luft:chain-cell-occupancy-bit
                          solid cell-x cell-y support-z))
                    (walking-player-clear-at-p
                     solid x y candidate-base))
            return (coerce candidate-base 'single-float))))

(defun try-walking-player-axis (player source axis amount)
  "Move PLAYER along one horizontal AXIS, sliding at blocked boundaries."
  (let* ((position (walking-player-position player))
         (x (+ (vec3:vec3-x position) (if (eq axis :x) amount 0.0)))
         (y (+ (vec3:vec3-y position) (if (eq axis :y) amount 0.0)))
         (support
           (walking-player-support-height
            source x y (vec3:vec3-z position))))
    (when support
      (setf (vec3:vec3-x position) x
            (vec3:vec3-y position) y
            (vec3:vec3-z position) support)
      t)))

(defun request-walking-player-jump (player)
  "Request one grounded jump on PLAYER's next simulation step."
  (setf (walking-player-jump-requested-p player) t)
  player)

(defun cast-walking-player-spell (player)
  "Ignite the staff orb; the renderer consumes this short cast envelope."
  (setf (walking-player-spell-flash player) 1.0)
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

(defun advance-walking-player (player source camera forward right seconds)
  "Advance PLAYER from camera-relative movement axes for SECONDS."
  (begin-walking-player-frame player)
  (let ((length (sqrt (+ (* forward forward) (* right right)))))
    (if (plusp length)
        (let* ((forward (/ forward length))
               (right (/ right length))
               (yaw (camera-yaw camera))
               (direction-x (+ (* (cos yaw) forward) (* (sin yaw) right)))
               (direction-y (+ (* (sin yaw) forward) (* (- (cos yaw)) right)))
               (distance (* seconds (walking-player-speed player)))
               (position (walking-player-position player))
               (before-x (vec3:vec3-x position))
               (before-y (vec3:vec3-y position)))
          (if (walking-player-grounded-p player)
              (progn
                (try-walking-player-axis player source :x (* direction-x distance))
                (try-walking-player-axis player source :y (* direction-y distance)))
              (let ((position (walking-player-position player)))
                (incf (vec3:vec3-x position) (* direction-x distance))
                (incf (vec3:vec3-y position) (* direction-y distance))))
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
           (walking-player-spell-flash player)))))
