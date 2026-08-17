;;; Riding an animal.  #W2E5HR is the design.
;;;
;;; Mounting is the same session transition as reading a world terminal
;;; (#8JCMA5) and it deliberately feels like one: TAB enters the interaction
;;; with whatever the crosshair is on, shift-TAB leaves it, ordinary player
;;; input stands down for the duration, and the camera eases from wherever it
;;; was into the pose the interaction asks for -- the same easing, at the same
;;; rate, into the same narrowed field of view.
;;;
;;; Two things make a mount different from a wall.  Its pose moves -- the pose
;;; is a seat on the animal's own back, so it is asked again every frame and
;;; the view rides the animal's gait and its slow turns; and it carries the
;;; player, so the scalar controller stands down completely and the player's
;;; body is set on the animal instead of left standing where it mounted.  That
;;; is what keeps chunk residency, checkpoints, and the return camera all
;;; following the ride rather than the place it started.

(in-package #:luvcraft)

;;; Where the rider sits is art direction rather than physics, and it is
;;; settled by looking: these are parameters so that a live image can move the
;;; seat between two screenshots.

(defparameter *critter-ride-seat-lift* 0.34d0
  "How far above the animal's own back the rider's eyes sit, in cells.

Measured from the top of the animal rather than from the ground, so a rider
sits on whatever they have mounted instead of at a height which only suits a
turtle.")

(defparameter *critter-ride-seat-offset* -0.42d0
  "How far back along the animal the rider sits, in cells: on it, not on its
head.")

(defparameter *critter-ride-seat-pitch* -0.33d0
  "How far a seated rider looks down, in radians.

Enough to keep the shell they are sitting on in the bottom of the frame and
the ground the animal is walking over in the middle of it.")

(defclass critter-ride ()
  ((critter :initarg :critter :reader critter-ride-critter))
  (:documentation "A player mounted on an animal, going where it goes.

The rider steers with the session's own movement intent.  Going forward is
going forward whether the player's legs or an animal's are doing it, so a
mount reads the same urges the player controller would have read rather than
keeping a second copy of them."))

(defmethod activate-luvcraft-critter ((critter turtle) session)
  "A turtle can be ridden.  It is not fast, but it is willing."
  (declare (ignore session))
  (make-instance 'critter-ride :critter critter))

(defmethod luvcraft-focus-carries-player-p ((focus critter-ride))
  (declare (ignore focus))
  t)

(defun carry-luvcraft-player (session critter)
  "Set the player on CRITTER's back, at rest and standing on nothing else."
  (let ((player (luvcraft-session-player session)))
    (when player
      (setf (player-x player) (critter-x critter)
            (player-y player) (critter-y critter)
            (player-z player) (critter-z critter)
            (player-velocity-x player) 0d0
            (player-velocity-y player) 0d0
            (player-velocity-z player) 0d0
            (player-grounded-p player) t))
    player))

(defparameter *dismount-offsets*
  '((0d0 0d0) (0.9d0 0d0) (-0.9d0 0d0) (0d0 0.9d0) (0d0 -0.9d0)
    (0.9d0 0.9d0) (-0.9d0 0.9d0) (0.9d0 -0.9d0) (-0.9d0 -0.9d0))
  "Where to try putting a rider down, nearest the animal first.")

(defun dismount-luvcraft-player (session critter)
  "Stand the player beside CRITTER somewhere their own box actually fits.

An animal can walk under a ledge its rider could not stand under, so getting
off is a search for room rather than simply letting go.  When nothing fits,
the player is left where the animal is and the ordinary controller resolves it
on the next step."
  (let ((player (luvcraft-session-player session))
        (world (luvcraft-session-world session)))
    (when player
      (loop for (offset-x offset-z) in *dismount-offsets*
            for x = (+ (critter-x critter) offset-x)
            for z = (+ (critter-z critter) offset-z)
            for y = (critter-y critter)
            when (body-position-clear-p player world x y z)
              do (setf (player-x player) x
                       (player-y player) y
                       (player-z player) z)
                 (return player)
            finally (return player)))))

(defmethod luvcraft-focus-left ((ride critter-ride) session)
  "Let the animal have its own mind back, and put the rider down safely."
  (urge-critter (critter-ride-critter ride) 0d0 0d0 0d0)
  (dismount-luvcraft-player session (critter-ride-critter ride))
  nil)

(defmethod handle-luvcraft-focus-event
    ((ride critter-ride) session canvas (event canvas-event))
  (declare (ignore ride session canvas event))
  ;; A rider is not editing blocks, and the mount owns the view: everything
  ;; else that arrives while mounted is simply not for anyone.
  t)

(defmethod luvcraft-focus-camera-pose ((ride critter-ride) session)
  "Sit the rider on the animal's back, looking out over its shell.

The seat is a place on the animal rather than a distance behind it, so the
pose is asked again every frame and the camera stays locked to a body that is
walking: what the ride feels like is the animal's own gait and its own slow
turns, not a camera trailing after it.  The mount's sway rides along, and the
easing which brings the view here on mounting is what smooths it."
  (declare (ignore session))
  (let ((critter (critter-ride-critter ride)))
    (multiple-value-bind (sway-lift sway-yaw) (critter-sway critter)
      (let ((yaw (critter-yaw critter)))
        (make-camera-pose
         (make-vec3 (+ (critter-x critter)
                       (* *critter-ride-seat-offset* (sin yaw)))
                    (+ (critter-y critter) (critter-height critter)
                       *critter-ride-seat-lift* sway-lift)
                    (+ (critter-z critter)
                       (* *critter-ride-seat-offset* (cos yaw))))
         (+ yaw sway-yaw)
         *critter-ride-seat-pitch*
         +luvcraft-camera-focused-vertical-field-of-view+)))))

(defmethod advance-luvcraft-focus ((ride critter-ride) session seconds)
  "Rein the animal, ride along with it, and get off if it is no longer there."
  (let ((critter (critter-ride-critter ride)))
    (cond ((not (find critter (critter-population-critters
                               (luvcraft-session-critters session))
                      :test #'eq))
           ;; The animal is gone -- it wandered out of the world, or the
           ;; population forgot it.  Leaving the focus puts the rider back on
           ;; their own feet and hands the camera back.
           (unfocus-luvcraft-session session))
          (t
           (let ((intent (luvcraft-session-movement-intent session)))
             (urge-critter critter
                           (movement-intent-axis intent :forward :backward)
                           (movement-intent-axis intent :right :left)
                           seconds))
           (carry-luvcraft-player session critter))))
  ride)
