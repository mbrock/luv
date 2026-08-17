;;; The animals of the block world, and the first of them: a turtle.
;;;
;;; #KTRMAO is the design; this is what it turned into.
;;;
;;; A critter is one of the genuinely individuated entities of #F6R9QA rather
;;; than a reading of a dense field.  It owns a position, a heading, a small
;;; mind, and an independently inspectable lifetime, and there are few enough
;;; of them that identity costs nothing: each animal is an ordinary CLOS
;;; instance and each species is a class.  What stays dense is the per-frame
;;; render product -- one interleaved vertex array over the ordinary block
;;; atlas and block surface pipeline, exactly like the smash fragments.
;;;
;;; Three protocols meet in an animal:
;;;
;;;   ADVANCE-CRITTER is its own behaviour, the only part a new species must
;;;   really think about;
;;;   the body protocol in SIMULATION.LISP is its contact with terrain -- the
;;;   same one-axis-at-a-time AABB sweep the player controller walks on; and
;;;   MAP-CRITTER-BOXES is its body as seen, a handful of textured boxes in a
;;;   local frame which the emitter here turns by the critter's yaw.
;;;
;;; Nothing an animal decides comes from a random state.  A critter's choices
;;; are hashed from its seed, so a turtle wanders the same way in the same
;;; world on every machine and in every replay, the same discipline the
;;; terrain source and the fragment bursts already keep.

(in-package #:luvcraft)

(luv.arithmetic:define-quantity-constant
    +critter-gravity+ 24d0
  :type double-float
  :quantity (:quantity :gravity-magnitude
             :unit ((:cell 1) (:second -2))))
(luv.arithmetic:define-quantity-constant
    +critter-terminal-fall-speed+ -32d0
  :type double-float
  :quantity (:quantity :world-velocity
             :unit ((:cell 1) (:second -1))))

(defconstant +maximum-critters+ 12
  "How many animals may live around the player at once.")
(defconstant +critter-model-box-limit+ 12
  "The most boxes any critter model may emit; the vertex buffer is sized for it.")
(defconstant +critter-vertices-per-box+
  (* 6 +block-mesh-vertices-per-face+))
(defconstant +critter-buffer-size+
  (* 4 +maximum-critters+ +critter-model-box-limit+
     +critter-vertices-per-box+ +block-mesh-floats-per-vertex+))

(defclass critter ()
  ((position :initarg :position
             :initform (make-vec3 0d0 0d0 0d0)
             :type vec3
             :quantity (:quantity :world-position :unit :cell
                        :tensor-order 1)
             :accessor critter-position)
   (velocity :initarg :velocity
             :initform (make-vec3 0d0 0d0 0d0)
             :type vec3
             :quantity (:quantity :world-velocity
                        :unit ((:cell 1) (:second -1))
                        :tensor-order 1)
             :accessor critter-velocity)
   (yaw :initarg :yaw :initform 0d0
        :type double-float
        :quantity (:quantity :critter-yaw :unit :radian)
        :accessor critter-yaw)
   (half-width :initarg :half-width :initform 0.30d0
               :type double-float
               :quantity (:quantity :critter-half-width :unit :cell)
               :reader critter-half-width)
   (height :initarg :height :initform 0.60d0
           :type double-float
           :quantity (:quantity :critter-height :unit :cell)
           :reader critter-height)
   (grounded-p :initarg :grounded-p :initform nil
               :accessor critter-grounded-p)
   ;; The animal's own deterministic identity: every choice it makes is a
   ;; hash of this seed and how many choices it has already made.
   (seed :initarg :seed :initform 0 :type (integer 0)
         :reader critter-seed)
   (decision-count :initform 0 :type (integer 0)
                   :accessor critter-decision-count))
  (:documentation "One individuated animal walking the block world.")
  (:metaclass luv.arithmetic.records:quantity-class))

(defmethod body-position ((body critter)) (critter-position body))
(defmethod body-velocity ((body critter)) (critter-velocity body))
(defmethod body-half-width ((body critter)) (critter-half-width body))
(defmethod body-height ((body critter)) (critter-height body))
(defmethod body-grounded-p ((body critter)) (critter-grounded-p body))
(defmethod (setf body-grounded-p) (grounded-p (body critter))
  (setf (critter-grounded-p body) grounded-p))

(defun critter-x (critter) (vec3-x (critter-position critter)))
(defun critter-y (critter) (vec3-y (critter-position critter)))
(defun critter-z (critter) (vec3-z (critter-position critter)))
(defun critter-velocity-x (critter) (vec3-x (critter-velocity critter)))
(defun critter-velocity-y (critter) (vec3-y (critter-velocity critter)))
(defun critter-velocity-z (critter) (vec3-z (critter-velocity critter)))
(defun (setf critter-velocity-x) (value critter)
  (setf (vec3-x (critter-velocity critter)) value))
(defun (setf critter-velocity-y) (value critter)
  (setf (vec3-y (critter-velocity critter)) value))
(defun (setf critter-velocity-z) (value critter)
  (setf (vec3-z (critter-velocity critter)) value))

(defun critter-noise (seed index salt)
  "Return a stable 0..1 reading for one small animal choice.

The same three integers always answer the same way, which is what lets a
wandering animal be reproducible without carrying a random state around."
  (let ((value (logand #xffffffff
                       (+ (* seed 374761393) (* index 668265263)
                          (* salt 2246822519)))))
    (setf value
          (logand #xffffffff
                  (* (logxor value (ash value -13)) 1274126177)))
    (/ (logand (logxor value (ash value -16)) #xffffff)
       (coerce #xffffff 'double-float))))

(defun critter-decision (critter salt)
  "Return a stable 0..1 reading for CRITTER's current decision and SALT."
  (critter-noise (critter-seed critter)
                 (critter-decision-count critter)
                 salt))

(defun critter-decide-again (critter)
  "Move CRITTER to its next decision, so its readings are all new ones."
  (incf (critter-decision-count critter)))

;;; The critter protocol proper.

(defgeneric advance-critter (critter world seconds)
  (:documentation
   "Advance CRITTER's behaviour and body by SECONDS in WORLD.

This is the only part of an animal a new species really has to think about;
terrain contact is the shared body protocol and appearance is
MAP-CRITTER-BOXES."))

(defgeneric map-critter-boxes (critter function)
  (:documentation
   "Call FUNCTION for each textured box of CRITTER's model.

Each call passes the box centre and half extents in the animal's own frame --
X to its right, Y up from the ground it stands on, Z ahead of it -- and the
atlas tile its faces wear: (X Y Z HALF-X HALF-Y HALF-Z TILE).  The emitter
turns the boxes by the critter's yaw; a model never sees world coordinates."))

(defgeneric critter-model-box-count (critter)
  (:documentation
   "How many boxes MAP-CRITTER-BOXES emits for CRITTER, at most
+CRITTER-MODEL-BOX-LIMIT+."))

(defgeneric critter-species (critter)
  (:documentation "The keyword naming CRITTER's species."))

(defgeneric urge-critter (critter forward turn seconds)
  (:documentation
   "Ask CRITTER to walk FORWARD and turn TURN over SECONDS, as a rider does.

FORWARD and TURN are wishes in -1..1, not velocities: what the animal makes of
them is the animal's business, and one which ignores its rider simply has no
method here."))

(defmethod urge-critter (critter forward turn seconds)
  (declare (ignore critter forward turn seconds))
  nil)

(defgeneric critter-sway (critter)
  (:documentation
   "Return the lift and yaw a rider is swayed by, as CRITTER moves under them.

An animal walking is not a platform gliding: whoever is sitting on it rides
its gait.  (VALUES LIFT YAW), both small and both zero for an animal which
does not say otherwise."))

(defmethod critter-sway (critter)
  (declare (ignore critter))
  (values 0d0 0d0))

(defgeneric spawn-critter-at (species world x y z seed)
  (:documentation
   "Make a SPECIES animal standing on the ground at cell X,Y,Z, or NIL.

SPECIES is a keyword, so a new animal joins the world's population by adding
one EQL method here rather than by editing a spawn table.  A method returns
NIL when the site does not suit it: this is where an animal says what ground
it lives on and how much room it needs."))

(defmethod spawn-critter-at (species world x y z seed)
  (declare (ignore world x y z seed))
  (error "No critter species is named ~S." species))

;;; The turtle: slow, deliberate, and unbothered.  It walks a while, stops a
;;; while, turns away from what it bumps into, and never jumps.

(defconstant +turtle-model-box-count+ 10)

(luv.arithmetic:define-quantity-constant
    +turtle-walk-speed+ 0.62d0
  :type double-float
  :quantity (:quantity :critter-walk-speed
             :unit ((:cell 1) (:second -1))))
(luv.arithmetic:define-quantity-constant
    +turtle-half-width+ 0.34d0
  :type double-float
  :quantity (:quantity :critter-half-width :unit :cell))
(luv.arithmetic:define-quantity-constant
    +turtle-height+ 0.46d0
  :type double-float
  :quantity (:quantity :critter-height :unit :cell))

(defconstant +turtle-turn-rate+ 1.1d0
  "How fast a turtle swings toward the heading it wants, in radians a second.")
(defconstant +turtle-step-length+ 0.42d0
  "How far a turtle walks per full stride, which sets its leg cadence.")

(defparameter *turtle-leg-offsets*
  '((-0.20d0 0.23d0) (0.20d0 0.23d0) (-0.20d0 -0.23d0) (0.20d0 -0.23d0))
  "Where a turtle's four legs stand: right of, and ahead of, its centre.

A walking turtle moves its legs in diagonal couplets, so the two legs of each
diagonal share a gait phase and the other pair is half a stride behind.")

(defclass turtle (critter)
  ((heading :initarg :heading :initform 0d0
            :type double-float
            :quantity (:quantity :critter-yaw :unit :radian)
            :accessor turtle-heading)
   (resting-p :initform nil :accessor turtle-resting-p)
   (mood-seconds :initform 0d0
                 :type double-float
                 :quantity (:quantity :critter-behavior-duration
                            :unit :second)
                 :accessor turtle-mood-seconds)
   ;; How far this turtle has walked, in strides: the phase its legs swing
   ;; on, kept as distance rather than time so a stopped turtle stands still.
   (gait-phase :initform 0d0 :type double-float
               :accessor turtle-gait-phase))
  (:default-initargs :half-width +turtle-half-width+
                     :height +turtle-height+)
  (:documentation "A small, slow, deliberate animal with a domed shell.")
  (:metaclass luv.arithmetic.records:quantity-class))

(defmethod critter-species ((critter turtle)) :turtle)
(defmethod critter-model-box-count ((critter turtle)) +turtle-model-box-count+)

(defun turtle-choose-mood (turtle)
  "Choose TURTLE's next spell of walking or resting, and where it heads."
  (critter-decide-again turtle)
  (let ((resting-p (not (turtle-resting-p turtle))))
    (setf (turtle-resting-p turtle) resting-p
          (turtle-mood-seconds turtle)
          (if resting-p
              (+ 1.8d0 (* 4.5d0 (critter-decision turtle 1)))
              (+ 3.5d0 (* 6.0d0 (critter-decision turtle 2)))))
    (unless resting-p
      (setf (turtle-heading turtle)
            (* 2d0 pi (critter-decision turtle 3)))))
  turtle)

(defun turtle-turn-away (turtle)
  "Send TURTLE off on a new heading after it has walked into something."
  (critter-decide-again turtle)
  (setf (turtle-heading turtle)
        (+ (critter-yaw turtle)
           (* (if (< (critter-decision turtle 4) 0.5d0) -1d0 1d0)
              (+ 1.4d0 (* 1.6d0 (critter-decision turtle 5))))))
  turtle)

(defmethod advance-critter ((turtle turtle) world seconds)
  (let ((seconds (coerce seconds 'double-float)))
    (decf (turtle-mood-seconds turtle) seconds)
    (when (minusp (turtle-mood-seconds turtle))
      (turtle-choose-mood turtle))
    ;; Turning is what a turtle does slowly and visibly, so the heading is a
    ;; wish and the yaw follows it at a bounded rate.
    (let ((difference (shortest-angle-difference
                       (turtle-heading turtle) (critter-yaw turtle)))
          (maximum-turn (* +turtle-turn-rate+ seconds)))
      (incf (critter-yaw turtle)
            (max (- maximum-turn) (min maximum-turn difference))))
    (let* ((walking-p (and (not (turtle-resting-p turtle))
                           (critter-grounded-p turtle)))
           (speed (if walking-p +turtle-walk-speed+ 0d0))
           (yaw (critter-yaw turtle)))
      (setf (critter-velocity-x turtle) (* speed (sin yaw))
            (critter-velocity-z turtle) (* speed (cos yaw)))
      (let ((distance-x (* (critter-velocity-x turtle) seconds))
            (distance-z (* (critter-velocity-z turtle) seconds)))
        (when (or (move-body-axis turtle world :x distance-x)
                  (move-body-axis turtle world :z distance-z))
          (turtle-turn-away turtle))
        (when walking-p
          (incf (turtle-gait-phase turtle)
                (/ (* 2d0 pi (abs (* speed seconds)))
                   +turtle-step-length+))))
      (decf (critter-velocity-y turtle) (* +critter-gravity+ seconds))
      (setf (critter-velocity-y turtle)
            (max +critter-terminal-fall-speed+ (critter-velocity-y turtle))
            (critter-grounded-p turtle) nil)
      (move-body-axis turtle world :y
                      (* (critter-velocity-y turtle) seconds))))
  turtle)

(defmethod map-critter-boxes ((turtle turtle) function)
  ;; A blocky animal is a stack of slabs: a stepped carapace over a pale
  ;; plastron, a head on the front, a stub of tail behind, and four legs
  ;; which swing and lift on the gait phase the walk has accumulated.
  (let* ((phase (turtle-gait-phase turtle))
         (walking-p (not (turtle-resting-p turtle)))
         (shell +turtle-carapace-tile+)
         (skin +turtle-skin-tile+)
         (plastron +turtle-plastron-tile+))
    (flet ((box (x y z half-x half-y half-z tile)
             (funcall function x y z half-x half-y half-z tile)))
      ;; The belly plate, then a carapace of three steps: the dome is made of
      ;; slabs because this is a world of blocks, but each step is deep enough
      ;; to read as a shell rather than as a stack of planks.
      (box 0.0d0 0.190d0 0.0d0 0.250d0 0.050d0 0.320d0 plastron)
      (box 0.0d0 0.265d0 0.0d0 0.285d0 0.060d0 0.365d0 shell)
      (box 0.0d0 0.350d0 -0.015d0 0.225d0 0.045d0 0.290d0 shell)
      (box 0.0d0 0.415d0 -0.030d0 0.130d0 0.035d0 0.175d0 shell)
      ;; A resting turtle draws its head back toward the shell; a walking one
      ;; cranes it a little further out on every stride.
      (box 0.0d0 0.240d0
           (+ 0.425d0 (if walking-p (* 0.035d0 (sin (* 0.5d0 phase))) -0.075d0))
           0.095d0 0.080d0 0.130d0 skin)
      (box 0.0d0 0.245d0 -0.435d0 0.050d0 0.040d0 0.075d0 skin)
      (loop for (offset-x offset-z) in *turtle-leg-offsets*
            for couplet = (if (plusp (* offset-x offset-z)) 0d0 pi)
            for leg-phase = (+ phase couplet)
            for swing = (if walking-p (* 0.075d0 (sin leg-phase)) 0d0)
            for lift = (if walking-p
                           (* 0.035d0 (max 0d0 (sin leg-phase)))
                           0d0)
            do (box offset-x (+ 0.090d0 lift) (+ offset-z swing)
                    0.080d0 0.090d0 0.090d0 skin)))))

(defconstant +turtle-rider-turn-rate+ 1.3d0
  "How fast a rider may swing a turtle's heading, in radians a second.")
(defconstant +turtle-rider-lead+ 0.7d0
  "How far ahead of its own yaw a rider may point a turtle, in radians.

Bounded, because a turtle turns at its own rate: without this, a held rein
would run the wanted heading far past what the animal can follow and leave it
turning long after the rider let go.")

(defmethod urge-critter ((turtle turtle) forward turn seconds)
  "A ridden turtle sets its own mind aside and goes where it is pointed.

Urging renews its walking spell rather than replacing its gait, so a ridden
turtle walks and stops like a turtle: slowly, and in its own time."
  (let ((seconds (coerce seconds 'double-float)))
    (unless (zerop turn)
      (let ((wanted (+ (turtle-heading turtle)
                       (* turn +turtle-rider-turn-rate+ seconds))))
        (setf (turtle-heading turtle)
              (+ (critter-yaw turtle)
                 (max (- +turtle-rider-lead+)
                      (min +turtle-rider-lead+
                           (shortest-angle-difference
                            wanted (critter-yaw turtle))))))))
    (setf (turtle-resting-p turtle) (not (plusp forward))
          (turtle-mood-seconds turtle)
          (max 0.75d0 (turtle-mood-seconds turtle))))
  turtle)

(defmethod critter-sway ((turtle turtle))
  "A walking turtle rocks its rider once a stride, and rolls a little with it.

The roll has to be read as a small yaw, because a camera pose here carries no
roll: what is left of a waddle is the shell's nose swinging across the
direction of travel."
  (let ((phase (turtle-gait-phase turtle)))
    (if (turtle-resting-p turtle)
        (values 0d0 0d0)
        (values (* 0.022d0 (sin (* 2d0 phase)))
                (* 0.030d0 (sin phase))))))

(defmethod spawn-critter-at ((species (eql :turtle)) world x y z seed)
  "Stand a turtle on grass or sand with clear air over it."
  (let ((ground (world-block-at world x (1- y) z)))
    (when (and (member ground (list *grass-block* *sand-block* *moss-block*)
                       :test #'eq)
               (not (world-terrain-solid-p world x y z))
               (not (world-terrain-solid-p world x (1+ y) z)))
      (make-instance 'turtle
                     :seed seed
                     :position (make-vec3 (+ x 0.5d0)
                                          (coerce y 'double-float)
                                          (+ z 0.5d0))
                     :yaw (* 2d0 pi (critter-noise seed 0 7))
                     :heading (* 2d0 pi (critter-noise seed 0 7))))))

;;; The population: the session-owned semantic object which holds the animals
;;; alive around the player, advances them, and keeps them company.

(defconstant +critter-spawn-radius+ 22
  "How far from the player a new animal may appear, in cells.")
(defconstant +critter-spawn-clearance+ 7
  "How near the player an animal may never simply appear, in cells.")
(defconstant +critter-despawn-distance+ 46d0
  "How far an animal may wander from the player before it is forgotten.")
(defconstant +critter-spawn-attempts-per-frame+ 2)

(defclass critter-population ()
  ((critters :initform (make-array +maximum-critters+
                                   :adjustable t :fill-pointer 0)
             :reader critter-population-critters)
   (species :initarg :species :initform (list :turtle)
            :reader critter-population-species)
   (target-count :initarg :target-count :initform 5
                 :reader critter-population-target-count)
   (seed :initarg :seed :initform 8675309 :reader critter-population-seed)
   (attempt-count :initform 0 :accessor critter-population-attempt-count))
  (:documentation "The living animals near the player, and how to find more."))

(defun critter-count (population)
  "Return the number of animals alive in POPULATION."
  (length (critter-population-critters population)))

(defun add-critter (population critter)
  "Add CRITTER to POPULATION unless it is already full, returning it or NIL."
  (check-type critter critter)
  (when (< (critter-count population) +maximum-critters+)
    (vector-push-extend critter (critter-population-critters population))
    critter))

(defun advance-critters (population world seconds)
  "Advance every animal in POPULATION by SECONDS in WORLD."
  (check-type population critter-population)
  (when (plusp seconds)
    (loop for critter across (critter-population-critters population)
          do (advance-critter critter world seconds)))
  population)

(defun forget-distant-critters (population x z)
  "Drop the animals of POPULATION which have left the neighbourhood of X,Z."
  (let ((critters (critter-population-critters population))
        (write-index 0))
    (dotimes (read-index (length critters))
      (let* ((critter (aref critters read-index))
             (dx (- (critter-x critter) x))
             (dz (- (critter-z critter) z)))
        (when (and (< (+ (* dx dx) (* dz dz))
                      (* +critter-despawn-distance+
                         +critter-despawn-distance+))
                   (> (critter-y critter) -8d0))
          (setf (aref critters write-index) critter)
          (incf write-index))))
    (setf (fill-pointer critters) write-index))
  population)

(defun critter-spawn-site (world x z)
  "Return the standing height on WORLD's resident column at X,Z, or NIL.

A column only offers a site when it is resident: an animal must not be
conjured onto terrain which has not been generated yet."
  (let ((height (chunk-shape-height
                 (voxel-space-chunk-shape (block-world-space world)))))
    (loop for y from (- height 2) downto 1
          do (multiple-value-bind (block status) (world-block-at world x y z)
               (unless (eq status :resident)
                 (return nil))
               (when (block-solid-p block)
                 (return (1+ y)))))))

(defun maintain-critter-population (population world x z)
  "Keep POPULATION's animals living on resident terrain near X,Z.

Called once a frame beside residency: animals which have wandered out of the
neighbourhood are forgotten, and a couple of deterministic candidate sites are
offered to the population's species until it is as full as it wants to be."
  (check-type population critter-population)
  (forget-distant-critters population x z)
  (let ((center-x (floor x))
        (center-z (floor z)))
    (dotimes (attempt +critter-spawn-attempts-per-frame+)
      (declare (ignorable attempt))
      (when (>= (critter-count population)
                (critter-population-target-count population))
        (return))
      (let* ((index (incf (critter-population-attempt-count population)))
             (seed (critter-population-seed population))
             (offset-x (round (* (- (* 2d0 (critter-noise seed index 11)) 1d0)
                                 +critter-spawn-radius+)))
             (offset-z (round (* (- (* 2d0 (critter-noise seed index 12)) 1d0)
                                 +critter-spawn-radius+)))
             (species (nth (mod index
                                (length (critter-population-species
                                         population)))
                           (critter-population-species population))))
        (when (>= (+ (* offset-x offset-x) (* offset-z offset-z))
                  (* +critter-spawn-clearance+ +critter-spawn-clearance+))
          (let* ((site-x (+ center-x offset-x))
                 (site-z (+ center-z offset-z))
                 (site-y (critter-spawn-site world site-x site-z)))
            (when site-y
              (let ((critter (spawn-critter-at
                              species world site-x site-y site-z
                              (logand (* index 2654435761) #xffffff))))
                (when critter
                  (add-critter population critter)))))))))
  population)

;;; Looking at an animal.  The block world answers a ray with a lattice
;;; traversal; an animal is not on the lattice, so its body answers for itself.
;;; Its body for this purpose is the same upright box its feet walk on, not its
;;; model: what the player aims at is the animal, not its left hind leg.

(defun critter-ray-distance (critter origin direction max-distance)
  "Return how far along a ray it enters CRITTER's body, or NIL if it misses.

ORIGIN and DIRECTION are VEC3 values in continuous cell coordinates, and
DIRECTION is assumed to be a unit vector, as a camera basis gives it."
  (let ((near 0d0)
        (far (coerce max-distance 'double-float))
        (half (critter-half-width critter)))
    (flet ((slab (origin direction minimum maximum)
             (let ((origin (coerce origin 'double-float))
                   (direction (coerce direction 'double-float)))
               (if (zerop direction)
                   (<= minimum origin maximum)
                   (let ((entering (/ (- minimum origin) direction))
                         (leaving (/ (- maximum origin) direction)))
                     (when (> entering leaving)
                       (rotatef entering leaving))
                     (setf near (max near entering)
                           far (min far leaving))
                     (<= near far))))))
      (and (slab (vec3-x origin) (vec3-x direction)
                 (- (critter-x critter) half) (+ (critter-x critter) half))
           (slab (vec3-y origin) (vec3-y direction)
                 (critter-y critter)
                 (+ (critter-y critter) (critter-height critter)))
           (slab (vec3-z origin) (vec3-z direction)
                 (- (critter-z critter) half) (+ (critter-z critter) half))
           near))))

(defun critter-along-ray (population origin direction max-distance)
  "Return the nearest animal of POPULATION on the ray, and its distance."
  (check-type population critter-population)
  (let ((nearest nil)
        (nearest-distance nil))
    (loop for critter across (critter-population-critters population)
          for distance = (critter-ray-distance critter origin direction
                                               max-distance)
          when (and distance
                    (or (null nearest-distance) (< distance nearest-distance)))
            do (setf nearest critter
                     nearest-distance distance))
    (values nearest nearest-distance)))

;;; The render product: one dense vertex array over the block surface
;;; pipeline.  A critter's boxes are not voxel faces, so they classify none of
;;; their edges: the fragment stage leaves their silhouette alone and shapes
;;; their surface from the material's own relief.

(defun critter-face-shade (normal-y)
  "Return the ambient shade of a critter face with world normal lane NORMAL-Y.

An animal carries no baked neighbourhood, but its underside is genuinely more
occluded than its shell, and saying so is what keeps it from reading as a
paper cut-out standing on the grass."
  (cond ((plusp normal-y) 1.0)
        ((minusp normal-y) 0.62)
        (t 0.86)))

(defun critter-light-levels (critter world)
  "Return CRITTER's normalized sky and block light, sampled where it stands."
  (multiple-value-bind (sky block status)
      (world-light-at world
                      (floor (critter-x critter))
                      (floor (+ (critter-y critter)
                                (* 0.5d0 (critter-height critter))))
                      (floor (critter-z critter)))
    (if (eq status :resident)
        (values (/ sky 15.0) (/ block 15.0))
        ;; An animal standing in terrain which is not resident is lit as if it
        ;; stood under open sky rather than blacked out.
        (values 1.0 0.0))))

(defconstant +critter-texture-cell-span+ 0.75d0
  "How much of the world one whole critter atlas tile covers, in cells.

A terrain material tiles once per cell, so a block face can simply take the
whole tile.  A critter's faces are all different fractions of a cell, and
stretching one tile across each of them would print scutes the size of the
shell on its top and smear the same scutes into bands around its rim.  Each
face instead takes the centred fraction of its tile which keeps texels
roughly the same size everywhere on the animal.")

(defun critter-face-texture-scales (face half-x half-y half-z)
  "Return how much of FACE's tile the face's two in-plane extents deserve.

The in-plane axes are the ones BLOCK-FACE-LOCAL-UV projects onto, read from
the same face normal it reads."
  (let ((normal (block-face-neighbor face)))
    (multiple-value-bind (extent-u extent-v)
        (cond ((not (zerop (voxel-direction-dy normal)))
               (values (* 2d0 half-x) (* 2d0 half-z)))
              ((not (zerop (voxel-direction-dz normal)))
               (values (* 2d0 half-x) (* 2d0 half-y)))
              (t (values (* 2d0 half-z) (* 2d0 half-y))))
      (values (min 1d0 (/ extent-u +critter-texture-cell-span+))
              (min 1d0 (/ extent-v +critter-texture-cell-span+))))))

(defun emit-critter-box
    (vertices critter box-x box-y box-z half-x half-y half-z tile
     sky-level block-level)
  "Append one of CRITTER's model boxes, turned by its yaw, to VERTICES.

The box arrives in the animal's own frame; the yaw rotation here is the only
place a critter model meets world coordinates."
  (let* ((position (critter-position critter))
         (yaw (critter-yaw critter))
         (cos-yaw (cos yaw))
         (sin-yaw (sin yaw))
         (origin-x (vec3-x position))
         (origin-y (vec3-y position))
         (origin-z (vec3-z position))
         (size +block-atlas-tile-size+)
         (atlas-width (* size +block-atlas-tile-count+)))
    (dolist (face *block-faces*)
      (let* ((normal (block-face-neighbor face))
             (nx (voxel-direction-dx normal))
             (ny (voxel-direction-dy normal))
             (nz (voxel-direction-dz normal))
             (world-nx (+ (* nx cos-yaw) (* nz sin-yaw)))
             (world-nz (- (* nz cos-yaw) (* nx sin-yaw)))
             (shade (critter-face-shade ny)))
        (multiple-value-bind (scale-u scale-v)
            (critter-face-texture-scales face half-x half-y half-z)
          (let ((origin-u (* 0.5d0 (- 1d0 scale-u)))
                (origin-v (* 0.5d0 (- 1d0 scale-v))))
            (dolist (index '(0 1 2 0 2 3))
              (let* ((corner (nth index (block-face-corners face)))
                     (local-x (+ box-x (* (- (first corner) 0.5d0)
                                          2d0 half-x)))
                     (local-y (+ box-y (* (- (second corner) 0.5d0)
                                          2d0 half-y)))
                     (local-z (+ box-z (* (- (third corner) 0.5d0)
                                          2d0 half-z))))
                (multiple-value-bind (u v) (block-face-local-uv face corner)
                  (let ((tile-u (+ origin-u (* u scale-u)))
                        (tile-v (+ origin-v (* v scale-v))))
                    (push-block-vertex-components
                     vertices
                     (+ origin-x (* local-x cos-yaw) (* local-z sin-yaw))
                     (+ origin-y local-y)
                     (+ origin-z (- (* local-z cos-yaw) (* local-x sin-yaw)))
                     (/ (+ (* tile size) 0.5 (* tile-u (1- size)))
                        atlas-width)
                     (/ (+ 0.5 (* tile-v (1- size))) size)
                     shade world-nx ny world-nz
                     sky-level block-level 0.0
                     +block-face-edge-flush+ +block-face-edge-flush+
                     +block-face-edge-flush+ +block-face-edge-flush+))))))))))
  vertices)

(defun emit-critter (vertices critter world)
  "Append CRITTER's whole model to VERTICES, lit where the animal stands."
  (let ((boxes (critter-model-box-count critter)))
    (unless (<= boxes +critter-model-box-limit+)
      (error "~S emits ~D model boxes, more than the ~D the buffer holds."
             (critter-species critter) boxes +critter-model-box-limit+)))
  (multiple-value-bind (sky-level block-level)
      (critter-light-levels critter world)
    (map-critter-boxes
     critter
     (lambda (x y z half-x half-y half-z tile)
       (emit-critter-box vertices critter x y z half-x half-y half-z tile
                         sky-level block-level))))
  vertices)

(defun critter-vertices (population world)
  "Build the block-pipeline vertex stream for POPULATION's animals."
  (check-type population critter-population)
  (let* ((critters (critter-population-critters population))
         (vertices
           (make-array
            (* (length critters) +critter-model-box-limit+
               +critter-vertices-per-box+ +block-mesh-floats-per-vertex+)
            :element-type 'single-float :adjustable t :fill-pointer 0)))
    (loop for critter across critters
          do (emit-critter vertices critter world))
    vertices))
