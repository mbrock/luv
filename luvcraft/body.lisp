;;; The player's own body as seen from inside it, and what its hands hold.
;;;
;;; #S27JKR is the design; this is what it turned into.
;;;
;;; The camera has always been a disembodied eye.  This gives it a pair of
;;; arms hanging into the bottom of the view, hands on the ends of them, and a
;;; grip protocol so that a hand can hold something: brought up out of a
;;; pocket, held while walking, put away again.
;;;
;;; The body is drawn exactly like an animal (see CRITTERS.LISP): a handful of
;;; textured boxes on the ordinary block surface pipeline.  What differs is
;;; the frame.  A critter's boxes turn only with its yaw; the body's boxes
;;; live in the *view* frame -- x to the right, y up, z forward, origin at
;;; the eye -- and turn with the whole camera, pitch included, so the hands
;;; stay put on the screen while the head looks around.  Each box may also
;;; carry its own small tilt, which is what lets a forearm slope up from the
;;; screen edge and a phone lean back in the palm instead of everything
;;; standing square to the lens.
;;;
;;; The first thing a hand can hold is a phone (PHONE.LISP).  It is
;;; deliberately a little too large: a slab of near-black glass with a live
;;; terminal on it, held up in front of the face like everyone holds theirs.

(in-package #:luvcraft)

(defconstant +player-body-box-limit+ 40
  "The most box-equivalents (36 vertices) the body and its held item may
emit per frame; a rounded slab counts for several.")
(defconstant +player-body-buffer-size+
  (* 4 +player-body-box-limit+
     +critter-vertices-per-box+ +block-mesh-floats-per-vertex+))

(defconstant +player-body-equip-rate+ 7.0d0
  "How quickly a held item rises into view or drops out of it, per second.")
(defconstant +player-body-bob-rate+ 9.0d0
  "Radians of walking bob per second at full walking speed.")

;;; ---------------------------------------------------------------------
;;; What a hand can hold.

(defgeneric hand-item-name (item)
  (:documentation "A short lowercase noun for ITEM, as the title shows it."))

(defgeneric map-hand-item-boxes (item body function)
  (:documentation
   "Call FUNCTION for each textured box of ITEM as held by BODY.

FUNCTION receives (X Y Z HALF-X HALF-Y HALF-Z TILE &KEY TILT STRETCH-P
EMISSION), like MAP-CRITTER-BOXES with three extras: TILT is a (PITCH YAW
ROLL) triple of radians turning the box about its own centre, STRETCH-P
stretches the whole atlas tile over each face instead of keeping texels
cell-sized, and EMISSION is the face's own light.  Coordinates are in the
grip frame: origin at the palm, x across the fingers, y up the item, z away
from the player."))

(defgeneric hand-item-box-count (item)
  (:documentation "How many boxes MAP-HAND-ITEM-BOXES emits for ITEM."))

(defgeneric emit-hand-item (item body vertices palm right up forward
                            sky-level block-level)
  (:documentation
   "Append ITEM's geometry, held by BODY, to VERTICES.

PALM is the world point of the grip and RIGHT UP FORWARD its unit frame.
The default method emits the item's MAP-HAND-ITEM-BOXES; an item whose
shape is not a heap of boxes -- a slab with genuinely round corners --
adds to that here."))

(defgeneric hand-item-carry-pose (item body)
  (:documentation
   "Where BODY holds ITEM: (X Y Z PITCH YAW ROLL) of the grip in the view
frame, before walking bob is added."))

(defgeneric hand-item-taken-out (item body session)
  (:documentation "Notify ITEM that BODY has just taken it in hand."))

(defmethod hand-item-taken-out (item body session)
  (declare (ignore item body session))
  nil)

(defgeneric hand-item-put-away (item body session)
  (:documentation "Notify ITEM that BODY has just pocketed it."))

(defmethod hand-item-put-away (item body session)
  (declare (ignore item body session))
  nil)

(defgeneric hand-item-use (item body session button)
  (:documentation
   "The player clicked BUTTON while BODY held ITEM.  Return true when the
item did something with the click, so it does not go on to edit the world;
the default hand holds things but does nothing with them."))

(defmethod hand-item-use (item body session button)
  (declare (ignore item body session button))
  nil)

(defgeneric advance-hand-item (item body seconds)
  (:documentation
   "Let ITEM ease its own state by SECONDS while BODY holds it."))

(defmethod advance-hand-item (item body seconds)
  (declare (ignore item body seconds))
  nil)

;;; ---------------------------------------------------------------------
;;; The body.

(defclass player-body ()
  ((hand-item :initform nil :accessor player-body-hand-item)
   ;; Items the body owns but is not holding.  A pocketed item keeps its
   ;; state -- the phone keeps its shell -- and comes back out as it was.
   (pocket :initform nil :accessor player-body-pocket)
   ;; How far the held item has come up into view, 0 (pocketed) to 1
   ;; (held).  Eased toward its target every frame so taking something out
   ;; is a motion rather than a cut.
   (equip-amount :initform 0d0 :type double-float
                 :accessor player-body-equip-amount)
   ;; The walking bob, accumulated from ground speed so standing still
   ;; stands still.
   (bob-phase :initform 0d0 :type double-float
              :accessor player-body-bob-phase)
   (bob-amount :initform 0d0 :type double-float
               :accessor player-body-bob-amount)
   ;; A time base for the small breathing sway of a body that is not
   ;; walking anywhere.
   (clock :initform 0d0 :type double-float :accessor player-body-clock))
  (:documentation "The first-person body: two arms and what they hold."))

(defun put-away-hand-item (body session)
  "Pocket whatever BODY holds and return it, or NIL when the hand was empty."
  (let ((item (player-body-hand-item body)))
    (when item
      (setf (player-body-hand-item body) nil)
      (pushnew item (player-body-pocket body))
      (hand-item-put-away item body session)
      (update-luvcraft-session-title session))
    item))

(defun take-out-hand-item (body session item)
  "Put ITEM in BODY's hand, pocketing whatever was there, and return it."
  (unless (eq item (player-body-hand-item body))
    (put-away-hand-item body session)
    (setf (player-body-pocket body)
          (remove item (player-body-pocket body))
          (player-body-hand-item body) item)
    (hand-item-taken-out item body session)
    (update-luvcraft-session-title session))
  item)

(defun toggle-hand-item (body session type)
  "Take out BODY's item of TYPE -- from the pocket, or newly made -- or put
it away when it is already the thing in hand.  Returns what is now held."
  (let ((held (player-body-hand-item body)))
    (if (typep held type)
        (put-away-hand-item body session)
        (take-out-hand-item body session
                            (or (find-if (lambda (item) (typep item type))
                                         (player-body-pocket body))
                                (make-instance type))))
    (player-body-hand-item body)))

(defun advance-player-body (body session seconds)
  "Ease BODY's held item and walking bob by SECONDS."
  (flet ((ease (current target rate)
           (let ((step (* rate seconds)))
             (cond ((> target current) (min target (+ current step)))
                   ((< target current) (max target (- current step)))
                   (t current)))))
    (incf (player-body-clock body) seconds)
    (setf (player-body-equip-amount body)
          (ease (player-body-equip-amount body)
                (if (player-body-hand-item body) 1d0 0d0)
                +player-body-equip-rate+))
    (alexandria:when-let ((item (player-body-hand-item body)))
      (advance-hand-item item body seconds))
    (let* ((player (luvcraft-session-player session))
           (speed
             (if (and player (player-grounded-p player))
                 (let ((velocity (player-velocity player)))
                   (sqrt (+ (expt (vec3-x velocity) 2)
                            (expt (vec3-z velocity) 2))))
                 0d0))
           (fraction (if player
                         (min 1d0 (/ speed (max 1d-3 (player-walk-speed player))))
                         0d0)))
      (incf (player-body-bob-phase body)
            (* fraction +player-body-bob-rate+ seconds))
      (setf (player-body-bob-amount body)
            (ease (player-body-bob-amount body) fraction 6d0))))
  body)

;;; ---------------------------------------------------------------------
;;; Boxes in a frame.

(defun rotate-frame (right up forward pitch yaw roll)
  "Return RIGHT UP FORWARD turned by YAW about up, then PITCH about the new
right, then ROLL about the new forward.  A positive yaw turns forward toward
the right; a positive pitch tips the top away from the viewer."
  (flet ((rotate (a b angle)
           ;; Rotate vectors A and B in their own plane by ANGLE.
           (let ((c (cos angle)) (s (sin angle)))
             (values
              (make-vec3 (+ (* c (vec3-x a)) (* s (vec3-x b)))
                         (+ (* c (vec3-y a)) (* s (vec3-y b)))
                         (+ (* c (vec3-z a)) (* s (vec3-z b))))
              (make-vec3 (- (* c (vec3-x b)) (* s (vec3-x a)))
                         (- (* c (vec3-y b)) (* s (vec3-y a)))
                         (- (* c (vec3-z b)) (* s (vec3-z a))))))))
    (unless (zerop yaw)
      (multiple-value-setq (forward right) (rotate forward right yaw)))
    (unless (zerop pitch)
      (multiple-value-setq (up forward) (rotate up forward pitch)))
    (unless (zerop roll)
      (multiple-value-setq (right up) (rotate right up roll)))
    (values right up forward)))

(defun frame-point (origin right up forward x y z)
  "The world point at X RIGHT + Y UP + Z FORWARD from ORIGIN."
  (make-vec3 (+ (vec3-x origin) (* x (vec3-x right)) (* y (vec3-x up))
                (* z (vec3-x forward)))
             (+ (vec3-y origin) (* x (vec3-y right)) (* y (vec3-y up))
                (* z (vec3-y forward)))
             (+ (vec3-z origin) (* x (vec3-z right)) (* y (vec3-z up))
                (* z (vec3-z forward)))))

(defun emit-framed-box
    (vertices origin right up forward half-x half-y half-z tile
     sky-level block-level emission stretch-p)
  "Append one box centred on ORIGIN with axes RIGHT UP FORWARD to VERTICES.

The box's local x, y, z are those axes; RIGHT UP FORWARD must be a unit
orthonormal frame, so the face normals come out unit too."
  (let* ((size +block-atlas-tile-size+)
         (atlas-width (* size +block-atlas-tile-count+)))
    (dolist (face *block-faces*)
      (let* ((normal (block-face-neighbor face))
             (nx (voxel-direction-dx normal))
             (ny (voxel-direction-dy normal))
             (nz (voxel-direction-dz normal))
             (world-normal (frame-point (make-vec3 0d0 0d0 0d0)
                                        right up forward nx ny nz))
             (shade (critter-face-shade (vec3-y world-normal))))
        (multiple-value-bind (scale-u scale-v)
            (if stretch-p
                (values 1d0 1d0)
                (critter-face-texture-scales face half-x half-y half-z))
          (let ((origin-u (* 0.5d0 (- 1d0 scale-u)))
                (origin-v (* 0.5d0 (- 1d0 scale-v))))
            (dolist (index '(0 1 2 0 2 3))
              (let* ((corner (nth index (block-face-corners face)))
                     (local-x (* (- (first corner) 0.5d0) 2d0 half-x))
                     (local-y (* (- (second corner) 0.5d0) 2d0 half-y))
                     (local-z (* (- (third corner) 0.5d0) 2d0 half-z))
                     (point (frame-point origin right up forward
                                         local-x local-y local-z)))
                (multiple-value-bind (u v) (block-face-local-uv face corner)
                  (let ((tile-u (+ origin-u (* u scale-u)))
                        (tile-v (+ origin-v (* v scale-v))))
                    (push-block-vertex-components
                     vertices
                     (vec3-x point) (vec3-y point) (vec3-z point)
                     (/ (+ (* tile size) 0.5 (* tile-u (1- size)))
                        atlas-width)
                     (/ (+ 0.5 (* tile-v (1- size))) size)
                     shade
                     (vec3-x world-normal) (vec3-y world-normal)
                     (vec3-z world-normal)
                     sky-level block-level emission
                     +block-face-edge-flush+ +block-face-edge-flush+
                     +block-face-edge-flush+ +block-face-edge-flush+))))))))))
  vertices)

;;; ---------------------------------------------------------------------
;;; The body's boxes.

(defun player-body-light-levels (session)
  "The normalized sky and block light where the eye is."
  (let ((camera (luvcraft-session-camera session)))
    (multiple-value-bind (sky block status)
        (world-light-at (luvcraft-session-world session)
                        (floor (camera-x camera))
                        (floor (camera-y camera))
                        (floor (camera-z camera)))
      (if (eq status :resident)
          (values (/ sky 15.0) (/ block 15.0))
          (values 1.0 0.0)))))

(defun lerp-pose (a b amount)
  (mapcar (lambda (x y) (+ x (* (- y x) amount))) a b))

(defun player-body-hand-pose (body side)
  "Where SIDE's (:LEFT or :RIGHT) palm is in the view frame, and its tilt.

Returns (X Y Z PITCH YAW ROLL).  The right hand is where a held item lives;
the left just hangs into the corner of the view."
  (let* ((clock (player-body-clock body))
         (bob-phase (player-body-bob-phase body))
         (bob (player-body-bob-amount body))
         (breathe (* 0.006d0 (sin (* 1.3d0 clock))))
         (bob-x (* bob 0.020d0 (sin bob-phase)))
         (bob-y (* bob 0.028d0 (abs (sin bob-phase))))
         (item (player-body-hand-item body))
         (equip (player-body-equip-amount body)))
    (ecase side
      (:right
       (let* ((empty '(0.36d0 -0.40d0 0.60d0 -0.30d0 -0.35d0 0.20d0))
              (pocket '(0.40d0 -0.72d0 0.55d0 -0.60d0 -0.35d0 0.20d0))
              (pose (if item
                        (lerp-pose pocket (hand-item-carry-pose item body)
                                   equip)
                        (lerp-pose pocket empty (- 1d0 equip)))))
         (destructuring-bind (x y z pitch yaw roll) pose
           (list (+ x bob-x) (+ y bob-y breathe) z
                 (+ pitch (* bob 0.05d0 (sin bob-phase))) yaw roll))))
      (:left
       (destructuring-bind (x y z pitch yaw roll)
           '(-0.36d0 -0.40d0 0.60d0 -0.30d0 0.35d0 -0.20d0)
         (list (- x bob-x) (+ y (* 0.7d0 bob-y) (- breathe)) z
               (- pitch (* bob 0.05d0 (sin bob-phase))) yaw roll))))))

(defun player-body-grip-frame (session)
  "The world-space frame of the right hand's grip this frame.

Returns the palm point and the unit RIGHT, UP, and FORWARD of the grip.  A
held item's boxes are placed in this frame, and a display drawn on a held
item (the phone's terminal) expresses the camera in it."
  (let ((camera (luvcraft-session-camera session))
        (body (luvcraft-session-body session)))
    (multiple-value-bind (right up forward) (camera-basis camera)
      (destructuring-bind (x y z pitch yaw roll)
          (player-body-hand-pose body :right)
        (multiple-value-bind (grip-right grip-up grip-forward)
            (rotate-frame right up forward pitch yaw roll)
          (values (frame-point (camera-position camera) right up forward x y z)
                  grip-right grip-up grip-forward))))))

(defun emit-player-arm
    (vertices eye right up forward pose sign sky block)
  "Append one arm reaching from off the bottom edge of the view to POSE.

SIGN is +1 for the right arm and -1 for the left.  The forearm is a sleeved
box sloping from the palm back toward the shoulder, which is somewhere
behind and below the eye and never in the picture; the hand is a skin box
at its end."
  (destructuring-bind (x y z pitch yaw roll) pose
    (let* ((shoulder-x (* sign 0.30d0))
           ;; The elbow follows the reach: a hand held far out is on a bent
           ;; arm, so the forearm keeps its slope instead of lying flat.
           (shoulder-z (- z 0.36d0))
           (shoulder-y -0.62d0)
           (dx (- x shoulder-x)) (dy (- y shoulder-y)) (dz (- z shoulder-z))
           (length (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
           ;; The forearm's own frame: its z runs from shoulder to palm.
           (arm-pitch (- (atan dy (sqrt (+ (* dx dx) (* dz dz))))))
           (arm-yaw (atan dx dz)))
      (multiple-value-bind (arm-right arm-up arm-forward)
          (rotate-frame right up forward arm-pitch arm-yaw 0d0)
        ;; The sleeve covers the far two thirds of the arm, so the wrist is
        ;; bare skin for a little way before the hand.
        (let ((cuff (frame-point eye right up forward
                                 (+ shoulder-x (* 0.42d0 dx))
                                 (+ shoulder-y (* 0.42d0 dy))
                                 (+ shoulder-z (* 0.42d0 dz)))))
          (emit-framed-box vertices cuff arm-right arm-up arm-forward
                           0.062d0 0.062d0 (* 0.24d0 length)
                           +player-sleeve-tile+ sky block 0.0 nil))
        (let ((wrist (frame-point eye right up forward
                                  (+ shoulder-x (* 0.88d0 dx))
                                  (+ shoulder-y (* 0.88d0 dy))
                                  (+ shoulder-z (* 0.88d0 dz)))))
          (emit-framed-box vertices wrist arm-right arm-up arm-forward
                           0.055d0 0.055d0 (* 0.10d0 length)
                           +player-skin-tile+ sky block 0.0 nil)))
      ;; The hand itself sits at the palm, turned as the pose says: a fist
      ;; when empty, a grip when holding.
      (multiple-value-bind (hand-right hand-up hand-forward)
          (rotate-frame right up forward pitch yaw roll)
        (let ((palm (frame-point eye right up forward x y z)))
          (emit-framed-box vertices palm hand-right hand-up hand-forward
                           0.080d0 0.062d0 0.070d0
                           +player-skin-tile+ sky block 0.0 nil))))))

(defun emit-player-body (vertices session)
  "Append SESSION's first-person body -- arms, hands, held item -- to VERTICES."
  (let* ((body (luvcraft-session-body session))
         (camera (luvcraft-session-camera session))
         (eye (camera-position camera)))
    (multiple-value-bind (right up forward) (camera-basis camera)
      (multiple-value-bind (sky block) (player-body-light-levels session)
        (let ((right-pose (player-body-hand-pose body :right))
              (left-pose (player-body-hand-pose body :left)))
          (emit-player-arm vertices eye right up forward left-pose -1 sky block)
          (emit-player-arm vertices eye right up forward right-pose 1 sky block)
          (let ((item (player-body-hand-item body)))
            (when item
              (multiple-value-bind (palm grip-right grip-up grip-forward)
                  (player-body-grip-frame session)
                (emit-hand-item item body vertices
                                palm grip-right grip-up grip-forward
                                sky block)))))))
    vertices))

(defmethod emit-hand-item (item body vertices palm right up forward
                           sky-level block-level)
  (map-hand-item-boxes
   item body
   (lambda (bx by bz half-x half-y half-z tile
            &key (tilt '(0d0 0d0 0d0)) stretch-p (emission 0.0))
     (multiple-value-bind (box-right box-up box-forward)
         (apply #'rotate-frame right up forward tilt)
       (emit-framed-box
        vertices
        (frame-point palm right up forward bx by bz)
        box-right box-up box-forward
        half-x half-y half-z tile
        sky-level block-level emission stretch-p))))
  vertices)

;;; ---------------------------------------------------------------------
;;; A slab with round corners.

(defun rounded-rectangle-outline (half-x half-y radius segments)
  "Return the (X . Y) corners of a rounded rectangle, counter-clockwise
from the right side's lower end, with SEGMENTS arcs per corner."
  (let ((points nil)
        (x (- half-x radius))
        (y (- half-y radius)))
    ;; Corner centres and the angle each arc starts at: lower right, upper
    ;; right, upper left, lower left, each arc turning a quarter.
    (loop for (cx cy start) in (list (list x (- y) (* -0.5d0 pi))
                                     (list x y 0d0)
                                     (list (- x) y (* 0.5d0 pi))
                                     (list (- x) (- y) pi))
          do (dotimes (i (1+ segments))
               (let ((angle (+ start (* (/ i segments) 0.5d0 pi))))
                 (push (cons (+ cx (* radius (cos angle)))
                             (+ cy (* radius (sin angle))))
                       points))))
    (nreverse points)))

(defun emit-rounded-slab
    (vertices origin right up forward half-x half-y half-z radius tile
     sky-level block-level &key (segments 6) (emission 0.0))
  "Append a slab centred on ORIGIN with axes RIGHT UP FORWARD whose four
corners are rounded to RADIUS in the RIGHT/UP plane: two flat faces and a
band of quads around the edge, all wearing TILE stretched once around."
  (let* ((outline (rounded-rectangle-outline half-x half-y radius segments))
         (count (length outline))
         (size +block-atlas-tile-size+)
         (atlas-width (* size +block-atlas-tile-count+)))
    (labels ((tile-u (u) (/ (+ (* tile size) 0.5 (* u (1- size))) atlas-width))
             (tile-v (v) (/ (+ 0.5 (* v (1- size))) size))
             (world-normal (nx ny nz)
               (frame-point (make-vec3 0d0 0d0 0d0) right up forward nx ny nz))
             (vertex (x y z normal u v)
               (let ((point (frame-point origin right up forward x y z)))
                 (push-block-vertex-components
                  vertices
                  (vec3-x point) (vec3-y point) (vec3-z point)
                  (tile-u u) (tile-v v)
                  (critter-face-shade (vec3-y normal))
                  (vec3-x normal) (vec3-y normal) (vec3-z normal)
                  sky-level block-level emission
                  +block-face-edge-flush+ +block-face-edge-flush+
                  +block-face-edge-flush+ +block-face-edge-flush+)))
             (face-uv (x y)
               (values (+ 0.5d0 (/ x (* 2 half-x)))
                       (- 0.5d0 (/ y (* 2 half-y))))))
      ;; The two flat faces, as fans from the centre.  The front (-z) face
      ;; is wound to face the eye; the back the other way.
      (dolist (side '(-1 1))
        (let* ((z (* side half-z))
               (normal (world-normal 0d0 0d0 side)))
          (loop for i below count
                for a = (nth i outline)
                for b = (nth (mod (1+ i) count) outline)
                do (multiple-value-bind (au av) (face-uv (car a) (cdr a))
                     (multiple-value-bind (bu bv) (face-uv (car b) (cdr b))
                       (vertex 0d0 0d0 z normal 0.5d0 0.5d0)
                       (if (minusp side)
                           (progn (vertex (car b) (cdr b) z normal bu bv)
                                  (vertex (car a) (cdr a) z normal au av))
                           (progn (vertex (car a) (cdr a) z normal au av)
                                  (vertex (car b) (cdr b) z normal bu bv))))))))
      ;; The rim: one quad per outline edge, its normal the edge's outward
      ;; perpendicular, the tile running once around it.
      (loop for i below count
            for a = (nth i outline)
            for b = (nth (mod (1+ i) count) outline)
            for ex = (- (car b) (car a))
            for ey = (- (cdr b) (cdr a))
            for length = (max 1d-9 (sqrt (+ (* ex ex) (* ey ey))))
            for normal = (world-normal (/ ey length) (- (/ ex length)) 0d0)
            for u0 = (/ i count)
            for u1 = (/ (1+ i) count)
            do (vertex (car a) (cdr a) (- half-z) normal u0 0d0)
               (vertex (car b) (cdr b) (- half-z) normal u1 0d0)
               (vertex (car b) (cdr b) half-z normal u1 1d0)
               (vertex (car a) (cdr a) (- half-z) normal u0 0d0)
               (vertex (car b) (cdr b) half-z normal u1 1d0)
               (vertex (car a) (cdr a) half-z normal u0 1d0))))
  vertices)

(defun player-body-vertices (session)
  "Build the block-pipeline vertex stream for the player's body this frame."
  (let ((vertices
          (make-array (* +player-body-box-limit+ +critter-vertices-per-box+
                         +block-mesh-floats-per-vertex+)
                      :element-type 'single-float :adjustable t
                      :fill-pointer 0)))
    (emit-player-body vertices session)
    (unless (<= (length vertices)
                (* +player-body-box-limit+ +critter-vertices-per-box+
                   +block-mesh-floats-per-vertex+))
      (error "The body emitted more than its ~D boxes." +player-body-box-limit+))
    vertices))
