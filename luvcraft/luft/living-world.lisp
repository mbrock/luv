;;;; Share the living world (#EA88ON, #U4TBMP): the host owns the turtles,
;;;; balls, springs, and sky clock (#8EZK9F), and guests draw them.
;;;;
;;;; A host's session sends an ENTITY-SNAPSHOT-MESSAGE about 25 times a
;;;; second, stamped with the simulation tick: every turtle, every ball, and
;;;; one :SKY entity carrying the clock and the day settings.  A guest's world
;;;; never simulates them.  Each entity it hears of gets a REMOTE-TRACK, the
;;;; one a remote player is drawn through (#VM5TN6), and each frame a turtle
;;;; is set to where its track shows it, so rides, focus, and drawing read an
;;;; ordinary turtle.  The guest's sky runs on between snapshots and is eased
;;;; to the host's.  A guest's throw is a THROW-MESSAGE the host performs.
;;;;
;;;; A ride begins and ends by reliable RIDE-MESSAGEs.  The rider's own
;;;; world moves the turtle under its reins, and the rider's pose, which then
;;;; carries the turtle's velocity, yaw, and id, is where the host puts it;
;;;; the turtle's mind follows the rider until the ride ends.
;;;;
;;;; Particles are not sent: each view derives them from the journal's
;;;; confirmed events.
(in-package #:luvcraft.luft.game)

(defparameter *entity-period* 1/25
  "Seconds between a host's entity snapshots.")

(defstruct (living-state (:conc-name living-))
  ;; Turtle -> its entity id, on the host and on a guest.
  (ids (make-hash-table :test #'eq) :read-only t)
  (next-id 0)
  ;; A guest's (KIND . ID) -> REPLICA of each entity its host shares, and
  ;; the tick of the newest snapshot it has taken.
  (replicas (make-hash-table :test #'equal) :read-only t)
  (newest-tick -1)
  ;; Turtle id -> the id of the player riding it.
  (rides (make-hash-table) :read-only t)
  (snapshot-due 0d0)
  ;; The guest peers a host has told of the rides in progress.
  (told nil)
  ;; A guest's day settings it has asked its host for and not yet seen
  ;; granted: setting -> (VALUE DEADLINE) (company.lisp).
  (sky-wishes nil))

(defstruct (replica (:constructor make-replica (track object)))
  "A guest's copy of one host entity: its track, and the turtle it moves, if any."
  (track nil :read-only t)
  (object nil :read-only t))

(defun living (world)
  (or (world-living world) (setf (world-living world) (make-living-state))))

(defun replica-world-p (world)
  "Whether WORLD draws its turtles, balls, and sky from a host's snapshots."
  (typep (world-session world) 'guest-session))

(defun world-local-player-id (world)
  (let ((session (world-session world)))
    (and session (player-id (session-local-player session)))))

;;; Coordinates.  Luvcraft's turtles and solver are Y-up and centred on the
;;; origin; the wire speaks Luft's cells, which are Z-up and offset by 1024.

(defun luft-vector (x y z)
  "Luvcraft's X Y Z as a wire vector of Luft cells."
  (vector (+ 1024d0 x) (+ 1024d0 z) (coerce y 'double-float)))

(defun luvcraft-vector (vector)
  "Wire VECTOR of Luft cells as a list of Luvcraft's X Y Z."
  (list (- (aref vector 0) 1024d0) (coerce (aref vector 2) 'double-float)
        (- (aref vector 1) 1024d0)))

(defun luft-velocity (x y z)
  "Luvcraft's velocity X Y Z along Luft's axes, short of the wire's speed limit."
  (let* ((v (vector (coerce x 'double-float) (coerce z 'double-float) (coerce y 'double-float)))
         (speed (sqrt (reduce #'+ v :key (lambda (c) (* c c))))))
    (if (> speed 512d0) (map 'vector (lambda (c) (* c (/ 512d0 speed))) v) v)))

(defun double-vector (vector)
  (map 'simple-vector (lambda (x) (coerce x 'double-float)) vector))

;;; Entity ids.

(defun turtle-entity-id (world turtle)
  (let ((living (living world)))
    (or (gethash turtle (living-ids living))
        (setf (gethash turtle (living-ids living)) (incf (living-next-id living))))))

(defun find-world-turtle (world id)
  (let ((ids (living-ids (living world))))
    (find-if (lambda (turtle) (eql (gethash turtle ids) id)) (world-animals world))))

;;; Balls, as a host simulates them or a guest is shown them.

(defun world-ball-list (world)
  "Each ball in WORLD as (ID CENTRE QUATERNION KIND RADIUS): CENTRE is the
solver's Y-up (X Y Z), QUATERNION its (X Y Z W), KIND its BODY-KIND, and
RADIUS the drawn one."
  (if (replica-world-p world)
      (loop for replica being the hash-values of (living-replicas (living world))
              using (hash-key key)
            for shown = (and (eq (car key) :body) (track-shown (replica-track replica)))
            for kind = (and shown (ignore-errors
                                   (aref luvcraft::*body-kinds*
                                         (luvcraft::body-kind-index
                                          (getf (pose-snapshot-state shown) :kind)))))
            when kind
              collect (list (cdr key)
                            (mapcar (lambda (x) (coerce x 'single-float))
                                    (luvcraft-vector (pose-snapshot-position shown)))
                            (map 'list (lambda (x) (coerce x 'single-float))
                                 (or (pose-snapshot-orientation shown) #(0d0 0d0 0d0 1d0)))
                            kind
                            (* (luvcraft::body-kind-radius kind) (luvcraft::body-kind-draw-scale kind))))
      (let ((physics (world-physics world)) (balls nil))
        (when physics
          (luvcraft::do-physics-bodies (handle physics)
            (multiple-value-bind (set row) (luvcraft::physics-body-set-and-local physics handle)
              (let* ((columns (luvcraft::physics-body-columns-for-set physics set))
                     (kind (aref luvcraft::*body-kinds*
                                 (aref (luvcraft::physics-body-columns-kind-lane columns) row))))
                (push (list handle
                            (multiple-value-list (luvcraft::physics-body-position physics handle))
                            (list (aref (luvcraft::physics-body-columns-qx-lane columns) row)
                                  (aref (luvcraft::physics-body-columns-qy-lane columns) row)
                                  (aref (luvcraft::physics-body-columns-qz-lane columns) row)
                                  (aref (luvcraft::physics-body-columns-qw-lane columns) row))
                            kind
                            (* (aref (luvcraft::physics-body-columns-radius-lane columns) row)
                               (luvcraft::body-kind-draw-scale kind)))
                      balls)))))
        (nreverse balls))))

;;; The host's snapshot.

(defun turtle-entity-state (world turtle)
  (let ((p (luvcraft::critter-position turtle)) (v (luvcraft::critter-velocity turtle)))
    (wire:make-entity-state
     :id (turtle-entity-id world turtle) :kind :turtle
     :position (luft-vector (vec3:vec3-x p) (vec3:vec3-y p) (vec3:vec3-z p))
     :velocity (luft-velocity (vec3:vec3-x v) (vec3:vec3-y v) (vec3:vec3-z v))
     :state (list :yaw (coerce (luvcraft::critter-yaw turtle) 'double-float)
                  :gait (coerce (luvcraft::turtle-gait-phase turtle) 'double-float)
                  :resting (and (luvcraft::turtle-resting-p turtle) t)
                  :seed (luvcraft::critter-seed turtle)))))

(defun ball-entity-states (world)
  (let ((physics (world-physics world)))
    (loop for (id centre quaternion kind) in (world-ball-list world)
          for q = (map 'vector (lambda (x) (coerce x 'double-float)) quaternion)
          for norm = (sqrt (reduce #'+ q :key (lambda (x) (* x x))))
          collect (wire:make-entity-state
                   :id id :kind :body
                   :position (apply #'luft-vector centre)
                   :velocity (multiple-value-call #'luft-velocity
                               (luvcraft::physics-body-velocity physics id))
                   :orientation (if (plusp norm)
                                    (map 'vector (lambda (x) (/ x norm)) q)
                                    (vector 0d0 0d0 0d0 1d0))
                   :state (list :kind (luvcraft::body-kind-name kind))))))

(defun sky-entity-state (world)
  (let ((clock (world-sky-clock world)))
    (wire:make-entity-state
     :id 0 :kind :sky
     :state (list :day (coerce (luvcraft::sky-clock-day-fraction clock) 'double-float)
                  :rate (coerce (luvcraft::sky-clock-rate clock) 'double-float)
                  :paused (and (luvcraft::sky-clock-paused-p clock) t)
                  :pinned (let ((pinned (luvcraft::sky-clock-pinned-day-fraction clock)))
                            (and pinned (coerce pinned 'double-float)))
                  :tilt (coerce (world-sun-orbit-tilt world) 'double-float)
                  :elapsed (coerce (world-sky-elapsed world) 'double-float)))))

(defun world-entity-snapshot (world)
  "WORLD's turtles, balls, and sky at its simulation tick, as the wire has them."
  (wire:make-entity-snapshot
   :tick (world-tick world)
   :entities (append (list (sky-entity-state world))
                     (mapcar (lambda (turtle) (turtle-entity-state world turtle))
                             (world-animals world))
                     (ball-entity-states world))))

;;; A guest's replicas.

(defun entity-pose-snapshot (entity sent)
  "ENTITY, sent at SENT seconds of its host's simulation, as a track's snapshot."
  (let ((state (wire:entity-state-state entity)))
    (make-pose-snapshot
     :sent sent
     :position (double-vector (wire:entity-state-position entity))
     :velocity (double-vector (wire:entity-state-velocity entity))
     :yaw (coerce (or (getf state :yaw) 0d0) 'double-float)
     :gait (coerce (or (getf state :gait) 0d0) 'double-float)
     :orientation (double-vector (wire:entity-state-orientation entity))
     :state state)))

(defun take-sky (world state)
  "Set WORLD's clock and day settings to its host's, easing small differences
in the time of day so that the sky does not judder."
  (let* ((clock (world-sky-clock world))
         (day (getf state :day))
         (ahead (- (mod (+ (- day (luvcraft::sky-clock-day-fraction clock)) 0.5d0) 1d0) 0.5d0))
         (elapsed (getf state :elapsed)))
    (setf (luvcraft::sky-clock-rate clock) (getf state :rate)
          (luvcraft::sky-clock-paused-p clock) (getf state :paused)
          (luvcraft::sky-clock-pinned-day-fraction clock) (getf state :pinned)
          (world-sun-orbit-tilt world) (coerce (getf state :tilt) 'single-float)
          (luvcraft::sky-clock-day-fraction clock)
          (if (> (abs ahead) 1/240)       ; six minutes of the day
              day
              (mod (+ (luvcraft::sky-clock-day-fraction clock) (* 0.25d0 ahead)) 1d0))
          (world-sky-elapsed world)
          (coerce (if (> (abs (- elapsed (world-sky-elapsed world))) 1d0)
                      elapsed
                      (+ (world-sky-elapsed world) (* 0.25d0 (- elapsed (world-sky-elapsed world)))))
                  'single-float)))
  (keep-sky-wishes world))

(defun raise-replica (world kind id entity)
  (make-replica
   (make-remote-track)
   (when (eq kind :turtle)
     (let* ((state (wire:entity-state-state entity))
            (yaw (coerce (getf state :yaw 0d0) 'double-float))
            (turtle (make-instance 'luvcraft::turtle
                                   :seed (getf state :seed 0) :yaw yaw :heading yaw
                                   :position (apply #'vec3:make-vec3
                                                    (luvcraft-vector
                                                     (wire:entity-state-position entity))))))
       (setf (gethash turtle (living-ids (living world))) id
             (world-animals world) (append (world-animals world) (list turtle)))
       turtle))))

(defun forget-replica (world key replica)
  (let ((living (living world)) (turtle (replica-object replica)))
    (remhash key (living-replicas living))
    (when turtle
      (remhash turtle (living-ids living))
      (remhash (cdr key) (living-rides living))
      (setf (world-animals world) (remove turtle (world-animals world))))))

(defun take-entity-snapshot (world snapshot &optional (now (wall-seconds)))
  "Take a host's SNAPSHOT onto WORLD's replicas: each entity's track hears of
it, entities it no longer names are forgotten, and the sky follows.  One no
newer than the newest taken is dropped, as datagrams may cross."
  (let ((living (living world))
        (tick (wire:entity-snapshot-tick snapshot)))
    (when (> tick (living-newest-tick living))
      (setf (living-newest-tick living) tick)
      (let ((sent (/ tick (float +simulation-ticks-per-second+ 1d0)))
            (named (make-hash-table :test #'equal))
            (replicas (living-replicas living)))
        (dolist (entity (wire:entity-snapshot-entities snapshot))
          (let ((kind (wire:entity-state-kind entity)) (id (wire:entity-state-id entity)))
            (if (eq kind :sky)
                (take-sky world (wire:entity-state-state entity))
                (let* ((key (cons kind id))
                       (replica (or (gethash key replicas)
                                    (setf (gethash key replicas)
                                          (raise-replica world kind id entity)))))
                  (setf (gethash key named) t)
                  (push-pose-snapshot (replica-track replica) (entity-pose-snapshot entity sent)
                                      now)))))
        (loop for key in (loop for key being the hash-keys of replicas
                               unless (gethash key named) collect key)
              do (forget-replica world key (gethash key replicas)))))
    world))

(defun show-replica-turtle (turtle shown)
  "Stand TURTLE where its track SHOWN it, walking or resting as it was."
  (destructuring-bind (x y z) (luvcraft-vector (pose-snapshot-position shown))
    (let ((p (luvcraft::critter-position turtle)) (v (luvcraft::critter-velocity turtle))
          (velocity (pose-snapshot-velocity shown)))
      (setf (vec3:vec3-x p) x (vec3:vec3-y p) y (vec3:vec3-z p) z
            (vec3:vec3-x v) (svref velocity 0) (vec3:vec3-y v) (svref velocity 2)
            (vec3:vec3-z v) (svref velocity 1))))
  (setf (luvcraft::critter-yaw turtle) (pose-snapshot-yaw shown)
        (luvcraft::turtle-heading turtle) (pose-snapshot-yaw shown)
        (luvcraft::turtle-gait-phase turtle) (pose-snapshot-gait shown)
        (luvcraft::turtle-resting-p turtle) (getf (pose-snapshot-state shown) :resting)
        (luvcraft::critter-grounded-p turtle) t))

(defun advance-world-replicas (world seconds)
  "Move a guest WORLD's replicas on by SECONDS.  The turtle its own player
rides goes under the reins here, as the host's would; the rest go where
their tracks show them."
  (let* ((living (living world))
         (now (wall-seconds))
         (me (world-local-player-id world))
         (contact (make-instance 'animal-contact
                                 :solid (terrain::inspection-source-solid (world-scene world)))))
    (maphash (lambda (key replica)
               (let ((shown (advance-remote-track (replica-track replica) now seconds))
                     (turtle (replica-object replica)))
                 (when turtle
                   (cond ((equal me (gethash (cdr key) (living-rides living)))
                          (let ((p (animal-luft-position turtle)))
                            (when (ground-resident-p world (floor (vec3:vec3-x p)) (floor (vec3:vec3-y p)))
                              (luvcraft::advance-critter turtle contact seconds))))
                         (shown (show-replica-turtle turtle shown))))))
             (living-replicas living))))

;;; Rides.

(defun turtle-rider-id (world turtle)
  "The id of the player riding TURTLE in WORLD, or NIL."
  (let ((living (living world)))
    (values (gethash (gethash turtle (living-ids living)) (living-rides living)))))

(defun ridden-by-another-p (world player turtle)
  (let ((rider (turtle-rider-id world turtle)))
    (and rider (not (equal rider (player-id player))))))

(defun begin-world-ride (world player turtle)
  "PLAYER mounts TURTLE: a host records and announces it; a guest asks.
NIL when someone else is known to be riding it."
  (let ((id (turtle-entity-id world turtle)) (session (world-session world)))
    (unless (ridden-by-another-p world player turtle)
      (setf (gethash id (living-rides (living world))) (player-id player))
      (when session
        (session-broadcast session (wire:make-ride-message :rider (player-id player) :entity id
                                                           :action :begin)))
      id)))

(defun end-world-ride (world player turtle)
  "PLAYER gets off TURTLE, if it was riding it."
  (let* ((living (living world))
         (id (gethash turtle (living-ids living)))
         (session (world-session world)))
    (when (and id (equal (gethash id (living-rides living)) (player-id player)))
      (remhash id (living-rides living))
      (when session
        (session-broadcast session (wire:make-ride-message :rider (player-id player) :entity id
                                                           :action :end))))))

(defun carry-turtle-by-rider (turtle rider seconds)
  "On the host, put TURTLE where its remote RIDER's pose says it is, and let
its mind follow: it heads and walks the way the rider takes it."
  (let ((shown (track-shown (player-track rider))))
    (when shown
      (destructuring-bind (x y z) (luvcraft-vector (pose-snapshot-position shown))
        (let* ((p (luvcraft::critter-position turtle)) (v (luvcraft::critter-velocity turtle))
               (velocity (pose-snapshot-velocity shown))
               (speed (sqrt (+ (expt (svref velocity 0) 2) (expt (svref velocity 1) 2))))
               (yaw (- (/ pi 2) (pose-snapshot-yaw shown)))
               (walking-p (> speed 0.05d0)))
          (setf (vec3:vec3-x p) x (vec3:vec3-y p) y (vec3:vec3-z p) z
                (vec3:vec3-x v) (svref velocity 0) (vec3:vec3-y v) (svref velocity 2)
                (vec3:vec3-z v) (svref velocity 1)
                (luvcraft::critter-yaw turtle) yaw
                (luvcraft::turtle-heading turtle) yaw
                (luvcraft::turtle-resting-p turtle) (not walking-p)
                (luvcraft::turtle-mood-seconds turtle) (max 0.75d0 (luvcraft::turtle-mood-seconds turtle))
                (luvcraft::critter-grounded-p turtle) t)
          (when walking-p
            (incf (luvcraft::turtle-gait-phase turtle)
                  (/ (* 2d0 pi speed seconds) luvcraft::+turtle-step-length+))))))))

(defun remote-rider (world turtle)
  "The remote player riding TURTLE in a host's WORLD, or NIL."
  (let* ((id (turtle-rider-id world turtle))
         (player (and id (find-world-player world id))))
    (and player (player-track player) player)))

(defun dress-riding-pose (world player pose)
  "POSE, with the velocity, yaw, and id of the turtle PLAYER rides, if any:
the rider's pose carries the turtle."
  (let* ((living (living world))
         (turtle (loop for id being the hash-keys of (living-rides living) using (hash-value rider)
                       when (equal rider (player-id player))
                         return (find-world-turtle world id))))
    (when (and pose turtle)
      (let ((v (luvcraft::critter-velocity turtle)))
        (setf (wire:pose-velocity pose) (luft-velocity (vec3:vec3-x v) (vec3:vec3-y v) (vec3:vec3-z v))
              (wire:pose-yaw pose) (wire:wrap-yaw (- (/ pi 2) (luvcraft::critter-yaw turtle)))
              (wire:pose-ride pose) (turtle-entity-id world turtle))))
    pose))

(defun release-ridden-turtle (turtle rider)
  "The ride is over: TURTLE stands where RIDER last said and has its mind back."
  (let* ((track (and rider (player-track rider)))
         (snapshots (and track (track-snapshots track))))
    (when (and snapshots (plusp (length snapshots)))
      (destructuring-bind (x y z)
          (luvcraft-vector (pose-snapshot-position (aref snapshots (1- (length snapshots)))))
        (let ((p (luvcraft::critter-position turtle)))
          (setf (vec3:vec3-x p) x (vec3:vec3-y p) y (vec3:vec3-z p) z)))))
  (luvcraft::urge-critter turtle 0d0 0d0 0d0)
  turtle)

;;; Messages.

(defmethod handle-session-message ((session host-session) peer (message wire:ride-message))
  (let* ((player (guest-peer-player session peer))
         (world (session-world session))
         (rides (living-rides (living world)))
         (id (wire:ride-message-entity message))
         (turtle (find-world-turtle world id)))
    (when player
      (ecase (wire:ride-message-action message)
        (:begin
         (if (and turtle (member (gethash id rides) (list nil (player-id player)) :test #'equal))
             (progn (setf (gethash id rides) (player-id player))
                    (session-broadcast session (wire:make-ride-message
                                                :rider (player-id player) :entity id :action :begin)))
             (net:send-message (session-endpoint session)
                               (wire:make-ride-message :rider (player-id player) :entity id
                                                       :action :refused)
                               peer)))
        (:end
         (when (equal (gethash id rides) (player-id player))
           (remhash id rides)
           (when turtle (release-ridden-turtle turtle player))
           (session-broadcast session (wire:make-ride-message
                                       :rider (player-id player) :entity id :action :end))))
        (:refused)))))

(defmethod handle-session-message ((session guest-session) peer (message wire:ride-message))
  (declare (ignore peer))
  (let* ((world (session-world session))
         (rides (living-rides (living world)))
         (id (wire:ride-message-entity message))
         (rider (wire:ride-message-rider message)))
    (ecase (wire:ride-message-action message)
      (:begin (setf (gethash id rides) rider))
      (:end (when (equal (gethash id rides) rider) (remhash id rides)))
      (:refused
       (when (equal (gethash id rides) rider)
         (remhash id rides))
       ;; Someone else is on it: get off.
       (let* ((viewer (session-viewer session))
              (focus (and viewer (render:viewer-modal-focus viewer))))
         (when (and (typep focus 'luvcraft::critter-ride)
                    (eql id (gethash (luvcraft::critter-ride-critter focus)
                                     (living-ids (living world)))))
           (render::unfocus-viewer viewer)))))))

(defmethod handle-session-message ((session guest-session) peer (message wire:entity-snapshot-message))
  (declare (ignore peer))
  (take-entity-snapshot (session-world session) (wire:entity-snapshot-message-snapshot message)))

(defmethod handle-session-message ((session host-session) peer (message wire:throw-message))
  (let ((player (guest-peer-player session peer)))
    (when player
      (handler-case
          (destructuring-bind (x y z) (luvcraft-vector (wire:throw-message-position message))
            (let ((v (wire:throw-message-velocity message)))
              (luvcraft::spawn-body-of-kind
               (ensure-world-physics (session-world session)) (wire:throw-message-kind message)
               x y z :vx (aref v 0) :vy (aref v 2) :vz (aref v 1))))
        (error (condition)
          (session-log session "Refused ~A's throw: ~A" (player-id player) condition))))))

(defun throw-world-body (world kind x y z vx vy vz)
  "Throw a body of KIND from Luvcraft's X Y Z at VX VY VZ: on the host, or
by asking it.  Return the host's body handle, or NIL for a request."
  (if (replica-world-p world)
      (progn
        (session-broadcast (world-session world)
                           (wire:make-throw-message :kind kind :position (luft-vector x y z)
                                                    :velocity (luft-velocity vx vy vz)))
        nil)
      (luvcraft::spawn-body-of-kind (ensure-world-physics world) kind x y z
                                    :vx vx :vy vy :vz vz)))

;;; The host's frame boundary.

(defgeneric advance-living-world (session now)
  (:documentation "Share SESSION's living world at a frame boundary.")
  (:method ((session world-session) now)
    (declare (ignore now))
    nil))

(defmethod advance-living-world ((session host-session) now)
  (let* ((world (session-world session))
         (living (living world))
         (peers (mapcar #'car (session-guests session))))
    ;; A guest that has left takes its rides with it.
    (maphash (lambda (id rider)
               (unless (find-world-player world rider)
                 (remhash id (living-rides living))
                 (session-broadcast session (wire:make-ride-message :rider rider :entity id
                                                                    :action :end))))
             (living-rides living))
    (dolist (peer peers)
      (unless (member peer (living-told living))
        (maphash (lambda (id rider)
                   (net:send-message (session-endpoint session)
                                     (wire:make-ride-message :rider rider :entity id :action :begin)
                                     peer))
                 (living-rides living))))
    (setf (living-told living) peers)
    (when (and peers (>= now (living-snapshot-due living)))
      (setf (living-snapshot-due living) (+ (max now (living-snapshot-due living)) *entity-period*))
      (session-broadcast session (wire:make-entity-snapshot-message
                                  :snapshot (world-entity-snapshot world))))))

;;; Views.  Each view keeps its own previous poses, so two windows on one
;;; world each have their own motion history for temporal rendering.

(defun remember-viewer-poses (viewer)
  "Keep the turtles' boxes and balls' centres as VIEWER last saw them."
  (let ((world (viewer-world viewer))
        (figures (viewer-previous-animal-figures viewer))
        (centres (viewer-previous-ball-centers viewer)))
    (clrhash figures)
    (dolist (animal (world-animals world))
      (setf (gethash animal figures) (animal-figures animal)))
    (clrhash centres)
    (loop for (id centre) in (world-ball-list world)
          do (setf (gethash id centres) centre))))

(defun derive-viewer-particles (viewer)
  "Burst the blocks that the journal's confirmed events since VIEWER last
looked have removed, whoever removed them."
  (let* ((scene (world-scene (viewer-world viewer)))
         (source (and (typep scene 'terrain:streaming-scene) (terrain:streaming-scene-source scene))))
    (when (typep source 'terrain:world-source)
      (let* ((journal (terrain:streaming-scene-journal scene))
             (events (terrain:world-journal-events journal))
             (watermark (terrain:world-journal-watermark journal))
             (from (or (viewer-particle-sequence viewer) watermark)))
        (loop for index from (max 0 (- from watermark)) below (length events)
              for event = (aref events index)
              for before = (values (journal-value-block (terrain:world-event-before event)))
              when (and before (null (values (journal-value-block (terrain:world-event-after event)))))
                do (luvcraft::smash-block-particles
                    (viewer-particles viewer) before
                    (apply #'luvcraft.world:make-world-coordinate
                           (game:site-luvcraft-cell source (terrain:world-event-site event)))))
        (setf (viewer-particle-sequence viewer) (terrain:world-journal-head journal))))))
