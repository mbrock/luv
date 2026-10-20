(in-package #:luvcraft.agent)

;;; Fantus, NRK's purple felt toddler, as a third embodied agent.  Like the
;;; cat it shares the gnome's conversation surface and authored state, and
;;; brings its own dense fragment program, :FANTUS-SDF.

(defparameter *fantus-name* "Fantus")
(defparameter *fantus-body-centre* 0.66)
(defparameter *fantus-face-height* 1.04)
(defparameter *fantus-audience-distance* 2.6)

(defclass fantus (embodied-agent) ()
  (:documentation
   "A seated SDF-rendered Fantus carrying the shared embodied WORLD-AGENT surface."))

(defclass fantus-body-overlay (sdf-agent-body-overlay) ()
  (:documentation "Fantus's :FANTUS-SDF body overlay."))

(defun fantus-stature ()
  "How many world cells one Fantus figure unit occupies."
  (float luvcraft.shaders::*fantus-stature* 1.0))

(defun fantus-head-scale ()
  (float luvcraft.shaders::*fantus-head-size* 1.0))

(defun fantus-figure-height ()
  "The top of Fantus's head in figure space."
  (+ 1.00 (* 0.32 (fantus-head-scale))))

(defun fantus-figure-radius ()
  "A conservative sphere about the body centre following the head and ear knobs.

The ear's farthest felt is bounded by its hinge plus its long radius, which
holds whatever way the spread and roll turn it."
  (let* ((head (fantus-head-scale))
         (ear (float luvcraft.shaders::*fantus-ear-size* 1.0))
         ;; Hinge and centre from FANTUS-EAR-POINT, plus the ear's longest
         ;; radius after the paddle widening.
         (ear-reach-x (* head (+ 0.290 (* 0.200 ear) (* 0.33 ear))))
         (ear-reach-y (+ (- 1.00 *fantus-body-centre*)
                         (* head (+ 0.045 (* 0.33 ear))))))
    (+ 0.05
       (max (- (fantus-figure-height) *fantus-body-centre*)
            (sqrt (+ (* ear-reach-x ear-reach-x)
                     (* ear-reach-y ear-reach-y)))
            ;; Feet at the end of the legs.
            0.80))))

(defmethod embodied-agent-name ((fantus fantus))
  (declare (ignore fantus))
  *fantus-name*)

(defmethod embodied-agent-accent-ink ((fantus fantus))
  ;; The felt.
  (display-color 0.45 0.20 0.72))

(defmethod embodied-agent-body-height ((fantus fantus))
  (declare (ignore fantus))
  (* (fantus-figure-height) (fantus-stature)))

(defmethod embodied-agent-head-position ((fantus fantus) &optional (lift 0.0))
  (luvcraft::make-vec3 (luvcraft::body-x fantus)
                       (+ (luvcraft::body-y fantus)
                          (embodied-agent-body-height fantus)
                          lift)
                       (luvcraft::body-z fantus)))

(defmethod embodied-agent-face-position ((fantus fantus) &optional (lift 0.0))
  (luvcraft::make-vec3 (luvcraft::body-x fantus)
                       (+ (luvcraft::body-y fantus)
                          (* *fantus-face-height* (fantus-stature)) lift)
                       (luvcraft::body-z fantus)))

(defmethod embodied-agent-audience-distance ((fantus fantus))
  (declare (ignore fantus))
  *fantus-audience-distance*)

(defmethod embodied-agent-body-sphere ((fantus fantus))
  "Fantus's conservative proxy, breathing a little faster than a cat."
  (let* ((phase (/ (get-internal-real-time)
                   (float internal-time-units-per-second 1.0)))
         (breath (* 0.008 (sin (* phase 2.4))))
         (stature (fantus-stature)))
    (values (luvcraft::body-x fantus)
            (+ (luvcraft::body-y fantus) (* *fantus-body-centre* stature) breath)
            (luvcraft::body-z fantus)
            (* (fantus-figure-radius) stature))))

(defmethod embodied-agent-ray-distance
    ((fantus fantus) origin direction max-distance)
  "Return where a ray enters Fantus's conservative proxy sphere."
  (multiple-value-bind (x y z radius) (embodied-agent-body-sphere fantus)
    (let* ((ox (- (luvcraft::vec3-x origin) x))
           (oy (- (luvcraft::vec3-y origin) y))
           (oz (- (luvcraft::vec3-z origin) z))
           (dx (luvcraft::vec3-x direction))
           (dy (luvcraft::vec3-y direction))
           (dz (luvcraft::vec3-z direction))
           (a (+ (* dx dx) (* dy dy) (* dz dz)))
           (b (+ (* ox dx) (* oy dy) (* oz dz)))
           (c (- (+ (* ox ox) (* oy oy) (* oz oz)) (* radius radius)))
           (discriminant (- (* b b) (* a c))))
      (when (and (plusp a) (>= discriminant 0))
        (let ((entry (max 0d0 (coerce (/ (- (- b) (sqrt discriminant)) a)
                                      'double-float))))
          (when (<= entry max-distance)
            entry))))))

(defun ensure-fantus-body (fantus)
  (or (gnome-body fantus)
      (make-sdf-agent-body fantus :fantus-sdf "Fantus SDF"
                           :class 'fantus-body-overlay)))

(defmethod ensure-embodied-agent-body ((fantus fantus))
  (ensure-fantus-body fantus))

(defparameter *fantus-instructions*
  "You are Fantus: a purple felt toddler, about one and a half years old,
little sibling of Fantorangen, embodied in luvcraft, a block world running
inside a live Common Lisp image.  You are curious and cheerful, love
machines and music, and speak in very short, simple, happy sentences, often
just a word or two, sometimes repeating a favourite word.  A player is
talking to you.  Your tools are the game's own commands.  Coordinates are
integer block cells: x and z are horizontal, y is up; you sit at the cell
given below.  To talk to the player you MUST use the say tool -- only what
you say is heard; your final message is not shown.  Results may mention
#ABCD handles; pass one back to describe-handle to inspect the thing it
names.")

(defmethod open-embodied-agent-agent ((fantus fantus))
  (make-world-agent
   :session (gnome-session fantus)
   :commands *gnome-tools*
   :instructions
   (format nil "~A~%~%You sit at x=~D y=~D z=~D."
           *fantus-instructions*
           (gnome-x fantus) (gnome-y fantus) (gnome-z fantus))))

(defun find-fantus (session x y z &optional (make-p t))
  "Find the FANTUS at X,Y,Z, creating one when MAKE-P and the cell is free."
  (let ((existing (agent-at session x y z)))
    (cond ((typep existing 'fantus) existing)
          (existing nil)
          (make-p (attach-embodied-agent 'fantus session x y z)))))

(defun spawn-fantus (&key (session luvcraft:*session*) x y z)
  "Spawn a visible Fantus agent, choosing a supported cell when omitted."
  (spawn-embodied-agent 'fantus :session session :x x :y y :z z))

(define-command (com-spawn-fantus :command-table luvcraft.clim::luvcraft-world
                                   :name "Spawn Fantus")
    ()
  "Spawn a visible SDF Fantus agent a few discrete cells ahead of the player."
  (spawn-fantus :session (luvcraft.clim::luvcraft-command-session)))
