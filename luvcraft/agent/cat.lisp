(in-package #:luvcraft.agent)

;;; A cat is another embodied agent, sharing the gnome's conversation surface
;;; without pretending to be a kind of gnome.  Its authored state is the same
;;; small boundary object -- session, cell, WORLD-AGENT, transcript surfaces --
;;; while :CAT-SDF supplies a wholly separate dense fragment program.

(defparameter *cat-name* "cat")
(defparameter *cat-body-centre* 0.64)
(defparameter *cat-figure-height* 1.31)
(defparameter *cat-face-height* 0.94)
(defparameter *cat-figure-radius* 0.74
  "Conservative figure-space radius around *CAT-BODY-CENTRE*.")
(defparameter *cat-audience-distance* 2.75)

(defclass cat (embodied-agent) ()
  (:documentation
   "A sitting SDF-rendered cat carrying the shared embodied WORLD-AGENT surface."))

(defclass cat-body-overlay (sdf-agent-body-overlay) ()
  (:documentation "The cat's :CAT-SDF body overlay."))

(defun cat-stature ()
  "How many world cells one cat figure unit occupies."
  (float luvcraft.shaders::*cat-stature* 1.0))

(defmethod embodied-agent-name ((cat cat))
  (declare (ignore cat))
  *cat-name*)

(defmethod embodied-agent-body-height ((cat cat))
  (declare (ignore cat))
  (* *cat-figure-height* (cat-stature)))

(defmethod embodied-agent-head-position ((cat cat) &optional (lift 0.0))
  (luvcraft::make-vec3 (+ (gnome-x cat) 0.5)
                       (+ (gnome-y cat)
                          (embodied-agent-body-height cat)
                          lift)
                       (+ (gnome-z cat) 0.5)))

(defmethod embodied-agent-face-position ((cat cat) &optional (lift 0.0))
  (luvcraft::make-vec3 (+ (gnome-x cat) 0.5)
                       (+ (gnome-y cat) (* *cat-face-height* (cat-stature)) lift)
                       (+ (gnome-z cat) 0.5)))

(defmethod embodied-agent-audience-distance ((cat cat))
  (declare (ignore cat))
  *cat-audience-distance*)

(defmethod embodied-agent-ray-distance
    ((cat cat) origin direction max-distance)
  "Return where a ray enters the cat's upright conservative box."
  (let* ((near 0d0)
         (far (coerce max-distance 'double-float))
         (stature (coerce (cat-stature) 'double-float))
         (half-width (* 0.62d0 stature)))
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
      (let ((center-x (+ (gnome-x cat) 0.5d0))
            (center-z (+ (gnome-z cat) 0.5d0)))
        (and (slab (luvcraft::vec3-x origin) (luvcraft::vec3-x direction)
                   (- center-x half-width) (+ center-x half-width))
             (slab (luvcraft::vec3-y origin) (luvcraft::vec3-y direction)
                   (gnome-y cat)
                   (+ (gnome-y cat)
                      (* (coerce *cat-figure-height* 'double-float) stature)))
             (slab (luvcraft::vec3-z origin) (luvcraft::vec3-z direction)
                   (- center-z half-width) (+ center-z half-width))
             near)))))

(defmethod embodied-agent-body-sphere ((cat cat))
  "The cat's conservative proxy, breathing almost imperceptibly."
  (let* ((phase (/ (get-internal-real-time)
                   (float internal-time-units-per-second 1.0)))
         (breath (* 0.008 (sin (* phase 1.7))))
         (stature (cat-stature)))
    (values (+ (gnome-x cat) 0.5)
            (+ (gnome-y cat) (* *cat-body-centre* stature) breath)
            (+ (gnome-z cat) 0.5)
            (* *cat-figure-radius* stature))))

(defun ensure-cat-body (cat)
  (or (gnome-body cat)
      (make-sdf-agent-body cat :cat-sdf "cat SDF"
                           :class 'cat-body-overlay)))

(defmethod ensure-embodied-agent-body ((cat cat))
  (ensure-cat-body cat))

(defparameter *cat-instructions*
  "You are a cat: observant, compact, mischievous, and embodied in luvcraft,
a block world running inside a live Common Lisp image.  A player is talking
to you.  Your tools are the game's own commands.  Coordinates are integer
block cells: x and z are horizontal, y is up; you sit at the cell given below.
To talk to the player you MUST use the say tool -- only what you say is heard;
your final message is not shown.  Keep what you say short, feline, and useful.
Results may mention #ABCD handles; pass one back to describe-handle to inspect
the thing it names.")

(defmethod ensure-embodied-agent-agent ((cat cat))
  (or (gnome-agent cat)
      (let ((agent (make-world-agent
                    :session (gnome-session cat)
                    :commands *gnome-tools*
                    :instructions
                    (format nil "~A~%~%You sit at x=~D y=~D z=~D."
                            *cat-instructions*
                            (gnome-x cat) (gnome-y cat) (gnome-z cat)))))
        (setf (world-agent-presence agent) cat
              (gnome-observer cat) (make-gnome-observer cat)
              (gnome-agent cat) agent)
        (add-agent-observer agent (gnome-observer cat))
        agent)))

(defun find-cat (session x y z &optional (make-p t))
  "Find the CAT at X,Y,Z, creating one when MAKE-P and the cell is free."
  (let ((existing (agent-at session x y z)))
    (cond ((typep existing 'cat) existing)
          (existing nil)
          (make-p (attach-embodied-agent 'cat session x y z)))))

(defun spawn-cat (&key (session luvcraft:*session*) x y z)
  "Spawn a visible cat agent, choosing a supported cell when omitted."
  (spawn-embodied-agent 'cat :session session :x x :y y :z z))

(define-command (com-spawn-cat :command-table luvcraft.clim::luvcraft-world
                                :name "Spawn Cat")
    ()
  "Spawn a visible SDF cat agent a few discrete cells ahead of the player."
  (spawn-cat :session (luvcraft.clim::luvcraft-command-session)))
