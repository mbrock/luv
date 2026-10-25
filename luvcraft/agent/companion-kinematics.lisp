(in-package #:luvcraft.agent)

;;; Like Moppe's articulated walker: fixed-length two-bone chains and a
;;; distance-driven gait. Unlike its ground-plane approximation, contacts
;;; here belong to the world, including their heading, and each foot probes
;;; resident terrain. The locomotion body still owns jumps and large steps.

(defconstant +companion-pelvis-drop+ 0.16d0)

(defun ik-offset (point vector &optional (scale 1))
  (make-vec3 (+ (vec3-x point) (* scale (vec3-x vector)))
             (+ (vec3-y point) (* scale (vec3-y vector)))
             (+ (vec3-z point) (* scale (vec3-z vector)))))

(defun ik-difference (a b) (ik-offset a b -1))

(defun ik-perpendicular (axis hint)
  "Unit projection of HINT perpendicular to AXIS, including parallel poles."
  (let ((projected (ik-offset hint axis (- (vec3-dot axis hint)))))
    (if (> (vec3-length projected) 1d-7)
        (vec3-normalize projected)
        ;; Choose the least parallel coordinate axis, not a fixed fallback
        ;; which itself degenerates when the limb happens to point along it.
        (let* ((coordinate (first (sort (list :x :y :z) #'<
                                       :key (lambda (a) (abs (vec3-component axis a))))))
               (basis (make-vec3 (if (eq coordinate :x) 1 0)
                                 (if (eq coordinate :y) 1 0)
                                 (if (eq coordinate :z) 1 0))))
          (vec3-normalize (ik-offset basis axis (- (vec3-dot axis basis))))))))

(defun solve-two-bone-chain (hip target pole upper-length lower-length)
  "Return KNEE and reachable ANKLE; never stretch either segment.
The pole names the bend side. Unequal lengths also impose an inner reach
limit, including when TARGET coincides with HIP."
  (let* ((delta (ik-difference target hip))
         (distance (vec3-length delta))
         (axis (if (> distance 1d-9) (vec3-scale delta (/ distance)) (make-vec3 0 -1 0)))
         (epsilon (* 1d-6 (+ upper-length lower-length)))
         (reach (max (+ (abs (- upper-length lower-length)) epsilon)
                     (min distance (- (+ upper-length lower-length) epsilon))))
         (along (/ (+ (* reach reach) (* upper-length upper-length)
                       (- (* lower-length lower-length))) (* 2 reach)))
         (height (sqrt (max 0 (- (* upper-length upper-length) (* along along)))))
         (bend (ik-perpendicular axis (ik-difference pole hip))))
    (values (ik-offset (ik-offset hip axis along) bend height)
            (ik-offset hip axis reach))))

(defun companion-palette-point (palette bone point)
  (flet ((row (axis)
           (let ((i (+ (* 12 bone) (* 4 axis))))
             (+ (* (aref palette i) (vec3-x point))
                (* (aref palette (+ i 1)) (vec3-y point))
                (* (aref palette (+ i 2)) (vec3-z point)) (aref palette (+ i 3))))))
    (make-vec3 (row 0) (row 1) (row 2))))

(defun companion-bone-frame (direction normal)
  (let* ((y (vec3-normalize direction)) (x (ik-perpendicular y normal)))
    (vector x y (vec3-cross x y))))

(defun write-companion-bone (palette bone rest-start rest-end rest-normal start end normal)
  (let ((rest (companion-bone-frame (ik-difference rest-end rest-start) rest-normal))
        (posed (companion-bone-frame (ik-difference end start) normal)))
    ;; R = posed-frame * transpose(rest-frame); t = start - R * rest-start.
    (loop for row below 3 for axis in '(:x :y :z)
          for base = (+ (* 12 bone) (* 4 row))
          do (loop for col below 3 for other in '(:x :y :z)
                   do (setf (aref palette (+ base col))
                            (float (loop for i below 3
                                         sum (* (vec3-component (aref posed i) axis)
                                                (vec3-component (aref rest i) other))) 1.0)))
             (setf (aref palette (+ base 3))
                   (float (- (vec3-component start axis)
                             (+ (* (aref palette base) (vec3-x rest-start))
                                (* (aref palette (+ base 1)) (vec3-y rest-start))
                                (* (aref palette (+ base 2)) (vec3-z rest-start)))) 1.0)))))

(defun companion-rotate (point yaw)
  (make-vec3 (+ (* (cos yaw) (vec3-x point)) (* (sin yaw) (vec3-z point)))
             (vec3-y point)
             (- (* (cos yaw) (vec3-z point)) (* (sin yaw) (vec3-x point)))))

(defun companion-world-point (agent point)
  (ik-offset (embodied-agent-position agent)
             (companion-rotate point (embodied-agent-facing-yaw agent))
             (companion-stature (embodied-agent-save-kind agent))))

(defun companion-local-point (agent point)
  (vec3-scale (companion-rotate (ik-difference point (embodied-agent-position agent))
                                (- (embodied-agent-facing-yaw agent)))
              (/ (companion-stature (embodied-agent-save-kind agent)))))

(defgeneric companion-ground-height (session x z ceiling floor)
  (:documentation "Resident upward contact between CEILING and FLOOR, in presentation Y-up axes.
Return NIL for a hole, unresident terrain, or a ray starting inside solid."))

(defmethod companion-ground-height ((session null) x z ceiling floor)
  (declare (ignore x z ceiling floor)) nil)

(defmethod companion-ground-height ((session luvcraft:luvcraft-session) x z ceiling floor)
  (let ((hit (luvcraft::raycast-block-world
              (luvcraft:luvcraft-session-world session) (make-vec3 x ceiling z)
              (make-vec3 0 -1 0) #'luvcraft::block-solid-p :max-distance (- ceiling floor))))
    (when (and hit (plusp (luvcraft::block-ray-hit-distance hit)))
      (- ceiling (luvcraft::block-ray-hit-distance hit)))))

(defun companion-foot-ground (agent session leg point)
  (let* ((scale (companion-stature (embodied-agent-save-kind agent)))
         (root-y (luvcraft::body-y agent))
         (height (companion-ground-height session (vec3-x point) (vec3-z point)
                                           (+ root-y (* scale 0.4)) (- root-y (* scale 0.6)))))
    (values (make-vec3 (vec3-x point)
                       (+ (or height root-y) (* scale (vec3-y (companion-leg-ankle leg))))
                       (vec3-z point))
            (not (null height)))))

(defun start-companion-step (foot target heading distance-driven-p)
  (setf (companion-foot-from foot) (companion-foot-point foot)
        (companion-foot-from-heading foot) (companion-foot-heading foot)
        (companion-foot-target foot) target
        (companion-foot-target-heading foot) heading
        (companion-foot-progress foot) 0d0
        (companion-foot-swing-p foot) t
        (companion-foot-supported-p foot) nil
        (companion-foot-distance-driven-p foot) distance-driven-p))

(defun advance-companion-feet (agent session pose seconds travel direction moving)
  (let* ((mesh (companion-pose-mesh pose)) (rig (companion-mesh-rig mesh))
         (scale (companion-pose-scale pose)) (stride (companion-mesh-stride mesh))
         (duty (companion-rig-stance rig)) (yaw (embodied-agent-facing-yaw agent))
         (any-swing (some #'companion-foot-swing-p (companion-pose-feet pose))))
    (loop for leg in (companion-rig-legs rig) for foot in (companion-pose-feet pose)
          for home = (companion-world-point agent (companion-leg-ankle leg))
          for phase = (mod (+ (companion-walk-phase agent) (companion-leg-phase leg)) 1)
          for stance = (< phase duty)
          do (multiple-value-bind (home-ground supported) (companion-foot-ground agent session leg home)
               (unless (companion-foot-point foot)
                 (setf (companion-foot-point foot) home-ground (companion-foot-heading foot) yaw
                       (companion-foot-supported-p foot) supported))
               (let* ((hip (companion-world-point
                            agent
                            (companion-palette-point (companion-pose-palette pose)
                                                     (first (companion-leg-bones leg))
                                                     (companion-leg-hip leg))))
                      ;; A reachable lower foothold must not trigger a step
                      ;; before the pelvis has a chance to satisfy it. Use
                      ;; the same downward allowance as the leg pose solve.
                      (reach-hip (make-vec3 (vec3-x hip)
                                            (max (- (vec3-y hip) (* scale +companion-pelvis-drop+))
                                                 (min (vec3-y hip) (vec3-y (companion-foot-point foot))))
                                            (vec3-z hip)))
                      (length (+ (vec3-length (ik-difference (companion-leg-knee leg) (companion-leg-hip leg)))
                                 (vec3-length (ik-difference (companion-leg-ankle leg) (companion-leg-knee leg)))))
                      (delta (ik-difference home-ground (companion-foot-point foot)))
                      (heading-error (atan (sin (- yaw (companion-foot-heading foot)))
                                           (cos (- yaw (companion-foot-heading foot)))))
                      (out-of-reach (> (vec3-length (ik-difference reach-hip (companion-foot-point foot)))
                                       (* scale length 0.995)))
                      (turn-step (and (not moving) (not any-swing)
                                      (or (> (abs heading-error) 0.55)
                                          (> (vec3-length delta) (* scale 0.12))))))
                 (when (and (not (companion-foot-swing-p foot))
                            (or out-of-reach turn-step
                                (and moving (not stance)
                                     (or (companion-foot-stance-p foot)
                                         (not (companion-pose-moving-p pose))))))
                   (let ((target (if moving
                                     (ik-offset home direction (* scale stride (- 1 (* duty 0.5))))
                                     home)))
                     (start-companion-step foot (companion-foot-ground agent session leg target) yaw moving)
                     (setf any-swing t)))
                 (when (and (companion-foot-swing-p foot) (not moving)
                            (companion-foot-distance-driven-p foot))
                   ;; A predicted landing assumes continued root travel. At
                   ;; a stop, finish at home rather than reaching ahead of
                   ;; a body which will never arrive there.
                   (start-companion-step foot home-ground yaw nil))
                 (cond
                   ((companion-foot-swing-p foot)
                    ;; Distance drives a walking swing. Time finishes an
                    ;; interrupted step, so stopping never leaves a paw aloft.
                    (incf (companion-foot-progress foot)
                          (if (and moving (companion-foot-distance-driven-p foot))
                              (/ travel (* scale stride (- 1 duty))) (/ seconds 0.22)))
                    (let* ((u (min 1d0 (companion-foot-progress foot)))
                           (smooth (* u u (- 3 (* 2 u))))
                           (point (ik-offset (companion-foot-from foot)
                                             (ik-difference (companion-foot-target foot) (companion-foot-from foot))
                                             smooth)))
                      (incf (vec3-y point) (* scale 0.09 (expt (sin (* pi u)) 2)))
                      (setf (companion-foot-point foot) point
                            (companion-foot-heading foot)
                            (+ (companion-foot-from-heading foot)
                               (* smooth (atan (sin (- (companion-foot-target-heading foot) (companion-foot-from-heading foot)))
                                               (cos (- (companion-foot-target-heading foot) (companion-foot-from-heading foot)))))))
                      (when (= u 1)
                        (multiple-value-bind (contact valid)
                            (companion-foot-ground agent session leg (companion-foot-target foot))
                          (setf (companion-foot-point foot) contact
                                (companion-foot-supported-p foot) valid
                                (companion-foot-heading foot) (companion-foot-target-heading foot)
                                (companion-foot-swing-p foot) nil)))))
                   (t
                    ;; Editing away the supporting block must release a
                    ;; contact. Missing terrain is never an invented anchor.
                    (multiple-value-bind (contact valid)
                        (companion-foot-ground agent session leg (companion-foot-point foot))
                      (unless (and valid (< (abs (- (vec3-y contact)
                                                    (vec3-y (companion-foot-point foot)))) 1d-5))
                        (setf (companion-foot-point foot) home-ground
                              (companion-foot-heading foot) yaw
                              (companion-foot-supported-p foot) supported)))))
                 (setf (companion-foot-stance-p foot) stance))))))

(defun pose-companion-legs (agent pose seconds)
  (let* ((mesh (companion-pose-mesh pose)) (palette (companion-pose-palette pose))
         (legs (companion-rig-legs (companion-mesh-rig mesh)))
         (feet (companion-pose-feet pose)) (lowering 0d0))
    ;; Lower the shared body only as far as reachable planted contacts need.
    ;; A whole voxel step exceeds these toy legs: the movement motor owns it.
    (loop for leg in legs for foot in feet
          for hip = (companion-palette-point palette (first (companion-leg-bones leg)) (companion-leg-hip leg))
          for ankle = (companion-local-point agent (companion-foot-point foot))
          for reach = (+ (vec3-length (ik-difference (companion-leg-knee leg) (companion-leg-hip leg)))
                         (vec3-length (ik-difference (companion-leg-ankle leg) (companion-leg-knee leg))))
          for horizontal = (+ (expt (- (vec3-x hip) (vec3-x ankle)) 2)
                              (expt (- (vec3-z hip) (vec3-z ankle)) 2))
          unless (companion-foot-swing-p foot)
            do (let ((height (sqrt (max 0 (- (expt (* 0.995 reach) 2) horizontal)))))
                 (setf lowering (min lowering (- (+ (vec3-y ankle) height) (vec3-y hip))))))
    (let* ((needed (max (- +companion-pelvis-drop+) lowering)) (old (companion-pose-pelvis pose)))
      ;; Ease upward; a downward reach constraint is immediate. Smoothing
      ;; that constraint would lift a planted sole off the ground each frame.
      (setf (companion-pose-pelvis pose)
            (min needed (+ old (* (- 1 (exp (* -24 seconds))) (- needed old))))))
    (loop for i from 7 below (length palette) by 12
          do (incf (aref palette i) (float (companion-pose-pelvis pose) 1.0)))
    (loop for leg in legs for foot in feet
          for bones = (companion-leg-bones leg)
          for rh = (companion-leg-hip leg) for rk = (companion-leg-knee leg)
          for ra = (companion-leg-ankle leg) for rp = (companion-leg-pole leg)
          for hip = (companion-palette-point palette (first bones) rh)
          for target = (companion-local-point agent (companion-foot-point foot))
          for pole = (ik-offset hip (ik-difference rp rh))
          do (multiple-value-bind (knee ankle)
                 (solve-two-bone-chain hip target pole
                                      (vec3-length (ik-difference rk rh)) (vec3-length (ik-difference ra rk)))
               (let ((rest-normal (vec3-cross (ik-difference ra rh) (ik-difference rp rh)))
                     (normal (vec3-cross (ik-difference ankle hip) (ik-difference knee hip))))
                 (write-companion-bone palette (first bones) rh rk rest-normal hip knee normal)
                 (write-companion-bone palette (second bones) rk ra rest-normal knee ankle normal))
               ;; The foot follows the solved ankle, not an unreachable target.
               ;; Preserve its world heading during stance, not the actor yaw.
               (let* ((angle (- (companion-foot-heading foot) (embodied-agent-facing-yaw agent)))
                      (forward (companion-rotate (make-vec3 0 0 1) angle)))
                 (write-companion-bone palette (third bones) ra (ik-offset ra (make-vec3 0 0 1))
                                       (make-vec3 1 0 0) ankle (ik-offset ankle forward)
                                       (companion-rotate (make-vec3 1 0 0) angle))))))
  pose)

(defun advance-companion-kinematics (agent session seconds travel direction moving)
  "Advance contacts once per simulation update, never once per render pass."
  (let* ((mesh (companion-mesh (embodied-agent-save-kind agent)))
         (scale (companion-stature (embodied-agent-save-kind agent)))
         (position (embodied-agent-position agent))
         (pose (companion-kinematics agent)))
    (when (or (null pose) (not (eq mesh (companion-pose-mesh pose)))
              (/= scale (companion-pose-scale pose))
              (not (luvcraft:body-grounded-p agent))
              (> (abs (- (vec3-y position) (vec3-y (companion-pose-position pose)))) (* scale 0.3))
              (> (vec3-length (ik-difference position (companion-pose-position pose)))
                 (max (* 12 seconds) (* scale 0.5))))
      (setf pose (make-companion-pose
                  :mesh mesh :scale scale :position position
                  :palette (make-array (* 12 (companion-mesh-bone-count mesh)) :element-type 'single-float)
                  :feet (loop for leg in (companion-rig-legs (companion-mesh-rig mesh))
                              collect (make-companion-foot)))
            (companion-kinematics agent) pose))
    (sample-companion-palette mesh (companion-walk-phase agent) (companion-walk-blend agent)
                              (companion-pose-palette pose))
    (when (luvcraft:body-grounded-p agent)
      (advance-companion-feet agent session pose seconds travel direction moving)
      (pose-companion-legs agent pose seconds))
    ;; Body setters may mutate their vec3 in place; retain a value snapshot.
    (setf (companion-pose-position pose) (make-vec3 (vec3-x position) (vec3-y position) (vec3-z position))
          (companion-pose-moving-p pose) moving)
    pose))
