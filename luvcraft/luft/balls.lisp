(in-package #:luvcraft.luft.game)

;;; The solver and palette stay Y-up. Only this boundary knows Luft's axes.
(defclass ball-terrain ()
  ((scene :initarg :scene :reader ball-terrain-scene)))

(defmethod luvcraft::physics-terrain-cell-query ((terrain ball-terrain))
  (let ((solid (terrain::inspection-source-solid (ball-terrain-scene terrain))))
    (lambda (x y z)
      (= 1 (terrain::collision-cell-occupancy-bit solid (+ x 1024) (+ z 1024) y)))))

(defmethod luvcraft::physics-terrain-resident-p
    ((terrain ball-terrain) x y z radius vx vy vz seconds)
  (declare (ignore y vy))
  (let* ((scene (ball-terrain-scene terrain))
         (store (terrain:streaming-scene-store scene))
         (margin (+ radius luvcraft::*physics-terrain-margin* 1))
         (reach-x (+ margin (* (abs vx) seconds)))
         (reach-z (+ margin (* (abs vz) seconds))))
    ;; Include the swept footprint and the contact solver's neighbouring
    ;; cells, not just the centre chunk: crossing an unloaded seam freezes.
    (loop for cx from (floor (- (+ x 1024) reach-x) 64)
            to (floor (+ x 1024 reach-x) 64)
          always (loop for cy from (floor (- (+ z 1024) reach-z) 64)
                         to (floor (+ z 1024 reach-z) 64)
                       always (nth-value 1 (gethash (luft:chunk-key-at (* cx 64) (* cy 64))
                                                    store))))))

(defun ensure-viewer-physics (viewer)
  (or (viewer-physics viewer)
      (setf (viewer-physics viewer)
            (luvcraft::make-physics-world
             :terrain (make-instance 'ball-terrain :scene (render:viewer-source viewer))))))

(defmethod render:apply-viewer-world-edit :around ((viewer ball-game-state) cell placement)
  (multiple-value-bind (edit status) (call-next-method)
    (when (and edit (viewer-physics viewer))
      (luvcraft::wake-physics-bodies-near
       (viewer-physics viewer) (- (luft:site-x cell) 1024) (luft:site-z cell)
       (- (luft:site-y cell) 1024) 2.0))
    (values edit status)))

(defun post-viewer-physics-boxes (viewer physics)
  (luvcraft::clear-physics-boxes physics)
  (let ((player (render:viewer-player viewer)))
    (when player
      (let* ((body (terrain:character-body player))
             (p (terrain:body-position body)) (v (terrain:body-velocity body))
             (x (- (vec3:vec3-x p) 1024)) (y (vec3:vec3-z p))
             (z (- (vec3:vec3-y p) 1024)) (r (terrain:body-radius body)))
        (luvcraft::post-physics-box physics (- x r) y (- z r)
                                    (+ x r) (+ y (terrain:body-height body)) (+ z r)
                                    :vx (vec3:vec3-x v) :vy (vec3:vec3-z v)
                                    :vz (vec3:vec3-y v) :owner player))))
  (dolist (animal (viewer-animals viewer))
    (let* ((p (luvcraft::critter-position animal)) (v (luvcraft::critter-velocity animal))
           (x (vec3:vec3-x p)) (y (vec3:vec3-y p)) (z (vec3:vec3-z p))
           (r (luvcraft::critter-half-width animal)))
      (luvcraft::post-physics-box physics (- x r) y (- z r)
                                  (+ x r) (+ y (luvcraft::critter-height animal)) (+ z r)
                                  :vx (vec3:vec3-x v) :vy (vec3:vec3-y v)
                                  :vz (vec3:vec3-z v) :owner animal))))

(defmethod render:advance-viewer-game :after ((viewer ball-game-state) seconds)
  (when (viewer-physics viewer)
    (let ((physics (viewer-physics viewer)))
      (clrhash (viewer-ball-previous-centers viewer))
      (luvcraft::do-physics-bodies (handle physics)
        (setf (gethash handle (viewer-ball-previous-centers viewer))
              (multiple-value-list (luvcraft::physics-body-position physics handle))))
      (incf (viewer-physics-clock viewer) seconds)
      (loop repeat luvcraft::*physics-max-steps-per-frame*
            while (>= (viewer-physics-clock viewer) luvcraft::*physics-step-seconds*)
            do (decf (viewer-physics-clock viewer) luvcraft::*physics-step-seconds*)
               (post-viewer-physics-boxes viewer physics)
               (luvcraft::step-physics-world physics)
               (luvcraft::handle-luvcraft-physics-events nil physics))
      (when (>= (viewer-physics-clock viewer) luvcraft::*physics-step-seconds*)
        (setf (viewer-physics-clock viewer) 0d0)))))

(defmethod luvcraft::throw-luvcraft-ball
    ((viewer luvcraft-viewer) &key (speed luvcraft::*ball-throw-speed*) (kind :ball)
                              (aim-right 0d0) (aim-up 0d0))
  (let* ((camera (render:viewer-camera viewer))
         (p (render:camera-position camera))
         (player (render:viewer-player viewer))
         (v (and player (terrain:body-velocity (terrain:character-body player)))))
    (multiple-value-bind (right up forward) (render:camera-basis camera)
      (let* ((d (render::add-scaled-directions
                 (vec3:make-vec3 0.0 0.0 0.0) forward 1 right aim-right up aim-up))
             (length (sqrt (+ (expt (vec3:vec3-x d) 2) (expt (vec3:vec3-y d) 2)
                              (expt (vec3:vec3-z d) 2))))
             (dx (/ (vec3:vec3-x d) length)) (dy (/ (vec3:vec3-y d) length))
             (dz (/ (vec3:vec3-z d) length)))
        (luvcraft::spawn-body-of-kind
         (ensure-viewer-physics viewer) kind
         (+ (- (vec3:vec3-x p) 1024) (* 0.7 dx) (* 0.25 (vec3:vec3-x right)))
         (+ (vec3:vec3-z p) (* 0.7 dz) -0.15)
         (+ (- (vec3:vec3-y p) 1024) (* 0.7 dy) (* 0.25 (vec3:vec3-y right)))
         :vx (+ (* speed dx) (if v (vec3:vec3-x v) 0))
         :vy (+ (* speed dz) (if v (vec3:vec3-z v) 0))
         :vz (+ (* speed dy) (if v (vec3:vec3-y v) 0)))))))

(clim:define-command (com-ball :command-table luvcraft-world
                              :name "Ball" :keystroke (:b)) ()
  (let ((viewer (render:viewer-command-viewer)))
    (setf (viewer-ball-in-hand-p viewer) (not (viewer-ball-in-hand-p viewer)))))

(defmethod mcluv:palette-hint :around ((viewer ball-game-state))
  (if (and (viewer-ball-in-hand-p viewer) (not (viewer-inventory-page viewer)))
      "Ball · left throw · right marbles · B put away"
      (call-next-method)))

(defmethod render::handle-viewer-mode-pointer-press :around
    (mode (viewer luvcraft-viewer) canvas event)
  (if (and (viewer-ball-in-hand-p viewer)
           (render:viewer-pointer-captured-p viewer)
           (member (luv:canvas-pointer-event-button event) '(:left :right)))
      (luvcraft::hand-item-use (viewer-ball-hand viewer) nil viewer
                              (luv:canvas-pointer-event-button event))
      (call-next-method)))

(defmethod render:viewer-figures :around ((viewer ball-game-state))
  (append (call-next-method)
          (when (viewer-physics viewer)
            (let ((physics (viewer-physics viewer)) (figures nil))
              (luvcraft::do-physics-bodies (handle physics)
                (multiple-value-bind (set row) (luvcraft::physics-body-set-and-local physics handle)
                  (let* ((columns (luvcraft::physics-body-columns-for-set physics set))
                         (p (multiple-value-list (luvcraft::physics-body-position physics handle)))
                         (old (or (gethash handle (viewer-ball-previous-centers viewer)) p))
                         (kind (aref luvcraft::*body-kinds*
                                     (aref (luvcraft::physics-body-columns-kind-lane columns) row)))
                         (radius (* (aref (luvcraft::physics-body-columns-radius-lane columns) row)
                                    (luvcraft::body-kind-draw-scale kind))))
                    (flet ((point (p) (list (+ 1024 (first p)) (+ 1024 (third p)) (second p) radius)))
                      (push (list (point p) (point old)
                                  (list (aref (luvcraft::physics-body-columns-qx-lane columns) row)
                                        (aref (luvcraft::physics-body-columns-qy-lane columns) row)
                                        (aref (luvcraft::physics-body-columns-qz-lane columns) row)
                                        (aref (luvcraft::physics-body-columns-qw-lane columns) row))
                                  (list (luvcraft::body-kind-pattern-index kind) 0.0 0.0 2.0)
                                  (list (luvcraft::body-kind-albedo-red kind)
                                        (luvcraft::body-kind-albedo-green kind)
                                        (luvcraft::body-kind-albedo-blue kind)
                                        (luvcraft::body-kind-emission kind))) figures)))))
              (nreverse figures)))))
