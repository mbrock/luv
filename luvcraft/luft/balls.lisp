(in-package #:luvcraft.luft.game)

;;; These are the very same objects and places as the original solver UI.
(defmethod luv.knobs:knobs-for-host ((viewer luvcraft-viewer))
  (append (remove-if (lambda (knob)
                       ;; These belong to the atelier's paper medium, not
                       ;; this game's photographic grade and quadratic fog.
                       (member (luv.knobs:knob-name knob)
                               '(render::paper-grain render::haze-height)))
                     (call-next-method))
          (mapcar #'luv.knobs:find-knob
                  '(luvcraft::physics-gravity luvcraft::ball-bounce
                    luvcraft::physics-substeps))
          (remove-if-not
           (lambda (knob)
             (or (eq (symbol-package (luv.knobs:knob-name knob))
                     (find-package :luvcraft.luft.game))
                 (and (eq (symbol-package (luv.knobs:knob-name knob))
                          (find-package :luvcraft))
                      (eq :text (luv.knobs:knob-group knob)))))
           luv.knobs:*knobs*)))

(defmethod luv.knobs:knob-rebuild-p :around
    ((knob luv.knobs:knob) (viewer luvcraft-viewer))
  (or (eq :text (luv.knobs:knob-group knob)) (call-next-method)))

(defmethod luv.knobs:realize-knob progn
    ((knob luv.knobs:scalar-knob) (viewer luvcraft-viewer))
  (when (eq :text (luv.knobs:knob-group knob))
    ;; World glyphs are tracked folded values; retained HUD glyphs also need
    ;; their shader family invalidated when these shared Slug values change.
    (luv.shader::note-shader-source-redefinition (luv.knobs:knob-name knob))))

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

(defun ensure-world-physics (world)
  (or (world-physics world)
      (setf (world-physics world)
            (luvcraft::make-physics-world
             :terrain (make-instance 'ball-terrain :scene (world-scene world))))))

(defun initialize-world-springs (world)
  (unless (world-springs-initialized-p world)
    (let ((springs (luvcraft::world-spring-coordinates (world-block-world world))))
      (dolist (spring springs)
        (let ((old (find-if
                    (lambda (old)
                      (and (= (luvcraft::luvcraft-spring-x old) (luvcraft::luvcraft-spring-x spring))
                           (= (luvcraft::luvcraft-spring-y old) (luvcraft::luvcraft-spring-y spring))
                           (= (luvcraft::luvcraft-spring-z old) (luvcraft::luvcraft-spring-z spring))
                           (eq (luvcraft::luvcraft-spring-kind old) (luvcraft::luvcraft-spring-kind spring))))
                    (world-springs world))))
          (when old
            (setf (luvcraft::luvcraft-spring-count spring) (luvcraft::luvcraft-spring-count old)))))
      (setf (world-springs world) springs
            (world-springs-initialized-p world) t))))

(defmethod luvcraft::spring-block-at ((world luvcraft-world) x y z)
  ;; Query published terrain, not the old world's generated/cached chunks.
  (let* ((scene (world-scene world))
         (site (game:luvcraft-cell-site (world-source world) x y z)))
    (when (and site (cell-resident-p world (luft:site-x site) (luft:site-y site)))
      (let ((placement (terrain:scene-material-placement-at scene site)))
        (values (and placement (game::luvcraft-block-for-placement placement)) :resident)))))

(defun post-world-physics-boxes (world physics)
  (luvcraft::clear-physics-boxes physics)
  (dolist (player (world-players world))
    (let ((character (player-character player)))
      (when character
        (let* ((body (terrain:character-body character))
               (p (terrain:body-position body)) (v (terrain:body-velocity body))
               (x (- (vec3:vec3-x p) 1024)) (y (vec3:vec3-z p))
               (z (- (vec3:vec3-y p) 1024)) (r (terrain:body-radius body)))
          (luvcraft::post-physics-box physics (- x r) y (- z r)
                                      (+ x r) (+ y (terrain:body-height body)) (+ z r)
                                      :vx (vec3:vec3-x v) :vy (vec3:vec3-z v)
                                      :vz (vec3:vec3-y v) :owner character)))))
  (dolist (animal (world-animals world))
    (let* ((p (luvcraft::critter-position animal)) (v (luvcraft::critter-velocity animal))
           (x (vec3:vec3-x p)) (y (vec3:vec3-y p)) (z (vec3:vec3-z p))
           (r (luvcraft::critter-half-width animal)))
      (luvcraft::post-physics-box physics (- x r) y (- z r)
                                  (+ x r) (+ y (luvcraft::critter-height animal)) (+ z r)
                                  :vx (vec3:vec3-x v) :vy (vec3:vec3-y v)
                                  :vz (vec3:vec3-z v) :owner animal))))

(defun advance-world-balls (world seconds)
  (when (world-block-world world)
    (initialize-world-springs world)
    (when (world-springs world) (ensure-world-physics world)))
  (when (world-physics world)
    (let ((physics (world-physics world)))
      (clrhash (world-ball-previous-centers world))
      (luvcraft::do-physics-bodies (handle physics)
        (setf (gethash handle (world-ball-previous-centers world))
              (multiple-value-list (luvcraft::physics-body-position physics handle))))
      (incf (world-physics-clock world) seconds)
      (loop repeat luvcraft::*physics-max-steps-per-frame*
            while (>= (world-physics-clock world) luvcraft::*physics-step-seconds*)
            do (decf (world-physics-clock world) luvcraft::*physics-step-seconds*)
               (post-world-physics-boxes world physics)
               (dolist (spring (world-springs world))
                 (luvcraft::spray-luvcraft-spring spring world physics))
               (luvcraft::step-physics-world physics)
               (luvcraft::handle-luvcraft-physics-events nil physics))
      (when (>= (world-physics-clock world) luvcraft::*physics-step-seconds*)
        (setf (world-physics-clock world) 0d0)))))

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
         (ensure-world-physics (viewer-world viewer)) kind
         (+ (- (vec3:vec3-x p) 1024) (* 0.7 dx) (* 0.25 (vec3:vec3-x right)))
         (+ (vec3:vec3-z p) (* 0.7 dz) -0.15)
         (+ (- (vec3:vec3-y p) 1024) (* 0.7 dy) (* 0.25 (vec3:vec3-y right)))
         :vx (+ (* speed dx) (if v (vec3:vec3-x v) 0))
         :vy (+ (* speed dz) (if v (vec3:vec3-z v) 0))
         :vz (+ (* speed dy) (if v (vec3:vec3-y v) 0)))))))

(clim:define-command (com-ball :command-table luvcraft-world
                              :name "Ball" :keystroke (:b)) ()
  (let ((player (viewer-local-player (render:viewer-command-viewer))))
    (setf (player-ball-in-hand-p player) (not (player-ball-in-hand-p player)))))

(defmethod render::handle-viewer-mode-pointer-press :around
    (mode (viewer luvcraft-viewer) canvas event)
  (if (and (player-ball-in-hand-p (viewer-local-player viewer))
           (render:viewer-pointer-captured-p viewer)
           (member (luv:canvas-pointer-event-button event) '(:left :right)))
      (luvcraft::hand-item-use (player-ball-hand (viewer-local-player viewer)) nil viewer
                              (luv:canvas-pointer-event-button event))
      (call-next-method)))

(defun ball-figures (viewer)
  "The balls of VIEWER's world, each with its previous centre for motion."
  (let ((world (viewer-world viewer)))
    (when (world-physics world)
      (let ((physics (world-physics world)) (figures nil))
        (luvcraft::do-physics-bodies (handle physics)
          (multiple-value-bind (set row) (luvcraft::physics-body-set-and-local physics handle)
            (let* ((columns (luvcraft::physics-body-columns-for-set physics set))
                   (p (multiple-value-list (luvcraft::physics-body-position physics handle)))
                   (old (or (gethash handle (world-ball-previous-centers world)) p))
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
