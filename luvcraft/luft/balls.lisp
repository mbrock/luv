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
             (let ((name (luv.knobs:knob-name knob)))
               (cond ((eq name 'gnome-auto-approve-builds) (selected-solo-gnome viewer))
                     ((eq (symbol-package name) (find-package :luvcraft.luft.game)) t)
                     ((eq (symbol-package name) (find-package :luvcraft))
                      (eq :text (luv.knobs:knob-group knob))))))
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
  ;; Remote players' bodies, which only their owners move (#VM5TN6).
  (dolist (body (terrain:simulation-strangers (world-simulation world)))
    (let* ((p (terrain:body-position body)) (v (terrain:body-velocity body))
           (x (- (vec3:vec3-x p) 1024)) (y (vec3:vec3-z p))
           (z (- (vec3:vec3-y p) 1024)) (r (terrain:body-radius body)))
      (luvcraft::post-physics-box physics (- x r) y (- z r)
                                  (+ x r) (+ y (terrain:body-height body)) (+ z r)
                                  :vx (vec3:vec3-x v) :vy (vec3:vec3-z v)
                                  :vz (vec3:vec3-y v) :owner body)))
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
        ;; The host throws; a guest asks it to (#EA88ON).
        (throw-world-body
         (viewer-world viewer) kind
         (+ (- (vec3:vec3-x p) 1024) (* 0.7 dx) (* 0.25 (vec3:vec3-x right)))
         (+ (vec3:vec3-z p) (* 0.7 dz) -0.15)
         (+ (- (vec3:vec3-y p) 1024) (* 0.7 dy) (* 0.25 (vec3:vec3-y right)))
         (+ (* speed dx) (if v (vec3:vec3-x v) 0))
         (+ (* speed dz) (if v (vec3:vec3-z v) 0))
         (+ (* speed dy) (if v (vec3:vec3-y v) 0)))))))

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

(defconstant +ball-hero-segments+ 16)
(defconstant +ball-hero-rings+ 10)
(defconstant +ball-medium-segments+ 10)
(defconstant +ball-medium-rings+ 6)
(defconstant +ball-compact-segments+ 6)
(defconstant +ball-compact-rings+ 4)
(defconstant +ball-shadow-segments+ 8)
(defconstant +ball-shadow-rings+ 4)

(defparameter *ball-culling-p* t
  "Frustum- and distance-cull physics balls and select procedural sphere LOD.")

(defun %ball-viewer-cull-parameters (viewer)
  "Return (VALUES CX CY CZ RX RY RZ UX UY UZ FX FY FZ TAN-X TAN-Y) or NIL."
  (let ((camera (and *ball-culling-p* (render:viewer-camera viewer))))
    (when camera
      (multiple-value-bind (right up forward) (render:camera-basis camera)
        (let* ((p (render:camera-position camera))
               (canvas (render:viewer-canvas viewer))
               (aspect (if canvas
                           (multiple-value-bind (w h) (luv:canvas-size canvas)
                             (if (and (numberp w) (numberp h) (plusp h))
                                 (max 1.5 (float (/ w h) 1.0))
                                 2.0))
                           2.0))
               (fov (float (render:camera-field-of-view camera) 1.0))
               ;; Generous 20% angular margin prevents edge popping on ultrawide displays.
               (tan-y (* (tan (* 0.5 fov)) 1.2))
               (tan-x (* tan-y aspect 1.2)))
          (values (float (vec3:vec3-x p) 1.0) (float (vec3:vec3-y p) 1.0) (float (vec3:vec3-z p) 1.0)
                  (float (vec3:vec3-x right) 1.0) (float (vec3:vec3-y right) 1.0) (float (vec3:vec3-z right) 1.0)
                  (float (vec3:vec3-x up) 1.0) (float (vec3:vec3-y up) 1.0) (float (vec3:vec3-z up) 1.0)
                  (float (vec3:vec3-x forward) 1.0) (float (vec3:vec3-y forward) 1.0) (float (vec3:vec3-z forward) 1.0)
                  tan-x tan-y))))))

(defun %ball-lod-tier (bx by bz radius cx cy cz rx ry rz ux uy uz fx fy fz tan-x tan-y)
  "Return :HERO, :MEDIUM, :COMPACT, or NIL when culled."
  (declare (type single-float bx by bz radius cx cy cz rx ry rz ux uy uz fx fy fz tan-x tan-y))
  (let* ((dx (- bx cx))
         (dy (- by cy))
         (dz (- bz cz))
         (dist2 (+ (* dx dx) (* dy dy) (* dz dz)))
         (max-dist (max 48.0 (* radius 1400.0))))
    (when (> dist2 (* max-dist max-dist))
      (return-from %ball-lod-tier nil))
    (let* ((fwd (+ (* dx fx) (* dy fy) (* dz fz)))
           (rgt (+ (* dx rx) (* dy ry) (* dz rz)))
           (upv (+ (* dx ux) (* dy uy) (* dz uz)))
           (margin (+ radius 1.5))
           (in-frustum (and (>= fwd (- margin))
                            (<= (abs rgt) (+ (* (max fwd 0.0) tan-x) margin))
                            (<= (abs upv) (+ (* (max fwd 0.0) tan-y) margin))))
           ;; Keep large nearby balls just outside the view so their shadows stay intact.
           (close-shadow (and (<= dist2 784.0) (>= radius 0.12))))
      (when (or in-frustum close-shadow)
        (let* ((dist (sqrt dist2))
               (ang (/ radius (max dist 0.5))))
          (cond ((>= ang 0.020) :hero)
                ((>= ang 0.008) :medium)
                (in-frustum :compact)
                (t nil)))))))

(defun %make-ball-figure (bx by bz obx oby obz radius qx qy qz qw kind tier)
  (let ((kind-code (ecase tier (:hero 2.0) (:medium 6.0) (:compact 7.0)))
        (segs (ecase tier
                (:hero (float +ball-hero-segments+ 1.0))
                (:medium (float +ball-medium-segments+ 1.0))
                (:compact (float +ball-compact-segments+ 1.0))))
        (rings (ecase tier
                 (:hero (float +ball-hero-rings+ 1.0))
                 (:medium (float +ball-medium-rings+ 1.0))
                 (:compact (float +ball-compact-rings+ 1.0)))))
    (list (list bx by bz radius)
          (list obx oby obz radius)
          (list qx qy qz qw)
          (list (luvcraft::body-kind-pattern-index kind) segs rings kind-code)
          (list (luvcraft::body-kind-albedo-red kind)
                (luvcraft::body-kind-albedo-green kind)
                (luvcraft::body-kind-albedo-blue kind)
                (luvcraft::body-kind-emission kind)))))

(defun ball-figures (viewer)
  "Visible balls of VIEWER's world, frustum/distance culled and bucketed by sphere LOD."
  (let ((world (viewer-world viewer))
        (prev-table (viewer-previous-ball-centers viewer))
        (hero nil)
        (medium nil)
        (compact nil))
    (multiple-value-bind (cx cy cz rx ry rz ux uy uz fx fy fz tan-x tan-y)
        (%ball-viewer-cull-parameters viewer)
      (flet ((emit (id x y z qx qy qz qw kind radius)
               (let* ((bx (+ 1024.0 (float x 1.0)))
                      (by (+ 1024.0 (float z 1.0)))
                      (bz (float y 1.0))
                      (r (float radius 1.0))
                      (tier (if cx
                                (%ball-lod-tier bx by bz r cx cy cz
                                                rx ry rz ux uy uz fx fy fz tan-x tan-y)
                                :hero)))
                 (when tier
                   (let* ((old (gethash id prev-table))
                          (obx (if old (+ 1024.0 (float (first old) 1.0)) bx))
                          (oby (if old (+ 1024.0 (float (third old) 1.0)) by))
                          (obz (if old (float (second old) 1.0) bz))
                          (fig (%make-ball-figure bx by bz obx oby obz r
                                                  (float qx 1.0) (float qy 1.0)
                                                  (float qz 1.0) (float qw 1.0)
                                                  kind tier)))
                     (ecase tier
                       (:hero (push fig hero))
                       (:medium (push fig medium))
                       (:compact (push fig compact))))))))
        (if (replica-world-p world)
            (loop for (id centre quaternion kind radius) in (world-ball-list world)
                  do (emit id (first centre) (second centre) (third centre)
                           (first quaternion) (second quaternion)
                           (third quaternion) (fourth quaternion)
                           kind radius))
            (let ((physics (world-physics world)))
              (when physics
                (dolist (columns (list (luvcraft::physics-world-awake physics)
                                       (luvcraft::physics-world-sleeping physics)
                                       (luvcraft::physics-world-suspended physics)))
                  (let ((len (luvcraft::physics-body-columns-length columns))
                        (handles (luvcraft::physics-body-columns-handle-lane columns))
                        (xs (luvcraft::physics-body-columns-x-lane columns))
                        (ys (luvcraft::physics-body-columns-y-lane columns))
                        (zs (luvcraft::physics-body-columns-z-lane columns))
                        (qxs (luvcraft::physics-body-columns-qx-lane columns))
                        (qys (luvcraft::physics-body-columns-qy-lane columns))
                        (qzs (luvcraft::physics-body-columns-qz-lane columns))
                        (qws (luvcraft::physics-body-columns-qw-lane columns))
                        (radii (luvcraft::physics-body-columns-radius-lane columns))
                        (kinds (luvcraft::physics-body-columns-kind-lane columns)))
                    (dotimes (row len)
                      (let* ((kind (aref luvcraft::*body-kinds* (aref kinds row)))
                             (radius (* (aref radii row) (luvcraft::body-kind-draw-scale kind))))
                        (emit (aref handles row)
                              (aref xs row) (aref ys row) (aref zs row)
                              (aref qxs row) (aref qys row) (aref qzs row) (aref qws row)
                              kind radius))))))))))
    ;; Group by kind so UPLOAD-FIGURE-INSTANCES emits at most 3 instanced draws total.
    (nconc (nreverse hero) (nreverse medium) (nreverse compact))))
