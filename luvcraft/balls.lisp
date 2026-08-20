;;; The things with weight: what the block world does with its physics.
;;;
;;; PHYSICS.LISP simulates spheres and knows nothing about the game; this is
;;; the game's side of that seam.  It decides what a body looks like (a
;;; kind: a colour, a pattern, a glow, a size, a bounce), draws every body
;;; as a true ray-traced sphere -- one instance record per body for the
;;; sphere pipeline's camera-facing quads, patterned and lit in the shader
;;; (LUVCRAFT/SHADERS.LISP, role :PHYSICS-SPHERE) -- while the shadow pass
;;; keeps rasterizing each body as a small turning cube, posts the player
;;; and the animals into the step as kinematic boxes so a ball rolls off a
;;; turtle and a walking player kicks what is in the way, and gives the
;;; world these ways to make bodies:
;;;
;;;   - the BALL, a hand item (B takes it out): left click throws one, right
;;;     click drops a whole handful; balls bounce, roll, pile up, and sleep;
;;;   - SCATTER-PARTY-BALLS, which bursts a deterministic party of beach
;;;     balls, smiley balls, and classic balls around a point;
;;;   - the FOUNTAIN block, which sprays drops of water that fall through
;;;     one another and splash out on whatever they land on;
;;;   - the LAVA SPRING block, which spits slow glowing gobbets that stick
;;;     where they land, cool, and go out.
;;;
;;; The events the step reports come back here too: a hit on a box owned by
;;; an animal startles it (CRITTER-STRUCK), and the step's own facts -- how
;;; many bodies are awake, how long it took -- are read off the world for
;;; the metabar's knobs.

(in-package #:luvcraft)

;;; ---------------------------------------------------------------------
;;; Three tiles of the atlas: what a ball, a drop, and a gobbet are painted with.

(defconstant +ball-tile+ :ball)
(defconstant +water-drop-tile+ :water)
(defconstant +lava-tile+ :lava)

(defmethod paint-block-atlas-tile ((tile (eql :ball)) x y)
  "A ball: worn red rubber with one pale band around it, so it is seen to turn."
  (let* ((seam (<= 6 y 8))
         (variation (round (block-atlas-variation x y tile) 3)))
    (if seam
        (shaded-block-atlas-pixel 226 214 190 variation)
        (shaded-block-atlas-pixel 196 62 48 variation))))

(defmethod paint-block-atlas-tile ((tile (eql :water)) x y)
  "Water: a clear cool blue with a lighter ripple through it."
  (let ((ripple (block-atlas-clump x y 733 5)))
    (shaded-block-atlas-pixel 78 140 214 (round (- ripple 128) 5))))

(defmethod paint-block-atlas-tile ((tile (eql :lava)) x y)
  "Lava: near-white heat veined through orange; the glow is added per body."
  (let ((vein (block-atlas-clump x y 977 4)))
    (if (> vein 175)
        (shaded-block-atlas-pixel 255 236 150 0)
        (shaded-block-atlas-pixel 240 96 24 (round (- vein 128) 6)))))

;;; ---------------------------------------------------------------------
;;; Body kinds: the game's palette of things a body can be.  The physics
;;; keeps only the index; everything the eye or the game wants is here.
;;; The albedo and pattern feed the sphere pipeline's instance stream, so
;;; this vector stays the single source of truth for what a kind looks
;;; like; the tile survives for the hand item and the atlas painters.

(defun body-pattern-index (pattern)
  "The sphere fragment stage's number for a surface PATTERN.

The shader selects the procedural pattern by this lane, so the pairing is
closed on both sides: a new pattern is a new shader arm and a new entry
here, together."
  (ecase pattern
    (:plain 0f0)
    (:banded 1f0)
    (:marble 2f0)
    (:beach 3f0)
    (:smiley 4f0)))

(defstruct (body-kind (:constructor make-body-kind
                          (name tile &key (emission 0.0) (radius 0.22)
                                          (mass 1.0) (restitution 0.5)
                                          (friction 0.5) (rolling-resistance 0.01)
                                          (damping 0.05) (collides-p t)
                                          (lifetime nil) (hit-report-p nil)
                                          (draw-scale 1.0)
                                          (albedo '(0.8 0.8 0.8))
                                          (pattern :plain)
                                          &aux
                                          (albedo-red (float (first albedo) 0f0))
                                          (albedo-green (float (second albedo) 0f0))
                                          (albedo-blue (float (third albedo) 0f0))
                                          (pattern-index
                                           (body-pattern-index pattern)))))
  (name :ball :type keyword)
  (tile :ball :type keyword)
  (emission 0.0 :type single-float)
  (radius 0.22 :type single-float)
  (mass 1.0 :type single-float)
  (restitution 0.5 :type single-float)
  (friction 0.5 :type single-float)
  (rolling-resistance 0.01 :type single-float)
  (damping 0.05 :type single-float)
  (collides-p t)
  (lifetime nil)
  (hit-report-p nil)
  ;; The drawn sphere's radius as a fraction of the physics radius.  One is
  ;; the honest size; the constructor keeps the knob for a kind that wants
  ;; to read smaller than it collides.
  (draw-scale 1.0 :type single-float)
  ;; The base colour the shader patterns over, resolved here so *BODY-KINDS*
  ;; stays the palette's one authority.
  (albedo-red 0.8 :type single-float)
  (albedo-green 0.8 :type single-float)
  (albedo-blue 0.8 :type single-float)
  (pattern :plain :type keyword)
  (pattern-index 0.0 :type single-float))

(defparameter *body-kinds*
  (vector
   (make-body-kind :ball +ball-tile+
                   :radius 0.22 :mass 1.0 :restitution 0.55 :friction 0.6
                   :rolling-resistance 0.015 :damping 0.08 :hit-report-p t
                   :albedo '(0.77 0.24 0.19) :pattern :banded)
   (make-body-kind :marble +ball-tile+
                   :radius 0.12 :mass 0.3 :restitution 0.4 :friction 0.5
                   :rolling-resistance 0.01 :damping 0.05
                   :albedo '(0.62 0.72 0.82) :pattern :marble)
   (make-body-kind :water-drop +water-drop-tile+
                   :radius 0.07 :mass 0.05 :restitution 0.15 :friction 0.2
                   :rolling-resistance 0.05 :damping 0.4 :collides-p nil
                   :lifetime 2.6 :albedo '(0.31 0.55 0.84))
   (make-body-kind :lava-gobbet +lava-tile+ :emission 1.5
                   :radius 0.14 :mass 0.6 :restitution 0.0 :friction 1.0
                   :rolling-resistance 0.4 :damping 1.2 :collides-p t
                   :lifetime 7.0 :albedo '(0.94 0.38 0.09))
   ;; The party kinds.  The beach ball is big, light, and bouncy; the
   ;; smiley ball is small, bouncier still, and wears its face in the
   ;; pattern frame, so the grin tumbles as it bounces.
   (make-body-kind :beach-ball +ball-tile+
                   :radius 0.45 :mass 0.4 :restitution 0.8 :friction 0.5
                   :rolling-resistance 0.008 :damping 0.06 :hit-report-p t
                   :albedo '(0.97 0.95 0.92) :pattern :beach)
   (make-body-kind :smiley-ball +ball-tile+
                   :radius 0.2 :mass 0.5 :restitution 0.9 :friction 0.55
                   :rolling-resistance 0.012 :damping 0.06 :hit-report-p t
                   :albedo '(0.99 0.80 0.16) :pattern :smiley))
  "The kinds a body may be, by index; the physics stores the index.")

(defun body-kind-index (name)
  (or (position name *body-kinds* :key #'body-kind-name)
      (error "There is no body kind named ~S." name)))

(defun spawn-body-of-kind (physics name x y z &key (vx 0.0) (vy 0.0) (vz 0.0)
                                                    lifetime)
  "Add a body of the kind NAME to PHYSICS and return its handle."
  (let* ((index (body-kind-index name))
         (kind (aref *body-kinds* index)))
    (spawn-physics-body physics x y z
                        :radius (body-kind-radius kind)
                        :mass (body-kind-mass kind)
                        :vx vx :vy vy :vz vz
                        :restitution (body-kind-restitution kind)
                        :friction (body-kind-friction kind)
                        :rolling-resistance (body-kind-rolling-resistance kind)
                        :damping (body-kind-damping kind)
                        :kind index
                        :collides-with-bodies-p (body-kind-collides-p kind)
                        :hit-report-p (body-kind-hit-report-p kind)
                        :lifetime (or lifetime (body-kind-lifetime kind)))))

;;; ---------------------------------------------------------------------
;;; Drawing.  The scene pass gets one 16-float instance record per body --
;;; centre and radius, orientation quaternion, albedo and emission, sampled
;;; light and pattern -- for the sphere pipeline's camera-facing quads; the
;;; shadow pass still rasterizes each body as a small turning cube in the
;;; block pipeline's vertex format, which is cheap and correct for a depth
;;; map.  Both streams are preallocated specialized arrays owned by one
;;; streams record in the session; displaced views of the filled parts are
;;; what the frame uploads.  Because the sphere quads blend without writing
;;; depth, the instance records are handed over farthest-from-the-eye
;;; first, so a pile of balls occludes itself correctly.

(defconstant +physics-render-body-limit+ 2000
  "The most bodies drawn in one frame; both streams are sized for it.")
(defconstant +physics-vertices-per-body+ 36
  "Cube vertices per body in the shadow stream.")
(defconstant +physics-shadow-vertex-buffer-size+
  (* 4 +physics-render-body-limit+ +physics-vertices-per-body+
     +block-mesh-floats-per-vertex+))
(defconstant +physics-instance-floats-per-body+ 16
  "Four vec4 lanes per body in the sphere instance stream.")
(defconstant +physics-instance-buffer-size+
  (* 4 +physics-render-body-limit+ +physics-instance-floats-per-body+))
(defconstant +physics-shadow-cube-scale+ 0.78
  "The shadow cube's half-size as a fraction of the drawn sphere's radius:
a touch under, toward the inscribed cube, so shadows do not read fat.")

(defparameter *physics-cube-template* nil
  "Thirty-six vertices of a unit cube about the origin: for each, the local
corner (-0.5..0.5), the tile-local UV, and the face normal, flat.")

(defun physics-cube-template ()
  (or *physics-cube-template*
      (setf *physics-cube-template*
            (let ((template (make-array (* 36 8) :element-type 'single-float))
                  (index 0))
              (dolist (face *block-faces*)
                (let* ((normal (block-face-neighbor face))
                       (corners (block-face-corners face)))
                  (dolist (corner-index '(0 1 2 0 2 3))
                    (let ((corner (nth corner-index corners)))
                      (multiple-value-bind (u v) (block-face-local-uv face corner)
                        (setf (aref template (+ index 0)) (- (float (first corner) 0f0) 0.5f0)
                              (aref template (+ index 1)) (- (float (second corner) 0f0) 0.5f0)
                              (aref template (+ index 2)) (- (float (third corner) 0f0) 0.5f0)
                              (aref template (+ index 3)) (float u 0f0)
                              (aref template (+ index 4)) (float v 0f0)
                              (aref template (+ index 5)) (float (voxel-direction-dx normal) 0f0)
                              (aref template (+ index 6)) (float (voxel-direction-dy normal) 0f0)
                              (aref template (+ index 7)) (float (voxel-direction-dz normal) 0f0))
                        (incf index 8))))))
              template))))

(defstruct (physics-body-streams (:constructor make-physics-body-streams ()))
  "The per-frame render products built from the physics columns.

INSTANCES holds the sphere records the scene pass draws, already ordered
farthest first for the blend; SCRATCH holds them in storage order before
the sort, with DEPTHS and ORDER as the sort's reusable working set.
SHADOW-VERTICES holds the little cubes the shadow pass rasterizes, whose
order no depth map cares about."
  (instances (make-array (* +physics-render-body-limit+
                            +physics-instance-floats-per-body+)
                         :element-type 'single-float :initial-element 0f0)
   :type (simple-array single-float (*)))
  (scratch (make-array (* +physics-render-body-limit+
                          +physics-instance-floats-per-body+)
                       :element-type 'single-float :initial-element 0f0)
   :type (simple-array single-float (*)))
  (depths (make-array +physics-render-body-limit+
                      :element-type 'single-float :initial-element 0f0)
   :type (simple-array single-float (*)))
  (order (make-array +physics-render-body-limit+
                     :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)))
  (shadow-vertices (make-array (* +physics-render-body-limit+
                                  +physics-vertices-per-body+
                                  +block-mesh-floats-per-vertex+)
                               :element-type 'single-float :initial-element 0f0)
   :type (simple-array single-float (*))))

(defun ensure-physics-body-streams (session)
  ;; The session's PHYSICS-VERTEX-STREAM slot holds the whole streams
  ;; record; its PHYSICS-VERTEX-COUNT slot holds the drawn body count.
  (or (luvcraft-session-physics-vertex-stream session)
      (setf (luvcraft-session-physics-vertex-stream session)
            (make-physics-body-streams))))

(defun physics-body-light-levels (world x y z)
  "Normalized sky and block light at a body's centre; open sky when absent."
  (multiple-value-bind (sky block status)
      (world-light-at world (floor x) (floor y) (floor z))
    (if (eq status :resident)
        (values (/ sky 15f0) (/ block 15f0))
        (values 1f0 0f0))))

(defun build-physics-body-streams (physics world streams eye-x eye-y eye-z)
  "Fill STREAMS from PHYSICS's bodies and return how many were drawn.

One pass writes each body's sphere instance record into the scratch stream
in storage order, its squared distance to the eye beside it, and its
shadow cube's corners -- position lanes only; the shadow stage reads
nothing else.  The instance records are then dealt into the upload stream
farthest first, because the scene pass blends them over each other without
writing depth."
  (declare (type physics-body-streams streams)
           (single-float eye-x eye-y eye-z)
           (optimize (speed 3) (safety 1)))
  (let* ((template (physics-cube-template))
         (kinds *body-kinds*)
         (limit +physics-render-body-limit+)
         (scratch (physics-body-streams-scratch streams))
         (instances (physics-body-streams-instances streams))
         (depths (physics-body-streams-depths streams))
         (order (physics-body-streams-order streams))
         (shadow (physics-body-streams-shadow-vertices streams))
         (drawn 0))
    (declare (type (simple-array single-float (*))
                   template scratch instances depths shadow)
             (type (simple-array fixnum (*)) order)
             (type simple-vector kinds)
             (fixnum drawn limit))
    (dolist (columns (list (physics-world-awake physics) (physics-world-sleeping physics)))
      (records:with-columnar-buffer-storage
          ((count row (xs x) (ys y) (zs z) (radii radius)
            (qxs qx) (qys qy) (qzs qz) (qws qw) (kind-indices kind))
           columns physics-body-columns)
        (declare (ignore row))
        (dotimes (i count)
          (when (>= drawn limit)
            (return))
          (let* ((kind (aref kinds (aref kind-indices i)))
                 (cx (aref xs i)) (cy (aref ys i)) (cz (aref zs i))
                 (drawn-radius (* (aref radii i) (body-kind-draw-scale kind)))
                 (emission (body-kind-emission kind))
                 (qx (aref qxs i)) (qy (aref qys i)) (qz (aref qzs i)) (qw (aref qws i))
                 ;; The rotation matrix of the unit quaternion, for the
                 ;; shadow cube's turned corners.
                 (xx (* qx qx)) (yy (* qy qy)) (zz (* qz qz))
                 (xy (* qx qy)) (xz (* qx qz)) (yz (* qy qz))
                 (wx (* qw qx)) (wy (* qw qy)) (wz (* qw qz))
                 (m00 (- 1f0 (* 2f0 (+ yy zz)))) (m01 (* 2f0 (- xy wz))) (m02 (* 2f0 (+ xz wy)))
                 (m10 (* 2f0 (+ xy wz))) (m11 (- 1f0 (* 2f0 (+ xx zz)))) (m12 (* 2f0 (- yz wx)))
                 (m20 (* 2f0 (- xz wy))) (m21 (* 2f0 (+ yz wx))) (m22 (- 1f0 (* 2f0 (+ xx yy))))
                 (scale (* 2f0 (* drawn-radius +physics-shadow-cube-scale+)))
                 (out (* drawn +physics-instance-floats-per-body+))
                 (shadow-out (* drawn +physics-vertices-per-body+
                                +block-mesh-floats-per-vertex+))
                 (dx (- cx eye-x)) (dy (- cy eye-y)) (dz (- cz eye-z)))
            (declare (type body-kind kind)
                     (single-float cx cy cz drawn-radius emission qx qy qz qw
                                   xx yy zz xy xz yz wx wy wz
                                   m00 m01 m02 m10 m11 m12 m20 m21 m22 scale
                                   dx dy dz)
                     (fixnum out shadow-out))
            (multiple-value-bind (sky-level block-level)
                (physics-body-light-levels world cx cy cz)
              (declare (single-float sky-level block-level))
              (setf (aref scratch (+ out 0)) cx
                    (aref scratch (+ out 1)) cy
                    (aref scratch (+ out 2)) cz
                    (aref scratch (+ out 3)) drawn-radius
                    (aref scratch (+ out 4)) qx
                    (aref scratch (+ out 5)) qy
                    (aref scratch (+ out 6)) qz
                    (aref scratch (+ out 7)) qw
                    (aref scratch (+ out 8)) (body-kind-albedo-red kind)
                    (aref scratch (+ out 9)) (body-kind-albedo-green kind)
                    (aref scratch (+ out 10)) (body-kind-albedo-blue kind)
                    (aref scratch (+ out 11)) emission
                    (aref scratch (+ out 12)) sky-level
                    (aref scratch (+ out 13)) block-level
                    (aref scratch (+ out 14)) (body-kind-pattern-index kind)
                    (aref scratch (+ out 15)) 0f0
                    (aref depths drawn) (+ (* dx dx) (+ (* dy dy) (* dz dz)))))
            (dotimes (v +physics-vertices-per-body+)
              (let* ((t0 (* v 8))
                     (lx (* scale (aref template t0)))
                     (ly (* scale (aref template (+ t0 1))))
                     (lz (* scale (aref template (+ t0 2)))))
                (declare (fixnum t0) (single-float lx ly lz))
                (setf (aref shadow (+ shadow-out 0))
                      (+ cx (* m00 lx) (* m01 ly) (* m02 lz))
                      (aref shadow (+ shadow-out 1))
                      (+ cy (* m10 lx) (* m11 ly) (* m12 lz))
                      (aref shadow (+ shadow-out 2))
                      (+ cz (* m20 lx) (* m21 ly) (* m22 lz)))
                (incf shadow-out +block-mesh-floats-per-vertex+)))
            (incf drawn)))))
    ;; Farthest first for the blend.  The displaced view is one small header
    ;; per frame; SBCL sorts vectors in place.
    (dotimes (i drawn)
      (setf (aref order i) i))
    (when (> drawn 1)
      (sort (make-array drawn :element-type 'fixnum :displaced-to order)
            (lambda (a b)
              (> (aref depths a) (aref depths b)))))
    (dotimes (slot drawn)
      (let ((source (* (aref order slot) +physics-instance-floats-per-body+))
            (target (* slot +physics-instance-floats-per-body+)))
        (declare (fixnum source target))
        (dotimes (lane +physics-instance-floats-per-body+)
          (setf (aref instances (+ target lane))
                (aref scratch (+ source lane))))))
    drawn))

(defun luvcraft-physics-body-count (session)
  "How many bodies the last build of the session's streams drew."
  (luvcraft-session-physics-vertex-count session))

(defun luvcraft-physics-instance-stream (session)
  "The filled sphere instance records, farthest first, for the upload."
  (make-array (* (luvcraft-session-physics-vertex-count session)
                 +physics-instance-floats-per-body+)
              :element-type 'single-float
              :displaced-to (physics-body-streams-instances
                             (ensure-physics-body-streams session))))

(defun luvcraft-physics-shadow-vertex-count (session)
  "How many cube vertices the shadow pass draws for the bodies."
  (* (luvcraft-session-physics-vertex-count session)
     +physics-vertices-per-body+))

(defun luvcraft-physics-shadow-stream (session)
  "The filled part of the shadow cube stream, for the upload."
  (make-array (* (luvcraft-physics-shadow-vertex-count session)
                 +block-mesh-floats-per-vertex+)
              :element-type 'single-float
              :displaced-to (physics-body-streams-shadow-vertices
                             (ensure-physics-body-streams session))))

;;; ---------------------------------------------------------------------
;;; The session's physics: made on first use, stepped at a fixed rate from
;;; the frame clock, fed the player and the animals as boxes, and asked
;;; afterwards what happened.

(defparameter *physics-step-seconds* (/ 1d0 60d0)
  "The fixed step the session's physics advances by; four substeps inside.")
(defparameter *physics-max-steps-per-frame* 3
  "How many steps one frame may catch up; beyond that time is dropped,
so a stall does not become a spiral.")

(defun ensure-luvcraft-physics (session)
  (or (luvcraft-session-physics session)
      (let ((physics (make-physics-world :terrain (luvcraft-session-world session))))
        (setf (physics-world-step-seconds physics)
              (coerce *physics-step-seconds* 'single-float)
              (luvcraft-session-physics session) physics)
        (find-luvcraft-springs session)
        physics)))

(defun post-luvcraft-kinematic-boxes (session physics)
  "Tell PHYSICS where the player and the animals are and how they move."
  (clear-physics-boxes physics)
  (let ((player (luvcraft-session-player session)))
    (when player
      (let ((x (player-x player)) (y (player-y player)) (z (player-z player))
            (half (player-half-width player)))
        (post-physics-box physics
                          (- x half) y (- z half)
                          (+ x half) (+ y (player-height player)) (+ z half)
                          :vx (player-velocity-x player)
                          :vy (player-velocity-y player)
                          :vz (player-velocity-z player)
                          :owner player))))
  (loop for critter across (critter-population-critters (luvcraft-session-critters session))
        do (let ((x (critter-x critter)) (y (critter-y critter)) (z (critter-z critter))
                 (half (critter-half-width critter)))
             (post-physics-box physics
                               (- x half) y (- z half)
                               (+ x half) (+ y (critter-height critter)) (+ z half)
                               :vx (critter-velocity-x critter)
                               :vy (critter-velocity-y critter)
                               :vz (critter-velocity-z critter)
                               :owner critter)))
  physics)

(defgeneric critter-struck (critter x y z speed)
  (:documentation
   "Something hit CRITTER at X,Y,Z arriving at SPEED.  Most animals ignore
it; a turtle takes it as a reason to go somewhere else."))

(defmethod critter-struck (critter x y z speed)
  (declare (ignore critter x y z speed))
  nil)

(defmethod critter-struck ((turtle turtle) x y z speed)
  (declare (ignore x y z))
  (when (> speed 2.5)
    (setf (turtle-resting-p turtle) nil
          (turtle-mood-seconds turtle) 3d0)
    (turtle-turn-away turtle)))

(defun handle-luvcraft-physics-events (session physics)
  "Act on what the step found out."
  (declare (ignore session))
  (do-physics-events (kind handle-a handle-b owner x y z speed physics)
    (when (and (eq kind :hit) (typep owner 'critter))
      (critter-struck owner x y z speed))))

(defun advance-luvcraft-physics (session seconds)
  "Step the session's physics as many fixed steps as SECONDS owe, then
build this frame's sphere instance and shadow cube streams."
  (let ((physics (ensure-luvcraft-physics session)))
    (incf (luvcraft-session-physics-clock session) seconds)
    (let ((steps 0))
      (loop while (and (>= (luvcraft-session-physics-clock session) *physics-step-seconds*)
                       (< steps *physics-max-steps-per-frame*))
            do (decf (luvcraft-session-physics-clock session) *physics-step-seconds*)
               (incf steps)
               (post-luvcraft-kinematic-boxes session physics)
               (spray-luvcraft-springs session physics)
               (step-physics-world physics)
               (handle-luvcraft-physics-events session physics))
      ;; A frame that fell far behind forgets the rest rather than
      ;; catching up forever.
      (when (>= (luvcraft-session-physics-clock session) *physics-step-seconds*)
        (setf (luvcraft-session-physics-clock session) 0d0)))
    (let ((eye (camera-position (luvcraft-session-camera session))))
      (setf (luvcraft-session-physics-vertex-count session)
            (build-physics-body-streams
             physics (luvcraft-session-world session)
             (ensure-physics-body-streams session)
             (coerce (vec3-x eye) 'single-float)
             (coerce (vec3-y eye) 'single-float)
             (coerce (vec3-z eye) 'single-float))))
    physics))

;;; ---------------------------------------------------------------------
;;; Springs: the fountain and the lava well.  A spring is a coordinate the
;;; session found in its world's authored edits, or was told about when the
;;; player placed one; each physics step it may throw something, if its
;;; cell is resident and still holds a spring.

(defstruct (luvcraft-spring (:constructor make-luvcraft-spring (x y z kind)))
  (x 0 :type fixnum) (y 0 :type fixnum) (z 0 :type fixnum)
  (kind :fountain :type keyword)
  (count 0 :type fixnum))

(defun world-spring-coordinates (world)
  "The (X Y Z KIND) of every spring block written into WORLD's edits."
  (let ((source (block-world-source world)))
    (when (typep source 'little-world-source)
      (loop for coordinate being the hash-keys
              of (block-edit-overlay-entries (little-world-source-edits source))
              using (hash-value block)
            when (typep block 'spring-block-kind)
              collect (destructuring-bind (x y z) coordinate
                        (make-luvcraft-spring x y z (block-kind-name block)))))))

(defun find-luvcraft-springs (session)
  "Rediscover the world's springs from its authored edits."
  (setf (luvcraft-session-springs session)
        (world-spring-coordinates (luvcraft-session-world session))))

(defmethod luvcraft-block-placed ((block spring-block-kind) session x y z)
  (push (make-luvcraft-spring x y z (block-kind-name block))
        (luvcraft-session-springs session)))

(defmethod luvcraft-block-removed ((block spring-block-kind) session x y z)
  (setf (luvcraft-session-springs session)
        (remove-if (lambda (spring)
                     (and (= x (luvcraft-spring-x spring))
                          (= y (luvcraft-spring-y spring))
                          (= z (luvcraft-spring-z spring))))
                   (luvcraft-session-springs session))))

(defun spring-noise (spring salt)
  "A stable 0..1 reading for SPRING's current throw and SALT."
  (critter-noise (+ (* 7919 (luvcraft-spring-x spring))
                    (* 104729 (luvcraft-spring-y spring))
                    (* 1299709 (luvcraft-spring-z spring)))
                 (luvcraft-spring-count spring) salt))

(defparameter *fountain-drops-per-step* 2)
(defparameter *lava-gobbet-period* 6
  "A lava spring spits one gobbet every this many steps.")

(defun spray-luvcraft-spring (spring session physics)
  (let ((x (luvcraft-spring-x spring))
        (y (luvcraft-spring-y spring))
        (z (luvcraft-spring-z spring)))
    (multiple-value-bind (block status)
        (world-block-at (luvcraft-session-world session) x y z)
      (when (and (eq status :resident) (typep block 'spring-block-kind))
        (ecase (luvcraft-spring-kind spring)
          (:fountain
           (dotimes (i *fountain-drops-per-step*)
             (incf (luvcraft-spring-count spring))
             (let* ((angle (* 2d0 pi (spring-noise spring 1)))
                    (spread (* 1.6d0 (spring-noise spring 2)))
                    (up (+ 9.5d0 (* 2.5d0 (spring-noise spring 3)))))
               (spawn-body-of-kind physics :water-drop
                                   (+ x 0.5d0 (* 0.12d0 (cos angle)))
                                   (+ y 1.1d0)
                                   (+ z 0.5d0 (* 0.12d0 (sin angle)))
                                   :vx (* spread (cos angle))
                                   :vy up
                                   :vz (* spread (sin angle))))))
          (:lava-spring
           (incf (luvcraft-spring-count spring))
           (when (zerop (mod (luvcraft-spring-count spring) *lava-gobbet-period*))
             (let* ((angle (* 2d0 pi (spring-noise spring 1)))
                    (spread (+ 0.4d0 (* 1.4d0 (spring-noise spring 2))))
                    (up (+ 5.0d0 (* 3.0d0 (spring-noise spring 3)))))
               (spawn-body-of-kind physics :lava-gobbet
                                   (+ x 0.5d0) (+ y 1.2d0) (+ z 0.5d0)
                                   :vx (* spread (cos angle))
                                   :vy up
                                   :vz (* spread (sin angle)))))))))))

(defun spray-luvcraft-springs (session physics)
  (dolist (spring (luvcraft-session-springs session))
    (spray-luvcraft-spring spring session physics)))

;;; ---------------------------------------------------------------------
;;; The ball in the hand.

(defparameter *ball-throw-speed* 14.0d0
  "How fast a thrown ball leaves the hand, cells per second.")

(defclass ball ()
  ((thrown :initform 0 :accessor ball-thrown-count
           :documentation "How many balls this hand has thrown."))
  (:documentation
   "A ball to throw.  The hand never runs out: each throw is a new body."))

(defmethod hand-item-name ((item ball)) "ball")
(defmethod hand-item-box-count ((item ball)) 1)

(defmethod hand-item-carry-pose ((item ball) body)
  (declare (ignore body))
  ;; Held low and to the right, a little forward, the way you carry a ball
  ;; you mean to throw.
  '(0.26d0 -0.40d0 0.66d0 0.15d0 -0.10d0 0.0d0))

(defmethod map-hand-item-boxes ((item ball) body function)
  (declare (ignore body))
  ;; Small in the hand: the grip frame sits close to the eye.
  (let ((half 0.055d0))
    (funcall function 0d0 half 0d0 half half half +ball-tile+ :stretch-p t)))

(defun throw-luvcraft-ball (session &key (speed *ball-throw-speed*) (kind :ball)
                                         (aim-right 0d0) (aim-up 0d0))
  "Throw a body of KIND from the player's eye along the view, nudged by
AIM-RIGHT and AIM-UP; return its handle."
  (let* ((physics (ensure-luvcraft-physics session))
         (camera (luvcraft-session-camera session))
         (player (luvcraft-session-player session))
         (position (camera-position camera)))
    (multiple-value-bind (right up forward) (camera-basis camera)
      (let* ((sx aim-right)
             (sy aim-up)
             (dx (+ (vec3-x forward) (* sx (vec3-x right)) (* sy (vec3-x up))))
             (dy (+ (vec3-y forward) (* sx (vec3-y right)) (* sy (vec3-y up))))
             (dz (+ (vec3-z forward) (* sx (vec3-z right)) (* sy (vec3-z up))))
             (length (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
             (dx (/ dx length)) (dy (/ dy length)) (dz (/ dz length))
             (start-distance 0.7d0))
        (spawn-body-of-kind
         physics kind
         (+ (vec3-x position) (* start-distance dx) (* 0.25d0 (vec3-x right)))
         (+ (vec3-y position) (* start-distance dy) -0.15d0)
         (+ (vec3-z position) (* start-distance dz) (* 0.25d0 (vec3-z right)))
         :vx (+ (* speed dx) (if player (player-velocity-x player) 0d0))
         :vy (+ (* speed dy) (if player (player-velocity-y player) 0d0))
         :vz (+ (* speed dz) (if player (player-velocity-z player) 0d0)))))))

(defmethod hand-item-use ((item ball) body session button)
  (declare (ignore body))
  (case button
    (:left
     (incf (ball-thrown-count item))
     ;; Every fifth ball out of the hand is a smiley: enough to be a
     ;; delight, not so many that the ball stops being the ball.
     (throw-luvcraft-ball
      session :kind (if (zerop (mod (ball-thrown-count item) 5))
                        :smiley-ball
                        :ball))
     t)
    (:right
     ;; A handful of marbles, scattered the same way from the same count.
     (dotimes (i 12)
       (incf (ball-thrown-count item))
       (throw-luvcraft-ball
        session :kind :marble :speed 9d0
        :aim-right (* 0.5d0 (- (critter-noise 4242 (ball-thrown-count item) 1) 0.5d0))
        :aim-up (* 0.5d0 (- (critter-noise 4242 (ball-thrown-count item) 2) 0.5d0))))
     t)
    (t nil)))

(defun toggle-luvcraft-ball (session)
  "Take the session's ball out, or put it away."
  (toggle-hand-item (luvcraft-session-body session) session 'ball))

;;; ---------------------------------------------------------------------
;;; The party: a burst of balls for a birthday.

(defparameter *party-ball-kinds* #(:beach-ball :smiley-ball :ball :smiley-ball)
  "The rotation SCATTER-PARTY-BALLS deals from, in order.")

(defun scatter-party-balls (session &key center (count 20))
  "Scatter a deterministic mix of beach balls, smiley balls, and classic
balls around CENTER, each with a small pop-up velocity, and return their
handles, newest first.

CENTER is an (X Y Z) list; without one the balls burst a few cells ahead
of the session's camera.  The scatter is hashed, not random, so the same
call throws the same party."
  (let* ((physics (ensure-luvcraft-physics session))
         (center
           (or center
               (let ((camera (luvcraft-session-camera session)))
                 (multiple-value-bind (right up forward) (camera-basis camera)
                   (declare (ignore right up))
                   (let ((eye (camera-position camera)))
                     (list (+ (vec3-x eye) (* 4d0 (vec3-x forward)))
                           (+ (vec3-y eye) (* 4d0 (vec3-y forward)))
                           (+ (vec3-z eye) (* 4d0 (vec3-z forward)))))))))
         (handles '()))
    (destructuring-bind (x y z) center
      (dotimes (i count)
        (let* ((kind (aref *party-ball-kinds*
                           (mod i (length *party-ball-kinds*))))
               (angle (* 2d0 pi (critter-noise 7331 i 1)))
               (reach (* 2.5d0 (sqrt (critter-noise 7331 i 2))))
               (lift (+ 0.6d0 (* 1.4d0 (critter-noise 7331 i 3))))
               (up (+ 2.0d0 (* 3.0d0 (critter-noise 7331 i 4))))
               (spread (+ 0.5d0 (* 1.5d0 (critter-noise 7331 i 5)))))
          (push (spawn-body-of-kind physics kind
                                    (+ x (* reach (cos angle)))
                                    (+ y lift)
                                    (+ z (* reach (sin angle)))
                                    :vx (* spread (cos angle))
                                    :vy up
                                    :vz (* spread (sin angle)))
                handles))))
    handles))

;;; ---------------------------------------------------------------------
;;; The knobs: what the metabar can turn on the physics.

(defun physics-gravity-magnitude ()
  (- *physics-gravity*))
(defun (setf physics-gravity-magnitude) (magnitude)
  (setf *physics-gravity* (- (coerce magnitude 'single-float)))
  magnitude)

(defun ball-bounciness ()
  (body-kind-restitution (aref *body-kinds* (body-kind-index :ball))))
(defun (setf ball-bounciness) (bounciness)
  (setf (body-kind-restitution (aref *body-kinds* (body-kind-index :ball)))
        (coerce bounciness 'single-float))
  bounciness)

(define-knob physics-gravity
    (:group :physics :label "weight of things"
     :quantity (:quantity :gravity-magnitude :unit ((:cell 1) (:second -2)))
     :minimum 0.0 :maximum 60.0 :step 1.0)
    (physics-gravity-magnitude))
(define-knob ball-bounce
    (:group :physics :label "bounce of a ball"
     :quantity (:quantity :bounciness :unit :one)
     :minimum 0.0 :maximum 1.0 :step 0.05)
    (ball-bounciness))
(define-knob fountain-rate
    (:group :physics :label "drops per step"
     :quantity (:quantity :body-count :unit :one)
     :type integer :minimum 0 :maximum 8 :step 1)
    *fountain-drops-per-step*)
(define-knob physics-substeps
    (:group :physics :quantity (:quantity :substep-count :unit :one)
     :type integer :minimum 1 :maximum 8 :step 1)
    *physics-substeps*)

;;; ---------------------------------------------------------------------
;;; Reading the physics from the metabar.

(defun luvcraft-physics-awake-count (session)
  (let ((physics (luvcraft-session-physics session)))
    (if physics (physics-body-columns-length (physics-world-awake physics)) 0)))

(defun luvcraft-physics-asleep-count (session)
  (let ((physics (luvcraft-session-physics session)))
    (if physics (physics-body-columns-length (physics-world-sleeping physics)) 0)))

(defun luvcraft-physics-step-milliseconds (session)
  (let ((physics (luvcraft-session-physics session)))
    (if physics (* 1000d0 (physics-world-last-step-real-seconds physics)) 0d0)))

(defun luvcraft-physics-contact-count (session)
  (let ((physics (luvcraft-session-physics session)))
    (if physics (physics-world-last-step-contact-count physics) 0)))

(defun clear-luvcraft-physics-bodies (session)
  "Remove every ball, drop, and gobbet from the session's world."
  (let ((physics (luvcraft-session-physics session)))
    (when physics
      (clear-physics-bodies physics))
    session))
