;;;; A remote player's presence (#VM5TN6).
;;;;
;;;; What the track draws is a body: the avatar's skinned mesh through the
;;;; companions' vertex-shader skinning, walked by the snapshot's gait, the
;;;; block from the pose in its right hand, and a nameplate over its head in
;;;; world text.  Its place in local physics is the track's body, which
;;;; session.lisp makes a stranger of the simulation; this file only draws.
(in-package #:luvcraft.luft.game)

;;; The avatar wears the companion body protocol, but no provider, motor, or
;;; dialogue: only its body overlay is ever added to a view.
(defclass remote-avatar (luvcraft.agent:embodied-agent)
  ((rest-pose :initform nil :accessor luvcraft.agent::companion-rest-pose
              :documentation "NIL to stand at rest, :SIT astride a turtle."))
  (:documentation "A remote player's skinned body, as one view draws it."))

(defmethod luvcraft.agent::embodied-agent-save-kind ((avatar remote-avatar))
  (declare (ignore avatar))
  :avatar)

(defmethod luvcraft.agent::embodied-agent-body-height ((avatar remote-avatar))
  (declare (ignore avatar))
  (luvcraft.agent::companion-body-height :avatar))

(defmethod luvcraft.agent::embodied-agent-body-sphere ((avatar remote-avatar))
  (let ((p (luvcraft.agent::embodied-agent-position avatar))
        (height (luvcraft.agent::companion-body-height :avatar)))
    (values (vec3:vec3-x p) (+ (vec3:vec3-y p) (* 0.5 height)) (vec3:vec3-z p)
            (* 0.65 height))))

(defparameter *nameplate-font-pathname*
  (asdf:system-relative-pathname "luft/render" "fonts/DMSans-Bold.ttf"))

(defclass remote-presences ()
  ((avatars :initform (make-hash-table :test #'eq) :reader presences-avatars
            :documentation "Remote player -> its REMOTE-AVATAR in this view.")
   (glyph-cache :initform nil :accessor presences-glyph-cache)
   (batch :initform nil :accessor presences-batch)
   (key :initform nil :accessor presences-key
        :documentation "What the nameplate batch was built for."))
  (:documentation "One view's remote bodies and their nameplates."))

(defun remote-presences (viewer)
  (or (find-if (lambda (overlay) (typep overlay 'remote-presences))
               (render:viewer-overlays viewer))
      (render:add-viewer-overlay viewer (make-instance 'remote-presences))))

(defun release-presence-avatar (viewer avatar)
  (let ((body (luvcraft.agent::gnome-body avatar)))
    (when body (render:remove-viewer-overlay viewer body))))

(defmethod render:release-viewer-overlay ((state remote-presences))
  (when (presences-batch state)
    (render::release-world-text-batch (presences-batch state))
    (setf (presences-batch state) nil))
  (when (presences-glyph-cache state)
    (luv.slug:release-slug-glyph-cache (presences-glyph-cache state))
    (setf (presences-glyph-cache state) nil)))

;;; The pose: on its feet where the track shows it, or astride the turtle it
;;; rides (#U4TBMP), which a rider's pose carries.

(defparameter *avatar-hip-height* 0.8
  "The avatar's hips above its soles, in figure units: what sits on a shell.")

(defun remote-ridden-turtle (world player)
  "The turtle WORLD knows PLAYER to ride, or NIL."
  (loop for id being the hash-keys of (living-rides (living world)) using (hash-value rider)
        when (equal rider (player-id player))
          return (find-world-turtle world id)))

(defun remote-body-walking-p (shown)
  (let ((v (pose-snapshot-velocity shown)))
    (and (not (pose-snapshot-flying shown))
         (> (+ (expt (svref v 0) 2) (expt (svref v 1) 2)) 0.09d0))))

(defun remote-body-placement (world player shown)
  "Where PLAYER's body stands, in presentation (Y-up) axes, its facing, and
whether it walks.  A rider sits in the seat a local rider's eyes are over,
hips on the shell, and does not walk; its legs go into the shell."
  (let ((turtle (remote-ridden-turtle world player)))
    (if turtle
        (multiple-value-bind (sway-lift sway-yaw) (luvcraft::critter-sway turtle)
          (let ((yaw (luvcraft::critter-yaw turtle))
                (setback luvcraft::*critter-ride-seat-offset*))
            (values (vec3:make-vec3
                     (+ (luvcraft::critter-x turtle) (* setback (sin yaw)))
                     (+ (luvcraft::critter-y turtle) (luvcraft::critter-height turtle) sway-lift
                        (- (* *avatar-hip-height* (luvcraft.agent::companion-stature :avatar))))
                     (+ (luvcraft::critter-z turtle) (* setback (cos yaw))))
                    (+ yaw sway-yaw) nil)))
        (let ((p (pose-snapshot-position shown))
              (offset (game::luvcraft-world-source-offset (world-source world))))
          ;; Luft's yaw turns from +x toward +y; a companion's from +z toward +x.
          (values (vec3:make-vec3 (- (svref p 0) offset) (svref p 2) (- (svref p 1) offset))
                  (- (/ pi 2) (pose-snapshot-yaw shown))
                  (remote-body-walking-p shown))))))

(defun remote-body-walk-phase (shown)
  "The Walk loop's phase for SHOWN's gait.  The gait turns by pi each half
step the body travels (+WALKING-HALF-STEP+ cells), and one loop of the
avatar's walk covers its stride, so the feet plant where they land."
  (let ((mesh (luvcraft.agent::companion-mesh :avatar)))
    (mod (/ (* (pose-snapshot-gait shown) terrain::+walking-half-step+)
            (* pi (luvcraft.agent::companion-stature :avatar)
               (luvcraft.agent::companion-mesh-stride mesh)))
         1d0)))

(defun pose-remote-avatar (world avatar player shown seconds)
  "Place AVATAR as PLAYER's body stands in WORLD, walking by SHOWN's
gait, or sitting on the turtle it rides."
  (multiple-value-bind (position facing walking-p)
      (remote-body-placement world player shown)
    (setf (luvcraft.agent::companion-rest-pose avatar)
          (and (remote-ridden-turtle world player) :sit)
          (luvcraft.agent::embodied-agent-position avatar) position
          (luvcraft.agent::embodied-agent-facing-yaw avatar) facing
          (luvcraft.agent::companion-walk-phase avatar) (remote-body-walk-phase shown))
    (let ((blend (luvcraft.agent::companion-walk-blend avatar)))
      (setf (luvcraft.agent::companion-walk-blend avatar)
            (+ blend (* (coerce (- 1 (exp (* -10 seconds))) 'single-float)
                        (- (if walking-p 1.0 0.0) blend))))))
  avatar)

(defun sync-remote-avatars (viewer seconds)
  "Give each remote player VIEWER can see a body, posed as its track draws
it, and take down the bodies of those gone."
  (let* ((state (remote-presences viewer))
         (avatars (presences-avatars state))
         (seen nil))
    (dolist (player (world-players (viewer-world viewer)))
      (let* ((track (player-track player))
             (shown (and track (track-shown track))))
        (when shown
          (let ((avatar (or (gethash player avatars)
                            (setf (gethash player avatars)
                                  (make-instance 'remote-avatar
                                                 :session viewer :x 0 :y 0 :z 0
                                                 :position (vec3:make-vec3 0 0 0))))))
            (push player seen)
            (pose-remote-avatar (viewer-world viewer) avatar player shown seconds)
            (luvcraft.agent::ensure-embodied-agent-body avatar)))))
    (loop for player being the hash-keys of avatars using (hash-value avatar)
          unless (member player seen)
            do (release-presence-avatar viewer avatar)
               (remhash player avatars))))

(defmethod luvcraft.agent::ensure-embodied-agent-body ((avatar remote-avatar))
  (or (luvcraft.agent::gnome-body avatar)
      (luvcraft.agent::make-companion-mesh-body avatar)))

(defmethod render:advance-viewer-game :after ((viewer luvcraft-viewer) seconds)
  (sync-remote-avatars viewer seconds))

;;; The block in hand: a box at the right palm, as the walk swings it or the
;;; seat holds it.

(defparameter *held-block-half-size* 0.12)

(defun remote-held-block-figure (viewer avatar block)
  (let* ((mesh (luvcraft.agent::companion-mesh :avatar))
         (palm (luvcraft.agent::companion-rig-palm (luvcraft.agent::companion-mesh-rig mesh)))
         ;; The pose the body is drawn in, seated or walking.
         (palette (luvcraft.agent::sample-agent-palette
                   avatar mesh (make-array (* 12 (luvcraft.agent::companion-mesh-bone-count mesh))
                                           :element-type 'single-float)))
         (local (luvcraft.agent::companion-palette-point palette (car palm) (cdr palm)))
         ;; Hold it below the palm and a little forward, not through the fist.
         (grip (luvcraft.agent::ik-offset local (vec3:make-vec3 0 -0.1 0.08)))
         (center (host-position viewer (luvcraft.agent::companion-world-point avatar grip)))
         (yaw (- (/ pi 2) (luvcraft.agent::embodied-agent-facing-yaw avatar)))
         (half *held-block-half-size*))
    (render:box-figure center center (cos yaw) (sin yaw) (list half half half)
                       (nth-value 1 (terrain:material-kind-oriented-tones
                                     (terrain:material-placement-kind
                                      (game:block-placement
                                       (luvcraft::block-identity-material block))))))))

(defun remote-player-figures (viewer)
  "The figure instances of VIEWER's remote players: the blocks in their hands.
Their bodies are skinned meshes, drawn by their own overlays."
  (let ((avatars (presences-avatars (remote-presences viewer))))
    (loop for player in (world-players (viewer-world viewer))
          for track = (player-track player)
          for shown = (and track (track-shown track))
          for avatar = (gethash player avatars)
          when (and shown avatar (pose-snapshot-held shown))
            collect (remote-held-block-figure viewer avatar (pose-snapshot-held shown)))))

;;; Nameplates: the player's name over its head, turned to this camera, and
;;; growing with distance so it stays legible across a valley.

(defparameter *nameplate-lift* 0.35 "Cells between the head and the plate's foot.")
(defparameter *nameplate-near-height* 0.2 "The em size of a nearby name, in cells.")
(defparameter *nameplate-angular-height* 0.025
  "The em size per cell of distance, once that is more than the near size.")
(defparameter *nameplate-ink* '(1.6 1.55 1.4))
(defparameter *nameplate-ground* '(0.015 0.015 0.02 0.5)
  "The plate behind the name, premultiplied.")

(defun nameplate-records (cache atlas name center right up em)
  "Panel and glyph records for NAME centred on CENTER, written along RIGHT
and UP at EM cells to the em, addressing ATLAS, which holds its glyphs."
  (let* ((loader (luv.slug:slug-font-loader *nameplate-font-pathname*))
         (glyphs (luv.slug:make-slug-glyph-placements
                  (luv.slug:cached-slug-shaped-text cache *nameplate-font-pathname* name)
                  loader cache *nameplate-font-pathname*))
         (panels (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0))
         (records (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0))
         (normal (vec3:vec3-cross right up)))
    (when glyphs
      (let* ((left (reduce #'min glyphs :key (lambda (g) (+ (luv.slug:slug-glyph-placement-origin-x g)
                                                          (luv.slug:slug-glyph-placement-outline-min-x g)))))
             (width (- (reduce #'max glyphs :key (lambda (g) (+ (luv.slug:slug-glyph-placement-origin-x g)
                                                              (luv.slug:slug-glyph-placement-outline-max-x g))))
                       left))
             (x0 (- (* em (+ left (/ width 2)))))
             (y0 (* em -0.35))
             (pad (* em 0.3)))
        (flet ((at (x y lift)
                 (vec3:make-vec3 (+ (vec3:vec3-x center) (* x (vec3:vec3-x right)) (* y (vec3:vec3-x up))
                                    (* lift (vec3:vec3-x normal)))
                                 (+ (vec3:vec3-y center) (* x (vec3:vec3-y right)) (* y (vec3:vec3-y up))
                                    (* lift (vec3:vec3-y normal)))
                                 (+ (vec3:vec3-z center) (* x (vec3:vec3-z right)) (* y (vec3:vec3-z up))
                                    (* lift (vec3:vec3-z normal)))))
               (edge (v length) (render::scaled-vec3 v length)))
          (render::push-panel-record panels (at (- (+ (* em (/ width 2)) pad)) (- (* em 0.62)) 0)
                                     (edge right (+ (* em width) (* 2 pad)))
                                     (edge up (* em 1.24)) *nameplate-ground*)
          (dolist (glyph glyphs)
            (let* ((resource (luv.slug:slug-glyph-placement-resource glyph))
                   (serialized (luv.slug:slug-device-glyph-serialized resource))
                   (location (gethash resource (luv.slug:slug-glyph-atlas-locations atlas)))
                   (min-x (luv.slug:slug-glyph-placement-outline-min-x glyph))
                   (min-y (luv.slug:slug-glyph-placement-outline-min-y glyph))
                   (max-x (luv.slug:slug-glyph-placement-outline-max-x glyph))
                   (max-y (luv.slug:slug-glyph-placement-outline-max-y glyph))
                   (origin (at (+ x0 (* em (+ (luv.slug:slug-glyph-placement-origin-x glyph) min-x)))
                               (+ y0 (* em (+ (luv.slug:slug-glyph-placement-origin-y glyph) min-y)))
                               0.004))
                   (across (edge right (* em (- max-x min-x))))
                   (along (edge up (* em (- max-y min-y)))))
              (render::push-floats
               records
               (vec3:vec3-x origin) (vec3:vec3-y origin) (vec3:vec3-z origin) min-x
               (vec3:vec3-x across) (vec3:vec3-y across) (vec3:vec3-z across) min-y
               (vec3:vec3-x along) (vec3:vec3-y along) (vec3:vec3-z along) max-x
               max-y
               (luv.slug:slug-serialized-outline-horizontal-band-count serialized)
               (luv.slug:slug-serialized-outline-vertical-band-count serialized)
               (first location) (second location) min-x min-y max-x max-y
               (first *nameplate-ink*) (second *nameplate-ink*) (third *nameplate-ink*))))
          (values panels records))))))

(defun nameplate-placements (viewer)
  "(NAME CENTER RIGHT UP EM) for each remote player VIEWER draws."
  (let ((eye (render:camera-position (render:viewer-camera viewer)))
        (head (luvcraft.agent::companion-body-height :avatar))
        (stature (luvcraft.agent::companion-stature :avatar)))
    (loop for player in (world-players (viewer-world viewer))
          for track = (player-track player)
          for shown = (and track (track-shown track))
          when shown
            ;; Over the body where it stands, or sits on its turtle.
            collect (let* ((p (let ((feet (host-position viewer (remote-body-placement
                                                                  (viewer-world viewer) player shown))))
                                (vector (vec3:vec3-x feet) (vec3:vec3-y feet) (vec3:vec3-z feet))))
                           (dx (- (vec3:vec3-x eye) (svref p 0)))
                           (dy (- (vec3:vec3-y eye) (svref p 1)))
                           (flat (max 1d-6 (sqrt (+ (* dx dx) (* dy dy)))))
                           (distance (sqrt (+ (* dx dx) (* dy dy)
                                              (expt (- (vec3:vec3-z eye) (svref p 2)) 2))))
                           (em (max *nameplate-near-height* (* *nameplate-angular-height* distance)))
                           ;; Face the camera: right is the camera's right.
                           (right (vec3:make-vec3 (- (/ dy flat)) (/ dx flat) 0)))
                      (list (player-name player)
                            (vec3:make-vec3 (svref p 0) (svref p 1)
                                            (+ (svref p 2) (* head stature) *nameplate-lift*
                                               (* 0.62 em)))
                            right (vec3:make-vec3 0 0 1) em)))))

(defun nameplate-key (placements)
  "PLACEMENTS rounded to what a reader would notice: a centimetre, a degree."
  (mapcar (lambda (placement)
            (destructuring-bind (name center right up em) placement
              (declare (ignore up))
              (list name (round (vec3:vec3-x center) 0.01) (round (vec3:vec3-y center) 0.01)
                    (round (vec3:vec3-z center) 0.01)
                    (round (atan (vec3:vec3-y right) (vec3:vec3-x right)) 0.017)
                    (round em 0.01))))
          placements))

(defmethod render::viewer-world-text-batches ((viewer luvcraft-viewer))
  (let ((state (find-if (lambda (overlay) (typep overlay 'remote-presences))
                        (render:viewer-overlays viewer))))
    (when state
      (let* ((placements (nameplate-placements viewer))
             (key (nameplate-key placements)))
        (unless (equal key (presences-key state))
          (let ((old (presences-batch state))
                (device (render:viewer-device viewer)))
            (setf (presences-key state) key
                  (presences-batch state) nil)
            (when placements
              (let ((cache (or (presences-glyph-cache state)
                               (setf (presences-glyph-cache state)
                                     (luv.slug:make-slug-glyph-cache device))))
                    (panels (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0))
                    (glyphs (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0))
                    (atlas nil))
                ;; One batch shares one atlas: lay every name out against it.
                (let ((all (luv.slug:make-slug-glyph-placements
                            (luv.slug:cached-slug-shaped-text
                             cache *nameplate-font-pathname*
                             (format nil "~{~A~^ ~}" (mapcar #'first placements)))
                            (luv.slug:slug-font-loader *nameplate-font-pathname*)
                            cache *nameplate-font-pathname*)))
                  (setf atlas (and all (luv.slug:slug-glyph-atlas-for cache all))))
                (dolist (placement (and atlas placements))
                  (multiple-value-bind (p g) (apply #'nameplate-records cache atlas placement)
                    (when p
                      (loop for x across p do (vector-push-extend x panels))
                      (loop for x across g do (vector-push-extend x glyphs)))))
                (when atlas
                  (setf (presences-batch state)
                        (render::make-world-text-batch
                         device (coerce panels '(simple-array single-float (*)))
                         (coerce glyphs '(simple-array single-float (*))) atlas)))))
            (when old (render::release-world-text-batch old))))
        (and (presences-batch state) (list (presences-batch state)))))))
