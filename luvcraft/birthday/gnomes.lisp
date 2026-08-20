;;; Dancing gnomes: the agent's SDF body, multiplied and set to music.
;;;
;;; Contract:
;;;   (ADD-DANCING-GNOMES session gnomes) registers one scene overlay drawing
;;;   every gnome as an instanced SDF billboard, reusing the gnome part
;;;   functions from luvcraft/agent/shaders.lisp (GNOME-DISTANCE and friends
;;;   live in #:luvcraft.shaders; define any dance variants here rather than
;;;   editing that file).  GNOMES is a list of plists
;;;   (:x :y :z :scale :phase :hue); each gnome dances -- bouncing, swaying,
;;;   slowly turning -- driven by the frame clock offset by its phase, and
;;;   its hue shifts the robe and hat so the troupe reads as individuals.
;;;   (REMOVE-DANCING-GNOMES session) releases the overlay.
;;;
;;; The CPU's whole contribution is one instance buffer written once: feet,
;;; radius, scale, phase and hue per gnome.  Everything that moves, moves in
;;; the fragment shader, on the bounded frame clock the sky's clouds already
;;; drift by.  The hop and the sway are rigid motions folded into the ray
;;; before the march starts; only the squash-and-stretch touches the field
;;; itself, and it pays for that with a shorter, humbler step.

;;; ---------------------------------------------------------------------
;;; The dance, as knobs
;;;
;;; Like the gnome's proportions, each number below stands in the shader
;;; source by name and folds to a literal, so tuning a step rebuilds the
;;; troupe's pipeline rather than writing a uniform.  The amplitudes are
;;; small on purpose: a four-year-old should giggle, and a gnome bouncing
;;; his own height stops being a gnome and starts being a hazard.

(in-package #:luvcraft.shaders)

(defparameter *dancing-gnome-tempo* 1.4
  "Hops per second, before each gnome's phase spreads the troupe out.

The frame clock wraps at 3600 seconds; at 1.4 that is a whole number of
hops, so the hour turns over without a stumble.")
(defparameter *dancing-gnome-hop* 0.20
  "How high the hop lifts his feet, in figure units.")
(defparameter *dancing-gnome-squash* 0.16
  "How much the hop squashes and stretches him: tall near the top of the
hop, flat at the landing, cross-section compensating either way so he
keeps his volume.")
(defparameter *dancing-gnome-sway* 0.11
  "The side-to-side lean, in radians, about his own forward axis.  It runs
at half the hop tempo, so he leans the other way on every second landing.")
(defparameter *dancing-gnome-turn* 0.06
  "Pirouettes per second: how fast the whole figure yaws.  Slow on purpose;
the turn is for showing the hat from every side, not for spinning.")

(macrolet
    ((define-dance-knob (name place quantity minimum maximum step
                         &optional documentation)
       `(luvcraft:define-knob ,name
            (:group :birthday :quantity (:quantity ,quantity
                                         :unit ,(if (eq quantity
                                                        :figure-proportion)
                                                    :one
                                                    :cell))
             :documentation ,documentation
             :minimum ,minimum :maximum ,maximum :step ,step)
          ,place)))
  (define-dance-knob dancing-gnome-tempo *dancing-gnome-tempo*
    :figure-proportion 0.2 4.0 0.05)
  (define-dance-knob dancing-gnome-hop *dancing-gnome-hop*
    :figure-extent 0.0 0.6 0.01)
  (define-dance-knob dancing-gnome-squash *dancing-gnome-squash*
    :figure-proportion 0.0 0.5 0.01)
  (define-dance-knob dancing-gnome-sway *dancing-gnome-sway*
    :figure-proportion 0.0 0.4 0.01)
  (define-dance-knob dancing-gnome-turn *dancing-gnome-turn*
    :figure-proportion 0.0 0.5 0.01))

;;; ---------------------------------------------------------------------
;;; Dance variants of the gnome's field
;;;
;;; The originals answer for one upright fellow and will not bend, so the
;;; two functions below wrap GNOME-DISTANCE and its tetrahedron normal in
;;; the one non-isometric part of the warp.  WIDE and TALL are the squash's
;;; axis factors; LIMBER, the smaller of them, scales the answer back to an
;;; honest lower bound so the march never oversteps the compressed axis.

(define-shader-function dancing-gnome-distance (point wide tall limber)
  "GNOME-DISTANCE through the squash-and-stretch.

The sample point is divided back into the unsquashed body; the answer is a
distance in the warped space only after LIMBER shrinks it."
  (* (gnome-distance (vec3 (/ (swizzle point :x) wide)
                           (/ (swizzle point :y) tall)
                           (/ (swizzle point :z) wide)))
     limber))

(define-shader-function dancing-gnome-warped-normal (point wide tall limber)
  "The tetrahedron normal over the squashed field.

Differentiating the warped field directly keeps the normal consistent with
the surface the march actually found: the constant LIMBER factor can only
shrink the gradient, never turn it, and NORMALIZE forgets it."
  (let* ((reach 0.0016)
         (a (vec3 1.0 -1.0 -1.0))
         (b (vec3 -1.0 -1.0 1.0))
         (c (vec3 -1.0 1.0 -1.0))
         (d (vec3 1.0 1.0 1.0)))
    (normalize
     (+ (+ (* a (dancing-gnome-distance (+ point (* a reach))
                                        wide tall limber))
           (* b (dancing-gnome-distance (+ point (* b reach))
                                        wide tall limber)))
        (+ (* c (dancing-gnome-distance (+ point (* c reach))
                                        wide tall limber))
           (* d (dancing-gnome-distance (+ point (* d reach))
                                        wide tall limber)))))))

(define-shader-function dancing-gnome-party-color (hue)
  "A festive colour on the hue wheel, HUE in turns.

Three phases of one cosine: as cheap as a rainbow gets, and it never
leaves [0,1]."
  (let* ((angle (* 6.2831853 hue)))
    (vec3 (+ 0.5 (* 0.5 (cos angle)))
          (+ 0.5 (* 0.5 (cos (- angle 2.0943951))))
          (+ 0.5 (* 0.5 (cos (- angle 4.1887902)))))))

(define-shader-function dancing-gnome-albedo (point hue)
  "GNOME-ALBEDO with the robe and hat dressed for a party.

The same five parts answer for the same materials -- shape and colour still
cannot drift apart -- but the hat wears HUE and the robe its
near-complement, toned back toward the garden palette so the troupe is
festive rather than fluorescent.  Beard, skin and boots keep their own
colours: it is a party, not a costume change."
  (let* ((robe (gnome-robe-distance point))
         (skin (gnome-skin-distance point))
         (beard (gnome-beard-distance point))
         (hat (gnome-hat-distance point))
         (boots (gnome-boot-distance point))
         (strand (* (- (lattice-noise (* point (vec3 34.0 7.0 34.0))) 0.5)
                    gnome-grain))
         (weave (* (- (lattice-noise (* point (vec3 15.0 15.0 15.0))) 0.5)
                   gnome-grain))
         (felt (* (- (lattice-noise (* point (vec3 24.0 24.0 24.0))) 0.5)
                  gnome-grain))
         (hat-party (dancing-gnome-party-color hue))
         (robe-party (dancing-gnome-party-color (+ hue 0.45)))
         (robe-color (* (+ (* robe-party 0.22) (vec3 0.03 0.035 0.05))
                        (+ 1.0 (* weave 0.30))))
         (skin-color (vec3 0.72 0.47 0.35))
         (beard-color (* (vec3 0.80 0.79 0.755) (+ 1.0 (* strand 0.22))))
         (hat-color (* (+ (* hat-party 0.44) (vec3 0.05 0.015 0.015))
                       (+ 1.0 (* felt 0.20))))
         (boot-color (* (vec3 0.105 0.075 0.055) (+ 1.0 (* weave 0.35))))
         ;; The running nearest-so-far, folded by hand as in GNOME-ALBEDO.
         (near-skin (step skin robe))
         (color-1 (mix robe-color skin-color near-skin))
         (distance-1 (min robe skin))
         (near-beard (step beard distance-1))
         (color-2 (mix color-1 beard-color near-beard))
         (distance-2 (min distance-1 beard))
         (near-hat (step hat distance-2))
         (color-3 (mix color-2 hat-color near-hat))
         (distance-3 (min distance-2 hat))
         (near-boots (step boots distance-3))
         (color-4 (mix color-3 boot-color near-boots))
         (skin-weight
           (- 1.0 (smoothstep 0.0 0.035 (- skin (min distance-3 boots))))))
    (gnome-face-detail point color-4 skin-weight)))

;;; ---------------------------------------------------------------------
;;; The pipeline stages
;;;
;;; The vertex stage is the gnome's billboard with one more lane riding
;;; through: (scale, phase, hue, spare) per instance, beside the bounding
;;; sphere the proxy is sized by.

(define-shader-method shader-specification-for
    dancing-gnome-sdf-vertex-specification
    ((role (eql :dancing-gnome-sdf)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((quad-corner :vec3 :location 0)
              (sphere-center-radius :vec4 :location 1)
              (dance-vector :vec4 :location 2))
     :outputs ((clip-position :vec4 :built-in :position)
               (proxy-world-position :vec3 :location 0)
               (sphere-output :vec4 :location 1)
               (dance-output :vec4 :location 2))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((center (swizzle sphere-center-radius :xyz))
         (radius (swizzle sphere-center-radius :w))
         (camera (representation (swizzle camera-vector :xyz)))
         (right (representation (swizzle right-vector :xyz)))
         (up (representation (swizzle up-vector :xyz)))
         (forward (representation (swizzle forward-vector :xyz)))
         (corner-x (- (* (swizzle quad-corner :x) 2.0) 1.0))
         (corner-y (- (* (swizzle quad-corner :y) 2.0) 1.0))
         (world-position
           (+ center (+ (* right (* corner-x radius))
                        (* up (* corner-y radius)))))
         (relative (- world-position camera))
         (view-x (dot relative right))
         (view-y (dot relative up))
         (view-z (dot relative forward))
         (x-scale (representation (swizzle projection-vector :x)))
         (y-scale (representation (swizzle projection-vector :y)))
         (z-scale (representation (swizzle projection-vector :z)))
         (z-offset (representation (swizzle projection-vector :w))))
    (set-output clip-position
                (vec4 (* view-x x-scale)
                      (- (* view-y y-scale))
                      (+ (* view-z z-scale) z-offset)
                      view-z))
    (set-output proxy-world-position world-position)
    (set-output sphere-output sphere-center-radius)
    (set-output dance-output dance-vector)))

(define-shader-method shader-specification-for
    dancing-gnome-sdf-fragment-specification
    ((role (eql :dancing-gnome-sdf)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((proxy-world-position :vec3 :location 0)
              (sphere-input :vec4 :location 1)
              (dance-input :vec4 :location 2))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((camera (representation (swizzle camera-vector :xyz)))
         (center (swizzle sphere-input :xyz))
         (radius (swizzle sphere-input :w))
         (figure-scale (max (swizzle dance-input :x) 0.001))
         (phase (swizzle dance-input :y))
         (hue (swizzle dance-input :z))
         (elapsed (representation (swizzle fog-vector :z)))
         ;; The beat: the frame clock at the troupe's tempo, this gnome's
         ;; phase ahead, so the troupe ripples instead of drilling.
         (beat (* 6.2831853 (+ (* dancing-gnome-tempo elapsed) phase)))
         (bounce (abs (sin beat)))
         (hop (* dancing-gnome-hop bounce))
         ;; Squash and stretch, volume held roughly constant: stretched near
         ;; the top of the hop, squashed at the landing.
         (tall (+ 1.0 (* dancing-gnome-squash (- bounce 0.35))))
         (wide (/ 1.0 (sqrt tall)))
         (limber (min tall wide))
         (sway (* dancing-gnome-sway (sin (* beat 0.5))))
         (sway-cos (cos sway))
         (sway-sin (sin sway))
         ;; The pirouette.  The stock gnome always faces the camera; this
         ;; one faces the camera only on average, yawing slowly through the
         ;; whole circle with his phase deciding where he started.
         (yaw (* 6.2831853 (+ (* dancing-gnome-turn elapsed) phase)))
         (yaw-cos (cos yaw))
         (yaw-sin (sin yaw))
         (ray (normalize (- proxy-world-position camera)))
         (toward-eye (- camera center))
         (facing-camera (normalize (vec3 (swizzle toward-eye :x)
                                         0.0
                                         (swizzle toward-eye :z))))
         (sideways-camera (vec3 (swizzle facing-camera :z)
                                0.0
                                (- (swizzle facing-camera :x))))
         ;; The figure frame, yawed in the ground plane.  Both basis vectors
         ;; are horizontal and unit, so the turned pair stays orthonormal.
         (facing (+ (* facing-camera yaw-cos) (* sideways-camera yaw-sin)))
         (sideways (- (* sideways-camera yaw-cos) (* facing-camera yaw-sin)))
         ;; Per-instance stature: the knob times this gnome's own scale,
         ;; entering once, exactly as in the stock fragment.  He stands with
         ;; his feet at figure y zero; the instance centre is the middle of
         ;; his bounding sphere, 0.85 above them (*GNOME-BODY-CENTRE*).
         (stature (* gnome-stature figure-scale))
         (local-camera (vec3 (/ (dot toward-eye sideways) stature)
                             (+ (/ (swizzle toward-eye :y) stature) 0.85)
                             (/ (dot toward-eye facing) stature)))
         (local-ray (vec3 (dot ray sideways)
                          (swizzle ray :y)
                          (dot ray facing)))
         ;; Bounding-sphere entry and exit, analytically as ever.  The CPU
         ;; sized the sphere to hold the whole dance -- hop, lean, stretch --
         ;; so the march can trust it.
         (figure-radius (/ radius stature))
         (to-center (- local-camera (vec3 0.0 0.85 0.0)))
         (half-way (- (dot to-center local-ray)))
         (gap (- (dot to-center to-center) (* figure-radius figure-radius)))
         (discriminant (- (* half-way half-way) gap))
         (span (sqrt (max discriminant 0.0)))
         (entry (max (- half-way span) 0.0))
         (exit (+ half-way span))
         ;; The hop and the sway are rigid motions, so their inverses fold
         ;; into the camera and the ray once -- un-lift, then un-lean about
         ;; the feet -- and the march runs in the body's own swaying space
         ;; at no cost per step.
         (dropped-y (- (swizzle local-camera :y) hop))
         (sway-camera (vec3 (+ (* (swizzle local-camera :x) sway-cos)
                               (* dropped-y sway-sin))
                            (- (* dropped-y sway-cos)
                               (* (swizzle local-camera :x) sway-sin))
                            (swizzle local-camera :z)))
         (sway-ray (vec3 (+ (* (swizzle local-ray :x) sway-cos)
                            (* (swizzle local-ray :y) sway-sin))
                         (- (* (swizzle local-ray :y) sway-cos)
                            (* (swizzle local-ray :x) sway-sin))
                         (swizzle local-ray :z)))
         (travel
           (counted-fold (march 64.0 ray-distance entry)
             (let* ((point (+ sway-camera (* sway-ray ray-distance)))
                    (distance (dancing-gnome-distance point
                                                      wide tall limber)))
               (if (< distance 0.0009)
                   ray-distance
                   (if (> ray-distance exit)
                       ray-distance
                       ;; The hat's domain warp plus the squash make this
                       ;; field a little less metric than the stock gnome's;
                       ;; the step keeps a wider margin of honesty.
                       (+ ray-distance (max (* distance 0.80) 0.0009)))))))
         (point (+ sway-camera (* sway-ray travel)))
         (surface-distance (dancing-gnome-distance point wide tall limber))
         (coverage (* (- 1.0 (step 0.0035 surface-distance))
                      (- 1.0 (step 0.0 (- discriminant)))))
         ;; The unsquashed body point, where the parts and the face live.
         (body-point (vec3 (/ (swizzle point :x) wide)
                           (/ (swizzle point :y) tall)
                           (/ (swizzle point :z) wide)))
         ;; The normal, back out through the warp: differentiated in swaying
         ;; space, rotated forward through the lean, then through the yawed
         ;; basis into the world.
         (sway-normal (dancing-gnome-warped-normal point wide tall limber))
         (lean-normal (vec3 (- (* (swizzle sway-normal :x) sway-cos)
                               (* (swizzle sway-normal :y) sway-sin))
                            (+ (* (swizzle sway-normal :x) sway-sin)
                               (* (swizzle sway-normal :y) sway-cos))
                            (swizzle sway-normal :z)))
         (normal (+ (* sideways (swizzle lean-normal :x))
                    (+ (vec3 0.0 (swizzle lean-normal :y) 0.0)
                       (* facing (swizzle lean-normal :z)))))
         (albedo (dancing-gnome-albedo body-point hue))
         ;; The lighting is the stock gnome's, verbatim: he is the same
         ;; fellow, just busier.
         (sun-direction (representation (swizzle sun-vector :xyz)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (ambient (representation (swizzle ambient-vector :xyz)))
         (lambert (dot normal sun-direction))
         (wrapped (max 0.0 (/ (+ lambert 0.35) 1.35)))
         (sky (+ 0.55 (* 0.45 (swizzle normal :y))))
         (occlusion (clamp (+ 0.35 (* 0.65 (smoothstep
                                            -0.15 0.35
                                            (swizzle body-point :y))))
                           0.35 1.0))
         (view-facing (max 0.0 (dot normal (* ray -1.0))))
         (rim (expt (- 1.0 view-facing) 3.5))
         (halfway (normalize (- sun-direction ray)))
         (specular (* 0.12 (expt (max 0.0 (dot normal halfway)) 28.0)))
         (illumination
           (+ (* ambient (* sky occlusion))
              (* sun-color (+ 0.12 (* wrapped 1.15)))))
         (radiance
           (+ (+ (* albedo illumination)
                 (* sun-color (* specular coverage)))
              (* (vec3 0.95 0.72 0.44) (* rim gnome-rim-light)))))
    ;; Premultiplied, like the scene target: misses leave no rectangular
    ;; trace of the conservative billboard.
    (set-output color-output (vec4 (* radiance coverage) coverage))))

;;; ---------------------------------------------------------------------
;;; The troupe overlay: one pipeline, one instance buffer, one draw.

(in-package #:luvcraft.birthday)

(defun dancing-gnome-figure-radius ()
  "The bounding radius, in figure units, of one dancing gnome mid-move.

GNOME-FIGURE-RADIUS answers for the fellow standing still; the dance adds
the hop's lift, the sway's sweep of everything up to the hat's tip, and the
stretch of the squash, each taken at its extreme.  Conservative on purpose:
the proxy edge does not blur what it cuts, it slices it flat."
  (+ (* (luvcraft.agent::gnome-figure-radius)
        (+ 1.0 (float luvcraft.shaders::*dancing-gnome-squash* 1.0)))
     (float luvcraft.shaders::*dancing-gnome-hop* 1.0)
     (* (float luvcraft.shaders::*dancing-gnome-sway* 1.0)
        (float luvcraft.shaders::*gnome-hat-height* 1.0))
     0.05))

(defun dancing-gnome-instance-data (gnomes)
  "Two vec4 lanes per gnome: (centre.xyz, radius) and (scale, phase, hue, 0).

The plist's :X :Y :Z place his feet in world cells; the record's centre is
the middle of his bounding sphere, *GNOME-BODY-CENTRE* figure units above
them at his own stature.  Phase and hue default to golden-ratio spreads so
an unadorned troupe still arrives as individuals rather than as a drill
team in uniform."
  (let ((figure-radius (dancing-gnome-figure-radius))
        (data (make-array (* 8 (length gnomes)) :element-type 'single-float))
        (index 0))
    (dolist (gnome gnomes data)
      (destructuring-bind (&key (x 0.0) (y 0.0) (z 0.0) (scale 1.0)
                                (phase (mod (* 0.618034 index) 1.0))
                                (hue (mod (* 0.618034 (+ index 2)) 1.0))
                           &allow-other-keys)
          gnome
        (let ((stature (* (luvcraft.agent::gnome-stature)
                          (float scale 1.0)))
              (base (* 8 index)))
          (setf (aref data (+ base 0)) (float x 1.0)
                (aref data (+ base 1))
                (float (+ y (* luvcraft.agent::*gnome-body-centre* stature))
                       1.0)
                (aref data (+ base 2)) (float z 1.0)
                (aref data (+ base 3)) (float (* figure-radius stature) 1.0)
                (aref data (+ base 4)) (float scale 1.0)
                (aref data (+ base 5)) (float phase 1.0)
                (aref data (+ base 6)) (float hue 1.0)
                (aref data (+ base 7)) 0.0))
        (incf index)))))

(defclass dancing-gnome-troupe-overlay ()
  ((pipeline :initarg :pipeline :accessor troupe-pipeline)
   (vertex-buffer :initarg :vertex-buffer :accessor troupe-vertex-buffer)
   (instance-buffer :initarg :instance-buffer
                    :accessor troupe-instance-buffer)
   (instance-count :initarg :instance-count :reader troupe-instance-count))
  (:documentation
   "Every dancing gnome in one instanced draw.  The instances were written
once when the troupe arrived; the dance itself lives in the shader."))

(defmethod luvcraft:luvcraft-focus-score
    ((overlay dancing-gnome-troupe-overlay) session)
  (declare (ignore overlay session))
  nil)

(defmethod luvcraft::luvcraft-overlay-live-shader-pipelines
    ((overlay dancing-gnome-troupe-overlay))
  (and (troupe-pipeline overlay)
       (list (troupe-pipeline overlay))))

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay dancing-gnome-troupe-overlay) session pass surface-texture)
  (let ((frame (luvcraft::luvcraft-frame-state session surface-texture)))
    (luv:set-pipeline
     pass
     (luvcraft::live-shader-pipeline-native-pipeline
      (troupe-pipeline overlay)))
    (luv:set-vertex-buffer pass 0 (troupe-vertex-buffer overlay))
    (luv:set-vertex-buffer pass 1 (troupe-instance-buffer overlay))
    (luv:set-bind-group pass 0
                        (luvcraft::luvcraft-frame-scene-bind-group frame))
    (luv:draw pass 6 (troupe-instance-count overlay)))
  overlay)

(defmethod luvcraft:release-luvcraft-overlay
    ((overlay dancing-gnome-troupe-overlay))
  (when (troupe-pipeline overlay)
    (luvcraft::release-live-shader-pipeline (troupe-pipeline overlay))
    (setf (troupe-pipeline overlay) nil))
  (dolist (resource (list (troupe-instance-buffer overlay)
                          (troupe-vertex-buffer overlay)))
    (when resource (luv:destroy resource)))
  (setf (troupe-instance-buffer overlay) nil
        (troupe-vertex-buffer overlay) nil)
  (values))

(defun add-dancing-gnomes (session gnomes)
  "Call in the troupe: draw GNOMES dancing in SESSION as one overlay.

GNOMES is a list of plists (:x :y :z :scale :phase :hue): feet position in
world cells, stature multiplier, dance phase in turns, and hue in turns
around the party colour wheel; scale, phase and hue may be omitted.  Any
troupe already dancing in SESSION is relieved first.  Returns the overlay,
or NIL when GNOMES is empty."
  (remove-dancing-gnomes session)
  (when gnomes
    (let* ((device (luvcraft:luvcraft-session-device session))
           (vertex-data (luvcraft::make-world-text-quad-vertices))
           (instance-data (dancing-gnome-instance-data gnomes))
           (vertex-buffer nil)
           (instance-buffer nil)
           (pipeline nil)
           (completed-p nil))
      (unwind-protect
           (progn
             (setf vertex-buffer
                   (luv:create
                    device
                    (luv:make-buffer-descriptor
                     :label "dancing gnome proxy"
                     :size (* 4 (length vertex-data))
                     :usage '(:vertex :copy-dst)))
                   instance-buffer
                   (luv:create
                    device
                    (luv:make-buffer-descriptor
                     :label "dancing gnome troupe"
                     :size (* 4 (length instance-data))
                     :usage '(:vertex :copy-dst)))
                   pipeline
                   (luvcraft::make-live-shader-pipeline
                    :role :dancing-gnome-sdf
                    :vertex-role :dancing-gnome-sdf
                    :label "dancing gnome troupe pipeline"
                    :device device
                    :layout
                    (luvcraft::live-shader-pipeline-layout
                     (luvcraft:luvcraft-session-block-pipeline session))
                    :vertex-buffers
                    '((:array-stride 12
                       :attributes
                       ((:shader-location 0 :offset 0 :format :float32x3)))
                      (:array-stride 32 :step-mode :instance
                       :attributes
                       ((:shader-location 1 :offset 0 :format :float32x4)
                        (:shader-location 2 :offset 16
                         :format :float32x4))))
                    :target-format luvcraft::+luvcraft-scene-color-format+
                    :target-blend :premultiplied-alpha
                    :primitive '(:topology :triangle-list)
                    :depth-stencil
                    '(:format :depth32-float
                      :depth-write-enabled nil
                      :depth-compare :less)))
             (luv:write-buffer vertex-buffer vertex-data)
             (luv:write-buffer instance-buffer instance-data)
             (let ((overlay (make-instance
                             'dancing-gnome-troupe-overlay
                             :pipeline pipeline
                             :vertex-buffer vertex-buffer
                             :instance-buffer instance-buffer
                             :instance-count (length gnomes))))
               (setf completed-p t)
               (luvcraft:add-luvcraft-overlay session overlay)
               overlay))
        (unless completed-p
          (when pipeline
            (ignore-errors
              (luvcraft::release-live-shader-pipeline pipeline)))
          (when instance-buffer (ignore-errors (luv:destroy instance-buffer)))
          (when vertex-buffer (ignore-errors (luv:destroy vertex-buffer))))))))

(defun remove-dancing-gnomes (session)
  "Send the troupe home: release SESSION's dancing-gnome overlay, if any."
  (let ((overlay (find-if (lambda (overlay)
                            (typep overlay 'dancing-gnome-troupe-overlay))
                          (luvcraft:luvcraft-session-overlays session))))
    (when overlay
      (luvcraft:remove-luvcraft-overlay session overlay))))
