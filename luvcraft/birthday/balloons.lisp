;;; Balloons: bright SDF spheres-with-knots bobbing on their strings.
;;;
;;; Contract:
;;;   (ADD-BIRTHDAY-BALLOONS session balloons) registers one scene overlay
;;;   drawing every balloon as an instanced, camera-facing, sphere-traced
;;;   billboard in the manner of the gnome's body overlay.  BALLOONS is a
;;;   list of plists (:x :y :z :radius :hue :phase); the overlay owns its
;;;   instance buffer and pipelines.  Balloons bob and sway gently on the
;;;   frame clock with per-instance phase, and each wears its own bright
;;;   glossy colour.  (REMOVE-BIRTHDAY-BALLOONS session) releases the overlay.
;;;
;;;   Shader sections live under (in-package #:luvcraft.shaders).
;;;
;;; A balloon is much simpler than a gnome and the shader below leans on
;;; that: it has no face to keep toward the camera -- a balloon is round
;;; about its string, so the field is marched straight in world axes -- and
;;; the whole dance is done in the vertex stage.  Each frame the vertex
;;; shader reads the frame clock out of the fog vector, bobs and sways the
;;; balloon's centre by its instance phase, and hands the *animated* centre
;;; to the fragment stage; the CPU writes the instance buffer once, when the
;;; balloons are tied on, and never again.  The billboard is centred on the
;;; animated bounding sphere, so nothing can slide out of its own proxy and
;;; be sliced flat at the edge -- the bound follows the balloon around
;;; rather than trying to be wide enough for everywhere it might go.

(in-package #:luvcraft.shaders)

;;; ---------------------------------------------------------------------
;;; The balloon's proportions, as knobs
;;;
;;; As with the gnome, each number stands in the shader source by name and
;;; folds to a literal when the source is parsed, so turning one rebuilds
;;; the pipeline.  The shape works in balloon units -- one unit is the
;;; balloon's radius at its widest -- and only the two amplitudes speak in
;;; world cells, because a big balloon and a small one hang from the same
;;; kind of breeze.

(defparameter *balloon-elongation* 1.16
  "How much taller than wide a balloon stands, in balloon radii.
One would be a ball, which is a different toy.")
(defparameter *balloon-string-length* 2.2
  "How far the string hangs below the knot, in balloon radii.")
(defparameter *balloon-bob-amplitude* 0.15
  "How high a balloon rides its bob, in world cells.")
(defparameter *balloon-sway-amplitude* 0.08
  "How far a balloon drifts sideways, in world cells.")
(defparameter *balloon-gloss* 0.9
  "The strength of the latex highlight.  Zero is a matte balloon,
which is a balloon that has given up.")
(defparameter *balloon-rim-light* 0.18
  "The pale outline that lifts a balloon off whatever stands behind it.")
(defparameter *balloon-translucency* 0.30
  "How much sunlight leaks through the latex when the sun is behind it.")

(macrolet
    ((define-balloon-knob (name place quantity minimum maximum step
                           &optional documentation)
       `(luvcraft:define-knob ,name
            (:group :balloon :quantity (:quantity ,quantity
                                        :unit ,(if (eq quantity
                                                       :figure-extent)
                                                   :cell
                                                   :one))
             :documentation ,documentation
             :minimum ,minimum :maximum ,maximum :step ,step)
          ,place)))
  (define-balloon-knob balloon-elongation *balloon-elongation*
    :figure-proportion 1.0 1.6 0.01)
  (define-balloon-knob balloon-string-length *balloon-string-length*
    :figure-proportion 0.2 4.0 0.1)
  (define-balloon-knob balloon-bob-amplitude *balloon-bob-amplitude*
    :figure-extent 0.0 0.5 0.01)
  (define-balloon-knob balloon-sway-amplitude *balloon-sway-amplitude*
    :figure-extent 0.0 0.4 0.01)
  (define-balloon-knob balloon-gloss *balloon-gloss*
    :figure-proportion 0.0 2.0 0.05)
  (define-balloon-knob balloon-rim-light *balloon-rim-light*
    :figure-proportion 0.0 1.0 0.02)
  (define-balloon-knob balloon-translucency *balloon-translucency*
    :figure-proportion 0.0 1.0 0.02))

;;; ---------------------------------------------------------------------
;;; The shape
;;;
;;; Three primitives in balloon units: the latex is an upright ellipsoid
;;; eased toward the knot by a small sphere so the bottom tapers the way a
;;; filled balloon actually hangs, the knot is a nub poking out under the
;;; taper, and the string is a thin capsule dropping from the knot.  The
;;; latex parts weld with the gnome's smooth minimum; the string joins by
;;; plain MIN, because a string is tied on, not grown.

(define-shader-function balloon-body-distance (point)
  "The latex: an ellipsoid eased into a teardrop, with a tied-off knot.

The taper sphere sits low inside the ellipsoid and the generous fillet
between them is what turns two primitives into one skin; the knot keeps a
small fillet so it still reads as a knot."
  (let* ((body (gnome-ellipsoid-distance
                point (vec3 0.0 0.0 0.0)
                (vec3 1.0 balloon-elongation 1.0)))
         (taper (gnome-sphere-distance point (vec3 0.0 -1.05 0.0) 0.30))
         (knot (gnome-sphere-distance point (vec3 0.0 -1.28 0.0) 0.14)))
    (gnome-smooth-union (gnome-smooth-union body taper 0.25) knot 0.06)))

(define-shader-function balloon-string-distance (point)
  "The string: a thin vertical capsule hanging from the knot."
  (let* ((drop (clamp (/ (- -1.30 (swizzle point :y))
                         balloon-string-length)
                      0.0 1.0))
         (nearest-y (- -1.30 (* drop balloon-string-length))))
    (- (gnome-length (vec3 (swizzle point :x)
                           (- (swizzle point :y) nearest-y)
                           (swizzle point :z)))
       0.035)))

(define-shader-function balloon-distance (point)
  "The whole balloon: latex welded, string tied on."
  (min (balloon-body-distance point) (balloon-string-distance point)))

(define-shader-function balloon-normal (point)
  "The gradient by the tetrahedron trick, exactly as the gnome takes it."
  (let* ((reach 0.002)
         (a (vec3 1.0 -1.0 -1.0))
         (b (vec3 -1.0 -1.0 1.0))
         (c (vec3 -1.0 1.0 -1.0))
         (d (vec3 1.0 1.0 1.0)))
    (normalize
     (+ (+ (* a (balloon-distance (+ point (* a reach))))
           (* b (balloon-distance (+ point (* b reach)))))
        (+ (* c (balloon-distance (+ point (* c reach))))
           (* d (balloon-distance (+ point (* d reach)))))))))

(define-shader-function balloon-albedo (hue)
  "Six party colours on a ring; HUE picks one or blends between neighbours.

Saturated primaries on purpose -- the very thing the gnome's palette
refuses.  A balloon is a toy, and should read as one from across the
meadow: red, orange, sunny yellow, green, sky blue, pink, and around
again to red."
  (let* ((turn (* (fract hue) 6.0))
         (red (vec3 0.85 0.07 0.09))
         (color-1 (mix red (vec3 0.95 0.38 0.05)
                       (clamp turn 0.0 1.0)))
         (color-2 (mix color-1 (vec3 1.0 0.78 0.06)
                       (clamp (- turn 1.0) 0.0 1.0)))
         (color-3 (mix color-2 (vec3 0.10 0.62 0.15)
                       (clamp (- turn 2.0) 0.0 1.0)))
         (color-4 (mix color-3 (vec3 0.15 0.45 0.95)
                       (clamp (- turn 3.0) 0.0 1.0)))
         (color-5 (mix color-4 (vec3 0.93 0.30 0.55)
                       (clamp (- turn 4.0) 0.0 1.0))))
    (mix color-5 red (clamp (- turn 5.0) 0.0 1.0))))

;;; ---------------------------------------------------------------------
;;; The pipeline stages
;;;
;;; The shape spans balloon-unit y from the string's tail up to the crown,
;;; so its bounding sphere is centred well below the balloon's own centre.
;;; Both stages state the bound the same way from the same knobs -- the
;;; vertex to size the proxy, the fragment to enter and leave the march
;;; analytically -- so they cannot drift apart.  The margin of three tenths
;;; covers the string's thickness and anything a smooth union swells.

(define-shader-method shader-specification-for
    balloon-sdf-vertex-specification
    ((role (eql :balloon-sdf)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((quad-corner :vec3 :location 0)
              (balloon-center-radius :vec4 :location 1)
              (balloon-tint-phase :vec4 :location 2))
     :outputs ((clip-position :vec4 :built-in :position)
               (proxy-world-position :vec3 :location 0)
               (balloon-output :vec4 :location 1)
               (tint-output :vec4 :location 2))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((center (swizzle balloon-center-radius :xyz))
         (radius (swizzle balloon-center-radius :w))
         (phase (swizzle balloon-tint-phase :y))
         ;; The dance, entirely here: a bob and two sways at incommensurate
         ;; rates, each offset by the instance phase, so a row of balloons
         ;; never marches in step.  The fragment receives the moved centre
         ;; and needs no clock of its own.
         (elapsed (representation (swizzle fog-vector :z)))
         (turn (* phase 6.28318))
         (bob (* balloon-bob-amplitude (sin (+ (* elapsed 2.1) turn))))
         (sway-x (* balloon-sway-amplitude (cos (+ (* elapsed 1.3) turn))))
         (sway-z (* balloon-sway-amplitude
                    (cos (+ (* elapsed 1.7) (+ turn 2.0)))))
         (bobbed (+ center (vec3 sway-x bob sway-z)))
         (camera (representation (swizzle camera-vector :xyz)))
         (right (representation (swizzle right-vector :xyz)))
         (up (representation (swizzle up-vector :xyz)))
         (forward (representation (swizzle forward-vector :xyz)))
         ;; The bound, in balloon units, then scaled by this instance.
         (bound-bottom (- -1.35 balloon-string-length))
         (bound-center-y (* 0.5 (+ balloon-elongation bound-bottom)))
         (bound-radius (+ (* 0.5 (- balloon-elongation bound-bottom)) 0.3))
         (extent (* radius (* bound-radius 1.02)))
         (billboard-center
           (+ bobbed (vec3 0.0 (* radius bound-center-y) 0.0)))
         (corner-x (- (* (swizzle quad-corner :x) 2.0) 1.0))
         (corner-y (- (* (swizzle quad-corner :y) 2.0) 1.0))
         (world-position
           (+ billboard-center (+ (* right (* corner-x extent))
                                  (* up (* corner-y extent)))))
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
    (set-output balloon-output (vec4 bobbed radius))
    (set-output tint-output balloon-tint-phase)))

(define-shader-method shader-specification-for
    balloon-sdf-fragment-specification
    ((role (eql :balloon-sdf)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((proxy-world-position :vec3 :location 0)
              (balloon-input :vec4 :location 1)
              (tint-input :vec4 :location 2))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((camera (representation (swizzle camera-vector :xyz)))
         (center (swizzle balloon-input :xyz))
         (radius (swizzle balloon-input :w))
         (hue (swizzle tint-input :x))
         (ray (normalize (- proxy-world-position camera)))
         ;; Balloon units: world over the instance radius.  No facing frame
         ;; -- a balloon is round about its string -- so the ray marches
         ;; unrotated and only the camera moves into the balloon's frame.
         (local-camera (/ (- camera center) radius))
         ;; The same bound the vertex stage stated.
         (bound-bottom (- -1.35 balloon-string-length))
         (bound-center
           (vec3 0.0 (* 0.5 (+ balloon-elongation bound-bottom)) 0.0))
         (bound-radius (+ (* 0.5 (- balloon-elongation bound-bottom)) 0.3))
         (to-center (- local-camera bound-center))
         (half-way (- (dot to-center ray)))
         (gap (- (dot to-center to-center) (* bound-radius bound-radius)))
         (discriminant (- (* half-way half-way) gap))
         (span (sqrt (max discriminant 0.0)))
         (entry (max (- half-way span) 0.0))
         (exit (+ half-way span))
         ;; Half the gnome's steps: three round primitives converge fast,
         ;; and the ellipsoid's bounded estimate already understates, so
         ;; the step scale stays close to one.
         (travel
           (counted-fold (march 32.0 ray-distance entry)
             (let* ((point (+ local-camera (* ray ray-distance)))
                    (distance (balloon-distance point)))
               (if (< distance 0.0012)
                   ray-distance
                   (if (> ray-distance exit)
                       ray-distance
                       (+ ray-distance (max (* distance 0.95) 0.0012)))))))
         (point (+ local-camera (* ray travel)))
         (surface-distance (balloon-distance point))
         ;; A feathered edge rather than the gnome's hard step: latex
         ;; against sky benefits from half a pixel of mercy.
         (coverage (* (- 1.0 (smoothstep 0.004 0.02 surface-distance))
                      (- 1.0 (step 0.0 (- discriminant)))))
         ;; Which material owns the hit, by the nearer field.
         (body (balloon-body-distance point))
         (string (balloon-string-distance point))
         (string-weight (step string body))
         (latex (balloon-albedo hue))
         (albedo (mix latex (vec3 0.92 0.92 0.88) string-weight))
         ;; The string dissolves over its lower half, so the balloon reads
         ;; as tied to the air rather than planted in the ground.
         (along (clamp (/ (- -1.30 (swizzle point :y))
                          balloon-string-length)
                       0.0 1.0))
         (string-fade (- 1.0 (smoothstep 0.55 1.0 along)))
         (faded-coverage (* coverage (mix 1.0 string-fade string-weight)))
         (normal (balloon-normal point))
         (sun-direction (representation (swizzle sun-vector :xyz)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (ambient (representation (swizzle ambient-vector :xyz)))
         (lambert (dot normal sun-direction))
         (wrapped (max 0.0 (/ (+ lambert 0.4) 1.4)))
         (sky (+ 0.55 (* 0.45 (swizzle normal :y))))
         (view-facing (max 0.0 (dot normal (* ray -1.0))))
         (rim (expt (- 1.0 view-facing) 3.0))
         (halfway (normalize (- sun-direction ray)))
         ;; Latex is two highlights: the tight glint that says glossy and a
         ;; broad sheen that says rubber rather than glass.  Neither
         ;; belongs on the string.
         (facing-half (max 0.0 (dot normal halfway)))
         (glint (* balloon-gloss (expt facing-half 90.0)))
         (sheen (* 0.10 (expt facing-half 8.0)))
         ;; Sunlight through the far side: the lift that makes a back-lit
         ;; balloon glow its own colour instead of going dark.
         (glow (* balloon-translucency (max 0.0 (- lambert))))
         (illumination
           (+ (* ambient sky)
              (* sun-color (+ 0.15 (* wrapped 1.1)))))
         (radiance
           (+ (+ (* albedo illumination)
                 (* sun-color (* (+ glint sheen)
                                 (- 1.0 string-weight))))
              (+ (* (* albedo sun-color) glow)
                 (* (vec3 1.0 0.98 0.92)
                    (* rim (* balloon-rim-light
                              (- 1.0 string-weight))))))))
    ;; Premultiplied alpha, as the scene target expects: a miss leaves no
    ;; rectangular trace of the proxy.
    (set-output color-output (vec4 (* radiance faded-coverage)
                                   faded-coverage))))

(in-package #:luvcraft.birthday)

;;; ---------------------------------------------------------------------
;;; The overlay: one pipeline, one quad, one instanced draw.
;;;
;;; Structured after the gnome's body overlay, minus its every-frame
;;; heartbeat: the instance buffer is written once when the balloons are
;;; tied on, and the vertex shader animates from the frame clock, so
;;; ENCODE only binds and draws.

(defclass birthday-balloon-overlay ()
  ((pipeline :initarg :pipeline :accessor balloon-overlay-pipeline)
   (vertex-buffer :initarg :vertex-buffer
                  :accessor balloon-overlay-vertex-buffer)
   (instance-buffer :initarg :instance-buffer
                    :accessor balloon-overlay-instance-buffer)
   (instance-count :initarg :instance-count
                   :reader balloon-overlay-instance-count)))

(defmethod luvcraft::luvcraft-overlay-live-shader-pipelines
    ((overlay birthday-balloon-overlay))
  (list (balloon-overlay-pipeline overlay)))

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay birthday-balloon-overlay) session pass surface-texture)
  (let ((frame (luvcraft::luvcraft-frame-state session surface-texture)))
    (luv:set-pipeline
     pass
     (luvcraft::live-shader-pipeline-native-pipeline
      (balloon-overlay-pipeline overlay)))
    (luv:set-vertex-buffer pass 0 (balloon-overlay-vertex-buffer overlay))
    (luv:set-vertex-buffer pass 1 (balloon-overlay-instance-buffer overlay))
    (luv:set-bind-group pass 0 (luvcraft::luvcraft-frame-scene-bind-group
                                frame))
    (luv:draw pass 6 (balloon-overlay-instance-count overlay)))
  overlay)

(defmethod luvcraft:release-luvcraft-overlay
    ((overlay birthday-balloon-overlay))
  (when (balloon-overlay-pipeline overlay)
    (luvcraft::release-live-shader-pipeline
     (balloon-overlay-pipeline overlay))
    (setf (balloon-overlay-pipeline overlay) nil))
  (dolist (resource (list (balloon-overlay-instance-buffer overlay)
                          (balloon-overlay-vertex-buffer overlay)))
    (when resource (luv:destroy resource)))
  (setf (balloon-overlay-instance-buffer overlay) nil
        (balloon-overlay-vertex-buffer overlay) nil)
  (values))

(defun balloon-instance-data (balloons)
  "Pack BALLOONS into the two-vec4 instance records the shader reads.

Each record is (centre.xyz, radius) then (hue, phase, spare, spare).
An unstated hue walks the palette ring by the golden ratio and an unstated
phase spreads similarly, so a bag of plain (:x :y :z) plists still comes
out as a mixed bunch that never bobs in unison."
  (let ((data (make-array (* 8 (length balloons))
                          :element-type 'single-float
                          :initial-element 0.0)))
    (loop for balloon in balloons
          for count from 0
          for base from 0 by 8
          do (flet ((put (offset value)
                      (setf (aref data (+ base offset))
                            (coerce value 'single-float))))
               (put 0 (getf balloon :x 0.0))
               (put 1 (getf balloon :y 0.0))
               (put 2 (getf balloon :z 0.0))
               (put 3 (getf balloon :radius 0.35))
               (put 4 (or (getf balloon :hue)
                          (mod (* count 0.618034) 1.0)))
               (put 5 (or (getf balloon :phase)
                          (mod (* count 0.377) 1.0)))))
    data))

(defun add-birthday-balloons (session balloons)
  "Tie BALLOONS -- plists of (:x :y :z :radius :hue :phase) -- into SESSION.

One overlay, one instanced draw.  Any balloons already tied on are taken
down first, so calling this twice redecorates rather than crowds.  Returns
the overlay, or NIL when BALLOONS is empty."
  (remove-birthday-balloons session)
  (when balloons
    (let* ((device (luvcraft:luvcraft-session-device session))
           (vertex-data (luvcraft::make-world-text-quad-vertices))
           (instance-data (balloon-instance-data balloons))
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
                     :label "balloon SDF proxy"
                     :size (* 4 (length vertex-data))
                     :usage '(:vertex :copy-dst)))
                   instance-buffer
                   (luv:create
                    device
                    (luv:make-buffer-descriptor
                     :label "balloon SDF instances"
                     :size (* 4 (length instance-data))
                     :usage '(:vertex :copy-dst)))
                   pipeline
                   (luvcraft::make-live-shader-pipeline
                    :role :balloon-sdf
                    :vertex-role :balloon-sdf
                    :label "balloon sphere SDF pipeline"
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
                             'birthday-balloon-overlay
                             :pipeline pipeline
                             :vertex-buffer vertex-buffer
                             :instance-buffer instance-buffer
                             :instance-count (length balloons))))
               (setf completed-p t)
               (luvcraft:add-luvcraft-overlay session overlay)))
        (unless completed-p
          (when pipeline
            (ignore-errors
              (luvcraft::release-live-shader-pipeline pipeline)))
          (when instance-buffer (ignore-errors (luv:destroy instance-buffer)))
          (when vertex-buffer (ignore-errors (luv:destroy vertex-buffer))))))))

(defun remove-birthday-balloons (session)
  "Take down every balloon overlay tied to SESSION, releasing its GPU state."
  (dolist (overlay (remove-if-not
                    (lambda (overlay)
                      (typep overlay 'birthday-balloon-overlay))
                    (luvcraft:luvcraft-session-overlays session)))
    (luvcraft:remove-luvcraft-overlay session overlay))
  (values))
