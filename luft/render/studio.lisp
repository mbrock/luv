(in-package #:luft.render)

(defvar *viewer* nil)

(defparameter *inspection-ink-p* t
  "Whether a ray hit gets a blueprint reticle and local triangle-edge lens.")

(defparameter *inspection-reach* 600.0
  "How far, in cells, the atelier's pointer ray may inspect.")

(defparameter *projection* :isometric
  "Either :PERSPECTIVE or :ISOMETRIC.

An isometric picture has no vanishing point, so two chamfers the same width
are the same width on screen wherever they sit.  That is what makes it the
projection to judge a shape rule in.")

(defparameter *isometric-height* 18.0
  "How many world units of height an isometric frame spans.")

(defparameter *character-time* nil
  "Animation clock, in seconds, to hold the traveler at, or NIL to run free.

A character plate needs the same pose every time it is rendered.  The viewer
otherwise derives the clock from its own frame counter, which no capture can
predict.")

(defconstant +orthographic-near+ -4096.0)
(defconstant +orthographic-far+ 4096.0)

(defclass fly-camera ()
  ((position :initarg :position :accessor camera-position)
   (yaw :initarg :yaw :initform 0.0 :accessor camera-yaw)
   (pitch :initarg :pitch :initform 0.0 :accessor camera-pitch)
   (field-of-view :initarg :field-of-view :initform (* 70.0 (/ pi 180))
                  :accessor camera-field-of-view)))

(defclass viewer-mode () ())

(defclass isometric-walk-mode (viewer-mode) ()
  (:documentation
   "Absolute-pointer LUFT play: hover terrain, click a route, scroll zoom."))

(defclass orbit-mode (viewer-mode) ()
  (:documentation
   "The original relative-pointer atelier orbit and direct keyboard mode."))

(defgeneric viewer-mode-inspection-p (mode)
  (:documentation "Whether MODE continuously points into LUFT terrain."))

(defmethod viewer-mode-inspection-p ((mode viewer-mode)) nil)
(defmethod viewer-mode-inspection-p ((mode isometric-walk-mode)) t)

(defgeneric viewer-mode-allows-atelier-keys-p (mode))
(defmethod viewer-mode-allows-atelier-keys-p ((mode viewer-mode)) nil)
(defmethod viewer-mode-allows-atelier-keys-p ((mode isometric-walk-mode)) t)

(defclass site-inspection ()
  ((source :initarg :source :reader site-inspection-source)
   (site :initarg :site :reader site-inspection-site)
   (cell :initarg :cell :reader site-inspection-cell)
   (point :initarg :point :reader site-inspection-point)
   (distance :initarg :distance :reader site-inspection-distance)
   (star-mask :initarg :star-mask :reader site-inspection-star-mask)
   (stock :initarg :stock :reader site-inspection-stock))
  (:documentation
   "One retained semantic ray hit, suitable for sparse inspection.

LUFT sites remain packed integers in dense products.  This object exists only
at the atelier boundary where a person has selected one site."))

(defmethod print-object ((inspection site-inspection) stream)
  (print-unreadable-object (inspection stream :type t)
    (let ((site (site-inspection-site inspection)))
      (format stream "(~D ~D ~D) extent ~3,'0B ~:[-~;+~] at ~,2F"
              (luft:site-x site) (luft:site-y site) (luft:site-z site)
              (luft:site-extent site) (luft:site-positive-p site)
              (site-inspection-distance inspection)))))

(defun make-fly-camera
    (&key (position
            (vec3:make-vec3 (+ 70.0 +sanctuary-origin-x+)
                            (+ -18.0 +sanctuary-origin-y+) 50.0))
          (yaw 2.2455373) (pitch -0.5165006)
          (field-of-view 0.9599311))
  (make-instance 'fly-camera :position position :yaw yaw :pitch pitch
                             :field-of-view field-of-view))

(defun reset-viewer-camera (&optional (viewer *viewer*))
  "Return VIEWER to the sanctuary spawn and its following isometric view."
  (when viewer
    (let ((camera (viewer-camera viewer))
          (player (viewer-player viewer)))
      (setf (camera-yaw camera) 2.2455373
            (camera-pitch camera) -0.5165006
            (camera-field-of-view camera) 0.9599311
            *projection* :isometric
            ;; The player owns the frame now: keep the playable view close
            ;; enough to read footsteps and terrain relief without losing the
            ;; next turn of the route.
            *isometric-height* (if player 18.0 64.0))
      (if player
          (let ((spawn (make-walking-player)))
            (setf (viewer-player viewer) spawn)
            (follow-walking-player camera spawn)
            ;; Reset is also a camera move: do not briefly reveal the old
            ;; outside perch when the spawn happens to be indoors.
            (constrain-viewer-follow-camera viewer))
          (setf (camera-position camera)
                (vec3:make-vec3 (+ 70.0 +sanctuary-origin-x+)
                                (+ -18.0 +sanctuary-origin-y+) 50.0)))
      (when (viewer-renderer viewer)
        (setf (renderer-history-valid-p (viewer-renderer viewer)) nil))))
  viewer)

(defun camera-basis (camera)
  (let* ((yaw (camera-yaw camera))
         (pitch (camera-pitch camera))
         (forward (vec3:make-vec3 (* (cos yaw) (cos pitch))
                                  (* (sin yaw) (cos pitch))
                                  (sin pitch)))
         (right (vec3:make-vec3 (sin yaw) (- (cos yaw)) 0.0))
         (up (vec3:vec3-cross right forward)))
    (values right up forward)))

(defun add-scaled-directions (origin &rest direction-scales)
  (let ((x (vec3:vec3-x origin))
        (y (vec3:vec3-y origin))
        (z (vec3:vec3-z origin)))
    (loop for (direction scale) on direction-scales by #'cddr
          do (incf x (* (vec3:vec3-x direction) scale))
             (incf y (* (vec3:vec3-y direction) scale))
             (incf z (* (vec3:vec3-z direction) scale)))
    (vec3:make-vec3 x y z)))

(defgeneric inspection-face-stock (source face)
  (:documentation "Return the atelier stock at FACE in SOURCE."))

(defmethod inspection-face-stock ((source t) face)
  (default-face-stock face))

(defmethod inspection-face-stock ((source scene) face)
  (scene-face-stock source face))

(defun ray-axis-crossings (position direction)
  "Return grid STEP, first crossing distance, and crossing interval."
  (cond ((plusp direction)
         (values 1 (/ (- (1+ (floor position)) position) direction)
                 (/ direction)))
        ((minusp direction)
         (values -1 (/ (- position (floor position)) (- direction))
                 (/ (- direction))))
        (t (values 0 most-positive-single-float most-positive-single-float))))

(defun ray-entry-face (solid cell-x cell-y cell-z axis step)
  "Return the outward face through which a ray entered CELL along AXIS."
  (let* ((domain (luft:chain-domain solid))
         (anchor-x (+ cell-x (if (and (eq axis :x) (minusp step)) 1 0)))
         (anchor-y (+ cell-y (if (and (eq axis :y) (minusp step)) 1 0)))
         (anchor-z (+ cell-z (if (and (eq axis :z) (minusp step)) 1 0)))
         (extent (ecase axis
                   (:x luft:+yz-face-extent+)
                   (:y luft:+xz-face-extent+)
                   (:z luft:+xy-face-extent+)))
         (geometry (luft:make-site domain anchor-x anchor-y anchor-z extent 1))
         (occupancy (lambda (x y z)
                      (luft:chain-cell-occupancy-bit solid x y z))))
    (luft:orient-face-outward domain geometry occupancy)))

(defun make-site-inspection (source face cell-x cell-y cell-z point distance)
  (let* ((solid (inspection-source-solid source))
         (domain (luft:chain-domain solid))
         (occupancy (lambda (x y z)
                      (luft:chain-cell-occupancy-bit solid x y z)))
         (cell (luft:make-site domain cell-x cell-y cell-z
                               luft:+cell-extent+ 1)))
    (make-instance
     'site-inspection :source source :site face :cell cell :point point
     :distance distance
     :star-mask
     (luft:site-star-occupancy-mask
      domain
      (luft:make-site domain cell-x cell-y cell-z luft:+vertex-extent+ 1)
      occupancy)
     :stock (inspection-face-stock source face))))

(defun raycast-site (source origin direction
                     &key (max-distance *inspection-reach*))
  "Return the first outward LUFT face met by a continuous lattice ray.

Tied edge and corner crossings advance together, so a ray never reports a
cell it merely touches.  The returned SITE-INSPECTION is the one sparse object
boundary over the packed chain and dense face records."
  (let* ((solid (inspection-source-solid source))
         (direction (vec3:vec3-normalize direction))
         (x (floor (vec3:vec3-x origin)))
         (y (floor (vec3:vec3-y origin)))
         (z (floor (vec3:vec3-z origin)))
         step-x step-y step-z
         next-x next-y next-z
         delta-x delta-y delta-z
         (distance 0.0)
         (entry-axis nil)
         (entry-step 0))
    (multiple-value-setq (step-x next-x delta-x)
      (ray-axis-crossings (vec3:vec3-x origin) (vec3:vec3-x direction)))
    (multiple-value-setq (step-y next-y delta-y)
      (ray-axis-crossings (vec3:vec3-y origin) (vec3:vec3-y direction)))
    (multiple-value-setq (step-z next-z delta-z)
      (ray-axis-crossings (vec3:vec3-z origin) (vec3:vec3-z direction)))
    (loop
      (when (= 1 (luft:chain-cell-occupancy-bit solid x y z))
        (when entry-axis
          (let* ((face (ray-entry-face solid x y z entry-axis entry-step))
                 (point (add-scaled-directions origin direction distance)))
            (when face
              (return
                (make-site-inspection source face x y z point distance))))))
      (let ((next (min next-x next-y next-z)))
        (when (> next max-distance) (return nil))
        (setf distance next
              entry-axis nil)
        ;; Remember one carrier face for tied crossings, choosing the axis
        ;; most aligned with the ray.  All tied cells still advance together.
        (flet ((remember (axis step component)
                 (when (or (null entry-axis)
                           (> (abs component)
                              (abs (vec3:vec3-component direction entry-axis))))
                   (setf entry-axis axis entry-step step))))
          (when (<= next-x (+ next 1.0e-6))
            (incf x step-x)
            (incf next-x delta-x)
            (remember :x step-x (vec3:vec3-x direction)))
          (when (<= next-y (+ next 1.0e-6))
            (incf y step-y)
            (incf next-y delta-y)
            (remember :y step-y (vec3:vec3-y direction)))
          (when (<= next-z (+ next 1.0e-6))
            (incf z step-z)
            (incf next-z delta-z)
            (remember :z step-z (vec3:vec3-z direction))))))))

(defun constrain-viewer-follow-camera (viewer)
  "Keep a following camera on the traveler's side of sanctuary geometry.

The player can pass through a gate while the preferred isometric perch is
still outside the rampart.  Cast from the look-at point to the *actual*
smoothed camera position, rather than the ideal perch: this preserves the
soft follow but never lets a wall sit between the eye and the hermit."
  (let ((player (viewer-player viewer)))
    (when player
      (let* ((player-position (walking-player-position player))
             (heading-x (walking-player-heading-x player))
             (heading-y (walking-player-heading-y player))
             ;; Match FOLLOW-WALKING-PLAYER's look-ahead exactly.  This is
             ;; the point the player-owned frame promises to keep visible.
             (aim-x (+ (vec3:vec3-x player-position) (* 2.4 heading-x)))
             (aim-y (+ (vec3:vec3-y player-position) (* 2.4 heading-y)))
             (aim-z (+ (vec3:vec3-z player-position) 1.45))
             (aim (vec3:make-vec3 aim-x aim-y aim-z))
             (camera-position (camera-position (viewer-camera viewer)))
             (dx (- (vec3:vec3-x camera-position) aim-x))
             (dy (- (vec3:vec3-y camera-position) aim-y))
             (dz (- (vec3:vec3-z camera-position) aim-z))
             (distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
        (when (> distance 0.01)
          (let ((inspection
                  ;; The sanctuary is finite while the pleasant isometric
                  ;; perch can look out across its edge.  An off-map ray is
                  ;; simply no occluder, never a reason to park the frame.
                  (handler-case
                      (raycast-site (viewer-source viewer) aim
                                    (vec3:make-vec3 dx dy dz)
                                    :max-distance distance)
                    (error () nil))))
            (when inspection
              ;; Leave enough room for the near plane and an over-the-
              ;; shoulder silhouette.  The camera therefore tucks into the
              ;; room smoothly instead of landing on the wall itself.
              (let* ((safe-distance
                       (max 0.75 (- (site-inspection-distance inspection)
                                    0.40)))
                     (scale (/ safe-distance distance)))
                (setf (vec3:vec3-x camera-position) (+ aim-x (* dx scale))
                      (vec3:vec3-y camera-position) (+ aim-y (* dy scale))
                      (vec3:vec3-z camera-position) (+ aim-z (* dz scale))))))))))
  viewer)

(defun projection-lane (width height field-of-view near far)
  "The four projection coefficients and the homogeneous-divisor selector.

Both projections use the same three rows: clip X and Y are the view
coordinates scaled, and clip Z is an affine function of view depth.  The
perspective divisor is the view depth and the isometric divisor is one, so
the selector is the whole of the difference."
  (let ((aspect (/ (coerce width 'single-float) height)))
    (ecase *projection*
      (:perspective
       (let ((focal (/ (tan (/ field-of-view 2.0)))))
         (values (/ focal aspect) focal
                 (/ far (- far near))
                 (/ (- (* far near)) (- far near))
                 1.0)))
      (:isometric
       (let ((half (/ *isometric-height* 2.0)))
         (values (/ 1.0 (* half aspect)) (/ 1.0 half)
                 (/ 1.0 (- far near))
                 (/ (- near) (- far near))
                 0.0))))))

(defstruct (frame-view (:constructor %make-frame-view))
  "One immutable camera sample shared by geometry and temporal motion."
  position right up forward projection divisor jitter)

(defun halton (index base)
  (loop with fraction = (/ 1.0 base)
        with value = 0.0
        while (plusp index)
        do (incf value (* fraction (mod index base)))
           (setf index (floor index base)
                 fraction (/ fraction base))
        finally (return (coerce value 'single-float))))

(defun temporal-jitter (frame-index width height)
  "Sample the eight-position Halton(2,3) sequence in clip coordinates."
  (let ((sample (1+ (mod frame-index 8))))
    (vector (coerce (/ (* 2.0 (- (halton sample 2) 0.5))
                       (max width 1))
                    'single-float)
            (coerce (/ (* 2.0 (- (halton sample 3) 0.5))
                       (max height 1))
                    'single-float))))

(defun capture-frame-view (camera width height jitter)
  (multiple-value-bind (right up forward) (camera-basis camera)
    (let ((near (if (eq *projection* :isometric)
                    +orthographic-near+ 0.1))
          (far (if (eq *projection* :isometric)
                   +orthographic-far+ 600.0)))
      (multiple-value-bind (px py pz pw divisor)
          (projection-lane width height (camera-field-of-view camera)
                           near far)
        (%make-frame-view
         :position (let ((position (camera-position camera)))
                     (vec3:make-vec3 (vec3:vec3-x position)
                                     (vec3:vec3-y position)
                                     (vec3:vec3-z position)))
         :right right :up up :forward forward
         :projection (vector px py pz pw)
         :divisor divisor :jitter jitter)))))

(defun camera-uniform-data
    (view previous inspection-parameters ink-strength player
     &optional (bevel-width luft:+mesh-bevel-width+) (exposure 1.0f0))
  (flet ((lane (vector fourth)
           (list (vec3:vec3-x vector) (vec3:vec3-y vector)
                 (vec3:vec3-z vector) fourth)))
    (multiple-value-bind (character previous-character character-direction
                          ball previous-ball)
        (if player
            (walking-player-render-lanes player)
            (values '(0.0 0.0 0.0 0.0) '(0.0 0.0 0.0 0.0)
                    '(0.0 1.0 0.0 1.0)
                    '(0.0 0.0 0.0 0.0) '(0.0 0.0 0.0 0.0)))
      (make-array
       108 :element-type 'single-float
       :initial-contents
       (mapcar
        (lambda (value) (coerce value 'single-float))
        (append (lane (frame-view-position view) 0.0)
                (lane (frame-view-right view) 0.0)
                (lane (frame-view-up view) 0.0)
                (lane (frame-view-forward view) 0.0)
                (coerce (frame-view-projection view) 'list)
                (list (/ bevel-width luft:+mesh-cell-size+)
                      *wireframe*
                      (frame-view-divisor view) ink-strength)
                (lane (frame-view-position previous) 0.0)
                (lane (frame-view-right previous) 0.0)
                (lane (frame-view-up previous) 0.0)
                (lane (frame-view-forward previous) 0.0)
                (coerce (frame-view-projection previous) 'list)
                ;; MetalFX receives the current sample offset separately and
                ;; its default motion-vector contract is unjittered.  Retain
                ;; the previous lane shape for ABI stability, but only the
                ;; current two components participate in rasterization.
                (list (aref (frame-view-jitter view) 0)
                      (aref (frame-view-jitter view) 1)
                      (aref (frame-view-jitter previous) 0)
                      (aref (frame-view-jitter previous) 1))
                (coerce inspection-parameters 'list)
                character
                (light-uniform-data *light* (frame-view-position view) exposure)
                previous-character character-direction ball previous-ball))))))

(defun viewer-logical-extent (viewer)
  (let ((canvas (viewer-canvas viewer)))
    (list (canvas-width canvas) (canvas-height canvas))))

(defun viewer-pointer-position (viewer)
  (let ((extent (viewer-logical-extent viewer)))
    (if (viewer-pointer-captured-p viewer)
        (values (/ (first extent) 2.0) (/ (second extent) 2.0))
        (values (or (viewer-pointer-x viewer) (/ (first extent) 2.0))
                (or (viewer-pointer-y viewer) (/ (second extent) 2.0))))))

(defun viewer-pointer-ray (viewer)
  "Return the world-space origin and direction through VIEWER's pointer."
  (let* ((extent (viewer-logical-extent viewer))
         (camera (viewer-camera viewer))
         (width (first extent))
         (height (second extent))
         (view (capture-frame-view camera width height #(0.0 0.0)))
         (projection (frame-view-projection view)))
    (multiple-value-bind (pointer-x pointer-y)
        (viewer-pointer-position viewer)
      (let* ((ndc-x (- (* 2.0 (/ pointer-x width)) 1.0))
             (ndc-y (- 1.0 (* 2.0 (/ pointer-y height))))
             (right-scale (/ ndc-x (aref projection 0)))
             (up-scale (/ ndc-y (aref projection 1))))
        (if (eq *projection* :perspective)
            (values
             (camera-position camera)
             (vec3:vec3-normalize
              (add-scaled-directions
               (frame-view-forward view)
               (frame-view-right view) right-scale
               (frame-view-up view) up-scale)))
            (values
             (add-scaled-directions
              (camera-position camera)
              (frame-view-right view) right-scale
              (frame-view-up view) up-scale)
             (frame-view-forward view)))))))

(defun same-inspected-site-p (left right)
  (or (eq left right)
      (and left right
           (= (site-inspection-site left) (site-inspection-site right)))))

(defun update-viewer-inspection (viewer)
  (multiple-value-bind (origin direction) (viewer-pointer-ray viewer)
    (let* ((inspection
             (handler-case
                 (raycast-site (viewer-source viewer) origin direction)
               (luft:outside-domain () nil)))
           (changed-p
             (not (same-inspected-site-p
                   inspection (viewer-inspection viewer)))))
      ;; Retain the exact current distance and point even while the semantic
      ;; site remains the same; repaint McCLIM only at semantic boundaries.
      (setf (viewer-inspection viewer) inspection)
      (when changed-p (refresh-viewer-inspector viewer))
      inspection)))

(defun viewer-inspection-parameters (viewer extent)
  (let ((logical-extent (viewer-logical-extent viewer)))
    (multiple-value-bind (pointer-x pointer-y)
        (viewer-pointer-position viewer)
      (vector (coerce (/ pointer-x (first logical-extent)) 'single-float)
              (coerce (/ pointer-y (second logical-extent)) 'single-float)
              (coerce (/ (first extent)) 'single-float)
              (coerce (/ (second extent)) 'single-float)))))

(declaim (ftype function viewer-surface-view
                viewer-inspector-p
                viewer-inspector-mirror
                viewer-instruments-present-p
                refresh-viewer-instruments
                encode-viewer-instruments
                release-viewer-instruments
                attach-viewer-lobby
                open-viewer-status-bar
                make-tracked-renderer
                attach-viewer-live-artifact
                viewer-live-artifact
                release-viewer-live-artifact
                force-viewer-live-artifact-refresh
                note-viewer-renderer-replacement
                %perform-viewer-stop))

(defun prepare-viewer-frame-renderer (viewer)
  "Publish frame-boundary instruments, then borrow VIEWER's renderer.

An instrument operation may transactionally replace the complete renderer
cohort and retire the previous one.  No frame may borrow that previous cohort
before the operation boundary, or it would encode through resources which the
  same canvas thread has just retired."
  (refresh-viewer-instruments viewer)
  ;; Inspector repaint is semantic and therefore sparse.  Shader definitions
  ;; are independently live, so the static retained stream still refreshes its
  ;; direct compositor here, before ENCODE-RENDERER-FRAME opens a pass.
  (when (and (viewer-inspector-p viewer)
             (viewer-inspector-mirror viewer))
    (mcluv:prepare-gpu-mirror-compositor
     (viewer-inspector-mirror viewer)))
  (or (viewer-renderer viewer)
      (error "LUFT viewer has no renderer after frame-boundary publication.")))

(defun encode-viewer-frame
    (viewer encoder surface-texture extent
     &key (inspector-p (viewer-inspector-p viewer)))
  (let* ((renderer (prepare-viewer-frame-renderer viewer))
         (surface-view (viewer-surface-view viewer surface-texture))
         (render-extent (render-scale-extent extent))
         (width (first render-extent))
         (height (second render-extent))
         (jitter (if (renderer-temporal-p renderer)
                     (temporal-jitter (renderer-frame-index renderer)
                                      width height)
                     #(0.0 0.0)))
         (view (capture-frame-view (viewer-camera viewer)
                                   width height jitter))
         (previous (or (renderer-previous-view renderer) view))
         (inspection
           (and (or inspector-p
                    (viewer-mode-inspection-p (viewer-mode viewer)))
                (update-viewer-inspection viewer)))
         (player (viewer-player viewer))
         (player-p (and player (typep (viewer-source viewer) 'scene)
                        (scene-player-p (viewer-source viewer))))
         (exposure
           (or (viewer-fixed-exposure viewer)
               (maintain-renderer-exposure renderer))))
    ;; Instrument state is now immutable for this frame.  Encoding below only
    ;; replays prepared GPU commands against the renderer borrowed afterward.
    (encode-renderer-frame
     renderer encoder surface-view extent
     (camera-uniform-data
      view previous (viewer-inspection-parameters viewer render-extent)
      (if (and inspection *inspection-ink-p*) 1.0 0.0)
      (and player-p player)
      (viewer-bevel-width viewer)
      exposure)
     :jitter jitter :view view
     :player-p player-p
     :effect-time (or *flame-time* (viewer-last-timestamp viewer) 0.0)
     ;; Dense lattice dots are a close-study diagnostic, not a terrain view:
     ;; streamed chunks can each contain more than a million markers. The
     ;; shader-backed construction lines remain controlled by *WIREFRAME*.
     :construction-p (and (plusp *wireframe*)
                          (not (typep (viewer-source viewer)
                                      'streaming-scene)))
     :overlay-encoder
     (and (or inspector-p (viewer-instruments-present-p viewer))
          (lambda (pass)
            (when inspector-p
              (mcluv:encode-direct-gpu-mirror
               (viewer-inspector-compositor viewer) pass surface-texture
               (viewer-inspector-state viewer extent)))
            ;; Instruments are ordered low-to-high so modal tools render last.
            (encode-viewer-instruments
             viewer pass surface-texture extent))))))

(clim:define-command-table luft-window)
(clim:define-command-table luft-window-release)
(clim:define-command-table luft-atelier)
(clim:define-command-table luft-atelier-release)

(defconstant +site-inspector-width+ 372)
(defconstant +site-inspector-height+ 306)

(defclass site-inspector-pane (clim:application-pane) ())

(clim:define-presentation-type luft-site ())

(defun site-extent-label (site)
  (with-output-to-string (stream)
    (dolist (axis '(:x :y :z))
      (when (luft:site-extends-p site axis)
        (write-char (char (symbol-name axis) 0) stream)))))

(defun inspector-row (stream y label value &key presentation)
  (clim:draw-text* stream label 18 y :align-y :center :text-size 12
                   :ink (clim:make-rgb-color 0.48 0.52 0.55))
  (flet ((draw ()
           (clim:draw-text* stream value 148 y :align-y :center :text-size 12
                            :ink (clim:make-rgb-color 0.86 0.88 0.84))))
    (if presentation
        (clim:with-output-as-presentation (stream presentation 'luft-site)
          (draw))
        (draw))))

(defun bevel-width-label (bevel-width)
  (let ((divisor (gcd bevel-width luft:+mesh-cell-size+)))
    (format nil "~D/~D"
            (/ bevel-width divisor)
            (/ luft:+mesh-cell-size+ divisor))))

(defun viewer-bevel-label (viewer)
  (if (viewer-bevel-profile viewer)
      "mixed"
      (bevel-width-label (viewer-bevel-width viewer))))

(defun next-bevel-width (bevel-width)
  (case bevel-width
    (1 2)
    (2 4)
    (otherwise 1)))

(defun display-site-inspector (viewer stream)
  "Draw VIEWER's current sparse ray hit as McCLIM presentations."
  (clim:draw-rectangle* stream 0 0 +site-inspector-width+
                        +site-inspector-height+
                        :ink (clim:make-rgb-color 0.025 0.070 0.090))
  (clim:draw-text* stream
                   (format nil "LUFT · BEVEL ~A · C LINES ~A"
                           (viewer-bevel-label viewer)
                           (if (plusp *wireframe*) "ON" "OFF"))
                   18 25 :align-y :center :text-size 14
                   :text-face :bold
                   :ink (clim:make-rgb-color 0.42 0.91 0.94))
  (let ((inspection (viewer-inspection viewer)))
    (if (null inspection)
        (progn
          (clim:draw-text* stream "point at the boundary" 18 60
                           :align-y :center :text-size 13
                           :ink (clim:make-rgb-color 0.58 0.62 0.62))
          (clim:draw-text* stream "escape releases input; click locks it" 18 84
                           :align-y :center :text-size 11
                           :ink (clim:make-rgb-color 0.40 0.44 0.45)))
        (let* ((site (site-inspection-site inspection))
               (cell (site-inspection-cell inspection))
               (point (site-inspection-point inspection))
               (star-mask (site-inspection-star-mask inspection)))
          (inspector-row
           stream 57 "site"
           (format nil "~D, ~D, ~D" (luft:site-x site)
                   (luft:site-y site) (luft:site-z site))
           :presentation site)
          (inspector-row stream 80 "extent"
                         (format nil "~A · dimension ~D"
                                 (site-extent-label site)
                                 (luft:site-dimension site)))
          (inspector-row stream 103 "orientation"
                         (if (luft:site-positive-p site) "+" "−"))
          (inspector-row
           stream 126 "solid cell"
           (format nil "~D, ~D, ~D" (luft:site-x cell)
                   (luft:site-y cell) (luft:site-z cell))
           :presentation cell)
          (inspector-row stream 149 "distance"
                         (format nil "~,3F cells"
                                 (site-inspection-distance inspection)))
          (inspector-row stream 172 "world hit"
                         (format nil "~,2F  ~,2F  ~,2F"
                                 (vec3:vec3-x point) (vec3:vec3-y point)
                                 (vec3:vec3-z point)))
          (inspector-row stream 195 "stock"
                         (format nil "~D" (site-inspection-stock inspection)))
          (inspector-row stream 218 "cell-corner star"
                         (format nil "#x~2,'0X" star-mask))
          (inspector-row stream 241 "star topology"
                         (if (luft:star-singular-p star-mask)
                             "singular · split sheets"
                             "regular"))
          (inspector-row
           stream 264 "spike junction"
           (handler-case
               (format nil "~D regular sheet~:P"
                       (length (luft:decompose-star-mask star-mask)))
             (error () "covered junction · next spike")))))))

(defun refresh-viewer-inspector (viewer)
  (let ((pane (and (viewer-inspector-p viewer)
                   (ignore-errors (clim:find-pane-named viewer 'inspector)))))
    (when pane
      (let ((mirror (clim:sheet-direct-mirror
                     (clim:frame-top-level-sheet viewer))))
        (setf (viewer-inspector-mirror viewer) mirror)
        (mcluv:call-with-gpu-mirror-sheet-repaint
         mirror pane
         (lambda ()
           (clim:redisplay-frame-pane viewer pane :force-p t)
           ;; Application panes may follow their output cursor even without
           ;; visible scroll bars; this fixed HUD stays pinned at its origin.
           (clim:scroll-extent pane 0 0))))))
  viewer)

(defun viewer-inspector-state (viewer extent)
  (declare (ignore extent))
  (when (viewer-inspector-mirror viewer)
    (let* ((logical-extent (viewer-logical-extent viewer))
           (width (first logical-extent))
           (height (second logical-extent))
           (margin 14.0)
           (half-width (/ +site-inspector-width+ width))
           (half-height (/ +site-inspector-height+ height))
           (center-x (- 1.0 (/ (* 2.0 margin) width) half-width))
           (center-y (- 1.0 (/ (* 2.0 margin) height) half-height)))
      (make-array
       12 :element-type 'single-float
       :initial-contents
       (mapcar (lambda (value) (coerce value 'single-float))
               (list center-x center-y 0.0 1.0
                     half-width 0.0 0.0 0.0
                     0.0 half-height 0.0 0.0))))))

(clim:define-application-frame viewer
    (clim:standard-application-frame canvas-event-handler)
  ((canvas :initarg :canvas :initform nil :reader viewer-canvas)
   (context :initarg :context :initform nil :reader viewer-context)
   (device :initarg :device :initform nil :reader viewer-device)
   (source :initarg :source :initform (make-mountain-sanctuary-scene)
           :accessor viewer-source)
   (renderer :initarg :renderer :initform nil :accessor viewer-renderer)
   (production-system :initarg :production-system :initform nil
                      :accessor viewer-production-system)
   (bevel-width :initarg :bevel-width :initform 2
                :accessor viewer-bevel-width)
   (bevel-profile :initarg :bevel-profile :initform nil
                  :accessor viewer-bevel-profile)
   (fixed-exposure :initarg :fixed-exposure :initform nil
                   :reader viewer-fixed-exposure)
   (camera :initarg :camera :initform (make-fly-camera) :reader viewer-camera)
   (player :initarg :player :initform (make-walking-player)
           :accessor viewer-player)
   (mode :initarg :mode :initform (make-instance 'isometric-walk-mode)
         :accessor viewer-mode)
   (surface-views :initform (make-hash-table :test #'eql)
                  :reader viewer-surface-views)
   (controls :initform (make-hash-table :test #'eq)
             :reader viewer-controls)
   (pointer-captured-p :initform nil :accessor viewer-pointer-captured-p)
   (pointer-x :initform nil :accessor viewer-pointer-x)
   (pointer-y :initform nil :accessor viewer-pointer-y)
   (inspection :initform nil :accessor viewer-inspection)
   (inspector-p :initarg :inspector-p :initform nil
                :accessor viewer-inspector-p)
   (inspector-mirror :initform nil :accessor viewer-inspector-mirror)
   (inspector-compositor :initform nil
                         :accessor viewer-inspector-compositor)
   (last-timestamp :initform nil :accessor viewer-last-timestamp)
   (speed :initarg :speed :initform 4.0 :accessor viewer-speed)
   (sensitivity :initarg :sensitivity :initform 0.0032
                :accessor viewer-sensitivity)
   (shader-diagnostic :initform nil :accessor viewer-shader-diagnostic)
   (running-p :initform t :accessor viewer-running-p)
   (stop-controller
    :initarg :stop-controller
    :initform (make-stop-controller :name "LUFT viewer")
    :reader viewer-stop-controller))
  ;; The frame is the application and the inspector is its first pane.  Its
  ;; command table inherits every input phase so McCLIM considers each command
  ;; executable; event dispatch still chooses one phase explicitly.
  (:command-table (luft-viewer
                   :inherit-from (luft-window luft-window-release
                                  luft-atelier luft-atelier-release)
                   :inherit-menu t))
  (:panes
   (inspector :application
              :display-function 'display-site-inspector
              :scroll-bars nil
              :default-text-style (clim:make-text-style :sans-serif nil
                                                        :normal)))
  (:layouts
   (default
    (clim:horizontally (:width +site-inspector-width+
                        :height +site-inspector-height+)
      inspector)))
  (:menu-bar nil))

(defun viewer-surface-view (viewer surface)
  "Return VIEWER's texture view for the current presentation slot."
  (let* ((context (viewer-context viewer))
         (key (canvas-frame-resource-key context surface))
         (views (viewer-surface-views viewer))
         (view (gethash key views)))
    (when (and view (not (eq surface (gpu-texture-view-texture view))))
      ;; Metal returns a fresh borrowed wrapper when it revisits a drawable.
      ;; Keep the stable slot but never let its view retain the old wrapper.
      (destroy view)
      (setf view nil))
    (or view
        (setf (gethash key views)
              (create (viewer-device viewer)
                      (make-texture-view-descriptor :texture surface))))))

(defun release-viewer-surface-views (viewer)
  (with-release-report
    (maphash (lambda (key view)
               (declare (ignore key))
               (releasing :surface-view (destroy view)))
             (viewer-surface-views viewer))
    (clrhash (viewer-surface-views viewer)))
  (values))

(defun release-viewer-surface-view (viewer surface)
  "Release only VIEWER's cached view of SURFACE, when one exists."
  (let* ((context (viewer-context viewer))
         (key (canvas-frame-resource-key context surface))
         (views (viewer-surface-views viewer)))
    (multiple-value-bind (view present-p) (gethash key views)
      (when present-p
        (when view (destroy view))
        (remhash key views))))
  (values))

(defun viewer-control-active-p (viewer direction)
  (gethash direction (viewer-controls viewer)))

(defun set-viewer-control (viewer direction active-p)
  (if active-p
      (progn
        (when (viewer-player viewer)
          (cancel-walking-player-route (viewer-player viewer)))
        (setf (gethash direction (viewer-controls viewer)) t))
      (remhash direction (viewer-controls viewer)))
  viewer)

(defun clear-viewer-controls (viewer)
  (clrhash (viewer-controls viewer))
  viewer)

(defun advance-viewer-camera (viewer timestamp)
  (let* ((last (viewer-last-timestamp viewer))
         (dt (if last (min 0.1 (max 0.0 (- timestamp last))) 0.0))
         (camera (viewer-camera viewer))
         (step (* dt (viewer-speed viewer))))
    (setf (viewer-last-timestamp viewer) timestamp)
    (if (viewer-player viewer)
        (let ((forward (- (if (viewer-control-active-p viewer :forward) 1 0)
                          (if (viewer-control-active-p viewer :backward) 1 0)))
              (right (- (if (viewer-control-active-p viewer :right) 1 0)
                        (if (viewer-control-active-p viewer :left) 1 0)))
              (maximum-distance nil))
          (when (and (zerop forward) (zerop right))
            (multiple-value-setq (forward right maximum-distance)
              (walking-player-route-control (viewer-player viewer) camera)))
          (advance-walking-player (viewer-player viewer)
                                  (viewer-source viewer) camera
                                  (or forward 0.0) (or right 0.0) dt
                                  :maximum-distance maximum-distance)
          (trim-walking-player-route (viewer-player viewer))
          ;; The first timestamp establishes the follow pose immediately.
          ;; Subsequent zero-duration samples preserve it; in Common Lisp a
          ;; numeric zero is true, so DT alone cannot express that distinction.
          (follow-walking-player camera (viewer-player viewer)
                                 :seconds (and last dt))
          (constrain-viewer-follow-camera viewer))
        (multiple-value-bind (right up forward) (camera-basis camera)
          (flet ((move (direction amount)
                   (let ((position (camera-position camera)))
                     (setf (camera-position camera)
                           (add-scaled-directions position direction amount)))))
            (when (viewer-control-active-p viewer :forward)
              (move forward step))
            (when (viewer-control-active-p viewer :backward)
              (move forward (- step)))
            (when (viewer-control-active-p viewer :right) (move right step))
            (when (viewer-control-active-p viewer :left) (move right (- step)))
            (when (viewer-control-active-p viewer :up) (move up step))
            (when (viewer-control-active-p viewer :down) (move up (- step))))))))

(defun advance-viewer-streaming (viewer)
  "Drain completed meshes and admit the next mock residency change."
  (let ((source (viewer-source viewer))
        (production-system (viewer-production-system viewer)))
    (when (and (typep source 'streaming-scene) production-system)
      (drain-streaming-scene-production
       source (viewer-renderer viewer) production-system)
      ;; One complete neighborhood cohort crosses the owner boundary before
      ;; another residency change can invalidate it.
      (when (and (null (streaming-scene-cohort source))
                 (null (streaming-scene-removals source)))
        (incf (streaming-scene-frame-counter source))
        (when (>= (streaming-scene-frame-counter source)
                  (streaming-scene-frames-per-load source))
          (setf (streaming-scene-frame-counter source) 0)
          (let ((position (camera-position (viewer-camera viewer))))
            (retarget-streaming-scene
             source production-system (viewer-bevel-width viewer)
             (vec3:vec3-x position) (vec3:vec3-y position)
             (viewer-bevel-profile viewer))))))))

(defun render-viewer-frame (viewer timestamp)
  (declare (ignore timestamp))
  (when (viewer-running-p viewer)
    ;; Source callbacks only advance revisions.  Compilation and complete
    ;; cohort publication happen here, before this frame borrows the renderer.
    (refresh-application-live-artifacts viewer)
    (advance-viewer-streaming viewer)
    (present-canvas-frame
     (viewer-context viewer)
     (lambda (surface-texture encoder presentation-time)
       (advance-viewer-camera viewer presentation-time)
       (let ((extent (canvas-extent (viewer-context viewer))))
         (encode-viewer-frame viewer encoder surface-texture extent))))))

(defun viewer-command-viewer ()
  "Return the LUFT application receiving the current McCLIM command."
  clim:*application-frame*)

(defun request-viewer-quit (viewer)
  "Begin an orderly stop once and return true when this call began it."
  ;; Native input closes capture admission without waiting.  The worker below
  ;; drains the capture beside this thread before crossing its frame barrier.
  (request-application-capture-shutdown viewer)
  (setf (viewer-running-p viewer) nil)
  ;; A native close and a command both run on the canvas thread.  Reserve the
  ;; one teardown owner here, but run it beside that thread so its synchronous
  ;; frame-boundary barrier can complete before CLOSE-CANVAS ends the loop.
  (nth-value
   0
   (request-controlled-stop
    (viewer-stop-controller viewer)
    (lambda () (%perform-viewer-stop viewer))
    :thread-name "LUFT viewer quit")))

(clim:define-command (com-start-moving :command-table luft-atelier
                                       :name "Start Moving")
    ((direction 'keyword :prompt "direction"))
  (set-viewer-control (viewer-command-viewer) direction t))

(clim:define-command (com-stop-moving :command-table luft-atelier-release
                                      :name "Stop Moving")
    ((direction 'keyword :prompt "direction"))
  (set-viewer-control (viewer-command-viewer) direction nil))

(clim:define-command (com-reset-view :command-table luft-atelier
                                     :name "Reset View"
                                     :keystroke (:r))
    ()
  (reset-viewer-camera (viewer-command-viewer)))

(clim:define-command (com-jump :command-table luft-atelier
                                :name "Jump"
                                :keystroke (:space))
    ()
  (let ((player (viewer-player (viewer-command-viewer))))
    (when player (request-walking-player-jump player))))

(clim:define-command (com-toggle-construction-lines
                      :command-table luft-atelier
                      :name "Toggle Construction Lines"
                      :keystroke (:c))
    ()
  (let ((viewer (viewer-command-viewer)))
    (setf *wireframe* (if (plusp *wireframe*) 0.0 1.0))
    (when (viewer-renderer viewer)
      (setf (renderer-history-valid-p (viewer-renderer viewer)) nil))
    (refresh-viewer-inspector viewer)))

(declaim (ftype function refresh-viewer-renderer))

(clim:define-command (com-toggle-bevel-width
                      :command-table luft-atelier
                      :name "Toggle Bevel Width"
                      :keystroke (:b))
    ()
  (let* ((viewer (viewer-command-viewer))
         (profile (viewer-bevel-profile viewer))
         (bevel-width (viewer-bevel-width viewer)))
    (cond
      (profile
       (refresh-viewer-renderer
        viewer :solid (viewer-source viewer) :bevel-width 1
               :bevel-profile nil))
      ((= bevel-width 4)
       (refresh-viewer-renderer
        viewer :solid (viewer-source viewer) :bevel-width 4
               :bevel-profile (make-material-bevel-profile)))
      (t
       (refresh-viewer-renderer
        viewer :solid (viewer-source viewer)
               :bevel-width (next-bevel-width bevel-width)
               :bevel-profile nil)))
    (refresh-viewer-inspector viewer)))

(clim:define-command (com-release-pointer :command-table luft-window
                                          :name "Release Pointer"
                                          :keystroke (:escape))
    ()
  (let* ((viewer (viewer-command-viewer))
         (canvas (viewer-canvas viewer)))
    (clear-viewer-controls viewer)
    (when (viewer-pointer-captured-p viewer)
      (set-canvas-relative-pointer-mode canvas nil)
      (setf (viewer-pointer-captured-p viewer) nil))))

(clim:define-command (com-toggle-fullscreen :command-table luft-window
                                            :name "Toggle Fullscreen"
                                            :keystroke (:f11))
    ()
  (let ((canvas (viewer-canvas (viewer-command-viewer))))
    (set-canvas-fullscreen canvas (not (canvas-fullscreen-p canvas)))))

(defun set-viewer-mode (viewer mode)
  "Install MODE and make its pointer ownership immediately true on screen."
  (check-type mode viewer-mode)
  (clear-viewer-controls viewer)
  (when (viewer-pointer-captured-p viewer)
    (set-canvas-relative-pointer-mode (viewer-canvas viewer) nil)
    (setf (viewer-pointer-captured-p viewer) nil))
  (setf (viewer-mode viewer) mode)
  viewer)

(clim:define-command (com-toggle-viewer-mode :command-table luft-window
                                             :name "Toggle Interaction Mode"
                                             :keystroke (:m))
    ()
  (let ((viewer (viewer-command-viewer)))
    (set-viewer-mode
     viewer
     (if (typep (viewer-mode viewer) 'isometric-walk-mode)
         (make-instance 'orbit-mode)
         (make-instance 'isometric-walk-mode)))))

(clim:define-command (com-quit :command-table luft-window
                               :name "Quit"
                               :keystroke (#\q :control))
    ()
  (request-viewer-quit (viewer-command-viewer)))

(defparameter +viewer-movement-keys+
  '((:w :forward) (:up :forward)
    (:s :backward) (:down :backward)
    (:a :left) (:left :left)
    (:d :right) (:right :right))
  "Physical keys whose press and release urge the active controller.")

(defun install-viewer-movement-commands ()
  "Install the atelier's held controls into their press and release tables."
  (dolist (binding +viewer-movement-keys+)
    (destructuring-bind (key direction) binding
      (let ((gesture (list key :any)))
        ;; Definition reloads replace the live vocabulary instead of stacking
        ;; another item at the same gesture coordinate.
        (dolist (table '(luft-atelier luft-atelier-release))
          (clim:remove-keystroke-from-command-table
           table gesture :errorp nil))
        (flet ((command-item (command)
                 (let ((direction direction))
                   (lambda (gesture numeric-argument)
                     (declare (ignore gesture numeric-argument))
                     (list command direction)))))
          (clim:add-keystroke-to-command-table
           'luft-atelier gesture :function
           (command-item 'com-start-moving) :errorp nil)
          (clim:add-keystroke-to-command-table
           'luft-atelier-release gesture :function
           (command-item 'com-stop-moving) :errorp nil)))))
  (values))

(install-viewer-movement-commands)

(defgeneric viewer-key-event-tables (event)
  (:documentation "Return the window and atelier tables for key EVENT."))

(defmethod viewer-key-event-tables ((event canvas-key-press-event))
  (values 'luft-window 'luft-atelier))

(defmethod viewer-key-event-tables ((event canvas-key-release-event))
  (values 'luft-window-release 'luft-atelier-release))

(defun viewer-key-command (viewer event)
  "Return the named McCLIM command VIEWER binds to key EVENT, or NIL."
  (multiple-value-bind (window atelier) (viewer-key-event-tables event)
    ;; Window commands are global application controls: fullscreen, Tracy,
    ;; quit, and pointer release remain available after Escape.  Unmodified
    ;; atelier/gameplay commands still require captured input; modified tools
    ;; such as M-x deliberately remain global as well.
    (or (mcluv:canvas-key-event-command
         viewer event :command-table window)
        (when (or (viewer-pointer-captured-p viewer)
                  (viewer-mode-allows-atelier-keys-p (viewer-mode viewer))
                  (intersection '(:control :meta :super)
                                (canvas-key-event-modifiers event)))
          (mcluv:canvas-key-event-command
           viewer event :command-table atelier)))))

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-window-close-request-event))
  (declare (ignore canvas event))
  (request-viewer-quit viewer)
  :defer-canvas-close)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-key-press-event))
  (declare (ignore canvas))
  (unless (canvas-key-event-repeat-p event)
    (let ((command (viewer-key-command viewer event)))
      (when command
        (clim:execute-frame-command viewer command))))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-key-release-event))
  (declare (ignore canvas))
  (let ((command (viewer-key-command viewer event)))
    (when command
      (clim:execute-frame-command viewer command)))
  nil)

(defgeneric handle-viewer-mode-pointer-press (mode viewer canvas event))

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-button-press-event))
  (setf (viewer-pointer-x viewer) (canvas-pointer-event-x event)
        (viewer-pointer-y viewer) (canvas-pointer-event-y event))
  (handle-viewer-mode-pointer-press
   (viewer-mode viewer) viewer canvas event)
  nil)

(defmethod handle-viewer-mode-pointer-press
    ((mode isometric-walk-mode) viewer canvas event)
  (declare (ignore mode canvas))
  (let ((player (viewer-player viewer)))
    (when player
      (case (canvas-pointer-event-button event)
        (:left
         (let ((inspection (update-viewer-inspection viewer)))
           (when inspection
             (let ((site (site-inspection-site inspection))
                   (cell (site-inspection-cell inspection)))
               ;; Only an upward horizontal surface promises a standable
               ;; destination.  Walls remain useful inspection targets but do
               ;; not turn into surprising roof teleports.
               (when (and (= luft:+xy-face-extent+ (luft:site-extent site))
                          (luft:site-positive-p site))
                 (start-walking-player-route
                  player (viewer-source viewer)
                  (luft:site-x cell) (luft:site-y cell)
                  (1+ (luft:site-z cell))))))))
        (:right
         (multiple-value-bind (origin direction) (viewer-pointer-ray viewer)
           (throw-walking-player-ball player origin direction)))))))

(defmethod handle-viewer-mode-pointer-press
    ((mode orbit-mode) viewer canvas event)
  (declare (ignore mode event))
  (unless (viewer-pointer-captured-p viewer)
    (set-canvas-relative-pointer-mode canvas t)
    (setf (viewer-pointer-captured-p viewer) t))
  (when (viewer-player viewer)
    (multiple-value-bind (origin direction) (viewer-pointer-ray viewer)
      (throw-walking-player-ball (viewer-player viewer) origin direction))))

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-wheel-event))
  (declare (ignore canvas))
  (let ((factor (expt 1.10 (- (canvas-pointer-event-scroll-y event)))))
    (if (eq *projection* :isometric)
        (setf *isometric-height*
              (max 6.0 (min 96.0 (* *isometric-height* factor))))
        (let ((camera (viewer-camera viewer)))
          (setf (camera-field-of-view camera)
                (max 0.43633232
                     (min 1.7453293
                          (* (camera-field-of-view camera) factor)))))))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-motion-event))
  (declare (ignore canvas))
  (setf (viewer-pointer-x viewer) (canvas-pointer-event-x event)
        (viewer-pointer-y viewer) (canvas-pointer-event-y event))
  (when (and (typep (viewer-mode viewer) 'orbit-mode)
             (viewer-pointer-captured-p viewer))
    (let ((camera (viewer-camera viewer))
          (sensitivity (viewer-sensitivity viewer)))
      (decf (camera-yaw camera)
            (* (canvas-pointer-event-delta-x event) sensitivity))
      (setf (camera-pitch camera)
            (max -1.5 (min 1.5
                           (- (camera-pitch camera)
                              (* (canvas-pointer-event-delta-y event)
                                 sensitivity)))))))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-window-focus-lost-event))
  (declare (ignore event))
  (clear-viewer-controls viewer)
  (when (viewer-pointer-captured-p viewer)
    (set-canvas-relative-pointer-mode canvas nil)
    (setf (viewer-pointer-captured-p viewer) nil))
  nil)

(defmethod handle-canvas-event ((viewer viewer) canvas event)
  (declare (ignore viewer canvas event))
  nil)

(defun normalized-viewer-fixed-exposure (value)
  "Return VALUE as a positive finite single float, or NIL for adaptation."
  (when value
    (unless (realp value)
      (error "A fixed viewer exposure must be a positive finite real, not ~S."
             value))
    (let ((single (coerce value 'single-float)))
      (unless (and (> single 0.0f0)
                   (= single single)
                   (<= single most-positive-single-float))
        (error "A fixed viewer exposure must be positive and finite, not ~S."
               value))
      single)))

(defun start-viewer (&key
                       (solid (make-mountain-sanctuary-scene))
                       (bevel-width 2)
                       bevel-profile
                       surface-mesh
                       surface-generation
                       fixed-exposure
                       (camera (make-fly-camera))
                       (title "LUFT — click to walk · scroll to zoom · M orbit")
                       (width 1100) (height 800)
                       fullscreen-p
                       (inspector-p nil)
                       (frames-per-second 60)
                       (provider *gpu-provider*))
  "Open the indexed-instanced LUFT renderer as a McCLIM atelier.

BEVEL-PROFILE enables one compiled material-selected site policy in static and
streaming scenes. BEVEL-WIDTH remains the camera/inspection reference and the
uniform fallback. SURFACE-MESH supplies an already constructed diagnostic mesh
while retaining SOLID as the semantic inspection source. SURFACE-GENERATION,
when supplied with that mesh, preserves its exact immutable realized-light
cohort. FIXED-EXPOSURE disables temporal adaptation for reproducible evidence."
  (check-type solid scene)
  (setf fixed-exposure (normalized-viewer-fixed-exposure fixed-exposure))
  (when (and surface-generation (null surface-mesh))
    (error "SURFACE-GENERATION is only meaningful with SURFACE-MESH."))
  (when surface-mesh
    (check-type surface-mesh luft:surface-mesh)
    (when bevel-profile
      (error "Specify either SURFACE-MESH or BEVEL-PROFILE, not both.")))
  (when surface-generation
    (check-type surface-generation scene-mesh-generation)
    (unless (eq solid (scene-mesh-generation-scene surface-generation))
      (error "SURFACE-GENERATION belongs to a different semantic scene.")))
  (let ((canvas
          (make-sdl-canvas
           :title title :width width :height height :visible-p nil
           :fullscreen-p fullscreen-p
           :high-pixel-density-p t
           :presentation-api (sdl-presentation-api-for provider)))
        (device nil)
        (renderer nil)
        (viewer-state nil)
        (renderer-source-values nil)
        (renderer-source-revision 0)
        (production-system nil)
        (completed-p nil))
    (open-canvas canvas)
    (unwind-protect-releasing
         (let* ((device*
                  (setf device
                        (request-gpu-device
                         provider (make-device-descriptor :label title))))
                (context
                  (make-canvas-context
                   canvas provider
                   ;; :copy-src drops the Metal framebuffer-only contract so
                   ;; CAPTURE-VIEWER-FRAME can read the drawable back.
                   (make-canvas-configuration
                    :device device*
                    :usage '(:render-attachment :copy-src))))
                (renderer*
                  (multiple-value-bind
                      (created source-values before after)
                      (make-tracked-renderer
                       device* (canvas-format context) (canvas-extent context))
                    ;; If source moved during construction, BEFORE deliberately
                    ;; remains the installed attempt: the first frame sees the
                    ;; newer AFTER revision and transactionally rebuilds.
                    (declare (ignore after))
                    (setf renderer created
                          renderer-source-values source-values
                          renderer-source-revision before)
                    created))
                (port (clim:find-port :server-path '(:luv-gpu)))
                (manager
                  (or (first (clim-internals::frame-managers port))
                      (make-instance 'mcluv:luv-frame-manager :port port)))
                (viewer
                  (setf viewer-state
                        (let ((mcluv:*embedded-mirror-target* canvas)
                              (mcluv:*embedded-mirror-context* context)
                              (mcluv:*embedded-mirror-device* device*))
                          (when (and (typep solid 'streaming-scene)
                                     (null surface-mesh))
                            (reset-streaming-scene-publication solid)
                            (setf production-system
                                  (production:make-single-worker-production-system
                                   :name "LUFT mesh producer")))
                          (clim:make-application-frame
                           'viewer :frame-manager manager :enable t
                                   :canvas canvas :context context
                                   :device device* :renderer renderer*
                                   :stop-controller
                                   (make-canvas-stop-controller
                                    canvas :name "LUFT viewer")
                                   :production-system production-system
                                   :camera camera :source solid
                                   :player (and (typep solid 'scene)
                                                (scene-player-p solid)
                                                (make-walking-player))
                                   :bevel-width bevel-width
                                   :bevel-profile bevel-profile
                                   :fixed-exposure fixed-exposure
                                   :inspector-p inspector-p)))))
           (cond
             (surface-mesh
              (renderer-set-mesh
               renderer* 0 surface-mesh
               :scene-generation surface-generation)
              ;; A synchronous streaming snapshot remains a static diagnostic
              ;; until a later explicit refresh.  Only successful renderer
              ;; publication makes its exact field reusable by the view.
              (when (and surface-generation
                         (typep solid 'streaming-scene))
                (setf (streaming-scene-light-generation solid)
                      (scene-mesh-generation-light-generation
                       surface-generation))))
             ((typep solid 'streaming-scene))
             (bevel-profile
              (multiple-value-bind (meshes generation)
                  (make-material-bevel-meshes solid bevel-profile)
                (renderer-set-meshes
                 renderer* meshes :scene-generation generation)))
             (t
              (multiple-value-bind (mesh generation)
                  (make-render-mesh solid :bevel-width bevel-width)
                (renderer-set-mesh
                 renderer* 0 mesh :scene-generation generation))))
           (attach-viewer-live-artifact
            viewer renderer-source-values renderer-source-revision)
           (setf (canvas-event-handler canvas) viewer)
           (when inspector-p
             (refresh-viewer-inspector viewer)
             (let* ((mirror (viewer-inspector-mirror viewer))
                    (compositor
                      (make-instance 'mcluv:direct-gpu-mirror-compositor
                                     :mirror mirror)))
               (setf (mcluv:mirror-compositor mirror) compositor
                     (viewer-inspector-compositor viewer) compositor)
               ;; Realization painted before the compositor existed; publish
               ;; only the inspector pane for direct final-pass replay.
               (refresh-viewer-inspector viewer)))
           ;; The radio is application infrastructure and remains active while
           ;; its old detailed panel is hidden.  The compact shared status line
           ;; is the default visible representation.
           (attach-viewer-lobby viewer)
           (open-viewer-status-bar viewer)
           (request-canvas-frame
            canvas (lambda (timestamp) (render-viewer-frame viewer timestamp)))
           (show-canvas canvas)
           (setf (canvas-clock canvas)
                 (make-cadence-clock
                  (lambda (native-canvas timestamp)
                    (declare (ignore native-canvas))
                    (render-viewer-frame viewer timestamp))
                  :frames-per-second frames-per-second))
           (setf *viewer* viewer
                 completed-p t)
           viewer)
      (unless completed-p
        (when viewer-state
          (releasing :instruments
            (release-viewer-instruments viewer-state)))
        (when production-system
          (releasing :production-system
            (production:stop-production-system production-system)))
        (if (and viewer-state (viewer-live-artifact viewer-state))
            (releasing :renderer-artifact
              (release-viewer-live-artifact viewer-state))
            (when renderer
              (releasing :renderer (destroy-renderer renderer))))
        (when (eq :open (canvas-state canvas))
          (releasing :canvas (close-canvas canvas)))
        (when device (releasing :device (destroy device)))))))

(defmethod luv:capture-canvas ((viewer viewer))
  (viewer-canvas viewer))

(defmethod luv:prepare-capture
    ((viewer viewer) (capture luv:application-capture))
  (when (eq :film (luv:capture-kind capture))
    (setf (luv:capture-client-state capture)
          (list :running-p (viewer-running-p viewer))
          (viewer-running-p viewer) nil))
  viewer)

(defmethod luv:advance-capture-frame
    ((viewer viewer) (capture luv:application-capture) frame-index)
  (declare (ignore capture frame-index))
  (advance-viewer-streaming viewer))

(defmethod luv:encode-capture-frame
    ((viewer viewer) (capture luv:application-capture)
     encoder target extent)
  (encode-viewer-frame
   viewer encoder target extent
   :inspector-p
   (luv:capture-option
    capture :inspector-p
    (and (eq :screenshot (luv:capture-kind capture))
         (viewer-inspector-p viewer)))))

(defmethod luv:cleanup-capture
    ((viewer viewer) (capture luv:application-capture))
  (let ((target (luv:capture-target capture))
        (canvas (viewer-canvas viewer))
        (saved-state (luv:capture-client-state capture)))
    (unwind-protect
         (when (and target (eq :open (canvas-state canvas)))
           (request-canvas-frame
            canvas
            (lambda (timestamp)
              (declare (ignore timestamp))
              ;; The shared target is still alive.  Evict only its cached
              ;; view; normal drawable views remain warm.
              (release-viewer-surface-view viewer target))))
      (when (and (eq :film (luv:capture-kind capture)) saved-state)
        ;; Test and publication share the capture gate lock.  If cleanup wins,
        ;; a following stop request writes NIL afterward; if shutdown wins,
        ;; cleanup cannot resurrect a terminal viewer.
        (call-if-application-captures-open
         viewer
         (lambda ()
           (setf (viewer-running-p viewer)
                 (getf saved-state :running-p)))))))
  (values))

(defun capture-viewer-frame
    (pathname &optional (viewer *viewer*)
     &key (inspector-p (viewer-inspector-p viewer)))
  "Render one native-resolution VIEWER frame offscreen into PATHNAME.

INSPECTOR-P defaults to VIEWER's inspector setting.  A source-defined
evidence capture may override it when the subject is the geometry rather than
the atelier UI."
  (luv:capture-application-screenshot
   viewer (merge-pathnames pathname)
   :label "LUFT screenshot"
   :options (list :inspector-p inspector-p)))

(defun film-viewer (viewer pathname
                    &key (seconds 8) (frame-rate 30) before-frame)
  "Film VIEWER offscreen into an MP4 at PATHNAME.

BEFORE-FRAME, when supplied, receives the frame index before streaming and
rendering that frame.  Recording is paced in real time so asynchronous chunk
production gets the same opportunity to publish as it does in the window."
  (luv:capture-application-film
   viewer pathname
   :seconds seconds
   :frame-rate frame-rate
   :before-frame before-frame
   :progress-function
   (lambda (frame frame-count)
     (when (zerop (mod frame frame-rate))
       (format t "LUFT film: frame ~D / ~D~%" frame frame-count)
       (force-output)))
   :label "LUFT film"
   :options '(:inspector-p nil)))

(defun refresh-viewer-renderer (&optional (viewer *viewer*)
                                &key (solid (make-mountain-sanctuary-scene))
                                     (bevel-width
                                       (and viewer
                                            (viewer-bevel-width viewer)))
                                     (bevel-profile
                                       (and viewer
                                            (viewer-bevel-profile viewer))))
  "Rebuild VIEWER at BEVEL-WIDTH or its material BEVEL-PROFILE."
  (when viewer
    (luv::call-on-sdl-canvas-thread
     (viewer-canvas viewer)
     (lambda ()
       (let* ((bevel-width (or bevel-width (viewer-bevel-width viewer)))
              (context (viewer-context viewer))
              (old (viewer-renderer viewer))
              (old-production-system (viewer-production-system viewer))
              (candidate-renderer nil)
              (candidate-source-values nil)
              (candidate-source-revision 0)
              (production-system nil)
              (was-running-p (viewer-running-p viewer)))
         (setf (viewer-running-p viewer) nil)
         (unwind-protect
              (handler-case
                  (let ((renderer
                          (multiple-value-bind
                              (created source-values before after)
                              (make-tracked-renderer
                               (viewer-device viewer)
                               (canvas-format context)
                               (canvas-extent context))
                            (unless (= before after)
                              (when created (destroy-renderer created))
                              (error 'renderer-source-changed-during-build
                                     :before before :after after))
                            (setf candidate-renderer created
                                  candidate-source-values source-values
                                  candidate-source-revision before)
                            created)))
                    (cond
                      ((typep solid 'streaming-scene)
                       (setf production-system
                             (production:make-single-worker-production-system
                              :name "LUFT mesh producer")))
                      (bevel-profile
                       (multiple-value-bind (meshes generation)
                           (make-material-bevel-meshes solid bevel-profile)
                         (renderer-set-meshes
                          renderer meshes :scene-generation generation)))
                      (t
                       (multiple-value-bind (mesh generation)
                           (make-render-mesh solid :bevel-width bevel-width)
                         (renderer-set-mesh
                          renderer 0 mesh :scene-generation generation))))
                    (when (typep solid 'streaming-scene)
                      (reset-streaming-scene-publication solid))
                    (setf (viewer-renderer viewer) renderer
                          (viewer-production-system viewer) production-system
                          (viewer-source viewer) solid
                          (viewer-player viewer)
                          (and (typep solid 'scene) (scene-player-p solid)
                               (or (viewer-player viewer)
                                   (make-walking-player)))
                          (viewer-bevel-width viewer) bevel-width
                          (viewer-bevel-profile viewer) bevel-profile
                          (viewer-inspection viewer) nil
                          candidate-renderer nil)
                    (note-viewer-renderer-replacement
                     viewer candidate-source-values
                     candidate-source-revision))
                (error (condition)
                  (when candidate-renderer
                    (destroy-renderer candidate-renderer))
                  (when production-system
                    (production:stop-production-system production-system))
                  (error condition)))
           (setf (viewer-running-p viewer) was-running-p))
         (when old-production-system
           (production:stop-production-system old-production-system))
         (when old (destroy-renderer old))))))
  (values))

(defun refresh-viewer-shaders (&optional (viewer *viewer*))
  "Force a complete renderer-cohort rebuild on VIEWER's canvas thread."
  (when viewer
    (luv::call-on-sdl-canvas-thread
     (viewer-canvas viewer)
     (lambda () (force-viewer-live-artifact-refresh viewer))))
  (values))

(defun %perform-viewer-stop (viewer)
  "Release VIEWER after its stop controller granted this caller ownership."
  ;; A native request only closed admission.  This sole off-canvas owner waits
  ;; for the active capture to encode and evict its surface view before any
  ;; renderer, canvas, or device release can begin.
  (quiesce-application-captures viewer)
  (setf (viewer-running-p viewer) nil)
  (let ((canvas (viewer-canvas viewer)))
    (unwind-protect-releasing
        (with-release-report
          (releasing :controls (clear-viewer-controls viewer))
          (when (member (canvas-state canvas) '(:opening :open))
            (releasing :clock
              (setf (canvas-clock canvas) (make-demand-clock)))
            (when (viewer-pointer-captured-p viewer)
              (releasing :pointer-capture
                (set-canvas-relative-pointer-mode canvas nil)
                (setf (viewer-pointer-captured-p viewer) nil)))
            (releasing :canvas-quiescence
              ;; A synchronous no-op after changing the clock is the frame
              ;; boundary: no encoder still borrows application resources.
              (request-canvas-frame
               canvas (lambda (timestamp) (declare (ignore timestamp))))))
          (releasing :event-handler
            (setf (canvas-event-handler canvas) nil))
          (releasing :instruments (release-viewer-instruments viewer))
          (when (viewer-production-system viewer)
            (releasing :production-system
              (production:stop-production-system
               (viewer-production-system viewer))
              (setf (viewer-production-system viewer) nil)))
          (releasing :surface-views (release-viewer-surface-views viewer))
          ;; The live artifact is the renderer cohort's sole owner.  It
          ;; detaches the renderer before native retirement and makes release
          ;; terminal even when the backend reports a retirement failure.
          (releasing :renderer-artifact
            (release-viewer-live-artifact viewer))
          (unless (eq :disowned (clim:frame-state viewer))
            (releasing :inspector-frame (clim:destroy-frame viewer)))
          ;; Native window and GPU device are deliberately the final handles.
          (when (member (canvas-state canvas) '(:opening :open))
            (releasing :canvas (close-canvas canvas)))
          (when (viewer-device viewer)
            (releasing :device (destroy (viewer-device viewer)))))
      (releasing :viewer-publication
        (when (eq viewer *viewer*) (setf *viewer* nil)))))
  (values))

(defun stop-viewer (&optional (viewer *viewer*))
  "Stop VIEWER exactly once and publish one result to every caller.

Native canvas-thread handlers use REQUEST-VIEWER-QUIT instead: a synchronous
  owner or waiter is rejected there because teardown crosses a frame boundary."
  (when viewer
    (request-application-capture-shutdown viewer)
    (setf (viewer-running-p viewer) nil)
    (call-with-stop-controller
     (viewer-stop-controller viewer)
     (lambda () (%perform-viewer-stop viewer))))
  (values))

(defun run-standalone-viewer ()
  (let ((viewer (start-viewer)))
    (unwind-protect
         (loop while (viewer-running-p viewer) do (sleep 0.05))
      (stop-viewer viewer))))
