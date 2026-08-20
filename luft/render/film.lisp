;;; Films of the studio: an orbiting camera rendered headlessly, frame by
;;; frame, into an MP4 through LUV:WITH-VIDEO-ENCODER.
;;;
;;; Motion is what the temporal resolve was built for, and an orbit shows
;;; both of its regimes: subpixel accumulation while the picture coheres,
;;; and history rejection whenever the style or scene is swapped mid-film.

(in-package #:luft.render)

(defun film-studio-orbit (pathname
                          &key (seconds 8) (frame-rate 30)
                               (width 960) (height 540)
                               (style :stock)
                               (styles (list style))
                               (effects (default-renderer-effects))
                               (scene (make-studio-scene))
                               (center-x 16.0) (center-y 14.0) (center-z 2.5)
                               (radius 16.0) (camera-height 9.5)
                               (field-of-view 0.85)
                               (turns 1.0))
  "Film one orbit of the studio into an MP4 at PATHNAME.

The camera circles CENTER at RADIUS and CAMERA-HEIGHT through TURNS
revolutions over SECONDS.  STYLES is the list of surface styles the film
cycles through, each getting an equal arc; a single-element list holds one
style throughout.  Returns PATHNAME and the frame count."
  (let* ((frame-count (max 1 (round (* seconds frame-rate))))
         (renderer (make-renderer :scene scene
                                  :camera (studio-camera
                                           (+ center-x radius) center-y
                                           camera-height
                                           :look-x center-x :look-y center-y
                                           :look-z center-z
                                           :field-of-view field-of-view)
                                  :width width :height height
                                  :style (first styles)
                                  :pipeline-styles styles
                                  :effects effects)))
    (unwind-protect
         (with-video-encoder (write-frame pathname width height
                              :frame-rate frame-rate
                              :format (renderer-color-format renderer))
           (dotimes (frame frame-count)
             (let* ((progress (/ (float frame 1.0) frame-count))
                    (angle (* 2.0 pi turns progress))
                    (style (nth (min (1- (length styles))
                                     (floor (* progress (length styles))))
                                styles)))
               (setf (renderer-style renderer) style
                     (renderer-camera renderer)
                     (studio-camera (+ center-x (* radius (cos angle)))
                                    (+ center-y (* radius (sin angle)))
                                    camera-height
                                    :look-x center-x :look-y center-y
                                    :look-z center-z
                                    :field-of-view field-of-view))
               (write-frame (render-pixels renderer)))))
      (destroy-renderer renderer))))

(defun film-clay-breath (pathname
                         &key (pieces '(:grotto :holm))
                              (seconds-per-shot 9) (frame-rate 30)
                              (width 720) (height 1280)
                              (light :golden)
                              (aperture 0.85)
                              (field-scale 1.25)
                              (effects (default-renderer-effects)))
  "Fly the atelier's architecture in :CLAY while the world breathes.

Each of PIECES gets the same two shots the drone films cut together -- its
authored aerial flyover, then its close route, both relaxed off the
masonry and sworn collision-free -- while the cell radius and the melt
reach ride two incommensurate sine waves over the whole reel, so the same
walls pass continuously through tight quilting, pillowed masonry, full
pearls, and deep bridging melt.  The clay surface never swells more than a
quarter of the melt past its cells, well inside the flights' sworn
clearance.

The frame is a phone held upright, the field of view widened a quarter
for the tall frame, the lens open for the tilt-shift focus at the centre
of frame, and the temporal resolve accumulates across the breath under
*TEMPORAL-SURFACE-DRIFT-P*: the knobs move the surface far less than a
pixel per frame, which is the drift accumulation exists for.  Returns
PATHNAME and the frame count."
  (let* ((*light* light)
         (*aperture* aperture)
         (*temporal-surface-drift-p* t)
         (shot-frames (max 1 (round (* seconds-per-shot frame-rate))))
         (scenes (mapcar (lambda (piece) (cons piece (atelier-scene piece)))
                         (remove-duplicates pieces)))
         (shots (loop for piece in pieces
                      append (list (list piece (atelier-flyover piece))
                                   (list piece (atelier-flight piece)))))
         (shot-count (length shots))
         (total-frames (* shot-count shot-frames))
         (frame-index 0)
         (renderer (make-renderer
                    :scene (cdr (first scenes))
                    :camera (cdr (first (atelier-cameras (first pieces))))
                    :width width :height height
                    :style :clay :pipeline-styles '(:clay)
                    :effects effects)))
    (unwind-protect
         (with-video-encoder (write-frame pathname width height
                              :frame-rate frame-rate
                              :format (renderer-color-format renderer))
           (loop for (piece waypoints) in shots
                 for shot from 0
                 do (let* ((scene (cdr (assoc piece scenes)))
                           (positions (mapcar #'first waypoints))
                           (looks (mapcar #'second waypoints))
                           (fields (mapcar (lambda (waypoint)
                                             (list (third waypoint)))
                                           waypoints))
                           (flight
                             (coerce
                              (assert-flight-clear
                               piece scene
                               (clear-flight-path
                                scene
                                (loop for frame below shot-frames
                                      for s = (/ (+ frame 0.5) shot-frames)
                                      collect (catmull-rom-sample
                                               positions
                                               (flight-shot-progress
                                                s shot shot-count)))))
                              'vector)))
                      (format t "~&clay breath: ~(~A~) shot ~D/~D~%"
                              piece (1+ shot) shot-count)
                      (finish-output)
                      (setf (renderer-scene renderer) scene)
                      (dotimes (frame shot-frames)
                        (let* ((s (/ (+ frame 0.5) shot-frames))
                               (eased (flight-shot-progress
                                       s shot shot-count))
                               (position (aref flight frame))
                               (look (catmull-rom-sample looks eased))
                               (field (* field-scale
                                         (first (catmull-rom-sample
                                                 fields eased))))
                               (reel (/ (float frame-index 1.0)
                                        total-frames))
                               ;; Two incommensurate breaths through the
                               ;; knob plane, over the whole reel.
                               (*clay-radius*
                                 (+ 0.31 (* 0.17 (sin (* 2.0 pi 3.0
                                                         reel)))))
                               (*clay-melt*
                                 (+ 0.19 (* 0.14 (sin (+ (* 2.0 pi 2.0
                                                            reel)
                                                         1.1))))))
                          (setf (renderer-camera renderer)
                                (studio-camera
                                 (first position) (second position)
                                 (third position)
                                 :look-x (first look) :look-y (second look)
                                 :look-z (third look)
                                 :field-of-view field))
                          (write-frame (render-pixels renderer))
                          (incf frame-index))))))
      (destroy-renderer renderer))))

(defun catmull-rom-sample (points s)
  "Sample the Catmull-Rom spline through POINTS at S in [0,1].

POINTS is a list of same-length lists of numbers; endpoints are clamped,
so the path passes through every point and starts and ends at rest."
  (let* ((count (length points))
         (segments (max 1 (1- count)))
         (u (* (min (max s 0.0) 1.0) segments))
         (index (min (floor u) (1- segments)))
         (v (- u index)))
    (flet ((point (i) (elt points (min (max i 0) (1- count)))))
      (mapcar (lambda (a b c d)
                (* 0.5
                   (+ (* 2.0 b)
                      (* v (- c a))
                      (* v v (+ (* 2.0 a) (* -5.0 b) (* 4.0 c) (- d)))
                      (* v v v (+ (- a) (* 3.0 b) (* -3.0 c) d)))))
              (point (1- index)) (point index)
              (point (1+ index)) (point (+ index 2))))))

;;; ------------------------------------------------------------------------
;;; Construction films: the boundary changes while the camera flies

(defun copy-scene-slots (scene)
  (and (scene-slots scene) (copy-seq (scene-slots scene))))

(defun construction-cell-score (mode cell domain ceiling)
  "The authored reveal order of CELL for construction MODE."
  (let* ((x (float (luft:site-x cell) 1.0))
         (y (float (luft:site-y cell) 1.0))
         (z (float (luft:site-z cell) 1.0))
         (cx (* 0.5 (luft:world-domain-x-period domain)))
         (cy (* 0.5 (luft:world-domain-y-period domain)))
         (dx (- x cx))
         (dy (- y cy))
         (radius (sqrt (+ (* dx dx) (* dy dy))))
         (angle (mod (+ (atan dy dx) (* 2.0 pi)) (* 2.0 pi))))
    (ecase mode
      (:rise
       ;; A slightly rippled horizontal front makes columns arrive together
       ;; without making the whole level pop on one frame.
       (+ z (* 0.22 (sin (+ (* 0.31 x) (* 0.19 y))))
          (* 0.002 (+ x (* 3.0 y)))))
      (:spiral
       ;; One turn winds outward and upward: angle is the broad ordering,
       ;; radius and height keep the wave from becoming a flat radial wipe.
       (+ angle (* 0.045 radius) (* 0.065 z)))
      (:carve
       ;; Subtraction begins at the far, high corners of the sacrificial
       ;; block and closes on the architecture near its heart.
       (- (+ (* dx dx) (* dy dy)
             (* 1.7 (expt (- z (* 0.45 ceiling)) 2))))))))

(defun target-construction (target mode)
  "Return MODE's initial scene and signed cells which turn it into TARGET."
  (let* ((domain (scene-domain target))
         (target-cells (luft:chain-sites (scene-solid target)))
         (ceiling (reduce #'max target-cells :key #'luft:site-z)))
    (ecase mode
      ((:rise :spiral)
       (values (make-scene domain
                           :slots (copy-scene-slots target)
                           :stocks (copy-seq (scene-stocks target)))
               (copy-seq target-cells)
               ceiling))
      (:carve
       (let* ((solid (luft:make-solid-chain domain))
              (slots (copy-scene-slots target))
              (stocks (copy-seq (scene-stocks target)))
              (stone-slot (or (position :granite stocks) 0))
              (margin
                (max 2
                     (floor
                      (min (luft:world-domain-x-period domain)
                           (luft:world-domain-y-period domain))
                      12)))
              (x0 margin)
              (x1 (- (luft:world-domain-x-period domain) margin 1))
              (y0 margin)
              (y1 (- (luft:world-domain-y-period domain) margin 1))
              (z1 (min (- luft:+vertical-cell-rows+ 2) (+ ceiling 2)))
              (edits (make-array 1024 :element-type 'luft:site
                                      :adjustable t :fill-pointer 0)))
         ;; The final world is already within the initial one.  The added
         ;; granite prism is sacrificial stock; its negative cells alone are
         ;; the edit chain which reveals the architecture.
         (luft:add-chain solid (scene-solid target))
         (loop for x from x0 to x1
               do (loop for y from y0 to y1
                        do (loop for z from 0 to z1
                                 unless (luft:solid-cell-p
                                         (scene-solid target) x y z)
                                   do (let ((cell
                                             (luft:make-site
                                              domain x y z luft:+cell-extent+)))
                                        (luft:add-chain-site solid cell)
                                        (when slots
                                          (setf (aref slots
                                                      (luft:cell-bit-index
                                                       domain x y z))
                                                stone-slot))
                                        (vector-push-extend
                                         (luft:opposite-site cell) edits)))))
         (values (make-scene domain :solid solid :slots slots :stocks stocks)
                 edits z1))))))

(defun construction-camera (domain ceiling progress)
  "A high, collision-free orbit which watches the whole edit field."
  (let* ((cx (* 0.5 (luft:world-domain-x-period domain)))
         (cy (* 0.5 (luft:world-domain-y-period domain)))
         (radius (* 0.43 (min (luft:world-domain-x-period domain)
                              (luft:world-domain-y-period domain))))
         (angle (+ -1.15 (* progress 2.35 pi)))
         (height (+ ceiling 16.0 (* 3.0 (cos (* progress 2.0 pi))))))
    (studio-camera (+ cx (* radius (cos angle)))
                   (+ cy (* radius (sin angle)))
                   height
                   :look-x cx :look-y cy
                   :look-z (* 0.42 ceiling)
                   :field-of-view 0.82)))

(defun construction-finished-p (scene target)
  (equalp (luft:chain-sites (scene-solid scene))
          (luft:chain-sites (scene-solid target))))

(defun film-atelier-construction
    (pathname &key (piece :holm) (mode :rise)
                   (seconds 10) (frame-rate 24)
                   (width 960) (height 540)
                   (style :stock)
                   (chamfer-width (/ 1.0 6.0))
                   (light :evening)
                   (aperture 0.35)
                   (effects (default-renderer-effects)))
  "Film PIECE becoming itself through exact incremental chain edits.

MODE is :RISE for a bottom-up front, :SPIRAL for a winding assembly, or
:CARVE for subtraction from a sacrificial granite prism.  Every frame batches
signed cells into one APPLY-SCENE-EDIT publication; the renderer uploads only
the changed face pages and occupancy words.  Returns PATHNAME, the frame count,
and the total incremental bytes uploaded."
  (let* ((*chamfer-width* chamfer-width)
         (*light* light)
         (*aperture* aperture)
         (target (atelier-scene piece)))
    (multiple-value-bind (scene edits ceiling)
        (target-construction target mode)
      (let* ((ordered (stable-sort
                       (copy-seq edits) #'<
                       :key (lambda (cell)
                              (construction-cell-score
                               mode cell (scene-domain scene) ceiling))))
             (frame-count (max 2 (round (* seconds frame-rate))))
             (renderer nil)
             (cursor 0)
             (uploaded-bytes 0)
             (uploaded-writes 0))
        ;; Assign all pages before GPU creation.  No later frame has to replace
        ;; the sites buffer merely because its edit wave reached a new chunk.
        (reserve-scene-edit-pages scene ordered)
        (setf renderer
              (make-renderer
               :scene scene
               :camera (construction-camera (scene-domain scene) ceiling 0.0)
               :width width :height height
               :style style :pipeline-styles (list style)
               :effects effects))
        (unwind-protect
             (with-video-encoder (write-frame pathname width height
                                  :frame-rate frame-rate
                                  :format (renderer-color-format renderer))
               (dotimes (frame frame-count)
                 (let* ((progress (/ (float frame 1.0) (1- frame-count)))
                        (edit-progress
                          (min 1.0 (max 0.0 (/ (- progress 0.06) 0.86))))
                        (eased (* edit-progress edit-progress
                                  (- 3.0 (* 2.0 edit-progress))))
                        (wanted (if (= frame (1- frame-count))
                                    (length ordered)
                                    (floor (* eased (length ordered)))))
                        (revision (scene-revision scene)))
                   (when (< cursor wanted)
                     (let ((edit (luft:make-chain (scene-domain scene))))
                       (loop for index from cursor below wanted
                             do (luft:add-chain-site edit (aref ordered index)))
                       (apply-scene-edit scene edit)
                       (setf cursor wanted)))
                   (setf (renderer-camera renderer)
                         (construction-camera
                          (scene-domain scene) ceiling progress))
                   (write-frame (render-pixels renderer))
                   (unless (= revision (scene-revision scene))
                     (incf uploaded-bytes
                           (renderer-last-scene-upload-bytes renderer))
                     (incf uploaded-writes
                           (renderer-last-scene-upload-writes renderer)))
                   (when (or (zerop (mod frame (max 1 frame-rate)))
                             (= frame (1- frame-count)))
                     (format t "~&~(~A~) ~A: frame ~D/~D, cells ~D/~D, ~
                                faces ~D, last upload ~:D bytes.~%"
                             mode piece (1+ frame) frame-count cursor
                             (length ordered)
                             (luft:chain-count (scene-surface scene))
                             (renderer-last-scene-upload-bytes renderer))
                     (finish-output)))))
          (when renderer (destroy-renderer renderer)))
        (unless (construction-finished-p scene target)
          (error "The ~S ~S film ended before reconstructing its target."
                 piece mode))
        (format t "~&~S ~S complete: ~:D incremental bytes in ~:D writes.~%"
                piece mode uploaded-bytes uploaded-writes)
        (values pathname frame-count uploaded-bytes)))))

(defun flight-cell-solid-p (scene x y z)
  "Whether the flight would be inside SCENE's masonry at cell X,Y,Z.

Below the world's floor counts as solid -- a drone does not tunnel --
and above its ceiling counts as sky."
  (cond ((< z 0) t)
        ((> z (- luft:+vertical-cell-rows+ 2)) nil)
        (t (luft:solid-cell-p (scene-solid scene) x y z))))

(defun flight-clearance-push (scene point radius)
  "The push moving POINT away from solid cells within RADIUS, or NIL."
  (let ((px (first point)) (py (second point)) (pz (third point))
        (push-x 0.0) (push-y 0.0) (push-z 0.0)
        (crowded-p nil))
    (loop for x from (floor (- px radius)) to (floor (+ px radius))
          do (loop for y from (floor (- py radius)) to (floor (+ py radius))
                   do (loop for z from (floor (- pz radius))
                              to (floor (+ pz radius))
                            when (flight-cell-solid-p scene x y z)
                              do (let* ((dx (- px (+ x 0.5)))
                                        (dy (- py (+ y 0.5)))
                                        (dz (- pz (+ z 0.5)))
                                        (distance
                                          (max (sqrt (+ (* dx dx) (* dy dy)
                                                        (* dz dz)))
                                               0.001)))
                                   (when (< distance radius)
                                     (setf crowded-p t)
                                     (let ((weight (/ (- radius distance)
                                                      distance)))
                                       (incf push-x (* weight dx))
                                       (incf push-y (* weight dy))
                                       (incf push-z (* weight dz))))))))
    (and crowded-p (list push-x push-y push-z))))

(defun flight-nearest-opening (scene point &key (reach 8))
  "The centre of the nearest empty cell within REACH of POINT, or NIL."
  (let* ((px (first point)) (py (second point)) (pz (third point))
         (cx (floor px)) (cy (floor py)) (cz (floor pz))
         (best nil)
         (best-distance nil))
    (loop for x from (- cx reach) to (+ cx reach)
          do (loop for y from (- cy reach) to (+ cy reach)
                   do (loop for z from (- cz reach) to (+ cz reach)
                            unless (flight-cell-solid-p scene x y z)
                              do (let* ((dx (- (+ x 0.5) px))
                                        (dy (- (+ y 0.5) py))
                                        (dz (- (+ z 0.5) pz))
                                        (distance (+ (* dx dx) (* dy dy)
                                                     (* dz dz))))
                                   (when (or (null best-distance)
                                             (< distance best-distance))
                                     (setf best-distance distance
                                           best (list (+ x 0.5) (+ y 0.5)
                                                      (+ z 0.5))))))))
    best))

(defun clear-flight-path (scene points
                          &key (radius 1.5) (passes 80) (step 0.25)
                               (smoothing 0.12))
  "Relax POINTS away from SCENE's masonry into a flyable path.

A point inside the masonry heads straight for the nearest opening --
deep in a wall the clearance pushes cancel, so escape is its own move.
Each pass then pushes every crowded point out of its clearance RADIUS by
STEP of the accumulated push, and rounds the dodge back into an arc with
a touch of Laplacian SMOOTHING on the interior.  Even the authored
endpoints move when crowded: a view is negotiable, a collision is not.
Returns the relaxed list."
  (let ((points (mapcar #'copy-list points)))
    (dotimes (pass passes points)
      (dolist (point points)
        (if (flight-cell-solid-p scene
                                 (floor (first point))
                                 (floor (second point))
                                 (floor (third point)))
            (let ((opening (flight-nearest-opening scene point)))
              (when opening
                (loop for axis below 3
                      do (incf (elt point axis)
                               (* 0.5 (- (elt opening axis)
                                         (elt point axis)))))))
            (let ((push (flight-clearance-push scene point radius)))
              (when push
                (incf (first point) (* step (first push)))
                (incf (second point) (* step (second push)))
                (incf (third point) (* step (third push)))))))
      (loop for (previous current next) on points
            while next
            do (loop for axis below 3
                     do (setf (elt current axis)
                              (+ (* (- 1.0 smoothing) (elt current axis))
                                 (* smoothing
                                    (* 0.5 (+ (elt previous axis)
                                              (elt next axis)))))))))))

(defun assert-flight-clear (piece scene points &key (clearance 0.6))
  "Error unless every flight sample in POINTS stays clear of the masonry.

Passing through a voxel is not a quality problem, it is a wrong film:
a sample inside a solid cell, or with a solid cell's centre within
CLEARANCE, names the piece and the sample and refuses to fly."
  (loop for point in points
        for index from 0
        do (when (flight-cell-solid-p scene
                                      (floor (first point))
                                      (floor (second point))
                                      (floor (third point)))
             (error "The ~S flight is inside masonry at sample ~D, ~
                     ~,2F ~,2F ~,2F."
                    piece index (first point) (second point) (third point)))
           (when (flight-clearance-push scene point clearance)
             (error "The ~S flight grazes masonry at sample ~D, ~
                     ~,2F ~,2F ~,2F: solid within ~,2F."
                    piece index (first point) (second point) (third point)
                    clearance)))
  points)

(defun flight-shot-progress (s shot shot-count)
  "SHOT's eased parameter at linear S: momentum survives every cut.

Only the reel's very first shot eases in and only its very last eases
out; every interior cut lands with both shots moving, the way a cut on
action does.  The edge easings end and begin at unit speed."
  (cond ((and (zerop shot) (= shot-count 1))
         (* s s (- 3.0 (* 2.0 s))))
        ((zerop shot) (* s s (- 2.0 s)))
        ((= shot (1- shot-count))
         (let ((tail (- 1.0 s)))
           (- 1.0 (* tail tail (- 2.0 tail)))))
        (t s)))

(defun film-atelier-flight (pathname
                            &key (pieces '(:arcade :viaduct :turret
                                           :grotto :headland :holm))
                                 (seconds-per-shot 7) (frame-rate 30)
                                 (width 1280) (height 720)
                                 (style :stock)
                                 (chamfer-width (/ 1.0 6.0))
                                 (light :evening)
                                 (aperture 0.85)
                                 (effects (default-renderer-effects))
                                 (field-scale 1.0))
  "Fly a drone through the atelier's architecture into an MP4 at PATHNAME.

Each of PIECES gets two shots cut together: a distant aerial pass over
its authored views (ATELIER-FLYOVER), then its close authored route
(ATELIER-FLIGHT) -- through the arches, along the decks, around the
shafts.  Every flight is a Catmull-Rom spline relaxed off the masonry
and sworn collision-free, and the easing bookends the whole reel rather
than each shot, so a cut never resets the drone's speed.  STYLE defaults
to :STOCK -- the chamfered multi-material style the wiki's stills use --
with CHAMFER-WIDTH of a sixth of a cell, wide enough that every arris
reads as a planed band.  FIELD-SCALE widens every authored field of
view; a portrait film scales it up, because the field of view is
vertical and the views were composed for landscape.  Returns PATHNAME
and the frame count."
  (let* ((*chamfer-width* chamfer-width)
         (*light* light)
         ;; The lens focuses at the centre of frame, where the flight is
         ;; always looking: with the aperture open, whatever stands beyond
         ;; the subject melts, which is the mountain games' tilt-shift.
         (*aperture* aperture)
         (shot-frames (max 1 (round (* seconds-per-shot frame-rate))))
         (scenes (mapcar (lambda (piece) (cons piece (atelier-scene piece)))
                         (remove-duplicates pieces)))
         (shots (loop for piece in pieces
                      append (list (list piece (atelier-flyover piece))
                                   (list piece (atelier-flight piece)))))
         (shot-count (length shots))
         (renderer (make-renderer
                    :scene (cdr (first scenes))
                    :camera (cdr (first (atelier-cameras (first pieces))))
                    :width width :height height
                    :style style :pipeline-styles (list style)
                    :effects effects)))
    (unwind-protect
         (with-video-encoder (write-frame pathname width height
                              :frame-rate frame-rate
                              :format (renderer-color-format renderer))
           (loop for (piece waypoints) in shots
                 for shot from 0
                 do (let* ((scene (cdr (assoc piece scenes)))
                           (positions (mapcar #'first waypoints))
                           (looks (mapcar #'second waypoints))
                           (fields (mapcar (lambda (waypoint)
                                             (list (third waypoint)))
                                           waypoints))
                           ;; The waypoints say where to look; the flight
                           ;; between them is sampled at every frame,
                           ;; relaxed off the masonry, and then sworn
                           ;; clear: a drone that passes through a voxel
                           ;; is an error, not a take.
                           (flight
                             (coerce
                              (assert-flight-clear
                               piece scene
                               (clear-flight-path
                                scene
                                (loop for frame below shot-frames
                                      for s = (/ (+ frame 0.5) shot-frames)
                                      collect (catmull-rom-sample
                                               positions
                                               (flight-shot-progress
                                                s shot shot-count)))))
                              'vector)))
                      (setf (renderer-scene renderer) scene)
                      (dotimes (frame shot-frames)
                        (let* ((s (/ (+ frame 0.5) shot-frames))
                               (eased (flight-shot-progress
                                       s shot shot-count))
                               (position (aref flight frame))
                               (look (catmull-rom-sample looks eased))
                               (field (* field-scale
                                         (first (catmull-rom-sample
                                                 fields eased)))))
                          (setf (renderer-camera renderer)
                                (studio-camera
                                 (first position) (second position)
                                 (third position)
                                 :look-x (first look) :look-y (second look)
                                 :look-z (third look)
                                 :field-of-view field))
                          (write-frame (render-pixels renderer)))))))
      (destroy-renderer renderer))))
