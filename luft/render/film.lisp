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

(defun film-atelier-flight (pathname
                            &key (pieces '(:arcade :viaduct :turret
                                           :grotto :headland :holm))
                                 (seconds-per-piece 8) (frame-rate 30)
                                 (width 1280) (height 720)
                                 (style :stock)
                                 (chamfer-width (/ 1.0 6.0))
                                 (light :golden)
                                 (aperture 0.85)
                                 (effects (default-renderer-effects))
                                 (look-distance 12.0)
                                 (field-scale 1.0))
  "Fly a drone through the atelier's architecture into an MP4 at PATHNAME.

Each of PIECES gets one continuous shot: a Catmull-Rom flight through the
piece's own authored cameras, looking where each of them looked, easing
in and out of the run.  STYLE defaults to :STOCK -- the chamfered
multi-material style the wiki's stills use -- with CHAMFER-WIDTH of a
sixth of a cell, wide enough that every arris reads as a planed band.
FIELD-SCALE widens every authored field of view; a portrait film scales
it up, because the field of view is vertical and the authored views were
composed for landscape.  Returns PATHNAME and the frame count."
  (let* ((*chamfer-width* chamfer-width)
         (*light* light)
         ;; The lens focuses at the centre of frame, where the flight is
         ;; always looking: with the aperture open, whatever stands beyond
         ;; the subject melts, which is the mountain games' tilt-shift.
         (*aperture* aperture)
         (piece-frames (max 1 (round (* seconds-per-piece frame-rate))))
         (first-piece (first pieces))
         (renderer (make-renderer
                    :scene (atelier-scene first-piece)
                    :camera (cdr (first (atelier-cameras first-piece)))
                    :width width :height height
                    :style style :pipeline-styles (list style)
                    :effects effects)))
    (unwind-protect
         (with-video-encoder (write-frame pathname width height
                              :frame-rate frame-rate
                              :format (renderer-color-format renderer))
           (dolist (piece pieces)
             (let* ((scene (atelier-scene piece))
                    (cameras (mapcar #'cdr (atelier-cameras piece)))
                    (positions
                      (mapcar (lambda (camera)
                                (let ((p (camera-position camera)))
                                  (list (vec3:vec3-x p) (vec3:vec3-y p)
                                        (vec3:vec3-z p))))
                              cameras))
                    (looks
                      (mapcar (lambda (camera)
                                (multiple-value-bind (right up forward)
                                    (camera-basis camera)
                                  (declare (ignore right up))
                                  (let ((p (camera-position camera)))
                                    (list (+ (vec3:vec3-x p)
                                             (* look-distance
                                                (vec3:vec3-x forward)))
                                          (+ (vec3:vec3-y p)
                                             (* look-distance
                                                (vec3:vec3-y forward)))
                                          (+ (vec3:vec3-z p)
                                             (* look-distance
                                                (vec3:vec3-z forward)))))))
                              cameras))
                    (fields
                      (mapcar (lambda (camera)
                                (list (camera-field-of-view camera)))
                              cameras))
                    ;; The authored cameras say where to look; the flight
                    ;; between them is sampled at every frame, relaxed off
                    ;; the masonry, and then sworn clear: a drone that
                    ;; passes through a voxel is an error, not a take.
                    (flight
                      (coerce
                       (assert-flight-clear
                        piece scene
                        (clear-flight-path
                         scene
                         (loop for frame below piece-frames
                               for s = (/ (+ frame 0.5) piece-frames)
                               for eased = (* s s (- 3.0 (* 2.0 s)))
                               collect (catmull-rom-sample positions
                                                           eased))))
                       'vector)))
               (setf (renderer-scene renderer) scene)
               (dotimes (frame piece-frames)
                 (let* ((s (/ (+ frame 0.5) piece-frames))
                        (eased (* s s (- 3.0 (* 2.0 s))))
                        (position (aref flight frame))
                        (look (catmull-rom-sample looks eased))
                        (field (* field-scale
                                  (first (catmull-rom-sample fields
                                                             eased)))))
                   (setf (renderer-camera renderer)
                         (studio-camera
                          (first position) (second position)
                          (third position)
                          :look-x (first look) :look-y (second look)
                          :look-z (third look)
                          :field-of-view field))
                   (write-frame (render-pixels renderer)))))))
      (destroy-renderer renderer))))
