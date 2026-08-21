(in-package #:luv.showcase)

;;; The first broad gallery folio: portrait compositions for phones, clean
;;; authored motion, and one deliberately gameplay-shaped landscape walk.
;;; Each subject still owns its world and motion; this file only shares the
;;; hidden-session lifecycle and the portrait frame. #RFUR2R #I380Q9

(defconstant +gallery-portrait-width+ 720)
(defconstant +gallery-portrait-height+ 1280)
(defconstant +gallery-landscape-width+ 1200)
(defconstant +gallery-landscape-height+ 800)
(defconstant +gallery-film-frame-rate+ 20)
(defconstant +gallery-film-seconds+ 5)
(defconstant +gallery-terrain-seed+ 33)
(defconstant +gallery-terrain-radius+ 3)
(defconstant +gallery-stage-floor-y+ 6)

(defun grade-gallery-terrain-stage
    (world minimum-x minimum-z maximum-x maximum-z
     &key (floor-y +gallery-stage-floor-y+)
       (surface luvcraft::*grass-block*))
  "Grade one bounded stage into generated WORLD without replacing its horizon.

Everything outside the inclusive X/Z rectangle remains seed terrain, including
its hills, trees, biome changes, fog depth, and streamed chunk boundaries.  The
stage is deliberately the small authored exception inside the real world, not
a flat-world substitute for it. #GEA2VH"
  (luvcraft::with-world-change-transaction (world)
    (loop for x from minimum-x to maximum-x do
      (loop for z from minimum-z to maximum-z do
        (dotimes (y 16)
          (setf (luvcraft:world-block-at world x y z)
                (cond ((< y (- floor-y 2)) luvcraft::*stone-block*)
                      ((< y floor-y) luvcraft::*dirt-block*)
                      ((= y floor-y) surface)
                      (t nil)))))))
  world)

(defun make-staged-gallery-terrain-world
    (builder &key
       (seed +gallery-terrain-seed+)
       (radius +gallery-terrain-radius+)
       (bounds '(0 0 15 15))
       (floor-y +gallery-stage-floor-y+)
       (surface luvcraft::*grass-block*))
  "Make generated terrain, grade BOUNDS, then let BUILDER author its subject.

BUILDER receives WORLD and FLOOR-Y.  Pre-materializing the resident window
makes the stage and the surrounding landscape one deterministic source-owned
fixture before any hidden rendering session begins. #GEA2VH"
  (destructuring-bind (minimum-x minimum-z maximum-x maximum-z) bounds
    (let ((world
            (luvcraft:make-little-block-world
             :seed seed :chunk-radius radius)))
      (grade-gallery-terrain-stage
       world minimum-x minimum-z maximum-x maximum-z
       :floor-y floor-y :surface surface)
      (funcall builder world floor-y)
      (luvcraft:relight-block-world world)
      world)))

(defun call-with-staged-gallery-terrain-session
    (function &key title pose day-fraction builder bounds width height clean-p
                   (floor-y +gallery-stage-floor-y+)
                   (seed +gallery-terrain-seed+)
                   (surface luvcraft::*grass-block*) critters
                   (exposure 0.48) (bloom-gain 0.18) (shaft-gain 0.28))
  "Call FUNCTION in the ordinary deterministic terrain plus one bounded stage."
  (call-with-gallery-session
   function
   :title title :width width :height height :clean-p clean-p
   :world (make-staged-gallery-terrain-world
           builder :seed seed :bounds bounds :floor-y floor-y :surface surface)
   :camera (make-gallery-camera pose)
   :sky-clock (luvcraft::make-pinned-sky-clock day-fraction)
   :sky-profile (luvcraft:make-default-sky-profile)
   :critters critters
   :residency-radius +gallery-terrain-radius+
   :exposure exposure :bloom-gain bloom-gain :shaft-gain shaft-gain))

(defun gallery-look-pose (x y z target-x target-y target-z field-of-view)
  "Make a camera pose at X/Y/Z looking directly at TARGET-X/Y/Z."
  (let* ((dx (- target-x x))
         (dy (- target-y y))
         (dz (- target-z z))
         (flat (sqrt (+ (* dx dx) (* dz dz)))))
    (luvcraft::make-camera-pose
     (luvcraft::make-vec3 x y z)
     (atan dx dz)
     (atan dy flat)
     field-of-view)))

(defun make-gallery-camera (pose)
  (let ((camera (make-instance 'luvcraft:fly-camera)))
    (luvcraft::set-camera-pose camera pose)
    camera))

(defun remove-gallery-hud-overlays (session)
  "Remove HUD-stage presentation without removing world/physics drawers."
  (dolist (overlay (copy-list (luvcraft:luvcraft-session-overlays session)))
    (when (eq :hud (luvcraft::luvcraft-overlay-stage overlay))
      (luvcraft:remove-luvcraft-overlay session overlay))))

(defun remove-gallery-lobby-overlay (session)
  "Remove only the lobby instrument while retaining the ordinary play HUD."
  (dolist (overlay (copy-list (luvcraft:luvcraft-session-overlays session)))
    (when (typep overlay 'mcluv::luvcraft-lobby-hud-overlay)
      (luvcraft:remove-luvcraft-overlay session overlay))))

(defun call-with-gallery-session
    (function &key title world camera sky-clock sky-profile critters
                   width height (residency-radius 4) clean-p
                   (exposure 0.48) (bloom-gain 0.18) (shaft-gain 0.28))
  "Call FUNCTION with one fully resident, source-owned gallery session."
  (call-with-pre-noon-ridge-grade
   (lambda ()
     ;; The grade owner has already saved every process-global value.  These
     ;; subject-local changes remain visible to the canvas thread and its
     ;; unwind restores the prior live grade after the session is closed.
     (setf luvcraft::*luvcraft-exposure* exposure
           luvcraft::*luvcraft-bloom-gain* bloom-gain
           luvcraft::*luvcraft-shaft-gain* shaft-gain
           luvcraft::*luvcraft-crosshair-p* (not clean-p))
     (let ((session nil))
       (unwind-protect
            (progn
              (setf session
                    (luvcraft:start-luvcraft
                     :title title :width width :height height
                     :visible-p nil :frames-per-second nil
                     :world world :camera camera
                     :sky-clock sky-clock :sky-profile sky-profile
                     :critters (or critters
                                   (make-instance 'luvcraft::critter-population))
                     :residency-radius residency-radius))
              (luvcraft:stop-luvcraft-lobby session)
              (if clean-p
                  (remove-gallery-hud-overlays session)
                  (remove-gallery-lobby-overlay session))
              (let ((desired
                      (hash-table-count
                       (luvcraft::luvcraft-session-desired-chunks session))))
                (format t "capture ~A: preparing ~D terrain chunks~%"
                        title desired)
                (finish-output)
                (luvcraft::wait-for-luvcraft-products
                 session :minimum desired))
              (format t "capture ~A: scene ready~%" title)
              (finish-output)
              (funcall function session))
         (when session
           (luvcraft:stop-luvcraft session)))))))

(defun call-with-generated-gallery-session
    (function &key title seed pose day-fraction width height clean-p)
  "Call FUNCTION in one deterministic generated little world."
  (call-with-gallery-session
   function
   :title title :width width :height height :clean-p clean-p
   :world (luvcraft:make-empty-little-block-world :seed seed)
   :camera (make-gallery-camera pose)
   :sky-clock (luvcraft::make-pinned-sky-clock day-fraction)
   :sky-profile (luvcraft:make-default-sky-profile)
   :residency-radius 4))

(defun gallery-ridge-pose (&key (portrait-p nil) (lateral-offset 0.0d0)
                                 (height 8.75d0) (pitch 0.20d0))
  "The proven seed-121 ridge composition, optionally widened for portrait."
  (let* ((yaw 1.05d0)
         (field-of-view
           (* luvcraft::+luvcraft-camera-vertical-field-of-view+
              (if portrait-p 1.23d0 1.0d0))))
    (luvcraft::make-camera-pose
     (luvcraft::make-vec3
      (+ -46.0d0 (* lateral-offset (cos yaw)))
      height
      (- -36.0d0 (* lateral-offset (sin yaw))))
     yaw pitch field-of-view)))

(defun film-gallery-camera-path
    (session pathname pose-at-frame
     &key
       (seconds +gallery-film-seconds+)
       (frame-rate +gallery-film-frame-rate+)
       (include-hud-p nil)
       (include-viewmodel-p nil))
  "Film POSE-AT-FRAME with deterministic shader time and visible progress."
  (let ((frame-count (* seconds frame-rate)))
    (luvcraft:film-luvcraft-session
     session pathname :seconds seconds :frame-rate frame-rate
     :include-hud-p include-hud-p
     :include-viewmodel-p include-viewmodel-p
     :before-frame
     (lambda (frame)
       (setf (luvcraft::luvcraft-session-last-frame-time session)
             (/ frame (coerce frame-rate 'double-float)))
       (luvcraft::set-camera-pose
        (luvcraft:luvcraft-session-camera session)
        (funcall pose-at-frame frame frame-count))
       (when (zerop (mod frame frame-rate))
         (format t "capture gallery film: second ~D/~D~%"
                 (1+ (/ frame frame-rate)) seconds)
         (finish-output))))))

;;; Generated terrain: the same real little world as #NLCFX0 and #TRYHPN,
;;; recomposed vertically rather than cropped after rendering.

(luv:define-capture pre-noon-under-the-trees
    (:figure NLCFX0 :kind :image :extension "png"
     :description
     "A lower clean ridge view using trunks and crowns as warm foreground.")
    (pathname)
  (call-with-generated-gallery-session
   (lambda (session)
     (luvcraft:capture-luvcraft-screenshot
      session pathname :include-hud-p nil :include-viewmodel-p nil))
   :title "pre-noon under the trees" :seed 121
   :pose (gallery-ridge-pose :lateral-offset -4.0d0
                            :height 7.85d0 :pitch 0.12d0)
   :day-fraction +pre-noon-ridge-day-fraction+
   :width +gallery-landscape-width+ :height +gallery-landscape-height+
   :clean-p t))

(luv:define-capture pre-noon-under-the-trees-portrait
    (:figure NLCFX0 :kind :image :extension "png" :layout :portrait
     :description
     "A phone-first low ridge portrait framed by seed 121's tree crowns.")
    (pathname)
  (call-with-generated-gallery-session
   (lambda (session)
     (luvcraft:capture-luvcraft-screenshot
      session pathname :include-hud-p nil :include-viewmodel-p nil))
   :title "pre-noon under the trees portrait" :seed 121
   :pose (gallery-ridge-pose :portrait-p t :lateral-offset -4.0d0
                            :height 7.85d0 :pitch 0.16d0)
   :day-fraction +pre-noon-ridge-day-fraction+
   :width +gallery-portrait-width+ :height +gallery-portrait-height+
   :clean-p t))

(luv:define-capture pre-noon-ridge-portrait-glide
    (:figure NLCFX0 :kind :video :extension "mp4" :layout :portrait
     :description
     "A clean phone-first glide through warm trees toward the central ridge.")
    (pathname)
  (call-with-generated-gallery-session
   (lambda (session)
     (film-gallery-camera-path
      session pathname
      (lambda (frame frame-count)
        (let ((progress (/ frame (coerce (max 1 (1- frame-count))
                                         'double-float))))
          (gallery-ridge-pose
           :portrait-p t :lateral-offset (+ -3.0d0 (* 6.0d0 progress))
           :height 8.2d0 :pitch 0.17d0)))))
   :title "pre-noon ridge portrait glide" :seed 121
   :pose (gallery-ridge-pose :portrait-p t :lateral-offset -3.0d0)
   :day-fraction +pre-noon-ridge-day-fraction+
   :width +gallery-portrait-width+ :height +gallery-portrait-height+
   :clean-p t))

(luv:define-capture ridge-play-view
    (:figure NLCFX0 :kind :image :extension "png" :section :play
     :description
     "The same ridge as ordinary play, retaining crosshair, hotbar, and body.")
    (pathname)
  (call-with-generated-gallery-session
   (lambda (session)
     (luvcraft:capture-luvcraft-screenshot
      session pathname :include-hud-p t :include-viewmodel-p t))
   :title "ridge play view" :seed 121
   :pose (gallery-ridge-pose)
   :day-fraction +pre-noon-ridge-day-fraction+
   :width +gallery-landscape-width+ :height +gallery-landscape-height+
   :clean-p nil))

(luv:define-capture ridge-walk-with-hud
    (:figure NLCFX0 :kind :video :extension "mp4" :section :play
     :description
     "A short real player walk with the ordinary crosshair, hotbar, and body.")
    (pathname)
  (call-with-generated-gallery-session
   (lambda (session)
     (let* ((frame-rate +gallery-film-frame-rate+)
            (seconds +gallery-film-seconds+)
            (intent (luvcraft:luvcraft-session-movement-intent session))
            (camera (luvcraft:luvcraft-session-camera session)))
       (unwind-protect
            (progn
              (setf (luvcraft:movement-urging-p intent :forward) t)
              (luvcraft:film-luvcraft-session
               session pathname :seconds seconds :frame-rate frame-rate
               :include-hud-p t :include-viewmodel-p t
               :before-frame
               (lambda (frame)
                 (setf (luvcraft:camera-yaw camera)
                       (+ 1.05d0 (* 0.08d0
                                    (sin (* 2d0 pi frame
                                            (/ 1d0 (* seconds frame-rate)))))))
                 (luvcraft::advance-luvcraft-session-to
                  session (/ frame (coerce frame-rate 'double-float)))
                 (when (zerop (mod frame frame-rate))
                   (format t "capture ridge-walk-with-hud: second ~D/~D~%"
                           (1+ (/ frame frame-rate)) seconds)
                   (finish-output)))))
         (luvcraft::clear-movement-intent intent))))
   :title "ridge walk with HUD" :seed 121
   :pose (gallery-ridge-pose)
   :day-fraction +pre-noon-ridge-day-fraction+
   :width +gallery-landscape-width+ :height +gallery-landscape-height+
   :clean-p nil))

(defun day-cycle-portrait-pose ()
  (gallery-look-pose
   8.0d0 11.0d0 -18.0d0
   8.0d0 6.2d0 4.0d0
   (* 1.22d0 luvcraft::+luvcraft-camera-vertical-field-of-view+)))

(defun capture-day-cycle-portrait (pathname day-fraction title)
  (call-with-generated-gallery-session
   (lambda (session)
     (luvcraft:capture-luvcraft-screenshot
      session pathname :include-hud-p nil :include-viewmodel-p nil))
   :title title :seed 121 :pose (day-cycle-portrait-pose)
   :day-fraction day-fraction
   :width +gallery-portrait-width+ :height +gallery-portrait-height+
   :clean-p t))

(luv:define-capture little-world-blue-hour-portrait
    (:figure TRYHPN :kind :image :extension "png" :layout :portrait
     :description "The fixed little world recomposed vertically at blue hour.")
    (pathname)
  (capture-day-cycle-portrait pathname 0.21 "little world blue hour portrait"))

(luv:define-capture little-world-sunrise-portrait
    (:figure TRYHPN :kind :image :extension "png" :layout :portrait
     :description "A tall sunrise view with long shadows below a cool zenith.")
    (pathname)
  (capture-day-cycle-portrait pathname 0.28 "little world sunrise portrait"))

(luv:define-capture little-world-sunset-portrait
    (:figure TRYHPN :kind :image :extension "png" :layout :portrait
     :description "A tall sunset view preserving warm fog and lateral light.")
    (pathname)
  (capture-day-cycle-portrait pathname 0.72 "little world sunset portrait"))

;;; Crystal grove: a compact source-owned set where blocklight, emissive
;;; material, foliage, masonry, and depth all remain legible in one frame.

(defun make-gallery-crystal-grove-world ()
  (make-staged-gallery-terrain-world
   (lambda (world floor-y)
     (loop for x from 4 to 27 do
       (loop for z from 4 to 27
             when (or (<= 14 x 17) (<= 14 z 17))
               do (setf (luvcraft:world-block-at world x floor-y z)
                        luvcraft::*stone-block*)))
     (dolist (coordinate '((15 1 15) (16 1 16) (17 1 17)
                           (9 1 22) (22 1 21) (23 1 10)))
       (destructuring-bind (x relative-y z) coordinate
         (setf (luvcraft:world-block-at world x (+ floor-y relative-y) z)
               luvcraft:*crystal-block*)))
     (dolist (tree '((8 18 5) (24 14 6) (24 25 5)))
       (destructuring-bind (x z relative-top) tree
         (let ((top (+ floor-y relative-top)))
           (loop for y from (1+ floor-y) below top do
             (setf (luvcraft:world-block-at world x y z)
                   luvcraft::*wood-block*))
           (loop for dx from -2 to 2 do
             (loop for dz from -2 to 2
                   when (<= (+ (abs dx) (abs dz)) 3)
                     do (setf (luvcraft:world-block-at
                               world (+ x dx) top (+ z dz))
                              luvcraft::*leaf-block*)))))))
   :bounds '(3 3 28 28)))

(defun gallery-crystal-grove-pose (angle &key portrait-p)
  (let* ((centre-x 16.0d0) (centre-z 16.0d0) (radius 15.5d0)
         (x (+ centre-x (* radius (sin angle))))
         (z (- centre-z (* radius (cos angle)))))
    (gallery-look-pose
     x (+ +gallery-stage-floor-y+ 4.2d0) z
     centre-x (+ +gallery-stage-floor-y+ 2.0d0) centre-z
     (* luvcraft::+luvcraft-camera-vertical-field-of-view+
        (if portrait-p 1.18d0 0.92d0)))))

(defun call-with-crystal-grove-session (function width height clean-p)
  (call-with-gallery-session
   function
   :title "crystal grove afterglow" :width width :height height
   :clean-p clean-p :residency-radius +gallery-terrain-radius+
   :world (make-gallery-crystal-grove-world)
   :camera (make-gallery-camera
            (gallery-crystal-grove-pose 0.0d0
                                        :portrait-p (> height width)))
   :sky-clock (luvcraft::make-pinned-sky-clock 0.84)
   :sky-profile (luvcraft:make-default-sky-profile)
   :exposure 0.68 :bloom-gain 0.34 :shaft-gain 0.10))

(luv:define-capture crystal-grove-afterglow-still
    (:figure Q8ZIFD :kind :image :extension "png"
     :description
     "A clean crystal grove showing facets, blocklight, foliage, and masonry.")
    (pathname)
  (call-with-crystal-grove-session
   (lambda (session)
     (luvcraft:capture-luvcraft-screenshot
      session pathname :include-hud-p nil :include-viewmodel-p nil))
   +gallery-landscape-width+ +gallery-landscape-height+ t))

(luv:define-capture crystal-grove-afterglow-portrait
    (:figure Q8ZIFD :kind :image :extension "png" :layout :portrait
     :description
     "The crystal grove composed vertically for luminous depth on a phone.")
    (pathname)
  (call-with-crystal-grove-session
   (lambda (session)
     (luvcraft:capture-luvcraft-screenshot
      session pathname :include-hud-p nil :include-viewmodel-p nil))
   +gallery-portrait-width+ +gallery-portrait-height+ t))

(luv:define-capture crystal-grove-night-portrait
    (:figure Q8ZIFD :kind :video :extension "mp4" :layout :portrait
     :description
     "A restrained portrait arc revealing the crystal blocklight volume.")
    (pathname)
  (call-with-crystal-grove-session
   (lambda (session)
     (film-gallery-camera-path
      session pathname
      (lambda (frame frame-count)
        (let ((progress (/ frame (coerce (max 1 (1- frame-count))
                                         'double-float))))
          (gallery-crystal-grove-pose
           (+ -0.24d0 (* 0.48d0 progress)) :portrait-p t)))))
   +gallery-portrait-width+ +gallery-portrait-height+ t))

;;; Inhabitants: the existing deterministic three-turtle cast, recomposed at
;;; their own height instead of presented as an aerial diagnostic plate.

(defun gallery-turtle-pose (angle &key portrait-p (close-p nil))
  (let* ((centre-x 7.8d0) (centre-z 7.6d0)
         (radius (if close-p 7.0d0 9.5d0))
         (x (+ centre-x (* radius (sin angle))))
         (z (- centre-z (* radius (cos angle)))))
    (gallery-look-pose
     x (+ +gallery-stage-floor-y+ (if close-p 2.15d0 2.55d0)) z
     centre-x (+ +gallery-stage-floor-y+ 1.45d0) centre-z
     (* luvcraft::+luvcraft-camera-focused-vertical-field-of-view+
        (if portrait-p 1.28d0 1.0d0)))))

(defun make-gallery-turtle-population ()
  "Pose the established turtle cast on the shared generated-terrain stage."
  (let ((population (make-instance 'luvcraft::critter-population))
        (y (coerce (1+ +gallery-stage-floor-y+) 'double-float)))
    (dolist (turtle
             (list (luvcraft::make-gazetteer-turtle
                    7.8d0 y 6.7d0 2.15d0 1.1d0 :seed 11)
                   (luvcraft::make-gazetteer-turtle
                    10.4d0 y 8.4d0 4.90d0 3.9d0 :seed 12)
                   (luvcraft::make-gazetteer-turtle
                    5.0d0 y 8.0d0 0.90d0 0d0 :resting-p t :seed 13)))
      (luvcraft::add-critter population turtle))
    population))

(defun call-with-turtle-gallery-session (function width height)
  (call-with-gallery-session
   function
   :title "turtle meadow portrait" :width width :height height
   :clean-p t :residency-radius +gallery-terrain-radius+
   :world (make-staged-gallery-terrain-world
           (lambda (world floor-y)
             (declare (ignore world floor-y)))
           :bounds '(3 3 13 12))
   :camera (make-gallery-camera
            (gallery-turtle-pose 0.0d0 :portrait-p (> height width)))
   :critters (make-gallery-turtle-population)
   :sky-clock (luvcraft::make-pinned-sky-clock 0.42)
   :sky-profile (luvcraft:make-default-sky-profile)
   :exposure 0.52 :bloom-gain 0.12 :shaft-gain 0.18))

(luv:define-capture turtle-meadow-portrait
    (:figure W7T2IT :kind :image :extension "png" :layout :portrait
     :section :inhabitants
     :description
     "Three turtles at their own height: walking, turning, and resting poses.")
    (pathname)
  (call-with-turtle-gallery-session
   (lambda (session)
     (luvcraft:capture-luvcraft-screenshot
      session pathname :include-hud-p nil :include-viewmodel-p nil))
   +gallery-portrait-width+ +gallery-portrait-height+))

(luv:define-capture turtle-meadow-closeup
    (:figure W7T2IT :kind :image :extension "png" :section :inhabitants
     :description
     "A low close meadow view preserving turtle scale, materials, and shadow.")
    (pathname)
  (call-with-turtle-gallery-session
   (lambda (session)
     (luvcraft:capture-luvcraft-screenshot
      session pathname
      :camera-pose (gallery-turtle-pose -0.18d0 :close-p t)
      :include-hud-p nil :include-viewmodel-p nil))
   +gallery-landscape-width+ +gallery-landscape-height+))

(luv:define-capture turtle-meadow-portrait-dolly
    (:figure W7T2IT :kind :video :extension "mp4" :layout :portrait
     :section :inhabitants
     :description
     "A quiet phone-first dolly around the three posed meadow turtles.")
    (pathname)
  (call-with-turtle-gallery-session
   (lambda (session)
     (film-gallery-camera-path
      session pathname
      (lambda (frame frame-count)
        (let ((progress (/ frame (coerce (max 1 (1- frame-count))
                                         'double-float))))
          (gallery-turtle-pose
           (+ -0.20d0 (* 0.40d0 progress)) :portrait-p t)))))
   +gallery-portrait-width+ +gallery-portrait-height+))

;;; Physics: a deterministic burst over source-owned stairs.  The film moves
;;; the bodies rather than orbiting a static proof; the stills advance the
;;; same fixed simulation to a settled composition.

(defun make-gallery-ball-stair-world ()
  (make-staged-gallery-terrain-world
   (lambda (world floor-y)
    (loop for z from 5 to 14
          for step = (1+ (floor (- z 5) 2))
          do (loop for x from 3 to 12 do
               (loop for y from 1 to step do
                 (setf (luvcraft:world-block-at world x (+ floor-y y) z)
                       (if (oddp step)
                           luvcraft::*bricks-block*
                           luvcraft::*stone-block*))))))
   :bounds '(2 3 13 15) :surface luvcraft::*stone-block*))

(defun gallery-ball-stair-pose (&key portrait-p)
  (gallery-look-pose
   8.0d0 (+ +gallery-stage-floor-y+ 5.8d0) -7.0d0
   8.0d0 (+ +gallery-stage-floor-y+ 4.0d0) 9.0d0
   (* luvcraft::+luvcraft-camera-vertical-field-of-view+
      (if portrait-p 1.18d0 0.92d0))))

(defun call-with-ball-gallery-session (function width height)
  (call-with-gallery-session
   (lambda (session)
     (luvcraft::scatter-party-balls
      session
      :center (list 8.0d0 (+ +gallery-stage-floor-y+ 10.0d0) 12.0d0)
      :count 20)
     (funcall function session))
   :title "party ball cascade" :width width :height height
   :clean-p t :residency-radius +gallery-terrain-radius+
   :world (make-gallery-ball-stair-world)
   :camera (make-gallery-camera
            (gallery-ball-stair-pose :portrait-p (> height width)))
   :sky-clock (luvcraft::make-pinned-sky-clock 0.38)
   :sky-profile (luvcraft:make-default-sky-profile)
   :exposure 0.52 :bloom-gain 0.12 :shaft-gain 0.20))

(defun advance-gallery-ball-cascade (session seconds frame-rate)
  (dotimes (frame (* seconds frame-rate))
    (luvcraft::advance-luvcraft-session-to
     session (/ frame (coerce frame-rate 'double-float)))
    (when (zerop (mod frame frame-rate))
      (format t "capture party ball cascade: settling second ~D/~D~%"
              (1+ (/ frame frame-rate)) seconds)
      (finish-output))))

(luv:define-capture party-ball-cascade-settled
    (:figure CSCFGF :kind :image :extension "png" :section :play
     :description
     "Beach, smiley, and classic balls settled across source-owned stairs.")
    (pathname)
  (call-with-ball-gallery-session
   (lambda (session)
     (advance-gallery-ball-cascade session 5 30)
     (luvcraft:capture-luvcraft-screenshot
      session pathname :include-hud-p nil :include-viewmodel-p nil))
   +gallery-landscape-width+ +gallery-landscape-height+))

(luv:define-capture party-ball-cascade-portrait-still
    (:figure CSCFGF :kind :image :extension "png" :layout :portrait
     :section :play
     :description
     "The settled patterned-ball staircase composed vertically for a phone.")
    (pathname)
  (call-with-ball-gallery-session
   (lambda (session)
     (advance-gallery-ball-cascade session 5 30)
     (luvcraft:capture-luvcraft-screenshot
      session pathname :include-hud-p nil :include-viewmodel-p nil))
   +gallery-portrait-width+ +gallery-portrait-height+))

(luv:define-capture party-ball-cascade-portrait
    (:figure CSCFGF :kind :video :extension "mp4" :layout :portrait
     :section :play
     :description
     "A deterministic patterned-ball burst bouncing and settling down stairs.")
    (pathname)
  (call-with-ball-gallery-session
   (lambda (session)
     (let ((frame-rate +gallery-film-frame-rate+)
           (seconds +gallery-film-seconds+))
       (luvcraft:film-luvcraft-session
        session pathname :seconds seconds :frame-rate frame-rate
        :include-hud-p nil :include-viewmodel-p nil
        :before-frame
        (lambda (frame)
          (luvcraft::advance-luvcraft-session-to
           session (/ frame (coerce frame-rate 'double-float)))
          (when (zerop (mod frame frame-rate))
            (format t "capture party-ball-cascade-portrait: second ~D/~D~%"
                    (1+ (/ frame frame-rate)) seconds)
            (finish-output))))))
   +gallery-portrait-width+ +gallery-portrait-height+))
