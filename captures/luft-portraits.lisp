(in-package #:luv.showcase)

;;; Reproducible LUFT plates and short upright cuts.  Still images own the exact
;;; scene and camera being studied; films keep motion and cleanup in LUFT's
;;; existing film owners. #Z5NDTA #SY26PO #2TQEBB

(luv:define-capture luft-miter-study
    (:figure Z5NDTA :kind :image :extension "png" :layout :landscape
     :description
     "The orthographic miter family: stepped mountain, mixed stars, and wall terminations.")
  (pathname)
  (let ((viewer nil)
        (old-projection luft.render:*projection*)
        (old-isometric-height luft.render:*isometric-height*)
        (old-wireframe luft.render:*wireframe*)
        (old-inspection-ink-p luft.render:*inspection-ink-p*))
    (unwind-protect
         (progn
           ;; The canvas loop owns another thread, so configure its global
           ;; renderer state before it starts rather than dynamically binding
           ;; these specials around START-VIEWER.
           (setf luft.render:*projection* :isometric
                 luft.render:*isometric-height* 7.0
                 luft.render:*wireframe* 0.85
                 luft.render:*inspection-ink-p* nil)
           (setf viewer
                 (luft.render:start-viewer
                  :solid (luft.render:make-miter-study-scene)
                  :bevel-width luft:+mesh-bevel-width+
                  :camera
                  (luft.render:make-fly-camera
                   :position
                   (luv.arithmetic.lisp.vec3:make-vec3 16.0 -8.0 9.0)
                   :yaw 2.0899425 :pitch -0.33)
                  :title "LUFT miter study"
                  :width 1280 :height 720))
           (luft.render:capture-viewer-frame
            pathname viewer :inspector-p nil))
      (when viewer (luft.render:stop-viewer viewer))
      (setf luft.render:*projection* old-projection
            luft.render:*isometric-height* old-isometric-height
            luft.render:*wireframe* old-wireframe
            luft.render:*inspection-ink-p* old-inspection-ink-p))))

(defun capture-luft-material-contact
    (pathname position isometric-height title
     &key (yaw 2.2455373) (pitch -0.5165006) player-p bevel-profile solid
       (bevel-width luft:+mesh-bevel-width+) (wireframe 0.0)
       inspection-ink-p)
  (let ((viewer nil)
        (old-projection luft.render:*projection*)
        (old-isometric-height luft.render:*isometric-height*)
        (old-wireframe luft.render:*wireframe*)
        (old-inspection-ink-p luft.render:*inspection-ink-p*))
    (unwind-protect
         (progn
           (setf luft.render:*projection* :isometric
                 luft.render:*isometric-height* isometric-height
                 luft.render:*wireframe* wireframe
                 luft.render:*inspection-ink-p* inspection-ink-p)
           (setf viewer
                 (luft.render:start-viewer
                  :solid
                  (let ((scene (or solid
                                   (luft.render:make-mountain-sanctuary-scene))))
                    ;; Material plates isolate the solid renderer.  A lighting
                    ;; plate deliberately keeps the separate SDF player pass.
                    (unless player-p
                      (setf (slot-value scene 'luft.render::player-p) nil))
                    scene)
                  :bevel-width bevel-width
                  :bevel-profile bevel-profile
                  :camera
                  (luft.render:make-fly-camera
                   :position position
                   :yaw yaw :pitch pitch)
                  :title title
                  :width 1100 :height 800))
           (luft.render:capture-viewer-frame
            pathname viewer :inspector-p nil))
      (when viewer (luft.render:stop-viewer viewer))
      (setf luft.render:*projection* old-projection
            luft.render:*isometric-height* old-isometric-height
            luft.render:*wireframe* old-wireframe
            luft.render:*inspection-ink-p* old-inspection-ink-p))))

(luv:define-capture luft-material-contact-study
    (:figure M4T3RL :kind :image :extension "png" :layout :landscape
     :description
     "The sanctuary stairs and foundations where cut stone bears on grass and soil.")
  (pathname)
  (capture-luft-material-contact
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 89.0 33.0 41.0)
   20.0 "LUFT material contact study"))

(luv:define-capture luft-stylized-lighting-study
    (:figure L1GHTS :kind :image :extension "png" :layout :landscape
     :description
     "The sanctuary, traveler, and terrain under LUFT's shared stylized sun.")
  (pathname)
  (capture-luft-material-contact
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 89.0 33.0 41.0)
   20.0 "LUFT stylized lighting study" :player-p t))

(luv:define-capture luft-material-contact-closeup
    (:figure ER7HST :kind :image :extension "png" :layout :landscape
     :description
     "The earth-set foot of the sanctuary's west turret at fillet scale.")
  (pathname)
  (capture-luft-material-contact
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 25.0 38.0 38.0)
   6.0 "LUFT material contact closeup"
   :yaw 0.90 :pitch -0.5165006))

(luv:define-capture luft-material-contact-stairs
    (:figure S8TAIR :kind :image :extension "png" :layout :landscape
     :description
     "A tight oblique view across the sanctuary stair and terrace contacts.")
  (pathname)
  (capture-luft-material-contact
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 80.0 43.0 32.0)
   7.0 "LUFT stair material contacts"))

(luv:define-capture luft-material-contact-west-foot
    (:figure W3STFT :kind :image :extension "png" :layout :landscape
     :description
     "The opposite sanctuary foot, viewed across turf toward the west turret.")
  (pathname)
  (capture-luft-material-contact
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 25.0 38.0 41.0)
   8.0 "LUFT west foundation contacts"
   :yaw 0.90 :pitch -0.5165006))

(luv:define-capture luft-material-contact-bridge-foot
    (:figure BR1DGE :kind :image :extension "png" :layout :landscape
     :description
     "The lower bridge piers where stone meets exposed banks and undersoil.")
  (pathname)
  (capture-luft-material-contact
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 75.0 19.0 20.0)
   7.0 "LUFT bridge foundation contacts"))

(luv:define-capture luft-material-ridge-beacon
    (:figure B3ACON :kind :image :extension "png" :layout :landscape
     :description
     "The ridge beacon's locally framed limestone courses and earth-set foot.")
  (pathname)
  (capture-luft-material-contact
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 112.0 50.0 42.0)
   10.0 "LUFT ridge beacon material frame"
   :yaw 2.2455373 :pitch -0.5165006))

(luv:define-capture luft-material-bevel-policy-high-country
    (:figure W1D4TH :kind :image :extension "png" :layout :landscape
     :description
     "A material bevel experiment: broad terrain at width four and crisp sanctuary stone at width one.")
  (pathname)
  (capture-luft-material-contact
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 117.0 59.0 50.0)
   14.0 "LUFT material bevel policy - high country"
   :yaw 2.3561945 :pitch -0.545
   :bevel-profile (luft.render:make-material-bevel-profile)))

(luv:define-capture luft-material-bevel-policy-mountain
    (:figure M1XWTH :kind :image :extension "png" :layout :landscape
     :description
     "The authored mountain under terrain-four, architecture-one, and mixed-contact-two bevel cohorts.")
  (pathname)
  (capture-luft-material-contact
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 102.0 6.0 50.0)
   32.0 "LUFT material bevel policy - mountain"
   :bevel-profile (luft.render:make-material-bevel-profile)))

(luv:define-capture luft-material-bevel-policy-contact
    (:figure J01NTS :kind :image :extension "png" :layout :landscape
     :description
     "The sanctuary's authored low wall protecting width-one stairs from the terrain-four field.")
  (pathname)
  (capture-luft-material-contact
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 80.0 43.0 32.0)
   7.0 "LUFT material bevel policy - contact"
   :bevel-profile (luft.render:make-material-bevel-profile)))

(luv:define-capture luft-material-bevel-policy-contact-construction
    (:figure WSEK3C :kind :image :extension "png" :layout :landscape
     :description
     "The unprotected material-width contact with triangle construction ink enabled.")
  (pathname)
  (capture-luft-material-contact
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 80.0 43.0 32.0)
   7.0 "LUFT material bevel policy - contact construction"
   :solid (luft.render:make-mountain-sanctuary-scene
           :stair-boundary :open)
   :bevel-profile (luft.render:make-material-bevel-profile)
   :wireframe 1.0))

(macrolet ((define-uniform-contact-capture (name width)
             `(luv:define-capture ,name
                  (:figure WSEK3C :kind :image :extension "png"
                   :layout :landscape
                   :description
                   ,(format nil
                            "The production material contact under uniform bevel width ~D."
                            width))
                (pathname)
                (capture-luft-material-contact
                 pathname
                 (luv.arithmetic.lisp.vec3:make-vec3 80.0 43.0 32.0)
                 7.0 ,(format nil "LUFT uniform width ~D - contact" width)
                 :solid (luft.render:make-mountain-sanctuary-scene
                         :stair-boundary :open)
                 :bevel-width ,width))))
  (define-uniform-contact-capture luft-bevel-width-one-contact 1)
  (define-uniform-contact-capture luft-bevel-width-two-contact 2)
  (define-uniform-contact-capture luft-bevel-width-four-contact 4))

(defun capture-luft-material-bevel-stair-boundary
    (pathname boundary wireframe title)
  (capture-luft-material-contact
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 62.0 42.0 33.0)
   9.0 title :yaw 1.5707964 :pitch -0.588
   :solid (luft.render:make-mountain-sanctuary-scene
           :stair-boundary boundary)
   :bevel-profile (luft.render:make-material-bevel-profile)
   :wireframe wireframe))

(luv:define-capture luft-material-bevel-stair-open
    (:figure WSEK3C :kind :image :extension "png" :layout :landscape
     :description
     "The unprotected production stair, centered as the baseline for authored support geometry.")
  (pathname)
  (capture-luft-material-bevel-stair-boundary
   pathname :open 0.0 "LUFT material bevel - open stair"))

(luv:define-capture luft-material-bevel-stair-open-construction
    (:figure WSEK3C :kind :image :extension "png" :layout :landscape
     :description
     "The unprotected stair baseline with triangle construction ink enabled.")
  (pathname)
  (capture-luft-material-bevel-stair-boundary
   pathname :open 1.0 "LUFT material bevel - open stair construction"))

(luv:define-capture luft-material-bevel-stair-border
    (:figure WSEK3C :kind :image :extension "png" :layout :landscape
     :description
     "The production stair with an authored one-cell stone border level with every tread.")
  (pathname)
  (capture-luft-material-bevel-stair-boundary
   pathname :border 0.0 "LUFT material bevel - stair border"))

(luv:define-capture luft-material-bevel-stair-border-construction
    (:figure WSEK3C :kind :image :extension "png" :layout :landscape
     :description
     "The one-cell stair border with triangle construction ink enabled.")
  (pathname)
  (capture-luft-material-bevel-stair-boundary
   pathname :border 1.0 "LUFT material bevel - stair border construction"))

(luv:define-capture luft-material-bevel-stair-low-wall
    (:figure WSEK3C :kind :image :extension "png" :layout :landscape
     :description
     "The production stair protected by a one-course ascending masonry wall.")
  (pathname)
  (capture-luft-material-bevel-stair-boundary
   pathname :low-wall 0.0 "LUFT material bevel - stair low wall"))

(luv:define-capture luft-material-bevel-stair-low-wall-construction
    (:figure WSEK3C :kind :image :extension "png" :layout :landscape
     :description
     "The authored low-wall stair with triangle construction ink enabled.")
  (pathname)
  (capture-luft-material-bevel-stair-boundary
   pathname :low-wall 1.0 "LUFT material bevel - stair low wall construction"))

(defun capture-luft-material-bevel-transition (pathname wireframe title)
  (capture-luft-material-contact
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 9.5 -0.5 6.5)
   2.5 title :yaw 2.0899425 :pitch -0.36
   :solid (luft.render:make-material-bevel-transition-study-scene)
   :bevel-profile (luft.render:make-material-bevel-profile)
   :wireframe wireframe))

(luv:define-capture luft-material-bevel-transition-clean
    (:figure WSEK3C :kind :image :extension "png" :layout :landscape
     :description
     "The isolated five-cell 1/2/4 transition after exact T-junction contraction.")
  (pathname)
  (capture-luft-material-bevel-transition
   pathname 0.0 "LUFT isolated material bevel transition"))

(luv:define-capture luft-material-bevel-transition-construction
    (:figure WSEK3C :kind :image :extension "png" :layout :landscape
     :description
     "The isolated five-cell 1/2/4 transition with triangle construction ink.")
  (pathname)
  (capture-luft-material-bevel-transition
   pathname 1.0 "LUFT isolated material bevel transition construction"))

(defun capture-luft-miter-closeup (pathname wireframe title)
  "Capture the #xCD wall termination at the normal chamfer width. #L7N4MO"
  (let ((viewer nil)
        (old-projection luft.render:*projection*)
        (old-isometric-height luft.render:*isometric-height*)
        (old-wireframe luft.render:*wireframe*)
        (old-inspection-ink-p luft.render:*inspection-ink-p*))
    (unwind-protect
         (progn
           (setf luft.render:*projection* :isometric
                 luft.render:*isometric-height* 1.0
                 luft.render:*wireframe* wireframe
                 luft.render:*inspection-ink-p* nil)
           ;; At this yaw/pitch the camera lies backward from the motivating
           ;; vertex (12,8,3), placing that exact join at frame centre.
           (setf viewer
                 (luft.render:start-viewer
                  :solid (luft.render:make-miter-study-scene)
                  :bevel-width luft:+mesh-bevel-width+
                  :camera
                  (luft.render:make-fly-camera
                   :position
                   (luv.arithmetic.lisp.vec3:make-vec3 20.02 -5.89 8.51)
                   :yaw 2.0899425 :pitch -0.33)
                  :title title :width 1200 :height 1200))
           (luft.render:capture-viewer-frame
            pathname viewer :inspector-p nil))
      (when viewer (luft.render:stop-viewer viewer))
      (setf luft.render:*projection* old-projection
            luft.render:*isometric-height* old-isometric-height
            luft.render:*wireframe* old-wireframe
            luft.render:*inspection-ink-p* old-inspection-ink-p))))

(luv:define-capture luft-miter-closeup-construction
    (:figure 78WA2W :kind :image :extension "png" :layout :landscape
     :description
     "The sharp #xCD miter at one-cell scale with construction edges visible.")
  (pathname)
  (capture-luft-miter-closeup pathname 1.0 "LUFT sharp miter construction"))

(luv:define-capture luft-miter-closeup-clean
    (:figure 6X0WRV :kind :image :extension "png" :layout :landscape
     :description
     "The same sharp #xCD miter at one-cell scale without construction ink.")
  (pathname)
  (capture-luft-miter-closeup pathname 0.0 "LUFT sharp miter clean"))

(luv:define-capture luft-holm-portrait
    (:figure SY26PO :kind :video :extension "mp4" :layout :portrait
     :description
     "An upright aerial and close pass over the atelier's island architecture.")
    (pathname)
  (uiop:symbol-call :luft.render :film-atelier-flight
   pathname
   :pieces '(:holm)
   :seconds-per-shot 5
   :frame-rate 30
   :width 720
   :height 1280
   :field-scale 1.25
   :style :stock
   :light :evening
   :aperture 0.85))

(luv:define-capture luft-vale-portrait
    (:figure SY26PO :kind :video :extension "mp4" :layout :portrait
     :description
     "An upright flight through the vale with its tree crowns modeled in clay.")
    (pathname)
  (uiop:symbol-call :luft.render :film-atelier-flight
                    pathname
                    :pieces '(:vale)
                    :seconds-per-shot 6
                    :frame-rate 30
                    :width 720
                    :height 1280
                    :field-scale 1.25
                    :style :stock
                    :light :evening
                    :aperture 0.85
                    :clay-stocks '(:conifer :leaf)))

(luv:define-capture luft-clay-holm-breath
    (:figure 2TQEBB :kind :video :extension "mp4" :layout :portrait
     :description
     "The holm's masonry breathing between quilted cells, pearls, and clay melt.")
    (pathname)
  (uiop:symbol-call :luft.render :film-clay-breath
   pathname
   :pieces '(:holm)
   :seconds-per-shot 5
   :frame-rate 30
   :width 720
   :height 1280
   :field-scale 1.25
   :light :golden
   :aperture 0.85))

;;; The traveler.  Both plates hold the animation clock still: a character
;;; recipe that cannot ask for the same pose twice is not a recipe. #TR4VLR

(defun capture-luft-traveler
    (pathname &key (character-time 0.5) (yaw 2.2455373) (pitch -0.5165006)
                   (isometric-height 5.0) (aim-height 15.2) (distance 24.0)
                   (scene-maker #'luft.render:make-traveler-study-scene)
                   (title "LUFT traveler study") (width 900) (height 900))
  (let ((viewer nil)
        (old-projection luft.render:*projection*)
        (old-isometric-height luft.render:*isometric-height*)
        (old-wireframe luft.render:*wireframe*)
        (old-inspection-ink-p luft.render:*inspection-ink-p*)
        (old-character-time luft.render:*character-time*))
    (unwind-protect
         (let* ((forward-x (* (cos yaw) (cos pitch)))
                (forward-y (* (sin yaw) (cos pitch)))
                (forward-z (sin pitch))
                ;; The traveler's own world position, from the same three
                ;; numbers the frame uniform packs for the shader.
                (target-x (+ 29.5 luft.render::+sanctuary-origin-x+))
                (target-y (+ (+ 24.5 luft.render::+sanctuary-origin-y+)
                             (* 10.5 (sin (* character-time 0.22))))))
           (setf luft.render:*projection* :isometric
                 luft.render:*isometric-height* isometric-height
                 luft.render:*wireframe* 0.0
                 luft.render:*inspection-ink-p* nil
                 luft.render:*character-time* character-time)
           (setf viewer
                 (luft.render:start-viewer
                  :solid (funcall scene-maker)
                  :bevel-width luft:+mesh-bevel-width+
                  :camera
                  (luft.render:make-fly-camera
                   :position
                   (luv.arithmetic.lisp.vec3:make-vec3
                    (- target-x (* forward-x distance))
                    (- target-y (* forward-y distance))
                    (- aim-height (* forward-z distance)))
                   :yaw yaw :pitch pitch)
                  :title title :width width :height height))
           (luft.render:capture-viewer-frame
            pathname viewer :inspector-p nil))
      (when viewer (luft.render:stop-viewer viewer))
      (setf luft.render:*projection* old-projection
            luft.render:*isometric-height* old-isometric-height
            luft.render:*wireframe* old-wireframe
            luft.render:*inspection-ink-p* old-inspection-ink-p
            luft.render:*character-time* old-character-time))))

(luv:define-capture luft-traveler-portrait
    (:figure TR4VLR :kind :image :extension "png" :layout :landscape
     :description
     "The sanctuary's hermit on a bare dais: linen robe, copper braid, staff.")
  (pathname)
  (capture-luft-traveler pathname :character-time 0.5
                                  :yaw -1.5707963 :pitch -0.16
                                  :isometric-height 4.4 :aim-height 15.0))

(luv:define-capture luft-traveler-on-the-bridge
    (:figure TR4VBR :kind :image :extension "png" :layout :landscape
     :description
     "The traveler at the sanctuary's own camera angle, shadow on the deck.")
  (pathname)
  (capture-luft-traveler
   pathname :character-time 0.5 :isometric-height 7.0
   :scene-maker #'luft.render:make-mountain-sanctuary-scene
   :title "LUFT traveler on the bridge"))

(defun aim-luft-camera (camera x y z)
  "Aim CAMERA at the world-space point X/Y/Z without changing its lens."
  (let* ((position (luft.render:camera-position camera))
         (dx (- x (luv.arithmetic.lisp.vec3:vec3-x position)))
         (dy (- y (luv.arithmetic.lisp.vec3:vec3-y position)))
         (dz (- z (luv.arithmetic.lisp.vec3:vec3-z position)))
         (flat (sqrt (+ (* dx dx) (* dy dy)))))
    (setf (luft.render:camera-yaw camera) (atan dy dx)
          (luft.render:camera-pitch camera) (atan dz flat)))
  camera)

(defun film-luft-wizard-bridge-walk
    (pathname &key (seconds 8) (frame-rate 24))
  "Film the real walking wizard crossing the authored sanctuary bridge."
  (let ((viewer nil)
        (old-projection luft.render:*projection*)
        (old-wireframe luft.render:*wireframe*)
        (old-inspection-ink-p luft.render:*inspection-ink-p*)
        (old-character-time luft.render:*character-time*))
    (unwind-protect
         (let* ((origin-x luft.render::+sanctuary-origin-x+)
                (origin-y luft.render::+sanctuary-origin-y+)
                (start-y (+ origin-y 10.5))
                (camera
                  (luft.render:make-fly-camera
                   :position
                   (luv.arithmetic.lisp.vec3:make-vec3
                    (+ origin-x 52.0) (- start-y 12.0) 23.5)
                   :field-of-view (* 46.0 (/ pi 180.0))))
                ;; Movement remains camera-relative in play.  Keep its authored
                ;; +Y bridge direction independent of the filming camera.
                (walking-camera
                  (luft.render:make-fly-camera :yaw (/ pi 2.0) :pitch 0.0)))
           (setf luft.render:*projection* :perspective
                 luft.render:*wireframe* 0.0
                 luft.render:*inspection-ink-p* nil
                 luft.render:*character-time* nil)
           (setf viewer
                 (luft.render:start-viewer
                  :solid (luft.render:make-mountain-sanctuary-scene)
                  :bevel-width luft:+mesh-bevel-width+
                  :bevel-profile (luft.render:make-material-bevel-profile)
                  :camera camera :title "LUFT wizard bridge walk"
                  :width 1280 :height 720))
           (setf (luft.render:viewer-player viewer)
                 (luft.render:make-walking-player
                  :position
                  (luv.arithmetic.lisp.vec3:make-vec3
                   (+ origin-x 29.5) start-y 14.0)
                  :heading-x 0.0 :heading-y 1.0 :speed 3.0))
           (luft.render:film-viewer
            viewer pathname :seconds seconds :frame-rate frame-rate
            :before-frame
            (lambda (frame)
              (declare (ignore frame))
              (let* ((player (luft.render:viewer-player viewer))
                     (dt (/ 1.0 frame-rate))
                     (player-position
                       (luft.render:walking-player-position player))
                     (player-y
                       (luv.arithmetic.lisp.vec3:vec3-y player-position))
                     (camera-position (luft.render:camera-position camera)))
                (luft.render::advance-walking-player
                 player (luft.render::viewer-source viewer)
                 walking-camera 1.0 0.0 dt)
                ;; A parallel dolly keeps the wizard readable while successive
                ;; arches, piers, and finally the gate move through the frame.
                (setf (luv.arithmetic.lisp.vec3:vec3-y camera-position)
                      (- player-y 12.0))
                (aim-luft-camera
                 camera (+ origin-x 29.5) (+ player-y 3.0) 15.8)))))
      (when viewer (luft.render:stop-viewer viewer))
      (setf luft.render:*projection* old-projection
            luft.render:*wireframe* old-wireframe
            luft.render:*inspection-ink-p* old-inspection-ink-p
            luft.render:*character-time* old-character-time))))

(luv:define-capture luft-wizard-bridge-walk
    (:figure WZBRDG :kind :video :extension "mp4" :layout :landscape
     :description
     "The sanctuary wizard walks its bridge under a restrained perspective lens.")
  (pathname)
  (film-luft-wizard-bridge-walk pathname))

;;; The streamed highlands. These two cameras retain the regional read and the
;;; citadel-scale read that caught the old sine terrain's repetition. #H1GHLD

(defun wait-for-luft-landscape-residency (viewer)
  (loop for attempt below 240
        for scene = (luft.render::viewer-source viewer)
        for loaded = (hash-table-count
                      (luft.render::streaming-scene-loaded scene))
        for outstanding = (hash-table-count
                           (luft.render::streaming-scene-outstanding scene))
        when (and (plusp loaded) (zerop outstanding)
                  (null (luft.render::streaming-scene-cohort scene)))
          do (format t
                     "capture LUFT highlands: ready with ~D near and ~D planar chunks~%"
                     (loop for width being the hash-values of
                           (luft.render::streaming-scene-loaded scene)
                           count (< width 4))
                     (loop for width being the hash-values of
                           (luft.render::streaming-scene-loaded scene)
                           count (= width 4)))
             (force-output)
             (return t)
        when (zerop (mod attempt 20))
          do (format t "capture LUFT highlands: ~D chunks loaded, ~D pending~%"
                     loaded outstanding)
             (force-output)
        do (sleep 0.05)
        finally (error "LUFT highland residency did not become ready.")))

(defun capture-luft-highland-landscape
    (pathname position yaw pitch title)
  (let ((viewer nil)
        (old-projection luft.render:*projection*)
        (old-wireframe luft.render:*wireframe*)
        (old-inspection-ink-p luft.render:*inspection-ink-p*))
    (unwind-protect
         (progn
           (setf luft.render:*projection* :perspective
                 luft.render:*wireframe* 0.0
                 luft.render:*inspection-ink-p* nil)
           (format t "capture LUFT highlands: building deterministic source~%")
           (force-output)
           (setf viewer
                 (luft.render:start-viewer
                  :solid (luft.render:make-highland-sanctuary-scene)
                  :bevel-width luft:+mesh-bevel-width+
                  :camera
                  (luft.render:make-fly-camera
                   :position position :yaw yaw :pitch pitch)
                  :title title :width 1280 :height 800))
           (wait-for-luft-landscape-residency viewer)
           (luft.render:capture-viewer-frame
            pathname viewer :inspector-p nil))
      (when viewer (luft.render:stop-viewer viewer))
      (setf luft.render:*projection* old-projection
            luft.render:*wireframe* old-wireframe
            luft.render:*inspection-ink-p* old-inspection-ink-p))))

(luv:define-capture luft-grand-highlands
    (:figure H1GHLD :kind :image :extension "png" :layout :landscape
     :description
     "The streamed highland region: rocky massifs, green valleys, shelves, and distant ruins.")
  (pathname)
  (capture-luft-highland-landscape
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 121.0 92.0 54.0)
   2.20 -0.27 "LUFT grand highlands"))

(luv:define-capture luft-highland-citadel
    (:figure R8NCIT :kind :image :extension "png" :layout :landscape
     :description
     "The open-court highland citadel where dressed stone meets terraced green country.")
  (pathname)
  (capture-luft-highland-landscape
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 217.0 136.0 48.0)
   2.18 -0.22 "LUFT highland citadel"))

(luv:define-capture luft-highland-lod-distance
    (:figure L0DDST :kind :image :extension "png" :layout :landscape
     :description
     "The widened highland horizon with full-detail near chunks and coplanar-merged far chunks.")
  (pathname)
  (capture-luft-highland-landscape
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 122.0 91.0 78.0)
   2.20 -0.20 "LUFT highland LoD distance"))

(defun film-luft-highland-flight (pathname &key (seconds 8) (frame-rate 24))
  "Film a slow highland traverse that crosses streaming chunk boundaries."
  (let ((viewer nil)
        (old-projection luft.render:*projection*)
        (old-wireframe luft.render:*wireframe*)
        (old-inspection-ink-p luft.render:*inspection-ink-p*))
    (unwind-protect
         (progn
           (setf luft.render:*projection* :perspective
                 luft.render:*wireframe* 0.0
                 luft.render:*inspection-ink-p* nil)
           (format t "LUFT film: building deterministic highlands~%")
           (force-output)
           (let ((camera
                   (luft.render:make-fly-camera
                    :position
                    (luv.arithmetic.lisp.vec3:make-vec3 122.0 91.0 78.0)
                    :yaw 2.20 :pitch -0.20)))
             (setf viewer
                   (luft.render:start-viewer
                    :solid (luft.render:make-highland-sanctuary-scene)
                    :bevel-width luft:+mesh-bevel-width+
                    :camera camera :title "LUFT highland LoD flight"
                    :width 1280 :height 720))
             (wait-for-luft-landscape-residency viewer)
             (let ((frame-count (max 1 (round (* seconds frame-rate)))))
               (luft.render:film-viewer
                viewer pathname :seconds seconds :frame-rate frame-rate
                :before-frame
                (lambda (frame)
                  (let* ((u (/ frame (float (max 1 (1- frame-count)))))
                         (ease (- (* 3.0 u u) (* 2.0 u u u)))
                         (position (luft.render:camera-position camera)))
                    (setf (luv.arithmetic.lisp.vec3:vec3-x position)
                          (+ 122.0 (* 116.0 ease))
                          (luv.arithmetic.lisp.vec3:vec3-y position)
                          (+ 91.0 (* 78.0 ease))
                          (luv.arithmetic.lisp.vec3:vec3-z position)
                          (+ 78.0 (* 10.0 (sin (* pi u))))
                          (luft.render:camera-yaw camera)
                          (+ 2.20 (* -0.24 ease))
                          (luft.render:camera-pitch camera)
                          (+ -0.20 (* -0.04 (sin (* pi u)))))))))))
      (when viewer (luft.render:stop-viewer viewer))
      (setf luft.render:*projection* old-projection
            luft.render:*wireframe* old-wireframe
            luft.render:*inspection-ink-p* old-inspection-ink-p))))

(luv:define-capture luft-highland-lod-flight
    (:figure L0DDST :kind :video :extension "mp4" :layout :landscape
     :description
     "A slow flight across the widened highlands and their three streaming LoD rings.")
  (pathname)
  (film-luft-highland-flight pathname))
