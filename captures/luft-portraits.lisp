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
     &key (yaw 2.2455373) (pitch -0.5165006))
  (let ((viewer nil)
        (old-projection luft.render:*projection*)
        (old-isometric-height luft.render:*isometric-height*)
        (old-wireframe luft.render:*wireframe*)
        (old-inspection-ink-p luft.render:*inspection-ink-p*))
    (unwind-protect
         (progn
           (setf luft.render:*projection* :isometric
                 luft.render:*isometric-height* isometric-height
                 luft.render:*wireframe* 0.0
                 luft.render:*inspection-ink-p* nil)
           (setf viewer
                 (luft.render:start-viewer
                  :solid
                  (let ((scene (luft.render:make-mountain-sanctuary-scene)))
                    ;; The material plate isolates the solid renderer; the
                    ;; walking player's separate SDF pass has its own plates.
                    (setf (slot-value scene 'luft.render::player-p) nil)
                    scene)
                  :bevel-width luft:+mesh-bevel-width+
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
