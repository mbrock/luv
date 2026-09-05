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
     &key (yaw 2.2455373) (pitch -0.5165006) player-p solid
       surface-mesh surface-generation
       (bevel-width luft:+mesh-bevel-width+) (wireframe 0.0)
       inspection-ink-p (width 1100) (height 800)
       (render-scale luft.render::*render-scale*)
       (temporal-upscaling-p luft.render::*temporal-upscaling-p*)
       after-start fixed-exposure)
  (let ((viewer nil)
        (old-projection luft.render:*projection*)
        (old-isometric-height luft.render:*isometric-height*)
        (old-wireframe luft.render:*wireframe*)
        (old-inspection-ink-p luft.render:*inspection-ink-p*)
        (old-render-scale luft.render::*render-scale*)
        (old-temporal-upscaling-p luft.render::*temporal-upscaling-p*))
    (unwind-protect
         (progn
           (setf luft.render:*projection* :isometric
                 luft.render:*isometric-height* isometric-height
                 luft.render:*wireframe* wireframe
                 luft.render:*inspection-ink-p* inspection-ink-p
                 luft.render::*render-scale* render-scale
                 luft.render::*temporal-upscaling-p* temporal-upscaling-p)
           (setf viewer
                 (luft.render:start-viewer
                  :solid
                  ;; A supplied immutable scene is authoritative.  Only the
                  ;; default sanctuary constructor authors the requested
                  ;; presence or absence of its separate SDF player pass.
                  (or solid
                      (luft.render:make-mountain-sanctuary-scene
                       :player-p player-p))
                  :bevel-width bevel-width
                  :surface-mesh surface-mesh
                  :surface-generation surface-generation
                  :camera
                  (luft.render:make-fly-camera
                   :position position
                   :yaw yaw :pitch pitch)
                  :fixed-exposure fixed-exposure
                  :title title
                  :width width :height height))
           (when after-start
             (funcall after-start viewer))
           (luft.render:capture-viewer-frame
            pathname viewer :inspector-p nil))
      (when viewer (luft.render:stop-viewer viewer))
      (setf luft.render:*projection* old-projection
            luft.render:*isometric-height* old-isometric-height
            luft.render:*wireframe* old-wireframe
            luft.render:*inspection-ink-p* old-inspection-ink-p
            luft.render::*render-scale* old-render-scale
            luft.render::*temporal-upscaling-p* old-temporal-upscaling-p))))

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

(defun make-luft-voxel-light-capture-light (source)
  "Return a dim environment that leaves local voxel light legible."
  (make-instance
   'luft.render:light
   :name :voxel-light-night
   :sun-direction (luft.render::light-sun-direction source)
   :sun-color #(0.12 0.08 0.14 0.18)
   :sky-color #(0.035 0.055 0.11 1.0)
   :ground-color #(0.018 0.012 0.032 1.0)
   :shadow-half-extent (luft.render::light-shadow-half-extent source)
   :shadow-depth-radius (luft.render::light-shadow-depth-radius source)
   :shadow-base-bias (luft.render::light-shadow-base-bias source)
   :shadow-filter-radius (luft.render::light-shadow-filter-radius source)))

(defun make-luft-voxel-light-capture-scene (propagation-p)
  "Return the shrine with emissive surfaces on and optional propagated light."
  ;; The propagation policy is immutable authored scene input.  NIL owns an
  ;; exact empty propagated generation while leaving direct crystal, flame,
  ;; and packed torch-body emission intact for the controlled A/B.
  (luft.render:make-voxel-light-shrine-scene
   :voxel-light-propagation-p propagation-p))

(defun capture-luft-voxel-light-shrine
    (pathname position isometric-height title
     &key (yaw 2.2455373) (pitch -0.28) (propagation-p t))
  "Capture the authored shrine under one fixed, deliberately dim light."
  (let ((old-light luft.render:*light*))
    (unwind-protect
         (progn
           ;; START-VIEWER owns another thread, so publish the capture light
           ;; before constructing it just like the other renderer globals.
           (setf luft.render:*light*
                 (make-luft-voxel-light-capture-light old-light))
           (capture-luft-material-contact
            pathname position isometric-height title
            :yaw yaw :pitch pitch
            :solid (make-luft-voxel-light-capture-scene propagation-p)))
      (setf luft.render:*light* old-light))))

(luv:define-capture luft-voxel-light-shrine-context
    (:figure VXLGHT :kind :image :extension "png" :layout :landscape
     :description
     "A dim receiving shrine showing colored voxel light, four face torches, and two translucent crystals.")
  (pathname)
  (capture-luft-voxel-light-shrine
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 23.0 -1.0 13.5)
   14.0 "LUFT voxel-light shrine"))

(luv:define-capture luft-voxel-light-shrine-propagation-off
    (:figure VXLGHT :kind :image :extension "png" :layout :landscape
     :description
     "The identical shrine and emissive materials with propagated voxel light disabled.")
  (pathname)
  (capture-luft-voxel-light-shrine
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 23.0 -1.0 13.5)
   14.0 "LUFT voxel-light shrine - propagation off" :propagation-p nil))

(luv:define-capture luft-voxel-light-shrine-close
    (:figure VXLGHT :kind :image :extension "png" :layout :landscape
     :description
     "A fixed close view of the wall torch, crystal transmission, and colored receiver light.")
  (pathname)
  (capture-luft-voxel-light-shrine
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 18.2 7.0 11.2)
   7.5 "LUFT voxel-light shrine close" :pitch -0.36))

(defun make-luft-gemstone-gallery-scene ()
  "Build one compact atlas of crystal/support relations and torch directions."
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 6)))
    ;; A grass court, dressed back wall, side return, and overhead stone beam
    ;; give the same crystal material four substantially different supports.
    (luft.render::scene-builder-box builder 4 33 4 29 2 3)
    (luft.render::scene-builder-box builder 4 33 28 29 4 15
                                    :architecture-p t)
    (luft.render::scene-builder-box builder 4 5 13 28 4 12
                                    :architecture-p t)
    (luft.render::scene-builder-box builder 6 27 11 13 15 15
                                    :architecture-p t)
    (luft.render::scene-builder-box builder 14 19 6 9 4 4
                                    :architecture-p t)
    ;; Grass jewels and a short stone-mounted constellation in the foreground.
    (dolist (cell '((8 7 4) (10 8 4) (8 10 4) (11 11 4)))
      (destructuring-bind (x y z) cell
        (luft.render::scene-builder-cell
         builder x y z :material luft.render::*crystal-material-placement*)))
    (dolist (cell '((15 7 5) (17 7 5) (19 7 5) (16 9 5) (18 9 5)))
      (destructuring-bind (x y z) cell
        (luft.render::scene-builder-cell
         builder x y z :material luft.render::*crystal-material-placement*)))
    ;; A deliberately contiguous wall row tests whether the grip becomes one
    ;; composed bevel rhythm instead of a line of unrelated glued-on sockets.
    (loop for x from 8 to 14
          do (luft.render::scene-builder-cell
              builder x 27 10
              :material luft.render::*crystal-material-placement*))
    ;; A diamond/cross pattern exposes both convex perimeter and concave joins.
    (dolist (cell '((21 27 10) (23 27 8) (23 27 10) (23 27 12)
                    (25 27 10)))
      (destructuring-bind (x y z) cell
        (luft.render::scene-builder-cell
         builder x y z :material luft.render::*crystal-material-placement*)))
    ;; Hanging points, plus a side-wall pair, rotate the same contact law onto
    ;; downward and horizontal normals.
    (dolist (x '(8 12 17 22 26))
      (luft.render::scene-builder-cell
       builder x 12 14 :material luft.render::*crystal-material-placement*))
    (dolist (cell '((6 18 7) (6 21 9) (6 24 7)))
      (destructuring-bind (x y z) cell
        (luft.render::scene-builder-cell
         builder x y z :material luft.render::*crystal-material-placement*)))
    ;; Four attachment normals share one authored flame implementation.
    (luft.render::scene-builder-torch builder 29 8 3 :z :high)
    (luft.render::scene-builder-torch builder 18 28 13 :y :low)
    (luft.render::scene-builder-torch builder 5 16 9 :x :high)
    (luft.render::scene-builder-torch builder 25 12 15 :z :low)
    (luft.render::finish-scene-builder builder)))

(defun make-luft-gemstone-capture-light (source)
  "A balanced studio environment whose warm and blue hemispheres refract."
  (make-instance
   'luft.render:light
   :name :gemstone-atelier
   :sun-direction (luft.render::light-sun-direction source)
   :sun-color #(0.78 0.56 0.37 0.64)
   :sky-color #(0.15 0.25 0.52 1.0)
   :ground-color #(0.10 0.055 0.045 1.0)
   :shadow-half-extent (luft.render::light-shadow-half-extent source)
   :shadow-depth-radius (luft.render::light-shadow-depth-radius source)
   :shadow-base-bias (luft.render::light-shadow-base-bias source)
   :shadow-filter-radius (luft.render::light-shadow-filter-radius source)))

(defun capture-luft-gemstone-gallery
    (pathname contact-width position isometric-height title
     &key (yaw 2.18) (pitch -0.28) (flame-time 0.43))
  "Capture one fixed gallery view while varying only the bezel contact width."
  (let ((old-light luft.render:*light*)
        (old-flame-time luft.render:*flame-time*))
    (unwind-protect
         (progn
           (setf luft.render:*light*
                 (make-luft-gemstone-capture-light old-light)
                 luft.render:*flame-time* flame-time)
           (capture-luft-material-contact
            pathname position isometric-height title :yaw yaw :pitch pitch
            :solid (make-luft-gemstone-gallery-scene)))
      (setf luft.render:*light* old-light
            luft.render:*flame-time* old-flame-time))))

(macrolet ((define-gemstone-width-capture (name width description)
             `(luv:define-capture ,name
                  (:figure G3MOPT :kind :image :extension "png"
                   :layout :landscape :description ,description)
                (pathname)
                (capture-luft-gemstone-gallery
                 pathname ,width
                 (luv.arithmetic.lisp.vec3:make-vec3 34.0 -7.0 16.0)
                 17.0 ,(format nil "LUFT gemstone gallery - bevel ~D" width)))))
  (define-gemstone-width-capture
      luft-gemstone-gallery-bevel-1 1
    "The fixed gemstone atlas with a narrow one-tick stone-crystal bezel.")
  (define-gemstone-width-capture
      luft-gemstone-gallery-bevel-2 2
    "The identical gemstone atlas with the two-tick stone-crystal bezel.")
  (define-gemstone-width-capture
      luft-gemstone-gallery-bevel-4 4
    "The identical gemstone atlas with the medial four-tick contact bezel."))

(luv:define-capture luft-gemstone-gallery-wall-row
    (:figure G3MOPT :kind :image :extension "png" :layout :landscape
     :description
     "A close view of a contiguous wall row and diamond-pattern crystal grips.")
  (pathname)
  (capture-luft-gemstone-gallery
   pathname 2
   (luv.arithmetic.lisp.vec3:make-vec3 29.0 5.0 15.0)
   10.5 "LUFT gemstone wall row" :yaw 2.05 :pitch -0.12))

(luv:define-capture luft-gemstone-gallery-hanging-and-grass
    (:figure G3MOPT :kind :image :extension "png" :layout :landscape
     :description
     "A close view comparing hanging, grass-borne, and stone-borne crystals.")
  (pathname)
  (capture-luft-gemstone-gallery
   pathname 2
   (luv.arithmetic.lisp.vec3:make-vec3 31.0 -3.0 18.0)
   11.0 "LUFT hanging and grass crystals" :yaw 2.24 :pitch -0.30))

(luv:define-capture luft-gemstone-gallery-grass-and-stone
    (:figure G3MOPT :kind :image :extension "png" :layout :landscape
     :description
     "A close view of crystal collars composed directly from grass and stone supports.")
  (pathname)
  (capture-luft-gemstone-gallery
   pathname 2
   (luv.arithmetic.lisp.vec3:make-vec3 30.0 -14.0 14.0)
   9.5 "LUFT grass and stone crystal grips" :yaw 2.18 :pitch -0.35))

(macrolet ((define-flame-phase-capture (name time description)
             `(luv:define-capture ,name
                  (:figure FL4MES :kind :image :extension "png"
                   :layout :landscape :description ,description)
                (pathname)
                (capture-luft-gemstone-gallery
                 pathname 2
                 (luv.arithmetic.lisp.vec3:make-vec3 34.0 2.0 7.0)
                 5.0 "LUFT volumetric torch flame phase"
                 :yaw 2.18 :pitch -0.35 :flame-time ,time))))
  (define-flame-phase-capture
      luft-gemstone-flame-phase-a 0.0
    "The fixed gemstone gallery at the authored start of its flame clock.")
  (define-flame-phase-capture
      luft-gemstone-flame-phase-b 0.65
    "The identical camera 0.65 seconds later in the volumetric flame field."))

(defun film-luft-gemstone-flames
    (pathname &key (seconds 4) (frame-rate 24))
  "Film one upright representative of the shared oriented flame volume."
  (let ((viewer nil)
        (old-light luft.render:*light*)
        (old-projection luft.render:*projection*)
        (old-isometric-height luft.render:*isometric-height*)
        (old-wireframe luft.render:*wireframe*)
        (old-inspection-ink-p luft.render:*inspection-ink-p*)
        (old-flame-time luft.render:*flame-time*))
    (unwind-protect
         (progn
           (setf luft.render:*light*
                 (make-luft-gemstone-capture-light old-light)
                 luft.render:*projection* :isometric
                 luft.render:*isometric-height* 6.0
                 luft.render:*wireframe* 0.0
                 luft.render:*inspection-ink-p* nil
                 luft.render:*flame-time* 0.0)
           (setf viewer
                 (luft.render:start-viewer
                  :solid (make-luft-gemstone-gallery-scene)
                  :bevel-width luft:+mesh-bevel-width+
                  :camera
                  (luft.render:make-fly-camera
                   :position
                   (luv.arithmetic.lisp.vec3:make-vec3 34.0 2.0 7.0)
                   :yaw 2.18 :pitch -0.35)
                  :title "LUFT volumetric torch flames"
                  :width 1100 :height 800))
           (luft.render:film-viewer
            viewer pathname :seconds seconds :frame-rate frame-rate
            :before-frame
            (lambda (frame)
              (setf luft.render:*flame-time* (/ frame (float frame-rate))))))
      (when viewer (luft.render:stop-viewer viewer))
      (setf luft.render:*light* old-light
            luft.render:*projection* old-projection
            luft.render:*isometric-height* old-isometric-height
            luft.render:*wireframe* old-wireframe
            luft.render:*inspection-ink-p* old-inspection-ink-p
            luft.render:*flame-time* old-flame-time))))

(luv:define-capture luft-gemstone-flames
    (:figure FL4MES :kind :video :extension "mp4" :layout :landscape
     :description
     "A close deterministic film of the SDF flame shared by all four gallery torch orientations.")
  (pathname)
  (film-luft-gemstone-flames pathname))

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

(defun capture-luft-sanctuary-tower (pathname bevel-width title)
  "Frame the sanctuary's ridge-beacon tower at one LUFT bevel width."
  (capture-luft-material-contact
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 112.0 50.0 42.0)
   10.0 title :yaw 2.2455373 :pitch -0.5165006
   :solid (luft.render:make-mountain-sanctuary-scene)
   :bevel-width bevel-width))

(luv:define-capture luft-sanctuary-tower-bevel-one
    (:figure T0W1R1 :kind :image :extension "png" :layout :landscape
     :description
     "The sanctuary ridge-beacon tower under a uniform one-eighth-cell bevel.")
  (pathname)
  (capture-luft-sanctuary-tower
   pathname 1 "LUFT sanctuary tower - bevel 1/8"))

(luv:define-capture luft-sanctuary-tower-bevel-two
    (:figure T0W2R2 :kind :image :extension "png" :layout :landscape
     :description
     "The same sanctuary ridge-beacon tower under a uniform quarter-cell bevel.")
  (pathname)
  (capture-luft-sanctuary-tower
   pathname 2 "LUFT sanctuary tower - bevel 1/4"))

(luv:define-capture luft-sanctuary-tower-bevel-four
    (:figure T0W4R4 :kind :image :extension "png" :layout :landscape
     :description
     "The same sanctuary ridge-beacon tower under a uniform half-cell medial bevel.")
  (pathname)
  (capture-luft-sanctuary-tower
   pathname 4 "LUFT sanctuary tower - bevel 1/2"))

(defun capture-luft-bevel-limit-cell (pathname width title)
  (let ((scene (luft.render:make-bevel-limit-study-scene)))
    (multiple-value-bind (mesh generation)
        (luft.render:make-render-mesh scene :bevel-width width)
      (declare (ignore generation))
      (multiple-value-bind (display-mesh display-generation)
          (luft.render::decorate-scene-mesh
           (luft:surface-mesh-with-triangle-ink mesh) scene)
        (capture-luft-material-contact
         pathname
         (luv.arithmetic.lisp.vec3:make-vec3 9.5 -0.5 5.75)
         1.35 title :yaw 2.0899425 :pitch -0.36
         :solid scene
         :surface-mesh display-mesh
         :surface-generation display-generation
         :bevel-width width :wireframe 1.0)))))

(luv:define-capture luft-bevel-limit-width-one-construction
    (:figure WSEK3C :kind :image :extension "png" :layout :landscape
     :description
     "One cell below the medial limit at width one, retaining faces, edge bands, and corner patches.")
  (pathname)
  (capture-luft-bevel-limit-cell
   pathname 1 "LUFT one-cell bevel width one"))

(luv:define-capture luft-bevel-limit-width-two-construction
    (:figure WSEK3C :kind :image :extension "png" :layout :landscape
     :description
     "The same cell at the ordinary width two, with narrower faces and broader bands.")
  (pathname)
  (capture-luft-bevel-limit-cell
   pathname 2 "LUFT one-cell bevel width two"))

(luv:define-capture luft-bevel-limit-width-four-construction
    (:figure WSEK3C :kind :image :extension "png" :layout :landscape
     :description
     "The same cell at the medial width four, where faces and edge bands have contracted away.")
  (pathname)
  (capture-luft-bevel-limit-cell
   pathname 4 "LUFT one-cell medial bevel width four"))

(defun capture-luft-manifold-spikes (pathname wireframe title)
  (let ((scene (luft.render:make-manifold-spike-scene)))
    (multiple-value-bind (mesh generation)
        (luft.render:make-render-mesh scene :bevel-width 2)
      (let ((display-mesh
              (if (plusp wireframe)
                  (luft:surface-mesh-with-triangle-ink mesh)
                  mesh)))
        (multiple-value-bind (display-mesh display-generation)
            (if (eq display-mesh mesh)
                (values display-mesh generation)
                (luft.render::decorate-scene-mesh display-mesh scene))
          (capture-luft-material-contact
           pathname
           (luv.arithmetic.lisp.vec3:make-vec3 7.6 3.1 9.9)
           6.5 title :yaw 0.7853982 :pitch -0.45
           :solid scene
           :surface-mesh display-mesh
           :surface-generation display-generation
           :bevel-width 2 :wireframe wireframe))))))

(luv:define-capture luft-manifold-spikes-clean
    (:figure WSEK3C :kind :image :extension "png" :layout :landscape
     :description
     "Edge-touching, corner-touching, and parity occupancy stars resolved into isolated manifold sheets.")
  (pathname)
  (capture-luft-manifold-spikes
   pathname 0.0 "LUFT singular occupancy stars"))

(luv:define-capture luft-manifold-spikes-construction
    (:figure WSEK3C :kind :image :extension "png" :layout :landscape
     :description
     "The three singular occupancy stars with their manifold-sheet triangulation exposed.")
  (pathname)
  (capture-luft-manifold-spikes
   pathname 1.0 "LUFT singular occupancy stars construction"))

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
                   :field-of-view (* 46.0 (/ pi 180.0)))))
           (setf luft.render:*projection* :perspective
                 luft.render:*wireframe* 0.0
                 luft.render:*inspection-ink-p* nil
                 luft.render:*character-time* nil)
           (setf viewer
                 (luft.render:start-viewer
                  :solid (luft.render:make-mountain-sanctuary-scene)
                  :bevel-width luft:+mesh-bevel-width+
                  :camera camera :title "LUFT wizard bridge walk"
                  :width 1280 :height 720))
           (luft.render:remove-simulation-character
            (luft.render:viewer-simulation viewer) (luft.render:viewer-player viewer))
           (setf (luft.render:viewer-player viewer)
                 (luft.render:make-walking-character
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
                       (luft.render:body-position (luft.render:character-body player)))
                     (player-y
                       (luv.arithmetic.lisp.vec3:vec3-y player-position))
                     (camera-position (luft.render:camera-position camera)))
                (luft.render:set-character-movement player 0.0 1.0)
                (luft.render:advance-world-simulation
                 (luft.render:viewer-simulation viewer) dt)
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
  (let ((canvas (luft.render::viewer-canvas viewer))
        (scene (luft.render::viewer-source viewer)))
    (loop for attempt below 240
          ;; Capture readiness must drive the same serialized native frame
          ;; transaction as the interactive cadence.  Sleeping on the capture
          ;; caller cannot itself schedule or publish a streaming cohort.
          for loaded =
            (progn
              (luv:request-canvas-frame
               canvas
               (lambda (timestamp)
                 (luft.render::render-viewer-frame viewer timestamp)))
              (hash-table-count
               (luft.render::streaming-scene-loaded scene)))
          for outstanding = (hash-table-count
                             (luft.render::streaming-scene-outstanding scene))
          when (and (plusp loaded) (zerop outstanding)
                    (null (luft.render::streaming-scene-cohort scene)))
            do (format t
                       "capture LUFT highlands: ready with ~D source chunks and ~D canonical owner slots~%"
                       loaded
                       (length
                        (luft.render::renderer-slot-order
                         (luft.render::viewer-renderer viewer))))
               (force-output)
               (return t)
          when (zerop (mod attempt 20))
            do (format t "capture LUFT highlands: ~D chunks loaded, ~D pending~%"
                       loaded outstanding)
               (force-output)
          do (sleep 0.05)
          finally (error "LUFT highland residency did not become ready."))))

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
     "The widened highland horizon through the same exact regional surface compiler at every distance.")
  (pathname)
  (capture-luft-highland-landscape
   pathname
   (luv.arithmetic.lisp.vec3:make-vec3 122.0 91.0 78.0)
   2.20 -0.20 "LUFT highland distance study"))

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
                    :camera camera :title "LUFT highland streaming flight"
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
     "A slow flight across exact regional meshes as chunk residency changes.")
  (pathname)
  (film-luft-highland-flight pathname))

;;; Native bevel/contact evidence.  These are deliberately more clinical than
;;; the broad gallery plates above: every clean/construction pair owns the same
;;; 2400 by 1600 camera, full-resolution raster, fixed light, and fixed flame
;;; phase.  The contact-width series changes only crystal relations; the
;;; terrain/architecture lanes stay fixed so an unrelated foundation cannot
;;; flare while a bezel is being judged.

(defconstant +luft-native-evidence-width+ 2400)
(defconstant +luft-native-evidence-height+ 1600)

(defun make-luft-native-evidence-light ()
  "Return the fully specified light shared by the native bevel evidence."
  (make-instance
   'luft.render:light
   :name :native-bevel-evidence
   :sun-direction
   (luv.arithmetic.lisp.vec3:make-vec3 -0.580319 0.360198 0.730402)
   :sun-color #(1.82 1.28 0.76 0.78)
   :sky-color #(0.22 0.34 0.68 1.0)
   :ground-color #(0.19 0.10 0.13 1.0)
   :shadow-half-extent 40.0
   :shadow-depth-radius 72.0
   :shadow-base-bias 0.00075
   :shadow-filter-radius 4.0))

(defun capture-luft-native-evidence
    (pathname scene position isometric-height title
     &key (yaw 2.18) (pitch -0.30) construction-p
       surface-mesh surface-generation
       (bevel-width 2) (flame-time 0.375) after-start
       (light (make-luft-native-evidence-light)))
  "Capture one native, temporally inert geometry plate under a fixed light."
  (let ((old-light luft.render:*light*)
        (old-flame-time luft.render:*flame-time*))
    (unwind-protect
         (progn
           (setf luft.render:*light* light
                 luft.render:*flame-time* flame-time)
           (capture-luft-material-contact
            pathname position isometric-height title
            :yaw yaw :pitch pitch :solid scene
            :surface-mesh surface-mesh
            :surface-generation surface-generation
            :bevel-width bevel-width
            :wireframe (if construction-p 1.0 0.0)
            :inspection-ink-p nil
            :width +luft-native-evidence-width+
            :height +luft-native-evidence-height+
            :render-scale 1.0
            :temporal-upscaling-p nil
            :after-start after-start
            :fixed-exposure 1.0))
      (setf luft.render:*light* old-light
            luft.render:*flame-time* old-flame-time))))

(defun make-luft-torch-frame-evidence-scene ()
  "Build one fixture spanning planar, band, convex, wall, and ceiling frames."
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 5)))
    ;; A fixed earth/stone floor gives the attachment fixture scene context.
    (luft.render::scene-builder-box builder 3 9 4 11 2 4)
    (luft.render::scene-builder-box
     builder 10 17 4 11 2 4 :architecture-p t)
    ;; A raised L supplies an ordinary convex/junction surface.
    (dolist (cell '((12 12 5) (13 12 5) (13 13 5)))
      (destructuring-bind (x y z) cell
        (luft.render::scene-builder-cell builder x y z :architecture-p t)))
    ;; Back wall and a deliberately narrow ceiling beam keep every attachment
    ;; visible from the context camera rather than hiding the floor tests.
    (luft.render::scene-builder-box
     builder 3 17 16 17 3 13 :architecture-p t)
    (luft.render::scene-builder-box
     builder 11 16 7 10 14 14 :architecture-p t)
    (luft.render::scene-builder-torch
     builder 6 7 4 :z :high :u 0.0 :v 0.0)
    ;; Low-Y outer edge: this chart point lies on a realized top/side band,
    ;; not on an internal cell edge continued by another top face.
    (luft.render::scene-builder-torch
     builder 8 4 4 :z :high :u 0.0 :v -1.0)
    ;; The raised L's +X/-Y corner has two exposed tangential directions and
    ;; therefore resolves onto its convex normal cone.
    (luft.render::scene-builder-torch
     builder 13 12 5 :z :high :u 1.0 :v -1.0)
    ;; Rotate the same actual band law onto the wall's low-X boundary.
    (luft.render::scene-builder-torch
     builder 3 16 9 :y :low :u -1.0 :v 0.0)
    ;; The downward face reverses its V chart axis to keep T/B/N right-handed;
    ;; negative V therefore reaches the beam's high-Y side.  Together with
    ;; positive U this is the actual high-X/high-Y convex corner.
    (luft.render::scene-builder-torch
     builder 16 10 14 :z :low :u 1.0 :v -1.0)
    (luft.render::finish-scene-builder builder)))

(defun make-luft-concave-step-torch-evidence-scene ()
  "Build the ordinary reentrant step from the attachment regression."
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 5)))
    ;; The support top remains exposed while its +X neighbor rises one cell.
    ;; U=0.6 is the former width-two normal-ray miss: local chart realization
    ;; now moves the socket 0.4 mesh tick onto the finished top patch.
    (dolist (cell '((6 7 4) (7 7 4) (7 7 5)))
      (destructuring-bind (x y z) cell
        (luft.render::scene-builder-cell
         builder x y z :architecture-p t)))
    (luft.render::scene-builder-torch
     builder 6 7 4 :z :high :u 0.6 :v 0.0)
    (luft.render::finish-scene-builder builder)))

(defconstant +luft-native-torch-close-isometric-height+ 2.0)

(defun luft-native-torch-close-camera-position
    (aim-x aim-y aim-z yaw pitch &optional (distance 8.0))
  "Return a position DISTANCE units behind one semantic chart point."
  (let ((horizontal (* distance (cos pitch))))
    (luv.arithmetic.lisp.vec3:make-vec3
     (- aim-x (* horizontal (cos yaw)))
     (- aim-y (* horizontal (sin yaw)))
     (- aim-z (* distance (sin pitch))))))

(defun make-luft-realized-light-direction-evidence-scene (propagation-p)
  "Build two neutral wall receivers for flat and diagonal torch light."
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 5)))
    ;; The floor and two-cell-thick limestone return are deliberately plain:
    ;; this plate tests realized torch light, not an emissive material contact.
    (luft.render::scene-builder-box
     builder 4 9 5 9 2 2 :architecture-p t)
    (luft.render::scene-builder-box
     builder 5 8 8 9 3 6 :architecture-p t)
    ;; The centered wall socket retains the authored -Y normal and seeds the
    ;; one air cell centered directly in front of it.
    (luft.render::scene-builder-torch
     builder 6 8 4 :y :low :u 0.0 :v 0.0)
    ;; The high-X edge of the same front wall is a finished width-two band.
    ;; Its +X/-Y normal moves the wick across the corner and produces three
    ;; quantized source cells beside the front and return receiver faces.
    (luft.render::scene-builder-torch
     builder 8 8 4 :y :low :u 1.0 :v 0.0)
    (luft.render::finish-scene-builder
     builder :voxel-light-propagation-p propagation-p)))

(defun capture-luft-realized-light-direction-evidence
    (pathname propagation-p title)
  "Capture the matched close realized-light direction fixture."
  (let ((yaw 2.18)
        (pitch -0.25))
    (capture-luft-native-evidence
     pathname
     (make-luft-realized-light-direction-evidence-scene propagation-p)
     (luft-native-torch-close-camera-position
      7.75 8.125 4.5 yaw pitch)
     2.0 title
     :yaw yaw :pitch pitch :bevel-width 2
     :light
     (make-luft-voxel-light-capture-light
      (make-luft-native-evidence-light)))))

(luv:define-capture luft-native-realized-torch-light-direction
    (:figure TRCHLT :kind :image :extension "png" :layout :landscape
     :description
     "A centered wall torch and an actual +X/-Y band torch lighting separate faces of the same neutral limestone receiver at their realized wick positions.")
  (pathname)
  (capture-luft-realized-light-direction-evidence
   pathname t "Native realized torch light direction"))

(luv:define-capture luft-native-realized-torch-light-propagation-off
    (:figure TRCHLT :kind :image :extension "png" :layout :landscape
     :description
     "The identical close camera, geometry, dim environment, exposure, and flame phase with immutable torch-light propagation disabled; direct body and flame emission remain.")
  (pathname)
  (capture-luft-realized-light-direction-evidence
   pathname nil "Native realized torch light - propagation off"))

(defun make-luft-opaque-bevel-occlusion-evidence-scene ()
  "Place one floor torch partly behind an ordinary opaque bevel edge."
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 5)))
    ;; The rear limestone floor owns the torch.  A connected, opaque return in
    ;; front of it ends at X=7; its finished high-X/front-Y width-two band is
    ;; the only foreground silhouette crossing the flame and body.
    (luft.render::scene-builder-box
     builder 5 9 7 10 2 2 :architecture-p t)
    (luft.render::scene-builder-box
     builder 5 6 6 6 2 5 :architecture-p t)
    ;; The off-centre chart coordinate puts the socket centre at X=7.1.  The
    ;; opaque silhouette at X=7 therefore removes a visible slice without
    ;; hiding the complete socket or flame.
    (luft.render::scene-builder-torch
     builder 7 8 2 :z :high :u -0.8 :v 0.0)
    (luft.render::finish-scene-builder builder)))

(defun capture-luft-opaque-bevel-occlusion-evidence
    (pathname construction-p title)
  "Capture the matched opaque-depth torch fixture at original-pixel scale."
  (let ((yaw 1.5707963)
        (pitch -0.12))
    ;; At 2400 by 1600 and height 1.9 the audited socket is 244 by 156
    ;; pixels, the complete body is 244 by 438, and the foreground bevel band
    ;; is 211 pixels wide.  Its X=7 silhouette crosses both proxy and body.
    (capture-luft-native-evidence
     pathname (make-luft-opaque-bevel-occlusion-evidence-scene)
     (luft-native-torch-close-camera-position
      7.0 8.0 3.5 yaw pitch)
     1.9 title
     :yaw yaw :pitch pitch :bevel-width 2
     :construction-p construction-p)))

(luv:define-capture luft-native-torch-opaque-bevel-occlusion
    (:figure TRCHDP :kind :image :extension "png" :layout :landscape
     :description
     "A floor torch partly hidden by the finished opaque bevel silhouette of an ordinary limestone return, exercising body depth and flame depth clipping together.")
  (pathname)
  (capture-luft-opaque-bevel-occlusion-evidence
   pathname nil "Native torch opaque bevel occlusion - clean"))

(luv:define-capture luft-native-torch-opaque-bevel-occlusion-construction
    (:figure TRCHDP :kind :image :extension "png" :layout :landscape
     :description
     "The identical close camera, exposure, geometry, and flame phase with construction ink exposing the opaque foreground band and the torch's finished support face.")
  (pathname)
  (capture-luft-opaque-bevel-occlusion-evidence
   pathname t "Native torch opaque bevel occlusion - construction"))

(macrolet
    ((define-native-torch-close-pair
         (clean-name construction-name aim yaw pitch bevel-width title description)
       (destructuring-bind (x y z) aim
         `(progn
            (luv:define-capture ,clean-name
                (:figure TRCHFR :kind :image :extension "png"
                 :layout :landscape :description ,description)
              (pathname)
              (capture-luft-native-evidence
               pathname (make-luft-torch-frame-evidence-scene)
               (luft-native-torch-close-camera-position
                ,x ,y ,z ,yaw ,pitch)
               +luft-native-torch-close-isometric-height+
               ,(format nil "~A - clean" title)
               :yaw ,yaw :pitch ,pitch :bevel-width ,bevel-width))
            (luv:define-capture ,construction-name
                (:figure TRCHFR :kind :image :extension "png"
                 :layout :landscape
                 :description
                 ,(format nil "~A Identical close camera with construction ink." description))
              (pathname)
              (capture-luft-native-evidence
               pathname (make-luft-torch-frame-evidence-scene)
               (luft-native-torch-close-camera-position
                ,x ,y ,z ,yaw ,pitch)
               +luft-native-torch-close-isometric-height+
               ,(format nil "~A - construction" title)
               :yaw ,yaw :pitch ,pitch :bevel-width ,bevel-width
               :construction-p t))))))
  ;; The two-world-unit vertical frame makes every audited socket footprint at
  ;; least 188 pixels in both screen axes.  Each pair isolates one semantic
  ;; chart point; the broader width-series plates above retain scene context.
  (define-native-torch-close-pair
      luft-native-torch-flat-face-close
      luft-native-torch-flat-face-close-construction
      (6.5 7.5 5.0) 2.18 -0.42 2 "Native torch flat face"
    "The centered floor torch on an actual width-two planar face.")
  (define-native-torch-close-pair
      luft-native-torch-floor-band-close
      luft-native-torch-floor-band-close-construction
      (8.5 4.0 5.0) 1.5707963 -0.62 2 "Native torch floor band"
    "The low-Y floor torch on its actual width-two top/side diagonal band.")
  (define-native-torch-close-pair
      luft-native-torch-convex-fan-close
      luft-native-torch-convex-fan-close-construction
      (13.75 12.25 6.0) 2.3561945 -0.58 3 "Native torch convex fan"
    "The raised-L corner torch on its actual width-three three-axis junction fan.")
  (define-native-torch-close-pair
      luft-native-torch-wall-band-close
      luft-native-torch-wall-band-close-construction
      (3.0 16.0 9.5) 0.7853982 -0.20 2 "Native torch wall band"
    "The wall torch on its actual width-two low-X/low-Y diagonal band.")
  (define-native-torch-close-pair
      luft-native-torch-ceiling-fan-close
      luft-native-torch-ceiling-fan-close-construction
      (16.75 10.75 14.0) 3.9269908 0.58 3
      "Native torch ceiling fan"
    "The downward torch on its actual width-three high-X/high-Y junction fan."))

(luv:define-capture luft-native-torch-concave-step-close
    (:figure TRCHFR :kind :image :extension "png" :layout :landscape
     :description
     "An isolated width-two reentrant step at the former U=0.6 ray miss, with the socket realized onto the nearest finished local support patch.")
  (pathname)
  (let ((yaw 1.5707963) (pitch -0.50))
    (capture-luft-native-evidence
     pathname (make-luft-concave-step-torch-evidence-scene)
     (luft-native-torch-close-camera-position
      6.75 7.5 5.0 yaw pitch)
     +luft-native-torch-close-isometric-height+
     "Native torch concave step - clean"
     :yaw yaw :pitch pitch :bevel-width 2)))

(luv:define-capture luft-native-torch-concave-step-close-construction
    (:figure TRCHFR :kind :image :extension "png" :layout :landscape
     :description
     "The identical isolated concave-step camera with construction ink exposing the selected finished support triangles.")
  (pathname)
  (let ((yaw 1.5707963) (pitch -0.50))
    (capture-luft-native-evidence
     pathname (make-luft-concave-step-torch-evidence-scene)
     (luft-native-torch-close-camera-position
      6.75 7.5 5.0 yaw pitch)
     +luft-native-torch-close-isometric-height+
     "Native torch concave step - construction"
     :yaw yaw :pitch pitch :bevel-width 2 :construction-p t)))

(defun film-luft-native-torch-frames
    (pathname &key (seconds 3) (frame-rate 24))
  "Film the complete torch fixture with an exact authored flame clock."
  (let ((viewer nil)
        (old-light luft.render:*light*)
        (old-projection luft.render:*projection*)
        (old-isometric-height luft.render:*isometric-height*)
        (old-wireframe luft.render:*wireframe*)
        (old-inspection-ink-p luft.render:*inspection-ink-p*)
        (old-render-scale luft.render::*render-scale*)
        (old-temporal-upscaling-p luft.render::*temporal-upscaling-p*)
        (old-flame-time luft.render:*flame-time*))
    (unwind-protect
         (progn
           (setf luft.render:*light* (make-luft-native-evidence-light)
                 luft.render:*projection* :isometric
                 luft.render:*isometric-height* 11.0
                 luft.render:*wireframe* 0.0
                 luft.render:*inspection-ink-p* nil
                 luft.render::*render-scale* 1.0
                 luft.render::*temporal-upscaling-p* nil
                 luft.render:*flame-time* 0.0)
           (setf viewer
                 (luft.render:start-viewer
                  :solid (make-luft-torch-frame-evidence-scene)
                  :bevel-width 2
                  :camera
                  (luft.render:make-fly-camera
                   :position
                   (luv.arithmetic.lisp.vec3:make-vec3 20.5 -4.0 12.0)
                   :yaw 2.18 :pitch -0.22)
                  :fixed-exposure 1.0
                  :title "Native deterministic torch frames"
                  :width +luft-native-evidence-width+
                  :height +luft-native-evidence-height+))
           (luft.render:film-viewer
            viewer pathname :seconds seconds :frame-rate frame-rate
            :before-frame
            (lambda (frame)
              (setf luft.render:*flame-time*
                    (/ frame (float frame-rate 1.0f0))))))
      (when viewer (luft.render:stop-viewer viewer))
      (setf luft.render:*light* old-light
            luft.render:*projection* old-projection
            luft.render:*isometric-height* old-isometric-height
            luft.render:*wireframe* old-wireframe
            luft.render:*inspection-ink-p* old-inspection-ink-p
            luft.render::*render-scale* old-render-scale
            luft.render::*temporal-upscaling-p* old-temporal-upscaling-p
            luft.render:*flame-time* old-flame-time))))

(luv:define-capture luft-native-torch-frames-film
    (:figure TRCHMV :kind :video :extension "mp4" :layout :landscape
     :description
     "The native fixed-camera torch fixture with deterministic 24 Hz flame time and temporal reconstruction disabled.")
  (pathname)
  (film-luft-native-torch-frames pathname))

(luv:define-capture luft-native-chunk-seam-streaming-async
    (:figure STRMSM :kind :image :extension "png" :layout :landscape
     :description
     "The same seam after the viewer asynchronously publishes its canonical owner cohort.")
  (pathname)
  (capture-luft-chunk-seam-async-evidence
   pathname nil
   "The production async streaming seam after atomic owner publication."))

(luv:define-capture luft-native-chunk-seam-streaming-async-construction
    (:figure STRMSM :kind :image :extension "png" :layout :landscape
     :description
     "The async viewer seam with construction ink exposing every published owner triangle.")
  (pathname)
  (capture-luft-chunk-seam-async-evidence
   pathname t
   "The production async streaming seam with regional construction ink."))
