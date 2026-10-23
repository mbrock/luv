(in-package #:luft.render)

;;; A frame is an ordered composition. Shadows feed scene lighting; temporal
;;; reconstruction consumes geometry depth and motion; flames enter the HDR
;;; image afterward; exposure measures that image; presentation grades it and
;;; places the application overlay last. Helpers below follow that same order.
;;; Resource ownership and replacement live in render.lisp. See #I4PRMD.

;;; Chunk culling.  The frame's own uniform data names both clip volumes: the
;;; camera lanes as the terrain vertex stage projects them, and the sun's
;;; four light rows.

(defvar *frame-uniform-data* nil
  "The scene uniform floats of the frame being encoded, for culling.")

(defvar *renderer-scene-overlay-encoder* nil
  "Optional host replay callback, called with the open depth-tested scene pass.")
(defvar *renderer-world-panel-encoder* nil
  "Optional host replay callback for native-density world panels.")

(defun camera-clip-function (data)
  "Project a world point to the camera's homogeneous clip, as MESH-VIEW-CLIP."
  (let ((divisor (aref data 22)))
    (lambda (x y z)
      (let* ((rx (- x (aref data 0))) (ry (- y (aref data 1))) (rz (- z (aref data 2)))
             (view-x (+ (* rx (aref data 4)) (* ry (aref data 5)) (* rz (aref data 6))))
             (view-y (+ (* rx (aref data 8)) (* ry (aref data 9)) (* rz (aref data 10))))
             (view-z (+ (* rx (aref data 12)) (* ry (aref data 13)) (* rz (aref data 14)))))
        (values (* view-x (aref data 16))
                (- (* view-y (aref data 17)))
                (+ (* view-z (aref data 18)) (aref data 19))
                (+ 1.0 (* (- view-z 1.0) divisor)))))))

(defun light-clip-function (data)
  "Project a world point through the sun's four light rows, as LIGHT-CLIP-POSITION."
  (flet ((row (base x y z)
           (+ (* x (aref data base)) (* y (aref data (+ base 1)))
              (* z (aref data (+ base 2))) (aref data (+ base 3)))))
    (lambda (x y z)
      (values (row 72 x y z) (row 76 x y z) (row 80 x y z) (row 84 x y z)))))

(defun box-outside-clip-p (bounds clip)
  "Whether all eight corners of BOUNDS lie outside one plane of CLIP's volume."
  (let ((outside (make-array 6 :initial-element 0)))
    (dolist (x (list (aref bounds 0) (aref bounds 3)))
      (dolist (y (list (aref bounds 1) (aref bounds 4)))
        (dolist (z (list (aref bounds 2) (aref bounds 5)))
          (multiple-value-bind (cx cy cz cw) (funcall clip x y z)
            (when (> cx cw) (incf (aref outside 0)))
            (when (< cx (- cw)) (incf (aref outside 1)))
            (when (> cy cw) (incf (aref outside 2)))
            (when (< cy (- cw)) (incf (aref outside 3)))
            (when (< cz 0.0) (incf (aref outside 4)))
            (when (> cz cw) (incf (aref outside 5)))))))
    (find 8 outside)))

(defun resident-drawn-p (resident shadow-p)
  "Whether this frame's SHADOW-P pass may see RESIDENT's terrain."
  (let ((bounds (resident-population-bounds resident))
        (data *frame-uniform-data*))
    (and bounds
         (or (not *terrain-culling-p*)
             (null data)
             (not (box-outside-clip-p
                   bounds
                   (if shadow-p (light-clip-function data) (camera-clip-function data))))))))

(defun encode-renderer-frame
    (renderer frame encoder surface-texture extent camera-uniform-data
     &key jitter view player-p construction-p overlay-encoder
       (effect-time
         (or *flame-time* (/ (renderer-frame-index renderer) 60.0))))
  (ensure-renderer-extent renderer extent)
  (upload-renderer-frame renderer frame camera-uniform-data effect-time)
  (let ((*frame-uniform-data* camera-uniform-data))
    (encode-renderer-passes renderer frame encoder surface-texture jitter view
                            player-p construction-p overlay-encoder))
  (incf (renderer-frame-index renderer))
  renderer)

(defun encode-renderer-view
    (renderer context encoder surface-texture surface-view extent camera
     &key (projection *projection*) figures (pointer '(0.5 0.5))
       (ink-strength 0.0) (bevel-width luft:+mesh-bevel-width+)
       fixed-exposure first-person-p construction-p overlay-encoder
       (effect-time
         (or *flame-time* (/ (renderer-frame-index renderer) 60.0))))
  "Encode one frame of RENDERER seen through CAMERA into SURFACE-VIEW.

This is a game's whole frame: the camera's view under PROJECTION, temporal
jitter and history continuity, exposure, and the scene uniforms, then every
pass.  SURFACE-TEXTURE is the presentation slot from CONTEXT that
SURFACE-VIEW views, and EXTENT its size in pixels. FIGURES is a list of
per-instance records, from FIGURE-LANES or BOX-FIGURE. POINTER, as fractions of the screen,
and INK-STRENGTH fill the inspection lanes.  FIXED-EXPOSURE replaces
measured exposure.  OVERLAY-ENCODER receives the
final pass."
  (ensure-renderer-extent renderer extent)
  (let* ((*projection* projection)
         (frame (renderer-frame-state-for renderer context surface-texture))
         (render-extent (renderer-render-scale-extent renderer extent))
         (width (first render-extent))
         (height (second render-extent))
         (jitter (if (renderer-temporal-p renderer)
                     (temporal-jitter (renderer-frame-index renderer)
                                      width height)
                     #(0.0 0.0)))
         (view (capture-frame-view camera width height jitter))
         (previous (or (renderer-previous-view renderer) view))
         (exposure
           (* *exposure-gain*
              (or fixed-exposure
                  (advance-exposure (renderer-exposure-control renderer))))))
    (unless (frame-views-continuous-p (renderer-previous-view renderer) view)
      (setf (renderer-history-valid-p renderer) nil))
    (when (typep (renderer-player renderer) 'figure-drawing)
      (upload-figure-instances
       (renderer-player renderer)
       (renderer-frame-drawing-binding renderer frame (renderer-player renderer))
       (pack-figure-instances figures)))
    (encode-renderer-frame
     renderer frame encoder surface-view extent
     (camera-uniform-data
      view previous
      (vector (coerce (first pointer) 'single-float)
              (coerce (second pointer) 'single-float)
              (coerce (/ width) 'single-float)
              (coerce (/ height) 'single-float))
      ink-strength (first (first figures)) bevel-width exposure first-person-p)
     :jitter jitter :view view
     :player-p (and figures t)
     :effect-time effect-time
     :construction-p construction-p
     :overlay-encoder overlay-encoder)))

(defun encode-renderer-passes
    (renderer frame encoder surface-texture jitter view player-p construction-p
     overlay-encoder)
  (dotimes (repetition (1- *benchmark-pass-repetitions*))
    (encode-renderer-shadows renderer frame encoder)
    (encode-renderer-scene renderer frame encoder player-p nil :repetition-p t))
  (encode-renderer-shadows renderer frame encoder)
  (encode-renderer-scene renderer frame encoder player-p construction-p)
  (when *renderer-scene-overlay-encoder*
    ;; Retained application shaders have one color output and one sample;
    ;; keep them independent of terrain's MSAA and temporal MRT contracts.
    (let ((pass
            (begin-render-pass
             encoder
             (make-render-pass-descriptor
              :label "world instruments"
              :color-attachments
              `((:view ,(renderer-scene-view renderer)
                 :load-op :load :store-op :store))
              :depth-stencil-attachment
              `(:view ,(renderer-depth-view renderer)
                :depth-load-op :load :depth-store-op :store)))))
      (funcall *renderer-scene-overlay-encoder* pass)
      (when (renderer-metalfx-temporal-p renderer)
        (signal-temporal-scaler-inputs pass (renderer-temporal-scaler renderer)))
      (end-pass pass)))
  (encode-renderer-reconstruction renderer frame encoder jitter view)
  (encode-renderer-composite renderer frame encoder)
  (encode-exposure (renderer-exposure-control renderer) encoder
                   (renderer-exposure-binding renderer)
                   (renderer-frame-index renderer))
  (encode-renderer-lens renderer frame encoder)
  (let ((pass
          (begin-render-pass
           encoder
           (make-render-pass-descriptor
            :label "native world panels"
            :color-attachments
            `((:view ,(renderer-panel-color-view renderer)
               :load-op :clear :store-op :store :clear-value #(0.0 0.0 0.0 0.0)))
            :depth-stencil-attachment
            `(:view ,(renderer-panel-depth-view renderer)
              :depth-load-op :clear :depth-store-op :store :depth-clear-value 1.0)))))
    (when *renderer-world-panel-encoder* (funcall *renderer-world-panel-encoder* pass))
    (end-pass pass))
  (prepare-texture encoder (renderer-panel-color-texture renderer) :texture-binding)
  (prepare-texture encoder (renderer-panel-depth-texture renderer) :texture-binding)
  (encode-renderer-presentation renderer frame encoder surface-texture overlay-encoder))

(defun upload-renderer-frame (renderer frame camera-uniform-data effect-time)
  (when (renderer-shader-temporal-p renderer)
    ;; These W components are padding to every geometry consumer.  The shader
    ;; resolve reads them as its per-frame validity and accumulation weight.
    (setf (aref camera-uniform-data 27)
          (if (renderer-history-valid-p renderer) 1.0f0 0.0f0)
          (aref camera-uniform-data 31) *temporal-history-weight*))
  ;; The lens-extent row describes images this renderer owns, so it is
  ;; written here rather than by the viewer that packed the rest.
  (let ((extent (renderer-extent renderer))
        (bloom (bloom-extent (renderer-extent renderer))))
    (setf (aref camera-uniform-data 120) (/ 1.0f0 (first bloom))
          (aref camera-uniform-data 121) (/ 1.0f0 (second bloom))
          (aref camera-uniform-data 122) (/ 1.0f0 (first extent))
          (aref camera-uniform-data 123) (/ 1.0f0 (second extent))))
  (write-buffer (renderer-frame-state-camera-buffer frame) camera-uniform-data)
  (upload-torch-frame (renderer-torches renderer)
                      (renderer-frame-state-flame-effect-buffer frame) effect-time))

(defun encode-renderer-shadows (renderer frame encoder)
  (let ((shadow-pass
          (begin-render-pass
           encoder
           (make-render-pass-descriptor
            :label "luft sun shadow"
            :color-attachments nil
            :depth-stencil-attachment
            `(:view ,(renderer-shadow-view renderer)
              :depth-load-op :clear :depth-store-op :store
              :depth-clear-value 1.0)))))
    (dolist (key (and (member *benchmark-terrain-p* '(t :shadow))
                      (renderer-slot-order renderer)))
      (let ((resident
              (mesh-slot-resident
               (gethash key (renderer-mesh-slots renderer)))))
        (when (resident-drawn-p resident t)
          (draw-resident-opaque-population
           (renderer-terrain renderer) shadow-pass resident
           (renderer-frame-resident-bind-group
            renderer frame resident t) :shadow-p t)
          (draw-resident-fixture-bodies renderer frame shadow-pass resident
                                        :shadow-p t))))
    (when (plusp (renderer-flame-instance-count renderer))
      (encode-torch-bodies
       (renderer-torches renderer) shadow-pass
       (renderer-frame-torch-body-bind-group renderer frame t)
       (renderer-flame-instance-count renderer) :shadow-p t))
    (encode-scene-drawing-shadow
     (renderer-player renderer) shadow-pass
     (renderer-frame-drawing-binding renderer frame (renderer-player renderer)))
    (end-pass shadow-pass))
  (prepare-texture encoder (renderer-shadow-texture renderer)
                   :texture-binding))

(defun encode-renderer-scene
    (renderer frame encoder player-p construction-p &key repetition-p)
  (flet ((target (multisampled single clear)
           ;; A temporal scene has one sample and draws straight into the
           ;; images reconstruction reads; otherwise MSAA resolves into them.
           (if multisampled
               `(:view ,multisampled :resolve-view ,single
                 :load-op :clear :store-op :store :clear-value ,clear)
               `(:view ,single :load-op :clear :store-op :store
                 :clear-value ,clear))))
    (let* ((color-attachments
             (cons (target (renderer-scene-msaa-view renderer)
                           (renderer-scene-view renderer) #(0.0 0.0 0.0 1.0))
                   (when (renderer-temporal-p renderer)
                     (list (target (renderer-motion-msaa-view renderer)
                                   (renderer-motion-view renderer)
                                   #(0.0 0.0 0.0 0.0))))))
           (depth-msaa-view (renderer-depth-msaa-view renderer))
           (pass
             (begin-render-pass
              encoder
              (make-render-pass-descriptor
               :label "luft site streams"
               :color-attachments color-attachments
               :depth-stencil-attachment
               `(:view ,(or depth-msaa-view (renderer-depth-view renderer))
                 ,@(when depth-msaa-view
                     `(:resolve-view ,(renderer-depth-view renderer)))
                 :depth-load-op :clear
                 :depth-store-op :store
                 :depth-clear-value 1.0)))))
      (encode-renderer-scene-drawings renderer frame pass player-p construction-p
                                      repetition-p))))

(defun encode-renderer-scene-drawings
    (renderer frame pass player-p construction-p &optional repetition-p)
  ;; The atmosphere is scene-linear world radiance: geometry overwrites it,
  ;; the selected temporal implementation reconstructs it, and the exposure
  ;; probe meters the same pixels presentation will grade.
  (encode-scene-drawing
   (renderer-sky renderer) pass
   (renderer-frame-drawing-binding renderer frame (renderer-sky renderer)))
  (dolist (key (and (member *benchmark-terrain-p* '(t :scene))
                    (renderer-slot-order renderer)))
    (let ((resident
            (mesh-slot-resident
             (gethash key (renderer-mesh-slots renderer)))))
      (when (resident-drawn-p resident nil)
        (draw-resident-opaque-population
         (renderer-terrain renderer) pass resident
         (renderer-frame-resident-bind-group renderer frame resident nil))
        (draw-resident-fixture-bodies renderer frame pass resident))))
  (when (plusp (renderer-flame-instance-count renderer))
    (encode-torch-bodies
     (renderer-torches renderer) pass
     (renderer-frame-torch-body-bind-group renderer frame nil)
     (renderer-flame-instance-count renderer)))
  ;; Text and panels stand on surfaces the opaque world has already drawn.
  (encode-renderer-world-text renderer frame pass)
  (when player-p
    (encode-scene-drawing
     (renderer-player renderer) pass
     (renderer-frame-drawing-binding renderer frame (renderer-player renderer))))
  (when (and construction-p (renderer-lattice renderer))
    ;; Populate at most one diagnostic slot per frame. The overlay is a
    ;; debugging view, so progressive readiness is preferable to freezing
    ;; one frame while every resident chunk is scanned.
    (loop for key in (renderer-slot-order renderer)
          for slot = (gethash key (renderer-mesh-slots renderer))
          unless (mesh-slot-lattice-point-buffer slot)
            do (ensure-mesh-slot-lattice-points renderer slot)
               (return))
    (dolist (key (renderer-slot-order renderer))
      (let ((slot (gethash key (renderer-mesh-slots renderer))))
        (when (plusp (mesh-slot-lattice-point-count slot))
          (encode-program (renderer-lattice renderer) pass
                          (renderer-frame-lattice-bind-group renderer frame slot)
                          (make-gpu-draw-command :vertex-count 6
                                                 :instance-count (mesh-slot-lattice-point-count slot)))))))
  ;; A benchmark repetition is not this frame's input to the scaler.
  (when (and (renderer-metalfx-temporal-p renderer) (not repetition-p)
             (not *renderer-scene-overlay-encoder*))
    (signal-temporal-scaler-inputs pass
                                   (renderer-temporal-scaler renderer)))
  (end-pass pass))

(defun encode-renderer-reconstruction (renderer frame encoder jitter view)
  (when (renderer-metalfx-temporal-p renderer)
    (let ((scaler (renderer-temporal-scaler renderer))
          (history-valid-p (renderer-history-valid-p renderer))
          (render-extent (renderer-render-extent renderer)))
      (encode-temporal-scale
       encoder scaler
       (renderer-scene-texture renderer)
       (renderer-depth-texture renderer)
       (renderer-motion-texture renderer)
       (renderer-resolved-texture renderer)
       ;; JITTER is clip-space at the internal scene resolution.  MetalFX
       ;; takes the same offset in input pixels—not output pixels—so using
       ;; EXTENT here overstates it whenever temporal upscaling is active.
       (vector (* 0.5 (first render-extent) (aref jitter 0))
               (* 0.5 (second render-extent) (aref jitter 1)))
       (not history-valid-p))
      (setf (renderer-previous-view renderer) view
            (renderer-history-valid-p renderer) t
            (renderer-history-used-p renderer) history-valid-p)))
  (when (renderer-shader-temporal-p renderer)
    (let ((history-valid-p (renderer-history-valid-p renderer)))
      (dolist (texture (list (renderer-scene-texture renderer)
                             (renderer-motion-texture renderer)
                             (renderer-depth-texture renderer)))
        (prepare-texture encoder texture :texture-binding))
      (let ((pass
              (begin-render-pass
               encoder
               (make-render-pass-descriptor
                :label "luft temporal neighbourhood"
                :color-attachments
                `((:view ,(renderer-bounds-low-view renderer)
                   :load-op :clear :store-op :store
                   :clear-value #(0.0 0.0 0.0 0.0))
                  (:view ,(renderer-bounds-high-view renderer)
                   :load-op :clear :store-op :store
                   :clear-value #(0.0 0.0 0.0 0.0)))))))
        (encode-reconstruction-neighbourhood
         (renderer-reconstruction renderer) pass
         (renderer-frame-neighbourhood-bind-group renderer frame))
        (end-pass pass))
      (prepare-texture encoder (renderer-bounds-low-texture renderer)
                       :texture-binding)
      (prepare-texture encoder (renderer-bounds-high-texture renderer)
                       :texture-binding)
      ;; History is weighted out when invalid, but never-written memory may
      ;; hold NaNs, which survive any weight.
      (unless history-valid-p
        (encode encoder
                (make-gpu-clear-texture-command
                 :texture (renderer-history-texture renderer)
                 :color #(0.0 0.0 0.0 0.0))))
      (prepare-texture encoder (renderer-history-texture renderer)
                       :texture-binding)
      ;; The two output images alternate roles each frame, so resolve never
      ;; reads the image it writes and nothing is copied.
      (let ((resolve-pass
              (begin-render-pass
               encoder
               (make-render-pass-descriptor
                :label "luft temporal resolve"
                :color-attachments
                `((:view ,(renderer-resolved-view renderer)
                   :load-op :clear :store-op :store
                   :clear-value #(0.0 0.0 0.0 1.0)))))))
        (encode-reconstruction-resolve
         (renderer-reconstruction renderer) resolve-pass
         (renderer-frame-temporal-bind-group renderer frame))
        (end-pass resolve-pass))
      (prepare-texture encoder (renderer-resolved-texture renderer)
                       :texture-binding)
      (setf (renderer-previous-view renderer) view
            (renderer-history-valid-p renderer) t
            (renderer-history-used-p renderer) history-valid-p)))
  (unless (renderer-temporal-p renderer)
    (prepare-texture encoder (renderer-scene-texture renderer)
                     :texture-binding)))

(defun encode-renderer-composite (renderer frame encoder)
  ;; Depth is read only after both the scene pass and the temporal encoder
  ;; have consumed it.  It is deliberately not attached to the following
  ;; pass, so sampling it while writing the distinct HDR composite cannot
  ;; form a Metal or Vulkan read/write texture hazard.
  (prepare-texture encoder (renderer-depth-texture renderer)
                   :texture-binding)
  (let ((composite-pass
          (begin-render-pass
           encoder
           (make-render-pass-descriptor
            :label "luft post-temporal HDR flame composite"
            :color-attachments
            `((:view ,(renderer-composite-view renderer)
               :load-op :clear :store-op :store
               :clear-value #(0.0 0.0 0.0 1.0)))))))
    (when (renderer-metalfx-temporal-p renderer)
      (wait-temporal-scaler-output
       composite-pass (renderer-temporal-scaler renderer)))
    (encode-composite (renderer-finishing renderer) composite-pass
                      (renderer-composite-source-bind-group renderer))
    (when (plusp (renderer-flame-instance-count renderer))
      (encode-torch-flames
       (renderer-torches renderer) composite-pass
       (renderer-frame-flame-bind-group renderer frame)
       (renderer-flame-instance-count renderer)))
    (end-pass composite-pass))
  (prepare-texture encoder (renderer-composite-texture renderer)
                   :texture-binding))

(defun encode-renderer-lens (renderer frame encoder)
  "Bright pass, two separable blur pairs, and sun shafts over the composite.
Each stage is one fullscreen triangle into a reduced image; the bloom ends in
the primary and the shafts in the secondary for presentation to add back."
  (let ((primary (renderer-bloom-primary-view renderer))
        (primary-texture (renderer-bloom-primary-texture renderer))
        (secondary (renderer-bloom-secondary-view renderer))
        (secondary-texture (renderer-bloom-secondary-texture renderer))
        (finishing (renderer-finishing renderer)))
    (flet ((stage (stage source target target-texture)
             (let ((pass
                     (begin-render-pass
                      encoder
                      (make-render-pass-descriptor
                       :label (format nil "luft lens ~(~A~)" stage)
                       :color-attachments
                       `((:view ,target :load-op :clear :store-op :store
                          :clear-value #(0.0 0.0 0.0 1.0)))))))
               (encode-lens-stage finishing stage pass
                                  (renderer-frame-lens-bind-group
                                   renderer frame stage source))
               (end-pass pass))
             (prepare-texture encoder target-texture :texture-binding)))
      (stage :bright (renderer-composite-view renderer) primary primary-texture)
      ;; Running the separable pair twice convolves the kernel with itself:
      ;; a glow wide enough to read as light rather than as an outline.
      (dotimes (iteration 2)
        (stage :horizontal primary secondary secondary-texture)
        (stage :vertical secondary primary primary-texture))
      (stage :shafts primary secondary secondary-texture))))

(defun encode-renderer-presentation
    (renderer frame encoder surface-texture overlay-encoder)
  (let ((present-pass
          (begin-render-pass
           encoder
           (make-render-pass-descriptor
            :label "luft HDR presentation"
            :color-attachments
            `((:view ,surface-texture :load-op :clear :store-op :store
               :clear-value #(0.0 0.0 0.0 1.0)))))))
    (encode-presentation (renderer-finishing renderer) present-pass
                         (renderer-frame-present-bind-group renderer frame))
    ;; Both MetalFX and the direct HDR path publish here.  The atelier
    ;; overlay remains later than tone mapping and glow in either case.
    (when overlay-encoder
      (funcall overlay-encoder present-pass))
    (end-pass present-pass)))
