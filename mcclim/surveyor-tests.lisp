(defpackage #:mcluv.surveyor-tests
  (:use #:cl #:rove))

(in-package #:mcluv.surveyor-tests)

(defclass direct-overlay-preparation-probe
    (mcluv:luvcraft-world-widget-overlay)
  ((observations :initform nil
                 :accessor direct-overlay-preparation-observations)))

(defmethod mcluv::prepare-mirror-compositor-target-revision
    ((overlay direct-overlay-preparation-probe)
     (mirror mcluv:luv-gpu-mirror) revision
     &key target-format depth-stencil)
  (declare (ignore mirror))
  (push (list revision target-format depth-stencil)
        (direct-overlay-preparation-observations overlay)))

(deftest static-luvcraft-world-overlay-prepares-its-hdr-panel-target
  (let* ((mirror
           (make-instance 'mcluv:luv-gpu-mirror
                          :sheet nil :target nil :context nil))
         (revision
           (mcluv::make-gpu-mirror-prepared-revision
            mirror nil #() #() #() #() #() #()))
         (overlay
           (make-instance 'direct-overlay-preparation-probe
                          :session nil :frame nil :mirror mirror))
         (expected
            (list
             (list revision :rgba16-float
                   '(:format :depth32-float
                     :depth-write-enabled t
                     :depth-compare :always)))))
    ;; Seed the static semantic revision while no compositor is attached.
    (mcluv::publish-gpu-mirror-prepared-revision mirror revision)
    (setf (mcluv:mirror-compositor mirror) overlay)
    ;; Repaint publication and static refresh must agree on the actual panel
    ;; attachment; otherwise each semantic edit would discard the cohort that
    ;; the pre-pass boundary had just installed.
    (mcluv::prepare-mirror-compositor-revision overlay mirror revision)
    (ok (equal expected
               (direct-overlay-preparation-observations overlay)))
    (setf (direct-overlay-preparation-observations overlay) nil)
    (luvcraft:refresh-luvcraft-overlay overlay nil)
    (ok (equal expected
               (direct-overlay-preparation-observations overlay)))))

(deftest surveyor-captures-one-dense-terrain-product
  (let* ((world (luvcraft:make-empty-little-block-world :seed 121))
         (camera
           (make-instance 'luvcraft:fly-camera
                          :position (luvcraft::make-vec3 12d0 10d0 34d0)))
         (player (luvcraft:make-player-for-camera camera))
         (session
           (make-instance 'luvcraft:luvcraft-session
                          :world world :player player))
         (snapshot
           (mcluv::capture-surveyor-map-snapshot
            session :width 8 :depth 6)))
    (ok (= 48 (length (mcluv::surveyor-snapshot-heights snapshot))))
    (ok (= 48 (length (mcluv::surveyor-snapshot-materials snapshot))))
    (ok (= 48 (length (mcluv::surveyor-snapshot-lights snapshot))))
    (ok (= 12 (mcluv::surveyor-snapshot-center-x snapshot)))
    (ok (= 34 (mcluv::surveyor-snapshot-center-z snapshot)))
    (ok (<= (mcluv::surveyor-snapshot-minimum-height snapshot)
            (mcluv::surveyor-snapshot-maximum-height snapshot)))
    (ok (every (lambda (material) (typep material 'luvcraft:block-kind))
               (coerce (mcluv::surveyor-snapshot-materials snapshot) 'list)))))

(deftest unavailable-world-light-is-explicit
  (let ((world (luvcraft:make-empty-little-block-world :seed 121)))
    (multiple-value-bind (sky block state)
        (luvcraft:world-light-levels-at world 200 8 200)
      (ok (zerop sky))
      (ok (zerop block))
      (ok (eq state :unavailable)))))

(deftest hotbar-palette-covers-every-placeable-block
  (loop for block in (luvcraft:placeable-block-kinds)
        do (ok (typep (mcluv::hotbar-material-color block) 'clim:color))
           (ok (typep (mcluv::hotbar-material-ink block 0 80)
                      'mcluv:linear-gradient))))

(deftest hotbar-is-composited-after-scene-postprocessing
  (ok (eq :hud
          (luvcraft:luvcraft-overlay-stage
           (allocate-instance
            (find-class 'mcluv:luvcraft-hotbar-overlay))))))

(deftest mcclim-commands-replay-in-each-overlays-final-pass
  (ok (subtypep 'mcluv::terminal-film-browser-overlay
                'mcluv:luvcraft-world-widget-overlay))
  (ok (not (subtypep 'mcluv:luvcraft-hotbar-overlay
                     'mcluv:luvcraft-world-widget-overlay)))
  (ok (eq :world-panel
          (luvcraft:luvcraft-overlay-stage
           (allocate-instance
            (find-class 'mcluv:luvcraft-world-widget-overlay)))))
  (dolist (class '(mcluv:luvcraft-hotbar-overlay
                   mcluv:luvcraft-inventory-overlay
                   mcluv::luvcraft-metabar-overlay))
    (ok (subtypep class 'mcluv:luvcraft-hud-widget-overlay)))
  (let ((overlay
          (allocate-instance
           (find-class 'mcluv:luvcraft-world-widget-overlay))))
    (dolist (command
              (list (mcluv::make-gpu-solid-command)
                    (mcluv::make-gpu-analytic-command)
                    (mcluv::make-gpu-relief-analytic-command)
                    (mcluv::make-gpu-gradient-analytic-command)
                    (mcluv::make-gpu-prepared-image-command)
                    (mcluv::make-gpu-prepared-lattice-command)
                    (mcluv::make-gpu-prepared-text-command)))
      (ok (not (mcluv::gpu-command-rasterized-p overlay command)))
      (ok (mcluv::gpu-command-rasterized-p nil command))))
  (let ((generic (fdefinition 'mcluv::encode-gpu-command))
        (context (mcluv::make-direct-widget-command-encode-context)))
    (dolist (command
              (list (mcluv::make-gpu-solid-command)
                    (mcluv::make-gpu-analytic-command)
                    (mcluv::make-gpu-relief-analytic-command)
                    (mcluv::make-gpu-gradient-analytic-command)
                    (mcluv::make-gpu-prepared-image-command)
                    (mcluv::make-gpu-prepared-lattice-command)
                    (mcluv::make-gpu-prepared-text-command)))
      (ok (compute-applicable-methods generic
                                      (list command nil context)))))
  (ok (null
       (mcluv::direct-gpu-mirror-depth-stencil
        (allocate-instance
         (find-class 'mcluv:luvcraft-hud-widget-overlay)))))
  (ok (equal '(:format :depth32-float
               :depth-write-enabled t :depth-compare :always)
             (mcluv::direct-gpu-mirror-depth-stencil
              (allocate-instance
               (find-class 'mcluv:luvcraft-world-widget-overlay)))))
  (dolist (specification
            (list (mcluv::direct-widget-solid-vertex-specification)
                  (mcluv::direct-widget-analytic-vertex-specification)
                  (mcluv::direct-widget-relief-vertex-specification)
                  (mcluv::direct-widget-gradient-vertex-specification)
                  (mcluv::direct-widget-image-vertex-specification)
                  (mcluv::direct-mirror-slug-vertex-specification)
                  (luv.analytic:lattice-fragment-specification)))
    (ok (> (length (spv:assemble-shader-specification specification)) 5))
    (ok (search "using namespace metal"
                (luv.msl:msl-document-source
                 (luv.msl:compile-msl specification))))))

(deftest world-panels-target-the-native-density-hdr-attachment
  (let* ((target (gensym "WORLD-PANEL-COLOR"))
         (renderer
           (make-instance
            'luvcraft::luvcraft-renderer
            :frame-attachments
            (list :world-panel-color-texture target)))
         (session
           (make-instance 'luvcraft:luvcraft-session :renderer renderer))
         (overlay
           (make-instance 'mcluv:luvcraft-world-widget-overlay
                          :session session :frame nil :mirror nil)))
    (ok (eq target
            (mcluv::luvcraft-widget-render-target overlay session nil)))))

(deftest widget-focus-probes-the-logical-retina-center
  (let* ((canvas (make-instance 'luv:sdl-canvas :width 1000 :height 500))
         (session (make-instance 'luvcraft:luvcraft-session :canvas canvas))
         (overlay
           (make-instance 'mcluv:luvcraft-world-widget-overlay
                          :session session :frame nil :mirror nil)))
    (setf (mcluv:widget-overlay-render-state overlay)
          (make-array
           16 :element-type 'single-float
           :initial-contents
           '(0.0 0.0 0.0 1.0
             1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 0.0 0.0)))
    (let ((coordinate
            (mcluv:luvcraft-widget-texture-coordinate overlay 500.0 250.0)))
      (ok coordinate)
      (ok (= 0.5 (first coordinate)))
      (ok (= 0.5 (second coordinate))))
    ;; This has no canvas context or drawable extent on purpose. Focus uses
    ;; the same logical point space as SDL pointer events, not GPU pixels.
    (ok (= 0.0 (luvcraft:luvcraft-focus-score overlay session)))))

(deftest inventory-is-a-modal-hud-with-stable-grid-hit-testing
  (ok (eq :hud
          (luvcraft:luvcraft-overlay-stage
           (allocate-instance
            (find-class 'mcluv:luvcraft-inventory-overlay)))))
  (ok (= 0 (mcluv::inventory-slot-at 0.21 0.12 9)))
  (ok (= 2 (mcluv::inventory-slot-at 0.50 0.12 9)))
  (ok (= 8 (mcluv::inventory-slot-at 0.61 0.36 9)))
  (ok (null (mcluv::inventory-slot-at 0.01 0.20 9)))
  (ok (null (mcluv::inventory-slot-at 0.50 0.95 9)))
  (ok (eq :all (mcluv::inventory-category-at 0.05 0.12)))
  (ok (eq :building (mcluv::inventory-category-at 0.05 0.28)))
  (ok (= -1 (mcluv::inventory-page-direction-at 0.22 0.05)))
  (ok (= 1 (mcluv::inventory-page-direction-at 0.75 0.05)))
  (ok (null (mcluv::inventory-page-direction-at 0.50 0.05)))
  (ok (= 0 (mcluv::inventory-quickbar-slot-at 0.20 0.56 9)))
  (ok (= 8 (mcluv::inventory-quickbar-slot-at 0.76 0.56 9)))
  (ok (mcluv::inventory-category-block-p
       :natural luvcraft::*grass-block*))
  (ok (not (mcluv::inventory-category-block-p
            :natural luvcraft::*stone-block*))))

(deftest terminal-film-browser-shows-directories-and-playable-files
  (let* ((root (asdf:system-source-directory :luv))
         (directory (merge-pathnames "libav/" root))
         (entries (mcluv::terminal-film-browser-entries directory)))
    (ok (mcluv::terminal-film-pathname-p
         (merge-pathnames "test-pattern.mp4" directory)))
    (ok (not (mcluv::terminal-film-pathname-p
              (merge-pathnames "README.org" directory))))
    (ok (find "test-pattern.mp4" entries :test #'string=
              :key (lambda (entry)
                     (and (eq :film (mcluv::terminal-film-entry-kind entry))
                          (file-namestring
                           (mcluv::terminal-film-entry-pathname entry))))))))

(deftest terminal-film-browser-shortens-long-directory-headings
  (let ((label
          (mcluv::terminal-film-browser-path-label
           #P"/a/directory/name/that/is/long/enough/to/collide/with/the/browser/title/")))
    (ok (<= (length label) mcluv::+terminal-film-browser-path-limit+))
    (ok (string= "..." label :end2 3))))
