(defpackage #:mcluv.surveyor-tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false #:fail #:group #:skip))

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

(define-test static-luvcraft-world-overlay-prepares-its-hdr-panel-target
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
    (true (equal expected
                 (direct-overlay-preparation-observations overlay)))
    (setf (direct-overlay-preparation-observations overlay) nil)
    (luvcraft:refresh-luvcraft-overlay overlay nil)
    (true (equal expected
                 (direct-overlay-preparation-observations overlay)))))

(define-test surveyor-captures-one-dense-terrain-product
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
    (true (= 48 (length (mcluv::surveyor-snapshot-heights snapshot))))
    (true (= 48 (length (mcluv::surveyor-snapshot-materials snapshot))))
    (true (= 48 (length (mcluv::surveyor-snapshot-lights snapshot))))
    (true (= 12 (mcluv::surveyor-snapshot-center-x snapshot)))
    (true (= 34 (mcluv::surveyor-snapshot-center-z snapshot)))
    (true (<= (mcluv::surveyor-snapshot-minimum-height snapshot)
              (mcluv::surveyor-snapshot-maximum-height snapshot)))
    (true (every (lambda (material) (typep material 'luvcraft:block-kind))
                 (coerce (mcluv::surveyor-snapshot-materials snapshot) 'list)))))

(define-test unavailable-world-light-is-explicit
  (let ((world (luvcraft:make-empty-little-block-world :seed 121)))
    (multiple-value-bind (sky block state)
        (luvcraft:world-light-levels-at world 200 8 200)
      (true (zerop sky))
      (true (zerop block))
      (true (eq state :unavailable)))))

(define-test hotbar-palette-covers-every-placeable-block
  (loop for block in (luvcraft:placeable-block-kinds)
        do (true (typep (mcluv::hotbar-material-color block) 'clim:color))
           (true (typep (mcluv::hotbar-material-ink block 0 80)
                        'mcluv:linear-gradient))))

(define-test hotbar-is-composited-after-scene-postprocessing
  (true (eq :hud
            (luvcraft:luvcraft-overlay-stage
             (allocate-instance
              (find-class 'mcluv:luvcraft-hotbar-overlay))))))

(define-test mcclim-commands-replay-in-each-overlays-final-pass
  (true (subtypep 'mcluv::terminal-film-browser-overlay
                  'mcluv:luvcraft-world-widget-overlay))
  (true (not (subtypep 'mcluv:luvcraft-hotbar-overlay
                       'mcluv:luvcraft-world-widget-overlay)))
  (true (eq :world-panel
            (luvcraft:luvcraft-overlay-stage
             (allocate-instance
              (find-class 'mcluv:luvcraft-world-widget-overlay)))))
  (dolist (class '(mcluv:luvcraft-hotbar-overlay
                   mcluv:luvcraft-inventory-overlay
                   mcluv::luvcraft-metabar-overlay))
    (true (subtypep class 'mcluv:luvcraft-hud-widget-overlay)))
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
      (true (not (mcluv::gpu-command-rasterized-p overlay command)))
      (true (mcluv::gpu-command-rasterized-p nil command))))
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
      (true (compute-applicable-methods generic
                                        (list command nil context)))))
  (true (null
         (mcluv::direct-gpu-mirror-depth-stencil
          (allocate-instance
           (find-class 'mcluv:luvcraft-hud-widget-overlay)))))
  (true (equal '(:format :depth32-float
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
    (true (> (length (spv:assemble-shader-specification specification)) 5))
    (true (search "using namespace metal"
                  (luv.msl:msl-document-source
                   (luv.msl:compile-msl specification))))))

(define-test world-panels-target-the-native-density-hdr-attachment
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
    (true (eq target
              (mcluv::luvcraft-widget-render-target overlay session nil)))))

(define-test widget-focus-probes-the-logical-retina-center
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
      (true coordinate)
      (true (= 0.5 (first coordinate)))
      (true (= 0.5 (second coordinate))))
    ;; This has no canvas context or drawable extent on purpose. Focus uses
    ;; the same logical point space as SDL pointer events, not GPU pixels.
    (true (= 0.0 (luvcraft:luvcraft-focus-score overlay session)))))

(define-test inventory-is-a-modal-hud-with-stable-grid-hit-testing
  (true (eq :hud
            (luvcraft:luvcraft-overlay-stage
             (allocate-instance
              (find-class 'mcluv:luvcraft-inventory-overlay)))))
  (true (= 0 (mcluv::inventory-slot-at 0.21 0.12 9)))
  (true (= 2 (mcluv::inventory-slot-at 0.50 0.12 9)))
  (true (= 8 (mcluv::inventory-slot-at 0.61 0.36 9)))
  (true (null (mcluv::inventory-slot-at 0.01 0.20 9)))
  (true (null (mcluv::inventory-slot-at 0.50 0.95 9)))
  (true (eq :all (mcluv::inventory-category-at 0.05 0.12)))
  (true (eq :building (mcluv::inventory-category-at 0.05 0.28)))
  (true (= -1 (mcluv::inventory-page-direction-at 0.22 0.05)))
  (true (= 1 (mcluv::inventory-page-direction-at 0.75 0.05)))
  (true (null (mcluv::inventory-page-direction-at 0.50 0.05)))
  (true (= 0 (mcluv::inventory-quickbar-slot-at 0.20 0.56 9)))
  (true (= 8 (mcluv::inventory-quickbar-slot-at 0.76 0.56 9)))
  (true (mcluv::inventory-category-block-p
         :natural luvcraft::*grass-block*))
  (true (not (mcluv::inventory-category-block-p
              :natural luvcraft::*stone-block*))))

(define-test terminal-film-browser-shows-directories-and-playable-files
  (let* ((root (asdf:system-source-directory :luv))
         (directory (merge-pathnames "libav/" root))
         (entries (mcluv::terminal-film-browser-entries directory)))
    (true (mcluv::terminal-film-pathname-p
           (merge-pathnames "test-pattern.mp4" directory)))
    (true (not (mcluv::terminal-film-pathname-p
                (merge-pathnames "README.org" directory))))
    (true (find "test-pattern.mp4" entries :test #'string=
                :key (lambda (entry)
                       (and (eq :film (mcluv::terminal-film-entry-kind entry))
                            (file-namestring
                             (mcluv::terminal-film-entry-pathname entry))))))))

(define-test terminal-film-browser-shortens-long-directory-headings
  (let ((label
          (mcluv::terminal-film-browser-path-label
           #P"/a/directory/name/that/is/long/enough/to/collide/with/the/browser/title/")))
    (true (<= (length label) mcluv::+terminal-film-browser-path-limit+))
    (true (string= "..." label :end2 3))))
