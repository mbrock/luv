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

(define-test hotbar-keeps-ten-icons-on-one-glass-strip-with-a-selected-underline
  (dolist (finite-p '(nil t))
    (let* ((inventory (luvcraft:make-block-inventory :quantity (when finite-p 17)))
           (session (make-instance 'luvcraft:luvcraft-session :inventory inventory))
           (blocks (luvcraft:block-inventory-quickbar-blocks inventory))
           (frame (clim:make-application-frame 'mcluv::luvcraft-hotbar :session session)))
      (true (= 10 (length blocks)))
      (loop for selected in blocks
            for index from 0
            do (let* ((medium (make-instance 'mcluv:luv-gpu-medium))
                      (pane (make-instance 'mcluv::hotbar-pane :frame frame
                                           :region (clim:make-bounding-rectangle
                                                    0 0 mcluv::+hotbar-width+
                                                    mcluv::*hotbar-height*))))
                 (setf (slot-value pane 'clim:medium) medium
                       (luvcraft:player-selected-block session) selected)
                 (clim:with-output-recording-options (pane :record nil)
                   (clim:handle-repaint pane clim:+everywhere+))
                 (let* ((commands (coerce (mcluv::gpu-medium-commands medium) 'list))
                        (text (remove-if-not
                               (lambda (command) (typep command 'mcluv::gpu-text-command))
                               commands))
                        (vertices (mcluv::gpu-medium-analytic-vertices medium)))
                   (true (= 2 (count-if
                               (lambda (command) (typep command 'mcluv::gpu-analytic-command))
                               commands)))
                   (true (= 10 (count-if
                                (lambda (command) (typep command 'mcluv::gpu-image-command))
                                commands)))
                   (true (= (if finite-p 20 10) (length text)))
                   (true (equal '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
                                (remove "17" (mapcar #'mcluv::gpu-text-command-string text)
                                        :test #'string=)))
                   ;; A low-opacity neutral fill and one opaque thin underline.
                   (true (= 0.28 (aref vertices 2)))
                   (true (= (aref vertices 9) (aref vertices 10) (aref vertices 11)))
                   (true (< (aref vertices 9) 0.02))
                   (true (= 1.0 (aref vertices (+ 72 2))))
                   (true (= 1.0 (aref vertices (+ 72 7))))
                   (true (null (mcluv:gpu-medium-fallback-report medium)))
                   (multiple-value-bind (left top right bottom)
                       (mcluv::hotbar-slot-geometry 10 index)
                     (declare (ignore top bottom))
                     (true (= index (mcluv::hotbar-slot-at
                                     10 (/ (+ left right) 2 mcluv::+hotbar-width+)))))))))))

(define-test inventory-tiles-remain-opaque-and-hotbar-gaps-remain-unselected
  (let ((medium (make-instance 'mcluv:luv-gpu-medium)))
    (mcluv::draw-hotbar-shell nil medium :width 400 :height 200)
    (mcluv::draw-hotbar-slot nil medium 10 10 60 64 :selected-p t)
    (true (= 4 (length (mcluv::gpu-medium-commands medium))))
    (loop with vertices = (mcluv::gpu-medium-analytic-vertices medium)
          for offset from 0 below (length vertices) by 72
          do (true (= 1.0 (aref vertices (+ offset 2))))))
  (dolist (count '(6 10))
    (loop for index below count
          do (multiple-value-bind (left top right bottom)
                 (mcluv::hotbar-slot-geometry count index)
               (declare (ignore left top bottom))
               (true (null (mcluv::hotbar-slot-at
                            count (/ (+ right 1) mcluv::+hotbar-width+))))))))

(define-test hotbar-is-composited-after-scene-postprocessing
  (true (eq :hud
            (luvcraft:luvcraft-overlay-stage
             (allocate-instance
              (find-class 'mcluv:luvcraft-hotbar-overlay))))))

(define-test mcclim-commands-replay-in-each-overlays-final-pass
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
  (flet ((slot (x y count &optional (scroll 0))
           (mcluv::inventory-slot-at (/ x 840) (/ y 364) count scroll)))
    (true (= 0 (slot 50 85 70)))
    (true (= 3 (slot 290 85 70)))
    (true (= 8 (slot 50 145 70)))
    (true (= 39 (slot 620 330 70)))
    (true (= 8 (slot 50 85 70 (mcluv::inventory-row-height))))
    (true (null (slot 50 355 70)))
    (true (null (slot 10 85 70)))
    (true (null (slot 700 85 70)))
    (true (= 0 (mcluv::inventory-max-scroll 40)))
    (true (= (* 4 (mcluv::inventory-row-height)) (mcluv::inventory-max-scroll 70)))
    (true (null (slot 620 330 9)))))

(define-test building-kit-keeps-six-unique-blocks-and-never-replaces-silently
  (let* ((inventory (luvcraft:make-block-inventory))
         (blocks (mapcar #'luvcraft:block-kind-named
                         '(:grass :planks :bricks :marble :lantern :gold)))
         (extra (luvcraft:block-kind-named :basalt)))
    (loop for block in blocks for index from 0 do
      (multiple-value-bind (slot status)
          (luvcraft::add-building-kit-block inventory block)
        (true (= slot index))
        (true (eq status :added))))
    (multiple-value-bind (slot status)
        (luvcraft::add-building-kit-block inventory (third blocks))
      (true (= slot 2))
      (true (eq status :present)))
    (multiple-value-bind (slot status)
        (luvcraft::add-building-kit-block inventory extra)
      (true (null slot))
      (true (eq status :full))
      (true (equal blocks (luvcraft::block-inventory-kit inventory))))
    (luvcraft::add-building-kit-block inventory extra 2)
    (true (eq extra (third (luvcraft::block-inventory-kit inventory))))
    (true (= 6 (length (luvcraft::block-inventory-kit inventory))))
    (true (= 10 (length (luvcraft:block-inventory-quickbar-blocks inventory))))
    (setf (luvcraft::block-inventory-simple-p inventory) t)
    (true (equal (luvcraft::block-inventory-kit inventory)
                 (luvcraft:block-inventory-quickbar-blocks inventory)))
    (true (luvcraft:block-inventory-entry-for inventory (third blocks)))))

(define-test player-kits-round-trip-independently-with-their-inventory-modes
  (let* ((data (mcluv::default-inventory-preferences))
         (path (merge-pathnames
                (format nil "luv-inventory-test-~A.sexp" (gensym))
                (uiop:temporary-directory)))
         (writer nil))
    (setf (getf data :active) 1
          (getf (first (getf data :players)) :kit) '(:marble :gold)
          (getf (second (getf data :players)) :kit) '(:planks :lantern)
          (getf (second (getf data :players)) :full) t)
    (unwind-protect
         (progn
           (setf writer (luvcraft::make-world-checkpoint-writer path))
           (luvcraft::request-world-checkpoint writer data)
           (luvcraft::stop-world-checkpoint-writer writer)
           (setf writer nil)
           (let ((restored (mcluv::read-inventory-preferences path)))
             (true (equal data restored))
             (true (= 1 (getf restored :active)))
             (false (getf (first (getf restored :players)) :full))
             (true (getf (second (getf restored :players)) :full))))
      (when writer (luvcraft::stop-world-checkpoint-writer writer))
      (when (probe-file path) (delete-file path)))))

(define-test damaged-player-kits-are-rejected-before-writing
  (dolist (change '((:active 2) (:version 99) (:players nil)))
    (let ((data (mcluv::default-inventory-preferences)))
      (setf (getf data (first change)) (second change))
      (fail (mcluv::validate-inventory-preferences data))))
  (dolist (bad-kit '((:grass :grass) (:grass "not a block") (:grass :stone :bricks :planks :gold :marble :lantern)))
    (let ((data (mcluv::default-inventory-preferences)))
      (setf (getf (first (getf data :players)) :kit) bad-kit)
      (fail (mcluv::validate-inventory-preferences data)))))

(define-test material-traits-use-real-light-values
  (let ((marble (luvcraft:block-kind-named :marble))
        (lantern (luvcraft:block-kind-named :lantern)))
    (true (equal '(:light-off "Light" "0 / 15")
                 (first (mcluv::inventory-material-traits marble))))
    (true (equal '(:light "Light" "14 / 15")
                 (first (mcluv::inventory-material-traits lantern))))
    (true (equal '(:glow "Glow" "1.10")
                 (second (mcluv::inventory-material-traits lantern))))
    (true (search "stone" (first (mcluv::inventory-material-description marble))))))
