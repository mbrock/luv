(in-package #:luvcraft.luft.game.tests)

(defvar *realized-knobs* nil)

(defun make-knob-test-viewer (scene &optional player)
  (make-instance 'host:luvcraft-viewer :source scene :player player
                 :hotbar (list (game:block-placement luvcraft::*stone-block*))))

(defmethod luv.knobs:realize-knob :around
    ((knob luv.knobs:knob) (viewer render:game-host))
  (when *realized-knobs* (push knob (car *realized-knobs*)))
  (call-next-method))

(define-test engine-knobs-round-trip-through-the-shared-metabar
  (let* ((scene (ball-test-scene))
         (player (render::make-scene-walking-character scene))
         (viewer (make-knob-test-viewer scene player))
         (*realized-knobs* (list nil)))
    (dolist (knob (luv.knobs:knobs-for-host viewer))
      (let ((saved (luv.knobs:knob-value knob viewer)))
        (unwind-protect
             (progn
               (when (typep knob 'luv.knobs:scalar-knob)
                 (true (luv.knobs:knob-quantity-specification knob))
                 (mcluv:perform-metabar-control-set-fraction knob viewer 0.5)
                 (let ((middle (luv.knobs:knob-value knob viewer)))
                   (true (equalp middle (luv.knobs:set-knob-value knob middle viewer))))
                 (mcluv:perform-metabar-control-set-fraction knob viewer 1.0)
                 (true (< (abs (- (luv.knobs:knob-maximum knob)
                                  (luv.knobs:knob-value knob viewer)))
                          0.000001)))
               (when (typep knob 'luv.knobs:switch-knob)
                 (mcluv:perform-metabar-control-toggle knob viewer)
                 (true (not (eql saved (luv.knobs:knob-value knob viewer)))))
               (true (member knob (car *realized-knobs*)))
               (true (stringp (mcluv:metabar-control-value-label knob viewer nil))))
          (luv.knobs:set-knob-value knob saved viewer))))
    (true (member :physics (mcluv:metabar-groups-for viewer)))
    (true (member :critters (mcluv:metabar-groups-for viewer)))
    (true (member (luv.knobs:find-knob 'luvcraft::slug-optical-weight)
                  (mcluv:metabar-controls-for viewer :text)))
    (true (luv.knobs:knob-rebuild-p (luv.knobs:find-knob 'luvcraft::slug-optical-weight) viewer))
    (true (= (render::viewer-speed viewer)
             (luft.terrain:character-speed player)))))

(define-test player-knobs-reach-locomotion-and-camera
  (let* ((scene (ball-test-scene))
         (player (render::make-scene-walking-character scene))
         (viewer (make-knob-test-viewer scene player))
         (gravity (luv.knobs:find-knob 'render::player-gravity))
         (saved (luv.knobs:knob-value gravity viewer))
         (eye (luv.knobs:find-knob 'render::eye-height))
         (saved-eye (luv.knobs:knob-value eye viewer)))
    (unwind-protect
         (progn
           (luv.knobs:set-knob-value gravity 12.0 viewer)
           (is = -12.0 (luft.terrain:body-gravity (luft.terrain:character-body player)))
           (luv.knobs:set-knob-value eye 1.2 viewer)
           (render::place-viewer-at-player-eyes viewer)
           (true (< (abs (- (luv.arithmetic.lisp.vec3:vec3-z
                            (render:camera-position (render:viewer-camera viewer)))
                           (luv.arithmetic.lisp.vec3:vec3-z
                            (luft.terrain:body-position (luft.terrain:character-body player)))
                           1.2)) 0.0001)))
      (luv.knobs:set-knob-value gravity saved viewer)
      (luv.knobs:set-knob-value eye saved-eye viewer))))

(define-test terminal-knobs-invalidate-idle-screens-and-resize-the-grid
  (let* ((scene (ball-test-scene))
         (viewer (make-knob-test-viewer scene))
         (surface (render::%make-terminal-surface :width 4 :height 3))
         (terminal (make-instance 'render::world-terminal :surface surface
                                  :columns 40 :rows 15 :terminal nil :glyph-cache nil))
         (state (render::viewer-terminals viewer))
         (scale (luv.knobs:find-knob 'render::terminal-font-scale))
         (ink (luv.knobs:find-knob 'render::terminal-ink-emission))
         (saved-scale (luv.knobs:knob-value scale viewer))
         (saved-ink (luv.knobs:knob-value ink viewer)))
    (setf (render::viewer-terminals-terminals state) (list terminal)
          (render::world-terminal-dirty-p terminal) nil)
    (unwind-protect
         (progn
           (luv.knobs:set-knob-value ink 3.0 viewer)
           (true (render::world-terminal-dirty-p terminal))
           (luv.knobs:set-knob-value scale 2.0 viewer)
           (true (< (render::world-terminal-rows terminal) 15)))
      (luv.knobs:set-knob-value ink saved-ink viewer)
      (luv.knobs:set-knob-value scale saved-scale viewer)
      (remhash viewer render::*viewer-terminals*))))

(define-test picture-knobs-change-the-frame-inputs
  (let ((viewer (make-knob-test-viewer (ball-test-scene))))
    (dolist (name '(render::time-of-day render::star-brightness render::moon-radiance
                    render::galaxy-strength render::bloom-gain render::bloom-threshold
                    render::shaft-gain render::shaft-decay render::vignette
                    render::paper-grain render::haze-density render::haze-height
                    render::shadow-base-bias render::shadow-filter-radius))
      (let* ((knob (luv.knobs:find-knob name))
             (saved (luv.knobs:knob-value knob viewer)))
        (flet ((inputs ()
                 (append (render::atmosphere-uniform-data render::*light*)
                         (render::light-uniform-data
                          render::*light* (luv.arithmetic.lisp.vec3:make-vec3 0 0 0)))))
          (let ((before (inputs)))
            (unwind-protect
                 (progn
                   (luv.knobs:step-knob knob viewer 1)
                   (true (not (equalp before (inputs)))))
              (luv.knobs:set-knob-value knob saved viewer))))))))

(define-test physics-knobs-reach-the-shared-solver
  (let* ((scene (ball-test-scene))
         (viewer (make-knob-test-viewer scene))
         (gravity (luv.knobs:find-knob 'luvcraft::physics-gravity))
         (substeps (luv.knobs:find-knob 'luvcraft::physics-substeps))
         (bounce (luv.knobs:find-knob 'luvcraft::ball-bounce))
         (saved-gravity (luv.knobs:knob-value gravity viewer))
         (saved-substeps (luv.knobs:knob-value substeps viewer))
         (saved-bounce (luv.knobs:knob-value bounce viewer)))
    (labels ((fall ()
               (let* ((physics (ball-test-physics scene))
                      (ball (luvcraft::spawn-body-of-kind physics :ball 8.5 10.0 8.5)))
                 (luvcraft::step-physics-world physics)
                 (nth-value 1 (luvcraft::physics-body-position physics ball))))
             (peak ()
               (let* ((physics (ball-test-physics scene))
                      (ball (luvcraft::spawn-body-of-kind physics :ball 8.5 5.0 8.5))
                      (landed nil) (peak 0.0))
                 (dotimes (i 100)
                   (luvcraft::step-physics-world physics)
                   (let ((height (nth-value 1 (luvcraft::physics-body-position physics ball)))
                         (speed (nth-value 1 (luvcraft::physics-body-velocity physics ball))))
                     (when (< height 2.3) (setf landed t))
                     (when (and landed (plusp speed)) (setf peak (max peak height)))))
                 peak)))
      (unwind-protect
           (progn
             (luv.knobs:set-knob-value gravity 0.0 viewer)
             (let ((hover (fall)))
               (luv.knobs:set-knob-value gravity 24.0 viewer)
               (true (< (fall) hover)))
             (luv.knobs:set-knob-value substeps 1 viewer)
             (let ((coarse (fall)))
               (luv.knobs:set-knob-value substeps 8 viewer)
               (true (/= coarse (fall))))
             (luv.knobs:set-knob-value bounce 0.0 viewer)
             (let ((dead (peak)))
               (luv.knobs:set-knob-value bounce 0.8 viewer)
               (true (> (peak) (+ dead 0.2)))))
        (luv.knobs:set-knob-value gravity saved-gravity viewer)
        (luv.knobs:set-knob-value substeps saved-substeps viewer)
        (luv.knobs:set-knob-value bounce saved-bounce viewer)))))

(define-test turtle-count-reseeds-the-running-population
  (let* ((grass (game:block-placement luvcraft::*grass-block*))
         (builder (luft.terrain:make-scene-builder
                   :horizontal-bits 11
                   :material-vocabulary (game:luvcraft-palette-vocabulary game:*luvcraft-palette*)
                   :ground-material grass :architecture-material grass)))
    (loop for x from 1024 below 1088 do
      (loop for y from 1024 below 1088 do
        (luft.terrain:scene-builder-cell builder x y 1)))
    (let* ((scene (luft.terrain:make-streaming-scene
                   (luft.terrain:finish-scene-builder builder)))
           (player (luft.terrain:make-walking-character
                    :position (luv.arithmetic.lisp.vec3:make-vec3 1050.5 1050.5 2.0)))
           (viewer (make-knob-test-viewer scene player))
           (knob (luv.knobs:find-knob 'host::turtle-count)))
      (luv.knobs:set-knob-value knob 3 viewer)
      (is = 3 (length (host::viewer-animals viewer)))
      (luv.knobs:set-knob-value knob 1 viewer)
      (is = 1 (length (host::viewer-animals viewer)))
      (luv.knobs:set-knob-value knob 0 viewer)
      (is = 0 (length (host::viewer-animals viewer)))
      (true (host::viewer-animals-initialized-p viewer)))))

(define-test residency-knob-retargets-without-player-motion
  (let* ((scene (luft.world:make-authored-world-streaming-scene
                 :horizontal-bits 10 :residency-radius 0))
         (viewer (make-knob-test-viewer scene))
         (knob (luv.knobs:find-knob 'render::residency-radius))
         (producer (luv.production:make-single-worker-production-system)))
    (unwind-protect
         (progn
           (render::retarget-authored-world scene producer 1 512 512)
           (let ((small (hash-table-count (render::streaming-scene-desired scene))))
             (luv.knobs:set-knob-value knob 2 viewer)
             (render::retarget-authored-world scene producer 1 512 512)
             (true (> (hash-table-count (render::streaming-scene-desired scene)) small))))
      (luv.production:stop-production-system producer))))
