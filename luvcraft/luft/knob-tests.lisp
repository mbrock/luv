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
