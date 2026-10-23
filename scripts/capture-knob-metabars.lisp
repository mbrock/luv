;;;; Headless real McCLIM/Vulkan captures with portable key/pointer events.
;;;; XDG_RUNTIME_DIR=/tmp/luv-runtime ./env --slim sbcl --script scripts/capture-knob-metabars.lisp
(require :asdf)
(asdf:load-system "luvcraft/luft/game")
(asdf:load-system "luvcraft/mcclim")

(defpackage #:knob-capture
  (:use #:cl)
  (:local-nicknames (#:ui #:mcluv) (#:knobs #:luv.knobs) (#:render #:luft.render)))
(in-package #:knob-capture)

(defun key (frame name)
  (ui::handle-metabar-key-event
   frame (make-instance 'luv:canvas-key-press-event :timestamp 0 :key-name name)))

(defun pointer (frame class x y)
  (ui::handle-metabar-pointer-event
   frame (apply #'make-instance class :timestamp 0 :x x :y y
                (unless (eq class 'luv:canvas-pointer-motion-event)
                  '(:button :left :clicks 1))) x y))

(defun capture (owner stem groups)
  (let* ((vocabulary (ui::capture-metabar-vocabulary owner))
         (height (ui::metabar-natural-height-for owner vocabulary))
         (port (clim:find-port :server-path '(:luv-gpu)))
         (manager (or (first (clim-internals::frame-managers port))
                      (make-instance 'ui::luv-frame-manager :port port)))
         (ui::*metabar-construction-height* height)
         (frame (clim:make-application-frame
                 'ui::metabar :frame-manager manager :enable nil
                 :owner owner :vocabulary vocabulary :logical-height height
                 :width ui::+metabar-width+ :height height)))
    (unwind-protect
         (let* ((sheet (clim:frame-top-level-sheet frame))
                (mirror (clim:sheet-direct-mirror sheet))
                (ui::*suppress-luv-mirror-visibility* t))
           (setf (clim:sheet-enabled-p sheet) t)
           (dolist (group groups)
             ;; Select and open the group using Return, not a direct value set.
             (ui::select-metabar-row
              frame (position group (ui::metabar-rows frame)
                              :key #'ui::metabar-row-subject))
             (key frame :return)
             (let* ((knob (if (and (eq group :grading) (typep owner 'render:game-host))
                              (knobs:find-knob 'luft.render.shaders::voxel-light-gain)
                              (first (ui:metabar-controls-for owner group))))
                    (index (position knob (ui::metabar-rows frame)
                                     :key #'ui::metabar-row-subject))
                    (y (+ (ui::metabar-row-top frame index) 52))
                    (saved (knobs:knob-value knob owner))
                    (after-key nil))
               (unwind-protect
                    (progn
                      (ui::select-metabar-row frame index)
                      (key frame :right)
                      (ui::drain-metabar-operations frame)
                      (setf after-key (knobs:knob-value knob owner))
                      (assert (not (equalp saved after-key)))
                      (pointer frame 'luv:canvas-pointer-button-press-event 180.0 y)
                      (pointer frame 'luv:canvas-pointer-motion-event 300.0 y)
                      (when (knobs:knob-rebuild-p knob owner)
                        (ui::drain-metabar-operations frame)
                        (assert (equalp after-key (knobs:knob-value knob owner))))
                      (pointer frame 'luv:canvas-pointer-button-release-event 300.0 y)
                      (ui::drain-metabar-operations frame)
                      (assert (not (equalp after-key (knobs:knob-value knob owner))))
                      (assert (null (ui::metabar-diagnostic frame)))
                      (let ((clim:*application-frame* frame))
                        (clim:redisplay-frame-panes frame :force-p t)
                        (ui::repaint-gpu-mirror mirror :present-p nil)
                        (ui::capture-gpu-mirror-screenshot
                         mirror (format nil ".amp/in/artifacts/~A-~(~A~).png" stem group)))
                      (format t "~&CAPTURE ~A ~A: key + drag -> ~A; pending=~S~%"
                              stem group (knobs:format-knob-value knob owner)
                              (ui::metabar-pending-operations frame)))
                 (knobs:set-knob-value knob saved owner)))))
      (clim:destroy-frame frame))))

(ensure-directories-exist #P".amp/in/artifacts/knobs.png")
(capture (make-instance 'luvcraft:luvcraft-session
                        :camera (make-instance 'luvcraft::fly-camera))
         "knobs-old" '(:grading :camera :text))
(let* ((scene (luft.world:make-authored-world-streaming-scene))
       (host (make-instance 'luvcraft.luft.game:luvcraft-viewer :source scene
                            :player (render::make-scene-walking-character scene)
                            :hotbar (list (luvcraft.luft:block-placement luvcraft::*stone-block*)))))
  (capture host "knobs-luft" '(:grading :player :physics :terminal :text)))
