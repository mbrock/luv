;;;; ./sly load luvcraft/luft/game
;;;; ./sly eval '(load "scripts/luft-companion-proof.lisp")'
;;;; Full SDL/offscreen game, real harness, CLIM tools, terrain, inventory,
;;;; mesh worker and Vulkan capture; only the provider is scripted.
;;;; Writes only build/companion-proof.sexp and .amp/in/artifacts/companion-*.
;;;; Leaves the reopened game paused at an evidence camera; stop/play to resume.
(in-package #:luvcraft.luft.game)

(defun check-luft-companions ()
  (when render:*viewer* (render:stop-viewer render:*viewer*))
  (let* ((viewer (play :seed 7 :relief :meadow :pathname #p"build/companion-proof.sexp"
                       :width 1100 :height 760))
         (canvas (render::viewer-canvas viewer))
         (factory *inhabitant-provider-factory*)
         (gnome nil) (turn nil) (closed nil) (start nil))
    (labels ((on-canvas (function)
               (luv.application-agent:call-in-application-frame viewer function))
             (settle ()
               (loop repeat 1800
                     when (on-canvas
                           (lambda ()
                             (render::advance-viewer-streaming viewer)
                             (and (cell-resident-p viewer 1024 1024)
                                  (gethash (luft:chunk-key-at 1024 1024)
                                           (terrain:streaming-scene-loaded (render:viewer-source viewer)))
                                  (null (terrain:streaming-scene-replacement (render:viewer-source viewer)))
                                  (render::viewer-player-ground-resident-p viewer))))
                       return t
                     do (sleep 0.05)
                     finally (error "Streaming did not settle")))
             (key (name &optional character)
               (on-canvas
                (lambda ()
                  (luv:handle-canvas-event
                   (luv:canvas-event-handler canvas) canvas
                   (make-instance 'luv:canvas-key-press-event
                                  :timestamp 0 :key-name name :character character)))))
             (capture (suffix)
               (render:capture-viewer-frame
                (format nil ".amp/in/artifacts/companion-~A.png" suffix) viewer))
             (pump-until (predicate)
               (loop repeat 1200
                     do (on-canvas (lambda ()
                                     (loop repeat 20 do (render:advance-viewer-game viewer (/ 1.0 60)))))
                        (let* ((provider (and gnome (inhabitant-provider gnome)))
                               (current (and provider (luv.application-agent:application-agent-current-turn provider))))
                          (when (and current (eq :failed (luv.application-agent:turn-status current)))
                            (error "Scripted turn failed: ~A" (luv.application-agent:turn-error current))))
                     when (funcall predicate) return t
                     do (sleep 0.01)
                     finally (error "Companion did not finish")))
             (call-tool (agent command arguments)
               (let ((text (openai:call-tool (luv.application-agent:make-command-tool command) arguments agent)))
                 (format t "~&~A => ~A~%" command text)
                 (assert (not (search "error:" text :test #'char-equal)))
                 text)))
      (unwind-protect
           (progn
             (settle)
             (on-canvas (lambda () (setf (render::viewer-pointer-captured-p viewer) t)))
             (capture "before")
             (key :g)
             (setf gnome (viewer-conversation-agent viewer)
                   start (vec3:vec3-list (inhabitant-position gnome)))
             (assert (eq :gnome (inhabitant-kind gnome)))
             (on-canvas (lambda ()
                          (luvcraft:add-block-to-inventory (viewer-inventory viewer) luvcraft::*bricks-block* 3)
                          (luvcraft:add-block-to-inventory (viewer-inventory viewer) luvcraft::*planks-block* 3)))
             (let* ((x (floor (first start))) (y (floor (second start))) (z (floor (third start))))
               (setf *inhabitant-provider-factory*
                     (lambda (host inhabitant)
                       (make-instance
                        'inhabitant-agent :application host :inhabitant inhabitant :model "scripted"
                        :close-function (lambda (agent) (declare (ignore agent)) (setf closed t))
                        :turn-function
                        (lambda (agent prompt)
                          (assert (string= prompt "Please walk, build a brick step and fetch a block."))
                          (call-tool agent 'com-look-around nil)
                          (assert (search "ARRIVED" (call-tool agent 'com-walk-to
                                                               `(("x" . ,(+ x 2)) ("y" . ,y) ("z" . ,z)))))
                          (flet ((place (px py pz)
                                   (settle)
                                   (assert (search "EDITED"
                                                   (call-tool agent 'com-place-cell
                                                              `(("x" . ,px) ("y" . ,py) ("z" . ,pz)
                                                                ("material" . "bricks")))))))
                            (place (+ x 3) (1+ y) z)
                            (place (+ x 3) (1+ y) (1+ z)))
                          (settle)
                          (assert (search "EDITED" (call-tool agent 'com-fetch-block
                                                              `(("x" . ,(+ x 3)) ("y" . ,(1+ y)) ("z" . ,(1+ z))))))
                          (settle)
                          (call-tool agent 'com-propose-cell
                                     `(("x" . ,(+ x 3)) ("y" . ,(1- y)) ("z" . ,z) ("material" . "planks")))
                          (settle)
                          (call-tool agent 'com-propose-cell
                                     `(("x" . ,(+ x 5)) ("y" . ,(1- y)) ("z" . ,z) ("material" . "planks")))
                          (setf (openai:agent-response-id agent) "scripted-response")
                          (make-instance 'openai:agent-response :id "scripted-response" :usage nil :reasoning ""
                                         :text "I walked over, built a brick step, fetched the top brick, and placed the plank you approved.")))))
               ;; Real G/T bindings, shell prompt key events, and Return submission.
               (key :t)
               (loop for c across "Please walk, build a brick step and fetch a block." do (key :x c))
               (capture "prompt")
               (key :return)
               (pump-until (lambda () (inhabitant-approval gnome)))
               (capture "approval")
               (key :y)
               (pump-until (lambda () (inhabitant-approval gnome)))
               (key :n)
               (pump-until
                (lambda ()
                  (let ((provider (inhabitant-provider gnome)))
                    (setf turn (and provider (luv.application-agent:application-agent-current-turn provider)))
                    (and turn (eq :done (luv.application-agent:turn-status turn))))))
               (settle)
               (on-canvas
                (lambda ()
                  (assert (string= "bricks" (cell-name viewer (agent-cell viewer (+ x 3) (1+ y) z))))
                  (assert (string= "air" (cell-name viewer (agent-cell viewer (+ x 3) (1+ y) (1+ z)))))
                  (assert (string= "planks" (cell-name viewer (agent-cell viewer (+ x 3) (1- y) z))))
                  (assert (string= "air" (cell-name viewer (agent-cell viewer (+ x 5) (1- y) z))))
                  (dolist (block (list luvcraft::*bricks-block* luvcraft::*planks-block*))
                    (assert (= 2 (luvcraft:block-inventory-entry-quantity
                                  (luvcraft:block-inventory-entry-for (viewer-inventory viewer) block)))))
                  (assert (> (first (vec3:vec3-list (inhabitant-position gnome))) (+ x 1.5)))
                  (show-companion-message viewer gnome)))
               (capture "result")
               (luv.application-agent:print-transcript turn)
               (format t "~&Feet before ~S; after ~S. Bricks 3 -> 1 -> 2; planks 3 -> 2.~%"
                       start (vec3:vec3-list (inhabitant-position gnome)))
               ;; Zero-stock and body-occupancy controls use the real game edit boundary.
               (on-canvas
                (lambda ()
                  (assert (eq :empty (nth-value 1 (edit-agent-cell viewer gnome
                                                                  (agent-cell viewer (+ x 5) (1+ y) z)
                                                                  (game:block-placement luvcraft::*stone-block*)))))
                  (assert (eq :occupied (nth-value 1
                                         (render:apply-viewer-world-edit
                                          viewer (agent-cell viewer
                                                             (floor (vec3:vec3-x (inhabitant-position gnome)))
                                                             (floor (vec3:vec3-y (inhabitant-position gnome))) z)
                                          (game:block-placement luvcraft::*bricks-block*)))))))
               (on-canvas (lambda () (summon-inhabitant viewer :cat) (summon-inhabitant viewer :fantus)))
               (capture "companions")
               (render:stop-viewer viewer)
               (assert closed)
               (multiple-value-bind (world resume inhabitants) (luvcraft:read-luvcraft-save #p"build/companion-proof.sexp")
                 (declare (ignore world))
                 (assert (= 3 (count-if (lambda (d) (member (first d) '(:gnome :cat :fantus))) inhabitants)))
                 (assert (string= "scripted-response" (getf (rest (assoc :gnome inhabitants)) :conversation)))
                 (let ((bag (restore-inventory resume)))
                   (assert (= 2 (luvcraft:block-inventory-entry-quantity
                                 (luvcraft:block-inventory-entry-for bag luvcraft::*bricks-block*)))))))
             (setf viewer (play :pathname #p"build/companion-proof.sexp" :width 1100 :height 760)
                   canvas (render::viewer-canvas viewer))
             (settle)
             (on-canvas (lambda () (initialize-viewer-agents viewer)))
             (assert (= 3 (length (viewer-agents viewer))))
             (on-canvas
              (lambda ()
                (setf (render::viewer-running-p viewer) nil)
                (let ((camera (render:viewer-camera viewer)))
                  (setf (render:camera-position camera) (vec3:make-vec3 1035.0 1034.0 10.5)
                        (render:camera-yaw camera) (atan -8.0 -7.0)
                        (render:camera-pitch camera) -0.4))))
             (capture "reopened")
             (format t "~&Companion proof passed: real prompt, tools, movement, build, fetch, approval/denial, controls, save/reopen and provider close.~%"))
        (setf *inhabitant-provider-factory* factory)))))

(check-luft-companions)
