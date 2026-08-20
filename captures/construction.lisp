(in-package #:luv.showcase)

;;; The construction proposal as a reproducible little scene.  This is the
;;; first capture recipe attached to the possible-world design. #Y7X7WK

(defun call-with-construction-proposal-capture (function)
  "Call FUNCTION with a hidden session, gnome, and inert wall proposal."
  (let ((session nil)
        (approval nil))
    (unwind-protect
         (let* ((world (luvcraft::make-gazetteer-meadow-world))
                (camera
                  (make-instance
                   'luvcraft:fly-camera
                   :position (luvcraft::make-vec3 5.5d0 5.0d0 1.5d0)
                   :yaw 0.15d0 :pitch -0.25d0)))
           (setf session
                 (luvcraft:start-luvcraft
                  :title "construction proposal capture"
                  :width 960 :height 640 :visible-p nil
                  :frames-per-second nil
                  :world world :camera camera
                  :sky-clock (luvcraft::make-pinned-sky-clock 0.42)))
           ;; The lobby is useful in a game and accidental in a design plate.
           (dolist (overlay (copy-list
                             (luvcraft:luvcraft-session-overlays session)))
             (when (typep overlay 'mcluv::luvcraft-lobby-hud-overlay)
               (luvcraft:remove-luvcraft-overlay session overlay)))
           (luvcraft::stop-luvcraft-lobby session)
           (let* ((gnome (agent:spawn-agent :session session :x 6 :y 2 :z 8))
                  (change-set
                    (agent::make-additive-box-change-set
                     world (luvcraft:block-kind-named :stone-bricks)
                     8 2 8 10 3 8))
                  (proposal
                    (setf approval
                          (make-instance
                           'agent::construction-approval
                           :agent nil :presence gnome :session session
                           :change-set change-set))))
             (agent::install-construction-approval proposal)
             (luvcraft:focus-luvcraft-session session gnome)
             (funcall function session gnome proposal)))
      (when approval
        (agent:deny-tool-approval approval "Capture complete."))
      (when session
        (luvcraft:stop-luvcraft session)))))

(luv:define-capture construction-proposal-still
    (:figure Y7X7WK :kind :image :extension "png"
     :description
     "The textured, glowing possible-world wall beside its proposing gnome.")
    (pathname)
  (call-with-construction-proposal-capture
   (lambda (session gnome approval)
     (declare (ignore approval))
     (luvcraft:capture-luvcraft-screenshot
      session pathname
      :camera-pose (luvcraft:luvcraft-focus-camera-pose gnome session)
      :include-viewmodel-p nil))))

(luv:define-capture construction-proposal-orbit
    (:figure Y7X7WK :kind :video :extension "mp4"
     :description
     "A short focus-camera orbit around the inert construction proposal.")
    (pathname)
  (call-with-construction-proposal-capture
   (lambda (session gnome approval)
     (declare (ignore approval))
     (let ((frame-rate 24))
       (luvcraft:film-luvcraft-session
        session pathname :seconds 4 :frame-rate frame-rate
        :include-viewmodel-p nil
        :before-frame
        (lambda (frame)
          (luvcraft::set-camera-pose
           (luvcraft:luvcraft-session-camera session)
           (luvcraft:luvcraft-focus-camera-pose gnome session))
          (when (zerop (mod frame frame-rate))
            (format t "capture construction-proposal-orbit: second ~D/4~%"
                    (1+ (/ frame frame-rate)))
            (finish-output))))))))
