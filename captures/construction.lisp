(in-package #:luv.showcase)

;;; The construction proposal as a reproducible little scene.  This is the
;;; first capture recipe attached to the possible-world design. #Y7X7WK

(defun call-with-construction-proposal-capture (function)
  "Call FUNCTION with a generated-world session, gnome, and inert proposal."
  (let ((approval nil)
        (world
          (make-staged-gallery-terrain-world
           (lambda (world floor-y)
             (declare (ignore world floor-y)))
           :bounds '(4 4 12 12))))
    (call-with-gallery-session
     (lambda (session)
       (unwind-protect
            (let* ((gnome
                     (agent:spawn-agent
                      :session session :x 6
                      :y (1+ +gallery-stage-floor-y+) :z 8))
                   (change-set
                     (agent::make-additive-box-change-set
                      world (luvcraft:block-kind-named :stone-bricks)
                      8 (1+ +gallery-stage-floor-y+) 8
                      10 (+ +gallery-stage-floor-y+ 2) 8))
                   (proposal
                     (setf approval
                           (make-instance
                            'agent::construction-approval
                            :agent nil :presence gnome :session session
                            :change-set change-set))))
              (agent::install-construction-approval proposal)
              (luvcraft:focus-luvcraft-session session gnome)
              (funcall function session gnome proposal))
         (when approval
           (agent:deny-tool-approval approval "Capture complete."))))
     :title "construction proposal capture"
     :width 960 :height 640 :clean-p t
     :world world
     :camera
     (make-gallery-camera
      (luvcraft::make-camera-pose
       (luvcraft::make-vec3
        5.5d0 (+ +gallery-stage-floor-y+ 4.0d0) 1.5d0)
       0.15d0 -0.25d0 luvcraft::+luvcraft-camera-vertical-field-of-view+))
     :sky-clock (luvcraft::make-pinned-sky-clock 0.42)
     :sky-profile (luvcraft:make-default-sky-profile)
     :residency-radius +gallery-terrain-radius+)))

(luv:define-capture construction-proposal-still
    (:figure Y7X7WK :kind :image :extension "png" :section :play
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
    (:figure Y7X7WK :kind :video :extension "mp4" :section :play
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
