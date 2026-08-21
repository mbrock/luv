(in-package #:luv.showcase)

;;; The first landscape subject in the showcase gazetteer: one seed, one
;;; art-directed view, and one grade shared by its still and motion proof.
;;; #NLCFX0 #2ILB5D

(defconstant +pre-noon-ridge-day-fraction+ 0.40625
  "Quarter to ten in the morning, expressed as a fraction of one day.")

(defconstant +pre-noon-ridge-width+ 1200)
(defconstant +pre-noon-ridge-height+ 800)
(defconstant +pre-noon-ridge-film-seconds+ 7)
(defconstant +pre-noon-ridge-film-frame-rate+ 24)

(defun make-pre-noon-ridge-sky-profile ()
  "Make the fixed warm morning environment used by both ridge captures."
  (luvcraft:make-sky-profile
   (list
    (luvcraft:make-sky-keyframe
     :day-fraction +pre-noon-ridge-day-fraction+
     :zenith-color #(0.18 0.42 0.88)
     :horizon-color #(0.62 0.76 0.96)
     :sun-color #(1.24 1.04 0.76)
     :ambient-color #(0.40 0.51 0.72)
     :fog-color #(0.48 0.66 0.88)
     :sun-angular-width 0.009
     :exposure 1.0
     :cloudiness 0.46
     :fog-near 34.0
     :fog-far 126.0))))

(defun pre-noon-ridge-camera-pose (&optional (lateral-offset 0.0d0))
  "Return the ridge pose, dollyed LATERAL-OFFSET cells along camera-right.

The eye stands at human height on seed 121's western grass shelf.  Looking
east-northeast puts its snowy central rise in the middle distance, three tree
crowns across the foreground, and the quarter-to-ten sun just beyond the
upper-left edge."
  (let ((yaw 1.05d0))
    (luvcraft::make-camera-pose
     (luvcraft::make-vec3
      (+ -46.0d0 (* lateral-offset (cos yaw)))
      8.75d0
      (- -36.0d0 (* lateral-offset (sin yaw))))
     yaw 0.20d0
     luvcraft::+luvcraft-camera-vertical-field-of-view+)))

(defun make-pre-noon-ridge-camera ()
  (let ((camera (make-instance 'luvcraft:fly-camera)))
    (luvcraft::set-camera-pose camera (pre-noon-ridge-camera-pose))
    camera))

(defun call-with-pre-noon-ridge-grade (function)
  "Call FUNCTION while the canvas-thread-visible live grade is pinned.

The renderer deliberately keeps these values global so its canvas thread sees
live SLY changes.  A capture therefore sets and restores the globals rather
than dynamically binding values on the directing thread."
  (let ((exposure luvcraft::*luvcraft-exposure*)
        (crosshair-p luvcraft::*luvcraft-crosshair-p*)
        (focus-blur-p luvcraft::*luvcraft-focus-blur-p*)
        (bloom-gain luvcraft::*luvcraft-bloom-gain*)
        (shaft-gain luvcraft::*luvcraft-shaft-gain*)
        (vignette luvcraft::*luvcraft-vignette*)
        (bloom-threshold luvcraft::*luvcraft-bloom-threshold*)
        (shaft-decay luvcraft::*luvcraft-shaft-decay*)
        (sun-orbit-tilt luvcraft::*sky-sun-orbit-tilt*)
        (shadow-base-bias luvcraft::*luvcraft-shadow-base-bias*)
        (shadow-slope-bias luvcraft::*luvcraft-shadow-slope-bias*)
        (shadow-minimum-radius
          luvcraft::*luvcraft-shadow-minimum-filter-radius*)
        (shadow-maximum-radius
          luvcraft::*luvcraft-shadow-maximum-filter-radius*)
        (chromatic-aberration
          luvcraft.shaders::*chromatic-aberration*)
        (grade-saturation luvcraft.shaders::*grade-saturation*)
        (grade-contrast luvcraft.shaders::*grade-contrast*))
    (unwind-protect
         (progn
           (setf luvcraft::*luvcraft-exposure* 0.48
                 luvcraft::*luvcraft-crosshair-p* nil
                 luvcraft::*luvcraft-focus-blur-p* nil
                 luvcraft::*luvcraft-bloom-gain* 0.18
                 luvcraft::*luvcraft-shaft-gain* 0.28
                 luvcraft::*luvcraft-vignette* 0.42
                 luvcraft::*luvcraft-bloom-threshold* 1.55
                 luvcraft::*luvcraft-shaft-decay* 0.955
                 luvcraft::*sky-sun-orbit-tilt* 0.44
                 luvcraft::*luvcraft-shadow-base-bias* 0.00405
                 luvcraft::*luvcraft-shadow-slope-bias* 0.002
                 luvcraft::*luvcraft-shadow-minimum-filter-radius* 8.0
                 luvcraft::*luvcraft-shadow-maximum-filter-radius* 24.0
                 luvcraft.shaders::*chromatic-aberration* 0.70
                 luvcraft.shaders::*grade-saturation* 1.10
                 luvcraft.shaders::*grade-contrast* 0.16)
           (funcall function))
      (setf luvcraft::*luvcraft-exposure* exposure
            luvcraft::*luvcraft-crosshair-p* crosshair-p
            luvcraft::*luvcraft-focus-blur-p* focus-blur-p
            luvcraft::*luvcraft-bloom-gain* bloom-gain
            luvcraft::*luvcraft-shaft-gain* shaft-gain
            luvcraft::*luvcraft-vignette* vignette
            luvcraft::*luvcraft-bloom-threshold* bloom-threshold
            luvcraft::*luvcraft-shaft-decay* shaft-decay
            luvcraft::*sky-sun-orbit-tilt* sun-orbit-tilt
            luvcraft::*luvcraft-shadow-base-bias* shadow-base-bias
            luvcraft::*luvcraft-shadow-slope-bias* shadow-slope-bias
            luvcraft::*luvcraft-shadow-minimum-filter-radius*
            shadow-minimum-radius
            luvcraft::*luvcraft-shadow-maximum-filter-radius*
            shadow-maximum-radius
            luvcraft.shaders::*chromatic-aberration* chromatic-aberration
            luvcraft.shaders::*grade-saturation* grade-saturation
            luvcraft.shaders::*grade-contrast* grade-contrast))))

(defun call-with-pre-noon-ridge-session (function)
  "Call FUNCTION with the fully resident hidden session for both ridge shots."
  (call-with-pre-noon-ridge-grade
   (lambda ()
     (let ((session nil))
       (unwind-protect
            (progn
              (setf session
                    (luvcraft:start-luvcraft
                     :title "showcase - pre-noon ridge"
                     :width +pre-noon-ridge-width+
                     :height +pre-noon-ridge-height+
                     :visible-p nil
                     :frames-per-second nil
                     :world (luvcraft::make-empty-little-block-world :seed 121)
                     :camera (make-pre-noon-ridge-camera)
                     :sky-clock
                     (luvcraft::make-pinned-sky-clock
                      +pre-noon-ridge-day-fraction+)
                     :sky-profile (make-pre-noon-ridge-sky-profile)
                     ;; Four chunks reach from the western shelf across the
                     ;; central ridge without making a small dolly stream.
                     :residency-radius 4))
              ;; A capture is not a lobby client, and no presentation overlay
              ;; is part of this clean landscape layer selection.
              (luvcraft::stop-luvcraft-lobby session)
              (dolist (overlay
                        (copy-list
                         (luvcraft:luvcraft-session-overlays session)))
                (luvcraft:remove-luvcraft-overlay session overlay))
              ;; The still needs the same complete terrain window that FILM-
              ;; LUVCRAFT-SESSION requires before beginning consecutive frames.
              (format t "capture pre-noon ridge: preparing ~D terrain chunks~%"
                      (hash-table-count
                       (luvcraft::luvcraft-session-desired-chunks session)))
              (finish-output)
              (luvcraft::wait-for-luvcraft-products
               session
               :minimum
               (hash-table-count
                (luvcraft::luvcraft-session-desired-chunks session)))
              (format t "capture pre-noon ridge: terrain ready~%")
              (finish-output)
              (funcall function session))
         (when session
           (luvcraft:stop-luvcraft session)))))))

(luv:define-capture pre-noon-ridge-still
    (:figure NLCFX0 :kind :image :extension "png"
     :description
     "Warm pre-noon light across seed 121's tree-broken central ridge.")
    (pathname)
  (call-with-pre-noon-ridge-session
   (lambda (session)
     (luvcraft:capture-luvcraft-screenshot
      session pathname
      :camera-pose (pre-noon-ridge-camera-pose)
      :include-hud-p nil
      :include-viewmodel-p nil))))

(luv:define-capture pre-noon-ridge-glide
    (:figure NLCFX0 :kind :video :extension "mp4"
     :description
     "A slow lateral glide holding the warm ridge, trees, fog, and sun steady.")
    (pathname)
  (call-with-pre-noon-ridge-session
   (lambda (session)
     (let* ((seconds +pre-noon-ridge-film-seconds+)
            (frame-rate +pre-noon-ridge-film-frame-rate+)
            (frame-count (* seconds frame-rate)))
       (luvcraft:film-luvcraft-session
        session pathname
        :seconds seconds
        :frame-rate frame-rate
        :include-hud-p nil
        :include-viewmodel-p nil
        :before-frame
        (lambda (frame)
          (let* ((progress (if (= frame-count 1)
                               0.0d0
                               (/ frame (1- frame-count))))
                 (lateral-offset (+ -2.0d0 (* 4.0d0 progress))))
            (luvcraft::set-camera-pose
             (luvcraft:luvcraft-session-camera session)
             (pre-noon-ridge-camera-pose lateral-offset)))
          (when (zerop (mod frame frame-rate))
            (format t "capture pre-noon-ridge-glide: second ~D/~D~%"
                    (1+ (/ frame frame-rate)) seconds)
            (finish-output))))))))
