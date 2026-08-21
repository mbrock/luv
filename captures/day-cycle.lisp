(in-package #:luv.showcase)

;;; One fixed little-world plate at six hours and across one continuous day.
;;; The clock is the only authored variable: world, camera, viewport, sky
;;; profile, presentation grade, and compositing layers stay fixed. #TRYHPN

(defparameter *little-world-day-cycle-grade*
  '((luvcraft::*luvcraft-exposure* . 0.45)
    (luvcraft::*luvcraft-bloom-threshold* . 1.5)
    (luvcraft::*luvcraft-bloom-gain* . 0.22)
    (luvcraft::*luvcraft-shaft-gain* . 0.30)
    (luvcraft::*luvcraft-shaft-decay* . 0.955)
    (luvcraft::*luvcraft-vignette* . 0.60)
    (luvcraft.shaders::*chromatic-aberration* . 0.70)
    (luvcraft.shaders::*grade-saturation* . 1.10)
    (luvcraft.shaders::*grade-contrast* . 0.16))
  "The complete fixed presentation grade shared by the day-cycle folio.")

(defun call-with-little-world-day-cycle-capture (day-fraction function)
  "Call FUNCTION with the fixed #TRYHPN session and its pinned sky clock.

The caller may change only the clock's pinned day fraction.  Teardown restores
the process-wide grade even when session shutdown or FUNCTION signals."
  (check-type day-fraction real)
  (let ((session nil)
        (saved-grade
          (mapcar (lambda (entry)
                    (cons (car entry) (symbol-value (car entry))))
                  *little-world-day-cycle-grade*)))
    (unwind-protect
         (progn
           ;; These controls are process globals because the renderer reads
           ;; them on its canvas thread.  Set them before constructing the
           ;; pipelines and restore them only after the session is gone.
           (dolist (entry *little-world-day-cycle-grade*)
             (setf (symbol-value (car entry)) (cdr entry)))
           (let* ((world (luvcraft:make-empty-little-block-world :seed 121))
                  (camera
                    (make-instance
                     'luvcraft:fly-camera
                     :position (luvcraft::make-vec3 8.0d0 11.0d0 -18.0d0)
                     :yaw 0.0d0 :pitch -0.30d0))
                  (clock (luvcraft::make-pinned-sky-clock day-fraction)))
             (setf session
                   (luvcraft:start-luvcraft
                    :title "little world - one fixed day"
                    :width 960 :height 640 :visible-p nil
                    :frames-per-second nil
                    :world world :camera camera
                    :residency-radius 4
                    :sky-clock clock
                    :sky-profile (luvcraft:make-default-sky-profile)))
             (luvcraft::stop-luvcraft-lobby session)
             (dolist (overlay
                       (copy-list
                        (luvcraft:luvcraft-session-overlays session)))
               (luvcraft:remove-luvcraft-overlay session overlay))
             (luvcraft:wait-for-luvcraft-products
              session
              :minimum
              (hash-table-count
               (luvcraft::luvcraft-session-desired-chunks session)))
             (funcall function session clock)))
      (unwind-protect
           (when session
             (luvcraft:stop-luvcraft session))
        (dolist (entry saved-grade)
          (setf (symbol-value (car entry)) (cdr entry)))))))

(defun capture-little-world-day-cycle-still
    (pathname day-fraction)
  "Capture one clean plate from the fixed day-cycle scene."
  (call-with-little-world-day-cycle-capture
   day-fraction
   (lambda (session clock)
     (declare (ignore clock))
     (luvcraft:capture-luvcraft-screenshot
      session pathname :include-hud-p nil :include-viewmodel-p nil))))

(luv:define-capture little-world-blue-hour
    (:figure TRYHPN :kind :image :extension "png"
     :description
     "The fixed little world still readable in the blue hour before sunrise.")
    (pathname)
  (capture-little-world-day-cycle-still pathname 0.21))

(luv:define-capture little-world-sunrise
    (:figure TRYHPN :kind :image :extension "png"
     :description
     "The fixed little world at sunrise: warm horizon and long shadows.")
    (pathname)
  (capture-little-world-day-cycle-still pathname 0.28))

(luv:define-capture little-world-pre-noon
    (:figure TRYHPN :kind :image :extension "png"
     :description
     "The fixed little world in neutral pre-noon light for comparison.")
    (pathname)
  (capture-little-world-day-cycle-still pathname 0.42))

(luv:define-capture little-world-noon
    (:figure TRYHPN :kind :image :extension "png"
     :description
     "The fixed little world at noon with short shadows and clear materials.")
    (pathname)
  (capture-little-world-day-cycle-still pathname 0.50))

(luv:define-capture little-world-sunset
    (:figure TRYHPN :kind :image :extension "png"
     :description
     "The fixed little world under warm lateral sunset light and fog.")
    (pathname)
  (capture-little-world-day-cycle-still pathname 0.72))

(luv:define-capture little-world-deep-night
    (:figure TRYHPN :kind :image :extension "png"
     :description
     "The fixed little world as an ambient deep-night silhouette.")
    (pathname)
  (capture-little-world-day-cycle-still pathname 0.90))

(luv:define-capture little-world-one-day
    (:figure TRYHPN :kind :video :extension "mp4"
     :description
     "Eight seconds through one whole sky day from a completely fixed view.")
    (pathname)
  (call-with-little-world-day-cycle-capture
   0.18
   (lambda (session clock)
     (let* ((seconds 8)
            (frame-rate 24)
            (frame-count (* seconds frame-rate)))
       (luvcraft:film-luvcraft-session
        session pathname :seconds seconds :frame-rate frame-rate
        :include-hud-p nil :include-viewmodel-p nil
        :before-frame
        (lambda (frame)
          ;; Hidden film frames do not pass through the presentation loop
          ;; which normally supplies elapsed time to the cloud shader.  Give
          ;; this authored film its deterministic capture time explicitly.
          (setf (luvcraft::luvcraft-session-last-frame-time session)
                (/ frame (coerce frame-rate 'double-float)))
          ;; The final encoded frame falls one interval before the starting
          ;; hour, making the following playback-loop boundary continuous.
          (setf (luvcraft:sky-clock-pinned-day-fraction clock)
                (mod (+ 0.18 (/ frame (coerce frame-count 'single-float)))
                     1.0))
          (when (zerop (mod frame frame-rate))
            (format t "capture little-world-one-day: second ~D/~D~%"
                    (1+ (/ frame frame-rate)) seconds)
            (finish-output))))))))
