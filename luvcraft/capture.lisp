;;; Direct luvcraft framebuffer capture for live use, smoke tests, and CI.

(in-package #:luvcraft)

(defmethod luv:capture-canvas ((session luvcraft-session))
  (luvcraft-session-canvas session))

(defmethod luv:prepare-capture
    ((session luvcraft-session) (capture luv:application-capture))
  ;; A still wants a useful initial neighborhood.  A film must not publish a
  ;; late chunk halfway through a supposedly continuous authored shot.
  (ecase (luv:capture-kind capture)
    (:screenshot
     (wait-for-luvcraft-products session))
    (:film
     (wait-for-luvcraft-products
      session
      :minimum
      (hash-table-count (luvcraft-session-desired-chunks session)))))
  session)

(defmethod luv:encode-capture-frame
    ((session luvcraft-session) (capture luv:application-capture)
     encoder target extent)
  (declare (ignore extent))
  (let* ((camera (luvcraft-session-camera session))
         (saved-pose (camera-pose-from-camera camera))
         (camera-pose (luv:capture-option capture :camera-pose))
         (pose (if (functionp camera-pose)
                   (funcall camera-pose session)
                   camera-pose))
         (metadata-function
           (luv:capture-option capture :metadata-function))
         (metadata nil))
    (unwind-protect
         (progn
           (when pose (set-camera-pose camera pose))
           (when metadata-function
             (setf metadata (funcall metadata-function session)))
           ;; ENCODE-LUVCRAFT-FRAME resolves its scene into TARGET.  The shared
           ;; capture transaction owns the following target-to-buffer copy.
           (encode-luvcraft-frame
            session target encoder
            :include-hud-p
            (luv:capture-option capture :include-hud-p t)
            :include-viewmodel-p
            (luv:capture-option capture :include-viewmodel-p t)))
      (set-camera-pose camera saved-pose))
    metadata))

(defun release-luvcraft-capture-frame-state (session target)
  "Release the per-target frame state cached while encoding TARGET."
  (let* ((renderer (luvcraft-session-renderer session))
         (states (luvcraft-session-frame-states session))
         (key (canvas-frame-resource-key
               (luvcraft-session-context session) target))
         (state (gethash key states)))
    (when state
      (with-release-report
        (dolist (resource
                  (remove-duplicates
                   (luvcraft-frame-state-resources state) :test #'eq))
          (releasing :capture-frame-state-resource
            (release-luvcraft-renderer-resource renderer resource)))
        (remhash key states))))
  (values))

(defmethod luv:cleanup-capture
    ((session luvcraft-session) (capture luv:application-capture))
  (let ((target (luv:capture-target capture))
        (canvas (luvcraft-session-canvas session)))
    (when (and target (eq :open (canvas-state canvas)))
      (request-canvas-frame
       canvas
       (lambda (timestamp)
         (declare (ignore timestamp))
         ;; The target is still alive here.  Never leave its identity and the
         ;; bind groups that name it retained in the long-lived renderer.
         (release-luvcraft-capture-frame-state session target)))))
  (values))

(defun capture-luvcraft-screenshot
    (session pathname &key camera-pose metadata-function
                           (include-hud-p t) (include-viewmodel-p t))
  "Render SESSION once offscreen and write its native GPU frame to PNG.

This works for both live and hidden sessions.  The call returns only after GPU
readback and compressed PNG writing have completed; it does not inspect the
host window or depend on the window being visible.  CAMERA-POSE may be a pose
or a function of SESSION evaluated on the canvas thread; the session camera is
restored before the call returns.  METADATA-FUNCTION, when supplied, is called
there under the capture pose and its value is returned last."
  (luv:capture-application-screenshot
   session pathname
   :label "luvcraft screenshot"
   :options
   (list :camera-pose camera-pose
         :metadata-function metadata-function
         :include-hud-p include-hud-p
         :include-viewmodel-p include-viewmodel-p)))

(defun temporal-derivative-rgba
    (current previous scale &optional previous-previous)
  "Return a visible RGBA temporal derivative and normalized summary values.

With PREVIOUS-PREVIOUS, compute the absolute second temporal derivative;
otherwise compute the absolute first derivative.  RGB channel order is
irrelevant because the output is grayscale.  Values returned after the image
are normalized mean magnitude, maximum magnitude, and changed-pixel fraction."
  (unless (= (length current) (length previous))
    (error "Temporal derivative frame sizes differ: ~D and ~D."
           (length current) (length previous)))
  (when (and previous-previous
             (/= (length current) (length previous-previous)))
    (error "Second temporal derivative frame sizes differ: ~D and ~D."
           (length current) (length previous-previous)))
  (unless (zerop (mod (length current) 4))
    (error "Temporal derivative expects tightly packed RGBA pixels."))
  (check-type scale (real 0))
  (let ((output (make-array (length current)
                            :element-type '(unsigned-byte 8)))
        (sum 0)
        (maximum 0)
        (changed 0)
        (pixel-count (/ (length current) 4)))
    (loop for offset from 0 below (length current) by 4
          for magnitude =
          (round
           (/ (loop for channel below 3
                    for current-value = (aref current (+ offset channel))
                    for previous-value = (aref previous (+ offset channel))
                    sum (abs
                         (if previous-previous
                             (+ current-value
                                (- (* 2 previous-value))
                                (aref previous-previous (+ offset channel)))
                             (- current-value previous-value))))
              3))
          for visible = (min 255 (round (* scale magnitude)))
          do (incf sum magnitude)
             (setf maximum (max maximum magnitude))
             (when (plusp magnitude)
               (incf changed))
             (setf (aref output offset) visible
                   (aref output (+ offset 1)) visible
                   (aref output (+ offset 2)) visible
                   (aref output (+ offset 3)) 255))
    (values output
            (/ sum (* pixel-count 255.0))
            (/ maximum 255.0)
            (/ changed (float pixel-count)))))

(defun write-temporal-derivative-summary (pathname rows)
  "Write temporal derivative metric ROWS to a small CSV file."
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (format stream "frame,d1_mean,d1_max,d1_changed,d2_mean,d2_max,d2_changed~%")
    (dolist (row rows)
      (format stream "~D,~,8F,~,8F,~,8F," (first row) (second row)
              (third row) (fourth row))
      (when (fifth row)
        (format stream "~,8F" (fifth row)))
      (write-char #\, stream)
      (when (sixth row)
        (format stream "~,8F" (sixth row)))
      (write-char #\, stream)
      (when (seventh row)
        (format stream "~,8F" (seventh row)))
      (terpri stream)))
  pathname)

(defun hidden-luvcraft-frame-pathname
    (directory index &optional (prefix "block-world"))
  (merge-pathnames
   (format nil "~A-~3,'0D.png" prefix index)
   (uiop:ensure-directory-pathname directory)))

(defun capture-hidden-luvcraft-screenshot
    (pathname &key
                (title "luv hidden block world")
                (width 960) (height 640)
                (world (make-empty-little-block-world))
                (mesher (make-instance 'exposed-face-mesher))
                (camera (make-instance 'fly-camera))
                (critters (make-instance 'critter-population))
                (provider *gpu-provider*)
                (world-text-string nil)
                (world-text-distance 8.0)
                (world-text-lift 3.0)
                (world-text-units-per-em 0.55)
                (sky-clock (make-instance 'sky-clock
                                          :pinned-day-fraction 0.5))
                (sky-profile (make-default-sky-profile)))
  "Open a hidden SDL canvas, render one block-world frame, and save it.

The sky clock arrives pinned at noon so captures stay byte-deterministic;
pass an unpinned clock to photograph another time of day.  A capture never
runs the frame simulation, so any animals it wants in shot arrive already
placed in CRITTERS rather than growing around a player."
  (let ((session nil))
    (unwind-protect
         (progn
           (setf session
                 (start-luvcraft
                  :title title :width width :height height
                  :frames-per-second nil :visible-p nil
                  ;; Authored hidden captures name output pixels explicitly.
                  ;; Live-window screenshots retain the session's native Retina
                  ;; presentation extent through CAPTURE-LUVCRAFT-SCREENSHOT.
                  :high-pixel-density-p nil
                  :provider provider
                  :world world :mesher mesher :camera camera
                  :critters critters
                  :world-text-string world-text-string
                  :world-text-distance world-text-distance
                  :world-text-lift world-text-lift
                  :world-text-units-per-em world-text-units-per-em
                  :sky-clock sky-clock
                  :sky-profile sky-profile))
           (capture-luvcraft-screenshot session pathname))
      (when session
        (stop-luvcraft session)))))

(defun capture-hidden-luvcraft-text-closeup
    (pathname &key (provider *gpu-provider*))
  "Render a native-resolution close-up of the world Slug text proof.

The wide viewport and enlarged world-space text make outline, band-selection,
and coverage defects visible without scaling up a smaller raster afterward.
#9G0Z19"
  (capture-hidden-luvcraft-screenshot
   pathname
   :title "luv Slug world text close-up"
   :width 1280 :height 360
   :provider provider
   :camera
   (make-instance
    'fly-camera
    :position (make-vec3 8.0 24.0 -6.0)
    :yaw 0.0 :pitch 0.02)
   :world-text-string "hello, world"
   :world-text-distance 6.0
   :world-text-lift 2.2
   :world-text-units-per-em 3.4))

(defun capture-hidden-luvcraft-frames
    (directory &key
                 (count 6)
                 (title "luv hidden block world")
                 (width 960) (height 640)
                 (yaw-step 0.35)
                 (forward-step 0.0)
                 day-start
                 (day-step 0.0)
                 difference-scale
                 (shadow-diagnostic-p nil)
                 (pathname-prefix "block-world")
                 (world (make-empty-little-block-world))
                 (mesher (make-instance 'exposed-face-mesher))
                 (camera (make-instance 'fly-camera))
                 (critters (make-instance 'critter-population))
                 (provider *gpu-provider*)
                 (sky-clock (make-instance 'sky-clock
                                           :pinned-day-fraction 0.5))
                 (sky-profile (make-default-sky-profile)))
  "Capture COUNT hidden block-world frames into DIRECTORY.

Each frame reuses one hidden SDL canvas on PROVIDER, advances CAMERA's yaw by
YAW-STEP, moves FORWARD-STEP world units along its initial heading, and moves
the evaluated sky by DAY-STEP day fractions.  DAY-START can replace the
clock's initial time.  When DIFFERENCE-SCALE is non-NIL, also write amplified
first and second frame derivatives plus a CSV of unscaled normalized metrics.
SHADOW-DIAGNOSTIC-P replaces block materials with direct-shadow visibility so
the derivatives are not confounded by albedo, sky colour, or fog changes.
This is a consecutive-view capture, not a set of independently restarted
scenes."
  (check-type count (integer 1))
  (check-type yaw-step real)
  (check-type forward-step real)
  (when day-start
    (check-type day-start real))
  (check-type day-step real)
  (when difference-scale
    (check-type difference-scale (real 0)))
  (let ((directory (uiop:ensure-directory-pathname directory))
        (session nil)
        (previous-pixels nil)
        (previous-previous-pixels nil)
        (derivative-rows '())
        (outputs '())
        (initial-x (camera-x camera))
        (initial-z (camera-z camera))
        (initial-yaw (camera-yaw camera))
        (initial-day-fraction
          (or day-start (sky-clock-current-day-fraction sky-clock))))
    (ensure-directories-exist directory)
    (unwind-protect
         (progn
           (setf session
                 (start-luvcraft
                  :title title :width width :height height
                  :frames-per-second nil :visible-p nil
                  :high-pixel-density-p nil
                  :provider provider
                  :world world :mesher mesher :camera camera
                  :critters critters
                  :sky-clock sky-clock :sky-profile sky-profile
                  :shadow-diagnostic-p shadow-diagnostic-p))
           ;; Temporal evidence requires a fixed scene.  The ordinary capture
           ;; threshold of nine products is enough for a useful screenshot but
           ;; allowed later desired chunks to publish in the middle of a
           ;; derivative sequence.
           (wait-for-luvcraft-products
            session
            :minimum
            (hash-table-count (luvcraft-session-desired-chunks session)))
           (loop for index below count
                 for pathname =
                 (hidden-luvcraft-frame-pathname
                  directory index pathname-prefix)
                 do (setf (camera-yaw camera)
                          (+ initial-yaw (* yaw-step index))
                          (camera-x camera)
                          (+ initial-x
                             (* forward-step index (sin initial-yaw)))
                          (camera-z camera)
                          (+ initial-z
                             (* forward-step index (cos initial-yaw))))
                    (let ((day-fraction
                            (mod (+ initial-day-fraction (* day-step index))
                                 1.0)))
                      (if (sky-clock-pinned-day-fraction sky-clock)
                          (setf (sky-clock-pinned-day-fraction sky-clock)
                                day-fraction)
                          (setf (sky-clock-day-fraction sky-clock)
                                day-fraction)))
                    (multiple-value-bind
                        (written pixels frame-width frame-height format)
                        (capture-luvcraft-screenshot session pathname)
                      (push written outputs)
                      (format t "~&capture ~A: frame ~D/~D~%"
                              pathname-prefix (1+ index) count)
                      (finish-output)
                      (when (and difference-scale previous-pixels)
                        (multiple-value-bind
                            (difference mean maximum changed)
                            (temporal-derivative-rgba
                             pixels previous-pixels difference-scale)
                          (let ((difference-pathname
                                  (hidden-luvcraft-frame-pathname
                                   directory index
                                   (format nil "~A-d1" pathname-prefix))))
                            (write-rgba-png difference-pathname difference
                                            frame-width frame-height format)
                            (push difference-pathname outputs))
                          (let ((row (list index mean maximum changed
                                           nil nil nil)))
                            (when previous-previous-pixels
                              (multiple-value-bind
                                  (second-difference second-mean second-maximum
                                   second-changed)
                                  (temporal-derivative-rgba
                                   pixels previous-pixels difference-scale
                                   previous-previous-pixels)
                                (let ((difference-pathname
                                        (hidden-luvcraft-frame-pathname
                                         directory index
                                         (format nil "~A-d2"
                                                 pathname-prefix))))
                                  (write-rgba-png
                                   difference-pathname second-difference
                                   frame-width frame-height format)
                                  (push difference-pathname outputs))
                                (setf (fifth row) second-mean
                                      (sixth row) second-maximum
                                      (seventh row) second-changed)))
                            (push row derivative-rows))))
                      (setf previous-previous-pixels previous-pixels
                            previous-pixels pixels)))
           (when difference-scale
             (let ((summary
                     (merge-pathnames
                      (format nil "~A-temporal.csv" pathname-prefix)
                      directory)))
               (write-temporal-derivative-summary
                summary (nreverse derivative-rows))
               (push summary outputs)))
           (nreverse outputs))
      (when session
        (stop-luvcraft session)))))
