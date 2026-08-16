;;; Offscreen luvcraft captures for smoke tests and CI-ish environments.

(in-package #:luvcraft)

(defun capture-luvcraft-screenshot (session pathname)
  "Render SESSION once, read its real color attachment, and write a PNG."
  (unless (eq :open (canvas-state (luvcraft-session-canvas session)))
    (error "Cannot capture a closed luvcraft session."))
  (wait-for-luvcraft-products session)
  (let* ((context (luvcraft-session-context session))
         (extent (canvas-extent context))
         (buffer
           (create
            (luvcraft-session-device session)
            (make-buffer-descriptor
             :label "block world screenshot readback"
             :size (* 4 (first extent) (second extent))
             :usage '(:copy-dst)))))
    (unwind-protect
         (progn
           (present-canvas-frame
            context
            (lambda (surface-texture encoder)
              (encode-luvcraft-frame
               session surface-texture encoder :readback-buffer buffer)))
           (ensure-directories-exist pathname)
           (let ((pixels (read-buffer buffer))
                 (format (canvas-format context)))
             (write-rgba-png
              pathname pixels (first extent) (second extent) format)
             (values pathname pixels (first extent) (second extent) format)))
      (destroy buffer))))

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
                (provider *gpu-provider*)
                (sky-clock (make-instance 'sky-clock
                                          :pinned-day-fraction 0.5))
                (sky-profile (make-default-sky-profile)))
  "Open a hidden SDL canvas, render one block-world frame, and save it.

The sky clock arrives pinned at noon so captures stay byte-deterministic;
pass an unpinned clock to photograph another time of day."
  (let ((session nil))
    (unwind-protect
         (progn
           (setf session
                 (start-luvcraft
                  :title title :width width :height height
                  :frames-per-second nil :visible-p nil
                  :provider provider
                  :world world :mesher mesher :camera camera
                  :sky-clock sky-clock
                  :sky-profile sky-profile))
           (capture-luvcraft-screenshot session pathname))
      (when session
        (stop-luvcraft session)))))

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
                  :provider provider
                  :world world :mesher mesher :camera camera
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
