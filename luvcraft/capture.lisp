;;; Offscreen luvcraft captures for smoke tests and CI-ish environments.

(in-package #:luv)

(defun wait-for-luvcraft-products
    (session &key minimum (timeout 10.0))
  "Wait outside frame encoding for a useful initial set of chunk meshes.

MINIMUM defaults to nine or the whole desired set when it is smaller.  This
keeps the procedural smoke path quick without making a one-chunk caller-owned
world wait for products which can never exist."
  (let* ((minimum (or minimum
                      (min 9 (hash-table-count
                              (luvcraft-session-desired-chunks session)))))
         (deadline (+ (get-internal-real-time)
                      (round (* timeout internal-time-units-per-second)))))
    (loop
      (let ((products nil))
        (request-canvas-frame
         (luvcraft-session-canvas session)
         (lambda (timestamp)
           (declare (ignore timestamp))
           (refresh-luvcraft-mesh session)
           (setf products
                 (hash-table-count (luvcraft-session-chunk-products session)))))
        (when (>= products minimum)
          (return session))
        (when (>= (get-internal-real-time) deadline)
          (error "Only ~D chunk meshes arrived within ~,2F seconds; expected ~D.~@[ Last worker error: ~A~]"
                 products
                 timeout minimum
                 (let ((result (first (luvcraft-session-production-errors session))))
                   (and result (production-result-condition result)))))
        (sleep 0.005)))))

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
           (write-rgba-png
            pathname (read-buffer buffer)
            (first extent) (second extent) (canvas-format context)))
      (destroy buffer))))

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
                (sky-clock (make-instance 'sky-clock
                                          :pinned-day-fraction 0.5))
                (sky-profile (make-default-sky-profile)))
  "Open a hidden SDL/Vulkan canvas, render one block-world frame, and save it.

The sky clock arrives pinned at noon so captures stay byte-deterministic;
pass an unpinned clock to photograph another time of day."
  (let ((session nil))
    (unwind-protect
         (progn
           (setf session
                 (start-luvcraft
                  :title title :width width :height height
                  :frames-per-second nil :visible-p nil
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
                 (pathname-prefix "block-world")
                 (world (make-empty-little-block-world))
                 (mesher (make-instance 'exposed-face-mesher))
                 (camera (make-instance 'fly-camera))
                 (sky-clock (make-instance 'sky-clock
                                           :pinned-day-fraction 0.5))
                 (sky-profile (make-default-sky-profile)))
  "Capture COUNT hidden block-world frames into DIRECTORY.

Each frame reuses one hidden SDL/Vulkan canvas, advances CAMERA's yaw by
YAW-STEP, moves FORWARD-STEP world units along its initial heading, and moves
the evaluated sky by DAY-STEP day fractions.  This is a consecutive-view
capture, not a set of independently restarted scenes."
  (check-type count (integer 1))
  (check-type yaw-step real)
  (check-type forward-step real)
  (when day-start
    (check-type day-start real))
  (check-type day-step real)
  (let ((directory (uiop:ensure-directory-pathname directory))
        (session nil)
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
                  :world world :mesher mesher :camera camera
                  :sky-clock sky-clock :sky-profile sky-profile))
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
                    (capture-luvcraft-screenshot session pathname)
                 collect pathname))
      (when session
        (stop-luvcraft session)))))
