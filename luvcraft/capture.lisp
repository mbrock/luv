;;; Offscreen luvcraft captures for smoke tests and CI-ish environments.

(in-package #:luv)

(defun wait-for-cube-world-products
    (demo &key minimum (timeout 10.0))
  "Wait outside frame encoding for a useful initial set of chunk meshes.

MINIMUM defaults to nine or the whole desired set when it is smaller.  This
keeps the procedural smoke path quick without making a one-chunk caller-owned
world wait for products which can never exist."
  (let* ((minimum (or minimum
                      (min 9 (hash-table-count
                              (cube-world-demo-desired-chunks demo)))))
         (deadline (+ (get-internal-real-time)
                      (round (* timeout internal-time-units-per-second)))))
    (loop
      (let ((products nil))
        (request-canvas-frame
       (cube-world-demo-canvas demo)
       (lambda (timestamp)
         (declare (ignore timestamp))
         (refresh-cube-world-mesh demo)
         (setf products
               (hash-table-count (cube-world-demo-chunk-products demo)))))
      (when (>= products minimum)
        (return demo))
      (when (>= (get-internal-real-time) deadline)
        (error "Only ~D chunk meshes arrived within ~,2F seconds; expected ~D.~@[ Last worker error: ~A~]"
               products
               timeout minimum
               (let ((result (first (cube-world-demo-production-errors demo))))
                 (and result (production-result-condition result)))))
      (sleep 0.005)))))

(defun capture-cube-world-screenshot (demo pathname)
  "Render DEMO once, read its real color attachment, and write a PNG."
  (unless (eq :open (canvas-state (cube-world-demo-canvas demo)))
    (error "Cannot capture a closed cube-world demo."))
  (wait-for-cube-world-products demo)
  (let* ((context (cube-world-demo-context demo))
         (extent (canvas-extent context))
         (buffer
           (create
            (cube-world-demo-device demo)
            (make-buffer-descriptor
             :label "block world screenshot readback"
             :size (* 4 (first extent) (second extent))
             :usage '(:copy-dst)))))
    (unwind-protect
         (progn
           (present-canvas-frame
            context
            (lambda (surface-texture encoder)
              (encode-cube-world-frame
               demo surface-texture encoder :readback-buffer buffer)))
           (ensure-directories-exist pathname)
           (write-rgba-png
            pathname (read-buffer buffer)
            (first extent) (second extent) (canvas-format context)))
      (destroy buffer))))

(defun hidden-cube-world-frame-pathname (directory index)
  (merge-pathnames
   (format nil "block-world-~3,'0D.png" index)
   (uiop:ensure-directory-pathname directory)))

(defun capture-hidden-cube-world-screenshot
    (pathname &key
                (title "luv hidden block world")
                (width 960) (height 640)
                (world (make-empty-little-block-world))
                (mesher (make-instance 'exposed-face-mesher))
                (camera (make-instance 'fly-camera)))
  "Open a hidden SDL/Vulkan canvas, render one block-world frame, and save it."
  (let ((demo nil))
    (unwind-protect
         (progn
           (setf demo
                 (start-cube-world-demo
                  :title title :width width :height height
                  :frames-per-second nil :visible-p nil
                  :world world :mesher mesher :camera camera))
           (capture-cube-world-screenshot demo pathname))
      (when demo
        (stop-cube-world-demo demo)))))

(defun capture-hidden-cube-world-frames
    (directory &key
                 (count 6)
                 (title "luv hidden block world")
                 (width 960) (height 640)
                 (yaw-step 0.35)
                 (world (make-empty-little-block-world))
                 (mesher (make-instance 'exposed-face-mesher))
                 (camera (make-instance 'fly-camera)))
  "Capture COUNT hidden block-world frames into DIRECTORY.

Each frame reuses one hidden SDL/Vulkan canvas and advances CAMERA's yaw by
YAW-STEP, returning the pathnames that were written."
  (check-type count (integer 1))
  (check-type yaw-step real)
  (let ((directory (uiop:ensure-directory-pathname directory))
        (demo nil)
        (initial-yaw (camera-yaw camera)))
    (ensure-directories-exist directory)
    (unwind-protect
         (progn
           (setf demo
                 (start-cube-world-demo
                  :title title :width width :height height
                  :frames-per-second nil :visible-p nil
                  :world world :mesher mesher :camera camera))
           (loop for index below count
                 for pathname =
                 (hidden-cube-world-frame-pathname directory index)
                 do (setf (camera-yaw camera)
                          (+ initial-yaw (* yaw-step index)))
                    (capture-cube-world-screenshot demo pathname)
                 collect pathname))
      (when demo
        (stop-cube-world-demo demo)))))
