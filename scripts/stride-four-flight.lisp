;;;; Replay the alpine flight in a separate game host. From the checkout:
;;;; ./env sbcl --script scripts/stride-four-flight.lisp
(require :asdf)
(pushnew (truename "./") asdf:*central-registry* :test #'equal)
(asdf:load-system "luvcraft/luft/game")
(load (merge-pathnames "stride-four-ridge-fixture.lisp" *load-truename*))

(defpackage #:stride-four-flight
  (:use #:cl)
  (:local-nicknames (#:r #:luft.render) (#:ter #:luft.terrain) (#:game #:luvcraft.luft.game)
                    (#:v #:luv.arithmetic.lisp.vec3)))
(in-package #:stride-four-flight)

(defvar *out* (uiop:ensure-directory-pathname (or (uiop:getenv "RING_FLIGHT_DIR") "build/stride-four-flight/")))
(defconstant +width+ 960)
(defconstant +height+ 540)
(defun milliseconds () (* (get-internal-real-time) (/ 1000d0 internal-time-units-per-second)))

(defun pose (camera frame)
  (let* ((angle (* 2 pi (/ (mod frame 120) 120)))
         (x (+ 1312 (* 16 (sin angle)))) (y (+ 1056 (* 12 (cos angle)))) (z 111d0)
         (dx (- 814 x)) (dy (- 1057 y)))
    (setf (r:camera-position camera) (v:make-vec3 x y z)
          (r:camera-yaw camera) (atan dy dx)
          (r:camera-pitch camera) (atan (- 81 z) (sqrt (+ (* dx dx) (* dy dy)))))))

(defun cross-ridge-pose (camera step)
  ;; Leave the stride-four ring, enter stride two, then fly over the exact ridge.
  (let* ((x (- 1312d0 (* 552d0 (/ step 240))))
         (y (+ 1056d0 (* 12d0 (cos (* pi (/ step 240))))))
         (dx (min -50d0 (- 814 x))) (dy (- 1057 y)))
    (setf (r:camera-position camera) (v:make-vec3 x y 111d0)
          (r:camera-yaw camera) (atan dy dx)
          (r:camera-pitch camera) (atan -30d0 (sqrt (+ (* dx dx) (* dy dy)))))))

(defun draw (host target)
  (let* ((device (r:viewer-device host))
         (queue (luv:device-queue device))
         (encoder (luv:create device (luv:make-command-encoder-descriptor)))
         (commands nil))
    (unwind-protect
         (progn
           (r::encode-viewer-frame host encoder target (list +width+ +height+))
           (setf commands (luv:finish encoder))
           (prog1 (luv:submit queue commands)
             (luv:submitted-work-done queue)))
      (when commands (luv:destroy commands))
      (luv:destroy encoder))))

(defun save-frame (host target buffer name)
  (let* ((device (r:viewer-device host))
         (encoder (luv:create device (luv:make-command-encoder-descriptor)))
         (commands nil))
    (unwind-protect
         (progn
           (luv:encode encoder (luv:make-gpu-copy-texture-to-buffer-command :source target :destination buffer))
           (setf commands (luv:finish encoder))
           (luv:submit (luv:device-queue device) commands)
           (luv:write-rgba-png (merge-pathnames name *out*) (luv:read-buffer buffer)
                               +width+ +height+ :bgra8-unorm-srgb))
      (when commands (luv:destroy commands))
      (luv:destroy encoder))))

(defun gpu-ms (reader submission)
  (when reader
    (loop repeat 1000 for seconds = (funcall reader submission)
          when seconds return (* 1000d0 seconds)
          do (sleep 0.001)
          finally (error "Missing GPU feedback for ~D" submission))))

(defun run-mode (host target buffer csv mode)
  (let* ((state (r::renderer-voxel-lod (r:viewer-renderer host)))
         (key (r::distant-tile-key 1 6 8))
         (camera (r::viewer-camera host))
         (reader (luv:queue-gpu-time-reader (luv:device-queue (r:viewer-device host))))
         (marker nil) (settled 0) (crossing nil))
    (setf r:*stride-four-mode* :heightfield)
    (pose camera 0)
    ;; Return through ordinary streaming after the preceding crossing. Warm the
    ;; inner rings before collecting the next outer-ring arrival distribution.
    (dotimes (i 240)
      (r::advance-viewer-streaming host)
      (when (zerop (mod i 4))
        #+darwin (luv.objective-c:with-autorelease-pool () (draw host target))
        #-darwin (draw host target)))
    (setf r:*stride-four-mode* mode)
    (loop for i below 15000 do
      (if crossing (cross-ridge-pose camera crossing) (pose camera i))
      (let* ((ring (r::voxel-lod-stride-four state))
             (previous (if ring (r::stride-four-ring-publications ring) 0))
             (start (milliseconds)))
        (r::advance-viewer-streaming host)
        (let* ((stream (- (milliseconds) start))
               (submission #+darwin (luv.objective-c:with-autorelease-pool () (draw host target))
                           #-darwin (draw host target)))
          (let* ((wall (- (milliseconds) start))
                 (gpu (gpu-ms reader submission))
                 (ring (r::voxel-lod-stride-four state))
                 (slot (and ring (gethash key (r::stride-four-ring-slots ring))))
                 (kind (and slot (r::stride-four-slot-provenance slot)))
                 (pending (and ring (r::stride-four-ring-pending ring)))
                 (published (and ring (> (r::stride-four-ring-publications ring) previous))))
            (format csv "~(~A~),~D,~A,~,4F,~,4F,~(~A~),~A~%" (if crossing (format nil "~A-crossing" mode) mode)
                    (or crossing i) (if published 1 0) stream wall kind
                    (if gpu (format nil "~,4F" gpu) ""))
            (when (and (eq mode :auto) (eq kind :direct) pending
                       (eq slot (r::request-ring-slot pending)) (eq :exact (r::request-ring-kind pending)))
              (unless marker (setf marker i)))
            (when (and marker (<= (- i marker) 40))
              (save-frame host target buffer (format nil "handoff-~3,'0D.png" (- i marker))))
            (when (and marker (not crossing) (eq kind :direct))
              (save-frame host target buffer "auto-before.png"))
            (when (and marker (eq :exact kind) (< (- i marker) 20))
              (save-frame host target buffer (format nil "auto-after-~2,'0D.png" (- i marker))))
            (when (and (not crossing) (zerop (mod i 120)) (or kind (eq mode :heightfield)))
              (save-frame host target buffer (format nil "~(~A~)-pose.png" mode)))
            (when (zerop (mod i 300))
              (format t "~&FLIGHT ~S ~D ~S~%" mode i (r::stride-four-ring-stats state))
              (finish-output) (finish-output csv))
            (when (and crossing (zerop (mod crossing 8)))
              (save-frame host target buffer (format nil "~(~A~)-crossing-~3,'0D.png" mode crossing)))
            (when (eql crossing 240) (return))
            (when crossing (incf crossing))
            (if (and (null crossing) (> i 120) (or (eq mode :heightfield) (and kind (null pending))))
                (incf settled) (setf settled 0))
            (when (>= settled 121)
              (when (eq mode :auto) (assert marker))
              (format t "~&DONE ~S ~D ~S~%" mode i (r::stride-four-ring-stats state))
              (setf crossing 0)))))
      finally (error "Ring failed to settle: ~S" mode))))

(defun flight (host)
  (let* ((device (r:viewer-device host)) (owner (make-instance 'r::gpu-resource-owner))
         (target (r::own-gpu-resource owner device
                                      (luv:make-texture-descriptor :dimensions :2d
                                       :size (list +width+ +height+ 1) :format :bgra8-unorm-srgb
                                       :usage '(:render-attachment :texture-binding :copy-src :copy-dst))))
         (buffer (r::own-gpu-resource owner device
                                      (luv:make-buffer-descriptor :size (* +width+ +height+ 4)
                                                                  :usage '(:copy-dst)))))
    (unwind-protect
         (with-open-file (csv (merge-pathnames "frames.csv" *out*) :direction :output :if-exists :supersede)
           (format csv "mode,frame,published,stream_ms,completion_ms,ridge,gpu_ms~%")
           (dolist (mode '(:heightfield :direct :exact :auto)) (run-mode host target buffer csv mode)))
      (r::release-viewer-surface-view host target)
      (luv:destroy owner))))

;; Isolated process, fixed exposure, time of day, and internal resolution.
(setf r::*fixed-render-scale* 0.75 r::*dynamic-resolution-frame-rate* nil
      r::*sky-minutes-per-day* nil r:*sky-hour* 10.0
      luv::*canvas-dispatch-timeout* 900.0
      luv::*canvas-watchdog-fatal-seconds* nil
      luv::*canvas-watchdog-sample-seconds* most-positive-single-float
      luv::*canvas-watchdog-warn-seconds* most-positive-single-float
      r:*voxel-lod-p* t r:*stride-four-mode* :heightfield)
(ensure-directories-exist *out*)
(defun main ()
  (let* ((source (cl-user::ridge-source))
         (camera (r:make-fly-camera :field-of-view 0.42d0)))
    (cl-user::carve-ridge source)
    (pose camera 0)
    (let* ((scene (ter:make-world-source-streaming-scene
                   source :mesh-preparer r:+render-population-preparer+))
           (world (game::make-luvcraft-world
                   :scene scene :block-world (luvcraft.luft::luvcraft-private-world
                                              source (luvcraft.luft::luvcraft-little-world-source source))))
           (player (game::make-luvcraft-player :id "ridge-flight" :name "Ridge flight"))
           (host (game::start-luvcraft-view world player
                                            (list :camera camera :player-p nil :fixed-exposure 0.65
                                                  :width +width+ :height +height+ :frames-per-second 1)
                                            :terrain-bevels-p t)))
      (unwind-protect
           (progn
             (setf (luv:canvas-clock (r:viewer-canvas host)) (luv:make-demand-clock)
                  (r::viewer-running-p host) nil
                  (r:viewer-mode host) (make-instance 'r::free-camera-mode)
                  (luvcraft::sky-clock-pinned-day-fraction (game::world-sky-clock world)) (/ 10.0 24.0))
             (luv::call-on-sdl-canvas-thread (r:viewer-canvas host) (lambda () (flight host))))
        (r:stop-viewer host)))))

(luv:call-with-sdl-main-thread #'main)
