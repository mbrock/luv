;;;; Replay the alpine flight in a separate game host. From the checkout:
;;;; ./env sbcl --script scripts/stride-four-flight.lisp
;;;; RING_FLIGHT_AUTO_ONLY=1 selects the cold automatic flight.
;;;; RING_FLIGHT_NO_IMAGES=1 suppresses capture and its allocations.
;;;; RING_FLIGHT_ALLOC_PROFILE=1 samples all threads during the automatic crossing.
(require :asdf)
(require :sb-sprof)
(pushnew (truename "./") asdf:*central-registry* :test #'equal)
(asdf:load-system "luvcraft/luft/game")
(load (merge-pathnames "stride-four-ridge-fixture.lisp" *load-truename*))

(defpackage #:stride-four-flight
  (:use #:cl)
  (:local-nicknames (#:r #:luft.render) (#:ter #:luft.terrain) (#:game #:luvcraft.luft.game)
                    (#:v #:luv.arithmetic.lisp.vec3)))
(in-package #:stride-four-flight)

(defvar *out* (uiop:ensure-directory-pathname (or (uiop:getenv "RING_FLIGHT_DIR") "build/stride-four-flight/")))
(defvar *capture-p* (null (uiop:getenv "RING_FLIGHT_NO_IMAGES")))
(defconstant +width+ 960)
(defconstant +height+ 540)
(defun milliseconds () (* (get-internal-real-time) (/ 1000d0 internal-time-units-per-second)))

;; Instrument only the canvas thread during timed frames. The voxel and distant
;; totals include their named children. Worker time never enters these columns.
(defparameter *stages*
  '((near r::drain-streaming-scene-production) (near-slots r::%make-renderer-mesh-slot)
    (sort-stars r::sort-sites-by-star) (bounds r::site-bounds) (arena-write r::gpu-buffer-arena-write)
    (near-prepare r::prepare-renderer-mesh-candidates) (near-validate r::validate-renderer-scene-generation-cohort)
    (edits r::apply-queued-world-events)
    (retarget r::retarget-viewer-streaming) (voxel r::update-renderer-voxel-lod)
    (stride4 r::update-stride-four-ring) (volume ter:build-world-source-coarse-tile)
    (mesh ter:mesh-coarse-voxel-tile) (upload r::%upload-prepared-coarse-population)
    (distant r::update-renderer-distant-terrain) (quads r::build-distant-tile-quads)))
(defvar *stage-times* nil)

(defun install-stage-timers ()
  (loop for stage in *stages* for function = (second stage) for i from 0 do
    (let ((index i) (original (symbol-function function)))
      (setf (symbol-function function)
        (lambda (&rest args)
          (if *stage-times*
              (let ((start (get-internal-real-time)))
                (multiple-value-prog1 (apply original args)
                  (incf (aref *stage-times* index) (- (get-internal-real-time) start))))
              (apply original args)))))))

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
  (unless *capture-p* (return-from save-frame))
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

(defun ring-coverage (renderer)
  "Demanded tiles lacking their target detail, and tiles lacking any drawable representation."
  (let* ((distant (r::renderer-distant-terrain renderer))
         (boxes (r::distant-terrain-ring-bounds distant))
         (tiles (r::distant-terrain-tiles distant))
         (bounds (r::voxel-lod-active-bounds (r::renderer-voxel-lod renderer)))
         (mesh-slots (r::renderer-mesh-slots renderer))
         (waiting 0) (uncovered 0))
    (dotimes (level r::+distant-level-count+)
      (let ((box (aref boxes level)) (inner (when (plusp level) (aref boxes (1- level)))))
        (loop for tx from (aref box 0) to (aref box 1) do
          (loop for ty from (aref box 2) to (aref box 3)
                unless (if inner (r::distant-tile-covered-by-inner-ring-p level tx ty inner)
                           (r::distant-level0-covered-by-near-p tx ty mesh-slots))
                  do (let ((voxel (case level
                                    (0 (r::voxel-lod-tile-present-p renderer tx ty))
                                    (1 (r::stride-four-tile-present-p renderer tx ty)))))
                       (when (and (zerop level) (not voxel) bounds
                                  (<= (aref bounds 0) tx (aref bounds 1))
                                  (<= (aref bounds 2) ty (aref bounds 3)))
                         (incf waiting))
                       (unless (or voxel (gethash (r::distant-tile-key level tx ty) tiles))
                         (incf uncovered)))))))
    (values waiting uncovered)))

(defun settle-crossing (host target buffer mode)
  ;; Hold the final pose. A worker can preserve frame times by trailing the
  ;; camera indefinitely, so verify that all demanded geometry catches up.
  (let* ((renderer (r:viewer-renderer host)) (state (r::renderer-voxel-lod renderer))
         (start (milliseconds)) (coverage-time nil) (detail-time nil))
    (loop for frame below 4000 do
      (r::advance-viewer-streaming host)
      #+darwin (luv.objective-c:with-autorelease-pool () (draw host target))
      #-darwin (draw host target)
      (multiple-value-bind (waiting uncovered) (ring-coverage renderer)
        (when (zerop uncovered)
          (unless coverage-time (setf coverage-time (- (milliseconds) start))))
        (when (and (zerop waiting) (zerop uncovered) (null detail-time))
          (setf detail-time (- (milliseconds) start)))
        (let ((ring (r::voxel-lod-stride-four state))
              (position (r:camera-position (r::viewer-camera host))))
          (when (and detail-time (zerop waiting) (zerop uncovered)
                     (or (eq mode :heightfield)
                         (and (null (r::stride-four-ring-pending ring))
                              (null (r::next-stride-four-job ring
                                     (r::voxel-lod-source state) (v:vec3-x position) (v:vec3-y position))))))
            (format t "~&SETTLED ~S frame ~D, first full coverage ~,1F ms, stride-two coverage ~,1F ms, final ~S~%"
                    mode frame coverage-time detail-time (r::stride-four-ring-stats state))
            (format t "~&HEIGHTFIELD ~S~%" (r::distant-terrain-stats renderer))
            (save-frame host target buffer (format nil "~(~A~)-settled.png" mode))
            (return))))
      finally (error "Terrain failed to catch up after the crossing in ~S" mode))))

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
      (when (and (eql crossing 0) (eq mode :auto) (uiop:getenv "RING_FLIGHT_ALLOC_PROFILE"))
        (sb-sprof:start-profiling :mode :alloc :threads :all :max-samples 30000))
      (if crossing (cross-ridge-pose camera crossing) (pose camera i))
      (let* ((ring (r::voxel-lod-stride-four state))
             (previous (if ring (r::stride-four-ring-publications ring) 0))
             (*stage-times* (make-array (length *stages*) :initial-element 0))
             (gc-start sb-ext:*gc-real-time*) (start (milliseconds)))
        (r::advance-viewer-streaming host)
        (let* ((stream (- (milliseconds) start))
               (submission #+darwin (luv.objective-c:with-autorelease-pool () (draw host target))
                           #-darwin (draw host target)))
          (let* ((wall (- (milliseconds) start))
                 (gc-ms (* (- sb-ext:*gc-real-time* gc-start) (/ 1000d0 internal-time-units-per-second)))
                 (gpu (gpu-ms reader submission))
                 (ring (r::voxel-lod-stride-four state))
                 (slot (and ring (gethash key (r::stride-four-ring-slots ring))))
                 (kind (and slot (r::stride-four-slot-provenance slot)))
                 (pending (and ring (r::stride-four-ring-pending ring)))
                 (published (and ring (> (r::stride-four-ring-publications ring) previous))))
            (format csv "~(~A~),~D,~A,~,4F,~,4F,~(~A~),~A" (if crossing (format nil "~A-crossing" mode) mode)
                    (or crossing i) (if published 1 0) stream wall kind
                    (if gpu (format nil "~,4F" gpu) ""))
            (loop for ticks across *stage-times* do
              (format csv ",~,4F" (* ticks (/ 1000d0 internal-time-units-per-second))))
            (format csv ",~D,~D" (hash-table-count (r::voxel-lod-gpu-tiles state))
                    (hash-table-count (r::distant-terrain-tiles (r::renderer-distant-terrain (r:viewer-renderer host)))))
            (multiple-value-bind (waiting uncovered) (ring-coverage (r:viewer-renderer host))
              (format csv ",~D,~D,~,4F,~D~%" waiting uncovered gc-ms (sb-ext:get-bytes-consed)))
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
            (when (eql crossing 240)
              (when (and (eq mode :auto) (uiop:getenv "RING_FLIGHT_ALLOC_PROFILE"))
                (sb-sprof:stop-profiling)
                (with-open-file (out (merge-pathnames "allocation.txt" *out*)
                                     :direction :output :if-exists :supersede)
                  (sb-sprof:report :type :flat :max 40 :stream out)
                  (sb-sprof:report :type :graph :max 40 :stream out)))
              (settle-crossing host target buffer mode)
              (return))
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
           (format csv "mode,frame,published,stream_ms,completion_ms,ridge,gpu_ms")
           (dolist (stage *stages*) (format csv ",~(~A~)_ms" (first stage)))
           (format csv ",voxel_residents,distant_residents,stride2_missing,uncovered,gc_wall_ms,process_bytes_consed~%")
           (install-stage-timers)
           (dolist (mode (if (uiop:getenv "RING_FLIGHT_AUTO_ONLY") '(:auto) '(:heightfield :direct :exact :auto)))
             (run-mode host target buffer csv mode)))
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
