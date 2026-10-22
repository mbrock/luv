;;;; Compare Luvcraft's and Luft's terrain rendering on one world.
;;;;
;;;; Load into the live image with Luvcraft playing.  EXPORT-LUVCRAFT-TERRAIN
;;;; copies every resident Luvcraft block into a Luft scene, cell for cell
;;;; (Luvcraft's Y is up, Luft's Z), so both renderers draw the same solid.
;;;; MEASURE-PASS-COST then times the GPU through each renderer's benchmark
;;;; knobs: the shadow and scene passes are repeated K times per frame, the
;;;; slope between two K values is one repetition's cost at a saturated GPU
;;;; clock, and the same slope with terrain hidden is subtracted to leave the
;;;; terrain's own share.

(defpackage #:terrain-comparison
  (:use #:cl)
  (:export #:export-luvcraft-terrain #:luvcraft-view #:show-in-luft
           #:measure-luvcraft #:measure-luft #:set-window-size))

(in-package #:terrain-comparison)

(defparameter *window-size* '(1280 800)
  "Logical window size both games are measured at.")

(defun luvcraft-session () luvcraft::*session*)

(defun block-material (kind)
  "The Luft material standing in for Luvcraft block KIND."
  (case (slot-value kind 'luvcraft::name)
    ((:grass :dirt :moss :leaves) luft.render::*terrain-material-placement*)
    ((:stone :slate) luft.render::*highland-rock-material-placement*)
    (:sand luft.render::*sand-material-placement*)
    (:snow luft.render::*snow-material-placement*)
    (:crystal luft.render::*crystal-material-placement*)
    (t luft.render::*sanctuary-material-placement*)))

(defun chunk-origin (chunk)
  (let ((coordinate (slot-value (slot-value chunk 'luvcraft.world::domain)
                                'luvcraft.world::coordinate)))
    (values (* 16 (luvcraft.world::chunk-coordinate-x coordinate))
            (* 16 (luvcraft.world::chunk-coordinate-z coordinate)))))

(defun export-luvcraft-terrain (&key (session (luvcraft-session)))
  "Return a Luft streaming scene of SESSION's resident blocks, the offset that
maps Luvcraft X/Z onto it, and the block count."
  (let* ((chunks (luvcraft::resident-world-chunks
                  (luvcraft::luvcraft-session-world session)))
         (min-x most-positive-fixnum) (min-z most-positive-fixnum)
         (count 0))
    (dolist (chunk chunks)
      (multiple-value-bind (x z) (chunk-origin chunk)
        (setf min-x (min min-x x) min-z (min min-z z))))
    (let ((builder (luft.render::make-scene-builder :horizontal-bits 8)))
      (dolist (chunk chunks)
        (multiple-value-bind (cx cz) (chunk-origin chunk)
          (dotimes (x 16)
            (dotimes (z 16)
              (dotimes (y 64)
                (let ((kind (luvcraft::chunk-block-at chunk x y z)))
                  (when kind
                    (incf count)
                    (luft.render::scene-builder-cell
                     builder (- (+ cx x) min-x) (- (+ cz z) min-z) y
                     :material (block-material kind)))))))))
      (values (luft.render:make-streaming-scene
               (luft.render::finish-scene-builder builder)
               :residency-radius 3)
              (list min-x min-z)
              count))))

(defun luvcraft-view (offset &key (session (luvcraft-session)))
  "Return Luvcraft's camera as Luft position, yaw, pitch, and field of view."
  (let* ((camera (luvcraft::luvcraft-session-camera session))
         (position (luvcraft::camera-position camera)))
    (list (luv.arithmetic.lisp.vec3:make-vec3
           (coerce (- (luv.arithmetic.lisp.vec3:vec3-x position) (first offset))
                   'single-float)
           (coerce (- (luv.arithmetic.lisp.vec3:vec3-z position) (second offset))
                   'single-float)
           (coerce (luv.arithmetic.lisp.vec3:vec3-y position) 'single-float))
          ;; Luvcraft looks along (sin yaw, -, cos yaw) in X/Z; Luft along
          ;; (cos yaw, sin yaw) in X/Y.
          (coerce (- (/ pi 2) (luvcraft::camera-yaw camera)) 'single-float)
          (coerce (luvcraft::camera-pitch camera) 'single-float)
          (coerce (luvcraft::camera-field-of-view camera) 'single-float))))

(defun set-window-size (canvas &optional (size *window-size*))
  (luv::call-on-sdl-canvas-thread
   canvas
   (lambda ()
     (let ((window (luv::sdl-canvas-window canvas)))
       (sdl3:set-window-size window (first size) (second size))
       (sdl3:raise-window window)))))

(defun show-in-luft (scene view &key (viewer luft.render:*viewer*))
  "Install SCENE in VIEWER with its free camera at VIEW."
  (luft.render::refresh-viewer-renderer viewer :solid scene)
  (destructuring-bind (position yaw pitch field-of-view) view
    (let ((camera (luft.render::viewer-camera viewer)))
      (setf (luft.render::camera-position camera) position
            (luft.render::camera-yaw camera) yaw
            (luft.render::camera-pitch camera) pitch
            (luft.render::camera-field-of-view camera) field-of-view)))
  viewer)

(defun gpu-seconds-per-frame (canvas queue seconds)
  "Average GPU busy seconds per presented frame over SECONDS, and frames/s."
  (let* ((progress (luv:queue-progress-reader queue))
         (timer (luv:queue-gpu-time-reader queue))
         (first-submission (1+ (funcall progress)))
         (first-frame (luv::sdl-canvas-frame-count canvas)))
    (sleep seconds)
    (let ((last-submission (funcall progress))
          (frames (- (luv::sdl-canvas-frame-count canvas) first-frame)))
      ;; Feedback trails completion slightly.
      (sleep 0.2)
      (let ((busy (loop for value from first-submission to last-submission
                        for time = (funcall timer value)
                        when time sum time)))
        (values (/ busy (max frames 1)) (/ frames seconds))))))

(defun measure (canvas queue set-repetitions set-terrain
                &key (low 4) (high 12) (settle 1.5) (seconds 3.0))
  "Return one pass repetition's GPU ms, its terrain share in each pass, and fps.

Each slope is the GPU time one more shadow-and-scene repetition adds, between
LOW and HIGH repetitions; terrain shares subtract the slope without terrain."
  (flet ((slope (terrain)
           (funcall set-terrain terrain)
           (let ((points
                   (loop for repetitions in (list low high)
                         collect (progn
                                   (funcall set-repetitions repetitions)
                                   (sleep settle)
                                   (multiple-value-list
                                    (gpu-seconds-per-frame canvas queue seconds))))))
             (values (* 1000 (/ (- (first (second points)) (first (first points)))
                                (- high low)))
                     points))))
    (unwind-protect
         (multiple-value-bind (all points) (slope t)
           (let ((none (slope nil))
                 (shadow (slope :shadow))
                 (scene (slope :scene)))
             (list :pass-ms all
                   :pass-without-terrain-ms none
                   :terrain-ms (- all none)
                   :shadow-terrain-ms (- shadow none)
                   :scene-terrain-ms (- scene none)
                   :frame-ms-at-low (* 1000 (first (first points)))
                   :fps (mapcar #'second points))))
      (funcall set-repetitions 1)
      (funcall set-terrain t))))

(defun measure-luvcraft (&rest arguments &key (session (luvcraft-session))
                         &allow-other-keys)
  (remf arguments :session)
  (apply #'measure
         (luvcraft::luvcraft-session-canvas session)
         (luv:device-queue (luvcraft::luvcraft-session-device session))
         (lambda (n) (setf luvcraft::*luvcraft-benchmark-pass-repetitions* n))
         (lambda (p) (setf luvcraft::*luvcraft-benchmark-terrain-p* p))
         arguments))

(defun measure-luft (&rest arguments &key (viewer luft.render:*viewer*)
                     &allow-other-keys)
  (remf arguments :viewer)
  (apply #'measure
         (luft.render::viewer-canvas viewer)
         (luv:device-queue
          (luft.render::renderer-device (luft.render::viewer-renderer viewer)))
         (lambda (n) (setf luft.render::*benchmark-pass-repetitions* n))
         (lambda (p) (setf luft.render::*benchmark-terrain-p* p))
         arguments))
