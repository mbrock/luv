;;;; Matched native seed-7 creatures, with no temporal/dynamic reconstruction.
;;;; SDL_VIDEODRIVER=offscreen ./env --slim sbcl --script scripts/capture-luvcraft-creatures.lisp
(require :asdf)
(asdf:load-system "luvcraft/luft/game")
(asdf:load-system "luvcraft")
(setf luv::*canvas-watchdog-fatal-seconds* nil
      luvcraft::*luvcraft-temporal-p* nil
      luft.render::*temporal-upscaling-p* nil
      luft.render::*dynamic-resolution-frame-rate* nil
      luft.render::*fixed-render-scale* 1.0)

(defpackage #:creature-proof
  (:use #:cl)
  (:local-nicknames (#:h #:luvcraft.luft.game) (#:g #:luvcraft.luft)
                    (#:r #:luft.render) (#:v #:luv.arithmetic.lisp.vec3)
                    (#:ter #:luft.terrain) (#:old #:luvcraft)))
(in-package #:creature-proof)

(defvar *scenario* :sun)

(defun world ()
  (let* ((world (old:make-empty-little-block-world :seed 7 :chunk-height 16 :relief :meadow))
         (edits (old:little-world-source-edits (old:block-world-source world))))
    (loop for x below 16 do
      (loop for z below 16 do
        (loop for y below 16 do
          (old::record-block-edit
           edits (cond ((= y 3) old::*grass-block*)
                       ((and (not (eq *scenario* :sun))
                             (or (= y 7) (and (<= 4 y 7) (or (= x 0) (= x 15) (= z 0) (= z 15)))))
                        old::*stone-block*)
                       ((and (eq *scenario* :lantern) (= x 6) (= y 4) (= z 6))
                        old::*lantern-block*)) x y z))))
    world))

(defun animal ()
  (make-instance 'old::turtle :position (v:make-vec3 8d0 4d0 6d0)
                             :yaw 0.65d0 :heading 0.65d0 :seed 7))

(defun settle (viewer)
  (let ((scene (r:viewer-source viewer)))
    (loop repeat 600 do
      (luv::call-on-sdl-canvas-thread (r:viewer-canvas viewer)
                                    (lambda () (r::advance-viewer-streaming viewer)))
      (when (and (plusp (hash-table-count (ter:streaming-scene-loaded scene)))
                 (null (ter::streaming-scene-replacement scene))
                 (zerop (hash-table-count (ter::streaming-scene-load-outstanding scene))))
        (return-from settle))
      (sleep 0.1))
    (error "Streaming did not settle")))

(defun new-proof (&optional (suffix ""))
  (let* ((world (world))
         (scene (g:make-luvcraft-streaming-scene
                 (g:luvcraft-world-luft-source world) :residency-radius 0
                 :mesh-preparer r:+render-population-preparer+))
         (camera (r:make-fly-camera :position (v:make-vec3 1032d0 1026d0 5.5d0)
                                    :yaw (/ pi 2) :pitch -0.35d0))
         (viewer (r:start-game-host
                  :solid scene :camera camera :player-p nil :width 1000 :height 700
                  :frames-per-second 1
                  :sky-factory 'h::make-luvcraft-sky-drawing
                  :terrain-factory 'h::make-luvcraft-terrain-drawing
                  :torch-factory 'h::make-luvcraft-torch-drawing
                  :finishing-factory 'h::make-luvcraft-image-finishing
                  :exposure-factory 'h::make-luvcraft-exposure
                  :figure-factory 'h::make-creature-figure-drawing
                  :frame-class 'h:luvcraft-viewer
                  :frame-initargs (list :luvcraft-world world
                                       :hotbar (list (g:block-placement old::*grass-block*))))))
    (unwind-protect
         (progn
           (setf (luv:canvas-clock (r:viewer-canvas viewer)) (luv:make-demand-clock)
                 (r:viewer-mode viewer) (make-instance 'r:first-person-mode)
                 (r:camera-field-of-view camera) (* 60d0 (/ pi 180))
                 (h::viewer-animals-initialized-p viewer) t
                 (h::viewer-animals viewer) (list (animal))
                 (old::sky-clock-hour (h::viewer-sky-clock viewer)) 10d0
                 (old::sky-clock-paused-p (h::viewer-sky-clock viewer)) t)
           (settle viewer)
           (assert (= 10 (length (r:viewer-figures viewer))))
           (assert (= 0.45 (r:exposure-value (r::renderer-exposure-control (r:viewer-renderer viewer)))))
           (r:capture-viewer-frame
            (format nil ".amp/in/artifacts/creatures-new-~(~A~)~A.png" *scenario* suffix) viewer)
           (format t "~&BODY LIGHT ~A ~S~%" *scenario*
                   (r:renderer-light-at (r:viewer-renderer viewer) 1032 1030 4))
           (format t "~&NEW native ~S, creature surface plus local light captured.~%"
                   (r::renderer-render-extent (r:viewer-renderer viewer))))
      (r:stop-viewer viewer))))

(defun old-proof ()
  (let* ((world (world))
         ;; Empty little worlds have no resident chunks. Raise exactly the
         ;; authored chunk before starting, so MINIMUM cannot silently be zero.
         (chunk (g::load-luvcraft-chunk (old:block-world-source world) world 0 0))
         (lit (old::relight-block-world world))
         (clock (make-instance 'old::sky-clock :day-fraction (/ 10d0 24) :paused-p t))
         (camera (make-instance 'old::fly-camera :position (v:make-vec3 8d0 5.5d0 2d0)
                                :yaw 0d0 :pitch -0.35d0 :field-of-view (* 60d0 (/ pi 180))))
         (population (make-instance 'old::critter-population))
         (session (old:start-luvcraft :world world :camera camera :width 1000 :height 700
                                      :visible-p nil :frames-per-second nil :residency-radius 0
                                      :critters population :sky-clock clock)))
    (declare (ignore chunk lit))
    (unwind-protect
         (progn
           (old::wait-for-luvcraft-products session :minimum 1 :timeout 60)
           (setf (fill-pointer (old::critter-population-critters population)) 0)
           (old::add-critter population (animal))
           (old:capture-luvcraft-screenshot
            session (format nil ".amp/in/artifacts/creatures-old-~(~A~).png" *scenario*))
           (format t "~&OLD BODY LIGHT ~A ~S~%" *scenario*
                   (multiple-value-list (old::critter-light-levels (animal) (old:luvcraft-session-world session))))
           (multiple-value-bind (sky block state) (old:world-light-at world 8 4 6)
             (format t "~&OLD RAW FIELD ~A sky=~D block=~D state=~S~%" *scenario* sky block state)
             (assert (member state '(:stable :provisional)))
             (assert (= sky (if (eq *scenario* :sun) 15 0)))
             (assert (if (eq *scenario* :lantern) (plusp block) (zerop block))))
           (format t "~&OLD native 1000x700, original creature captured.~%"))
      (old:stop-luvcraft session))))

(ensure-directories-exist #P".amp/in/artifacts/creatures-old.png")
(dolist (*scenario* '(:sun :cave :lantern))
  (old-proof)
  (new-proof))
