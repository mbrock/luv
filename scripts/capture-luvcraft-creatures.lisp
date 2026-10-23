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

(defun world ()
  (let* ((world (old:make-empty-little-block-world :seed 7 :chunk-height 16 :relief :meadow))
         (edits (old:little-world-source-edits (old:block-world-source world))))
    (loop for x below 16 do
      (loop for z below 16 do
        (loop for y below 16 do
          (old::record-block-edit edits (and (= y 3) old::*grass-block*) x y z))))
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
                  :frames-per-second 1 :fixed-exposure 0.7
                  :sky-factory 'h::make-luvcraft-sky-drawing
                  :terrain-factory 'h::make-luvcraft-terrain-drawing
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
           (r:capture-viewer-frame (format nil ".amp/in/artifacts/creatures-new~A.png" suffix) viewer)
           ;; A control removes only the caster from the sun pass, not its surface.
           (let ((drawing (r::renderer-player (r:viewer-renderer viewer))))
             (defmethod r::encode-scene-drawing-shadow :after
                 ((drawing h::creature-figure-drawing) pass (binding h::creature-figure-binding))
               (declare (ignore drawing pass binding)))
             (r:capture-viewer-frame
              (format nil ".amp/in/artifacts/creatures-new~A-no-shadow.png" suffix) viewer)
             (defmethod r::encode-scene-drawing-shadow :after
                 ((drawing h::creature-figure-drawing) pass (binding h::creature-figure-binding))
               (h::encode-creatures drawing pass binding t))
             (assert (typep drawing 'h::creature-figure-drawing)))
           (format t "~&NEW native ~S, creature surface plus sun-map caster/control captured.~%"
                   (r::renderer-render-extent (r:viewer-renderer viewer))))
      (r:stop-viewer viewer))))

(defun old-proof ()
  (let* ((clock (make-instance 'old::sky-clock :day-fraction (/ 10d0 24) :paused-p t))
         (camera (make-instance 'old::fly-camera :position (v:make-vec3 8d0 5.5d0 2d0)
                                :yaw 0d0 :pitch -0.35d0 :field-of-view (* 60d0 (/ pi 180))))
         (population (make-instance 'old::critter-population))
         (session (old:start-luvcraft :world (world) :camera camera :width 1000 :height 700
                                      :visible-p nil :frames-per-second nil :residency-radius 0
                                      :critters population :sky-clock clock)))
    (unwind-protect
         (progn
           (old::wait-for-luvcraft-products session)
           (setf (fill-pointer (old::critter-population-critters population)) 0)
           (old::add-critter population (animal))
           (old:capture-luvcraft-screenshot session ".amp/in/artifacts/creatures-old.png")
           (format t "~&OLD native 1000x700, original creature captured.~%"))
      (old:stop-luvcraft session))))

(ensure-directories-exist #P".amp/in/artifacts/creatures-old.png")
(old-proof)
(new-proof)
(let ((r::*temporal-upscaling-p* t)) (new-proof "-temporal"))
