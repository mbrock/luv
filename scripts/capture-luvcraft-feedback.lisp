;;;; Matching native-resolution software-Vulkan action feedback in both games.
;;;; XDG_RUNTIME_DIR=/tmp/luv-runtime ./env --slim sbcl --script scripts/capture-luvcraft-feedback.lisp
(require :asdf)
(asdf:load-system "luvcraft/luft/game")
(asdf:load-system "luvcraft")
(setf luv::*canvas-watchdog-fatal-seconds* nil)
(defpackage #:feedback-proof
  (:use #:cl)
  (:local-nicknames (#:h #:luvcraft.luft.game) (#:g #:luvcraft.luft)
                    (#:r #:luft.render) (#:v #:luv.arithmetic.lisp.vec3)
                    (#:ter #:luft.terrain) (#:old #:luvcraft)))
(in-package #:feedback-proof)

(defvar *fixture-proof-p* nil)

(defun world ()
  (let* ((w (old:make-empty-little-block-world :seed 7 :chunk-height 16 :relief :meadow))
         (edits (old:little-world-source-edits (old:block-world-source w))))
    (loop for x from 0 below 16 do
      (loop for z from 0 below 16 do
        (loop for y from 0 below 16 do
          (old::record-block-edit edits (when (= y 3) old::*stone-block*) x y z))))
    (old::record-block-edit edits (if *fixture-proof-p* (old:block-kind-named :barrel) old::*bricks-block*) 8 5 9)
    w))

(defun click (owner canvas button)
  (luv:handle-canvas-event
   owner canvas (make-instance 'luv:canvas-pointer-button-press-event
                               :timestamp 0 :x 400 :y 300 :button button :clicks 1)))

(defun key (owner canvas name)
  (luv:handle-canvas-event owner canvas
                          (make-instance 'luv:canvas-key-press-event :timestamp 0 :key-name name)))

(defun settle (viewer)
  (let ((scene (r:viewer-source viewer)))
    (loop repeat 600 do
      (luv::call-on-sdl-canvas-thread (r:viewer-canvas viewer)
                                    (lambda () (r::advance-viewer-streaming viewer)))
      (when (and (plusp (hash-table-count (ter:streaming-scene-loaded scene)))
                 (null (ter::streaming-scene-replacement scene))
                 (zerop (hash-table-count (ter::streaming-scene-load-outstanding scene))))
        (return-from settle t))
      (sleep 0.1))
    (error "Streaming did not settle: ~S" (ter::streaming-scene-production-errors scene))))

(defun new-proof ()
  (let* ((w (world)) (source (g:luvcraft-world-luft-source w))
         (scene (g:make-luvcraft-streaming-scene source :residency-radius 0
                                               :mesh-preparer r:+render-population-preparer+))
         (camera (r:make-fly-camera :position (v:make-vec3 1032.5 1030.5 5.5)
                                    :yaw (/ pi 2) :pitch 0.0))
         (viewer (r:start-game-host :solid scene :camera camera :player-p nil
                                    :width 800 :height 600 :frames-per-second 1
                                    :fixed-exposure 0.7 :figure-factory 'h::make-ball-figure-drawing
                                    :frame-class 'h:luvcraft-viewer
                                    :frame-initargs (list :luvcraft-world w :hotbar (list (g:block-placement old::*red-wool-block*))))))
    (unwind-protect
         (let ((canvas (r:viewer-canvas viewer)))
           (setf (luv:canvas-clock canvas) (luv:make-demand-clock))
           (setf (r:viewer-mode viewer) (make-instance 'r:first-person-mode)
                 (r:camera-field-of-view camera) (* 70d0 (/ pi 180))
                 (h::viewer-creative-p viewer) t
                 (old:player-selected-block viewer) old::*red-wool-block*)
           (settle viewer)
           (format t "~&NEW loaded ~D~%" (hash-table-count (ter:streaming-scene-loaded scene)))
           ;; SDL's offscreen driver has no native relative-pointer mode.
           ;; Establish capture, then dispatch real portable action events.
           (setf (r:viewer-pointer-captured-p viewer) t)
           (r:capture-viewer-frame (if *fixture-proof-p* ".amp/in/artifacts/new-fixture.png"
                                       ".amp/in/artifacts/new-target.png") viewer)
           (format t "~&NEW target ~S boundary ~S~%" (r::viewer-inspection viewer)
                   (let ((i (r::viewer-inspection viewer)))
                     (and i (h::target-face-boundary scene (r:site-inspection-site i) (r:site-inspection-cell i)))))
           (when *fixture-proof-p* (return-from new-proof t))
           (assert (zerop (old::block-particle-count (h::viewer-particles viewer))))
           (luv::call-on-sdl-canvas-thread canvas (lambda () (key viewer canvas :e)))
           (assert (eq :edited (r:viewer-last-edit-status viewer)))
           (settle viewer)
           (r:capture-viewer-frame ".amp/in/artifacts/new-place.png" viewer)
           (assert (eq (g:block-placement old::*red-wool-block*)
                       (ter:scene-material-placement-at
                        scene (g:luvcraft-cell-site source 8 5 8))))
           (format t "~&NEW place ~S particles ~D~%" (r:viewer-last-edit-status viewer)
                   (old::block-particle-count (h::viewer-particles viewer)))
           (luv::call-on-sdl-canvas-thread canvas (lambda () (click viewer canvas :left)))
           (assert (eq :edited (r:viewer-last-edit-status viewer)))
           (settle viewer)
           (old::advance-block-particles (h::viewer-particles viewer) 0.12)
           (assert (= 18 (old::block-particle-count (h::viewer-particles viewer))))
           (r:capture-viewer-frame ".amp/in/artifacts/new-break.png" viewer)
           (format t "~&NEW break ~S particles ~D~%" (r:viewer-last-edit-status viewer)
                   (old::block-particle-count (h::viewer-particles viewer)))
           (old::advance-block-particles (h::viewer-particles viewer) 1.0)
           (assert (zerop (old::block-particle-count (h::viewer-particles viewer))))
           (setf (r:viewer-pointer-captured-p viewer) nil)
           (r:capture-viewer-frame ".amp/in/artifacts/new-released.png" viewer)
           (setf (r:viewer-pointer-captured-p viewer) t
                 (r:camera-yaw camera) 0.0)
           (r:capture-viewer-frame ".amp/in/artifacts/new-air.png" viewer)
           (assert (null (r::viewer-inspection viewer)))
           ;; The extended figure stage must still draw the existing spheres.
           (luv::call-on-sdl-canvas-thread
            canvas (lambda () (key viewer canvas :b) (click viewer canvas :left)))
           (assert (= 1 (old::physics-world-body-count (h::viewer-physics viewer))))
           (r:capture-viewer-frame ".amp/in/artifacts/new-ball.png" viewer)
           (dolist (overlay (r:viewer-overlays viewer))
             (when (typep overlay 'h::target-marker)
               (assert (zerop (hash-table-count (h::marker-buffers overlay)))))))
      (r:stop-viewer viewer))))

(defun old-proof ()
  (let* ((camera (make-instance 'old::fly-camera :position (v:make-vec3 8.5d0 5.5d0 6.5d0)
                                :yaw 0d0 :pitch 0d0))
         (session (old:start-luvcraft :world (world) :camera camera :width 800 :height 600
                                      :visible-p nil :frames-per-second nil :residency-radius 0
                                      :selected-block old::*red-wool-block*)))
    (unwind-protect
         (let ((canvas (old::luvcraft-session-canvas session)))
           (old::wait-for-luvcraft-products session)
           (setf (old:player-selected-block session) old::*red-wool-block*)
           (luv::call-on-sdl-canvas-thread canvas
             (lambda ()
               (setf (old:camera-position camera) (v:make-vec3 8.5d0 5.5d0 6.5d0))
               (setf (old:camera-field-of-view camera) (* 70d0 (/ pi 180)))
               (setf (old::luvcraft-session-pointer-captured-p session) t)))
           (old:capture-luvcraft-screenshot session (if *fixture-proof-p* ".amp/in/artifacts/old-fixture.png"
                                                         ".amp/in/artifacts/old-target.png"))
           (format t "~&OLD target ~S~%" (old::luvcraft-session-target session))
           (when *fixture-proof-p* (return-from old-proof t))
           (luv::call-on-sdl-canvas-thread canvas (lambda () (click session canvas :right)))
           (old:capture-luvcraft-screenshot session ".amp/in/artifacts/old-place.png")
           (assert (eq old::*red-wool-block*
                       (old:world-block-at (old::luvcraft-session-world session) 8 5 8)))
           (format t "~&OLD place ~S particles ~D~%" (old:world-block-at (old::luvcraft-session-world session) 8 5 8)
                   (old::block-particle-count (old::luvcraft-session-particle-system session)))
           (luv::call-on-sdl-canvas-thread canvas (lambda () (click session canvas :left)))
           (old::advance-block-particles (old::luvcraft-session-particle-system session) 0.12)
           (assert (= 18 (old::block-particle-count (old::luvcraft-session-particle-system session))))
           (old:capture-luvcraft-screenshot session ".amp/in/artifacts/old-break.png")
           (format t "~&OLD break particles ~D~%" (old::block-particle-count (old::luvcraft-session-particle-system session))))
      (old:stop-luvcraft session))))

(ensure-directories-exist #P".amp/in/artifacts/feedback.png")
(new-proof)
(old-proof)
(let ((*fixture-proof-p* t)) (new-proof) (old-proof))
