;;;; XDG_RUNTIME_DIR=/tmp/luv-runtime ./env --slim sbcl --script scripts/capture-frontier-light.lisp
;;;; Seed-7 night, a supported lantern, then a nearby occluding cell edited
;;;; through the live viewer's owner/schedule/publication path.
(require :asdf)
(asdf:load-system "luvcraft/luft/game")

(defpackage #:capture-frontier-light
  (:use #:cl)
  (:local-nicknames (#:terrain #:luft.terrain) (#:render #:luft.render)
                    (#:game #:luvcraft.luft) (#:v #:luv.arithmetic.lisp.vec3)))
(in-package #:capture-frontier-light)

(setf render::*sky-minutes-per-day* nil render:*sky-hour* 0.0
      render::*projection* :perspective)

(let* ((source (game:make-luvcraft-world-source :seed 7))
       (domain (terrain:world-source-domain source))
       (planks (game::block-placement luvcraft::*planks-block*))
       (lantern (game::block-placement luvcraft::*lantern-block*))
       (edits (terrain::world-source-edits source)))
  (flet ((cell (x y z) (luft:make-site domain x y z luft:+cell-extent+ 1)))
    ;; A small test landing in the generated cave near spawn, not a finite
    ;; scene adapter: all surrounding terrain is demand-streamed seed 7.
    (loop for x from 1000 to 1004 do
      (loop for y from 1082 to 1088 do
        (setf (gethash (cell x y 7) edits) planks)
        (loop for z from 8 to 10 do (setf (gethash (cell x y z) edits) nil))))
    (setf (gethash (cell 1002 1086 8) edits) lantern)
    (let* ((scene (terrain:make-world-source-streaming-scene
                   source :mesh-preparer render:+render-population-preparer+))
           (camera (render:make-fly-camera
                    :position (v:make-vec3 1002.5 1082.3 9.8)
                    :yaw (/ pi 2) :pitch -0.35))
           (viewer (render:start-viewer :solid scene :camera camera
                                       :player-p nil :world-pathname nil
                                       :width 960 :height 640 :fixed-exposure 1.0
                                       :frames-per-second 15)))
      (labels ((on-owner (function)
                 (luv::call-on-sdl-canvas-thread (render::viewer-canvas viewer) function))
               (sample (state)
                 (on-owner
                  (lambda ()
                    (let ((field (terrain:realized-light-generation-field
                                  (terrain:streaming-scene-light-generation scene))))
                      (format t "~&FRONTIER-CAPTURE ~A attachments=~D light-front=~D~%"
                              state (length (terrain:scene-attachments scene))
                              (luft:voxel-light-at field 1002 1083 8))))))
               (settle ()
                 (loop repeat 120
                       when (on-owner
                             (lambda ()
                               (and (plusp (hash-table-count (terrain:streaming-scene-loaded scene)))
                                    (zerop (hash-table-count (terrain::streaming-scene-load-outstanding scene)))
                                    (zerop (terrain::streaming-scene-pending-mesh-count scene))
                                    (null (terrain::streaming-scene-replacement scene)))))
                         do (return)
                       do (sleep 0.5)
                       finally (error "Streaming did not settle."))))
        (unwind-protect
             (progn
               ;; Give the actual asynchronous source real-time frames.
               (sleep 25)
               (settle)
               (sample :before)
               (ensure-directories-exist #P".amp/in/artifacts/frontier-before.png")
               (render:capture-viewer-frame ".amp/in/artifacts/frontier-before.png" viewer)
               (let ((status
                       (on-owner
                        (lambda ()
                          (nth-value 1 (render::apply-viewer-world-edit
                                        viewer (cell 1002 1084 8) planks))))))
                 (unless (eq status :edited) (error "Live edit refused: ~S." status)))
               (settle)
               (sleep 1)
               (sample :after)
               (render:capture-viewer-frame ".amp/in/artifacts/frontier-after.png" viewer)
               (format t "~&FRONTIER-CAPTURE seed=7 hour=0 edit=(1002 1084 8) resident=~D tiles=~D~%"
                       (hash-table-count (terrain:streaming-scene-store scene))
                       (hash-table-count (terrain:streaming-scene-light-tiles scene))))
          (render:stop-viewer viewer))))))
