;;;; Read-only old/new material comparison at a saved player's eye pose.
;;;; ./env sbcl --script scripts/capture-luvcraft-save-materials.lisp SAVE OUTPUT-DIRECTORY
(require :asdf)
(asdf:load-asd (truename "luv.asd"))
(asdf:load-asd (truename "luvcraft.asd"))
(asdf:load-system "luvcraft")
(asdf:load-system "luvcraft/luft/game")

(defpackage #:luvcraft-save-materials
  (:use #:cl)
  (:local-nicknames (#:old #:luvcraft) (#:host #:luvcraft.luft.game)
                    (#:game #:luvcraft.luft) (#:render #:luft.render)
                    (#:terrain #:luft.terrain) (#:v #:luv.arithmetic.lisp.vec3)))
(in-package #:luvcraft-save-materials)

(defun on-canvas (viewer function)
  (luv::call-on-sdl-canvas-thread (render:viewer-canvas viewer) function))

(defun settle (viewer)
  (loop repeat 600
        when (on-canvas
              viewer
              (lambda ()
                (render::advance-viewer-streaming viewer)
                (let ((scene (render:viewer-source viewer)))
                  (and (plusp (hash-table-count (terrain:streaming-scene-loaded scene)))
                       (zerop (hash-table-count (terrain::streaming-scene-load-outstanding scene)))
                       (null (terrain::streaming-scene-replacement scene))))))
          return t
        do (sleep 0.1)
        finally (error "Saved-world comparison did not finish streaming.")))

(defun capture (save directory)
  (ensure-directories-exist directory)
  (multiple-value-bind (world resume) (old:read-luvcraft-save save)
    (let* ((source (game:luvcraft-world-luft-source world))
           (position (getf resume :player-position))
           (yaw (getf (getf resume :look) :yaw))
           (pitch (getf (getf resume :look) :pitch))
           (eye (v:make-vec3 (first position) (+ (second position) 1.62) (third position)))
           (fov old::+luvcraft-camera-vertical-field-of-view+))
      ;; No checkpoint writer, no inhabitants, and no game simulation.
      (let ((session (old:start-luvcraft
                      :world world :camera (make-instance 'old:fly-camera
                                                          :position eye :yaw yaw :pitch pitch
                                                          :field-of-view fov)
                      :width 1100 :height 700 :visible-p nil :high-pixel-density-p nil
                      :frames-per-second nil :residency-radius 2
                      :sky-clock (make-instance 'old:sky-clock :pinned-day-fraction 0.5))))
        (unwind-protect
             (old:capture-luvcraft-screenshot session (merge-pathnames "original.png" directory)
                                             :include-hud-p nil :include-viewmodel-p nil
                                             :metadata-function
                                             (lambda (session)
                                               (format t "~&OLD camera ~S~%"
                                                       (old::camera-pose-from-camera (old:luvcraft-session-camera session)))))
          (old:stop-luvcraft session)))
      (let* ((scene (game:make-luvcraft-streaming-scene
                     source :residency-radius 1 :mesh-preparer render:+render-population-preparer+))
             (offset (game::luvcraft-world-source-offset source))
             (viewer (render:start-game-host
                      :solid scene :title "Saved-world material comparison"
                      :world-pathname nil :player-p nil :width 1100 :height 700 :frames-per-second 1
                      :camera (render:make-fly-camera
                               :position (v:make-vec3 (+ offset (v:vec3-x eye))
                                                      (+ offset (v:vec3-z eye)) (v:vec3-y eye))
                               :yaw (- (/ pi 2) yaw) :pitch pitch :field-of-view fov)
                      :sky-factory 'host::make-luvcraft-sky-drawing
                      :terrain-factory 'host::make-luvcraft-terrain-drawing
                      :torch-factory 'host::make-luvcraft-torch-drawing
                      :finishing-factory 'host::make-luvcraft-image-finishing
                      :exposure-factory 'host::make-luvcraft-exposure
                      :frame-class 'host:luvcraft-viewer
                      :frame-initargs (list :luvcraft-world world :sky-origin-offset offset
                                            :hotbar (list (game:block-placement old::*stone-block*))))))
        (unwind-protect
             (progn
               (on-canvas viewer
                          (lambda ()
                            (setf (luv:canvas-clock (render:viewer-canvas viewer)) (luv:make-demand-clock)
                                  (render::viewer-running-p viewer) nil
                                  (render:viewer-mode viewer) (make-instance 'render:first-person-mode)
                                  (old::sky-clock-pinned-day-fraction (host::viewer-sky-clock viewer)) 0.5)))
               (settle viewer)
               (format t "~&NEW camera ~S~%" (render:camera-pose-from-camera (render:viewer-camera viewer)))
               (let ((overlays (render:viewer-overlays viewer)))
                 (unwind-protect
                      (progn
                        (on-canvas viewer (lambda () (setf (render:viewer-overlays viewer) nil)))
                        (render:capture-viewer-frame (merge-pathnames "luft.png" directory) viewer))
                   (on-canvas viewer (lambda () (setf (render:viewer-overlays viewer) overlays)))))
               (assert (null (luv:canvas-failures (render:viewer-canvas viewer)))))
          (render:stop-viewer viewer))))))

(let ((arguments (uiop:command-line-arguments)))
  (unless (= (length arguments) 2) (error "Expected SAVE and OUTPUT-DIRECTORY."))
  (setf luv::*canvas-watchdog-fatal-seconds* nil)
  (luv:call-with-sdl-main-thread
   (lambda () (capture (pathname (first arguments))
                       (uiop:ensure-directory-pathname (second arguments))))))
