;;;; Native seed-7 tonal comparisons. Run from the repository root:
;;;; SDL_VIDEODRIVER=offscreen ./env --slim sbcl --script scripts/capture-luvcraft-finish.lisp DIR
;;;; OLD is the original renderer; BEFORE/AFTER isolate grade and extinction.
(require :asdf)
(asdf:load-system "luvcraft/luft/game")
(asdf:load-system "luvcraft")
(setf luv::*canvas-watchdog-fatal-seconds* nil
      luft.render::*temporal-upscaling-p* nil
      luft.render::*dynamic-resolution-frame-rate* nil
      luft.render::*fixed-render-scale* 1.0)

(defpackage #:finish-proof
  (:use #:cl)
  (:local-nicknames (#:r #:luft.render) (#:h #:luvcraft.luft.game)
                    (#:g #:luvcraft.luft) (#:ter #:luft.terrain)
                    (#:v #:luv.arithmetic.lisp.vec3) (#:old #:luvcraft)))
(in-package #:finish-proof)

(defvar *eye* '(-15 -24 45))
(defvar *yaw* 0.9)
(defvar *pitch* -0.55)

(defun material-worlds ()
  (let ((builder (ter:make-scene-builder
                  :material-vocabulary (g:luvcraft-palette-vocabulary g:*luvcraft-palette*)))
        (world (old::make-block-world
                :source (make-instance 'old::gazetteer-open-sky-source))))
    (loop for name in '(:black-wool :obsidian :iron-plate :copper :stone :white-wool)
          for index from 0 for x = (* index 4) do
            (dotimes (dx 3)
              (dotimes (dy 3)
                (dotimes (dz 3)
                  (let ((block (old:block-kind-named name)))
                    (ter:scene-builder-cell builder (+ x dx) (+ 6 dy) (+ 1 dz)
                                            :material (g:block-placement block))
                    (old::ensure-world-chunk world (floor (+ x dx) 16) 0 0)
                    (setf (old:world-block-at world (+ x dx) (+ 1 dz) (+ 6 dy)) block))))))
    (old::relight-block-world world)
    (values (ter:finish-scene-builder builder :voxel-light-propagation-p nil) world)))

(defun worlds (relief)
  (let* ((source (g:make-luvcraft-world-source :seed 7 :relief relief :offset 0))
         (builder (ter:make-scene-builder
                   :material-vocabulary (g:luvcraft-palette-vocabulary g:*luvcraft-palette*)))
         (world (old::make-block-world
                 :source (make-instance 'old::gazetteer-open-sky-source))))
    (ter:generate-world-source-chunk
     source 0
     (lambda (x y z material)
       (when (and (< x 32) (< y 32))
         (ter:scene-builder-cell builder x y z :material material)
         (old::ensure-world-chunk world (floor x 16) (floor z 16) (floor y 16))
         (setf (old:world-block-at world x z y)
               (old:block-kind-named (ter:material-placement-name material))))))
    (old::relight-block-world world)
    (values (ter:finish-scene-builder builder :voxel-light-propagation-p nil) world)))

(defun before-terrain (device formats samples)
  (let ((luft.render.shaders::*terrain-cloud-visibility-function*
          'luft.render.shaders::luvcraft-cloud-visibility))
    (r:make-terrain-drawing device formats samples)))

(defun new-capture (scene world hour pathname after-p exposure)
  (let ((viewer
          (r:start-game-host
           :solid scene :player-p nil :width 960 :height 640 :frames-per-second 1
           :fixed-exposure exposure :world-pathname nil
           :camera (r:make-fly-camera :position (apply #'v:make-vec3 *eye*)
                                      :yaw *yaw* :pitch *pitch* :field-of-view 0.85)
           :sky-factory 'h::make-luvcraft-sky-drawing
           :terrain-factory (if after-p 'h::make-luvcraft-terrain-drawing 'before-terrain)
           :torch-factory (if after-p 'h::make-luvcraft-torch-drawing 'r::make-framed-torch-drawing)
           :finishing-factory (if after-p 'h::make-luvcraft-image-finishing 'r::make-image-finishing)
           :frame-class 'h:luvcraft-viewer
           :frame-initargs (list :luvcraft-world world :hotbar (list (g:block-placement old::*stone-block*))))))
    (unwind-protect
         (progn
           (setf (luv:canvas-clock (r:viewer-canvas viewer)) (luv:make-demand-clock)
                 (r::viewer-running-p viewer) nil
                 (r:viewer-mode viewer) (make-instance 'r:first-person-mode)
                 (old::sky-clock-pinned-day-fraction (h::viewer-sky-clock viewer)) (/ hour 24.0))
           (r:capture-viewer-frame pathname viewer :inspector-p nil)
           (assert (null (luv:canvas-failures (r:viewer-canvas viewer))))
           (format t "~&FINISH ~A exposure=~F temporal=~S scale=1~%" pathname exposure r::*temporal-upscaling-p*))
      (r:stop-viewer viewer))))

(defun capture-comparison (directory relief scene world hour)
  (flet ((path (role)
           (merge-pathnames (format nil "~(~A~)-~2,'0D-~A.png" relief (round (* hour 10)) role)
                            directory)))
    (old:capture-hidden-luvcraft-screenshot
     (path "old") :width 960 :height 640 :world world
     :camera (make-instance 'old:fly-camera
                            :position (v:make-vec3 (first *eye*) (third *eye*) (second *eye*))
                            :yaw (- (/ pi 2) *yaw*) :pitch *pitch* :field-of-view 0.85)
     :sky-clock (make-instance 'old:sky-clock :pinned-day-fraction (/ hour 24.0)))
    (new-capture scene world hour (path "before") nil 0.45)
    (new-capture scene world hour (path "after") t 0.45)
    (when (= hour 12.0)
      (new-capture scene world hour (path "exposure-1") nil 1.0))))

(defun main ()
  (luv:call-with-sdl-main-thread
   (lambda ()
     (let ((directory (uiop:ensure-directory-pathname (first (uiop:command-line-arguments)))))
       (ensure-directories-exist directory)
       (dolist (relief '(:alpine :meadow))
         (multiple-value-bind (scene world) (worlds relief)
           (dolist (hour '(0.0 6.7 12.0 17.3))
             (capture-comparison directory relief scene world hour))))
       (let ((*eye* '(12 -18 10)) (*yaw* (/ pi 2)) (*pitch* -0.25))
         (multiple-value-bind (scene world) (material-worlds)
           (dolist (hour '(12.0 17.3))
             (capture-comparison directory :materials scene world hour))))))))

(unless (member "library" (uiop:command-line-arguments) :test #'equal)
  (main))
