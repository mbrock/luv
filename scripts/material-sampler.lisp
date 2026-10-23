;;;; Daylight evidence for Luvcraft's entire Luft material vocabulary.
;;;; ./env sbcl --dynamic-space-size 8192 --script scripts/material-sampler.lisp DIR
;;;; Optional second argument: directory of baseline materials.lisp,
;;;; shaders.lisp, and palette.lisp saved with git show before the experiment.
(require :asdf)
(dolist (file (directory "*.asd")) (asdf:load-asd file))
(asdf:load-system :luvcraft/luft)
(asdf:load-system :luft/render)

(when (second (uiop:command-line-arguments))
  (dolist (file '("materials.lisp" "shaders.lisp" "palette.lisp"))
    (load (merge-pathnames file (uiop:ensure-directory-pathname
                                (second (uiop:command-line-arguments)))))))

(setf luft.render:*sky-hour* 12.0
      luft.render::*sky-minutes-per-day* nil
      luft.render::*sky-elapsed* 0.0)

(defun sampler-world (relief)
  (let* ((source (luvcraft.luft:make-luvcraft-world-source
                  :seed 7 :relief relief :offset 0))
         (builder (luft.terrain:make-scene-builder
                   :material-vocabulary
                   (luvcraft.luft:luvcraft-palette-vocabulary
                    luvcraft.luft:*luvcraft-palette*))))
    (luft.terrain:generate-world-source-chunk
     source 0
     (lambda (x y z material)
       (when (and (< x 32) (< y 32))
         (luft.terrain:scene-builder-cell builder x y z :material material))))
    (luft.terrain:finish-scene-builder builder :voxel-light-propagation-p nil)))

(defun sampler-scene (placements)
  (let ((builder (luft.terrain:make-scene-builder
                  :material-vocabulary
                  (luvcraft.luft:luvcraft-palette-vocabulary
                   luvcraft.luft:*luvcraft-palette*))))
    (loop for placement in placements for index from 0
          for x = (+ 12 (* 6 (mod index 4)))
          for y = (+ 12 (* 6 (floor index 4)))
          do (dotimes (dx 2)
               (dotimes (dy 2)
                 (let ((kind (luft.terrain:material-placement-kind placement)))
                   (if (luft.terrain:material-kind-attachment-form kind)
                       (progn
                         (luft.terrain:scene-builder-cell
                          builder (+ x dx) (+ y dy) 1
                          :material (luvcraft.luft:block-placement luvcraft::*stone-block*))
                         (luft.terrain:scene-builder-attach
                          builder (+ x dx) (+ y dy) 1 :z :high kind))
                       (dotimes (z 2)
                         (luft.terrain:scene-builder-cell
                          builder (+ x dx) (+ y dy) (+ z 1) :material placement)))))))
    (luft.terrain:finish-scene-builder builder :voxel-light-propagation-p nil)))

(defun sampler-capture (scene pathname camera)
  (let ((viewer (luft.render:start-viewer
                  :solid scene :camera camera :player-p nil
                  :world-pathname nil :fixed-exposure 1.0
                  :width 1440 :height 900 :frames-per-second 1)))
    (unwind-protect
         (progn
           (setf (luft.render::viewer-running-p viewer) nil)
           (luv::call-with-capture-target
            (lambda (capture)
              (let ((times nil) (pixels nil))
                (dotimes (index 10)
                  (let ((start (get-internal-real-time)))
                    (setf pixels (luv::render-capture-frame capture))
                    (when (> index 1)
                      (push (* 1000.0 (/ (- (get-internal-real-time) start)
                                        internal-time-units-per-second)) times))))
                (destructuring-bind (width height) (luv::capture-extent capture)
                  (luv::write-rgba-png pathname pixels width height
                                       (luv::capture-format capture)))
                ;; Synchronous offscreen render + readback, not PNG encoding.
                ;; This is wall time, not GPU timestamps (absent on lavapipe).
                (format t "~&SAMPLER ~A frame-readback-ms ~S heap-MB ~,1F~%"
                        pathname (reverse times)
                        (/ (sb-kernel:dynamic-usage) 1048576.0))))
            (make-instance 'luv:application-capture :application viewer
                           :kind :screenshot :options '(:inspector-p nil))))
      (luft.render:stop-viewer viewer))))

(luv:call-with-sdl-main-thread
 (lambda ()
   (let* ((directory (uiop:ensure-directory-pathname
                      (first (uiop:command-line-arguments))))
          (placements (coerce (luv.domains:identity-vocabulary-members
                               (luvcraft.luft:luvcraft-palette-vocabulary
                                luvcraft.luft:*luvcraft-palette*)) 'list)))
     (ensure-directories-exist directory)
     (loop for start from 0 below (length placements) by 12
           for page from 1
           for group = (subseq placements start (min (+ start 12) (length placements)))
           do (with-open-file (out (merge-pathnames (format nil "page-~D.txt" page) directory)
                                   :direction :output :if-exists :supersede)
                (loop for placement in group for i from 0
                      do (format out "~D ~D ~A~%" (mod i 4) (floor i 4)
                                 (luft.terrain:material-placement-name placement))))
              (sampler-capture
               (sampler-scene group)
               (merge-pathnames (format nil "page-~D.png" page) directory)
               (luft.render:make-fly-camera
                :position (luft.render::make-vec3 12 -12 23)
                :yaw 1.28 :pitch -0.6 :field-of-view 0.75)))
     (dolist (relief '(:alpine :meadow))
       (sampler-capture
        (sampler-world relief)
        (merge-pathnames (format nil "seed-7-~(~A~).png" relief) directory)
        (luft.render:make-fly-camera
         :position (luft.render::make-vec3 -15 -24 45)
         :yaw 0.9 :pitch -0.55 :field-of-view 0.85))))))
