;;;; Disposable real-Metal appearance comparison; never load into live Sly.
(require :asdf)
(dolist (file (directory "*.asd")) (asdf:load-asd file))
(asdf:load-system :luvcraft/luft/game)
(defparameter *relief-baseline* (or (uiop:getenv "RELIEF_BASELINE") "build/relief-baseline.lisp"))
(defparameter *relief-output* (uiop:ensure-directory-pathname
                              (or (uiop:getenv "RELIEF_OUTPUT") "build/shared-relief-quality/")))
;; Reuse the material sampler's fixtures, without running its capture driver.
(with-open-file (stream "scripts/material-sampler.lisp")
  (loop for form = (read stream nil :eof)
        until (or (eq form :eof)
                  (and (consp form) (eq (first form) 'luv:call-with-sdl-main-thread)))
        do (eval form)))

(defmethod luv:show-canvas :around ((canvas luv::sdl-canvas)) canvas)

(defun quality-capture (scene directory name camera temporal-p &key motion-p (projection :perspective))
  ;; The SDL owner thread must see the same choices as construction.
  (setf luft.render::*temporal-upscaling-p* temporal-p
        luft.render::*scene-sample-count* 1
        luft.render::*dynamic-resolution-frame-rate* nil
        luft.render::*projection* projection)
  (let* ((host (luft.render:start-game-host
                :solid scene :camera camera :player-p nil :world-pathname nil
                :frame-class 'luft.render:viewer
                :fixed-exposure 0.45 :width 960 :height 600 :frames-per-second 1
                :sky-factory 'luvcraft.luft.game::make-luvcraft-sky-drawing
                :terrain-factory 'luvcraft.luft.game::make-luvcraft-terrain-drawing
                :torch-factory 'luvcraft.luft.game::make-luvcraft-torch-drawing
                :finishing-factory 'luvcraft.luft.game::make-luvcraft-image-finishing))
         (renderer (luft.render:viewer-renderer host)))
    (unwind-protect
         (progn
           (setf (luv:canvas-clock (luft.render:viewer-canvas host)) (luv:make-demand-clock)
                 (luft.render::viewer-running-p host) nil
                 (luft.render:viewer-mode host) (make-instance 'luft.render:first-person-mode))
           (luv::call-with-capture-target
            (lambda (capture)
              (destructuring-bind (width height) (luv::capture-extent capture)
                (format t "~&QUALITY ~A ~A ~Dx~D temporal=~S scale=~S~%"
                        directory name width height
                        (luft.render::renderer-temporal-resolve-kind renderer)
                        (if temporal-p 0.75 1.0))
                (dotimes (frame (if motion-p 48 16))
                  (when (>= frame 16)
                    (luv::call-on-sdl-canvas-thread
                     (luft.render:viewer-canvas host)
                     (lambda ()
                       (incf (luft.render::vec3-x (luft.render:camera-position camera)) 0.025))))
                  (let ((pixels (luv::render-capture-frame capture)))
                    (when (or (= frame 0) (= frame 15) (>= frame 16))
                      (luv::write-rgba-png
                       (merge-pathnames (format nil "~A-~2,'0D.png" name frame) directory)
                       pixels width height (luv::capture-format capture)))))))
            (make-instance 'luv:application-capture :application host :kind :screenshot
                           :options (list :inspector-p nil :render-scale (if temporal-p 0.75 1.0)))))
      (luft.render:stop-viewer host))))

(luv:call-with-sdl-main-thread
 (lambda ()
   (let ((placements (coerce (luv.domains:identity-vocabulary-members
                              (luvcraft.luft:luvcraft-palette-vocabulary
                               luvcraft.luft:*luvcraft-palette*)) 'list)))
     (dolist (variant '("control" "shared"))
       (load (if (equal variant "control")
                 *relief-baseline*
                 "luft/render/shaders.lisp"))
       (dolist (temporal-p '(nil t))
         (let ((directory (uiop:ensure-directory-pathname
                           (merge-pathnames (format nil "~A-~A/" variant (if temporal-p "temporal" "native"))
                                            *relief-output*))))
           (ensure-directories-exist directory)
           (loop for start from 0 below (length placements) by 12
                 for page from 1
                 for group = (subseq placements start (min (+ start 12) (length placements)))
                 do (with-open-file (stream (merge-pathnames (format nil "page-~D.txt" page) directory)
                                            :direction :output :if-exists :supersede)
                      (loop for placement in group for i from 0
                            do (format stream "~D ~D ~A~%" (mod i 4) (floor i 4)
                                       (luft.terrain:material-placement-name placement))))
                    (quality-capture
                     (sampler-scene group) directory (format nil "page-~D" page)
                     (luft.render:make-fly-camera
                      :position (luft.render::make-vec3 12 -12 23)
                      :yaw 1.28 :pitch -0.6 :field-of-view 0.75) temporal-p))
           (dolist (names '((:grass :stone) (:bricks :stone-bricks) (:red-wool :tapestry)
                            (:wood :hay-bale) (:iron-plate :crystal) (:mosaic :half-timber)))
             (dolist (grazing-p '(nil t))
               (quality-capture
                (sampler-scene
                 (mapcar (lambda (name)
                           (or (find name placements :key #'luft.terrain:material-placement-name)
                               (error "Missing material ~S" name))) names))
                directory (format nil "~(~A~)-~A" (first names) (if grazing-p "grazing" "near"))
                (luft.render:make-fly-camera
                 :position (if grazing-p (luft.render::make-vec3 5 11.2 2.5)
                               (luft.render::make-vec3 15 5 4.4))
                 :yaw (if grazing-p 0.03 (/ pi 2)) :pitch (if grazing-p 0.0 -0.15)
                 :field-of-view 1.0)
                temporal-p :motion-p t)))))))))
(sb-ext:exit :code 0 :abort t)
