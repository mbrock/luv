;;;; Additional extended surfaces, crossing both 64-cell resident boundaries.
(require :asdf)
(dolist (file (directory "*.asd")) (asdf:load-asd file))
(asdf:load-system :luvcraft/luft/game)
(with-open-file (stream "wiki/measurements/yawl-shared-relief-dd41727/quality.lisp")
  (loop for form = (read stream nil :eof)
        until (or (eq form :eof)
                  (and (consp form) (eq (first form) 'luv:call-with-sdl-main-thread)))
        do (eval form)))

(defun boundary-scene ()
  (let* ((palette luvcraft.luft:*luvcraft-palette*)
         (builder (luft.terrain:make-scene-builder
                   :horizontal-bits 7
                   :material-vocabulary (luvcraft.luft:luvcraft-palette-vocabulary palette))))
    (flet ((placement (name) (gethash name (luvcraft.luft::luvcraft-palette-by-name palette))))
      (loop for x from 48 below 80 do
        (loop for y from 48 below 80 do
          (luft.terrain:scene-builder-cell builder x y 3
                                          :material (placement (if (< x 64) :grass :stone)))))
      (loop for x from 48 below 80 do
        (loop for z from 4 below 11 do
          (luft.terrain:scene-builder-cell
           builder x 64 z :material
           (placement (nth (mod (floor (- x 48) 4) 4)
                           '(:bricks :red-wool :tapestry :mosaic)))))))
    (luft.terrain:finish-scene-builder builder :voxel-light-propagation-p nil)))

(luv:call-with-sdl-main-thread
 (lambda ()
   (dolist (variant '("control" "shared"))
     (handler-bind ((warning #'muffle-warning))
       (load (if (equal variant "control")
                 *relief-baseline* "luft/render/shaders.lisp")))
     (dolist (temporal-p '(nil t))
       (let ((directory (uiop:ensure-directory-pathname
                         (merge-pathnames (format nil "~A-~A/" variant (if temporal-p "temporal" "native"))
                                          *relief-output*))))
         (dolist (pose `(("floor-near" (63.8 56.0 4.7) ,(/ pi 2) -0.65 :perspective)
                         ("floor-grazing" (63.8 50.0 4.7) ,(/ pi 2) -0.08 :perspective)
                         ("wall-grazing" (50.0 63.0 7.0) 0.12 0.0 :perspective)
                         ("isometric" (63.8 40.0 25.0) ,(/ pi 2) -0.55 :isometric)))
           (destructuring-bind (name position yaw pitch projection) pose
             (quality-capture
              (boundary-scene) directory name
              (luft.render:make-fly-camera
               :position (apply #'luft.render::make-vec3 position)
               :yaw yaw :pitch pitch :field-of-view 0.85)
              temporal-p :motion-p t :projection projection))))))))
(sb-ext:exit :code 0 :abort t)
