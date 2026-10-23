(require :asdf)
(assert (uiop:getenv "INTERLEAVED_LIBRARY"))
(load "wiki/measurements/yawl-runtime-f87a44f/interleaved.lisp")
(load "wiki/measurements/yawl-bevel-retirement-2a231b9/probe.lisp")
(in-package #:gpu-fps)

(defun select-retirement-state (drawing mode)
  (setf r::*retirement-limit*
        (unless (equal mode "control") (float (parse-integer mode :start 1))))
  (destructuring-bind (vertices normals indices ranges)
      (getf (gethash drawing r::*retirement-drawings*)
            (if r::*retirement-limit* :candidate :control))
    (setf (r::terrain-scene-vertices drawing) vertices
          (r::terrain-normals drawing) normals
          (r::terrain-scene-indices drawing) indices
          (r::terrain-star-ranges drawing) ranges)))

(defun retirement-capture (host pathname width height)
  "Read back the exact benchmark extent, independent of Retina window density."
  (let ((pixels
          (luv::call-on-sdl-canvas-thread
           (r:viewer-canvas host)
           (lambda ()
             (let* ((device (r::renderer-device (r:viewer-renderer host)))
                    (owner (make-instance 'r::gpu-resource-owner)))
               (unwind-protect
                    (progn
                      (let* ((target (r::own-gpu-resource
                                      owner device
                                      (luv:make-texture-descriptor
                                       :dimensions :2d :size (list width height 1)
                                       :format :bgra8-unorm-srgb
                                       :usage '(:render-attachment :copy-src :texture-binding))))
                             (readback (r::own-gpu-resource
                                        owner device
                                        (luv:make-buffer-descriptor
                                         :size (* width height 4) :usage '(:copy-dst))))
                             (encoder nil) (commands nil))
                        (unwind-protect
                             (progn
                               (setf encoder (luv:create device (luv:make-command-encoder-descriptor)))
                               (r::encode-viewer-frame host encoder target (list width height))
                               (luv:encode encoder (luv:make-gpu-copy-texture-to-buffer-command
                                                    :source target :destination readback))
                               (setf commands (luv:finish encoder))
                               (luv:submit (luv:device-queue device) commands)
                               (luv:read-buffer readback))
                          (when commands (luv:destroy commands))
                          (when encoder (luv:destroy encoder)))))
                 (luv:destroy owner)))))))
    (luv::write-rgba-png pathname pixels width height :bgra8-unorm-srgb)))

(let ((original (symbol-function 'block-program)))
  (setf (symbol-function 'block-program)
        (lambda (renderer mode)
          (let ((drawing (r::renderer-terrain renderer)))
            (unless (gethash drawing r::*retirement-drawings*)
              (let ((control (list (r::terrain-scene-vertices drawing) (r::terrain-normals drawing)
                                   (r::terrain-scene-indices drawing) (r::terrain-star-ranges drawing))))
                (multiple-value-bind (vertices normals indices sv si ranges) (r::retirement-tables)
                  (declare (ignore sv si))
                  (flet ((upload (words usage)
                           (let ((buffer (r::own-gpu-resource
                                          drawing (r::renderer-device renderer)
                                          (luv:make-buffer-descriptor :size (* 4 (length words))
                                                                      :usage (list usage :copy-dst)))))
                             (luv:write-buffer buffer words) buffer)))
                    (setf (gethash drawing r::*retirement-drawings*)
                          (list :control control :candidate
                                (list (upload vertices :storage) (upload normals :storage)
                                      (upload indices :index) ranges)))))))
            (if (equal mode "control") (funcall original renderer mode)
                (let ((limit (float (parse-integer mode :start 1)))
                      (vertex (symbol-function 'luft.render.shaders:terrain-vertex-specification)))
                  (unwind-protect
                       (progn
                         (setf (symbol-function 'luft.render.shaders:terrain-vertex-specification)
                               (lambda () (luft.render.shaders::retirement-vertex limit)))
                         (funcall original renderer "control-copy"))
                    (setf (symbol-function 'luft.render.shaders:terrain-vertex-specification) vertex))))))))

(let ((original (symbol-function 'advance-fixture)))
  (setf (symbol-function 'advance-fixture)
        (lambda (kind host old-p)
          (prog1 (funcall original kind host old-p)
            (when *interleaved-programs*
              (select-retirement-state *interleaved-drawing* *interleaved-mode*))))))

(setf *interleaved-inspection*
      (lambda (host programs)
        (let* ((renderer (r:viewer-renderer host))
               (drawing (r::renderer-terrain renderer))
               (directory (uiop:getenv "RETIREMENT_CAPTURE_DIR")))
          (luv::call-on-sdl-canvas-thread
           (r:viewer-canvas host)
           (lambda ()
             (dolist (entry programs)
               (unless (equal (car entry) "control")
                 (let ((limit (float (parse-integer (car entry) :start 1)))
                       (near-sites 0) (total-sites 0) (triangles 0) (original-triangles 0)
                       (bytes 0) (draws 0)
                       (ranges (getf (gethash drawing r::*retirement-drawings*) :candidate)))
                   (setf ranges (fourth ranges))
                   (maphash
                    (lambda (key slot)
                      (declare (ignore key))
                      (let* ((resident (r::mesh-slot-resident slot))
                             (state (gethash resident r::*retirement-residents*)))
                        (when (let ((r::*frame-uniform-data* r::*retirement-last-data*))
                                (r::resident-drawn-p resident nil))
                          (r::retirement-partition state (r::resident-population-star-runs resident)
                                                   r::*retirement-last-data* limit)
                          (incf bytes (* 4 (length (r::retirement-resident-words state))))
                          (loop for (star first count) in (r::resident-population-star-runs resident)
                                for near = (aref (r::retirement-resident-counts state) star)
                                for full = (/ (aref (aref ranges star) 1) 3)
                                for flat = (/ (aref (aref ranges (+ star 256)) 1) 3)
                                do (incf total-sites count) (incf near-sites near)
                                   (incf original-triangles (* count full))
                                   (incf triangles (+ (* near full) (* (- count near) flat)))
                                   (when (and (plusp near) (plusp full)) (incf draws))
                                   (when (and (< near count) (plusp flat)) (incf draws))))))
                    (r::renderer-mesh-slots renderer))
                   (format t "~&RETIREMENT ~A: sites=~D near=~D flat=~D triangles=~D/~D draws=~D upload=~D bytes/frame~%"
                           (car entry) total-sites near-sites (- total-sites near-sites)
                           triangles original-triangles draws bytes))))))
          (dolist (entry programs)
            (when directory
              (luv::call-on-sdl-canvas-thread
               (r:viewer-canvas host)
               (lambda ()
                 (select-retirement-state drawing (car entry))
                 (setf *interleaved-mode* (car entry)
                       (r::terrain-scene-program drawing) (cdr entry))))
              (retirement-capture host
               (merge-pathnames (format nil "~A.png" (car entry))
                                (uiop:ensure-directory-pathname directory))
               (first (r::renderer-extent renderer)) (second (r::renderer-extent renderer)))))
          ;; Native-only short motion inspection, without advancing streaming.
          ;; Each variant follows the same poses over the same resident scene.
          (when (and directory (uiop:getenv "RETIREMENT_MOTION"))
            (let* ((camera (r:viewer-camera host))
                   (position (r:camera-position camera))
                   (x (v:vec3-x position)))
              (unwind-protect
                   (dolist (entry programs)
                     (dotimes (frame 8)
                       (luv::call-on-sdl-canvas-thread
                        (r:viewer-canvas host)
                        (lambda ()
                          (setf (v:vec3-x position) (+ x (* frame 0.5)))
                          (select-retirement-state drawing (car entry))
                          (setf *interleaved-mode* (car entry)
                                (r::terrain-scene-program drawing) (cdr entry))))
                       (retirement-capture
                        host (merge-pathnames (format nil "~A-motion-~2,'0D.png" (car entry) frame)
                                              (uiop:ensure-directory-pathname directory))
                        (first (r::renderer-extent renderer)) (second (r::renderer-extent renderer)))))
                (luv::call-on-sdl-canvas-thread
                 (r:viewer-canvas host) (lambda () (setf (v:vec3-x position) x))))))
          (luv::call-on-sdl-canvas-thread
           (r:viewer-canvas host)
           (lambda () (select-retirement-state drawing "control"))))))

(unless (uiop:getenv "RETIREMENT_LIBRARY")
  (main)
  (sb-ext:exit :code 0 :abort t))
