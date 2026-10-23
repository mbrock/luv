(require :asdf)
(assert (uiop:getenv "INTERLEAVED_LIBRARY"))
(load "wiki/measurements/yawl-runtime-f87a44f/interleaved.lisp")
(load "wiki/measurements/yawl-bevel-slivers-5ee6831/scene-only.lisp")
(in-package #:gpu-fps)

(defvar *bevel-probe-states* (make-hash-table :test #'eq))

(defun bevel-probe-state (drawing)
  (list (r::terrain-scene-vertices drawing) (r::terrain-normals drawing)
        (r::terrain-scene-indices drawing) (r::terrain-star-ranges drawing)))

(defun select-bevel-probe-state (drawing mode)
  (destructuring-bind (vertices normals indices ranges)
      (getf (gethash drawing *bevel-probe-states*) (if (equal mode "flat") :flat :control))
    (setf (r::terrain-scene-vertices drawing) vertices
          (r::terrain-normals drawing) normals
          (r::terrain-scene-indices drawing) indices
          (r::terrain-star-ranges drawing) ranges)))

(let ((original (symbol-function 'block-program)))
  (setf (symbol-function 'block-program)
        (lambda (renderer mode)
          (let ((drawing (r::renderer-terrain renderer)))
            (unless (gethash drawing *bevel-probe-states*)
              (let ((owner (make-instance 'r::gpu-resource-owner))
                    (control (bevel-probe-state drawing)))
                (r::with-gpu-construction (owner)
                  (multiple-value-bind (vertices normals indices shadow-vertices shadow-indices ranges)
                      (r::bevel-probe-flat-tables)
                    (declare (ignore shadow-vertices shadow-indices))
                    (flet ((upload (words usage)
                             (let ((buffer (r::own-gpu-resource
                                            owner (r::renderer-device renderer)
                                            (luv:make-buffer-descriptor :size (* 4 (length words))
                                                                       :usage (list usage :copy-dst)))))
                               (luv:write-buffer buffer words)
                               buffer)))
                      (setf (gethash drawing *bevel-probe-states*)
                            (list :control control :flat
                                  (list (upload vertices :storage) (upload normals :storage)
                                        (upload indices :index) ranges)))))
                  (r::own-gpu-object drawing owner))))
            (funcall original renderer (if (equal mode "flat") "control-copy" mode))))))

(let ((original (symbol-function 'advance-fixture)))
  (setf (symbol-function 'advance-fixture)
        (lambda (kind host old-p)
          (prog1 (funcall original kind host old-p)
            (when *interleaved-programs*
              (select-bevel-probe-state *interleaved-drawing* *interleaved-mode*))))))

(setf *interleaved-inspection*
      (lambda (host programs)
        (let ((drawing (r::renderer-terrain (r:viewer-renderer host)))
              (directory (uiop:getenv "BEVEL_CAPTURE_DIR")))
          (when directory
            (dolist (entry programs)
              (luv::call-on-sdl-canvas-thread
               (r:viewer-canvas host)
               (lambda ()
                 (select-bevel-probe-state drawing (car entry))
                 (setf *interleaved-mode* (car entry)
                       (r::terrain-scene-program drawing) (cdr entry))))
              (r:capture-viewer-frame
               (merge-pathnames (format nil "~A.png" (car entry))
                                (uiop:ensure-directory-pathname directory)) host)))
          (luv::call-on-sdl-canvas-thread
           (r:viewer-canvas host) (lambda () (select-bevel-probe-state drawing "control"))))))

(main)
(sb-ext:exit :code 0 :abort t)
