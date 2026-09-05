(in-package #:luft.render.tests)

(define-test static-scenes-retain-regional-geometry-and-light-through-publication
  (dolist (cells '(nil ((10 10)) ((63 63) (64 64))))
    (let ((builder (render::make-scene-builder :horizontal-bits 7)))
      (dolist (xy cells)
        (render::scene-builder-cell builder (first xy) (second xy) 2))
      (let* ((scene (render::finish-scene-builder builder))
             (owners (render::make-scene-regional-meshes scene 1))
             (device (make-instance 'gpu-test-device))
             (renderer (render:make-renderer device :bgra8-unorm '(640 480))))
        (unwind-protect
             (multiple-value-bind (mesh generation) (render::make-render-mesh scene)
               (true (equalp
                      (apply #'concatenate '(simple-array (unsigned-byte 32) (*))
                             (mapcar (lambda (entry)
                                       (luft:surface-mesh-star-site-words (cdr entry)))
                                     owners))
                      (luft:surface-mesh-star-site-words mesh)))
               (true (eq (render::realized-light-generation-field
                          (render::scene-mesh-generation-light-generation generation))
                         (luft:surface-mesh-voxel-light mesh)))
               (render::renderer-update-meshes
                renderer (list (cons 0 mesh)) nil :scene-generation generation)
               (true (equal '(0) (render::renderer-slot-order renderer))))
          (render:destroy-renderer renderer))))))
