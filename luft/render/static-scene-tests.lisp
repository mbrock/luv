(in-package #:luft.render.tests)

(define-test static-scenes-retain-regional-geometry-and-light-through-publication
  (dolist (cells '(nil ((10 10)) ((63 63) (64 64))))
    (let ((builder (render::make-world-scene-builder :horizontal-bits 7)))
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

(define-test a-translucent-cell-is-meshed-apart-from-the-opaque-world
  ;; #LNRIRA: the opaque sheet sees glass as air and the glass sheet sees
  ;; only glass, so stone beside glass keeps its face toward it, the glass is
  ;; a whole cube, and neither sheet hides the other.
  (let* ((stone (make-instance 'render::material-placement
                               :name :stone
                               :kind (make-instance 'render::material-kind
                                                    :name :stone :base-tone '(0.5 0.5 0.5))))
         (glass (make-instance 'render::material-placement
                               :name :glass
                               :kind (make-instance 'render::material-kind
                                                    :name :glass :base-tone '(0.2 0.6 0.8)
                                                    :coverage 0.3)))
         (palette (luv.domains:make-identity-vocabulary-domain
                   :members (list stone glass) :limit #xff)))
    (flet ((population (cells)
             (let ((builder (render::make-scene-builder
                             :horizontal-bits 7 :material-vocabulary palette
                             :ground-material stone)))
               (loop for (x material) in cells
                     do (render::scene-builder-cell builder x 10 2 :material material))
               (render::make-render-population
                (list (render::make-render-mesh (render::finish-scene-builder builder)))))))
      (let ((both (population (list (list 10 stone) (list 11 glass))))
            (stone-alone (population (list (list 10 stone))))
            (glass-as-stone (population (list (list 11 stone)))))
        (true (null (render::render-population-translucent-sites stone-alone)))
        (let ((flags (render::render-population-translucent-sites both)))
          (true flags)
          ;; The glass sheet is the lone cube; the opaque sheet the lone stone.
          (true (= (render::render-population-site-count glass-as-stone) (count 1 flags)))
          (true (= (render::render-population-site-count stone-alone) (count 0 flags))))
        ;; Glass throws no sun shadow: only the stone's faces are casters.
        (true (equalp (render::render-population-shadow-rectangles stone-alone)
                      (render::render-population-shadow-rectangles both)))))))
