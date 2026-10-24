(in-package #:luft.render.tests)

(define-test world-text-drawing-rolls-back-every-allocation-prefix
  (let* ((device (make-instance 'gpu-test-device))
         (drawing (render::make-world-text-drawing
                   device '(:rgba16-float :rg16-float) 4))
         (allocations (test-gpu-attempts device)))
    (luv:destroy drawing)
    (loop for failure from 1 to allocations do
      (let ((device (make-instance 'gpu-test-device :fail-at failure)))
        (fail (render::make-world-text-drawing
               device '(:rgba16-float :rg16-float) 4))
        (dolist (resource (test-gpu-resources device))
          (true (= 1 (test-gpu-release-attempts resource))))))))

(define-test world-text-shaders-assemble-for-vulkan
  (dolist (name '(luft.render.shaders:world-panel-vertex-specification
                  luft.render.shaders:world-panel-fragment-specification
                  luft.render.shaders:world-glyph-vertex-specification
                  luft.render.shaders:world-glyph-fragment-specification))
    (true (= #x07230203
             (aref (luv.spir-v:assemble-shader-specification (funcall name)) 0))
          "~S lowers to a SPIR-V module" name)))

(define-test terminal-surfaces-face-their-viewer
  ;; Facing -X, a viewer looking east has +Y on their left, so the screen's
  ;; right runs toward -Y and its origin is the lower corner at the north
  ;; edge (y = 29), in a plane just west of the cells.
  (let ((surface (render::make-terminal-surface-from-cells
                  '((40 27 16) (40 28 16) (40 27 17) (40 28 17))
                  '(-1 0 0))))
    (true (= 2 (render::terminal-surface-width surface)))
    (true (= 2 (render::terminal-surface-height surface)))
    (true (< (abs (- 0.0 (vec3-x (render::terminal-surface-right surface)))) 1e-6))
    (true (= -1 (vec3-y (render::terminal-surface-right surface))))
    (let ((origin (render::terminal-surface-origin surface)))
      (true (< 39.9 (vec3-x origin) 40.0) "the screen floats west of x = 40")
      (true (= 29 (vec3-y origin)))
      (true (= 16 (vec3-z origin)))))
  ;; Facing +Y, a viewer looking south has -X on their right.
  (let ((surface (render::make-terminal-surface-from-cells
                  '((10 5 3) (11 5 3) (12 5 3))
                  '(0 1 0))))
    (true (= 3 (render::terminal-surface-width surface)))
    (true (= 1 (render::terminal-surface-height surface)))
    (true (= -1 (vec3-x (render::terminal-surface-right surface))))
    (let ((origin (render::terminal-surface-origin surface)))
      (true (= 13 (vec3-x origin)))
      (true (< 6.0 (vec3-y origin) 6.1) "the screen floats north of y = 6"))))

(define-test terminal-walls-join-exposed-cells-and-retire-when-covered
  (let ((builder (render::make-world-scene-builder :horizontal-bits 6)))
    (loop for x from 10 to 16 do
      (loop for z from 4 to 7 do
        (render::scene-builder-cell builder x 20 z
                                    :material render::*terminal-material-placement*)))
    ;; Neither a disconnected terminal nor an adjoining ordinary block is
    ;; part of the shell's wall, even when they share its face plane.
    (render::scene-builder-cell builder 8 20 4
                                :material render::*terminal-material-placement*)
    (render::scene-builder-cell builder 17 20 4)
    (let* ((scene (render::finish-scene-builder builder :voxel-light-propagation-p nil))
           (surface (render::find-terminal-surface scene 13 20 5 '(0 -1 0))))
      (true surface)
      (true (= 28 (length (render::terminal-surface-cells surface))))
      (true (= 7 (render::terminal-surface-width surface)))
      (true (= 4 (render::terminal-surface-height surface)))
      (true (render::terminal-surface-standing-p scene surface))
      (true (null (render::find-terminal-surface scene 13 20 5 '(0 0 1)))
            "tops do not open shells")
      (true (null (render::find-terminal-surface scene 13 20 5 '(0 0 -1)))
            "bottoms do not open shells")
      (render::scene-builder-cell builder 13 19 5)
      (let ((covered (render::finish-scene-builder builder :voxel-light-propagation-p nil)))
        (true (not (render::terminal-surface-standing-p covered surface)))
        (true (null (render::find-terminal-surface covered 13 20 5 '(0 -1 0))))
        (true (render::terminal-surface-standing-p scene surface)
              "a successor scene does not mutate the original terminal wall")))))

(define-test single-terminal-cells-have-a-usable-grid-on-every-wall-side
  (let ((builder (render::make-world-scene-builder :horizontal-bits 6)))
    (render::scene-builder-cell builder 10 20 4
                                :material render::*terminal-material-placement*)
    (let ((scene (render::finish-scene-builder builder :voxel-light-propagation-p nil)))
      (zpb-ttf:with-font-loader (font render::*terminal-font-pathname*)
        (dolist (normal '((1 0 0) (-1 0 0) (0 1 0) (0 -1 0)))
          (let ((surface (render::find-terminal-surface scene 10 20 4 normal)))
            (true surface)
            (true (= 1 (render::terminal-surface-width surface)))
            (true (= 1 (render::terminal-surface-height surface)))
            (multiple-value-bind (columns rows) (render::terminal-grid-size surface font)
              (true (plusp columns))
              (true (= render::*terminal-rows-per-cell* rows))
              (multiple-value-bind (scale left bottom)
                  (render::terminal-grid-placement surface columns rows font)
                (true (plusp scale))
                (true (<= render::*terminal-margin* (+ left 1e-6)))
                (true (<= render::*terminal-margin* (+ bottom 1e-6)))))))))))
