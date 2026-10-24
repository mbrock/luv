;;;; Adjacent frames using the actual pre-change and integrated shaders.
(require :asdf)
(load "wiki/measurements/yawl-runtime-f87a44f/interleaved.lisp")
(in-package #:gpu-fps)

;; The host's initial pipeline must also use production, not the old probe.
(setf (symbol-function 'luft.render.shaders:star-fragment-specification)
      luft.render.shaders::*probe-original-fragment*)

(defun integrated-relief-program (renderer mode)
  (handler-bind ((warning #'muffle-warning))
    (load (if (equal mode "control")
              (or (uiop:getenv "RELIEF_BASELINE") "build/relief-baseline.lisp")
              "luft/render/shaders.lisp")))
  (let ((luft.render.shaders::*terrain-cloud-visibility-function*
          'luft.render.shaders::luvcraft-cloud-visibility)
        (luft.render.shaders::*surface-atmosphere-function*
          'luft.render.shaders::luvcraft-atmospheric-surface))
    (r::make-drawing-program
     (r::renderer-device renderer) :label (format nil "integrated relief ~A" mode)
     :vertex (luft.render.shaders:terrain-vertex-specification)
     :fragment (luft.render.shaders:star-fragment-specification)
     :targets '((:format :rgba16-float))
     :sample-count (r::renderer-scene-sample-count renderer)
     :depth-stencil '(:format :depth32-float :depth-write-enabled t :depth-compare :less))))

(setf (symbol-function 'block-program) #'integrated-relief-program)
(main)
(sb-ext:exit :code 0 :abort t)
