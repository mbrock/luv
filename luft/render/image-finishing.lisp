(in-package #:luft.render)

;;; Copy reconstructed radiance into the HDR composite, then grade that
;;; composite for the display after flames and exposure. The frame specifies
;;; that order. Target generations own images and their borrowed bindings.

(defclass image-finishing (gpu-resource-owner)
  ((composite-program :accessor finishing-composite-program)
   (present-program :accessor finishing-present-program)
   (sampler :accessor finishing-sampler)))

(defgeneric make-composite-binding (finishing device scene))
(defgeneric make-presentation-binding (finishing device scene depth camera))
(defgeneric encode-composite (finishing pass binding))
(defgeneric encode-presentation (finishing pass binding))

(defmethod make-composite-binding ((finishing image-finishing) device scene)
  (make-program-binding (finishing-composite-program finishing) device
                        :scene scene :scene-sampler (finishing-sampler finishing)))

(defmethod make-presentation-binding ((finishing image-finishing) device scene depth camera)
  (make-program-binding (finishing-present-program finishing) device
                        :scene scene :scene-sampler (finishing-sampler finishing)
                        :scene-depth depth :camera-state camera))

(defmethod encode-composite ((finishing image-finishing) pass binding)
  (encode-program (finishing-composite-program finishing) pass binding
                  (make-gpu-draw-command :vertex-count 3)))

(defmethod encode-presentation ((finishing image-finishing) pass binding)
  (encode-program (finishing-present-program finishing) pass binding
                  (make-gpu-draw-command :vertex-count 3)))

(defun make-image-finishing (device color-format)
  (let ((finishing (make-instance 'image-finishing)))
    (with-gpu-construction (finishing)
      (setf (finishing-sampler finishing)
            (own-gpu-resource finishing device
                              (make-sampler-descriptor :label "luft image filtering"
                                                       :mag-filter :linear :min-filter :linear))
            (finishing-composite-program finishing)
            (own-gpu-object
             finishing
             (make-drawing-program
              device :label "luft HDR composite copy"
              :vertex (shaders:present-vertex-specification)
              :fragment (shaders::hdr-copy-fragment-specification)
              :targets '((:format :rgba16-float))))
            (finishing-present-program finishing)
            (own-gpu-object
             finishing
             (make-drawing-program
              device :label "luft HDR presentation"
              :vertex (shaders:present-vertex-specification)
              :fragment (shaders:present-fragment-specification)
              :targets `((:format ,color-format))))))))
