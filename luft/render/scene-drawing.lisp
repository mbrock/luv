(in-package #:luft.render)

;;; A scene drawing contributes to an already-open HDR/depth pass. The frame
;;; supplies camera/pose and shadow inputs; the drawing owns its GPU program.
;;; The renderer chooses placement (sky before geometry, player afterward).
;;; Keeping that order at the composition site is part of the interface.

(defclass scene-drawing () ())

(defgeneric make-scene-drawing-binding
    (drawing device camera-buffer shadow-view shadow-sampler)
  (:documentation
   "Return a fresh binding borrowing the frame inputs, or NIL.
The frame owns the result and releases it before its inputs or DRAWING."))

(defgeneric encode-scene-drawing (drawing pass binding)
  (:documentation "Draw into the active scene PASS using its frame BINDING."))

(defgeneric release-scene-drawing (drawing)
  (:documentation
   "Release the program after its frame bindings; retry any failed releases."))

(defun make-scene-drawing (factory device target-formats sample-count)
  "Construct one fresh drawing, or omit it when FACTORY is NIL."
  (when factory
    (let ((drawing (funcall factory device target-formats sample-count)))
      (check-type drawing scene-drawing)
      drawing)))

(defmethod make-scene-drawing-binding
    ((drawing null) device camera-buffer shadow-view shadow-sampler)
  (declare (ignore device camera-buffer shadow-view shadow-sampler))
  nil)

(defmethod encode-scene-drawing ((drawing null) pass binding)
  (declare (ignore pass binding))
  (values))

(defmethod release-scene-drawing ((drawing null))
  (values))

;;; Both current drawings use one triangle-list pipeline. This implementation
;;; shares allocation and draw mechanics without prescribing other drawings'
;;; representation or forcing the renderer to know their layouts or shaders.

(defclass pipeline-scene-drawing (scene-drawing gpu-resource-owner)
  ((layout :accessor scene-drawing-layout)
   (pipeline :accessor scene-drawing-pipeline)
   (vertex-count :initarg :vertex-count :reader scene-drawing-vertex-count)))

(defmethod encode-scene-drawing ((drawing pipeline-scene-drawing) pass binding)
  (set-pipeline pass (scene-drawing-pipeline drawing))
  (set-bind-group pass 0 binding)
  (draw pass (scene-drawing-vertex-count drawing) 1))

(defmethod release-scene-drawing ((drawing pipeline-scene-drawing))
  (release-owned-gpu-resources drawing))

(defun make-pipeline-scene-drawing
    (class device &key label entries vertex fragment targets sample-count
                       depth-compare vertex-count)
  (let ((drawing (make-instance class :vertex-count vertex-count)))
    (with-gpu-construction (drawing)
      (flet ((own (descriptor) (own-gpu-resource drawing device descriptor)))
        (setf (scene-drawing-layout drawing)
              (own (make-bind-group-layout-descriptor
                    :label label :entries entries)))
        (let ((vertex
                (own (make-shader-module-descriptor
                      :label (concatenate 'string label " vertex")
                      :language :mathematical :code vertex)))
              (fragment
                (own (make-shader-module-descriptor
                      :label (concatenate 'string label " fragment")
                      :language :mathematical :code fragment))))
          (setf (scene-drawing-pipeline drawing)
                (own (make-render-pipeline-descriptor
                      :label label :layout (scene-drawing-layout drawing)
                      :vertex `(:module ,vertex)
                      :fragment `(:module ,fragment :targets ,targets)
                      :primitive '(:topology :triangle-list)
                      :sample-count sample-count
                      :depth-stencil
                      `(:format :depth32-float :depth-write-enabled nil
                        :depth-compare ,depth-compare)))))))))
