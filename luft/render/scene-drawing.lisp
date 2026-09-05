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
  ((program :accessor scene-drawing-program)
   (vertex-count :initarg :vertex-count :reader scene-drawing-vertex-count)))

(defmethod encode-scene-drawing ((drawing pipeline-scene-drawing) pass binding)
  (encode-program (scene-drawing-program drawing) pass binding
                  (make-gpu-draw-command :vertex-count (scene-drawing-vertex-count drawing))))

(defmethod release-scene-drawing ((drawing pipeline-scene-drawing))
  (release-owned-gpu-resources drawing))

(defun make-pipeline-scene-drawing
    (class device &key label vertex fragment targets sample-count depth-compare vertex-count)
  (let ((drawing (make-instance class :vertex-count vertex-count)))
    (with-gpu-construction (drawing)
      (setf (scene-drawing-program drawing)
            (own-gpu-object
             drawing
             (make-drawing-program
              device :label label :vertex vertex :fragment fragment
              :targets targets :sample-count sample-count
              :depth-stencil `(:format :depth32-float :depth-write-enabled nil
                               :depth-compare ,depth-compare)))))))
