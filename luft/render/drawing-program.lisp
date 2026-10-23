(in-package #:luft.render)

;;; A drawing program joins shader stages, a checked input interface, and
;;; fixed pipeline state. Components compose programs; frame/publication
;;; owners supply their changing inputs and own the resulting bindings.

(defclass drawing-program (gpu-resource-owner)
  ((inputs :initarg :inputs :reader program-inputs)
   (layout :accessor program-layout)
   (pipeline :accessor program-pipeline)))

(defclass vertex-drawing-program (drawing-program)
  ((vertex-buffers :initarg :vertex-buffers :initform nil :reader program-vertex-buffers)))

(defgeneric encode-program (program pass binding command)
  (:documentation "Select PROGRAM and BINDING, then encode its draw COMMAND."))

(defgeneric release-program (program)
  (:documentation "Release owned resources; retain failed releases for retry."))

(defun make-program-binding (program device &rest inputs)
  "Create a caller-owned binding from shader resource names and GPU handles."
  (let ((resolved (apply #'luv.shader:bind-shader-resources (program-inputs program) inputs)))
    (create device
            (make-bind-group-descriptor
             :layout (program-layout program)
             :entries (loop for (declaration . resource) in resolved
                            collect `(:binding ,(luv.shader:shader-resource-binding declaration)
                                      :resource ,resource))))))

(defmethod encode-program ((program drawing-program) pass binding command)
  (set-pipeline pass (program-pipeline program))
  (set-bind-group pass 0 binding)
  (encode pass command))

(defmethod encode-program :before ((program vertex-drawing-program) pass binding command)
  (declare (ignore pass binding))
  (check-type command (or luv::gpu-draw-command luv::gpu-draw-indexed-command)))

(defmethod release-program ((program drawing-program))
  (release-owned-gpu-resources program))

(defmethod destroy ((program drawing-program))
  (release-program program))

(defun make-drawing-program
    (device &key label vertex fragment targets (sample-count 1) depth-stencil vertex-buffers)
  "Build a vertex program from its checked shader interface.
A vertex stage is required. Descriptor set zero is supported; invalid
stages, sets, and conflicting inputs fail before GPU allocation."
  (unless vertex
    (error "Drawing program ~S needs a vertex stage." label))
  (let ((geometry vertex))
    (unless (eq :vertex (luv.shader:shader-specification-stage geometry))
      (error "Drawing program ~S needs a vertex shader in its geometry stage." label))
    (when (and fragment (not (eq :fragment (luv.shader:shader-specification-stage fragment))))
      (error "Drawing program ~S needs a fragment shader in its fragment stage." label))
    (let* ((inputs (apply #'luv.shader:link-shader-resources
                          (remove nil (list geometry fragment))))
           (entries (mapcar #'program-input-layout-entry inputs))
           (program (make-instance 'vertex-drawing-program
                                   :inputs inputs :vertex-buffers vertex-buffers)))
      (with-gpu-construction (program)
        (labels ((own (descriptor) (own-gpu-resource program device descriptor))
                 (shader (specification)
                   (own (make-shader-module-descriptor
                         :label label :language :mathematical :code specification))))
          (setf (program-layout program)
                (own (make-bind-group-layout-descriptor :label label :entries entries)))
          (let ((geometry-module (shader geometry))
                (fragment-module (when fragment (shader fragment))))
            (setf (program-pipeline program)
                  (own (program-pipeline-descriptor
                        program geometry-module
                        :label label :layout (program-layout program)
                        :fragment (when fragment-module
                                    `(:module ,fragment-module :targets ,targets))
                        :sample-count sample-count :depth-stencil depth-stencil)))))))))

(defgeneric program-pipeline-descriptor (program geometry-module &rest state))

(defmethod program-pipeline-descriptor
    ((program vertex-drawing-program) geometry-module &rest state)
  (apply #'make-render-pipeline-descriptor
         :vertex `(:module ,geometry-module :buffers ,(program-vertex-buffers program))
         :primitive '(:topology :triangle-list) state))

(defun program-input-layout-entry (resource)
  (unless (zerop (luv.shader:shader-resource-descriptor-set resource))
    (error "Drawing program input ~S uses descriptor set ~D; this host supports set zero."
           (luv.shader:shader-resource-key resource)
           (luv.shader:shader-resource-descriptor-set resource)))
  (list :binding (luv.shader:shader-resource-binding resource)
        :type (ecase (luv.shader:shader-type-opaque-kind
                      (luv.shader:shader-declaration-type resource))
                (:uniform-block :uniform-buffer)
                (:storage-buffer :storage-buffer)
                (:texture :texture)
                (:sampler :sampler))))
