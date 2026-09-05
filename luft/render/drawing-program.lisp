(in-package #:luft.render)

;;; A drawing program joins shader stages, a checked input interface, and
;;; fixed pipeline state. Components compose programs; frame/publication
;;; owners supply their changing inputs and own the resulting bindings.

(defclass drawing-program (gpu-resource-owner)
  ((inputs :initarg :inputs :reader program-inputs)
   (layout :accessor program-layout)
   (pipeline :accessor program-pipeline)))

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
  (check-type command luv::gpu-draw-command)
  (set-pipeline pass (program-pipeline program))
  (set-bind-group pass 0 binding)
  (encode pass command))

(defmethod release-program ((program drawing-program))
  (release-owned-gpu-resources program))

(defmethod destroy ((program drawing-program))
  (release-program program))

(defun make-drawing-program
    (device &key label vertex fragment targets (sample-count 1) depth-stencil)
  "Build a triangle-list program transactionally from its shader declarations.
This first implementation uses vertex drawing and descriptor set zero. Both
restrictions are checked before allocation; resource names and numbers come
entirely from the DSL. No host-side layout ledger is needed."
  (unless (and vertex (eq :vertex (luv.shader:shader-specification-stage vertex)))
    (error "Drawing program ~S needs a vertex shader." label))
  (when (and fragment (not (eq :fragment (luv.shader:shader-specification-stage fragment))))
    (error "Drawing program ~S needs a fragment shader in its fragment stage." label))
  (let* ((inputs (apply #'luv.shader:link-shader-resources (remove nil (list vertex fragment))))
         (entries (mapcar #'program-input-layout-entry inputs))
         (program (make-instance 'drawing-program :inputs inputs)))
    (with-gpu-construction (program)
      (labels ((own (descriptor) (own-gpu-resource program device descriptor))
               (shader (specification)
                 (own (make-shader-module-descriptor
                       :label label :language :mathematical :code specification))))
        (setf (program-layout program)
              (own (make-bind-group-layout-descriptor :label label :entries entries)))
        (let ((vertex-module (shader vertex))
              (fragment-module (when fragment (shader fragment))))
          (setf (program-pipeline program)
                (own (make-render-pipeline-descriptor
                      :label label :layout (program-layout program)
                      :vertex `(:module ,vertex-module)
                      :fragment (when fragment-module
                                  `(:module ,fragment-module :targets ,targets))
                      :primitive '(:topology :triangle-list)
                      :sample-count sample-count :depth-stencil depth-stencil))))))))

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
                (:texture-2d :texture)
                (:sampler :sampler))))
