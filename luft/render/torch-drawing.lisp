(in-package #:luft.render)

;;; One positioned torch population contributes in three places: its opaque
;;; bodies to shadows and scene geometry, and its animated flames to the HDR
;;; image after temporal reconstruction. The frame keeps that ordering visible.
;;;
;;; This interface starts with already-realized attachment frames. It neither
;;; chooses attachments nor solves voxel light. Publications own instance data;
;;; presentation slots own effect uploads; bindings borrow those inputs and
;;; the sun/depth views. Release those bindings before the drawing's program.

(defclass torch-drawing () ())

(defgeneric make-torch-frame-buffer (drawing device)
  (:documentation "Return a fresh frame-owned effect buffer, or NIL."))

(defgeneric upload-torch-frame (drawing buffer time)
  (:documentation "Update the safely reusable frame's effect state for TIME."))

(defgeneric make-torch-body-binding
    (drawing device instances camera shadow-view shadow-sampler &key shadow-p)
  (:documentation "Return a fresh caller-owned binding for scene or shadow bodies."))

(defgeneric make-torch-flame-binding (drawing device instances camera effect depth-view)
  (:documentation
   "Return a fresh caller-owned binding borrowing opaque depth and frame inputs.
NIL EFFECT selects the drawing's immutable initial effect for staged bindings."))

(defgeneric encode-torch-bodies (drawing pass binding count &key shadow-p))
(defgeneric encode-torch-flames (drawing pass binding count))
(defgeneric release-torch-drawing (drawing)
  (:documentation "Release program resources, retaining failed handles for retry."))

;;; NIL omits GPU drawing without changing the authored attachment/light data.

(defmethod make-torch-frame-buffer ((drawing null) device)
  (declare (ignore device))
  nil)

(defmethod upload-torch-frame ((drawing null) buffer time)
  (declare (ignore buffer time))
  (values))

(defmethod make-torch-body-binding
    ((drawing null) device instances camera shadow-view shadow-sampler &key shadow-p)
  (declare (ignore device instances camera shadow-view shadow-sampler shadow-p))
  nil)

(defmethod make-torch-flame-binding
    ((drawing null) device instances camera effect depth-view)
  (declare (ignore device instances camera effect depth-view))
  nil)

(defmethod encode-torch-bodies ((drawing null) pass binding count &key shadow-p)
  (declare (ignore pass binding count shadow-p))
  (values))

(defmethod encode-torch-flames ((drawing null) pass binding count)
  (declare (ignore pass binding count))
  (values))

(defmethod release-torch-drawing ((drawing null))
  (values))

;;; The current implementation shares one three-row attachment frame between
;;; a canonical bronze body mesh and a ray-integrated flame. Shader modules are
;;; private resources; only the operational layouts/pipelines need named slots.

(defclass framed-torch-drawing (torch-drawing gpu-resource-owner)
  ((vertices :accessor torch-drawing-vertices)
   (body-layout :accessor torch-drawing-body-layout)
   (shadow-layout :accessor torch-drawing-shadow-layout)
   (body-pipeline :accessor torch-drawing-body-pipeline)
   (shadow-pipeline :accessor torch-drawing-shadow-pipeline)
   (flame-layout :accessor torch-drawing-flame-layout)
   (flame-pipeline :accessor torch-drawing-flame-pipeline)
   (depth-sampler :accessor torch-drawing-depth-sampler)
   (initial-effect :accessor torch-drawing-initial-effect)))

(defun torch-frame-buffer-descriptor ()
  (make-buffer-descriptor
   :label "luft torch flame effect" :size (torch-flame-effect-byte-size)
   :usage '(:uniform :copy-dst)))

(defmethod make-torch-frame-buffer ((drawing framed-torch-drawing) device)
  (create device (torch-frame-buffer-descriptor)))

(defmethod upload-torch-frame ((drawing framed-torch-drawing) buffer time)
  (check-type time real)
  (write-buffer buffer (torch-flame-effect-uniform-data (coerce time 'single-float))))

(defmethod make-torch-body-binding
    ((drawing framed-torch-drawing) device instances camera shadow-view shadow-sampler
     &key shadow-p)
  (create device
          (make-bind-group-descriptor
           :label (if shadow-p "luft torch-body shadows" "luft torch bodies")
           :layout (if shadow-p (torch-drawing-shadow-layout drawing)
                       (torch-drawing-body-layout drawing))
           :entries
           `((:binding 0 :resource ,instances)
             (:binding 1 :resource ,(torch-drawing-vertices drawing))
             (:binding 2 :resource ,camera)
             ,@(unless shadow-p
                 `((:binding 4 :resource ,shadow-view)
                   (:binding 5 :resource ,shadow-sampler)))))))

(defmethod make-torch-flame-binding
    ((drawing framed-torch-drawing) device instances camera effect depth-view)
  (create device
          (make-bind-group-descriptor
           :label "luft post-temporal torch flames"
           :layout (torch-drawing-flame-layout drawing)
           :entries
           `((:binding 0 :resource ,instances)
             (:binding 1 :resource ,camera)
             (:binding 2 :resource ,(or effect (torch-drawing-initial-effect drawing)))
             (:binding 3 :resource ,depth-view)
             (:binding 4 :resource ,(torch-drawing-depth-sampler drawing))))))

(defmethod encode-torch-bodies
    ((drawing framed-torch-drawing) pass binding count &key shadow-p)
  (when (plusp count)
    (set-pipeline pass (if shadow-p (torch-drawing-shadow-pipeline drawing)
                          (torch-drawing-body-pipeline drawing)))
    (set-bind-group pass 0 binding)
    (draw pass (torch-body-vertex-count) count)))

(defmethod encode-torch-flames ((drawing framed-torch-drawing) pass binding count)
  (when (plusp count)
    (set-pipeline pass (torch-drawing-flame-pipeline drawing))
    (set-bind-group pass 0 binding)
    (draw pass 6 count)))

(defmethod release-torch-drawing ((drawing framed-torch-drawing))
  (release-owned-gpu-resources drawing))

(defun make-framed-torch-drawing (device target-formats sample-count)
  "Construct the body/shadow/flame programs as one transactional GPU owner."
  (let ((drawing (make-instance 'framed-torch-drawing)))
    (with-gpu-construction (drawing)
      (labels ((own (descriptor) (own-gpu-resource drawing device descriptor))
               (shader (label specification)
                 (own (make-shader-module-descriptor
                       :label label :language :mathematical :code specification))))
        (let ((vertices (torch-body-vertex-data)))
          (setf (torch-drawing-vertices drawing)
                (own (make-buffer-descriptor
                      :label "luft canonical framed torch body"
                      :size (* 4 (length vertices)) :usage '(:storage :copy-dst))))
          (write-buffer (torch-drawing-vertices drawing) vertices))
        (setf (torch-drawing-initial-effect drawing) (own (torch-frame-buffer-descriptor)))
        (upload-torch-frame drawing (torch-drawing-initial-effect drawing) 0.0)
        (setf (torch-drawing-body-layout drawing)
              (own (make-bind-group-layout-descriptor
                    :label "luft framed torch-body layout"
                    :entries '((:binding 0 :type :storage-buffer)
                               (:binding 1 :type :storage-buffer)
                               (:binding 2 :type :uniform-buffer)
                               (:binding 4 :type :texture)
                               (:binding 5 :type :sampler))))
              (torch-drawing-shadow-layout drawing)
              (own (make-bind-group-layout-descriptor
                    :label "luft torch shadow layout"
                    :entries '((:binding 0 :type :storage-buffer)
                               (:binding 1 :type :storage-buffer)
                               (:binding 2 :type :uniform-buffer))))
              (torch-drawing-flame-layout drawing)
              (own (make-bind-group-layout-descriptor
                    :label "luft torch flame layout"
                    :entries '((:binding 0 :type :storage-buffer)
                               (:binding 1 :type :uniform-buffer)
                               (:binding 2 :type :uniform-buffer)
                               (:binding 3 :type :texture)
                               (:binding 4 :type :sampler))))
              (torch-drawing-depth-sampler drawing)
              (own (make-sampler-descriptor
                    :label "luft torch flame opaque-depth sampler"
                    :mag-filter :nearest :min-filter :nearest :mipmap-filter :nearest)))
        (let ((body-vertex (shader "luft torch-body vertex"
                                   (shaders:torch-body-vertex-specification)))
              (body-fragment (shader "luft torch-body fragment"
                                     (shaders:torch-body-fragment-specification)))
              (shadow-vertex (shader "luft torch shadow vertex"
                                     (shaders:torch-body-shadow-vertex-specification)))
              (flame-vertex (shader "luft torch flame vertex"
                                    (shaders:torch-flame-vertex-specification)))
              (flame-fragment (shader "luft torch flame fragment"
                                      (shaders:torch-flame-fragment-specification))))
          (setf (torch-drawing-body-pipeline drawing)
                (own (make-render-pipeline-descriptor
                      :label "luft framed opaque torch bodies"
                      :layout (torch-drawing-body-layout drawing)
                      :vertex `(:module ,body-vertex)
                      :fragment `(:module ,body-fragment
                                  :targets ,(mapcar (lambda (format) `(:format ,format))
                                                    target-formats))
                      :primitive '(:topology :triangle-list) :sample-count sample-count
                      :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                       :depth-compare :less)))
                (torch-drawing-shadow-pipeline drawing)
                (own (make-render-pipeline-descriptor
                      :label "luft framed torch-body shadows"
                      :layout (torch-drawing-shadow-layout drawing)
                      :vertex `(:module ,shadow-vertex)
                      :primitive '(:topology :triangle-list)
                      :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                       :depth-compare :less)))
                (torch-drawing-flame-pipeline drawing)
                (own (make-render-pipeline-descriptor
                      :label "luft volumetric torch flame pipeline"
                      :layout (torch-drawing-flame-layout drawing)
                      :vertex `(:module ,flame-vertex)
                      :fragment `(:module ,flame-fragment
                                  :targets ((:format :rgba16-float
                                             :blend :premultiplied-alpha)))
                      :primitive '(:topology :triangle-list)))))))))
