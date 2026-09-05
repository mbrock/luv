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
   "Return a fresh caller-owned binding borrowing opaque depth and frame inputs."))

(defgeneric encode-torch-bodies (drawing pass binding count &key shadow-p))
(defgeneric encode-torch-flames (drawing pass binding count))
(defgeneric release-torch-drawing (drawing)
  (:documentation "Release program resources, retaining failed handles for retry."))

(defmethod destroy ((component torch-drawing))
  (release-torch-drawing component))

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
;;; private resources; each pass has a program; geometry and sampling remain component-owned.

(defclass framed-torch-drawing (torch-drawing gpu-resource-owner)
  ((vertices :accessor torch-drawing-vertices)
   (body-program :accessor torch-drawing-body-program)
   (shadow-program :accessor torch-drawing-shadow-program)
   (flame-program :accessor torch-drawing-flame-program)
   (depth-sampler :accessor torch-drawing-depth-sampler)))

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
  (apply #'make-program-binding
         (if shadow-p (torch-drawing-shadow-program drawing) (torch-drawing-body-program drawing))
         device :torch-frames instances :torch-body-vertices (torch-drawing-vertices drawing)
         :camera-state camera
         (unless shadow-p (list :shadow-map shadow-view :shadow-sampler shadow-sampler))))

(defmethod make-torch-flame-binding
    ((drawing framed-torch-drawing) device instances camera effect depth-view)
  (make-program-binding
   (torch-drawing-flame-program drawing) device
   :flame-instances instances :camera-state camera
   :effect-state effect
   :opaque-depth depth-view :depth-sampler (torch-drawing-depth-sampler drawing)))

(defmethod encode-torch-bodies
    ((drawing framed-torch-drawing) pass binding count &key shadow-p)
  (when (plusp count)
    (encode-program
     (if shadow-p (torch-drawing-shadow-program drawing) (torch-drawing-body-program drawing))
     pass binding (make-gpu-draw-command :vertex-count (torch-body-vertex-count)
                                       :instance-count count))))

(defmethod encode-torch-flames ((drawing framed-torch-drawing) pass binding count)
  (when (plusp count)
    (encode-program (torch-drawing-flame-program drawing) pass binding
                    (make-gpu-draw-command :vertex-count 6 :instance-count count))))

(defmethod release-torch-drawing ((drawing framed-torch-drawing))
  (release-owned-gpu-resources drawing))

(defun make-framed-torch-drawing (device target-formats sample-count)
  "Construct the body/shadow/flame programs as one transactional GPU owner."
  (let ((drawing (make-instance 'framed-torch-drawing)))
    (with-gpu-construction (drawing)
      (labels ((own (descriptor) (own-gpu-resource drawing device descriptor))
               (program (&rest description)
                 (own-gpu-object drawing (apply #'make-drawing-program device description))))
        (let ((vertices (torch-body-vertex-data)))
          (setf (torch-drawing-vertices drawing)
                (own (make-buffer-descriptor
                      :label "luft canonical framed torch body"
                      :size (* 4 (length vertices)) :usage '(:storage :copy-dst))))
          (write-buffer (torch-drawing-vertices drawing) vertices))
        (setf (torch-drawing-depth-sampler drawing)
              (own (make-sampler-descriptor
                    :label "luft torch flame opaque-depth sampler"
                    :mag-filter :nearest :min-filter :nearest :mipmap-filter :nearest))
              (torch-drawing-body-program drawing)
              (program :label "luft framed opaque torch bodies"
                       :vertex (shaders:torch-body-vertex-specification)
                       :fragment (shaders:torch-body-fragment-specification)
                       :targets (mapcar (lambda (format) `(:format ,format)) target-formats)
                       :sample-count sample-count
                       :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                        :depth-compare :less))
              (torch-drawing-shadow-program drawing)
              (program :label "luft framed torch-body shadows"
                       :vertex (shaders:torch-body-shadow-vertex-specification)
                       :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                        :depth-compare :less))
              (torch-drawing-flame-program drawing)
              (program :label "luft volumetric torch flame pipeline"
                       :vertex (shaders:torch-flame-vertex-specification)
                       :fragment (shaders:torch-flame-fragment-specification)
                       :targets '((:format :rgba16-float :blend :premultiplied-alpha))))))))
