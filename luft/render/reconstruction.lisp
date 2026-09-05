(in-package #:luft.render)

;;; Reconstruction selects an implementation and tracks whether the previous
;;; view may contribute. Shader reconstruction owns a drawing program; MetalFX
;;; owns its native scaler with the resized targets. Direct rendering needs
;;; neither. The frame retains explicit texture transitions and pass order.

(defclass reconstruction (gpu-resource-owner)
  ((kind :initarg :kind :reader reconstruction-kind)
   (program :initform nil :accessor reconstruction-program)
   (previous-view :initform nil :accessor reconstruction-previous-view)
   (history-valid-p :initform nil :accessor reconstruction-history-valid-p)
   (history-used-p :initform nil :accessor reconstruction-history-used-p)))

(defgeneric make-reconstruction-binding (reconstruction device current motion history sampler camera))
(defgeneric encode-reconstruction-resolve (reconstruction pass binding))

(defmethod make-reconstruction-binding
    ((reconstruction reconstruction) device current motion history sampler camera)
  (make-program-binding (reconstruction-program reconstruction) device
                        :current current :motion-texture motion :history history
                        :temporal-sampler sampler :camera-state camera))

(defmethod encode-reconstruction-resolve ((reconstruction reconstruction) pass binding)
  (encode-program (reconstruction-program reconstruction) pass binding
                  (make-gpu-draw-command :vertex-count 3)))

(defun make-reconstruction (device)
  (let ((reconstruction (make-instance 'reconstruction :kind (temporal-resolve-kind device))))
    (with-gpu-construction (reconstruction)
      (when (eq :shader (reconstruction-kind reconstruction))
        (setf (reconstruction-program reconstruction)
              (own-gpu-object
               reconstruction
               (make-drawing-program
                device :label "luft temporal resolve"
                :vertex (shaders:present-vertex-specification)
                :fragment (shaders:temporal-resolve-fragment-specification)
                :targets '((:format :rgba16-float)))))))))

;;; Device policy at the composition boundary.

(defun temporal-resolve-kind (device)
  "Return the temporal implementation selected for DEVICE, or NIL."
  #-darwin (declare (ignore device))
  (when *temporal-upscaling-p*
    #+darwin
    (if (typep device 'metal-gpu-device) :metalfx :shader)
    #-darwin :shader))
