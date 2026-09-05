(in-package #:luft.render)

;;; Window-sized images and their bindings form one transactional owner.
;;; Build privately, publish one identity, then retire the previous owner.

(defclass renderer-target-generation (gpu-resource-owner)
  ;; Initialize privately, then publish this complete identity in one swap.
  ((extent :initform nil :reader renderer-target-generation-extent)
   (render-extent :initform nil :reader renderer-target-generation-render-extent)
   (temporal-scaler :initform nil :reader renderer-target-generation-temporal-scaler)
   (depth-msaa-texture :initform nil :reader renderer-target-generation-depth-msaa-texture)
   (depth-msaa-view :initform nil :reader renderer-target-generation-depth-msaa-view)
   (depth-texture :initform nil :reader renderer-target-generation-depth-texture)
   (depth-view :initform nil :reader renderer-target-generation-depth-view)
   (scene-msaa-texture :initform nil :reader renderer-target-generation-scene-msaa-texture)
   (scene-msaa-view :initform nil :reader renderer-target-generation-scene-msaa-view)
   (scene-texture :initform nil :reader renderer-target-generation-scene-texture)
   (scene-view :initform nil :reader renderer-target-generation-scene-view)
   (motion-msaa-texture :initform nil :reader renderer-target-generation-motion-msaa-texture)
   (motion-msaa-view :initform nil :reader renderer-target-generation-motion-msaa-view)
   (motion-texture :initform nil :reader renderer-target-generation-motion-texture)
   (motion-view :initform nil :reader renderer-target-generation-motion-view)
   (resolved-texture :initform nil :reader renderer-target-generation-resolved-texture)
   (resolved-view :initform nil :reader renderer-target-generation-resolved-view)
   (history-texture :initform nil :reader renderer-target-generation-history-texture)
   (history-view :initform nil :reader renderer-target-generation-history-view)
   (composite-texture :initform nil :reader renderer-target-generation-composite-texture)
   (composite-view :initform nil :reader renderer-target-generation-composite-view)
   (composite-source-bind-group :initform nil :reader renderer-target-generation-composite-source-bind-group)
   (exposure-binding :initform nil :reader renderer-target-generation-exposure-binding)))

(defun %make-empty-renderer-target-generation ()
  (make-instance 'renderer-target-generation))

(defun renderer-extent (renderer)
  (renderer-target-generation-extent
   (renderer-target-generation renderer)))

(defun renderer-render-extent (renderer)
  (renderer-target-generation-render-extent
   (renderer-target-generation renderer)))

(defun renderer-temporal-scaler (renderer)
  (renderer-target-generation-temporal-scaler
   (renderer-target-generation renderer)))

(defun renderer-depth-texture (renderer)
  (renderer-target-generation-depth-texture
   (renderer-target-generation renderer)))

(defun renderer-depth-msaa-view (renderer)
  (renderer-target-generation-depth-msaa-view
   (renderer-target-generation renderer)))

(defun renderer-depth-view (renderer)
  (renderer-target-generation-depth-view
   (renderer-target-generation renderer)))

(defun renderer-scene-texture (renderer)
  (renderer-target-generation-scene-texture
   (renderer-target-generation renderer)))

(defun renderer-scene-msaa-view (renderer)
  (renderer-target-generation-scene-msaa-view
   (renderer-target-generation renderer)))

(defun renderer-scene-view (renderer)
  (renderer-target-generation-scene-view
   (renderer-target-generation renderer)))

(defun renderer-motion-texture (renderer)
  (renderer-target-generation-motion-texture
   (renderer-target-generation renderer)))

(defun renderer-motion-msaa-view (renderer)
  (renderer-target-generation-motion-msaa-view
   (renderer-target-generation renderer)))

(defun renderer-motion-view (renderer)
  (renderer-target-generation-motion-view
   (renderer-target-generation renderer)))

(defun renderer-resolved-texture (renderer)
  (renderer-target-generation-resolved-texture
   (renderer-target-generation renderer)))

(defun renderer-resolved-view (renderer)
  (renderer-target-generation-resolved-view
   (renderer-target-generation renderer)))

(defun renderer-history-texture (renderer)
  (renderer-target-generation-history-texture
   (renderer-target-generation renderer)))

(defun renderer-history-view (renderer)
  (renderer-target-generation-history-view
   (renderer-target-generation renderer)))

(defun renderer-composite-texture (renderer)
  (renderer-target-generation-composite-texture
   (renderer-target-generation renderer)))

(defun renderer-composite-view (renderer)
  (renderer-target-generation-composite-view
   (renderer-target-generation renderer)))

(defun renderer-composite-source-bind-group (renderer)
  (renderer-target-generation-composite-source-bind-group
   (renderer-target-generation renderer)))

(defun renderer-exposure-binding (renderer)
  (renderer-target-generation-exposure-binding
   (renderer-target-generation renderer)))

(defun destroy-renderer-target-generation (generation)
  (destroy generation))

(defun destroy-renderer-targets (renderer)
  "Retire the target owner, retaining it for retry if a resource release fails."
  (destroy-renderer-target-generation (renderer-target-generation renderer))
  (setf (renderer-target-generation renderer) (%make-empty-renderer-target-generation)))

(defun render-scale-extent (extent)
  "Return the even-sized internal render extent for output EXTENT."
  (mapcar (lambda (dimension)
            (max 2 (* 2 (round (* 0.5 *render-scale* dimension)))))
          extent))

(defun renderer-render-scale-extent (renderer extent)
  "Return RENDERER's internal extent for output EXTENT.

MetalFX performs temporal upscaling from *RENDER-SCALE*.  Luft's inspectable
Vulkan resolve is the original native-resolution TAA algorithm; it accumulates
subpixel samples but does not claim a stable reconstruction-upscaling filter."
  (if (renderer-shader-temporal-p renderer)
      (copy-list extent)
      (render-scale-extent extent)))

(defun make-renderer-target-generation (renderer extent)
  "Stage the images and bindings for one output size under one GPU owner."
  (let* ((device (renderer-device renderer))
         (extent (copy-list extent))
         (render-extent (renderer-render-scale-extent renderer extent))
         (generation (%make-empty-renderer-target-generation))
         (shader-p (renderer-shader-temporal-p renderer))
         (scaler nil))
    (with-gpu-construction (generation)
      (labels ((own (descriptor) (own-gpu-resource generation device descriptor))
               (usage (base extra) (remove-duplicates (append base extra)))
               (image (name format size usage &optional (samples 1))
                 ;; Every image has the same texture/view ownership relation.
                 ;; NAME selects its operational slots; custody is recorded by OWN.
                 (let* ((texture (own (make-texture-descriptor
                                      :label (format nil "luft ~A" name)
                                      :format format :size size :dimensions :2d
                                      :usage usage :sample-count samples)))
                        (view (own (make-texture-view-descriptor :texture texture))))
                   (setf (slot-value generation (intern (format nil "~A-TEXTURE" name) '#:luft.render)) texture
                         (slot-value generation (intern (format nil "~A-VIEW" name) '#:luft.render)) view))))
        (setf (slot-value generation 'extent) extent
              (slot-value generation 'render-extent) render-extent)
        (when (renderer-metalfx-temporal-p renderer)
          (setf scaler (own (make-temporal-scaler-descriptor
                             :label "luft MetalFX temporal scaler"
                             :input-size render-extent :output-size extent))
                (slot-value generation 'temporal-scaler) scaler))
        (image :depth :depth32-float render-extent
               (usage '(:render-attachment :texture-binding)
                      (and scaler (gpu-temporal-scaler-depth-usage scaler))))
        (image :depth-msaa :depth32-float render-extent :render-attachment *scene-sample-count*)
        (image :scene :rgba16-float render-extent
               (usage '(:render-attachment :texture-binding)
                      (and scaler (gpu-temporal-scaler-color-usage scaler))))
        (image :scene-msaa :rgba16-float render-extent :render-attachment *scene-sample-count*)
        (when (renderer-temporal-p renderer)
          (image :motion :rg16-float render-extent
                 (usage (if shader-p '(:render-attachment :texture-binding) '(:render-attachment))
                        (and scaler (gpu-temporal-scaler-motion-usage scaler))))
          (image :motion-msaa :rg16-float render-extent :render-attachment *scene-sample-count*)
          (image :resolved :rgba16-float extent
                 (usage (if shader-p '(:render-attachment :texture-binding :copy-src) '(:texture-binding))
                        (and scaler (gpu-temporal-scaler-output-usage scaler))))
          (when shader-p (image :history :rgba16-float extent '(:texture-binding :copy-dst))))
        (image :composite :rgba16-float extent '(:render-attachment :texture-binding))
        (setf (slot-value generation 'composite-source-bind-group)
              (own-gpu-object
               generation
               (make-composite-binding
                (renderer-finishing renderer) device
                (or (renderer-target-generation-resolved-view generation)
                    (renderer-target-generation-scene-view generation))))
              (slot-value generation 'exposure-binding)
              (own-gpu-object
               generation
               (make-exposure-binding (renderer-exposure-control renderer) device
                                      (renderer-target-generation-composite-view generation)
                                      (renderer-sampler renderer))))
        (when scaler
          ;; Once used, the native scaler may retain input images. Retire it
          ;; before those images; during failed construction it has not seen them.
          (setf (owned-gpu-resources generation)
                (cons scaler (remove scaler (owned-gpu-resources generation)))))))))

(defvar *renderer-target-generation-precommit-hook* nil
  "Test hook called with renderer and complete target candidate before swap.")

(defun replace-renderer-target-generation (renderer extent)
  "Atomically publish a complete generation, then retire the prior one."
  (let ((old-generation (renderer-target-generation renderer))
        (candidate nil)
        (installed-p nil))
    (unwind-protect
         (progn
           (setf candidate (make-renderer-target-generation renderer extent))
           (when *renderer-target-generation-precommit-hook*
             (funcall *renderer-target-generation-precommit-hook*
                      renderer candidate))
           ;; This is the sole target-identity publication write.  Temporal
           ;; history is invalidated only after the complete resource cohort
           ;; is visible, and the old resources remain live until afterward.
           (setf (renderer-target-generation renderer) candidate
                 installed-p t)
           (with-release-warnings
             (releasing :superseded-frame-bindings
               (clear-renderer-frame-bind-groups renderer)))
           (setf (renderer-previous-view renderer) nil
                 (renderer-history-valid-p renderer) nil
                 (renderer-history-used-p renderer) nil)
           (with-release-warnings
             (releasing :retired-targets (retire-gpu-object renderer old-generation)))
           candidate)
      (unless installed-p
        (when candidate
          (with-release-warnings
            (releasing :unpublished-targets (retire-gpu-object renderer candidate))))))))

(defun ensure-renderer-extent (renderer extent)
  (unless (equal extent (renderer-extent renderer))
    (replace-renderer-target-generation renderer extent))
  renderer)
