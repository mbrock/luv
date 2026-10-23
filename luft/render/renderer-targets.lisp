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
   (bounds-low-texture :initform nil :reader renderer-target-generation-bounds-low-texture)
   (bounds-low-view :initform nil :reader renderer-target-generation-bounds-low-view)
   (bounds-high-texture :initform nil :reader renderer-target-generation-bounds-high-texture)
   (bounds-high-view :initform nil :reader renderer-target-generation-bounds-high-view)
   (composite-texture :initform nil :reader renderer-target-generation-composite-texture)
   (composite-view :initform nil :reader renderer-target-generation-composite-view)
   (bloom-primary-texture :initform nil :reader renderer-target-generation-bloom-primary-texture)
   (bloom-primary-view :initform nil :reader renderer-target-generation-bloom-primary-view)
   (bloom-secondary-texture :initform nil :reader renderer-target-generation-bloom-secondary-texture)
   (bloom-secondary-view :initform nil :reader renderer-target-generation-bloom-secondary-view)
   (composite-source-bind-group :initform nil :reader renderer-target-generation-composite-source-bind-group)
   (history-composite-source-bind-group
    :initform nil :reader renderer-target-generation-history-composite-source-bind-group)
   (exposure-binding :initform nil :reader renderer-target-generation-exposure-binding)
   ;; Output-sized objects borrowed from the generation this one replaces,
   ;; whose custody passes to it only at publication.
   (adopted :initform nil :reader renderer-target-generation-adopted)))

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

;;; The shader resolve keeps two output-sized images and alternates them:
;;; each frame reads the one the last frame wrote and writes the other, so
;;; history never needs copying.  MetalFX writes RESOLVED and keeps its own.

(defun renderer-history-swapped-p (renderer)
  (and (renderer-shader-temporal-p renderer)
       (oddp (renderer-frame-index renderer))))

(defun renderer-resolved-texture (renderer)
  (let ((generation (renderer-target-generation renderer)))
    (if (renderer-history-swapped-p renderer)
        (renderer-target-generation-history-texture generation)
        (renderer-target-generation-resolved-texture generation))))

(defun renderer-resolved-view (renderer)
  (let ((generation (renderer-target-generation renderer)))
    (if (renderer-history-swapped-p renderer)
        (renderer-target-generation-history-view generation)
        (renderer-target-generation-resolved-view generation))))

(defun renderer-history-texture (renderer)
  (let ((generation (renderer-target-generation renderer)))
    (if (renderer-history-swapped-p renderer)
        (renderer-target-generation-resolved-texture generation)
        (renderer-target-generation-history-texture generation))))

(defun renderer-history-view (renderer)
  (let ((generation (renderer-target-generation renderer)))
    (if (renderer-history-swapped-p renderer)
        (renderer-target-generation-resolved-view generation)
        (renderer-target-generation-history-view generation))))

(defun renderer-bounds-low-view (renderer)
  (renderer-target-generation-bounds-low-view
   (renderer-target-generation renderer)))

(defun renderer-bounds-low-texture (renderer)
  (renderer-target-generation-bounds-low-texture
   (renderer-target-generation renderer)))

(defun renderer-bounds-high-view (renderer)
  (renderer-target-generation-bounds-high-view
   (renderer-target-generation renderer)))

(defun renderer-bounds-high-texture (renderer)
  (renderer-target-generation-bounds-high-texture
   (renderer-target-generation renderer)))

(defun renderer-composite-texture (renderer)
  (renderer-target-generation-composite-texture
   (renderer-target-generation renderer)))

(defun renderer-composite-view (renderer)
  (renderer-target-generation-composite-view
   (renderer-target-generation renderer)))

;;; The lens chain ping-pongs between two reduced images: the bright pass and
;;; each vertical blur leave the bloom in the primary, and the sun shafts
;;; finish in the secondary.

(defun renderer-bloom-primary-texture (renderer)
  (renderer-target-generation-bloom-primary-texture
   (renderer-target-generation renderer)))

(defun renderer-bloom-primary-view (renderer)
  (renderer-target-generation-bloom-primary-view
   (renderer-target-generation renderer)))

(defun renderer-bloom-secondary-texture (renderer)
  (renderer-target-generation-bloom-secondary-texture
   (renderer-target-generation renderer)))

(defun renderer-bloom-secondary-view (renderer)
  (renderer-target-generation-bloom-secondary-view
   (renderer-target-generation renderer)))

(defun renderer-composite-source-bind-group (renderer)
  (let ((generation (renderer-target-generation renderer)))
    (if (renderer-history-swapped-p renderer)
        (renderer-target-generation-history-composite-source-bind-group generation)
        (renderer-target-generation-composite-source-bind-group generation))))

(defun renderer-exposure-binding (renderer)
  (renderer-target-generation-exposure-binding
   (renderer-target-generation renderer)))

(defun destroy-renderer-target-generation (generation)
  (destroy generation))

(defun destroy-renderer-targets (renderer)
  "Retire the target owner, retaining it for retry if a resource release fails."
  (destroy-renderer-target-generation (renderer-target-generation renderer))
  (setf (renderer-target-generation renderer) (%make-empty-renderer-target-generation)))

(defun render-scale-extent (extent &optional (share 1.0))
  "Return the even-sized internal render extent for output EXTENT.

SHARE is the part of *RENDER-SCALE* dynamic resolution currently draws."
  (mapcar (lambda (dimension)
            (max 2 (* 2 (round (* 0.5 *render-scale* share dimension)))))
          extent))

(defun renderer-render-scale-extent (renderer extent)
  "Return RENDERER's internal extent for output EXTENT.

Both temporal implementations reconstruct the output from the scene;
without one, presentation enlarges it directly."
  (if *fixed-render-scale*
      (mapcar (lambda (dimension)
                (max 1 (round (* dimension *fixed-render-scale*))))
              extent)
      (render-scale-extent extent (renderer-scene-share renderer))))

(defparameter *renderer-output-images*
  '(:resolved :history :composite :bloom-primary :bloom-secondary)
  "The images sized by the output, which a scene-only resize keeps.")

(defun make-renderer-target-generation (renderer extent &optional previous)
  "Stage the images and bindings for one output size under one GPU owner.

When PREVIOUS has the same output extent, its output-sized images (and the
temporal history in them) are borrowed rather than rebuilt: only the scene
changed size.  MetalFX's scaler is fixed to its input size, so it always
starts over."
  (let* ((device (renderer-device renderer))
         (extent (copy-list extent))
         (render-extent (renderer-render-scale-extent renderer extent))
         (generation (%make-empty-renderer-target-generation))
         (shader-p (renderer-shader-temporal-p renderer))
         (samples (renderer-scene-sample-count renderer))
         (reuse (and previous
                     (not (renderer-metalfx-temporal-p renderer))
                     (equal extent (renderer-target-generation-extent previous))
                     previous))
         (scaler nil))
    (with-gpu-construction (generation)
      (labels ((own (descriptor) (own-gpu-resource generation device descriptor))
               (slot (name kind)
                 (intern (format nil "~A-~A" name kind) '#:luft.render))
               (adopt (name)
                 (dolist (kind '("TEXTURE" "VIEW"))
                   (let ((object (slot-value reuse (slot name kind))))
                     (setf (slot-value generation (slot name kind)) object)
                     (push object (slot-value generation 'adopted)))))
               (usage (base extra) (remove-duplicates (append base extra)))
               (image (name format size usage &optional (samples 1))
                 ;; Every image has the same texture/view ownership relation.
                 ;; NAME selects its operational slots; custody is recorded by OWN.
                 (when (and reuse (member name *renderer-output-images*))
                   (return-from image (adopt name)))
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
        (when (> samples 1)
          (image :depth-msaa :depth32-float render-extent :render-attachment samples))
        (image :scene :rgba16-float render-extent
               (usage '(:render-attachment :texture-binding)
                      (and scaler (gpu-temporal-scaler-color-usage scaler))))
        (when (> samples 1)
          (image :scene-msaa :rgba16-float render-extent :render-attachment samples))
        (when (renderer-temporal-p renderer)
          (image :motion :rg16-float render-extent
                 (usage (if shader-p '(:render-attachment :texture-binding) '(:render-attachment))
                        (and scaler (gpu-temporal-scaler-motion-usage scaler))))
          ;; RESOLVED and HISTORY trade roles each frame, and history is
          ;; cleared before its first use, a transfer on Vulkan.
          (image :resolved :rgba16-float extent
                 (usage (if shader-p
                            '(:render-attachment :texture-binding :copy-dst)
                            '(:texture-binding))
                        (and scaler (gpu-temporal-scaler-output-usage scaler))))
          (when shader-p
            (image :history :rgba16-float extent
                   '(:render-attachment :texture-binding :copy-dst))
            (image :bounds-low :rgba16-float render-extent '(:render-attachment :texture-binding))
            (image :bounds-high :rgba16-float render-extent '(:render-attachment :texture-binding))))
        (image :composite :rgba16-float extent '(:render-attachment :texture-binding))
        (image :bloom-primary :rgba16-float (bloom-extent extent)
               '(:render-attachment :texture-binding))
        (image :bloom-secondary :rgba16-float (bloom-extent extent)
               '(:render-attachment :texture-binding))
        (setf (slot-value generation 'composite-source-bind-group)
              (own-gpu-object
               generation
               (make-composite-binding
                (renderer-finishing renderer) device
                (or (renderer-target-generation-resolved-view generation)
                    (renderer-target-generation-scene-view generation))))
              (slot-value generation 'history-composite-source-bind-group)
              (and shader-p
                   (own-gpu-object
                    generation
                    (make-composite-binding
                     (renderer-finishing renderer) device
                     (renderer-target-generation-history-view generation))))
              (slot-value generation 'exposure-binding)
              (if reuse
                  (let ((binding (renderer-target-generation-exposure-binding reuse)))
                    (push binding (slot-value generation 'adopted))
                    binding)
                  (own-gpu-object
                   generation
                   (make-exposure-binding (renderer-exposure-control renderer) device
                                          (renderer-target-generation-composite-view generation)
                                          (renderer-sampler renderer)))))
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
           (setf candidate (make-renderer-target-generation
                            renderer extent old-generation))
           (when *renderer-target-generation-precommit-hook*
             (funcall *renderer-target-generation-precommit-hook*
                      renderer candidate))
           ;; This is the sole target-identity publication write.  Temporal
           ;; history is invalidated only after the complete resource cohort
           ;; is visible, and the old resources remain live until afterward.
           (setf (renderer-target-generation renderer) candidate
                 installed-p t)
           ;; Custody of borrowed output images passes with publication.  They
           ;; go last in the candidate's list: its newer bindings name them and
           ;; must be released first.
           (let ((adopted (renderer-target-generation-adopted candidate)))
             (when adopted
               (setf (owned-gpu-resources candidate)
                     (append (owned-gpu-resources candidate)
                             (remove-if-not (lambda (object) (member object adopted))
                                            (owned-gpu-resources old-generation)))
                     (owned-gpu-resources old-generation)
                     (remove-if (lambda (object) (member object adopted))
                                (owned-gpu-resources old-generation)))))
           (with-release-warnings
             (releasing :superseded-frame-bindings
               (clear-renderer-frame-bind-groups renderer)))
           ;; A scene-only resize keeps the output images and so the history
           ;; in them; it lives on the output grid and only the sample count
           ;; behind each output pixel changed.
           (unless (renderer-target-generation-adopted candidate)
             (setf (renderer-previous-view renderer) nil
                   (renderer-history-valid-p renderer) nil
                   (renderer-history-used-p renderer) nil))
           (with-release-warnings
             (releasing :retired-targets (retire-gpu-object renderer old-generation)))
           candidate)
      (unless installed-p
        (when candidate
          (with-release-warnings
            (releasing :unpublished-targets (retire-gpu-object renderer candidate))))))))

(defun ensure-renderer-extent (renderer extent)
  (unless (and (equal extent (renderer-extent renderer))
               (equal (renderer-render-scale-extent renderer extent)
                      (renderer-render-extent renderer)))
    (replace-renderer-target-generation renderer extent))
  renderer)
