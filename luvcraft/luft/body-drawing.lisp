(in-package #:luvcraft.luft.game)

;;; Held items keep their original vertex stream, atlas and block shader.
;;; No replacement ball model: MAP-HAND-ITEM-BOXES remains the authority.
(defun viewer-body-visible-p (viewer)
  (let ((focus (render:viewer-modal-focus viewer)))
    (or (typep focus 'luvcraft::critter-ride)
        (and (null focus) (typep (render:viewer-mode viewer) 'render:first-person-mode)))))

(defclass held-item-drawing (render::gpu-resource-owner)
  ((pipeline :accessor held-item-pipeline)
   (layout :accessor held-item-layout)
   (atlas :accessor held-item-atlas)
   (normal :accessor held-item-normal)
   (sampler :accessor held-item-sampler)
   (comparison :accessor held-item-comparison)
   (frames :initform (luv:make-canvas-frame-resource-cache) :reader held-item-frames)))

(defmethod render:viewer-overlay-stage ((drawing held-item-drawing)) :viewmodel)

(defmethod render:attach-viewer-overlay ((drawing held-item-drawing) (viewer luvcraft-viewer))
  (let ((device (render:viewer-device viewer)))
    (render::with-gpu-construction (drawing)
      (flet ((own (descriptor) (render::own-gpu-resource drawing device descriptor)))
        (setf (held-item-layout drawing)
              (own (luv:make-bind-group-layout-descriptor
                    :entries '((:binding 0 :type :texture) (:binding 1 :type :sampler)
                               (:binding 2 :type :uniform-buffer) (:binding 3 :type :texture)
                               (:binding 4 :type :sampler) (:binding 5 :type :sampler)
                               (:binding 6 :type :texture))))
              (held-item-sampler drawing)
              (own (luv:make-sampler-descriptor :min-filter :nearest :mag-filter :nearest))
              (held-item-comparison drawing)
              (own (luv:make-sampler-descriptor :compare :less-or-equal)))
        (loop for pixels in (list (luvcraft::make-block-texture-atlas)
                                  (luvcraft::make-block-normal-atlas))
              for slot in '(atlas normal)
              for format in (list luvcraft::+block-atlas-texture-format+
                                   luvcraft::+block-normal-atlas-texture-format+)
              do (let* ((width (array-dimension pixels 1)) (height (array-dimension pixels 0))
                        (texture (own (luv:make-texture-descriptor
                                       :size (list width height) :dimensions :2d :format format
                                       :usage '(:copy-dst :texture-binding))))
                        (view (own (luv:make-texture-view-descriptor :texture texture))))
                   (luv:write-texture
                    (luv:device-queue device) (luv:make-texture-copy :texture texture) pixels
                    (luv:make-texture-data-layout :bytes-per-row (* width 4) :rows-per-image height)
                    (list width height))
                   (setf (slot-value drawing slot) view)))
        (setf (held-item-pipeline drawing)
              (luv:make-live-shader-pipeline
               :device device :role :block-surface :vertex-role :block-surface
               :layout (held-item-layout drawing) :label "original held-item geometry on Luft"
               :vertex-buffers
               '((:array-stride 56
                  :attributes ((:shader-location 0 :offset 0 :format :float32x3)
                               (:shader-location 1 :offset 12 :format :float32x3)
                               (:shader-location 2 :offset 24 :format :float32x3)
                               (:shader-location 3 :offset 36 :format :float32x3)
                               (:shader-location 4 :offset 48 :format :float32x2))))
               :target-format :rgba16-float :primitive '(:topology :triangle-list)
               :depth-stencil '(:format :depth32-float
                                :depth-write-enabled t :depth-compare :less)))))))

(defun release-held-item-frame (frame)
  (dolist (resource frame) (luv:destroy resource)))

(defmethod render:evict-viewer-overlay-frame-key ((drawing held-item-drawing) key)
  (luv:evict-canvas-frame-resource-key (held-item-frames drawing) key #'release-held-item-frame))

(defmethod render:release-viewer-overlay ((drawing held-item-drawing))
  (luv:destroy-canvas-frame-resource-cache (held-item-frames drawing) #'release-held-item-frame)
  (when (slot-boundp drawing 'pipeline)
    (luv:release-live-shader-pipeline (held-item-pipeline drawing)))
  (luv:destroy drawing))

(defmethod render:refresh-viewer-overlay ((drawing held-item-drawing) (viewer luvcraft-viewer))
  (luv:refresh-live-shader-pipeline (held-item-pipeline drawing)))

(defmethod render:encode-viewer-overlay
    ((drawing held-item-drawing) (viewer luvcraft-viewer) pass surface)
  (when (and (viewer-body-visible-p viewer)
             (luvcraft::player-body-hand-item (viewer-body viewer)))
    (luvcraft.agent::sdf-agent-scene-bind-group viewer surface)
    (let* ((state (companion-presentation viewer))
           (uniforms (second (luv:canvas-frame-resource
                              (presentation-frames state) (render:viewer-context viewer) surface
                              (lambda (&rest ignored) (declare (ignore ignored)) (error "Missing frame")))))
           (frame (luv:canvas-frame-resource
                   (held-item-frames drawing) (render:viewer-context viewer) surface
                   (lambda (key target)
                     (declare (ignore key target))
                     (let* ((device (render:viewer-device viewer))
                            (buffer (luv:create device (luv:make-buffer-descriptor
                                                        :size luvcraft::+player-body-buffer-size+
                                                        :usage '(:vertex :copy-dst))))
                            (group (handler-case
                                       (luv:create
                                        device
                                        (luv:make-bind-group-descriptor
                                         :layout (held-item-layout drawing)
                                         :entries
                                         `((:binding 0 :resource ,(held-item-atlas drawing))
                                           (:binding 1 :resource ,(held-item-sampler drawing))
                                           (:binding 2 :resource ,uniforms)
                                           (:binding 3 :resource ,(render::renderer-shadow-view (render:viewer-renderer viewer)))
                                           (:binding 4 :resource ,(held-item-sampler drawing))
                                           (:binding 5 :resource ,(held-item-comparison drawing))
                                           (:binding 6 :resource ,(held-item-normal drawing)))))
                                     (error (condition)
                                       (luv:destroy buffer)
                                       (error condition)))))
                       (list group buffer)))))
           (vertices (luvcraft::player-body-vertices viewer)))
      (luv:write-buffer (second frame) vertices)
      (luv:set-pipeline pass (luv:live-shader-pipeline-native-pipeline (held-item-pipeline drawing)))
      (luv:set-bind-group pass 0 (first frame))
      (luv:set-vertex-buffer pass 0 (second frame))
      (luv:draw pass (/ (length vertices) luvcraft::+block-mesh-floats-per-vertex+) 1))))
