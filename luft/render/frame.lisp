(in-package #:luft.render)

;;; A frame is an ordered composition. Shadows feed scene lighting; temporal
;;; reconstruction consumes geometry depth and motion; flames enter the HDR
;;; image afterward; exposure measures that image; presentation grades it and
;;; places the application overlay last. Helpers below follow that same order.
;;; Resource ownership and replacement live in render.lisp. See #I4PRMD.

(defun encode-renderer-frame
    (renderer frame encoder surface-texture extent camera-uniform-data
     &key jitter view player-p construction-p overlay-encoder
       (effect-time
         (or *flame-time* (/ (renderer-frame-index renderer) 60.0))))
  (ensure-renderer-extent renderer extent)
  (upload-renderer-frame renderer frame camera-uniform-data effect-time)
  (encode-renderer-shadows renderer frame encoder)
  (encode-renderer-scene renderer frame encoder player-p construction-p)
  (encode-renderer-reconstruction renderer frame encoder jitter view)
  (encode-renderer-composite renderer frame encoder)
  (encode-exposure (renderer-exposure-control renderer) encoder
                   (renderer-exposure-binding renderer)
                   (renderer-frame-index renderer))
  (encode-renderer-presentation renderer frame encoder surface-texture overlay-encoder)
  (incf (renderer-frame-index renderer))
  renderer)

(defun upload-renderer-frame (renderer frame camera-uniform-data effect-time)
  (when (renderer-shader-temporal-p renderer)
    ;; These W components are padding to every geometry consumer.  The Vulkan
    ;; resolve reads them as its per-frame validity and accumulation weight.
    (setf (aref camera-uniform-data 27)
          (if (renderer-history-valid-p renderer) 1.0f0 0.0f0)
          (aref camera-uniform-data 31) *vulkan-temporal-history-weight*))
  (write-buffer (renderer-frame-state-camera-buffer frame) camera-uniform-data)
  (check-type effect-time real)
  (write-buffer
   (renderer-frame-state-flame-effect-buffer frame)
   (torch-flame-effect-uniform-data (coerce effect-time 'single-float))))

(defun encode-renderer-shadows (renderer frame encoder)
  (let ((shadow-pass
          (begin-render-pass
           encoder
           (make-render-pass-descriptor
            :label "luft sun shadow"
            :color-attachments nil
            :depth-stencil-attachment
            `(:view ,(renderer-shadow-view renderer)
              :depth-load-op :clear :depth-store-op :store
              :depth-clear-value 1.0)))))
    (set-pipeline shadow-pass (renderer-shadow-pipeline renderer))
    (dolist (key (renderer-slot-order renderer))
      (let ((resident
              (mesh-slot-resident
               (gethash key (renderer-mesh-slots renderer)))))
        (draw-resident-opaque-population
         shadow-pass resident
         (renderer-frame-resident-bind-group
          renderer frame resident t))))
    (when (plusp (renderer-flame-instance-count renderer))
      (set-pipeline shadow-pass
                    (renderer-torch-body-shadow-pipeline renderer))
      (set-bind-group shadow-pass 0
                      (renderer-frame-torch-body-bind-group
                       renderer frame t))
      (draw shadow-pass (torch-body-vertex-count)
            (renderer-flame-instance-count renderer)))
    (end-pass shadow-pass))
  (prepare-texture encoder (renderer-shadow-texture renderer)
                   :texture-binding))

(defun encode-renderer-scene (renderer frame encoder player-p construction-p)
  (let* ((temporal-p (renderer-temporal-p renderer))
         (color-view (renderer-scene-msaa-view renderer))
         (color-attachments
           (if temporal-p
               `((:view ,color-view
                  :resolve-view ,(renderer-scene-view renderer)
                  :load-op :clear :store-op :store
                  :clear-value #(0.0 0.0 0.0 1.0))
                 (:view ,(renderer-motion-msaa-view renderer)
                  :resolve-view ,(renderer-motion-view renderer)
                  :load-op :clear :store-op :store
                  :clear-value #(0.0 0.0 0.0 0.0)))
               `((:view ,color-view
                  :resolve-view ,(renderer-scene-view renderer)
                  :load-op :clear :store-op :store
                  :clear-value #(0.0 0.0 0.0 1.0)))))
         (pass
           (begin-render-pass
            encoder
            (make-render-pass-descriptor
             :label "luft site streams"
             :color-attachments color-attachments
             :depth-stencil-attachment
             `(:view ,(renderer-depth-msaa-view renderer)
               :resolve-view ,(renderer-depth-view renderer)
               :depth-load-op :clear
               :depth-store-op :store
               :depth-clear-value 1.0)))))
    ;; The atmosphere is scene-linear world radiance: geometry overwrites it,
    ;; the selected temporal implementation reconstructs it, and the exposure
    ;; probe meters the same pixels presentation will grade.
    (set-pipeline pass (renderer-sky-pipeline renderer))
    (set-bind-group pass 0 (renderer-frame-sky-bind-group renderer frame))
    (draw pass 3)
    (set-pipeline pass (renderer-pipeline renderer))
    (dolist (key (renderer-slot-order renderer))
      (let ((resident
              (mesh-slot-resident
               (gethash key (renderer-mesh-slots renderer)))))
        (draw-resident-opaque-population
         pass resident
         (renderer-frame-resident-bind-group renderer frame resident nil))))
    (when (plusp (renderer-flame-instance-count renderer))
      (set-pipeline pass (renderer-torch-body-pipeline renderer))
      (set-bind-group pass 0
                      (renderer-frame-torch-body-bind-group renderer frame nil))
      (draw pass (torch-body-vertex-count)
            (renderer-flame-instance-count renderer)))
    (when player-p
      (set-pipeline pass (renderer-player-sdf-pipeline renderer))
      (set-bind-group pass 0 (renderer-frame-player-bind-group renderer frame))
      (draw pass 6 1))
    (when construction-p
      ;; Populate at most one diagnostic slot per frame. The overlay is a
      ;; debugging view, so progressive readiness is preferable to freezing
      ;; one frame while every resident chunk is scanned.
      (loop for key in (renderer-slot-order renderer)
            for slot = (gethash key (renderer-mesh-slots renderer))
            unless (mesh-slot-lattice-point-buffer slot)
              do (ensure-mesh-slot-lattice-points renderer slot)
                 (return))
      (set-pipeline pass (renderer-lattice-point-pipeline renderer))
      (dolist (key (renderer-slot-order renderer))
        (let ((slot (gethash key (renderer-mesh-slots renderer))))
          (when (plusp (mesh-slot-lattice-point-count slot))
            (set-bind-group pass 0
                            (renderer-frame-lattice-bind-group
                             renderer frame slot))
            (draw pass 6 (mesh-slot-lattice-point-count slot))))))
    (when (renderer-metalfx-temporal-p renderer)
      (signal-temporal-scaler-inputs pass
                                     (renderer-temporal-scaler renderer)))
    (end-pass pass)))

(defun encode-renderer-reconstruction (renderer frame encoder jitter view)
  (when (renderer-metalfx-temporal-p renderer)
    (let ((scaler (renderer-temporal-scaler renderer))
          (history-valid-p (renderer-history-valid-p renderer))
          (render-extent (renderer-render-extent renderer)))
      (encode-temporal-scale
       encoder scaler
       (renderer-scene-texture renderer)
       (renderer-depth-texture renderer)
       (renderer-motion-texture renderer)
       (renderer-resolved-texture renderer)
       ;; JITTER is clip-space at the internal scene resolution.  MetalFX
       ;; takes the same offset in input pixels—not output pixels—so using
       ;; EXTENT here overstates it whenever temporal upscaling is active.
       (vector (* 0.5 (first render-extent) (aref jitter 0))
               (* 0.5 (second render-extent) (aref jitter 1)))
       (not history-valid-p))
      (setf (renderer-previous-view renderer) view
            (renderer-history-valid-p renderer) t
            (renderer-history-used-p renderer) history-valid-p)))
  (when (renderer-shader-temporal-p renderer)
    (let ((history-valid-p (renderer-history-valid-p renderer)))
      (prepare-texture encoder (renderer-scene-texture renderer)
                       :texture-binding)
      (prepare-texture encoder (renderer-motion-texture renderer)
                       :texture-binding)
      (unless history-valid-p
        (encode encoder
                (make-gpu-clear-texture-command
                 :texture (renderer-history-texture renderer)
                 :color #(0.0 0.0 0.0 0.0))))
      (prepare-texture encoder (renderer-history-texture renderer)
                       :texture-binding)
      (let ((resolve-pass
              (begin-render-pass
               encoder
               (make-render-pass-descriptor
                :label "luft temporal resolve"
                :color-attachments
                `((:view ,(renderer-resolved-view renderer)
                   :load-op :clear :store-op :store
                   :clear-value #(0.0 0.0 0.0 1.0)))))))
        (set-pipeline resolve-pass (renderer-temporal-pipeline renderer))
        (set-bind-group resolve-pass 0
                        (renderer-frame-temporal-bind-group renderer frame))
        (draw resolve-pass 3)
        (end-pass resolve-pass))
      ;; One explicit full-resolution history keeps the extent cohort small:
      ;; resolve never reads and writes the same image, and the completed
      ;; result becomes next frame's input only after the render pass ends.
      (encode encoder
              (make-gpu-copy-texture-command
               :source (renderer-resolved-texture renderer)
               :destination (renderer-history-texture renderer)))
      (prepare-texture encoder (renderer-resolved-texture renderer)
                       :texture-binding)
      (prepare-texture encoder (renderer-history-texture renderer)
                       :texture-binding)
      (setf (renderer-previous-view renderer) view
            (renderer-history-valid-p renderer) t
            (renderer-history-used-p renderer) history-valid-p)))
  (unless (renderer-temporal-p renderer)
    (prepare-texture encoder (renderer-scene-texture renderer)
                     :texture-binding)))

(defun encode-renderer-composite (renderer frame encoder)
  ;; Depth is read only after both the scene pass and the temporal encoder
  ;; have consumed it.  It is deliberately not attached to the following
  ;; pass, so sampling it while writing the distinct HDR composite cannot
  ;; form a Metal or Vulkan read/write texture hazard.
  (prepare-texture encoder (renderer-depth-texture renderer)
                   :texture-binding)
  (let ((composite-pass
          (begin-render-pass
           encoder
           (make-render-pass-descriptor
            :label "luft post-temporal HDR flame composite"
            :color-attachments
            `((:view ,(renderer-composite-view renderer)
               :load-op :clear :store-op :store
               :clear-value #(0.0 0.0 0.0 1.0)))))))
    (when (renderer-metalfx-temporal-p renderer)
      (wait-temporal-scaler-output
       composite-pass (renderer-temporal-scaler renderer)))
    (set-pipeline composite-pass (renderer-composite-pipeline renderer))
    (set-bind-group composite-pass 0
                    (renderer-composite-source-bind-group renderer))
    (draw composite-pass 3)
    (when (plusp (renderer-flame-instance-count renderer))
      (set-pipeline composite-pass (renderer-flame-pipeline renderer))
      (set-bind-group composite-pass 0
                      (renderer-frame-flame-bind-group renderer frame))
      (draw composite-pass 6 (renderer-flame-instance-count renderer)))
    (end-pass composite-pass))
  (prepare-texture encoder (renderer-composite-texture renderer)
                   :texture-binding))

(defun encode-renderer-presentation
    (renderer frame encoder surface-texture overlay-encoder)
  (let ((present-pass
          (begin-render-pass
           encoder
           (make-render-pass-descriptor
            :label "luft HDR presentation"
            :color-attachments
            `((:view ,surface-texture :load-op :clear :store-op :store
               :clear-value #(0.0 0.0 0.0 1.0)))))))
    (set-pipeline present-pass (renderer-present-pipeline renderer))
    (set-bind-group present-pass 0
                    (renderer-frame-present-bind-group renderer frame))
    (draw present-pass 3)
    ;; Both MetalFX and the direct HDR path publish here.  The atelier
    ;; overlay remains later than tone mapping and glow in either case.
    (when overlay-encoder
      (funcall overlay-encoder present-pass))
    (end-pass present-pass)))
