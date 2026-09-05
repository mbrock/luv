(in-package #:luft.render)

;;; Presentation-slot uploads and bindings: reuse only after the canvas fence.
;;; Each binding borrows current component, residency, and target inputs.

(defstruct (renderer-frame-state
             (:constructor %make-renderer-frame-state
                 (&key camera-buffer flame-effect-buffer)))
  "Mutable uploads and dependent bindings local to one presentation slot."
  camera-buffer
  flame-effect-buffer
  (bind-groups (make-hash-table :test #'equal)))

(defun make-renderer-frame-state (renderer)
  "Allocate one complete mutable upload cohort for RENDERER."
  (let ((camera nil)
        (effect nil)
        (completed-p nil))
    (unwind-protect
         (progn
           (setf camera
                 (create
                  (renderer-device renderer)
                  (make-buffer-descriptor
                   :label "luft presentation-slot camera state"
                   :size (shaders::scene-uniform-byte-size)
                   :usage '(:uniform :copy-dst)))
                 effect (make-torch-frame-buffer
                         (renderer-torches renderer) (renderer-device renderer)))
           (setf completed-p t)
           (%make-renderer-frame-state
            :camera-buffer camera :flame-effect-buffer effect))
      (unless completed-p
        (when effect (ignore-errors (destroy effect)))
        (when camera (ignore-errors (destroy camera)))))))

(defun destroy-renderer-frame-state (state)
  "Retain failed releases in STATE so its presentation cache can retry them."
  (with-release-report
    (let ((groups (renderer-frame-state-bind-groups state)))
      (dolist (key (loop for key being the hash-keys of groups collect key))
        (releasing :frame-bind-group
          (when (gethash key groups) (destroy (gethash key groups)))
          (remhash key groups))))
    (when (renderer-frame-state-flame-effect-buffer state)
      (releasing :frame-flame-effect-buffer
        (destroy (renderer-frame-state-flame-effect-buffer state))
        (setf (renderer-frame-state-flame-effect-buffer state) nil)))
    (when (renderer-frame-state-camera-buffer state)
      (releasing :frame-camera-buffer
        (destroy (renderer-frame-state-camera-buffer state))
        (setf (renderer-frame-state-camera-buffer state) nil))))
  (values))

(defun clear-renderer-frame-bind-groups (renderer)
  "Drop bindings derived from a superseded target or scene generation."
  (map-canvas-frame-resources
   (lambda (state key)
     (declare (ignore key))
     (let ((groups (renderer-frame-state-bind-groups state)))
       (with-release-report
         (dolist (binding-key
                   (loop for key being the hash-keys of groups collect key))
           (releasing (list :frame-bind-group binding-key)
             (when (gethash binding-key groups)
               (destroy (gethash binding-key groups)))
             (remhash binding-key groups))))))
   (renderer-frame-resources renderer))
  renderer)

(defun renderer-frame-program-binding (renderer frame key program &rest inputs)
  (renderer-frame-component-binding
   frame key (lambda () (apply #'make-program-binding program (renderer-device renderer) inputs))))

(defun renderer-frame-state-for (renderer context surface-texture)
  "Acquire RENDERER's safely reusable mutable state for SURFACE-TEXTURE."
  (canvas-frame-resource
   (renderer-frame-resources renderer) context surface-texture
   (lambda (key surface)
     (declare (ignore key surface))
     (make-renderer-frame-state renderer))))

(defun renderer-frame-resident-bind-group (renderer frame resident shadow-p)
  "Bind one immutable resident population to FRAME's camera upload."
  (renderer-frame-component-binding
   frame (list :resident resident shadow-p (renderer-shadow-view renderer))
   (lambda ()
     (make-terrain-binding
      (renderer-terrain renderer) (renderer-device renderer)
      (resident-population-instance-buffer resident) (renderer-frame-state-camera-buffer frame)
      :appearances (resident-population-appearance-buffer resident)
      :descriptors (resident-population-descriptor-buffer resident)
      :shadow-map (renderer-shadow-view renderer) :shadow-sampler (renderer-shadow-sampler renderer)
      :shadow-p shadow-p))))

(defun renderer-frame-component-binding (frame key make-binding)
  "Cache even a component's NIL binding; the presentation slot owns the result."
  (let ((groups (renderer-frame-state-bind-groups frame)))
    (multiple-value-bind (binding present-p) (gethash key groups)
      (if present-p binding
          (setf (gethash key groups) (funcall make-binding))))))

(defun renderer-frame-torch-body-bind-group (renderer frame shadow-p)
  (renderer-frame-component-binding
   frame (list :torch-body shadow-p (renderer-flame-instance-buffer renderer))
   (lambda ()
     (make-torch-body-binding
      (renderer-torches renderer) (renderer-device renderer)
      (renderer-flame-instance-buffer renderer)
      (renderer-frame-state-camera-buffer frame)
      (renderer-shadow-view renderer) (renderer-shadow-sampler renderer)
      :shadow-p shadow-p))))

(defun renderer-frame-drawing-binding (renderer frame drawing)
  "The frame owns borrowed camera/shadow bindings; DRAWING owns its program."
  (when drawing
    (renderer-frame-component-binding
     frame (list :scene-drawing drawing (renderer-shadow-view renderer))
     (lambda ()
       (make-scene-drawing-binding
        drawing (renderer-device renderer)
        (renderer-frame-state-camera-buffer frame)
        (renderer-shadow-view renderer) (renderer-shadow-sampler renderer))))))

(defun renderer-frame-lattice-bind-group (renderer frame slot)
  (renderer-frame-program-binding
   renderer frame (list :lattice slot (mesh-slot-lattice-point-buffer slot))
   (renderer-lattice renderer) :lattice-points (mesh-slot-lattice-point-buffer slot)
   :camera-state (renderer-frame-state-camera-buffer frame)))

(defun renderer-frame-temporal-bind-group (renderer frame)
  (renderer-frame-component-binding
   frame (list :temporal (renderer-scene-view renderer)
               (renderer-motion-view renderer) (renderer-history-view renderer))
   (lambda ()
     (make-reconstruction-binding
      (renderer-reconstruction renderer) (renderer-device renderer)
      (renderer-scene-view renderer) (renderer-motion-view renderer) (renderer-history-view renderer)
      (renderer-sampler renderer) (renderer-frame-state-camera-buffer frame)))))

(defun renderer-frame-flame-bind-group (renderer frame)
  (renderer-frame-component-binding
   frame (list :flame (renderer-flame-instance-buffer renderer)
               (renderer-depth-view renderer))
   (lambda ()
     (make-torch-flame-binding
      (renderer-torches renderer) (renderer-device renderer)
      (renderer-flame-instance-buffer renderer)
      (renderer-frame-state-camera-buffer frame)
      (renderer-frame-state-flame-effect-buffer frame)
      (renderer-depth-view renderer)))))

(defun renderer-frame-present-bind-group (renderer frame)
  (renderer-frame-component-binding
   frame (list :present (renderer-composite-view renderer) (renderer-depth-view renderer))
   (lambda ()
     (make-presentation-binding
      (renderer-finishing renderer) (renderer-device renderer)
      (renderer-composite-view renderer) (renderer-depth-view renderer)
      (renderer-frame-state-camera-buffer frame)))))

(defun draw-resident-opaque-population (terrain pass resident bind-group &key shadow-p)
  "Dispatch one direct mesh workgroup per active lattice site."
  (encode-terrain terrain pass bind-group
                  (render-population-mesh-workgroup-count (resident-population-population resident))
                  :shadow-p shadow-p))
