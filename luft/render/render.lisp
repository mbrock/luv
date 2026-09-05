(in-package #:luft.render)

;;; Start here: the renderer composes owners, and frame.lisp gives their
;;; execution order. Components own programs; targets own resized images;
;;; residency owns published geometry; presentation slots own mutable uploads
;;; and bindings. No numeric shader bindings or GPU teardown inventory lives
;;; in this composition. Source details follow their corresponding interfaces.

(defclass renderer (gpu-resource-owner)
  ((device :initarg :device :reader renderer-device)
   (component-options :initarg :component-options :reader %renderer-component-options)
   (terrain :initform nil :accessor renderer-terrain)
   (sun-shadow :initform nil :accessor renderer-sun-shadow)
   (lattice :initform nil :accessor renderer-lattice)
   (sky :initform nil :accessor renderer-sky)
   (player :initform nil :accessor renderer-player)
   (torches :initform nil :accessor renderer-torches)
   (reconstruction :initform nil :accessor renderer-reconstruction)
   (finishing :initform nil :accessor renderer-finishing)
   (exposure-control :initform nil :accessor renderer-exposure-control)
   ;; These three identities have independent replacement clocks. Bindings
   ;; retire before the component programs and inputs they borrow.
   (publication :initform (%make-empty-renderer-publication) :accessor renderer-publication)
   (target-generation :initform (%make-empty-renderer-target-generation)
                      :accessor renderer-target-generation)
   (frame-resources :initform (make-canvas-frame-resource-cache) :reader renderer-frame-resources)
   (frame-index :initform 0 :accessor renderer-frame-index)))

(defun make-renderer (device color-format extent
                      &key (terrain-factory 'make-terrain-drawing)
                        (shadow-factory 'make-sun-shadow)
                        (lattice-factory 'make-lattice-drawing)
                        (reconstruction-factory 'make-reconstruction)
                        (finishing-factory 'make-image-finishing)
                        (exposure-factory 'make-automatic-exposure)
                        (sky-factory 'make-sky-drawing)
                        (player-factory 'make-player-drawing)
                        (torch-factory 'make-framed-torch-drawing))
  "Compose the renderer's independently owned subsystems.
Terrain, lattice, sky, player, and torch factories receive DEVICE, scene
formats, and sample count. Shadow, reconstruction, and exposure factories
receive DEVICE; finishing receives DEVICE and COLOR-FORMAT. NIL omits lattice,
sky, player, or torches. Factories return fresh owners. Refresh preserves all
choices. Resident geometry and resized targets are published independently."
  (unless (and terrain-factory shadow-factory reconstruction-factory finishing-factory exposure-factory)
    (error "Terrain, shadow, reconstruction, finishing, and exposure factories are required."))
  (let ((renderer
          (make-instance
           'renderer :device device
           :component-options
           (list :terrain-factory terrain-factory :shadow-factory shadow-factory
                 :lattice-factory lattice-factory :reconstruction-factory reconstruction-factory
                 :finishing-factory finishing-factory :exposure-factory exposure-factory
                 :sky-factory sky-factory :player-factory player-factory :torch-factory torch-factory)))
        (completed-p nil))
    (unwind-protect
         (labels ((component (factory type &rest arguments)
                    (when factory
                      (let ((value (apply factory arguments)))
                        ;; Adopt before checking so a bad factory result that
                        ;; owns GPU resources is also covered by rollback.
                        (when value (own-gpu-object renderer value))
                        (unless (typep value type)
                          (error "Renderer factory ~S returned ~S, expected ~S." factory value type))
                        value))))
           (setf (renderer-reconstruction renderer)
                 (component reconstruction-factory 'reconstruction device)
                 (renderer-sun-shadow renderer) (component shadow-factory 'sun-shadow device)
                 (renderer-finishing renderer) (component finishing-factory 'image-finishing device color-format))
           (let ((formats (if (renderer-temporal-p renderer)
                              '(:rgba16-float :rg16-float) '(:rgba16-float))))
             (setf (renderer-terrain renderer)
                   (component terrain-factory 'terrain-drawing device formats *scene-sample-count*)
                   (renderer-lattice renderer)
                   (component lattice-factory 'drawing-program device formats *scene-sample-count*)
                   (renderer-sky renderer)
                   (component sky-factory 'scene-drawing device formats *scene-sample-count*)
                   (renderer-player renderer)
                   (component player-factory 'scene-drawing device formats *scene-sample-count*)
                   (renderer-torches renderer)
                   (component torch-factory 'torch-drawing device formats *scene-sample-count*)
                   (renderer-exposure-control renderer)
                   (component exposure-factory 'exposure-control device)))
           (multiple-value-bind (data count buffer)
               (%make-renderer-flame-resources renderer (make-array 0 :element-type 'single-float))
             (declare (ignore data count))
             (setf (renderer-publication renderer)
                   (%make-empty-renderer-publication :flame-instance-buffer buffer)))
           (replace-renderer-target-generation renderer extent)
           (setf completed-p t)
           renderer)
      (unless completed-p
        (with-release-warnings
          (releasing :renderer-construction (destroy-renderer renderer)))))))

(defun destroy-renderer (renderer)
  "Retire borrowed bindings and residency before releasing the component owners."
  (with-release-report
    (releasing :presentation-slots
      (destroy-canvas-frame-resource-cache
       (renderer-frame-resources renderer) #'destroy-renderer-frame-state))
    (releasing :targets (destroy-renderer-targets renderer))
    (loop for slot being the hash-values of (renderer-mesh-slots renderer)
          do (own-gpu-object renderer slot))
    (own-gpu-object renderer (renderer-publication renderer))
    (setf (renderer-publication renderer) (%make-empty-renderer-publication))
    (releasing :components (release-owned-gpu-resources renderer)))
  (values))

(defun renderer-component-options (renderer)
  "Preserve all composition choices when rebuilding a renderer."
  (copy-list (%renderer-component-options renderer)))

(defun renderer-exposure-factory (renderer)
  (getf (%renderer-component-options renderer) :exposure-factory))

(defun renderer-shadow-texture (renderer)
  (sun-shadow-texture (renderer-sun-shadow renderer)))

(defun renderer-shadow-view (renderer)
  (sun-shadow-view (renderer-sun-shadow renderer)))

(defun renderer-shadow-sampler (renderer)
  (sun-shadow-sampler (renderer-sun-shadow renderer)))

(defun renderer-sampler (renderer)
  (finishing-sampler (renderer-finishing renderer)))

(defun renderer-temporal-resolve-kind (renderer)
  (reconstruction-kind (renderer-reconstruction renderer)))

(defun renderer-temporal-p (renderer)
  (not (null (renderer-temporal-resolve-kind renderer))))

(defun renderer-previous-view (renderer)
  (reconstruction-previous-view (renderer-reconstruction renderer)))

(defun (setf renderer-previous-view) (value renderer)
  (setf (reconstruction-previous-view (renderer-reconstruction renderer)) value))

(defun renderer-history-valid-p (renderer)
  (reconstruction-history-valid-p (renderer-reconstruction renderer)))

(defun (setf renderer-history-valid-p) (value renderer)
  (setf (reconstruction-history-valid-p (renderer-reconstruction renderer)) value))

(defun renderer-history-used-p (renderer)
  (reconstruction-history-used-p (renderer-reconstruction renderer)))

(defun (setf renderer-history-used-p) (value renderer)
  (setf (reconstruction-history-used-p (renderer-reconstruction renderer)) value))

(defun renderer-exposure (renderer)
  (exposure-value (renderer-exposure-control renderer)))

(defun renderer-metalfx-temporal-p (renderer)
  (eq :metalfx (renderer-temporal-resolve-kind renderer)))

(defun renderer-shader-temporal-p (renderer)
  (eq :shader (renderer-temporal-resolve-kind renderer)))
