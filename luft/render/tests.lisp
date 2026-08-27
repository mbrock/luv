(defpackage #:luft.render.tests
  (:use #:cl #:rove)
  (:local-nicknames (#:clim #:clim)
                    (#:climi #:clim-internals)
                    (#:luv #:luv)
                    (#:production #:luv.production)
                    (#:render #:luft.render)))

(in-package #:luft.render.tests)

(defclass flame-resource-probe-device (luv:gpu-device)
  ((events :initform nil :accessor flame-resource-probe-events)
   (created-resources :initform nil
                      :accessor flame-resource-probe-created-resources)
   (destroyed-resources :initform nil
                        :accessor flame-resource-probe-destroyed-resources)
   (create-count :initform 0 :accessor flame-resource-probe-create-count)
   (fail-create-at :initform nil
                   :accessor flame-resource-probe-fail-create-at)
   (fail-bind-group-p :initform nil
                      :accessor flame-resource-probe-fail-bind-group-p)
   (fail-bind-group-label :initform nil
                          :accessor flame-resource-probe-fail-bind-group-label)))

(defclass flame-resource-probe ()
  ((kind :initarg :kind :reader flame-resource-probe-kind)
   (device :initarg :device :reader flame-resource-probe-device)
   (descriptor :initarg :descriptor :initform nil
               :reader flame-resource-probe-descriptor)
   (data :initform nil :accessor flame-resource-probe-data)))

(defclass flame-temporal-scaler-probe
    (luv:gpu-temporal-scaler flame-resource-probe)
  ())

(defun begin-flame-resource-probe-create (device)
  (let ((ordinal (incf (flame-resource-probe-create-count device))))
    (when (eql ordinal (flame-resource-probe-fail-create-at device))
      (error "Injected GPU resource creation failure at ordinal ~D." ordinal))))

(defun record-flame-resource-probe-create (device resource)
  (push resource (flame-resource-probe-created-resources device))
  resource)

(defmethod luv:create
    ((device flame-resource-probe-device) (descriptor luv::buffer-descriptor))
  (begin-flame-resource-probe-create device)
  (let ((resource
          (make-instance 'flame-resource-probe
                         :kind :buffer :device device
                         :descriptor descriptor)))
    (push (list :create-buffer (luv::buffer-descriptor-size descriptor))
          (flame-resource-probe-events device))
    (record-flame-resource-probe-create device resource)))

(defmethod luv:create
    ((device flame-resource-probe-device) (descriptor luv::texture-descriptor))
  (begin-flame-resource-probe-create device)
  (let ((resource
          (make-instance 'flame-resource-probe
                         :kind :texture :device device
                         :descriptor descriptor)))
    (push (list :create-texture (luv::gpu-descriptor-label descriptor))
          (flame-resource-probe-events device))
    (record-flame-resource-probe-create device resource)))

(defmethod luv:create
    ((device flame-resource-probe-device)
     (descriptor luv::texture-view-descriptor))
  (begin-flame-resource-probe-create device)
  (let ((resource
          (make-instance 'flame-resource-probe
                         :kind :texture-view :device device
                         :descriptor descriptor)))
    (push (list :create-texture-view
                (luv::texture-view-descriptor-texture descriptor))
          (flame-resource-probe-events device))
    (record-flame-resource-probe-create device resource)))

(defmethod luv:create
    ((device flame-resource-probe-device) (descriptor luv::sampler-descriptor))
  (begin-flame-resource-probe-create device)
  (let ((resource
          (make-instance 'flame-resource-probe
                         :kind :sampler :device device
                         :descriptor descriptor)))
    (push (list :create-sampler (luv::gpu-descriptor-label descriptor))
          (flame-resource-probe-events device))
    (record-flame-resource-probe-create device resource)))

(defmethod luv:create
    ((device flame-resource-probe-device)
     (descriptor luv::temporal-scaler-descriptor))
  (begin-flame-resource-probe-create device)
  (let ((resource
          (make-instance
           'flame-temporal-scaler-probe
           :kind :temporal-scaler :device device :descriptor descriptor
           :label (luv::gpu-descriptor-label descriptor)
           :input-size (luv::temporal-scaler-descriptor-input-size descriptor)
           :output-size (luv::temporal-scaler-descriptor-output-size descriptor)
           :color-usage '(:texture-binding)
           :depth-usage '(:texture-binding)
           :motion-usage '(:render-attachment)
           :output-usage '(:texture-binding))))
    (push (list :create-temporal-scaler
                (luv::temporal-scaler-descriptor-input-size descriptor)
                (luv::temporal-scaler-descriptor-output-size descriptor))
          (flame-resource-probe-events device))
    (record-flame-resource-probe-create device resource)))

(defmethod luv:create
    ((device flame-resource-probe-device)
     (descriptor luv::bind-group-descriptor))
  (begin-flame-resource-probe-create device)
  (when (flame-resource-probe-fail-bind-group-p device)
    (error "Injected flame bind-group construction failure."))
  (when (equal (flame-resource-probe-fail-bind-group-label device)
               (luv::gpu-descriptor-label descriptor))
    (error "Injected bind-group failure for ~A."
           (luv::gpu-descriptor-label descriptor)))
  (let ((resource
          (make-instance 'flame-resource-probe
                         :kind :bind-group :device device
                         :descriptor descriptor)))
    (push (list :create-bind-group
                (length (luv::bind-group-descriptor-entries descriptor)))
          (flame-resource-probe-events device))
    (record-flame-resource-probe-create device resource)))

(defmethod luv:write-buffer
    ((resource flame-resource-probe) data &key (offset 0))
  (setf (flame-resource-probe-data resource) (copy-seq data))
  (push (list :write offset (length data))
        (flame-resource-probe-events
         (flame-resource-probe-device resource)))
  resource)

(defmethod luv:destroy ((resource flame-resource-probe))
  (push resource
        (flame-resource-probe-destroyed-resources
         (flame-resource-probe-device resource)))
  (push (list :destroy (flame-resource-probe-kind resource))
        (flame-resource-probe-events
         (flame-resource-probe-device resource)))
  (values))

(defun make-two-chunk-streaming-scene ()
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 7)))
    ;; These cells share a face across the X chunk boundary.
    (luft.render::scene-builder-cell builder 63 4 4)
    (luft.render::scene-builder-cell builder 64 4 4)
    (render:make-streaming-scene
     (luft.render::finish-scene-builder builder) :frames-per-load 1)))

(defun make-streaming-material-seam-test-scene ()
  "A mixed terrain, stone, and crystal union crossing the X chunk seam."
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 7)))
    (luft.render::scene-builder-box builder 62 65 7 10 3 3)
    (luft.render::scene-builder-cell builder 63 8 4 :architecture-p t)
    (luft.render::scene-builder-cell
     builder 64 8 4 :material luft.render::*crystal-material-placement*)
    (luft.render::finish-scene-builder builder)))

(defun make-streaming-torch-seam-test-scene ()
  "A support-owned torch whose top-edge band is owned across the X seam."
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 7)))
    ;; X=63 is owned by the left chunk, while the lattice anchor at X=64 is
    ;; owned by the right.  The remote crystal keeps that right owner resident
    ;; without changing the isolated support cell's local surface.
    (luft.render::scene-builder-cell builder 63 8 4 :architecture-p t)
    (luft.render::scene-builder-cell
     builder 64 20 4 :material luft.render::*crystal-material-placement*)
    (luft.render::scene-builder-torch
     builder 63 8 4 :z :high :u 1.0 :v 0.0)
    (luft.render::finish-scene-builder builder)))

(defun streaming-store-keys (scene)
  (sort (loop for key being the hash-keys of
              (luft.render::streaming-scene-store scene)
              collect key)
        #'<))

(defun load-all-streaming-chunks (scene bevel-width)
  (dolist (key (streaming-store-keys scene) scene)
    (setf (gethash key (luft.render::streaming-scene-loaded scene))
          bevel-width)))

(defun make-streaming-retarget-light-test-scene (&key near-p)
  "Return a torch support two chunks from a far inert owner."
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 9))
         (support-x 160)
         (near-x 224)
         (far-x 32)
         (y 80))
    (luft.render::scene-builder-cell
     builder support-x y 4 :architecture-p t)
    (luft.render::scene-builder-torch builder support-x y 4 :z :high)
    (luft.render::scene-builder-cell builder far-x y 4)
    (when near-p
      (luft.render::scene-builder-cell builder near-x y 4))
    (values
     (render:make-streaming-scene
      (luft.render::finish-scene-builder builder) :residency-radius 0)
     (luft:chunk-key-at support-x y)
     (and near-p (luft:chunk-key-at near-x y))
     (luft:chunk-key-at far-x y))))

(defun install-streaming-retarget-light-test-residency
    (scene source-keys bevel-width &optional bevel-profile)
  "Synchronously establish exact installed light for a test residency."
  (clrhash (luft.render::streaming-scene-loaded scene))
  (dolist (key source-keys)
    (setf (gethash key (luft.render::streaming-scene-loaded scene))
          bevel-width))
  (setf (luft.render::streaming-scene-geometry-policy-signature scene)
        (luft.render::material-bevel-profile-geometry-signature bevel-profile))
  (let* ((output-keys
           (luft.render::streaming-scene-canonical-owner-closure
            scene source-keys))
         (snapshot
           (luft.render::make-streaming-region-snapshot
            scene output-keys bevel-width bevel-profile)))
    (multiple-value-bind (owners census diagnostics generation)
        (luft.render::mesh-streaming-snapshot snapshot)
      (declare (ignore owners census diagnostics))
      (setf (luft.render::streaming-scene-light-generation scene)
            (render:scene-mesh-generation-light-generation generation))
      generation)))

(defun capture-streaming-retarget-snapshot
    (scene bevel-width focus-x focus-y &optional bevel-profile)
  "Retarget once and return its result and synchronously observed snapshot."
  (let ((snapshot nil)
        (system
          (production:make-single-worker-production-system
           :name "LUFT realized-light retarget matrix")))
    (unwind-protect
         (let ((luft.render::*streaming-mesh-snapshot-observer*
                 (lambda (value) (setf snapshot value))))
           (values
            (luft.render::retarget-streaming-scene
             scene system bevel-width focus-x focus-y bevel-profile)
            snapshot))
      (production:stop-production-system system))))

(defun surface-mesh-tree-meshes (root)
  "Return ROOT and every recursively attached companion mesh."
  (labels ((walk (mesh)
             (cons mesh
                   (mapcan #'walk (luft:surface-mesh-companions mesh)))))
    (if root (walk root) nil)))

(defun canonical-mesh-cohorts-equal-p (left right)
  (luft::%triangle-counts=
   (luft::%canonical-triangle-record-counts left)
   (luft::%canonical-triangle-record-counts right)))

(defun prepared-owner-mesh (prepared-owners key)
  (when (typep prepared-owners 'luft.render::streaming-mesh-result)
    (setf prepared-owners
          (luft.render::streaming-mesh-result-meshes prepared-owners)))
  (let ((entry (assoc key prepared-owners :test #'eql)))
    (and entry
         (luft.render::prepared-render-mesh-mesh (cdr entry)))))

(defun make-renderer-publication-probe ()
  (let* ((device (make-instance 'flame-resource-probe-device))
         (resource
           (make-instance 'flame-resource-probe
                          :kind :fixture :device device))
         (material-buffer
           (make-instance 'flame-resource-probe
                          :kind :material-buffer :device device))
         (effect
           (make-instance 'flame-resource-probe
                          :kind :effect :device device))
         (depth-sampler
           (make-instance 'flame-resource-probe
                          :kind :depth-sampler :device device))
         (old-flame-group
           (make-instance 'flame-resource-probe
                          :kind :bind-group :device device))
         (vocabulary luft.render::*surface-assembly-vocabulary*)
         (revision
           (luv.domains:identity-vocabulary-revision vocabulary))
         (count
           (length (luv.domains:identity-vocabulary-members vocabulary)))
         (words
           (luft.render::surface-assembly-descriptor-words vocabulary)))
    (let ((renderer
            (make-instance
             'luft.render::renderer
             :device device :camera-buffer resource
             :publication
             (luft.render::%make-empty-renderer-publication
              :material-buffer material-buffer :material-vocabulary vocabulary
              :material-vocabulary-revision revision
              :material-descriptor-count count :material-descriptor-words words)
             :layout :mesh-layout :shadow-layout :shadow-layout
             :shadow-view resource :shadow-sampler resource
             :flame-layout :flame-layout :flame-effect-buffer effect
             :flame-depth-sampler depth-sampler
             :torch-body-layout :torch-body-layout
             :torch-body-vertex-buffer resource)))
      ;; Publication tests exercise the target-owned population/depth join
      ;; without allocating a complete resize cohort.
      (setf (luft.render::renderer-target-generation renderer)
            (luft.render::%make-renderer-target-generation
             (luft.render::%make-renderer-target-resources
              :extent '(1 1) :render-extent '(1 1) :depth-view resource)
             (luft.render::%make-renderer-flame-target-join old-flame-group)))
      (values renderer device))))

(defun make-renderer-publication-test-mesh ()
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 4)))
    (luft.render::scene-builder-cell builder 4 4 4)
    (render:make-render-mesh (luft.render::finish-scene-builder builder))))

(deftest global-torch-frames-follow-their-sparse-owner-without-padding
  (let* ((empty (make-renderer-publication-test-mesh))
         (torch (make-renderer-publication-test-mesh))
         (frame
           (render:pack-torch-flame-frame
            0 0 0 0.25 0 0 1 0 1 0 0 1)))
    (setf (luft:surface-mesh-attachments torch) (list frame))
    (let ((data
            (luft.render::mesh-slots-torch-frame-data
             (list
              (cons 0 (luft.render::%make-mesh-slot :mesh empty))
              (cons 1 (luft.render::%make-mesh-slot :mesh torch))))))
      (ok (= render:+torch-flame-instance-scalar-count+ (length data)))
      (ok (equalp frame data)))))

(defun make-renderer-target-probe (temporal-p &optional
                                                (temporal-kind
                                                  (and temporal-p :metalfx)))
  (let* ((device (make-instance 'flame-resource-probe-device))
         (camera
           (make-instance 'flame-resource-probe
                          :kind :camera :device device))
         (sampler
           (make-instance 'flame-resource-probe
                          :kind :sampler :device device))
         (depth-sampler
           (make-instance 'flame-resource-probe
                          :kind :depth-sampler :device device))
         (effect
           (make-instance 'flame-resource-probe
                          :kind :effect :device device))
         (flame-buffer
           (make-instance 'flame-resource-probe
                          :kind :flame-buffer :device device))
         (renderer
           (make-instance
            'luft.render::renderer
            :device device :color-format :bgra8-unorm :temporal-p temporal-p
            :temporal-resolve-kind temporal-kind
            :camera-buffer camera
            :publication
            (luft.render::%make-empty-renderer-publication
             :flame-instance-buffer flame-buffer)
            :flame-layout :flame-layout :flame-effect-buffer effect
            :flame-depth-sampler depth-sampler
            :composite-layout :composite-layout)))
    (setf (luft.render::renderer-present-layout renderer) :present-layout
          (luft.render::renderer-exposure-probe-layout renderer)
          :exposure-layout
          (luft.render::renderer-composite-layout renderer)
          :composite-layout
          (luft.render::renderer-temporal-layout renderer) :temporal-layout
          (luft.render::renderer-sampler renderer) sampler)
    (values renderer device)))

(defun renderer-target-generation-resources (generation)
  (remove
   nil
   (list
    (luft.render::renderer-target-generation-temporal-scaler generation)
    (luft.render::renderer-target-generation-depth-msaa-texture generation)
    (luft.render::renderer-target-generation-depth-msaa-view generation)
    (luft.render::renderer-target-generation-depth-texture generation)
    (luft.render::renderer-target-generation-depth-view generation)
    (luft.render::renderer-target-generation-scene-msaa-texture generation)
    (luft.render::renderer-target-generation-scene-msaa-view generation)
    (luft.render::renderer-target-generation-scene-texture generation)
    (luft.render::renderer-target-generation-scene-view generation)
    (luft.render::renderer-target-generation-motion-msaa-texture generation)
    (luft.render::renderer-target-generation-motion-msaa-view generation)
    (luft.render::renderer-target-generation-motion-texture generation)
    (luft.render::renderer-target-generation-motion-view generation)
    (luft.render::renderer-target-generation-resolved-texture generation)
    (luft.render::renderer-target-generation-resolved-view generation)
    (luft.render::renderer-target-generation-history-texture generation)
    (luft.render::renderer-target-generation-history-view generation)
    (luft.render::renderer-target-generation-temporal-bind-group generation)
    (luft.render::renderer-target-generation-composite-texture generation)
    (luft.render::renderer-target-generation-composite-view generation)
    (luft.render::renderer-target-generation-composite-source-bind-group
     generation)
    (luft.render::renderer-target-generation-flame-bind-group generation)
    (luft.render::renderer-target-generation-present-bind-group generation)
    (luft.render::renderer-target-generation-exposure-probe-bind-group
     generation))))

(defun probe-bind-group-resource (bind-group binding)
  (let* ((descriptor (flame-resource-probe-descriptor bind-group))
         (entry
           (find binding (luv::bind-group-descriptor-entries descriptor)
                 :key (lambda (candidate) (getf candidate :binding)))))
    (getf entry :resource)))

(defun probe-texture-view-texture (view)
  (luv::texture-view-descriptor-texture
   (flame-resource-probe-descriptor view)))

(defun every-probe-resource-destroyed-once-p (resources device)
  (let ((destroyed (flame-resource-probe-destroyed-resources device)))
    (loop for resource in resources
          always (= 1 (count resource destroyed :test #'eq)))))

(deftest flame-depth-sampling-has-one-dedicated-nearest-sampler-boundary
  (let* ((device (make-instance 'flame-resource-probe-device))
         (sampler (luft.render::make-renderer-flame-depth-sampler device))
         (descriptor (flame-resource-probe-descriptor sampler)))
    (ok (eq :nearest (luv::sampler-descriptor-mag-filter descriptor)))
    (ok (eq :nearest (luv::sampler-descriptor-min-filter descriptor)))
    (ok (eq :nearest (luv::sampler-descriptor-mipmap-filter descriptor)))
    (luv:destroy sampler)
    (ok (every-probe-resource-destroyed-once-p (list sampler) device)))
  (let ((device (make-instance 'flame-resource-probe-device)))
    (setf (flame-resource-probe-fail-create-at device) 1)
    (ok (signals
         (luft.render::make-renderer-flame-depth-sampler device)
         'error))
    (ok (null (flame-resource-probe-created-resources device)))
    (ok (null (flame-resource-probe-destroyed-resources device)))))

(deftest a-frame-target-generation-is-one-coherent-resource-identity
  (dolist (temporal-p '(nil t))
    (multiple-value-bind (renderer device)
        (make-renderer-target-probe temporal-p)
      (let* ((requested-extent (list 640 360))
             (generation
               (luft.render::replace-renderer-target-generation
                renderer requested-extent))
             (resources (renderer-target-generation-resources generation))
             (present-group
               (luft.render::renderer-present-bind-group renderer))
             (probe-group
               (luft.render::renderer-exposure-probe-bind-group renderer))
             (composite-source-group
               (luft.render::renderer-composite-source-bind-group renderer))
             (flame-group
               (luft.render::renderer-flame-bind-group renderer))
             (base-source
               (if temporal-p
                   (luft.render::renderer-resolved-view renderer)
                   (luft.render::renderer-scene-view renderer))))
        (ok (eq generation
                (luft.render::renderer-target-generation renderer)))
        (ok (equal requested-extent
                   (luft.render::renderer-extent renderer)))
        (ok (not (eq requested-extent
                     (luft.render::renderer-extent renderer)))
            "the generation owns its extent value")
        (ok (equal (luft.render::render-scale-extent requested-extent)
                   (luft.render::renderer-render-extent renderer)))
        (ok (eq (luft.render::renderer-depth-view renderer)
                (probe-bind-group-resource present-group 2)))
        (ok (eq (luft.render::renderer-depth-texture renderer)
                (probe-texture-view-texture
                 (luft.render::renderer-depth-view renderer))))
        (let ((multisampled-depth
                (luft.render::renderer-target-generation-depth-msaa-texture
                 generation)))
          (ok (= luft.render::*scene-sample-count*
                 (luv::texture-descriptor-sample-count
                  (flame-resource-probe-descriptor multisampled-depth))))
          (ok (equal '(:render-attachment)
                     (luv::texture-descriptor-usage
                      (flame-resource-probe-descriptor multisampled-depth)))))
        (ok (eq (luft.render::renderer-scene-texture renderer)
                (probe-texture-view-texture
                 (luft.render::renderer-scene-view renderer))))
        (ok (eq (luft.render::renderer-composite-texture renderer)
                (probe-texture-view-texture
                 (luft.render::renderer-composite-view renderer))))
        (ok (eq base-source
                (probe-bind-group-resource composite-source-group 0)))
        (ok (eq (luft.render::renderer-composite-view renderer)
                (probe-bind-group-resource present-group 0)))
        (ok (eq (luft.render::renderer-composite-view renderer)
                (probe-bind-group-resource probe-group 0)))
        (ok (eq (luft.render::renderer-sampler renderer)
                (probe-bind-group-resource present-group 1)))
        (ok (eq (luft.render::renderer-camera-buffer renderer)
                (probe-bind-group-resource present-group 3)))
        (ok (eq (luft.render::renderer-flame-instance-buffer renderer)
                (probe-bind-group-resource flame-group 0)))
        (ok (eq (luft.render::renderer-camera-buffer renderer)
                (probe-bind-group-resource flame-group 1)))
        (ok (eq (luft.render::renderer-flame-effect-buffer renderer)
                (probe-bind-group-resource flame-group 2)))
        (ok (eq (luft.render::renderer-depth-view renderer)
                (probe-bind-group-resource flame-group 3)))
        (ok (eq (luft.render::renderer-flame-depth-sampler renderer)
                (probe-bind-group-resource flame-group 4)))
        (ok (eql temporal-p
                 (not (null
                       (luft.render::renderer-temporal-scaler renderer)))))
        (ok (eql temporal-p
                 (not (null
                       (luft.render::renderer-motion-texture renderer)))))
        (ok (eql temporal-p
                 (not (null
                       (luft.render::renderer-resolved-texture renderer)))))
        (ok (= (if temporal-p 21 14) (length resources)))
        (ok (= (length resources)
               (length (flame-resource-probe-created-resources device))))
        (let ((previous-view (list :stable-previous-view))
              (create-count (flame-resource-probe-create-count device)))
          (setf (luft.render::renderer-previous-view renderer) previous-view
                (luft.render::renderer-history-valid-p renderer) t
                (luft.render::renderer-history-used-p renderer) t)
          (ok (eq renderer
                  (luft.render::ensure-renderer-extent
                   renderer (copy-list requested-extent))))
          (ok (eq generation
                  (luft.render::renderer-target-generation renderer)))
          (ok (eq previous-view
                  (luft.render::renderer-previous-view renderer)))
          (ok (luft.render::renderer-history-valid-p renderer))
          (ok (luft.render::renderer-history-used-p renderer))
          (ok (= create-count
                 (flame-resource-probe-create-count device))
              "an equal extent is a resource and history no-op"))
        (luft.render::destroy-renderer-targets renderer)
        (ok (not (eq generation
                     (luft.render::renderer-target-generation renderer))))
        (ok (null (luft.render::renderer-extent renderer)))
        (ok (null (renderer-target-generation-resources
                   (luft.render::renderer-target-generation renderer))))
        (ok (every-probe-resource-destroyed-once-p resources device))))))

(deftest initial-frame-target-staging-cleans-every-allocation-boundary
  (dolist (temporal-p '(nil t))
    (loop for failure-ordinal from 1 to (if temporal-p 21 14)
          do
             (multiple-value-bind (renderer device)
                 (make-renderer-target-probe temporal-p)
               (let ((initial
                       (luft.render::renderer-target-generation renderer))
                     (previous-view (list :initial-view)))
                 (setf (luft.render::renderer-previous-view renderer)
                       previous-view
                       (luft.render::renderer-history-valid-p renderer) t
                       (luft.render::renderer-history-used-p renderer) t
                       (flame-resource-probe-fail-create-at device)
                       failure-ordinal)
                 (ok (signals
                      (luft.render::create-frame-targets renderer '(640 360))
                      'error)
                     (format nil "~:[direct~;temporal~] create ~D fails"
                             temporal-p failure-ordinal))
                 (ok (eq initial
                         (luft.render::renderer-target-generation renderer)))
                 (ok (eq previous-view
                         (luft.render::renderer-previous-view renderer)))
                 (ok (luft.render::renderer-history-valid-p renderer))
                 (ok (luft.render::renderer-history-used-p renderer))
                 (let ((created
                         (flame-resource-probe-created-resources device)))
                   (ok (= (1- failure-ordinal) (length created)))
                   (ok (= (length created)
                          (length
                           (flame-resource-probe-destroyed-resources device))))
                   (ok (every-probe-resource-destroyed-once-p
                        created device))))))))

(deftest vulkan-temporal-targets-own-an-explicit-resolve-history
  (multiple-value-bind (renderer device)
      (make-renderer-target-probe t :shader)
    (let* ((generation
             (luft.render::replace-renderer-target-generation
              renderer '(640 360)))
           (history (luft.render::renderer-history-texture renderer))
           (resolved (luft.render::renderer-resolved-texture renderer))
           (group (luft.render::renderer-temporal-bind-group renderer))
           (frame
             (luft.render::%make-renderer-frame-state
              :camera-buffer :presentation-slot-camera
              :flame-effect-buffer :unused))
           (frame-group
             (luft.render::renderer-frame-temporal-bind-group
              renderer frame)))
      (ok (null (luft.render::renderer-temporal-scaler renderer)))
      (ok (equal '(640 360)
                 (luft.render::renderer-render-extent renderer)))
      (ok history)
      (ok resolved)
      (ok group)
      (ok (eq (luft.render::renderer-scene-view renderer)
              (probe-bind-group-resource group 0)))
      (ok (eq (luft.render::renderer-motion-view renderer)
              (probe-bind-group-resource group 1)))
      (ok (eq (luft.render::renderer-history-view renderer)
              (probe-bind-group-resource group 2)))
      (ok (eq (luft.render::renderer-sampler renderer)
              (probe-bind-group-resource group 3)))
      (ok (eq (luft.render::renderer-camera-buffer renderer)
              (probe-bind-group-resource group 4)))
      ;; The live encode uses the slot-derived group, not GENERATION's legacy
      ;; inspectable group.  Its mutable camera upload comes from the
      ;; reacquired presentation slot.
      (ok (eq :presentation-slot-camera
              (probe-bind-group-resource frame-group 4)))
      (ok (equal '(640 360 1)
                 (luv::texture-descriptor-size
                  (flame-resource-probe-descriptor history))))
      (ok (member :copy-dst
                  (luv::texture-descriptor-usage
                   (flame-resource-probe-descriptor history))))
      (ok (member :render-attachment
                  (luv::texture-descriptor-usage
                   (flame-resource-probe-descriptor resolved))))
      (ok (member :copy-src
                  (luv::texture-descriptor-usage
                   (flame-resource-probe-descriptor resolved))))
      (ok (member :texture-binding
                  (luv::texture-descriptor-usage
                   (flame-resource-probe-descriptor
                    (luft.render::renderer-motion-texture renderer)))))
      (ok (= 23 (length (renderer-target-generation-resources generation))))
      (luv:destroy frame-group)
      (luft.render::destroy-renderer-targets renderer)
      (ok (every-probe-resource-destroyed-once-p
           (flame-resource-probe-created-resources device) device)))))

(deftest ordinary-gpu-devices-select-the-inspectable-temporal-resolve
  (let ((device (make-instance 'flame-resource-probe-device)))
    (let ((luft.render::*temporal-upscaling-p* t))
      (ok (eq :shader (luft.render::temporal-resolve-kind device))))
    (let ((luft.render::*temporal-upscaling-p* nil))
      (ok (null (luft.render::temporal-resolve-kind device))))))

(deftest failed-frame-target-resize-preserves-the-exact-old-generation
  (dolist (temporal-p '(nil t))
    (multiple-value-bind (renderer device)
        (make-renderer-target-probe temporal-p)
      (luft.render::create-frame-targets renderer '(640 360))
      (let* ((old-generation
               (luft.render::renderer-target-generation renderer))
             (old-resources
               (renderer-target-generation-resources old-generation))
             (old-previous-view (list :old-previous-view))
             (created-before
               (copy-list
                (flame-resource-probe-created-resources device))))
        (setf (luft.render::renderer-previous-view renderer) old-previous-view
              (luft.render::renderer-history-valid-p renderer) t
              (luft.render::renderer-history-used-p renderer) t
              ;; Fail at the final target-dependent bind group, maximizing the
              ;; staged cohort whose cleanup is observable.
              (flame-resource-probe-fail-create-at device)
              (+ (flame-resource-probe-create-count device)
                 (if temporal-p 21 14)))
        (ok (signals
             (luft.render::ensure-renderer-extent renderer '(800 450))
             'error))
        (ok (eq old-generation
                (luft.render::renderer-target-generation renderer)))
        (ok (eq old-previous-view
                (luft.render::renderer-previous-view renderer)))
        (ok (luft.render::renderer-history-valid-p renderer))
        (ok (luft.render::renderer-history-used-p renderer))
        (ok (every (lambda (resource)
                     (not (member
                           resource
                           (flame-resource-probe-destroyed-resources device)
                           :test #'eq)))
                   old-resources)
            "no old target resource is retired on failure")
        (let ((staged
                (set-difference
                 (flame-resource-probe-created-resources device)
                 created-before :test #'eq)))
          (ok (= (1- (if temporal-p 21 14)) (length staged)))
          (ok (every-probe-resource-destroyed-once-p staged device)))
        (setf (flame-resource-probe-fail-create-at device) nil)
        (let ((event-count-before-success
                (length (flame-resource-probe-events device))))
          (luft.render::ensure-renderer-extent renderer '(800 450))
          (let* ((new-event-count
                   (- (length (flame-resource-probe-events device))
                      event-count-before-success))
                 (events
                   (reverse
                    (subseq (flame-resource-probe-events device)
                            0 new-event-count)))
                 (last-create
                   (position-if
                    (lambda (event)
                      (member (car event)
                              '(:create-temporal-scaler :create-texture
                                :create-texture-view :create-bind-group)))
                    events :from-end t))
                 (first-destroy (position :destroy events :key #'car)))
            (ok (< last-create first-destroy)
                "all candidate creation precedes old-generation retirement")))
        (let ((new-generation
                (luft.render::renderer-target-generation renderer)))
          (ok (not (eq old-generation new-generation)))
          (ok (not (eq
                    (luft.render::renderer-target-generation-resources
                     old-generation)
                    (luft.render::renderer-target-generation-resources
                     new-generation)))
              "resize owns a distinct complete target cohort")
          (ok (equal '(800 450) (luft.render::renderer-extent renderer)))
          (ok (null (luft.render::renderer-previous-view renderer)))
          (ok (null (luft.render::renderer-history-valid-p renderer)))
          (ok (null (luft.render::renderer-history-used-p renderer)))
          (ok (every-probe-resource-destroyed-once-p old-resources device))
          (ok (every (lambda (resource)
                       (not (member
                             resource
                             (flame-resource-probe-destroyed-resources device)
                             :test #'eq)))
                     (renderer-target-generation-resources new-generation))
              "the installed generation remains live")
          (luft.render::destroy-renderer-targets renderer))))))

(deftest frame-target-precommit-failure-retires-only-the-candidate
  (multiple-value-bind (renderer device) (make-renderer-target-probe t)
    (luft.render::create-frame-targets renderer '(640 360))
    (let* ((old-generation
             (luft.render::renderer-target-generation renderer))
           (old-resources
             (renderer-target-generation-resources old-generation))
           (old-previous-view (list :old-previous-view))
           (created-before
             (copy-list (flame-resource-probe-created-resources device)))
           (observed-candidate nil))
      (setf (luft.render::renderer-previous-view renderer) old-previous-view
            (luft.render::renderer-history-valid-p renderer) t
            (luft.render::renderer-history-used-p renderer) t)
      (let ((luft.render::*renderer-target-generation-precommit-hook*
              (lambda (owner candidate)
                (ok (eq renderer owner))
                (ok (eq old-generation
                        (luft.render::renderer-target-generation owner))
                    "the old generation is still published at precommit")
                (setf observed-candidate candidate)
                (ok (= 21 (length
                           (renderer-target-generation-resources candidate))))
                (error "Injected target-generation precommit failure."))))
        (ok (signals
             (luft.render::ensure-renderer-extent renderer '(800 450))
             'error)))
      (ok observed-candidate)
      (ok (eq old-generation
              (luft.render::renderer-target-generation renderer)))
      (ok (eq old-previous-view
              (luft.render::renderer-previous-view renderer)))
      (ok (luft.render::renderer-history-valid-p renderer))
      (ok (luft.render::renderer-history-used-p renderer))
      (ok (every-probe-resource-destroyed-once-p
           (renderer-target-generation-resources observed-candidate) device))
      (ok (every (lambda (resource)
                   (not (member
                         resource
                         (flame-resource-probe-destroyed-resources device)
                         :test #'eq)))
                 old-resources))
      (ok (= 21
             (length
              (set-difference
               (flame-resource-probe-created-resources device)
               created-before :test #'eq))))
      (luft.render::destroy-renderer-targets renderer))))

(deftest a-streaming-mesh-request-owns-an-immutable-residency-snapshot
  (let* ((scene (make-two-chunk-streaming-scene))
         (left (luft:chunk-key-at 63 4))
         (right (luft:chunk-key-at 64 4)))
    (setf (gethash left (luft.render::streaming-scene-loaded scene)) t)
    (let* ((snapshot
             (luft.render::make-streaming-mesh-snapshot
              scene left luft:+mesh-bevel-width+))
           (request
             (make-instance 'luft.render::streaming-mesh-request
                            :key luft.render::+streaming-cohort-production-key+
                            :snapshot snapshot))
           (before
             (prepared-owner-mesh
              (production:perform-production-request request) left)))
      (ok before)
      (ok (luft.render::current-streaming-mesh-request-p scene request))
      (setf (gethash right (luft.render::streaming-scene-loaded scene)) t)
      (ok (not (luft.render::current-streaming-mesh-request-p scene request)))
      ;; The old request remains independently executable, while a current
      ;; oracle observes and closes the newly resident cross-chunk seam.
      (ok (luft::%same-surface-mesh-representation-p
           before
           (prepared-owner-mesh
            (production:perform-production-request request) left)))
      (ok (not
           (canonical-mesh-cohorts-equal-p
            (list before)
            (list (render:mesh-streaming-chunk
                   scene left luft:+mesh-bevel-width+))))))))

(deftest streaming-cell-edits-publish-reversible-chain-material-and-light-state
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (x 4) (y 5) (z 3))
    (luft.render::scene-builder-cell builder x y z)
    (let* ((scene
             (render:make-streaming-scene
              (luft.render::finish-scene-builder builder)))
           (domain (luft:chain-domain (render:scene-solid scene)))
           (cell (luft:make-site domain x y z luft:+cell-extent+ 1))
           (original (render:scene-solid scene))
           (original-light-revision
             (luft.render::scene-authored-light-revision scene)))
      (multiple-value-bind (removal status key)
          (luft.render::edit-streaming-scene-cell scene cell nil)
        (ok (eq :edited status))
        (ok (= key (luft:site-chunk-key cell)))
        (ok (eq luft.render::*terrain-material-placement*
                (luft.render::scene-edit-old-placement removal)))
        (ok (null (luft.render::scene-edit-new-placement removal)))
        (ok (zerop (luft:chain-cell-occupancy-bit
                    (render:scene-solid scene) x y z)))
        (ok (null (nth-value 1
                            (gethash cell
                                     (luft.render::scene-material-cells scene)))))
        (ok (= 1 (luft.render::scene-content-revision scene)))
        (ok (= (1+ original-light-revision)
               (luft.render::scene-authored-light-revision scene)))
        (multiple-value-bind (restoration restoration-status restored-key)
            (luft.render::edit-streaming-scene-cell
             scene cell (luft.render::scene-edit-old-placement removal))
          (declare (ignore restoration))
          (ok (eq :edited restoration-status))
          (ok (= key restored-key))
          (ok (luft:chain= original (render:scene-solid scene)))
          (ok (eq luft.render::*terrain-material-placement*
                  (luft.render::scene-material-placement-at scene cell)))
          (ok (= 2 (luft.render::scene-content-revision scene))))))))

(deftest an-authored-edit-cannot-change-an-existing-worker-snapshot
  (let* ((scene (make-two-chunk-streaming-scene))
         (cell
           (luft:make-site
            (luft:chain-domain (render:scene-solid scene))
            63 4 4 luft:+cell-extent+ 1))
         (left (luft:site-chunk-key cell)))
    (setf (gethash left (luft.render::streaming-scene-loaded scene)) 2)
    (let* ((snapshot
             (luft.render::make-streaming-mesh-snapshot scene left 2))
           (input (luft.render::streaming-mesh-snapshot-input-scene snapshot))
           (request
             (make-instance 'luft.render::streaming-mesh-request
                            :key luft.render::+streaming-cohort-production-key+
                            :snapshot snapshot)))
      (multiple-value-bind (edit status key)
          (luft.render::edit-streaming-scene-cell scene cell nil)
        (declare (ignore edit key))
        (ok (eq :edited status)))
      (ok (not (luft.render::current-streaming-mesh-request-p scene request)))
      (ok (= 1 (luft:chain-cell-occupancy-bit
                (render:scene-solid input) 63 4 4)))
      (ok (eq luft.render::*terrain-material-placement*
              (luft.render::scene-material-placement-at input cell)))
      (ok (zerop (luft:chain-cell-occupancy-bit
                  (render:scene-solid scene) 63 4 4)))
      ;; Executing the obsolete request remains valid and cannot observe the
      ;; replacement material table or authored light generation.
      (ok (prepared-owner-mesh
           (production:perform-production-request request) left)))))

(deftest placing-an-inactive-material-recloses-the-scene-material-program
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (x 4) (y 5) (z 3))
    (luft.render::scene-builder-cell builder x y z)
    (let* ((scene
             (render:make-streaming-scene
              (luft.render::finish-scene-builder builder)))
           (domain (luft:chain-domain (render:scene-solid scene)))
           (cell (luft:make-site domain (1+ x) y z luft:+cell-extent+ 1))
           (old-program (luft.render::scene-material-program scene)))
      (ok (= 7 (luft.render::material-program-summary-count old-program)))
      (multiple-value-bind (edit status key)
          (luft.render::edit-streaming-scene-cell
           scene cell luft.render::*crystal-material-placement*)
        (declare (ignore edit key))
        (ok (eq :edited status)))
      (ok (< (luft.render::material-program-summary-count old-program)
             (luft.render::material-program-summary-count
              (luft.render::scene-material-program scene))))
      (ok (render:make-render-mesh scene)))))

(deftest a-scheduled-cell-edit-is-one-busy-publication-cohort
  (let* ((scene (make-two-chunk-streaming-scene))
         (domain (luft:chain-domain (render:scene-solid scene)))
         (left-cell
           (luft:make-site domain 63 4 4 luft:+cell-extent+ 1))
         (right-cell
           (luft:make-site domain 64 4 4 luft:+cell-extent+ 1))
         (system
           (production:make-single-worker-production-system
            :name "LUFT authored edit publication test")))
    (load-all-streaming-chunks scene 2)
    (unwind-protect
         (multiple-value-bind (edit status key)
             (luft.render::edit-streaming-scene-cell scene left-cell nil)
           (declare (ignore edit))
           (ok (eq :edited status))
           (let ((affected
                   (luft.render::schedule-streaming-scene-edit
                    scene system key 2 nil)))
             (ok affected)
             (ok (equal affected
                        (luft.render::streaming-scene-cohort scene)))
             (multiple-value-bind (next next-status next-key)
                 (luft.render::edit-streaming-scene-cell
                  scene right-cell nil)
               (ok (null next))
               (ok (eq :busy next-status))
               (ok (null next-key)))))
      (production:stop-production-system system))))

(deftest torch-attachments-protect-their-support-and-clearance-cells
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (x 4) (y 5) (z 3))
    (luft.render::scene-builder-cell builder x y z :architecture-p t)
    (luft.render::scene-builder-torch builder x y z :z :high)
    (let* ((scene
             (render:make-streaming-scene
              (luft.render::finish-scene-builder builder)))
           (domain (luft:chain-domain (render:scene-solid scene)))
           (support (luft:make-site domain x y z luft:+cell-extent+ 1))
           (clearance
             (luft:make-site domain x y (1+ z) luft:+cell-extent+ 1)))
      (dolist (edit (list (list support nil)
                          (list clearance
                                luft.render::*terrain-material-placement*)))
        (multiple-value-bind (record status key)
            (luft.render::edit-streaming-scene-cell
             scene (first edit) (second edit))
          (ok (null record))
          (ok (eq :attachment status))
          (ok (null key)))))))

(deftest streaming-temporary-boundaries-use-resident-source-materials
  (labels ((make-scene (include-crystal-p)
             (let ((builder
                     (luft.render::make-scene-builder :horizontal-bits 7)))
               (luft.render::scene-builder-cell builder 63 4 4)
               (when include-crystal-p
                 (luft.render::scene-builder-cell
                  builder 64 4 4
                  :material luft.render::*crystal-material-placement*))
               (luft.render::finish-scene-builder builder))))
    (let* ((full-scene (make-scene t))
           (left-only-scene (make-scene nil))
           (streaming (render:make-streaming-scene full-scene))
           (left (luft:chunk-key-at 63 4))
           (right (luft:chunk-key-at 64 4)))
      ;; The captured occupancy view says the unloaded crystal is air.  The
      ;; temporary X=64 boundary must consequently retain the resident terrain
      ;; cell's stock instead of reverse-probing the global authored union and
      ;; borrowing the crystal's render class.
      (setf (gethash left (luft.render::streaming-scene-loaded streaming)) 2)
      (let* ((output-keys
               (luft.render::streaming-scene-canonical-owner-closure
                streaming (list left)))
             (streamed
               (luft.render::mesh-streaming-snapshot
                (luft.render::make-streaming-region-snapshot
                 streaming output-keys 2)))
             (oracle
               (render:make-render-mesh left-only-scene :bevel-width 2)))
        (ok (equal output-keys (mapcar #'car streamed)))
        (ok (canonical-mesh-cohorts-equal-p
             (mapcar #'cdr streamed)
             (surface-mesh-tree-meshes oracle))))
      ;; Once both source chunks are resident, that temporary face disappears
      ;; and the same producer matches the fully authored occupied union.
      (setf (gethash right (luft.render::streaming-scene-loaded streaming)) 2)
      (let* ((output-keys
               (luft.render::streaming-scene-canonical-owner-closure
                streaming (list left right)))
             (streamed
               (luft.render::mesh-streaming-snapshot
                (luft.render::make-streaming-region-snapshot
                 streaming output-keys 2)))
             (oracle (render:make-render-mesh full-scene :bevel-width 2)))
        (ok (equal output-keys (mapcar #'car streamed)))
        (ok (canonical-mesh-cohorts-equal-p
             (mapcar #'cdr streamed)
             (surface-mesh-tree-meshes oracle)))))))

(deftest a-streaming-residency-cohort-is-staged-atomically
  (let* ((scene (make-two-chunk-streaming-scene))
         (left (luft:chunk-key-at 63 4))
         (right (luft:chunk-key-at 64 4))
         (keys (sort (list left right) #'<)))
    (setf (gethash left (luft.render::streaming-scene-loaded scene)) 2
          (gethash right (luft.render::streaming-scene-loaded scene)) 2
          (luft.render::streaming-scene-cohort scene) keys)
    (let* ((snapshot
             (luft.render::make-streaming-region-snapshot scene keys 2))
           (request
             (make-instance
              'luft.render::streaming-mesh-request
              :key luft.render::+streaming-cohort-production-key+
              :snapshot snapshot))
           (meshes (list (cons (first keys) :first-mesh)
                         (cons (second keys) :second-mesh)))
           (generation
             (luft.render::make-scene-mesh-generation-value
              scene (luft.render::streaming-mesh-snapshot-stamp snapshot)
              (luft.render::streaming-scene-light-generation scene)))
           (result
             (luft.render::%make-streaming-mesh-result meshes generation)))
      (setf (production:production-request-ticket request) 17
            (gethash (first keys)
                     (luft.render::streaming-scene-outstanding scene)) 17
            ;; One superseded owner rejects the whole candidate; no partial
            ;; cohort may enter staging.
            (gethash (second keys)
                     (luft.render::streaming-scene-outstanding scene)) 18)
      (ok (null (luft.render::accept-streaming-mesh-result
                 scene request result)))
      (ok (zerop (hash-table-count
                  (luft.render::streaming-scene-staged scene))))
      (multiple-value-bind (ready ready-generation ready-p)
          (luft.render::ready-streaming-scene-meshes scene)
        (ok (null ready-generation))
        (ok (null ready))
        (ok (null ready-p)))
      (setf (gethash (second keys)
                     (luft.render::streaming-scene-outstanding scene)) 17)
      (ok (luft.render::accept-streaming-mesh-result scene request result))
      (ok (= 2 (hash-table-count
                (luft.render::streaming-scene-staged scene))))
      (multiple-value-bind (ready ready-generation ready-p)
          (luft.render::ready-streaming-scene-meshes scene)
        (ok ready-p)
        (ok (eq generation ready-generation))
        (ok (equal meshes ready))))))

(deftest renderer-publication-invalidates-only-real-temporal-history-changes
  (let* ((mesh (make-renderer-publication-test-mesh))
         (cohort (list (cons 7 mesh))))
    (multiple-value-bind (renderer device) (make-renderer-publication-probe)
      ;; Publication, rather than worker completion or the following encode,
      ;; is the precise boundary that invalidates the history used next frame.
      (setf (luft.render::renderer-history-valid-p renderer) t
            (luft.render::renderer-history-used-p renderer) t)
      (let* ((old-targets
               (luft.render::renderer-target-generation renderer))
             (target-resources
               (luft.render::renderer-target-generation-resources old-targets))
             (old-flame-group
               (luft.render::renderer-flame-bind-group renderer)))
        (luft.render::renderer-update-meshes renderer cohort nil)
        (ok (not (eq old-targets
                     (luft.render::renderer-target-generation renderer))))
        (ok (eq target-resources
                (luft.render::renderer-target-generation-resources
                 (luft.render::renderer-target-generation renderer)))
            "publication retains the exact target cohort, not a clone")
        (ok (= 1 (count old-flame-group
                        (flame-resource-probe-destroyed-resources device)
                        :test #'eq))
            "publication retires only the old cross-product join"))
      (ok (null (luft.render::renderer-history-valid-p renderer)))
      (ok (luft.render::renderer-history-used-p renderer))
      ;; Replacing an already resident owner is still a real publication.
      (setf (luft.render::renderer-history-valid-p renderer) t)
      (luft.render::renderer-update-meshes renderer cohort nil)
      (ok (null (luft.render::renderer-history-valid-p renderer)))
      ;; Asking to remove an owner that is not resident is a true no-op.  It
      ;; does not merely preserve equivalent values: every installed mesh and
      ;; torch-frame resource remains the exact object visible beforehand.
      (setf (luft.render::renderer-history-valid-p renderer) t)
      (let ((table (luft.render::renderer-mesh-slots renderer))
            (slot (gethash 7 (luft.render::renderer-mesh-slots renderer)))
            (order (luft.render::renderer-slot-order renderer))
            (frame-data (luft.render::renderer-torch-frame-data renderer))
            (flame-buffer
              (luft.render::renderer-flame-instance-buffer renderer))
            (flame-group (luft.render::renderer-flame-bind-group renderer))
            (body-group
              (luft.render::renderer-torch-body-bind-group renderer))
            (body-shadow-group
              (luft.render::renderer-torch-body-shadow-bind-group renderer))
            (events (copy-tree (flame-resource-probe-events device))))
        (luft.render::renderer-update-meshes renderer nil (list 99))
        (ok (luft.render::renderer-history-valid-p renderer))
        (ok (luft.render::renderer-history-used-p renderer))
        (ok (eq table (luft.render::renderer-mesh-slots renderer)))
        (ok (eq slot (gethash 7 (luft.render::renderer-mesh-slots renderer))))
        (ok (eq order (luft.render::renderer-slot-order renderer)))
        (ok (equal '(7) (luft.render::renderer-slot-order renderer)))
        (ok (eq frame-data
                (luft.render::renderer-torch-frame-data renderer)))
        (ok (eq flame-buffer
                (luft.render::renderer-flame-instance-buffer renderer)))
        (ok (eq flame-group
                (luft.render::renderer-flame-bind-group renderer)))
        (ok (eq body-group
                (luft.render::renderer-torch-body-bind-group renderer)))
        (ok (eq body-shadow-group
                (luft.render::renderer-torch-body-shadow-bind-group renderer)))
        (ok (equal events (flame-resource-probe-events device)))))))

(deftest renderer-rejects-malformed-updates-before-publication-or-allocation
  (let* ((mesh (make-renderer-publication-test-mesh))
         (cohort (list (cons 7 mesh))))
    (multiple-value-bind (renderer device) (make-renderer-publication-probe)
      (luft.render::renderer-update-meshes renderer cohort nil)
      (setf (luft.render::renderer-history-valid-p renderer) t
            (luft.render::renderer-history-used-p renderer) t)
      (labels ((table-entries ()
                 (sort
                  (loop for key being the hash-keys of
                        (luft.render::renderer-mesh-slots renderer)
                        using (hash-value slot)
                        collect (cons key slot))
                  #'< :key #'car))
               (snapshot ()
                 (list
                  :table (luft.render::renderer-mesh-slots renderer)
                  :entries (table-entries)
                  :order (luft.render::renderer-slot-order renderer)
                  :resources
                  (list
                   (luft.render::renderer-torch-frame-data renderer)
                   (luft.render::renderer-flame-instance-buffer renderer)
                   (luft.render::renderer-flame-bind-group renderer)
                   (luft.render::renderer-torch-body-bind-group renderer)
                   (luft.render::renderer-torch-body-shadow-bind-group renderer))
                  :flame-count
                  (luft.render::renderer-flame-instance-count renderer)
                  :history
                  (list (luft.render::renderer-history-valid-p renderer)
                        (luft.render::renderer-history-used-p renderer))
                  :events (copy-tree (flame-resource-probe-events device))))
               (same-entry-identities-p (before after)
                 (and (= (length before) (length after))
                      (loop for (before-key . before-slot) in before
                            for (after-key . after-slot) in after
                            always (and (eql before-key after-key)
                                        (eq before-slot after-slot)))))
               (assert-preserved (before name)
                 (let ((after (snapshot)))
                   (ok (equal (getf before :events) (getf after :events))
                       (format nil "~A allocates no GPU resources" name))
                   (ok (eq (getf before :table) (getf after :table))
                       (format nil "~A preserves the slot table" name))
                   (ok (same-entry-identities-p
                        (getf before :entries) (getf after :entries))
                       (format nil "~A preserves installed slot identities" name))
                   (ok (eq (getf before :order) (getf after :order))
                       (format nil "~A preserves slot order identity" name))
                   (ok (every #'eq
                              (getf before :resources)
                              (getf after :resources))
                       (format nil "~A preserves torch resource identities" name))
                   (ok (eql (getf before :flame-count)
                            (getf after :flame-count))
                       (format nil "~A preserves the torch population" name))
                   (ok (equal (getf before :history) (getf after :history))
                       (format nil "~A preserves temporal history" name)))))
        (dolist (case
                  (list
                   (list :duplicate-candidates
                         (list (cons 7 mesh) (cons 7 mesh)) nil)
                   (list :candidate-non-chunk-key
                         (list (cons -1 mesh)) nil)
                   (list :candidate-nonnumeric-key
                         (list (cons :owner mesh)) nil)
                   (list :removal-non-chunk-key nil (list -1))
                   (list :removal-nonnumeric-key nil (list :owner))
                   (list :duplicate-removals nil (list 99 99))
                   (list :candidate-removal-overlap
                         (list (cons 7 mesh)) (list 7))))
          (destructuring-bind (name candidates removals) case
            (let ((before (snapshot)))
              (ok (signals
                   (luft.render::renderer-update-meshes
                    renderer candidates removals)
                   'error)
                  (format nil "~A is rejected" name))
              (assert-preserved before name))))))))

(deftest renderer-publication-rolls-back-after-complete-resource-staging
  (let* ((mesh (make-renderer-publication-test-mesh))
         (first (list (cons 7 mesh))))
    (multiple-value-bind (renderer device) (make-renderer-publication-probe)
      (luft.render::renderer-update-meshes renderer first nil)
      (setf (luft.render::renderer-history-valid-p renderer) t
            (luft.render::renderer-history-used-p renderer) t)
      (let* ((publication (luft.render::renderer-publication renderer))
             (target-generation
               (luft.render::renderer-target-generation renderer))
             (table (luft.render::renderer-mesh-slots renderer))
             (order (luft.render::renderer-slot-order renderer))
             (frame-data (luft.render::renderer-torch-frame-data renderer))
             (flame-buffer
               (luft.render::renderer-flame-instance-buffer renderer))
             (flame-group (luft.render::renderer-flame-bind-group renderer))
             (body-group
               (luft.render::renderer-torch-body-bind-group renderer))
             (body-shadow-group
               (luft.render::renderer-torch-body-shadow-bind-group renderer))
             (events-before
               (copy-list (flame-resource-probe-events device))))
        ;; This hook runs after the fresh table, order, slots, and every global
        ;; attachment resource exist, but before the sole publication pointer
        ;; is written.  A CPU condition here is the old table-rehash/order
        ;; allocation failure class made deterministic.
        (let ((luft.render::*renderer-publication-precommit-hook*
                (lambda (owner candidate)
                  (declare (ignore owner candidate))
                  (error "Injected renderer precommit failure."))))
          (ok (signals
               (luft.render::renderer-update-meshes
                renderer (list (cons 8 mesh)) nil)
               'error)))
        (ok (eq publication (luft.render::renderer-publication renderer)))
        (ok (eq target-generation
                (luft.render::renderer-target-generation renderer)))
        (ok (eq table (luft.render::renderer-mesh-slots renderer)))
        (ok (eq order (luft.render::renderer-slot-order renderer)))
        (ok (eq frame-data
                (luft.render::renderer-torch-frame-data renderer)))
        (ok (eq flame-buffer
                (luft.render::renderer-flame-instance-buffer renderer)))
        (ok (eq flame-group
                (luft.render::renderer-flame-bind-group renderer)))
        (ok (eq body-group
                (luft.render::renderer-torch-body-bind-group renderer)))
        (ok (eq body-shadow-group
                (luft.render::renderer-torch-body-shadow-bind-group renderer)))
        (ok (equal '(7) (luft.render::renderer-slot-order renderer)))
        (ok (null (gethash 8 (luft.render::renderer-mesh-slots renderer))))
        (ok (luft.render::renderer-history-valid-p renderer))
        (ok (luft.render::renderer-history-used-p renderer))
        (let* ((events-after (flame-resource-probe-events device))
               (new-event-count (- (length events-after)
                                   (length events-before)))
               (events (subseq events-after 0 new-event-count)))
          (ok (= (count :create-buffer events :key #'car)
                 (count '(:destroy :buffer) events :test #'equal))
              "every staged buffer is retired")
          (ok (= (count :create-bind-group events :key #'car)
                 (count '(:destroy :bind-group) events :test #'equal))
              "every staged bind group is retired"))))))

(deftest renderer-publication-never-retains-stale-scene-generation
  (multiple-value-bind (mesh generation)
      (make-renderer-publication-test-mesh)
    (multiple-value-bind (renderer device) (make-renderer-publication-probe)
      (declare (ignore device))
      (render:renderer-set-mesh renderer 0 mesh :scene-generation generation)
      (let ((published
              (luft.render::renderer-publication-scene-generation
               (luft.render::renderer-publication renderer))))
        (ok (luft.render::scene-mesh-generation-result-stamp=
             generation published))
        (ok (= 1
               (length
                (luft.render::scene-mesh-generation-mesh-manifest
                 published)))))
      ;; Any visible cohort change without exact generation evidence clears
      ;; provenance instead of retaining the old mesh/light claim.
      (render:renderer-set-mesh renderer 1 mesh)
      (ok (null
           (luft.render::renderer-publication-scene-generation
            (luft.render::renderer-publication renderer))))
      ;; Once a publication has honestly discarded its provenance, a partial
      ;; generation cannot retroactively certify an unprovenanced retained
      ;; slot.  Remove the diagnostic cohort, then publish from exact evidence.
      (render:renderer-clear-meshes renderer)
      (render:renderer-set-mesh renderer 0 mesh :scene-generation generation)
      (ok (luft.render::scene-mesh-generation-result-stamp=
           generation
           (luft.render::renderer-publication-scene-generation
            (luft.render::renderer-publication renderer))))
      (render:renderer-clear-meshes renderer)
      (ok (null
           (luft.render::renderer-publication-scene-generation
            (luft.render::renderer-publication renderer)))))))

(deftest renderer-rejects-foreign-or-mutated-geometry-provenance-preallocation
  (labels ((rejected-before-allocation (renderer device mesh generation)
             (let ((events
                     (copy-list (flame-resource-probe-events device))))
               (ok (signals
                    (render:renderer-set-mesh
                     renderer 0 mesh :scene-generation generation)
                    'error))
               (ok (equal events (flame-resource-probe-events device)))
               (ok (null (luft.render::renderer-slot-order renderer))))))
    ;; With no torch, all widths intentionally share the exact base light
    ;; field. Geometry identity must independently reject width four and a
    ;; separately compiled same-width whole-domain topology.
    (let ((builder (luft.render::make-scene-builder :horizontal-bits 5)))
      (luft.render::scene-builder-cell builder 4 4 4 :architecture-p t)
      (let ((scene (luft.render::finish-scene-builder builder)))
        (multiple-value-bind (width-one width-one-generation)
            (render:make-render-mesh scene :bevel-width 1)
          (multiple-value-bind (width-four width-four-generation)
              (render:make-render-mesh scene :bevel-width 4)
            (declare (ignore width-four-generation))
            (multiple-value-bind (same-width-foreign foreign-generation)
                (render:make-whole-domain-diagnostic-mesh
                 scene :bevel-width 1)
              (declare (ignore foreign-generation))
              (multiple-value-bind (renderer device)
                  (make-renderer-publication-probe)
                (ok (eq
                     (luft:surface-mesh-voxel-light width-one)
                     (luft:surface-mesh-voxel-light width-four)))
                (rejected-before-allocation
                 renderer device width-four width-one-generation)
                (rejected-before-allocation
                 renderer device same-width-foreign width-one-generation)
                ;; Companion shape is part of the exact tree witness, even
                ;; when every existing geometry object and light field is EQ.
                (let ((companions
                        (luft:surface-mesh-companions width-one)))
                  (unwind-protect
                       (progn
                         (setf (luft:surface-mesh-companions width-one)
                               (append companions (list same-width-foreign)))
                         (rejected-before-allocation
                          renderer device width-one width-one-generation))
                    (setf (luft:surface-mesh-companions width-one)
                          companions)))))))))
    ;; Packed body/flame scalar content is copied into the witness as well.
    (let ((builder (luft.render::make-scene-builder :horizontal-bits 5)))
      (luft.render::scene-builder-cell builder 4 4 4 :architecture-p t)
      (luft.render::scene-builder-torch builder 4 4 4 :z :high)
      (multiple-value-bind (mesh generation)
          (render:make-render-mesh
           (luft.render::finish-scene-builder builder) :bevel-width 1)
        (multiple-value-bind (renderer device)
            (make-renderer-publication-probe)
          (let* ((frame-owner
                   (find-if
                    (lambda (surface)
                      (luft:surface-mesh-attachments surface))
                    (surface-mesh-tree-meshes mesh)))
                 (frame
                   (first (luft:surface-mesh-attachments frame-owner)))
                 (origin-x (aref frame 0)))
            (unwind-protect
                 (progn
                   (setf (aref frame 0) (+ origin-x 0.125f0))
                   (rejected-before-allocation
                    renderer device mesh generation))
              (setf (aref frame 0) origin-x))))))))

(deftest renderer-publication-merges-exact-partial-slot-provenance
  (let* ((scene (make-two-chunk-streaming-scene))
         (source-keys (streaming-store-keys scene)))
    (load-all-streaming-chunks scene 2)
    (let* ((all-output-keys
             (luft.render::streaming-scene-canonical-owner-closure
              scene source-keys))
           (all-snapshot
             (luft.render::make-streaming-region-snapshot
              scene all-output-keys 2)))
      (multiple-value-bind (all-owners census diagnostics all-generation)
          (luft.render::mesh-streaming-snapshot all-snapshot)
        (declare (ignore census diagnostics))
        (multiple-value-bind (swap-renderer swap-device)
            (make-renderer-publication-probe)
          (let* ((first (first all-owners))
                 (second (second all-owners))
                 (swapped
                   (append
                    (list (cons (car first) (cdr second))
                          (cons (car second) (cdr first)))
                    (cddr all-owners)))
                 (events
                   (copy-list
                    (flame-resource-probe-events swap-device))))
            (ok (signals
                 (render:renderer-set-meshes
                  swap-renderer swapped :scene-generation all-generation)
                 'error))
            (ok (equal events
                       (flame-resource-probe-events swap-device)))))
        (multiple-value-bind (renderer device)
            (make-renderer-publication-probe)
          (declare (ignore device))
          (render:renderer-set-meshes
           renderer all-owners :scene-generation all-generation)
          (let* ((old-publication
                   (luft.render::renderer-publication renderer))
                 (retained-key (second all-output-keys))
                 (retained-slot
                   (gethash retained-key
                            (luft.render::renderer-mesh-slots renderer)))
                 (changed-key (first all-output-keys))
                 (partial-snapshot
                   (luft.render::make-streaming-region-snapshot
                    scene (list changed-key) 2)))
            (multiple-value-bind
                  (partial-owners partial-census partial-diagnostics
                   partial-generation)
                (luft.render::mesh-streaming-snapshot partial-snapshot)
              (declare (ignore partial-census partial-diagnostics))
              (render:renderer-set-meshes
               renderer partial-owners :scene-generation partial-generation)
              (let* ((publication
                       (luft.render::renderer-publication renderer))
                     (merged
                       (luft.render::renderer-publication-scene-generation
                        publication)))
                (ok (not (eq old-publication publication)))
                (ok (not (eq partial-generation merged)))
                (ok (luft.render::scene-mesh-generation-result-stamp=
                     partial-generation merged))
                (ok (= (length all-output-keys)
                       (length
                        (luft.render::scene-mesh-generation-mesh-manifest
                         merged))))
                (ok (= (length all-output-keys)
                       (length
                        (luft.render::scene-mesh-generation-slot-provenances
                         merged))))
                (ok (eq retained-slot
                        (gethash
                         retained-key
                         (luft.render::renderer-mesh-slots renderer))))
                ;; A live renderer reconstruction consumes the aggregate's
                ;; exact keyed trees and retains that aggregate object itself.
                (multiple-value-bind (rebuilt rebuilt-device)
                    (make-renderer-publication-probe)
                  (declare (ignore rebuilt-device))
                  (render:renderer-set-meshes
                   rebuilt
                   (mapcar
                    (lambda (key)
                      (cons
                       key
                       (luft.render::mesh-slot-prepared-mesh
                        (gethash
                         key (luft.render::renderer-mesh-slots renderer)))))
                    (luft.render::renderer-slot-order renderer))
                   :scene-generation merged)
                  (ok (eq merged
                          (luft.render::renderer-publication-scene-generation
                           (luft.render::renderer-publication rebuilt)))))))))))))

(deftest streaming-publication-failure-preserves-both-generations-and-retry
  (multiple-value-bind (scene support near far)
      (make-streaming-retarget-light-test-scene)
    (declare (ignore near far))
    (setf (gethash support (luft.render::streaming-scene-loaded scene)) 1
          (luft.render::streaming-scene-geometry-policy-signature scene)
          (luft.render::material-bevel-profile-geometry-signature nil))
    (let* ((keys
             (luft.render::streaming-scene-canonical-owner-closure
              scene (list support)))
           (old-snapshot
             (luft.render::make-streaming-region-snapshot scene keys 1)))
      (multiple-value-bind (old-meshes old-census old-diagnostics
                            old-generation)
          (luft.render::mesh-streaming-snapshot old-snapshot)
        (declare (ignore old-census old-diagnostics))
        (multiple-value-bind (renderer device)
            (make-renderer-publication-probe)
          (declare (ignore device))
          (render:renderer-set-meshes
           renderer old-meshes :scene-generation old-generation)
          (setf (luft.render::streaming-scene-light-generation scene)
                (render:scene-mesh-generation-light-generation
                 old-generation))
          (let ((old-publication
                  (luft.render::renderer-publication renderer))
                (old-light
                  (luft.render::streaming-scene-light-generation scene)))
            (setf (gethash support
                           (luft.render::streaming-scene-loaded scene)) 4)
            (let ((new-snapshot
                    (luft.render::make-streaming-region-snapshot
                     scene keys 4)))
              (multiple-value-bind (new-meshes new-census new-diagnostics
                                    new-generation)
                  (luft.render::mesh-streaming-snapshot new-snapshot)
                (declare (ignore new-census new-diagnostics))
                (let ((request
                        (make-instance
                         'luft.render::streaming-mesh-request
                         :key luft.render::+streaming-cohort-production-key+
                         :snapshot new-snapshot)))
                  (setf (luft.render::streaming-scene-cohort scene) keys
                        (luft.render::streaming-scene-removals scene) nil
                        (luft.render::streaming-scene-staged-generation scene)
                        new-generation)
                  (dolist (entry new-meshes)
                    (setf (gethash
                           (car entry)
                           (luft.render::streaming-scene-staged scene))
                          (cons request (cdr entry))))
                  (let ((luft.render::*renderer-publication-precommit-hook*
                          (lambda (owner publication)
                            (declare (ignore owner publication))
                            (error "Injected streaming publication failure."))))
                    (ok (signals
                         (luft.render::publish-ready-streaming-scene
                          scene renderer)
                         'error)))
                  (ok (eq old-publication
                          (luft.render::renderer-publication renderer)))
                  (ok (eq old-light
                          (luft.render::streaming-scene-light-generation
                           scene)))
                  (ok (equal keys
                             (luft.render::streaming-scene-cohort scene)))
                  (ok (eq new-generation
                          (luft.render::streaming-scene-staged-generation
                           scene)))
                  (ok (= (length keys)
                         (hash-table-count
                          (luft.render::streaming-scene-staged scene))))
                  (ok (= (length keys)
                         (luft.render::publish-ready-streaming-scene
                          scene renderer)))
                  (ok (null (luft.render::streaming-scene-cohort scene)))
                  (ok (zerop
                       (hash-table-count
                        (luft.render::streaming-scene-staged scene))))
                  (ok (eq
                       (render:scene-mesh-generation-light-generation
                        new-generation)
                       (luft.render::streaming-scene-light-generation scene)))
                  (let* ((published
                           (luft.render::renderer-publication-scene-generation
                            (luft.render::renderer-publication renderer)))
                         (field
                           (luft.render::realized-light-generation-field
                            (render:scene-mesh-generation-light-generation
                             published))))
                    (ok (luft.render::scene-mesh-generation-result-stamp=
                         new-generation published))
                    (ok (loop for key in
                              (luft.render::renderer-slot-order renderer)
                              always
                              (luft.render::surface-mesh-tree-uses-light-field-p
                               (luft.render::mesh-slot-mesh
                                (gethash
                                 key
                                 (luft.render::renderer-mesh-slots renderer)))
                               field)))))))))))))

(deftest failed-population-depth-join-preserves-both-installed-generations
  (let ((mesh (make-renderer-publication-test-mesh)))
    (multiple-value-bind (renderer device) (make-renderer-publication-probe)
      (luft.render::renderer-update-meshes
       renderer (list (cons 7 mesh)) nil)
      (let ((publication (luft.render::renderer-publication renderer))
            (targets (luft.render::renderer-target-generation renderer))
            (flame-group (luft.render::renderer-flame-bind-group renderer))
            (created-before
              (copy-list (flame-resource-probe-created-resources device))))
        (setf (luft.render::renderer-history-valid-p renderer) t
              (flame-resource-probe-fail-bind-group-label device)
              "luft post-temporal torch flames")
        (ok (signals
             (luft.render::renderer-update-meshes
              renderer (list (cons 8 mesh)) nil)
             'error))
        (ok (eq publication (luft.render::renderer-publication renderer)))
        (ok (eq targets
                (luft.render::renderer-target-generation renderer)))
        (ok (eq flame-group (luft.render::renderer-flame-bind-group renderer)))
        (ok (luft.render::renderer-history-valid-p renderer))
        (ok (not (member
                  flame-group
                  (flame-resource-probe-destroyed-resources device)
                  :test #'eq)))
        (ok (every
             (lambda (resource)
               (or (eq resource flame-group)
                   (not (member
                         resource
                         (flame-resource-probe-destroyed-resources device)
                         :test #'eq))))
             (renderer-target-generation-resources targets))
            "join rollback cannot retire borrowed target resources")
        (let ((staged
                (set-difference
                 (flame-resource-probe-created-resources device)
                 created-before :test #'eq)))
          (ok staged)
          (ok (every-probe-resource-destroyed-once-p staged device)))))))

(deftest renderer-grows-an-append-compatible-material-abi-atomically
  (let* ((members
           (copy-seq
            (luv.domains:identity-vocabulary-members
             luft.render::*surface-assembly-vocabulary*)))
         (luft.render::*surface-assembly-vocabulary*
           (luv.domains:make-identity-vocabulary-domain
            :members members :limit #x1000))
         (mesh (make-renderer-publication-test-mesh))
         (prefix-prepared (luft.render::prepare-render-mesh mesh)))
    (multiple-value-bind (renderer device) (make-renderer-publication-probe)
      (luft.render::renderer-update-meshes
       renderer (list (cons 7 prefix-prepared)) nil)
      (let* ((old-publication (luft.render::renderer-publication renderer))
             (old-material-buffer
               (luft.render::renderer-material-buffer renderer))
             (old-slot (gethash 7 (luft.render::renderer-mesh-slots renderer)))
             (old-count
               (luft.render::renderer-material-descriptor-count renderer)))
        ;; This is a real append with a valid closed shader kernel and a
        ;; distinct semantic identity.  A raw one-cell oracle below references
        ;; the newly appended offset exactly, so descriptor growth is tested by
        ;; a real packed stock rather than an unused vocabulary suffix.
        (let* ((new-assembly
                 (luft.render::intern-surface-assembly
                  :face luft.render::*grass-reading* :kernel :stone
                  :name :renderer-abi-growth-probe))
               (new-stock
                 (luft.render::surface-assembly-offset new-assembly))
               (domain (luft:make-world-domain :horizontal-bits 4))
               (builder (luft:make-chain-builder domain))
               (site (luft:make-site domain 4 4 4 luft:+cell-extent+ 1))
               (solid
                 (progn
                   (luft:chain-builder-add-site builder site)
                   (luft:finish-chain-builder builder)))
               (new-mesh
                 (luft:make-surface-mesh
                  solid :stock-function (constantly new-stock)
                  :chamfer-stock-function
                  (lambda (stocks)
                    (declare (ignore stocks))
                    new-stock)))
               (prepared (luft.render::prepare-render-mesh new-mesh))
               (population
                 (luft.render::prepared-render-mesh-population prepared))
               (packed-stocks
                 (loop with words =
                       (luft.render::render-population-instance-words
                        population)
                       for offset from 3 below (length words)
                         by luft:+mesh-instance-word-count+
                       collect
                       (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                            (aref words offset)))))
          (ok (= old-count new-stock))
          (ok packed-stocks)
          (ok (= old-count (reduce #'max packed-stocks)))
          (ok (> (luft.render::render-population-material-descriptor-count
                  population)
                 old-count))
          ;; Fail after the candidate material buffer, retained/replacement
          ;; slots, table/order, and torch resources all exist.  The renderer
          ;; must retain the old complete generation and retire every staged
          ;; resource, including the candidate descriptor buffer.
          (let* ((events-before
                   (copy-list (flame-resource-probe-events device)))
                 (luft.render::*renderer-publication-precommit-hook*
                   (lambda (owner candidate)
                     (declare (ignore owner candidate))
                     (error "Injected material ABI publication failure."))))
            (ok (signals
                 (luft.render::renderer-update-meshes
                  renderer (list (cons 8 prepared)) nil)
                 'error))
            (ok (eq old-publication
                    (luft.render::renderer-publication renderer)))
            (ok (eq old-material-buffer
                    (luft.render::renderer-material-buffer renderer)))
            (ok (eq old-slot
                    (gethash 7 (luft.render::renderer-mesh-slots renderer))))
            (ok (null (gethash 8 (luft.render::renderer-mesh-slots renderer))))
            (let* ((events-after (flame-resource-probe-events device))
                   (new-event-count
                     (- (length events-after) (length events-before)))
                   (events (subseq events-after 0 new-event-count)))
              (ok (= (count :create-buffer events :key #'car)
                     (count '(:destroy :buffer) events :test #'equal)))
              (ok (= (count :create-bind-group events :key #'car)
                     (count '(:destroy :bind-group) events :test #'equal)))))
          (let ((requested
                  (luft.render::renderer-update-meshes
                   renderer (list (cons 8 prepared)) nil)))
            (ok (equal '(8) (mapcar #'car requested))))
          (ok (not (eq old-publication
                       (luft.render::renderer-publication renderer))))
          (ok (not (eq old-material-buffer
                       (luft.render::renderer-material-buffer renderer))))
          (ok (> (luft.render::renderer-material-descriptor-count renderer)
                 old-count))
          (ok (= 2 (hash-table-count
                    (luft.render::renderer-mesh-slots renderer))))
          ;; Retained slots have bind groups over a concrete material buffer;
          ;; growth therefore re-realizes them in the candidate generation.
          (ok (not (eq old-slot
                       (gethash 7
                                (luft.render::renderer-mesh-slots renderer)))))
          (let ((material-buffer
                  (luft.render::renderer-material-buffer renderer)))
            (dolist (key '(7 8))
              (let* ((slot
                       (gethash key
                                (luft.render::renderer-mesh-slots renderer)))
                     (resident (luft.render::mesh-slot-resident slot))
                     (bind-group
                       (luft.render::resident-population-bind-group resident)))
                (ok (eq material-buffer
                        (probe-bind-group-resource bind-group 3))))))
          (ok (member '(:destroy :material-buffer)
                      (flame-resource-probe-events device) :test #'equal)))))
    ;; A renderer frozen after an append owns a strict descriptor superset and
    ;; may safely accept an older prepared prefix: append-only offsets did not
    ;; move, and no descriptor-buffer replacement is necessary.
    (multiple-value-bind (renderer device) (make-renderer-publication-probe)
      (let ((material-buffer (luft.render::renderer-material-buffer renderer)))
        (luft.render::renderer-update-meshes
         renderer (list (cons 7 prefix-prepared)) nil)
        (ok (= 1 (hash-table-count
                  (luft.render::renderer-mesh-slots renderer))))
        (ok (eq material-buffer
                (luft.render::renderer-material-buffer renderer)))
        (ok (not (member '(:destroy :material-buffer)
                         (flame-resource-probe-events device)
                         :test #'equal)))))))

(deftest renderer-rejects-a-divergent-material-abi-before-gpu-allocation
  (let* ((baseline-members
           (copy-seq
            (luv.domains:identity-vocabulary-members
             luft.render::*surface-assembly-vocabulary*)))
         (luft.render::*surface-assembly-vocabulary*
           (luv.domains:make-identity-vocabulary-domain
            :members baseline-members :limit #x1000))
         (mesh (make-renderer-publication-test-mesh)))
    (multiple-value-bind (renderer device) (make-renderer-publication-probe)
      (let* ((divergent-members (copy-seq baseline-members))
             (crystal-index
               (position luft.render::*crystal-surface* divergent-members
                         :test #'eq))
             (prepared nil))
        (rotatef (aref divergent-members 0)
                 (aref divergent-members crystal-index))
        (let ((luft.render::*surface-assembly-vocabulary*
                (luv.domains:make-identity-vocabulary-domain
                 :members divergent-members :limit #x1000)))
          (setf prepared (luft.render::prepare-render-mesh mesh)))
        (let ((publication (luft.render::renderer-publication renderer))
              (events (copy-list (flame-resource-probe-events device))))
          (ok (signals
               (luft.render::renderer-update-meshes
                renderer (list (cons 7 prepared)) nil)
               'error))
          (ok (eq publication (luft.render::renderer-publication renderer)))
          (ok (zerop (hash-table-count
                      (luft.render::renderer-mesh-slots renderer))))
          (ok (equal events (flame-resource-probe-events device))
              "a non-prefix material ABI allocates no GPU resource"))))))

(deftest a-streaming-window-is-bounded-around-its-focus
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 8)))
    (dotimes (chunk-x 3)
      (dotimes (chunk-y 3)
        (luft.render::scene-builder-cell
         builder (* chunk-x luft:+chunk-size+)
         (* chunk-y luft:+chunk-size+) 0)))
    (let ((scene
            (render:make-streaming-scene
             (luft.render::finish-scene-builder builder)
             :residency-radius 1)))
      (ok (= 4 (length (luft.render::streaming-scene-keys-near scene 0 0))))
      (ok (= 9 (length (luft.render::streaming-scene-keys-near scene 1 1))))
      (ok (zerop (hash-table-count
                  (luft.render::streaming-scene-loaded scene)))))))

(deftest a-streaming-camera-outside-the-world-retains-the-boundary-window
  (let* ((scene
           (render:make-streaming-scene
            (make-streaming-material-seam-test-scene)
            :residency-radius 1))
         (system
           (production:make-single-worker-production-system
            :name "LUFT boundary focus clamp test")))
    (unwind-protect
         (progn
           ;; The camera is just below the finite world while looking at the
           ;; X=64 seam.  Chunk keys are unsigned, so using Y=-1 directly
           ;; would wrap the focus to chunk-grid Y=4095 and load nothing.
           (ok (luft.render::retarget-streaming-scene
                scene system 2 72.5d0 -1.0d0))
           (ok (equal '(1 . 0) (luft.render::streaming-scene-focus scene)))
           (ok (equal
                (streaming-store-keys scene)
                (sort
                 (loop for key being the hash-keys of
                       (luft.render::streaming-scene-loaded scene)
                       collect key)
                 #'<)))
           (ok (plusp
                (hash-table-count
                 (luft.render::streaming-scene-outstanding scene)))))
      (production:stop-production-system system))))

(deftest a-streaming-residency-window-has-one-geometric-width
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 8)))
    (dotimes (chunk-x 3)
      (dotimes (chunk-y 3)
        (luft.render::scene-builder-cell
         builder (* chunk-x luft:+chunk-size+)
         (* chunk-y luft:+chunk-size+) 0)))
    (let* ((scene
             (render:make-streaming-scene
              (luft.render::finish-scene-builder builder)
              :residency-radius 1))
           (system
             (production:make-single-worker-production-system
              :name "LUFT uniform streaming width test")))
      (unwind-protect
           (progn
             (ok (luft.render::retarget-streaming-scene
                  scene system 2 64 64))
             (ok (= 9 (hash-table-count
                        (luft.render::streaming-scene-loaded scene))))
             (ok (loop for width being the hash-values of
                       (luft.render::streaming-scene-loaded scene)
                       always (= width 2))))
        (production:stop-production-system system)))))

(deftest static-and-streaming-uniform-meshes-have-the-same-triangle-multiset
  (let ((scene (make-streaming-material-seam-test-scene)))
    (dolist (width '(1 2 3 4))
      (let* ((streaming (render:make-streaming-scene scene))
             (keys (progn
                     (load-all-streaming-chunks streaming width)
                     (streaming-store-keys streaming)))
             (whole
               (luft.render::%make-scene-union-mesh
                scene (render:scene-solid scene) width nil
                (luft.render::make-compiled-material-chamfer-stock-function
                 (luft.render::scene-material-program scene))))
             (static (render:make-render-mesh scene :bevel-width width)))
        (multiple-value-bind (regional regional-census regional-diagnostics)
            (luft.render::make-scene-regional-meshes scene width)
          (declare (ignore regional-census regional-diagnostics))
          (multiple-value-bind (streamed streamed-census streamed-diagnostics)
              (luft.render::mesh-streaming-snapshot
               (luft.render::make-streaming-region-snapshot
                streaming keys width))
            (declare (ignore streamed-census streamed-diagnostics))
            (let ((static-meshes (surface-mesh-tree-meshes static))
                  (regional-meshes (mapcar #'cdr regional))
                  (streamed-meshes (mapcar #'cdr streamed)))
              (ok (equal
                   (luft.render::streaming-scene-canonical-owner-closure
                    streaming keys)
                   (mapcar #'car regional)))
              (ok (equal keys (mapcar #'car streamed)))
              (ok (luft::%mesh-closed-p whole))
              (ok (luft::%meshes-closed-p regional-meshes))
              (ok (luft::%meshes-closed-p streamed-meshes))
              (ok (canonical-mesh-cohorts-equal-p
                   (list whole) static-meshes))
              (ok (canonical-mesh-cohorts-equal-p
                   (list whole) regional-meshes))
              (ok (canonical-mesh-cohorts-equal-p
                   (list whole) streamed-meshes)))))))))

(deftest streaming-torch-frames-see-nonpublished-seam-context
  (let* ((scene (make-streaming-torch-seam-test-scene))
         (attachment (aref (render:scene-torches scene) 0))
         (support-key
           (luft:site-chunk-key
            (luft.render::torch-attachment-support-cell attachment)))
         (profile
           (render:make-material-bevel-profile
            :terrain-width 4 :architecture-width 1
            :crystal-width 4 :contact-width 2))
         (modes `((:uniform-1 1 nil)
                  (:uniform-2 2 nil)
                  (:uniform-4 4 nil)
                  (:material 1 ,profile))))
    (labels ((resolve-on (meshes)
               (handler-case
                   (luft:resolve-surface-attachment-frame
                    meshes (luft.render::torch-attachment-face attachment)
                    :u (luft.render::torch-attachment-chart-u attachment)
                    :v (luft.render::torch-attachment-chart-v attachment))
                 (error () nil)))
             (edge-primitive-p (frame)
               (and frame
                    (some (lambda (kind)
                            (member kind '(:band :junction)))
                          (luft:surface-attachment-frame-primitive-kinds
                           frame)))))
      (dolist (mode modes)
        (destructuring-bind (name width bevel-profile) mode
          (declare (ignore name))
          (let* ((streaming (render:make-streaming-scene scene))
                 (keys
                   (progn
                     (load-all-streaming-chunks streaming width)
                     (streaming-store-keys streaming))))
            (multiple-value-bind (static static-census static-diagnostics)
                (luft.render::make-scene-regional-meshes
                 scene width bevel-profile)
              (declare (ignore static-census static-diagnostics))
              (multiple-value-bind
                    (streamed streamed-census streamed-diagnostics)
                  (luft.render::mesh-streaming-snapshot
                   (luft.render::make-streaming-region-snapshot
                    streaming (list support-key) width bevel-profile))
                (declare (ignore streamed-census streamed-diagnostics))
                (let* ((streamed-entry (assoc support-key streamed :test #'eql))
                       (static-entry (assoc support-key static :test #'eql))
                       (streamed-frames
                         (luft:surface-mesh-attachments (cdr streamed-entry)))
                       (static-frames
                         (luft:surface-mesh-attachments (cdr static-entry)))
                       (whole-frame (resolve-on (mapcar #'cdr static)))
                       (context-frame
                         (some (lambda (entry)
                                 (unless (= support-key (car entry))
                                   (let ((frame (resolve-on (cdr entry))))
                                     (and (edge-primitive-p frame) frame))))
                               static)))
                  ;; Context realization may inform the result but must never
                  ;; expand the worker's output/publication owner set.
                  (ok (equal (list support-key) (mapcar #'car streamed)))
                  (ok (= 2 (length keys)))
                  (ok (= 1 (length streamed-frames)))
                  (ok (= 1 (length static-frames)))
                  (ok (loop for entry in static
                            always
                            (or (= support-key (car entry))
                                (null (luft:surface-mesh-attachments
                                       (cdr entry))))))
                  ;; The top-edge chart point is carried by a band/fan whose
                  ;; canonical anchor, and therefore primitive owner, is the
                  ;; neighboring X chunk.  That owner remains context-only.
                  (ok context-frame)
                  (ok (edge-primitive-p whole-frame))
                  (ok (equalp (first streamed-frames)
                              (first static-frames)))
                  (ok (equalp (subseq (first streamed-frames) 4 7)
                              (luft:surface-attachment-frame-normal
                               whole-frame)))
                  (ok (equalp (subseq (first streamed-frames) 8 11)
                              (luft:surface-attachment-frame-tangent
                               whole-frame))))))))))))

(deftest virtual-owner-closure-does-not-admit-unloaded-torches
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 7))
         (scene
           (progn
             (luft.render::scene-builder-cell builder 63 8 4 :architecture-p t)
             (luft.render::scene-builder-cell builder 64 8 4 :architecture-p t)
             (luft.render::scene-builder-torch
              builder 64 8 4 :z :high :u 0.0 :v 0.0)
             (luft.render::finish-scene-builder builder)))
         (streaming (render:make-streaming-scene scene))
         (left-key (luft:chunk-key-at 63 8))
         (right-key (luft:chunk-key-at 64 8))
         (output-keys
           (luft.render::streaming-scene-canonical-owner-closure
            streaming (list left-key)))
         (frame-count
           (lambda (owners)
             (loop for entry in owners
                   sum (length
                        (luft:surface-mesh-attachments (cdr entry)))))))
    ;; RIGHT-KEY is published as the left source's canonical high-side owner,
    ;; but its authored support source is not resident yet.
    (setf (gethash left-key (luft.render::streaming-scene-loaded streaming)) 2)
    (let* ((snapshot
             (luft.render::make-streaming-region-snapshot
              streaming output-keys 2))
           (owners (luft.render::mesh-streaming-snapshot snapshot)))
      (ok (member right-key output-keys :test #'eql))
      (ok (equal (list left-key)
                 (luft.render::streaming-mesh-snapshot-resident-source-keys
                  snapshot)))
      (ok (zerop (funcall frame-count owners))))
    ;; Once the support source is logically resident, exactly one shared
    ;; body/flame frame is attached to its own canonical owner.
    (setf (gethash right-key (luft.render::streaming-scene-loaded streaming)) 2)
    (let* ((snapshot
             (luft.render::make-streaming-region-snapshot
              streaming output-keys 2))
           (owners (luft.render::mesh-streaming-snapshot snapshot))
           (support (assoc right-key owners :test #'eql)))
      (ok (member right-key
                  (luft.render::streaming-mesh-snapshot-resident-source-keys
                   snapshot)
                  :test #'eql))
      (ok (= 1 (funcall frame-count owners)))
      (ok (= 1 (length (luft:surface-mesh-attachments (cdr support))))))))

(deftest coplanar-compression-keeps-the-medial-chunk-surface-exact
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 6)))
    (loop for x from 4 below 12 do
      (loop for y from 4 below 12 do
        (dotimes (z (+ 2 (floor (+ x y) 3)))
          (luft.render::scene-builder-cell builder x y z))))
    (let* ((scene (luft.render::finish-scene-builder builder))
           (key (luft:chunk-key-at 4 4))
           (chunk nil))
      (luft:map-chain-chunks
       (lambda (chunk-key chain)
         (when (= chunk-key key) (setf chunk chain)))
       (luft.render::scene-solid scene))
      (handler-bind
          ((luft:missing-chunk
             (lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'luft:treat-as-air)))
           (luft:outside-domain
             (lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'luft:treat-as-air))))
        (let* ((medial (luft:mesh-chunk chunk key :bevel-width 4))
               (merged (luft:coplanar-compressed-surface-mesh medial)))
          (ok (< (luft:surface-mesh-triangle-count merged)
                 (luft:surface-mesh-triangle-count medial)))
          (ok (luft::%mesh-closed-p merged))
          (ok (luft::%same-plane-areas-p
               (luft::%mesh-oriented-plane-areas medial)
               (luft::%mesh-oriented-plane-areas merged))))))))

(deftest changing-the-uniform-width-remeshes-the-whole-resident-window
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 8)))
    (dotimes (chunk-x 3)
      (dotimes (chunk-y 3)
        (luft.render::scene-builder-cell
         builder (* chunk-x luft:+chunk-size+)
         (* chunk-y luft:+chunk-size+) 0)))
    (let* ((scene
             (render:make-streaming-scene
              (luft.render::finish-scene-builder builder)
              :residency-radius 1))
           (system
             (production:make-single-worker-production-system
              :name "LUFT uniform width change test")))
      (unwind-protect
           (progn
             (ok (luft.render::retarget-streaming-scene
                  scene system 2 64 64))
             (ok (null (luft.render::retarget-streaming-scene
                        scene system 4 64 64)))
             ;; Model completion of the initial cohort; tickets already in the
             ;; worker are harmless because the second scheduling supersedes
             ;; the one cohort production key with a newer ticket.
             (setf (luft.render::streaming-scene-cohort scene) nil
                   (luft.render::streaming-scene-removals scene) nil)
             (clrhash (luft.render::streaming-scene-outstanding scene))
             (ok (luft.render::retarget-streaming-scene
                  scene system 4 64 64))
             ;; The authored 3x3 source window realizes its directional
             ;; canonical owner closure, including the +X/+Y/+XY boundary
             ;; owners needed to publish a closed surface.
             (ok (= 16 (length
                       (luft.render::streaming-scene-cohort scene))))
             (ok (= 16
                    (hash-table-count
                     (luft.render::streaming-scene-outstanding scene))))
             (ok (loop for width being the hash-values of
                       (luft.render::streaming-scene-loaded scene)
                       always (= width 4)))
             (ok (= 1
                    (length
                     (remove-duplicates
                      (loop for ticket being the hash-values of
                            (luft.render::streaming-scene-outstanding scene)
                            collect ticket)))))
             (setf (luft.render::streaming-scene-cohort scene) nil
                   (luft.render::streaming-scene-removals scene) nil)
             (clrhash (luft.render::streaming-scene-outstanding scene))
             (ok (null (luft.render::retarget-streaming-scene
                        scene system 4 64 64))))
        (production:stop-production-system system)))))

(deftest retargeting-remeshes-all-owners-for-profile-policy-changes
  (let* ((scene (make-two-chunk-streaming-scene))
         (system
           (production:make-single-worker-production-system
            :name "LUFT streaming policy change test"))
         (profile
           (render:make-material-bevel-profile
            :terrain-width 2 :architecture-width 2
            :crystal-width 2 :contact-width 2))
         (equal-profile
           (render:make-material-bevel-profile
            :terrain-width 2 :architecture-width 2
            :crystal-width 2 :contact-width 2))
         (changed-profile
           (render:make-material-bevel-profile
            :terrain-width 4 :architecture-width 1
            :crystal-width 4 :contact-width 2)))
    (labels ((loaded-keys ()
               (sort
                (loop for key being the hash-keys of
                      (luft.render::streaming-scene-loaded scene)
                      collect key)
                #'<))
             (finish-publication ()
               (setf (luft.render::streaming-scene-cohort scene) nil
                     (luft.render::streaming-scene-removals scene) nil)
               (clrhash (luft.render::streaming-scene-outstanding scene)))
             (assert-complete-owner-cohort (source-keys)
               (let ((owners
                       (luft.render::streaming-scene-canonical-owner-closure
                        scene source-keys)))
                 (ok (equal source-keys (loaded-keys)))
                 (ok (equal owners
                            (luft.render::streaming-scene-cohort scene)))
                 (ok (null (luft.render::streaming-scene-removals scene)))
                 (ok (= (length owners)
                        (hash-table-count
                         (luft.render::streaming-scene-outstanding scene)))))))
      (unwind-protect
           (progn
             (ok (not (eq profile equal-profile)))
             (ok (equalp
                  (luft.render::material-bevel-profile-geometry-signature
                   profile)
                  (luft.render::material-bevel-profile-geometry-signature
                   equal-profile)))
             (ok (not (equalp
                       (luft.render::material-bevel-profile-geometry-signature
                        profile)
                       (luft.render::material-bevel-profile-geometry-signature
                        changed-profile))))
             (ok (luft.render::retarget-streaming-scene
                  scene system 2 64 4 profile))
             (let ((source-keys (loaded-keys)))
               (assert-complete-owner-cohort source-keys)
               (finish-publication)
               ;; Equal compiled signatures are semantic equality; allocating
               ;; another profile object must not churn residency or workers.
               (ok (null (luft.render::retarget-streaming-scene
                          scene system 2 64 4 equal-profile)))
               (ok (equal source-keys (loaded-keys)))
               (ok (null (luft.render::streaming-scene-cohort scene)))
               (ok (zerop
                    (hash-table-count
                     (luft.render::streaming-scene-outstanding scene))))
               ;; A real geometry-policy change remeshes every canonical owner
               ;; even though neither focus nor source residency moved.
               (ok (luft.render::retarget-streaming-scene
                    scene system 2 64 4 changed-profile))
               (assert-complete-owner-cohort source-keys)
               (finish-publication)))
        (production:stop-production-system system)))))

(deftest torch-light-retargeting-has-one-exact-residency-and-policy-matrix
  (labels ((realize-flag (snapshot)
             (luft.render::streaming-mesh-snapshot-realize-torch-light-p
              snapshot))
           (assert-full-torch-closure (scene support snapshot)
             (declare (ignore support))
             (ok (eq t (realize-flag snapshot)))
             (ok (typep (realize-flag snapshot) 'boolean))
             (let ((loaded
                     (sort
                      (loop for key being the hash-keys of
                            (luft.render::streaming-scene-loaded scene)
                            collect key)
                      #'<)))
               (ok (equal
                    (luft.render::streaming-scene-canonical-owner-closure
                     scene loaded)
                    (luft.render::streaming-mesh-snapshot-output-keys
                     snapshot))))))
    ;; First torch arrival realizes light and every desired output owner.
    (multiple-value-bind (scene support near far)
        (make-streaming-retarget-light-test-scene)
      (declare (ignore near))
      (install-streaming-retarget-light-test-residency scene (list far) 2)
      (multiple-value-bind (changed-p snapshot)
          (capture-streaming-retarget-snapshot scene 2 160 80)
        (ok changed-p)
        (assert-full-torch-closure scene support snapshot)))
    ;; A neighboring support chunk entering or leaving changes the resolved
    ;; final surface and therefore takes the same full torch closure.
    (dolist (arrival-p '(t nil))
      (multiple-value-bind (scene support near far)
          (make-streaming-retarget-light-test-scene :near-p t)
        (declare (ignore far))
        (install-streaming-retarget-light-test-residency
         scene (if arrival-p (list support) (list support near)) 2)
        (when arrival-p
          (setf (luft.render::streaming-scene-residency-radius scene) 1))
        (multiple-value-bind (changed-p snapshot)
            (capture-streaming-retarget-snapshot scene 2 160 80)
          (ok changed-p)
          (assert-full-torch-closure scene support snapshot))))
    ;; A far inert owner may enter without re-solving torch light.  Its partial
    ;; request retains the exact installed immutable generation and a literal
    ;; NIL flag rather than a truthy width-change list.
    (multiple-value-bind (scene support near far)
        (make-streaming-retarget-light-test-scene)
      (declare (ignore near far))
      (install-streaming-retarget-light-test-residency scene (list support) 2)
      (let ((installed
              (luft.render::streaming-scene-light-generation scene)))
        (multiple-value-bind (changed-p snapshot)
            (capture-streaming-retarget-snapshot scene 2 160 80)
          ;; Radius zero is unchanged; widen the immutable scene policy and
          ;; retarget again in a fresh state below for the actual far arrival.
          (ok (null changed-p))
          (ok (null snapshot)))
        (setf (luft.render::streaming-scene-residency-radius scene) 2)
        (multiple-value-bind (changed-p snapshot)
            (capture-streaming-retarget-snapshot scene 2 160 80)
          (ok changed-p)
          (ok (null (realize-flag snapshot)))
          (ok (typep (realize-flag snapshot) 'boolean))
          (ok (eq installed
                  (luft.render::streaming-mesh-snapshot-reusable-light-generation
                   snapshot))))))
    ;; A far inert departure with no affected desired output stages removal
    ;; under the exact installed realized-light pointer.
    (multiple-value-bind (scene support near far)
        (make-streaming-retarget-light-test-scene)
      (declare (ignore near))
      (install-streaming-retarget-light-test-residency
       scene (list support far) 2)
      (let ((installed
              (luft.render::streaming-scene-light-generation scene)))
        (multiple-value-bind (changed-p snapshot)
            (capture-streaming-retarget-snapshot scene 2 160 80)
          (ok changed-p)
          (ok (null snapshot))
          (ok (eq installed
                  (render:scene-mesh-generation-light-generation
                   (luft.render::streaming-scene-staged-generation scene)))))))
    ;; When the final torch departs and there are no desired outputs, the
    ;; staged generation is the scene's exact authored/base light object.
    (multiple-value-bind (scene support near far)
        (make-streaming-retarget-light-test-scene)
      (declare (ignore near far))
      (install-streaming-retarget-light-test-residency scene (list support) 2)
      (multiple-value-bind (changed-p snapshot)
          (capture-streaming-retarget-snapshot scene 2 480 480)
        (ok changed-p)
        (ok (null snapshot))
        (ok (eq
             (luft.render::scene-authored-light-generation scene)
             (render:scene-mesh-generation-light-generation
              (luft.render::streaming-scene-staged-generation scene))))))
    ;; Width 1 -> 2/3/4 and a profile policy change all carry a literal T,
    ;; remesh the complete resident torch closure, and intentionally reuse the
    ;; exact light object when the quantized realized wick seeds are unchanged.
    (dolist (target-width '(2 3 4))
      (multiple-value-bind (scene support near far)
          (make-streaming-retarget-light-test-scene)
        (declare (ignore near far))
        (install-streaming-retarget-light-test-residency scene (list support) 1)
        (let ((installed
                (luft.render::streaming-scene-light-generation scene)))
          (multiple-value-bind (changed-p snapshot)
              (capture-streaming-retarget-snapshot
               scene target-width 160 80)
            (ok changed-p)
            (assert-full-torch-closure scene support snapshot)
            (multiple-value-bind (owners census diagnostics generation)
                (luft.render::mesh-streaming-snapshot snapshot)
              (declare (ignore owners census diagnostics))
              (ok (eq installed
                      (render:scene-mesh-generation-light-generation
                       generation))))))))
    (multiple-value-bind (scene support near far)
        (make-streaming-retarget-light-test-scene)
      (declare (ignore near far))
      (let ((base-profile
              (render:make-material-bevel-profile
               :terrain-width 2 :architecture-width 2
               :crystal-width 2 :contact-width 2))
            (changed-profile
              (render:make-material-bevel-profile
               :terrain-width 2 :architecture-width 4
               :crystal-width 2 :contact-width 2)))
        (install-streaming-retarget-light-test-residency
         scene (list support) 2 base-profile)
        (setf (luft.render::streaming-scene-geometry-policy-signature scene)
              (luft.render::material-bevel-profile-geometry-signature
               base-profile))
        (multiple-value-bind (changed-p snapshot)
            (capture-streaming-retarget-snapshot
             scene 2 160 80 changed-profile)
          (ok changed-p)
          (assert-full-torch-closure scene support snapshot))))))

(deftest authored-light-revision-is-exact-static-generation-input
  (labels ((make-scene (repeat-p)
             (let ((builder
                     (luft.render::make-scene-builder :horizontal-bits 7)))
               (luft.render::scene-builder-cell builder 63 4 4)
               (luft.render::scene-builder-cell builder 64 4 4)
               (when repeat-p
                 ;; Same final authored cells, distinct non-torch light input
                 ;; revision. Immutable scenes never receive a field SETF.
                 (luft.render::scene-builder-cell builder 63 4 4))
               (luft.render::finish-scene-builder builder))))
    (let ((first-scene (make-scene nil))
          (second-scene (make-scene t)))
      (multiple-value-bind (first-mesh first-generation)
          (render:make-render-mesh first-scene)
        (declare (ignore first-mesh))
        (multiple-value-bind (second-mesh second-generation)
            (render:make-render-mesh second-scene)
          (declare (ignore second-mesh))
          (ok (/= (luft.render::scene-authored-light-revision first-scene)
                  (luft.render::scene-authored-light-revision second-scene)))
          (ok (not (equalp
                    (render:scene-mesh-generation-request-stamp
                     first-generation)
                    (render:scene-mesh-generation-request-stamp
                     second-generation))))
          (ok (not
               (luft.render::realized-light-stamp=
                (luft.render::realized-light-generation-stamp
                 (render:scene-mesh-generation-light-generation
                  first-generation))
                (luft.render::realized-light-generation-stamp
                 (render:scene-mesh-generation-light-generation
                  second-generation))))))))))

(deftest finishing-a-scene-copies-every-builder-owned-material-input
  (labels ((same-mesh-tree-p (left right)
             (let ((left (surface-mesh-tree-meshes left))
                   (right (surface-mesh-tree-meshes right)))
               (and (= (length left) (length right))
                    (every #'luft::%same-surface-mesh-representation-p
                           left right)))))
    (let* ((builder
             (luft.render::make-scene-builder :horizontal-bits 5))
           (domain (luft.render::scene-builder-domain builder))
           (cell (luft:make-site domain 4 4 4 luft:+cell-extent+ 1))
           (face (luft:site-boundary-high domain cell :z)))
      (luft.render::scene-builder-cell builder 4 4 4)
      (let* ((scene-a (luft.render::finish-scene-builder builder))
             (reading-a (luft.render::scene-face-reading scene-a face))
             (vocabulary-a
               (copy-seq
                (luv.domains:identity-vocabulary-members
                 (luft.render::scene-material-vocabulary scene-a))))
             (program-a
               (copy-seq
                (luft.render::material-program-placement-face-stocks
                 (luft.render::scene-material-program scene-a))))
             (field-a (render:scene-authored-voxel-light scene-a)))
        (multiple-value-bind (mesh-a generation-a)
            (render:make-render-mesh scene-a)
          (let* ((custom-kind
                   (make-instance
                    'luft.render::stone-material-kind
                    :name :builder-successor-stone
                    :base-tone '(0.71 0.23 0.17)
                    :roughness 0.61 :relief :weathered-stone))
                 (custom-placement
                   (make-instance
                    'luft.render::material-placement
                    :name :builder-successor-stone
                    :kind custom-kind :finish :cut
                    :frame luft.render::*world-material-frame*
                    :role :architecture)))
            ;; Reusing the builder changes both an existing placement and its
            ;; vocabulary, then adds new geometry under a new placement.
            (luft.render::scene-builder-cell
             builder 4 4 4 :material luft.render::*crystal-material-placement*)
            (luft.render::scene-builder-cell
             builder 12 12 4 :material custom-placement)
            (let ((scene-b (luft.render::finish-scene-builder builder)))
              (multiple-value-bind (mesh-a-again generation-a-again)
                  (render:make-render-mesh scene-a)
                (multiple-value-bind (mesh-b generation-b)
                    (render:make-render-mesh scene-b)
                  (ok (eq reading-a
                          (luft.render::scene-face-reading scene-a face)))
                  (ok (equalp vocabulary-a
                              (luv.domains:identity-vocabulary-members
                               (luft.render::scene-material-vocabulary
                                scene-a))))
                  (ok (equalp program-a
                              (luft.render::material-program-placement-face-stocks
                               (luft.render::scene-material-program scene-a))))
                  (ok (eq field-a (render:scene-authored-voxel-light scene-a)))
                  (ok (zerop (luft:voxel-light-at-site field-a cell)))
                  (ok (eq
                       (render:scene-mesh-generation-light-generation
                        generation-a)
                       (render:scene-mesh-generation-light-generation
                        generation-a-again)))
                  (ok (equalp
                       (render:scene-mesh-generation-result-stamp generation-a)
                       (render:scene-mesh-generation-result-stamp
                        generation-a-again)))
                  (ok (same-mesh-tree-p mesh-a mesh-a-again))
                  (ok (plusp
                       (luft:voxel-light-at-site
                        (render:scene-authored-voxel-light scene-b) cell)))
                  (ok (> (length
                          (luv.domains:identity-vocabulary-members
                           (luft.render::scene-material-vocabulary scene-b)))
                         (length vocabulary-a)))
                  (ok (not
                       (luft.render::realized-light-stamp=
                        (luft.render::realized-light-generation-stamp
                         (render:scene-mesh-generation-light-generation
                          generation-a))
                        (luft.render::realized-light-generation-stamp
                         (render:scene-mesh-generation-light-generation
                          generation-b)))))
                  (ok (not (same-mesh-tree-p mesh-a mesh-b))))))))))))

(deftest static-generation-reuse-is-scene-owned-and-provenance-exact
  (labels ((make-scene (distant-material)
             (let ((builder
                     (luft.render::make-scene-builder :horizontal-bits 6)))
               (luft.render::scene-builder-cell builder 8 8 4
                                                :architecture-p t)
               (luft.render::scene-builder-torch builder 8 8 4 :z :high)
               (luft.render::scene-builder-cell
                builder 24 24 4 :material distant-material)
               (luft.render::finish-scene-builder builder))))
    (let ((crystal-scene
            (make-scene luft.render::*crystal-material-placement*))
          (terrain-scene
            (make-scene luft.render::*terrain-material-placement*)))
      (ok (= (luft.render::scene-authored-light-revision crystal-scene)
             (luft.render::scene-authored-light-revision terrain-scene)))
      (multiple-value-bind (first-mesh first-generation)
          (render:make-render-mesh crystal-scene :bevel-width 2)
        (declare (ignore first-mesh))
        (ok (eq crystal-scene
                (luft.render::scene-mesh-generation-scene first-generation)))
        (multiple-value-bind (second-mesh second-generation)
            (render:make-render-mesh
             crystal-scene :bevel-width 2
             :reusable-light-generation first-generation)
          (ok (eq crystal-scene
                  (luft.render::scene-mesh-generation-scene
                   second-generation)))
          (ok (eq (render:scene-mesh-generation-light-generation
                   first-generation)
                  (render:scene-mesh-generation-light-generation
                   second-generation)))
          (let ((field
                  (luft.render::realized-light-generation-field
                   (render:scene-mesh-generation-light-generation
                    second-generation))))
            (ok (every (lambda (mesh)
                         (eq field (luft:surface-mesh-voxel-light mesh)))
                       (surface-mesh-tree-meshes second-mesh)))))
        (multiple-value-bind (terrain-mesh terrain-generation)
            (render:make-render-mesh terrain-scene :bevel-width 2)
          (declare (ignore terrain-mesh))
          (ok (not
               (luft.render::realized-light-stamp=
                (luft.render::realized-light-generation-stamp
                 (render:scene-mesh-generation-light-generation
                  first-generation))
                (luft.render::realized-light-generation-stamp
                 (render:scene-mesh-generation-light-generation
                  terrain-generation)))))
          (ok (signals
               (render:make-render-mesh
                terrain-scene :bevel-width 2
                :reusable-light-generation first-generation)
               'error)))))))

(deftest distant-owners-cannot-enter-a-torch-attachment-resolution
  (labels ((make-scene (distant-p)
             (let ((builder
                     (luft.render::make-scene-builder :horizontal-bits 7)))
               (luft.render::scene-builder-cell builder 8 8 4
                                                :architecture-p t)
               (luft.render::scene-builder-torch
                builder 8 8 4 :z :high :u 0.75 :v 0.75)
               (when distant-p
                 (luft.render::scene-builder-cell builder 96 96 4))
               (luft.render::finish-scene-builder builder)))
           (frames (mesh)
             (loop for owner in (surface-mesh-tree-meshes mesh)
                   append (luft:surface-mesh-attachments owner))))
    (let* ((local (render:make-render-mesh (make-scene nil)))
           (distant (render:make-render-mesh (make-scene t)))
           (local-frames (frames local))
           (distant-frames (frames distant)))
      (ok (= 1 (length local-frames) (length distant-frames)))
      (ok (equalp (first local-frames) (first distant-frames))))))

(deftest highland-landscape-is-deterministic-and-regionally-varied
  (let* ((size 256)
         (heights
           (loop for x below size by 8 append
             (loop for y below size by 8
                   collect
                   (luft.render::highland-landscape-height
                    x y size :seed 121))))
         (again
           (loop for x below size by 8 append
             (loop for y below size by 8
                   collect
                   (luft.render::highland-landscape-height
                    x y size :seed 121)))))
    (ok (equal heights again))
    (ok (>= (- (reduce #'max heights) (reduce #'min heights)) 24))
    (ok (>= (length (remove-duplicates heights)) 20))
    (ok (not (equal heights
                    (loop for x below size by 8 append
                      (loop for y below size by 8
                            collect
                            (luft.render::highland-landscape-height
                             x y size :seed 913))))))))

(deftest highland-landscape-streams-by-default
  (let ((scene
          (render:make-highland-sanctuary-scene :horizontal-bits 6)))
    (ok (typep scene 'render:streaming-scene))
    (ok (= 3 (luft.render::streaming-scene-residency-radius scene)))
    (ok (= 1 (hash-table-count
              (luft.render::streaming-scene-store scene))))))

(deftest retargeting-replaces-one-complete-residency-window
  (let* ((scene (make-two-chunk-streaming-scene))
         (left (luft:chunk-key-at 63 4))
         (right (luft:chunk-key-at 64 4))
         (system
           (production:make-single-worker-production-system
            :name "LUFT retarget test")))
    (setf (luft.render::streaming-scene-residency-radius scene) 0
          (gethash left (luft.render::streaming-scene-loaded scene)) 2)
    (unwind-protect
         (progn
           (ok (luft.render::retarget-streaming-scene
                scene system luft:+mesh-bevel-width+ 64 4))
           (ok (null (gethash left
                              (luft.render::streaming-scene-loaded scene))))
           (ok (gethash right (luft.render::streaming-scene-loaded scene)))
           (let* ((old-owner-closure
                    (luft.render::streaming-scene-canonical-owner-closure
                     scene (list left)))
                  (owner-closure
                    (luft.render::streaming-scene-canonical-owner-closure
                     scene (list right)))
                  (departed-owners
                    (set-difference old-owner-closure owner-closure
                                    :test #'eql)))
             ;; Publication follows the exact directional geometry-owner
             ;; closure.  Retargeting onto the high-X boundary retains the
             ;; shared seam owners and retires only the old low-side owners.
             (ok (equal owner-closure
                        (luft.render::streaming-scene-cohort scene)))
             (ok (equal departed-owners
                        (luft.render::streaming-scene-removals scene)))
             (ok (= (length owner-closure)
                    (hash-table-count
                     (luft.render::streaming-scene-outstanding scene))))))
      (production:stop-production-system system))))

(defun key-event (class key-name &key character modifiers repeat-p)
  (make-instance class
                 :timestamp 0
                 :key-name key-name
                 :character character
                 :unshifted-character character
                 :modifiers modifiers
                 :repeat-p repeat-p))

(defun key-press (key-name &key character modifiers repeat-p)
  (key-event 'luv:canvas-key-press-event key-name
             :character character :modifiers modifiers :repeat-p repeat-p))

(defun key-release (key-name &key character modifiers)
  (key-event 'luv:canvas-key-release-event key-name
             :character character :modifiers modifiers))

(deftest the-viewer-is-the-mcclim-application
  (ok (= 2 luft:+mesh-bevel-width+))
  (ok (string= "1/8" (luft.render::bevel-width-label 1)))
  (ok (string= "1/4" (luft.render::bevel-width-label 2)))
  (ok (string= "1/2" (luft.render::bevel-width-label 4)))
  (ok (= 2 (luft.render::next-bevel-width 1)))
  (ok (= 4 (luft.render::next-bevel-width 2)))
  (ok (= 1 (luft.render::next-bevel-width 4)))
  (let ((viewer (clim:make-application-frame 'render:viewer)))
    (ok (typep viewer 'clim:application-frame))
    (ok (null (climi::frame-process viewer)))
    (ok (not (luft.render::viewer-inspector-p viewer)))
    (ok (luft.render::viewer-inspector-p
         (clim:make-application-frame 'render:viewer :inspector-p t)))
    (ok (typep (render:viewer-mode viewer) 'render:isometric-walk-mode))
    (ok (equal '(luft.render::com-start-moving :forward)
               (luft.render::viewer-key-command viewer (key-press :w))))
    (setf (render:viewer-mode viewer) (make-instance 'render:orbit-mode))
    (ok (null (luft.render::viewer-key-command viewer (key-press :w))))
    (ok (equal '(luft.render::com-toggle-fullscreen)
               (luft.render::viewer-key-command
                viewer (key-press :f11))))
    (ok (equal '(luft.render::com-release-pointer)
               (luft.render::viewer-key-command
                viewer (key-press :escape))))
    (setf (luft.render::viewer-pointer-captured-p viewer) t)
    (ok (equal '(luft.render::com-start-moving :forward)
               (luft.render::viewer-key-command viewer (key-press :w))))
    (ok (equal '(luft.render::com-stop-moving :forward)
               (luft.render::viewer-key-command viewer (key-release :w))))
    (ok (equal '(luft.render::com-reset-view)
               (luft.render::viewer-key-command viewer (key-press :r))))
    (ok (equal '(luft.render::com-toggle-construction-lines)
               (luft.render::viewer-key-command viewer (key-press :c))))
    (ok (equal '(luft.render::com-toggle-bevel-width)
               (luft.render::viewer-key-command viewer (key-press :b))))
    (ok (equal '(luft.render::com-rotate-view-clockwise)
               (luft.render::viewer-key-command viewer (key-press :tab))))
    (ok (equal '(luft.render::com-rotate-view-counterclockwise)
               (luft.render::viewer-key-command
                viewer (key-press :tab :modifiers '(:shift)))))
    (ok (equal '(luft.render::com-toggle-fullscreen)
               (luft.render::viewer-key-command viewer (key-press :f11))))
    (ok (equal '(luft.render::com-toggle-viewer-mode)
               (luft.render::viewer-key-command viewer (key-press :m))))
    (ok (equal '(luft.render::com-quit)
               (luft.render::viewer-key-command
                viewer (key-press :q :character #\q
                                     :modifiers '(:control)))))
    (clim:execute-frame-command
     viewer (luft.render::viewer-key-command viewer (key-press :w)))
    (ok (luft.render::viewer-control-active-p viewer :forward))
    (clim:execute-frame-command
     viewer (luft.render::viewer-key-command viewer (key-release :w)))
    (ok (not (luft.render::viewer-control-active-p viewer :forward)))))

(deftest tab-orbits-the-following-camera-in-eighth-turns
  (let* ((viewer (clim:make-application-frame 'render:viewer))
         (camera (render:viewer-camera viewer)))
    (setf (render:camera-yaw camera) 0.0)
    (luft.render::rotate-viewer-eighth-turn viewer 1)
    (ok (< (abs (- (/ pi 4) (render:camera-yaw camera))) 1.0e-6))
    (luft.render::rotate-viewer-eighth-turn viewer -1)
    (ok (< (abs (render:camera-yaw camera)) 1.0e-6))
    (setf (render:viewer-mode viewer) (make-instance 'render:orbit-mode))
    (luft.render::rotate-viewer-eighth-turn viewer 1)
    (ok (< (abs (render:camera-yaw camera)) 1.0e-6))))

(deftest world-edit-mode-enters-exits-and-protects-the-player
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (player
           (render:make-walking-player
            :position (luv.arithmetic.lisp.vec3:make-vec3 1.5 2.5 1.0))))
    (luft.render::scene-builder-cell builder 1 2 0)
    (let* ((scene
             (render:make-streaming-scene
              (luft.render::finish-scene-builder builder)))
           (domain (luft:chain-domain (render:scene-solid scene)))
           (player-cell
             (luft:make-site domain 1 2 1 luft:+cell-extent+ 1))
           (viewer
             (clim:make-application-frame
              'render:viewer :source scene :production-system t
                             :player player)))
      (let ((clim:*application-frame* viewer))
        (luft.render::com-enter-world-edit-mode)
        (ok (typep (render:viewer-mode viewer) 'render:world-edit-mode))
        (ok (eq :ready (luft.render::viewer-last-edit-status viewer)))
        (multiple-value-bind (edit status)
            (luft.render::record-viewer-world-edit
             viewer player-cell luft.render::*terrain-material-placement*)
          (ok (null edit))
          (ok (eq :player status))
          (ok (zerop (luft:chain-cell-occupancy-bit
                      (render:scene-solid scene) 1 2 1))))
        (luft.render::com-release-pointer)
        (ok (typep (render:viewer-mode viewer)
                   'render:isometric-walk-mode))
        (ok (not (typep (render:viewer-mode viewer)
                        'render:world-edit-mode)))))))

(deftest an-inspected-outward-face-selects-its-adjacent-empty-cell
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (x 4) (y 5) (z 3))
    (luft.render::scene-builder-cell builder x y z)
    (let* ((scene (luft.render::finish-scene-builder builder))
           (inspection
             (luft.render::raycast-site
              scene
              (luv.arithmetic.lisp.vec3:make-vec3 (+ x 0.5) (+ y 0.5) 10.0)
              (luv.arithmetic.lisp.vec3:make-vec3 0.0 0.0 -1.0)))
           (adjacent
             (luft.render::site-inspection-adjacent-cell inspection)))
      (ok (= x (luft:site-x adjacent)))
      (ok (= y (luft:site-y adjacent)))
      (ok (= (1+ z) (luft:site-z adjacent))))))

(deftest click-to-walk-routes-around-a-character-high-wall
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (player
           (render:make-walking-player
            :position (luv.arithmetic.lisp.vec3:make-vec3 1.5 2.5 1.0))))
    (luft.render::scene-builder-box builder 0 15 0 15 0 0)
    ;; The direct row is blocked.  The only short way around the north end
    ;; crosses Y=5, proving the click produced a route rather than a velocity.
    (loop for y from 0 to 4 do
      (loop for z from 1 to 4 do
        (luft.render::scene-builder-cell builder 3 y z)))
    (let* ((scene (luft.render::finish-scene-builder builder :player-p t))
           (route
             (render:start-walking-player-route player scene 5 2 1)))
      (ok (eq :running (render:walking-route-status route)))
      (ok (find 5 (render:walking-route-cells route)
                :key #'luft:site-y))
      (ok (> (length (render:walking-route-cells route)) 4))
      (ok (plusp (render:walking-route-visits route)))
      (loop repeat 1200
            while (eq :running (render:walking-route-status route))
            do (multiple-value-bind (forward right maximum-distance)
                   (luft.render::walking-player-route-control
                    player (render:make-fly-camera :yaw 0.0))
                 (luft.render::advance-walking-player
                  player scene (render:make-fly-camera :yaw 0.0)
                  (or forward 0.0) (or right 0.0) (/ 1.0 120.0)
                  :maximum-distance maximum-distance)
                 (luft.render::trim-walking-player-route player)))
      (ok (eq :arrived (render:walking-route-status route)))
      (ok (< (abs (- 5.5
                     (luv.arithmetic.lisp.vec3:vec3-x
                      (render:walking-player-position player))))
             0.12))
      (ok (< (abs (- 2.5
                     (luv.arithmetic.lisp.vec3:vec3-y
                      (render:walking-player-position player))))
             0.12)))))

(deftest click-to-walk-authors-straight-diagonal-waypoints
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (player
           (render:make-walking-player
            :position (luv.arithmetic.lisp.vec3:make-vec3 1.5 1.5 1.0))))
    (luft.render::scene-builder-box builder 0 15 0 15 0 0)
    (let* ((scene (luft.render::finish-scene-builder builder :player-p t))
           (route (render:start-walking-player-route player scene 6 6 1))
           (cells (render:walking-route-cells route)))
      (ok (= 5 (length cells)))
      (loop for cell in cells
            for coordinate from 2
            do (ok (= coordinate (luft:site-x cell) (luft:site-y cell)))))))

(deftest orthographic-walk-moves-on-the-ground-without-zooming
  (let* ((viewer (clim:make-application-frame 'render:viewer))
         (camera (render:viewer-camera viewer))
         (player (render:viewer-player viewer))
         (before-player-x
           (luv.arithmetic.lisp.vec3:vec3-x
            (render:walking-player-position player)))
         (before-player-y
           (luv.arithmetic.lisp.vec3:vec3-y
            (render:walking-player-position player)))
         (render:*projection* :isometric)
         (render:*isometric-height* 18.0))
    (luft.render::advance-viewer-camera viewer 1.0)
    (let ((before-camera-z
            (luv.arithmetic.lisp.vec3:vec3-z
             (render:camera-position camera))))
      (luft.render::set-viewer-control viewer :forward t)
      (luft.render::advance-viewer-camera viewer 1.1)
      (let ((after (render:walking-player-position player)))
        (ok (or (/= before-player-x
                    (luv.arithmetic.lisp.vec3:vec3-x after))
                (/= before-player-y
                    (luv.arithmetic.lisp.vec3:vec3-y after))))
        ;; A same-height walking step translates the following camera without
        ;; changing its orbit height.
        (ok (= before-camera-z
               (luv.arithmetic.lisp.vec3:vec3-z
                (render:camera-position camera))))
        (ok (= 18.0 render:*isometric-height*))))))

(deftest following-camera-ignores-small-relief-and-catches-large-jumps
  (let* ((camera (render:make-fly-camera :yaw 0.0 :pitch -0.5))
         (player (render:make-walking-player)))
    (luft.render::follow-walking-player camera player)
    (let* ((camera-position (render:camera-position camera))
           (settled-z (luv.arithmetic.lisp.vec3:vec3-z camera-position)))
      ;; A stair-sized vertical discrepancy belongs to the traveler, not the
      ;; composition, and remains inside the camera's quiet zone.
      (incf (luv.arithmetic.lisp.vec3:vec3-z
             (render:walking-player-position player))
            0.75)
      (luft.render::follow-walking-player camera player :seconds 0.1)
      (ok (= settled-z
             (luv.arithmetic.lisp.vec3:vec3-z camera-position)))
      ;; A fall or teleport is well outside that zone and closes most of its
      ;; error promptly instead of inheriting the gentle local response.
      (incf (luv.arithmetic.lisp.vec3:vec3-z
             (render:walking-player-position player))
            12.0)
      (let ((before-error
              (abs (- (- (+ (luv.arithmetic.lisp.vec3:vec3-z
                             (render:walking-player-position player))
                            1.45)
                         (* 18.0 (sin -0.5)))
                      (luv.arithmetic.lisp.vec3:vec3-z camera-position)))))
        (luft.render::follow-walking-player camera player :seconds 0.1)
        (let ((after-error
                (abs (- (- (+ (luv.arithmetic.lisp.vec3:vec3-z
                               (render:walking-player-position player))
                              1.45)
                           (* 18.0 (sin -0.5)))
                        (luv.arithmetic.lisp.vec3:vec3-z camera-position)))))
          (ok (< after-error (* 0.45 before-error))))))))

(deftest walking-player-climbs-one-step-but-not-a-character-high-wall
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (camera (render:make-fly-camera
                  :position
                  (luv.arithmetic.lisp.vec3:make-vec3 0.0 0.0 0.0)
                  :yaw 0.0 :pitch -0.5))
         (player
           (render:make-walking-player
            :position (luv.arithmetic.lisp.vec3:make-vec3 2.5 2.5 1.0))))
    (luft.render::scene-builder-box builder 0 7 0 5 0 0)
    (luft.render::scene-builder-cell builder 3 2 1)
    (let ((scene (luft.render::finish-scene-builder builder :player-p t)))
      (luft.render::advance-walking-player player scene camera 1 0 0.2)
      (ok (= 2.0 (luv.arithmetic.lisp.vec3:vec3-z
                  (render:walking-player-position player))))
      (ok (> (luft.render::walking-player-gait player) 0.0))
      (loop repeat 60 do
        (luft.render::advance-walking-player
         player scene camera 0 0 (/ 1.0 60.0)))
      (let ((step-coordinate
              (/ (luft.render::walking-player-gait player) pi)))
        (ok (< (abs (- step-coordinate (round step-coordinate))) 1e-5)))
      ;; The next column is filled through the character's head.  Axis
      ;; separation leaves the player at the near edge instead of climbing or
      ;; teleporting to its remote roof.
      (loop for z from 1 to 4 do
        (luft.render::scene-builder-cell builder 4 2 z))
      (let* ((blocked-scene
               (luft.render::finish-scene-builder builder :player-p t))
             (before-x
               (luv.arithmetic.lisp.vec3:vec3-x
                (render:walking-player-position player))))
        (luft.render::advance-walking-player
         player blocked-scene camera 1 0 0.2)
        (ok (= before-x
               (luv.arithmetic.lisp.vec3:vec3-x
                (render:walking-player-position player))))))))

(deftest gameplay-treats-the-finite-world-domain-as-a-wall
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (camera
           (render:make-fly-camera
            :position (luv.arithmetic.lisp.vec3:make-vec3 0.0 0.0 0.0)
            :yaw 0.0 :pitch -0.5))
         (player
           (render:make-walking-player
            :position
            (luv.arithmetic.lisp.vec3:make-vec3 15.5 8.5 1.0))))
    (luft.render::scene-builder-box builder 0 15 0 15 0 0)
    (let ((scene (luft.render::finish-scene-builder builder :player-p t)))
      (ok (= 1 (luft.render::collision-cell-occupancy-bit
                (luft.render::scene-solid scene) 16 8 1)))
      (let ((before
              (luv.arithmetic.lisp.vec3:vec3-x
               (render:walking-player-position player))))
        (luft.render::advance-walking-player player scene camera 1 0 0.2)
        (ok (= before
               (luv.arithmetic.lisp.vec3:vec3-x
                (render:walking-player-position player))))
        (setf (luft.render::walking-player-grounded-p player) nil)
        (ok (null (luft.render::try-walking-player-air-axis
                   player scene :x 1.0)))
        (ok (= before
               (luv.arithmetic.lisp.vec3:vec3-x
                (render:walking-player-position player))))))))

(deftest casting-a-fireball-turns-the-wizard-and-launches-from-the-staff
  (let* ((player
           (render:make-walking-player
            :position
            (luv.arithmetic.lisp.vec3:make-vec3 4.0 5.0 1.0)))
         (target (luv.arithmetic.lisp.vec3:make-vec3 14.0 5.0 2.5)))
    (setf (render:walking-player-route player)
          (make-instance 'render:walking-route :start nil :destination nil
                         :cells nil :status :running))
    (luft.render::cast-walking-player-fireball player target)
    (ok (< (abs (- 1.0 (luft.render::walking-player-heading-x player)))
           1.0e-6))
    (ok (< (abs (luft.render::walking-player-heading-y player)) 1.0e-6))
    (ok (eq :cancelled
            (render:walking-route-status
             (render:walking-player-route player))))
    (ok (= 1.0 (luft.render::walking-player-spell-flash player)))
    (ok (luft.render::walking-player-fireball-velocity player))
    (let* ((staff (luft.render::walking-player-staff-head-position player))
           (fireball (luft.render::walking-player-fireball-position player))
           (dx (- (luv.arithmetic.lisp.vec3:vec3-x fireball)
                  (luv.arithmetic.lisp.vec3:vec3-x staff)))
           (dy (- (luv.arithmetic.lisp.vec3:vec3-y fireball)
                  (luv.arithmetic.lisp.vec3:vec3-y staff)))
           (dz (- (luv.arithmetic.lisp.vec3:vec3-z fireball)
                  (luv.arithmetic.lisp.vec3:vec3-z staff)))
           (launch-distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
      (ok (< (abs (- 0.48
                     launch-distance))
             1.0e-5))
      ;; This position belongs to the wizard, not the old camera-ray origin.
      (ok (> (luv.arithmetic.lisp.vec3:vec3-x fireball) 5.0))
      (ok (> (luv.arithmetic.lisp.vec3:vec3-z fireball) 3.0)))
    (let ((before (luv.arithmetic.lisp.vec3:vec3-x
                   (luft.render::walking-player-fireball-position player))))
      (luft.render::advance-walking-player-fireball player 0.1)
      (ok (> (luv.arithmetic.lisp.vec3:vec3-x
              (luft.render::walking-player-fireball-position player)) before)))
    (multiple-value-bind (character previous direction-lane
                          fireball previous-fireball)
        (luft.render::walking-player-render-lanes player)
      (declare (ignore character previous direction-lane previous-fireball))
      (ok (= luft.render::+fireball-radius+ (fourth fireball))))))

(deftest a-fireball-flies-straight-and-ends-at-its-target
  (let* ((player (render:make-walking-player))
         (origin (luv.arithmetic.lisp.vec3:make-vec3 14.0 8.0 4.0))
         (direction (luv.arithmetic.lisp.vec3:make-vec3 1.0 0.0 0.0)))
    (luft.render::launch-walking-player-fireball
     player origin direction :distance 3.0)
    (let* ((before (luft.render::walking-player-fireball-position player))
           (before-x (luv.arithmetic.lisp.vec3:vec3-x before))
           (before-y (luv.arithmetic.lisp.vec3:vec3-y before))
           (before-z (luv.arithmetic.lisp.vec3:vec3-z before)))
      (luft.render::advance-walking-player-fireball player 0.05)
      (let ((after (luft.render::walking-player-fireball-position player)))
        (ok (> (luv.arithmetic.lisp.vec3:vec3-x after) before-x))
        (ok (= before-y (luv.arithmetic.lisp.vec3:vec3-y after)))
        (ok (= before-z (luv.arithmetic.lisp.vec3:vec3-z after))))
      (luft.render::advance-walking-player-fireball player 1.0)
      (ok (null (luft.render::walking-player-fireball-position player))))))

(deftest the-spike-scene-is-three-site-instance-streams
  (let* ((mesh (render:make-render-mesh
                (render:make-manifold-spike-scene)))
         (templates (luft:surface-mesh-template-vertex-words mesh)))
    (ok (plusp (luft:surface-mesh-face-triangle-count mesh)))
    (ok (plusp (luft:surface-mesh-band-triangle-count mesh)))
    (ok (plusp (luft:surface-mesh-fan-triangle-count mesh)))
    (ok (plusp (luft:surface-mesh-singular-star-count mesh)))
    (ok (plusp (luft:surface-mesh-face-instance-count mesh)))
    (ok (plusp (luft:surface-mesh-band-instance-count mesh)))
    (ok (plusp (luft:surface-mesh-fan-instance-count mesh)))
    (ok (plusp (luft:surface-mesh-template-count mesh)))
    (ok (zerop (mod (length templates)
                    luft:+mesh-template-vertex-word-count+)))))

(deftest the-walking-player-belongs-to-the-sanctuary
  (ok (luft.render::scene-player-p
       (render:make-mountain-sanctuary-scene)))
  (ok (not (luft.render::scene-player-p
            (render:make-manifold-spike-scene))))
  (ok (not (luft.render::scene-player-p
            (render:make-miter-study-scene)))))

(deftest the-elevated-sanctuary-rim-is-an-authored-stone-battlement
  (let ((scene (render:make-mountain-sanctuary-scene)))
    (multiple-value-bind (west east present-p)
        (luft.render::mountain-sanctuary-terrain-x-bounds 47)
      (declare (ignore west))
      (ok present-p)
      (let* ((height
               (luft.render::mountain-sanctuary-terrain-height east 47))
             (x (+ luft.render::+sanctuary-origin-x+ east))
             (y (+ luft.render::+sanctuary-origin-y+ 47))
             (solid (luft.render::scene-solid scene))
             (wall-cell
               (luft:make-site (luft:chain-domain solid) x y height
                               luft:+cell-extent+ 1)))
        (ok (>= height luft.render::+sanctuary-plateau-height+))
        ;; Two continuous courses stop the one-step walker; this even
        ;; contour column also carries the alternating crenellation.
        (ok (= 1 (luft:chain-cell-occupancy-bit solid x y height)))
        (ok (= 1 (luft:chain-cell-occupancy-bit solid x y (1+ height))))
        (ok (= 1 (luft:chain-cell-occupancy-bit solid x y (+ height 2))))
        (ok (zerop (luft:chain-cell-occupancy-bit solid (1+ x) y height)))
        (ok (eq luft.render::*sanctuary-material-placement*
                (luft.render::scene-material-placement-at scene wall-cell)))))
    ;; The low southern shore remains open rather than walling in the route.
    (multiple-value-bind (west east present-p)
        (luft.render::mountain-sanctuary-terrain-x-bounds -15)
      (declare (ignore west))
      (ok present-p)
      (let ((height
              (luft.render::mountain-sanctuary-terrain-height east -15)))
        (ok (< height luft.render::+sanctuary-plateau-height+))
        (ok (zerop
             (luft:chain-cell-occupancy-bit
              (luft.render::scene-solid scene)
              (+ luft.render::+sanctuary-origin-x+ east)
              (+ luft.render::+sanctuary-origin-y+ -15)
              height)))))))

(deftest scene-builders-translate-authored-sites-at-the-boundary
  (let* ((builder (luft.render::make-scene-builder
                   :horizontal-bits 5 :origin-x 7 :origin-y 11))
         (scene (progn
                  (luft.render::scene-builder-cell builder 2 3 4)
                  (luft.render::finish-scene-builder builder)))
         (solid (luft.render::scene-solid scene)))
    (ok (= 1 (luft:chain-cell-occupancy-bit solid 9 14 4)))
    (ok (zerop (luft:chain-cell-occupancy-bit solid 2 3 4)))))

(deftest the-sanctuary-curtain-is-bedded-into-the-mountain
  (let* ((scene (render:make-mountain-sanctuary-scene))
         (solid (luft.render::scene-solid scene))
         (domain (luft:chain-domain solid)))
    (flet ((occupied-p (x y z)
             (= 1 (luft:chain-cell-occupancy-bit solid x y z)))
           (architecture-p (x y z)
             (eq :architecture
                 (luft.render::material-placement-role
                  (luft.render::scene-material-placement-at
                   scene (luft:make-site domain x y z luft:+cell-extent+ 1))))))
      ;; The front curtain and both round keeps have continuous stone shoes
      ;; where the procedural ridge can otherwise fall below their fixed base.
      (dolist (point '((20 45) (40 45) (15 41) (45 41)))
        (destructuring-bind (x y) point
          (incf x luft.render::+sanctuary-origin-x+)
          (incf y luft.render::+sanctuary-origin-y+)
          (ok (occupied-p x y 17))
          (ok (occupied-p x y 18))
          (ok (architecture-p x y 17))
          (ok (architecture-p x y 18))))
      ;; The stair arrives at a supported masonry threshold, while the gate
      ;; opening itself remains clear at the sanctuary floor.
      (let ((x (+ 30 luft.render::+sanctuary-origin-x+))
            (y (+ 45 luft.render::+sanctuary-origin-y+)))
        (ok (occupied-p x y 18))
        (ok (architecture-p x y 18))
        (ok (not (occupied-p x y 19))))
      ;; Terrain and inhabited architecture now continue well beyond the old
      ;; 64-cell diorama, including the remote back-ridge beacon.
      (ok (occupied-p (+ luft.render::*sanctuary-beacon-x*
                         luft.render::+sanctuary-origin-x+)
                      (+ luft.render::*sanctuary-beacon-y*
                         luft.render::+sanctuary-origin-y+)
                      20))
      (ok (architecture-p
           (+ luft.render::*sanctuary-beacon-x*
              luft.render::+sanctuary-origin-x+)
           (+ luft.render::*sanctuary-beacon-y*
              luft.render::+sanctuary-origin-y+)
           (+ 8
              (luft.render::mountain-sanctuary-terrain-height
               luft.render::*sanctuary-beacon-x*
               luft.render::*sanctuary-beacon-y*)))))))

(deftest scene-cells-store-vocabulary-closed-material-offsets
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene (progn
                  (luft.render::scene-builder-cell builder 2 2 2)
                  (luft.render::scene-builder-cell
                   builder 2 2 3 :architecture-p t)
                  (luft.render::finish-scene-builder builder)))
         (domain (luft:chain-domain (luft.render::scene-solid scene)))
         (earth-site (luft:make-site domain 2 2 2 luft:+cell-extent+ 1))
         (stone-site (luft:make-site domain 2 2 3 luft:+cell-extent+ 1)))
    (ok (every (lambda (offset) (typep offset '(unsigned-byte 16)))
               (loop for offset being the hash-values
                       of (luft.render::scene-material-cells scene)
                     collect offset)))
    (ok (eq luft.render::*terrain-material-placement*
            (luft.render::scene-material-placement-at scene earth-site)))
    (ok (eq luft.render::*sanctuary-material-placement*
            (luft.render::scene-material-placement-at scene stone-site)))
    (ok (eq luft.render::*sanctuary-material-frame*
            (luft.render::material-placement-frame
             (luft.render::scene-material-placement-at scene stone-site))))))

(deftest semantic-surface-assemblies-retain-the-named-built-in-abi
  (ok (equal
       (subseq
        (loop for assembly across
                (luv.domains:identity-vocabulary-members
                 luft.render::*surface-assembly-vocabulary*)
              collect (luft.render::surface-assembly-name assembly))
        0 9)
       '(:grass :soil :subsoil :limestone :turf-set-limestone
         :soil-set-limestone :deep-set-limestone :turf-edge
         :foundation-limestone)))
  (ok (equal (loop for offset below 9 collect offset)
             (list luft.render::+grass-stock+ luft.render::+soil-stock+
                   luft.render::+subsoil-stock+ luft.render::+stone-stock+
                   luft.render::+turf-set-stone-stock+
                   luft.render::+soil-set-stone-stock+
                   luft.render::+deep-set-stone-stock+
                   luft.render::+turf-edge-stock+
                   luft.render::+foundation-stone-stock+))))

(deftest semantic-closure-selects-earth-and-stone-appearances-by-meaning
  (let* ((program
           (luft.render::make-material-program
            (luft.render::make-scene-material-vocabulary)))
         (resolve
           (luft.render::make-compiled-material-chamfer-stock-function program)))
    (flet ((assembly (&rest stocks)
             (luft.render::surface-assembly-at (funcall resolve stocks))))
      (ok (eq :earth-set-stone
              (luft.render::surface-assembly-kernel
               (assembly luft.render::+stone-stock+
                         luft.render::+grass-stock+))))
      (ok (eq :exposed-top
              (luft.render::surface-reading-role
               (luft.render::surface-assembly-secondary
                (assembly luft.render::+stone-stock+
                          luft.render::+grass-stock+)))))
      (ok (eq :underside
              (luft.render::surface-reading-role
               (luft.render::surface-assembly-secondary
                (assembly luft.render::+stone-stock+
                          luft.render::+grass-stock+
                          luft.render::+subsoil-stock+)))))
      (ok (eq :turf-edge
              (luft.render::surface-assembly-kernel
               (assembly luft.render::+grass-stock+
                         luft.render::+soil-stock+)))))))

(deftest material-semantics-compile-once-before-dense-meshing
  (let ((luft.render::*material-placement-compilation-count* 0))
    (let* ((scene (render:make-mountain-sanctuary-scene))
           (compilations
             luft.render::*material-placement-compilation-count*)
           (placement-count
             (length
              (luv.domains:identity-vocabulary-members
               (luft.render::scene-material-vocabulary scene))))
           (program (luft.render::scene-material-program scene)))
      (ok (= placement-count compilations))
      (ok (typep
           (luft.render::material-program-placement-face-stocks program)
           '(simple-array (unsigned-byte 16) (*))))
      (ok (typep (luft.render::material-program-placement-flags program)
                 '(simple-array (unsigned-byte 8) (*))))
      (render:make-render-mesh scene)
      (ok (= compilations
             luft.render::*material-placement-compilation-count*)))))

(deftest equivalent-custom-placements-reuse-canonical-material-semantics
  (labels ((make-placement ()
             (make-instance
              'luft.render::material-placement
              :name :interned-test-stone
              :kind
              (make-instance
               'luft.render::stone-material-kind
               :name :interned-test-stone :base-tone '(0.41 0.42 0.43)
               :roughness 0.67 :relief :weathered-stone)
              :finish :dressed
              :frame
              (make-instance
               'luft.render::material-frame
               :name :interned-test-frame :origin '(11 13 0)
               :axes '((0 1 0) (-1 0 0) (0 0 1)))
              :role :architecture))
           (make-vocabulary (placements)
             (let ((vocabulary
                     (luft.render::make-scene-material-vocabulary)))
               (dolist (placement placements vocabulary)
                 (luv.domains:identity-vocabulary-offset
                  vocabulary placement))))
           (reading (placement role)
             (gethash role
                      (luft.render::material-placement-readings placement)))
           (all-eq-p (objects)
             (every (lambda (object) (eq object (first objects)))
                    (rest objects)))
           (assembly-count ()
             (length
              (luv.domains:identity-vocabulary-members
               luft.render::*surface-assembly-vocabulary*))))
    (let* ((first-placements (list (make-placement) (make-placement)))
           (first-vocabulary (make-vocabulary first-placements))
           ;; Program construction closes every ordered reading pair.  The
           ;; two authored placements are deliberately identity-distinct but
           ;; semantically equal, so a successful return is also the direct
           ;; regression against equal-key contact tie errors.
           (first-program
             (luft.render::make-material-program first-vocabulary))
           (first-assembly-count (assembly-count))
           (first-reading-count
             (hash-table-count luft.render::*surface-reading-intern-table*))
           (second-placements (list (make-placement) (make-placement)))
           (second-vocabulary (make-vocabulary second-placements))
           (second-program
             (luft.render::make-material-program second-vocabulary))
           (all-placements (append first-placements second-placements))
           (architecture-readings
             (mapcar (lambda (placement)
                       (reading placement :architecture))
                     all-placements))
           (foundation-readings
             (mapcar (lambda (placement)
                       (reading placement :foundation))
                     all-placements))
           (canonical-architecture
             (mapcar #'luft.render::canonical-surface-reading
                     architecture-readings))
           (canonical-foundation
             (mapcar #'luft.render::canonical-surface-reading
                     foundation-readings))
           (architecture-assemblies
             (mapcar #'luft.render::face-reading-assembly
                     architecture-readings))
           (foundation-assemblies
             (mapcar #'luft.render::face-reading-assembly
                     foundation-readings)))
      (ok first-program)
      (ok second-program)
      (ok (not (eq (first first-placements)
                   (second first-placements))))
      (ok (not (eq
                (luft.render::material-placement-kind
                 (first first-placements))
                (luft.render::material-placement-kind
                 (second first-placements)))))
      (ok (not (eq
                (luft.render::material-placement-frame
                 (first first-placements))
                (luft.render::material-placement-frame
                 (second first-placements)))))
      ;; Reconstructing and recompiling the same semantics may not extend the
      ;; renderer-global ABI or its canonical reading table.
      (ok (= first-assembly-count (assembly-count)))
      (ok (= first-reading-count
             (hash-table-count
              luft.render::*surface-reading-intern-table*)))
      (ok (= (length
              (luv.domains:identity-vocabulary-members first-vocabulary))
             (length
              (luv.domains:identity-vocabulary-members second-vocabulary))))
      ;; Authored reading objects remain placement-local, while the cold
      ;; semantic boundary maps every equivalent reconstruction to one EQ
      ;; reading and one EQ face assembly for each distinct role.
      (ok (not (all-eq-p architecture-readings)))
      (ok (not (all-eq-p foundation-readings)))
      (ok (all-eq-p canonical-architecture))
      (ok (all-eq-p canonical-foundation))
      (ok (not (eq (first canonical-architecture)
                   (first canonical-foundation))))
      (ok (all-eq-p architecture-assemblies))
      (ok (all-eq-p foundation-assemblies))
      ;; The historically failing equal-key comparison now collapses before
      ;; ranking, and contact assembly interning is likewise order-independent
      ;; and stable across both compilation rounds.
      (ok (eq (first architecture-assemblies)
              (luft.render::reading-contact-surface-assembly
               (first architecture-readings)
               (second architecture-readings))))
      (let ((contacts
              (loop for reading in architecture-readings append
                (list
                 (luft.render::reading-contact-surface-assembly
                  reading luft.render::*grass-reading*)
                 (luft.render::reading-contact-surface-assembly
                  luft.render::*grass-reading* reading)))))
        (ok (all-eq-p contacts)))
      (ok (= (luft.render::material-program-summary-count first-program)
             (luft.render::material-program-summary-count second-program)))
      (ok (equalp
           (luft.render::material-program-placement-face-stocks first-program)
           (luft.render::material-program-placement-face-stocks
            second-program))))))

(defclass unsupported-contact-material-kind
    (luft.render::material-kind) ())

(deftest semantic-contacts-are-host-owned-commutative-and-densely-compiled
  (let* ((program
           (luft.render::make-material-program
            (luft.render::make-scene-material-vocabulary)))
         (crystal-stock
           (luft.render::surface-assembly-offset
            luft.render::*crystal-surface*))
         (grass-crystal
           (luft.render::compiled-material-chamfer-stock
            program (list luft.render::+grass-stock+ crystal-stock)))
         (crystal-grass
           (luft.render::compiled-material-chamfer-stock
            program (list crystal-stock luft.render::+grass-stock+)))
         (stone-crystal
           (luft.render::compiled-material-chamfer-stock
            program (list luft.render::+stone-stock+ crystal-stock)))
         (crystal-stone
           (luft.render::compiled-material-chamfer-stock
            program (list crystal-stock luft.render::+stone-stock+)))
         (grass-contact
           (luft.render::surface-assembly-at grass-crystal))
         (stone-contact
           (luft.render::surface-assembly-at stone-crystal))
         (summary-masks
           (luft.render::material-program-assembly-summary-masks program))
         (summary-stocks
           (luft.render::material-program-summary-stocks program))
         (summary-count
           (luft.render::material-program-summary-count program)))
    ;; The terrain or architecture host owns the rendered contact, while the
    ;; crystal reading remains explicit context rather than becoming soil.
    (ok (= grass-crystal crystal-grass))
    (ok (eq luft.render::*grass-reading*
            (luft.render::surface-assembly-primary grass-contact)))
    (ok (eq luft.render::*crystal-reading*
            (luft.render::surface-assembly-secondary grass-contact)))
    (ok (= stone-crystal crystal-stone))
    (ok (eq luft.render::*stone-reading*
            (luft.render::surface-assembly-primary stone-contact)))
    (ok (eq luft.render::*crystal-reading*
            (luft.render::surface-assembly-secondary stone-contact)))
    ;; Every reachable stock round-trips through simple dense summary lanes.
    ;; The mask itself is the ACI join state; there are no identity flags or
    ;; ordered-pair special cases in the hot resolver.
    (ok (typep summary-masks '(simple-array (unsigned-byte 16) (*))))
    (ok (typep summary-stocks '(simple-array (unsigned-byte 16) (*))))
    (ok (= (1+ summary-count) (length summary-stocks)))
    (ok
     (loop for stock below (length summary-masks)
           for mask = (aref summary-masks stock)
           always (or (= mask luft.render::+material-program-no-summary+)
                      (and (<= 1 mask summary-count)
                           (= stock (aref summary-stocks mask))))))
    ;; An unmodelled material family fails at the cold protocol boundary.  It
    ;; cannot silently inherit the historical soil result.
    (let* ((kind
             (make-instance
              'unsupported-contact-material-kind
              :name :unsupported :base-tone '(0.0 0.0 0.0)
              :roughness 1.0 :relief :none))
           (reading
             (make-instance
              'luft.render::surface-reading :name :unsupported
              :kind kind :tone '(0.0 0.0 0.0) :finish :none
              :frame luft.render::*world-material-frame* :role :unsupported)))
      (ok
       (signals
        (luft.render::material-contact-surface-assembly
         kind kind reading reading)
        'error)))))

(deftest unsupported-face-roles-fail-before-dense-material-compilation
  (let* ((kind
           (make-instance
            'luft.render::earth-material-kind
            :name :unsupported-face-role :base-tone '(0.13 0.17 0.19)
            :roughness 0.8 :relief :granular))
         (reading
           (make-instance
            'luft.render::surface-reading
            :name :unsupported-face-role :kind kind
            :tone '(0.13 0.17 0.19) :finish :raw
            :frame luft.render::*world-material-frame*
            :role :unsupported-face-role)))
    (ok (signals (luft.render::face-reading-assembly reading) 'error))
    (ok (signals
         (luft.render::surface-reading-material-bevel-mask reading)
         'error))))

(deftest distinct-same-family-contacts-have-exact-canonical-pair-summaries
  (let* ((crystal-kind-a
           (make-instance
            'luft.render::crystal-material-kind
            :name :pair-crystal-a :base-tone '(0.12 0.41 0.79)
            :roughness 0.2 :relief :crystal))
         (crystal-kind-b
           (make-instance
            'luft.render::crystal-material-kind
            :name :pair-crystal-b :base-tone '(0.71 0.31 0.18)
            :roughness 0.3 :relief :crystal))
         (crystal-a
           (make-instance
            'luft.render::surface-reading
            :name :pair-crystal-a :kind crystal-kind-a
            :tone '(0.12 0.41 0.79) :finish :faceted
            :frame luft.render::*world-material-frame* :role :crystal))
         (crystal-b
           (make-instance
            'luft.render::surface-reading
            :name :pair-crystal-b :kind crystal-kind-b
            :tone '(0.71 0.31 0.18) :finish :faceted
            :frame luft.render::*sanctuary-material-frame* :role :crystal))
         (pairs
           (list (list luft.render::*grass-reading*
                       luft.render::*soil-reading*)
                 (list luft.render::*stone-reading*
                       luft.render::*foundation-stone-reading*)
                 (list crystal-a crystal-b))))
    (dolist (pair pairs)
      (destructuring-bind (left right) pair
        (let* ((forward
                 (luft.render::reading-contact-surface-assembly left right))
               (reverse
                 (luft.render::reading-contact-surface-assembly right left))
               (expected
                 (luft.render::intern-surface-closure-summary
                  (list left right)))
               (actual
                 (luft.render::surface-assembly-closure-summary forward)))
          (ok (not (eq (luft.render::canonical-surface-reading left)
                       (luft.render::canonical-surface-reading right))))
          (ok (eq forward reverse))
          (ok (eq expected actual))
          (ok (= 2
                 (length
                  (luft.render::surface-closure-summary-readings actual)))))))))

(deftest custom-material-readings-and-descriptors-have-only-authored-lineage
  (let* ((earth-top '(0.17 0.63 0.24))
         (earth-side '(0.31 0.22 0.11))
         (earth-under '(0.09 0.07 0.04))
         (stone-tone '(0.73 0.51 0.27))
         (crystal-tone '(0.15 0.49 0.91))
         (earth-kind
           (make-instance
            'luft.render::earth-material-kind
            :name :lineage-earth :base-tone '(0.2 0.2 0.2)
            :top-tone earth-top :side-tone earth-side
            :underside-tone earth-under
            :roughness 0.83 :relief :granular))
         (default-earth-kind
           (make-instance
            'luft.render::earth-material-kind
            :name :lineage-default-earth :base-tone '(0.44 0.12 0.38)
            :roughness 0.88 :relief :granular))
         (stone-kind
           (make-instance
            'luft.render::stone-material-kind
            :name :lineage-stone :base-tone stone-tone
            :roughness 0.57 :relief :weathered-stone))
         (crystal-kind
           (make-instance
            'luft.render::crystal-material-kind
            :name :lineage-crystal :base-tone crystal-tone
            :roughness 0.19 :relief :crystal))
         (earth-frame
           (make-instance
            'luft.render::material-frame :name :lineage-earth-frame
            :origin '(3 5 7) :axes '((0 1 0) (-1 0 0) (0 0 1))))
         (stone-frame
           (make-instance
            'luft.render::material-frame :name :lineage-stone-frame
            :origin '(11 13 17) :axes '((1 0 0) (0 0 1) (0 -1 0))))
         (crystal-frame
           (make-instance
            'luft.render::material-frame :name :lineage-crystal-frame
            :origin '(19 23 29) :axes '((0 -1 0) (1 0 0) (0 0 1))))
         (earth-placement
           (make-instance
            'luft.render::material-placement
            :name :lineage-earth :kind earth-kind :finish :hand-cut
            :frame earth-frame :role :terrain))
         (default-earth-placement
           (make-instance
            'luft.render::material-placement
            :name :lineage-default-earth :kind default-earth-kind
            :finish :raw :frame earth-frame :role :terrain))
         (stone-placement
           (make-instance
            'luft.render::material-placement
            :name :lineage-stone :kind stone-kind :finish :dressed
            :frame stone-frame :role :architecture))
         (crystal-placement
           (make-instance
            'luft.render::material-placement
            :name :lineage-crystal :kind crystal-kind :finish :faceted
            :frame crystal-frame :role :crystal))
         (earth-readings
           (luft.render::compile-material-placement
            earth-kind earth-placement))
         (default-earth-readings
           (luft.render::compile-material-placement
            default-earth-kind default-earth-placement))
         (stone-readings
           (luft.render::compile-material-placement
            stone-kind stone-placement))
         (crystal-readings
           (luft.render::compile-material-placement
            crystal-kind crystal-placement))
         (earth-side-reading (aref earth-readings 0))
         (earth-top-reading (aref earth-readings 4))
         (earth-under-reading (aref earth-readings 5))
         (stone-reading (aref stone-readings 0))
         (foundation-reading (aref stone-readings 6))
         (crystal-reading (aref crystal-readings 0))
         (earth-face
           (luft.render::face-reading-assembly earth-top-reading))
         (foundation-face
           (luft.render::face-reading-assembly foundation-reading))
         (crystal-face
           (luft.render::face-reading-assembly crystal-reading))
         (contact
           (luft.render::reading-contact-surface-assembly
            stone-reading earth-side-reading))
         (assemblies (vector earth-face foundation-face crystal-face contact))
         (vocabulary
           (luv.domains:make-identity-vocabulary-domain
            :members (coerce assemblies 'list) :limit #x1000))
         (words
           (luft.render::surface-assembly-descriptor-words vocabulary))
         (stride (* luft.render::+surface-assembly-descriptor-row-count+ 4)))
    (labels ((assert-reading (reading tone frame finish)
               (ok (equalp tone (luft.render::surface-reading-tone reading)))
               (ok (eq frame (luft.render::surface-reading-frame reading)))
               (ok (eq finish (luft.render::surface-reading-finish reading))))
             (row (assembly row-index)
               (let* ((assembly-index
                        (luv.domains:identity-vocabulary-offset
                         vocabulary assembly))
                      (start (+ (* assembly-index stride) (* row-index 4))))
                 (subseq words start (+ start 4))))
             (tone-row (assembly row-index tone)
               (ok (equalp (coerce tone 'vector)
                           (subseq (row assembly row-index) 0 3))))
             (frame-rows (assembly frame)
               (ok (equalp (coerce
                            (luft.render::material-frame-origin frame) 'vector)
                           (subseq (row assembly 4) 0 3)))
               (loop for axis in (luft.render::material-frame-axes frame)
                     for row-index from 5
                     do (ok (equalp (coerce axis 'vector)
                                    (subseq (row assembly row-index)
                                            0 3))))))
      (assert-reading earth-top-reading earth-top earth-frame :hand-cut)
      (assert-reading earth-side-reading earth-side earth-frame :hand-cut)
      (assert-reading earth-under-reading earth-under earth-frame :hand-cut)
      (loop for index in '(0 4 5)
            do (ok (equalp '(0.44 0.12 0.38)
                           (luft.render::surface-reading-tone
                            (aref default-earth-readings index)))))
      (assert-reading stone-reading stone-tone stone-frame :dressed)
      (assert-reading foundation-reading stone-tone stone-frame :dressed)
      (assert-reading crystal-reading crystal-tone crystal-frame :faceted)
      ;; A foundation singleton contains no invented global soil/subsoil
      ;; context; descriptor fallback repeats its own authored primary.
      (ok (null (luft.render::surface-assembly-secondary foundation-face)))
      (ok (null (luft.render::surface-assembly-tertiary foundation-face)))
      (dotimes (row-index 3)
        (tone-row foundation-face row-index stone-tone))
      (frame-rows foundation-face stone-frame)
      ;; Contact lanes name only the two actual placements.  Row two falls
      ;; back to the incident earth secondary rather than a global palette.
      (ok (eq (luft.render::canonical-surface-reading stone-reading)
              (luft.render::surface-assembly-primary contact)))
      (ok (eq (luft.render::canonical-surface-reading earth-side-reading)
              (luft.render::surface-assembly-secondary contact)))
      (ok (null (luft.render::surface-assembly-tertiary contact)))
      (tone-row contact 0 stone-tone)
      (tone-row contact 1 earth-side)
      (tone-row contact 2 earth-side)
      (frame-rows contact stone-frame)
      (tone-row earth-face 0 earth-top)
      (frame-rows earth-face earth-frame)
      (tone-row crystal-face 0 crystal-tone)
      (frame-rows crystal-face crystal-frame))))

;;; ---------------------------------------------------------------------------
;;; Closure-summary algebra

(deftest closure-summary-hot-algebra-is-aci-and-retains-derived-provenance
  (let* ((program
           (luft.render::make-material-program
            (luft.render::make-scene-material-vocabulary)))
         (resolve
           (luft.render::make-compiled-material-chamfer-stock-function program))
         (crystal
           (luft.render::surface-assembly-offset
            luft.render::*crystal-surface*))
         (basis
           (list luft.render::+grass-stock+ luft.render::+soil-stock+
                 luft.render::+subsoil-stock+ luft.render::+stone-stock+
                 crystal)))
    (flet ((join (&rest stocks) (funcall resolve stocks)))
      ;; Exhaust the ACI laws over the five material readings which matter at
      ;; terrain, masonry, and crystal junctions.  Parenthesized joins feed
      ;; derived stocks back through the same hot resolver used by vertex fans.
      (ok (loop for left in basis
                always (= (join left) (join left left))))
      (ok (loop for left in basis
                always
                (loop for right in basis
                      always (= (join left right) (join right left)))))
      (ok (loop for left in basis
                always
                (loop for right in basis
                      always
                      (loop for third in basis
                            for direct = (join left right third)
                            always
                            (and (= direct (join (join left right) third))
                                 (= direct
                                    (join left (join right third))))))))
      ;; Soil+subsoil renders as the same soil face appearance as soil alone,
      ;; but it is a distinct stock because its provenance must survive a later
      ;; fan join.
      (let* ((soil luft.render::+soil-stock+)
             (soil-subsoil
               (join soil luft.render::+subsoil-stock+))
             (soil-appearance (luft.render::surface-assembly-at soil))
             (derived-appearance
               (luft.render::surface-assembly-at soil-subsoil)))
        (ok (/= soil soil-subsoil))
        (ok (eq (luft.render::surface-assembly-primary soil-appearance)
                (luft.render::surface-assembly-primary derived-appearance)))
        (ok (eq (luft.render::surface-assembly-kernel soil-appearance)
                (luft.render::surface-assembly-kernel derived-appearance)))
        (ok (= 2
               (length
                (luft.render::surface-closure-summary-readings
                 (luft.render::surface-assembly-closure-summary
                  derived-appearance))))))
      ;; Crystal remains explicit visible context at the three-way closure;
      ;; every permutation and parenthesization resolves to this same stock.
      (let* ((triple
               (list luft.render::+grass-stock+ luft.render::+stone-stock+
                     crystal))
             (expected (apply #'join triple))
             (assembly (luft.render::surface-assembly-at expected)))
        (labels ((permutations (items)
                   (if (null items)
                       (list nil)
                       (loop for item in items append
                         (mapcar
                          (lambda (tail) (cons item tail))
                          (permutations
                           (remove item items :count 1 :test #'=)))))))
          (dolist (permutation (permutations triple))
            (ok (= expected (apply #'join permutation)))))
        (ok (eq luft.render::*stone-reading*
                (luft.render::surface-assembly-primary assembly)))
        (ok (eq luft.render::*crystal-reading*
                (luft.render::surface-assembly-secondary assembly)))
        (ok (= 3
               (length
                (luft.render::surface-closure-summary-readings
                 (luft.render::surface-assembly-closure-summary assembly)))))
        (ok (= luft.render::+material-bevel-three-way-mask+
               (luft.render::surface-assembly-material-bevel-mask assembly)))))))

(deftest custom-placement-closure-compiles-frames-tones-and-complete-summary
  (let* ((earth-kind
           (make-instance
            'luft.render::earth-material-kind
            :name :closure-test-earth :base-tone '(0.31 0.27 0.19)
            :roughness 0.81 :relief :granular))
         (stone-kind
           (make-instance
            'luft.render::stone-material-kind
            :name :closure-test-stone :base-tone '(0.61 0.57 0.51)
            :roughness 0.63 :relief :weathered-stone))
         (earth-frame
           (make-instance
            'luft.render::material-frame :name :closure-test-earth-frame
            :origin '(7 9 2) :axes '((0 1 0) (-1 0 0) (0 0 1))))
         (stone-frame
           (make-instance
            'luft.render::material-frame :name :closure-test-stone-frame
            :origin '(11 13 3) :axes '((1 0 0) (0 0 1) (0 -1 0))))
         (earth
           (make-instance
            'luft.render::material-placement :name :closure-test-earth
            :kind earth-kind :finish :living :frame earth-frame :role :terrain))
         (stone
           (make-instance
            'luft.render::material-placement :name :closure-test-stone
            :kind stone-kind :finish :dressed :frame stone-frame
            :role :architecture))
         (vocabulary
           (luv.domains:make-identity-vocabulary-domain
            :members (list earth stone) :limit #x10000))
         (program (luft.render::make-material-program vocabulary))
         (faces (luft.render::material-program-placement-face-stocks program))
         (earth-top (aref faces 4))
         (stone-face
           (aref faces luft.render::+material-placement-face-stride+))
         (stock
           (luft.render::compiled-material-chamfer-stock
            program (list earth-top stone-face)))
         (assembly (luft.render::surface-assembly-at stock))
         (primary (luft.render::surface-assembly-primary assembly))
         (secondary (luft.render::surface-assembly-secondary assembly)))
    (ok (= 31 (luft.render::material-program-summary-count program)))
    (ok (equalp '(0.61 0.57 0.51)
                (luft.render::material-kind-base-tone
                 (luft.render::surface-reading-kind primary))))
    (ok (equalp '(0.31 0.27 0.19)
                (luft.render::material-kind-base-tone
                 (luft.render::surface-reading-kind secondary))))
    (ok (equalp (luft.render::material-frame-origin stone-frame)
                (luft.render::material-frame-origin
                 (luft.render::surface-reading-frame primary))))
    (ok (equalp (luft.render::material-frame-axes earth-frame)
                (luft.render::material-frame-axes
                 (luft.render::surface-reading-frame secondary))))
    (ok (= 2
           (length
            (luft.render::surface-closure-summary-readings
             (luft.render::surface-assembly-closure-summary assembly)))))))

(deftest unrelated-global-assemblies-do-not-expand-a-scene-closure-automaton
  (let* ((vocabulary (luft.render::make-scene-material-vocabulary))
         (before (luft.render::make-material-program vocabulary))
         (before-count (luft.render::material-program-summary-count before))
         (before-lane-length
           (length
            (luft.render::material-program-assembly-summary-masks before)))
         (unrelated
           (make-instance
            'luft.render::surface-reading
            :name :unrelated-closure-reading
            :kind luft.render::*limestone-material*
            :tone '(0.73 0.11 0.29) :finish :polished
            :frame
            (make-instance
             'luft.render::material-frame :name :unrelated-closure-frame
             :origin '(101 103 7) :axes '((1 0 0) (0 1 0) (0 0 1)))
            :role :architecture)))
    (luft.render::intern-surface-assembly
     :face unrelated :kernel :stone :closure-readings (list unrelated))
    (let ((after (luft.render::make-material-program vocabulary)))
      (ok (= before-count
             (luft.render::material-program-summary-count after)))
      (ok (= before-lane-length
             (length
              (luft.render::material-program-assembly-summary-masks after)))))))

(deftest scene-local-material-program-seeds-only-active-placements
  (let ((vocabulary (luft.render::make-scene-material-vocabulary)))
    ;; Terrain has three face readings, sanctuary masonry two, and crystal one.
    (ok (= 7
           (luft.render::material-program-summary-count
            (luft.render::make-material-program
             vocabulary :active-placement-offsets (list 0)))))
    (ok (= 3
           (luft.render::material-program-summary-count
            (luft.render::make-material-program
             vocabulary :active-placement-offsets (list 2)))))
    (ok (= 1
           (luft.render::material-program-summary-count
            (luft.render::make-material-program
             vocabulary :active-placement-offsets (list 3)))))
    (ok (= 127
           (luft.render::material-program-summary-count
            (luft.render::make-material-program vocabulary))))
    ;; An empty scene deliberately retains the terrain singleton algebra so its
    ;; resolver remains callable; malformed active sets fail at the cold API.
    (ok (= 7
           (luft.render::material-program-summary-count
            (luft.render::make-material-program
             vocabulary :active-placement-offsets nil))))
    (ok (signals
         (luft.render::make-material-program
          vocabulary :active-placement-offsets (list 0 0))
         'error))
    (ok (signals
         (luft.render::make-material-program
          vocabulary :active-placement-offsets (list 99))
         'error))))

(deftest compiled-placement-local-materials-match-the-semantic-oracle
  (labels ((same-surface-p (left right)
             (canonical-mesh-cohorts-equal-p
              (surface-mesh-tree-meshes left)
              (surface-mesh-tree-meshes right))))
    (let ((scene (render:make-mountain-sanctuary-scene)))
      (ok
       (same-surface-p
        (render:make-render-mesh scene)
        (render:make-whole-domain-diagnostic-mesh
         scene
         :stock-function
         (lambda (face)
           (luft.render::surface-assembly-offset
            (luft.render::face-reading-assembly
             (luft.render::scene-face-reading scene face))))
         :chamfer-stock-function
         (lambda (stocks)
           (luft.render::surface-assembly-offset
            (luft.render::closure-surface-assembly
             (mapcar #'luft.render::surface-assembly-at stocks))))))))))

(deftest compiled-contacts-retain-both-authored-placement-frames
  (let* ((earth-frame
           (make-instance 'luft.render::material-frame
                          :name :test-earth :origin '(4 4 2)
                          :axes '((1 0 0) (0 1 0) (0 0 1))))
         (stone-frame
           (make-instance 'luft.render::material-frame
                          :name :test-stone :origin '(5 5 3)
                          :axes '((0 1 0) (-1 0 0) (0 0 1))))
         (earth
           (make-instance 'luft.render::material-placement
                          :name :test-earth :kind luft.render::*earth-material*
                          :finish :living :frame earth-frame :role :terrain))
         (stone
           (make-instance
            'luft.render::material-placement
            :name :test-stone :kind luft.render::*limestone-material*
            :finish :dressed :frame stone-frame :role :architecture))
         (builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene
           (progn
             (luft.render::scene-builder-box
              builder 4 6 4 6 2 2 :material earth)
             (luft.render::scene-builder-cell
              builder 5 5 3 :material stone)
             (luft.render::finish-scene-builder builder)))
         (compiled (render:make-render-mesh scene))
         (semantic
         (render:make-whole-domain-diagnostic-mesh
            scene
            :stock-function
            (lambda (face)
              (luft.render::surface-assembly-offset
               (luft.render::face-reading-assembly
                (luft.render::scene-face-reading scene face))))
            :chamfer-stock-function
            (lambda (stocks)
              (luft.render::surface-assembly-offset
               (luft.render::closure-surface-assembly
                (mapcar #'luft.render::surface-assembly-at stocks)))))))
    (ok
     (every #'equalp
            (list (luft:surface-mesh-face-instance-words compiled)
                  (luft:surface-mesh-band-instance-words compiled)
                  (luft:surface-mesh-fan-instance-words compiled))
            (list (luft:surface-mesh-face-instance-words semantic)
                  (luft:surface-mesh-band-instance-words semantic)
                  (luft:surface-mesh-fan-instance-words semantic))))
    (ok
     (find-if
      (lambda (assembly)
        (and (eq :contact
                 (luft.render::surface-assembly-relation assembly))
             (let ((actual
                     (luft.render::surface-reading-frame
                      (luft.render::surface-assembly-primary assembly))))
               (and (equal (luft.render::material-frame-name stone-frame)
                           (luft.render::material-frame-name actual))
                    (equalp (luft.render::material-frame-origin stone-frame)
                            (luft.render::material-frame-origin actual))
                    (equalp (luft.render::material-frame-axes stone-frame)
                            (luft.render::material-frame-axes actual))))
             (let ((actual
                     (luft.render::surface-reading-frame
                      (luft.render::surface-assembly-secondary assembly))))
               (and (equal (luft.render::material-frame-name earth-frame)
                           (luft.render::material-frame-name actual))
                    (equalp (luft.render::material-frame-origin earth-frame)
                            (luft.render::material-frame-origin actual))
                    (equalp (luft.render::material-frame-axes earth-frame)
                            (luft.render::material-frame-axes actual))))))
      (luv.domains:identity-vocabulary-members
       luft.render::*surface-assembly-vocabulary*)))))

(deftest surface-assembly-descriptors-compile-semantic-material-data
  (let* ((words (luft.render::surface-assembly-descriptor-words))
         (stride (* luft.render::+surface-assembly-descriptor-row-count+ 4))
         (body-stock
           (luft.render::surface-assembly-offset
            luft.render::*torch-body-surface*))
         (body-row (* body-stock stride)))
    (ok (= 8 luft.render::+surface-assembly-descriptor-row-count+))
    (ok (<= (* 9 luft.render::+surface-assembly-descriptor-row-count+ 4)
            (length words)))
    (ok (equalp #(0.18 0.31 0.105 7.0) (subseq words 0 4)))
    (ok (equalp #(0.0 0.0 0.0 0.0) (subseq words 12 16)))
    (let ((contact (* luft.render::+turf-set-stone-stock+
                      luft.render::+surface-assembly-descriptor-row-count+ 4)))
      (ok (equalp #(0.53 0.49 0.39 1.0)
                  (subseq words contact (+ contact 4))))
      (ok (equalp #(0.18 0.31 0.105 0.0)
                  (subseq words (+ contact 4) (+ contact 8)))))
    (ok (typep luft.render::*torch-body-material*
               'luft.render::metal-material-kind))
    (ok (not (typep luft.render::*torch-body-material*
                    'luft.render::stone-material-kind)))
    (ok (eq :forged-metal
            (luft.render::material-kind-relief
             luft.render::*torch-body-material*)))
    (ok (equalp #(0.88 0.0 0.0 0.0)
                (subseq words (+ body-row 12) (+ body-row 16))))
    (ok (signals
         (make-instance
          'luft.render::material-kind :name :invalid-negative-metalness
          :base-tone '(0 0 0) :roughness 1.0 :metalness -0.01
          :relief :granular)
         'error))
    (ok (signals
         (make-instance
          'luft.render::material-kind :name :invalid-high-metalness
          :base-tone '(0 0 0) :roughness 1.0 :metalness 1.01
          :relief :granular)
         'error))
    (let* ((low-kind
             (make-instance
              'luft.render::metal-material-kind
              :name :test-metalness-key :base-tone '(0.4 0.2 0.1)
              :roughness 0.5 :metalness 0.2 :relief :forged-metal))
           (high-kind
             (make-instance
              'luft.render::metal-material-kind
              :name :test-metalness-key :base-tone '(0.4 0.2 0.1)
              :roughness 0.5 :metalness 0.8 :relief :forged-metal))
           (low
             (make-instance
              'luft.render::surface-reading :name :test-metalness-reading
              :kind low-kind :tone '(0.4 0.2 0.1) :finish :forged
              :frame luft.render::*world-material-frame* :role :attachment))
           (high
             (make-instance
              'luft.render::surface-reading :name :test-metalness-reading
              :kind high-kind :tone '(0.4 0.2 0.1) :finish :forged
              :frame luft.render::*world-material-frame* :role :attachment)))
      (ok (not (equalp (luft.render::surface-reading-semantic-key low)
                       (luft.render::surface-reading-semantic-key high))))
      (ok (not (eq (luft.render::canonical-surface-reading low)
                   (luft.render::canonical-surface-reading high)))))))

(deftest crystal-optics-compile-as-independent-render-and-light-facts
  (let* ((words (luft.render::surface-assembly-descriptor-words))
         (stride (* luft.render::+surface-assembly-descriptor-row-count+ 4))
         (crystal (* (luft.render::surface-assembly-offset
                      luft.render::*crystal-surface*)
                     stride))
         (vocabulary (luft.render::make-scene-material-vocabulary))
         (opacities
           (luft.render::compile-material-light-opacity-table vocabulary))
         (crystal-placement
           (luv.domains:identity-vocabulary-offset
            vocabulary luft.render::*crystal-material-placement*)))
    ;; Crystal-only secondary/tertiary tone lanes are a compact optical ABI.
    (ok (equalp #(1.62 0.48 0.58 0.42)
                (subseq words (+ crystal 4) (+ crystal 8))))
    (ok (equalp #(18.0 0.30 0.88 0.14)
                (subseq words (+ crystal 8) (+ crystal 12))))
    ;; Descriptor Y.w is visual opacity and Z.w is visible HDR emission.  The
    ;; explicit physical row shifts the authored frame to rows four through
    ;; seven without conflating either optical fact with metalness.
    (ok (< (abs (- 0.48 (aref words (+ crystal 27)))) 1.0e-6))
    (ok (< (abs (- 0.30 (aref words (+ crystal 31)))) 1.0e-6))
    ;; Flame is a volume/effect material, not a fake surface assembly.  Its
    ;; authored facts feed voxel light and the HDR effect uniform directly.
    (ok (typep luft.render::*torch-flame-material*
               'luft.render::luminous-material-kind))
    (ok (= 1.0
           (luft.render::material-kind-opacity
            luft.render::*torch-flame-material*)))
    (ok (= 1.8
           (luft.render::material-kind-surface-emission
            luft.render::*torch-flame-material*)))
    (ok (= (luft:pack-voxel-light 15 9 3)
           (luft.render::material-kind-packed-light-emission
            luft.render::*torch-flame-material*)))
    (ok (not (boundp 'luft.render::*torch-flame-reading*)))
    (ok (not (boundp 'luft.render::*torch-flame-surface*)))
    (ok
     (notany
      (lambda (assembly)
        (eq luft.render::*torch-flame-material*
            (luft.render::surface-reading-kind
             (luft.render::surface-assembly-primary assembly))))
      (coerce
       (luv.domains:identity-vocabulary-members
        luft.render::*surface-assembly-vocabulary*)
       'list)))
    ;; Propagation is a separate CPU lane: crystal transmits with entered
    ;; opacity one while ordinary authored solids remain fully blocking.
    (ok (= 1 (aref opacities crystal-placement)))
    (ok (= 15 (aref opacities 0)))
    (ok (= (luft:pack-voxel-light 3 11 15)
           (luft.render::material-kind-packed-light-emission
            luft.render::*crystal-material*)))))

(deftest realized-torch-seeds-preserve-the-six-flat-face-center-source
  (let* ((domain (luft:make-world-domain :horizontal-bits 4))
         (provenance (gensym "FLAT-LIGHT-"))
         (emission (luft:pack-voxel-light 15 9 3))
         (cases
           '((#(4.0f0 4.5f0 4.5f0) #(-1.0f0 0.0f0 0.0f0) (3 4 4))
             (#(5.0f0 4.5f0 4.5f0) #(1.0f0 0.0f0 0.0f0) (5 4 4))
             (#(4.5f0 4.0f0 4.5f0) #(0.0f0 -1.0f0 0.0f0) (4 3 4))
             (#(4.5f0 5.0f0 4.5f0) #(0.0f0 1.0f0 0.0f0) (4 5 4))
             (#(4.5f0 4.5f0 4.0f0) #(0.0f0 0.0f0 -1.0f0) (4 4 3))
             (#(4.5f0 4.5f0 5.0f0) #(0.0f0 0.0f0 1.0f0) (4 4 5)))))
    (dolist (case cases)
      (destructuring-bind (origin normal coordinates) case
        (destructuring-bind (x y z) coordinates
          (let* ((wick
                   (luft.render::realized-torch-wick-point
                    origin normal 1.0f0 0.5f0))
                 (site (luft:make-site
                        domain x y z luft:+cell-extent+ 1))
                 (seeds
                   (luft.render::realized-torch-light-seeds
                    domain (constantly nil) wick emission))
                 (stamp
                   (luft.render::make-realized-light-stamp
                    provenance 7 seeds))
                 (packed
                   (luft.render::%realized-light-voxel-sources #() stamp)))
            (ok (= 1 (luft.render::realized-light-seeds-count seeds)))
            (ok (equalp (vector site)
                        (luft.render::realized-light-seeds-sites seeds)))
            (ok (equalp (vector emission)
                        (luft.render::realized-light-seeds-lights seeds)))
            ;; This is byte-for-byte the former adjacent cell-center source,
            ;; now derived uniformly from the final surface normal.
            (ok (equalp
                 (vector (luft:make-voxel-light-source site emission))
                 packed))))))))

(deftest realized-light-seeds-reproduce-the-quantized-l1-cone
  (let ((domain (luft:make-world-domain :horizontal-bits 3))
        (provenance (gensym "L1-LIGHT-"))
        (emission (luft:pack-voxel-light 15 11 7))
        (material-cells (make-hash-table :test #'eql))
        (opacity (make-array 0 :element-type '(unsigned-byte 8)))
        (authored (make-array 0 :element-type '(unsigned-byte 64))))
    (ok (= 10 (luft.render::%quantize-max-plus-light-lane 10 0.4999d0)))
    (ok (= 10 (luft.render::%quantize-max-plus-light-lane 10 0.5d0)))
    (ok (= 9 (luft.render::%quantize-max-plus-light-lane 10 0.5001d0)))
    (flet ((distance-to-center (point axis cell-coordinate)
             (abs
              (- (coerce (aref point axis) 'double-float)
                 (+ cell-coordinate 0.5d0)))))
      (dotimes (index 12)
        (let* ((point
               (make-array
                3 :element-type 'single-float
                :initial-contents
                (list (+ 1.6f0 (* 0.01f0 (mod (+ 11 (* index 37)) 420)))
                      (+ 1.7f0 (* 0.01f0 (mod (+ 23 (* index 53)) 410)))
                      (+ 1.8f0 (* 0.01f0 (mod (+ 31 (* index 29)) 390))))))
               (seeds
                 (luft.render::realized-torch-light-seeds
                  domain (constantly nil) point emission))
               (generation
                (luft.render::solve-realized-light-generation
                  domain material-cells opacity authored provenance 0 seeds))
               (field
                 (luft.render::realized-light-generation-field generation)))
          ;; Deterministic pseudorandom sub-cell phases cover either side of the
          ;; center-bracketing and half-up quantization boundaries.  At every
          ;; lattice center the settled max/subtractive field is the same direct
          ;; quantized L1 cone as the continuous authored source law.
          (dolist (coordinates '((0 0 0) (1 3 5) (2 2 2) (3 4 1)
                                 (4 3 6) (5 5 5) (6 1 4) (7 7 7)))
            (destructuring-bind (x y z) coordinates
              (let* ((site (luft:make-site
                            domain x y z luft:+cell-extent+ 1))
                     (distance
                       (+ (distance-to-center point 0 x)
                          (distance-to-center point 1 y)
                          (distance-to-center point 2 z)))
                     (expected
                       (luft.render::%attenuate-realized-light
                        emission distance)))
                (ok (= expected
                       (luft:voxel-light-at-site field site)))))))))))

(deftest realized-light-seeds-exclude-authored-occupied-brackets
  (let* ((domain (luft:make-world-domain :horizontal-bits 3))
         (point #(3.0f0 3.0f0 3.0f0))
         (emission (luft:pack-voxel-light 15 9 3))
         (excluded (luft:make-site domain 2 2 2 luft:+cell-extent+ 1))
         (seeds
           (luft.render::realized-torch-light-seeds
            domain (lambda (cell) (= cell excluded)) point emission)))
    (ok (= 7 (luft.render::realized-light-seeds-count seeds)))
    (ok (not (find excluded
                   (luft.render::realized-light-seeds-sites seeds))))
    (ok (signals
         (luft.render::realized-torch-light-seeds
          domain (constantly t) point emission)
         'luft.render::unrealizable-torch-light-source))))

(deftest realized-light-duplicates-and-competing-colors-meet-by-component-max
  (let* ((domain (luft:make-world-domain :horizontal-bits 3))
         (provenance (gensym "COLOR-LIGHT-"))
         (site (luft:make-site domain 3 3 3 luft:+cell-extent+ 1))
         (warm (luft:pack-voxel-light 15 2 1))
         (cool (luft:pack-voxel-light 3 11 14))
         (joined (luft:pack-voxel-light 15 11 14))
         (seeds
           (luft.render::make-realized-light-seeds
            (vector site site) (vector warm cool)))
         (authored
           (make-array
            1 :element-type '(unsigned-byte 64)
            :initial-element (luft:make-voxel-light-source site warm)))
         (generation
           (luft.render::solve-realized-light-generation
            domain (make-hash-table :test #'eql)
            (make-array 0 :element-type '(unsigned-byte 8))
            authored provenance 0
            (luft.render::make-realized-light-seeds
             (vector site) (vector cool)))))
    (ok (= 1 (luft.render::realized-light-seeds-count seeds)))
    (ok (equalp (vector joined)
                (luft.render::realized-light-seeds-lights seeds)))
    (ok (= joined
           (luft:voxel-light-at-site
            (luft.render::realized-light-generation-field generation)
            site)))))

(deftest realized-light-seeds-and-stamps-own-exact-copies
  (let* ((domain (luft:make-world-domain :horizontal-bits 3))
         (provenance (gensym "COPY-LIGHT-"))
         (first (luft:make-site domain 2 2 2 luft:+cell-extent+ 1))
         (second (luft:make-site domain 4 4 4 luft:+cell-extent+ 1))
         (sites (vector second first))
         (lights (vector (luft:pack-voxel-light 2 3 4)
                         (luft:pack-voxel-light 9 8 7)))
         (seeds (luft.render::make-realized-light-seeds sites lights))
         (stamp
           (luft.render::make-realized-light-stamp provenance 19 seeds))
         (same-stamp
           (luft.render::make-realized-light-stamp provenance 19 seeds))
         (foreign-stamp
           (luft.render::make-realized-light-stamp
            (gensym "FOREIGN-LIGHT-") 19 seeds))
         (site-copy (luft.render::realized-light-stamp-seed-sites stamp))
         (light-copy (luft.render::realized-light-stamp-seed-lights stamp)))
    (setf (aref sites 0) first
          (aref lights 0) 0
          (aref site-copy 0) second
          (aref light-copy 0) 0)
    (ok (equalp (vector first second)
                (luft.render::realized-light-stamp-seed-sites stamp)))
    (ok (equalp
         (vector (luft:pack-voxel-light 9 8 7)
                 (luft:pack-voxel-light 2 3 4))
         (luft.render::realized-light-stamp-seed-lights stamp)))
    (ok (luft.render::realized-light-stamp= stamp same-stamp))
    (ok (not (luft.render::realized-light-stamp= stamp foreign-stamp)))))

(deftest voxel-light-shrine-separates-authored-base-from-realized-torches
  (let* ((scene (render:make-voxel-light-shrine-scene))
         (solid (render:scene-solid scene))
         (domain (luft:chain-domain solid))
         (base-field (render:scene-authored-voxel-light scene))
         (crystal (luft:make-site domain 12 13 5 luft:+cell-extent+ 1))
         (backing (luft:make-site domain 12 13 4 luft:+cell-extent+ 1))
         (crystal-light (luft:voxel-light-at-site base-field crystal)))
    (ok (= 4 (length (render:scene-torches scene))))
    (ok (render:scene-voxel-light-propagation-p scene))
    (ok (eq base-field (render:scene-voxel-light scene)))
    ;; Collision and meshing retain one occupied union.  The immutable scene
    ;; owns only its material-source solve; no authored axis-adjacent torch
    ;; source is smuggled into that base generation.
    (ok (luft:chain-site-p solid crystal))
    (ok (luft:chain-site-p solid backing))
    (ok (eq luft.render::*crystal-material-placement*
            (luft.render::scene-material-placement-at scene crystal)))
    (ok (eq luft.render::*sanctuary-material-placement*
            (luft.render::scene-material-placement-at scene backing)))
    (ok (>= (luft:voxel-light-red crystal-light) 3))
    (ok (>= (luft:voxel-light-green crystal-light) 11))
    (ok (= (luft:voxel-light-blue crystal-light) 15))
    (ok (zerop
         (length
          (luft.render::realized-light-stamp-seed-sites
           (luft.render::realized-light-generation-stamp
            (luft.render::scene-authored-light-generation scene))))))
    (multiple-value-bind (mesh generation) (render:make-render-mesh scene)
      (let* ((light-generation
               (render:scene-mesh-generation-light-generation generation))
             (field
               (luft.render::realized-light-generation-field
                light-generation))
             (frames
               (loop for owner in (surface-mesh-tree-meshes mesh)
                     append (luft:surface-mesh-attachments owner))))
        (ok (not (eq field base-field)))
        (ok (plusp (luft:voxel-light-field-visits field)))
        (ok (= 4 (length frames)))
        (loop for attachment across (render:scene-torches scene)
              for support =
                (luft.render::torch-attachment-support-cell attachment)
              for clearance =
                (luft.render::torch-attachment-clearance-cell attachment)
              for base-light = (luft:voxel-light-at-site base-field clearance)
              for realized-light = (luft:voxel-light-at-site field clearance)
              do (ok (luft:chain-site-p solid support))
                 (ok (not (luft:chain-site-p solid clearance)))
                 (ok (<= (luft:voxel-light-red base-light) 3))
                 (ok (= 15 (luft:voxel-light-red realized-light)))
                 (ok (>= (luft:voxel-light-green realized-light) 9))
                 (ok (>= (luft:voxel-light-blue realized-light) 3)))))))

(defun realized-torch-test-frame (mesh face)
  (let* ((surface (luft:resolve-surface-attachment-frame mesh face))
         (origin (luft:surface-attachment-frame-origin surface))
         (normal (luft:surface-attachment-frame-normal surface))
         (tangent (luft:surface-attachment-frame-tangent surface)))
    (render:pack-torch-flame-frame
     (aref origin 0) (aref origin 1) (aref origin 2)
     (luft.render::torch-flame-face-seed face)
     (aref normal 0) (aref normal 1) (aref normal 2) 0
     (aref tangent 0) (aref tangent 1) (aref tangent 2) 1.0)))

(deftest realized-face-torch-frames-are-right-handed
  (let* ((domain (luft:make-world-domain :horizontal-bits 4))
         (cell (luft:make-site domain 7 7 7 luft:+cell-extent+ 1))
         (builder (luft:make-chain-builder domain))
         (solid (progn (luft:chain-builder-add-site builder cell)
                       (luft:finish-chain-builder builder)))
         (mesh (luft:make-surface-mesh solid :bevel-width 2))
         (faces
           (loop for axis in '(:x :y :z)
                 append
                 (list (luft:site-boundary-low domain cell axis)
                       (luft:site-boundary-high domain cell axis))))
         (clock (render:torch-flame-effect-uniform-data 4.25))
         (expected-radiance
           (let ((strength
                   (luft.render::material-kind-surface-emission
                    luft.render::*torch-flame-material*)))
             (mapcar (lambda (channel) (* channel strength))
                     (luft.render::material-kind-base-tone
                      luft.render::*torch-flame-material*)))))
    (ok (= 3 render:+torch-flame-instance-row-count+))
    (ok (= 12 render:+torch-flame-instance-scalar-count+))
    (ok (equalp (coerce (cons 4.25 expected-radiance) 'vector) clock))
    (dolist (face faces)
      (let ((frame (realized-torch-test-frame mesh face)))
        (ok (typep frame '(simple-array single-float (12))))
        (ok (eq frame (render:validate-torch-flame-frame frame)))
        (multiple-value-bind
              (origin-x origin-y origin-z seed
               normal-x normal-y normal-z flags
               tangent-x tangent-y tangent-z scale)
            (render:unpack-torch-flame-frame frame)
          (declare (ignore origin-x origin-y origin-z))
          (multiple-value-bind (expected-x expected-y expected-z)
              (luft:face-oriented-normal face)
            (ok (= expected-x normal-x))
            (ok (= expected-y normal-y))
            (ok (= expected-z normal-z)))
          (ok (<= 0.0 seed))
          (ok (< seed 1.0))
          (ok (= 0.0 flags))
          (ok (= 1.0 scale))
          ;; B=NxT and therefore TxB=N for every polarity, not merely +Z.
          (let* ((tangent-length-squared
                   (+ (* tangent-x tangent-x)
                      (* tangent-y tangent-y)
                      (* tangent-z tangent-z)))
                 (normal-tangent-dot
                   (+ (* normal-x tangent-x)
                      (* normal-y tangent-y)
                      (* normal-z tangent-z)))
                 (bitangent-x
                   (- (* normal-y tangent-z) (* normal-z tangent-y)))
                 (bitangent-y
                   (- (* normal-z tangent-x) (* normal-x tangent-z)))
                 (bitangent-z
                   (- (* normal-x tangent-y) (* normal-y tangent-x)))
                 (handed-x
                   (- (* tangent-y bitangent-z) (* tangent-z bitangent-y)))
                 (handed-y
                   (- (* tangent-z bitangent-x) (* tangent-x bitangent-z)))
                 (handed-z
                   (- (* tangent-x bitangent-y) (* tangent-y bitangent-x))))
            (ok (< (abs (- 1.0 tangent-length-squared)) 1.0e-6))
            (ok (< (abs normal-tangent-dot) 1.0e-6))
            (ok (> (+ (* handed-x normal-x)
                      (* handed-y normal-y)
                      (* handed-z normal-z))
                   0.999))))))
    (let ((top (realized-torch-test-frame
                mesh (luft:site-boundary-high domain cell :z))))
      (let ((density
              (render:torch-flame-reference-density
               top 7.5 7.5 8.62 1.25)))
        (ok (plusp density))
        (ok (= density
               (render:torch-flame-reference-density
                top 7.5 7.5 8.62 1.25))))
      (multiple-value-bind (red green blue alpha)
          (render:torch-flame-reference-integrate-ray
           top 7.5 7.5 7.83 0.0 0.0 1.0 1.25)
        (ok (> red green blue 0.0))
        (ok (< 0.0 alpha 1.0))))))

(deftest oblique-torch-frames-pack-validate-and-drive-the-cpu-flame
  (let* ((root-five (sqrt 5.0f0))
         (normal-x (/ 1.0f0 3.0f0))
         (normal-y (/ 2.0f0 3.0f0))
         (normal-z (/ 2.0f0 3.0f0))
         (tangent-x (/ 2.0f0 root-five))
         (tangent-y (/ -1.0f0 root-five))
         (tangent-z 0.0f0)
         (scale 1.25f0)
         (frame
           (render:pack-torch-flame-frame
            2.25f0 3.5f0 4.75f0 0.375f0
            normal-x normal-y normal-z 13
            tangent-x tangent-y tangent-z scale))
         (copy (copy-seq frame))
         (centre-distance
           (* scale (+ luft.render::+torch-flame-wick-offset+
                       (* 0.3f0 luft.render::+torch-flame-length+))))
         (point-x (+ 2.25f0 (* normal-x centre-distance)))
         (point-y (+ 3.5f0 (* normal-y centre-distance)))
         (point-z (+ 4.75f0 (* normal-z centre-distance))))
    (ok (equalp frame copy))
    (ok (= 12 (length frame)))
    (let ((values (multiple-value-list
                   (render:unpack-torch-flame-frame frame))))
      (ok (= 12 (length values)))
      (ok (= 2.25f0 (first values)))
      (ok (= 0.375f0 (fourth values)))
      (ok (= 13.0f0 (eighth values)))
      (ok (= scale (nth 11 values))))
    (let ((density
            (render:torch-flame-reference-density
             frame point-x point-y point-z 1.25f0)))
      (ok (plusp density))
      (ok (= density
             (render:torch-flame-reference-density
              frame point-x point-y point-z 1.25f0))))
    (let* ((proxy-distance
             (* scale luft.render::+torch-flame-proxy-radius+))
           (ray-origin-distance
             (- (* scale luft.render::+torch-flame-wick-offset+)
                proxy-distance)))
      (multiple-value-bind (red green blue alpha)
          (render:torch-flame-reference-integrate-ray
           frame
           (+ 2.25f0 (* normal-x ray-origin-distance))
           (+ 3.5f0 (* normal-y ray-origin-distance))
           (+ 4.75f0 (* normal-z ray-origin-distance))
           normal-x normal-y normal-z 1.25f0)
        (ok (> red green blue 0.0))
        (ok (< 0.0 alpha 1.0))))
    (ok (signals
         (render:pack-torch-flame-frame
          0 0 0 0.5 1 1 0 0 0 0 1 1)
         'error))
    (ok (signals
         (render:pack-torch-flame-frame
          0 0 0 0.5 1 0 0 0 1 0 0 1)
         'error))
    (ok (signals
         (render:pack-torch-flame-frame
          0 0 0 0.5 1 0 0 0 0 1 0 0)
         'error))))

(deftest opaque-depth-clips-the-reference-flame-integral-not-its-proxy
  (let* ((frame
           (render:pack-torch-flame-frame
            0 0 0 0.25 0 0 1 0 1 0 0 1))
         (arguments (list frame 0.0 0.0 0.12 0.0 0.0 1.0 0.75))
         (full
           (multiple-value-list
            (apply #'render:torch-flame-reference-integrate-ray arguments)))
         (unbounded
           (multiple-value-list
            (apply #'render:torch-flame-reference-integrate-ray
                   (append arguments '(:maximum-path-length 100.0)))))
         (occluded
           (multiple-value-list
            (apply #'render:torch-flame-reference-integrate-ray
                   (append arguments '(:maximum-path-length 0.0))))))
    (ok (some #'plusp full))
    (ok (equal full unbounded)
        "depth behind the proxy reproduces the canonical full chord")
    (ok (equal '(0.0 0.0 0.0 0.0) occluded)
        "opaque depth at the proxy front yields no hidden radiance")))

(deftest canonical-torch-body-is-closed-framed-and-light-bearing
  (let* ((data (render:torch-body-vertex-data))
         (second-copy (render:torch-body-vertex-data))
         (count (render:torch-body-vertex-count))
         (packed-light (luft:pack-voxel-light 3 11 15))
         (flags (render:pack-torch-body-frame-flags 37 packed-light))
         (axis-frame
           (render:pack-torch-flame-frame
            1.0f0 2.0f0 3.0f0 0.25f0
            0.0f0 0.0f0 1.0f0 flags
            1.0f0 0.0f0 0.0f0 2.0f0))
         (root-five (sqrt 5.0f0))
         (normal (vector (/ 1.0f0 3.0f0)
                         (/ 2.0f0 3.0f0)
                         (/ 2.0f0 3.0f0)))
         (tangent (vector (/ 2.0f0 root-five)
                          (/ -1.0f0 root-five)
                          0.0f0))
         (bitangent
           (vector
            (- (* (aref normal 1) (aref tangent 2))
               (* (aref normal 2) (aref tangent 1)))
            (- (* (aref normal 2) (aref tangent 0))
               (* (aref normal 0) (aref tangent 2)))
            (- (* (aref normal 0) (aref tangent 1))
               (* (aref normal 1) (aref tangent 0)))))
         (oblique-frame
           (render:pack-torch-flame-frame
            4.0f0 5.0f0 6.0f0 0.75f0
            (aref normal 0) (aref normal 1) (aref normal 2) flags
            (aref tangent 0) (aref tangent 1) (aref tangent 2) 1.25f0)))
    (labels ((near (left right &optional (tolerance 2.0e-5))
               (< (abs (- left right)) tolerance))
             (lane (vertex lane)
               (aref data (+ (* vertex
                                render:+torch-body-vertex-scalar-count+)
                             lane)))
             (dot3 (x y z vector)
               (+ (* x (aref vector 0))
                  (* y (aref vector 1))
                  (* z (aref vector 2))))
             (point (vertex)
               (list (lane vertex 0) (lane vertex 1) (lane vertex 2)))
             (point< (left right)
               (or (< (first left) (first right))
                   (and (= (first left) (first right))
                        (or (< (second left) (second right))
                            (and (= (second left) (second right))
                                 (< (third left) (third right)))))))
             (edge-key (left right)
               (if (point< right left)
                   (list right left)
                   (list left right))))
      (ok (= 2 render:+torch-body-vertex-row-count+))
      (ok (= 8 render:+torch-body-vertex-scalar-count+))
      ;; Eight-sided bottom/top caps and two eight-sided frusta: 48 triangles.
      (ok (= 144 count))
      (ok (= (* count render:+torch-body-vertex-scalar-count+)
             (length data)))
      (ok (typep data '(simple-array single-float (*))))
      (ok (not (eq data second-copy)))
      (ok (equalp data second-copy))
      (multiple-value-bind (assembly-id decoded-light)
          (render:unpack-torch-body-frame-flags flags)
        (ok (= 37 assembly-id))
        (ok (= packed-light decoded-light)))
      (ok (signals (render:pack-torch-body-frame-flags 4096 0) 'type-error))
      (ok (signals (render:pack-torch-body-frame-flags 0 4096) 'type-error))
      (ok (signals (render:unpack-torch-body-frame-flags 0.5) 'error))
      ;; Every expanded triangle carries unit flat normals agreeing with its
      ;; actual winding.  Barycentrics are structural, but construction ink is
      ;; suppressed on the artificial triangulation diagonals.
      (ok
       (loop for vertex below count
             for normal-length
               = (sqrt (+ (* (lane vertex 4) (lane vertex 4))
                          (* (lane vertex 5) (lane vertex 5))
                          (* (lane vertex 6) (lane vertex 6))))
             always
             (and (= (mod vertex 3) (round (lane vertex 3)))
                  (= 0 (round (lane vertex 7)))
                  (near 1.0 normal-length)
                  (<= 0.0 (lane vertex 2) 0.5))))
      (ok
       (loop for vertex from 0 below count by 3
             for ab-x = (- (lane (+ vertex 1) 0) (lane vertex 0))
             for ab-y = (- (lane (+ vertex 1) 1) (lane vertex 1))
             for ab-z = (- (lane (+ vertex 1) 2) (lane vertex 2))
             for ac-x = (- (lane (+ vertex 2) 0) (lane vertex 0))
             for ac-y = (- (lane (+ vertex 2) 1) (lane vertex 1))
             for ac-z = (- (lane (+ vertex 2) 2) (lane vertex 2))
             for cross-x = (- (* ab-y ac-z) (* ab-z ac-y))
             for cross-y = (- (* ab-z ac-x) (* ab-x ac-z))
             for cross-z = (- (* ab-x ac-y) (* ab-y ac-x))
             for cross-length
               = (sqrt (+ (* cross-x cross-x)
                          (* cross-y cross-y)
                          (* cross-z cross-z)))
             for agreement
               = (/ (+ (* cross-x (lane vertex 4))
                       (* cross-y (lane vertex 5))
                       (* cross-z (lane vertex 6)))
                    cross-length)
             always
             (and (> agreement 0.9999)
                  (near (lane vertex 4) (lane (+ vertex 1) 4))
                  (near (lane vertex 5) (lane (+ vertex 1) 5))
                  (near (lane vertex 6) (lane (+ vertex 1) 6)))))
      ;; Position-identical undirected edges occur exactly twice, including
      ;; both ring seams.  This is the closed-solid oracle, not a screenshot
      ;; inference from a conveniently hidden back side.
      (let ((edges (make-hash-table :test #'equalp)))
        (loop for vertex from 0 below count by 3
              do (dolist (pair '((0 1) (1 2) (2 0)))
                   (incf
                    (gethash
                     (edge-key (point (+ vertex (first pair)))
                               (point (+ vertex (second pair))))
                     edges 0))))
        (ok (= 72 (hash-table-count edges)))
        (ok (loop for incidence being the hash-values of edges
                  always (= 2 incidence))))
      ;; The axis frame is the transparent case: local XYZ becomes world XYZ,
      ;; uniformly scaled about the exact realized surface origin.
      (ok
       (loop for vertex in '(0 1 24 57 96 143)
             always
             (multiple-value-bind
                   (world-x world-y world-z normal-x normal-y normal-z
                    barycentric-index boundary-edge-mask)
                 (render:torch-body-reference-vertex axis-frame vertex)
               (and (near world-x (+ 1.0 (* 2.0 (lane vertex 0))))
                    (near world-y (+ 2.0 (* 2.0 (lane vertex 1))))
                    (near world-z (+ 3.0 (* 2.0 (lane vertex 2))))
                    (near normal-x (lane vertex 4))
                    (near normal-y (lane vertex 5))
                    (near normal-z (lane vertex 6))
                    (= barycentric-index (round (lane vertex 3)))
                    (= boundary-edge-mask (round (lane vertex 7)))))))
      ;; An oblique frame preserves all local coordinates and normals under
      ;; projection onto T, B=NxT, and N.  In particular the bottom cap stays
      ;; on the realized tangent plane instead of snapping to a cubical axis.
      (ok
       (loop for vertex below count
             always
             (multiple-value-bind
                   (world-x world-y world-z normal-x normal-y normal-z)
                 (render:torch-body-reference-vertex oblique-frame vertex)
               (let ((delta-x (- world-x 4.0f0))
                     (delta-y (- world-y 5.0f0))
                     (delta-z (- world-z 6.0f0)))
                 (and
                  (near (lane vertex 0)
                        (/ (dot3 delta-x delta-y delta-z tangent) 1.25f0))
                  (near (lane vertex 1)
                        (/ (dot3 delta-x delta-y delta-z bitangent) 1.25f0))
                  (near (lane vertex 2)
                        (/ (dot3 delta-x delta-y delta-z normal) 1.25f0))
                  (near (lane vertex 4)
                        (dot3 normal-x normal-y normal-z tangent))
                  (near (lane vertex 5)
                        (dot3 normal-x normal-y normal-z bitangent))
                  (near (lane vertex 6)
                        (dot3 normal-x normal-y normal-z normal))
                  (near 1.0
                        (sqrt (+ (* normal-x normal-x)
                                 (* normal-y normal-y)
                                 (* normal-z normal-z))))))))))))

(deftest renderer-torch-frame-resources-are-owned-and-transactional
  (let* ((device (make-instance 'flame-resource-probe-device))
         (camera
           (make-instance 'flame-resource-probe :kind :camera :device device))
         (effect
           (make-instance 'flame-resource-probe :kind :effect :device device))
         (body-vertices
           (make-instance 'flame-resource-probe
                          :kind :body-vertices :device device))
         (materials
           (make-instance 'flame-resource-probe :kind :materials :device device))
         (shadow
           (make-instance 'flame-resource-probe :kind :shadow :device device))
         (sampler
           (make-instance 'flame-resource-probe :kind :sampler :device device))
         (vocabulary luft.render::*surface-assembly-vocabulary*)
         (descriptor-words
           (luft.render::surface-assembly-descriptor-words vocabulary))
         (renderer
           (make-instance
            'luft.render::renderer
            :device device :camera-buffer camera
            :publication
            (luft.render::%make-empty-renderer-publication
             :material-buffer materials :material-vocabulary vocabulary
             :material-vocabulary-revision
             (luv.domains:identity-vocabulary-revision vocabulary)
             :material-descriptor-count
             (length (luv.domains:identity-vocabulary-members vocabulary))
             :material-descriptor-words descriptor-words)
            :flame-layout :flame-layout :flame-effect-buffer effect
            :torch-body-layout :torch-body-layout
            :torch-body-vertex-buffer body-vertices
            :shadow-view shadow :shadow-sampler sampler
            :shadow-layout :shadow-layout))
         (source
           (concatenate
            '(simple-array single-float (*))
            (render:pack-torch-flame-frame
             7 7 8 0.125 0 0 1 0 1 0 0 1)
            (render:pack-torch-flame-frame
             8 7 7 0.625 1 0 0 0 0 1 0 1))))
    (multiple-value-bind (data count buffer body-group shadow-group)
        (luft.render::%make-renderer-flame-resources renderer source)
      (ok (= 2 count))
      (ok (equalp source data))
      (ok (equalp source (flame-resource-probe-data buffer)))
      (setf (aref source 0) 0.0f0)
      (ok (= 7.0f0 (aref data 0)))
      (dolist (resource (list shadow-group body-group buffer))
        (luv:destroy resource)))
    ;; Failure after candidate-buffer creation retires that unpublished
    ;; candidate.  Renderer publication owns this helper's complete result or
    ;; none of it; no authored or partial frame population can leak through.
    (setf (flame-resource-probe-fail-bind-group-p device) t)
    (ok (handler-case
            (progn
              (luft.render::%make-renderer-flame-resources renderer source)
              nil)
          (error () t)))
    (ok (member '(:destroy :buffer)
                (flame-resource-probe-events device) :test #'equal))
    (ok (handler-case
            (progn
              (luft.render::%copy-torch-frame-data #(1 2 3))
              nil)
          (error () t)))))

(deftest authored-placement-frames-compile-to-distinct-dense-assemblies
  (let* ((scene (render:make-mountain-sanctuary-scene))
         (domain (luft:chain-domain (luft.render::scene-solid scene)))
         (x (+ luft.render::+sanctuary-origin-x+
               luft.render::*sanctuary-beacon-x*))
         (y (+ luft.render::+sanctuary-origin-y+
               luft.render::*sanctuary-beacon-y*))
         (z (+ 8 (luft.render::mountain-sanctuary-terrain-height
                  luft.render::*sanctuary-beacon-x*
                  luft.render::*sanctuary-beacon-y*)))
         (cell (luft:make-site domain x y z luft:+cell-extent+ 1)))
    (ok (eq luft.render::*beacon-material-placement*
            (luft.render::scene-material-placement-at scene cell)))
    (render:make-render-mesh scene)
    (let* ((assembly
             (find luft.render::*beacon-material-frame*
                   (luv.domains:identity-vocabulary-members
                    luft.render::*surface-assembly-vocabulary*)
                   :key (lambda (candidate)
                          (luft.render::surface-reading-frame
                           (luft.render::surface-assembly-primary candidate)))))
           (offset (luft.render::surface-assembly-offset assembly))
           (row (* offset
                   luft.render::+surface-assembly-descriptor-row-count+ 4))
           (words (luft.render::surface-assembly-descriptor-words)))
      (ok assembly)
      (ok (equalp #(90.0 78.0 0.0 2.0)
                  (subseq words (+ row 16) (+ row 20))))
      (ok (equalp #(0.70710677 0.70710677 0.0 0.024)
                  (subseq words (+ row 20) (+ row 24)))))))

(deftest surface-assembly-ids-use-the-widened-instance-field
  (let* ((assembly-id #xabc)
         (mesh
           (render:make-whole-domain-diagnostic-mesh
            (render:make-miter-study-scene)
            :stock-function (lambda (face) (declare (ignore face)) assembly-id)
            :chamfer-stock-function
            (lambda (stocks) (declare (ignore stocks)) assembly-id))))
    (dolist (words (list (luft:surface-mesh-face-instance-words mesh)
                         (luft:surface-mesh-band-instance-words mesh)
                         (luft:surface-mesh-fan-instance-words mesh)))
      (ok (plusp (length words)))
      (ok (loop for offset from 3 below (length words) by 4
                always (= assembly-id
                          (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                               (aref words offset))))))
    (ok (handler-case
            (progn (luft.render::make-render-population (list mesh)) nil)
          (error () t)))))

(deftest player-gait-anchors-stance-feet-and-rises-over-support
  (let ((step-length 0.75)
        (leg-length 1.07737)
        (hip-height 1.01))
    (labels ((foot-sample (step-coordinate parity)
               (let* ((cycle (* 0.5 (- step-coordinate parity)))
                      (cycle-index (floor cycle))
                      (phase (- cycle cycle-index))
                      (swing-time
                        (min 1.0 (max 0.0 (* 2.0 (- phase 0.5)))))
                      (swing-weight
                        (* swing-time swing-time swing-time
                           (+ 10.0
                              (* swing-time
                                 (+ -15.0 (* 6.0 swing-time)))))))
                 (values
                  (* step-length
                     (+ parity (* 2.0 cycle-index) 0.5
                        (* 2.0 swing-weight)))
                  (* 0.19 (sin (* pi swing-time))))))
             (pelvis-height (step-coordinate)
               (let* ((phase (- step-coordinate (floor step-coordinate)))
                      (offset (* step-length (- 0.5 phase))))
                 (sqrt (- (* leg-length leg-length) (* offset offset))))))
      ;; Each alternating stance interval holds one foot at one exact world
      ;; coordinate while the root advances by a complete half-step.
      (multiple-value-bind (left-a left-a-lift) (foot-sample 0.1 0.0)
        (multiple-value-bind (left-b left-b-lift) (foot-sample 0.9 0.0)
          (ok (= left-a left-b (* step-length 0.5)))
          (ok (zerop left-a-lift))
          (ok (zerop left-b-lift))))
      (multiple-value-bind (right-a right-a-lift) (foot-sample 1.1 1.0)
        (multiple-value-bind (right-b right-b-lift) (foot-sample 1.9 1.0)
          (ok (= right-a right-b (* step-length 1.5)))
          (ok (zerop right-a-lift))
          (ok (zerop right-b-lift))))
      ;; The other foot clears the deck during transfer and lands at zero
      ;; height; fourteen half-steps span the bridge's 10.5-cell half-route.
      (multiple-value-bind (mid-swing mid-lift) (foot-sample 0.5 1.0)
        (declare (ignore mid-swing))
        (ok (> mid-lift 0.18)))
      (ok (= 10.5 (* 14 step-length)))
      ;; A fixed leg is shortest at double support and tallest over the
      ;; planted foot at mid-stance, giving the body its non-arbitrary bob.
      (ok (= (pelvis-height 0.0) (pelvis-height 1.0)))
      (ok (> (pelvis-height 0.5) (pelvis-height 0.0)))
      ;; The height equation now uses the same hip and ankle centres as the
      ;; rendered SDF.  Its stance-leg reach is constant at the endpoints and
      ;; over the support contact, rather than only looking approximately so.
      (let* ((half-step (* step-length 0.5))
             (contact-height (pelvis-height 0.0))
             (mid-height (pelvis-height 0.5))
             (stance-reach
               (sqrt (+ (* contact-height contact-height)
                        (* half-step half-step)))))
        (ok (< (abs (- contact-height hip-height)) 1e-4))
        (ok (< (abs (- stance-reach leg-length)) 1e-6))
        (ok (< (abs (- mid-height leg-length)) 1e-6))))))

(deftest player-motion-channels-preserve-key-poses-and-foot-rockers
  (labels ((ease (amount)
             (let ((time (min 1.0 (max 0.0 amount))))
               (* time time time
                  (+ 10.0 (* time (+ -15.0 (* 6.0 time)))))))
           (segment (phase beginning end beginning-value end-value)
             (+ beginning-value
                (* (- end-value beginning-value)
                   (ease (/ (- phase beginning) (- end beginning))))))
           (channel (phase contact down passing up next-contact)
             (cond ((< phase 0.16)
                    (segment phase 0.0 0.16 contact down))
                   ((< phase 0.50)
                    (segment phase 0.16 0.50 down passing))
                   ((< phase 0.72)
                    (segment phase 0.50 0.72 passing up))
                   (t
                    (segment phase 0.72 1.0 up next-contact))))
           (rocker (cycle-phase)
             (if (< cycle-phase 0.5)
                 (let ((stance-time (* cycle-phase 2.0)))
                   (cond ((< stance-time 0.18)
                          (segment stance-time 0.0 0.18 0.17 0.0))
                         ((< stance-time 0.72) 0.0)
                         (t (segment stance-time 0.72 1.0 0.0 -0.30))))
                 (let ((swing-time (* (- cycle-phase 0.5) 2.0)))
                   (cond ((< swing-time 0.32)
                          (segment swing-time 0.0 0.32 -0.30 0.10))
                         ((< swing-time 0.78) 0.10)
                         (t (segment swing-time 0.78 1.0 0.10 0.17)))))))
    ;; Authored values survive exactly at semantic pose boundaries.
    (ok (= 0.20 (channel 0.0 0.20 -0.10 0.30 0.40 0.50)))
    (ok (= -0.10 (channel 0.16 0.20 -0.10 0.30 0.40 0.50)))
    (ok (= 0.30 (channel 0.50 0.20 -0.10 0.30 0.40 0.50)))
    (ok (= 0.40 (channel 0.72 0.20 -0.10 0.30 0.40 0.50)))
    (ok (= 0.50 (channel 1.0 0.20 -0.10 0.30 0.40 0.50)))
    ;; The planted boot accepts weight from heel to flat, stays flat through
    ;; the ankle rocker, then rolls over its toe.  Swing dorsiflexion clears
    ;; the ground and returns continuously to the next heel contact.
    (ok (> (rocker 0.0) 0.16))
    (ok (zerop (rocker 0.15)))
    (ok (< (rocker 0.48) -0.25))
    (ok (< (abs (- (rocker 0.49999) (rocker 0.5))) 1e-4))
    (ok (> (rocker 0.70) 0.09))
    (ok (< (abs (- (rocker 0.99999) (rocker 0.0))) 1e-4))))

(defun instance-signature (base-x base-y base-z packed vertices start count)
  (let ((signature
          (make-array (+ 5 (* count luft:+mesh-template-vertex-word-count+))
                      :element-type '(unsigned-byte 32))))
    (setf (aref signature 0) base-x
          (aref signature 1) base-y
          (aref signature 2) base-z
          (aref signature 3) (logand packed #xffff0000)
          (aref signature 4) count)
    (replace signature vertices :start1 5
                                :start2 (* start
                                           luft:+mesh-template-vertex-word-count+)
                                :end2 (* (+ start count)
                                         luft:+mesh-template-vertex-word-count+))
    signature))

(defun word-vector< (left right)
  (loop for a across left
        for b across right
        when (/= a b) return (< a b)
        finally (return (< (length left) (length right)))))

(defun mesh-instance-signatures (mesh)
  (let ((ranges (luft:surface-mesh-template-ranges mesh))
        (vertices (luft:surface-mesh-template-vertex-words mesh))
        (signatures nil))
    (dolist (words (list (luft:surface-mesh-face-instance-words mesh)
                         (luft:surface-mesh-band-instance-words mesh)
                         (luft:surface-mesh-fan-instance-words mesh)))
      (loop for offset from 0 below (length words) by 4
            for packed = (aref words (+ offset 3))
            for template-id = (ldb (byte 16 0) packed)
            for start = (aref ranges (* 2 template-id))
            for count = (aref ranges (1+ (* 2 template-id)))
            do (push (instance-signature
                      (aref words offset) (aref words (+ offset 1))
                      (aref words (+ offset 2)) packed vertices start count)
                     signatures)))
    signatures))

(defun population-instance-signatures (population)
  (let* ((words (luft.render::render-population-instance-words population))
         (vertices (luft.render::render-population-template-words population))
         (triangle-count
           (luft.render::render-population-triangle-instance-count population))
         (signatures nil))
    (loop for offset from 0 below (length words) by 4
          for instance-index from 0
          for packed = (aref words (+ offset 3))
          for template-id = (ldb (byte 16 0) packed)
          for count = (if (< instance-index triangle-count) 3 6)
          for start = (* template-id luft.render::+render-template-vertex-count+)
          do (push (instance-signature
                    (aref words offset) (aref words (+ offset 1))
                    (aref words (+ offset 2)) packed vertices start count)
                   signatures))
    signatures))

(deftest resident-meshes-form-one-exact-two-draw-population
  (let* ((miter (render:make-render-mesh (render:make-miter-study-scene)))
         (spike (render:make-render-mesh (render:make-manifold-spike-scene)))
         (meshes (list miter spike))
         (population (luft.render::make-render-population meshes))
         (source-signatures
           (mapcan #'mesh-instance-signatures meshes))
         (population-signatures
           (population-instance-signatures population))
         (triangle-count
           (luft.render::render-population-triangle-instance-count population))
         (quad-count
           (luft.render::render-population-quad-instance-count population)))
    (ok (equalp (sort source-signatures #'word-vector<)
                (sort population-signatures #'word-vector<)))
    (ok (= (+ triangle-count quad-count)
           (+ (luft:surface-mesh-face-instance-count miter)
              (luft:surface-mesh-band-instance-count miter)
              (luft:surface-mesh-fan-instance-count miter)
              (luft:surface-mesh-face-instance-count spike)
              (luft:surface-mesh-band-instance-count spike)
              (luft:surface-mesh-fan-instance-count spike))))
    (ok (<= (+ (if (plusp triangle-count) 1 0)
               (if (plusp quad-count) 1 0))
            2))))

(deftest canonical-templates-are-shared-between-resident-meshes
  (let* ((mesh (render:make-render-mesh (render:make-miter-study-scene)))
         (single (luft.render::make-render-population (list mesh)))
         (double (luft.render::make-render-population (list mesh mesh)))
         (stride (* luft.render::+render-template-vertex-count+
                    luft:+mesh-template-vertex-word-count+)))
    (ok (= (/ (length (luft.render::render-population-template-words single))
              stride)
           (/ (length (luft.render::render-population-template-words double))
              stride)))
    (ok (= (* 2 (length (luft.render::render-population-instance-words single)))
           (length (luft.render::render-population-instance-words double))))))

(defun mesh-open-edges (mesh)
  (let ((records (luft::%mesh-geometric-edge-records mesh)))
    (loop for edge being the hash-keys of records using (hash-value record)
          when (= 1 (car record)) collect edge)))

(defun make-centred-crystal-bezel-test-scene ()
  "One crystal protruding from the centre of a three-by-three stone support."
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 4)))
    (luft.render::scene-builder-box
     builder 4 6 4 6 4 4 :architecture-p t)
    (luft.render::scene-builder-cell
     builder 5 5 5
     :material luft.render::*crystal-material-placement*)
    (luft.render::finish-scene-builder builder)))

(defun direct-material-bevel-union (scene profile)
  "Build the mixed union directly, without the static render entry point."
  (let* ((chamfer-stock-function
           (luft.render::make-compiled-material-chamfer-stock-function
            (luft.render::scene-material-program scene)))
         (witness
           (luft.render::%make-scene-union-mesh
            scene (render:scene-solid scene) 1 nil chamfer-stock-function)))
    (multiple-value-bind (stock-masks site-widths)
        (luft.render::compile-material-bevel-site-policy profile)
      (luft:vary-surface-mesh-bevel-widths-from-stock-masks
       witness stock-masks site-widths))))

(deftest static-crystal-meshes-are-the-direct-union-at-widths-one-through-four
  (let ((scene (make-centred-crystal-bezel-test-scene)))
    (dolist (width '(1 2 3 4))
      (let* ((profile
               (render:make-material-bevel-profile
                :terrain-width 2 :architecture-width 2
                :crystal-width 4 :contact-width width))
             (material-mesh (render:make-material-bevel-mesh scene profile))
             (material-oracle (direct-material-bevel-union scene profile))
             (uniform-mesh
               (render:make-render-mesh scene :bevel-width width))
             (uniform-oracle
               (luft.render::%make-scene-union-mesh
                scene (render:scene-solid scene) width nil
                (luft.render::make-compiled-material-chamfer-stock-function
                 (luft.render::scene-material-program scene)))))
        (ok (canonical-mesh-cohorts-equal-p
             (list material-oracle)
             (surface-mesh-tree-meshes material-mesh)))
        (ok (canonical-mesh-cohorts-equal-p
             (list uniform-oracle)
             (surface-mesh-tree-meshes uniform-mesh)))
        (ok (luft::%mesh-closed-p material-mesh))
        (ok (luft::%mesh-nondegenerate-p material-mesh))))))

(defun make-gallery-support-crystal-test-scene (position)
  "The reduced gallery plinth with one crystal on an edge or corner."
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 5)))
    (luft.render::scene-builder-box
     builder 14 19 6 9 4 4 :architecture-p t)
    (destructuring-bind (x y z)
        (ecase position
          (:edge '(16 9 5))
          (:corner '(19 9 5)))
      (luft.render::scene-builder-cell
       builder x y z
       :material luft.render::*crystal-material-placement*))
    (luft.render::finish-scene-builder builder)))

(deftest gallery-support-edge-and-corner-crystals-build-as-one-closed-union
  (dolist (position '(:edge :corner))
    (let* ((scene (make-gallery-support-crystal-test-scene position))
           (mesh
             (render:make-material-bevel-mesh
              scene
              (render:make-material-bevel-profile
               :terrain-width 2 :architecture-width 2
               :crystal-width 4 :contact-width 2)))
           (population (luft.render::make-render-population (list mesh))))
      (ok (null (luft:surface-mesh-companions mesh)))
      (ok (luft::%mesh-closed-p mesh))
      (ok (luft::%mesh-nondegenerate-p mesh))
      (ok (plusp
           (luft.render::render-population-opaque-triangle-instance-count
            population)))
      (ok (plusp
           (luft.render::render-population-translucent-triangle-instance-count
            population))))))

(deftest contiguous-crystal-row-has-one-original-perimeter-without-cell-teeth
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene
           (progn
             (luft.render::scene-builder-box
              builder 3 7 3 5 3 3 :architecture-p t)
             (loop for x from 4 to 6
                   do (luft.render::scene-builder-cell
                       builder x 4 4
                       :material luft.render::*crystal-material-placement*))
             (luft.render::finish-scene-builder builder)))
         (mesh
           (render:make-material-bevel-mesh
            scene
            (render:make-material-bevel-profile
             :terrain-width 2 :architecture-width 2
             :crystal-width 4 :contact-width 2)))
         (crystal-stock
           (luft.render::surface-assembly-offset
            luft.render::*crystal-surface*))
         (crystal-exterior
           (luft:select-surface-mesh-stocks
            mesh (lambda (stock) (= stock crystal-stock))))
         (edges (mesh-open-edges crystal-exterior))
         (points
           (sort
            (remove-duplicates (mapcan #'copy-list edges) :test #'equal)
            #'luft::%point-order<)))
    ;; Canonical-site subdivisions remain along the long sides, but all points
    ;; lie on one chamfered outer collar; no host tooth rises between crystals.
    (ok
     (equal
      '((32 34 34) (32 38 34) (34 32 34) (34 40 34)
        (38 32 34) (38 40 34) (40 32 34) (40 40 34)
        (42 32 34) (42 40 34) (46 32 34) (46 40 34)
        (48 32 34) (48 40 34) (50 32 34) (50 40 34)
        (54 32 34) (54 40 34) (56 34 34) (56 38 34))
      points))
    (ok (= 20 (length edges)))
    (ok (luft::%mesh-closed-p mesh))))

(deftest shrine-population-keeps-light-parallel-and-translucency-separate
  (let ((scene (render:make-voxel-light-shrine-scene)))
    (multiple-value-bind (mesh census diagnostics generation)
        (render:make-material-bevel-mesh
         scene (render:make-material-bevel-profile))
      (declare (ignore census diagnostics))
      (let* ((field
               (luft.render::realized-light-generation-field
                (render:scene-mesh-generation-light-generation generation)))
             (meshes (surface-mesh-tree-meshes mesh))
             (population (luft.render::make-render-population (list mesh)))
             (instances
               (+ (luft.render::render-population-triangle-instance-count
                   population)
                  (luft.render::render-population-quad-instance-count
                   population)))
             (frames
               (loop for owner in meshes
                     append (luft:surface-mesh-attachments owner))))
        (ok (luft::%meshes-closed-p meshes))
        (ok (every #'luft::%mesh-nondegenerate-p meshes))
        (ok (not (eq field (render:scene-authored-voxel-light scene))))
        (ok (every (lambda (owner)
                     (eq field (luft:surface-mesh-voxel-light owner)))
                   meshes))
        (ok (= (* 4 instances)
               (length
                (luft.render::render-population-instance-words population))))
        (ok (= (* 2 instances)
               (length
                (luft.render::render-population-light-words population))))
        (ok (plusp
             (luft.render::render-population-opaque-triangle-instance-count
              population)))
        (ok (plusp
             (luft.render::render-population-translucent-triangle-instance-count
              population)))
        ;; Sparse torches are no longer companion surface meshes.  Each resident
        ;; support owner carries one immutable realized frame, and that exact frame
        ;; drives both the canonical opaque body and animated flame pipelines.
        (ok (= (length (render:scene-torches scene)) (length frames)))
        (let ((body-stock (luft.render::surface-assembly-offset
                           luft.render::*torch-body-surface*)))
          (dolist (frame frames)
            (ok (eq frame (render:validate-torch-flame-frame frame)))
            (multiple-value-bind (assembly packed-light)
                (render:unpack-torch-body-frame-flags (aref frame 7))
              (ok (= body-stock assembly))
              (ok (plusp packed-light)))))
        (ok (some #'plusp
                  (coerce
                   (luft.render::render-population-light-words population)
                   'list)))))))

(deftest a-stone-crystal-chunk-seam-is-one-union-before-opacity-classification
  (let* ((scene (make-streaming-material-seam-test-scene))
         (profile
           (render:make-material-bevel-profile
            :terrain-width 4 :architecture-width 1
            :crystal-width 4 :contact-width 2))
         (streaming (render:make-streaming-scene scene))
         (keys (progn
                 (load-all-streaming-chunks streaming 1)
                 (streaming-store-keys streaming)))
         (whole (direct-material-bevel-union scene profile))
         (static (render:make-material-bevel-mesh scene profile)))
    (multiple-value-bind (regional regional-census regional-diagnostics)
        (luft.render::make-scene-regional-meshes scene 1 profile)
      (multiple-value-bind (streamed streamed-census streamed-diagnostics)
          (luft.render::mesh-streaming-snapshot
           (luft.render::make-streaming-region-snapshot
            streaming keys 1 profile))
        (let* ((static-meshes (surface-mesh-tree-meshes static))
               (regional-meshes (mapcar #'cdr regional))
               (streamed-meshes (mapcar #'cdr streamed))
               (population
                 (luft.render::make-render-population streamed-meshes)))
          (flet ((interface-face-p (mesh)
                   (let ((found nil))
                     (luft::%map-mesh-triangles
                      (lambda (kind a b c)
                        (when (and
                               (eq kind :face)
                               (every (lambda (point)
                                        (= (* 64 luft:+mesh-cell-size+)
                                           (first point)))
                                      (list a b c)))
                          (setf found t)))
                      mesh)
                     found)))
            (ok (equal
                 (luft.render::streaming-scene-canonical-owner-closure
                  streaming keys)
                 (mapcar #'car regional)))
            (ok (equal keys (mapcar #'car streamed)))
            (ok (equalp regional-census streamed-census))
            (ok (equal regional-diagnostics streamed-diagnostics))
            (ok (zerop (getf streamed-diagnostics :residual-edge-count)))
            (ok (luft::%mesh-closed-p whole))
            (ok (luft::%meshes-closed-p static-meshes))
            (ok (luft::%meshes-closed-p regional-meshes))
            (ok (luft::%meshes-closed-p streamed-meshes))
            (ok (every #'luft::%mesh-nondegenerate-p streamed-meshes))
            (ok (canonical-mesh-cohorts-equal-p
                 (list whole) static-meshes))
            (ok (canonical-mesh-cohorts-equal-p
                 (list whole) regional-meshes))
            (ok (canonical-mesh-cohorts-equal-p
                 (list whole) streamed-meshes))
            ;; There is no material-phase interface at X=64.  The finished
            ;; union is split into opaque and translucent draw runs only here,
            ;; while compiling its render population.
            (ok (notany #'interface-face-p streamed-meshes))
            (ok (plusp
                 (+ (luft.render::render-population-opaque-triangle-instance-count
                     population)
                    (luft.render::render-population-opaque-quad-instance-count
                     population))))
            (ok (plusp
                 (+ (luft.render::render-population-translucent-triangle-instance-count
                     population)
                    (luft.render::render-population-translucent-quad-instance-count
                     population))))))))))

(deftest an-empty-region-has-no-owners-but-a-requested-slot-is-an-empty-root
  (let* ((scene
           (luft.render::finish-scene-builder
            (luft.render::make-scene-builder :horizontal-bits 7)))
         (streaming (render:make-streaming-scene scene))
         (key (luft:chunk-key-at 64 64))
         (static (render:make-render-mesh scene :bevel-width 2)))
    (setf (gethash key (luft.render::streaming-scene-loaded streaming)) 2)
    (multiple-value-bind (owners census diagnostics)
        (luft.render::make-scene-regional-meshes
         scene 1 (render:make-material-bevel-profile))
      (ok (null owners))
      (ok (equalp #(0 0 0 0 0) census))
      (ok (zerop (getf diagnostics :collapsed-triangle-count)))
      (ok (zerop (getf diagnostics :residual-edge-count))))
    (let ((mesh (render:mesh-streaming-chunk streaming key 2)))
      (ok (zerop (luft:surface-mesh-triangle-count static)))
      (ok (zerop (luft:surface-mesh-triangle-count mesh)))
      (ok (null (luft:surface-mesh-companions mesh)))
      (ok (eq (render:scene-voxel-light scene)
              (luft:surface-mesh-voxel-light mesh))))))

(deftest the-connected-miter-study-uses-the-site-stream-abi
  (dolist (bevel-width '(1 2 3 4))
    (let ((mesh (render:make-render-mesh
                 (render:make-miter-study-scene)
                 :bevel-width bevel-width)))
      (ok (= bevel-width (luft:surface-mesh-bevel-width mesh)))
      (if (= bevel-width 4)
          (progn
            (ok (zerop (luft:surface-mesh-face-triangle-count mesh)))
            (ok (zerop (luft:surface-mesh-band-triangle-count mesh))))
          (progn
            (ok (plusp (luft:surface-mesh-face-triangle-count mesh)))
            (ok (plusp (luft:surface-mesh-band-triangle-count mesh)))))
      (ok (plusp (luft:surface-mesh-fan-triangle-count mesh)))
      (ok (zerop (luft:surface-mesh-singular-star-count mesh)))
      (ok (luft::%mesh-closed-p mesh))
      (let ((lattice (luft.render::mesh-lattice-point-words mesh)))
        (ok (loop for offset from 3 below (length lattice) by 4
                  thereis (zerop (aref lattice offset))))
        (ok (loop for offset from 3 below (length lattice) by 4
                  thereis (= 1 (aref lattice offset))))
        (ok (loop for offset from 3 below (length lattice) by 4
                  thereis (= 2 (aref lattice offset))))
        (ok (loop for offset from 0 below (length lattice) by 4
                  always
                  (or (/= 2 (aref lattice (+ offset 3)))
                      (and
                       (zerop (mod (aref lattice offset)
                                   luft:+mesh-cell-size+))
                       (zerop (mod (aref lattice (+ offset 1))
                                   luft:+mesh-cell-size+))
                       (zerop (mod (aref lattice (+ offset 2))
                                   luft:+mesh-cell-size+))))))))))

(deftest material-bevel-profile-compiles-semantic-widths-once
  (let* ((profile (render:make-material-bevel-profile
                   :terrain-width 4 :architecture-width 1 :contact-width 2))
         (widths (render:compile-material-bevel-profile profile))
         (crystal-stock
           (luft.render::surface-assembly-offset
            luft.render::*crystal-surface*))
         (body-stock
           (luft.render::surface-assembly-offset
            luft.render::*torch-body-surface*)))
    (ok (= 4 (aref widths luft.render::+grass-stock+)))
    (ok (= 4 (aref widths luft.render::+soil-stock+)))
    (ok (= 4 (aref widths luft.render::+turf-edge-stock+)))
    (ok (= 1 (aref widths luft.render::+stone-stock+)))
    (ok (= 1 (aref widths luft.render::+foundation-stone-stock+)))
    (ok (= 2 (aref widths luft.render::+turf-set-stone-stock+)))
    (ok (= 2 (aref widths luft.render::+soil-set-stone-stock+)))
    (ok (= 2 (aref widths luft.render::+deep-set-stone-stock+)))
    (ok (= 4 (aref widths crystal-stock)))
    (ok (zerop (aref widths body-stock)))
    (multiple-value-bind (stock-masks site-widths)
        (luft.render::compile-material-bevel-site-policy profile)
      (ok (= luft.render::+material-bevel-terrain-mask+
             (aref stock-masks luft.render::+grass-stock+)))
      (ok (= luft.render::+material-bevel-architecture-mask+
             (aref stock-masks luft.render::+stone-stock+)))
      (ok (= luft.render::+material-bevel-crystal-mask+
             (aref stock-masks crystal-stock)))
      (ok (= luft.render::+material-bevel-non-meshed-mask+
             (aref stock-masks body-stock)))
      (ok (= (logior luft.render::+material-bevel-terrain-mask+
                     luft.render::+material-bevel-architecture-mask+)
             (aref stock-masks luft.render::+turf-set-stone-stock+)))
      (ok (= 4 (aref site-widths
                     luft.render::+material-bevel-terrain-mask+)))
      (ok (= 1 (aref site-widths
                     luft.render::+material-bevel-architecture-mask+)))
      (ok (= 4 (aref site-widths
                     luft.render::+material-bevel-crystal-mask+)))
      (ok (= 2 (aref site-widths
                     (logior luft.render::+material-bevel-terrain-mask+
                             luft.render::+material-bevel-architecture-mask+))))
      (ok (= 2 (aref site-widths
                     luft.render::+material-bevel-terrain-crystal-mask+)))
      (ok (= 2 (aref site-widths
                     luft.render::+material-bevel-architecture-crystal-mask+)))
      (ok (= 2 (aref site-widths
                     luft.render::+material-bevel-three-way-mask+)))
      ;; A positive non-meshed sentinel keeps the packed byte policy eligible,
      ;; but necessarily falls outside the eight-entry topology width table if
      ;; an attachment stock ever leaks into a surface witness.
      (let* ((scene (render:make-material-bevel-transition-study-scene))
             (witness (render:make-render-mesh scene :bevel-width 1))
             (owners (surface-mesh-tree-meshes witness))
             (topology-stocks
               (loop for owner in owners append
                 (loop for words in
                       (list (luft:surface-mesh-face-instance-words owner)
                             (luft:surface-mesh-band-instance-words owner)
                             (luft:surface-mesh-fan-instance-words owner))
                       append
                       (loop for offset from 3 below (length words) by 4
                             collect
                             (ldb (byte luft:+mesh-instance-stock-bit-count+
                                        16)
                                  (aref words offset)))))))
        (ok (luft::%paged-byte-stock-mask-policy-p
             (luft:surface-mesh-domain witness) stock-masks site-widths))
        (ok (plusp (length topology-stocks)))
        (ok (every (lambda (stock)
                     (<= 1 (aref stock-masks stock) 7))
                   topology-stocks))
        (let ((invalid-witness
                (render:make-whole-domain-diagnostic-mesh
                 scene :bevel-width 1
                 :stock-function (lambda (face)
                                   (declare (ignore face))
                                   body-stock)
                 :chamfer-stock-function
                 (lambda (stocks)
                   (declare (ignore stocks))
                   body-stock))))
          (ok (signals
               (luft:vary-surface-mesh-bevel-widths-from-stock-masks
                invalid-witness stock-masks site-widths)
               'error))))
      ;; Width three is a first-class legal profile value, not an accidental
      ;; escape from a declaration inferred from the old 1/2/4 defaults.
      (let* ((all-three
               (render:make-material-bevel-profile
                :terrain-width 3 :architecture-width 3 :crystal-width 3
                :contact-width 3))
             (three-widths
               (render:compile-material-bevel-profile all-three)))
        (ok
         (loop for stock below (length three-widths)
               for mask = (aref stock-masks stock)
               always (= (aref three-widths stock)
                         (if (= mask
                                luft.render::+material-bevel-non-meshed-mask+)
                             0
                             3))))))))

(deftest crystal-contact-widths-do-not-move-terrain-architecture-sites
  (let ((baseline
          (render:make-material-bevel-profile
           :contact-width 2
           :terrain-architecture-width 3
           :terrain-crystal-width 1
           :architecture-crystal-width 2
           :three-way-width 4))
        (changed-crystal-contacts
          (render:make-material-bevel-profile
           :contact-width 2
           :terrain-architecture-width 3
           :terrain-crystal-width 4
           :architecture-crystal-width 1
           :three-way-width 2)))
    (multiple-value-bind (baseline-masks baseline-widths)
        (luft.render::compile-material-bevel-site-policy baseline)
      (declare (ignore baseline-masks))
      (multiple-value-bind (changed-masks changed-widths)
          (luft.render::compile-material-bevel-site-policy
           changed-crystal-contacts)
        (declare (ignore changed-masks))
        (ok (= 3 (aref baseline-widths
                       luft.render::+material-bevel-terrain-architecture-mask+)))
        (ok (= (aref baseline-widths
                     luft.render::+material-bevel-terrain-architecture-mask+)
               (aref changed-widths
                     luft.render::+material-bevel-terrain-architecture-mask+)))
        (ok (/= (aref baseline-widths
                      luft.render::+material-bevel-terrain-crystal-mask+)
                (aref changed-widths
                      luft.render::+material-bevel-terrain-crystal-mask+)))
        (ok (/= (aref baseline-widths
                      luft.render::+material-bevel-architecture-crystal-mask+)
                (aref changed-widths
                      luft.render::+material-bevel-architecture-crystal-mask+)))
        (ok (/= (aref baseline-widths
                      luft.render::+material-bevel-three-way-mask+)
                (aref changed-widths
                      luft.render::+material-bevel-three-way-mask+)))))))

(deftest material-bevel-policy-builds-one-closed-site-local-surface
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene
           (progn
             (luft.render::scene-builder-box builder 4 9 4 9 2 2)
             (luft.render::scene-builder-box
              builder 5 6 5 6 3 5 :architecture-p t)
             (luft.render::finish-scene-builder builder)))
         (profile (render:make-material-bevel-profile))
         (meshes (render:make-material-bevel-meshes scene profile)))
    (multiple-value-bind (mesh width-census)
        (render:make-material-bevel-mesh scene profile)
      (ok (= 4 (luft:surface-mesh-bevel-width mesh)))
      (ok (luft::%mesh-closed-p mesh))
      (ok (luft::%mesh-nondegenerate-p mesh))
      (ok (plusp (aref width-census 1)))
      (ok (plusp (aref width-census 2)))
      (ok (zerop (aref width-census 3)))
      (ok (plusp (aref width-census 4)))
      (ok (= 1 (length meshes)))
      (ok (= 0 (caar meshes)))
      (ok (luft::%mesh-closed-p (cdar meshes))))))

(defun mesh-triangle-quality (mesh)
  "Return minimum angle, maximum longest-edge/altitude ratio, and sliver count."
  (let ((minimum-angle 180.0d0)
        (maximum-aspect 0.0d0)
        (aspect-over-five 0))
    (labels ((distance-squared (left right)
               (loop for l in left
                     for r in right
                     sum (expt (- l r) 2)))
             (angle (left-squared right-squared opposite-squared)
               (* (/ 180.0d0 pi)
                  (acos
                   (max -1.0d0
                        (min 1.0d0
                             (/ (- (+ left-squared right-squared)
                                   opposite-squared)
                                (* 2.0d0
                                   (sqrt (* left-squared
                                            right-squared))))))))))
      (luft::%map-mesh-triangles
       (lambda (kind a b c)
         (declare (ignore kind))
         (let* ((ab2 (distance-squared a b))
                (bc2 (distance-squared b c))
                (ca2 (distance-squared c a))
                (cross
                  (luft::%cross (luft::%point- b a)
                                (luft::%point- c a)))
                (cross2
                  (loop for component in cross
                        sum (* component component)))
                (aspect
                  (/ (float (max ab2 bc2 ca2) 1.0d0)
                     (sqrt cross2))))
           (setf minimum-angle
                 (min minimum-angle
                      (angle ab2 ca2 bc2)
                      (angle ab2 bc2 ca2)
                      (angle bc2 ca2 ab2))
                 maximum-aspect (max maximum-aspect aspect))
           (when (> aspect 5.0d0)
             (incf aspect-over-five))))
       mesh))
    (values minimum-angle maximum-aspect aspect-over-five)))

(deftest material-bevel-transition-contracts-the-medial-t-junction
  (let* ((scene (render:make-material-bevel-transition-study-scene))
         (width-one (render:make-render-mesh scene :bevel-width 1)))
    (multiple-value-bind (mesh width-census diagnostics)
        (render:make-material-bevel-mesh
         scene (render:make-material-bevel-profile))
      (ok (plusp (aref width-census 1)))
      (ok (plusp (aref width-census 2)))
      (ok (plusp (aref width-census 4)))
      (ok (equalp #(0 11 5 0 7) width-census))
      (ok (= 31 (getf diagnostics :collapsed-triangle-count)))
      (ok (= 3 (getf diagnostics :unmatched-edge-count)))
      (ok (= 1 (getf diagnostics :repaired-edge-count)))
      (ok (zerop (getf diagnostics :residual-edge-count)))
      (ok (equal '(((48 34 26) (48 36 28) (48 38 30)))
                 (getf diagnostics :candidate-splits)))
      (ok (= 190 (luft:surface-mesh-triangle-count mesh)))
      (ok (luft::%mesh-closed-p mesh))
      (ok (luft::%mesh-nondegenerate-p mesh))
      ;; Contracting the medial T-junction may subdivide a neighbour, but it
      ;; must not make triangle quality worse than the width-one topology
      ;; witness from which the mixed surface was evaluated.
      (multiple-value-bind (minimum-angle maximum-aspect sliver-count)
          (mesh-triangle-quality mesh)
        (multiple-value-bind
              (witness-minimum-angle witness-maximum-aspect
               witness-sliver-count)
            (mesh-triangle-quality width-one)
          (ok (>= minimum-angle (- witness-minimum-angle 1.0d-9)))
          (ok (<= maximum-aspect (+ witness-maximum-aspect 1.0d-9)))
          (ok (<= sliver-count witness-sliver-count)))))))

(deftest compiled-material-site-field-matches-its-generic-repair-oracle
  (let* ((scene (render:make-material-bevel-transition-study-scene))
         ;; Compile the material vocabulary only after witness construction;
         ;; chamfer assembly can intern stocks while building that witness.
         (witness (render:make-render-mesh scene :bevel-width 1))
         (profile (render:make-material-bevel-profile)))
    (multiple-value-bind (stock-masks site-widths)
        (luft.render::compile-material-bevel-site-policy profile)
      (ok (luft::%paged-byte-stock-mask-policy-p
           (luft:surface-mesh-domain witness) stock-masks site-widths))
      (flet ((generic-width (x y z stocks)
               (declare (ignore x y z))
               (let ((site-mask 0))
                 (dolist (stock stocks)
                   (setf site-mask
                         (logior site-mask (aref stock-masks stock))))
                 (aref site-widths site-mask))))
        (dolist (contract-p '(nil t))
          (multiple-value-bind
                (generic generic-census generic-diagnostics)
              (funcall
               (if contract-p
                   #'luft:vary-surface-mesh-bevel-widths
                   #'luft:vary-uncontracted-surface-mesh-bevel-widths-diagnostic)
               witness #'generic-width)
            (multiple-value-bind
                  (compiled compiled-census compiled-diagnostics)
                (funcall
                 (if contract-p
                     #'luft:vary-surface-mesh-bevel-widths-from-stock-masks
                     #'luft:vary-uncontracted-surface-mesh-bevel-widths-from-stock-masks-diagnostic)
                 witness stock-masks site-widths)
              (ok (luft::%same-surface-mesh-representation-p
                   generic compiled))
              (ok (equalp generic-census compiled-census))
              (ok (equal generic-diagnostics compiled-diagnostics)))))))))

(deftest material-bevel-transition-can-exhibit-the-uncontracted-t-junction
  (multiple-value-bind (mesh width-census diagnostics)
      (render:make-uncontracted-material-bevel-diagnostic-mesh
       (render:make-material-bevel-transition-study-scene)
       (render:make-material-bevel-profile))
    (declare (ignore width-census))
    (ok (= 3 (getf diagnostics :unmatched-edge-count)))
    (ok (zerop (getf diagnostics :repaired-edge-count)))
    (ok (= 3 (getf diagnostics :residual-edge-count)))
    (ok (not (luft::%mesh-closed-p mesh)))
    ;; The diagnostic mesh omits zero-area triangles.  Its defect is solely
    ;; the long-edge/short-edge connectivity mismatch exposed by construction
    ;; ink, not a retained degenerate primitive.
    (ok (luft::%mesh-nondegenerate-p mesh))))

(deftest material-bevel-transition-isolates-the-exact-split-neighborhood
  (let ((scene (render:make-material-bevel-transition-study-scene))
        (profile (render:make-material-bevel-profile)))
    (flet ((neighborhood (contract-p)
             (multiple-value-bind (mesh width-census diagnostics)
                 (if contract-p
                     (render:make-material-bevel-mesh scene profile)
                     (render:make-uncontracted-material-bevel-diagnostic-mesh
                      scene profile))
               (declare (ignore width-census))
               (luft:surface-mesh-split-neighborhood
                mesh (first (getf diagnostics :candidate-splits))))))
      (let ((uncontracted (neighborhood nil))
            (contracted (neighborhood t)))
        (ok (= 3 (luft:surface-mesh-triangle-count uncontracted)))
        (ok (= 4 (luft:surface-mesh-triangle-count contracted)))
        (ok (luft::%mesh-nondegenerate-p uncontracted))
        (ok (luft::%mesh-nondegenerate-p contracted))
        (let ((inked (luft:surface-mesh-with-triangle-ink contracted)))
          (ok (= (luft:surface-mesh-triangle-count contracted)
                 (luft:surface-mesh-triangle-count inked)))
          (ok (luft::%same-plane-areas-p
               (luft::%mesh-oriented-plane-areas contracted)
               (luft::%mesh-oriented-plane-areas inked))))))))

(defun check-authored-stair-boundary (boundary)
  (multiple-value-bind (mesh width-census diagnostics)
      (render:make-material-bevel-mesh
       (render:make-mountain-sanctuary-scene :stair-boundary boundary)
       (render:make-material-bevel-profile))
    (declare (ignore diagnostics))
    (ok (plusp (aref width-census 1)))
    (ok (plusp (aref width-census 2)))
    (ok (plusp (aref width-census 4)))
    (let ((meshes (surface-mesh-tree-meshes mesh)))
      (ok (luft::%meshes-closed-p meshes))
      (ok (every #'luft::%mesh-nondegenerate-p meshes)))))

(deftest open-stair-remains-an-ordinary-closed-material-surface
  (check-authored-stair-boundary :open))

(deftest bordered-stair-remains-an-ordinary-closed-material-surface
  (check-authored-stair-boundary :border))

(deftest low-wall-stair-remains-an-ordinary-closed-material-surface
  (check-authored-stair-boundary :low-wall))

(deftest terrain-chamfers-distinguish-the-living-top-edge
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene (progn
                  (luft.render::scene-builder-cell builder 4 4 4)
                  (luft.render::finish-scene-builder builder)))
         (mesh (render:make-render-mesh scene)))
    (flet ((instance-stocks (words)
             (loop for offset from 3 below (length words) by 4
                   collect (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                                (aref words offset)))))
      (let ((stocks
              (mapcan #'instance-stocks
                      (list (luft:surface-mesh-band-instance-words mesh)
                            (luft:surface-mesh-fan-instance-words mesh)))))
        (ok (plusp (length stocks)))
        (ok (member luft.render::+turf-edge-stock+ stocks))
        (ok (member luft.render::+soil-stock+ stocks))))))

(deftest flat-terrain-closures-retain-a-living-edge-reading
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene (progn
                  (luft.render::scene-builder-box builder 4 5 4 5 4 4)
                  (luft.render::finish-scene-builder builder)))
         (mesh (render:make-render-mesh scene)))
    (flet ((contains-turf-edge-p (words)
             (loop for offset from 3 below (length words) by 4
                   thereis (= luft.render::+turf-edge-stock+
                              (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                                   (aref words offset))))))
      (ok (contains-turf-edge-p
           (luft:surface-mesh-band-instance-words mesh)))
      (ok (contains-turf-edge-p
           (luft:surface-mesh-fan-instance-words mesh))))))

(deftest miter-study-chamfers-do-not-use-the-terrain-top-stock
  (let ((mesh (render:make-render-mesh (render:make-miter-study-scene))))
    (dolist (words (list (luft:surface-mesh-band-instance-words mesh)
                         (luft:surface-mesh-fan-instance-words mesh)))
      (ok (notany (lambda (stock) (zerop stock))
                  (loop for offset from 3 below (length words) by 4
                        collect (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                                     (aref words offset))))))))

(deftest stone-terrain-chamfers-have-an-earth-set-reading
  (ok (= luft.render::+stone-stock+
         (luft.render::scene-chamfer-stock
          (list luft.render::+stone-stock+))))
  (ok (= luft.render::+turf-set-stone-stock+
         (luft.render::scene-chamfer-stock
          (list luft.render::+stone-stock+ luft.render::+grass-stock+))))
  (ok (= luft.render::+soil-set-stone-stock+
         (luft.render::scene-chamfer-stock
          (list luft.render::+soil-stock+ luft.render::+stone-stock+))))
  (let* ((stock
           (luft.render::scene-chamfer-stock
            (list luft.render::+stone-stock+ luft.render::+grass-stock+
                  luft.render::+subsoil-stock+)))
         (assembly (luft.render::surface-assembly-at stock)))
    ;; The three-reading closure is visually deep-set but owns a distinct
    ;; provenance stock from the two-reading built-in deep-set assembly.
    (ok (/= luft.render::+deep-set-stone-stock+ stock))
    (ok (eq :earth-set-stone
            (luft.render::surface-assembly-kernel assembly)))
    (ok (eq :underside
            (luft.render::surface-reading-role
             (luft.render::surface-assembly-secondary assembly))))
    (ok (= 3
           (length
            (luft.render::surface-closure-summary-readings
             (luft.render::surface-assembly-closure-summary assembly))))))
  (ok (= luft.render::+turf-edge-stock+
         (luft.render::scene-chamfer-stock
          (list luft.render::+grass-stock+ luft.render::+soil-stock+)))))

(deftest earth-set-readings-are-confined-to-stone-terrain-chamfers
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene (progn
                  (luft.render::scene-builder-box builder 4 6 4 6 2 2)
                  (luft.render::scene-builder-cell
                   builder 5 5 3 :architecture-p t)
                  (luft.render::finish-scene-builder builder)))
         (mesh (render:make-render-mesh scene)))
    (flet ((stocks (words)
             (loop for offset from 3 below (length words) by 4
                   collect (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                                (aref words offset))))
           (earth-set-p (stock)
             (eq :earth-set-stone
                 (luft.render::surface-assembly-kernel
                  (luft.render::surface-assembly-at stock)))))
      (ok (notany #'earth-set-p
                  (stocks (luft:surface-mesh-face-instance-words mesh))))
      (ok (some #'earth-set-p
                (append
                 (stocks (luft:surface-mesh-band-instance-words mesh))
                 (stocks (luft:surface-mesh-fan-instance-words mesh))))))))

(deftest terrain-borne-architecture-marks-only-its-lowest-face-course
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene (progn
                  (luft.render::scene-builder-box builder 4 6 4 6 2 2)
                  (luft.render::scene-builder-box
                   builder 5 5 5 5 3 4 :architecture-p t)
                  (luft.render::finish-scene-builder builder)))
         (mesh (render:make-render-mesh scene))
         (face-stocks
           (loop with words = (luft:surface-mesh-face-instance-words mesh)
                 for offset from 3 below (length words) by 4
                 collect (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                              (aref words offset)))))
    (ok (member luft.render::+foundation-stone-stock+ face-stocks))
    (ok (member luft.render::+stone-stock+ face-stocks))
    (ok (every
         (lambda (stock)
           (eq :face
               (luft.render::surface-assembly-relation
                (luft.render::surface-assembly-at stock))))
         face-stocks))))

(deftest directional-star-ambient-occlusion-measures-the-outward-hemisphere
  (ok (= 0 (luft::%directional-star-ambient-occlusion #b00000000 '(0 0 1))))
  (ok (= 1 (luft::%directional-star-ambient-occlusion #b00010000 '(0 0 1))))
  (ok (= 3 (luft::%directional-star-ambient-occlusion #b11110000 '(0 0 1))))
  (ok (= 3 (luft::%directional-star-ambient-occlusion #b10001000 '(1 1 0)))))

(deftest topology-ao-is-confined-to-bevels-and-junctions
  (let ((mesh (render:make-render-mesh (render:make-miter-study-scene))))
    (flet ((levels (words)
             (loop for offset from 3 below (length words) by 4
                   collect (ldb (byte 2 28) (aref words offset)))))
      (ok (every #'zerop (levels (luft:surface-mesh-face-instance-words mesh))))
      (ok (some #'plusp
                (append (levels (luft:surface-mesh-band-instance-words mesh))
                        (levels (luft:surface-mesh-fan-instance-words mesh))))))))

(deftest mesh-and-presentation-shaders-lower-through-both-conventional-backends
  (let* ((vertex (luft.render.shaders:mesh-vertex-specification))
         (fragment (luft.render.shaders:mesh-fragment-specification))
         (shadow-vertex
           (luft.render.shaders:shadow-vertex-specification))
         (lattice-vertex
           (luft.render.shaders:lattice-point-vertex-specification))
         (lattice-fragment
           (luft.render.shaders:lattice-point-fragment-specification))
         (player-vertex
           (luft.render.shaders:player-sdf-vertex-specification))
         (player-fragment
           (luft.render.shaders:player-sdf-fragment-specification))
         (flame-vertex
           (luft.render.shaders:torch-flame-vertex-specification))
         (flame-fragment
           (luft.render.shaders:torch-flame-fragment-specification))
         (flame-composite-copy
           (luft.render.shaders::torch-flame-composite-copy-fragment-specification))
         (torch-body-vertex
           (luft.render.shaders:torch-body-vertex-specification))
         (torch-body-shadow-vertex
           (luft.render.shaders:torch-body-shadow-vertex-specification))
         (present-vertex
           (luft.render.shaders:present-vertex-specification))
         (present-fragment
           (luft.render.shaders:present-fragment-specification))
         (sky-fragment
           (luft.render.shaders:sky-fragment-specification))
         (sky-temporal-fragment
           (luft.render.shaders:sky-temporal-fragment-specification))
         (temporal-resolve-fragment
           (luft.render.shaders:temporal-resolve-fragment-specification))
         (exposure-probe-fragment
           (luft.render.shaders:exposure-probe-fragment-specification))
         (vertex-msl
           (luv.msl:msl-document-source (luv.msl:compile-msl vertex)))
         (fragment-msl
           (luv.msl:msl-document-source (luv.msl:compile-msl fragment)))
         (flame-vertex-msl
           (luv.msl:msl-document-source (luv.msl:compile-msl flame-vertex)))
         (flame-fragment-msl
           (luv.msl:msl-document-source (luv.msl:compile-msl flame-fragment)))
         (flame-composite-copy-msl
           (luv.msl:msl-document-source
            (luv.msl:compile-msl flame-composite-copy)))
         (torch-body-vertex-msl
           (luv.msl:msl-document-source
            (luv.msl:compile-msl torch-body-vertex)))
         (torch-body-shadow-vertex-msl
           (luv.msl:msl-document-source
            (luv.msl:compile-msl torch-body-shadow-vertex)))
         (present-fragment-msl
           (luv.msl:msl-document-source
            (luv.msl:compile-msl present-fragment))))
    (ok (search "[[vertex_id]]" vertex-msl))
    (ok (search "[[instance_id]]" vertex-msl))
    (ok (search "const device uint4* instances" vertex-msl))
    (ok (search "const device uint4* template_vertices" vertex-msl))
    (ok (search "primitive_kind" vertex-msl))
    (ok (search "const device float4* material_descriptors" fragment-msl))
    (ok (search "assembly_id * 8.0f" fragment-msl))
    (ok (search "descriptor_row + uint(3.0f)" fragment-msl))
    (ok (search "metalness" fragment-msl))
    (ok (search "if (metalness > 0.0f)" fragment-msl))
    (ok (search "float3 f0 = mix(float3(0.04f" fragment-msl))
    (ok (search "clamp(base, float3(0.0f" fragment-msl))
    (ok (search "1.0f - metalness" fragment-msl))
    (ok (search "metal_environment_reflection" fragment-msl))
    (ok (search "forged_metal" fragment-msl))
    (ok (not (search "kernel_code - 9.0f" fragment-msl)))
    (ok (search "depth2d<float> shadow_map" fragment-msl))
    (ok (search "sampler shadow_sampler" fragment-msl))
    (ok (search "barycentric" fragment-msl))
    (ok (search "primitive_kind" fragment-msl))
    (ok (search "primitive_feature_pixels" fragment-msl))
    (ok (search "motion_output" fragment-msl))
    (ok (search "gemstone_radiance" fragment-msl))
    (ok (search "[[instance_id]]" flame-vertex-msl))
    (ok (search "const device float4* flame_instances" flame-vertex-msl))
    (ok (search "instance_index * uint(3.0f)" flame-vertex-msl))
    (ok (search "world_up_projection" flame-fragment-msl))
    (ok (search "bitangent" flame-fragment-msl))
    (ok (not (search "orientation" flame-vertex-msl)))
    (ok (not (search "previous_clip" flame-vertex-msl)))
    (ok (not (search "motion_output" flame-fragment-msl)))
    (ok (search "depth2d<float> opaque_depth" flame-fragment-msl))
    (ok (search "sampler depth_sampler" flame-fragment-msl))
    (ok (search "scene_depth" flame-fragment-msl))
    (ok (search "opaque_view_depth" flame-fragment-msl))
    (ok (search "proxy_view_depth" flame-fragment-msl))
    (ok (search "ray_view_rate" flame-fragment-msl))
    (ok (search "flame_effect_parameters" flame-fragment-msl))
    (ok (search "texture2d<float> scene" flame-composite-copy-msl))
    (ok (search "const device float4* torch_frames" torch-body-vertex-msl))
    (ok (search "const device float4* torch_body_vertices"
                torch-body-vertex-msl))
    (ok (search "vertex_index * uint(2.0f)" torch-body-vertex-msl))
    (ok (search "frame_flags" torch-body-vertex-msl))
    (ok (search "packed_light" torch-body-vertex-msl))
    (ok (search "torch_frame_world_position" torch-body-vertex-msl))
    (ok (search "torch_frame_world_normal" torch-body-vertex-msl))
    (ok (search "const device float4* torch_frames"
                torch-body-shadow-vertex-msl))
    (ok (search "torch_frame_world_position" torch-body-shadow-vertex-msl))
    ;; The expensive dielectric response must remain structured control flow,
    ;; not an eager select paid by every ordinary terrain fragment.
    (ok (search "if (abs((kernel_code - 8.0f)) < 0.5f)" fragment-msl))
    (ok (search "depth2d<float> scene_depth" present-fragment-msl))
    (ok (search "highlight_energy" present-fragment-msl))
    (ok (search "paper_grade" present-fragment-msl))
    (ok (search "[[instance_id]]"
                (luv.msl:msl-document-source
                 (luv.msl:compile-msl lattice-vertex))))
    (ok (luv.msl:compile-msl lattice-fragment))
    (ok (luv.spir-v:compile-shader-specification vertex))
    (ok (luv.spir-v:compile-shader-specification fragment))
    (ok (luv.msl:compile-msl shadow-vertex))
    (ok (luv.spir-v:compile-shader-specification shadow-vertex))
    (ok (luv.spir-v:compile-shader-specification lattice-vertex))
    (ok (luv.spir-v:compile-shader-specification lattice-fragment))
    (ok (luv.msl:compile-msl player-vertex))
    (ok (luv.msl:compile-msl player-fragment))
    (ok (luv.spir-v:compile-shader-specification player-vertex))
    (ok (luv.spir-v:compile-shader-specification player-fragment))
    (ok (luv.spir-v:compile-shader-specification flame-vertex))
    (ok (luv.spir-v:compile-shader-specification flame-fragment))
    (ok (luv.spir-v:compile-shader-specification flame-composite-copy))
    (ok (luv.spir-v:compile-shader-specification torch-body-vertex))
    (ok (luv.spir-v:compile-shader-specification torch-body-shadow-vertex))
    (ok (luv.msl:compile-msl sky-fragment))
    (ok (luv.msl:compile-msl sky-temporal-fragment))
    (ok (luv.msl:compile-msl temporal-resolve-fragment))
    (ok (luv.msl:compile-msl exposure-probe-fragment))
    (ok (luv.spir-v:compile-shader-specification sky-fragment))
    (ok (luv.spir-v:compile-shader-specification sky-temporal-fragment))
    (ok (luv.spir-v:compile-shader-specification
         temporal-resolve-fragment))
    (ok (luv.spir-v:compile-shader-specification exposure-probe-fragment))
    (ok (luv.spir-v:compile-shader-specification present-vertex))
    (ok (luv.spir-v:compile-shader-specification present-fragment))))

(deftest exposure-probes-decode-and-adapt-with-moppes-asymmetric-rates
  (let* ((luminance 0.16d0)
         (encoded
           (round
            (* 255d0
               (/ (+ (log luminance) 9.21034d0) 11.98293d0))))
         (bytes
           (make-array render::+exposure-probe-byte-count+
                       :element-type '(unsigned-byte 8)
                       :initial-element 0)))
    (loop for index from 0 below (length bytes) by 4
          do (setf (aref bytes index) encoded))
    (ok (< (abs (- luminance
                   (render::exposure-probe-average-luminance bytes)))
           0.01d0))
    ;; Looking into more light closes down quickly; opening into darkness is
    ;; deliberately slower, matching Moppe's eye-adaptation architecture.
    (ok (< (abs (- 0.955f0
                   (render::adapted-exposure 1.0f0 0.32f0)))
           1.0e-6))
    (ok (< (abs (- 1.036f0
                   (render::adapted-exposure 1.0f0 0.08f0)))
           1.0e-6))
    (ok (< (render::adapted-exposure 1.9f0 1000.0f0) 1.9f0))
    (ok (> (render::adapted-exposure 0.55f0 0.00001f0) 0.55f0))))

(deftest the-camera-block-packs-both-projections
  (let ((camera (render:make-fly-camera))
        (player (render:make-walking-player)))
    (flet ((lane (projection)
             (let ((render:*projection* projection))
               (let ((view
                       (luft.render::capture-frame-view
                        camera 1100 800 #(0.0 0.0))))
                 (luft.render::camera-uniform-data
                  view view #(0.5 0.5 0.001 0.001) 1.0 player)))))
      (let ((perspective (lane :perspective))
            (isometric (lane :isometric))
            (eighth
              (let ((render:*projection* :isometric))
                (let ((view
                        (luft.render::capture-frame-view
                         camera 1100 800 #(0.0 0.0))))
                  (luft.render::camera-uniform-data
                   view view #(0.5 0.5 0.001 0.001) 1.0 player 1)))))
        (ok (= 108 (length perspective)))
        (ok (typep perspective '(simple-array single-float (108))))
        (ok (= 1.0 (aref perspective 22)))
        (ok (= 0.0 (aref isometric 22)))
        (flet ((depth (data view-z)
                 (let ((clip (+ (* view-z (aref data 18)) (aref data 19))))
                   (if (zerop (aref data 22)) clip (/ clip view-z)))))
          (ok (< (abs (depth perspective 0.1)) 1d-4))
          (ok (< (abs (- (depth perspective 600.0) 1.0)) 1d-4))
          (ok (< (abs (depth isometric
                            luft.render::+orthographic-near+)) 1d-4))
          (ok (< (abs (- (depth isometric
                               luft.render::+orthographic-far+) 1.0))
                 1d-4)))
        (ok (= (aref perspective 20) 0.25))
        (ok (= (aref eighth 20) 0.125))
        (ok (= (aref perspective 21) render:*wireframe*))
        (ok (equalp #(0.5 0.5 0.001 0.001)
                    (subseq perspective 48 52)))
        (ok (equalp #(61.5 48.5 15.48 0.0)
                    (subseq perspective 52 56)))
        (ok (equalp (luft.render::light-sun-color luft.render:*light*)
                    (subseq perspective 60 64)))
        (ok (equalp (luft.render::light-sky-color luft.render:*light*)
                    (subseq perspective 64 68)))
        (ok (equalp (luft.render::light-ground-color luft.render:*light*)
                    (subseq perspective 68 72)))
        (ok (= (/ luft.render::+shadow-map-size+)
               (aref perspective 88)))
        (ok (= (luft.render::light-shadow-filter-radius luft.render:*light*)
               (aref perspective 91)))
        (ok (equalp #(61.5 48.5 15.48 0.0)
                    (subseq perspective 92 96)))
        (ok (equalp #(0.0 1.0 0.0 0.0)
                    (subseq perspective 96 100)))))))

(deftest an-off-centre-pointer-ray-inverts-the-rendered-projection
  (let* ((canvas (make-instance 'luv:sdl-canvas :width 1000 :height 800))
         (camera
           (render:make-fly-camera
            :position (luv.arithmetic.lisp.vec3:make-vec3 0.0 0.0 10.0)
            :yaw 0.0 :pitch 0.0))
         (viewer
           (clim:make-application-frame
            'render:viewer :canvas canvas :camera camera))
         (render:*projection* :isometric)
         (render:*isometric-height* 20.0))
    ;; At yaw and pitch zero camera UP is world +Z.  A pointer one quarter of
    ;; the viewport below centre must therefore start five cells below the
    ;; camera, not at the vertically mirrored point five cells above it.
    (setf (luft.render::viewer-pointer-x viewer) 500.0
          (luft.render::viewer-pointer-y viewer) 600.0)
    (multiple-value-bind (origin direction)
        (luft.render::viewer-pointer-ray viewer)
      (ok (< (abs (- 5.0
                     (luv.arithmetic.lisp.vec3:vec3-z origin)))
             1.0e-6))
      (ok (< (abs (- 1.0
                     (luv.arithmetic.lisp.vec3:vec3-x direction)))
             1.0e-6)))))

(deftest the-light-frame-is-texel-stable-under-subtexel-camera-motion
  (let* ((light luft.render:*light*)
         (center (luv.arithmetic.lisp.vec3:make-vec3 31.0 47.0 13.0))
         (rows (luft.render::light-shadow-rows light center))
         (texel (/ (* 2.0 (luft.render::light-shadow-half-extent light))
                   luft.render::+shadow-map-size+))
         (nearby
           (luv.arithmetic.lisp.vec3:make-vec3
            (+ (luv.arithmetic.lisp.vec3:vec3-x center) (* texel 0.1))
            (luv.arithmetic.lisp.vec3:vec3-y center)
            (luv.arithmetic.lisp.vec3:vec3-z center)))
         (nearby-rows (luft.render::light-shadow-rows light nearby)))
    (ok (= 16 (length rows)))
    ;; Snapping is in the light plane: a tiny arbitrary world translation may
    ;; cross no light-space texel boundary, and therefore leaves X/Y rows exact.
    (ok (equalp (subseq rows 0 8) (subseq nearby-rows 0 8)))
    (ok (= 36 (length (luft.render::light-uniform-data light center))))))

(deftest bright-frame-discontinuities-tolerate-local-motion
  (let* ((width 9)
         (height 7)
         (bytes (* width height 4))
         (previous
           (make-array bytes :element-type '(unsigned-byte 8)
                             :initial-element 0))
         (current
           (make-array bytes :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (labels ((brighten (pixels x y value)
               (let ((offset (* 4 (+ x (* y width)))))
                 (setf (aref pixels offset) value
                       (aref pixels (+ offset 1)) value
                       (aref pixels (+ offset 2)) value
                       (aref pixels (+ offset 3)) 255))))
      ;; A bright point moving by one pixel is explained by the old local
      ;; neighborhood; a remote bright eruption is not.
      (brighten previous 2 3 250)
      (brighten current 3 3 250)
      (brighten current 7 4 245)
      (multiple-value-bind (count largest samples)
          (luft.render::%bright-frame-discontinuity
           current previous width height
           :top 0 :threshold 230 :jump 40 :radius 1 :border 0)
        (ok (= 1 count))
        (ok (= 245 largest))
        (ok (equal '((7 4 245 0 245)) samples))))))

(deftest bright-frame-transients-must-vanish-on-both-sides
  (let* ((width 9)
         (height 7)
         (bytes (* width height 4))
         (previous
           (make-array bytes :element-type '(unsigned-byte 8)
                             :initial-element 0))
         (current
           (make-array bytes :element-type '(unsigned-byte 8)
                             :initial-element 0))
         (following
           (make-array bytes :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (labels ((brighten (pixels x y value)
               (let ((offset (* 4 (+ x (* y width)))))
                 (setf (aref pixels offset) value
                       (aref pixels (+ offset 1)) value
                       (aref pixels (+ offset 2)) value
                       (aref pixels (+ offset 3)) 255))))
      ;; Smooth motion is explained on both sides.  A new surface entering in
      ;; CURRENT and persisting in FOLLOWING is explained on its latter side.
      ;; Only the one-frame eruption is a transient.
      (brighten previous 1 2 250)
      (brighten current 2 2 250)
      (brighten following 3 2 250)
      (brighten current 5 3 245)
      (brighten following 5 3 245)
      (brighten current 7 4 240)
      (multiple-value-bind (count largest samples)
          (luft.render::%bright-frame-discontinuity
           current previous width height
           :top 0 :threshold 230 :jump 40 :radius 1 :border 0
           :following following)
        (ok (= 1 count))
        (ok (= 240 largest))
        (ok (equal '((7 4 240 0 0 240)) samples))))))

(deftest a-live-frame-ring-keeps-only-its-final-time-window
  (let ((recorder
          (luft.render::make-viewer-frame-recorder-state
           :seconds 2.0d0 :capacity 5
           :frames (vector :zero :one :two :three :four)
           :times (vector 0.0d0 1.0d0 2.0d0 3.0d0 4.0d0)
           :frame-numbers (vector 0 1 2 3 4))))
    (ok (equal '((2 2.0d0 :two) (3 3.0d0 :three) (4 4.0d0 :four))
               (luft.render::%viewer-frame-recorder-entries recorder)))))

(deftest a-pointer-ray-retains-the-semantic-boundary-site
  (let* ((domain (luft:make-world-domain :horizontal-bits 4))
         (builder (luft:make-chain-builder domain)))
    (luft:chain-builder-add-site
     builder (luft:make-site domain 4 4 4 luft:+cell-extent+ 1))
    (let* ((solid (luft:finish-chain-builder builder))
           (inspection
             (luft.render::raycast-site
              solid
              (luv.arithmetic.lisp.vec3:make-vec3 4.5 4.5 8.0)
              (luv.arithmetic.lisp.vec3:make-vec3 0.0 0.0 -1.0)))
           (site (luft.render::site-inspection-site inspection))
           (cell (luft.render::site-inspection-cell inspection)))
      (ok inspection)
      (ok (= 3.0 (luft.render::site-inspection-distance inspection)))
      (ok (= luft:+xy-face-extent+ (luft:site-extent site)))
      (ok (luft:site-positive-p site))
      (ok (= 4 (luft:site-x site) (luft:site-x cell)))
      (ok (= 4 (luft:site-y site) (luft:site-y cell)))
      (ok (= 5 (luft:site-z site)))
      (ok (= 4 (luft:site-z cell)))
      (ok (= #x80 (render:site-inspection-star-mask inspection)))
      (ok (not (luft:star-singular-p
                (render:site-inspection-star-mask inspection)))))))

(deftest film-cleanup-cannot-resurrect-a-shutting-down-viewer
  (flet ((make-probe ()
           (clim:make-application-frame
            'render:viewer :canvas (make-instance 'luv:canvas))))
    (let* ((viewer (make-probe))
           (capture
             (make-instance 'luv:application-capture
                            :application viewer :kind :film)))
      (setf (luv:capture-client-state capture) '(:running-p t)
            (render::viewer-running-p viewer) nil)
      (luv:cleanup-capture viewer capture)
      (ok (render::viewer-running-p viewer)))
    (let* ((viewer (make-probe))
           (capture
             (make-instance 'luv:application-capture
                            :application viewer :kind :film)))
      (setf (luv:capture-client-state capture) '(:running-p t)
            (render::viewer-running-p viewer) nil)
      (luv:request-application-capture-shutdown viewer)
      (luv:cleanup-capture viewer capture)
      (ok (not (render::viewer-running-p viewer))))))
