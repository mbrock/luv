(in-package #:luft.render)

(defparameter *wireframe* 0.0
  "Global construction-edge strength.  The atelier toggles it between 0 and 1.")

(defparameter *render-scale* 0.75
  "Linear internal resolution of the LUFT scene before temporal upscaling.")

(defparameter *scene-sample-count* 4
  "Raster samples used by Luft's geometry, motion, and depth scene pass.")

(defparameter *temporal-upscaling-p* t
  "Whether LUFT uses temporal reconstruction on supported GPU devices.")

(defparameter *vulkan-temporal-history-weight* 0.97f0
  "Baseline retained history for Luft's inspectable Vulkan temporal resolve.")

(defparameter *flame-time* nil
  "Optional deterministic torch-flame time in seconds.

NIL lets the live viewer pass its monotonic presentation clock.  Captures may
dynamically bind a real value to reproduce the exact same flame field.")

(defconstant +render-template-vertex-count+ 6)

(defconstant +star-meshlet-triangle-capacity+ 25)
(defconstant +star-meshlet-vertex-capacity+
  (* 3 +star-meshlet-triangle-capacity+))
(defconstant +star-meshlet-record-count+
  (1+ +star-meshlet-vertex-capacity+))
(defconstant +star-meshlet-coordinate-bias+ 8)

(defun star-meshlet-template-words ()
  "Return the fixed 256-record triangle-soup atlas consumed by mesh shaders.

Each star owns one fixed-size block (#0UAD9N).  Its first uvec4 contains the triangle
count; the remaining records are the three vertices of each triangle in
outward order.  Coordinates are biased only to keep this first ABI unsigned."
  (let ((words
          (make-array (* 256 +star-meshlet-record-count+ 4)
                      :element-type '(unsigned-byte 32)
                      :initial-element 0)))
    (dotimes (star 256 words)
      (let* ((triangles (luft:star-atlas-owned-triangles star))
             (appearance (luft:star-atlas-owned-appearance-masks star))
             (triangle-count (length triangles))
             (block (* star +star-meshlet-record-count+ 4)))
        (when (> triangle-count +star-meshlet-triangle-capacity+)
          (error "Star #x~2,'0X owns ~D triangles; the meshlet ABI admits ~D."
                 star triangle-count +star-meshlet-triangle-capacity+))
        (setf (aref words block) triangle-count)
        (loop for triangle in triangles
              for (material-mask light-mask) in appearance
              for triangle-index from 0
              do (loop for point in triangle
                       for corner from 0
                       for record = (+ 1 (* 3 triangle-index) corner)
                       for offset = (+ block (* 4 record))
                       do (destructuring-bind (x y z) point
                            (setf (aref words offset)
                                  (+ x +star-meshlet-coordinate-bias+)
                                  (aref words (+ offset 1))
                                  (+ y +star-meshlet-coordinate-bias+)
                                  (aref words (+ offset 2))
                                  (+ z +star-meshlet-coordinate-bias+)
                                  (aref words (+ offset 3))
                                  (if (zerop corner)
                                      (logior material-mask
                                              (ash light-mask 8))
                                      0)))))))))

(defun pack-terrain-appearance-codes (codes)
  "Pack eight u8 sample codes per star into the GPU's parallel uvec2 lane."
  (declare (type (simple-array (unsigned-byte 8) (*)) codes)
           (optimize (speed 3) (safety 1)))
  (unless (zerop (mod (length codes) 8))
    (error "Terrain appearance has ~D bytes, not eight per active star."
           (length codes)))
  (let ((words (make-array (/ (length codes) 4)
                           :element-type '(unsigned-byte 32))))
    (loop for offset fixnum from 0 below (length codes) by 4
          for word fixnum from 0
          do (setf (aref words word)
                   (logior (aref codes offset)
                           (ash (aref codes (+ offset 1)) 8)
                           (ash (aref codes (+ offset 2)) 16)
                           (ash (aref codes (+ offset 3)) 24))))
    words))

(defstruct (render-population
             (:constructor %make-render-population
                 (instance-words appearance-words descriptor-words
                  mesh-workgroup-count))
             (:copier nil))
  "Geometry sites plus a one-for-one, independently replaceable appearance."
  (instance-words #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  (appearance-words #() :type (simple-array (unsigned-byte 32) (*))
                    :read-only t)
  (descriptor-words #() :type (simple-array (unsigned-byte 32) (*))
                    :read-only t)
  (mesh-workgroup-count 0 :type (integer 0 *) :read-only t))

(defstruct (resident-population
             (:constructor %make-resident-population
                 (population instance-buffer template-buffer appearance-buffer
                  descriptor-buffer bind-group shadow-bind-group))
             (:copier nil))
  "One chunk's CPU population and independently retained GPU realization."
  (population nil :type render-population :read-only t)
  (instance-buffer nil :read-only t)
  (template-buffer nil :read-only t)
  (appearance-buffer nil :read-only t)
  (descriptor-buffer nil :read-only t)
  (bind-group nil :read-only t)
  (shadow-bind-group nil :read-only t))

(defstruct (prepared-render-mesh
             (:constructor %make-prepared-render-mesh (mesh population))
             (:copier nil))
  "Worker-transferable CPU realization of one semantic surface mesh."
  (mesh nil :type luft:surface-mesh :read-only t)
  (population nil :type render-population :read-only t))

(zdefun (prepare-render-mesh :zone :luft/prepare-population) (mesh)
  "Canonicalize one MESH before it crosses to the renderer owner."
  (check-type mesh luft:surface-mesh)
  (%make-prepared-render-mesh mesh (make-render-population (list mesh))))

(defun %make-star-render-population (meshes)
  "Flatten geometry and its parallel active-star appearance independently."
  (let* ((site-words
           (apply #'concatenate '(simple-array (unsigned-byte 32) (*))
                  (mapcar #'luft:surface-mesh-star-site-words meshes)))
         (appearance-codes
           (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                  (mapcar #'luft:surface-mesh-appearance-codes meshes)))
         (descriptor-words
           (or (loop for mesh in meshes
                     for words =
                       (luft:surface-mesh-appearance-descriptor-words mesh)
                     when (plusp (length words)) return words)
               (compile-terrain-material-descriptors
                (make-scene-material-vocabulary)))))
    (unless (= (* 2 (length site-words)) (length appearance-codes))
      (error "~D site words do not have a one-for-one eight-byte appearance (~D bytes)."
             (length site-words) (length appearance-codes)))
    (dolist (mesh meshes)
      (let ((words (luft:surface-mesh-appearance-descriptor-words mesh)))
        (unless (or (zerop (length words)) (equalp words descriptor-words))
          (error "One terrain population contains incompatible material palettes."))))
    (%make-render-population
     site-words (pack-terrain-appearance-codes appearance-codes)
     descriptor-words (/ (length site-words) 4))))

(zdefun (make-render-population :zone :luft/build-render-population
                                :value (length meshes))
    (meshes)
  "Prepare the sole terrain ABI: active sites indexing the fixed star atlas."
  (%make-star-render-population meshes))

(defstruct (renderer-slot-provenance
             (:constructor %make-renderer-slot-provenance
                 (key scene request-stamp result-stamp light-generation tree))
             (:copier nil))
  "Narrow immutable evidence for one exact installed semantic mesh tree."
  (key nil :read-only t)
  (scene nil :type scene :read-only t)
  (request-stamp nil :read-only t)
  (result-stamp nil :read-only t)
  (light-generation nil :type realized-light-generation :read-only t)
  (tree nil :type surface-mesh-tree-manifest :read-only t))

(defstruct (mesh-slot (:constructor %make-mesh-slot) (:copier nil))
  "One mesh's semantic residency and optional construction-overlay resources."
  (mesh nil)
  (provenance nil :type (or null renderer-slot-provenance))
  (resident nil)
  (lattice-point-buffer nil)
  (lattice-point-count 0)
  (lattice-point-group nil))

(defstruct (renderer-publication
             (:constructor %make-renderer-publication
                 (mesh-slots slot-order torch-frame-data flame-instance-count
                  flame-instance-buffer torch-body-bind-group
                  torch-body-shadow-bind-group scene-generation))
             (:copier nil))
  "One atomically installed renderer residency/resource generation.

The table itself is never mutated after publication.  Mesh slots retained from
the preceding generation may be shared, but the sorted order and every global
attachment resource describe exactly this table."
  (mesh-slots (make-hash-table :test #'eql) :read-only t)
  (slot-order nil :type list :read-only t)
  (torch-frame-data (make-array 0 :element-type 'single-float) :read-only t)
  (flame-instance-count 0 :type (integer 0 *) :read-only t)
  (flame-instance-buffer nil :read-only t)
  (torch-body-bind-group nil :read-only t)
  (torch-body-shadow-bind-group nil :read-only t)
  (scene-generation nil :type (or null scene-mesh-generation) :read-only t))

(defun %make-empty-renderer-publication
    (&key flame-instance-buffer torch-body-bind-group
          torch-body-shadow-bind-group)
  (%make-renderer-publication
   (make-hash-table :test #'eql) nil
   (make-array 0 :element-type 'single-float) 0
   flame-instance-buffer torch-body-bind-group
   torch-body-shadow-bind-group nil))

(defstruct (renderer-target-resources
             (:constructor %make-renderer-target-resources
                 (&key extent render-extent temporal-scaler
                  depth-msaa-texture depth-msaa-view depth-texture depth-view
                  scene-msaa-texture scene-msaa-view scene-texture scene-view
                  motion-msaa-texture motion-msaa-view
                  motion-texture motion-view
                  resolved-texture resolved-view history-texture history-view
                  temporal-bind-group composite-texture composite-view
                  composite-source-bind-group present-bind-group
                  exposure-binding))
             (:copier nil))
  "One immutable output-size-dependent texture/view/resource cohort."
  (extent nil :type list :read-only t)
  (render-extent nil :type list :read-only t)
  (temporal-scaler nil :read-only t)
  (depth-msaa-texture nil :read-only t)
  (depth-msaa-view nil :read-only t)
  (depth-texture nil :read-only t)
  (depth-view nil :read-only t)
  (scene-msaa-texture nil :read-only t)
  (scene-msaa-view nil :read-only t)
  (scene-texture nil :read-only t)
  (scene-view nil :read-only t)
  (motion-msaa-texture nil :read-only t)
  (motion-msaa-view nil :read-only t)
  (motion-texture nil :read-only t)
  (motion-view nil :read-only t)
  (resolved-texture nil :read-only t)
  (resolved-view nil :read-only t)
  (history-texture nil :read-only t)
  (history-view nil :read-only t)
  (temporal-bind-group nil :read-only t)
  (composite-texture nil :read-only t)
  (composite-view nil :read-only t)
  (composite-source-bind-group nil :read-only t)
  (present-bind-group nil :read-only t)
  (exposure-binding nil :read-only t))

(defstruct (renderer-flame-target-join
             (:constructor %make-renderer-flame-target-join (bind-group))
             (:copier nil))
  "One owned population-times-depth binding, independent of target textures."
  (bind-group nil :read-only t))

(defstruct (renderer-target-generation
             (:constructor %make-renderer-target-generation (resources flame-join))
             (:copier nil))
  "The immutable product of one target cohort and one flame/depth join.

A resize creates and owns a fresh pair.  A residency publication stages only a
new join while borrowing the installed resource cohort; at the noninterleavable
owner-thread commit, ownership of that exact cohort remains with the renderer
and only the old join retires.  Thus no target texture/view is ever cloned,
ambiguously co-owned, or retired by a population rollback."
  (resources nil :type renderer-target-resources :read-only t)
  (flame-join nil :type renderer-flame-target-join :read-only t))

(defun %make-empty-renderer-target-generation ()
  (%make-renderer-target-generation
   (%make-renderer-target-resources
    :extent nil :render-extent nil)
   (%make-renderer-flame-target-join nil)))

(defmacro define-renderer-target-resource-reader (name)
  `(defun ,(intern (format nil "RENDERER-TARGET-GENERATION-~A" name))
       (generation)
     (,(intern (format nil "RENDERER-TARGET-RESOURCES-~A" name))
      (renderer-target-generation-resources generation))))

(define-renderer-target-resource-reader extent)
(define-renderer-target-resource-reader render-extent)
(define-renderer-target-resource-reader temporal-scaler)
(define-renderer-target-resource-reader depth-msaa-texture)
(define-renderer-target-resource-reader depth-msaa-view)
(define-renderer-target-resource-reader depth-texture)
(define-renderer-target-resource-reader depth-view)
(define-renderer-target-resource-reader scene-msaa-texture)
(define-renderer-target-resource-reader scene-msaa-view)
(define-renderer-target-resource-reader scene-texture)
(define-renderer-target-resource-reader scene-view)
(define-renderer-target-resource-reader motion-msaa-texture)
(define-renderer-target-resource-reader motion-msaa-view)
(define-renderer-target-resource-reader motion-texture)
(define-renderer-target-resource-reader motion-view)
(define-renderer-target-resource-reader resolved-texture)
(define-renderer-target-resource-reader resolved-view)
(define-renderer-target-resource-reader history-texture)
(define-renderer-target-resource-reader history-view)
(define-renderer-target-resource-reader temporal-bind-group)
(define-renderer-target-resource-reader composite-texture)
(define-renderer-target-resource-reader composite-view)
(define-renderer-target-resource-reader composite-source-bind-group)
(define-renderer-target-resource-reader present-bind-group)
(define-renderer-target-resource-reader exposure-binding)

(defun renderer-target-generation-flame-bind-group (generation)
  (renderer-flame-target-join-bind-group
   (renderer-target-generation-flame-join generation)))

(defun %copy-torch-frame-data (source)
  "Validate and copy packed three-Vec4 frames across an ownership boundary."
  (check-type source vector)
  (unless (zerop (mod (length source) +torch-flame-instance-scalar-count+))
    (error "A ~D-scalar flame stream is not an integral frame population."
           (length source)))
  (let ((copy (map '(simple-array single-float (*))
                   (lambda (scalar)
                     (check-type scalar single-float)
                     scalar)
                   source)))
    (loop for offset from 0 below (length copy)
            by +torch-flame-instance-scalar-count+
          do (validate-torch-flame-frame copy offset))
    copy))

(defun surface-mesh-torch-frame-data (mesh)
  "Flatten MESH's sparse attachment frames, including companion owners."
  (check-type mesh luft:surface-mesh)
  (let ((frames nil))
    (labels ((visit (surface)
               (dolist (frame (luft:surface-mesh-attachments surface))
                 (push frame frames))
               (dolist (companion (luft:surface-mesh-companions surface))
                 (visit companion))))
      (visit mesh))
    (let* ((frames (nreverse frames))
           (data
             (make-array (* +torch-flame-instance-scalar-count+ (length frames))
                         :element-type 'single-float)))
      (loop for frame in frames
            for offset from 0 by +torch-flame-instance-scalar-count+
            do (validate-torch-flame-frame frame)
               (replace data frame :start1 offset))
      data)))

(defun mesh-slots-torch-frame-data (entries)
  "Flatten attachment frames from sorted key-to-MESH-SLOT ENTRIES."
  (let* ((runs
           (mapcar (lambda (entry)
                     (surface-mesh-torch-frame-data
                      (mesh-slot-mesh (cdr entry))))
                   entries))
         (length (reduce #'+ runs :key #'length :initial-value 0))
         (data (make-array length :element-type 'single-float)))
    (loop with offset = 0
          for run in runs
          do (replace data run :start1 offset)
             (incf offset (length run)))
    data))

(defclass renderer ()
  ((device :initarg :device :reader renderer-device)
   ;; One owner-thread pointer swap publishes the complete keyed residency and
   ;; all resources derived from it.  Readers never observe a table paired
   ;; with another generation's order or attachment buffers.
   (publication :initarg :publication :accessor renderer-publication)
   (camera-buffer :initarg :camera-buffer :accessor renderer-camera-buffer)
   (star-template-buffer :initarg :star-template-buffer :initform nil
                         :accessor renderer-star-template-buffer)
   (frame-resources
    :initform (make-canvas-frame-resource-cache)
    :reader renderer-frame-resources)
   (layout :initarg :layout :accessor renderer-layout)
   (vertex-module :initarg :vertex-module :accessor renderer-vertex-module)
   (fragment-module :initarg :fragment-module :accessor renderer-fragment-module)
   (torch-body-fragment-module
    :initarg :torch-body-fragment-module
    :initform nil
    :accessor renderer-torch-body-fragment-module)
   (pipeline :initarg :pipeline :accessor renderer-pipeline)
   (flame-effect-buffer :initarg :flame-effect-buffer
                        :accessor renderer-flame-effect-buffer)
   (flame-layout :initarg :flame-layout :accessor renderer-flame-layout)
   (flame-vertex-module :initarg :flame-vertex-module
                        :accessor renderer-flame-vertex-module)
   (flame-fragment-module :initarg :flame-fragment-module
                          :accessor renderer-flame-fragment-module)
   (flame-pipeline :initarg :flame-pipeline
                   :accessor renderer-flame-pipeline)
   (flame-depth-sampler :initarg :flame-depth-sampler
                        :accessor renderer-flame-depth-sampler)
   (composite-layout :initarg :composite-layout
                     :accessor renderer-composite-layout)
   (composite-fragment-module :initarg :composite-fragment-module
                              :accessor renderer-composite-fragment-module)
   (composite-pipeline :initarg :composite-pipeline
                       :accessor renderer-composite-pipeline)
   ;; The opaque socket/shaft and animated flame consume the exact same
   ;; realized three-row frame population.  Only the immutable canonical body
   ;; vertices and their render pipelines differ.
   (torch-body-vertex-buffer :initarg :torch-body-vertex-buffer
                             :accessor renderer-torch-body-vertex-buffer)
   (torch-body-layout :initarg :torch-body-layout
                      :accessor renderer-torch-body-layout)
   (torch-body-vertex-module :initarg :torch-body-vertex-module
                             :accessor renderer-torch-body-vertex-module)
   (torch-body-shadow-vertex-module
    :initarg :torch-body-shadow-vertex-module
    :accessor renderer-torch-body-shadow-vertex-module)
   (torch-body-pipeline :initarg :torch-body-pipeline
                        :accessor renderer-torch-body-pipeline)
   (torch-body-shadow-pipeline :initarg :torch-body-shadow-pipeline
                               :accessor renderer-torch-body-shadow-pipeline)
   (shadow-texture :initarg :shadow-texture :accessor renderer-shadow-texture)
   (shadow-view :initarg :shadow-view :accessor renderer-shadow-view)
   (shadow-sampler :initarg :shadow-sampler :accessor renderer-shadow-sampler)
   (shadow-layout :initarg :shadow-layout :accessor renderer-shadow-layout)
   (shadow-vertex-module :initarg :shadow-vertex-module
                         :accessor renderer-shadow-vertex-module)
   (shadow-pipeline :initarg :shadow-pipeline
                    :accessor renderer-shadow-pipeline)
   (player-sdf-layout :initarg :player-sdf-layout
                      :accessor renderer-player-sdf-layout)
   (player-sdf-bind-group :initarg :player-sdf-bind-group
                          :accessor renderer-player-sdf-bind-group)
   (player-sdf-vertex-module :initarg :player-sdf-vertex-module
                             :accessor renderer-player-sdf-vertex-module)
   (player-sdf-fragment-module :initarg :player-sdf-fragment-module
                               :accessor renderer-player-sdf-fragment-module)
   (player-sdf-pipeline :initarg :player-sdf-pipeline
                        :accessor renderer-player-sdf-pipeline)
   (lattice-point-layout :initarg :lattice-point-layout
                         :accessor renderer-lattice-point-layout)
   (lattice-point-vertex-module :initarg :lattice-point-vertex-module
                                :accessor renderer-lattice-point-vertex-module)
   (lattice-point-fragment-module :initarg :lattice-point-fragment-module
                                  :accessor renderer-lattice-point-fragment-module)
   (lattice-point-pipeline :initarg :lattice-point-pipeline
                           :accessor renderer-lattice-point-pipeline)
   (sky-layout :initform nil :accessor renderer-sky-layout)
   (sky-bind-group :initform nil :accessor renderer-sky-bind-group)
   (sky-fragment-module :initform nil :accessor renderer-sky-fragment-module)
   (sky-pipeline :initform nil :accessor renderer-sky-pipeline)
   (color-format :initarg :color-format :reader renderer-color-format)
   (temporal-p :initarg :temporal-p :reader renderer-temporal-p)
   (temporal-resolve-kind :initarg :temporal-resolve-kind :initform nil
                          :reader renderer-temporal-resolve-kind)
   (temporal-layout :initform nil :accessor renderer-temporal-layout)
   (temporal-fragment-module :initform nil
                             :accessor renderer-temporal-fragment-module)
   (temporal-pipeline :initform nil :accessor renderer-temporal-pipeline)
   ;; The complete resize-owned identity is published by this one pointer.
   ;; Resource creation, target-dependent binding, and failure cleanup happen
   ;; before it changes; no frame can observe a partially replaced target set.
   (target-generation
    :initarg :target-generation
    :initform (%make-empty-renderer-target-generation)
    :accessor renderer-target-generation)
   (present-layout :initform nil :accessor renderer-present-layout)
   (present-vertex-module :initform nil
                          :accessor renderer-present-vertex-module)
   (present-fragment-module :initform nil
                            :accessor renderer-present-fragment-module)
   (present-pipeline :initform nil :accessor renderer-present-pipeline)
   (exposure-control :initarg :exposure-control :reader renderer-exposure-control)
   (exposure-factory :initarg :exposure-factory :reader renderer-exposure-factory)
   (sampler :initform nil :accessor renderer-sampler)
   (frame-index :initform 0 :accessor renderer-frame-index)
   (previous-view :initform nil :accessor renderer-previous-view)
   (history-valid-p :initform nil :accessor renderer-history-valid-p)
   (history-used-p :initform nil :accessor renderer-history-used-p)))

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
                 effect
                 (create
                  (renderer-device renderer)
                  (make-buffer-descriptor
                   :label "luft presentation-slot flame effect"
                   :size (torch-flame-effect-byte-size)
                   :usage '(:uniform :copy-dst))))
           (setf completed-p t)
           (%make-renderer-frame-state
            :camera-buffer camera :flame-effect-buffer effect))
      (unless completed-p
        (when effect (ignore-errors (destroy effect)))
        (when camera (ignore-errors (destroy camera)))))))

(defun destroy-renderer-frame-state (state)
  "Release one presentation-slot upload cohort and its derived bindings."
  (with-release-report
    (maphash
     (lambda (key group)
       (declare (ignore key))
       (releasing :frame-bind-group (destroy group)))
     (renderer-frame-state-bind-groups state))
    (clrhash (renderer-frame-state-bind-groups state))
    (releasing :frame-flame-effect-buffer
      (destroy (renderer-frame-state-flame-effect-buffer state)))
    (releasing :frame-camera-buffer
      (destroy (renderer-frame-state-camera-buffer state))))
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
             (destroy (gethash binding-key groups))
             (remhash binding-key groups))))))
   (renderer-frame-resources renderer))
  renderer)

(defun renderer-frame-bind-group (renderer frame key label layout entries)
  "Return FRAME's binding KEY, creating it transactionally from ENTRIES."
  (let ((groups (renderer-frame-state-bind-groups frame)))
    (or (gethash key groups)
        (let ((group
                (create
                 (renderer-device renderer)
                 (make-bind-group-descriptor
                  :label label :layout layout :entries entries))))
          (setf (gethash key groups) group)))))

(defun renderer-frame-state-for (renderer context surface-texture)
  "Acquire RENDERER's safely reusable mutable state for SURFACE-TEXTURE."
  (canvas-frame-resource
   (renderer-frame-resources renderer) context surface-texture
   (lambda (key surface)
     (declare (ignore key surface))
     (make-renderer-frame-state renderer))))

(defun renderer-frame-resident-bind-group (renderer frame resident shadow-p)
  "Bind one immutable resident population to FRAME's camera upload."
  (let ((camera (renderer-frame-state-camera-buffer frame)))
    (if shadow-p
        (renderer-frame-bind-group
         renderer frame (list :resident-shadow resident)
         "luft frame-local resident shadow population"
         (renderer-shadow-layout renderer)
         `((:binding 0 :resource ,(resident-population-instance-buffer resident))
           (:binding 1 :resource ,(resident-population-template-buffer resident))
           (:binding 2 :resource ,camera)))
        (renderer-frame-bind-group
         renderer frame (list :resident-scene resident
                              (renderer-shadow-view renderer))
         "luft frame-local resident site population"
         (renderer-layout renderer)
         `((:binding 0 :resource ,(resident-population-instance-buffer resident))
           (:binding 1 :resource ,(resident-population-template-buffer resident))
           (:binding 2 :resource ,camera)
           (:binding 3 :resource ,(resident-population-appearance-buffer resident))
           (:binding 4 :resource ,(renderer-shadow-view renderer))
           (:binding 5 :resource ,(renderer-shadow-sampler renderer))
           (:binding 6 :resource ,(resident-population-descriptor-buffer resident)))))))

(defun renderer-frame-torch-body-bind-group (renderer frame shadow-p)
  (let ((camera (renderer-frame-state-camera-buffer frame))
        (instances (renderer-flame-instance-buffer renderer)))
    (if shadow-p
        (renderer-frame-bind-group
         renderer frame (list :torch-shadow instances)
         "luft frame-local torch-body shadows"
         (renderer-shadow-layout renderer)
         `((:binding 0 :resource ,instances)
           (:binding 1 :resource ,(renderer-torch-body-vertex-buffer renderer))
           (:binding 2 :resource ,camera)))
        (renderer-frame-bind-group
         renderer frame (list :torch-scene instances
                              (renderer-shadow-view renderer))
         "luft frame-local torch bodies"
         (renderer-torch-body-layout renderer)
         `((:binding 0 :resource ,instances)
           (:binding 1 :resource ,(renderer-torch-body-vertex-buffer renderer))
           (:binding 2 :resource ,camera)
           (:binding 4 :resource ,(renderer-shadow-view renderer))
           (:binding 5 :resource ,(renderer-shadow-sampler renderer)))))))

(defun renderer-frame-sky-bind-group (renderer frame)
  (renderer-frame-bind-group
   renderer frame '(:sky) "luft frame-local HDR sky"
   (renderer-sky-layout renderer)
   `((:binding 0 :resource ,(renderer-frame-state-camera-buffer frame)))))

(defun renderer-frame-player-bind-group (renderer frame)
  (renderer-frame-bind-group
   renderer frame (list :player (renderer-shadow-view renderer))
   "luft frame-local walking player SDF"
   (renderer-player-sdf-layout renderer)
   `((:binding 0 :resource ,(renderer-frame-state-camera-buffer frame))
     (:binding 1 :resource ,(renderer-shadow-view renderer))
     (:binding 2 :resource ,(renderer-shadow-sampler renderer)))))

(defun renderer-frame-lattice-bind-group (renderer frame slot)
  (renderer-frame-bind-group
   renderer frame (list :lattice slot (mesh-slot-lattice-point-buffer slot))
   "luft frame-local eighth-cell lattice points"
   (renderer-lattice-point-layout renderer)
   `((:binding 0 :resource ,(mesh-slot-lattice-point-buffer slot))
     (:binding 1 :resource ,(renderer-frame-state-camera-buffer frame)))))

(defun renderer-frame-temporal-bind-group (renderer frame)
  (renderer-frame-bind-group
   renderer frame
   (list :temporal (renderer-scene-view renderer)
         (renderer-motion-view renderer) (renderer-history-view renderer))
   "luft frame-local temporal resolve inputs"
   (renderer-temporal-layout renderer)
   `((:binding 0 :resource ,(renderer-scene-view renderer))
     (:binding 1 :resource ,(renderer-motion-view renderer))
     (:binding 2 :resource ,(renderer-history-view renderer))
     (:binding 3 :resource ,(renderer-sampler renderer))
     (:binding 4 :resource ,(renderer-frame-state-camera-buffer frame)))))

(defun renderer-frame-flame-bind-group (renderer frame)
  (renderer-frame-bind-group
   renderer frame
   (list :flame (renderer-flame-instance-buffer renderer)
         (renderer-depth-view renderer))
   "luft frame-local post-temporal torch flames"
   (renderer-flame-layout renderer)
   `((:binding 0 :resource ,(renderer-flame-instance-buffer renderer))
     (:binding 1 :resource ,(renderer-frame-state-camera-buffer frame))
     (:binding 2 :resource ,(renderer-frame-state-flame-effect-buffer frame))
     (:binding 3 :resource ,(renderer-depth-view renderer))
     (:binding 4 :resource ,(renderer-flame-depth-sampler renderer)))))

(defun renderer-frame-present-bind-group (renderer frame)
  (renderer-frame-bind-group
   renderer frame
   (list :present (renderer-composite-view renderer)
         (renderer-depth-view renderer))
   "luft frame-local HDR presentation"
   (renderer-present-layout renderer)
   `((:binding 0 :resource ,(renderer-composite-view renderer))
     (:binding 1 :resource ,(renderer-sampler renderer))
     (:binding 2 :resource ,(renderer-depth-view renderer))
     (:binding 3 :resource ,(renderer-frame-state-camera-buffer frame)))))

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

(defun renderer-temporal-bind-group (renderer)
  (renderer-target-generation-temporal-bind-group
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

(defun renderer-present-bind-group (renderer)
  (renderer-target-generation-present-bind-group
   (renderer-target-generation renderer)))

(defun renderer-exposure (renderer)
  (exposure-value (renderer-exposure-control renderer)))

(defun renderer-exposure-binding (renderer)
  (renderer-target-generation-exposure-binding
   (renderer-target-generation renderer)))

(defun renderer-mesh-slots (renderer)
  (renderer-publication-mesh-slots (renderer-publication renderer)))

(defun renderer-slot-order (renderer)
  (renderer-publication-slot-order (renderer-publication renderer)))

(defun renderer-torch-frame-data (renderer)
  (renderer-publication-torch-frame-data (renderer-publication renderer)))

(defun renderer-flame-instance-count (renderer)
  (renderer-publication-flame-instance-count
   (renderer-publication renderer)))

(defun renderer-flame-instance-buffer (renderer)
  (renderer-publication-flame-instance-buffer
   (renderer-publication renderer)))

(defun renderer-flame-bind-group (renderer)
  (renderer-target-generation-flame-bind-group
   (renderer-target-generation renderer)))

(defun renderer-torch-body-bind-group (renderer)
  (renderer-publication-torch-body-bind-group
   (renderer-publication renderer)))

(defun renderer-torch-body-shadow-bind-group (renderer)
  (renderer-publication-torch-body-shadow-bind-group
   (renderer-publication renderer)))

(defun %make-renderer-flame-resources (renderer source)
  "Build, but do not publish, one validated flame-frame GPU population."
  (check-type renderer renderer)
  (let* ((data (%copy-torch-frame-data source))
         (count (/ (length data) +torch-flame-instance-scalar-count+))
         (device (renderer-device renderer))
         (buffer nil)
         (body-bind-group nil)
         (body-shadow-bind-group nil)
         (completed-p nil))
    (unwind-protect
         (progn
           (setf buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft global torch flame instances"
                          :size (max 16 (* 4 (length data)))
                          :usage '(:storage :copy-dst))))
           (when (plusp (length data))
             (write-buffer buffer data))
           (setf body-bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft realized torch bodies"
                          :layout (renderer-torch-body-layout renderer)
                          :entries
                          `((:binding 0 :resource ,buffer)
                            (:binding 1
                             :resource
                             ,(renderer-torch-body-vertex-buffer renderer))
                            (:binding 2
                             :resource ,(renderer-camera-buffer renderer))
                            (:binding 4
                             :resource ,(renderer-shadow-view renderer))
                            (:binding 5
                             :resource ,(renderer-shadow-sampler renderer))))))
           (setf body-shadow-bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft realized torch-body shadows"
                          :layout (renderer-shadow-layout renderer)
                          :entries
                          `((:binding 0 :resource ,buffer)
                            (:binding 1
                             :resource
                             ,(renderer-torch-body-vertex-buffer renderer))
                            (:binding 2
                             :resource ,(renderer-camera-buffer renderer))))))
           (setf completed-p t)
           (values data count buffer body-bind-group body-shadow-bind-group))
      (unless completed-p
        (when body-shadow-bind-group
          (ignore-errors (destroy body-shadow-bind-group)))
        (when body-bind-group (ignore-errors (destroy body-bind-group)))
        (when buffer (ignore-errors (destroy buffer)))))))

(defun temporal-resolve-kind (device)
  "Return the temporal implementation selected for DEVICE, or NIL."
  #-darwin (declare (ignore device))
  (when *temporal-upscaling-p*
    #+darwin
    (if (typep device 'metal-gpu-device) :metalfx :shader)
    #-darwin :shader))

(defun renderer-metalfx-temporal-p (renderer)
  (eq :metalfx (renderer-temporal-resolve-kind renderer)))

(defun renderer-shader-temporal-p (renderer)
  (eq :shader (renderer-temporal-resolve-kind renderer)))

(defun make-renderer-flame-depth-sampler (device)
  "Create the renderer-lifetime nearest sampler used only for opaque depth."
  (create device
          (make-sampler-descriptor
           :label "luft torch flame opaque-depth sampler"
           :mag-filter :nearest :min-filter :nearest
           :mipmap-filter :nearest)))

(defun make-renderer-target-flame-bind-group
    (renderer flame-instance-buffer depth-view)
  "Join one immutable flame population to one immutable depth target."
  (unless (and flame-instance-buffer depth-view)
    (error "A flame composite binding needs both frame storage and opaque depth."))
  (create
   (renderer-device renderer)
   (make-bind-group-descriptor
    :label "luft post-temporal torch flames"
    :layout (renderer-flame-layout renderer)
    :entries
    `((:binding 0 :resource ,flame-instance-buffer)
      (:binding 1 :resource ,(renderer-camera-buffer renderer))
      (:binding 2 :resource ,(renderer-flame-effect-buffer renderer))
      (:binding 3 :resource ,depth-view)
      (:binding 4 :resource ,(renderer-flame-depth-sampler renderer))))))

(defun make-retargeted-renderer-target-generation
    (renderer flame-instance-buffer)
  "Stage a target record whose only new resource is its flame/depth join.

All other resources are borrowed from the currently installed immutable
generation.  The caller either publishes this record and transfers ownership
of that cohort, retiring only the old join, or destroys the returned new join
on rollback.  No target texture is duplicated for an ordinary residency edit."
  (let* ((old (renderer-target-generation renderer))
         (depth-view (renderer-target-generation-depth-view old)))
    (if (null depth-view)
        (values old nil)
        (let (flame-group candidate (completed-p nil))
          (unwind-protect
               (progn
                 (setf flame-group
                       (make-renderer-target-flame-bind-group
                        renderer flame-instance-buffer depth-view)
                       candidate
                       (%make-renderer-target-generation
                        (renderer-target-generation-resources old)
                        (%make-renderer-flame-target-join flame-group))
                       completed-p t)
                 (values candidate flame-group))
            (unless completed-p
              (when flame-group (ignore-errors (destroy flame-group)))))))))

(defun destroy-renderer-flame-target-join (join)
  "Retire only JOIN's population-times-depth bind group."
  (check-type join renderer-flame-target-join)
  (let ((group (renderer-flame-target-join-bind-group join)))
    (when group (ignore-errors (destroy group))))
  (values))

(defun destroy-renderer-target-resources (resources)
  "Retire one unreferenced output-size texture/view/resource cohort."
  (check-type resources renderer-target-resources)
  ;; Bind groups die before the views they name; views die before textures.
  ;; The temporal scaler has its own backend-owned auxiliary resources and is
  ;; retired before the external texture cohort used to execute it.
  (dolist (resource
            (list
             (renderer-target-resources-present-bind-group resources)
             (renderer-target-resources-exposure-binding resources)
             (renderer-target-resources-composite-source-bind-group resources)
             (renderer-target-resources-temporal-bind-group resources)
             (renderer-target-resources-temporal-scaler resources)
             (renderer-target-resources-composite-view resources)
             (renderer-target-resources-history-view resources)
             (renderer-target-resources-resolved-view resources)
             (renderer-target-resources-motion-view resources)
             (renderer-target-resources-motion-msaa-view resources)
             (renderer-target-resources-scene-view resources)
             (renderer-target-resources-scene-msaa-view resources)
             (renderer-target-resources-depth-view resources)
             (renderer-target-resources-depth-msaa-view resources)
             (renderer-target-resources-composite-texture resources)
             (renderer-target-resources-history-texture resources)
             (renderer-target-resources-resolved-texture resources)
             (renderer-target-resources-motion-texture resources)
             (renderer-target-resources-motion-msaa-texture resources)
             (renderer-target-resources-scene-texture resources)
             (renderer-target-resources-scene-msaa-texture resources)
             (renderer-target-resources-depth-texture resources)
             (renderer-target-resources-depth-msaa-texture resources)))
    (when resource (ignore-errors (destroy resource))))
  (values))

(defun destroy-renderer-target-generation (generation)
  "Retire both independently owned halves of an unpublished generation."
  (check-type generation renderer-target-generation)
  (destroy-renderer-flame-target-join
   (renderer-target-generation-flame-join generation))
  (destroy-renderer-target-resources
   (renderer-target-generation-resources generation))
  (values))

(defun destroy-renderer-targets (renderer)
  "Unpublish and retire RENDERER's complete resize-owned generation."
  (let ((old-generation (renderer-target-generation renderer)))
    (setf (renderer-target-generation renderer)
          (%make-empty-renderer-target-generation))
    (destroy-renderer-target-generation old-generation)))

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
  "Stage one complete output-size generation without publishing it."
  (let* ((device (renderer-device renderer))
         (temporal-p (renderer-temporal-p renderer))
         (metalfx-p (renderer-metalfx-temporal-p renderer))
         (shader-temporal-p (renderer-shader-temporal-p renderer))
         ;; These owned copies are the immutable dimensions of the candidate.
         ;; Validate/list-copy before the first GPU allocation.
         (extent (copy-list extent))
         (render-extent (renderer-render-scale-extent renderer extent))
         scaler depth-msaa depth-msaa-view depth depth-view
         scene-msaa scene-msaa-view scene scene-view
         motion-msaa motion-msaa-view motion motion-view
         resolved resolved-view history history-view temporal-group
         composite composite-view
         composite-source-group flame-group present-group exposure-group
         resource-cohort flame-join generation
         (completed-p nil))
    (labels ((usage (base extra)
               (remove-duplicates (append base extra)))
             (cleanup-locals ()
               (dolist (resource
                         (list present-group exposure-group flame-group
                               composite-source-group temporal-group scaler
                               composite-view history-view resolved-view
                               motion-view motion-msaa-view
                               scene-view scene-msaa-view
                               depth-view depth-msaa-view composite
                               history resolved motion motion-msaa
                               scene scene-msaa depth depth-msaa))
                 (when resource (ignore-errors (destroy resource))))))
      (unwind-protect
           (progn
             (when metalfx-p
               (setf scaler
                     (create
                      device
                      (make-temporal-scaler-descriptor
                       :label "luft MetalFX temporal scaler"
                       :input-size render-extent :output-size extent))))
             (setf depth
                   (create
                    device
                    (make-texture-descriptor
                     :label "luft temporal depth" :size render-extent
                     :dimensions :2d :format :depth32-float
                     :usage
                     (usage '(:render-attachment :texture-binding)
                            (and scaler
                                 (gpu-temporal-scaler-depth-usage scaler)))))
                   depth-view
                   (create
                    device (make-texture-view-descriptor :texture depth))
                   depth-msaa
                   (create
                    device
                    (make-texture-descriptor
                     :label "luft multisampled depth" :size render-extent
                     :dimensions :2d :format :depth32-float
                     :usage :render-attachment
                     :sample-count *scene-sample-count*))
                   depth-msaa-view
                   (create
                    device
                    (make-texture-view-descriptor :texture depth-msaa))
                   scene
                   (create
                    device
                    (make-texture-descriptor
                     :label "luft HDR color" :size render-extent
                     :dimensions :2d :format :rgba16-float
                     :usage
                     (usage '(:render-attachment :texture-binding)
                            (and scaler
                                 (gpu-temporal-scaler-color-usage scaler)))))
                   scene-view
                   (create
                    device (make-texture-view-descriptor :texture scene))
                   scene-msaa
                   (create
                    device
                    (make-texture-descriptor
                     :label "luft multisampled HDR color" :size render-extent
                     :dimensions :2d :format :rgba16-float
                     :usage :render-attachment
                     :sample-count *scene-sample-count*))
                   scene-msaa-view
                   (create
                    device
                    (make-texture-view-descriptor :texture scene-msaa)))
             (when temporal-p
               (setf motion
                     (create
                      device
                      (make-texture-descriptor
                       :label "luft temporal motion" :size render-extent
                       :dimensions :2d :format :rg16-float
                       :usage
                       (usage (if shader-temporal-p
                                  '(:render-attachment :texture-binding)
                                  '(:render-attachment))
                              (and scaler
                                   (gpu-temporal-scaler-motion-usage scaler)))))
                     motion-view
                     (create
                      device (make-texture-view-descriptor :texture motion))
                     motion-msaa
                     (create
                      device
                      (make-texture-descriptor
                       :label "luft multisampled temporal motion"
                       :size render-extent :dimensions :2d
                       :format :rg16-float :usage :render-attachment
                       :sample-count *scene-sample-count*))
                     motion-msaa-view
                     (create
                      device
                      (make-texture-view-descriptor :texture motion-msaa))
                     resolved
                     (create
                      device
                      (make-texture-descriptor
                       :label "luft temporal resolve" :size extent
                       :dimensions :2d :format :rgba16-float
                       :usage
                       (usage
                        (if shader-temporal-p
                            '(:render-attachment :texture-binding :copy-src)
                            '(:texture-binding))
                        (and scaler
                             (gpu-temporal-scaler-output-usage scaler)))))
                     resolved-view
                     (create
                      device (make-texture-view-descriptor :texture resolved)))
               (when shader-temporal-p
                 (setf history
                       (create
                        device
                        (make-texture-descriptor
                         :label "luft temporal history" :size extent
                         :dimensions :2d :format :rgba16-float
                         :usage '(:texture-binding :copy-dst)))
                       history-view
                       (create
                        device (make-texture-view-descriptor :texture history))
                       temporal-group
                       (create
                        device
                        (make-bind-group-descriptor
                         :label "luft temporal resolve inputs"
                         :layout (renderer-temporal-layout renderer)
                         :entries
                         `((:binding 0 :resource ,scene-view)
                           (:binding 1 :resource ,motion-view)
                           (:binding 2 :resource ,history-view)
                           (:binding 3 :resource ,(renderer-sampler renderer))
                           (:binding 4
                            :resource ,(renderer-camera-buffer renderer))))))))
             (setf composite
                   (create
                    device
                    (make-texture-descriptor
                     :label "luft post-temporal HDR composite" :size extent
                     :dimensions :2d :format :rgba16-float
                     :usage '(:render-attachment :texture-binding)))
                   composite-view
                   (create
                    device (make-texture-view-descriptor :texture composite)))
             (let ((base-view (or resolved-view scene-view)))
               (setf composite-source-group
                     (create
                      device
                      (make-bind-group-descriptor
                       :label "luft HDR composite source"
                       :layout (renderer-composite-layout renderer)
                       :entries
                       `((:binding 0 :resource ,base-view)
                         (:binding 1 :resource ,(renderer-sampler renderer)))))
                     flame-group
                     (make-renderer-target-flame-bind-group
                      renderer (renderer-flame-instance-buffer renderer)
                      depth-view)
                     present-group
                     (create
                      device
                      (make-bind-group-descriptor
                       :label "luft HDR presentation"
                       :layout (renderer-present-layout renderer)
                       :entries
                       `((:binding 0 :resource ,composite-view)
                         (:binding 1 :resource ,(renderer-sampler renderer))
                         (:binding 2 :resource ,depth-view)
                         (:binding 3
                          :resource ,(renderer-camera-buffer renderer)))))
                     exposure-group
                     (make-exposure-binding
                      (renderer-exposure-control renderer)
                      device composite-view (renderer-sampler renderer))))
             (setf resource-cohort
                   (%make-renderer-target-resources
                    :extent extent :render-extent render-extent
                    :temporal-scaler scaler
                    :depth-msaa-texture depth-msaa
                    :depth-msaa-view depth-msaa-view
                    :depth-texture depth :depth-view depth-view
                    :scene-msaa-texture scene-msaa
                    :scene-msaa-view scene-msaa-view
                    :scene-texture scene :scene-view scene-view
                    :motion-msaa-texture motion-msaa
                    :motion-msaa-view motion-msaa-view
                    :motion-texture motion :motion-view motion-view
                    :resolved-texture resolved :resolved-view resolved-view
                    :history-texture history :history-view history-view
                    :temporal-bind-group temporal-group
                    :composite-texture composite :composite-view composite-view
                    :composite-source-bind-group composite-source-group
                    :present-bind-group present-group
                    :exposure-binding exposure-group)
                   flame-join
                   (%make-renderer-flame-target-join flame-group)
                   generation
                   (%make-renderer-target-generation resource-cohort flame-join)
                   completed-p t)
             generation)
        (unless completed-p
          ;; A CPU condition can occur after the final GPU create but before
          ;; the immutable record exists.  Locals retain ownership until then.
          (if generation
              (destroy-renderer-target-generation generation)
              (cleanup-locals)))))))

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
           (destroy-renderer-target-generation old-generation)
           candidate)
      (unless installed-p
        (when candidate
          (destroy-renderer-target-generation candidate))))))

(defun create-frame-targets (renderer extent)
  "Compatibility entry point for initial target publication."
  (replace-renderer-target-generation renderer extent)
  renderer)

(defun ensure-renderer-extent (renderer extent)
  (unless (equal extent (renderer-extent renderer))
    (replace-renderer-target-generation renderer extent))
  renderer)

(zdefun (mesh-lattice-point-words :zone :luft/prepare-overlay) (mesh)
  "LUFT vertex sites, mesh vertices, and eighth-step boundary-edge samples."
  (let ((points (make-hash-table :test #'eql))
        (result (make-array 64 :element-type '(unsigned-byte 32)
                              :adjustable t :fill-pointer 0))
        (templates (luft:surface-mesh-template-vertex-words mesh))
        (ranges (luft:surface-mesh-template-ranges mesh)))
    (labels ((pack-point (x y z)
               ;; World coordinates are non-negative and comfortably below
               ;; twenty bits at the eighth-cell scale. One fixnum is a
               ;; cons-free hash key for the diagnostic point vocabulary.
               (unless (and (typep x '(unsigned-byte 20))
                            (typep y '(unsigned-byte 20))
                            (typep z '(unsigned-byte 20)))
                 (error "LUFT lattice point (~D ~D ~D) exceeds packed range."
                        x y z))
               (logior x (ash y 20) (ash z 40)))
             (remember (x y z marker-kind)
               (let ((key (pack-point x y z)))
                 (setf (gethash key points)
                       (max marker-kind (gethash key points 0)))))
             (template-coordinate (base vertex axis)
               (+ (* luft:+mesh-cell-size+ base)
                  (- (aref templates
                           (+ (* vertex luft:+mesh-template-vertex-word-count+)
                              axis))
                     luft:+mesh-template-coordinate-bias+)))
             (sample-axis-edge (ax ay az bx by bz)
               (cond
                 ((and (= ay by) (= az bz) (/= ax bx))
                  (loop for x from (min ax bx) to (max ax bx)
                        do (remember x ay az 0)))
                 ((and (= ax bx) (= az bz) (/= ay by))
                  (loop for y from (min ay by) to (max ay by)
                        do (remember ax y az 0)))
                 ((and (= ax bx) (= ay by) (/= az bz))
                  (loop for z from (min az bz) to (max az bz)
                        do (remember ax ay z 0)))))
             (visit-stream (words fan-p)
               (loop for instance-offset from 0 below (length words) by 4
                     for base-x = (aref words instance-offset)
                     for base-y = (aref words (+ instance-offset 1))
                     for base-z = (aref words (+ instance-offset 2))
                     for packed = (aref words (+ instance-offset 3))
                     for template-id = (ldb (byte 16 0) packed)
                     for vertex-start = (aref ranges (* 2 template-id))
                     for vertex-count = (aref ranges (1+ (* 2 template-id)))
                     do (when fan-p
                          (remember (* luft:+mesh-cell-size+ base-x)
                                    (* luft:+mesh-cell-size+ base-y)
                                    (* luft:+mesh-cell-size+ base-z) 2))
                        (loop for vertex from vertex-start
                                below (+ vertex-start vertex-count)
                              do (remember
                                  (template-coordinate base-x vertex 0)
                                  (template-coordinate base-y vertex 1)
                                  (template-coordinate base-z vertex 2) 1))
                        (loop for vertex from vertex-start
                                below (+ vertex-start vertex-count) by 3
                              for attributes =
                                (aref templates
                                      (+ (* vertex 4) 3))
                              for edge-mask = (ldb (byte 3 10) attributes)
                              for ax = (template-coordinate base-x vertex 0)
                              for ay = (template-coordinate base-y vertex 1)
                              for az = (template-coordinate base-z vertex 2)
                              for bx = (template-coordinate base-x (1+ vertex) 0)
                              for by = (template-coordinate base-y (1+ vertex) 1)
                              for bz = (template-coordinate base-z (1+ vertex) 2)
                              for cx = (template-coordinate base-x (+ vertex 2) 0)
                              for cy = (template-coordinate base-y (+ vertex 2) 1)
                              for cz = (template-coordinate base-z (+ vertex 2) 2)
                              when (logbitp 0 edge-mask)
                                do (sample-axis-edge bx by bz cx cy cz)
                              when (logbitp 1 edge-mask)
                                do (sample-axis-edge ax ay az cx cy cz)
                              when (logbitp 2 edge-mask)
                                do (sample-axis-edge ax ay az bx by bz)))))
      (visit-stream (luft:surface-mesh-face-instance-words mesh) nil)
      (visit-stream (luft:surface-mesh-band-instance-words mesh) nil)
      (visit-stream (luft:surface-mesh-fan-instance-words mesh) t))
    (maphash
     (lambda (point marker-kind)
       (vector-push-extend (ldb (byte 20 0) point) result)
       (vector-push-extend (ldb (byte 20 20) point) result)
       (vector-push-extend (ldb (byte 20 40) point) result)
       (vector-push-extend marker-kind result))
     points)
    (coerce result '(simple-array (unsigned-byte 32) (*)))))

(defun %destroy-mesh-slot (slot)
  (%destroy-resident-population (mesh-slot-resident slot))
  (dolist (resource (list (mesh-slot-lattice-point-group slot)
                          (mesh-slot-lattice-point-buffer slot)))
    (when resource (ignore-errors (destroy resource))))
  (values))

(defun mesh-slot-prepared-mesh (slot)
  "Borrow SLOT's immutable CPU realization for renderer reconstruction."
  (%make-prepared-render-mesh
   (mesh-slot-mesh slot)
   (resident-population-population (mesh-slot-resident slot))))

(defun %make-renderer-mesh-slot (renderer mesh-or-prepared provenance)
  "Upload one independently retained chunk slot.

MESH-OR-PREPARED may carry worker-built dense population arrays. Construction
overlay data is deliberately absent until construction mode asks for it."
  (let* ((prepared
           (if (typep mesh-or-prepared 'prepared-render-mesh)
               mesh-or-prepared
               (prepare-render-mesh mesh-or-prepared)))
         (mesh (prepared-render-mesh-mesh prepared))
         (slot (%make-mesh-slot :mesh mesh :provenance provenance))
         (completed-p nil))
    (unwind-protect
         (progn
           (setf (mesh-slot-resident slot)
                 (%upload-render-population
                  renderer (prepared-render-mesh-population prepared)))
           (setf completed-p t)
           slot)
      (unless completed-p
        (%destroy-mesh-slot slot)))))

(defun ensure-mesh-slot-lattice-points (renderer slot)
  "Create SLOT's diagnostic overlay on first use, never during normal streaming."
  (unless (mesh-slot-lattice-point-buffer slot)
    (let* ((device (renderer-device renderer))
           (camera-buffer (renderer-camera-buffer renderer))
           (lattice-point-words
             (mesh-lattice-point-words (mesh-slot-mesh slot)))
           (lattice-point-count (/ (length lattice-point-words) 4))
           (completed-p nil))
      (flet ((stream-buffer (label words)
               (let ((buffer (create device
                                     (make-buffer-descriptor
                                      :label label
                                      :size (max 16 (* 4 (length words)))
                                      :usage '(:storage :copy-dst)))))
                 (when (plusp (length words))
                   (write-buffer buffer words))
                 buffer)))
        (unwind-protect
             (progn
               (setf (mesh-slot-lattice-point-count slot) lattice-point-count
                     (mesh-slot-lattice-point-buffer slot)
                     (stream-buffer "luft unique eighth-cell lattice points"
                                    lattice-point-words))
               (setf (mesh-slot-lattice-point-group slot)
                     (create device
                             (make-bind-group-descriptor
                              :label "luft eighth-cell lattice points"
                              :layout (renderer-lattice-point-layout renderer)
                              :entries
                              `((:binding 0
                                 :resource ,(mesh-slot-lattice-point-buffer
                                             slot))
                                (:binding 1 :resource ,camera-buffer)))))
               (setf completed-p t))
          (unless completed-p
            (dolist (resource (list (mesh-slot-lattice-point-group slot)
                                    (mesh-slot-lattice-point-buffer slot)))
              (when resource (ignore-errors (destroy resource))))
            (setf (mesh-slot-lattice-point-group slot) nil
                  (mesh-slot-lattice-point-buffer slot) nil
                  (mesh-slot-lattice-point-count slot) 0))))))
  slot)

(defun %destroy-resident-population (resident)
  (when resident
    (dolist (resource (list (resident-population-bind-group resident)
                            (resident-population-shadow-bind-group resident)
                            (resident-population-descriptor-buffer resident)
                            (resident-population-appearance-buffer resident)
                            (resident-population-instance-buffer resident)))
      (when resource (ignore-errors (destroy resource)))))
  (values))

(zdefun (%upload-render-population :zone :luft/upload-slot)
    (renderer population)
  "Build and upload one candidate population without changing RENDERER."
  (let* ((device (renderer-device renderer))
         (instance-words (render-population-instance-words population))
         (appearance-words (render-population-appearance-words population))
         (descriptor-words (render-population-descriptor-words population))
         instance-buffer template-buffer appearance-buffer descriptor-buffer bind-group
         shadow-bind-group
         (completed-p nil))
    (flet ((stream-buffer (label words)
             (let ((buffer
                     (create device
                             (make-buffer-descriptor
                              :label label
                              :size (max 16 (* 4 (length words)))
                              :usage '(:storage :copy-dst)))))
               (when (plusp (length words))
                 (write-buffer buffer words))
               buffer)))
      (unwind-protect
           (progn
             (setf instance-buffer
                   (stream-buffer "luft resident site instances" instance-words)
                   template-buffer
                   (renderer-star-template-buffer renderer)
                   appearance-buffer
                   (stream-buffer "luft active-star appearance sidecars"
                                  appearance-words)
                   descriptor-buffer
                   (stream-buffer "luft terrain material descriptors"
                                  descriptor-words)
                   bind-group
                   (create device
                           (make-bind-group-descriptor
                            :label "luft resident site population"
                            :layout (renderer-layout renderer)
                            :entries
                            `((:binding 0 :resource ,instance-buffer)
                              (:binding 1 :resource ,template-buffer)
                              (:binding 2
                               :resource ,(renderer-camera-buffer renderer))
                              (:binding 3 :resource ,appearance-buffer)
                              (:binding 4
                               :resource ,(renderer-shadow-view renderer))
                              (:binding 5
                               :resource ,(renderer-shadow-sampler renderer))
                              (:binding 6 :resource ,descriptor-buffer))))
                   shadow-bind-group
                   (create device
                           (make-bind-group-descriptor
                            :label "luft resident shadow population"
                            :layout (renderer-shadow-layout renderer)
                            :entries
                            `((:binding 0 :resource ,instance-buffer)
                              (:binding 1 :resource ,template-buffer)
                              (:binding 2
                               :resource ,(renderer-camera-buffer renderer))))))
             (let ((resident
                     (%make-resident-population
                      population instance-buffer template-buffer appearance-buffer
                      descriptor-buffer bind-group shadow-bind-group)))
               (setf completed-p t)
               resident))
        (unless completed-p
          (dolist (resource
                    (list shadow-bind-group bind-group descriptor-buffer
                          appearance-buffer instance-buffer))
            (when resource (ignore-errors (destroy resource)))))))))

(defun renderer-set-mesh (renderer key mesh &key scene-generation)
  "Make MESH resident under KEY, replacing any previous resident mesh."
  (cdar (renderer-set-meshes
         renderer (list (cons key mesh))
         :scene-generation scene-generation)))

(defun renderer-set-meshes (renderer meshes &key scene-generation)
  "Transactionally replace the keyed MESHES as one visible residency cohort.

MESHES is an alist of key to surface mesh. Every GPU slot is created before
the renderer's table changes; a failed upload therefore leaves the installed
cohort untouched. No frame can interleave with the owner-thread publication."
  (renderer-update-meshes
   renderer meshes nil :scene-generation scene-generation))

(defun prospective-renderer-slot-entries (renderer candidates removed-keys)
  "Return the sorted slot population after applying one candidate cohort."
  (let ((entries nil))
    (loop for key being the hash-keys of (renderer-mesh-slots renderer)
          for slot = (gethash key (renderer-mesh-slots renderer))
          unless (or (assoc key candidates :test #'eql)
                     (member key removed-keys :test #'eql))
            do (push (cons key slot) entries))
    (dolist (entry candidates)
      (push entry entries))
    (sort entries #'< :key #'car)))

(defun renderer-slot-table (entries)
  "Materialize sorted key-to-slot ENTRIES into a fresh publication table."
  (let ((table (make-hash-table :test #'eql :size (length entries))))
    (dolist (entry entries table)
      (setf (gethash (car entry) table) (cdr entry)))))

(defun validate-renderer-mesh-update (meshes removed-keys)
  "Validate one keyed residency transaction before any GPU allocation."
  (check-type meshes list)
  (check-type removed-keys list)
  (let ((candidate-keys (make-hash-table :test #'eql))
        (removal-keys (make-hash-table :test #'eql)))
    (dolist (entry meshes)
      (unless (consp entry)
        (error "A renderer mesh cohort entry must be (KEY . MESH), not ~S."
               entry))
      (let ((key (car entry))
            (mesh (cdr entry)))
        (check-type key luft:chunk-key)
        (unless (or (typep mesh 'luft:surface-mesh)
                    (typep mesh 'prepared-render-mesh))
          (error "Renderer owner ~D has invalid mesh value ~S." key mesh))
        (when (nth-value 1 (gethash key candidate-keys))
          (error "Renderer mesh cohort repeats owner ~D." key))
        (setf (gethash key candidate-keys) t)))
    (dolist (key removed-keys)
      (check-type key luft:chunk-key)
      (when (nth-value 1 (gethash key removal-keys))
        (error "Renderer removal cohort repeats owner ~D." key))
      (when (nth-value 1 (gethash key candidate-keys))
        (error "Renderer owner ~D cannot be replaced and removed together."
               key))
      (setf (gethash key removal-keys) t)))
  (values))

(defun prepare-renderer-mesh-candidates (meshes)
  "Prepare MESHES into immutable CPU populations without allocating on the GPU."
  (loop for (key . mesh) in meshes
        for prepared = (if (typep mesh 'prepared-render-mesh)
                           mesh
                           (prepare-render-mesh mesh))
        collect (cons key prepared)))

(defun surface-mesh-tree-uses-light-field-p (mesh field)
  "Whether MESH and every companion retain exact immutable FIELD."
  (and (eq field (luft:surface-mesh-voxel-light mesh))
       (every (lambda (companion)
                (surface-mesh-tree-uses-light-field-p companion field))
              (luft:surface-mesh-companions mesh))))

(defun make-renderer-slot-provenance (key generation manifest-index)
  "Project GENERATION's matched output into one nonrecursive slot witness."
  (let* ((manifest
           (scene-mesh-generation-mesh-manifest generation))
         (entry (aref manifest manifest-index))
         (merged
           (scene-mesh-generation-slot-provenances generation)))
    (if (plusp (length merged))
        (let ((provenance (aref merged manifest-index)))
          (unless (and (eql key (renderer-slot-provenance-key provenance))
                       (eq (scene-mesh-generation-scene generation)
                           (renderer-slot-provenance-scene provenance)))
            (error "Merged scene-generation provenance does not name renderer owner ~S."
                   key))
          provenance)
        (%make-renderer-slot-provenance
         key (scene-mesh-generation-scene generation)
         (copy-scene-generation-stamp-value
          (scene-mesh-generation-request-stamp generation))
         (copy-scene-generation-stamp-value
          (scene-mesh-generation-result-stamp generation))
         (scene-mesh-generation-light-generation generation)
         (scene-mesh-output-manifest-entry-tree entry)))))

(defun consume-renderer-generation-manifest
    (prepared-meshes generation)
  "Match every candidate key/tree one-to-one against GENERATION's manifest."
  (let* ((manifest (scene-mesh-generation-mesh-manifest generation))
         (used (make-array (length manifest) :element-type 'bit
                                             :initial-element 0)))
    (unless (= (length prepared-meshes) (length manifest))
      (error "Renderer candidate count ~D does not match generation output count ~D."
             (length prepared-meshes) (length manifest)))
    (loop for (key . prepared) in prepared-meshes
          for mesh = (prepared-render-mesh-mesh prepared)
          for index =
            (loop for index below (length manifest)
                  for entry = (aref manifest index)
                  when (and (zerop (aref used index))
                            (or (not
                                 (scene-mesh-output-manifest-entry-keyed-p
                                  entry))
                                (eql key
                                     (scene-mesh-output-manifest-entry-key
                                      entry)))
                            (surface-mesh-tree-manifest-matches-p
                             (scene-mesh-output-manifest-entry-tree entry)
                             mesh))
                    return index)
          unless index
            do (error "Renderer owner ~S is not the exact mesh tree named by its scene generation."
                      key)
          do (setf (aref used index) 1)
          collect (cons key
                        (make-renderer-slot-provenance
                         key generation index)))))

(defun prospective-renderer-semantic-entries
    (renderer prepared-meshes removed-keys candidate-provenances)
  "Return sorted (KEY MESH PROVENANCE) tuples before any GPU allocation."
  (let ((entries nil))
    (loop for key being the hash-keys of (renderer-mesh-slots renderer)
          using (hash-value slot)
          unless (or (assoc key prepared-meshes :test #'eql)
                     (member key removed-keys :test #'eql))
            do (push (list key (mesh-slot-mesh slot)
                           (and
                            (renderer-publication-scene-generation
                             (renderer-publication renderer))
                            (mesh-slot-provenance slot)))
                     entries))
    (dolist (entry prepared-meshes)
      (push
       (list (car entry)
             (prepared-render-mesh-mesh (cdr entry))
             (cdr (assoc (car entry) candidate-provenances :test #'eql)))
       entries))
    (sort entries #'< :key #'car)))

(defun scene-generation-exactly-manifests-semantic-entries-p
    (generation entries)
  "Whether a previously merged GENERATION already names all keyed ENTRIES."
  (let* ((manifest (scene-mesh-generation-mesh-manifest generation))
         (provenances (scene-mesh-generation-slot-provenances generation))
         (used (make-array (length manifest) :element-type 'bit
                                             :initial-element 0)))
    (and (= (length entries) (length manifest) (length provenances))
         (every
          (lambda (semantic)
            (let ((key (first semantic))
                  (mesh (second semantic))
                  (provenance (third semantic)))
              (loop for index below (length manifest)
                    for entry = (aref manifest index)
                    when (and (zerop (aref used index))
                              (scene-mesh-output-manifest-entry-keyed-p entry)
                              (eql key
                                   (scene-mesh-output-manifest-entry-key entry))
                              (eq provenance (aref provenances index))
                              (surface-mesh-tree-manifest-matches-p
                               (scene-mesh-output-manifest-entry-tree entry)
                               mesh))
                      do (setf (aref used index) 1)
                         (return t)
                    finally (return nil))))
          entries))))

(defun make-renderer-publication-scene-generation (generation entries)
  "Merge the current transaction stamp with exact prospective slot lineage."
  (when generation
    (if (scene-generation-exactly-manifests-semantic-entries-p
         generation entries)
        generation
        (make-scene-mesh-generation-value
         (scene-mesh-generation-scene generation)
         (scene-mesh-generation-request-stamp generation)
         (scene-mesh-generation-light-generation generation)
         :mesh-entries
         (mapcar (lambda (entry) (cons (first entry) (second entry))) entries)
         :slot-provenances (map 'vector #'third entries)))))

(defun validate-renderer-scene-generation-cohort
    (renderer prepared-meshes removed-keys generation)
  "Validate and return prospective semantic entries before GPU allocation."
  (when generation
    (let* ((light-generation
             (scene-mesh-generation-light-generation generation))
           (field (realized-light-generation-field light-generation))
           (candidate-provenances
             (consume-renderer-generation-manifest
              prepared-meshes generation))
           (entries
             (prospective-renderer-semantic-entries
              renderer prepared-meshes removed-keys candidate-provenances)))
      (dolist (entry entries)
        (destructuring-bind (key mesh provenance) entry
          (unless provenance
            (error "Retained renderer owner ~S has no exact scene-generation provenance."
                   key))
          (unless (and
                   (eq (scene-mesh-generation-scene generation)
                       (renderer-slot-provenance-scene provenance))
                   (eq light-generation
                       (renderer-slot-provenance-light-generation provenance))
                   (surface-mesh-tree-uses-light-field-p mesh field)
                   (surface-mesh-tree-manifest-matches-p
                    (renderer-slot-provenance-tree provenance) mesh))
            (error "Renderer owner ~S does not retain its exact claimed mesh/light generation."
                   key))))
      entries)))

(defvar *renderer-publication-precommit-hook* nil
  "Optional test instrumentation run after complete staging, before commit.")

(defun renderer-publication-retired-slots (old-publication new-table)
  (let ((retired nil))
    (loop for key being the hash-keys of
          (renderer-publication-mesh-slots old-publication)
          using (hash-value old-slot)
          unless (eq old-slot (gethash key new-table))
            do (push old-slot retired))
    retired))

(defun destroy-renderer-publication-resources (publication)
  "Retire only PUBLICATION's global resources; mesh slots have separate sharing."
  (dolist (resource
            (list (renderer-publication-torch-body-shadow-bind-group publication)
                  (renderer-publication-torch-body-bind-group publication)
                  (renderer-publication-flame-instance-buffer publication)))
    (when resource (ignore-errors (destroy resource))))
  (values))

(zdefun (renderer-update-meshes :zone :luft/publish-residency)
    (renderer meshes removed-keys &key scene-generation)
  "Transactionally replace geometry and its realized torch-frame cohort."
  (validate-renderer-mesh-update meshes removed-keys)
  (when scene-generation
    (check-type scene-generation scene-mesh-generation))
  (let ((prepared-meshes nil)
        (semantic-entries nil)
        (publication-scene-generation nil)
        (candidates nil)
        (requested-candidates nil)
        (old-publication (renderer-publication renderer))
        (old-target-generation (renderer-target-generation renderer))
        (staged-publication nil)
        (staged-target-generation nil)
        (staged-target-flame-group nil)
        (publication-changed-p
          (or meshes
              (some (lambda (key)
                      (nth-value
                       1 (gethash key (renderer-mesh-slots renderer))))
                    removed-keys)))
        (torch-frame-data nil)
        (flame-count 0)
        (flame-buffer nil)
        (body-group nil)
        (body-shadow-group nil)
        (installed-p nil))
    (unless publication-changed-p
      (return-from renderer-update-meshes nil))
    (unwind-protect
         (progn
           ;; CPU canonicalization finishes before candidate GPU allocation.
           (setf prepared-meshes
                 (prepare-renderer-mesh-candidates meshes))
           (when scene-generation
             (setf semantic-entries
                   (validate-renderer-scene-generation-cohort
                    renderer prepared-meshes removed-keys scene-generation)
                   publication-scene-generation
                   (make-renderer-publication-scene-generation
                    scene-generation semantic-entries)))
           (dolist (entry prepared-meshes)
             (push (cons (car entry)
                         (%make-renderer-mesh-slot
                          renderer (cdr entry)
                          (and scene-generation
                               (third
                                (find (car entry) semantic-entries
                                      :key #'first :test #'eql)))))
                   candidates))
           (setf candidates (nreverse candidates))
           (setf requested-candidates
                 (mapcar
                  (lambda (entry)
                    (cons (car entry)
                          (cdr (assoc (car entry) candidates :test #'eql))))
                  prepared-meshes))
           (let* ((entries
                    (prospective-renderer-slot-entries
                     renderer candidates removed-keys))
                  (table (renderer-slot-table entries))
                  (order (mapcar #'car entries)))
             ;; Every fallible GPU and CPU staging operation finishes before
             ;; any part of this generation becomes visible.
             (multiple-value-setq
                 (torch-frame-data flame-count flame-buffer
                  body-group body-shadow-group)
               (%make-renderer-flame-resources
                renderer (mesh-slots-torch-frame-data entries)))
             (setf staged-publication
                   (%make-renderer-publication
                    table order torch-frame-data flame-count flame-buffer
                    body-group body-shadow-group publication-scene-generation))
             (multiple-value-setq
                 (staged-target-generation staged-target-flame-group)
               (make-retargeted-renderer-target-generation
                renderer flame-buffer))
             (when *renderer-publication-precommit-hook*
               (funcall *renderer-publication-precommit-hook*
                        renderer staged-publication))
             ;; Frames execute only on this owner thread.  These adjacent
             ;; pointer writes therefore publish the semantic residency and
             ;; its target-coupled flame/depth join as one noninterleavable
             ;; renderer transaction; both candidates are complete already.
             (setf (renderer-publication renderer) staged-publication
                   (renderer-target-generation renderer)
                   staged-target-generation
                   installed-p t)
             (with-release-warnings
               (releasing :superseded-frame-bindings
                 (clear-renderer-frame-bind-groups renderer)))
             ;; Residency changes invalidate the previous color/depth/motion
             ;; correspondence.  The next temporal encode must reset rather
             ;; than blend arrivals with history in which they did not exist,
             ;; or retain departed silhouettes as ghosts.
             (setf (renderer-history-valid-p renderer) nil)
             (dolist (slot
                       (renderer-publication-retired-slots
                        old-publication table))
               (%destroy-mesh-slot slot))
             (unless (eq staged-target-generation old-target-generation)
               (destroy-renderer-flame-target-join
                (renderer-target-generation-flame-join
                 old-target-generation)))
             (destroy-renderer-publication-resources old-publication))
           requested-candidates)
      (unless installed-p
        (when staged-target-flame-group
          (ignore-errors (destroy staged-target-flame-group)))
        (when staged-publication
          (destroy-renderer-publication-resources staged-publication))
        ;; A condition may occur after GPU staging but before the publication
        ;; record itself is allocated.  Those locals still own the resources.
        (unless staged-publication
          (when body-shadow-group (ignore-errors (destroy body-shadow-group)))
          (when body-group (ignore-errors (destroy body-group)))
          (when flame-buffer (ignore-errors (destroy flame-buffer))))
        (dolist (entry candidates) (%destroy-mesh-slot (cdr entry)))))))

(defun renderer-remove-mesh (renderer key)
  (renderer-update-meshes renderer nil (list key))
  (values))

(defun renderer-clear-meshes (renderer)
  (renderer-update-meshes renderer nil (copy-list (renderer-slot-order renderer)))
  (values))

(defun make-renderer (device color-format extent
                      &key (exposure-factory 'make-automatic-exposure))
  "Compose the GPU renderer, including a separately owned exposure control.

EXPOSURE-FACTORY is a function designator receiving DEVICE and returning a
fresh EXPOSURE-CONTROL. Its default names a function so live redefinition is
observed by the next rebuild. The renderer owns that value and calls its
interface; target generations own only the bindings to their HDR images.
Shader refresh preserves the factory.
Meshes arrive separately through RENDERER-SET-MESH."
  (let* ((temporal-kind (temporal-resolve-kind device))
         (temporal-p (not (null temporal-kind)))
         (target-formats (if temporal-p
                             '(:rgba16-float :rg16-float)
                             '(:rgba16-float)))
         camera-buffer star-template-buffer
         layout
         vertex-module fragment-module pipeline
         shadow-texture shadow-view shadow-sampler shadow-layout
         shadow-vertex-module shadow-pipeline
         player-sdf-layout player-sdf-bind-group player-sdf-vertex-module
         player-sdf-fragment-module player-sdf-pipeline
         flame-layout flame-instance-buffer flame-effect-buffer
         flame-vertex-module flame-fragment-module flame-pipeline
         flame-depth-sampler composite-layout composite-fragment-module
         composite-pipeline
         torch-body-layout torch-body-vertex-buffer torch-body-bind-group
         torch-body-shadow-bind-group torch-body-vertex-module
         torch-body-shadow-vertex-module torch-body-fragment-module
         torch-body-pipeline
         torch-body-shadow-pipeline
         lattice-point-layout lattice-point-vertex-module
         lattice-point-fragment-module lattice-point-pipeline
         present-layout present-bind-group present-vertex-module
         present-fragment-module present-pipeline sampler
         temporal-layout temporal-fragment-module temporal-pipeline
         sky-layout sky-bind-group sky-fragment-module sky-pipeline
         exposure-control
         renderer
         (completed-p nil))
    (unwind-protect
         (progn
           (setf camera-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft frame state"
                          :size (shaders::scene-uniform-byte-size)
                          :usage '(:uniform :copy-dst)))
                 flame-effect-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft torch flame effect"
                          :size (torch-flame-effect-byte-size)
                          :usage '(:uniform :copy-dst))))
           ;; Publish ownership to the constructor unwind list before the
           ;; first fallible upload touches this resource.
           (write-buffer flame-effect-buffer
                         (torch-flame-effect-uniform-data 0.0))
           (setf flame-instance-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft empty torch flame instances"
                          :size 16 :usage '(:storage :copy-dst)))
                 star-template-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft 256 star meshlets"
                          :size (* 4 256 +star-meshlet-record-count+ 4)
                          :usage '(:storage :copy-dst)))
                 shadow-texture
                 (create device
                         (make-texture-descriptor
                          :label "luft sun shadow depth"
                          :size (list +shadow-map-size+ +shadow-map-size+)
                          :dimensions :2d :format :depth32-float
                          :usage '(:render-attachment :texture-binding)))
                 shadow-view
                 (create device
                         (make-texture-view-descriptor :texture shadow-texture))
                 shadow-sampler
                 (create device
                         (make-sampler-descriptor
                          :label "luft soft shadow comparison sampler"
                          :mag-filter :linear :min-filter :linear
                          :mipmap-filter :nearest :compare :less-or-equal)))
           (write-buffer star-template-buffer (star-meshlet-template-words))
           (let ((body-vertices (torch-body-vertex-data)))
             (setf torch-body-vertex-buffer
                   (create device
                           (make-buffer-descriptor
                            :label "luft canonical framed torch body"
                            :size (* 4 (length body-vertices))
                            :usage '(:storage :copy-dst))))
             (write-buffer torch-body-vertex-buffer body-vertices))
           (setf layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft mesh layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :storage-buffer)
                                     (:binding 2 :type :uniform-buffer)
                                     (:binding 3 :type :storage-buffer)
                                     (:binding 4 :type :texture)
                                     (:binding 5 :type :sampler)
                                     (:binding 6 :type :storage-buffer))))
                 shadow-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft shadow layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :storage-buffer)
                                     (:binding 2 :type :uniform-buffer))))
                 vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft mesh vertex"
                          :language :mathematical
                          :code (shaders:mesh-vertex-specification)))
                 fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft mesh fragment" :language :mathematical
                          :code (shaders:star-fragment-specification)))
                 pipeline
                 (create device
                         (make-mesh-render-pipeline-descriptor
                          :label "luft site stream pipeline" :layout layout
                          :task nil :mesh `(:module ,vertex-module)
                          :fragment `(:module ,fragment-module
                                      :targets
                                      ,(mapcar (lambda (format)
                                                 `(:format ,format))
                                               target-formats))
                          :sample-count *scene-sample-count*
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled t
                            :depth-compare :less)))
                 shadow-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft shadow vertex"
                          :language :mathematical
                          :code (shaders:shadow-vertex-specification)))
                 shadow-pipeline
                 (create device
                         (make-mesh-render-pipeline-descriptor
                          :label "luft sun shadow pipeline"
                          :layout shadow-layout
                          :task nil :mesh `(:module ,shadow-vertex-module)
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled t
                            :depth-compare :less))))
           (setf torch-body-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft framed torch-body layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :storage-buffer)
                                     (:binding 2 :type :uniform-buffer)
                                     (:binding 4 :type :texture)
                                     (:binding 5 :type :sampler))))
                 torch-body-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft framed torch-body vertex"
                          :language :mathematical
                          :code
                          (shaders:torch-body-vertex-specification)))
                 torch-body-shadow-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft framed torch-body shadow vertex"
                          :language :mathematical
                          :code
                          (shaders:torch-body-shadow-vertex-specification)))
                 torch-body-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft framed torch-body fragment"
                          :language :mathematical
                          :code (shaders:torch-body-fragment-specification)))
                 torch-body-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft framed opaque torch bodies"
                          :layout torch-body-layout
                          :vertex `(:module ,torch-body-vertex-module)
                          :fragment
                          `(:module ,torch-body-fragment-module
                            :targets
                            ,(mapcar (lambda (format) `(:format ,format))
                                     target-formats))
                          :primitive '(:topology :triangle-list)
                          :sample-count *scene-sample-count*
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled t
                            :depth-compare :less)))
                 torch-body-shadow-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft framed torch-body shadows"
                          :layout shadow-layout
                          :vertex `(:module ,torch-body-shadow-vertex-module)
                          :primitive '(:topology :triangle-list)
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled t
                            :depth-compare :less)))
                 torch-body-bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft empty framed torch bodies"
                          :layout torch-body-layout
                          :entries
                          `((:binding 0 :resource ,flame-instance-buffer)
                            (:binding 1 :resource ,torch-body-vertex-buffer)
                            (:binding 2 :resource ,camera-buffer)
                            (:binding 4 :resource ,shadow-view)
                            (:binding 5 :resource ,shadow-sampler))))
                 torch-body-shadow-bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft empty framed torch-body shadows"
                          :layout shadow-layout
                          :entries
                          `((:binding 0 :resource ,flame-instance-buffer)
                            (:binding 1 :resource ,torch-body-vertex-buffer)
                            (:binding 2 :resource ,camera-buffer)))))
           (setf player-sdf-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft player sdf layout"
                          :entries '((:binding 0 :type :uniform-buffer)
                                     (:binding 1 :type :texture)
                                     (:binding 2 :type :sampler))))
                 player-sdf-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft player sdf vertex"
                          :language :mathematical
                          :code (shaders:player-sdf-vertex-specification)))
                 player-sdf-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft player sdf fragment"
                          :language :mathematical
                          :code (shaders:player-sdf-fragment-specification)))
                 player-sdf-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft walking player sdf pipeline"
                          :layout player-sdf-layout
                          :vertex `(:module ,player-sdf-vertex-module)
                          :fragment
                          `(:module ,player-sdf-fragment-module
                            :targets
                            ,(loop for format in target-formats
                                   for first = t then nil
                                   collect `(:format ,format
                                             ,@(when first
                                                 '(:blend :premultiplied-alpha)))))
                          :primitive '(:topology :triangle-list)
                          :sample-count *scene-sample-count*
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled nil
                            :depth-compare :less)))
                 player-sdf-bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft walking player sdf"
                          :layout player-sdf-layout
                          :entries
                          `((:binding 0 :resource ,camera-buffer)
                            (:binding 1 :resource ,shadow-view)
                            (:binding 2 :resource ,shadow-sampler)))))
           (setf flame-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft torch flame layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :uniform-buffer)
                                     (:binding 2 :type :uniform-buffer)
                                     (:binding 3 :type :texture)
                                     (:binding 4 :type :sampler))))
                 flame-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft torch flame vertex"
                          :language :mathematical
                          :code (shaders:torch-flame-vertex-specification)))
                 flame-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft torch flame fragment"
                          :language :mathematical
                          :code (shaders:torch-flame-fragment-specification)))
                 flame-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft volumetric torch flame pipeline"
                          :layout flame-layout
                          :vertex `(:module ,flame-vertex-module)
                          :fragment
                          `(:module ,flame-fragment-module
                            :targets
                            ((:format :rgba16-float
                              :blend :premultiplied-alpha)))
                          :primitive '(:topology :triangle-list))))
           (setf lattice-point-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft lattice point layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :uniform-buffer))))
                 lattice-point-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft lattice point vertex"
                          :language :mathematical
                          :code (shaders:lattice-point-vertex-specification)))
                 lattice-point-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft lattice point fragment"
                          :language :mathematical
                          :code (shaders:lattice-point-fragment-specification)))
                 lattice-point-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft eighth-cell lattice point pipeline"
                          :layout lattice-point-layout
                          :vertex `(:module ,lattice-point-vertex-module)
                          :fragment
                          `(:module ,lattice-point-fragment-module
                            :targets
                            ,(loop for format in target-formats
                                   for first = t then nil
                                   collect `(:format ,format
                                             ,@(when first
                                                 '(:blend :premultiplied-alpha)))))
                          :primitive '(:topology :triangle-list)
                          :sample-count *scene-sample-count*
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled nil
                            :depth-compare :less))))
           (setf sampler
                 (create device
                         (make-sampler-descriptor
                          :label "luft presentation sampler"
                          :mag-filter :linear :min-filter :linear))
                 flame-depth-sampler
                 (make-renderer-flame-depth-sampler device)
                 composite-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft HDR composite source layout"
                          :entries '((:binding 0 :type :texture)
                                     (:binding 1 :type :sampler))))
                 sky-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft HDR sky layout"
                          :entries '((:binding 0 :type :uniform-buffer))))
                 sky-bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft HDR sky"
                          :layout sky-layout
                          :entries `((:binding 0 :resource ,camera-buffer)))))
           (setf present-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft presentation layout"
                          :entries '((:binding 0 :type :texture)
                                     (:binding 1 :type :sampler)
                                     (:binding 2 :type :texture)
                                     (:binding 3 :type :uniform-buffer))))
                 present-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft presentation vertex"
                          :language :mathematical
                          :code (shaders:present-vertex-specification)))
                 present-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft presentation fragment"
                          :language :mathematical
                          :code (shaders:present-fragment-specification)))
                 present-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft HDR presentation pipeline"
                          :layout present-layout
                          :vertex `(:module ,present-vertex-module)
                          :fragment `(:module ,present-fragment-module
                                      :targets ((:format ,color-format)))
                          :primitive '(:topology :triangle-list)))
                 composite-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft HDR composite copy fragment"
                          :language :mathematical
                          :code
                          (shaders::torch-flame-composite-copy-fragment-specification)))
                 composite-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft post-temporal HDR composite copy"
                          :layout composite-layout
                          :vertex `(:module ,present-vertex-module)
                          :fragment `(:module ,composite-fragment-module
                                      :targets ((:format :rgba16-float)))
                          :primitive '(:topology :triangle-list)))
                 sky-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft HDR sky fragment"
                          :language :mathematical
                          :code (if temporal-p
                                    (shaders:sky-temporal-fragment-specification)
                                    (shaders:sky-fragment-specification))))
                 sky-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft HDR sky pipeline"
                          :layout sky-layout
                          :vertex `(:module ,present-vertex-module)
                          :fragment `(:module ,sky-fragment-module
                                      :targets
                                      ,(mapcar (lambda (format)
                                                 `(:format ,format))
                                               target-formats))
                          :primitive '(:topology :triangle-list)
                          :sample-count *scene-sample-count*
                          ;; The sky is drawn inside the scene pass, whose
                          ;; depth attachment geometry subsequently owns.
                          ;; Match that pass without touching its depth.
                          :depth-stencil
                          '(:format :depth32-float
                            :depth-write-enabled nil
                            :depth-compare :always))))
           (when (eq temporal-kind :shader)
             (setf temporal-layout
                   (create
                    device
                    (make-bind-group-layout-descriptor
                     :label "luft temporal resolve layout"
                     :entries '((:binding 0 :type :texture)
                                (:binding 1 :type :texture)
                                (:binding 2 :type :texture)
                                (:binding 3 :type :sampler)
                                (:binding 4 :type :uniform-buffer))))
                   temporal-fragment-module
                   (create
                    device
                    (make-shader-module-descriptor
                     :label "luft temporal resolve fragment"
                     :language :mathematical
                     :code (shaders:temporal-resolve-fragment-specification)))
                   temporal-pipeline
                   (create
                    device
                    (make-render-pipeline-descriptor
                     :label "luft temporal resolve pipeline"
                     :layout temporal-layout
                     :vertex `(:module ,present-vertex-module)
                     :fragment `(:module ,temporal-fragment-module
                                 :targets ((:format :rgba16-float)))
                     :primitive '(:topology :triangle-list)))))
           (setf exposure-control (funcall exposure-factory device))
           (check-type exposure-control exposure-control)
           (setf renderer
                 (make-instance 'renderer
                                :exposure-control exposure-control
                                :exposure-factory exposure-factory
                                :device device
                                :color-format color-format
                                :temporal-p temporal-p
                                :temporal-resolve-kind temporal-kind
                                :camera-buffer camera-buffer
                                :star-template-buffer star-template-buffer
                                :publication
                                (%make-empty-renderer-publication
                                 :flame-instance-buffer flame-instance-buffer
                                 :torch-body-bind-group torch-body-bind-group
                                 :torch-body-shadow-bind-group
                                 torch-body-shadow-bind-group)
                                :layout layout
                                :vertex-module vertex-module
                                :fragment-module fragment-module
                                :pipeline pipeline
                                :flame-effect-buffer flame-effect-buffer
                                :flame-layout flame-layout
                                :flame-vertex-module flame-vertex-module
                                :flame-fragment-module flame-fragment-module
                                :flame-pipeline flame-pipeline
                                :flame-depth-sampler flame-depth-sampler
                                :composite-layout composite-layout
                                :composite-fragment-module
                                composite-fragment-module
                                :composite-pipeline composite-pipeline
                                :torch-body-vertex-buffer
                                torch-body-vertex-buffer
                                :torch-body-layout torch-body-layout
                                :torch-body-vertex-module
                                torch-body-vertex-module
                                :torch-body-shadow-vertex-module
                                torch-body-shadow-vertex-module
                                :torch-body-fragment-module
                                torch-body-fragment-module
                                :torch-body-pipeline torch-body-pipeline
                                :torch-body-shadow-pipeline
                                torch-body-shadow-pipeline
                                :shadow-texture shadow-texture
                                :shadow-view shadow-view
                                :shadow-sampler shadow-sampler
                                :shadow-layout shadow-layout
                                :shadow-vertex-module shadow-vertex-module
                                :shadow-pipeline shadow-pipeline
                                :player-sdf-layout player-sdf-layout
                                :player-sdf-bind-group player-sdf-bind-group
                                :player-sdf-vertex-module player-sdf-vertex-module
                                :player-sdf-fragment-module
                                player-sdf-fragment-module
                                :player-sdf-pipeline player-sdf-pipeline
                                :lattice-point-layout lattice-point-layout
                                :lattice-point-vertex-module
                                lattice-point-vertex-module
                                :lattice-point-fragment-module
                                lattice-point-fragment-module
                                :lattice-point-pipeline lattice-point-pipeline))
           (setf (renderer-present-layout renderer) present-layout
                 (renderer-sampler renderer) sampler
                 (renderer-present-vertex-module renderer)
                 present-vertex-module
                 (renderer-present-fragment-module renderer)
                 present-fragment-module
                 (renderer-present-pipeline renderer) present-pipeline)
           (setf (renderer-temporal-layout renderer) temporal-layout
                 (renderer-temporal-fragment-module renderer)
                 temporal-fragment-module
                 (renderer-temporal-pipeline renderer) temporal-pipeline)
           (setf (renderer-sky-layout renderer) sky-layout
                 (renderer-sky-bind-group renderer) sky-bind-group
                 (renderer-sky-fragment-module renderer) sky-fragment-module
                 (renderer-sky-pipeline renderer) sky-pipeline)
           (create-frame-targets renderer extent)
           (setf completed-p t)
           renderer)
      (unless completed-p
        (if renderer
            (destroy-renderer renderer)
            (progn
              (when exposure-control
                (with-release-warnings
                  (releasing :exposure (release-exposure exposure-control))))
              (dolist (resource
                       (list temporal-pipeline temporal-fragment-module
                                        temporal-layout
                                        present-pipeline present-fragment-module
                                        composite-pipeline
                                        composite-fragment-module
                                        present-vertex-module sampler
                                        flame-depth-sampler composite-layout
                                        present-bind-group sky-pipeline
                                        sky-fragment-module sky-bind-group
                                        sky-layout
                                        present-layout lattice-point-pipeline
                                        lattice-point-fragment-module
                                        lattice-point-vertex-module
                                        lattice-point-layout
                                        player-sdf-bind-group player-sdf-pipeline
                                        player-sdf-fragment-module
                                        player-sdf-vertex-module player-sdf-layout
                                        torch-body-shadow-bind-group
                                        torch-body-bind-group
                                        torch-body-shadow-pipeline
                                        torch-body-pipeline
                                        torch-body-fragment-module
                                        torch-body-shadow-vertex-module
                                        torch-body-vertex-module
                                        torch-body-layout
                                        torch-body-vertex-buffer
                                        flame-pipeline
                                        flame-fragment-module flame-vertex-module
                                        flame-layout flame-effect-buffer
                                        flame-instance-buffer
                                        shadow-pipeline shadow-vertex-module
                                        shadow-layout shadow-sampler shadow-view
                                        shadow-texture
                                        pipeline fragment-module
                                        vertex-module layout star-template-buffer
                                        camera-buffer))
                (when resource (ignore-errors (destroy resource))))))))))

(defun draw-resident-opaque-population (pass resident bind-group)
  "Dispatch one direct mesh workgroup per active lattice site."
  (let* ((population (resident-population-population resident))
         (workgroup-count
           (render-population-mesh-workgroup-count population)))
    (when (plusp workgroup-count)
      (set-bind-group pass 0 bind-group)
      (draw-mesh-workgroups pass workgroup-count))))

(defun destroy-renderer (renderer)
  (with-release-report
    (destroy-canvas-frame-resource-cache
     (renderer-frame-resources renderer) #'destroy-renderer-frame-state)
    (destroy-renderer-targets renderer)
    (releasing :exposure
      (release-exposure (renderer-exposure-control renderer)))
    (loop for slot being the hash-values of (renderer-mesh-slots renderer)
          do (%destroy-mesh-slot slot))
    (destroy-renderer-publication-resources (renderer-publication renderer))
    (dolist (resource
              (list (renderer-sky-pipeline renderer)
                    (renderer-sky-fragment-module renderer)
                    (renderer-sky-bind-group renderer)
                    (renderer-sky-layout renderer)
                    (renderer-present-pipeline renderer)
                    (renderer-present-fragment-module renderer)
                    (renderer-temporal-pipeline renderer)
                    (renderer-temporal-fragment-module renderer)
                    (renderer-temporal-layout renderer)
                    (renderer-composite-pipeline renderer)
                    (renderer-composite-fragment-module renderer)
                    (renderer-present-vertex-module renderer)
                    (renderer-sampler renderer)
                    (renderer-flame-depth-sampler renderer)
                    (renderer-composite-layout renderer)
                    (renderer-present-layout renderer)
                    (renderer-lattice-point-pipeline renderer)
                    (renderer-lattice-point-fragment-module renderer)
                    (renderer-lattice-point-vertex-module renderer)
                    (renderer-lattice-point-layout renderer)
                    (renderer-torch-body-shadow-pipeline renderer)
                    (renderer-torch-body-pipeline renderer)
                    (renderer-torch-body-fragment-module renderer)
                    (renderer-torch-body-shadow-vertex-module renderer)
                    (renderer-torch-body-vertex-module renderer)
                    (renderer-torch-body-layout renderer)
                    (renderer-torch-body-vertex-buffer renderer)
                    (renderer-flame-pipeline renderer)
                    (renderer-flame-fragment-module renderer)
                    (renderer-flame-vertex-module renderer)
                    (renderer-flame-layout renderer)
                    (renderer-flame-effect-buffer renderer)
                    (renderer-player-sdf-bind-group renderer)
                    (renderer-player-sdf-pipeline renderer)
                    (renderer-player-sdf-fragment-module renderer)
                    (renderer-player-sdf-vertex-module renderer)
                    (renderer-player-sdf-layout renderer)
                    (renderer-shadow-pipeline renderer)
                    (renderer-shadow-vertex-module renderer)
                    (renderer-shadow-layout renderer)
                    (renderer-shadow-sampler renderer)
                    (renderer-shadow-view renderer)
                    (renderer-shadow-texture renderer)
                    (renderer-star-template-buffer renderer)
                    (renderer-pipeline renderer) (renderer-fragment-module renderer)
                    (renderer-vertex-module renderer)
                    (renderer-layout renderer)
                    (and (slot-boundp renderer 'camera-buffer)
                         (renderer-camera-buffer renderer))))
      (when resource (ignore-errors (destroy resource))))
    (setf (renderer-present-pipeline renderer) nil
          (renderer-sky-pipeline renderer) nil
          (renderer-sky-fragment-module renderer) nil
          (renderer-sky-bind-group renderer) nil
          (renderer-sky-layout renderer) nil
          (renderer-present-fragment-module renderer) nil
          (renderer-temporal-pipeline renderer) nil
          (renderer-temporal-fragment-module renderer) nil
          (renderer-temporal-layout renderer) nil
          (renderer-composite-pipeline renderer) nil
          (renderer-composite-fragment-module renderer) nil
          (renderer-present-vertex-module renderer) nil
          (renderer-sampler renderer) nil
          (renderer-flame-depth-sampler renderer) nil
          (renderer-composite-layout renderer) nil
          (renderer-present-layout renderer) nil
          (renderer-lattice-point-pipeline renderer) nil
          (renderer-lattice-point-fragment-module renderer) nil
          (renderer-lattice-point-vertex-module renderer) nil
          (renderer-lattice-point-layout renderer) nil
          (renderer-publication renderer) (%make-empty-renderer-publication)
          (renderer-torch-body-shadow-pipeline renderer) nil
          (renderer-torch-body-pipeline renderer) nil
          (renderer-torch-body-fragment-module renderer) nil
          (renderer-torch-body-shadow-vertex-module renderer) nil
          (renderer-torch-body-vertex-module renderer) nil
          (renderer-torch-body-layout renderer) nil
          (renderer-torch-body-vertex-buffer renderer) nil
          (renderer-flame-pipeline renderer) nil
          (renderer-flame-fragment-module renderer) nil
          (renderer-flame-vertex-module renderer) nil
          (renderer-flame-layout renderer) nil
          (renderer-flame-effect-buffer renderer) nil
          (renderer-player-sdf-bind-group renderer) nil
          (renderer-player-sdf-pipeline renderer) nil
          (renderer-player-sdf-fragment-module renderer) nil
          (renderer-player-sdf-vertex-module renderer) nil
          (renderer-player-sdf-layout renderer) nil
          (renderer-shadow-pipeline renderer) nil
          (renderer-shadow-vertex-module renderer) nil
          (renderer-shadow-layout renderer) nil
          (renderer-shadow-sampler renderer) nil
          (renderer-shadow-view renderer) nil
          (renderer-shadow-texture renderer) nil
          (renderer-star-template-buffer renderer) nil
          (renderer-pipeline renderer) nil
          (renderer-fragment-module renderer) nil
          (renderer-vertex-module renderer) nil
          (renderer-layout renderer) nil
          (renderer-camera-buffer renderer) nil)
    (values)))
