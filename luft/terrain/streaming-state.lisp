(in-package #:luft.terrain)

;;; A streaming scene retains authored/source residency separately from GPU
;;; publication. REPLACEMENT is either NIL or one whole-region transaction:
;;; output owners and departures, then an immutable worker request and result.
;;; There are no per-owner tickets: the worker compiles the region as one unit.
;;; The owner thread alone changes these fields; workers read frozen snapshots.

(defclass streaming-scene (scene)
  ((source :initarg :source :initform nil :reader streaming-scene-source)
   (store :initform (make-hash-table :test #'eql)
          :reader streaming-scene-store)
   (desired :initform (make-hash-table :test #'eql)
            :reader streaming-scene-desired)
   (load-outstanding :initform (make-hash-table :test #'eql)
                     :reader streaming-scene-load-outstanding)
   (next-demand-token :initform 0
                      :accessor streaming-scene-next-demand-token)
   (next-incarnation :initform 0
                     :accessor streaming-scene-next-incarnation)
   (loaded :initform (make-hash-table :test #'eql)
           :reader streaming-scene-loaded)
   (replacement :initform nil :accessor streaming-scene-replacement)
   (mesh-cache :initform nil :accessor streaming-scene-mesh-cache)
   (production-errors :initform nil
                      :accessor streaming-scene-production-errors)
   (frames-per-load :initarg :frames-per-load :initform 15
                    :accessor streaming-scene-frames-per-load)
   (residency-radius :initarg :residency-radius :initform 1
                     :accessor streaming-scene-residency-radius)
   (focus :initform nil :accessor streaming-scene-focus)
   (mesh-preparer :initarg :mesh-preparer :initform nil
                  :reader streaming-scene-mesh-preparer
                  :documentation "The consumer that realizes each finished mesh on the worker, or NIL.")
   (light-generation
    :initarg :light-generation
    :accessor streaming-scene-light-generation)
   (light-tiles
    :initarg :light-tiles :initform nil
    :reader streaming-scene-light-tiles
    :documentation
    "Published regional light tiles by chunk key, or NIL when this scene
publishes one whole-scene light field instead (#BQMYNU).")
   (frame-counter :initform 0 :accessor streaming-scene-frame-counter)))

(defstruct (streaming-mesh-snapshot
             (:constructor %make-streaming-mesh-snapshot
                 (scene input-scene output-keys witness-keys resident-source-keys
                  bevel-width union-neighborhood stamp
                  realize-attachment-light-p reusable-light-generation
                  &optional mesh-cache light-tiles)))
  "Immutable CPU input for one dependency-closed regional mesh request."
  (scene nil :read-only t)
  ;; The owning streaming scene remains mutable on the canvas thread.  Workers
  ;; borrow this frozen scene value so a later edit cannot mix new materials or
  ;; light with the snapshot's old occupancy fibers.
  (input-scene nil :type scene :read-only t)
  (output-keys nil :type list :read-only t)
  (witness-keys nil :type list :read-only t)
  ;; Logical authored residency is deliberately distinct from OUTPUT-KEYS:
  ;; the latter includes virtual canonical owners needed for closed geometry.
  (resident-source-keys nil :type list :read-only t)
  (bevel-width luft:+mesh-bevel-width+ :read-only t)
  ;; Occupancy phase is not topology.  A snapshot therefore captures exactly
  ;; one mixed-material union; a consumer's preparation classifies its
  ;; finished instances into opaque and translucent passes later.
  (union-neighborhood nil :type hash-table :read-only t)
  (stamp nil :read-only t)
  (realize-attachment-light-p t :type boolean :read-only t)
  (reusable-light-generation nil
                             :type (or null realized-light-generation)
                             :read-only t)
  (mesh-cache nil :type list :read-only t)
  ;; A private copy of the published regional light tiles, or NIL when the
  ;; scene publishes one whole-scene field.
  (light-tiles nil :type (or null hash-table) :read-only t))

(defstruct (streaming-star-product
             (:constructor make-streaming-star-product
                 (key signature descriptors mesh prepared))
             (:copier nil))
  "Immutable CPU product; GPU residency and light publication are independent.
PREPARED is the scene's MESH-PREPARER's realization of MESH.  #WQCMA3"
  (key nil :read-only t)
  (signature nil :read-only t)
  (descriptors nil :read-only t)
  (mesh nil :read-only t)
  (prepared nil :read-only t))

;;; A consumer of streamed meshes may realize each one on the worker, beside
;;; the mesh, so the owner thread only installs it.  The renderer's preparer
;;; builds its CPU population; with no preparer the product is the mesh.

(defgeneric prepare-streamed-mesh (preparer mesh previous)
  (:documentation
   "Realize MESH for PREPARER.  PREVIOUS is the realization of an earlier mesh
with the same star sites and appearance, which may be reused, or NIL.")
  (:method ((preparer null) mesh previous)
    (declare (ignore previous))
    mesh))

(defgeneric streamed-mesh-preparation-signature (preparer)
  (:documentation
   "Identity of PREPARER's compiler, so a redefinition misses the mesh cache.")
  (:method ((preparer null)) nil))

(defclass streaming-mesh-request (production:production-request)
  ((snapshot :initarg :snapshot :reader streaming-mesh-request-snapshot)))

(defstruct (streaming-mesh-result
             (:constructor %make-streaming-mesh-result
                 (meshes generation &optional mesh-cache light-tiles))
             (:copier nil))
  "One worker-complete prepared owner cohort and its exact light generation."
  (meshes nil :type list :read-only t)
  (generation nil :type scene-mesh-generation :read-only t)
  (mesh-cache nil :type list :read-only t)
  (light-tiles nil :type list :read-only t))

(defstruct (streaming-replacement
             (:constructor make-streaming-replacement
                 (output-keys removals request &optional result)))
  (output-keys nil :type list :read-only t)
  (removals nil :type list :read-only t)
  (request nil :read-only t)
  (result nil :type (or null streaming-mesh-result))
  (failure nil :type (or null condition)))
