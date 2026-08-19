;;; The voxel light field: a derived domain beside block content.
;;;
;;; Each resident chunk may own a CHUNK-LIGHT-FIELD: dense sky and blocklight
;;; levels with their own revision and boundary revisions, so relighting never
;;; impersonates a content edit.  The semantic objects live at the chunk
;;; boundary; the 4096-site columns inside are plain (unsigned-byte 8) arrays,
;;; and the solver dispatches block light behavior once per palette entry
;;; rather than per cell.
;;;
;;; This file owns capture, publication, and the closed production solver
;;; protocol.  The compiled frontier programs live in FRONTIER-LIGHT.LISP;
;;; the deliberately simple differential oracle is loaded only by the
;;; LUVCRAFT/LIGHT-REFERENCE system.

(in-package #:luvcraft)

(defconstant +maximum-light-level+ 15)

(records:define-columnar-materialization voxel-light-columns
  (sky-level 0 :type (unsigned-byte 8))
  (block-level 0 :type (unsigned-byte 8)))

(defmethod fields:field-representation-domain ((columns voxel-light-columns))
  (voxel-light-columns-domain columns))

(defclass chunk-light-field ()
  ((columns :initarg :columns :reader chunk-light-field-columns)
   (revision :initform 0 :accessor chunk-light-field-revision)
   ;; -X, +X, -Y, +Y, -Z, +Z, indexed like content boundary revisions.
   (boundary-revisions
    :initform (make-array 6 :element-type '(unsigned-byte 64)
                            :initial-element 0)
    :reader chunk-light-field-boundary-revisions)
   (state :initform :unlit :accessor chunk-light-field-state))
  (:documentation
   "One chunk's derived sky and blocklight levels, revised independently
from block content.  STATE is :UNLIT before any solve, :STABLE when every
boundary was known, and :PROVISIONAL when an unknown residency boundary
contributed to the result."))

(declaim
 (inline chunk-light-field-sky-definition
         chunk-light-field-block-definition
         chunk-light-field-sky-levels
         chunk-light-field-block-levels))

(defun chunk-light-field-sky-definition (field)
  (records:columnar-row-lane-declaration
   (voxel-light-columns-row-declaration (chunk-light-field-columns field))
   'sky-level))

(defun chunk-light-field-block-definition (field)
  (records:columnar-row-lane-declaration
   (voxel-light-columns-row-declaration (chunk-light-field-columns field))
   'block-level))

(defun chunk-light-field-sky-levels (field)
  (voxel-light-columns-sky-level-lane (chunk-light-field-columns field)))

(defun chunk-light-field-block-levels (field)
  (voxel-light-columns-block-level-lane (chunk-light-field-columns field)))

(defmethod luvcraft.world.fields:materialized-field-definition
    ((field chunk-light-field) (field-name (eql :sky-light)))
  (declare (ignore field-name))
  (chunk-light-field-sky-definition field))

(defmethod luvcraft.world.fields:materialized-field-definition
    ((field chunk-light-field) (field-name (eql :block-light)))
  (declare (ignore field-name))
  (chunk-light-field-block-definition field))

(defmethod fields:materialized-field-representation
    ((field chunk-light-field) (field-name (eql :sky-light)))
  (declare (ignore field-name))
  (chunk-light-field-columns field))

(defmethod fields:materialized-field-representation
    ((field chunk-light-field) (field-name (eql :block-light)))
  (declare (ignore field-name))
  (chunk-light-field-columns field))

(defun make-chunk-light-field (domain)
  "Make an unlit resident field whose columns are bound to DOMAIN."
  (make-instance
   'chunk-light-field
   :columns
   (make-voxel-light-columns
    domain
    :declarations
    `((sky-level . ,(fields:field-definition-for :sky-light))
      (block-level . ,(fields:field-definition-for :block-light))))))

(defun chunk-light-field-boundary-revision (field direction)
  (aref (chunk-light-field-boundary-revisions field)
        (chunk-boundary-index direction)))

;;; What a missing resident neighbor means is a world/source decision.  The
;;; solver never equates :ABSENT with open sky on its own.

(defgeneric absent-chunk-light-semantics (source world chunk-key direction)
  (:documentation
   "How light should treat the absent neighbor of CHUNK-KEY in DIRECTION.

Return :OPEN-SKY for a boundary known to see the sky, :CLOSED for a boundary
known to be outside the world, or :UNKNOWN for terrain which merely is not
resident."))

(defmethod absent-chunk-light-semantics ((source t) world chunk-key direction)
  (declare (ignore world chunk-key direction))
  :unknown)

(defmethod absent-chunk-light-semantics
    ((source little-world-source) world chunk-key direction)
  ;; The little generated world declares open sky above its known vertical
  ;; extent and a closed floor beneath it.  Lateral terrain is generatable
  ;; but simply not resident, which is exactly :UNKNOWN.
  (declare (ignore world chunk-key))
  (cond ((plusp (voxel-direction-dy direction)) :open-sky)
        ((minusp (voxel-direction-dy direction)) :closed)
        (t :unknown)))

;;; Palette-indexed light tables: one generic dispatch per palette entry,
;;; then dense u8 lookups in every hot loop.

(defclass block-palette-domain ()
  ((palette :initarg :palette :reader block-palette-domain-palette))
  (:documentation "The finite block-state sites addressed by palette code."))

(defmethod domains:domain-cardinality ((domain block-palette-domain))
  (length (block-palette-domain-palette domain)))

(records:define-columnar-materialization block-light-properties
  (propagation-loss 0 :type (unsigned-byte 8))
  (emission-level 0 :type (unsigned-byte 8)))

(defmethod fields:field-representation-domain
    ((properties block-light-properties))
  (block-light-properties-domain properties))

(defun represented-block-slot-declaration (slot-name)
  "Project BLOCK-KIND's semantic slot metadata into a storage declaration."
  (let ((slot (records:record-slot-declaration 'block-kind slot-name)))
    (math:make-represented-value-declaration
     :representation-type (math:declaration-representation-type slot)
     :quantity-specification
     (math:declaration-quantity-specification slot)
     :source-form (math:declaration-source-form slot))))

(defun block-palette-light-properties (palette)
  "Materialize propagation loss and emission over PALETTE's entry domain."
  (let* ((domain (make-instance 'block-palette-domain :palette palette))
         (properties
           (make-block-light-properties
            domain
            :declarations
            `((propagation-loss
               . ,(represented-block-slot-declaration 'light-opacity))
              (emission-level
               . ,(represented-block-slot-declaration 'light-emission))))))
    (records:with-columnar-materialization-storage
        ((borrowed-domain extent row
                          (losses propagation-loss)
                          (emissions emission-level))
         properties block-light-properties)
      (declare (ignore row))
      (assert (eq domain borrowed-domain))
      (dotimes (index extent)
        (let ((block (aref palette index)))
          (setf (aref losses index)
                (min +maximum-light-level+
                     (max 0 (block-light-opacity block)))
                (aref emissions index)
                (min +maximum-light-level+
                     (max 0 (block-light-emission block)))))))
    properties))

;;; A captured region: every resident chunk's dense content beside fresh
;;; work arrays.  The reference solver reads and writes only this capture,
;;; then publishes complete fields in one pass.

(defstruct (light-region-entry (:constructor %make-light-region-entry))
  (chunk nil)
  (key nil :type (or null chunk-coordinate))
  (content-definition nil)
  (indices nil :type (or null (simple-array (unsigned-byte 16) (*))))
  (block-properties nil :type (or null block-light-properties))
  (light-columns nil :type (or null voxel-light-columns)))

(declaim
 (inline light-region-entry-domain
         light-region-entry-sky-definition
         light-region-entry-block-definition
         light-region-entry-opacity-lut
         light-region-entry-emission-lut
         light-region-entry-sky
         light-region-entry-block))

(defun light-region-entry-domain (entry)
  (voxel-light-columns-domain (light-region-entry-light-columns entry)))

(defun light-region-entry-sky-definition (entry)
  (records:columnar-row-lane-declaration
   (voxel-light-columns-row-declaration
    (light-region-entry-light-columns entry))
   'sky-level))

(defun light-region-entry-block-definition (entry)
  (records:columnar-row-lane-declaration
   (voxel-light-columns-row-declaration
    (light-region-entry-light-columns entry))
   'block-level))

(defun light-region-entry-opacity-lut (entry)
  (block-light-properties-propagation-loss-lane
   (light-region-entry-block-properties entry)))

(defun light-region-entry-emission-lut (entry)
  (block-light-properties-emission-level-lane
   (light-region-entry-block-properties entry)))

(defun light-region-entry-sky (entry)
  (voxel-light-columns-sky-level-lane
   (light-region-entry-light-columns entry)))

(defun light-region-entry-block (entry)
  (voxel-light-columns-block-level-lane
   (light-region-entry-light-columns entry)))

(defmethod luvcraft.world.fields:materialized-field-definition
    ((entry light-region-entry) (field-name (eql :block-content)))
  (declare (ignore field-name))
  (light-region-entry-content-definition entry))

(defmethod luvcraft.world.fields:materialized-field-definition
    ((entry light-region-entry) (field-name (eql :sky-light)))
  (declare (ignore field-name))
  (light-region-entry-sky-definition entry))

(defmethod luvcraft.world.fields:materialized-field-definition
    ((entry light-region-entry) (field-name (eql :block-light)))
  (declare (ignore field-name))
  (light-region-entry-block-definition entry))

(defmethod fields:materialized-field-representation
    ((entry light-region-entry) (field-name (eql :sky-light)))
  (declare (ignore field-name))
  (light-region-entry-light-columns entry))

(defmethod fields:materialized-field-representation
    ((entry light-region-entry) (field-name (eql :block-light)))
  (declare (ignore field-name))
  (light-region-entry-light-columns entry))

(defstruct (light-region (:constructor %make-light-region))
  (world nil)
  (space nil :type (or null voxel-space))
  (entries (make-hash-table :test #'equalp) :type hash-table)
  ;; Frozen regions carry the source's answer for every absent boundary so a
  ;; producer never consults the live world while solving a captured batch.
  (absent-semantics nil :type (or null hash-table))
  ;; A from-scratch capture enumerates its entries eagerly.  An incremental
  ;; candidate instead materializes entries on first touch, initialized from
  ;; the chunk's current published field, so propagation may wander into any
  ;; resident chunk without precomputing the affected set.
  (ensure-entry nil :type (or null function)))

(defun add-light-region-entry
    (region chunk &key from-field-p copy-content-p)
  "Materialize CHUNK's dense capture in REGION and return the new entry."
  (with-block-content-storage (domain palette indices) chunk
    (let* ((key (chunk-domain-coordinate domain))
           (field (and from-field-p (block-chunk-light-field chunk)))
           (sky-definition
             (if field
                 (fields:materialized-field-definition field :sky-light)
                 (fields:field-definition-for :sky-light)))
           (block-definition
             (if field
                 (fields:materialized-field-definition field :block-light)
                 (fields:field-definition-for :block-light)))
           (light-columns
             (make-voxel-light-columns
              domain
              :declarations `((sky-level . ,sky-definition)
                              (block-level . ,block-definition))))
           (sky (voxel-light-columns-sky-level-lane light-columns))
           (block-levels
             (voxel-light-columns-block-level-lane light-columns)))
        (when field
          (replace sky (chunk-light-field-sky-levels field))
          (replace block-levels (chunk-light-field-block-levels field)))
        (setf (gethash key (light-region-entries region))
              (%make-light-region-entry
               :chunk chunk :key key
               :content-definition
               (luvcraft.world.fields:materialized-field-definition
                (block-chunk-content chunk) :block-content)
               :indices (coerce (if copy-content-p (copy-seq indices) indices)
                                '(simple-array (unsigned-byte 16) (*)))
               :block-properties (block-palette-light-properties palette)
               :light-columns light-columns)))))

(defun light-region-boundary-key (key direction)
  (list key (voxel-direction-dx direction) (voxel-direction-dy direction)
        (voxel-direction-dz direction)))

(defun capture-light-region (world &key immutable-p)
  "Capture every resident chunk of WORLD for a from-scratch relight.

With IMMUTABLE-P, copy content indices and capture absent-boundary semantics;
the returned region may then be solved without reading the live world."
  (let ((region (%make-light-region
                 :world world :space (block-world-space world)
                 :absent-semantics
                 (and immutable-p (make-hash-table :test #'equalp)))))
    (dolist (chunk (resident-world-chunks world))
      (add-light-region-entry region chunk :copy-content-p immutable-p))
    (when immutable-p
      (let ((source (block-world-source world)))
        (maphash
         (lambda (key entry)
           (declare (ignore entry))
           (dolist (direction *voxel-face-directions*)
             (let ((neighbor (chunk-coordinate-neighbor key direction)))
               (declare (dynamic-extent neighbor))
               (unless (gethash neighbor (light-region-entries region))
                 (setf (gethash (light-region-boundary-key key direction)
                                (light-region-absent-semantics region))
                       (absent-chunk-light-semantics
                        source world key direction))))))
         (light-region-entries region))))
    region))

(defun make-light-candidate (world)
  "A lazily populated region whose entries start from current fields."
  (%make-light-region
   :world world :space (block-world-space world)
   :ensure-entry
   (lambda (region key)
     (multiple-value-bind (chunk present-p)
         (world-chunk-at-coordinate world key)
       (when present-p
         (add-light-region-entry region chunk :from-field-p t))))))

(declaim (inline light-region-locate-components))
(defun light-region-locate-components (region x y z)
  "Resolve scalar world components to ENTRY, OFFSET, and availability."
  (multiple-value-bind
        (chunk-x chunk-y chunk-z local-x local-y local-z)
      (voxel-space-decompose-components
       (light-region-space region) x y z)
    (let* ((key (make-chunk-coordinate chunk-x chunk-y chunk-z))
           (entry (or (gethash key (light-region-entries region))
                      (let ((ensure (light-region-ensure-entry region)))
                        (and ensure (funcall ensure region key))))))
      (if entry
          (values entry
                  (chunk-domain-offset-components
                   (light-region-entry-domain entry)
                   local-x local-y local-z)
                  :available)
          (values nil nil :unavailable)))))

(declaim (inline light-region-locate))
(defun light-region-locate (region coordinate)
  "Resolve a WORLD-COORDINATE to (VALUES ENTRY OFFSET) or NIL when absent."
  (multiple-value-bind (entry offset availability)
      (light-region-locate-components
       region
       (world-coordinate-x coordinate)
       (world-coordinate-y coordinate)
       (world-coordinate-z coordinate))
    (declare (ignore availability))
    (values entry offset)))

(defmethod locate-chunk-window-site ((region light-region) x y z)
  (light-region-locate-components region x y z))

(defun light-region-opacity (entry offset)
  (aref (light-region-entry-opacity-lut entry)
        (aref (light-region-entry-indices entry) offset)))

(defun light-region-neighbor-resident-p (region coordinate direction)
  (let ((neighbor (chunk-coordinate-neighbor coordinate direction)))
    (declare (dynamic-extent neighbor))
    (if (light-region-absent-semantics region)
        (nth-value 1 (gethash neighbor (light-region-entries region)))
        (nth-value 1
                   (world-chunk-at-coordinate
                    (light-region-world region) neighbor)))))

(defun light-region-absent-boundary-semantics (region key direction)
  (let ((captured (light-region-absent-semantics region)))
    (if captured
        (gethash (light-region-boundary-key key direction) captured)
        (let ((world (light-region-world region)))
          (absent-chunk-light-semantics
           (block-world-source world) world key direction)))))

(defun light-region-provisional-p (region key)
  "Whether chunk KEY currently borders any :UNKNOWN residency boundary."
  (let ((world (light-region-world region)))
    (declare (ignore world))
    (loop for direction in *voxel-face-directions*
          thereis (and (not (light-region-neighbor-resident-p
                             region key direction))
                       (eq (light-region-absent-boundary-semantics
                            region key direction)
                           :unknown)))))

(defun map-entry-face-sites (entry direction function)
  "Call FUNCTION with OFFSET and borrowed LOCAL for one face of ENTRY.

LOCAL has dynamic extent and must be copied before FUNCTION retains it."
  (let ((domain (light-region-entry-domain entry)))
    (do-chunk-domain-face (offset local domain direction)
      (funcall function offset local))))

(defparameter *voxel-light-solver* :compiled
  "The voxel-light program selected for production solves.

Only :COMPILED is implemented by the runtime.  The legacy implementation is
loaded explicitly by the LUVCRAFT/LIGHT-REFERENCE system as a differential
test oracle; unsupported names signal through the closed EQL dispatch.
#X7Q90E #PJY6E1 #K3WRD3")

(defgeneric solve-light-region-using
    (solver region &key &allow-other-keys)
  (:documentation
   "Solve captured REGION with the explicitly implemented voxel-light SOLVER.

There is deliberately no default method: unsupported solver names signal
instead of silently selecting another implementation."))

;;; Publication compares complete candidate arrays against the chunk's
;;; current field and advances light revisions only: content revisions and
;;; the world revision are authored-data facts this derived domain must not
;;; touch.

(defun light-boundary-plane-changed-p
    (domain old-levels new-levels direction)
  (do-chunk-domain-face (offset local domain direction)
    (declare (ignore local))
    (when (/= (aref old-levels offset) (aref new-levels offset))
      (return-from light-boundary-plane-changed-p t)))
  nil)

(defun publish-light-region (region)
  "Install every changed candidate field; return the changed chunks."
  (let ((changed nil))
    (maphash
     (lambda (key entry)
       (let* ((chunk (light-region-entry-chunk entry))
              (domain (light-region-entry-domain entry))
              (field (block-chunk-light-field chunk))
              (new-sky (light-region-entry-sky entry))
              (new-block (light-region-entry-block entry))
              (state (if (light-region-provisional-p region key)
                         :provisional
                         :stable)))
         (cond
           ((null field)
            (let ((field (make-chunk-light-field domain)))
              (replace (chunk-light-field-sky-levels field) new-sky)
              (replace (chunk-light-field-block-levels field) new-block)
              (setf (chunk-light-field-state field) state
                    (chunk-light-field-revision field) 1)
              (let ((revisions (chunk-light-field-boundary-revisions field)))
                (dotimes (face 6) (setf (aref revisions face) 1)))
              (setf (block-chunk-light-field chunk) field)
              (push chunk changed)))
           (t
            (let* ((old-sky (chunk-light-field-sky-levels field))
                   (old-block (chunk-light-field-block-levels field))
                   (levels-changed-p
                     (or (not (equalp old-sky new-sky))
                         (not (equalp old-block new-block))))
                   (state-changed-p
                     (not (eq state (chunk-light-field-state field)))))
              (when (or levels-changed-p state-changed-p)
                (let ((revisions
                        (chunk-light-field-boundary-revisions field)))
                  (dolist (direction *voxel-face-directions*)
                    (when (or (light-boundary-plane-changed-p
                               domain old-sky new-sky direction)
                              (light-boundary-plane-changed-p
                               domain old-block new-block direction))
                      (incf (aref revisions
                                  (chunk-boundary-index direction))))))
                (replace old-sky new-sky)
                (replace old-block new-block)
                (setf (chunk-light-field-state field) state)
                (incf (chunk-light-field-revision field))
                (push chunk changed)))))))
     (light-region-entries region))
    changed))

(defun relight-block-world (world)
  "Solve and publish voxel light from scratch for WORLD's resident chunks.

Returns the chunks whose published light changed.  The selected production
solver must have an explicit SOLVE-LIGHT-REGION-USING method."
  (publish-light-region
   (solve-light-region-using
    *voxel-light-solver* (capture-light-region world))))

;;; Sparse light accessors, for inspectors and tests.  Dense consumers
;;; (the mesher's snapshot halo) read the field arrays directly.

(defun chunk-light-levels-at-coordinate (chunk local)
  "Return (VALUES SKY BLOCK STATE) at one LOCAL-COORDINATE in CHUNK."
  (let ((field (block-chunk-light-field chunk)))
    (if field
        (let ((offset (chunk-domain-offset (block-chunk-domain chunk) local)))
          (values (aref (chunk-light-field-sky-levels field) offset)
                  (aref (chunk-light-field-block-levels field) offset)
                  (chunk-light-field-state field)))
        (values 0 0 :unlit))))

(defun chunk-light-levels-at (chunk x y z)
  "Scalar convenience wrapper around CHUNK-LIGHT-LEVELS-AT-COORDINATE."
  (let ((local (make-local-coordinate x y z)))
    (declare (dynamic-extent local))
    (chunk-light-levels-at-coordinate chunk local)))

(defun world-light-levels-at (world x y z)
  "Return sparse SKY, BLOCK, and STATE readings at world site X,Y,Z.

This is the inspector-scale light counterpart to WORLD-BLOCK-AT. Dense light
consumers should bind a chunk field once rather than resolving every site."
  (multiple-value-bind (chunk offset availability)
      (locate-chunk-window-site world x y z)
    (ecase availability
      (:available
       (let ((field (block-chunk-light-field chunk)))
         (if field
             (values (aref (chunk-light-field-sky-levels field) offset)
                     (aref (chunk-light-field-block-levels field) offset)
                     (chunk-light-field-state field))
             (values 0 0 :unlit))))
      (:unavailable (values 0 0 :unavailable)))))

(defclass luvcraft-lighting-state ()
  ((world :initarg :world :reader lighting-state-world)
   (dirty-cells :initform (make-hash-table :test #'equalp)
                :reader lighting-state-dirty-cells)
   (arrivals :initform (make-hash-table :test #'equalp)
             :reader lighting-state-arrivals)
   (departures :initform (make-hash-table :test #'equalp)
               :reader lighting-state-departures)
   ;; Work counters, exposed so performance claims come from measurements.
   (cells-visited :initform 0 :accessor lighting-state-cells-visited)
   (chunks-touched :initform 0 :accessor lighting-state-chunks-touched)
   (publications :initform 0 :accessor lighting-state-publications)
   (last-latency-seconds
    :initform 0d0
    :type double-float
    :quantity (:quantity :lighting-reconciliation-duration :unit :second)
    :accessor lighting-state-last-latency-seconds))
  (:metaclass luv.arithmetic.records:quantity-class)
  (:documentation
   "Accumulated lighting dirtiness for one world, owned by its session.

Content edits and residency transitions feed this object through the
world's hooks; RECONCILE-LIGHTING settles the queues and publishes."))

(defun attach-lighting-state (world)
  "Subscribe a fresh lighting state to WORLD's content and residency hooks.

Chunks already resident at attachment are treated as arrivals, so the
first reconcile lights a caller-built world without a separate protocol."
  (let ((state (make-instance 'luvcraft-lighting-state :world world)))
    (dolist (chunk (resident-world-chunks world))
      (setf (gethash (chunk-domain-coordinate (block-chunk-domain chunk))
                     (lighting-state-arrivals state))
            t))
    (setf (block-world-cell-change-hook world)
          (lambda (chunk x y z)
            (declare (ignore chunk))
            (setf (gethash (make-world-coordinate x y z)
                           (lighting-state-dirty-cells state))
                  t))
          (block-world-residency-change-hook world)
          (lambda (x y z event)
            (let ((key (make-chunk-coordinate x y z)))
              (ecase event
                (:arrived
                 (setf (gethash key (lighting-state-arrivals state)) t))
                (:departed
                 (setf (gethash key (lighting-state-departures state)) t))))))
    state))

(defun lighting-state-dirty-p (state)
  (or (plusp (hash-table-count (lighting-state-dirty-cells state)))
      (plusp (hash-table-count (lighting-state-arrivals state)))
      (plusp (hash-table-count (lighting-state-departures state)))))

(defun lighting-state-residency-dirty-p (state)
  "Whether STATE includes chunk arrivals or departures."
  (or (plusp (hash-table-count (lighting-state-arrivals state)))
      (plusp (hash-table-count (lighting-state-departures state)))))

(defun reconcile-lighting (state)
  "Settle STATE's queues over a candidate and publish once.

Returns the chunks whose published light changed.  Work runs on the calling
owner thread; the counters record its actual cost so any move to captured
producer batches is justified by measurement rather than guesswork."
  (unless (lighting-state-dirty-p state)
    (return-from reconcile-lighting nil))
  (let* ((start (get-internal-real-time))
         (world (lighting-state-world state))
         (region (make-light-candidate world))
         (visited (reconcile-light-region-using *voxel-light-solver* state region)))
    (let ((changed (publish-light-region region)))
      (clrhash (lighting-state-dirty-cells state))
      (clrhash (lighting-state-arrivals state))
      (clrhash (lighting-state-departures state))
      (incf (lighting-state-cells-visited state) visited)
      (incf (lighting-state-chunks-touched state)
            (hash-table-count (light-region-entries region)))
      (incf (lighting-state-publications state))
      (setf (lighting-state-last-latency-seconds state)
            (/ (- (get-internal-real-time) start)
               (coerce internal-time-units-per-second 'double-float)))
      changed)))

(defgeneric reconcile-light-region-using (solver state region)
  (:documentation
   "Settle STATE's dirty cells, departures, and arrivals over candidate
REGION with the explicitly implemented incremental relighter named by SOLVER.
There is deliberately no default method: unsupported names signal rather than
falling back to a different algorithm. #K3WRD3"))

(defun world-light-at-coordinate (world coordinate)
  "Return (VALUES SKY BLOCK STATE) at COORDINATE, or zeros when absent."
  (multiple-value-bind (chunk-coordinate local)
      (world-coordinate-chunk-and-local (block-world-space world) coordinate)
    (declare (dynamic-extent chunk-coordinate local))
    (multiple-value-bind (chunk present-p)
        (world-chunk-at-coordinate world chunk-coordinate)
      (if present-p
          (chunk-light-levels-at-coordinate chunk local)
          (values 0 0 :absent)))))

(defun world-light-at (world x y z)
  "Scalar convenience wrapper around WORLD-LIGHT-AT-COORDINATE."
  (let ((coordinate (make-world-coordinate x y z)))
    (declare (dynamic-extent coordinate))
    (world-light-at-coordinate world coordinate)))
