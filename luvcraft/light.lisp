;;; The voxel light field: a derived domain beside block content.
;;;
;;; Each resident chunk may own a CHUNK-LIGHT-FIELD: dense sky and blocklight
;;; levels with their own revision and boundary revisions, so relighting never
;;; impersonates a content edit.  The semantic objects live at the chunk
;;; boundary; the 4096-site columns inside are plain (unsigned-byte 8) arrays,
;;; and the solver dispatches block light behavior once per palette entry
;;; rather than per cell.
;;;
;;; This file provides the from-scratch reference relight: it clears the
;;; captured region, seeds known sky boundaries and emitters, and propagates
;;; to fixation.  It favors obvious correctness; the incremental runtime
;;; relighter is checked against it.

(in-package #:luvcraft)

(defconstant +maximum-light-level+ 15)

(defclass chunk-light-field ()
  ((sky-definition
    :initarg :sky-definition
    :initform (luvcraft.world.fields:field-definition-for :sky-light)
    :reader chunk-light-field-sky-definition)
   (block-definition
    :initarg :block-definition
    :initform (luvcraft.world.fields:field-definition-for :block-light)
    :reader chunk-light-field-block-definition)
   (sky-levels :initarg :sky-levels :reader chunk-light-field-sky-levels)
   (block-levels :initarg :block-levels
                 :reader chunk-light-field-block-levels)
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

(defmethod luvcraft.world.fields:materialized-field-definition
    ((field chunk-light-field) (field-name (eql :sky-light)))
  (declare (ignore field-name))
  (chunk-light-field-sky-definition field))

(defmethod luvcraft.world.fields:materialized-field-definition
    ((field chunk-light-field) (field-name (eql :block-light)))
  (declare (ignore field-name))
  (chunk-light-field-block-definition field))

(defun make-chunk-light-field (cardinality)
  (flet ((levels ()
           (make-array cardinality :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
    (make-instance 'chunk-light-field
                   :sky-levels (levels) :block-levels (levels))))

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

(defun block-palette-light-tables (palette)
  "Return dense opacity and emission vectors indexed like PALETTE."
  (let* ((count (length palette))
         (opacity (make-array count :element-type '(unsigned-byte 8)))
         (emission (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (index count)
      (let ((block (aref palette index)))
        (setf (aref opacity index)
              (min +maximum-light-level+
                   (max 0 (block-light-opacity block)))
              (aref emission index)
              (min +maximum-light-level+
                   (max 0 (block-light-emission block))))))
    (values opacity emission)))

;;; A captured region: every resident chunk's dense content beside fresh
;;; work arrays.  The reference solver reads and writes only this capture,
;;; then publishes complete fields in one pass.

(defstruct (light-region-entry (:constructor %make-light-region-entry))
  (chunk nil)
  (domain nil :type (or null chunk-domain))
  (key nil :type (or null chunk-coordinate))
  (content-definition nil)
  (sky-definition nil)
  (block-definition nil)
  (indices nil :type (or null (simple-array (unsigned-byte 16) (*))))
  (opacity-lut nil)
  (emission-lut nil)
  (sky nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (block nil :type (or null (simple-array (unsigned-byte 8) (*)))))

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

(records:define-columnar-buffer light-worklist-bucket
  ;; ENTRY supplies the semantic chunk boundary, while OFFSET and LEVEL are
  ;; unboxed site data.  The generated buffer owns their shared extent.
  (entry nil :type (or null light-region-entry) :clear-on-remove t)
  (offset 0 :type (unsigned-byte 32))
  (level 0 :type (unsigned-byte 8)))

(defstruct (light-worklist (:constructor %make-light-worklist))
  (scheduling :lifo :type (member :lifo :level) :read-only t)
  (field-definition nil
                    :type (or null
                              luvcraft.world.fields:voxel-field-definition)
                    :read-only t)
  (buckets #() :type simple-vector :read-only t)
  (maximum-level -1 :type fixnum)
  (count 0 :type fixnum))

(defun make-light-worklist (&key (scheduling :lifo) field-definition)
  "Make a reusable packed lighting frontier with LIFO or level scheduling.

Both modes retain ENTRY plus dense OFFSET and LEVEL lanes, never a cons or
coordinate object per item.  LEVEL scheduling consumes brighter buckets
first; LIFO puts every item in bucket zero and preserves the former order.
#QS1ERH.  FIELD-DEFINITION binds LEVEL's exact quantity meaning once. #LDP5UR"
  (check-type scheduling (member :lifo :level))
  (check-type field-definition
              (or null luvcraft.world.fields:voxel-field-definition))
  (let* ((layout-definition
           (records:columnar-layout-definition-for 'light-worklist-bucket))
         (row-declaration
           (records:make-columnar-row-declaration
            layout-definition
            (and field-definition `((level . ,field-definition)))))
         (buckets (make-array (1+ +maximum-light-level+))))
    (dotimes (level (length buckets))
      (setf (aref buckets level)
            (make-light-worklist-bucket
             :capacity 256 :row-declaration row-declaration)))
    (%make-light-worklist
     :scheduling scheduling :field-definition field-definition
     :buckets buckets)))

(defmethod luvcraft.world.fields:materialized-field-definition
    ((worklist light-worklist) field-name)
  (let ((definition (light-worklist-field-definition worklist)))
    (and definition
         (eq field-name
             (luvcraft.world.fields:voxel-field-definition-name definition))
         definition)))

(declaim (inline light-worklist-empty-p light-worklist-push))
(defun light-worklist-empty-p (worklist)
  (zerop (light-worklist-count worklist)))

(defun light-worklist-push (worklist entry offset level)
  "Push ENTRY, domain OFFSET, and propagation LEVEL onto WORKLIST."
  (check-type entry light-region-entry)
  (check-type offset (unsigned-byte 32))
  (check-type level (unsigned-byte 8))
  (let* ((bucket-level
           (ecase (light-worklist-scheduling worklist)
             (:lifo 0)
             (:level level)))
         (bucket (aref (light-worklist-buckets worklist) bucket-level)))
    (light-worklist-bucket-push bucket entry offset level)
    (incf (light-worklist-count worklist))
    (setf (light-worklist-maximum-level worklist)
          (max bucket-level (light-worklist-maximum-level worklist)))
    worklist))

(defun light-worklist-pop (worklist)
  "Pop (VALUES ENTRY OFFSET LEVEL PRESENT-P), clearing retained ENTRY."
  (when (light-worklist-empty-p worklist)
    (return-from light-worklist-pop (values nil nil nil nil)))
  (let* ((bucket-level (light-worklist-maximum-level worklist))
         (bucket (aref (light-worklist-buckets worklist) bucket-level)))
    (multiple-value-bind (entry offset level present-p)
        (light-worklist-bucket-pop bucket)
      (unless present-p
        (error "Lighting worklist count disagrees with bucket ~D." bucket-level))
      (decf (light-worklist-count worklist))
      (when (zerop (light-worklist-bucket-length bucket))
        (loop for candidate downfrom (1- bucket-level) to 0
              when (plusp
                    (light-worklist-bucket-length
                     (aref (light-worklist-buckets worklist) candidate)))
                do (setf (light-worklist-maximum-level worklist) candidate)
                   (return)
              finally (setf (light-worklist-maximum-level worklist) -1)))
      (values entry offset level t))))

(defstruct (light-removal-queue
             (:constructor %make-light-removal-queue))
  (field-name nil :type keyword :read-only t)
  (field-definition nil :type luvcraft.world.fields:voxel-field-definition
                        :read-only t)
  (field-reader nil :type function :read-only t)
  (skylight-p nil :type boolean :read-only t)
  (worklist (make-light-worklist) :type light-worklist :read-only t)
  (removed (make-hash-table :test #'equalp) :type hash-table :read-only t))

(defun make-light-removal-queue
    (field-name field-reader &key skylight-p (scheduling :level))
  "Make a removal frontier whose raw levels retain one field's exact meaning."
  (let ((definition (luvcraft.world.fields:field-definition-for field-name)))
    (unless definition
      (error "There is no voxel field definition named ~S." field-name))
    (%make-light-removal-queue
     :field-name field-name :field-definition definition :field-reader field-reader
     :skylight-p skylight-p
     :worklist (make-light-worklist
                :scheduling scheduling :field-definition definition))))

(defmethod luvcraft.world.fields:materialized-field-definition
    ((queue light-removal-queue) field-name)
  (and (eq field-name (light-removal-queue-field-name queue))
       (light-removal-queue-field-definition queue)))

(defun enqueue-light-removal (queue entry offset level)
  "Admit one entry-local OFFSET under QUEUE's retained field definition."
  (unless (typep level
                 (luvcraft.world.fields:voxel-field-definition-legal-value-type
                  (light-removal-queue-field-definition queue)))
    (error "Light removal level ~S is illegal for field ~S."
           level (light-removal-queue-field-name queue)))
  (light-worklist-push
   (light-removal-queue-worklist queue) entry offset level)
  queue)

(defun add-light-region-entry
    (region chunk &key from-field-p copy-content-p)
  "Materialize CHUNK's dense capture in REGION and return the new entry."
  (with-block-content-storage (domain palette indices) chunk
    (multiple-value-bind (opacity emission)
        (block-palette-light-tables palette)
      (let* ((key (chunk-domain-coordinate domain))
             (cardinality (length indices))
             (field (and from-field-p (block-chunk-light-field chunk)))
             (sky (make-array cardinality :element-type '(unsigned-byte 8)
                                          :initial-element 0))
             (block-levels (make-array cardinality
                                       :element-type '(unsigned-byte 8)
                                       :initial-element 0)))
        (when field
          (replace sky (chunk-light-field-sky-levels field))
          (replace block-levels (chunk-light-field-block-levels field)))
        (setf (gethash key (light-region-entries region))
              (%make-light-region-entry
               :chunk chunk :domain domain :key key
               :content-definition
               (luvcraft.world.fields:materialized-field-definition
                (block-chunk-content chunk) :block-content)
               :sky-definition
               (if field
                   (luvcraft.world.fields:materialized-field-definition
                    field :sky-light)
                   (luvcraft.world.fields:field-definition-for :sky-light))
               :block-definition
               (if field
                   (luvcraft.world.fields:materialized-field-definition
                    field :block-light)
                   (luvcraft.world.fields:field-definition-for :block-light))
               :indices (coerce (if copy-content-p (copy-seq indices) indices)
                                '(simple-array (unsigned-byte 16) (*)))
               :opacity-lut opacity :emission-lut emission
               :sky sky :block block-levels))))))

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

;;; Propagation.  One rule pair covers any number of vertical chunks:
;;;
;;; - Sky light moving DOWN loses only the target cell's opacity, so a
;;;   direct beam survives an arbitrarily tall transparent column.
;;; - Sky light moving laterally or up, and blocklight moving anywhere,
;;;   loses one level per step plus the target cell's opacity.

(defun propagate-light-region (region field-reader queue skylight-p)
  "Run FIELD's max-fixpoint propagation from packed sites in QUEUE.

Returns the number of cells visited, for the runtime's work counters."
  (let ((visited 0))
    (loop until (light-worklist-empty-p queue)
          do (multiple-value-bind (entry offset queued-level present-p)
                 (light-worklist-pop queue)
               (declare (ignore queued-level present-p))
               (let* ((domain (light-region-entry-domain entry))
                      (local (chunk-domain-local-coordinate domain offset)))
                 (declare (dynamic-extent local))
                 (incf visited)
                 (let ((level (aref (funcall field-reader entry) offset)))
                   (when (plusp level)
                     (do-chunk-window-neighbors
                         (neighbor-offset destination crossing direction
                          materialization availability
                          region domain local *voxel-face-directions*)
                       (values destination)
                       (let ((neighbor
                               (ecase availability
                                 (:local entry)
                                 (:available materialization)
                                 (:unavailable nil))))
                         (when neighbor
                           (let* ((opacity
                                    (light-region-opacity
                                     neighbor neighbor-offset))
                                  (loss
                                    (if (and skylight-p
                                             (eq direction
                                                 +voxel-negative-y+))
                                        opacity
                                        (+ 1 opacity)))
                                  (candidate (- level loss))
                                  (levels (funcall field-reader neighbor)))
                             (when (> candidate
                                      (aref levels neighbor-offset))
                               (setf (aref levels neighbor-offset) candidate)
                               (light-worklist-push
                                queue neighbor neighbor-offset
                                candidate)))))))))))
    visited))

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

(defun seed-open-sky-at-offset (entry offset downward-p)
  "Admit boundary sky at ENTRY's OFFSET and report whether it grew."
  (let* ((opacity (light-region-opacity entry offset))
         (level (- +maximum-light-level+
                   (if downward-p opacity (+ 1 opacity)))))
    (when (> level (aref (light-region-entry-sky entry) offset))
      (setf (aref (light-region-entry-sky entry) offset) level)
      t)))

(defun seed-entry-open-boundaries (region key entry enqueue)
  "Seed ENTRY's faces whose absent neighbors are known open sky."
  (dolist (direction *voxel-face-directions*)
    (when (and (not (light-region-neighbor-resident-p region key direction))
               (eq (light-region-absent-boundary-semantics
                    region key direction)
                   :open-sky))
        (map-entry-face-sites
         entry direction
         (lambda (offset local)
           (values local)
           (when (seed-open-sky-at-offset
                  entry offset (eq direction +voxel-positive-y+))
             (funcall enqueue entry offset
                      (aref (light-region-entry-sky entry) offset))))))))

(defun seed-region-sky-boundaries (region &key (scheduling :level))
  "Seed every entry's open-sky boundary light; return the seed queue."
  (let ((queue
          (make-light-worklist
           :scheduling scheduling
           :field-definition (fields:field-definition-for :sky-light))))
    (maphash (lambda (key entry)
               (seed-entry-open-boundaries
                region key entry
                (lambda (seed-entry offset level)
                  (light-worklist-push
                   queue seed-entry offset level))))
             (light-region-entries region))
    queue))

(defun seed-region-emitters (region &key (scheduling :level))
  "Seed every emitting cell at its configured blocklight level."
  (let ((queue
          (make-light-worklist
           :scheduling scheduling
           :field-definition (fields:field-definition-for :block-light))))
    (maphash
     (lambda (key entry)
       (declare (ignore key))
       (let ((indices (light-region-entry-indices entry))
             (emission (light-region-entry-emission-lut entry))
             (levels (light-region-entry-block entry))
             (domain (light-region-entry-domain entry)))
         (do-chunk-domain-sites (offset local domain)
           (values local)
           (let ((level (aref emission (aref indices offset))))
             (when (plusp level)
               (setf (aref levels offset) level)
               (light-worklist-push queue entry offset level))))))
     (light-region-entries region))
    queue))

(defun solve-light-region (region &key (scheduling :level))
  "Seed and propagate both light fields to fixation."
  (let ((visited
          (+ (propagate-light-region
              region #'light-region-entry-sky
              (seed-region-sky-boundaries region :scheduling scheduling) t)
             (propagate-light-region
              region #'light-region-entry-block
              (seed-region-emitters region :scheduling scheduling) nil))))
    (values region visited)))

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
            (let ((field (make-chunk-light-field (length new-sky))))
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
  "From-scratch reference relight of WORLD's resident chunks.

Returns the chunks whose published light changed.  This is the oracle the
incremental runtime relighter is checked against, and a recovery path when
incremental state is suspect."
  (publish-light-region
   (solve-light-region (capture-light-region world))))

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

;;; The incremental runtime relighter.  Content edits and residency events
;;; accumulate in a LUVCRAFT-LIGHTING-STATE through the world's hooks; a
;;; reconcile pass runs removal then addition queues over a lazily captured
;;; candidate and publishes complete fields through the same
;;; PUBLISH-LIGHT-REGION transaction as the reference solver.
;;;
;;; Removal deliberately over-removes: any cell whose value could have been
;;; fed by a removed cell is cleared, and every brighter survivor met along
;;; the way re-enters the addition queue, so the max fixpoint is restored
;;; from true sources only.

(defun unlight-light-region (region queue sources)
  "Clear light reachable from LIGHT-REMOVAL records owned by QUEUE.

Marks cleared cells in QUEUE's removed table, pushes surviving brighter cells
onto the packed addition worklist SOURCES, and returns the number of removal
steps performed."
  (let ((field-reader (light-removal-queue-field-reader queue))
        (skylight-p (light-removal-queue-skylight-p queue))
        (removed (light-removal-queue-removed queue))
        (worklist (light-removal-queue-worklist queue))
        (visited 0))
    (loop until (light-worklist-empty-p worklist)
          do (multiple-value-bind (entry source-offset level present-p)
                 (light-worklist-pop worklist)
               (declare (ignore present-p))
               (let* ((domain (light-region-entry-domain entry))
                      (local
                        (chunk-domain-local-coordinate domain source-offset)))
                 (declare (dynamic-extent local))
                 (incf visited)
                 (do-chunk-window-neighbors
                     (offset destination crossing direction
                      materialization availability
                      region domain local *voxel-face-directions*)
                   (let ((neighbor
                           (ecase availability
                             (:local entry)
                             (:available materialization)
                             (:unavailable nil))))
                     (when neighbor
                       (let* ((levels (funcall field-reader neighbor))
                              (value (aref levels offset)))
                         (when (plusp value)
                           (if (or (< value level)
                                   (and skylight-p
                                        (eq direction +voxel-negative-y+)
                                        (= value level)))
                               (let ((coordinate
                                       (chunk-domain-world-coordinate
                                        (light-region-entry-domain neighbor)
                                        destination)))
                                 (setf (aref levels offset) 0
                                       (gethash coordinate removed) t)
                                 (enqueue-light-removal
                                  queue neighbor offset value))
                               (light-worklist-push
                                sources neighbor offset value))))))))))
    visited))

(defun reseed-cell-open-boundaries (region coordinate enqueue)
  "Re-admit boundary sky at one cell which may sit on an open face."
  (multiple-value-bind (entry offset)
      (light-region-locate region coordinate)
    (when entry
      (let* ((domain (light-region-entry-domain entry))
             (key (light-region-entry-key entry))
             (local (chunk-domain-local-coordinate domain offset)))
        (declare (dynamic-extent local))
        (dolist (direction *voxel-face-directions*)
          (with-chunk-domain-step (neighbor-offset neighbor crossing)
              domain local direction
            (declare (ignore neighbor-offset neighbor))
            (when (and crossing
                       (not (light-region-neighbor-resident-p
                             region key direction))
                       (eq (light-region-absent-boundary-semantics
                            region key direction)
                           :open-sky)
                       (seed-open-sky-at-offset
                        entry offset (eq direction +voxel-positive-y+)))
              (funcall enqueue entry offset
                       (aref (light-region-entry-sky entry) offset)))))))))

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

(defun seed-arrived-chunk (region key seed-sky seed-block)
  "Seed one newly resident chunk and import its neighbors' boundary light."
  (let* ((world (light-region-world region))
         (entry (multiple-value-bind (chunk present-p)
                    (world-chunk-at-coordinate world key)
                  (and present-p
                       (or (gethash key (light-region-entries region))
                           (add-light-region-entry
                            region chunk :from-field-p t))))))
    (when entry
      (seed-entry-open-boundaries region key entry seed-sky)
      ;; The chunk's own emitters.
      (let ((indices (light-region-entry-indices entry))
            (emission (light-region-entry-emission-lut entry))
            (levels (light-region-entry-block entry))
            (domain (light-region-entry-domain entry)))
        (do-chunk-domain-sites (offset local domain)
          (values local)
          (let ((level (aref emission (aref indices offset))))
            (when (> level (aref levels offset))
              (setf (aref levels offset) level)
              (funcall seed-block entry offset level)))))
      ;; Resident neighbors: their facing boundary cells re-propagate into
      ;; the newcomer.
      (dolist (direction *voxel-face-directions*)
        (let* ((neighbor-key (chunk-coordinate-neighbor key direction))
               (neighbor-entry
                 (multiple-value-bind (neighbor-chunk present-p)
                     (world-chunk-at-coordinate world neighbor-key)
                   (and present-p
                        (or (gethash neighbor-key
                                     (light-region-entries region))
                            (add-light-region-entry
                             region neighbor-chunk :from-field-p t))))))
          (declare (dynamic-extent neighbor-key))
          (when neighbor-entry
            (map-entry-face-sites
             neighbor-entry (opposite-voxel-direction direction)
             (lambda (offset local)
               (declare (ignore local))
               (funcall seed-sky neighbor-entry offset
                        (aref (light-region-entry-sky neighbor-entry) offset))
               (funcall seed-block neighbor-entry offset
                        (aref (light-region-entry-block neighbor-entry)
                              offset))))))))))

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
         (sky-removals
           (make-light-removal-queue
            :sky-light #'light-region-entry-sky :skylight-p t))
         (block-removals
           (make-light-removal-queue
            :block-light #'light-region-entry-block))
         (sky-seeds
           (make-light-worklist
            :scheduling :level
            :field-definition (fields:field-definition-for :sky-light)))
         (block-seeds
           (make-light-worklist
            :scheduling :level
            :field-definition (fields:field-definition-for :block-light)))
         (visited 0))
    (flet ((seed-sky (entry offset level)
             (light-worklist-push sky-seeds entry offset level))
           (seed-block (entry offset level)
             (light-worklist-push block-seeds entry offset level)))
      ;; Dirty cells lose both their values; removal rediscovers what they
      ;; fed, and surviving neighbors return as addition sources.
      (maphash
       (lambda (cell present)
         (declare (ignore present))
         (multiple-value-bind (entry offset)
             (light-region-locate region cell)
           (when entry
             (let ((old-sky (aref (light-region-entry-sky entry) offset))
                   (old-block
                     (aref (light-region-entry-block entry) offset)))
               (setf (aref (light-region-entry-sky entry) offset) 0
                     (aref (light-region-entry-block entry) offset) 0
                     (gethash cell
                              (light-removal-queue-removed sky-removals)) t
                     (gethash cell
                              (light-removal-queue-removed block-removals)) t)
               (enqueue-light-removal sky-removals entry offset old-sky)
               (enqueue-light-removal block-removals entry offset old-block)))))
       (lighting-state-dirty-cells state))
      ;; A departed neighbor may have been feeding the retained faces.
      (maphash
       (lambda (key present)
         (declare (ignore present))
         (dolist (direction *voxel-face-directions*)
           (let* ((neighbor-key (chunk-coordinate-neighbor key direction))
                  (entry
                    (multiple-value-bind (chunk present-p)
                        (world-chunk-at-coordinate world neighbor-key)
                      (and present-p
                           (or (gethash neighbor-key
                                        (light-region-entries region))
                               (add-light-region-entry
                                region chunk :from-field-p t))))))
             (declare (dynamic-extent neighbor-key))
             (when entry
               (map-entry-face-sites
                entry (opposite-voxel-direction direction)
                (lambda (offset local)
                  (let ((old-sky
                          (aref (light-region-entry-sky entry) offset))
                        (old-block
                          (aref (light-region-entry-block entry) offset)))
                    (when (or (plusp old-sky) (plusp old-block))
                      (let ((coordinate
                              (chunk-domain-world-coordinate
                               (light-region-entry-domain entry) local)))
                        (when (plusp old-sky)
                          (setf (aref (light-region-entry-sky entry) offset) 0
                                (gethash coordinate
                                         (light-removal-queue-removed
                                          sky-removals)) t)
                          (enqueue-light-removal
                           sky-removals entry offset old-sky))
                        (when (plusp old-block)
                          (setf (aref (light-region-entry-block entry) offset) 0
                                (gethash coordinate
                                         (light-removal-queue-removed
                                          block-removals)) t)
                          (enqueue-light-removal
                           block-removals entry offset old-block)))))))))))
       (lighting-state-departures state))
      ;; Removal to exhaustion, collecting surviving sources.
      (incf visited
            (unlight-light-region region sky-removals sky-seeds))
      (incf visited
            (unlight-light-region region block-removals block-seeds))
      ;; Every cleared cell whose block emits re-seeds its own emission:
      ;; edited cells pick up a newly placed emitter, and removal waves
      ;; which swept over an untouched emitter must not extinguish it.
      (maphash
       (lambda (cell present)
         (declare (ignore present))
         (multiple-value-bind (entry offset)
             (light-region-locate region cell)
           (when entry
             (let ((level (aref (light-region-entry-emission-lut entry)
                                (aref (light-region-entry-indices entry)
                                      offset))))
               (when (> level
                        (aref (light-region-entry-block entry) offset))
                 (setf (aref (light-region-entry-block entry) offset)
                       level)
                 (seed-block entry offset level))))))
       (light-removal-queue-removed block-removals))
      ;; Cells cleared by sky removal may sit on an open boundary whose
      ;; direct light must be re-admitted.
      (maphash (lambda (cell present)
                 (declare (ignore present))
                 (reseed-cell-open-boundaries region cell #'seed-sky))
               (light-removal-queue-removed sky-removals))
      ;; Arrivals solve their own chunk and import neighbor boundaries.
      (maphash (lambda (key present)
                 (declare (ignore present))
                 (seed-arrived-chunk region key #'seed-sky #'seed-block))
               (lighting-state-arrivals state))
      ;; Addition to the max fixpoint.
      (incf visited
            (propagate-light-region
             region #'light-region-entry-sky sky-seeds t))
      (incf visited
            (propagate-light-region
             region #'light-region-entry-block block-seeds nil)))
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
