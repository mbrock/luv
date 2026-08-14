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

(in-package #:luv)

(defconstant +maximum-light-level+ 15)

(defclass chunk-light-field ()
  ((sky-levels :initarg :sky-levels :reader chunk-light-field-sky-levels)
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

(defun make-chunk-light-field (cardinality)
  (flet ((levels ()
           (make-array cardinality :element-type '(unsigned-byte 8)
                                   :initial-element 0)))
    (make-instance 'chunk-light-field
                   :sky-levels (levels) :block-levels (levels))))

(defun chunk-light-field-boundary-revision (field dx dy dz)
  (aref (chunk-light-field-boundary-revisions field)
        (chunk-boundary-index dx dy dz)))

;;; What a missing resident neighbor means is a world/source decision.  The
;;; solver never equates :ABSENT with open sky on its own.

(defgeneric absent-chunk-light-semantics (source world chunk-key direction)
  (:documentation
   "How light should treat the absent neighbor of CHUNK-KEY in DIRECTION.

Return :OPEN-SKY for a boundary known to see the sky, :CLOSED for a boundary
known to be outside the world, or :UNKNOWN for terrain which merely is not
resident.  DIRECTION is a (dx dy dz) unit list in chunk coordinates."))

(defmethod absent-chunk-light-semantics ((source t) world chunk-key direction)
  (declare (ignore world chunk-key direction))
  :unknown)

(defmethod absent-chunk-light-semantics
    ((source little-world-source) world chunk-key direction)
  ;; The little generated world declares open sky above its known vertical
  ;; extent and a closed floor beneath it.  Lateral terrain is generatable
  ;; but simply not resident, which is exactly :UNKNOWN.
  (declare (ignore world chunk-key))
  (destructuring-bind (dx dy dz) direction
    (declare (ignore dx dz))
    (cond ((plusp dy) :open-sky)
          ((minusp dy) :closed)
          (t :unknown))))

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
  (key nil)
  (origin-x 0 :type integer)
  (origin-y 0 :type integer)
  (origin-z 0 :type integer)
  (indices nil :type (or null (simple-array (unsigned-byte 16) (*))))
  (opacity-lut nil)
  (emission-lut nil)
  (sky nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (block nil :type (or null (simple-array (unsigned-byte 8) (*)))))

(defstruct (light-region (:constructor %make-light-region))
  (world nil)
  (width 16 :type (integer 1))
  (height 16 :type (integer 1))
  (depth 16 :type (integer 1))
  (entries (make-hash-table :test #'equal) :type hash-table)
  ;; A from-scratch capture enumerates its entries eagerly.  An incremental
  ;; candidate instead materializes entries on first touch, initialized from
  ;; the chunk's current published field, so propagation may wander into any
  ;; resident chunk without precomputing the affected set.
  (ensure-entry nil :type (or null function)))

(defun add-light-region-entry (region chunk &key from-field-p)
  "Materialize CHUNK's dense capture in REGION and return the new entry."
  (with-block-content-storage (domain palette indices) chunk
    (multiple-value-bind (opacity emission)
        (block-palette-light-tables palette)
      (let* ((origin (chunk-domain-origin domain))
             (key (block-chunk-key chunk))
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
               :chunk chunk :key key
               :origin-x (world-coordinate-x origin)
               :origin-y (world-coordinate-y origin)
               :origin-z (world-coordinate-z origin)
               :indices (coerce indices
                                '(simple-array (unsigned-byte 16) (*)))
               :opacity-lut opacity :emission-lut emission
               :sky sky :block block-levels))))))

(defun capture-light-region (world)
  "Capture every resident chunk of WORLD for a from-scratch relight."
  (let* ((shape (voxel-space-chunk-shape (block-world-space world)))
         (region (%make-light-region
                  :world world
                  :width (chunk-shape-width shape)
                  :height (chunk-shape-height shape)
                  :depth (chunk-shape-depth shape))))
    (dolist (chunk (resident-world-chunks world))
      (add-light-region-entry region chunk))
    region))

(defun make-light-candidate (world)
  "A lazily populated region whose entries start from current fields."
  (let ((shape (voxel-space-chunk-shape (block-world-space world))))
    (%make-light-region
     :world world
     :width (chunk-shape-width shape)
     :height (chunk-shape-height shape)
     :depth (chunk-shape-depth shape)
     :ensure-entry
     (lambda (region key)
       (multiple-value-bind (chunk present-p)
           (apply #'world-chunk-at world key)
         (when present-p
           (add-light-region-entry region chunk :from-field-p t)))))))

(declaim (inline light-region-locate))
(defun light-region-locate (region x y z)
  "Resolve world coordinates to (VALUES ENTRY OFFSET) or NIL when absent."
  (multiple-value-bind (chunk-x local-x) (floor x (light-region-width region))
    (multiple-value-bind (chunk-y local-y)
        (floor y (light-region-height region))
      (multiple-value-bind (chunk-z local-z)
          (floor z (light-region-depth region))
        (let* ((key (list chunk-x chunk-y chunk-z))
               (entry (or (gethash key (light-region-entries region))
                          (let ((ensure (light-region-ensure-entry region)))
                            (and ensure (funcall ensure region key))))))
          (when entry
            (values entry
                    (+ local-x
                       (* (light-region-width region)
                          (+ local-y
                             (* (light-region-height region)
                                local-z)))))))))))

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
  "Run FIELD's max-fixpoint BFS from the seeded QUEUE of (x y z) conses.

Returns the number of cells visited, for the runtime's work counters."
  (let ((visited 0))
    (loop while queue
          do (destructuring-bind (x y z) (pop queue)
               (incf visited)
               (multiple-value-bind (entry offset)
                   (light-region-locate region x y z)
                 (when entry
                   (let ((level (aref (funcall field-reader entry) offset)))
                     (when (plusp level)
                       (loop for (dx dy dz) in *chunk-neighbor-directions*
                             do (let ((nx (+ x dx))
                                      (ny (+ y dy))
                                      (nz (+ z dz)))
                                  (multiple-value-bind
                                        (neighbor neighbor-offset)
                                      (light-region-locate region nx ny nz)
                                    (when neighbor
                                      (let* ((opacity
                                               (light-region-opacity
                                                neighbor neighbor-offset))
                                             (loss
                                               (if (and skylight-p (= dy -1))
                                                   opacity
                                                   (+ 1 opacity)))
                                             (candidate (- level loss))
                                             (levels (funcall field-reader
                                                              neighbor)))
                                        (when (> candidate
                                                 (aref levels
                                                       neighbor-offset))
                                          (setf (aref levels neighbor-offset)
                                                candidate)
                                          (push (list nx ny nz)
                                                queue)))))))))))))
    visited))

(defun chunk-neighbor-resident-p (world key direction)
  (destructuring-bind (dx dy dz) direction
    (nth-value 1 (world-chunk-at world
                                 (+ (first key) dx)
                                 (+ (second key) dy)
                                 (+ (third key) dz)))))

(defun light-region-provisional-p (region key)
  "Whether chunk KEY currently borders any :UNKNOWN residency boundary."
  (let* ((world (light-region-world region))
         (source (block-world-source world)))
    (loop for direction in *chunk-neighbor-directions*
          thereis (and (not (chunk-neighbor-resident-p world key direction))
                       (eq (absent-chunk-light-semantics
                            source world key direction)
                           :unknown)))))

(defun map-entry-face-cells (region entry dx dy dz function)
  "Call FUNCTION with the world coordinates of each cell on one face."
  (flet ((limits (delta extent origin)
           (cond ((= delta -1) (values origin origin))
                 ((= delta 1)
                  (values (+ origin extent -1) (+ origin extent -1)))
                 (t (values origin (+ origin extent -1))))))
    (multiple-value-bind (x-low x-high)
        (limits dx (light-region-width region)
                (light-region-entry-origin-x entry))
      (multiple-value-bind (y-low y-high)
          (limits dy (light-region-height region)
                  (light-region-entry-origin-y entry))
        (multiple-value-bind (z-low z-high)
            (limits dz (light-region-depth region)
                    (light-region-entry-origin-z entry))
          (loop for z from z-low to z-high do
            (loop for y from y-low to y-high do
              (loop for x from x-low to x-high do
                (funcall function x y z)))))))))

(defun seed-open-sky-cell (region x y z downward-p)
  "Admit boundary sky into one cell; return its coordinates when it grew."
  (multiple-value-bind (entry offset) (light-region-locate region x y z)
    (when entry
      (let* ((opacity (light-region-opacity entry offset))
             (level (- +maximum-light-level+
                       (if downward-p opacity (+ 1 opacity)))))
        (when (> level (aref (light-region-entry-sky entry) offset))
          (setf (aref (light-region-entry-sky entry) offset) level)
          (list x y z))))))

(defun seed-entry-open-boundaries (region key entry enqueue)
  "Seed ENTRY's faces whose absent neighbors are known open sky."
  (let* ((world (light-region-world region))
         (source (block-world-source world)))
    (dolist (direction *chunk-neighbor-directions*)
      (destructuring-bind (dx dy dz) direction
        (when (and (not (chunk-neighbor-resident-p world key direction))
                   (eq (absent-chunk-light-semantics
                        source world key direction)
                       :open-sky))
          (map-entry-face-cells
           region entry dx dy dz
           (lambda (x y z)
             (let ((seed (seed-open-sky-cell region x y z (= dy 1))))
               (when seed (funcall enqueue seed))))))))))

(defun seed-region-sky-boundaries (region)
  "Seed every entry's open-sky boundary light; return the seed queue."
  (let ((queue nil))
    (maphash (lambda (key entry)
               (seed-entry-open-boundaries
                region key entry (lambda (seed) (push seed queue))))
             (light-region-entries region))
    queue))

(defun seed-region-emitters (region)
  "Seed every emitting cell at its configured blocklight level."
  (let ((queue nil))
    (maphash
     (lambda (key entry)
       (declare (ignore key))
       (let ((indices (light-region-entry-indices entry))
             (emission (light-region-entry-emission-lut entry))
             (levels (light-region-entry-block entry))
             (width (light-region-width region))
             (height (light-region-height region))
             (origin-x (light-region-entry-origin-x entry))
             (origin-y (light-region-entry-origin-y entry))
             (origin-z (light-region-entry-origin-z entry)))
         (dotimes (offset (length indices))
           (let ((level (aref emission (aref indices offset))))
             (when (plusp level)
               (setf (aref levels offset) level)
               (multiple-value-bind (z remainder)
                   (floor offset (* width height))
                 (multiple-value-bind (y x) (floor remainder width)
                   (push (list (+ origin-x x)
                               (+ origin-y y)
                               (+ origin-z z))
                         queue))))))))
     (light-region-entries region))
    queue))

(defun solve-light-region (region)
  "Seed and propagate both light fields to fixation."
  (propagate-light-region
   region #'light-region-entry-sky (seed-region-sky-boundaries region) t)
  (propagate-light-region
   region #'light-region-entry-block (seed-region-emitters region) nil)
  region)

;;; Publication compares complete candidate arrays against the chunk's
;;; current field and advances light revisions only: content revisions and
;;; the world revision are authored-data facts this derived domain must not
;;; touch.

(defun light-boundary-plane-changed-p
    (old-levels new-levels width height depth face)
  (flet ((changed-in-range (x-low x-high y-low y-high z-low z-high)
           (loop for z from z-low to z-high
                 thereis
                 (loop for y from y-low to y-high
                       thereis
                       (loop for x from x-low to x-high
                             for offset = (+ x (* width (+ y (* height z))))
                             thereis (/= (aref old-levels offset)
                                         (aref new-levels offset)))))))
    (ecase face
      (0 (changed-in-range 0 0 0 (1- height) 0 (1- depth)))
      (1 (changed-in-range (1- width) (1- width) 0 (1- height) 0 (1- depth)))
      (2 (changed-in-range 0 (1- width) 0 0 0 (1- depth)))
      (3 (changed-in-range 0 (1- width) (1- height) (1- height) 0 (1- depth)))
      (4 (changed-in-range 0 (1- width) 0 (1- height) 0 0))
      (5 (changed-in-range 0 (1- width) 0 (1- height)
                           (1- depth) (1- depth))))))

(defun publish-light-region (region)
  "Install every changed candidate field; return the changed chunks."
  (let ((width (light-region-width region))
        (height (light-region-height region))
        (depth (light-region-depth region))
        (changed nil))
    (maphash
     (lambda (key entry)
       (let* ((chunk (light-region-entry-chunk entry))
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
                  (dotimes (face 6)
                    (when (or (light-boundary-plane-changed-p
                               old-sky new-sky width height depth face)
                              (light-boundary-plane-changed-p
                               old-block new-block width height depth face))
                      (incf (aref revisions face)))))
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

(defun chunk-light-levels-at (chunk x y z)
  "Return (VALUES SKY BLOCK STATE) for CHUNK-local coordinates."
  (let ((field (block-chunk-light-field chunk)))
    (if field
        (let ((offset (chunk-domain-offset-components
                       (block-chunk-domain chunk) x y z)))
          (values (aref (chunk-light-field-sky-levels field) offset)
                  (aref (chunk-light-field-block-levels field) offset)
                  (chunk-light-field-state field)))
        (values 0 0 :unlit))))

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

(defun unlight-light-region (region field-reader queue skylight-p removed)
  "Clear light reachable from QUEUE of (x y z level) removal records.

Marks cleared cells in the REMOVED table and returns (VALUES SOURCES
VISITED): surviving brighter cells to re-propagate from, and the number of
removal steps performed."
  (let ((sources nil)
        (visited 0))
    (loop while queue
          do (destructuring-bind (x y z level) (pop queue)
               (incf visited)
               (loop for (dx dy dz) in *chunk-neighbor-directions*
                     do (let ((nx (+ x dx)) (ny (+ y dy)) (nz (+ z dz)))
                          (multiple-value-bind (neighbor offset)
                              (light-region-locate region nx ny nz)
                            (when neighbor
                              (let* ((levels (funcall field-reader neighbor))
                                     (value (aref levels offset)))
                                (when (plusp value)
                                  (if (or (< value level)
                                          (and skylight-p (= dy -1)
                                               (= value level)))
                                      (progn
                                        (setf (aref levels offset) 0)
                                        (setf (gethash (list nx ny nz)
                                                       removed)
                                              t)
                                        (push (list nx ny nz value) queue))
                                      (push (list nx ny nz)
                                            sources))))))))))
    (values sources visited)))

(defun reseed-cell-open-boundaries (region x y z enqueue)
  "Re-admit boundary sky at one cell which may sit on an open face."
  (multiple-value-bind (entry offset) (light-region-locate region x y z)
    (declare (ignore offset))
    (when entry
      (let* ((width (light-region-width region))
             (height (light-region-height region))
             (depth (light-region-depth region))
             (local-x (- x (light-region-entry-origin-x entry)))
             (local-y (- y (light-region-entry-origin-y entry)))
             (local-z (- z (light-region-entry-origin-z entry)))
             (world (light-region-world region))
             (source (block-world-source world))
             (key (light-region-entry-key entry)))
        (dolist (direction *chunk-neighbor-directions*)
          (destructuring-bind (dx dy dz) direction
            (when (and (or (and (= dx -1) (= local-x 0))
                           (and (= dx 1) (= local-x (1- width)))
                           (and (= dy -1) (= local-y 0))
                           (and (= dy 1) (= local-y (1- height)))
                           (and (= dz -1) (= local-z 0))
                           (and (= dz 1) (= local-z (1- depth))))
                       (not (chunk-neighbor-resident-p world key direction))
                       (eq (absent-chunk-light-semantics
                            source world key direction)
                           :open-sky))
              (let ((seed (seed-open-sky-cell region x y z (= dy 1))))
                (when seed (funcall enqueue seed))))))))))

(defclass luvcraft-lighting-state ()
  ((world :initarg :world :reader lighting-state-world)
   (dirty-cells :initform (make-hash-table :test #'equal)
                :reader lighting-state-dirty-cells)
   (arrivals :initform (make-hash-table :test #'equal)
             :reader lighting-state-arrivals)
   (departures :initform (make-hash-table :test #'equal)
               :reader lighting-state-departures)
   ;; Work counters, exposed so performance claims come from measurements.
   (cells-visited :initform 0 :accessor lighting-state-cells-visited)
   (chunks-touched :initform 0 :accessor lighting-state-chunks-touched)
   (publications :initform 0 :accessor lighting-state-publications)
   (last-latency-seconds :initform 0d0
                         :accessor lighting-state-last-latency-seconds))
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
      (setf (gethash (block-chunk-key chunk)
                     (lighting-state-arrivals state))
            t))
    (setf (block-world-cell-change-hook world)
          (lambda (chunk x y z)
            (declare (ignore chunk))
            (setf (gethash (list x y z)
                           (lighting-state-dirty-cells state))
                  t))
          (block-world-residency-change-hook world)
          (lambda (x y z event)
            (let ((key (list x y z)))
              (ecase event
                (:arrived
                 (setf (gethash key (lighting-state-arrivals state)) t)
                 (remhash key (lighting-state-departures state)))
                (:departed
                 (setf (gethash key (lighting-state-departures state)) t)
                 (remhash key (lighting-state-arrivals state)))))))
    state))

(defun lighting-state-dirty-p (state)
  (or (plusp (hash-table-count (lighting-state-dirty-cells state)))
      (plusp (hash-table-count (lighting-state-arrivals state)))
      (plusp (hash-table-count (lighting-state-departures state)))))

(defun seed-arrived-chunk (region key seed-sky seed-block)
  "Seed one newly resident chunk and import its neighbors' boundary light."
  (let* ((world (light-region-world region))
         (entry (multiple-value-bind (chunk present-p)
                    (apply #'world-chunk-at world key)
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
            (width (light-region-width region))
            (height (light-region-height region)))
        (dotimes (offset (length indices))
          (let ((level (aref emission (aref indices offset))))
            (when (> level (aref levels offset))
              (setf (aref levels offset) level)
              (multiple-value-bind (z remainder)
                  (floor offset (* width height))
                (multiple-value-bind (y x) (floor remainder width)
                  (funcall seed-block
                           (list (+ (light-region-entry-origin-x entry) x)
                                 (+ (light-region-entry-origin-y entry) y)
                                 (+ (light-region-entry-origin-z entry)
                                    z)))))))))
      ;; Resident neighbors: their facing boundary cells re-propagate into
      ;; the newcomer.
      (dolist (direction *chunk-neighbor-directions*)
        (destructuring-bind (dx dy dz) direction
          (let* ((neighbor-key (list (+ (first key) dx)
                                     (+ (second key) dy)
                                     (+ (third key) dz)))
                 (neighbor-entry
                   (multiple-value-bind (neighbor-chunk present-p)
                       (apply #'world-chunk-at world neighbor-key)
                     (and present-p
                          (or (gethash neighbor-key
                                       (light-region-entries region))
                              (add-light-region-entry
                               region neighbor-chunk :from-field-p t))))))
            (when neighbor-entry
              (map-entry-face-cells
               region neighbor-entry (- dx) (- dy) (- dz)
               (lambda (cell-x cell-y cell-z)
                 (funcall seed-sky (list cell-x cell-y cell-z))
                 (funcall seed-block
                          (list cell-x cell-y cell-z)))))))))))

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
         (sky-removals nil) (block-removals nil)
         (sky-removed (make-hash-table :test #'equal))
         (block-removed (make-hash-table :test #'equal))
         (sky-seeds nil) (block-seeds nil)
         (visited 0))
    (flet ((seed-sky (cell) (push cell sky-seeds))
           (seed-block (cell) (push cell block-seeds)))
      ;; Dirty cells lose both their values; removal rediscovers what they
      ;; fed, and surviving neighbors return as addition sources.
      (maphash
       (lambda (cell present)
         (declare (ignore present))
         (destructuring-bind (x y z) cell
           (multiple-value-bind (entry offset)
               (light-region-locate region x y z)
             (when entry
               (let ((old-sky (aref (light-region-entry-sky entry) offset))
                     (old-block
                       (aref (light-region-entry-block entry) offset)))
                 (setf (aref (light-region-entry-sky entry) offset) 0
                       (aref (light-region-entry-block entry) offset) 0
                       (gethash cell sky-removed) t
                       (gethash cell block-removed) t)
                 (push (list x y z old-sky) sky-removals)
                 (push (list x y z old-block) block-removals))))))
       (lighting-state-dirty-cells state))
      ;; A departed neighbor may have been feeding the retained faces.
      (maphash
       (lambda (key present)
         (declare (ignore present))
         (dolist (direction *chunk-neighbor-directions*)
           (destructuring-bind (dx dy dz) direction
             (let* ((neighbor-key (list (+ (first key) dx)
                                        (+ (second key) dy)
                                        (+ (third key) dz)))
                    (entry
                      (multiple-value-bind (chunk present-p)
                          (apply #'world-chunk-at world neighbor-key)
                        (and present-p
                             (or (gethash neighbor-key
                                          (light-region-entries region))
                                 (add-light-region-entry
                                  region chunk :from-field-p t))))))
               (when entry
                 (map-entry-face-cells
                  region entry (- dx) (- dy) (- dz)
                  (lambda (x y z)
                    (multiple-value-bind (cell-entry offset)
                        (light-region-locate region x y z)
                      (declare (ignore cell-entry))
                      (let ((old-sky
                              (aref (light-region-entry-sky entry) offset))
                            (old-block
                              (aref (light-region-entry-block entry)
                                    offset)))
                        (when (plusp old-sky)
                          (setf (aref (light-region-entry-sky entry)
                                      offset)
                                0
                                (gethash (list x y z) sky-removed) t)
                          (push (list x y z old-sky) sky-removals))
                        (when (plusp old-block)
                          (setf (aref (light-region-entry-block entry)
                                      offset)
                                0
                                (gethash (list x y z) block-removed) t)
                          (push (list x y z old-block)
                                block-removals)))))))))))
       (lighting-state-departures state))
      ;; Removal to exhaustion, collecting surviving sources.
      (multiple-value-bind (sources count)
          (unlight-light-region
           region #'light-region-entry-sky sky-removals t sky-removed)
        (setf sky-seeds (nconc sources sky-seeds))
        (incf visited count))
      (multiple-value-bind (sources count)
          (unlight-light-region
           region #'light-region-entry-block block-removals nil
           block-removed)
        (setf block-seeds (nconc sources block-seeds))
        (incf visited count))
      ;; Every cleared cell whose block emits re-seeds its own emission:
      ;; edited cells pick up a newly placed emitter, and removal waves
      ;; which swept over an untouched emitter must not extinguish it.
      (maphash
       (lambda (cell present)
         (declare (ignore present))
         (destructuring-bind (x y z) cell
           (multiple-value-bind (entry offset)
               (light-region-locate region x y z)
             (when entry
               (let ((level (aref (light-region-entry-emission-lut entry)
                                  (aref (light-region-entry-indices entry)
                                        offset))))
                 (when (> level
                          (aref (light-region-entry-block entry) offset))
                   (setf (aref (light-region-entry-block entry) offset)
                         level)
                   (seed-block cell)))))))
       block-removed)
      ;; Cells cleared by sky removal may sit on an open boundary whose
      ;; direct light must be re-admitted.
      (maphash (lambda (cell present)
                 (declare (ignore present))
                 (destructuring-bind (x y z) cell
                   (reseed-cell-open-boundaries region x y z #'seed-sky)))
               sky-removed)
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

(defun world-light-at (world x y z)
  "Return (VALUES SKY BLOCK STATE) at a world coordinate, or zeros when
the chunk is absent or unlit."
  (multiple-value-bind (world-coordinate chunk-coordinate local)
      (locate-world-coordinate world x y z)
    (declare (ignore world-coordinate))
    (multiple-value-bind (chunk present-p)
        (world-chunk-at world
                        (chunk-coordinate-x chunk-coordinate)
                        (chunk-coordinate-y chunk-coordinate)
                        (chunk-coordinate-z chunk-coordinate))
      (if present-p
          (chunk-light-levels-at chunk
                                 (local-coordinate-x local)
                                 (local-coordinate-y local)
                                 (local-coordinate-z local))
          (values 0 0 :absent)))))
