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
  (block nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (unknown-boundary-p nil))

(defstruct (light-region (:constructor %make-light-region))
  (world nil)
  (width 16 :type (integer 1))
  (height 16 :type (integer 1))
  (depth 16 :type (integer 1))
  (entries (make-hash-table :test #'equal) :type hash-table))

(defun capture-light-region (world)
  "Capture every resident chunk of WORLD for a from-scratch relight."
  (let* ((shape (voxel-space-chunk-shape (block-world-space world)))
         (width (chunk-shape-width shape))
         (height (chunk-shape-height shape))
         (depth (chunk-shape-depth shape))
         (region (%make-light-region
                  :world world :width width :height height :depth depth))
         (cardinality (* width height depth)))
    (dolist (chunk (resident-world-chunks world))
      (with-block-content-storage (domain palette indices) chunk
        (multiple-value-bind (opacity emission)
            (block-palette-light-tables palette)
          (let* ((origin (chunk-domain-origin domain))
                 (key (block-chunk-key chunk)))
            (setf (gethash key (light-region-entries region))
                  (%make-light-region-entry
                   :chunk chunk :key key
                   :origin-x (world-coordinate-x origin)
                   :origin-y (world-coordinate-y origin)
                   :origin-z (world-coordinate-z origin)
                   :indices (coerce indices
                                    '(simple-array (unsigned-byte 16) (*)))
                   :opacity-lut opacity :emission-lut emission
                   :sky (make-array cardinality
                                    :element-type '(unsigned-byte 8)
                                    :initial-element 0)
                   :block (make-array cardinality
                                      :element-type '(unsigned-byte 8)
                                      :initial-element 0)))))))
    region))

(declaim (inline light-region-locate))
(defun light-region-locate (region x y z)
  "Resolve world coordinates to (VALUES ENTRY OFFSET) or NIL when absent."
  (multiple-value-bind (chunk-x local-x) (floor x (light-region-width region))
    (multiple-value-bind (chunk-y local-y)
        (floor y (light-region-height region))
      (multiple-value-bind (chunk-z local-z)
          (floor z (light-region-depth region))
        (let ((entry (gethash (list chunk-x chunk-y chunk-z)
                              (light-region-entries region))))
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
  "Run FIELD's max-fixpoint BFS from the seeded QUEUE of (x y z) conses."
  (loop while queue
        do (destructuring-bind (x y z) (pop queue)
             (multiple-value-bind (entry offset)
                 (light-region-locate region x y z)
               (when entry
                 (let ((level (aref (funcall field-reader entry) offset)))
                   (when (plusp level)
                     (loop for (dx dy dz) in *chunk-neighbor-directions*
                           do (let ((nx (+ x dx)) (ny (+ y dy)) (nz (+ z dz)))
                                (multiple-value-bind (neighbor neighbor-offset)
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
                                               (aref levels neighbor-offset))
                                        (setf (aref levels neighbor-offset)
                                              candidate)
                                        (push (list nx ny nz)
                                              queue)))))))))))))
  region)

(defun seed-region-sky-boundaries (region)
  "Seed open-sky boundary light and record unknown boundaries; return seeds."
  (let* ((world (light-region-world region))
         (source (block-world-source world))
         (width (light-region-width region))
         (height (light-region-height region))
         (depth (light-region-depth region))
         (queue nil))
    (flet ((seed-face-cell (entry x y z downward-p)
             ;; Light crossing into a boundary cell from a known open sky.
             (multiple-value-bind (cell-entry offset)
                 (light-region-locate region x y z)
               (declare (ignore cell-entry))
               (let* ((opacity (light-region-opacity entry offset))
                      (level (- +maximum-light-level+
                                (if downward-p opacity (+ 1 opacity)))))
                 (when (> level
                          (aref (light-region-entry-sky entry) offset))
                   (setf (aref (light-region-entry-sky entry) offset) level)
                   (push (list x y z) queue))))))
      (maphash
       (lambda (key entry)
         (destructuring-bind (chunk-x chunk-y chunk-z) key
           (dolist (direction *chunk-neighbor-directions*)
             (destructuring-bind (dx dy dz) direction
               (let ((neighbor-key (list (+ chunk-x dx)
                                         (+ chunk-y dy)
                                         (+ chunk-z dz))))
                 (unless (gethash neighbor-key (light-region-entries region))
                   (ecase (absent-chunk-light-semantics
                           source world key direction)
                     (:open-sky
                      (let ((origin-x (light-region-entry-origin-x entry))
                            (origin-y (light-region-entry-origin-y entry))
                            (origin-z (light-region-entry-origin-z entry)))
                        (flet ((face-limits (delta extent origin)
                                 (cond ((= delta -1) (list origin origin))
                                       ((= delta 1)
                                        (list (+ origin extent -1)
                                              (+ origin extent -1)))
                                       (t (list origin
                                                (+ origin extent -1))))))
                          (destructuring-bind (x-low x-high)
                              (face-limits dx width origin-x)
                            (destructuring-bind (y-low y-high)
                                (face-limits dy height origin-y)
                              (destructuring-bind (z-low z-high)
                                  (face-limits dz depth origin-z)
                                (loop for z from z-low to z-high do
                                  (loop for y from y-low to y-high do
                                    (loop for x from x-low to x-high do
                                      (seed-face-cell
                                       entry x y z (= dy 1)))))))))))
                     (:closed nil)
                     (:unknown
                      (setf (light-region-entry-unknown-boundary-p entry)
                            t)))))))))
       (light-region-entries region)))
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
       (declare (ignore key))
       (let* ((chunk (light-region-entry-chunk entry))
              (field (block-chunk-light-field chunk))
              (new-sky (light-region-entry-sky entry))
              (new-block (light-region-entry-block entry))
              (state (if (light-region-entry-unknown-boundary-p entry)
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
