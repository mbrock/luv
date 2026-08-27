(in-package #:luft)

;;; Width-one chunk meshing as a finite columnar query
;;;
;;; The surface-proportional mesher and the first packed-job batch remain in
;;; MESH.LISP as executable references.  This production path keeps their
;;; compiled 256-star geometry but changes the execution model:
;;;
;;;   occupancy fibers -> active-site selection -> site/material columns
;;;     -> face, band, and fan relations -> final grouped instance streams
;;;
;;; A row names ordinals and small integers, never an intermediate geometry
;;; object.  The immutable template vocabulary is compiled once from the
;;; reference tables.  Chunk jobs therefore do no template hashing, allocate
;;; no builder, and never repack a site plus a material offset into separate
;;; face/edge/vertex job words only to decode them in the next phase.

(defstruct (width-one-query-sites
             (:constructor %make-width-one-query-sites
                 (x y z mask material-base)))
  ;; X and Y are relative to the owner origin and include its high seam.
  (x (make-array 0 :element-type '(unsigned-byte 8))
     :type (simple-array (unsigned-byte 8) (*)))
  (y (make-array 0 :element-type '(unsigned-byte 8))
     :type (simple-array (unsigned-byte 8) (*)))
  (z (make-array 0 :element-type '(unsigned-byte 8))
     :type (simple-array (unsigned-byte 8) (*)))
  (mask (make-array 0 :element-type '(unsigned-byte 8))
        :type (simple-array (unsigned-byte 8) (*)))
  ;; Offset of this site's first dense boundary contributor.
  (material-base (make-array 0 :element-type '(unsigned-byte 32))
                 :type (simple-array (unsigned-byte 32) (*)))
  (count 0 :type fixnum))

(defstruct (width-one-query-faces
             (:constructor %make-width-one-query-faces
                 (site template material-offset)))
  (site (make-array 0 :element-type '(unsigned-byte 32))
        :type (simple-array (unsigned-byte 32) (*)))
  (template (make-array 0 :element-type '(unsigned-byte 16))
            :type (simple-array (unsigned-byte 16) (*)))
  ;; Absolute offset in the material STOCKS lane.
  (material-offset (make-array 0 :element-type '(unsigned-byte 32))
                   :type (simple-array (unsigned-byte 32) (*)))
  (count 0 :type fixnum))

(defstruct (width-one-query-patches
             (:constructor %make-width-one-query-patches
                 (site template contributor-mask ambient-star)))
  (site (make-array 0 :element-type '(unsigned-byte 32))
        :type (simple-array (unsigned-byte 32) (*)))
  (template (make-array 0 :element-type '(unsigned-byte 16))
            :type (simple-array (unsigned-byte 16) (*)))
  ;; Bits name compact per-site material slots, not radial transitions or
  ;; global cube edges.  Projection can consequently reduce one row without
  ;; consulting topology again.
  (contributor-mask (make-array 0 :element-type '(unsigned-byte 16))
                    :type (simple-array (unsigned-byte 16) (*)))
  (ambient-star (make-array 0 :element-type '(unsigned-byte 8))
                :type (simple-array (unsigned-byte 8) (*)))
  (count 0 :type fixnum))

(defstruct (width-one-query-workspace
             (:constructor %make-width-one-query-workspace
                 (sites faces bands fans stocks summaries)))
  (sites nil :type width-one-query-sites :read-only t)
  (faces nil :type width-one-query-faces :read-only t)
  (bands nil :type width-one-query-patches :read-only t)
  (fans nil :type width-one-query-patches :read-only t)
  (stocks (make-array 0 :element-type '(unsigned-byte 16))
          :type (simple-array (unsigned-byte 16) (*)))
  (summaries (make-array 0 :element-type '(unsigned-byte 16))
             :type (simple-array (unsigned-byte 16) (*))))

(defun %empty-width-one-query-workspace ()
  (%make-width-one-query-workspace
   (%make-width-one-query-sites
    (make-array 0 :element-type '(unsigned-byte 8))
    (make-array 0 :element-type '(unsigned-byte 8))
    (make-array 0 :element-type '(unsigned-byte 8))
    (make-array 0 :element-type '(unsigned-byte 8))
    (make-array 0 :element-type '(unsigned-byte 32)))
   (%make-width-one-query-faces
    (make-array 0 :element-type '(unsigned-byte 32))
    (make-array 0 :element-type '(unsigned-byte 16))
    (make-array 0 :element-type '(unsigned-byte 32)))
   (%make-width-one-query-patches
    (make-array 0 :element-type '(unsigned-byte 32))
    (make-array 0 :element-type '(unsigned-byte 16))
    (make-array 0 :element-type '(unsigned-byte 16))
    (make-array 0 :element-type '(unsigned-byte 8)))
   (%make-width-one-query-patches
    (make-array 0 :element-type '(unsigned-byte 32))
    (make-array 0 :element-type '(unsigned-byte 16))
    (make-array 0 :element-type '(unsigned-byte 16))
    (make-array 0 :element-type '(unsigned-byte 8)))
   (make-array 0 :element-type '(unsigned-byte 16))
   (make-array 0 :element-type '(unsigned-byte 16))))

(defun %query-ub8-capacity (array capacity)
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array (unsigned-byte 8) (*)) array)
           (type fixnum capacity))
  (if (>= (length array) capacity)
      array
      (make-array (max 16 capacity (* 2 (length array)))
                  :element-type '(unsigned-byte 8))))

(defun %query-ub16-capacity (array capacity)
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array (unsigned-byte 16) (*)) array)
           (type fixnum capacity))
  (if (>= (length array) capacity)
      array
      (make-array (max 16 capacity (* 2 (length array)))
                  :element-type '(unsigned-byte 16))))

(defun %query-ub32-capacity (array capacity)
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array (unsigned-byte 32) (*)) array)
           (type fixnum capacity))
  (if (>= (length array) capacity)
      array
      (make-array (max 16 capacity (* 2 (length array)))
                  :element-type '(unsigned-byte 32))))

(defun %borrow-width-one-query-workspace ()
  (if *surface-mesh-workspace*
      (or (surface-mesh-workspace-width-one-query-workspace
           *surface-mesh-workspace*)
          (setf (surface-mesh-workspace-width-one-query-workspace
                 *surface-mesh-workspace*)
                (%empty-width-one-query-workspace)))
      (%empty-width-one-query-workspace)))

;;; ---------------------------------------------------------------------------
;;; One immutable template and topology dimension

(defstruct (width-one-query-dimension
             (:constructor %make-width-one-query-dimension
                 (contributor-counts
                  face-starts face-templates face-material-slots
                  face-source-offsets
                  band-starts band-templates band-contributor-masks
                  band-source-witnesses band-ambient-stars
                  fan-starts fan-templates fan-contributor-masks)))
  "Query-native rows for the complete 256-mask topology dimension."
  (contributor-counts #() :type (simple-array (unsigned-byte 8) (*))
                          :read-only t)
  ;; STARTS is CSR-style: mask M occupies rows [STARTS[M], STARTS[M+1]).
  (face-starts #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  (face-templates #() :type (simple-array (unsigned-byte 16) (*)) :read-only t)
  (face-material-slots #() :type (simple-array (unsigned-byte 8) (*))
                            :read-only t)
  ;; Bits 0 and 1 are the X/Y distance from the lattice site to the source cell.
  (face-source-offsets #() :type (simple-array (unsigned-byte 8) (*))
                            :read-only t)
  (band-starts #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  (band-templates #() :type (simple-array (unsigned-byte 16) (*)) :read-only t)
  (band-contributor-masks #() :type (simple-array (unsigned-byte 16) (*))
                              :read-only t)
  ;; Four bits name source-cell offsets DX | (DY << 1).  A nonambient row is
  ;; owned when any named source lies in the owner's half-open cell box.
  (band-source-witnesses #() :type (simple-array (unsigned-byte 8) (*))
                              :read-only t)
  (band-ambient-stars #() :type (simple-array (unsigned-byte 8) (*))
                           :read-only t)
  (fan-starts #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  (fan-templates #() :type (simple-array (unsigned-byte 16) (*)) :read-only t)
  (fan-contributor-masks #() :type (simple-array (unsigned-byte 16) (*))
                             :read-only t))

(defstruct (width-one-query-vocabulary
             (:constructor %make-width-one-query-vocabulary
                 (dimension vertex-words-by-width ranges
                  normal-x normal-y normal-z)))
  (dimension nil :type width-one-query-dimension :read-only t)
  ;; Widths one through three share topology and instance rows.  Only the
  ;; canonical template offsets differ.  Width four collapses sheets and
  ;; therefore remains on the reference/variable-width repair path.
  (vertex-words-by-width #() :type simple-vector :read-only t)
  (ranges #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  (normal-x #() :type (simple-array (signed-byte 8) (*)) :read-only t)
  (normal-y #() :type (simple-array (signed-byte 8) (*)) :read-only t)
  (normal-z #() :type (simple-array (signed-byte 8) (*)) :read-only t))

(defun %width-one-query-face-vertices (axis-number source-bit)
  "Ask the retained scalar emitter for one canonical face template."
  (let ((builder (%make-surface-mesh-builder *width-one-table-domain* 1)))
    (%emit-width-one-face
     builder +width-one-table-site+ +width-one-table-site+
     +width-one-table-site+ axis-number source-bit 0)
    (let ((templates (surface-mesh-builder-templates builder)))
      (unless (= 1 (fill-pointer templates))
        (error "Canonical width-one face produced ~D templates."
               (fill-pointer templates)))
      (copy-seq (mesh-template-vertices (aref templates 0))))))

(defun %uniform-query-template-words (width width-one-words)
  "Scale WIDTH-ONE-WORDS to one non-medial uniform bevel WIDTH."
  (check-type width (integer 1 3))
  (let ((words (copy-seq width-one-words)))
    (loop for word from 0 below (length words)
          by +mesh-template-vertex-word-count+ do
      (dotimes (axis 3)
        (let* ((index (+ word axis))
               (offset (- (aref words index)
                          +mesh-template-coordinate-bias+))
               (scaled
                 (ecase offset
                   (-1 (- width))
                   (0 0)
                   (1 width)
                   (7 (- +mesh-cell-size+ width)))))
          (setf (aref words index)
                (+ +mesh-template-coordinate-bias+ scaled)))))
    words))

(defun %compile-width-one-query-vocabulary ()
  (let ((template-index (make-hash-table :test #'equalp))
        (templates (make-array 64 :adjustable t :fill-pointer 0))
        (face-ids (make-array 6 :element-type '(unsigned-byte 16)))
        (descriptor-ids (make-hash-table :test #'eq))
        (contributor-counts
          (make-array 256 :element-type '(unsigned-byte 8)))
        (face-starts (make-array 257 :element-type '(unsigned-byte 32)))
        (face-templates
          (make-array 768 :element-type '(unsigned-byte 16)
                          :adjustable t :fill-pointer 0))
        (face-material-slots
          (make-array 768 :element-type '(unsigned-byte 8)
                          :adjustable t :fill-pointer 0))
        (face-source-offsets
          (make-array 768 :element-type '(unsigned-byte 8)
                          :adjustable t :fill-pointer 0))
        (band-starts (make-array 257 :element-type '(unsigned-byte 32)))
        (band-templates
          (make-array 1024 :element-type '(unsigned-byte 16)
                           :adjustable t :fill-pointer 0))
        (band-contributor-masks
          (make-array 1024 :element-type '(unsigned-byte 16)
                           :adjustable t :fill-pointer 0))
        (band-source-witnesses
          (make-array 1024 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
        (band-ambient-stars
          (make-array 1024 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
        (fan-starts (make-array 257 :element-type '(unsigned-byte 32)))
        (fan-templates
          (make-array 1024 :element-type '(unsigned-byte 16)
                           :adjustable t :fill-pointer 0))
        (fan-contributor-masks
          (make-array 1024 :element-type '(unsigned-byte 16)
                           :adjustable t :fill-pointer 0)))
    (labels ((intern-template (vertices)
               (multiple-value-bind (id present-p)
                   (gethash vertices template-index)
                 (unless present-p
                   (setf id (fill-pointer templates))
                   (unless (typep id '(unsigned-byte 16))
                     (error "Width-one query template vocabulary exceeds 16 bits."))
                   (let ((copy (copy-seq vertices)))
                     (vector-push-extend copy templates)
                     (setf (gethash copy template-index) id)))
                 id))
             (record-descriptor (descriptor)
               (setf (gethash descriptor descriptor-ids)
                     (intern-template
                      (width-one-template-descriptor-vertices descriptor))))
             (descriptor-template-id (descriptor)
               (multiple-value-bind (id present-p)
                   (gethash descriptor descriptor-ids)
                 (unless present-p
                   (error "Width-one descriptor is absent from the query vocabulary."))
                 id))
             (compact-contributors (pattern cube-mask)
               (let ((compact 0)
                     (slots
                       (width-one-vertex-pattern-contributor-slots pattern)))
                 (dotimes (contributor-index 12 compact)
                   (when (logbitp contributor-index cube-mask)
                     (let ((slot (aref slots contributor-index)))
                       (when (minusp slot)
                         (error "Query primitive names absent contributor ~D."
                                contributor-index))
                       (setf compact (logior compact (ash 1 slot))))))))
             (edge-cube-contributors (transition-mask contributor-indices)
               (let ((cube-mask 0))
                 (dotimes (transition 4 cube-mask)
                   (when (logbitp transition transition-mask)
                     (let ((index (aref contributor-indices transition)))
                       (when (minusp index)
                         (error "Query edge names absent radial transition ~D."
                                transition))
                       (setf cube-mask (logior cube-mask (ash 1 index))))))))
             (source-offset (pattern contributor-index)
               (let* ((contributor
                        (aref (width-one-vertex-pattern-contributors pattern)
                              contributor-index))
                      (sample (ldb (byte 3 0) contributor)))
                 (logior (if (logbitp 0 sample) 0 1)
                         (if (logbitp 1 sample) 0 2))))
             (source-witness (pattern contributor-indices transition-mask)
               (let ((witness 0))
                 (dotimes (transition 4 witness)
                   (when (logbitp transition transition-mask)
                     (let ((index (aref contributor-indices transition)))
                       (when (minusp index)
                         (error "Query edge names absent radial transition ~D."
                                transition))
                       (setf witness
                             (logior witness
                                     (ash 1 (source-offset pattern index))))))))))
      (dotimes (axis-number 3)
        (dotimes (source-bit 2)
          (setf (aref face-ids (+ (* axis-number 2) source-bit))
                (intern-template
                 (%width-one-query-face-vertices axis-number source-bit)))))
      (dotimes (state 16)
        (let ((pattern (svref *width-one-edge-pattern-table* state)))
          (dotimes (axis-number 3)
            (loop for descriptor across
                  (svref (width-one-edge-pattern-descriptors pattern)
                         axis-number)
                  do (record-descriptor descriptor)))))
      (dotimes (mask 256)
        (loop for descriptor across
              (width-one-vertex-pattern-descriptors
               (svref *width-one-vertex-pattern-table* mask))
              do (record-descriptor descriptor)))
      ;; Compile every topology translation while descriptor identity and
      ;; cube-edge provenance are still explicit.  Runtime planning sees only
      ;; typed rows indexed by the selected star mask.
      (dotimes (mask 256)
        (let ((pattern
                (the width-one-vertex-pattern
                  (svref *width-one-vertex-pattern-table* mask))))
          (setf (aref contributor-counts mask)
                (width-one-vertex-pattern-contributor-count pattern)
                (aref face-starts mask) (fill-pointer face-templates)
                (aref band-starts mask) (fill-pointer band-templates)
                (aref fan-starts mask) (fill-pointer fan-templates))
          (dotimes (axis-number 3)
            (let* ((u (svref +axis-u+ axis-number))
                   (v (svref +axis-v+ axis-number))
                   (low-sample (logior (ash 1 u) (ash 1 v)))
                   (high-sample
                     (logior low-sample (ash 1 axis-number)))
                   (face-state
                     (logior (if (logbitp low-sample mask) 1 0)
                             (if (logbitp high-sample mask) 2 0)))
                   (source-bit
                     (svref *width-one-face-source-table* face-state)))
              (unless (minusp source-bit)
                (let ((source-sample
                        (if (zerop source-bit) low-sample high-sample)))
                  (vector-push-extend
                   (aref face-ids (+ (* axis-number 2) source-bit))
                   face-templates)
                  (vector-push-extend
                   (%width-one-contributor-slot
                    pattern (aref *width-one-face-contributor-indices*
                                  axis-number))
                   face-material-slots)
                  (vector-push-extend
                   (logior (if (logbitp 0 source-sample) 0 1)
                           (if (logbitp 1 source-sample) 0 2))
                   face-source-offsets)))
              (let* ((edge-state (%width-one-edge-state mask axis-number))
                     (edge-pattern
                       (the width-one-edge-pattern
                         (svref *width-one-edge-pattern-table* edge-state)))
                     (contributor-indices
                       (the (simple-array fixnum (*))
                         (svref
                          (width-one-edge-pattern-contributor-indices
                           edge-pattern)
                          axis-number))))
                (loop for descriptor across
                      (svref (width-one-edge-pattern-descriptors edge-pattern)
                             axis-number)
                      for transition-mask =
                        (width-one-template-descriptor-contributor-mask
                         descriptor)
                      do (vector-push-extend
                          (descriptor-template-id descriptor) band-templates)
                         (vector-push-extend
                          (compact-contributors
                           pattern
                           (edge-cube-contributors
                            transition-mask contributor-indices))
                          band-contributor-masks)
                         (vector-push-extend
                          (source-witness
                           pattern contributor-indices transition-mask)
                          band-source-witnesses)
                         (vector-push-extend
                          (if
                           (width-one-template-descriptor-ambient-star-p
                            descriptor)
                           1 0)
                          band-ambient-stars)))))
          (loop for descriptor across
                (width-one-vertex-pattern-descriptors pattern)
                do (vector-push-extend
                    (descriptor-template-id descriptor) fan-templates)
                   (vector-push-extend
                    (compact-contributors
                     pattern
                     (width-one-template-descriptor-contributor-mask descriptor))
                    fan-contributor-masks))))
      (setf (aref face-starts 256) (fill-pointer face-templates)
            (aref band-starts 256) (fill-pointer band-templates)
            (aref fan-starts 256) (fill-pointer fan-templates))
      (let* ((template-count (fill-pointer templates))
             (vertex-count
               (loop for template across templates sum (length template)))
             (words
               (make-array (* +mesh-template-vertex-word-count+ vertex-count)
                           :element-type '(unsigned-byte 32)))
             (ranges (make-array (* 2 template-count)
                                 :element-type '(unsigned-byte 32)))
             (normal-x (make-array template-count
                                   :element-type '(signed-byte 8)))
             (normal-y (make-array template-count
                                   :element-type '(signed-byte 8)))
             (normal-z (make-array template-count
                                   :element-type '(signed-byte 8)))
             (vertex-start 0)
             (write 0))
        (dotimes (id template-count)
          (let* ((vertices (aref templates id))
                 (attributes (%packed-template-attributes (aref vertices 0))))
            (setf (aref ranges (* 2 id)) vertex-start
                  (aref ranges (1+ (* 2 id))) (length vertices)
                  (aref normal-x id) (- (ldb (byte 2 0) attributes) 1)
                  (aref normal-y id) (- (ldb (byte 2 2) attributes) 1)
                  (aref normal-z id) (- (ldb (byte 2 4) attributes) 1))
            (loop for vertex across vertices do
              (setf (aref words write)
                    (ldb (byte +mesh-template-coordinate-bit-count+ 0) vertex)
                    (aref words (+ write 1))
                    (ldb (byte +mesh-template-coordinate-bit-count+
                               +mesh-template-coordinate-bit-count+)
                         vertex)
                    (aref words (+ write 2))
                    (ldb (byte +mesh-template-coordinate-bit-count+
                               (* 2 +mesh-template-coordinate-bit-count+))
                         vertex)
                    (aref words (+ write 3))
                    (%packed-template-attributes vertex))
              (incf write +mesh-template-vertex-word-count+))
            (incf vertex-start (length vertices))))
        (let ((words-by-width (make-array 4 :initial-element nil)))
          (loop for width from 1 to 3 do
            (setf (aref words-by-width width)
                  (%uniform-query-template-words width words)))
          (%make-width-one-query-vocabulary
           (%make-width-one-query-dimension
            contributor-counts face-starts
            (coerce face-templates
                    '(simple-array (unsigned-byte 16) (*)))
            (coerce face-material-slots
                    '(simple-array (unsigned-byte 8) (*)))
            (coerce face-source-offsets
                    '(simple-array (unsigned-byte 8) (*)))
            band-starts
            (coerce band-templates
                    '(simple-array (unsigned-byte 16) (*)))
            (coerce band-contributor-masks
                    '(simple-array (unsigned-byte 16) (*)))
            (coerce band-source-witnesses
                    '(simple-array (unsigned-byte 8) (*)))
            (coerce band-ambient-stars
                    '(simple-array (unsigned-byte 8) (*)))
            fan-starts
            (coerce fan-templates
                    '(simple-array (unsigned-byte 16) (*)))
            (coerce fan-contributor-masks
                    '(simple-array (unsigned-byte 16) (*))))
           words-by-width ranges
           normal-x normal-y normal-z))))))

(defparameter *width-one-query-vocabulary*
  (%compile-width-one-query-vocabulary))

;;; ---------------------------------------------------------------------------
;;; Selection and relation materialization

(defun %gather-width-one-query-sites
    (field domain x0 x1 y0 y1 outside-domain-policy z0 z1 sites)
  "Run the retained scalar/SB-SIMD selection into one packed selection vector."
  (macrolet ((gather-with (function)
               `(,function field domain x0 x1 y0 y1 outside-domain-policy
                           z0 z1 sites)))
    #+x86-64
    (cond ((%simd-instruction-set-available-p :avx2)
           (gather-with %gather-width-one-sites-avx2))
          ((%simd-instruction-set-available-p :sse2)
           (gather-with %gather-width-one-sites-sse2))
          (t (gather-with %gather-width-one-sites-scalar)))
    #+arm64
    (if (%simd-instruction-set-available-p :neon)
        (gather-with %gather-width-one-sites-neon)
        (gather-with %gather-width-one-sites-scalar))
    #-(or x86-64 arm64)
    (gather-with %gather-width-one-sites-scalar)))

(defun %maximum-width-one-edge-descriptors ()
  (loop for state below 16
        for pattern = (svref *width-one-edge-pattern-table* state)
        maximize
        (loop for axis-number below 3
              maximize
              (length
               (svref (width-one-edge-pattern-descriptors pattern)
                      axis-number)))))

(defun %maximum-width-one-fan-descriptors ()
  (loop for mask below 256
        maximize
        (length
         (width-one-vertex-pattern-descriptors
          (svref *width-one-vertex-pattern-table* mask)))))

(defparameter *maximum-width-one-edge-descriptors*
  (%maximum-width-one-edge-descriptors))

(defparameter *maximum-width-one-fan-descriptors*
  (%maximum-width-one-fan-descriptors))

(declaim (type (integer 0 12) *maximum-width-one-edge-descriptors*
                                *maximum-width-one-fan-descriptors*))

(defun %prepare-width-one-query-relations (workspace site-count)
  (declare (optimize (speed 3) (safety 1))
           (type width-one-query-workspace workspace)
           (type (integer 0 #.(* 65 65 256)) site-count))
  (let* ((sites (width-one-query-workspace-sites workspace))
         (faces (width-one-query-workspace-faces workspace))
         (bands (width-one-query-workspace-bands workspace))
         (fans (width-one-query-workspace-fans workspace))
         (face-capacity (* 3 site-count))
         (band-capacity
           (* 3 *maximum-width-one-edge-descriptors* site-count))
         (fan-capacity (* *maximum-width-one-fan-descriptors* site-count)))
    (setf (width-one-query-sites-x sites)
          (%query-ub8-capacity (width-one-query-sites-x sites) site-count)
          (width-one-query-sites-y sites)
          (%query-ub8-capacity (width-one-query-sites-y sites) site-count)
          (width-one-query-sites-z sites)
          (%query-ub8-capacity (width-one-query-sites-z sites) site-count)
          (width-one-query-sites-mask sites)
          (%query-ub8-capacity (width-one-query-sites-mask sites) site-count)
          (width-one-query-sites-material-base sites)
          (%query-ub32-capacity
           (width-one-query-sites-material-base sites) site-count)
          (width-one-query-sites-count sites) site-count
          (width-one-query-faces-site faces)
          (%query-ub32-capacity (width-one-query-faces-site faces)
                                face-capacity)
          (width-one-query-faces-template faces)
          (%query-ub16-capacity (width-one-query-faces-template faces)
                                face-capacity)
          (width-one-query-faces-material-offset faces)
          (%query-ub32-capacity
           (width-one-query-faces-material-offset faces) face-capacity)
          (width-one-query-faces-count faces) 0)
    (flet ((prepare-patches (relation capacity)
             (declare (type width-one-query-patches relation)
                      (type fixnum capacity))
             (setf (width-one-query-patches-site relation)
                   (%query-ub32-capacity
                    (width-one-query-patches-site relation) capacity)
                   (width-one-query-patches-template relation)
                   (%query-ub16-capacity
                    (width-one-query-patches-template relation) capacity)
                   (width-one-query-patches-contributor-mask relation)
                   (%query-ub16-capacity
                    (width-one-query-patches-contributor-mask relation)
                    capacity)
                   (width-one-query-patches-ambient-star relation)
                   (%query-ub8-capacity
                    (width-one-query-patches-ambient-star relation) capacity)
                   (width-one-query-patches-count relation) 0)))
      (prepare-patches bands band-capacity)
      (prepare-patches fans fan-capacity)))
  workspace)

(declaim (inline %append-width-one-query-face
                 %append-width-one-query-patch))

(defun %append-width-one-query-face
    (relation site template material-offset)
  (declare (optimize (speed 3) (safety 1))
           (type width-one-query-faces relation)
           (type (unsigned-byte 32) site material-offset)
           (type (unsigned-byte 16) template))
  (let ((row (width-one-query-faces-count relation)))
    (declare (type fixnum row))
    (setf (aref (width-one-query-faces-site relation) row) site
          (aref (width-one-query-faces-template relation) row) template
          (aref (width-one-query-faces-material-offset relation) row)
          material-offset
          (width-one-query-faces-count relation) (1+ row))))

(defun %append-width-one-query-patch
    (relation site template contributor-mask ambient-star-p)
  (declare (optimize (speed 3) (safety 1))
           (type width-one-query-patches relation)
           (type (unsigned-byte 32) site)
           (type (unsigned-byte 16) template contributor-mask))
  (let ((row (width-one-query-patches-count relation)))
    (declare (type fixnum row))
    (setf (aref (width-one-query-patches-site relation) row) site
          (aref (width-one-query-patches-template relation) row) template
          (aref (width-one-query-patches-contributor-mask relation) row)
          contributor-mask
          (aref (width-one-query-patches-ambient-star relation) row)
          (if ambient-star-p 1 0)
          (width-one-query-patches-count relation) (1+ row))))

(defun %plan-width-one-query
    (packed-sites workspace x0 x1 y0 y1 ox1 oy1)
  "Copy selected stars' compiled topology rows into primitive relations."
  (declare (optimize (speed 3) (safety 1))
           (type (vector (unsigned-byte 32)) packed-sites)
           (type width-one-query-workspace workspace)
           (type (integer 0 #.(1+ (ash 1 17)))
                 x0 x1 y0 y1 ox1 oy1))
  (let* ((sites (width-one-query-workspace-sites workspace))
         (faces (width-one-query-workspace-faces workspace))
         (bands (width-one-query-workspace-bands workspace))
         (fans (width-one-query-workspace-fans workspace))
         (dimension
           (width-one-query-vocabulary-dimension
            *width-one-query-vocabulary*))
         (contributor-counts
           (width-one-query-dimension-contributor-counts dimension))
         (face-starts (width-one-query-dimension-face-starts dimension))
         (face-templates (width-one-query-dimension-face-templates dimension))
         (face-material-slots
           (width-one-query-dimension-face-material-slots dimension))
         (face-source-offsets
           (width-one-query-dimension-face-source-offsets dimension))
         (band-starts (width-one-query-dimension-band-starts dimension))
         (band-templates (width-one-query-dimension-band-templates dimension))
         (band-contributor-masks
           (width-one-query-dimension-band-contributor-masks dimension))
         (band-source-witnesses
           (width-one-query-dimension-band-source-witnesses dimension))
         (band-ambient-stars
           (width-one-query-dimension-band-ambient-stars dimension))
         (fan-starts (width-one-query-dimension-fan-starts dimension))
         (fan-templates (width-one-query-dimension-fan-templates dimension))
         (fan-contributor-masks
           (width-one-query-dimension-fan-contributor-masks dimension))
         (material-count 0)
         (singular-count 0))
    (declare (type fixnum material-count singular-count))
    (macrolet ((append-face-row (row)
                 `(%append-width-one-query-face
                   faces site-row (aref face-templates ,row)
                   (+ material-count (aref face-material-slots ,row))))
               (append-band-row (row)
                 `(%append-width-one-query-patch
                   bands site-row (aref band-templates ,row)
                   (aref band-contributor-masks ,row)
                   (not (zerop (aref band-ambient-stars ,row)))))
               (append-fan-row (row)
                 `(%append-width-one-query-patch
                   fans site-row (aref fan-templates ,row)
                   (aref fan-contributor-masks ,row) t)))
      (loop for packed across packed-sites
            for site-row fixnum from 0 do
        (let* ((local-x
                 (ldb (byte 7 +width-one-site-x-shift+) packed))
               (local-y
                 (ldb (byte 7 +width-one-site-y-shift+) packed))
               (site-x (+ x0 local-x))
               (site-y (+ y0 local-y))
               (site-z (%width-one-site-z packed))
               (star-mask (%width-one-site-mask packed))
               (site-owned-p (and (< site-x ox1) (< site-y oy1)))
               (interior-p
                 (and (< x0 site-x x1) (< y0 site-y y1)))
               (contributor-count (aref contributor-counts star-mask))
               (face-start (the fixnum (aref face-starts star-mask)))
               (face-end (the fixnum (aref face-starts (1+ star-mask))))
               (band-start (the fixnum (aref band-starts star-mask)))
               (band-end (the fixnum (aref band-starts (1+ star-mask))))
               (fan-start (the fixnum (aref fan-starts star-mask)))
               (fan-end (the fixnum (aref fan-starts (1+ star-mask)))))
          (setf (aref (width-one-query-sites-x sites) site-row) local-x
                (aref (width-one-query-sites-y sites) site-row) local-y
                (aref (width-one-query-sites-z sites) site-row) site-z
                (aref (width-one-query-sites-mask sites) site-row) star-mask
                (aref (width-one-query-sites-material-base sites) site-row)
                material-count)
          (when (> (+ material-count contributor-count)
                   +width-one-job-material-limit+)
            (error "Width-one query material relation exceeds 24-bit offsets."))
          (when (and site-owned-p
                     (= 1 (sbit *star-singular-bits* star-mask)))
            (incf singular-count))
          (if interior-p
              (progn
                (loop for row fixnum from face-start below face-end
                      do (append-face-row row))
                (loop for row fixnum from band-start below band-end
                      do (append-band-row row)))
              (let ((source-owned-mask 0))
                (declare (type (unsigned-byte 4) source-owned-mask))
                (dotimes (offset 4)
                  (let ((source-x (- site-x (logand offset 1)))
                        (source-y (- site-y (ash offset -1))))
                    (when (and (<= x0 source-x) (< source-x x1)
                               (<= y0 source-y) (< source-y y1))
                      (setf source-owned-mask
                            (logior source-owned-mask (ash 1 offset))))))
                (loop for row fixnum from face-start below face-end
                      when (logbitp (aref face-source-offsets row)
                                    source-owned-mask)
                        do (append-face-row row))
                (loop for row fixnum from band-start below band-end
                      for ambient-star-p =
                        (not (zerop (aref band-ambient-stars row)))
                      when (if ambient-star-p
                               site-owned-p
                               (logtest (aref band-source-witnesses row)
                                        source-owned-mask))
                        do (append-band-row row))))
          (when site-owned-p
            (loop for row fixnum from fan-start below fan-end
                  do (append-fan-row row)))
          (incf material-count contributor-count))))
    (values material-count singular-count)))

(defun %materialize-width-one-query-materials
    (workspace domain stock-function algebra x0 y0 material-count)
  "Evaluate the material dimension once for every selected boundary edge."
  (declare (optimize (speed 3) (safety 1))
           (type width-one-query-workspace workspace)
           (type world-domain domain)
           (type function stock-function)
           (type compiled-chamfer-algebra algebra)
           (type (integer 0 #.(ash 1 17)) x0 y0)
           (type fixnum material-count))
  (let* ((sites (width-one-query-workspace-sites workspace))
         (stocks
           (%query-ub16-capacity
            (width-one-query-workspace-stocks workspace) material-count))
         (summaries
           (%query-ub16-capacity
            (width-one-query-workspace-summaries workspace) material-count))
         (write 0))
    (declare (type (simple-array (unsigned-byte 16) (*)) stocks summaries)
             (type fixnum write))
    (setf (width-one-query-workspace-stocks workspace) stocks
          (width-one-query-workspace-summaries workspace) summaries)
    (dotimes (site-row (width-one-query-sites-count sites))
      (let* ((star-mask (aref (width-one-query-sites-mask sites) site-row))
             (site-x (+ x0 (aref (width-one-query-sites-x sites) site-row)))
             (site-y (+ y0 (aref (width-one-query-sites-y sites) site-row)))
             (site-z (aref (width-one-query-sites-z sites) site-row))
             (pattern (svref *width-one-vertex-pattern-table* star-mask)))
        (loop for contributor across
              (width-one-vertex-pattern-contributors pattern)
              unless (minusp contributor) do
                (let* ((sample (ldb (byte 3 0) contributor))
                       (axis-number (ldb (byte 2 3) contributor))
                       (normal-sign (if (logbitp 5 contributor) 1 -1))
                       (stock
                         (the (unsigned-byte 16)
                           (%width-one-source-stock
                            stock-function domain site-x site-y site-z
                            sample axis-number normal-sign))))
                  (setf (aref stocks write) stock
                        (aref summaries write)
                        (%compiled-chamfer-stock-summary algebra stock))
                  (incf write)))))
    (unless (= write material-count)
      (error "Width-one query planned ~D material rows, wrote ~D."
             material-count write))
    workspace))

(defun %materialize-width-one-query-lane-materials
    (workspace chunk chunk-key facts field material-source algebra
     x0 y0 material-count)
  "Evaluate the material dimension through compiled chain-rank lanes.

Each contributor's cell resolves to a rank in its chunk's chain, the chain's
lane entry at that rank names the authored placement and foundation face,
and the dense face-stock table finishes the lookup without touching authored
hashes.  Contributors classify by exact chunk key: boundary sites sample
cells one step past the owner box on every side, and each such neighbor's
chain identity was recorded by the occupancy FIELD when its chunk resolved."
  (declare (optimize (speed 3) (safety 1))
           (type width-one-query-workspace workspace)
           (type chain chunk)
           (type fixnum chunk-key)
           (type chain-chunk-facts facts)
           (type occupancy-field field)
           (type width-one-material-source material-source)
           (type compiled-chamfer-algebra algebra)
           (type (integer 0 #.(ash 1 17)) x0 y0)
           (type fixnum material-count))
  (let* ((sites (width-one-query-workspace-sites workspace))
         (stocks
           (%query-ub16-capacity
            (width-one-query-workspace-stocks workspace) material-count))
         (summaries
           (%query-ub16-capacity
            (width-one-query-workspace-summaries workspace) material-count))
         (face-stocks
           (width-one-material-source-face-stocks material-source))
         (face-stride
           (width-one-material-source-face-stride material-source))
         (foundation-face-index
           (width-one-material-source-foundation-face-index material-source))
         (owner-entries
           (chain-material-lane-entries
            (%chain-material-lane chunk material-source)))
         (neighbor-key -1)
         (neighbor-facts facts)
         (neighbor-entries owner-entries)
         (write 0))
    (declare (type (simple-array (unsigned-byte 16) (*)) stocks summaries)
             (type (simple-array (unsigned-byte 32) (*))
                   owner-entries neighbor-entries)
             (type chain-chunk-facts neighbor-facts)
             (type fixnum neighbor-key write))
    (setf (width-one-query-workspace-stocks workspace) stocks
          (width-one-query-workspace-summaries workspace) summaries)
    (dotimes (site-row (width-one-query-sites-count sites))
      (let* ((star-mask (aref (width-one-query-sites-mask sites) site-row))
             (site-x (+ x0 (aref (width-one-query-sites-x sites) site-row)))
             (site-y (+ y0 (aref (width-one-query-sites-y sites) site-row)))
             (site-z (aref (width-one-query-sites-z sites) site-row))
             (pattern (svref *width-one-vertex-pattern-table* star-mask)))
        (loop for contributor across
              (width-one-vertex-pattern-contributors pattern)
              unless (minusp contributor) do
                (let* ((sample (ldb (byte 3 0) contributor))
                       (axis-number (ldb (byte 2 3) contributor))
                       (cell-x (- site-x (if (logbitp 0 sample) 0 1)))
                       (cell-y (- site-y (if (logbitp 1 sample) 0 1)))
                       (cell-z (- site-z (if (logbitp 2 sample) 0 1)))
                       (cell-key (chunk-key-at cell-x cell-y))
                       (cell-facts facts)
                       (entries owner-entries))
                  (declare (type (simple-array (unsigned-byte 32) (*))
                                 entries))
                  (unless (= cell-key chunk-key)
                    ;; Halo contributors cluster by chunk, so one memoized
                    ;; neighbor covers almost every consecutive lookup.
                    (unless (= cell-key neighbor-key)
                      (let ((neighbor
                              (gethash
                               cell-key
                               (occupancy-field-chunk-chains field))))
                        (unless neighbor
                          (error
                           "No chain recorded for halo chunk ~D of ~D."
                           cell-key chunk-key))
                        (setf neighbor-key cell-key
                              neighbor-facts (%chain-chunk-facts neighbor)
                              neighbor-entries
                              (chain-material-lane-entries
                               (%chain-material-lane
                                neighbor material-source)))))
                    (setf cell-facts neighbor-facts
                          entries neighbor-entries))
                  (let* ((rank (%chain-facts-cell-rank
                                cell-facts cell-x cell-y cell-z))
                         (entry (aref entries rank))
                         (face-index
                           (if (logbitp 0 entry)
                               foundation-face-index
                               (+ (* 2 axis-number)
                                  (if (logbitp 5 contributor) 0 1))))
                         (stock
                           (aref face-stocks
                                 (+ (* (ash entry -1) face-stride)
                                    face-index))))
                    (setf (aref stocks write) stock
                          (aref summaries write)
                          (%compiled-chamfer-stock-summary algebra stock))
                    (incf write))))))
    (unless (= write material-count)
      (error "Width-one query planned ~D material rows, wrote ~D."
             material-count write))
    workspace))

;;; ---------------------------------------------------------------------------
;;; Projection into the renderer ABI

(defun %width-one-query-stream-layout (templates row-count)
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array (unsigned-byte 16) (*)) templates)
           (type fixnum row-count))
  (let* ((template-count
           (/ (length
               (width-one-query-vocabulary-ranges
                *width-one-query-vocabulary*))
              2))
         (counts (make-array template-count
                             :element-type '(unsigned-byte 32)
                             :initial-element 0))
         (starts (make-array template-count
                             :element-type '(unsigned-byte 32)))
         (writes (make-array template-count
                             :element-type '(unsigned-byte 32)))
         (words (make-array (* +mesh-instance-word-count+ row-count)
                            :element-type '(unsigned-byte 32))))
    (declare (type fixnum template-count)
             (type (simple-array (unsigned-byte 32) (*))
                   counts starts writes words))
    (dotimes (row row-count)
      (incf (aref counts (aref templates row))))
    (let ((start 0))
      (dotimes (template template-count)
        (setf (aref starts template) start
              (aref writes template) start)
        (incf start (aref counts template))))
    (values counts starts writes words)))

(defun %width-one-query-draws (counts starts)
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array (unsigned-byte 32) (*)) counts starts))
  (let ((ranges
          (width-one-query-vocabulary-ranges
           *width-one-query-vocabulary*))
        (draws nil)
        (triangle-count 0))
    (declare (type fixnum triangle-count))
    (dotimes (template (length counts))
      (let ((instances (aref counts template)))
        (when (plusp instances)
          (let ((vertex-start (aref ranges (* 2 template)))
                (vertex-count (aref ranges (1+ (* 2 template)))))
            (push (list template vertex-start vertex-count
                        (aref starts template) instances)
                  draws)
            (incf triangle-count (* instances (truncate vertex-count 3)))))))
    (values (nreverse draws) triangle-count)))

(defun %project-width-one-query-faces (workspace x0 y0)
  (declare (optimize (speed 3) (safety 1))
           (type width-one-query-workspace workspace)
           (type (integer 0 #.(ash 1 17)) x0 y0))
  (let* ((sites (width-one-query-workspace-sites workspace))
         (faces (width-one-query-workspace-faces workspace))
         (templates (width-one-query-faces-template faces))
         (row-count (width-one-query-faces-count faces))
         (stocks (width-one-query-workspace-stocks workspace)))
    (multiple-value-bind (counts starts writes words)
        (%width-one-query-stream-layout templates row-count)
      (declare (type (simple-array (unsigned-byte 32) (*))
                     counts starts writes words))
      ;; Reverse traversal preserves the reference finisher's bucket order.
      (loop for row fixnum downfrom (1- row-count) to 0 do
        (let* ((template (aref templates row))
               (site-row (aref (width-one-query-faces-site faces) row))
               (instance (aref writes template))
               (write (* +mesh-instance-word-count+ instance)))
          (setf (aref words write)
                (+ x0 (aref (width-one-query-sites-x sites) site-row))
                (aref words (+ write 1))
                (+ y0 (aref (width-one-query-sites-y sites) site-row))
                (aref words (+ write 2))
                (aref (width-one-query-sites-z sites) site-row)
                (aref words (+ write 3))
                (logior
                 template
                 (ash
                  (aref stocks
                        (aref (width-one-query-faces-material-offset faces)
                              row))
                  +mesh-instance-stock-shift+))
                (aref writes template) (1+ instance))))
      (multiple-value-bind (draws triangles)
          (%width-one-query-draws counts starts)
        (values words draws triangles)))))

(defun %width-one-query-patch-stock (workspace site-row contributor-mask algebra)
  (declare (optimize (speed 3) (safety 1))
           (type width-one-query-workspace workspace)
           (type fixnum site-row)
           (type (unsigned-byte 16) contributor-mask)
           (type compiled-chamfer-algebra algebra))
  (let* ((sites (width-one-query-workspace-sites workspace))
         (summaries (width-one-query-workspace-summaries workspace))
         (base (aref (width-one-query-sites-material-base sites) site-row))
         (summary 0))
    (declare (type (unsigned-byte 16) summary contributor-mask))
    (dotimes (slot 12)
      (when (logbitp slot contributor-mask)
        (setf summary (logior summary (aref summaries (+ base slot))))))
    (%compiled-chamfer-summary-stock algebra summary)))

(defun %project-width-one-query-patches
    (relation workspace algebra x0 y0)
  (declare (optimize (speed 3) (safety 1))
           (type width-one-query-patches relation)
           (type width-one-query-workspace workspace)
           (type compiled-chamfer-algebra algebra)
           (type (integer 0 #.(ash 1 17)) x0 y0))
  (let* ((sites (width-one-query-workspace-sites workspace))
         (templates (width-one-query-patches-template relation))
         (row-count (width-one-query-patches-count relation))
         (vocabulary *width-one-query-vocabulary*))
    (multiple-value-bind (counts starts writes words)
        (%width-one-query-stream-layout templates row-count)
      (declare (type (simple-array (unsigned-byte 32) (*))
                     counts starts writes words))
      (loop for row fixnum downfrom (1- row-count) to 0 do
        (let* ((template (aref templates row))
               (site-row (aref (width-one-query-patches-site relation) row))
               (instance (aref writes template))
               (write (* +mesh-instance-word-count+ instance))
               (star-mask (aref (width-one-query-sites-mask sites) site-row))
               (ambient
                 (the (unsigned-byte 2)
                   (if (zerop
                        (aref (width-one-query-patches-ambient-star relation)
                              row))
                       0
                       (%star-normal-ambient-occlusion
                        star-mask
                        (aref
                         (width-one-query-vocabulary-normal-x vocabulary)
                         template)
                        (aref
                         (width-one-query-vocabulary-normal-y vocabulary)
                         template)
                        (aref
                         (width-one-query-vocabulary-normal-z vocabulary)
                         template)))))
               (stock
                 (the (unsigned-byte 16)
                   (%width-one-query-patch-stock
                    workspace site-row
                    (aref (width-one-query-patches-contributor-mask relation)
                          row)
                    algebra))))
          (setf (aref words write)
                (+ x0 (aref (width-one-query-sites-x sites) site-row))
                (aref words (+ write 1))
                (+ y0 (aref (width-one-query-sites-y sites) site-row))
                (aref words (+ write 2))
                (aref (width-one-query-sites-z sites) site-row)
                (aref words (+ write 3))
                (logior template
                        (ash stock +mesh-instance-stock-shift+)
                        (ash ambient
                             +mesh-instance-ambient-occlusion-shift+))
                (aref writes template) (1+ instance))))
      (multiple-value-bind (draws triangles)
          (%width-one-query-draws counts starts)
        (values words draws triangles)))))

(defun %finish-width-one-query
    (workspace domain algebra singular-count x0 y0 &optional (bevel-width 1))
  (multiple-value-bind (face-words face-draws face-triangles)
      (%project-width-one-query-faces workspace x0 y0)
    (multiple-value-bind (band-words band-draws band-triangles)
        (%project-width-one-query-patches
         (width-one-query-workspace-bands workspace)
         workspace algebra x0 y0)
      (multiple-value-bind (fan-words fan-draws fan-triangles)
          (%project-width-one-query-patches
           (width-one-query-workspace-fans workspace)
           workspace algebra x0 y0)
        (%make-surface-mesh
         domain bevel-width
         (aref
          (width-one-query-vocabulary-vertex-words-by-width
           *width-one-query-vocabulary*)
          bevel-width)
         (width-one-query-vocabulary-ranges *width-one-query-vocabulary*)
         face-words face-draws band-words band-draws fan-words fan-draws
         face-triangles band-triangles fan-triangles singular-count)))))

;;; ---------------------------------------------------------------------------
;;; Chunk-only entry point

(defun %mesh-width-one-chunk-query
    (chunk chunk-key stock-function algebra outside-domain-policy
     material-source bevel-width)
  (let* ((domain (chain-domain chunk))
         (grid-x (chunk-key-x chunk-key))
         (grid-y (chunk-key-y chunk-key))
         (x0 (chunk-origin-x chunk-key))
         (y0 (chunk-origin-y chunk-key))
         (x1 (min (+ x0 +chunk-size+) (world-domain-x-limit domain)))
         (y1 (min (+ y0 +chunk-size+) (world-domain-y-limit domain)))
         (ox1 (if (>= (+ x0 +chunk-size+) (world-domain-x-limit domain))
                  (1+ (world-domain-x-limit domain))
                  (+ x0 +chunk-size+)))
         (oy1 (if (>= (+ y0 +chunk-size+) (world-domain-y-limit domain))
                  (1+ (world-domain-y-limit domain))
                  (+ y0 +chunk-size+)))
         (facts (%chain-chunk-facts chunk))
         (field (%chain-facts-occupancy-field facts domain x0 x1 y0 y1)))
    ;; The derived facts already validated the chain as a single chunk's
    ;; positive cells, so membership is one key comparison, not a scan.
    (unless (or (minusp (chain-chunk-facts-chunk-key facts))
                (= (chain-chunk-facts-chunk-key facts) chunk-key))
      (error "Chunk chain with key ~D does not belong to chunk ~D."
             (chain-chunk-facts-chunk-key facts) chunk-key))
    (multiple-value-bind (z0 z1)
        (%width-one-chunk-star-z-bounds chunk field grid-x grid-y)
      (let* ((packed-sites
               (%borrow-width-one-sites (max 16 (* 8 (chain-count chunk)))))
             (workspace (%borrow-width-one-query-workspace)))
        (when z0
          (%gather-width-one-query-sites
           field domain x0 x1 y0 y1 outside-domain-policy z0 z1 packed-sites))
        (%prepare-width-one-query-relations workspace (length packed-sites))
        (multiple-value-bind (material-count singular-count)
            (%plan-width-one-query
             packed-sites workspace x0 x1 y0 y1 ox1 oy1)
          (if material-source
              (%materialize-width-one-query-lane-materials
               workspace chunk chunk-key facts field material-source algebra
               x0 y0 material-count)
              (%materialize-width-one-query-materials
               workspace domain stock-function algebra x0 y0 material-count))
          (%finish-width-one-query
           workspace domain algebra singular-count x0 y0 bevel-width))))))

(defun mesh-chunk
    (chunk chunk-key
     &key (stock-function (constantly 0))
          source-stock-function
          (chamfer-stock-function (lambda (stocks) (first stocks)))
          chamfer-algebra
          outside-domain-policy
          material-source
          (bevel-width +mesh-bevel-width+))
  "Mesh one streaming chunk, retaining the general mesher as its oracle.

Widths one through three plus a compiled CHAMFER-ALGEBRA run the finite
columnar query.  They share one topology and differ only in the immutable
template vocabulary selected at projection.  Medial width four remains on the
reference path because it collapses sheets and repairs the resulting topology.
A MATERIAL-SOURCE additionally compiles the authored materials into
chain-rank lanes, replacing per-contributor stock-function calls with dense
array reads; the stock functions remain the reference path and the fallback.
Callers without a closed material algebra use the retained surface-proportional
implementation, including its boundary restart contract."
  (check-type chunk chain)
  (check-type stock-function function)
  (check-type source-stock-function (or null function))
  (check-type chamfer-stock-function function)
  (check-type chamfer-algebra (or null compiled-chamfer-algebra))
  (check-type outside-domain-policy (member nil :air :solid))
  (check-type material-source (or null width-one-material-source))
  (unless (and (integerp bevel-width)
               (<= 1 bevel-width (/ +mesh-cell-size+ 2)))
    (error "Bevel width ~S must be an integer between one and four ticks."
           bevel-width))
  (if (and (< bevel-width 4) chamfer-algebra)
      (%mesh-width-one-chunk-query
       chunk chunk-key
       (%make-face-stock-resolver
        (chain-domain chunk) stock-function source-stock-function)
       chamfer-algebra outside-domain-policy material-source bevel-width)
      (%mesh-chunk-reference
       chunk chunk-key
       :stock-function stock-function
       :source-stock-function source-stock-function
       :chamfer-stock-function chamfer-stock-function
       :chamfer-algebra chamfer-algebra
       :outside-domain-policy outside-domain-policy
       :bevel-width bevel-width)))
