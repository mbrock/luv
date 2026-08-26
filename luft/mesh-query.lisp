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
;;; One immutable template dimension table

(defstruct (width-one-query-vocabulary
             (:constructor %make-width-one-query-vocabulary
                 (face-template-ids descriptor-template-ids
                  vertex-words ranges normal-x normal-y normal-z)))
  (face-template-ids #() :type (simple-array (unsigned-byte 16) (*))
                          :read-only t)
  (descriptor-template-ids nil :type hash-table :read-only t)
  (vertex-words #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
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

(defun %compile-width-one-query-vocabulary ()
  (let ((template-index (make-hash-table :test #'equalp))
        (templates (make-array 64 :adjustable t :fill-pointer 0))
        (face-ids (make-array 6 :element-type '(unsigned-byte 16)))
        (descriptor-ids (make-hash-table :test #'eq)))
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
                      (width-one-template-descriptor-vertices descriptor)))))
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
        (%make-width-one-query-vocabulary
         face-ids descriptor-ids words ranges normal-x normal-y normal-z)))))

(defparameter *width-one-query-vocabulary*
  (%compile-width-one-query-vocabulary))

(defun %width-one-query-descriptor-template-id (descriptor)
  (multiple-value-bind (id present-p)
      (gethash descriptor
               (width-one-query-vocabulary-descriptor-template-ids
                *width-one-query-vocabulary*))
    (unless present-p
      (error "Width-one descriptor is absent from the query vocabulary."))
    id))

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

(defun %width-one-query-compact-contributors (pattern cube-mask)
  (declare (optimize (speed 3) (safety 1))
           (type width-one-vertex-pattern pattern)
           (type (unsigned-byte 12) cube-mask))
  (let ((compact 0)
        (slots (width-one-vertex-pattern-contributor-slots pattern)))
    (declare (type (unsigned-byte 12) compact)
             (type (simple-array fixnum (*)) slots))
    (dotimes (contributor-index 12 compact)
      (when (logbitp contributor-index cube-mask)
        (let ((slot (aref slots contributor-index)))
          (when (minusp slot)
            (error "Query primitive names absent contributor ~D."
                   contributor-index))
          (setf compact
                (logior compact
                        (ash 1 (the (integer 0 11) slot)))))))))

(defun %width-one-query-edge-cube-contributors
    (transition-mask contributor-indices)
  (declare (optimize (speed 3) (safety 1))
           (type (unsigned-byte 4) transition-mask)
           (type (simple-array fixnum (*)) contributor-indices))
  (let ((cube-mask 0))
    (declare (type (unsigned-byte 12) cube-mask))
    (dotimes (transition 4 cube-mask)
      (when (logbitp transition transition-mask)
        (let ((index (aref contributor-indices transition)))
          (when (minusp index)
            (error "Query edge names absent radial transition ~D." transition))
          (setf cube-mask
                (logior cube-mask
                        (ash 1 (the (integer 0 11) index)))))))))

(defun %plan-width-one-query
    (packed-sites workspace x0 x1 y0 y1 ox1 oy1)
  "Lower selected stars to three typed primitive relations."
  (declare (optimize (speed 3) (safety 1))
           (type (vector (unsigned-byte 32)) packed-sites)
           (type width-one-query-workspace workspace)
           (type (integer 0 #.(1+ (ash 1 17)))
                 x0 x1 y0 y1 ox1 oy1))
  (let* ((sites (width-one-query-workspace-sites workspace))
         (faces (width-one-query-workspace-faces workspace))
         (bands (width-one-query-workspace-bands workspace))
         (fans (width-one-query-workspace-fans workspace))
         (face-template-ids
           (width-one-query-vocabulary-face-template-ids
            *width-one-query-vocabulary*))
         (material-count 0)
         (singular-count 0))
    (declare (type fixnum material-count singular-count))
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
             (vertex-pattern
               (the width-one-vertex-pattern
                 (svref *width-one-vertex-pattern-table* star-mask))))
        (setf (aref (width-one-query-sites-x sites) site-row) local-x
              (aref (width-one-query-sites-y sites) site-row) local-y
              (aref (width-one-query-sites-z sites) site-row) site-z
              (aref (width-one-query-sites-mask sites) site-row) star-mask
              (aref (width-one-query-sites-material-base sites) site-row)
              material-count)
        (when (> (+ material-count
                    (width-one-vertex-pattern-contributor-count
                     vertex-pattern))
                 +width-one-job-material-limit+)
          (error "Width-one query material relation exceeds 24-bit offsets."))
        (when (and site-owned-p
                   (= 1 (sbit *star-singular-bits* star-mask)))
          (incf singular-count))
        (dotimes (axis-number 3)
          (let* ((u (the (integer 0 2) (svref +axis-u+ axis-number)))
                 (v (the (integer 0 2) (svref +axis-v+ axis-number)))
                 (low-sample (logior (ash 1 u) (ash 1 v)))
                 (high-sample (logior low-sample (ash 1 axis-number)))
                 (face-state
                   (logior (if (logbitp low-sample star-mask) 1 0)
                           (if (logbitp high-sample star-mask) 2 0)))
                 (source-bit
                   (the fixnum
                     (svref *width-one-face-source-table* face-state))))
            (unless (minusp source-bit)
              (let* ((source-sample
                       (if (zerop source-bit) low-sample high-sample))
                     (source-x
                       (- site-x
                          (if (logbitp 0 source-sample) 0 1)))
                     (source-y
                       (- site-y
                          (if (logbitp 1 source-sample) 0 1))))
                (when (and (<= x0 source-x) (< source-x x1)
                           (<= y0 source-y) (< source-y y1))
                  (%append-width-one-query-face
                   faces site-row
                   (aref face-template-ids
                         (+ (* axis-number 2) source-bit))
                   (+ material-count
                      (%width-one-contributor-slot
                       vertex-pattern
                       (aref
                        (the (simple-array (unsigned-byte 8) (3))
                          *width-one-face-contributor-indices*)
                        axis-number)))))))
            (let* ((edge-state
                     (%width-one-edge-state star-mask axis-number))
                   (edge-pattern
                     (the width-one-edge-pattern
                       (svref *width-one-edge-pattern-table* edge-state)))
                   (descriptors
                     (the simple-vector
                       (svref
                        (width-one-edge-pattern-descriptors edge-pattern)
                        axis-number))))
              (when (plusp (length descriptors))
                (let* ((contributor-indices
                         (the (simple-array fixnum (*))
                           (svref
                            (width-one-edge-pattern-contributor-indices
                             edge-pattern)
                            axis-number)))
                       (source-owned-mask
                         (the (unsigned-byte 4)
                           (%width-one-edge-source-owned-mask
                            vertex-pattern contributor-indices site-x site-y
                            x0 x1 y0 y1))))
                  (loop for descriptor across descriptors
                        for transition-mask =
                          (the (unsigned-byte 4)
                            (width-one-template-descriptor-contributor-mask
                             descriptor))
                        when
                          (if
                           (width-one-template-descriptor-ambient-star-p
                            descriptor)
                           site-owned-p
                           (logtest transition-mask source-owned-mask))
                          do
                             (%append-width-one-query-patch
                              bands site-row
                              (the (unsigned-byte 16)
                                (%width-one-query-descriptor-template-id
                                 descriptor))
                              (%width-one-query-compact-contributors
                               vertex-pattern
                               (%width-one-query-edge-cube-contributors
                                transition-mask contributor-indices))
                              (width-one-template-descriptor-ambient-star-p
                               descriptor))))))))
        (when site-owned-p
          (loop with descriptors of-type simple-vector =
                  (width-one-vertex-pattern-descriptors vertex-pattern)
                for descriptor across descriptors
                do (%append-width-one-query-patch
                    fans site-row
                    (the (unsigned-byte 16)
                      (%width-one-query-descriptor-template-id descriptor))
                    (%width-one-query-compact-contributors
                     vertex-pattern
                     (width-one-template-descriptor-contributor-mask
                      descriptor))
                    t)))
        (incf material-count
              (width-one-vertex-pattern-contributor-count vertex-pattern))))
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

(defun %finish-width-one-query (workspace domain algebra singular-count x0 y0)
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
         domain 1
         (width-one-query-vocabulary-vertex-words
          *width-one-query-vocabulary*)
         (width-one-query-vocabulary-ranges *width-one-query-vocabulary*)
         face-words face-draws band-words band-draws fan-words fan-draws
         face-triangles band-triangles fan-triangles singular-count)))))

;;; ---------------------------------------------------------------------------
;;; Chunk-only entry point

(defun %mesh-width-one-chunk-query
    (chunk chunk-key stock-function algebra outside-domain-policy)
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
         (field (%materialize-occupancy chunk x0 x1 y0 y1)))
    (loop for cell across (%chain-sites chunk) do
      (unless (= (site-chunk-key cell) chunk-key)
        (error "Cell ~S does not belong to chunk ~D." cell chunk-key)))
    (multiple-value-bind (z0 z1)
        (%width-one-chunk-star-z-bounds
         chunk field grid-x grid-y x0 x1 y0 y1)
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
          (%materialize-width-one-query-materials
           workspace domain stock-function algebra x0 y0 material-count)
          (%finish-width-one-query
           workspace domain algebra singular-count x0 y0))))))

(defun mesh-chunk
    (chunk chunk-key
     &key (stock-function (constantly 0))
          source-stock-function
          (chamfer-stock-function (lambda (stocks) (first stocks)))
          chamfer-algebra
          outside-domain-policy
          (bevel-width +mesh-bevel-width+))
  "Mesh one streaming chunk, retaining the general mesher as its oracle.

Width one plus a compiled CHAMFER-ALGEBRA runs the finite columnar query.
Other widths and callers without a closed material algebra use the retained
surface-proportional implementation, including its boundary restart contract."
  (check-type chunk chain)
  (check-type stock-function function)
  (check-type source-stock-function (or null function))
  (check-type chamfer-stock-function function)
  (check-type chamfer-algebra (or null compiled-chamfer-algebra))
  (check-type outside-domain-policy (member nil :air :solid))
  (unless (and (integerp bevel-width)
               (<= 1 bevel-width (/ +mesh-cell-size+ 2)))
    (error "Bevel width ~S must be an integer between one and four ticks."
           bevel-width))
  (if (and (= bevel-width 1) chamfer-algebra)
      (%mesh-width-one-chunk-query
       chunk chunk-key
       (%make-face-stock-resolver stock-function source-stock-function)
       chamfer-algebra outside-domain-policy)
      (%mesh-chunk-reference
       chunk chunk-key
       :stock-function stock-function
       :source-stock-function source-stock-function
       :chamfer-stock-function chamfer-stock-function
       :chamfer-algebra chamfer-algebra
       :outside-domain-policy outside-domain-policy
       :bevel-width bevel-width)))
