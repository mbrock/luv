(in-package #:luft)

;;; Integer site-stream surface materialization
;;;
;;; Geometry is a sum over lattice sites.  Face records select a fixed inset
;;; square, edge records select flat collars or crease bands, and vertex records
;;; select flat corner patches or Arc junction fans.  Records contain only a
;;; lattice base coordinate, a stock, local ambient accessibility, and a
;;; template index.  Template vertices are exact small integer offsets from
;;; that base; vertex-owned offsets stay inside the configured bevel-width
;;; domain around the lattice site.
;;;
;;; The mesher speaks the same packed-integer language as the chain substrate.
;;; Its working representations, and the identities they preserve:
;;;
;;; - Occupancy is materialized once per build into an EQL table keyed by the
;;;   packed positive cell sites of the solid; every probe is one lookup.
;;; - Lattice keys (edge anchors, vertex sites, boundary anchors) pack as
;;;   axis<<59 | (x+1)<<34 | (y+1)<<9 | (z+1), so numeric order is the
;;;   (axis, x, y, z) lexicographic order the streams are deterministically
;;;   sorted by.
;;; - Template vertices pack as one fixnum each: three 5-bit biased local
;;;   coordinates plus the ABI attribute bits.  Templates intern by content.
;;; - Instances append to columnar (unsigned-byte 32) streams whose fourth
;;;   word is already the final ABI meta word (template | stock | ambient).
;;; - The open boundary is parity-counted over packed canonical edges
;;;   (anchor key + two 12-bit anchor-local endpoints); each surviving open
;;;   edge becomes a single fixnum of site-local endpoints and stock, and the
;;;   whole fan phase runs on those.
;;; - STAR-SINGULAR-P and directional ambient occlusion are pure in at most
;;;   thirteen bits and are read from tables built once at load.

(defconstant +mesh-cell-size+ 8)
(defconstant +mesh-bevel-width+ 2
  "Default bevel width in eighth-cell integer ticks (one quarter cell).")
(defconstant +mesh-instance-word-count+ 4)
(defconstant +mesh-template-vertex-word-count+ 4)
(defconstant +mesh-template-coordinate-bit-count+ 12)
(defconstant +mesh-template-coordinate-bias+ 2048)
(defconstant +mesh-instance-stock-shift+ 16)
(defconstant +mesh-instance-stock-bit-count+ 12)
(defconstant +mesh-instance-ambient-occlusion-shift+ 28)

(deftype mesh-global-tick ()
  '(integer 0 #.(ash 1 (+ 17 3))))

(defstruct (surface-mesh
             (:constructor %make-surface-mesh
                 (domain bevel-width template-vertex-words template-ranges
                  face-instance-words face-draws
                  band-instance-words band-draws
                  fan-instance-words fan-draws
                  face-triangle-count band-triangle-count fan-triangle-count
                  singular-star-count))
             (:copier nil))
  (domain nil :type world-domain :read-only t)
  (bevel-width +mesh-bevel-width+ :type (integer 1 4) :read-only t)
  (template-vertex-words #()
                         :type (simple-array (unsigned-byte 32) (*))
                         :read-only t)
  (template-ranges #()
                   :type (simple-array (unsigned-byte 32) (*))
                   :read-only t)
  (face-instance-words #()
                       :type (simple-array (unsigned-byte 32) (*))
                       :read-only t)
  (face-draws nil :type list :read-only t)
  (band-instance-words #()
                       :type (simple-array (unsigned-byte 32) (*))
                       :read-only t)
  (band-draws nil :type list :read-only t)
  (fan-instance-words #()
                      :type (simple-array (unsigned-byte 32) (*))
                      :read-only t)
  (fan-draws nil :type list :read-only t)
  (face-triangle-count 0 :type (integer 0 *) :read-only t)
  (band-triangle-count 0 :type (integer 0 *) :read-only t)
  (fan-triangle-count 0 :type (integer 0 *) :read-only t)
  (singular-star-count 0 :type (integer 0 *) :read-only t)
  ;; Derived render products stay beside the compact topology instead of
  ;; consuming its already-full four-word instance ABI.
  (voxel-light nil :type (or null voxel-light-field))
  (companions nil :type list))

(defun surface-mesh-template-count (mesh)
  (/ (length (surface-mesh-template-ranges mesh)) 2))

(defun surface-mesh-face-instance-count (mesh)
  (/ (length (surface-mesh-face-instance-words mesh))
     +mesh-instance-word-count+))

(defun surface-mesh-band-instance-count (mesh)
  (/ (length (surface-mesh-band-instance-words mesh))
     +mesh-instance-word-count+))

(defun surface-mesh-fan-instance-count (mesh)
  (/ (length (surface-mesh-fan-instance-words mesh))
     +mesh-instance-word-count+))

(defun surface-mesh-triangle-count (mesh)
  (+ (surface-mesh-face-triangle-count mesh)
     (surface-mesh-band-triangle-count mesh)
     (surface-mesh-fan-triangle-count mesh)))

(defun %read-arc-junction-table ()
  (let ((table (make-array 256 :initial-element nil))
        (pathname
          (asdf:system-relative-pathname
           "luft" #P"luft/blender-arc-stars.sexp")))
    (with-open-file (stream pathname :direction :input)
      (let ((corpus (read stream nil nil)))
        (unless corpus
          (error "The Blender Arc corpus is empty: ~A" pathname))
        (dolist (case (getf corpus :cases))
          (let ((mask (getf case :mask)))
            (setf (aref table mask)
                  (list :regular-star (getf (getf case :input) :regular-star)
                        :faces (getf (getf case :junction) :faces)))))))
    (unless (every #'identity table)
      (error "The Blender Arc corpus does not contain all 256 stars."))
    table))

(defparameter *arc-junction-table* (%read-arc-junction-table))

(defun %sample-direction-component (sample axis-number)
  (if (logbitp axis-number sample) 1 -1))

(defun %cube-edge-key (a b)
  (when (> a b) (rotatef a b))
  (logior a (ash b 3)))

(defun %cube-edge-low (edge)
  (ldb (byte 3 0) edge))

(defun %cube-edge-high (edge)
  (ldb (byte 3 3) edge))

(defparameter *star-cube-edges*
  (loop for sample below 8 append
    (loop for axis-number below 3
          unless (logbitp axis-number sample)
            collect (%cube-edge-key
                     sample (logxor sample (ash 1 axis-number)))))
  "The twelve edges of the cube whose vertices are the eight incident cells.")

(defun %boundary-edge-p (mask edge)
  (not (eq (logbitp (%cube-edge-low edge) mask)
           (logbitp (%cube-edge-high edge) mask))))

(defun %star-boundary-edges (mask)
  (remove-if-not (lambda (edge) (%boundary-edge-p mask edge))
                 *star-cube-edges*))

(defun %other-axis-numbers (axis-number)
  (loop for candidate below 3
        unless (= candidate axis-number)
          collect candidate))

(defun %radial-samples (axis-number sign)
  (destructuring-bind (u v) (%other-axis-numbers axis-number)
    (let ((base (if (plusp sign) (ash 1 axis-number) 0)))
      (vector base
              (logior base (ash 1 u))
              (logior base (ash 1 u) (ash 1 v))
              (logior base (ash 1 v))))))

(defun %radial-transition-groups (mask axis-number sign)
  "Pair link edges at one signed lattice-edge ray.

The ordinary two transitions are one pair.  A checkerboard has four
transitions; occupied-side topology pairs the two transitions surrounding
each occupied quadrant, producing two independent surface sheets."
  (let* ((samples (%radial-samples axis-number sign))
         (transitions
           (loop for index below 4
                 for next = (mod (1+ index) 4)
                 unless (eq (logbitp (aref samples index) mask)
                            (logbitp (aref samples next) mask))
                   collect index)))
    (case (length transitions)
      (0 nil)
      (2 (list
          (mapcar (lambda (index)
                    (%cube-edge-key
                     (aref samples index)
                     (aref samples (mod (1+ index) 4))))
                  transitions)))
      (4 (loop for index below 4
               when (logbitp (aref samples index) mask)
                 collect
                 (list (%cube-edge-key
                        (aref samples (mod (+ index 3) 4))
                        (aref samples index))
                       (%cube-edge-key
                        (aref samples index)
                        (aref samples (mod (1+ index) 4))))))
      (t (error "Impossible radial transition count ~D." (length transitions))))))

(defun %add-link-neighbors (neighbors left right)
  (push right (gethash left neighbors))
  (push left (gethash right neighbors)))

(defun %star-sheet-cycles (mask)
  (let ((neighbors (make-hash-table :test #'eql)))
    (dolist (edge (%star-boundary-edges mask))
      (setf (gethash edge neighbors) nil))
    (dotimes (axis-number 3)
      (dolist (sign '(-1 1))
        (dolist (pair (%radial-transition-groups mask axis-number sign))
          (%add-link-neighbors neighbors (first pair) (second pair)))))
    (maphash
     (lambda (edge adjacent)
       (unless (= 2 (length adjacent))
         (error "Resolved star ~2,'0X leaves link edge ~D with degree ~D."
                mask edge (length adjacent))))
     neighbors)
    (let ((unseen (make-hash-table :test #'eql))
          (cycles '()))
      (maphash (lambda (edge value)
                 (declare (ignore value))
                 (setf (gethash edge unseen) t))
               neighbors)
      (loop while (plusp (hash-table-count unseen)) do
        (let* ((start (loop for edge being the hash-keys of unseen return edge))
               (previous nil)
               (current start)
               (cycle '()))
          (loop
            (push current cycle)
            (remhash current unseen)
            (let* ((adjacent (gethash current neighbors))
                   (next (if (eql (first adjacent) previous)
                             (second adjacent)
                             (first adjacent))))
              (setf previous current
                    current next))
            (when (eql current start) (return)))
          (push (nreverse cycle) cycles)))
      (sort cycles #'< :key #'first))))

(defun %cycle-virtual-mask (original-mask cycle)
  "Return the occupied side of one resolved link CYCLE as a regular star."
  (let* ((first-edge (first cycle))
         (low (%cube-edge-low first-edge))
         (high (%cube-edge-high first-edge))
         (start (if (logbitp low original-mask) low high))
         (barrier (make-hash-table :test #'eql))
         (seen (make-hash-table :test #'eql))
         (queue (list start))
         (mask 0))
    (dolist (edge cycle) (setf (gethash edge barrier) t))
    (setf (gethash start seen) t)
    (loop while queue do
      (let ((sample (pop queue)))
        (setf mask (logior mask (ash 1 sample)))
        (dotimes (axis-number 3)
          (let* ((neighbor (logxor sample (ash 1 axis-number)))
                 (edge (%cube-edge-key sample neighbor)))
            (unless (or (gethash edge barrier) (gethash neighbor seen))
              (setf (gethash neighbor seen) t)
              (push neighbor queue))))))
    mask))

(defun decompose-star-mask (mask)
  "Resolve MASK into the regular occupied-side masks supported by this spike.

The returned list has one regular mask per simple boundary-link cycle.  Empty
and full stars have no boundary and therefore return NIL.  A cycle which needs
duplicated radial vertices cannot yet be represented by the Blender regular
star corpus; signal that boundary explicitly instead of silently welding it."
  (check-type mask (integer 0 255))
  (mapcar (lambda (cycle)
            (let ((virtual-mask (%cycle-virtual-mask mask cycle)))
              (unless (getf (aref *arc-junction-table* virtual-mask)
                            :regular-star)
                (error "Sheet cycle from ~2,'0X needs a covered junction; its ordinary mask is ~2,'0X."
                       mask virtual-mask))
              virtual-mask))
          (%star-sheet-cycles mask)))

(defun %checkerboard-ray-p (mask axis-number sign)
  (= 2 (length (%radial-transition-groups mask axis-number sign))))

(defun star-singular-p (mask)
  "Whether MASK's unsplit cubical boundary fails to be one manifold sheet."
  (check-type mask (integer 0 255))
  (or (loop for axis-number below 3
            thereis (loop for sign in '(-1 1)
                          thereis (%checkerboard-ray-p
                                   mask axis-number sign)))
      (> (length (%star-sheet-cycles mask)) 1)))

(defparameter *star-singular-bits*
  (let ((bits (make-array 256 :element-type 'bit)))
    (dotimes (mask 256 bits)
      (setf (sbit bits mask) (if (star-singular-p mask) 1 0))))
  "STAR-SINGULAR-P of every eight-bit star, computed once.")

;;; ---------------------------------------------------------------------------
;;; Exact list-vector helpers retained for cold construction and tests

(defun %point- (left right)
  (mapcar #'- left right))

(defun %cross (left right)
  (list (- (* (second left) (third right))
           (* (third left) (second right)))
        (- (* (third left) (first right))
           (* (first left) (third right)))
        (- (* (first left) (second right))
           (* (second left) (first right)))))

(defun %dot (left right)
  (reduce #'+ (mapcar #'* left right)))

(defun %normal-direction-code (normal)
  "Reduce an exact polygon normal to the trit direction stored by the ABI."
  (unless (and (= 3 (length normal))
               (every #'integerp normal)
               (some (complement #'zerop) normal))
    (error "Mesh normal is not a nonzero integer direction: ~S." normal))
  (mapcar #'signum normal))

(defun %pack-template-attributes
    (normal barycentric-index kind boundary-edge-mask)
  (let ((normal (%normal-direction-code normal)))
    (unless (<= 0 barycentric-index 2)
      (error "Unpackable barycentric index: ~S." barycentric-index))
    (let ((kind-code (ecase kind (:face 0) (:band 1) (:junction 2))))
      (logior (+ 1 (first normal))
              (ash (+ 1 (second normal)) 2)
              (ash (+ 1 (third normal)) 4)
              (ash barycentric-index 6)
              (ash kind-code 8)
              (ash boundary-edge-mask 10)))))

(defun %directional-star-ambient-occlusion (mask normal)
  "Quantize occupancy in NORMAL's outward local hemisphere to two AO bits."
  (check-type mask (unsigned-byte 8))
  (let ((samples 0)
        (occupied 0))
    (dotimes (sample 8)
      (when (loop for component in normal
                  for axis-number below 3
                  always (or (zerop component)
                             (= (signum component)
                                (if (logbitp axis-number sample) 1 -1))))
        (incf samples)
        (when (logbitp sample mask)
          (incf occupied))))
    (floor (+ (* 3 occupied) (floor samples 2)) samples)))

(defparameter *directional-ambient-occlusion-table*
  (let ((table (make-array (* 27 256) :element-type '(unsigned-byte 2))))
    (dotimes (trit-key 27 table)
      (let ((normal (list (- (mod trit-key 3) 1)
                          (- (mod (floor trit-key 3) 3) 1)
                          (- (floor trit-key 9) 1))))
        (dotimes (mask 256)
          (setf (aref table (+ (* trit-key 256) mask))
                (%directional-star-ambient-occlusion mask normal))))))
  "Directional AO for every (normal trit key, star mask) pair, built once.")

(declaim (inline %normal-trit-key %star-normal-ambient-occlusion))
(defun %normal-trit-key (nx ny nz)
  (+ (1+ (signum nx))
     (* 3 (1+ (signum ny)))
     (* 9 (1+ (signum nz)))))

(defun %star-normal-ambient-occlusion (star-mask nx ny nz)
  (aref *directional-ambient-occlusion-table*
        (+ (* (%normal-trit-key nx ny nz) 256) star-mask)))

;;; ---------------------------------------------------------------------------
;;; Packed lattice keys
;;;
;;; Unwrapped mesher coordinates: X and Y lie in [-1, 2^18], Z in [-1, 256].
;;; The +1 bias keeps boundary anchors one step below zero packable.  Numeric
;;; key order is (axis, x, y, z) lexicographic order.

(defconstant +mesh-key-x-shift+ 34)
(defconstant +mesh-key-y-shift+ 9)
(defconstant +mesh-key-axis-shift+ 59)

(declaim (inline %lattice-key %lattice-key-x %lattice-key-y %lattice-key-z))
(defun %lattice-key (x y z)
  (declare (optimize (speed 3) (safety 1))
           (type (integer -1 #.(ash 1 18)) x y)
           (type (integer -1 511) z))
  (logior (ash (+ x 1) +mesh-key-x-shift+)
          (ash (+ y 1) +mesh-key-y-shift+)
          (+ z 1)))

(defun %lattice-key-x (key)
  (- (ldb (byte 25 +mesh-key-x-shift+) key) 1))
(defun %lattice-key-y (key)
  (- (ldb (byte 25 +mesh-key-y-shift+) key) 1))
(defun %lattice-key-z (key)
  (- (ldb (byte 9 0) key) 1))

;;; ---------------------------------------------------------------------------
;;; Materialized occupancy
;;;
;;; A field binds one solid's occupancy to a cell-coordinate box.  Probes
;;; inside an ordinary-size box are one dense bit-vector lookup; probes outside
;;; it signal OUTSIDE-DOMAIN with the boundary restarts, so the caller's
;;; policy (whole-world air, a chunk store, a strict test) decides the edge.

(defstruct (occupancy-field
             (:constructor %make-occupancy-field
                 (domain bits table x0 x1 y0 y1 y-span)))
  (domain nil :type world-domain :read-only t)
  ;; Resident occupancy is dense unless the horizontal box would make an
  ;; unreasonable allocation.  The sparse fallback preserves whole-domain
  ;; meshing for very large, lightly populated worlds.
  (bits nil :type (or null simple-bit-vector) :read-only t)
  (table nil :type (or null hash-table) :read-only t)
  ;; Half-open cell-coordinate bounds of the resident box.
  (x0 0 :type fixnum :read-only t)
  (x1 0 :type fixnum :read-only t)
  (y0 0 :type fixnum :read-only t)
  (y1 0 :type fixnum :read-only t)
  (y-span 1 :type (integer 1 #.(ash 1 18)) :read-only t)
  ;; Resolutions of probes past the box but inside the world: chunk key to
  ;; :AIR, :SOLID, or a cell table, each obtained from one MISSING-CHUNK
  ;; signal and cached for every later probe into that chunk.
  (resolutions (make-hash-table :test #'eql) :type hash-table :read-only t))

(defun %chunk-cells-table (chain)
  "Index CHAIN's cells by packed lattice key, validating them as positive."
  (let ((table (make-hash-table :test #'eql
                                :size (max 64 (chain-count chain)))))
    (loop for cell across (%chain-sites chain) do
      (unless (and (= (site-extent cell) +cell-extent+)
                   (site-positive-p cell))
        (error "A solid mesh requires positive cells, not ~S." cell))
      (setf (gethash (%lattice-key (site-x cell) (site-y cell) (site-z cell))
                     table)
            t))
    table))

(defun %materialize-occupancy (solid x0 x1 y0 y1)
  "Validate SOLID's cells and materialize them over the given cell box."
  (declare (type fixnum x0 x1 y0 y1))
  (let* ((y-span (- y1 y0))
         (volume (* (- x1 x0) y-span +top-z+))
         (dense-p (<= volume (* 128 1024 1024)))
         (bits (when dense-p
                 (make-array volume :element-type 'bit :initial-element 0)))
         (table (unless dense-p
                  (make-hash-table :test #'eql
                                   :size (max 64 (chain-count solid))))))
    (loop for cell across (%chain-sites solid) do
      (unless (and (= (site-extent cell) +cell-extent+)
                   (site-positive-p cell))
        (error "A solid mesh requires positive cells, not ~S." cell))
      (let ((x (site-x cell)) (y (site-y cell)) (z (site-z cell)))
        (unless (and (<= x0 x) (< x x1) (<= y0 y) (< y y1)
                     (<= 0 z) (< z +top-z+))
          (error "Cell ~S lies outside its occupancy box." cell))
        (if bits
            (setf (sbit bits
                        (+ z (* +top-z+
                                (+ (- y y0) (* y-span (- x x0))))))
                  1)
            (setf (gethash (%lattice-key x y z) table) t))))
    (%make-occupancy-field
     (chain-domain solid) bits table x0 x1 y0 y1 y-span)))

(defun %resolve-chunk (field key)
  "Signal MISSING-CHUNK once for KEY and cache the handler's resolution."
  (let ((resolution
          (restart-case
              (error 'missing-chunk
                     :domain (occupancy-field-domain field) :key key)
            (use-chunk (chain)
              :report "Supply the chunk's chain."
              (%chunk-cells-table chain))
            (treat-as-air ()
              :report "Treat the whole chunk as air."
              :air)
            (treat-as-solid ()
              :report "Treat the whole chunk as solid."
              :solid))))
    (setf (gethash key (occupancy-field-resolutions field)) resolution)))

(defun %field-chunk-resolution (field key)
  (or (gethash key (occupancy-field-resolutions field))
      (%resolve-chunk field key)))

(declaim (inline %occupied-bit))
(declaim (inline %dense-occupancy-index))
(defun %dense-occupancy-index (field x y z)
  (declare (optimize (speed 3) (safety 1))
           (type occupancy-field field)
           (type fixnum x y z))
  (the fixnum
       (+ z
          (the fixnum
               (* +top-z+
                  (the fixnum
                       (+ (the fixnum (- y (occupancy-field-y0 field)))
                          (the fixnum
                               (* (occupancy-field-y-span field)
                                  (the fixnum
                                       (- x
                                          (occupancy-field-x0 field))))))))))))

(defun %occupied-bit (field domain x y z)
  "Occupancy of one cell: air beyond Z, a lookup inside the field's box,
one cached MISSING-CHUNK resolution per non-resident chunk inside the
world, and an OUTSIDE-DOMAIN signal past the world's own edges."
  (declare (optimize (speed 3) (safety 1))
           (type occupancy-field field)
           (type world-domain domain)
           (type fixnum x y z))
  (cond ((or (< z 0) (>= z +top-z+)) 0)
        ((and (<= (occupancy-field-x0 field) x)
              (< x (occupancy-field-x1 field))
              (<= (occupancy-field-y0 field) y)
              (< y (occupancy-field-y1 field)))
         (let ((bits (occupancy-field-bits field)))
           (if bits
               (sbit bits (%dense-occupancy-index field x y z))
               (if (gethash (%lattice-key x y z)
                            (occupancy-field-table field))
                   1
                   0))))
        ((or (< x 0) (>= x (world-domain-x-limit domain))
             (< y 0) (>= y (world-domain-y-limit domain)))
         (outside-domain-occupancy domain x y z))
        (t
         (let ((resolution (%field-chunk-resolution field (chunk-key-at x y))))
           (case resolution
             (:air 0)
             (:solid 1)
             (t (if (gethash (%lattice-key x y z) resolution) 1 0)))))))

(defun %star-mask-at (field domain x y z)
  "Pack the eight-cell occupancy star of the lattice vertex at X Y Z.

Bit conventions match SITE-STAR-OCCUPANCY-MASK on a vertex site."
  (declare (optimize (speed 3) (safety 1))
           (type occupancy-field field)
           (type world-domain domain)
           (type fixnum x y z))
  (let ((mask 0))
    (dotimes (sample 8 mask)
      (when (= 1 (%occupied-bit
                  field domain
                  (- x (if (logbitp 0 sample) 0 1))
                  (- y (if (logbitp 1 sample) 0 1))
                  (- z (if (logbitp 2 sample) 0 1))))
        (setf mask (logior mask (ash 1 sample)))))))

;;; ---------------------------------------------------------------------------
;;; Templates, instance streams, and the builder

(defstruct (mesh-template
             (:constructor %make-mesh-template (id vertices)))
  (id 0 :type (integer 0 *) :read-only t)
  ;; Packed vertices: (x+16) | (y+16)<<5 | (z+16)<<10 | attributes<<15.
  (vertices (make-array 0 :element-type 'fixnum)
            :type (simple-array fixnum (*)) :read-only t))

(defconstant +mesh-vertex-attribute-shift+
  (* 3 +mesh-template-coordinate-bit-count+))

(declaim (inline %pack-template-vertex))
(defun %pack-template-vertex (x y z attributes)
  (unless (and (<= (- +mesh-template-coordinate-bias+) x
                   (1- +mesh-template-coordinate-bias+))
               (<= (- +mesh-template-coordinate-bias+) y
                   (1- +mesh-template-coordinate-bias+))
               (<= (- +mesh-template-coordinate-bias+) z
                   (1- +mesh-template-coordinate-bias+)))
    (error "Template coordinate ~S does not fit the signed ~D-bit ABI."
           (list x y z) +mesh-template-coordinate-bit-count+))
  (logior (+ x +mesh-template-coordinate-bias+)
          (ash (+ y +mesh-template-coordinate-bias+)
               +mesh-template-coordinate-bit-count+)
          (ash (+ z +mesh-template-coordinate-bias+)
               (* 2 +mesh-template-coordinate-bit-count+))
          (ash attributes +mesh-vertex-attribute-shift+)))

(defstruct (instance-stream
             (:constructor %make-instance-stream-from-words (words)))
  ;; Four ABI words per instance in emission order: base x, y, z, meta.
  (words (make-array 0 :element-type '(unsigned-byte 32)
                        :adjustable t :fill-pointer 0)
         :type (vector (unsigned-byte 32))))

(defun %make-instance-stream (&optional (word-capacity 4096))
  (%make-instance-stream-from-words
   (make-array word-capacity :element-type '(unsigned-byte 32)
                             :adjustable t :fill-pointer 0)))

(defun instance-stream-count (stream)
  (/ (fill-pointer (instance-stream-words stream))
     +mesh-instance-word-count+))

(defstruct (surface-mesh-builder
             (:constructor %make-surface-mesh-builder (domain bevel-width)))
  (domain nil :type world-domain :read-only t)
  (bevel-width +mesh-bevel-width+ :type (integer 1 4) :read-only t)
  (templates (make-array 64 :adjustable t :fill-pointer 0)
             :type (vector t) :read-only t)
  ;; Content hash of the packed vertices to a bucket of template candidates.
  (template-index (make-hash-table :test #'eql) :type hash-table :read-only t)
  (vertex-scratch (make-array 6 :element-type 'fixnum)
                  :type (simple-array fixnum (6)) :read-only t)
  (face-stream (%make-instance-stream) :type instance-stream :read-only t)
  (band-stream (%make-instance-stream) :type instance-stream :read-only t)
  (fan-stream (%make-instance-stream) :type instance-stream :read-only t)
  ;; When configured for bevel construction, face and band emission update
  ;; this shared parity table directly from the already-oriented scratch
  ;; vertices.  Builders without it retain the general replay path used by
  ;; mesh transformations elsewhere in this file.
  (boundary-packing nil)
  (boundary-observations nil)
  (singular-star-count 0 :type (integer 0 *)))

(defun %reserve-builder-triangle-capacities
    (builder face-triangles band-triangles fan-triangles)
  "Replace an unused builder's default streams with measured capacities."
  (flet ((reserve (stream triangles)
           (setf (instance-stream-words stream)
                 (make-array (* +mesh-instance-word-count+ triangles)
                             :element-type '(unsigned-byte 32)
                             :adjustable t :fill-pointer 0))))
    (reserve (surface-mesh-builder-face-stream builder) face-triangles)
    (reserve (surface-mesh-builder-band-stream builder) band-triangles)
    (reserve (surface-mesh-builder-fan-stream builder) fan-triangles))
  builder)

(defun %intern-template (builder scratch count)
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array fixnum (*)) scratch)
           (type (integer 0 6) count))
  (let ((hash count))
    (declare (type fixnum hash))
    (dotimes (i count)
      (setf hash (logand (+ (* (logand hash #xffffffffffff) 67)
                            (aref scratch i))
                         most-positive-fixnum)))
    (let ((index (surface-mesh-builder-template-index builder)))
      (dolist (template (gethash hash index))
        (let ((vertices (mesh-template-vertices template)))
          (when (and (= (length vertices) count)
                     (loop for i below count
                           always (= (aref vertices i) (aref scratch i))))
            (return-from %intern-template template))))
      (let* ((templates (surface-mesh-builder-templates builder))
             (vertices (make-array count :element-type 'fixnum))
             (template (%make-mesh-template (fill-pointer templates)
                                            vertices)))
        (replace vertices scratch :end2 count)
        (vector-push-extend template templates)
        (push template (gethash hash index))
        template))))

(defun %builder-stream (builder kind)
  (ecase kind
    (:face (surface-mesh-builder-face-stream builder))
    (:band (surface-mesh-builder-band-stream builder))
    (:junction (surface-mesh-builder-fan-stream builder))))

(defun %emit-instance (builder kind base-x base-y base-z stock
                       ambient-occlusion count)
  "Intern the vertex scratch prefix and append one columnar instance."
  (check-type stock (unsigned-byte #.+mesh-instance-stock-bit-count+))
  (check-type ambient-occlusion (unsigned-byte 2))
  (unless (and (typep base-x '(unsigned-byte 32))
               (typep base-y '(unsigned-byte 32))
               (typep base-z '(unsigned-byte 32)))
    (error "Instance base coordinate is unsigned: ~S."
           (list base-x base-y base-z)))
  (when (and (member kind '(:face :band))
             (surface-mesh-builder-boundary-observations builder))
    (%observe-scratch-boundary-edges builder base-x base-y base-z stock count))
  (let* ((template (%intern-template
                    builder (surface-mesh-builder-vertex-scratch builder)
                    count))
         (words (instance-stream-words (%builder-stream builder kind))))
    (vector-push-extend base-x words)
    (vector-push-extend base-y words)
    (vector-push-extend base-z words)
    (vector-push-extend
     (logior (mesh-template-id template)
             (ash stock +mesh-instance-stock-shift+)
             (ash ambient-occlusion +mesh-instance-ambient-occlusion-shift+))
     words)
    template))

;;; ---------------------------------------------------------------------------
;;; Scalar triangle assembly

(defun %scratch-triangle (scratch offset kind-code boundary-edge-mask
                          ax ay az bx by bz cx cy cz nx ny nz)
  "Write one oriented triangle of template-local vertices; return next offset."
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array fixnum (*)) scratch)
           (type (integer 0 3) offset)
           (type (integer 0 2) kind-code)
           (type (unsigned-byte 3) boundary-edge-mask)
           (type (integer -2048 2047) ax ay az bx by bz cx cy cz)
           (type (signed-byte 29) nx ny nz))
  (let* ((ux (the (signed-byte 13) (- bx ax)))
         (uy (the (signed-byte 13) (- by ay)))
         (uz (the (signed-byte 13) (- bz az)))
         (vx (the (signed-byte 13) (- cx ax)))
         (vy (the (signed-byte 13) (- cy ay)))
         (vz (the (signed-byte 13) (- cz az)))
         (px (the (signed-byte 26)
               (- (the fixnum (* uy vz)) (the fixnum (* uz vy)))))
         (py (the (signed-byte 26)
               (- (the fixnum (* uz vx)) (the fixnum (* ux vz)))))
         (pz (the (signed-byte 26)
               (- (the fixnum (* ux vy)) (the fixnum (* uy vx)))))
         (orientation
           (the fixnum
             (+ (the fixnum (* px nx))
                (the fixnum (* py ny))
                (the fixnum (* pz nz))))))
    (when (zerop orientation)
      (error "Degenerate ~[face~;band~;junction~] triangle ~S ~S ~S."
             kind-code (list ax ay az) (list bx by bz) (list cx cy cz)))
    (when (minusp orientation)
      (rotatef bx cx) (rotatef by cy) (rotatef bz cz))
    (let ((attributes (logior (1+ (signum nx))
                              (ash (1+ (signum ny)) 2)
                              (ash (1+ (signum nz)) 4)
                              (ash kind-code 8)
                              (ash boundary-edge-mask 10))))
      (setf (aref scratch offset)
            (%pack-template-vertex ax ay az attributes)
            (aref scratch (+ offset 1))
            (%pack-template-vertex bx by bz (logior attributes (ash 1 6)))
            (aref scratch (+ offset 2))
            (%pack-template-vertex cx cy cz (logior attributes (ash 2 6))))))
  (+ offset 3))

(defun %emit-quad (builder kind base-x base-y base-z p0 p1 p2 p3
                   nx ny nz stock ambient-occlusion)
  "Emit one instance for the quad P0 P1 P2 P3 (global ticks, simple-vectors)."
  (let ((ox (* +mesh-cell-size+ base-x))
        (oy (* +mesh-cell-size+ base-y))
        (oz (* +mesh-cell-size+ base-z))
        (scratch (surface-mesh-builder-vertex-scratch builder))
        (kind-code (ecase kind (:face 0) (:band 1))))
    (let ((offset (%scratch-triangle
                   scratch 0 kind-code #b101
                   (- (svref p0 0) ox) (- (svref p0 1) oy) (- (svref p0 2) oz)
                   (- (svref p1 0) ox) (- (svref p1 1) oy) (- (svref p1 2) oz)
                   (- (svref p2 0) ox) (- (svref p2 1) oy) (- (svref p2 2) oz)
                   nx ny nz)))
      (%scratch-triangle
       scratch offset kind-code #b011
       (- (svref p0 0) ox) (- (svref p0 1) oy) (- (svref p0 2) oz)
       (- (svref p2 0) ox) (- (svref p2 1) oy) (- (svref p2 2) oz)
       (- (svref p3 0) ox) (- (svref p3 1) oy) (- (svref p3 2) oz)
       nx ny nz))
    (%emit-instance builder kind base-x base-y base-z stock
                    ambient-occlusion 6)))

(defun %emit-fan-triangle (builder base-x base-y base-z boundary-edge-mask
                           ax ay az bx by bz cx cy cz nx ny nz
                           stock star-mask)
  "Emit one junction triangle whose coordinates are already site-local."
  (let ((scratch (surface-mesh-builder-vertex-scratch builder)))
    (%scratch-triangle scratch 0 2 boundary-edge-mask
                       ax ay az bx by bz cx cy cz nx ny nz)
    (%emit-instance builder :junction base-x base-y base-z stock
                    (%star-normal-ambient-occlusion star-mask nx ny nz) 3)))

;;; ---------------------------------------------------------------------------
;;; Cell faces

;; Tangent axis numbers (u v) for each normal axis number.
(defconstant +axis-u+ (if (boundp '+axis-u+) (symbol-value '+axis-u+) #(1 0 0)))
(defconstant +axis-v+ (if (boundp '+axis-v+) (symbol-value '+axis-v+) #(2 2 1)))

(defun %emit-cell-face
    (builder field domain cell axis-number side stock-function
     chamfer-stock-function)
  (declare (optimize (speed 3) (safety 1)))
  (let* ((bevel-width (surface-mesh-builder-bevel-width builder))
         (cx (site-x cell)) (cy (site-y cell)) (cz (site-z cell))
         (u (svref +axis-u+ axis-number))
         (v (svref +axis-v+ axis-number))
         (face (if (minusp side)
                   (site-boundary-low domain cell (index-axis axis-number))
                   (site-boundary-high domain cell (index-axis axis-number))))
         (stock (funcall stock-function face))
         ;; The central square remains face-owned.  Its four collars are the
         ;; planar part of the chamfer, so they must take the same one-stock
         ;; policy as bevel bands and vertex closures.
         (chamfer-stock (funcall chamfer-stock-function (list stock)))
         (nx (if (= axis-number 0) side 0))
         (ny (if (= axis-number 1) side 0))
         (nz (if (= axis-number 2) side 0))
         (coords (make-array 3))
         (u-cuts (make-array 4))
         (v-cuts (make-array 4))
         (p0 (make-array 3)) (p1 (make-array 3))
         (p2 (make-array 3)) (p3 (make-array 3)))
    (declare (dynamic-extent coords u-cuts v-cuts p0 p1 p2 p3))
    (setf (svref coords 0) cx (svref coords 1) cy (svref coords 2) cz)
    (labels ((crease-p (tangent-axis tangent-side)
               ;; A crease insets the face edge unless the coplanar tangent
               ;; neighbor continues the surface flat.
               (let ((x cx) (y cy) (z cz))
                 (case tangent-axis
                   (0 (incf x tangent-side))
                   (1 (incf y tangent-side))
                   (t (incf z tangent-side)))
                 (let ((neighbor (%occupied-bit field domain x y z)))
                   (case axis-number
                     (0 (incf x side))
                     (1 (incf y side))
                     (t (incf z side)))
                   (not (and (= 1 neighbor)
                             (= 0 (%occupied-bit field domain x y z))))))))
      (let* ((plane (* +mesh-cell-size+
                       (+ (svref coords axis-number)
                          (if (plusp side) 1 0))))
             (u-anchor (* +mesh-cell-size+ (svref coords u)))
             (v-anchor (* +mesh-cell-size+ (svref coords v))))
        (setf (svref u-cuts 0)
              (+ u-anchor (if (crease-p u -1) bevel-width 0))
              (svref u-cuts 1) (+ u-anchor bevel-width)
              (svref u-cuts 2) (- (+ u-anchor +mesh-cell-size+) bevel-width)
              (svref u-cuts 3)
              (- (+ u-anchor +mesh-cell-size+)
                 (if (crease-p u 1) bevel-width 0))
              (svref v-cuts 0)
              (+ v-anchor (if (crease-p v -1) bevel-width 0))
              (svref v-cuts 1) (+ v-anchor bevel-width)
              (svref v-cuts 2) (- (+ v-anchor +mesh-cell-size+) bevel-width)
              (svref v-cuts 3)
              (- (+ v-anchor +mesh-cell-size+)
                 (if (crease-p v 1) bevel-width 0)))
        (flet ((set-point (point uu vv)
                 (setf (svref point axis-number) plane
                       (svref point u) uu
                       (svref point v) vv)))
          ;; Uniformly partition the exact old face rectangle.  Its 6x6 heart
          ;; is face-owned and its nonempty side cells are edge-owned.  Leave
          ;; the four corner cells open: the lattice-site fan closes their
          ;; actual boundary after every face and edge instance is present.
          (dotimes (u-cell 3)
            (dotimes (v-cell 3)
              (let ((u0 (svref u-cuts u-cell))
                    (u1 (svref u-cuts (1+ u-cell)))
                    (v0 (svref v-cuts v-cell))
                    (v1 (svref v-cuts (1+ v-cell))))
                (when (and (< u0 u1) (< v0 v1)
                           (or (= u-cell 1) (= v-cell 1)))
                  (let ((kind (if (and (= u-cell 1) (= v-cell 1))
                                  :face
                                  :band))
                        (base-x cx) (base-y cy) (base-z cz))
                    (macrolet ((bump (axis-form amount)
                                 `(let ((axis ,axis-form))
                                    (case axis
                                      (0 (incf base-x ,amount))
                                      (1 (incf base-y ,amount))
                                      (t (incf base-z ,amount))))))
                      (bump axis-number (if (plusp side) 1 0))
                      (bump u (if (= u-cell 2) 1 0))
                      (bump v (if (= v-cell 2) 1 0)))
                    (set-point p0 u0 v0)
                    (set-point p1 u1 v0)
                    (set-point p2 u1 v1)
                    (set-point p3 u0 v1)
                    (%emit-quad builder kind base-x base-y base-z
                                p0 p1 p2 p3 nx ny nz
                                (if (eq kind :face) stock chamfer-stock)
                                0)))))))))))

;;; ---------------------------------------------------------------------------
;;; Edge bands

;; Quadrant (u v) components around a lattice edge, in cyclic order.
(defconstant +quadrant-u+
  (if (boundp '+quadrant-u+) (symbol-value '+quadrant-u+) #(-1 1 1 -1)))
(defconstant +quadrant-v+
  (if (boundp '+quadrant-v+) (symbol-value '+quadrant-v+) #(-1 -1 1 1)))

(defun %edge-run-transition-groups (states)
  "Group the transition indices of one four-bit quadrant occupancy run."
  (let ((transitions
          (loop for index below 4
                unless (eq (logbitp index states)
                           (logbitp (mod (1+ index) 4) states))
                  collect index)))
    (case (length transitions)
      (0 nil)
      (2 (list transitions))
      (4 (loop for index below 4
               when (logbitp index states)
                 collect (list (mod (+ index 3) 4) index)))
      (t (error "Impossible edge transition count ~D." (length transitions))))))

(defparameter *edge-transition-group-table*
  (let ((table (make-array 16)))
    (dotimes (states 16 table)
      (setf (svref table states) (%edge-run-transition-groups states))))
  "Transition groups for every quadrant occupancy pattern, built once.")

(defun %make-edge-candidates (cell-count)
  "An emission-order buffer of packed lattice edges discovered at faces."
  (make-array (max 64 cell-count)
              :element-type '(unsigned-byte 64)
              :adjustable t :fill-pointer 0))

(defun %append-face-edge-keys
    (candidates cell-x cell-y cell-z axis-number side)
  "Append the four lattice edges of one exposed cell face."
  (declare (optimize (speed 3) (safety 1))
           (type (vector (unsigned-byte 64)) candidates)
           (type fixnum cell-x cell-y cell-z axis-number side))
  (let ((x cell-x) (y cell-y) (z cell-z)
        (u (svref +axis-u+ axis-number))
        (v (svref +axis-v+ axis-number)))
    (macrolet ((bump (axis amount)
                 `(case ,axis
                    (0 (incf x ,amount))
                    (1 (incf y ,amount))
                    (t (incf z ,amount))))
               (edge (edge-axis)
                 `(vector-push-extend
                   (logior (ash ,edge-axis +mesh-key-axis-shift+)
                           (%lattice-key x y z))
                   candidates)))
      (when (plusp side) (bump axis-number 1))
      (edge u)
      (bump v 1)
      (edge u)
      (bump v -1)
      (edge v)
      (bump u 1)
      (edge v)))
  candidates)

(defun %unique-edge-candidates (candidates)
  "Sort and compact CANDIDATES into an exact simple packed edge vector."
  (declare (optimize (speed 3) (safety 1))
           (type (vector (unsigned-byte 64)) candidates))
  (sort candidates #'<)
  (let* ((count (fill-pointer candidates))
         (unique-count
           (if (zerop count)
               0
               (let ((write 1)
                     (previous (aref candidates 0)))
                 (loop for read from 1 below count
                       for key = (aref candidates read)
                       unless (= key previous)
                         do (setf (aref candidates write) key
                                  previous key)
                            (incf write))
                 write)))
         (result (make-array unique-count
                             :element-type '(unsigned-byte 64))))
    (replace result candidates :end2 unique-count)
    result))

(defun %chain-cell-keys (chain)
  "The packed lattice keys of CHAIN's cells, in chain order."
  (let* ((sites (%chain-sites chain))
         (keys (make-array (length sites) :element-type '(unsigned-byte 64))))
    (loop for cell across sites
          for index from 0
          do (setf (aref keys index)
                   (%lattice-key (site-x cell) (site-y cell) (site-z cell))))
    keys))

(defun %emit-edge-bands
    (builder field domain key stock-function chamfer-stock-function)
  (declare (optimize (speed 3) (safety 1)))
  (let* ((bevel-width (surface-mesh-builder-bevel-width builder))
         (axis-number (ash key (- +mesh-key-axis-shift+)))
         (ax (%lattice-key-x key))
         (ay (%lattice-key-y key))
         (az (%lattice-key-z key))
         (u (svref +axis-u+ axis-number))
         (v (svref +axis-v+ axis-number))
         (states 0))
    (flet ((quadrant-cell-x (index)
             (let ((x ax))
               (when (and (= u 0) (minusp (svref +quadrant-u+ index)))
                 (decf x))
               (when (and (= v 0) (minusp (svref +quadrant-v+ index)))
                 (decf x))
               x))
           (quadrant-cell-y (index)
             (let ((y ay))
               (when (and (= u 1) (minusp (svref +quadrant-u+ index)))
                 (decf y))
               (when (and (= v 1) (minusp (svref +quadrant-v+ index)))
                 (decf y))
               y))
           (quadrant-cell-z (index)
             (let ((z az))
               (when (and (= u 2) (minusp (svref +quadrant-u+ index)))
                 (decf z))
               (when (and (= v 2) (minusp (svref +quadrant-v+ index)))
                 (decf z))
               z)))
      (dotimes (index 4)
        (when (= 1 (%occupied-bit field domain
                                  (quadrant-cell-x index)
                                  (quadrant-cell-y index)
                                  (quadrant-cell-z index)))
          (setf states (logior states (ash 1 index)))))
      (flet ((transition (transition-index)
               ;; Resolve one occupancy transition into its face normal axis
               ;; and sign, the occupied side's cross-axis offset sign, and
               ;; the oriented boundary face carrying the stock.
               (let* ((next-index (mod (1+ transition-index) 4))
                      (occupied-index (if (logbitp transition-index states)
                                          transition-index
                                          next-index))
                      (empty-index (if (= occupied-index transition-index)
                                       next-index
                                       transition-index))
                      (qu-occupied (svref +quadrant-u+ occupied-index))
                      (qv-occupied (svref +quadrant-v+ occupied-index))
                      (qu-empty (svref +quadrant-u+ empty-index))
                      (qv-empty (svref +quadrant-v+ empty-index))
                      (normal-axis (if (/= qu-occupied qu-empty) u v))
                      (other-axis (if (= normal-axis u) v u))
                      (normal-sign (if (= normal-axis u) qu-empty qv-empty))
                      (other-sign (if (= other-axis u)
                                      qu-occupied
                                      qv-occupied))
                      (cell (make-site domain
                                       (quadrant-cell-x occupied-index)
                                       (quadrant-cell-y occupied-index)
                                       (quadrant-cell-z occupied-index)
                                       +cell-extent+ 1))
                      (face (if (minusp normal-sign)
                                (site-boundary-low
                                 domain cell (index-axis normal-axis))
                                (site-boundary-high
                                 domain cell (index-axis normal-axis)))))
                 (values normal-axis normal-sign other-axis other-sign
                         face))))
        (dolist (group (svref *edge-transition-group-table* states))
          (multiple-value-bind (left-axis left-sign left-other
                                left-other-sign left-face)
              (transition (first group))
            (multiple-value-bind (right-axis right-sign right-other
                                  right-other-sign right-face)
                (transition (second group))
              ;; Equal normals are one flat face continued across a cell
              ;; boundary; opposite normals are two sheets touching at the
              ;; lattice edge.  Neither relation owns a bevel band.
              (unless (= left-axis right-axis)
                (let ((nx 0) (ny 0) (nz 0)
                      (left-rail (make-array 3))
                      (right-rail (make-array 3))
                      (p0 (make-array 3)) (p1 (make-array 3))
                      (p2 (make-array 3)) (p3 (make-array 3)))
                  (declare (dynamic-extent left-rail right-rail p0 p1 p2 p3))
                  (macrolet ((add-normal (axis-form sign-form)
                               `(let ((axis ,axis-form))
                                  (case axis
                                    (0 (incf nx ,sign-form))
                                    (1 (incf ny ,sign-form))
                                    (t (incf nz ,sign-form))))))
                    (add-normal left-axis left-sign)
                    (add-normal right-axis right-sign))
                  (setf (svref left-rail 0) (* +mesh-cell-size+ ax)
                        (svref left-rail 1) (* +mesh-cell-size+ ay)
                        (svref left-rail 2) (* +mesh-cell-size+ az))
                  (replace right-rail left-rail)
                  (incf (svref left-rail left-other)
                        (* bevel-width left-other-sign))
                  (incf (svref right-rail right-other)
                        (* bevel-width right-other-sign))
                  (let* ((axis-low
                           (* +mesh-cell-size+
                              (ecase axis-number (0 ax) (1 ay) (2 az))))
                         (low (+ axis-low bevel-width))
                         (high (- (+ axis-low +mesh-cell-size+)
                                  bevel-width))
                         (stock
                           (funcall chamfer-stock-function
                                    (list (funcall stock-function left-face)
                                          (funcall stock-function
                                                   right-face))))
                         (ambient-occlusion
                           (%star-normal-ambient-occlusion
                            (%star-mask-at field domain ax ay az)
                            nx ny nz)))
                    (flet ((rail-point (point rail coordinate)
                             (replace point rail)
                             (setf (svref point axis-number) coordinate)))
                      ;; The edge owns the middle after both vertex-site
                      ;; domains are removed.  Those width-sized ends belong
                      ;; to the two fans.
                      (rail-point p0 left-rail low)
                      (rail-point p1 right-rail low)
                      (rail-point p2 right-rail high)
                      (rail-point p3 left-rail high)
                      (%emit-quad builder :band ax ay az p0 p1 p2 p3
                                  nx ny nz stock ambient-occlusion))))))))))))

;;; ---------------------------------------------------------------------------
;;; Singular vertex stars

(defun %count-singular-vertex-stars
    (builder field domain cells ox0 ox1 oy0 oy1)
  "Count singular stars at the owned lattice vertices incident to CELLS.

Ownership is the half-open box [OX0, OX1) x [OY0, OY1) of site coordinates."
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array (unsigned-byte 64) (*)) cells))
  (let ((seen (make-hash-table :test #'eql
                               :size (* 4 (max 8 (length cells)))))
        (count 0))
    (loop for cell-key across cells do
      (let ((cx (%lattice-key-x cell-key))
            (cy (%lattice-key-y cell-key))
            (cz (%lattice-key-z cell-key)))
        (dotimes (sample 8)
          (setf (gethash (%lattice-key
                          (+ cx (if (logbitp 0 sample) 1 0))
                          (+ cy (if (logbitp 1 sample) 1 0))
                          (+ cz (if (logbitp 2 sample) 1 0)))
                         seen)
                t))))
    (loop for key being the hash-keys of seen
          do (let ((x (%lattice-key-x key))
                   (y (%lattice-key-y key))
                   (z (%lattice-key-z key)))
               (when (and (<= ox0 x) (< x ox1) (<= oy0 y) (< y oy1)
                          (= 1 (sbit *star-singular-bits*
                                     (%star-mask-at field domain x y z))))
                 (incf count))))
    (incf (surface-mesh-builder-singular-star-count builder) count)))

;;; ---------------------------------------------------------------------------
;;; Open-boundary parity scan
;;;
;;; Every face and band triangle edge is counted against a canonical key: the
;;; per-axis floor-of-eighths anchor of its two endpoints, plus both endpoints
;;; as 12-bit anchor-local coordinates.  Edges observed once are the open
;;; boundary; each is then attributed to the lattice vertex whose bevel domain
;;; contains it and packed as one fixnum of biased site-local endpoints:
;;; left12<<24 | right12<<12 | stock12, where a point12 is
;;; (x+4)<<8|(y+4)<<4|(z+4).
;;;
;;; The key itself is one fixnum, because a spatial-edge packing states the
;;; horizontal anchor box the scan covers -- one chunk plus its halo, or a
;;; whole solid's own extent -- and stores anchors relative to that box's
;;; origin: ((x * y-span + y) * 257 + (z + 1)) << 24 | undirected edge.  A
;;; solid too wide for that product to stay a fixnum has to be meshed by
;;; chunks; MESH-CHUNK's boxes are always small enough.

(declaim (inline %fan-point %fan-point-x %fan-point-y %fan-point-z
                 %fan-record-left %fan-record-right %fan-record-stock
                 %fan-record-undirected))
(defconstant +fan-local-bias+ 4)
(defconstant +fan-origin-point+
  (logior (ash +fan-local-bias+ 8) (ash +fan-local-bias+ 4) +fan-local-bias+))
(defconstant +fan-record-stock-bit-count+ 12)
(defconstant +fan-record-right-shift+ 12)
(defconstant +fan-record-left-shift+ 24)
(defconstant +boundary-observation-count-shift+ 36)

(defun %fan-point (x y z)
  (logior (ash (+ x +fan-local-bias+) 8)
          (ash (+ y +fan-local-bias+) 4)
          (+ z +fan-local-bias+)))
(defun %fan-point-x (point) (- (ldb (byte 4 8) point) +fan-local-bias+))
(defun %fan-point-y (point) (- (ldb (byte 4 4) point) +fan-local-bias+))
(defun %fan-point-z (point) (- (ldb (byte 4 0) point) +fan-local-bias+))

(defun %fan-record-left (record)
  (ldb (byte 12 +fan-record-left-shift+) record))
(defun %fan-record-right (record)
  (ldb (byte 12 +fan-record-right-shift+) record))
(defun %fan-record-stock (record)
  (ldb (byte +fan-record-stock-bit-count+ 0) record))
(defun %fan-record-undirected (record)
  (let ((left (%fan-record-left record))
        (right (%fan-record-right record)))
    (if (< left right)
        (logior (ash left 12) right)
        (logior (ash right 12) left))))

(defconstant +spatial-edge-anchor-z-span+ 257
  "Anchor Z values run from -1 through 255 inclusive.")

(defstruct (spatial-edge-packing
             (:constructor %make-spatial-edge-packing
                 (origin-x origin-y y-span x-stride)))
  "The anchor box one parity scan covers, and thus its fixnum key layout."
  (origin-x 0 :type fixnum :read-only t)
  (origin-y 0 :type fixnum :read-only t)
  (y-span 1 :type fixnum :read-only t)
  (x-stride +spatial-edge-anchor-z-span+ :type fixnum :read-only t))

(defun %make-spatial-edge-packing-for-box (x0 x1 y0 y1)
  "Pack anchors of the cell box [X0, X1) x [Y0, Y1).

Instance bases lie inside the box and template offsets reach at most one
eighth-cell anchor beyond it, so the packed anchor box is widened by two."
  (let* ((origin-x (- x0 2))
         (origin-y (- y0 2))
         (x-span (+ (- x1 x0) 4))
         (y-span (+ (- y1 y0) 4)))
    (unless (typep (ash (* x-span y-span
                           +spatial-edge-anchor-z-span+)
                        24)
                   'fixnum)
      (error "A solid spanning ~Dx~D cells is too wide for one spatial-edge ~
              scan; mesh it by chunks."
             x-span y-span))
    (%make-spatial-edge-packing
     origin-x origin-y y-span
     (* y-span +spatial-edge-anchor-z-span+))))

(declaim (inline %spatial-edge-key-from-anchor
                 %spatial-edge-key-anchor-x
                 %spatial-edge-key-anchor-y %spatial-edge-key-anchor-z))
(defun %spatial-edge-key-from-anchor
    (packing anchor-x anchor-y anchor-z edge)
  (let ((anchor-index
          (the fixnum
            (+ (the fixnum
                 (* (the fixnum
                      (- anchor-x
                         (spatial-edge-packing-origin-x packing)))
                    (spatial-edge-packing-x-stride packing)))
               (the fixnum
                 (* (the fixnum
                      (- anchor-y
                         (spatial-edge-packing-origin-y packing)))
                    +spatial-edge-anchor-z-span+))
               (the fixnum (1+ anchor-z))))))
    (the fixnum (logior (the fixnum (ash anchor-index 24)) edge))))

(defun %spatial-edge-key-anchor-x (packing key)
  (+ (spatial-edge-packing-origin-x packing)
     (truncate (ash key -24)
               (spatial-edge-packing-x-stride packing))))
(defun %spatial-edge-key-anchor-y (packing key)
  (+ (spatial-edge-packing-origin-y packing)
     (mod (truncate (ash key -24) +spatial-edge-anchor-z-span+)
          (spatial-edge-packing-y-span packing))))
(defun %spatial-edge-key-anchor-z (key)
  (1- (mod (ash key -24) +spatial-edge-anchor-z-span+)))

(declaim (inline %pack-spatial-edge))
(defun %pack-spatial-edge (packing lx ly lz rx ry rz)
  "Return one bounded-box key and the directed local endpoints of an edge.

PACKING supplies only the anchor's horizontal frame.  The low 24 key bits are
the two undirected 12-bit endpoint coordinates inside their shared eighth-cell
anchor.  This codec is shared by open-sheet boundary assembly and by local
variable-bevel transition repair; boundary status is a use of the key, not a
different geometric representation."
  (declare (optimize (speed 3) (safety 1))
           (type spatial-edge-packing packing)
           (type fixnum lx ly lz rx ry rz))
  (let* ((anchor-x (ash (min lx rx) -3))
         (anchor-y (ash (min ly ry) -3))
         (anchor-z (ash (min lz rz) -3))
         (lox (- lx (ash anchor-x 3)))
         (loy (- ly (ash anchor-y 3)))
         (loz (- lz (ash anchor-z 3)))
         (rox (- rx (ash anchor-x 3)))
         (roy (- ry (ash anchor-y 3)))
         (roz (- rz (ash anchor-z 3))))
    (unless (and (<= 0 lox 15) (<= 0 loy 15) (<= 0 loz 15)
                 (<= 0 rox 15) (<= 0 roy 15) (<= 0 roz 15))
      (error "Mesh edge ~S--~S spans more than one anchor cell."
             (list lx ly lz) (list rx ry rz)))
    (let* ((left12 (logior (ash lox 8) (ash loy 4) loz))
           (right12 (logior (ash rox 8) (ash roy 4) roz))
           (edge (if (< left12 right12)
                     (logior (ash left12 12) right12)
                     (logior (ash right12 12) left12))))
      (values (%spatial-edge-key-from-anchor
               packing anchor-x anchor-y anchor-z edge)
              left12 right12))))

(defun %spatial-edge-points (packing key)
  "Decode KEY to its two lexicographically ordered global tick points."
  (let* ((anchor-x (%spatial-edge-key-anchor-x packing key))
         (anchor-y (%spatial-edge-key-anchor-y packing key))
         (anchor-z (%spatial-edge-key-anchor-z key))
         (edge (ldb (byte 24 0) key))
         (left (ldb (byte 12 12) edge))
         (right (ldb (byte 12 0) edge)))
    (flet ((point (local)
             (list (+ (ash anchor-x 3) (ldb (byte 4 8) local))
                   (+ (ash anchor-y 3) (ldb (byte 4 4) local))
                   (+ (ash anchor-z 3) (ldb (byte 4 0) local)))))
      (values (point left) (point right)))))

(defconstant +source-anchor-filter-bit-limit+ (* 16 1024 1024))
(defconstant +maximum-variable-bevel-displacement+ 3)

(defstruct (source-anchor-filter
             (:constructor %make-source-anchor-filter-record
                 (bits table x0 y0 z0 x-span y-span z-span)))
  "A dense-or-sparse conservative preimage of transformed spatial edges."
  (bits nil :type (or null simple-bit-vector) :read-only t)
  (table nil :type (or null hash-table) :read-only t)
  (x0 0 :type fixnum :read-only t)
  (y0 0 :type fixnum :read-only t)
  (z0 0 :type fixnum :read-only t)
  (x-span 1 :type fixnum :read-only t)
  (y-span 1 :type fixnum :read-only t)
  (z-span 1 :type fixnum :read-only t))

(declaim (inline %spatial-edge-minimum-components))
(defun %spatial-edge-minimum-components (packing key)
  "Decode the componentwise minimum endpoint of one spatial-edge KEY."
  (let* ((anchor-x (%spatial-edge-key-anchor-x packing key))
         (anchor-y (%spatial-edge-key-anchor-y packing key))
         (anchor-z (%spatial-edge-key-anchor-z key))
         (edge (ldb (byte 24 0) key))
         (left (ldb (byte 12 12) edge))
         (right (ldb (byte 12 0) edge)))
    (values (+ (ash anchor-x 3)
               (min (ldb (byte 4 8) left) (ldb (byte 4 8) right)))
            (+ (ash anchor-y 3)
               (min (ldb (byte 4 4) left) (ldb (byte 4 4) right)))
            (+ (ash anchor-z 3)
               (min (ldb (byte 4 0) left) (ldb (byte 4 0) right))))))

(defun %make-source-anchor-filter (packing spatial-edges)
  "Return the bounded preimage of SPATIAL-EDGES under variable bevel motion.

A width-one endpoint moves by at most three ticks per axis.  Componentwise
edge minima therefore move by at most three ticks, so each transformed edge
has at most two possible source anchors per axis and eight in total.  The
filter may admit false positives, but cannot omit an edge that can transform
to one of SPATIAL-EDGES."
  (when (plusp (hash-table-count spatial-edges))
    (let ((x0 most-positive-fixnum) (x1 most-negative-fixnum)
          (y0 most-positive-fixnum) (y1 most-negative-fixnum)
          (z0 most-positive-fixnum) (z1 most-negative-fixnum))
      (loop for key being the hash-keys of spatial-edges do
        (multiple-value-bind (x y z)
            (%spatial-edge-minimum-components packing key)
          (let ((low-x
                  (ash (- x +maximum-variable-bevel-displacement+) -3))
                (high-x
                  (ash (+ x +maximum-variable-bevel-displacement+) -3))
                (low-y
                  (ash (- y +maximum-variable-bevel-displacement+) -3))
                (high-y
                  (ash (+ y +maximum-variable-bevel-displacement+) -3))
                (low-z
                  (ash (- z +maximum-variable-bevel-displacement+) -3))
                (high-z
                  (ash (+ z +maximum-variable-bevel-displacement+) -3)))
            (setf x0 (min x0 low-x) x1 (max x1 high-x)
                  y0 (min y0 low-y) y1 (max y1 high-y)
                  z0 (min z0 low-z) z1 (max z1 high-z)))))
      (let* ((x-span (1+ (- x1 x0)))
             (y-span (1+ (- y1 y0)))
             (z-span (1+ (- z1 z0)))
             (volume (* x-span y-span z-span))
             (bits
               (when (<= volume +source-anchor-filter-bit-limit+)
                 (make-array volume :element-type 'bit :initial-element 0)))
             (table
               (unless bits
                 (make-hash-table
                  :test #'eql
                  :size (max 16 (* 8 (hash-table-count spatial-edges))))))
             (filter
               (%make-source-anchor-filter-record
                bits table x0 y0 z0 x-span y-span z-span)))
        (flet ((mark (x y z)
                 (if bits
                     (setf (sbit bits
                                 (+ (- z z0)
                                    (* z-span
                                       (+ (- y y0)
                                          (* y-span (- x x0))))))
                           1)
                     (setf (gethash (%lattice-key x y z) table) t))))
          (loop for key being the hash-keys of spatial-edges do
            (multiple-value-bind (x y z)
                (%spatial-edge-minimum-components packing key)
              (loop for source-x
                      from (ash (- x +maximum-variable-bevel-displacement+) -3)
                        to (ash (+ x +maximum-variable-bevel-displacement+) -3)
                    do (loop for source-y
                              from (ash
                                    (- y +maximum-variable-bevel-displacement+)
                                    -3)
                                to (ash
                                    (+ y +maximum-variable-bevel-displacement+)
                                    -3)
                             do (loop for source-z
                                       from (ash
                                             (- z
                                                +maximum-variable-bevel-displacement+)
                                             -3)
                                         to (ash
                                             (+ z
                                                +maximum-variable-bevel-displacement+)
                                             -3)
                                      do (mark source-x source-y source-z)))))))
        filter))))

(declaim (inline %source-edge-anchor-filter-member-p
                 %triangle-touches-source-anchor-filter-p))
(defun %source-edge-anchor-filter-member-p
    (filter lx ly lz rx ry rz)
  (declare (optimize (speed 3) (safety 1))
           (type source-anchor-filter filter)
           (type fixnum lx ly lz rx ry rz))
  (let ((x (ash (min lx rx) -3))
        (y (ash (min ly ry) -3))
        (z (ash (min lz rz) -3))
        (bits (source-anchor-filter-bits filter)))
    (if bits
        (let ((dx (the fixnum (- x (source-anchor-filter-x0 filter))))
              (dy (the fixnum (- y (source-anchor-filter-y0 filter))))
              (dz (the fixnum (- z (source-anchor-filter-z0 filter)))))
          (declare (type fixnum dx dy dz))
          (and (<= 0 dx) (< dx (source-anchor-filter-x-span filter))
               (<= 0 dy) (< dy (source-anchor-filter-y-span filter))
               (<= 0 dz) (< dz (source-anchor-filter-z-span filter))
               (= 1 (sbit bits
                          (the fixnum
                            (+ dz
                               (the fixnum
                                 (* (source-anchor-filter-z-span filter)
                                    (the fixnum
                                      (+ dy
                                         (the fixnum
                                           (* (source-anchor-filter-y-span filter)
                                              dx))))))))))))
        (gethash (%lattice-key x y z)
                 (source-anchor-filter-table filter)))))

(defun %triangle-touches-source-anchor-filter-p
    (filter ax ay az bx by bz cx cy cz)
  (declare (optimize (speed 3) (safety 1))
           (type source-anchor-filter filter)
           (type fixnum ax ay az bx by bz cx cy cz))
  (or (%source-edge-anchor-filter-member-p filter ax ay az bx by bz)
      (%source-edge-anchor-filter-member-p filter bx by bz cx cy cz)
      (%source-edge-anchor-filter-member-p filter cx cy cz ax ay az)))

(defun %stream-triangle-count (stream templates)
  (declare (optimize (speed 3) (safety 1)))
  (let ((words (instance-stream-words stream))
        (count 0))
    (loop for offset from 3 below (fill-pointer words)
          by +mesh-instance-word-count+
          do (incf count
                   (truncate
                    (length (mesh-template-vertices
                             (aref templates
                                   (ldb (byte 16 0) (aref words offset)))))
                    3)))
    count))

(defun %builder-open-boundary-table (builders packing)
  "Parity-count the BUILDERS' face and band streams' directed triangle edges.

Returns a table from PACKING's fixnum edge keys to count<<36 | left12<<24 |
right12<<12 | stock12, where the 12-bit points are anchor-local."
  (declare (optimize (speed 3) (safety 1)))
  (let ((observations
          (surface-mesh-builder-boundary-observations (first builders))))
    (when observations
      (unless (every (lambda (builder)
                       (and (eq observations
                                (surface-mesh-builder-boundary-observations
                                 builder))
                            (eq packing
                                (surface-mesh-builder-boundary-packing
                                 builder))))
                     builders)
        (error "Sheet builders do not share one boundary observation table."))
      (return-from %builder-open-boundary-table observations)))
  (let* ((triangles
           (loop for builder in builders
                 for templates = (surface-mesh-builder-templates builder)
                 sum (%stream-triangle-count
                      (surface-mesh-builder-face-stream builder) templates)
                 sum (%stream-triangle-count
                      (surface-mesh-builder-band-stream builder) templates)))
         ;; Each triangle contributes three directed edges, and an interior
         ;; edge is observed twice, so the table holds about 3/2 keys per
         ;; triangle.  Sizing it now spares a long chain of rehashes.
         (observations (make-hash-table :test #'eql
                                        :size (max 4096
                                                   (ceiling (* 3 triangles)
                                                            2)))))
    (dolist (builder builders observations)
      (let ((templates (surface-mesh-builder-templates builder)))
        (%scan-stream-boundary-edges
         (surface-mesh-builder-face-stream builder) templates packing
         observations)
        (%scan-stream-boundary-edges
         (surface-mesh-builder-band-stream builder) templates packing
         observations)))))

(defun %enable-boundary-observations (builders packing estimated-cells)
  "Make BUILDERS update one boundary parity table during sheet emission."
  (let ((observations
          (make-hash-table :test #'eql
                           :size (max 4096 (* 4 estimated-cells)))))
    (dolist (builder builders observations)
      (setf (surface-mesh-builder-boundary-packing builder) packing
            (surface-mesh-builder-boundary-observations builder)
            observations))))

(defun %observe-scratch-boundary-edges
    (builder base-x base-y base-z stock count)
  "Parity-count oriented SCRATCH triangles before template interning."
  (declare (optimize (speed 3) (safety 1))
           (type fixnum base-x base-y base-z count))
  (let ((packing (surface-mesh-builder-boundary-packing builder))
        (observations
          (surface-mesh-builder-boundary-observations builder))
        (vertices (surface-mesh-builder-vertex-scratch builder))
        (ox (* +mesh-cell-size+ base-x))
        (oy (* +mesh-cell-size+ base-y))
        (oz (* +mesh-cell-size+ base-z)))
    (flet ((global-x (vertex)
             (+ ox (- (ldb (byte +mesh-template-coordinate-bit-count+ 0)
                           (aref vertices vertex))
                      +mesh-template-coordinate-bias+)))
           (global-y (vertex)
             (+ oy (- (ldb (byte +mesh-template-coordinate-bit-count+
                                 +mesh-template-coordinate-bit-count+)
                               (aref vertices vertex))
                      +mesh-template-coordinate-bias+)))
           (global-z (vertex)
             (+ oz (- (ldb (byte +mesh-template-coordinate-bit-count+
                                 (* 2 +mesh-template-coordinate-bit-count+))
                               (aref vertices vertex))
                      +mesh-template-coordinate-bias+))))
      (loop for triangle from 0 below count by 3 do
        (dotimes (index 3)
          (let* ((left (+ triangle index))
                 (right (+ triangle (mod (1+ index) 3)))
                 (lx (global-x left)) (ly (global-y left))
                 (lz (global-z left))
                 (rx (global-x right)) (ry (global-y right))
                 (rz (global-z right))
                 (key nil) (left12 nil) (right12 nil))
            (multiple-value-setq (key left12 right12)
              (%pack-spatial-edge packing lx ly lz rx ry rz))
            (let ((existing (gethash key observations)))
              (if existing
                  (let ((next-count
                          (1+ (ash existing
                                   (- +boundary-observation-count-shift+)))))
                    (when (> next-count 2)
                      (error "Face and edge streams meet ~D times at ~S."
                             next-count key))
                    (setf (gethash key observations)
                          (dpb next-count
                               (byte 4 +boundary-observation-count-shift+)
                               existing)))
                  (setf (gethash key observations)
                        (logior
                         (ash 1 +boundary-observation-count-shift+)
                         (ash left12 +fan-record-left-shift+)
                         (ash right12 +fan-record-right-shift+)
                         stock))))))))))

(defun %scan-stream-boundary-edges (stream templates packing observations)
  (declare (optimize (speed 3) (safety 1)))
  (let ((words (instance-stream-words stream)))
    (loop for offset from 0 below (fill-pointer words)
              by +mesh-instance-word-count+
              do (let* ((base-x (aref words offset))
                        (base-y (aref words (+ offset 1)))
                        (base-z (aref words (+ offset 2)))
                        (meta (aref words (+ offset 3)))
                        (template (aref templates (ldb (byte 16 0) meta)))
                        (stock (ldb (byte +mesh-instance-stock-bit-count+
                                          +mesh-instance-stock-shift+)
                                    meta))
                        (vertices (mesh-template-vertices template))
                        (ox (* +mesh-cell-size+ base-x))
                        (oy (* +mesh-cell-size+ base-y))
                        (oz (* +mesh-cell-size+ base-z)))
                   (flet ((global-x (vertex)
                            (+ ox (- (ldb (byte +mesh-template-coordinate-bit-count+
                                               0)
                                          (aref vertices vertex))
                                     +mesh-template-coordinate-bias+)))
                          (global-y (vertex)
                            (+ oy (- (ldb (byte +mesh-template-coordinate-bit-count+
                                               +mesh-template-coordinate-bit-count+)
                                          (aref vertices vertex))
                                     +mesh-template-coordinate-bias+)))
                          (global-z (vertex)
                            (+ oz (- (ldb (byte +mesh-template-coordinate-bit-count+
                                               (* 2 +mesh-template-coordinate-bit-count+))
                                          (aref vertices vertex))
                                     +mesh-template-coordinate-bias+))))
                     (loop for triangle from 0 below (length vertices) by 3
                           do (dotimes (index 3)
                                (let* ((left (+ triangle index))
                                       (right (+ triangle
                                                 (mod (1+ index) 3)))
                                       (lx (global-x left))
                                       (ly (global-y left))
                                       (lz (global-z left))
                                       (rx (global-x right))
                                       (ry (global-y right))
                                       (rz (global-z right)))
                                  (multiple-value-bind (key left12 right12)
                                      (%pack-spatial-edge
                                       packing lx ly lz rx ry rz)
                                    (let ((existing
                                            (gethash key observations)))
                                      (cond
                                        ((null existing)
                                         (setf (gethash key observations)
                                               (logior
                                                (ash 1
                                                     +boundary-observation-count-shift+)
                                                (ash left12
                                                     +fan-record-left-shift+)
                                                (ash right12
                                                     +fan-record-right-shift+)
                                                stock)))
                                        (t
                                         (let ((count
                                                 (1+ (ash existing
                                                          (- +boundary-observation-count-shift+)))))
                                           (when (> count 2)
                                             (error "Face and edge streams meet ~D times at ~S."
                                                    count key))
                                           (setf (gethash key observations)
                                                 (dpb count
                                                      (byte
                                                       4
                                                       +boundary-observation-count-shift+)
                                                      existing)))))))))))))))

(defun %attribute-open-edges-to-sites
    (builders packing bevel-width drop-nonlocal-p)
  "Group the open boundary's directed edges by owning lattice vertex.

Returns a table from packed site keys to packed vectors of fan records whose 12-bit
points are site-local with the fan bias.  An open edge not contained in any
vertex's bevel domain is an invariant violation for a whole solid; for a
chunk's witness scan it is the witness truncation boundary, provably outside
every owned site's bevel domain, and DROP-NONLOCAL-P discards it."
  (declare (optimize (speed 3) (safety 1)))
  (let ((observations (%builder-open-boundary-table builders packing))
        (by-site (make-hash-table :test #'eql :size 1024)))
    (maphash
     (lambda (key value)
       (when (= 1 (ash value (- +boundary-observation-count-shift+)))
         (block attribute
           (let* ((anchor-x (%spatial-edge-key-anchor-x packing key))
                  (anchor-y (%spatial-edge-key-anchor-y packing key))
                  (anchor-z (%spatial-edge-key-anchor-z key))
                  (left12 (ldb (byte 12 +fan-record-left-shift+) value))
                  (right12 (ldb (byte 12 +fan-record-right-shift+) value))
                  (stock (ldb (byte +fan-record-stock-bit-count+ 0) value))
                  (lx (+ (* 8 anchor-x) (ldb (byte 4 8) left12)))
                  (ly (+ (* 8 anchor-y) (ldb (byte 4 4) left12)))
                  (lz (+ (* 8 anchor-z) (ldb (byte 4 0) left12)))
                  (rx (+ (* 8 anchor-x) (ldb (byte 4 8) right12)))
                  (ry (+ (* 8 anchor-y) (ldb (byte 4 4) right12)))
                  (rz (+ (* 8 anchor-z) (ldb (byte 4 0) right12))))
             (flet ((site-coordinate (l r)
                      ;; Find the lattice vertex whose bevel domain contains
                      ;; the edge, exactly as the exact-rational original did.
                      (let ((coordinate
                              (round (+ l r) (* 2 +mesh-cell-size+))))
                        (unless (and (<= (abs (- l (* +mesh-cell-size+
                                                      coordinate)))
                                         bevel-width)
                                     (<= (abs (- r (* +mesh-cell-size+
                                                      coordinate)))
                                         bevel-width))
                          (when drop-nonlocal-p
                            (return-from attribute))
                          (error "Open edge ~S--~S is not local to a lattice vertex."
                                 (list lx ly lz) (list rx ry rz)))
                        coordinate)))
               (let* ((site-x (site-coordinate lx rx))
                      (site-y (site-coordinate ly ry))
                      (site-z (site-coordinate lz rz))
                      (record
                        (logior
                         (ash (%fan-point (- lx (* 8 site-x))
                                          (- ly (* 8 site-y))
                                          (- lz (* 8 site-z)))
                              +fan-record-left-shift+)
                         (ash (%fan-point (- rx (* 8 site-x))
                                          (- ry (* 8 site-y))
                                          (- rz (* 8 site-z)))
                              +fan-record-right-shift+)
                         stock)))
                 (let* ((key (%lattice-key site-x site-y site-z))
                        (records
                          (or (gethash key by-site)
                              (setf (gethash key by-site)
                                    (make-array
                                     8 :element-type '(unsigned-byte 64)
                                       :adjustable t :fill-pointer 0)))))
                   (vector-push-extend record records))))))))
     observations)
    by-site))

(defun %fan-record-cycles (site-x site-y site-z records)
  "Order consistently directed fan RECORDS into loops at the site."
  (let ((used (make-array (length records) :element-type 'bit
                                           :initial-element 0))
        (cycles nil))
    (dotimes (start (length records))
      (when (zerop (sbit used start))
        (let* ((first-record (aref records start))
               (first-point (%fan-record-left first-record))
               (next-point (%fan-record-right first-record))
               (cycle (list first-record)))
          (setf (sbit used start) 1)
          (loop until (= next-point first-point) do
            (let ((next-index nil))
              (dotimes (index (length records))
                (when (and (zerop (sbit used index))
                           (= next-point
                              (%fan-record-left (aref records index))))
                  (setf next-index index)
                  (return)))
              (unless next-index
                (error "Open boundary at lattice site ~S stops at ~S."
                       (list site-x site-y site-z)
                       (list (%fan-point-x next-point)
                             (%fan-point-y next-point)
                             (%fan-point-z next-point))))
              (let ((next-record (aref records next-index)))
                (setf (sbit used next-index) 1
                      cycle (cons next-record cycle)
                      next-point (%fan-record-right next-record)))))
          (push (nreverse cycle) cycles))))
    (nreverse cycles)))

(defun %rescale-fan-record (record site-x site-y site-z
                            source-width target-width)
  "Evaluate RECORD's site-local affine bevel coordinates at TARGET-WIDTH."
  (flet ((rescale (coordinate global)
           (let ((numerator (* coordinate target-width)))
             (unless (zerop (rem numerator source-width))
               (error "Boundary coordinate ~S at site ~S has no integer ~D/~D limit."
                      global (list site-x site-y site-z)
                      target-width source-width))
             (truncate numerator source-width))))
    (let ((left (%fan-record-left record))
          (right (%fan-record-right record)))
      (flet ((rescale-point (point)
               (let ((x (%fan-point-x point))
                     (y (%fan-point-y point))
                     (z (%fan-point-z point)))
                 (let ((global (list (+ (* 8 site-x) x)
                                     (+ (* 8 site-y) y)
                                     (+ (* 8 site-z) z))))
                   (%fan-point (rescale x global)
                               (rescale y global)
                               (rescale z global))))))
        (logior (ash (rescale-point left) +fan-record-left-shift+)
                (ash (rescale-point right) +fan-record-right-shift+)
                (%fan-record-stock record))))))

(defun %cycle-planar-through-site-p (cycle)
  "Whether every left endpoint of CYCLE is coplanar with the site origin."
  (let* ((count (length cycle))
         (xs (make-array count)) (ys (make-array count))
         (zs (make-array count)))
    (declare (dynamic-extent xs ys zs))
    (loop for record in cycle
          for index from 0
          for point = (%fan-record-left record)
          do (setf (svref xs index) (%fan-point-x point)
                   (svref ys index) (%fan-point-y point)
                   (svref zs index) (%fan-point-z point)))
    (let ((nx 0) (ny 0) (nz 0))
      (loop named search
            for i from 0 below count
            do (loop for j from (1+ i) below count
                     do (let ((cx (- (* (svref ys i) (svref zs j))
                                     (* (svref zs i) (svref ys j))))
                              (cy (- (* (svref zs i) (svref xs j))
                                     (* (svref xs i) (svref zs j))))
                              (cz (- (* (svref xs i) (svref ys j))
                                     (* (svref ys i) (svref xs j)))))
                          (unless (and (zerop cx) (zerop cy) (zerop cz))
                            (setf nx cx ny cy nz cz)
                            (return-from search)))))
      (and (not (and (zerop nx) (zerop ny) (zerop nz)))
           (loop for index below count
                 always (zerop (+ (* nx (svref xs index))
                                  (* ny (svref ys index))
                                  (* nz (svref zs index)))))))))

(defun %vertex-fan-uses-center-p (cycle star-mask)
  (or (%cycle-planar-through-site-p cycle)
      ;; The ordinary five-cell concave corner and its upside-down three-cell
      ;; complement both pass through the site.  The six- and seven-cell
      ;; chamfer/fillet runs do not; coning those creates the ornaments.
      (member (logcount star-mask) '(3 5))))

(defun %emit-triangular-boundary-cap
    (builder site-x site-y site-z cycle stock star-mask)
  (let* ((a (%fan-record-left (first cycle)))
         (b (%fan-record-left (second cycle)))
         (c (%fan-record-left (third cycle)))
         (ax (%fan-point-x a)) (ay (%fan-point-y a)) (az (%fan-point-z a))
         (bx (%fan-point-x b)) (by (%fan-point-y b)) (bz (%fan-point-z b))
         (cx (%fan-point-x c)) (cy (%fan-point-y c)) (cz (%fan-point-z c))
         (ux (- cx ax)) (uy (- cy ay)) (uz (- cz az))
         (vx (- bx ax)) (vy (- by ay)) (vz (- bz az))
         (nx (- (* uy vz) (* uz vy)))
         (ny (- (* uz vx) (* ux vz)))
         (nz (- (* ux vy) (* uy vx))))
    ;; The observed loop follows the existing surface winding.  Reverse its
    ;; order so the cap pairs every boundary edge with opposite winding.
    (%emit-fan-triangle builder site-x site-y site-z #b111
                        ax ay az cx cy cz bx by bz nx ny nz
                        stock star-mask)))

(defun %emit-boundary-strip
    (builder site-x site-y site-z cycle stock star-mask)
  "Triangulate CYCLE without introducing its lattice-site origin as geometry."
  (let ((points (mapcar #'%fan-record-left cycle))
        (boundary-stocks (make-hash-table :test #'eql)))
    (dolist (record cycle)
      (setf (gethash (%fan-record-undirected record) boundary-stocks)
            (%fan-record-stock record)))
    (labels ((undirected (a b)
               (if (< a b) (logior (ash a 12) b) (logior (ash b 12) a)))
             (squared-distance (a b)
               (let ((dx (- (%fan-point-x a) (%fan-point-x b)))
                     (dy (- (%fan-point-y a) (%fan-point-y b)))
                     (dz (- (%fan-point-z a) (%fan-point-z b))))
                 (+ (* dx dx) (* dy dy) (* dz dz))))
             (strip-triangle (a b c)
               (let* ((ax (%fan-point-x a)) (ay (%fan-point-y a))
                      (az (%fan-point-z a))
                      (bx (%fan-point-x b)) (by (%fan-point-y b))
                      (bz (%fan-point-z b))
                      (cx (%fan-point-x c)) (cy (%fan-point-y c))
                      (cz (%fan-point-z c))
                      (ux (- bx ax)) (uy (- by ay)) (uz (- bz az))
                      (vx (- cx ax)) (vy (- cy ay)) (vz (- cz az))
                      (nx (- (* uy vz) (* uz vy)))
                      (ny (- (* uz vx) (* ux vz)))
                      (nz (- (* ux vy) (* uy vx)))
                      (mask
                        (logior
                         (if (gethash (undirected b c) boundary-stocks)
                             #b001 0)
                         (if (gethash (undirected c a) boundary-stocks)
                             #b010 0)
                         (if (gethash (undirected a b) boundary-stocks)
                             #b100 0))))
                 (%emit-fan-triangle builder site-x site-y site-z mask
                                     ax ay az bx by bz cx cy cz nx ny nz
                                     stock star-mask))))
      ;; Repeatedly remove the end whose replacement diagonal is shorter.  The
      ;; remaining vertices stay a contiguous interval of the boundary, giving
      ;; a deterministic local triangle strip rather than a long fan of spokes.
      (loop while (> (length points) 3) do
        (let* ((first (first points))
               (second (second points))
               (last (car (last points)))
               (penultimate (car (last points 2))))
          (if (<= (squared-distance second last)
                  (squared-distance first penultimate))
              (progn
                (strip-triangle first last second)
                (setf points (rest points)))
              (progn
                (strip-triangle first last penultimate)
                (setf points (butlast points))))))
      (destructuring-bind (a b c) points
        (strip-triangle a c b)))))

(defun %emit-centered-boundary-fan
    (builder site-x site-y site-z cycle stock star-mask)
  (dolist (record cycle)
    (let ((left (%fan-record-left record))
          (right (%fan-record-right record)))
      ;; A boundary edge incident on the center is already a radial edge of
      ;; this fan.  Its neighboring non-radial segment emits the triangle.
      (unless (or (= left +fan-origin-point+) (= right +fan-origin-point+))
        (let* ((lx (%fan-point-x left)) (ly (%fan-point-y left))
               (lz (%fan-point-z left))
               (rx (%fan-point-x right)) (ry (%fan-point-y right))
               (rz (%fan-point-z right))
               (nx (- (* ry lz) (* rz ly)))
               (ny (- (* rz lx) (* rx lz)))
               (nz (- (* rx ly) (* ry lx))))
          (when (and (zerop nx) (zerop ny) (zerop nz))
            (error "Open edge ~S--~S is radial to lattice site ~S."
                   (list lx ly lz) (list rx ry rz)
                   (list site-x site-y site-z)))
          ;; Bit zero denotes the outer RIGHT--LEFT edge.  The other two
          ;; edges are triangulation diagonals inside the complete fan.
          ;; Keep each sector as its own instance because the construction
          ;; mask varies around the junction, even though STOCK is uniform
          ;; for this whole chamfer.
          (%emit-fan-triangle builder site-x site-y site-z #b001
                              0 0 0 rx ry rz lx ly lz nx ny nz
                              stock star-mask))))))

(defun %emit-boundary-derived-fans
    (builder field domain chamfer-stock-function sheet-builders packing
     ox0 ox1 oy0 oy1 drop-nonlocal-p)
  "Close the SHEET-BUILDERS' open loops with site-local templates in BUILDER.

Only lattice sites inside the half-open [OX0, OX1) x [OY0, OY1) box get
fans; a chunk's neighbor owns the rest and closes them from its own
witness scan."
  (let* ((source-width (surface-mesh-builder-bevel-width
                        (first sheet-builders)))
         (target-width (surface-mesh-builder-bevel-width builder))
         (by-site (%attribute-open-edges-to-sites sheet-builders packing
                                                  source-width
                                                  drop-nonlocal-p))
         (site-keys (make-array (hash-table-count by-site)
                                :element-type '(unsigned-byte 64)))
         (write 0))
    (loop for key being the hash-keys of by-site
          do (setf (aref site-keys write) key)
             (incf write))
    (sort site-keys #'<)
    (loop for key across site-keys
          when (let ((x (%lattice-key-x key))
                     (y (%lattice-key-y key)))
                 (and (<= ox0 x) (< x ox1) (<= oy0 y) (< y oy1)))
            do
      (let* ((site-x (%lattice-key-x key))
             (site-y (%lattice-key-y key))
             (site-z (%lattice-key-z key))
             (star-mask (%star-mask-at field domain site-x site-y site-z))
             (records (sort (gethash key by-site) #'>
                            :key #'%fan-record-undirected)))
        (dolist (cycle (%fan-record-cycles site-x site-y site-z records))
          (let* ((cycle
                   (if (= source-width target-width)
                       cycle
                       (mapcar (lambda (record)
                                 (%rescale-fan-record
                                  record site-x site-y site-z
                                  source-width target-width))
                               cycle)))
                 (stock (funcall chamfer-stock-function
                                 (mapcar #'%fan-record-stock cycle))))
            (cond ((= 3 (length cycle))
                   (%emit-triangular-boundary-cap
                    builder site-x site-y site-z cycle stock star-mask))
                  ((%vertex-fan-uses-center-p cycle star-mask)
                   (%emit-centered-boundary-fan
                    builder site-x site-y site-z cycle stock star-mask))
                  (t
                   (%emit-boundary-strip
                    builder site-x site-y site-z cycle stock
                    star-mask)))))))))

;;; ---------------------------------------------------------------------------
;;; Finishing

(defun %template-words (builder)
  (let* ((templates (surface-mesh-builder-templates builder))
         (template-count (fill-pointer templates))
         (vertex-count
           (loop for index below template-count
                 sum (length (mesh-template-vertices
                              (aref templates index)))))
         (words (make-array (* +mesh-template-vertex-word-count+ vertex-count)
                            :element-type '(unsigned-byte 32)))
         (ranges (make-array (* 2 template-count)
                             :element-type '(unsigned-byte 32)))
         (vertex-start 0)
         (write 0))
    (dotimes (index template-count)
      (let* ((template (aref templates index))
             (vertices (mesh-template-vertices template)))
        (setf (aref ranges (* 2 (mesh-template-id template))) vertex-start
              (aref ranges (1+ (* 2 (mesh-template-id template))))
              (length vertices))
        (loop for vertex across vertices
              do (setf (aref words write)
                       (ldb (byte +mesh-template-coordinate-bit-count+ 0)
                            vertex)
                       (aref words (+ write 1))
                       (ldb (byte +mesh-template-coordinate-bit-count+
                                  +mesh-template-coordinate-bit-count+)
                            vertex)
                       (aref words (+ write 2))
                       (ldb (byte +mesh-template-coordinate-bit-count+
                                  (* 2 +mesh-template-coordinate-bit-count+))
                            vertex)
                       (aref words (+ write 3))
                       (ash vertex (- +mesh-vertex-attribute-shift+)))
                 (incf write 4))
        (incf vertex-start (length vertices))))
    (values words ranges)))

(defun %finish-instance-stream (stream ranges)
  "Counting-scatter one columnar stream by template and derive its draws."
  (let* ((source (instance-stream-words stream))
         (count (instance-stream-count stream))
         (template-count (truncate (length ranges) 2))
         (counts (make-array template-count
                             :element-type '(unsigned-byte 32)
                             :initial-element 0))
         (starts (make-array template-count
                             :element-type '(unsigned-byte 32)))
         (writes (make-array template-count
                             :element-type '(unsigned-byte 32)))
         (words (make-array (* +mesh-instance-word-count+ count)
                            :element-type '(unsigned-byte 32)))
         (draws nil)
         (triangle-count 0))
    (flet ((template-id (index)
             (ldb (byte 16 0)
                  (aref source (+ (* +mesh-instance-word-count+ index) 3)))))
      (dotimes (source-index count)
        (incf (aref counts (template-id source-index))))
      (let ((start 0))
        (dotimes (template-id template-count)
          (setf (aref starts template-id) start
                (aref writes template-id) start)
          (incf start (aref counts template-id))))
      ;; Descending source traversal preserves the historical reverse
      ;; emission order within each template bucket without a permutation.
      (loop for source-index downfrom (1- count) to 0 do
        (let* ((template-id (template-id source-index))
               (instance-index (aref writes template-id)))
          (replace words source
                   :start1 (* +mesh-instance-word-count+ instance-index)
                   :start2 (* +mesh-instance-word-count+ source-index)
                   :end2 (* +mesh-instance-word-count+ (1+ source-index)))
          (incf (aref writes template-id))))
      (dotimes (template-id template-count)
        (let ((instances (aref counts template-id)))
          (when (plusp instances)
            (let ((vertex-start (aref ranges (* 2 template-id)))
                  (vertex-count (aref ranges (1+ (* 2 template-id)))))
              (push (list template-id vertex-start vertex-count
                          (aref starts template-id) instances)
                    draws)
              (incf triangle-count
                    (* instances (truncate vertex-count 3))))))))
    (values words (nreverse draws) triangle-count)))

(defun %finish-surface-mesh (builder)
  (multiple-value-bind (template-words template-ranges)
      (%template-words builder)
    (multiple-value-bind (face-words face-draws face-triangles)
        (%finish-instance-stream
         (surface-mesh-builder-face-stream builder) template-ranges)
      (multiple-value-bind (band-words band-draws band-triangles)
          (%finish-instance-stream
           (surface-mesh-builder-band-stream builder) template-ranges)
        (multiple-value-bind (fan-words fan-draws fan-triangles)
            (%finish-instance-stream
             (surface-mesh-builder-fan-stream builder) template-ranges)
          (%make-surface-mesh
           (surface-mesh-builder-domain builder)
           (surface-mesh-builder-bevel-width builder)
           template-words template-ranges
           face-words face-draws band-words band-draws fan-words fan-draws
           face-triangles band-triangles fan-triangles
           (surface-mesh-builder-singular-star-count builder)))))))

(defun select-surface-mesh-stocks (mesh predicate)
  "Return MESH's instances whose packed stock satisfies PREDICATE.

The exact template vocabulary and geometry width remain borrowed from MESH;
only the three dense instance streams are partitioned.  This is intended for
coarse materialization policy such as independently resident material cohorts,
not as a substitute for a variable-width junction construction."
  (check-type mesh surface-mesh)
  (check-type predicate function)
  (labels ((selected-stream (words)
             (let ((stream (%make-instance-stream)))
               (loop for offset from 0 below (length words)
                       by +mesh-instance-word-count+
                     for meta = (aref words (+ offset 3))
                     for stock =
                       (ldb (byte +mesh-instance-stock-bit-count+
                                  +mesh-instance-stock-shift+)
                            meta)
                     when (funcall predicate stock)
                       do (loop for word-offset below
                                +mesh-instance-word-count+
                                do (vector-push-extend
                                    (aref words (+ offset word-offset))
                                    (instance-stream-words stream))))
               (%finish-instance-stream
                stream (surface-mesh-template-ranges mesh)))))
    (multiple-value-bind (face-words face-draws face-triangles)
        (selected-stream (surface-mesh-face-instance-words mesh))
      (multiple-value-bind (band-words band-draws band-triangles)
          (selected-stream (surface-mesh-band-instance-words mesh))
        (multiple-value-bind (fan-words fan-draws fan-triangles)
            (selected-stream (surface-mesh-fan-instance-words mesh))
          (%make-surface-mesh
           (surface-mesh-domain mesh)
           (surface-mesh-bevel-width mesh)
           (surface-mesh-template-vertex-words mesh)
           (surface-mesh-template-ranges mesh)
           face-words face-draws band-words band-draws fan-words fan-draws
           face-triangles band-triangles fan-triangles
           (surface-mesh-singular-star-count mesh)))))))

;;; ---------------------------------------------------------------------------
;;; Exact coplanar compression

(defun %point-order< (left right)
  (loop for l in left
        for r in right
        when (/= l r) return (< l r)
        finally (return nil)))

(defun %ordered-point-edge (left right)
  (if (%point-order< left right)
      (list left right)
      (list right left)))

(defun %point-cross (a b c)
  (let ((ux (- (first b) (first a)))
        (uy (- (second b) (second a)))
        (uz (- (third b) (third a)))
        (vx (- (first c) (first a)))
        (vy (- (second c) (second a)))
        (vz (- (third c) (third a))))
    (list (- (* uy vz) (* uz vy))
          (- (* uz vx) (* ux vz))
          (- (* ux vy) (* uy vx)))))

(defun %point-dot (left right)
  (+ (* (first left) (first right))
     (* (second left) (second right))
     (* (third left) (third right))))

(defun %primitive-plane-normal (a b c)
  (let* ((cross (%point-cross a b c))
         (divisor (reduce #'gcd cross :key #'abs)))
    (unless (plusp divisor)
      (error "Degenerate triangle in coplanar compression: ~S ~S ~S."
             a b c))
    (mapcar (lambda (coordinate) (/ coordinate divisor)) cross)))

(defun %map-surface-mesh-triangle-records (function mesh)
  "Call FUNCTION with kind, stock, ambient, mask, normal, and three points."
  (let ((templates (surface-mesh-template-vertex-words mesh))
        (ranges (surface-mesh-template-ranges mesh)))
    (labels ((point (base vertex)
               (loop for axis below 3
                     collect (+ (* +mesh-cell-size+ (nth axis base))
                                (- (aref templates
                                         (+ (* vertex
                                               +mesh-template-vertex-word-count+)
                                            axis))
                                   +mesh-template-coordinate-bias+))))
             (visit (words kind)
               (loop for offset from 0 below (length words) by 4
                     for base = (list (aref words offset)
                                      (aref words (+ offset 1))
                                      (aref words (+ offset 2)))
                     for meta = (aref words (+ offset 3))
                     for template-id = (ldb (byte 16 0) meta)
                     for stock = (ldb (byte +mesh-instance-stock-bit-count+
                                            +mesh-instance-stock-shift+)
                                      meta)
                     for ambient = (ldb (byte 2
                                              +mesh-instance-ambient-occlusion-shift+)
                                        meta)
                     for start = (aref ranges (* 2 template-id))
                     for count = (aref ranges (1+ (* 2 template-id)))
                     do (loop for vertex from start below (+ start count) by 3
                              for attributes =
                                (aref templates
                                      (+ (* vertex
                                            +mesh-template-vertex-word-count+)
                                         3))
                              for a = (point base vertex)
                              for b = (point base (1+ vertex))
                              for c = (point base (+ vertex 2))
                              for normal = (%primitive-plane-normal a b c)
                              do (funcall function kind stock ambient
                                          (ldb (byte 3 10) attributes)
                                          normal a b c)))))
      (visit (surface-mesh-face-instance-words mesh) :face)
      (visit (surface-mesh-band-instance-words mesh) :band)
      (visit (surface-mesh-fan-instance-words mesh) :junction))))

(defmacro %do-surface-mesh-triangle-scalars
    ((mesh kind stock ambient mask
      ax ay az bx by bz cx cy cz)
     &body body)
  "Iterate MESH's packed triangles with scalar coordinates and no callback.

This is the dense-loop counterpart to %MAP-SURFACE-MESH-TRIANGLE-RECORDS.
The latter deliberately materializes convenient point and normal lists for
cold transformations and inspection; performance-sensitive compilers should
keep the packed instance/template representation through their inner loop."
  (let ((mesh-value (gensym "MESH"))
        (templates (gensym "TEMPLATES"))
        (ranges (gensym "RANGES"))
        (visit (gensym "VISIT"))
        (words (gensym "WORDS"))
        (kind-value (gensym "KIND"))
        (offset (gensym "OFFSET"))
        (base-x (gensym "BASE-X"))
        (base-y (gensym "BASE-Y"))
        (base-z (gensym "BASE-Z"))
        (meta (gensym "META"))
        (stock-value (gensym "STOCK"))
        (ambient-value (gensym "AMBIENT"))
        (template-id (gensym "TEMPLATE-ID"))
        (start (gensym "START"))
        (count (gensym "COUNT"))
        (vertex (gensym "VERTEX"))
        (attributes (gensym "ATTRIBUTES")))
    (labels ((coordinate (base vertex-offset axis)
               `(the mesh-global-tick
                  (+ (ash (the fixnum ,base) 3)
                     (- (aref ,templates
                              (+ (* (+ ,vertex ,vertex-offset)
                                    +mesh-template-vertex-word-count+)
                                 ,axis))
                        +mesh-template-coordinate-bias+)))))
      `(let* ((,mesh-value ,mesh)
              (,templates (surface-mesh-template-vertex-words ,mesh-value))
              (,ranges (surface-mesh-template-ranges ,mesh-value)))
         (flet ((,visit (,words ,kind-value)
                  (declare (type (simple-array (unsigned-byte 32) (*)) ,words))
                  (loop for ,offset fixnum from 0 below (length ,words)
                          by +mesh-instance-word-count+
                        for ,base-x = (aref ,words ,offset)
                        for ,base-y = (aref ,words (+ ,offset 1))
                        for ,base-z = (aref ,words (+ ,offset 2))
                        for ,meta = (aref ,words (+ ,offset 3))
                        for ,template-id = (ldb (byte 16 0) ,meta)
                        for ,stock-value =
                          (ldb (byte +mesh-instance-stock-bit-count+
                                     +mesh-instance-stock-shift+)
                               ,meta)
                        for ,ambient-value =
                          (ldb (byte 2
                                     +mesh-instance-ambient-occlusion-shift+)
                               ,meta)
                        for ,start = (aref ,ranges (* 2 ,template-id))
                        for ,count = (aref ,ranges (1+ (* 2 ,template-id)))
                        do (loop for ,vertex fixnum from ,start
                                   below (+ ,start ,count) by 3
                                 for ,attributes =
                                   (aref ,templates
                                         (+ (* ,vertex
                                               +mesh-template-vertex-word-count+)
                                            3))
                                 do (let ((,kind ,kind-value)
                                          (,stock ,stock-value)
                                          (,ambient ,ambient-value)
                                          (,mask (ldb (byte 3 10) ,attributes))
                                          (,ax ,(coordinate base-x 0 0))
                                          (,ay ,(coordinate base-y 0 1))
                                          (,az ,(coordinate base-z 0 2))
                                          (,bx ,(coordinate base-x 1 0))
                                          (,by ,(coordinate base-y 1 1))
                                          (,bz ,(coordinate base-z 1 2))
                                          (,cx ,(coordinate base-x 2 0))
                                          (,cy ,(coordinate base-y 2 1))
                                          (,cz ,(coordinate base-z 2 2)))
                                      (declare
                                       (ignorable ,kind ,stock ,ambient ,mask
                                                  ,ax ,ay ,az ,bx ,by ,bz
                                                  ,cx ,cy ,cz)
                                       (type fixnum ,stock ,ambient ,mask)
                                       (type mesh-global-tick
                                             ,ax ,ay ,az ,bx ,by ,bz
                                             ,cx ,cy ,cz))
                                      ,@body)))))
           (,visit (surface-mesh-face-instance-words ,mesh-value) :face)
           (,visit (surface-mesh-band-instance-words ,mesh-value) :band)
           (,visit (surface-mesh-fan-instance-words ,mesh-value) :junction))))))

(declaim (inline %unit-bevel-coordinate-site-and-direction)
         (ftype (function (mesh-global-tick)
                  (values (integer 0 #.(ash 1 17))
                          (integer -1 1) &optional))
                %unit-bevel-coordinate-site-and-direction))
(defun %unit-bevel-coordinate-site-and-direction (coordinate)
  "Decode one nonnegative width-one tick coordinate without materialization."
  (declare (optimize (speed 3) (safety 1))
           (type mesh-global-tick coordinate))
  (let ((cell (ash coordinate -3)))
    (case (logand coordinate 7)
      (0 (values cell 0))
      (1 (values cell 1))
      (7 (values (1+ cell) -1))
      (t (error "Width-one point coordinate ~D has no canonical lattice-site owner."
                coordinate)))))

(declaim (inline %unit-bevel-point-owner)
         (ftype (function
                  (mesh-global-tick mesh-global-tick mesh-global-tick)
                  (values (integer 0 #.(ash 1 17))
                          (integer 0 #.(ash 1 17))
                          (integer 0 #.(ash 1 17))
                          (integer -1 1) (integer -1 1) (integer -1 1)
                          &optional))
                %unit-bevel-point-owner))
(defun %unit-bevel-point-owner (x y z)
  "Return the scalar owner site and local direction for a witness point."
  (multiple-value-bind (site-x direction-x)
      (%unit-bevel-coordinate-site-and-direction x)
    (multiple-value-bind (site-y direction-y)
        (%unit-bevel-coordinate-site-and-direction y)
      (multiple-value-bind (site-z direction-z)
          (%unit-bevel-coordinate-site-and-direction z)
        (values site-x site-y site-z
                direction-x direction-y direction-z)))))

(defconstant +global-mesh-point-z-bit-count+ 12)
(defconstant +global-mesh-point-axis-bit-count+ 21)
(defconstant +global-mesh-point-y-shift+ +global-mesh-point-z-bit-count+)
(defconstant +global-mesh-point-x-shift+
  (+ +global-mesh-point-z-bit-count+ +global-mesh-point-axis-bit-count+))

(declaim (inline %pack-global-mesh-point
                 %global-mesh-point-x %global-mesh-point-y
                 %global-mesh-point-z %global-mesh-point-distance-squared))
(defun %pack-global-mesh-point (x y z)
  "Pack a world-domain tick point into one lexicographically ordered fixnum."
  (declare (optimize (speed 3) (safety 1))
           (type mesh-global-tick x y z))
  (unless (and (typep x '(unsigned-byte #.+global-mesh-point-axis-bit-count+))
               (typep y '(unsigned-byte #.+global-mesh-point-axis-bit-count+))
               (typep z '(unsigned-byte #.+global-mesh-point-z-bit-count+)))
    (error "Global mesh point ~S exceeds the LUFT world-domain tick range."
           (list x y z)))
  (logior (ash x +global-mesh-point-x-shift+)
          (ash y +global-mesh-point-y-shift+)
          z))

(defun %global-mesh-point-x (point)
  (ldb (byte +global-mesh-point-axis-bit-count+
             +global-mesh-point-x-shift+)
       point))
(defun %global-mesh-point-y (point)
  (ldb (byte +global-mesh-point-axis-bit-count+
             +global-mesh-point-y-shift+)
       point))
(defun %global-mesh-point-z (point)
  (ldb (byte +global-mesh-point-z-bit-count+ 0) point))

(defun %global-mesh-point-distance-squared (left right)
  (let ((dx (- (%global-mesh-point-x right) (%global-mesh-point-x left)))
        (dy (- (%global-mesh-point-y right) (%global-mesh-point-y left)))
        (dz (- (%global-mesh-point-z right) (%global-mesh-point-z left))))
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defun %global-mesh-point-list (point)
  (list (%global-mesh-point-x point)
        (%global-mesh-point-y point)
        (%global-mesh-point-z point)))

(declaim (inline %triangle-cross-scalars)
         (ftype (function
                  (mesh-global-tick mesh-global-tick mesh-global-tick
                   mesh-global-tick mesh-global-tick mesh-global-tick
                   mesh-global-tick mesh-global-tick mesh-global-tick)
                  (values (signed-byte 29) (signed-byte 29)
                          (signed-byte 29) &optional))
                %triangle-cross-scalars))
(defun %triangle-cross-scalars (ax ay az bx by bz cx cy cz)
  (declare (optimize (speed 3) (safety 1))
           (type mesh-global-tick ax ay az bx by bz cx cy cz))
  (let ((ux (the (signed-byte 14) (- bx ax)))
        (uy (the (signed-byte 14) (- by ay)))
        (uz (the (signed-byte 14) (- bz az)))
        (vx (the (signed-byte 14) (- cx ax)))
        (vy (the (signed-byte 14) (- cy ay)))
        (vz (the (signed-byte 14) (- cz az))))
    (values (the (signed-byte 29)
              (- (the fixnum (* uy vz)) (the fixnum (* uz vy))))
            (the (signed-byte 29)
              (- (the fixnum (* uz vx)) (the fixnum (* ux vz))))
            (the (signed-byte 29)
              (- (the fixnum (* ux vy)) (the fixnum (* uy vx)))))))

(defun %emit-global-triangle-scalars
    (builder kind stock ambient mask nx ny nz
     ax ay az bx by bz cx cy cz)
  "Emit one global-tick triangle without point, base, origin, or normal lists."
  (declare (optimize (speed 3) (safety 1))
           (type surface-mesh-builder builder)
           (type fixnum stock ambient mask)
           (type (signed-byte 29) nx ny nz)
           (type mesh-global-tick ax ay az bx by bz cx cy cz))
  (let* ((base-x (ash (min ax bx cx) -3))
         (base-y (ash (min ay by cy) -3))
         (base-z (ash (min az bz cz) -3))
         (origin-x (ash base-x 3))
         (origin-y (ash base-y 3))
         (origin-z (ash base-z 3))
         (scratch (surface-mesh-builder-vertex-scratch builder)))
    (%scratch-triangle
     scratch 0 (ecase kind (:face 0) (:band 1) (:junction 2)) mask
     (- ax origin-x) (- ay origin-y) (- az origin-z)
     (- bx origin-x) (- by origin-y) (- bz origin-z)
     (- cx origin-x) (- cy origin-y) (- cz origin-z)
     nx ny nz)
    (%emit-instance builder kind base-x base-y base-z stock ambient 3)))

(defun %unit-bevel-point-site (point)
  "Return the canonical lattice site and local direction owning POINT.

POINT must come from a width-one LUFT surface, so every coordinate is exactly
on, one tick above, or one tick below its owning lattice plane."
  (let ((site nil)
        (direction nil))
    (dolist (coordinate point)
      (multiple-value-bind (cell remainder)
          (floor coordinate +mesh-cell-size+)
        (case remainder
          (0 (push cell site) (push 0 direction))
          (1 (push cell site) (push 1 direction))
          (7 (push (1+ cell) site) (push -1 direction))
          (t (error "Width-one point coordinate ~D has no canonical lattice-site owner."
                    coordinate)))))
    (values (nreverse site) (nreverse direction))))

(declaim (ftype function %triangulate-coplanar-loop))

(defconstant +dense-bevel-site-field-byte-limit+ (* 16 1024 1024))
(defconstant +dense-bevel-site-field-sparsity-limit+ 16)
(defconstant +bevel-site-page-edge+ 8)
(defconstant +bevel-site-page-volume+
  (* +bevel-site-page-edge+ +bevel-site-page-edge+ +bevel-site-page-edge+))
;; Budget every possible page plus a conservative two-word directory entry.
;; The actual directory is one pointer per page, so this keeps the fast path
;; bounded without depending on implementation-specific object sizes.
(defconstant +bevel-site-page-directory-limit+
  (floor +dense-bevel-site-field-byte-limit+
         (+ +bevel-site-page-volume+ 16)))

(declaim (inline %dense-bevel-site-index))
(defun %dense-bevel-site-index
    (x y z x0 y0 z0 y-span z-span)
  (declare (optimize (speed 3) (safety 1))
           (type fixnum x y z x0 y0 z0 y-span z-span))
  (the fixnum
    (+ (the fixnum (- z z0))
       (the fixnum
         (* z-span
            (the fixnum
              (+ (the fixnum (- y y0))
                 (the fixnum (* y-span (the fixnum (- x x0)))))))))))

(defun %paged-byte-stock-mask-policy-p (domain stock-masks site-widths)
  "Whether STOCK-MASKS can use the bounded direct page directory for DOMAIN."
  (and (typep stock-masks '(simple-array (unsigned-byte 8) (*)))
       (typep site-widths '(simple-array (unsigned-byte 8) (*)))
       ;; Zero is the unobserved-site sentinel inside a page.  Wider or zero
       ;; masks retain the fully general EQL hash compiler below.
       (loop for stock-mask across stock-masks always (plusp stock-mask))
       (let* ((x-pages (ceiling (1+ (world-domain-x-limit domain))
                               +bevel-site-page-edge+))
              (y-pages (ceiling (1+ (world-domain-y-limit domain))
                               +bevel-site-page-edge+))
              (z-pages (ceiling (1+ +top-z+) +bevel-site-page-edge+)))
         (<= (* x-pages y-pages z-pages)
             +bevel-site-page-directory-limit+))))

(defun %compile-paged-byte-stock-mask-bevel-sites
    (witness stock-masks site-widths width-census)
  "Fold a positive byte stock lane through sparse 8-cubed pages.

Return the sparse or dense realized width field, its exact site count and
maximum width, and its inclusive coordinate bounds.  Pages are only the
one-pass accumulation language; the returned field has the same tight layout
used by the generic compiler and all realization passes."
  (declare (optimize (speed 3) (safety 1))
           (type surface-mesh witness)
           (type (simple-array (unsigned-byte 8) (*)) stock-masks)
           (type (simple-array (unsigned-byte 8) (*)) site-widths)
           (type (simple-array (unsigned-byte 32) (5)) width-census))
  (let* ((domain (surface-mesh-domain witness))
         (x-pages (ceiling (1+ (world-domain-x-limit domain))
                           +bevel-site-page-edge+))
         (y-pages (ceiling (1+ (world-domain-y-limit domain))
                           +bevel-site-page-edge+))
         (z-pages (ceiling (1+ +top-z+) +bevel-site-page-edge+))
         (directory-count (* x-pages y-pages z-pages))
         (pages (make-array directory-count :initial-element nil))
         (touched
           (make-array (min 1024 directory-count)
                       :element-type '(unsigned-byte 32)
                       :adjustable t :fill-pointer 0))
         (site-count 0)
         (minimum-site-x most-positive-fixnum)
         (maximum-site-x most-negative-fixnum)
         (minimum-site-y most-positive-fixnum)
         (maximum-site-y most-negative-fixnum)
         (minimum-site-z most-positive-fixnum)
         (maximum-site-z most-negative-fixnum))
    (declare (type fixnum x-pages y-pages z-pages directory-count site-count
                          minimum-site-x maximum-site-x
                          minimum-site-y maximum-site-y
                          minimum-site-z maximum-site-z))
    (labels ((directory-index (x y z)
               (the fixnum
                 (+ (ash z -3)
                    (the fixnum
                      (* z-pages
                         (the fixnum
                           (+ (ash y -3)
                              (the fixnum (* y-pages (ash x -3))))))))))
             (local-index (x y z)
               (the (unsigned-byte 9)
                 (logior (logand z 7)
                         (ash (logand y 7) 3)
                         (ash (logand x 7) 6))))
             (observe (x y z stock-mask)
               (declare (type (integer 0 #.(ash 1 17)) x y)
                        (type (integer 0 255) z)
                        (type (unsigned-byte 8) stock-mask))
               (let* ((page-index (directory-index x y z))
                      (page (aref pages page-index)))
                 (unless page
                   (setf page
                         (make-array +bevel-site-page-volume+
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)
                         (aref pages page-index) page)
                   (vector-push-extend page-index touched))
                 (let* ((page
                          (the (simple-array (unsigned-byte 8)
                                             (#.+bevel-site-page-volume+))
                            page))
                        (index (local-index x y z))
                        (old (aref page index)))
                   (when (zerop old)
                     (incf site-count)
                     (setf minimum-site-x (min minimum-site-x x)
                           maximum-site-x (max maximum-site-x x)
                           minimum-site-y (min minimum-site-y y)
                           maximum-site-y (max maximum-site-y y)
                           minimum-site-z (min minimum-site-z z)
                           maximum-site-z (max maximum-site-z z)))
                   (setf (aref page index) (logior old stock-mask))))))
      (declare
       (inline directory-index local-index observe)
       (ftype (function (fixnum fixnum fixnum) fixnum)
              directory-index local-index)
       (ftype (function (fixnum fixnum fixnum (unsigned-byte 8)) *) observe))
      (%do-surface-mesh-triangle-scalars
          (witness kind stock ambient mask
                   ax ay az bx by bz cx cy cz)
        (declare (ignore kind ambient mask))
        (unless (< stock (length stock-masks))
          (error "Mesh stock ~D is outside the compiled bevel policy of ~D entries."
                 stock (length stock-masks)))
        (let ((stock-mask (aref stock-masks stock)))
          (multiple-value-bind (asx asy asz)
              (%unit-bevel-point-owner ax ay az)
            (multiple-value-bind (bsx bsy bsz)
                (%unit-bevel-point-owner bx by bz)
              (multiple-value-bind (csx csy csz)
                  (%unit-bevel-point-owner cx cy cz)
                (observe asx asy asz stock-mask)
                (unless (and (= asx bsx) (= asy bsy) (= asz bsz))
                  (observe bsx bsy bsz stock-mask))
                (unless (or (and (= asx csx) (= asy csy) (= asz csz))
                            (and (= bsx csx) (= bsy csy) (= bsz csz)))
                  (observe csx csy csz stock-mask))))))))
    (when (zerop site-count)
      (setf minimum-site-x 0 maximum-site-x 0
            minimum-site-y 0 maximum-site-y 0
            minimum-site-z 0 maximum-site-z 0))
    (let* ((site-x-span (1+ (- maximum-site-x minimum-site-x)))
           (site-y-span (1+ (- maximum-site-y minimum-site-y)))
           (site-z-span (1+ (- maximum-site-z minimum-site-z)))
           (site-volume (* site-x-span site-y-span site-z-span))
           (dense-widths
             (when (and (plusp site-count)
                        (<= site-volume +dense-bevel-site-field-byte-limit+)
                        (<= site-volume
                            (* +dense-bevel-site-field-sparsity-limit+
                               site-count)))
               (make-array site-volume :element-type '(unsigned-byte 8)
                                        :initial-element 0)))
           (width-by-site
             (unless dense-widths
               (make-hash-table :test #'eql :size (max 16 site-count))))
           (maximum-width 1))
      (declare (type fixnum site-x-span site-y-span site-z-span site-volume
                            maximum-width))
      (loop for page-index across touched do
        (multiple-value-bind (page-x remainder)
            (truncate page-index (* y-pages z-pages))
          (multiple-value-bind (page-y page-z)
              (truncate remainder z-pages)
            (let ((page
                    (the (simple-array (unsigned-byte 8)
                                       (#.+bevel-site-page-volume+))
                      (aref pages page-index))))
              (dotimes (index +bevel-site-page-volume+)
                (let ((site-mask (aref page index)))
                  (unless (zerop site-mask)
                    (let ((x (+ (ash page-x 3) (ash index -6)))
                          (y (+ (ash page-y 3) (ldb (byte 3 3) index)))
                          (z (+ (ash page-z 3) (ldb (byte 3 0) index))))
                      (declare (type fixnum x y z))
                      (unless (< site-mask (length site-widths))
                        (error "Incident mesh stocks compiled to invalid bevel mask ~D at ~S."
                               site-mask (list x y z)))
                      (let ((width (aref site-widths site-mask)))
                        (unless (and (integerp width) (<= 1 width 4))
                          (error "Site-local bevel policy assigned invalid width ~S at ~S."
                                 width (list x y z)))
                        (setf maximum-width (max maximum-width width))
                        (incf (aref width-census width))
                        (if dense-widths
                            (setf (aref dense-widths
                                        (%dense-bevel-site-index
                                         x y z
                                         minimum-site-x minimum-site-y
                                         minimum-site-z
                                         site-y-span site-z-span))
                                  width)
                            (setf (gethash (%lattice-key x y z) width-by-site)
                                  width)))))))))))
      (values width-by-site dense-widths site-count maximum-width
              minimum-site-x maximum-site-x
              minimum-site-y maximum-site-y
              minimum-site-z maximum-site-z))))

(defun %vary-surface-mesh-bevel-widths
    (witness width-function stock-masks site-widths contract-t-junctions-p)
  "Compile one of the two site policies and realize its shared scalar mesh."
  (check-type witness surface-mesh)
  (when width-function
    (check-type width-function function))
  (when stock-masks
    (check-type stock-masks vector)
    (check-type site-widths vector))
  (unless (if width-function
              (and (null stock-masks) (null site-widths))
              (and stock-masks site-widths))
    (error "Specify exactly one site-local bevel policy representation."))
  (unless (= 1 (surface-mesh-bevel-width witness))
    (error "A site-local bevel witness must have width one, not ~D."
           (surface-mesh-bevel-width witness)))
  ;; Site policy remains semantic.  The renderer's positive byte-mask lane
  ;; folds through bounded sparse pages; arbitrary masks and the generic
  ;; callback retain the EQL table oracle.  Triangle realization stays in the
  ;; witness's packed scalar language, and both compilers produce the same
  ;; tight dense-or-sparse width field below.
  (let ((width-by-site nil)
        (dense-widths nil)
        (site-count 0)
        (width-census (make-array 5 :element-type '(unsigned-byte 32)
                                   :initial-element 0))
        (maximum-width 1)
        (minimum-site-x most-positive-fixnum)
        (maximum-site-x most-negative-fixnum)
        (minimum-site-y most-positive-fixnum)
        (maximum-site-y most-negative-fixnum)
        (minimum-site-z most-positive-fixnum)
        (maximum-site-z most-negative-fixnum))
    (if (and stock-masks
             (%paged-byte-stock-mask-policy-p
              (surface-mesh-domain witness) stock-masks site-widths))
        (multiple-value-setq
            (width-by-site dense-widths site-count maximum-width
             minimum-site-x maximum-site-x
             minimum-site-y maximum-site-y
             minimum-site-z maximum-site-z)
          (%compile-paged-byte-stock-mask-bevel-sites
           witness stock-masks site-widths width-census))
        (progn
          (setf width-by-site
                (make-hash-table
                 :test #'eql
                 :size
                 (max 16 (truncate (surface-mesh-triangle-count witness) 8))))
          (labels ((owner-key (x y z)
                     (multiple-value-bind
                           (site-x site-y site-z
                            direction-x direction-y direction-z)
                         (%unit-bevel-point-owner x y z)
                       (declare (ignore direction-x direction-y direction-z))
                       (setf minimum-site-x (min minimum-site-x site-x)
                             maximum-site-x (max maximum-site-x site-x)
                             minimum-site-y (min minimum-site-y site-y)
                             maximum-site-y (max maximum-site-y site-y)
                             minimum-site-z (min minimum-site-z site-z)
                             maximum-site-z (max maximum-site-z site-z))
                       (%lattice-key site-x site-y site-z)))
                   (observe-stock (key stock)
                     (pushnew stock (gethash key width-by-site) :test #'=))
                   (observe-stock-mask (key stock-mask)
                     (setf (gethash key width-by-site)
                           (the fixnum
                             (logior stock-mask
                                     (the fixnum
                                       (gethash key width-by-site 0)))))))
            (declare
             (inline owner-key observe-stock observe-stock-mask)
             (ftype (function
                      (mesh-global-tick mesh-global-tick mesh-global-tick)
                      fixnum)
                    owner-key)
             (ftype (function (fixnum fixnum) *)
                    observe-stock observe-stock-mask))
            (if stock-masks
                (%do-surface-mesh-triangle-scalars
                    (witness kind stock ambient mask
                             ax ay az bx by bz cx cy cz)
                  (declare (ignore kind ambient mask))
                  (unless (< stock (length stock-masks))
                    (error "Mesh stock ~D is outside the compiled bevel policy of ~D entries."
                           stock (length stock-masks)))
                  (let ((stock-mask (aref stock-masks stock)))
                    (unless (typep stock-mask '(unsigned-byte 61))
                      (error "Mesh stock ~D has invalid compiled bevel mask ~S."
                             stock stock-mask))
                    (let* ((stock-mask (the fixnum stock-mask))
                           (a (owner-key ax ay az))
                           (b (owner-key bx by bz))
                           (c (owner-key cx cy cz)))
                      (observe-stock-mask a stock-mask)
                      (unless (= b a)
                        (observe-stock-mask b stock-mask))
                      (unless (or (= c a) (= c b))
                        (observe-stock-mask c stock-mask)))))
                (%do-surface-mesh-triangle-scalars
                    (witness kind stock ambient mask
                             ax ay az bx by bz cx cy cz)
                  (declare (ignore kind ambient mask))
                  (let ((a (owner-key ax ay az))
                        (b (owner-key bx by bz))
                        (c (owner-key cx cy cz)))
                    (observe-stock a stock)
                    (unless (= b a)
                      (observe-stock b stock))
                    (unless (or (= c a) (= c b))
                      (observe-stock c stock))))))
          (labels ((record-width (key width)
                     (unless (and (integerp width)
                                  (<= 1 width (/ +mesh-cell-size+ 2)))
                       (error "Site-local bevel policy assigned invalid width ~S at ~S."
                              width (list (%lattice-key-x key)
                                          (%lattice-key-y key)
                                          (%lattice-key-z key))))
                     (setf (gethash key width-by-site) width
                           maximum-width (max maximum-width width))
                     (incf (aref width-census width))))
            (if stock-masks
                (maphash
                 (lambda (key site-mask)
                   (unless (and (plusp site-mask)
                                (< site-mask (length site-widths)))
                     (error "Incident mesh stocks compiled to invalid bevel mask ~D at ~S."
                            site-mask (list (%lattice-key-x key)
                                            (%lattice-key-y key)
                                            (%lattice-key-z key))))
                   (record-width key (aref site-widths site-mask)))
                 width-by-site)
                (maphash
                 (lambda (key stocks)
                   (record-width
                    key
                    (funcall width-function
                             (%lattice-key-x key)
                             (%lattice-key-y key)
                             (%lattice-key-z key)
                             (sort stocks #'<))))
                 width-by-site)))
          (setf site-count (hash-table-count width-by-site))
          (when (zerop site-count)
            (setf minimum-site-x 0 maximum-site-x 0
                  minimum-site-y 0 maximum-site-y 0
                  minimum-site-z 0 maximum-site-z 0))))
    (let* ((site-x-span (1+ (- maximum-site-x minimum-site-x)))
           (site-y-span (1+ (- maximum-site-y minimum-site-y)))
           (site-z-span (1+ (- maximum-site-z minimum-site-z)))
           (site-volume (* site-x-span site-y-span site-z-span))
           (dense-widths
             (or dense-widths
                 (when (and (plusp site-count)
                            (<= site-volume +dense-bevel-site-field-byte-limit+)
                            (<= site-volume
                                (* +dense-bevel-site-field-sparsity-limit+
                                   site-count)))
                   (make-array site-volume :element-type '(unsigned-byte 8)
                                            :initial-element 0))))
           (packing
             (%make-spatial-edge-packing-for-box
              minimum-site-x (1+ maximum-site-x)
              minimum-site-y (1+ maximum-site-y)))
           (builder (%make-surface-mesh-builder
                     (surface-mesh-domain witness) maximum-width))
           ;; Only a three-distinct-point collinear collapse can create the
           ;; long-edge/two-short-edge mismatch.  Count that exact local edge
           ;; neighborhood, not every edge in the otherwise closed witness.
           (candidate-splits (make-hash-table :test #'eql))
           (queried-edge-counts (make-hash-table :test #'eql))
           (repair-splits (make-hash-table :test #'eql))
           (queried-source-filter nil)
           (repair-source-filter nil)
           (live-triangle-counts
             (make-array 3 :element-type '(unsigned-byte 32)
                           :initial-element 0))
           (collapsed-triangle-count 0))
      (declare (type fixnum site-x-span site-y-span site-z-span
                            site-volume site-count))
      (when (and dense-widths width-by-site
                 (plusp (hash-table-count width-by-site)))
        (maphash
         (lambda (key width)
           (setf (aref dense-widths
                       (%dense-bevel-site-index
                        (%lattice-key-x key)
                        (%lattice-key-y key)
                        (%lattice-key-z key)
                        minimum-site-x minimum-site-y minimum-site-z
                        site-y-span site-z-span))
                 width))
         width-by-site)
        (clrhash width-by-site))
      (setf (surface-mesh-builder-singular-star-count builder)
            (surface-mesh-singular-star-count witness))
      (labels ((site-width (site-x site-y site-z)
                 (let ((width
                         (if dense-widths
                             (aref dense-widths
                                   (%dense-bevel-site-index
                                    site-x site-y site-z
                                    minimum-site-x minimum-site-y minimum-site-z
                                    site-y-span site-z-span))
                             (gethash (%lattice-key site-x site-y site-z)
                                      width-by-site))))
                   (unless (and (integerp width) (<= 1 width 4))
                     (error "No site-local bevel width was compiled for ~S."
                            (list site-x site-y site-z)))
                   (the (integer 1 4) width)))
               (transformed-triangle
                   (ax ay az bx by bz cx cy cz)
                 (multiple-value-bind
                       (asx asy asz adx ady adz)
                     (%unit-bevel-point-owner ax ay az)
                   (multiple-value-bind
                         (bsx bsy bsz bdx bdy bdz)
                       (%unit-bevel-point-owner bx by bz)
                     (multiple-value-bind
                           (csx csy csz cdx cdy cdz)
                         (%unit-bevel-point-owner cx cy cz)
                       (let* ((aw (site-width asx asy asz))
                              (bw
                                (if (and (= asx bsx) (= asy bsy) (= asz bsz))
                                    aw
                                    (site-width bsx bsy bsz)))
                              (cw
                                (cond
                                  ((and (= asx csx) (= asy csy) (= asz csz))
                                   aw)
                                  ((and (= bsx csx) (= bsy csy) (= bsz csz))
                                   bw)
                                  (t (site-width csx csy csz))))
                              (ad (1- aw)) (bd (1- bw)) (cd (1- cw)))
                         (declare (type (integer 1 4) aw bw cw)
                                  (type (integer 0 3) ad bd cd))
                         (values (+ ax (* ad adx))
                                 (+ ay (* ad ady))
                                 (+ az (* ad adz))
                                 (+ bx (* bd bdx))
                                 (+ by (* bd bdy))
                                 (+ bz (* bd bdz))
                                 (+ cx (* cd cdx))
                                 (+ cy (* cd cdy))
                                 (+ cz (* cd cdz))))))))
               (spatial-edge-key (lx ly lz rx ry rz)
                 (nth-value 0
                   (%pack-spatial-edge packing lx ly lz rx ry rz)))
               (packed-point-edge-key (left right)
                 (spatial-edge-key
                  (%global-mesh-point-x left)
                  (%global-mesh-point-y left)
                  (%global-mesh-point-z left)
                  (%global-mesh-point-x right)
                  (%global-mesh-point-y right)
                  (%global-mesh-point-z right)))
               (record-candidate (lx ly lz rx ry rz middle)
                 (let ((edge (spatial-edge-key lx ly lz rx ry rz)))
                   (pushnew middle (gethash edge candidate-splits) :test #'=)))
               (discover-transition (kind ax ay az bx by bz cx cy cz)
                 (multiple-value-bind
                       (tax tay taz tbx tby tbz tcx tcy tcz)
                     (transformed-triangle
                      ax ay az bx by bz cx cy cz)
                   (multiple-value-bind (nx ny nz)
                       (%triangle-cross-scalars
                        tax tay taz tbx tby tbz tcx tcy tcz)
                     (if (and (zerop nx) (zerop ny) (zerop nz))
                         (progn
                           (incf collapsed-triangle-count)
                           (unless
                               (or (and (= tax tbx) (= tay tby) (= taz tbz))
                                   (and (= tbx tcx) (= tby tcy) (= tbz tcz))
                                   (and (= tcx tax) (= tcy tay) (= tcz taz)))
                             (let* ((pa
                                      (%pack-global-mesh-point tax tay taz))
                                    (pb
                                      (%pack-global-mesh-point tbx tby tbz))
                                    (pc
                                      (%pack-global-mesh-point tcx tcy tcz))
                                    (ab
                                      (%global-mesh-point-distance-squared pa pb))
                                    (bc
                                      (%global-mesh-point-distance-squared pb pc))
                                    (ca
                                      (%global-mesh-point-distance-squared pc pa)))
                               ;; Match the reference reduction's later-edge
                               ;; tie preference, though a collinear interior
                               ;; point gives the long edge a strict maximum.
                               (if (> ab bc)
                                   (if (> ab ca)
                                       (record-candidate
                                        tax tay taz tbx tby tbz pc)
                                       (record-candidate
                                        tcx tcy tcz tax tay taz pb))
                                   (if (> bc ca)
                                       (record-candidate
                                        tbx tby tbz tcx tcy tcz pa)
                                       (record-candidate
                                        tcx tcy tcz tax tay taz pb))))))
                         (incf
                          (aref live-triangle-counts
                                (ecase kind
                                  (:face 0)
                                  (:band 1)
                                  (:junction 2))))))))
               (count-queried-edge (lx ly lz rx ry rz)
                 (let ((key (spatial-edge-key lx ly lz rx ry rz)))
                   (multiple-value-bind (count present-p)
                       (gethash key queried-edge-counts)
                     (when present-p
                       (setf (gethash key queried-edge-counts) (1+ count))))))
               (scan-queried-transition (ax ay az bx by bz cx cy cz)
                 (multiple-value-bind
                       (tax tay taz tbx tby tbz tcx tcy tcz)
                     (transformed-triangle
                      ax ay az bx by bz cx cy cz)
                   (multiple-value-bind (nx ny nz)
                       (%triangle-cross-scalars
                        tax tay taz tbx tby tbz tcx tcy tcz)
                     (unless (and (zerop nx) (zerop ny) (zerop nz))
                       (count-queried-edge tax tay taz tbx tby tbz)
                       (count-queried-edge tbx tby tbz tcx tcy tcz)
                       (count-queried-edge tcx tcy tcz tax tay taz)))))
               (edge-splits (lx ly lz rx ry rz)
                 (let* ((edge (spatial-edge-key lx ly lz rx ry rz))
                        (points (gethash edge repair-splits)))
                   (when points
                     (let* ((left (%pack-global-mesh-point lx ly lz))
                            (right (%pack-global-mesh-point rx ry rz))
                            (ordered (if (< left right) points (reverse points))))
                       (mapcar #'%global-mesh-point-list ordered)))))
               (edge-points (left right)
                 (append (list left)
                         (edge-splits
                          (first left) (second left) (third left)
                          (first right) (second right) (third right))
                         (list right)))
               (mark-visible-edge (table left right bit mask)
                 (when (logtest bit mask)
                   (loop for points on (edge-points left right)
                         while (rest points)
                         do (setf (gethash
                                   (%ordered-point-edge
                                    (first points) (second points))
                                   table)
                                  t))))
               (triangle-boundary-mask (visible a b c)
                 (logior (if (gethash (%ordered-point-edge b c) visible)
                             #b001 0)
                         (if (gethash (%ordered-point-edge c a) visible)
                             #b010 0)
                         (if (gethash (%ordered-point-edge a b) visible)
                             #b100 0)))
               (emit-transition
                   (kind stock ambient mask ax ay az bx by bz cx cy cz
                    repair-neighborhood-p)
                 (multiple-value-bind
                       (tax tay taz tbx tby tbz tcx tcy tcz)
                     (transformed-triangle
                      ax ay az bx by bz cx cy cz)
                   (multiple-value-bind (nx ny nz)
                       (%triangle-cross-scalars
                        tax tay taz tbx tby tbz tcx tcy tcz)
                     (unless (and (zerop nx) (zerop ny) (zerop nz))
                           (multiple-value-bind (onx ony onz)
                               (%triangle-cross-scalars
                                ax ay az bx by bz cx cy cz)
                             (unless
                                 (plusp
                                  (the fixnum
                                    (+ (the fixnum (* nx onx))
                                       (the fixnum (* ny ony))
                                       (the fixnum (* nz onz)))))
                               (error "Site-local bevel folded ~S triangle ~S ~S ~S into ~S ~S ~S."
                                      kind
                                      (list ax ay az) (list bx by bz) (list cx cy cz)
                                      (list tax tay taz) (list tbx tby tbz)
                                      (list tcx tcy tcz))))
                           (let ((ab (when repair-neighborhood-p
                                       (edge-splits
                                        tax tay taz tbx tby tbz)))
                                 (bc (when repair-neighborhood-p
                                       (edge-splits
                                        tbx tby tbz tcx tcy tcz)))
                                 (ca (when repair-neighborhood-p
                                       (edge-splits
                                        tcx tcy tcz tax tay taz))))
                             (if (not (or ab bc ca))
                                 (%emit-global-triangle-scalars
                                  builder kind stock ambient mask nx ny nz
                                  tax tay taz tbx tby tbz tcx tcy tcz)
                                 (let* ((ta (list tax tay taz))
                                        (tb (list tbx tby tbz))
                                        (tc (list tcx tcy tcz))
                                        (cross (list nx ny nz))
                                        (loop (append (list ta) ab (list tb) bc
                                                      (list tc) ca))
                                        (triangles
                                          (%triangulate-coplanar-loop loop cross))
                                        (visible (make-hash-table :test #'equal)))
                                   (unless triangles
                                     (error "Could not contract site-local bevel T-junction around ~S."
                                            loop))
                                   (mark-visible-edge visible ta tb #b100 mask)
                                   (mark-visible-edge visible tb tc #b001 mask)
                                   (mark-visible-edge visible tc ta #b010 mask)
                                   (dolist (triangle triangles)
                                     (destructuring-bind (a b c) triangle
                                       (%emit-global-triangle
                                        builder kind stock ambient
                                        (triangle-boundary-mask visible a b c)
                                        (%primitive-plane-normal a b c)
                                        triangle)))))))))))
        (declare
         (inline site-width transformed-triangle spatial-edge-key
                 packed-point-edge-key record-candidate count-queried-edge
                 edge-splits)
         (ftype (function (fixnum fixnum fixnum) (integer 1 4)) site-width)
         (ftype (function
                  (mesh-global-tick mesh-global-tick mesh-global-tick
                   mesh-global-tick mesh-global-tick mesh-global-tick
                   mesh-global-tick mesh-global-tick mesh-global-tick)
                  (values mesh-global-tick mesh-global-tick mesh-global-tick
                          mesh-global-tick mesh-global-tick mesh-global-tick
                          mesh-global-tick mesh-global-tick mesh-global-tick
                          &optional))
                transformed-triangle))
        (%do-surface-mesh-triangle-scalars
            (witness kind stock ambient mask
                     ax ay az bx by bz cx cy cz)
          (declare (ignore stock ambient mask))
          (discover-transition kind ax ay az bx by bz cx cy cz))
        (setf queried-edge-counts
              (make-hash-table
               :test #'eql :size (max 16 (* 3 (hash-table-count candidate-splits)))))
        (maphash
         (lambda (edge points)
           (multiple-value-bind (left right)
               (%spatial-edge-points packing edge)
             (let* ((left-packed
                      (%pack-global-mesh-point
                       (first left) (second left) (third left)))
                    (right-packed
                      (%pack-global-mesh-point
                       (first right) (second right) (third right)))
                    (points
                      (sort points #'<
                            :key (lambda (point)
                                   (%global-mesh-point-distance-squared
                                    left-packed point)))))
               (setf (gethash edge candidate-splits) points
                     (gethash edge queried-edge-counts) 0)
               (loop with previous = left-packed
                     for point in points
                     do (setf (gethash (packed-point-edge-key previous point)
                                       queried-edge-counts)
                              0
                              previous point)
                     finally
                        (setf (gethash
                               (packed-point-edge-key previous right-packed)
                               queried-edge-counts)
                              0)))))
         candidate-splits)
        (setf queried-source-filter
              (%make-source-anchor-filter packing queried-edge-counts))
        (when queried-source-filter
          (%do-surface-mesh-triangle-scalars
              (witness kind stock ambient mask
                       ax ay az bx by bz cx cy cz)
            (declare (ignore kind stock ambient mask))
            (when (%triangle-touches-source-anchor-filter-p
                   queried-source-filter ax ay az bx by bz cx cy cz)
              (scan-queried-transition ax ay az bx by bz cx cy cz))))
        (let ((unmatched-edge-count
                (loop for count being the hash-values of queried-edge-counts
                      count (not (or (zerop count) (= count 2))))))
          (when contract-t-junctions-p
            (maphash
             (lambda (edge points)
               (when (= 1 (gethash edge queried-edge-counts 0))
                 (multiple-value-bind (left right)
                     (%spatial-edge-points packing edge)
                   (let ((left
                           (%pack-global-mesh-point
                            (first left) (second left) (third left)))
                         (right
                           (%pack-global-mesh-point
                            (first right) (second right) (third right))))
                     (when (loop with previous = left
                                 for point in points
                                 always (= 1 (gethash
                                              (packed-point-edge-key previous point)
                                              queried-edge-counts 0))
                                 do (setf previous point)
                                 finally (return
                                           (= 1 (gethash
                                                 (packed-point-edge-key
                                                  previous right)
                                                 queried-edge-counts 0))))
                       (setf (gethash edge repair-splits) points))))))
             candidate-splits)
            (setf repair-source-filter
                  (%make-source-anchor-filter packing repair-splits))
            ;; Prove that the selected contractions account for the entire
            ;; mismatch before changing any triangles.  A new failure mode
            ;; must become explicit rather than rendering another hairline
            ;; crack.
            (maphash
             (lambda (edge points)
               (decf (gethash edge queried-edge-counts))
               (multiple-value-bind (left right)
                   (%spatial-edge-points packing edge)
                 (let ((left
                         (%pack-global-mesh-point
                          (first left) (second left) (third left)))
                       (right
                         (%pack-global-mesh-point
                          (first right) (second right) (third right))))
                   (loop with previous = left
                         for point in points
                         do (incf (gethash
                                   (packed-point-edge-key previous point)
                                   queried-edge-counts 0))
                            (setf previous point)
                         finally
                            (incf (gethash
                                   (packed-point-edge-key previous right)
                                   queried-edge-counts 0))))))
             repair-splits))
          (let ((residual-edge-count
                  (loop for count being the hash-values of queried-edge-counts
                        count (not (or (zerop count) (= count 2))))))
            (when (and contract-t-junctions-p
                       (plusp residual-edge-count))
              (error "Site-local bevel contraction left ~D of ~D unmatched edges after ~D repairs."
                     residual-edge-count unmatched-edge-count
                     (hash-table-count repair-splits)))
            (let ((repair-point-count
                    (loop for points being the hash-values of repair-splits
                          sum (length points))))
              (%reserve-builder-triangle-capacities
               builder
               (+ (aref live-triangle-counts 0) repair-point-count)
               (+ (aref live-triangle-counts 1) repair-point-count)
               (+ (aref live-triangle-counts 2) repair-point-count)))
            (%do-surface-mesh-triangle-scalars
                (witness kind stock ambient mask
                         ax ay az bx by bz cx cy cz)
              (emit-transition kind stock ambient mask
                               ax ay az bx by bz cx cy cz
                               (and repair-source-filter
                                    (%triangle-touches-source-anchor-filter-p
                                     repair-source-filter
                                     ax ay az bx by bz cx cy cz))))
            (values
             (%finish-surface-mesh builder)
             width-census
             (list :collapsed-triangle-count collapsed-triangle-count
                   :unmatched-edge-count unmatched-edge-count
                   :repaired-edge-count (hash-table-count repair-splits)
                   :residual-edge-count residual-edge-count
                   :candidate-splits
                   (loop for edge being the hash-keys of candidate-splits
                           using (hash-value points)
                         append
                         (multiple-value-bind (left right)
                             (%spatial-edge-points packing edge)
                           (loop for point in points
                                 collect
                                 (list left (%global-mesh-point-list point)
                                       right))))))))))))

(defun vary-surface-mesh-bevel-widths
    (witness width-function &key (contract-t-junctions-p t))
  "Evaluate one closed width-one WITNESS at a local width per vertex site.

WIDTH-FUNCTION is called once for each canonical lattice vertex as
  (WIDTH-FUNCTION X Y Z INCIDENT-STOCKS)
where INCIDENT-STOCKS is a sorted, duplicate-free list of the packed stocks on
witness triangles using that site.  It must return an integer width from one
through four.

Every witness vertex has the exact affine form 8*S + Q with Q in {-1,0,1}^3.
The result replaces it by 8*S + WIDTH(S)*Q.  Since every incident primitive
uses the same canonical S, shared vertices remain equal without stitching.
At the medial limit a witness triangle can collapse to three collinear points.
The result contracts that triangle by splitting its surviving neighbour's long
edge at the middle point, eliminating the otherwise visible T-junction without
inventing a surface.  WITNESS remains the rebuild oracle for topology and
uniform-width geometry.  Transition triangles may leave the uniform mesher's
26 exact normal directions.  The packed trit normal remains an orientation
witness; fragment shading derives the actual primitive normal from world-space
position derivatives, so the new directions are not lighting-quantized.

The second value is a five-entry site census indexed by width.  The third is a
diagnostic plist containing the collapsed-triangle, locally unmatched-edge,
repaired-edge, and residual-edge counts.  For the required closed width-one
witness, only a collapse can change edge parity, so the queried local counts
are the complete transition defect.

CONTRACT-T-JUNCTIONS-P defaults true.  NIL deliberately omits collapsed
triangles without subdividing their surviving neighbours, returning the open
diagnostic surface that motivates the contraction.  Production callers should
retain the default; the uncontracted surface exists only for topology study."
  (check-type width-function function)
  (%vary-surface-mesh-bevel-widths
   witness width-function nil nil contract-t-junctions-p))

(defun vary-surface-mesh-bevel-widths-from-stock-masks
    (witness stock-masks site-widths &key (contract-t-junctions-p t))
  "Evaluate a closed width-one WITNESS from an incident-stock mask policy.

STOCK-MASKS is indexed by packed triangle stock.  The masks of every stock
incident on a canonical vertex site are combined with LOGIOR, then that mask
indexes SITE-WIDTHS.  This is the dense production form of the generic callback
contract: it preserves the same shared site field and the same realization and
repair algorithm without constructing stock lists in the triangle loop.

Every referenced stock mask must be a positive fixnum bit mask, and each
combined mask must be a valid SITE-WIDTHS index.  Index zero is unused; every
selected entry must be an integer width from one through four."
  (check-type stock-masks vector)
  (check-type site-widths vector)
  (%vary-surface-mesh-bevel-widths
   witness nil stock-masks site-widths contract-t-junctions-p))

(defun %coplanar-group-loops (triangles)
  "Return oriented boundary loops, or NIL when the union is not simple."
  (let ((edges (make-hash-table :test #'equal)))
    (dolist (triangle triangles)
      (destructuring-bind (mask a b c) triangle
        (declare (ignore mask))
        (dolist (edge (list (list a b) (list b c) (list c a)))
          (let ((key (%ordered-point-edge (first edge) (second edge))))
            (if (gethash key edges)
                (remhash key edges)
                (setf (gethash key edges) edge))))))
    (let ((next (make-hash-table :test #'equal))
          (incoming (make-hash-table :test #'equal)))
      (loop for edge being the hash-values of edges do
        (destructuring-bind (start end) edge
          (when (gethash start next)
            (return-from %coplanar-group-loops nil))
          (setf (gethash start next) end)
          (incf (gethash end incoming 0))))
      (loop for start being the hash-keys of next
            unless (= 1 (gethash start incoming 0))
              do (return-from %coplanar-group-loops nil))
      (let ((loops nil))
        (loop while (plusp (hash-table-count next)) do
          (let* ((start (sort (loop for point being the hash-keys of next
                                    collect point)
                              #'%point-order<))
                 (start (first start))
                 (point start)
                 (loop nil))
            (loop do (push point loop)
                     (multiple-value-bind (following present-p)
                         (gethash point next)
                       (unless present-p
                         (return-from %coplanar-group-loops nil))
                       (remhash point next)
                       (setf point following))
                  until (equal point start))
            (push (nreverse loop) loops)))
        (nreverse loops)))))

(defun %point-in-oriented-triangle-p (point a b c normal)
  (and (not (member point (list a b c) :test #'equal))
       (>= (%point-dot (%point-cross a b point) normal) 0)
       (>= (%point-dot (%point-cross b c point) normal) 0)
       (>= (%point-dot (%point-cross c a point) normal) 0)))

(defun %triangulate-coplanar-loop (loop normal)
  "Ear-clip one positively oriented simple integer polygon."
  ;; Retain collinear boundary vertices: another coplanar attribute group or
  ;; differently oriented plane may meet there. Removing such a vertex would
  ;; preserve the continuous surface but introduce a topological T-junction.
  (let ((points loop)
        (triangles nil))
    (when (< (length points) 3)
      (return-from %triangulate-coplanar-loop nil))
    (loop while (> (length points) 3) do
      (let ((ear-index nil)
            (count (length points)))
        (dotimes (index count)
          (let ((a (nth (mod (1- index) count) points))
                (b (nth index points))
                (c (nth (mod (1+ index) count) points)))
            (when (and (plusp (%point-dot (%point-cross a b c) normal))
                       (notany (lambda (point)
                                 (%point-in-oriented-triangle-p
                                  point a b c normal))
                               points))
              (setf ear-index index)
              (return))))
        (unless ear-index
          (return-from %triangulate-coplanar-loop nil))
        (let* ((count (length points))
               (a (nth (mod (1- ear-index) count) points))
               (b (nth ear-index points))
               (c (nth (mod (1+ ear-index) count) points)))
          (push (list a b c) triangles)
          (setf points
                (loop for point in points
                      for index from 0
                      unless (= index ear-index) collect point)))))
    (push points triangles)
    (nreverse triangles)))

(defun %emit-global-triangle (builder kind stock ambient mask normal triangle)
  (destructuring-bind (a b c) triangle
    (let* ((minimums
             (loop for axis below 3
                   collect (min (nth axis a) (nth axis b) (nth axis c))))
           (base (mapcar (lambda (coordinate)
                           (floor coordinate +mesh-cell-size+))
                         minimums))
           (origin (mapcar (lambda (coordinate)
                             (* coordinate +mesh-cell-size+))
                           base))
           (scratch (surface-mesh-builder-vertex-scratch builder)))
      (%scratch-triangle
       scratch 0 (ecase kind (:face 0) (:band 1) (:junction 2)) mask
       (- (first a) (first origin))
       (- (second a) (second origin))
       (- (third a) (third origin))
       (- (first b) (first origin))
       (- (second b) (second origin))
       (- (third b) (third origin))
       (- (first c) (first origin))
       (- (second c) (second origin))
       (- (third c) (third origin))
       (first normal) (second normal) (third normal))
      (%emit-instance builder kind (first base) (second base) (third base)
                      stock ambient 3))))

;;; ---------------------------------------------------------------------------
;;; Semantic face attachments

(defun %torch-axis-vector (axis)
  (ecase axis
    (:x '(1 0 0))
    (:y '(0 1 0))
    (:z '(0 0 1))))

(defun %torch-vector-cross (a b)
  (list (- (* (second a) (third b)) (* (third a) (second b)))
        (- (* (third a) (first b)) (* (first a) (third b)))
        (- (* (first a) (second b)) (* (second a) (first b)))))

(defun %torch-vector-dot (a b)
  (+ (* (first a) (first b))
     (* (second a) (second b))
     (* (third a) (third b))))

(defun %torch-vector-scale (amount vector)
  (mapcar (lambda (component) (* amount component)) vector))

(defun %torch-vector+ (&rest vectors)
  (loop for axis below 3
        collect (loop for vector in vectors sum (nth axis vector))))

(defun %torch-face-frame (domain face)
  (%require-face domain face)
  (multiple-value-bind (u-axis v-axis) (face-tangent-axes face)
    (multiple-value-bind (nx ny nz) (face-oriented-normal face)
      (let* ((normal (list nx ny nz))
             (u (%torch-axis-vector u-axis))
             (v (%torch-axis-vector v-axis)))
        ;; Maintain a right-handed frame even when FACE has negative polarity.
        (when (minusp (%torch-vector-dot (%torch-vector-cross u v) normal))
          (setf v (%torch-vector-scale -1 v)))
        (values u v normal)))))

(defun %torch-face-center (face)
  (loop for coordinate in (list (site-x face) (site-y face) (site-z face))
        for axis-number from 0
        collect (+ (* +mesh-cell-size+ coordinate)
                   (if (logbitp axis-number (site-extent face))
                       (/ +mesh-cell-size+ 2)
                       0))))

(defun %emit-facing-torch-triangle
    (builder stock expected-normal a b c)
  (let ((normal (%primitive-plane-normal a b c)))
    (when (minusp (%torch-vector-dot normal expected-normal))
      (rotatef b c)
      (setf normal (%primitive-plane-normal a b c)))
    (%emit-global-triangle builder :junction stock 0 #b111 normal
                           (list a b c))))

(defun %torch-ring-center (ring)
  (loop for axis below 3
        collect (truncate
                 (loop for point in ring sum (nth axis point))
                 (length ring))))

(defun %emit-torch-cone
    (builder stock apex ring normal ring-radius axial-distance tip-p)
  (dotimes (index (length ring))
    (let* ((next (mod (1+ index) (length ring)))
           (a (nth index ring))
           (b (nth next ring))
           (ring-center (%torch-ring-center ring))
           (radial (%torch-vector+
                    (%torch-vector+ a b)
                    (%torch-vector-scale -2 ring-center)))
           (expected
             (%torch-vector+
              (%torch-vector-scale axial-distance radial)
              (%torch-vector-scale
               (if tip-p (* 2 ring-radius) (* -2 ring-radius))
               normal))))
      (if tip-p
          (%emit-facing-torch-triangle builder stock expected a apex b)
          (%emit-facing-torch-triangle builder stock expected apex b a)))))

(defun %emit-torch-frustum
    (builder stock lower upper normal lower-radius upper-radius axial-distance)
  (unless (= (length lower) (length upper))
    (error "Torch frustum rings have different vertex counts."))
  (dotimes (index (length lower))
    (let* ((next (mod (1+ index) (length lower)))
           (a (nth index lower))
           (b (nth next lower))
           (c (nth next upper))
           (d (nth index upper))
           (lower-center (%torch-ring-center lower))
           (upper-center (%torch-ring-center upper))
           (radial (%torch-vector+
                    (%torch-vector+ a b c d)
                    (%torch-vector-scale -2 lower-center)
                    (%torch-vector-scale -2 upper-center)))
           (expected
             (%torch-vector+
              (%torch-vector-scale axial-distance radial)
              (%torch-vector-scale
               (* -4 (- upper-radius lower-radius)) normal))))
      (%emit-facing-torch-triangle builder stock expected a c d)
      (%emit-facing-torch-triangle builder stock expected a b c))))

(defun make-face-torch-mesh
    (domain faces body-stock flame-stock &key (bevel-width +mesh-bevel-width+))
  "Compile sparse oriented face attachments into an independent site stream.

Each torch touches its semantic cubical face at one exact point, then grows
only into the outward neighboring cell.  The attachment therefore remains
coherent when a material bevel reaches width four and its central face patch
vanishes.  FACES are oriented face sites; BODY-STOCK and FLAME-STOCK retain
independent opaque/luminous material semantics in the renderer."
  (check-type domain world-domain)
  (check-type body-stock (unsigned-byte #.+mesh-instance-stock-bit-count+))
  (check-type flame-stock (unsigned-byte #.+mesh-instance-stock-bit-count+))
  (check-type bevel-width (integer 1 4))
  (let ((builder (%make-surface-mesh-builder domain bevel-width)))
    (map nil
         (lambda (face)
           (multiple-value-bind (u v normal)
               (%torch-face-frame domain face)
             (let ((center (%torch-face-center face)))
               (labels ((point (distance u-radius v-radius)
                          (%torch-vector+
                           center
                           (%torch-vector-scale distance normal)
                           (%torch-vector-scale u-radius u)
                           (%torch-vector-scale v-radius v)))
                        (ring (distance radius)
                          (list (point distance radius 0)
                                (point distance radius radius)
                                (point distance 0 radius)
                                (point distance (- radius) radius)
                                (point distance (- radius) 0)
                                (point distance (- radius) (- radius))
                                (point distance 0 (- radius))
                                (point distance radius (- radius)))))
                 (let ((socket (ring 1 1))
                       (shaft (ring 4 1))
                       (tip (point 7 0 0)))
                   (%emit-torch-cone builder body-stock center socket
                                     normal 1 1 nil)
                   (%emit-torch-frustum builder body-stock socket shaft
                                        normal 1 1 3)
                   (%emit-torch-cone builder flame-stock tip shaft
                                     normal 1 3 t))))))
         faces)
    (%finish-surface-mesh builder)))

(defun surface-mesh-with-triangle-ink (mesh)
  "Return MESH's exact triangles with every primitive edge marked visible.

The geometry, stock, ambient value, primitive class, and winding are retained.
Only the three construction-mask bits change.  This diagnostic realization
exposes connectivity that the ordinary semantic edge mask intentionally hides;
it must not be substituted for the production mesh outside topology captures."
  (check-type mesh surface-mesh)
  (let ((builder (%make-surface-mesh-builder
                  (surface-mesh-domain mesh) (surface-mesh-bevel-width mesh))))
    (setf (surface-mesh-builder-singular-star-count builder)
          (surface-mesh-singular-star-count mesh))
    (%map-surface-mesh-triangle-records
     (lambda (kind stock ambient mask normal a b c)
       (declare (ignore mask))
       (%emit-global-triangle builder kind stock ambient #b111 normal
                              (list a b c)))
     mesh)
    (%finish-surface-mesh builder)))

(defun surface-mesh-split-neighborhood (mesh split)
  "Return only the triangles incident to the three points in SPLIT.

SPLIT is (LEFT MIDDLE RIGHT), as reported in the :CANDIDATE-SPLITS bevel
diagnostic.  A triangle is retained when two or more of its vertices are
split points, so the result is the smallest actual mesh patch that contrasts
one long edge with its two short neighbours.  Every retained edge is marked
visible; no vertex or triangle geometry is otherwise changed.  This is the
executable closeup used by the mixed-bevel degeneracy atlas. #WSEK3C"
  (check-type mesh surface-mesh)
  (unless (and (listp split) (= 3 (length split)))
    (error "A mesh split neighborhood needs (LEFT MIDDLE RIGHT), not ~S."
           split))
  (let ((builder (%make-surface-mesh-builder
                  (surface-mesh-domain mesh) (surface-mesh-bevel-width mesh))))
    (%map-surface-mesh-triangle-records
     (lambda (kind stock ambient mask normal a b c)
       (declare (ignore mask))
       (when (>= (count-if (lambda (point)
                             (member point split :test #'equal))
                           (list a b c))
                 2)
         (%emit-global-triangle builder kind stock ambient #b111 normal
                                (list a b c))))
     mesh)
    (%finish-surface-mesh builder)))

(defun %coplanar-group-key< (left right)
  (flet ((numeric-key (key)
           (destructuring-bind (kind stock ambient normal plane) key
             (list (ecase kind (:face 0) (:band 1) (:junction 2))
                   stock ambient
                   (first normal) (second normal) (third normal) plane))))
    (loop for l in (numeric-key left)
          for r in (numeric-key right)
          when (/= l r) return (< l r)
          finally (return nil))))

(defun %coplanar-merged-surface-mesh (mesh)
  "Dissolve only interior edges between exactly coplanar equal-attribute faces.

The output has the same points, oriented planes, stocks, ambient values,
silhouette, and depth as MESH. Groups with a non-simple boundary retain their
original triangles, making the unmerged medial mesh a local rebuild oracle."
  (let ((groups (make-hash-table :test #'equal))
        (builder (%make-surface-mesh-builder
                  (surface-mesh-domain mesh) (surface-mesh-bevel-width mesh))))
    (%map-surface-mesh-triangle-records
     (lambda (kind stock ambient mask normal a b c)
       (let ((key (list kind stock ambient normal (%point-dot normal a))))
         (push (list mask a b c) (gethash key groups))))
     mesh)
    (dolist (key (sort (loop for key being the hash-keys of groups collect key)
                       #'%coplanar-group-key<))
      (let ((triangles (gethash key groups)))
        (destructuring-bind (kind stock ambient normal plane) key
               (declare (ignore plane))
               (let ((loops (%coplanar-group-loops triangles))
                     (merged nil)
                     (boundary (make-hash-table :test #'equal)))
                 (when loops
                   (dolist (loop loops)
                     (loop for point on loop
                           for a = (first point)
                           for b = (or (second point) (first loop))
                           do (setf (gethash (%ordered-point-edge a b) boundary)
                                    t)))
                   (setf merged
                         (loop for loop in loops
                               when (plusp
                                     (loop for point on loop
                                           for a = (first point)
                                           for b = (or (second point)
                                                       (first loop))
                                           sum (%point-dot
                                                (%point-cross '(0 0 0) a b)
                                                normal)))
                                 append (%triangulate-coplanar-loop loop normal)
                               else do (setf loops nil))))
                 (if (and loops merged)
                     (dolist (triangle merged)
                       (destructuring-bind (a b c) triangle
                         (let ((mask
                                 (logior
                                  (if (gethash (%ordered-point-edge b c)
                                               boundary) #b001 0)
                                  (if (gethash (%ordered-point-edge c a)
                                               boundary) #b010 0)
                                  (if (gethash (%ordered-point-edge a b)
                                               boundary) #b100 0))))
                           (%emit-global-triangle builder kind stock ambient
                                                  mask normal triangle))))
                     (dolist (triangle triangles)
                       (%emit-global-triangle builder kind stock ambient
                                              (first triangle) normal
                                              (rest triangle))))))))
    (setf (surface-mesh-builder-singular-star-count builder)
          (surface-mesh-singular-star-count mesh))
    (%finish-surface-mesh builder)))

;;; ---------------------------------------------------------------------------
;;; Entry point

(defun %call-with-boundary-policy (policy thunk)
  "Run THUNK under one OUTSIDE-DOMAIN policy.

:AIR answers every out-of-box probe with the TREAT-AS-AIR restart; :SIGNAL
leaves the condition for the caller's own handlers (a chunk store, a test)."
  (ecase policy
    (:air (handler-bind ((outside-domain
                           (lambda (condition)
                             (declare (ignore condition))
                             (invoke-restart 'treat-as-air))))
            (funcall thunk)))
    (:signal (funcall thunk))))

(defun make-surface-mesh
    (solid &key (stock-function (constantly 0))
                (chamfer-stock-function (lambda (stocks) (first stocks)))
                (bevel-width +mesh-bevel-width+)
                (boundary :air))
  "Classify SOLID into exact integer face, edge, and vertex instance streams.

Below the medial limit, every exposed cell face emits the same width-dependent
central square and crease edges own the intervening bands.  At the half-cell
limit those two families become zero-area seams: a sub-medial witness retains
their boundary cycles while only the expanded site-local patches are emitted.
Each stream is sorted by template so the renderer can issue direct instanced
draws.
STOCK-FUNCTION is called with an oriented boundary face.  CHAMFER-STOCK-FUNCTION
receives the face stocks incident to one edge-owned collar, bevel, or
lattice-site closure.  It must return one stock for that entire chamfer."
  (check-type solid chain)
  (check-type stock-function function)
  (check-type chamfer-stock-function function)
  (unless (and (integerp bevel-width)
               (<= 1 bevel-width (/ +mesh-cell-size+ 2)))
    (error "Bevel width ~S must be an integer between one and four ticks."
           bevel-width))
  (check-type boundary (member :air :signal))
  (let* ((domain (chain-domain solid))
         (field (%materialize-occupancy
                 solid
                 0 (world-domain-x-limit domain)
                 0 (world-domain-y-limit domain)))
         (builder (%make-surface-mesh-builder domain bevel-width))
         (boundary-builder
           (if (= bevel-width (/ +mesh-cell-size+ 2))
               (%make-surface-mesh-builder
                domain (1- (/ +mesh-cell-size+ 2)))
               builder)))
    (%call-with-boundary-policy
     boundary
     (lambda ()
       (let ((cells (%chain-cell-keys solid))
             (edge-candidates
               (%make-edge-candidates (chain-count solid)))
             (x-limit (world-domain-x-limit domain))
             (y-limit (world-domain-y-limit domain)))
         (multiple-value-bind (x0 x1 y0 y1) (%cell-key-box cells)
           (let ((packing (%make-spatial-edge-packing-for-box x0 x1 y0 y1)))
             (%enable-boundary-observations
              (list boundary-builder) packing (chain-count solid))
             (%emit-exposed-cell-faces boundary-builder field domain
                                       (%chain-sites solid)
                                       stock-function chamfer-stock-function
                                       edge-candidates)
             (loop for key across (%unique-edge-candidates edge-candidates)
                   do (%emit-edge-bands
                       boundary-builder field domain key
                       stock-function chamfer-stock-function))
             (%count-singular-vertex-stars builder field domain cells
                                           0 (1+ x-limit) 0 (1+ y-limit))
             (%emit-boundary-derived-fans builder field domain
                                          chamfer-stock-function
                                          (list boundary-builder)
                                          packing
                                          0 (1+ x-limit) 0 (1+ y-limit)
                                          nil)))
         (%finish-surface-mesh builder))))))

(defun %cell-key-box (cells)
  "The half-open horizontal cell box spanned by the packed keys in CELLS."
  (declare (type (simple-array (unsigned-byte 64) (*)) cells))
  (if (zerop (length cells))
      (values 0 1 0 1)
      (let ((x0 most-positive-fixnum) (x1 most-negative-fixnum)
            (y0 most-positive-fixnum) (y1 most-negative-fixnum))
        (loop for key across cells
              do (let ((x (%lattice-key-x key))
                       (y (%lattice-key-y key)))
                   (setf x0 (min x0 x) x1 (max x1 x)
                         y0 (min y0 y) y1 (max y1 y))))
        (values x0 (1+ x1) y0 (1+ y1)))))

(defun %emit-exposed-cell-faces
    (target field domain cells stock-function chamfer-stock-function
     &optional edge-candidates)
  "Emit every exposed face of the packed cell sites in CELLS into TARGET."
  (loop for cell across cells do
    (let ((cx (site-x cell)) (cy (site-y cell)) (cz (site-z cell)))
      (dotimes (axis-number 3)
        (dolist (side '(-1 1))
          (when (= 0 (%occupied-bit
                      field domain
                      (+ cx (if (= axis-number 0) side 0))
                      (+ cy (if (= axis-number 1) side 0))
                      (+ cz (if (= axis-number 2) side 0))))
            (when edge-candidates
              (%append-face-edge-keys edge-candidates cx cy cz
                                      axis-number side))
            (%emit-cell-face target field domain cell axis-number side
                             stock-function chamfer-stock-function))))))
  edge-candidates)

(defconstant +planar-coordinate-bit-count+ 16)

(declaim (inline %pack-planar-coordinate %planar-coordinate-u
                 %planar-coordinate-v))
(defun %pack-planar-coordinate (u v)
  (logior u (ash v +planar-coordinate-bit-count+)))

(defun %planar-coordinate-u (coordinate)
  (ldb (byte +planar-coordinate-bit-count+ 0) coordinate))

(defun %planar-coordinate-v (coordinate)
  (ldb (byte +planar-coordinate-bit-count+
             +planar-coordinate-bit-count+)
       coordinate))

(defun %planar-face-group< (left right)
  (loop for l in left
        for r in right
        when (/= l r) return (< l r)
        finally (return nil)))

(defun %emit-greedy-planar-faces
    (builder field domain cells stock-function)
  "Merge exposed cubical faces into maximal coplanar rectangles. #YGP21F

Faces merge only when their axis, orientation, plane, and stock agree. The
pass therefore changes neither position, normal, material, silhouette, nor
depth relative to the cubical boundary; it only dissolves interior edges."
  (let ((groups (make-hash-table :test #'equal)))
    (loop for cell across cells do
      (let ((coordinates (vector (site-x cell) (site-y cell) (site-z cell))))
        (dotimes (axis-number 3)
          (dolist (side '(-1 1))
            (let ((x (svref coordinates 0))
                  (y (svref coordinates 1))
                  (z (svref coordinates 2)))
              (case axis-number
                (0 (incf x side))
                (1 (incf y side))
                (t (incf z side)))
              (when (= 0 (%occupied-bit field domain x y z))
                (let* ((u (svref +axis-u+ axis-number))
                       (v (svref +axis-v+ axis-number))
                       (face
                         (if (minusp side)
                             (site-boundary-low
                              domain cell (index-axis axis-number))
                             (site-boundary-high
                              domain cell (index-axis axis-number))))
                       (stock (funcall stock-function face))
                       (plane (+ (svref coordinates axis-number)
                                 (if (plusp side) 1 0)))
                       (group-key (list axis-number side plane stock))
                       (group
                         (or (gethash group-key groups)
                             (setf (gethash group-key groups)
                                   (make-hash-table :test #'eql)))))
                  (setf (gethash
                         (%pack-planar-coordinate
                          (svref coordinates u) (svref coordinates v))
                         group)
                        t))))))))
    (let ((group-keys
            (sort (loop for key being the hash-keys of groups collect key)
                  #'%planar-face-group<)))
      (dolist (group-key group-keys)
        (destructuring-bind (axis-number side plane stock) group-key
          (let* ((u-axis (svref +axis-u+ axis-number))
                 (v-axis (svref +axis-v+ axis-number))
                 (group (gethash group-key groups))
                 (nx (if (= axis-number 0) side 0))
                 (ny (if (= axis-number 1) side 0))
                 (nz (if (= axis-number 2) side 0)))
            (loop while (plusp (hash-table-count group)) do
              (let* ((first
                       (loop for coordinate being the hash-keys of group
                             minimize coordinate))
                     (u0 (%planar-coordinate-u first))
                     (v0 (%planar-coordinate-v first))
                     (u1
                       (loop for u from u0
                             while (gethash (%pack-planar-coordinate u v0)
                                            group)
                             finally (return u)))
                     (v1
                       (loop for v from (1+ v0)
                             while (loop for u from u0 below u1
                                         always
                                         (gethash
                                          (%pack-planar-coordinate u v)
                                          group))
                             finally (return v))))
                (loop for v from v0 below v1 do
                  (loop for u from u0 below u1 do
                    (remhash (%pack-planar-coordinate u v) group)))
                (let ((base (vector 0 0 0))
                      (p0 (vector 0 0 0)) (p1 (vector 0 0 0))
                      (p2 (vector 0 0 0)) (p3 (vector 0 0 0)))
                  (setf (svref base axis-number) plane
                        (svref base u-axis) u0
                        (svref base v-axis) v0)
                  (flet ((set-point (point u v)
                           (setf (svref point axis-number)
                                 (* +mesh-cell-size+ plane)
                                 (svref point u-axis) (* +mesh-cell-size+ u)
                                 (svref point v-axis) (* +mesh-cell-size+ v))))
                    (set-point p0 u0 v0)
                    (set-point p1 u1 v0)
                    (set-point p2 u1 v1)
                    (set-point p3 u0 v1)
                    (%emit-quad builder :face
                                (svref base 0) (svref base 1) (svref base 2)
                                p0 p1 p2 p3 nx ny nz stock 0)))))))))))

(defun %make-planar-merged-chunk-mesh
    (domain bevel-width field cells stock-function)
  (let ((builder (%make-surface-mesh-builder domain bevel-width)))
    (%emit-greedy-planar-faces builder field domain cells stock-function)
    (%finish-surface-mesh builder)))

(defun mesh-chunk
    (chunk chunk-key
     &key (stock-function (constantly 0))
          (chamfer-stock-function (lambda (stocks) (first stocks)))
          (bevel-width +mesh-bevel-width+)
          planar-merge-p
          coplanar-merge-p)
  "Classify one chunk's solid CHUNK into the instance-stream ABI.

CHUNK holds exactly the cells of the chunk named by CHUNK-KEY.  Probes
leaving the chunk signal MISSING-CHUNK once per neighboring chunk -- bind a
handler that answers USE-CHUNK from a store, or TREAT-AS-AIR to fill in --
  and probes past the world's box signal OUTSIDE-DOMAIN; MESH-CHUNK sets no
  policy of its own.  The mesh ships only what this chunk owns: faces of its
own solid cells, bands whose edge anchors lie inside it, and fans at its
own lattice vertices.  Witness faces and bands are recomputed from the
  one-cell halo and scanned but never shipped, so seam fans close exactly as
  a whole-world mesh would close them.

When PLANAR-MERGE-P is true, emit the exact unbeveled cubical boundary as
greedily merged coplanar rectangles. This far-distance representation keeps
occupancy, normals, materials, silhouette, and chunk seams while omitting
bevel ornament and all geometrically redundant interior face edges.

When COPLANAR-MERGE-P is true, first construct the requested bevel surface,
then exactly dissolve its coplanar interior edges. This retains the full
surface and falls back group-by-group whenever a boundary is not simple."
  (check-type chunk chain)
  (check-type stock-function function)
  (check-type chamfer-stock-function function)
  (unless (and (integerp bevel-width)
               (<= 1 bevel-width (/ +mesh-cell-size+ 2)))
    (error "Bevel width ~S must be an integer between one and four ticks."
           bevel-width))
  (let* ((domain (chain-domain chunk))
         (grid-x (chunk-key-x chunk-key))
         (grid-y (chunk-key-y chunk-key))
         (x0 (chunk-origin-x chunk-key))
         (y0 (chunk-origin-y chunk-key))
         (x1 (min (+ x0 +chunk-size+) (world-domain-x-limit domain)))
         (y1 (min (+ y0 +chunk-size+) (world-domain-y-limit domain)))
         (field (%materialize-occupancy chunk x0 x1 y0 y1))
         (medial-p (= bevel-width (/ +mesh-cell-size+ 2)))
         (sheet-bevel (if medial-p (1- bevel-width) bevel-width))
         (builder (%make-surface-mesh-builder domain bevel-width))
         (ship-sheets (if medial-p
                          (%make-surface-mesh-builder domain sheet-bevel)
                          builder))
         (witness-sheets (%make-surface-mesh-builder domain sheet-bevel)))
    (loop for cell across (%chain-sites chunk) do
      (unless (= (site-chunk-key cell) chunk-key)
        (error "Cell ~S does not belong to chunk ~D." cell chunk-key)))
    (when planar-merge-p
      (return-from mesh-chunk
        (%make-planar-merged-chunk-mesh
         domain bevel-width field (%chain-sites chunk) stock-function)))
    ;; Owned sites are the half-open coordinate box of this chunk's grid
    ;; cell; anchors on the far seam belong to the next chunk over.  Owned
    ;; sites' cell stars reach exactly one cell below the origin per axis,
    ;; so only the low-side ring of neighbor cells witnesses the seams; the
    ;; high seams are the low sides of the next chunks over, which witness
    ;; them symmetrically.
    (let ((ox1 (if (>= (+ x0 +chunk-size+) (world-domain-x-limit domain))
                   (1+ (world-domain-x-limit domain))
                   (+ x0 +chunk-size+)))
          (oy1 (if (>= (+ y0 +chunk-size+) (world-domain-y-limit domain))
                   (1+ (world-domain-y-limit domain))
                   (+ y0 +chunk-size+)))
          (halo-list '()))
      (loop for (dx dy) in '((-1 0) (0 -1) (-1 -1)) do
        (let ((nx (+ grid-x dx)) (ny (+ grid-y dy)))
          (when (and (<= 0 nx) (<= 0 ny))
            (let ((resolution
                    (%field-chunk-resolution
                     field (%chunk-morton nx ny))))
              (case resolution
                (:air nil)
                (:solid
                 (error "A fully solid chunk resolution cannot feed ~
                         a mesh halo yet."))
                (t
                 (loop for key being the hash-keys of resolution
                       do (let ((x (%lattice-key-x key))
                                (y (%lattice-key-y key)))
                            (when (and (<= (1- x0) x) (< x x1)
                                       (<= (1- y0) y) (< y y1)
                                       (or (< x x0) (< y y0)))
                              (push key halo-list))))))))))
      (let* ((halo (sort (coerce halo-list
                                 '(simple-array (unsigned-byte 64) (*)))
                         #'<))
             (own-cells (%chain-cell-keys chunk))
             (edge-candidates
               (%make-edge-candidates (+ (chain-count chunk)
                                         (length halo))))
             (region-cells (concatenate
                            '(simple-array (unsigned-byte 64) (*))
                            own-cells halo))
             (halo-sites (make-array (length halo)
                                     :element-type '(unsigned-byte 64))))
        (let ((packing (%make-spatial-edge-packing-for-box
                        (1- x0) x1 (1- y0) y1)))
          (%enable-boundary-observations
           (list ship-sheets witness-sheets) packing
           (+ (chain-count chunk) (length halo)))
          (loop for key across halo
                for index from 0
                do (setf (aref halo-sites index)
                         (make-site domain
                                    (%lattice-key-x key)
                                    (%lattice-key-y key)
                                    (%lattice-key-z key)
                                    +cell-extent+ 1)))
          ;; Owned faces ship; halo faces are witnesses for the seam scan.
          (%emit-exposed-cell-faces ship-sheets field domain
                                    (%chain-sites chunk)
                                    stock-function chamfer-stock-function
                                    edge-candidates)
          (%emit-exposed-cell-faces witness-sheets field domain halo-sites
                                    stock-function chamfer-stock-function
                                    edge-candidates)
          (loop for key across (%unique-edge-candidates edge-candidates)
                do (let* ((anchor (ldb (byte +mesh-key-axis-shift+ 0) key))
                          (x (%lattice-key-x anchor))
                          (y (%lattice-key-y anchor)))
                     ;; High-seam anchors belong to the next chunk over and
                     ;; matter to none of this chunk's fans; skip them.
                     (when (and (<= (1- x0) x) (< x ox1)
                                (<= (1- y0) y) (< y oy1))
                       (%emit-edge-bands
                        (if (and (<= x0 x) (<= y0 y))
                            ship-sheets
                            witness-sheets)
                        field domain key
                        stock-function chamfer-stock-function))))
          (%count-singular-vertex-stars builder field domain region-cells
                                        x0 ox1 y0 oy1)
          (%emit-boundary-derived-fans builder field domain
                                       chamfer-stock-function
                                       (list ship-sheets witness-sheets)
                                       packing
                                       x0 ox1 y0 oy1 t)
          (let ((mesh (%finish-surface-mesh builder)))
            (if coplanar-merge-p
                (%coplanar-merged-surface-mesh mesh)
                mesh)))))))
