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
;;; - Sheet quads append only their four external directed edges to packed
;;;   UB64 streams.  A stable radix pass over the bounded chunk-relative edge
;;;   key cancels pairs; oversized whole-domain diagnostics retain the EQL hash
;;;   oracle.  Each surviving edge becomes one site-local endpoints/stock word.
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
  (companions nil :type list)
  ;; Sparse, render-owned semantic realizations.  Core meshing never reads or
  ;; interprets these objects; regional producers attach them only after the
  ;; final topology and variable-width transform are complete.
  (attachments nil :type list))

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
;;; Compiled chamfer material algebra

(defstruct (compiled-chamfer-algebra
             (:constructor %make-compiled-chamfer-algebra
                 (stock-summary-masks summary-stocks summary-count))
             (:copier nil))
  "Dense material-summary lanes borrowed by one meshing batch.

STOCK-SUMMARY-MASKS maps an already compiled face stock to its nonzero
contributor mask.  Bitwise OR is the associative, commutative, idempotent join;
SUMMARY-STOCKS lowers the joined mask back to one compiled appearance stock.
The semantic material compiler owns both arrays.  The mesher only borrows them
for a bounded scan and never constructs per-edge or per-fan stock lists."
  (stock-summary-masks #()
                       :type (simple-array (unsigned-byte 16) (*))
                       :read-only t)
  (summary-stocks #()
                  :type (simple-array (unsigned-byte 16) (*))
                  :read-only t)
  (summary-count 0 :type (integer 1 #xffff) :read-only t))

(defun make-compiled-chamfer-algebra
    (stock-summary-masks summary-stocks summary-count)
  "Bind checked dense material-summary lanes for chunk meshing."
  (check-type stock-summary-masks
              (simple-array (unsigned-byte 16) (*)))
  (check-type summary-stocks (simple-array (unsigned-byte 16) (*)))
  (check-type summary-count (integer 1 #xffff))
  (unless (> (length summary-stocks) summary-count)
    (error "Summary stock lane of length ~D does not contain mask ~D."
           (length summary-stocks) summary-count))
  (%make-compiled-chamfer-algebra
   stock-summary-masks summary-stocks summary-count))

(declaim (inline %compiled-chamfer-stock-summary
                 %compiled-chamfer-summary-stock))
(defun %compiled-chamfer-stock-summary (algebra stock)
  (declare (optimize (speed 3) (safety 1))
           (type compiled-chamfer-algebra algebra)
           (type (unsigned-byte 16) stock))
  (let ((masks (compiled-chamfer-algebra-stock-summary-masks algebra)))
    (unless (< stock (length masks))
      (error "Assembly stock ~D is outside this compiled chamfer algebra."
             stock))
    (let ((summary (aref masks stock)))
      (unless (<= 1 summary
                  (compiled-chamfer-algebra-summary-count algebra))
        (error "Assembly stock ~D has no summary in this chamfer algebra."
               stock))
      summary)))

(defun %compiled-chamfer-summary-stock (algebra summary)
  (declare (optimize (speed 3) (safety 1))
           (type compiled-chamfer-algebra algebra)
           (type (unsigned-byte 16) summary))
  (unless (<= 1 summary (compiled-chamfer-algebra-summary-count algebra))
    (error "Joined material summary ~D is outside this chamfer algebra."
           summary))
  (aref (compiled-chamfer-algebra-summary-stocks algebra) summary))

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

(defstruct (surface-mesh-workspace
             (:constructor %make-surface-mesh-workspace ()))
  "Reusable dense scratch owned by one sequential mesh cohort."
  (edge-records nil :type (or null (vector (unsigned-byte 64))))
  (radix-scratch nil :type (or null (vector (unsigned-byte 64))))
  (radix-counts nil
                :type (or null
                          (simple-array (unsigned-byte 32) (*))))
  (fan-records nil :type (or null (vector (unsigned-byte 64))))
  (fan-links nil :type (or null (vector (unsigned-byte 64))))
  (site-heads nil :type (or null (vector (unsigned-byte 32))))
  (touched-sites nil :type (or null (vector (unsigned-byte 32)))))

(defvar *surface-mesh-workspace* nil)

(defun %prepare-workspace-ub64-vector (vector minimum-capacity)
  (declare (type fixnum minimum-capacity))
  (if (or (null vector)
          (< (array-total-size vector) minimum-capacity))
      (make-array (max 16 minimum-capacity
                       (* 2 (if vector (array-total-size vector) 0)))
                  :element-type '(unsigned-byte 64)
                  :adjustable t :fill-pointer 0)
      (progn
        (setf (fill-pointer vector) 0)
        vector)))

(defun %prepare-workspace-ub32-vector (vector minimum-capacity)
  (declare (type fixnum minimum-capacity))
  (if (or (null vector)
          (< (array-total-size vector) minimum-capacity))
      (make-array (max 16 minimum-capacity
                       (* 2 (if vector (array-total-size vector) 0)))
                  :element-type '(unsigned-byte 32)
                  :adjustable t :fill-pointer 0
                  :initial-element 0)
      (progn
        (setf (fill-pointer vector) 0)
        vector)))

(defun %reset-surface-mesh-workspace (workspace)
  "Release all borrowed lanes, clearing only site heads actually touched."
  (when workspace
    (let ((heads (surface-mesh-workspace-site-heads workspace))
          (sites (surface-mesh-workspace-touched-sites workspace)))
      (when (and heads sites)
        (loop for site-index across sites
              do (setf (aref heads site-index) 0))))
    (dolist (vector
             (list
              (surface-mesh-workspace-edge-records workspace)
              (surface-mesh-workspace-radix-scratch workspace)
              (surface-mesh-workspace-fan-records workspace)
              (surface-mesh-workspace-fan-links workspace)
              (surface-mesh-workspace-site-heads workspace)
              (surface-mesh-workspace-touched-sites workspace)))
      (when vector
        (setf (fill-pointer vector) 0))))
  workspace)

(defmacro with-surface-mesh-workspace (() &body body)
  "Run BODY with reusable scratch for its sequential chunk-meshing calls."
  `(let ((*surface-mesh-workspace*
           (or *surface-mesh-workspace* (%make-surface-mesh-workspace))))
     (unwind-protect
          (progn ,@body)
       (%reset-surface-mesh-workspace *surface-mesh-workspace*))))

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
  ;; one shared packed observation stream, or the hash oracle for an oversized
  ;; whole-domain scan.  Builders without either retain the general replay
  ;; path used by mesh transformations elsewhere in this file.
  (boundary-packing nil)
  (boundary-edge-records nil
                         :type (or null (vector (unsigned-byte 64))))
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

(declaim (ftype function %observe-quad-boundary-edges))

(defun %emit-quad (builder kind base-x base-y base-z p0 p1 p2 p3
                   nx ny nz stock ambient-occlusion)
  "Emit one instance for the quad P0 P1 P2 P3 (global ticks, simple-vectors)."
  (when (or (surface-mesh-builder-boundary-edge-records builder)
            (surface-mesh-builder-boundary-observations builder))
    (%observe-quad-boundary-edges builder p0 p1 p2 p3 nx ny nz stock))
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

(defun %make-face-stock-resolver (stock-function source-stock-function)
  "Return the internal four-argument face-stock callback.

Legacy STOCK-FUNCTION sees only the oriented face.  SOURCE-STOCK-FUNCTION is
the provenance-aware form and receives FACE, occupied source CELL, normal
AXIS, and whether that cell lies :FORWARD or :BACKWARD of FACE."
  (if source-stock-function
      source-stock-function
      (lambda (face cell axis side)
        (declare (ignore cell axis side))
        (funcall stock-function face))))

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
         (stock
           (funcall stock-function face cell (index-axis axis-number)
                    (if (minusp side) :forward :backward)))
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
                         face cell))))
        (dolist (group (svref *edge-transition-group-table* states))
          (multiple-value-bind
                (left-axis left-sign left-other left-other-sign
                 left-face left-cell)
              (transition (first group))
            (multiple-value-bind
                  (right-axis right-sign right-other right-other-sign
                   right-face right-cell)
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
                                    (list (funcall
                                           stock-function left-face left-cell
                                           (index-axis left-axis)
                                           (if (minusp left-sign)
                                               :forward :backward))
                                          (funcall stock-function
                                                   right-face right-cell
                                                   (index-axis right-axis)
                                                   (if (minusp right-sign)
                                                       :forward
                                                       :backward)))))
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
(defconstant +boundary-edge-observation-direction-shift+ 12)
(defconstant +boundary-edge-observation-key-shift+ 13)
(defconstant +packed-boundary-anchor-limit+ (ash 1 27)
  "Maximum anchor count whose key, direction, and stock fit one UB64 word.")
(defconstant +boundary-edge-radix-bit-count+ 12)
(defconstant +boundary-edge-radix-size+
  (ash 1 +boundary-edge-radix-bit-count+))

(defparameter *boundary-observation-strategy* :auto
  "Boundary parity reducer: :AUTO, :PACKED, or the retained :HASH oracle.")

(defun %borrow-boundary-edge-records (minimum-capacity)
  (if *surface-mesh-workspace*
      (let ((vector
              (%prepare-workspace-ub64-vector
               (surface-mesh-workspace-edge-records
                *surface-mesh-workspace*)
               minimum-capacity)))
        (setf (surface-mesh-workspace-edge-records *surface-mesh-workspace*)
              vector)
        vector)
      (make-array minimum-capacity :element-type '(unsigned-byte 64)
                                   :adjustable t :fill-pointer 0)))

(defun %borrow-boundary-radix-scratch (minimum-capacity)
  (if *surface-mesh-workspace*
      (let ((vector
              (%prepare-workspace-ub64-vector
               (surface-mesh-workspace-radix-scratch
                *surface-mesh-workspace*)
               minimum-capacity)))
        (setf (surface-mesh-workspace-radix-scratch
               *surface-mesh-workspace*)
              vector)
        vector)
      (make-array minimum-capacity :element-type '(unsigned-byte 64))))

(defun %borrow-boundary-radix-counts ()
  (if *surface-mesh-workspace*
      (or (surface-mesh-workspace-radix-counts *surface-mesh-workspace*)
          (setf (surface-mesh-workspace-radix-counts
                 *surface-mesh-workspace*)
                (make-array +boundary-edge-radix-size+
                            :element-type '(unsigned-byte 32)
                            :initial-element 0)))
      (make-array +boundary-edge-radix-size+
                  :element-type '(unsigned-byte 32)
                  :initial-element 0)))

(defun %borrow-boundary-fan-records (minimum-capacity)
  (if *surface-mesh-workspace*
      (let ((vector
              (%prepare-workspace-ub64-vector
               (surface-mesh-workspace-fan-records *surface-mesh-workspace*)
               minimum-capacity)))
        (setf (surface-mesh-workspace-fan-records *surface-mesh-workspace*)
              vector)
        vector)
      (make-array minimum-capacity :element-type '(unsigned-byte 64)
                                   :adjustable t :fill-pointer 0)))

(defun %borrow-boundary-fan-links (minimum-capacity)
  (if *surface-mesh-workspace*
      (let ((vector
              (%prepare-workspace-ub64-vector
               (surface-mesh-workspace-fan-links *surface-mesh-workspace*)
               minimum-capacity)))
        (setf (surface-mesh-workspace-fan-links *surface-mesh-workspace*) vector)
        vector)
      (make-array minimum-capacity :element-type '(unsigned-byte 64)
                                   :adjustable t :fill-pointer 0)))

(defun %borrow-boundary-site-heads (minimum-capacity)
  (if *surface-mesh-workspace*
      (let ((vector
              (%prepare-workspace-ub32-vector
               (surface-mesh-workspace-site-heads *surface-mesh-workspace*)
               minimum-capacity)))
        (setf (surface-mesh-workspace-site-heads *surface-mesh-workspace*) vector)
        vector)
      (make-array minimum-capacity :element-type '(unsigned-byte 32)
                                   :adjustable t :fill-pointer 0
                                   :initial-element 0)))

(defun %borrow-boundary-touched-sites (minimum-capacity)
  (if *surface-mesh-workspace*
      (let ((vector
              (%prepare-workspace-ub32-vector
               (surface-mesh-workspace-touched-sites *surface-mesh-workspace*)
               minimum-capacity)))
        (setf (surface-mesh-workspace-touched-sites
               *surface-mesh-workspace*)
              vector)
        vector)
      (make-array minimum-capacity :element-type '(unsigned-byte 32)
                                   :adjustable t :fill-pointer 0)))

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
                 (origin-x origin-y y-span x-stride anchor-count)))
  "The anchor box one parity scan covers, and thus its fixnum key layout."
  (origin-x 0 :type fixnum :read-only t)
  (origin-y 0 :type fixnum :read-only t)
  (y-span 1 :type fixnum :read-only t)
  (x-stride +spatial-edge-anchor-z-span+ :type fixnum :read-only t)
  (anchor-count +spatial-edge-anchor-z-span+
                :type (integer 1 *) :read-only t))

(defun %make-spatial-edge-packing-for-box (x0 x1 y0 y1)
  "Pack anchors of the cell box [X0, X1) x [Y0, Y1).

Instance bases lie inside the box and template offsets reach at most one
eighth-cell anchor beyond it, so the packed anchor box is widened by two."
  (let* ((origin-x (- x0 2))
         (origin-y (- y0 2))
         (x-span (+ (- x1 x0) 4))
         (y-span (+ (- y1 y0) 4))
         (anchor-count (* x-span y-span
                          +spatial-edge-anchor-z-span+)))
    (unless (typep (ash anchor-count 24)
                   'fixnum)
      (error "A solid spanning ~Dx~D cells is too wide for one spatial-edge ~
              scan; mesh it by chunks."
             x-span y-span))
    (%make-spatial-edge-packing
     origin-x origin-y y-span
     (* y-span +spatial-edge-anchor-z-span+)
     anchor-count)))

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

(defun %scan-builders-open-boundary-table (builders packing)
  "Replay BUILDERS' triangle streams into the retained boundary hash oracle."
  (declare (optimize (speed 3) (safety 1)))
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

(declaim (inline %packed-boundary-observations-supported-p
                 %pack-boundary-edge-observation
                 %boundary-edge-observation-key
                 %boundary-edge-observation-left
                 %boundary-edge-observation-right
                 %boundary-edge-observation-stock))

(defun %packed-boundary-observations-supported-p (packing)
  (<= (spatial-edge-packing-anchor-count packing)
      +packed-boundary-anchor-limit+))

(defun %pack-boundary-edge-observation (key left12 right12 stock)
  "Pack one spatial key, directed orientation, and stock into a UB64 word."
  (declare (optimize (speed 3) (safety 1))
           (type fixnum key left12 right12 stock))
  (the (unsigned-byte 64)
       (logior
        (ash key +boundary-edge-observation-key-shift+)
        (if (> left12 right12)
            (ash 1 +boundary-edge-observation-direction-shift+)
            0)
        stock)))

(defun %boundary-edge-observation-key (record)
  (declare (type (unsigned-byte 64) record))
  (ash record (- +boundary-edge-observation-key-shift+)))

(defun %boundary-edge-observation-left (record)
  (declare (type (unsigned-byte 64) record))
  (let ((key (%boundary-edge-observation-key record)))
    (if (logbitp +boundary-edge-observation-direction-shift+ record)
        (ldb (byte 12 0) key)
        (ldb (byte 12 12) key))))

(defun %boundary-edge-observation-right (record)
  (declare (type (unsigned-byte 64) record))
  (let ((key (%boundary-edge-observation-key record)))
    (if (logbitp +boundary-edge-observation-direction-shift+ record)
        (ldb (byte 12 12) key)
        (ldb (byte 12 0) key))))

(defun %boundary-edge-observation-stock (record)
  (declare (type (unsigned-byte 64) record))
  (ldb (byte +fan-record-stock-bit-count+ 0) record))

(defun %radix-sort-packed-boundary-observations (records)
  "Stably sort RECORDS by their bounded spatial key without boxed comparisons."
  (declare (optimize (speed 3) (safety 1))
           (type (vector (unsigned-byte 64)) records))
  (let ((count (length records)))
    (declare (type fixnum count))
    (when (> count 1)
      (let ((maximum-key 0))
        (declare (type fixnum maximum-key))
        (dotimes (index count)
          (setf maximum-key
                (max maximum-key
                     (%boundary-edge-observation-key (aref records index)))))
        (let ((scratch (%borrow-boundary-radix-scratch count))
              (counts (%borrow-boundary-radix-counts))
              (source records)
              (target nil))
          (declare (type (vector (unsigned-byte 64)) scratch source)
                   (type (or null (vector (unsigned-byte 64))) target)
                   (type (simple-array (unsigned-byte 32) (*)) counts))
          ;; Workspace scratch has a fill pointer so it can be reset cheaply.
          ;; Give sequence operations the active radix extent as well as the
          ;; backing array extent used by AREF below.
          (when (array-has-fill-pointer-p scratch)
            (setf (fill-pointer scratch) count))
          (setf target scratch)
          (loop for shift fixnum from 0 by +boundary-edge-radix-bit-count+
                while (< shift (integer-length maximum-key))
                do
            (fill counts 0)
            (dotimes (index count)
              (let ((digit
                      (ldb (byte +boundary-edge-radix-bit-count+
                                 (+ +boundary-edge-observation-key-shift+
                                    shift))
                           (aref source index))))
                (declare (type (integer 0 #.(1- (ash 1 12))) digit))
                (incf (aref counts digit))))
            (let ((position 0))
              (declare (type (unsigned-byte 32) position))
              (dotimes (digit +boundary-edge-radix-size+)
                (let ((frequency (aref counts digit)))
                  (declare (type (unsigned-byte 32) frequency))
                  (setf (aref counts digit) position)
                  (incf position frequency))))
            (dotimes (index count)
              (let* ((record (aref source index))
                     (digit
                       (ldb (byte +boundary-edge-radix-bit-count+
                                  (+ +boundary-edge-observation-key-shift+
                                     shift))
                            record))
                     (position (aref counts digit)))
                (declare (type (unsigned-byte 64) record)
                         (type (integer 0 #.(1- (ash 1 12))) digit)
                         (type (unsigned-byte 32) position))
                (setf (aref target position) record
                      (aref counts digit) (1+ position))))
            (rotatef source target))
          (unless (eq source records)
            (replace records source :end1 count :end2 count))))))
  records)

(defun %reduce-packed-boundary-observations (records)
  "Sort packed observations and compact their singleton open edges in place."
  (declare (optimize (speed 3) (safety 1))
           (type (vector (unsigned-byte 64)) records))
  (%radix-sort-packed-boundary-observations records)
  (let ((read 0)
        (write 0)
        (count (length records)))
    (declare (type fixnum read write count))
    (loop while (< read count) do
      (let* ((record (aref records read))
             (key (%boundary-edge-observation-key record))
             (next (1+ read)))
        (declare (type (unsigned-byte 64) record)
                 (type fixnum key next))
        (loop while (and (< next count)
                         (= key (%boundary-edge-observation-key
                                 (aref records next))))
              do (incf next))
        (case (- next read)
          (1
           (setf (aref records write) record)
           (incf write))
          (2 nil)
          (t
           (error "Face and edge streams meet ~D times at ~S."
                  (- next read) key)))
        (setf read next)))
    (setf (fill-pointer records) write)
    records))

(defun %builder-open-boundary-source (builders packing)
  "Return packed open records, or the retained hash boundary representation."
  (let* ((first (first builders))
         (records (surface-mesh-builder-boundary-edge-records first))
         (observations
           (surface-mesh-builder-boundary-observations first)))
    (labels ((shared-p (builder)
               (and (eq packing
                        (surface-mesh-builder-boundary-packing builder))
                    (eq records
                        (surface-mesh-builder-boundary-edge-records builder))
                    (eq observations
                        (surface-mesh-builder-boundary-observations builder)))))
      (cond
        (records
         (unless (every #'shared-p builders)
           (error "Sheet builders do not share one packed boundary stream."))
         (values (%reduce-packed-boundary-observations records) nil))
        (observations
         (unless (every #'shared-p builders)
           (error "Sheet builders do not share one boundary observation table."))
         (values nil observations))
        (t
         (values nil (%scan-builders-open-boundary-table builders packing)))))))

(defun %enable-boundary-observations (builders packing estimated-cells)
  "Make BUILDERS share the selected boundary reducer during sheet emission."
  (let* ((strategy
           (ecase *boundary-observation-strategy*
             (:auto
              (if (%packed-boundary-observations-supported-p packing)
                  :packed
                  :hash))
             (:packed
              (unless (%packed-boundary-observations-supported-p packing)
                (error "Boundary box has ~D anchors; packed observations support at most ~D."
                       (spatial-edge-packing-anchor-count packing)
                       +packed-boundary-anchor-limit+))
              :packed)
             (:hash :hash)))
         (records
           (when (eq strategy :packed)
             (%borrow-boundary-edge-records
              (max 4096 (* 8 estimated-cells)))))
         (observations
           (when (eq strategy :hash)
             (make-hash-table :test #'eql
                              :size (max 4096 (* 4 estimated-cells))))))
    (dolist (builder builders strategy)
      (setf (surface-mesh-builder-boundary-packing builder) packing
            (surface-mesh-builder-boundary-edge-records builder) records
            (surface-mesh-builder-boundary-observations builder)
            observations))))

(defun %observe-quad-boundary-edges (builder p0 p1 p2 p3 nx ny nz stock)
  "Parity-count one sheet quad's four perimeter edges.

The two emitted triangles share a construction diagonal.  Observing the quad
before triangulation avoids packing, hashing, and cancelling that edge."
  (declare (optimize (speed 3) (safety 1))
           (type simple-vector p0 p1 p2 p3)
           (type fixnum nx ny nz stock))
  (let* ((packing (surface-mesh-builder-boundary-packing builder))
         (records (surface-mesh-builder-boundary-edge-records builder))
         (observations
           (surface-mesh-builder-boundary-observations builder))
         (ux (- (the fixnum (svref p1 0)) (the fixnum (svref p0 0))))
         (uy (- (the fixnum (svref p1 1)) (the fixnum (svref p0 1))))
         (uz (- (the fixnum (svref p1 2)) (the fixnum (svref p0 2))))
         (vx (- (the fixnum (svref p2 0)) (the fixnum (svref p0 0))))
         (vy (- (the fixnum (svref p2 1)) (the fixnum (svref p0 1))))
         (vz (- (the fixnum (svref p2 2)) (the fixnum (svref p0 2))))
         (orientation
           (+ (* (- (* uy vz) (* uz vy)) nx)
              (* (- (* uz vx) (* ux vz)) ny)
              (* (- (* ux vy) (* uy vx)) nz))))
    (declare (type fixnum ux uy uz vx vy vz orientation))
    (flet ((observe (left right)
             (declare (type simple-vector left right))
             (multiple-value-bind (key left12 right12)
                 (%pack-spatial-edge
                  packing
                  (svref left 0) (svref left 1) (svref left 2)
                  (svref right 0) (svref right 1) (svref right 2))
               (if records
                   (vector-push-extend
                    (%pack-boundary-edge-observation key left12 right12 stock)
                    records)
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
                                stock))))))))
      (if (minusp orientation)
          (progn
            (observe p0 p3)
            (observe p3 p2)
            (observe p2 p1)
            (observe p1 p0))
          (progn
            (observe p0 p1)
            (observe p1 p2)
            (observe p2 p3)
            (observe p3 p0))))))

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

(declaim (inline %nearest-edge-site-coordinate))
(defun %nearest-edge-site-coordinate (left right)
  "Nearest lattice site to an edge midpoint, with ROUND's ties-to-even rule."
  (declare (optimize (speed 3) (safety 1))
           (type fixnum left right))
  ;; Site spacing is eight ticks, so rounding the midpoint to a site is
  ;; ROUND((LEFT + RIGHT) / 16).  Arithmetic shift and LOGAND give the floor
  ;; and non-negative remainder for negative sums too.
  (let* ((sum (the fixnum (+ left right)))
         (lower (the fixnum (ash sum -4)))
         (remainder (the (integer 0 15) (logand sum 15))))
    (declare (type fixnum sum lower))
    (the fixnum
         (+ lower
            (if (or (> remainder 8)
                    (and (= remainder 8) (oddp lower)))
                1
                0)))))

(defconstant +boundary-site-z-span+ 257)
(defconstant +dense-boundary-site-limit+ (* 2 1024 1024)
  "Maximum direct site-head entries for one boundary reduction.")

(defstruct (boundary-site-groups
             (:constructor %make-boundary-site-groups
                 (table heads links records sites
                  origin-x origin-y origin-z y-span z-span)))
  "Open fan records grouped sparsely, or by a chunk-local direct site index."
  (table nil :type (or null hash-table) :read-only t)
  (heads nil
         :type (or null (vector (unsigned-byte 32)))
         :read-only t)
  (links nil :type (or null (vector (unsigned-byte 64))) :read-only t)
  (records nil :type (or null (vector (unsigned-byte 64))) :read-only t)
  (sites nil :type (or null (vector (unsigned-byte 32))) :read-only t)
  (origin-x 0 :type fixnum :read-only t)
  (origin-y 0 :type fixnum :read-only t)
  (origin-z 0 :type fixnum :read-only t)
  (y-span 1 :type fixnum :read-only t)
  (z-span 1 :type fixnum :read-only t))

(defun %attribute-open-edges-to-sites
    (builders packing bevel-width ox0 ox1 oy0 oy1 drop-nonlocal-p)
  "Group the open boundary's directed edges by owning lattice vertex.

Returns BOUNDARY-SITE-GROUPS.  Chunk-sized ownership boxes use an unboxed
direct site-head array; larger one-pass boxes retain a sparse table.  Records'
12-bit points are site-local with the fan bias.  An open edge not contained in
any vertex's bevel domain is an invariant violation for a whole solid; for a
chunk's witness scan it is the witness truncation boundary, provably outside
every owned site's bevel domain, and DROP-NONLOCAL-P discards it."
  (declare (optimize (speed 3) (safety 1))
           (type fixnum bevel-width ox0 ox1 oy0 oy1))
  (multiple-value-bind (edge-observations observations)
      (%builder-open-boundary-source builders packing)
    (let* ((x-span (the fixnum (- ox1 ox0)))
           (y-span (the fixnum (- oy1 oy0)))
           (open-edge-estimate
             (if edge-observations
                 (length edge-observations)
                 (ceiling (* 3 (hash-table-count observations)) 5)))
           (record-capacity (max 16 open-edge-estimate))
           (records
             (%borrow-boundary-fan-records record-capacity))
           ;; This lane first holds full-box site indices.  Dense grouping then
           ;; overwrites each entry with the preceding record link.
           (site-lane
             (%borrow-boundary-fan-links record-capacity))
           (minimum-z +boundary-site-z-span+)
           (maximum-z -1))
      (declare (type fixnum x-span y-span open-edge-estimate record-capacity
                     minimum-z maximum-z))
      (unless (and (plusp x-span) (plusp y-span))
        (error "Empty boundary ownership box [~D,~D) x [~D,~D)."
               ox0 ox1 oy0 oy1))
      (labels
          ((attribute (key left12 right12 stock)
             (declare (type fixnum key)
                      (type (unsigned-byte 12) left12 right12 stock))
             (block attribute
               (let* ((anchor-x (%spatial-edge-key-anchor-x packing key))
                      (anchor-y (%spatial-edge-key-anchor-y packing key))
                      (anchor-z (%spatial-edge-key-anchor-z key))
                      (lx (+ (* 8 anchor-x) (ldb (byte 4 8) left12)))
                      (ly (+ (* 8 anchor-y) (ldb (byte 4 4) left12)))
                      (lz (+ (* 8 anchor-z) (ldb (byte 4 0) left12)))
                      (rx (+ (* 8 anchor-x) (ldb (byte 4 8) right12)))
                      (ry (+ (* 8 anchor-y) (ldb (byte 4 4) right12)))
                      (rz (+ (* 8 anchor-z) (ldb (byte 4 0) right12))))
                 (declare (type fixnum anchor-x anchor-y anchor-z
                                lx ly lz rx ry rz))
                 (flet ((site-coordinate (l r)
                          (declare (type fixnum l r))
                          ;; Find the lattice vertex whose bevel domain contains
                          ;; the edge, exactly as the exact-rational original did.
                          (let ((coordinate
                                  (%nearest-edge-site-coordinate l r)))
                            (declare (type fixnum coordinate))
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
                   (let ((site-x (site-coordinate lx rx))
                         (site-y (site-coordinate ly ry))
                         (site-z (site-coordinate lz rz)))
                     (declare (type fixnum site-x site-y site-z))
                     ;; Neighbor-owned witness sites never contribute to this
                     ;; chunk.  Discard them before either grouping path.
                     (when (and (<= ox0 site-x) (< site-x ox1)
                                (<= oy0 site-y) (< site-y oy1))
                       (unless (<= 0 site-z (1- +boundary-site-z-span+))
                         (error "Boundary edge owns out-of-domain Z site ~D."
                                site-z))
                       (let ((record
                               (logior
                                (ash (%fan-point (- lx (* 8 site-x))
                                                 (- ly (* 8 site-y))
                                                 (- lz (* 8 site-z)))
                                     +fan-record-left-shift+)
                                (ash (%fan-point (- rx (* 8 site-x))
                                                 (- ry (* 8 site-y))
                                                 (- rz (* 8 site-z)))
                                     +fan-record-right-shift+)
                                stock))
                             (site-index
                               (+ site-z
                                  (* +boundary-site-z-span+
                                     (+ (- site-y oy0)
                                        (* y-span (- site-x ox0)))))))
                         (declare (type (unsigned-byte 64) record site-index))
                         (vector-push-extend record records)
                         (vector-push-extend site-index site-lane)
                         (setf minimum-z (min minimum-z site-z)
                               maximum-z (max maximum-z site-z))))))))))
        (if edge-observations
            (loop for observation across edge-observations
                  do (attribute
                      (%boundary-edge-observation-key observation)
                      (%boundary-edge-observation-left observation)
                      (%boundary-edge-observation-right observation)
                      (%boundary-edge-observation-stock observation)))
            (maphash
             (lambda (key value)
               (declare (type fixnum key value))
               (when (= 1 (ash value (- +boundary-observation-count-shift+)))
                 (attribute
                  key
                  (ldb (byte 12 +fan-record-left-shift+) value)
                  (ldb (byte 12 +fan-record-right-shift+) value)
                  (ldb (byte +fan-record-stock-bit-count+ 0) value))))
             observations)))
      (let* ((record-count (fill-pointer records))
             (origin-z (if (plusp record-count) minimum-z 0))
             (z-span (if (plusp record-count)
                         (1+ (- maximum-z minimum-z))
                         1))
             (dense-site-count (* x-span y-span z-span)))
        (declare (type fixnum record-count origin-z z-span dense-site-count))
        (if (<= dense-site-count +dense-boundary-site-limit+)
            (let ((heads
                    (%borrow-boundary-site-heads dense-site-count))
                  (sites
                    (%borrow-boundary-touched-sites
                     (max 16 (min dense-site-count
                                  (ceiling record-capacity 4))))))
              (dotimes (record-index record-count)
                (let* ((full-index (aref site-lane record-index))
                       (site-z (mod full-index +boundary-site-z-span+))
                       (horizontal-index
                         (truncate full-index +boundary-site-z-span+))
                       (site-index
                         (+ (- site-z origin-z)
                            (* z-span horizontal-index)))
                       (head (aref heads site-index)))
                  (declare (type fixnum site-z horizontal-index site-index)
                           (type (unsigned-byte 32) head))
                  (when (zerop head)
                    (vector-push-extend site-index sites))
                  (setf (aref site-lane record-index) head
                        (aref heads site-index) (1+ record-index))))
              (%make-boundary-site-groups
               nil heads site-lane records sites
               ox0 oy0 origin-z y-span z-span))
            (let ((by-site
                    (make-hash-table
                     :test #'eql :size (max 16 (ceiling record-count 4)))))
              (dotimes (record-index record-count)
                (let* ((full-index (aref site-lane record-index))
                       (site-z (mod full-index +boundary-site-z-span+))
                       (horizontal-index
                         (truncate full-index +boundary-site-z-span+))
                       (site-y (+ oy0 (mod horizontal-index y-span)))
                       (site-x (+ ox0 (truncate horizontal-index y-span)))
                       (key (%lattice-key site-x site-y site-z))
                       (site-records
                         (or (gethash key by-site)
                             (setf (gethash key by-site)
                                   (make-array
                                    8 :element-type '(unsigned-byte 64)
                                      :adjustable t :fill-pointer 0)))))
                  (declare (type fixnum site-z horizontal-index
                                 site-y site-x key))
                  (vector-push-extend (aref records record-index)
                                      site-records)))
              (%make-boundary-site-groups
               by-site nil nil nil nil ox0 oy0 0 y-span 1)))))))

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

(defun %emit-boundary-site-fans
    (builder field domain chamfer-stock-function source-width target-width
     site-x site-y site-z records)
  "Close one lattice site's consistently directed open edge cycles."
  (let* ((star-mask (%star-mask-at field domain site-x site-y site-z))
         (records (sort records #'> :key #'%fan-record-undirected)))
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
                builder site-x site-y site-z cycle stock star-mask)))))))

(defun %emit-boundary-derived-fans
    (builder field domain chamfer-stock-function sheet-builders packing
     ox0 ox1 oy0 oy1 drop-nonlocal-p)
  "Close the SHEET-BUILDERS' open loops with site-local templates in BUILDER.

Only lattice sites inside the half-open [OX0, OX1) x [OY0, OY1) box get
fans; a chunk's neighbor owns the rest and closes them from its own
  witness scan."
  (let* ((source-width (surface-mesh-builder-bevel-width
                        (first sheet-builders)))
         (target-width (surface-mesh-builder-bevel-width builder)))
    (unwind-protect
         (let* ((groups (%attribute-open-edges-to-sites
                         sheet-builders packing source-width
                         ox0 ox1 oy0 oy1 drop-nonlocal-p))
                (by-site (boundary-site-groups-table groups)))
           (if by-site
               (let ((site-keys (make-array (hash-table-count by-site)
                                            :element-type '(unsigned-byte 64)))
                     (write 0))
                 (loop for key being the hash-keys of by-site
                       do (setf (aref site-keys write) key)
                          (incf write))
                 (sort site-keys #'<)
                 (loop for key across site-keys
                       do (%emit-boundary-site-fans
                           builder field domain chamfer-stock-function
                           source-width target-width
                           (%lattice-key-x key)
                           (%lattice-key-y key)
                           (%lattice-key-z key)
                           (gethash key by-site))))
               (let ((heads (boundary-site-groups-heads groups))
                     (links (boundary-site-groups-links groups))
                     (records (boundary-site-groups-records groups))
                     (sites (boundary-site-groups-sites groups))
                     (origin-x (boundary-site-groups-origin-x groups))
                     (origin-y (boundary-site-groups-origin-y groups))
                     (origin-z (boundary-site-groups-origin-z groups))
                     (y-span (boundary-site-groups-y-span groups))
                     (z-span (boundary-site-groups-z-span groups))
                     (site-records
                       (make-array 16 :element-type '(unsigned-byte 64)
                                      :adjustable t :fill-pointer 0)))
                 (sort sites #'<)
                 (loop for site-index across sites do
                   (let* ((site-z (+ origin-z (mod site-index z-span)))
                          (horizontal-index (truncate site-index z-span))
                          (site-y (+ origin-y (mod horizontal-index y-span)))
                          (site-x (+ origin-x (truncate horizontal-index y-span)))
                          (head (aref heads site-index)))
                     (setf (fill-pointer site-records) 0)
                     (loop while (plusp head) do
                       (let ((record-index (1- head)))
                         (vector-push-extend
                          (aref records record-index) site-records)
                         (setf head (aref links record-index))))
                     (%emit-boundary-site-fans
                      builder field domain chamfer-stock-function
                      source-width target-width site-x site-y site-z
                      site-records))))))
      (%reset-surface-mesh-workspace *surface-mesh-workspace*))))

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
                source-stock-function
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
STOCK-FUNCTION is called with an oriented boundary face.  When supplied,
SOURCE-STOCK-FUNCTION supersedes it and receives FACE, the occupied source
CELL, its normal AXIS, and :FORWARD or :BACKWARD incidence.  This provenance
form lets a captured or streaming occupancy view classify the exact cell that
caused emission without reverse-probing another solid.  CHAMFER-STOCK-FUNCTION
receives the face stocks incident to one edge-owned collar, bevel, or
lattice-site closure.  It must return one stock for that entire chamfer."
  (check-type solid chain)
  (check-type stock-function function)
  (check-type source-stock-function (or null function))
  (check-type chamfer-stock-function function)
  (unless (and (integerp bevel-width)
               (<= 1 bevel-width (/ +mesh-cell-size+ 2)))
    (error "Bevel width ~S must be an integer between one and four ticks."
           bevel-width))
  (check-type boundary (member :air :signal))
  (let* ((domain (chain-domain solid))
         (stock-resolver
           (%make-face-stock-resolver stock-function source-stock-function))
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
                                       stock-resolver chamfer-stock-function
                                       edge-candidates)
             (loop for key across (%unique-edge-candidates edge-candidates)
                   do (%emit-edge-bands
                       boundary-builder field domain key
                       stock-resolver chamfer-stock-function))
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

;;; ---------------------------------------------------------------------------
;;; Width-one local topology compiler and bounded chunk kernel
;;;
;;; MAKE-SURFACE-MESH above remains the deliberately surface-proportional
;;; oracle.  The tables below are compiled once from that oracle.  Production
;;; chunk meshing at width one scans owned lattice vertices, reads one eight-bit
;;; star, and emits the face, edge, and vertex records anchored there without
;;; candidate discovery, boundary reduction, site grouping, or cycle walking.

(defstruct (width-one-template-descriptor
             (:constructor %make-width-one-template-descriptor
                 (vertices contributor-mask ambient-star-p))
             (:copier nil))
  (vertices #() :type (simple-array fixnum (*)) :read-only t)
  (contributor-mask 0 :type (unsigned-byte 12) :read-only t)
  (ambient-star-p nil :type boolean :read-only t))

(defstruct (width-one-edge-pattern
             (:constructor %make-width-one-edge-pattern
                 (transitions descriptors))
             (:copier nil))
  ;; Four entries in radial transition order.  -1 is not a boundary;
  ;; otherwise bits 0..1 name the occupied quadrant, bit 2 selects V rather
  ;; than U as the normal axis, and bit 3 says the normal sign is positive.
  (transitions #() :type simple-vector :read-only t)
  ;; Three complete oriented descriptor vectors, indexed by edge axis.
  (descriptors #() :type simple-vector :read-only t))

(defstruct (width-one-vertex-pattern
             (:constructor %make-width-one-vertex-pattern
                 (contributors descriptors))
             (:copier nil))
  ;; Twelve cube-edge entries parallel to *STAR-CUBE-EDGES*.  -1 is not a
  ;; boundary; otherwise bits 0..2 name the occupied sample, bits 3..4 the
  ;; face-normal axis, and bit 5 says the normal sign is positive.
  (contributors #() :type simple-vector :read-only t)
  (descriptors #() :type simple-vector :read-only t))

(defconstant +width-one-table-site+ 8)

(defun %width-one-local-star-solid (domain mask)
  (let ((builder (make-chain-builder domain :initial-capacity 8)))
    (dotimes (sample 8)
      (when (logbitp sample mask)
        (chain-builder-add-site
         builder
         (make-site
          domain
          (+ +width-one-table-site+
             (if (logbitp 0 sample) 0 -1))
          (+ +width-one-table-site+
             (if (logbitp 1 sample) 0 -1))
          (+ +width-one-table-site+
             (if (logbitp 2 sample) 0 -1))
          +cell-extent+ 1))))
    (finish-chain-builder builder)))

(defun %width-one-cell-sample-at-table-site (cell)
  (let ((sample 0))
    (dotimes (axis-number 3 sample)
      (let ((coordinate
              (ecase axis-number
                (0 (site-x cell)) (1 (site-y cell)) (2 (site-z cell)))))
        (cond ((= coordinate +width-one-table-site+)
               (setf sample (logior sample (ash 1 axis-number))))
              ((/= coordinate (1- +width-one-table-site+))
               (return-from %width-one-cell-sample-at-table-site nil)))))))

(defun %width-one-face-plane-coordinate (cell axis-number normal-sign)
  (+ (ecase axis-number
       (0 (site-x cell)) (1 (site-y cell)) (2 (site-z cell)))
     (if (plusp normal-sign) 1 0)))

(defun %width-one-vertex-contributor-stock (cell axis side)
  "Give one central-site incident face a distinct cube-edge bit."
  (let* ((axis-number (axis-index axis))
         (normal-sign (if (eq side :forward) -1 1))
         (sample (%width-one-cell-sample-at-table-site cell)))
    (if (and sample
             (= +width-one-table-site+
                (%width-one-face-plane-coordinate
                 cell axis-number normal-sign)))
        (let* ((edge (%cube-edge-key
                      sample (logxor sample (ash 1 axis-number))))
               (index (position edge *star-cube-edges* :test #'=)))
          (unless index
            (error "Central star face has no cube-edge contributor."))
          (ash 1 index))
        0)))

(defun %width-one-quadrant-index (u-coordinate v-coordinate)
  (cond ((and (= u-coordinate (1- +width-one-table-site+))
              (= v-coordinate (1- +width-one-table-site+))) 0)
        ((and (= u-coordinate +width-one-table-site+)
              (= v-coordinate (1- +width-one-table-site+))) 1)
        ((and (= u-coordinate +width-one-table-site+)
              (= v-coordinate +width-one-table-site+)) 2)
        ((and (= u-coordinate (1- +width-one-table-site+))
              (= v-coordinate +width-one-table-site+)) 3)
        (t nil)))

(defun %width-one-edge-contributor-stock
    (cell axis side edge-axis-number)
  "Give one face incident to the selected positive edge its radial bit."
  (let* ((axis-number (axis-index axis))
         (normal-sign (if (eq side :forward) -1 1))
         (u (svref +axis-u+ edge-axis-number))
         (v (svref +axis-v+ edge-axis-number))
         (coordinates (vector (site-x cell) (site-y cell) (site-z cell))))
    (declare (dynamic-extent coordinates))
    (if (or (= axis-number edge-axis-number)
            (/= (aref coordinates edge-axis-number)
                +width-one-table-site+)
            (/= +width-one-table-site+
                (%width-one-face-plane-coordinate
                 cell axis-number normal-sign)))
        0
        (let* ((occupied
                 (%width-one-quadrant-index
                  (aref coordinates u) (aref coordinates v)))
               (empty-u (+ (aref coordinates u)
                           (if (= axis-number u) normal-sign 0)))
               (empty-v (+ (aref coordinates v)
                           (if (= axis-number v) normal-sign 0)))
               (empty (%width-one-quadrant-index empty-u empty-v)))
          (if (and occupied empty)
              (let ((transition
                      (loop for index below 4
                            for next = (mod (1+ index) 4)
                            when (or (and (= index occupied) (= next empty))
                                     (and (= index empty) (= next occupied)))
                              return index)))
                (unless transition
                  (error "Canonical edge face has no radial transition."))
                (ash 1 transition))
              0)))))

(defun %width-one-or-contributors (stocks)
  (reduce #'logior stocks))

(declaim (inline %packed-template-coordinate %packed-template-attributes))
(defun %packed-template-coordinate (vertex axis-number)
  (- (ldb (byte +mesh-template-coordinate-bit-count+
                (* axis-number +mesh-template-coordinate-bit-count+))
          vertex)
     +mesh-template-coordinate-bias+))

(defun %packed-template-attributes (vertex)
  (ash vertex (- +mesh-vertex-attribute-shift+)))

(defun %finished-instance-template-vertices (mesh meta)
  (let* ((template-id (ldb (byte 16 0) meta))
         (ranges (surface-mesh-template-ranges mesh))
         (start (aref ranges (* 2 template-id)))
         (count (aref ranges (1+ (* 2 template-id))))
         (words (surface-mesh-template-vertex-words mesh))
         (vertices (make-array count :element-type 'fixnum)))
    (dotimes (index count vertices)
      (let ((offset (* +mesh-template-vertex-word-count+ (+ start index))))
        (setf (aref vertices index)
              (%pack-template-vertex
               (- (aref words offset) +mesh-template-coordinate-bias+)
               (- (aref words (+ offset 1)) +mesh-template-coordinate-bias+)
               (- (aref words (+ offset 2)) +mesh-template-coordinate-bias+)
               (aref words (+ offset 3))))))))

(defun %width-one-edge-descriptors-for-state (domain state axis-number)
  (let ((mask 0))
    (let ((u (svref +axis-u+ axis-number))
          (v (svref +axis-v+ axis-number)))
      (dotimes (quadrant 4)
        (when (logbitp quadrant state)
          (let ((sample
                  (logior
                   (ash 1 axis-number)
                   (if (plusp (svref +quadrant-u+ quadrant))
                       (ash 1 u) 0)
                   (if (plusp (svref +quadrant-v+ quadrant))
                       (ash 1 v) 0))))
            (setf mask (logior mask (ash 1 sample)))))))
    (let* ((u (svref +axis-u+ axis-number))
           (v (svref +axis-v+ axis-number))
           (solid (%width-one-local-star-solid domain mask))
           (mesh
             (make-surface-mesh
              solid :bevel-width 1
              :source-stock-function
              (lambda (face cell axis side)
                (declare (ignore face))
                (%width-one-edge-contributor-stock
                 cell axis side axis-number))
              :chamfer-stock-function #'%width-one-or-contributors))
           (words (surface-mesh-band-instance-words mesh))
           (descriptors nil)
           (origin (* +mesh-cell-size+ +width-one-table-site+)))
      (loop for offset from 0 below (length words)
              by +mesh-instance-word-count+
            for meta = (aref words (+ offset 3))
            for vertices = (%finished-instance-template-vertices mesh meta)
            when (= 6 (length vertices)) do
              (let ((canonical (make-array 6 :element-type 'fixnum))
                    (minimum-edge most-positive-fixnum)
                    (maximum-edge most-negative-fixnum)
                    (transverse-p t))
                (dotimes (index 6)
                  (let* ((vertex (aref vertices index))
                         (global-x
                           (+ (* +mesh-cell-size+ (aref words offset))
                              (%packed-template-coordinate vertex 0)))
                         (global-y
                           (+ (* +mesh-cell-size+ (aref words (+ offset 1)))
                              (%packed-template-coordinate vertex 1)))
                         (global-z
                           (+ (* +mesh-cell-size+ (aref words (+ offset 2)))
                              (%packed-template-coordinate vertex 2)))
                         (x (- global-x origin))
                         (y (- global-y origin))
                         (z (- global-z origin)))
                    (let ((relative (vector x y z)))
                      (declare (dynamic-extent relative))
                      (unless (and (<= -1 (aref relative u) 1)
                                   (<= -1 (aref relative v) 1))
                        (setf transverse-p nil))
                      (setf minimum-edge
                            (min minimum-edge
                                 (aref relative axis-number))
                            maximum-edge
                            (max maximum-edge
                                 (aref relative axis-number))))
                    (setf (aref canonical index)
                          (%pack-template-vertex
                           x y z (%packed-template-attributes vertex)))))
                (when (and transverse-p
                           (= minimum-edge 1) (= maximum-edge 7))
                  (let ((contributors
                          (ldb (byte +mesh-instance-stock-bit-count+
                                     +mesh-instance-stock-shift+)
                               meta)))
                    (unless (plusp contributors)
                      (error "Canonical edge descriptor lost its contributors."))
                    (push (%make-width-one-template-descriptor
                           canonical contributors (> (logcount contributors) 1))
                          descriptors)))))
      (coerce (nreverse descriptors) 'simple-vector))))

(defun %width-one-edge-transition-descriptors (state)
  (let ((descriptors (make-array 4 :initial-element -1)))
    (dotimes (index 4 descriptors)
      (let ((next (mod (1+ index) 4)))
        (unless (eq (logbitp index state) (logbitp next state))
          (let* ((occupied (if (logbitp index state) index next))
                 (empty (if (= occupied index) next index))
                 (qu-occupied (svref +quadrant-u+ occupied))
                 (qu-empty (svref +quadrant-u+ empty))
                 (qv-empty (svref +quadrant-v+ empty))
                 (normal-v-p (= qu-occupied qu-empty))
                 (normal-sign (if normal-v-p qv-empty qu-empty)))
            (setf (svref descriptors index)
                  (logior occupied
                          (if normal-v-p #b100 0)
                          (if (plusp normal-sign) #b1000 0)))))))))

(defun %compile-width-one-edge-table (domain)
  (let ((table (make-array 16)))
    (dotimes (state 16 table)
      (setf (svref table state)
            (%make-width-one-edge-pattern
             (%width-one-edge-transition-descriptors state)
             (let ((oriented (make-array 3)))
               (dotimes (axis-number 3 oriented)
                 (setf (svref oriented axis-number)
                       (%width-one-edge-descriptors-for-state
                        domain state axis-number)))))))))

(defun %width-one-vertex-contributor-descriptors (mask)
  (let ((contributors (make-array 12 :initial-element -1)))
    (loop for edge in *star-cube-edges*
          for index from 0
          when (%boundary-edge-p mask edge) do
            (let* ((low (%cube-edge-low edge))
                   (high (%cube-edge-high edge))
                   (occupied (if (logbitp low mask) low high))
                   (empty (if (= occupied low) high low))
                   (axis-number (1- (integer-length (logxor low high))))
                   (normal-sign-positive-p (logbitp axis-number empty)))
              (setf (svref contributors index)
                    (logior occupied (ash axis-number 3)
                            (if normal-sign-positive-p #b100000 0)))))
    contributors))

(defun %width-one-vertex-descriptors-for-mask (domain mask)
  (let* ((solid (%width-one-local-star-solid domain mask))
         (mesh
           (make-surface-mesh
            solid :bevel-width 1
            :source-stock-function
            (lambda (face cell axis side)
              (declare (ignore face))
              (%width-one-vertex-contributor-stock cell axis side))
            :chamfer-stock-function #'%width-one-or-contributors))
         (words (surface-mesh-fan-instance-words mesh))
         (descriptors nil))
    (loop for offset from 0 below (length words) by +mesh-instance-word-count+
          when (and (= (aref words offset) +width-one-table-site+)
                    (= (aref words (+ offset 1)) +width-one-table-site+)
                    (= (aref words (+ offset 2)) +width-one-table-site+))
            do (let* ((meta (aref words (+ offset 3)))
                      (vertices (%finished-instance-template-vertices mesh meta))
                      (contributors
                        (ldb (byte +mesh-instance-stock-bit-count+
                                   +mesh-instance-stock-shift+)
                             meta)))
                 (unless (= 3 (length vertices))
                   (error "Width-one fan table encountered a nontriangle."))
                 (unless (plusp contributors)
                   (error "Width-one fan descriptor lost its contributors."))
                 (push (%make-width-one-template-descriptor
                        vertices contributors t)
                       descriptors)))
    (coerce (nreverse descriptors) 'simple-vector)))

(defun %compile-width-one-vertex-table (domain)
  (let ((table (make-array 256)))
    (dotimes (mask 256 table)
      (setf (svref table mask)
            (%make-width-one-vertex-pattern
             (%width-one-vertex-contributor-descriptors mask)
             (%width-one-vertex-descriptors-for-mask domain mask))))))

(defparameter *width-one-face-source-table* #(-1 0 1 -1)
  "Occupied sample along one face normal for its two-bit state.")

(defparameter *width-one-table-domain*
  (make-world-domain :horizontal-bits 5))

(defparameter *width-one-edge-pattern-table*
  (%compile-width-one-edge-table *width-one-table-domain*))

(defparameter *width-one-vertex-pattern-table*
  (%compile-width-one-vertex-table *width-one-table-domain*))

(defun %width-one-sample-cell (domain site-x site-y site-z sample)
  (make-site domain
             (- site-x (if (logbitp 0 sample) 0 1))
             (- site-y (if (logbitp 1 sample) 0 1))
             (- site-z (if (logbitp 2 sample) 0 1))
             +cell-extent+ 1))

(defun %width-one-source-face (domain cell axis-number normal-sign)
  (if (minusp normal-sign)
      (site-boundary-low domain cell (index-axis axis-number))
      (site-boundary-high domain cell (index-axis axis-number))))

(defun %width-one-source-stock
    (stock-function domain site-x site-y site-z sample axis-number normal-sign)
  (let* ((cell (%width-one-sample-cell
                domain site-x site-y site-z sample))
         (face (%width-one-source-face
                domain cell axis-number normal-sign)))
    (funcall stock-function face cell (index-axis axis-number)
             (if (minusp normal-sign) :forward :backward))))

(defun %width-one-lower-contributors (algebra summaries contributor-mask)
  (declare (optimize (speed 3) (safety 1))
           (type compiled-chamfer-algebra algebra)
           (type (simple-array (unsigned-byte 16) (*)) summaries)
           (type (unsigned-byte 12) contributor-mask))
  (let ((summary 0))
    (dotimes (index (length summaries))
      (when (logbitp index contributor-mask)
        (setf summary (logior summary (aref summaries index)))))
    (%compiled-chamfer-summary-stock algebra summary)))

(defun %width-one-descriptor-normal (descriptor)
  (let* ((vertex (aref (width-one-template-descriptor-vertices descriptor) 0))
         (attributes (%packed-template-attributes vertex)))
    (values (- (ldb (byte 2 0) attributes) 1)
            (- (ldb (byte 2 2) attributes) 1)
            (- (ldb (byte 2 4) attributes) 1))))

(defun %emit-width-one-descriptor
    (builder kind site-x site-y site-z descriptor stock star-mask)
  (let* ((vertices (width-one-template-descriptor-vertices descriptor))
         (scratch (surface-mesh-builder-vertex-scratch builder)))
    (dotimes (index (length vertices))
      (setf (aref scratch index) (aref vertices index)))
    (multiple-value-bind (nx ny nz)
        (%width-one-descriptor-normal descriptor)
      (%emit-instance
       builder kind site-x site-y site-z stock
       (if (width-one-template-descriptor-ambient-star-p descriptor)
           (%star-normal-ambient-occlusion star-mask nx ny nz)
           0)
       (length vertices)))))

(defun %emit-width-one-face
    (builder domain stock-function site-x site-y site-z axis-number source-bit)
  (let* ((u (svref +axis-u+ axis-number))
         (v (svref +axis-v+ axis-number))
         (sample (logior (ash 1 u) (ash 1 v)
                         (if (= source-bit 1) (ash 1 axis-number) 0)))
         (normal-sign (if (zerop source-bit) 1 -1))
         (stock (%width-one-source-stock
                 stock-function domain site-x site-y site-z
                 sample axis-number normal-sign))
         (base (vector site-x site-y site-z))
         (p0 (make-array 3)) (p1 (make-array 3))
         (p2 (make-array 3)) (p3 (make-array 3))
         (plane (* +mesh-cell-size+ (aref base axis-number)))
         (u0 (+ (* +mesh-cell-size+ (aref base u)) 1))
         (u1 (+ (* +mesh-cell-size+ (aref base u)) 7))
         (v0 (+ (* +mesh-cell-size+ (aref base v)) 1))
         (v1 (+ (* +mesh-cell-size+ (aref base v)) 7))
         (nx (if (= axis-number 0) normal-sign 0))
         (ny (if (= axis-number 1) normal-sign 0))
         (nz (if (= axis-number 2) normal-sign 0)))
    (declare (dynamic-extent base p0 p1 p2 p3))
    (flet ((point (target uu vv)
             (setf (aref target axis-number) plane
                   (aref target u) uu
                   (aref target v) vv)))
      (point p0 u0 v0) (point p1 u1 v0)
      (point p2 u1 v1) (point p3 u0 v1))
    (%emit-quad builder :face site-x site-y site-z p0 p1 p2 p3
                nx ny nz stock 0)))

(defun %width-one-edge-state (star-mask axis-number)
  (let ((u (svref +axis-u+ axis-number))
        (v (svref +axis-v+ axis-number))
        (state 0))
    (dotimes (quadrant 4 state)
      (let ((sample
              (logior (ash 1 axis-number)
                      (if (plusp (svref +quadrant-u+ quadrant))
                          (ash 1 u) 0)
                      (if (plusp (svref +quadrant-v+ quadrant))
                          (ash 1 v) 0))))
        (when (logbitp sample star-mask)
          (setf state (logior state (ash 1 quadrant))))))))

(defun %emit-width-one-edge
    (builder domain stock-function algebra site-x site-y site-z
     axis-number star-mask pattern x0 x1 y0 y1 edge-owned-p)
  (let* ((u (svref +axis-u+ axis-number))
         (v (svref +axis-v+ axis-number))
         (transitions (width-one-edge-pattern-transitions pattern))
         (summaries (make-array 4 :element-type '(unsigned-byte 16)
                                  :initial-element 0))
         (source-owned-mask 0))
    (declare (dynamic-extent summaries))
    (dotimes (index 4)
      (let ((descriptor (svref transitions index)))
        (unless (minusp descriptor)
          (let* ((quadrant (ldb (byte 2 0) descriptor))
                 (normal-axis
                   (if (logbitp 2 descriptor) v u))
                 (normal-sign (if (logbitp 3 descriptor) 1 -1))
                 (sample
                   (logior
                    (ash 1 axis-number)
                    (if (plusp (svref +quadrant-u+ quadrant))
                        (ash 1 u) 0)
                    (if (plusp (svref +quadrant-v+ quadrant))
                        (ash 1 v) 0)))
                 (stock (%width-one-source-stock
                         stock-function domain site-x site-y site-z
                         sample normal-axis normal-sign)))
            (setf (aref summaries index)
                  (%compiled-chamfer-stock-summary algebra stock))
            (let ((source-x
                    (- site-x (if (logbitp 0 sample) 0 1)))
                  (source-y
                    (- site-y (if (logbitp 1 sample) 0 1))))
              (when (and (<= x0 source-x) (< source-x x1)
                         (<= y0 source-y) (< source-y y1))
                (setf source-owned-mask
                      (logior source-owned-mask (ash 1 index)))))))))
    (loop for descriptor across
          (svref (width-one-edge-pattern-descriptors pattern) axis-number)
          for contributors =
            (width-one-template-descriptor-contributor-mask descriptor)
          when (if (width-one-template-descriptor-ambient-star-p descriptor)
                   edge-owned-p
                   (logtest contributors source-owned-mask))
            do (let ((stock (%width-one-lower-contributors
                             algebra summaries contributors)))
                 (%emit-width-one-descriptor
                  builder :band site-x site-y site-z
                  descriptor stock star-mask)))))

(defun %emit-width-one-vertex
    (builder domain stock-function algebra site-x site-y site-z
     star-mask pattern)
  (let ((summaries (make-array 12 :element-type '(unsigned-byte 16)
                                 :initial-element 0)))
    (declare (dynamic-extent summaries))
    (loop for contributor across
          (width-one-vertex-pattern-contributors pattern)
          for index from 0
          unless (minusp contributor) do
            (let* ((sample (ldb (byte 3 0) contributor))
                   (axis-number (ldb (byte 2 3) contributor))
                   (normal-sign (if (logbitp 5 contributor) 1 -1))
                   (stock (%width-one-source-stock
                           stock-function domain site-x site-y site-z
                           sample axis-number normal-sign)))
              (setf (aref summaries index)
                    (%compiled-chamfer-stock-summary algebra stock))))
    (loop for descriptor across
          (width-one-vertex-pattern-descriptors pattern)
          for stock = (%width-one-lower-contributors
                       algebra summaries
                       (width-one-template-descriptor-contributor-mask
                        descriptor))
          do (%emit-width-one-descriptor
              builder :junction site-x site-y site-z
              descriptor stock star-mask))))

(defun %width-one-chunk-star-z-bounds
    (chunk field grid-x grid-y x0 x1 y0 y1)
  "Bounds of cells which can touch this chunk's owned lattice vertices."
  (let ((minimum-z +top-z+) (maximum-z -1))
    (flet ((observe (x y z)
             (when (and (<= (1- x0) x) (< x x1)
                        (<= (1- y0) y) (< y y1))
               (setf minimum-z (min minimum-z z)
                     maximum-z (max maximum-z z)))))
      (loop for cell across (%chain-sites chunk)
            do (observe (site-x cell) (site-y cell) (site-z cell)))
      (loop for (dx dy) in '((-1 0) (0 -1) (-1 -1)) do
        (let ((nx (+ grid-x dx)) (ny (+ grid-y dy)))
          (when (and (<= 0 nx) (<= 0 ny))
            (let ((resolution
                    (%field-chunk-resolution field (%chunk-morton nx ny))))
              (case resolution
                (:air nil)
                (:solid
                 (error "A fully solid chunk resolution cannot feed a mesh halo yet."))
                (t
                 (loop for key being the hash-keys of resolution
                       do (let ((x (%lattice-key-x key))
                                (y (%lattice-key-y key)))
                            (when (or (< x x0) (< y y0))
                              (observe x y (%lattice-key-z key)))))))))))
      (if (minusp maximum-z)
          (values nil nil)
          (values minimum-z (1+ maximum-z))))))

(defun %mesh-width-one-chunk-scan
    (chunk chunk-key field domain stock-function algebra builder
     x0 x1 y0 y1 ox1 oy1)
  "Emit one bounded owner directly from its finite star tables."
  (let ((grid-x (chunk-key-x chunk-key))
        (grid-y (chunk-key-y chunk-key)))
    (multiple-value-bind (z0 z1)
        (%width-one-chunk-star-z-bounds
         chunk field grid-x grid-y x0 x1 y0 y1)
      (when z0
        ;; Source-owned faces and flat collars may be anchored on the high
        ;; seam.  True edge bands and fans still obey the half-open site box.
        (loop for site-x from x0 to x1 do
          (loop for site-y from y0 to y1 do
            (loop for site-z from z0 to z1 do
              (let ((star-mask
                      (%star-mask-at field domain site-x site-y site-z)))
                (unless (or (zerop star-mask) (= star-mask #xff))
                  (let ((site-owned-p
                          (and (< site-x ox1) (< site-y oy1))))
                    (when (and site-owned-p
                               (= 1 (sbit *star-singular-bits* star-mask)))
                      (incf
                       (surface-mesh-builder-singular-star-count builder)))
                    (dotimes (axis-number 3)
                      (let* ((u (svref +axis-u+ axis-number))
                             (v (svref +axis-v+ axis-number))
                             (low-sample (logior (ash 1 u) (ash 1 v)))
                             (high-sample
                               (logior low-sample (ash 1 axis-number)))
                             (face-state
                               (logior
                                (if (logbitp low-sample star-mask) 1 0)
                                (if (logbitp high-sample star-mask) 2 0)))
                             (source-bit
                               (svref *width-one-face-source-table*
                                      face-state)))
                        (unless (minusp source-bit)
                          (let* ((source-sample
                                   (if (zerop source-bit)
                                       low-sample high-sample))
                                 (source-x
                                   (- site-x
                                      (if (logbitp 0 source-sample) 0 1)))
                                 (source-y
                                   (- site-y
                                      (if (logbitp 1 source-sample) 0 1))))
                            ;; Faces belong to their occupied source cell, not
                            ;; to the lattice vertex which anchors them.
                            (when (and (<= x0 source-x) (< source-x x1)
                                       (<= y0 source-y) (< source-y y1))
                              (%emit-width-one-face
                               builder domain stock-function
                               site-x site-y site-z
                               axis-number source-bit))))
                        (let* ((edge-state
                                 (%width-one-edge-state
                                  star-mask axis-number))
                               (pattern
                                 (svref *width-one-edge-pattern-table*
                                        edge-state)))
                          (when (plusp
                                 (length
                                  (svref
                                   (width-one-edge-pattern-descriptors pattern)
                                   axis-number)))
                            (%emit-width-one-edge
                             builder domain stock-function algebra
                             site-x site-y site-z axis-number star-mask pattern
                             x0 x1 y0 y1 site-owned-p)))))
                    (when site-owned-p
                      (let ((pattern
                              (svref *width-one-vertex-pattern-table*
                                     star-mask)))
                        (when (plusp
                               (length
                                (width-one-vertex-pattern-descriptors pattern)))
                          (%emit-width-one-vertex
                           builder domain stock-function algebra
                           site-x site-y site-z star-mask pattern)))))))))))))
  (%finish-surface-mesh builder))

(defun mesh-chunk
    (chunk chunk-key
     &key (stock-function (constantly 0))
          source-stock-function
          (chamfer-stock-function (lambda (stocks) (first stocks)))
          chamfer-algebra
          (bevel-width +mesh-bevel-width+))
  "Classify one chunk's solid CHUNK into the instance-stream ABI.

CHUNK holds exactly the cells of the chunk named by CHUNK-KEY.  Probes
leaving the chunk signal MISSING-CHUNK once per neighboring chunk -- bind a
handler that answers USE-CHUNK from a store, or TREAT-AS-AIR to fill in --
  and probes past the world's box signal OUTSIDE-DOMAIN; MESH-CHUNK sets no
  policy of its own.  The mesh ships only what this chunk owns: faces of its
own solid cells, bands whose edge anchors lie inside it, and fans at its
own lattice vertices.  At width one, CHAMFER-ALGEBRA selects the bounded
finite-neighborhood scan; omitting it retains the surface-proportional oracle.
The oracle recomputes witness faces and bands from the one-cell halo but never
ships them, so seam fans close exactly as a whole-world mesh would close them.
Exact coplanar compression is a
separate, explicitly named transform; this canonical topology producer has no
alternate representation switch."
  (check-type chunk chain)
  (check-type stock-function function)
  (check-type source-stock-function (or null function))
  (check-type chamfer-stock-function function)
  (check-type chamfer-algebra (or null compiled-chamfer-algebra))
  (unless (and (integerp bevel-width)
               (<= 1 bevel-width (/ +mesh-cell-size+ 2)))
    (error "Bevel width ~S must be an integer between one and four ticks."
           bevel-width))
  (let* ((domain (chain-domain chunk))
         (stock-resolver
           (%make-face-stock-resolver stock-function source-stock-function))
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
    (when (and (= bevel-width 1) chamfer-algebra)
      (return-from mesh-chunk
        (%mesh-width-one-chunk-scan
         chunk chunk-key field domain stock-resolver chamfer-algebra builder
         x0 x1 y0 y1
         (if (>= (+ x0 +chunk-size+) (world-domain-x-limit domain))
             (1+ (world-domain-x-limit domain))
             (+ x0 +chunk-size+))
         (if (>= (+ y0 +chunk-size+) (world-domain-y-limit domain))
             (1+ (world-domain-y-limit domain))
             (+ y0 +chunk-size+)))))
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
                                    stock-resolver chamfer-stock-function
                                    edge-candidates)
          (%emit-exposed-cell-faces witness-sheets field domain halo-sites
                                    stock-resolver chamfer-stock-function
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
                        stock-resolver chamfer-stock-function))))
          (%count-singular-vertex-stars builder field domain region-cells
                                        x0 ox1 y0 oy1)
          (%emit-boundary-derived-fans builder field domain
                                       chamfer-stock-function
                                       (list ship-sheets witness-sheets)
                                       packing
                                       x0 ox1 y0 oy1 t)
          (%finish-surface-mesh builder))))))
