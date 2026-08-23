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
(defconstant +mesh-bevel-width+ 1
  "Default bevel width in eighth-cell integer ticks.")
(defconstant +mesh-instance-word-count+ 4)
(defconstant +mesh-template-vertex-word-count+ 4)
(defconstant +mesh-template-coordinate-bias+ 16)
(defconstant +mesh-instance-stock-shift+ 16)
(defconstant +mesh-instance-ambient-occlusion-shift+ 20)

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
  (singular-star-count 0 :type (integer 0 *) :read-only t))

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
;;; inside the box are one EQL lookup on a packed lattice key; probes outside
;;; it signal OUTSIDE-DOMAIN with the boundary restarts, so the caller's
;;; policy (whole-world air, a chunk store, a strict test) decides the edge.

(defstruct (occupancy-field
             (:constructor %make-occupancy-field (domain table x0 x1 y0 y1)))
  (domain nil :type world-domain :read-only t)
  (table nil :type hash-table :read-only t)
  ;; Half-open cell-coordinate bounds of the resident box.
  (x0 0 :type fixnum :read-only t)
  (x1 0 :type fixnum :read-only t)
  (y0 0 :type fixnum :read-only t)
  (y1 0 :type fixnum :read-only t)
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
  "Validate SOLID's cells and index them over the given cell box."
  (%make-occupancy-field (chain-domain solid) (%chunk-cells-table solid)
                         x0 x1 y0 y1))

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
(defun %occupied-bit (field domain x y z)
  "Occupancy of one cell: air beyond Z, a lookup inside the field's box,
one cached MISSING-CHUNK resolution per non-resident chunk inside the
world, and an OUTSIDE-DOMAIN signal past the world's own edges."
  (declare (optimize (speed 3) (safety 1)))
  (cond ((or (< z 0) (>= z +top-z+)) 0)
        ((and (<= (occupancy-field-x0 field) x)
              (< x (occupancy-field-x1 field))
              (<= (occupancy-field-y0 field) y)
              (< y (occupancy-field-y1 field)))
         (if (gethash (%lattice-key x y z) (occupancy-field-table field)) 1 0))
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
  (declare (optimize (speed 3) (safety 1)))
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

(defconstant +mesh-vertex-attribute-shift+ 15)

(declaim (inline %pack-template-vertex))
(defun %pack-template-vertex (x y z attributes)
  (unless (and (<= -16 x 15) (<= -16 y 15) (<= -16 z 15))
    (error "Template coordinate ~S does not fit the signed five-bit ABI."
           (list x y z)))
  (logior (+ x +mesh-template-coordinate-bias+)
          (ash (+ y +mesh-template-coordinate-bias+) 5)
          (ash (+ z +mesh-template-coordinate-bias+) 10)
          (ash attributes +mesh-vertex-attribute-shift+)))

(defstruct (instance-stream (:constructor %make-instance-stream ()))
  ;; Four ABI words per instance in emission order: base x, y, z, meta.
  (words (make-array 4096 :element-type '(unsigned-byte 32)
                          :adjustable t :fill-pointer 0)
         :type (vector (unsigned-byte 32))))

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
  (singular-star-count 0 :type (integer 0 *)))

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
  (check-type stock (unsigned-byte 4))
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
           (type fixnum offset ax ay az bx by bz cx cy cz nx ny nz))
  (let* ((ux (- bx ax)) (uy (- by ay)) (uz (- bz az))
         (vx (- cx ax)) (vy (- cy ay)) (vz (- cz az))
         (px (- (* uy vz) (* uz vy)))
         (py (- (* uz vx) (* ux vz)))
         (pz (- (* ux vy) (* uy vx)))
         (orientation (+ (* px nx) (* py ny) (* pz nz))))
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

(defun %collect-edge-keys (cells)
  "Return the packed (axis, anchor) keys of every edge incident to CELLS.

CELLS is a vector of packed lattice keys naming solid cell anchors."
  (declare (optimize (speed 3) (safety 1))
           (type (simple-array (unsigned-byte 64) (*)) cells))
  (let ((seen (make-hash-table :test #'eql
                               :size (* 8 (max 8 (length cells))))))
    (loop for cell-key across cells do
      (let ((cx (%lattice-key-x cell-key))
            (cy (%lattice-key-y cell-key))
            (cz (%lattice-key-z cell-key)))
        (dotimes (axis-number 3)
          (let ((u (svref +axis-u+ axis-number))
                (v (svref +axis-v+ axis-number)))
            (dotimes (u-side 2)
              (dotimes (v-side 2)
                (let ((x cx) (y cy) (z cz))
                  (macrolet ((bump (axis-form amount)
                               `(let ((axis ,axis-form))
                                  (case axis
                                    (0 (incf x ,amount))
                                    (1 (incf y ,amount))
                                    (t (incf z ,amount))))))
                    (bump u u-side)
                    (bump v v-side))
                  (setf (gethash (logior (ash axis-number
                                              +mesh-key-axis-shift+)
                                         (%lattice-key x y z))
                                 seen)
                        t))))))))
    (let ((keys (make-array (hash-table-count seen)
                            :element-type '(unsigned-byte 64)))
          (write 0))
      (loop for key being the hash-keys of seen
            do (setf (aref keys write) key)
               (incf write))
      (sort keys #'<))))

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
;;; left12<<16 | right12<<4 | stock, where a point12 is (x+4)<<8|(y+4)<<4|(z+4).
;;;
;;; The key itself is one fixnum, because a boundary packing states the
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

(defun %fan-point (x y z)
  (logior (ash (+ x +fan-local-bias+) 8)
          (ash (+ y +fan-local-bias+) 4)
          (+ z +fan-local-bias+)))
(defun %fan-point-x (point) (- (ldb (byte 4 8) point) +fan-local-bias+))
(defun %fan-point-y (point) (- (ldb (byte 4 4) point) +fan-local-bias+))
(defun %fan-point-z (point) (- (ldb (byte 4 0) point) +fan-local-bias+))

(defun %fan-record-left (record) (ldb (byte 12 16) record))
(defun %fan-record-right (record) (ldb (byte 12 4) record))
(defun %fan-record-stock (record) (ldb (byte 4 0) record))
(defun %fan-record-undirected (record)
  (let ((left (%fan-record-left record))
        (right (%fan-record-right record)))
    (if (< left right)
        (logior (ash left 12) right)
        (logior (ash right 12) left))))

(defconstant +boundary-z-span+ 257
  "Anchor Z values run from -1 through 255 inclusive.")

(defstruct (boundary-packing
             (:constructor %make-boundary-packing (origin-x origin-y y-span)))
  "The anchor box one parity scan covers, and thus its fixnum key layout."
  (origin-x 0 :type fixnum :read-only t)
  (origin-y 0 :type fixnum :read-only t)
  (y-span 1 :type (integer 1 *) :read-only t))

(defun %make-boundary-packing-for-box (x0 x1 y0 y1)
  "Pack anchors of the cell box [X0, X1) x [Y0, Y1).

Instance bases lie inside the box and template offsets reach at most one
eighth-cell anchor beyond it, so the packed anchor box is widened by two."
  (let* ((origin-x (- x0 2))
         (origin-y (- y0 2))
         (x-span (+ (- x1 x0) 4))
         (y-span (+ (- y1 y0) 4)))
    (unless (typep (ash (* x-span y-span +boundary-z-span+) 24) 'fixnum)
      (error "A solid spanning ~Dx~D cells is too wide for one boundary ~
              scan; mesh it by chunks."
             x-span y-span))
    (%make-boundary-packing origin-x origin-y y-span)))

(declaim (inline %boundary-edge-key %boundary-key-anchor-x
                 %boundary-key-anchor-y %boundary-key-anchor-z))
(defun %boundary-edge-key (packing anchor-x anchor-y anchor-z edge)
  (logior (ash (+ (* (+ (* (- anchor-x (boundary-packing-origin-x packing))
                           (boundary-packing-y-span packing))
                        (- anchor-y (boundary-packing-origin-y packing)))
                     +boundary-z-span+)
                  (1+ anchor-z))
               24)
          edge))

(defun %boundary-key-anchor-x (packing key)
  (+ (boundary-packing-origin-x packing)
     (truncate (ash key -24)
               (* (boundary-packing-y-span packing) +boundary-z-span+))))
(defun %boundary-key-anchor-y (packing key)
  (+ (boundary-packing-origin-y packing)
     (mod (truncate (ash key -24) +boundary-z-span+)
          (boundary-packing-y-span packing))))
(defun %boundary-key-anchor-z (key)
  (1- (mod (ash key -24) +boundary-z-span+)))

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

Returns a table from PACKING's fixnum edge keys to count<<28 | left12<<16 |
right12<<4 | stock, where the 12-bit points are anchor-local."
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
                        (stock (ldb (byte 4 +mesh-instance-stock-shift+)
                                    meta))
                        (vertices (mesh-template-vertices template))
                        (ox (* +mesh-cell-size+ base-x))
                        (oy (* +mesh-cell-size+ base-y))
                        (oz (* +mesh-cell-size+ base-z)))
                   (flet ((global-x (vertex)
                            (+ ox (- (ldb (byte 5 0) (aref vertices vertex))
                                     +mesh-template-coordinate-bias+)))
                          (global-y (vertex)
                            (+ oy (- (ldb (byte 5 5) (aref vertices vertex))
                                     +mesh-template-coordinate-bias+)))
                          (global-z (vertex)
                            (+ oz (- (ldb (byte 5 10) (aref vertices vertex))
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
                                       (rz (global-z right))
                                       (anchor-x (ash (min lx rx) -3))
                                       (anchor-y (ash (min ly ry) -3))
                                       (anchor-z (ash (min lz rz) -3))
                                       (left12
                                         (logior
                                          (ash (- lx (* 8 anchor-x)) 8)
                                          (ash (- ly (* 8 anchor-y)) 4)
                                          (- lz (* 8 anchor-z))))
                                       (right12
                                         (logior
                                          (ash (- rx (* 8 anchor-x)) 8)
                                          (ash (- ry (* 8 anchor-y)) 4)
                                          (- rz (* 8 anchor-z))))
                                       (key
                                         (%boundary-edge-key
                                          packing anchor-x anchor-y anchor-z
                                          (if (< left12 right12)
                                              (logior (ash left12 12)
                                                      right12)
                                              (logior (ash right12 12)
                                                      left12))))
                                       (existing
                                         (gethash key observations)))
                                  (cond
                                    ((null existing)
                                     (setf (gethash key observations)
                                           (logior (ash 1 28)
                                                   (ash left12 16)
                                                   (ash right12 4)
                                                   stock)))
                                    (t
                                     (let ((count
                                             (1+ (ash existing -28))))
                                       (when (> count 2)
                                         (error "Face and edge streams meet ~D times at ~S."
                                                count key))
                                       (setf (gethash key observations)
                                             (dpb count (byte 4 28)
                                                  existing)))))))))))))

(defun %attribute-open-edges-to-sites
    (builders packing bevel-width drop-nonlocal-p)
  "Group the open boundary's directed edges by owning lattice vertex.

Returns a table from packed site keys to lists of fan records whose 12-bit
points are site-local with the fan bias.  An open edge not contained in any
vertex's bevel domain is an invariant violation for a whole solid; for a
chunk's witness scan it is the witness truncation boundary, provably outside
every owned site's bevel domain, and DROP-NONLOCAL-P discards it."
  (declare (optimize (speed 3) (safety 1)))
  (let ((observations (%builder-open-boundary-table builders packing))
        (by-site (make-hash-table :test #'eql :size 1024)))
    (maphash
     (lambda (key value)
       (when (= 1 (ash value -28))
         (block attribute
           (let* ((anchor-x (%boundary-key-anchor-x packing key))
                  (anchor-y (%boundary-key-anchor-y packing key))
                  (anchor-z (%boundary-key-anchor-z key))
                  (left12 (ldb (byte 12 16) value))
                  (right12 (ldb (byte 12 4) value))
                  (stock (ldb (byte 4 0) value))
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
                              16)
                         (ash (%fan-point (- rx (* 8 site-x))
                                          (- ry (* 8 site-y))
                                          (- rz (* 8 site-z)))
                              4)
                         stock)))
                 (push record
                       (gethash (%lattice-key site-x site-y site-z)
                                by-site))))))))
     observations)
    by-site))

(defun %fan-record-cycles (site-x site-y site-z records)
  "Order consistently directed fan RECORDS into loops at the site."
  (let ((pending (copy-list records))
        (cycles nil))
    (loop while pending do
      (let* ((first-record (pop pending))
             (first-point (%fan-record-left first-record))
             (next-point (%fan-record-right first-record))
             (cycle (list first-record)))
        (loop until (= next-point first-point) do
          (let ((next-record
                  (find next-point pending :key #'%fan-record-left)))
            (unless next-record
              (error "Open boundary at lattice site ~S stops at ~S."
                     (list site-x site-y site-z)
                     (list (%fan-point-x next-point)
                           (%fan-point-y next-point)
                           (%fan-point-z next-point))))
            (setf pending (delete next-record pending :count 1)
                  cycle (append cycle (list next-record))
                  next-point (%fan-record-right next-record))))
        (push cycle cycles)))
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
        (logior (ash (rescale-point left) 16)
                (ash (rescale-point right) 4)
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
              do (setf (aref words write) (ldb (byte 5 0) vertex)
                       (aref words (+ write 1)) (ldb (byte 5 5) vertex)
                       (aref words (+ write 2)) (ldb (byte 5 10) vertex)
                       (aref words (+ write 3))
                       (ash vertex (- +mesh-vertex-attribute-shift+)))
                 (incf write 4))
        (incf vertex-start (length vertices))))
    (values words ranges)))

(defun %finish-instance-stream (stream ranges)
  "Order one columnar stream by template and derive its draws."
  (let* ((source (instance-stream-words stream))
         (count (instance-stream-count stream))
         (order (make-array count :element-type '(unsigned-byte 32)))
         (words (make-array (* +mesh-instance-word-count+ count)
                            :element-type '(unsigned-byte 32)))
         (draws nil)
         (triangle-count 0))
    ;; Seed the permutation with reversed emission order: instances were
    ;; historically accumulated by PUSH, so equal templates draw in reverse
    ;; emission order, and the stable sort preserves that tie order.
    (dotimes (index count)
      (setf (aref order index) (- count 1 index)))
    (flet ((template-id (index)
             (ldb (byte 16 0)
                  (aref source (+ (* +mesh-instance-word-count+ index) 3)))))
      (setf order (stable-sort order #'< :key #'template-id))
      (loop for instance-index from 0
            for source-index across order
            for template-id = (template-id source-index)
            for vertex-start = (aref ranges (* 2 template-id))
            for vertex-count = (aref ranges (1+ (* 2 template-id)))
            do (replace words source
                        :start1 (* +mesh-instance-word-count+ instance-index)
                        :start2 (* +mesh-instance-word-count+ source-index)
                        :end2 (* +mesh-instance-word-count+
                                 (1+ source-index)))
               (incf triangle-count (truncate vertex-count 3))
               (let ((draw (first draws)))
                 (if (and draw (= template-id (first draw)))
                     (incf (fifth draw))
                     (push (list template-id vertex-start vertex-count
                                 instance-index 1)
                           draws)))))
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
             (x-limit (world-domain-x-limit domain))
             (y-limit (world-domain-y-limit domain)))
         (%emit-exposed-cell-faces boundary-builder field domain
                                   (%chain-sites solid)
                                   stock-function chamfer-stock-function)
         (loop for key across (%collect-edge-keys cells)
               do (%emit-edge-bands boundary-builder field domain key
                                    stock-function chamfer-stock-function))
         (%count-singular-vertex-stars builder field domain cells
                                       0 (1+ x-limit) 0 (1+ y-limit))
         (multiple-value-bind (x0 x1 y0 y1) (%cell-key-box cells)
           (%emit-boundary-derived-fans builder field domain
                                        chamfer-stock-function
                                        (list boundary-builder)
                                        (%make-boundary-packing-for-box
                                         x0 x1 y0 y1)
                                        0 (1+ x-limit) 0 (1+ y-limit) nil))
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
    (target field domain cells stock-function chamfer-stock-function)
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
            (%emit-cell-face target field domain cell axis-number side
                             stock-function chamfer-stock-function)))))))

(defun mesh-chunk
    (chunk chunk-key
     &key (stock-function (constantly 0))
          (chamfer-stock-function (lambda (stocks) (first stocks)))
          (bevel-width +mesh-bevel-width+))
  "Classify one chunk's solid CHUNK into the instance-stream ABI.

CHUNK holds exactly the cells of the chunk named by CHUNK-KEY.  Probes
leaving the chunk signal MISSING-CHUNK once per neighboring chunk -- bind a
handler that answers USE-CHUNK from a store, or TREAT-AS-AIR to fill in --
and probes past the world's box signal OUTSIDE-DOMAIN; MESH-CHUNK sets no
policy of its own.  The mesh ships only what this chunk owns: faces of its
own solid cells, bands whose edge anchors lie inside it, and fans at its
own lattice vertices.  Witness faces and bands are recomputed from the
one-cell halo and scanned but never shipped, so seam fans close exactly as
a whole-world mesh would close them."
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
             (region-cells (concatenate
                            '(simple-array (unsigned-byte 64) (*))
                            own-cells halo))
             (halo-sites (make-array (length halo)
                                     :element-type '(unsigned-byte 64))))
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
                                  stock-function chamfer-stock-function)
        (%emit-exposed-cell-faces witness-sheets field domain halo-sites
                                  stock-function chamfer-stock-function)
        (loop for key across (%collect-edge-keys region-cells)
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
                                     ;; The scan covers this chunk's cells
                                     ;; plus its low-side halo ring.
                                     (%make-boundary-packing-for-box
                                      (1- x0) x1 (1- y0) y1)
                                     x0 ox1 y0 oy1 t)
        (%finish-surface-mesh builder)))))
