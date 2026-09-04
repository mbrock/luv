(in-package #:luft)

;;; ---------------------------------------------------------------------------
;;; Chunk fibers: dense column occupancy
;;;
;;; A chain is the authored, persistent, algebraic form of a solid.  The
;;; computational form of one resident chunk is its fibers: for each of the
;;; 64x64 XY columns, four 64-bit words whose bit Z says whether the cell at
;;; height Z is solid.  The star selector was written against this view, and
;;; that is why it is fast: a probe is one LOGBITP, a column is four words,
;;; and a mixed star is word-parallel bit logic.  Collision, support, vertical
;;; rays, editing, and lighting all want exactly the same words, so the
;;; fibers are a public chunk representation rather than a mesher cache.
;;;
;;; Fibers are values.  A chain derives its fibers once through a weak cache,
;;; and an edit produces a fresh copy.  Only construction mutates in place.

(defconstant +fiber-word-count+ 4
  "64-bit words per XY column: 256 vertical cells.")
(defconstant +chunk-column-count+ (* +chunk-size+ +chunk-size+))
(defconstant +chunk-fiber-word-count+
  (* +chunk-column-count+ +fiber-word-count+))
(defconstant +fiber-word-mask+ #xffffffffffffffff)

(deftype fiber-vector ()
  '(simple-array (unsigned-byte 64) (*)))

(defstruct (chunk-fibers
             (:constructor %make-chunk-fibers (domain key words))
             (:copier nil))
  "One chunk's occupancy as 64x64 columns of four 64-bit words.

KEY is the chunk key.  WORDS is X-major: column (X, Y) begins at
(FIBER-BASE X Y), and bit Z of its word (ash Z -6) is the cell at height Z."
  (domain nil :type world-domain :read-only t)
  (key 0 :type chunk-key :read-only t)
  (words #.(make-array 0 :element-type '(unsigned-byte 64))
         :type fiber-vector :read-only t))

(defmethod print-object ((fibers chunk-fibers) stream)
  (print-unreadable-object (fibers stream :type t)
    (format stream "chunk ~D (~D ~D) ~D cells"
            (chunk-fibers-key fibers)
            (chunk-key-x (chunk-fibers-key fibers))
            (chunk-key-y (chunk-fibers-key fibers))
            (fibers-cell-count fibers))))

(declaim (inline fiber-base))
(defun fiber-base (x y)
  "The word index of column (X, Y)'s first fiber word; X and Y may be global."
  (declare (optimize (speed 3) (safety 1))
           (type fixnum x y))
  (* +fiber-word-count+
     (+ (logand y (1- +chunk-size+))
        (ash (logand x (1- +chunk-size+)) +chunk-bits+))))

(defun make-chunk-fibers (domain key)
  "Fresh all-air fibers for chunk KEY in DOMAIN."
  (check-type domain world-domain)
  (%make-chunk-fibers
   domain key
   (make-array +chunk-fiber-word-count+
               :element-type '(unsigned-byte 64) :initial-element 0)))

(defun copy-chunk-fibers (fibers)
  "A fresh, independently mutable copy of FIBERS."
  (let ((words (make-array +chunk-fiber-word-count+
                           :element-type '(unsigned-byte 64))))
    (replace words (chunk-fibers-words fibers))
    (%make-chunk-fibers (chunk-fibers-domain fibers)
                        (chunk-fibers-key fibers) words)))

(defun fibers= (a b)
  "Whether A and B hold the same chunk with identical occupancy."
  (and (= (chunk-fibers-key a) (chunk-fibers-key b))
       (equalp (chunk-fibers-words a) (chunk-fibers-words b))))

(defun fibers-contain-column-p (fibers x y)
  "Whether world column (X, Y) lies inside FIBERS' chunk."
  (= (chunk-key-at x y) (chunk-fibers-key fibers)))

(declaim (inline fibers-cell-bit))
(defun fibers-cell-bit (fibers x y z)
  "The occupancy bit of world cell (X, Y, Z), which must lie in this chunk.
Heights outside 0..254 are air."
  (declare (optimize (speed 3) (safety 1))
           (type chunk-fibers fibers) (type fixnum x y z))
  (if (or (< z 0) (>= z +top-z+))
      0
      (let ((word (aref (chunk-fibers-words fibers)
                        (+ (fiber-base x y) (ash z -6)))))
        (if (logbitp (logand z 63) word) 1 0))))

(defun (setf fibers-cell-bit) (bit fibers x y z)
  "Set one cell during construction.  Published fibers are values; edit
those with FIBERS-WITH-CELL or FIBERS-WITH-CHAIN instead."
  (declare (type bit bit) (type fixnum x y z))
  (unless (and (<= 0 z) (< z +top-z+))
    (error "Height ~D is outside the fiber range 0..~D." z (1- +top-z+)))
  (let* ((words (chunk-fibers-words fibers))
         (index (+ (fiber-base x y) (ash z -6)))
         (mask (ash 1 (logand z 63))))
    (setf (aref words index)
          (if (= bit 1)
              (logior (aref words index) mask)
              (logandc2 (aref words index) mask)))
    bit))

(defun fibers-column-word (fibers x y word)
  "The WORDth 64-bit occupancy word of column (X, Y)."
  (aref (chunk-fibers-words fibers) (+ (fiber-base x y) word)))

(defun fibers-cell-count (fibers)
  "The number of solid cells."
  (let ((count 0))
    (declare (type fixnum count))
    (loop for word across (chunk-fibers-words fibers)
          do (incf count (logcount word)))
    count))

(defun fibers-empty-p (fibers)
  (every #'zerop (chunk-fibers-words fibers)))

(defun map-fibers-cells (function fibers)
  "Call FUNCTION with world X, Y, Z of each solid cell in site order."
  (let ((words (chunk-fibers-words fibers))
        (x0 (chunk-origin-x (chunk-fibers-key fibers)))
        (y0 (chunk-origin-y (chunk-fibers-key fibers))))
    (dotimes (y +chunk-size+)
      (dotimes (x +chunk-size+)
        (let ((base (fiber-base x y)))
          (dotimes (word +fiber-word-count+)
            (let ((bits (aref words (+ base word))))
              (loop while (plusp bits) do
                (let ((bit (1- (integer-length (logand bits (- bits))))))
                  (funcall function (+ x0 x) (+ y0 y) (+ (* 64 word) bit))
                  (setf bits (logand bits (1- bits))))))))))
    fibers))

;;; Vertical probes.  These are the primitives under collision, support,
;;; picking, and light: one column is four words, so a question about a
;;; height range is a mask and a few bit-scans rather than a loop of cells.

(declaim (inline %range-word-mask))
(defun %range-word-mask (low high)
  "Bits LOW (inclusive) to HIGH (exclusive) of one word, both clamped 0..64."
  (let ((low (max 0 (min 64 low)))
        (high (max 0 (min 64 high))))
    (if (>= low high)
        0
        (ldb (byte 64 0) (ash (ldb (byte (- high low) 0) -1) low)))))

(defun fibers-column-clear-p (fibers x y low high)
  "Whether every cell of column (X, Y) with LOW <= Z < HIGH is air.
Heights outside 0..254 count as air."
  (let ((words (chunk-fibers-words fibers))
        (base (fiber-base x y))
        (low (max 0 low))
        (high (min +top-z+ high)))
    (loop for word from (ash low -6) to (ash (max low (1- high)) -6)
          always (zerop (logand (aref words (+ base word))
                                (%range-word-mask (- low (* 64 word))
                                                  (- high (* 64 word))))))))

(defun fibers-highest-cell-below (fibers x y z)
  "The greatest solid Z' < Z in column (X, Y), or NIL."
  (let ((words (chunk-fibers-words fibers))
        (base (fiber-base x y))
        (z (min z +top-z+)))
    (when (plusp z)
      (loop for word from (ash (1- z) -6) downto 0
            for bits = (logand (aref words (+ base word))
                               (%range-word-mask 0 (- z (* 64 word))))
            when (plusp bits)
              return (+ (* 64 word) (1- (integer-length bits)))))))

(defun fibers-lowest-cell-above (fibers x y z)
  "The least solid Z' >= Z in column (X, Y), or NIL."
  (let ((words (chunk-fibers-words fibers))
        (base (fiber-base x y))
        (z (max z 0)))
    (when (< z +top-z+)
      (loop for word from (ash z -6) below +fiber-word-count+
            for bits = (logand (aref words (+ base word))
                               (%range-word-mask (- z (* 64 word)) 64))
            when (plusp bits)
              return (+ (* 64 word)
                        (1- (integer-length (logand bits (- bits)))))))))

;;; Edits produce fresh values.

(defun fibers-with-cell (fibers x y z bit)
  "FIBERS with world cell (X, Y, Z) set to BIT, as a fresh value."
  (let ((copy (copy-chunk-fibers fibers)))
    (setf (fibers-cell-bit copy x y z) bit)
    copy))

(defun fibers-with-chain (fibers delta)
  "FIBERS plus DELTA: positive cells become solid, negative cells air.
Every cell of DELTA must lie in FIBERS' chunk."
  (let ((copy (copy-chunk-fibers fibers))
        (key (chunk-fibers-key fibers)))
    (map-chain
     (lambda (site)
       (unless (= +cell-extent+ (site-extent site))
         (error "Fibers hold cells, not ~S." site))
       (unless (= key (site-chunk-key site))
         (error "Cell ~S lies outside chunk ~D." site key))
       (setf (fibers-cell-bit copy (site-x site) (site-y site) (site-z site))
             (if (site-positive-p site) 1 0)))
     delta)
    copy))

;;; Conversion between the two forms.

(defun %derive-chain-fibers (chain)
  "Materialize a single-chunk positive cell CHAIN as fibers."
  (let* ((domain (chain-domain chain))
         (sites (%chain-sites chain))
         (count (length sites)))
    (when (zerop count)
      (error "An empty chain names no chunk; use MAKE-CHUNK-FIBERS."))
    (let* ((key (site-chunk-key (aref sites 0)))
           (fibers (make-chunk-fibers domain key))
           (words (chunk-fibers-words fibers)))
      (unless (= key (site-chunk-key (aref sites (1- count))))
        (error "Chunk fibers need a single-chunk chain."))
      (loop for cell across sites do
        (unless (and (= +cell-extent+ (site-extent cell))
                     (site-positive-p cell))
          (error "Chunk fibers need positive cells, not ~S." cell))
        (let ((z (site-z cell)))
          (unless (< z +top-z+)
            (error "Cell ~S lies above the fiber range." cell))
          (let ((index (+ (fiber-base (site-x cell) (site-y cell))
                          (ash z -6))))
            (setf (aref words index)
                  (logior (aref words index) (ash 1 (logand z 63)))))))
      fibers)))

(defvar *chain-fibers-table*
  (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Weak identity cache from immutable chains to their immutable fibers.")

(defun chain-fibers (chain &optional key)
  "The fibers of single-chunk CHAIN, derived once per chain identity.
An empty chain needs KEY to say which chunk it is the empty fibers of."
  (check-type chain chain)
  (if (chain-empty-p chain)
      (make-chunk-fibers (chain-domain chain)
                         (or key (error "An empty chain needs a chunk key.")))
      (or (gethash chain *chain-fibers-table*)
          (setf (gethash chain *chain-fibers-table*)
                (%derive-chain-fibers chain)))))

(defun fibers-chain (fibers)
  "The normalized positive cell chain of FIBERS.
Cells are emitted directly in site order, so no sort is needed."
  (let* ((domain (chunk-fibers-domain fibers))
         (sites (make-array (fibers-cell-count fibers)
                            :element-type '(unsigned-byte 64)))
         (write 0))
    (map-fibers-cells
     (lambda (x y z)
       (setf (aref sites write) (make-site domain x y z +cell-extent+ 1))
       (incf write))
     fibers)
    (%make-chain domain sites)))

;;; ---------------------------------------------------------------------------
;;; Fiber stores: a world of resident chunks
;;;
;;; The whole-world chain is gone.  A store maps chunk keys to fibers, and a
;;; world probe resolves its chunk once, then asks the fibers.  Probes past
;;; the domain box or into an absent chunk use the same OUTSIDE-DOMAIN and
;;; MISSING-CHUNK restarts as meshing, so the caller still owns the policy.

(defstruct (fiber-store
             (:constructor make-fiber-store (domain))
             (:copier nil))
  "Resident chunk fibers keyed by chunk key."
  (domain nil :type world-domain :read-only t)
  (table (make-hash-table :test #'eql) :type hash-table :read-only t))

(defun fiber-store-chunk (store key)
  "The resident fibers of chunk KEY, or NIL."
  (values (gethash key (fiber-store-table store))))

(defun (setf fiber-store-chunk) (fibers store key)
  (if fibers
      (setf (gethash key (fiber-store-table store)) fibers)
      (remhash key (fiber-store-table store)))
  fibers)

(defun copy-fiber-store (store)
  "A store holding the same immutable chunk values as STORE.
Scenes replace their store on edit so worker snapshots keep the old one."
  (let ((copy (make-fiber-store (fiber-store-domain store))))
    (maphash (lambda (key fibers) (setf (gethash key (fiber-store-table copy)) fibers))
             (fiber-store-table store))
    copy))

(defun fiber-store-keys (store)
  (loop for key being the hash-keys of (fiber-store-table store) collect key))

(defun map-fiber-store (function store)
  "Call FUNCTION with each resident (KEY FIBERS)."
  (maphash function (fiber-store-table store))
  store)

(defun fiber-store-count (store)
  (hash-table-count (fiber-store-table store)))

(defun make-fiber-store-from-chain (chain)
  "A store holding every chunk of a multi-chunk CHAIN."
  (let ((store (make-fiber-store (chain-domain chain))))
    (map-chain-chunks
     (lambda (key chunk-chain)
       (setf (fiber-store-chunk store key) (chain-fibers chunk-chain key)))
     chain)
    store))

(defun fiber-store-chain (store)
  "The normalized union chain of every resident chunk."
  (let ((keys (sort (fiber-store-keys store) #'<)))
    (concatenate-disjoint-chains
     (fiber-store-domain store)
     (loop for key in keys
           for chain = (fibers-chain (fiber-store-chunk store key))
           unless (chain-empty-p chain) collect chain))))

(defun %missing-store-chunk (store key)
  "Resolve absent KEY through the MISSING-CHUNK restarts."
  (restart-case
      (error 'missing-chunk :domain (fiber-store-domain store) :key key)
    (use-chunk (chunk)
      :report "Supply the chunk as a chain or fibers."
      (etypecase chunk
        (chunk-fibers chunk)
        (chain (chain-fibers chunk key))))
    (treat-as-air ()
      :report "Treat this whole chunk as air."
      :air)
    (treat-as-solid ()
      :report "Treat this whole chunk as solid."
      :solid)))

(defun fiber-store-resolve (store x y)
  "The fibers holding world column (X, Y), or :AIR or :SOLID by restart.
Columns beyond the domain box signal OUTSIDE-DOMAIN."
  (let ((domain (fiber-store-domain store)))
    (if (or (< x 0) (>= x (world-domain-x-limit domain))
            (< y 0) (>= y (world-domain-y-limit domain)))
        (ecase (outside-domain-occupancy domain x y 0)
          (0 :air)
          (1 :solid))
        (let ((key (chunk-key-at x y)))
          (or (fiber-store-chunk store key)
              (%missing-store-chunk store key))))))

(defun fiber-store-cell-bit (store x y z)
  "The occupancy bit of world cell (X, Y, Z)."
  (if (or (< z 0) (>= z +top-z+))
      0
      (let ((fibers (fiber-store-resolve store x y)))
        (case fibers
          (:air 0)
          (:solid 1)
          (t (fibers-cell-bit fibers x y z))))))

(defun fiber-store-column-clear-p (store x y low high)
  "Whether column (X, Y) is air for LOW <= Z < HIGH."
  (let ((fibers (fiber-store-resolve store x y)))
    (case fibers
      (:air t)
      (:solid (>= (max 0 low) (min +top-z+ high)))
      (t (fibers-column-clear-p fibers x y low high)))))

(defun fiber-store-highest-cell-below (store x y z)
  "The greatest solid Z' < Z in column (X, Y), or NIL."
  (let ((fibers (fiber-store-resolve store x y)))
    (case fibers
      (:air nil)
      (:solid (and (plusp z) (1- (min z +top-z+))))
      (t (fibers-highest-cell-below fibers x y z)))))

(defun fiber-store-lowest-cell-above (store x y z)
  "The least solid Z' >= Z in column (X, Y), or NIL."
  (let ((fibers (fiber-store-resolve store x y)))
    (case fibers
      (:air nil)
      (:solid (and (< z +top-z+) (max z 0)))
      (t (fibers-lowest-cell-above fibers x y z)))))

(defun fiber-store-edit-cell (store x y z bit)
  "Replace the chunk holding (X, Y, Z) with a value whose cell is BIT.
Returns the new fibers.  An absent chunk starts as air."
  (let* ((key (chunk-key-at x y))
         (old (or (fiber-store-chunk store key)
                  (make-chunk-fibers (fiber-store-domain store) key)))
         (new (fibers-with-cell old x y z bit)))
    (setf (fiber-store-chunk store key) new)))
