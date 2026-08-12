;;; Discrete world coordinates, finite chunk domains, and resident block data.

(in-package #:luv)

;;; Coordinate values are deliberately distinct even though all three are
;;; represented by integer triples.  World and chunk coordinates may be
;;; negative; local coordinates are checked by a particular chunk domain.

(defstruct (world-coordinate
             (:constructor %make-world-coordinate (x y z)))
  (x 0 :type integer :read-only t)
  (y 0 :type integer :read-only t)
  (z 0 :type integer :read-only t))

(defun make-world-coordinate (x y z)
  (check-type x integer)
  (check-type y integer)
  (check-type z integer)
  (%make-world-coordinate x y z))

(defstruct (chunk-coordinate
             (:constructor %make-chunk-coordinate (x y z)))
  (x 0 :type integer :read-only t)
  (y 0 :type integer :read-only t)
  (z 0 :type integer :read-only t))

(defun make-chunk-coordinate (x y z)
  (check-type x integer)
  (check-type y integer)
  (check-type z integer)
  (%make-chunk-coordinate x y z))

(defstruct (local-coordinate
             (:constructor %make-local-coordinate (x y z)))
  (x 0 :type integer :read-only t)
  (y 0 :type integer :read-only t)
  (z 0 :type integer :read-only t))

(defun make-local-coordinate (x y z)
  (check-type x integer)
  (check-type y integer)
  (check-type z integer)
  (%make-local-coordinate x y z))

(defstruct (chunk-shape
             (:constructor %make-chunk-shape (width height depth)))
  (width 16 :type (integer 1) :read-only t)
  (height 16 :type (integer 1) :read-only t)
  (depth 16 :type (integer 1) :read-only t))

(defun make-chunk-shape (&key (width 16) (height 16) (depth 16))
  (dolist (dimension (list width height depth))
    (check-type dimension (integer 1)))
  (%make-chunk-shape width height depth))

(defun normalize-cell-extent (extent)
  (let ((components
          (etypecase extent
            (real (list extent extent extent))
            (sequence
             (unless (= (length extent) 3)
               (error "A voxel cell extent needs exactly three components."))
             (coerce extent 'list)))))
    (unless (every (lambda (component)
                     (and (realp component) (plusp component)))
                   components)
      (error "Voxel cell extents must be positive real numbers: ~S" extent))
    (map 'vector (lambda (component) (coerce component 'double-float))
         components)))

(defclass voxel-space ()
  ((id :initarg :id :reader voxel-space-id)
   (chunk-shape :initarg :chunk-shape :reader voxel-space-chunk-shape)
   (cell-extent :initarg :cell-extent :reader voxel-space-cell-extent)))

(defun make-voxel-space (&key (id (gensym "VOXEL-SPACE-"))
                              (chunk-shape (make-chunk-shape))
                              (cell-extent 1d0))
  (check-type chunk-shape chunk-shape)
  (make-instance 'voxel-space
                 :id id
                 :chunk-shape chunk-shape
                 :cell-extent (normalize-cell-extent cell-extent)))

(defun world-coordinate-cell-origin (space coordinate)
  "Map a discrete cell coordinate through SPACE's explicit metric extent."
  (check-type space voxel-space)
  (check-type coordinate world-coordinate)
  (let ((extent (voxel-space-cell-extent space)))
    (vector (* (world-coordinate-x coordinate) (aref extent 0))
            (* (world-coordinate-y coordinate) (aref extent 1))
            (* (world-coordinate-z coordinate) (aref extent 2)))))

(defun world-coordinate-chunk-and-local (space coordinate)
  "Decompose COORDINATE by Euclidean division in SPACE.

The local result is always non-negative, including for negative world
coordinates."
  (check-type space voxel-space)
  (check-type coordinate world-coordinate)
  (let ((shape (voxel-space-chunk-shape space)))
    (multiple-value-bind (chunk-x local-x)
        (floor (world-coordinate-x coordinate) (chunk-shape-width shape))
      (multiple-value-bind (chunk-y local-y)
          (floor (world-coordinate-y coordinate) (chunk-shape-height shape))
        (multiple-value-bind (chunk-z local-z)
            (floor (world-coordinate-z coordinate) (chunk-shape-depth shape))
          (values (make-chunk-coordinate chunk-x chunk-y chunk-z)
                  (make-local-coordinate local-x local-y local-z)))))))

(defun chunk-local-world-coordinate (space chunk local)
  (check-type space voxel-space)
  (check-type chunk chunk-coordinate)
  (check-type local local-coordinate)
  (let ((shape (voxel-space-chunk-shape space)))
    (unless (and (< (local-coordinate-x local) (chunk-shape-width shape))
                 (< (local-coordinate-y local) (chunk-shape-height shape))
                 (< (local-coordinate-z local) (chunk-shape-depth shape))
                 (not (minusp (local-coordinate-x local)))
                 (not (minusp (local-coordinate-y local)))
                 (not (minusp (local-coordinate-z local))))
      (error "Local coordinate ~S is outside chunk shape ~S." local shape))
    (make-world-coordinate
     (+ (* (chunk-coordinate-x chunk) (chunk-shape-width shape))
        (local-coordinate-x local))
     (+ (* (chunk-coordinate-y chunk) (chunk-shape-height shape))
        (local-coordinate-y local))
     (+ (* (chunk-coordinate-z chunk) (chunk-shape-depth shape))
        (local-coordinate-z local)))))

;;; A chunk domain gives one resident materialization a stable site identity
;;; and owns the checked local-coordinate <-> dense-offset correspondence.
;;; X is the contiguous axis: x + width * (y + height * z).

(defclass chunk-domain ()
  ((space :initarg :space :reader chunk-domain-space)
   (coordinate :initarg :coordinate :reader chunk-domain-coordinate)))

(defun make-chunk-domain (space coordinate)
  (check-type space voxel-space)
  (check-type coordinate chunk-coordinate)
  (make-instance 'chunk-domain :space space :coordinate coordinate))

(defun chunk-domain-cardinality (domain)
  (check-type domain chunk-domain)
  (let ((shape (voxel-space-chunk-shape (chunk-domain-space domain))))
    (* (chunk-shape-width shape)
       (chunk-shape-height shape)
       (chunk-shape-depth shape))))

(defun chunk-domain-origin (domain)
  (chunk-local-world-coordinate
   (chunk-domain-space domain)
   (chunk-domain-coordinate domain)
   (make-local-coordinate 0 0 0)))

(defun chunk-domain-local-coordinate-p (domain local)
  (check-type domain chunk-domain)
  (check-type local local-coordinate)
  (let ((shape (voxel-space-chunk-shape (chunk-domain-space domain))))
    (and (<= 0 (local-coordinate-x local))
         (< (local-coordinate-x local) (chunk-shape-width shape))
         (<= 0 (local-coordinate-y local))
         (< (local-coordinate-y local) (chunk-shape-height shape))
         (<= 0 (local-coordinate-z local))
         (< (local-coordinate-z local) (chunk-shape-depth shape)))))

(defun chunk-domain-offset (domain local)
  (unless (chunk-domain-local-coordinate-p domain local)
    (error "Local coordinate ~S is outside domain ~S." local domain))
  (chunk-domain-offset-components
   domain
   (local-coordinate-x local)
   (local-coordinate-y local)
   (local-coordinate-z local)))

(defun chunk-domain-offset-components (domain x y z)
  "Map scalar local coordinates to a dense offset without a row object."
  (check-type domain chunk-domain)
  (let ((shape (voxel-space-chunk-shape (chunk-domain-space domain))))
    (unless (and (typep x 'integer) (<= 0 x) (< x (chunk-shape-width shape))
                 (typep y 'integer) (<= 0 y) (< y (chunk-shape-height shape))
                 (typep z 'integer) (<= 0 z) (< z (chunk-shape-depth shape)))
      (error "Local coordinate (~S ~S ~S) is outside domain ~S."
             x y z domain))
    (+ x
       (* (chunk-shape-width shape)
          (+ y
             (* (chunk-shape-height shape)
                z))))))

(defun chunk-domain-local-coordinate (domain offset)
  (check-type domain chunk-domain)
  (check-type offset (integer 0))
  (unless (< offset (chunk-domain-cardinality domain))
    (error "Offset ~D is outside domain ~S." offset domain))
  (let* ((shape (voxel-space-chunk-shape (chunk-domain-space domain)))
         (width (chunk-shape-width shape))
         (height (chunk-shape-height shape)))
    (multiple-value-bind (z remainder) (floor offset (* width height))
      (multiple-value-bind (y x) (floor remainder width)
        (make-local-coordinate x y z)))))

;;; Block content is a narrow semantic field.  The presentable value is a
;;; shared Lisp object (or NIL for air); the physical column is a dense u16
;;; palette index.  A site is an offset in a domain, not an object with an
;;; identity or allocation of its own.  Computational code should borrow the
;;; aggregate storage below and dispatch once per chunk.  Per-site descriptor
;;; access is kept deliberately loud and allocating for inspectors, sparse
;;; interaction, and other genuinely row-shaped work.

(defclass block-content-column ()
  ((palette :initarg :palette :reader block-content-column-palette)
   (indices :initarg :indices :reader block-content-column-indices)))

(defun make-block-content-column (cardinality)
  (make-instance
   'block-content-column
   :palette (make-array 1 :adjustable t :fill-pointer 1
                          :initial-element nil)
   :indices (make-array cardinality :element-type '(unsigned-byte 16)
                                   :initial-element 0)))

(defun block-content-at-offset (column offset)
  (aref (block-content-column-palette column)
        (aref (block-content-column-indices column) offset)))

(defgeneric borrow-block-content-storage (chunk)
  (:documentation
   "Return DOMAIN, PALETTE, and INDICES borrowed from CHUNK without copying.

This is the ordinary entry point for whole-domain computation.  Generic
dispatch chooses a representation once; the caller then traverses the dense
specialized arrays rather than describing individual cells through CLOS."))

(defmacro with-block-content-storage
    ((domain palette indices) chunk &body body)
  "Execute BODY with the three borrowed block-content storage values."
  `(multiple-value-bind (,domain ,palette ,indices)
       (borrow-block-content-storage ,chunk)
     ,@body))

(defun ensure-block-content-palette-index (column block)
  (or (position block (block-content-column-palette column) :test #'eq)
      (let ((next-index (length (block-content-column-palette column))))
        (unless (<= next-index #xffff)
          (error "A block-content palette cannot contain more than 65536 states."))
        (vector-push-extend block (block-content-column-palette column))
        next-index)))

(defun set-block-content-at-offset (column offset block)
  (let* ((indices (block-content-column-indices column))
         (new-index (ensure-block-content-palette-index column block))
         (old-index (aref indices offset)))
    (unless (= old-index new-index)
      (setf (aref indices offset) new-index)
      t)))

(defclass block-chunk ()
  ((domain :initarg :domain :reader block-chunk-domain)
   (content :initarg :content :reader block-chunk-content)
   (change-hook :initarg :change-hook :initform nil)
   (revision :initform 0 :reader block-chunk-revision)
   ;; -X, +X, -Y, +Y, -Z, +Z.  A derived product for one chunk depends on
   ;; only the opposing boundary revision of each neighbor, not on arbitrary
   ;; edits in that neighbor's interior.
   (boundary-revisions
    :initform (make-array 6 :element-type '(unsigned-byte 64)
                           :initial-element 0))))

(defun make-block-chunk (domain &key change-hook)
  (check-type domain chunk-domain)
  (unless (or (null change-hook) (functionp change-hook))
    (error "A block chunk change hook must be a function or NIL."))
  (make-instance 'block-chunk
                 :domain domain
                 :change-hook change-hook
                 :content (make-block-content-column
                           (chunk-domain-cardinality domain))))

(defmethod borrow-block-content-storage ((chunk block-chunk))
  (let ((column (block-chunk-content chunk)))
    (values (block-chunk-domain chunk)
            (block-content-column-palette column)
            (block-content-column-indices column))))

(defun chunk-block-at (chunk x y z)
  (check-type chunk block-chunk)
  (chunk-block-at-offset
   chunk (chunk-domain-offset-components (block-chunk-domain chunk) x y z)))

(defun chunk-block-at-offset (chunk offset)
  "Read one dense site from CHUNK without constructing a coordinate object."
  (check-type chunk block-chunk)
  (check-type offset (integer 0))
  (unless (< offset (chunk-domain-cardinality (block-chunk-domain chunk)))
    (error "Offset ~D is outside chunk ~S." offset chunk))
  (block-content-at-offset (block-chunk-content chunk) offset))

(defun chunk-boundary-index (dx dy dz)
  (cond ((and (= dx -1) (zerop dy) (zerop dz)) 0)
        ((and (= dx 1) (zerop dy) (zerop dz)) 1)
        ((and (zerop dx) (= dy -1) (zerop dz)) 2)
        ((and (zerop dx) (= dy 1) (zerop dz)) 3)
        ((and (zerop dx) (zerop dy) (= dz -1)) 4)
        ((and (zerop dx) (zerop dy) (= dz 1)) 5)
        (t (error "(~D ~D ~D) is not a block-face direction." dx dy dz))))

(defun block-chunk-boundary-revision (chunk dx dy dz)
  "Return CHUNK's revision for the boundary facing DX,DY,DZ."
  (check-type chunk block-chunk)
  (aref (slot-value chunk 'boundary-revisions)
        (chunk-boundary-index dx dy dz)))

(defun note-chunk-boundary-change (chunk x y z)
  (let* ((shape
           (voxel-space-chunk-shape
            (chunk-domain-space (block-chunk-domain chunk))))
         (revisions (slot-value chunk 'boundary-revisions)))
    (when (zerop x) (incf (aref revisions 0)))
    (when (= x (1- (chunk-shape-width shape))) (incf (aref revisions 1)))
    (when (zerop y) (incf (aref revisions 2)))
    (when (= y (1- (chunk-shape-height shape))) (incf (aref revisions 3)))
    (when (zerop z) (incf (aref revisions 4)))
    (when (= z (1- (chunk-shape-depth shape))) (incf (aref revisions 5)))))

(defun (setf chunk-block-at) (block chunk x y z)
  (check-type chunk block-chunk)
  (setf (chunk-block-at-offset
         chunk
         (chunk-domain-offset-components (block-chunk-domain chunk) x y z))
        block))

(defun (setf chunk-block-at-offset) (block chunk offset)
  "Set one dense site while retaining chunk and boundary invalidation."
  (check-type chunk block-chunk)
  (check-type offset (integer 0))
  (let* ((domain (block-chunk-domain chunk))
         (shape (voxel-space-chunk-shape (chunk-domain-space domain)))
         (width (chunk-shape-width shape))
         (height (chunk-shape-height shape)))
    (unless (< offset (chunk-domain-cardinality domain))
      (error "Offset ~D is outside chunk ~S." offset chunk))
    (when (set-block-content-at-offset
           (block-chunk-content chunk) offset block)
      (incf (slot-value chunk 'revision))
      (multiple-value-bind (z remainder) (floor offset (* width height))
        (multiple-value-bind (y x) (floor remainder width)
          (note-chunk-boundary-change chunk x y z)))
      (let ((change-hook (slot-value chunk 'change-hook)))
        (when change-hook
          (funcall change-hook chunk)))))
  block)

(defun map-chunk-blocks (function chunk)
  "Describe each site to FUNCTION as BLOCK, local X, Y, and Z.

This row-shaped convenience protocol is for presentation and irregular local
work.  Whole-domain algorithms should use WITH-BLOCK-CONTENT-STORAGE."
  (check-type chunk block-chunk)
  (with-block-content-storage (domain palette indices) chunk
    (let ((shape (voxel-space-chunk-shape (chunk-domain-space domain)))
          (offset 0))
      (dotimes (z (chunk-shape-depth shape))
        (dotimes (y (chunk-shape-height shape))
          (dotimes (x (chunk-shape-width shape))
            (funcall function (aref palette (aref indices offset)) x y z)
            (incf offset))))))
  chunk)

;;; A block world is the resident environment, not the complete world
;;; description.  Its residency revision changes only when chunks enter or
;;; leave; its general revision includes residency and content changes; and
;;; each chunk has its own content revision.

(define-condition chunk-not-resident (error)
  ((world-coordinate :initarg :world-coordinate
                     :reader chunk-not-resident-world-coordinate)
   (chunk-coordinate :initarg :chunk-coordinate
                     :reader chunk-not-resident-chunk-coordinate))
  (:report
   (lambda (condition stream)
     (format stream "No chunk is resident for world coordinate ~S (chunk ~S)."
             (chunk-not-resident-world-coordinate condition)
             (chunk-not-resident-chunk-coordinate condition)))))

(defclass block-world ()
  ((space :initarg :space :reader block-world-space)
   (source :initarg :source :initform nil :reader block-world-source)
   (chunks :initform (make-hash-table :test #'equal)
           :reader block-world-chunks)
   (revision :initform 0 :reader block-world-revision)
   (residency-revision :initform 0
                       :reader block-world-residency-revision)
   (change-transaction-depth :initform 0)
   (change-pending-p :initform nil)))

(defun note-block-world-change (world)
  (if (plusp (slot-value world 'change-transaction-depth))
      (setf (slot-value world 'change-pending-p) t)
      (incf (slot-value world 'revision))))

(defun call-with-world-change-transaction (world function)
  "Call FUNCTION and coalesce all WORLD changes into one general revision.

Chunk and residency revisions retain their ordinary precision.  The outermost
transaction advances WORLD's general revision once if any change occurred,
including when FUNCTION exits non-locally after making a partial change."
  (check-type world block-world)
  (check-type function function)
  (incf (slot-value world 'change-transaction-depth))
  (unwind-protect
       (funcall function)
    (decf (slot-value world 'change-transaction-depth))
    (when (and (zerop (slot-value world 'change-transaction-depth))
               (slot-value world 'change-pending-p))
      (setf (slot-value world 'change-pending-p) nil)
      (incf (slot-value world 'revision)))))

(defmacro with-world-change-transaction ((world) &body body)
  `(call-with-world-change-transaction ,world (lambda () ,@body)))

(defun make-block-world (&key (id (gensym "BLOCK-WORLD-"))
                              (chunk-width 16)
                              (chunk-height 16)
                              (chunk-depth 16)
                              (cell-extent 1d0)
                              source)
  (make-instance
   'block-world
   :source source
   :space (make-voxel-space
           :id id
           :chunk-shape (make-chunk-shape :width chunk-width
                                          :height chunk-height
                                          :depth chunk-depth)
           :cell-extent cell-extent)))

(defun chunk-key (x y z)
  (check-type x integer)
  (check-type y integer)
  (check-type z integer)
  (list x y z))

(defun world-chunk-at (world x y z)
  "Return the resident chunk at chunk coordinate X,Y,Z and whether it exists."
  (check-type world block-world)
  (gethash (chunk-key x y z) (block-world-chunks world)))

(defun ensure-world-chunk (world x y z)
  "Return the resident chunk at X,Y,Z, creating an all-air chunk if absent."
  (multiple-value-bind (chunk present-p) (world-chunk-at world x y z)
    (if present-p
        chunk
        (let* ((coordinate (make-chunk-coordinate x y z))
               (domain (make-chunk-domain (block-world-space world) coordinate))
               (new-chunk
                 (make-block-chunk
                  domain
                  :change-hook
                  (lambda (changed-chunk)
                    (declare (ignore changed-chunk))
                    (note-block-world-change world)))))
          (setf (gethash (chunk-key x y z) (block-world-chunks world))
                new-chunk)
          (incf (slot-value world 'residency-revision))
          (note-block-world-change world)
          new-chunk))))

(defun remove-world-chunk (world x y z)
  "Remove a resident chunk.  Return the chunk and whether it was present."
  (multiple-value-bind (chunk present-p)
      (gethash (chunk-key x y z) (block-world-chunks world))
    (when present-p
      ;; A removed chunk may outlive its residency in an inspector or cache,
      ;; but it no longer owns the right to invalidate this world.
      (setf (slot-value chunk 'change-hook) nil)
      (remhash (chunk-key x y z) (block-world-chunks world))
      (incf (slot-value world 'residency-revision))
      (note-block-world-change world))
    (values chunk present-p)))

(defun resident-world-chunks (world)
  "Return resident chunks in deterministic chunk-coordinate order."
  (check-type world block-world)
  (sort (loop for chunk being the hash-values of (block-world-chunks world)
              collect chunk)
        (lambda (left right)
          (let ((a (chunk-domain-coordinate (block-chunk-domain left)))
                (b (chunk-domain-coordinate (block-chunk-domain right))))
            (or (< (chunk-coordinate-x a) (chunk-coordinate-x b))
                (and (= (chunk-coordinate-x a) (chunk-coordinate-x b))
                     (or (< (chunk-coordinate-y a) (chunk-coordinate-y b))
                         (and (= (chunk-coordinate-y a) (chunk-coordinate-y b))
                              (< (chunk-coordinate-z a)
                                 (chunk-coordinate-z b))))))))))

(defgeneric describe-block-allocatingly (world x y z)
  (:documentation
   "Describe one site as BLOCK and :RESIDENT, or NIL and :ABSENT.

This intentionally conspicuous row API constructs coordinate descriptors and
a chunk lookup key.  It is appropriate for inspectors, ray hits, and sparse
interaction.  Algorithms over many cells should select a chunk/domain once
and use WITH-BLOCK-CONTENT-STORAGE instead."))

(defgeneric (setf describe-block-allocatingly) (block world x y z))

(defun locate-world-coordinate (world x y z)
  (let ((coordinate (make-world-coordinate x y z)))
    (multiple-value-bind (chunk-coordinate local-coordinate)
        (world-coordinate-chunk-and-local (block-world-space world) coordinate)
      (values coordinate chunk-coordinate local-coordinate))))

(defmethod describe-block-allocatingly ((world block-world) x y z)
  (multiple-value-bind (world-coordinate chunk-coordinate local-coordinate)
      (locate-world-coordinate world x y z)
    (declare (ignore world-coordinate))
    (multiple-value-bind (chunk present-p)
        (world-chunk-at world
                        (chunk-coordinate-x chunk-coordinate)
                        (chunk-coordinate-y chunk-coordinate)
                        (chunk-coordinate-z chunk-coordinate))
      (if present-p
          (values (chunk-block-at chunk
                                  (local-coordinate-x local-coordinate)
                                  (local-coordinate-y local-coordinate)
                                  (local-coordinate-z local-coordinate))
                  :resident)
          (values nil :absent)))))

(defmethod (setf describe-block-allocatingly)
    (block (world block-world) x y z)
  (multiple-value-bind (world-coordinate chunk-coordinate local-coordinate)
      (locate-world-coordinate world x y z)
    (multiple-value-bind (chunk present-p)
        (world-chunk-at world
                        (chunk-coordinate-x chunk-coordinate)
                        (chunk-coordinate-y chunk-coordinate)
                        (chunk-coordinate-z chunk-coordinate))
      (unless present-p
        (error 'chunk-not-resident
               :world-coordinate world-coordinate
               :chunk-coordinate chunk-coordinate))
      (setf (chunk-block-at chunk
                            (local-coordinate-x local-coordinate)
                            (local-coordinate-y local-coordinate)
                            (local-coordinate-z local-coordinate))
            block))))

;;; Sparse edits are an overlay on a materialized world, not another dense
;;; chunk field.  NIL is a meaningful stored value (an explicitly removed
;;; block), so hash-table presence distinguishes it from no edit.

(defclass block-edit-overlay ()
  ((entries :initform (make-hash-table :test #'equal)
            :reader block-edit-overlay-entries)))

(defun make-block-edit-overlay ()
  (make-instance 'block-edit-overlay))

(defun block-edit-overlay-count (overlay)
  (hash-table-count (block-edit-overlay-entries overlay)))

(defun record-block-edit (overlay block x y z)
  "Record BLOCK as the explicit value for world site X,Y,Z."
  (check-type overlay block-edit-overlay)
  (let ((coordinate (make-world-coordinate x y z)))
    (setf (gethash (list (world-coordinate-x coordinate)
                         (world-coordinate-y coordinate)
                         (world-coordinate-z coordinate))
                   (block-edit-overlay-entries overlay))
          block))
  block)

(defun block-edit-at (overlay x y z)
  "Return the edited value at X,Y,Z and whether an edit is present."
  (check-type overlay block-edit-overlay)
  (let ((coordinate (make-world-coordinate x y z)))
    (gethash (list (world-coordinate-x coordinate)
                   (world-coordinate-y coordinate)
                   (world-coordinate-z coordinate))
             (block-edit-overlay-entries overlay))))

(defun same-chunk-coordinate-p (left right)
  (and (= (chunk-coordinate-x left) (chunk-coordinate-x right))
       (= (chunk-coordinate-y left) (chunk-coordinate-y right))
       (= (chunk-coordinate-z left) (chunk-coordinate-z right))))

(defun apply-block-edits-to-chunk (overlay world chunk)
  "Apply every edit in OVERLAY addressed to resident CHUNK."
  (check-type overlay block-edit-overlay)
  (check-type world block-world)
  (check-type chunk block-chunk)
  (let ((target (chunk-domain-coordinate (block-chunk-domain chunk))))
    (with-world-change-transaction (world)
      (maphash
       (lambda (key block)
         (destructuring-bind (x y z) key
           (multiple-value-bind (coordinate local)
               (world-coordinate-chunk-and-local
                (block-world-space world) (make-world-coordinate x y z))
             (declare (ignore local))
             (when (same-chunk-coordinate-p coordinate target)
               (setf (describe-block-allocatingly world x y z) block)))))
       (block-edit-overlay-entries overlay))))
  chunk)

;;; Ray traversal is expressed in continuous lattice coordinates: integer
;;; planes are cell boundaries, independently of the physical cell extent.
;;; The traversal stops at absent terrain rather than silently seeing air.

(defstruct (block-ray-hit
             (:constructor %make-block-ray-hit
                 (coordinate adjacent-coordinate block distance)))
  (coordinate nil :type world-coordinate :read-only t)
  (adjacent-coordinate nil :type (or null world-coordinate) :read-only t)
  (block nil :read-only t)
  (distance 0d0 :type double-float :read-only t))

(defun ray-component (sequence index)
  (unless (and (typep sequence 'sequence) (= (length sequence) 3))
    (error "A block-world ray needs a three-component sequence: ~S" sequence))
  (let ((component (elt sequence index)))
    (check-type component real)
    (coerce component 'double-float)))

(defun raycast-block-world
    (world origin direction occupied-p &key (max-distance 8d0))
  "Trace a ray through WORLD's resident lattice.

Return a BLOCK-RAY-HIT and :HIT, NIL and :ABSENT when traversal reaches a
non-resident chunk, or NIL and :MISS.  ORIGIN and DIRECTION are
three-component sequences in continuous cell coordinates."
  (check-type world block-world)
  (check-type max-distance (real 0))
  (unless (functionp occupied-p)
    (error "OCCUPIED-P must be a function."))
  (let* ((origin-x (ray-component origin 0))
         (origin-y (ray-component origin 1))
         (origin-z (ray-component origin 2))
         (raw-x (ray-component direction 0))
         (raw-y (ray-component direction 1))
         (raw-z (ray-component direction 2))
         (magnitude (sqrt (+ (* raw-x raw-x)
                             (* raw-y raw-y)
                             (* raw-z raw-z)))))
    (unless (plusp magnitude)
      (error "A block-world ray direction must be non-zero."))
    (let* ((direction-x (/ raw-x magnitude))
           (direction-y (/ raw-y magnitude))
           (direction-z (/ raw-z magnitude))
           (cell-x (floor origin-x))
           (cell-y (floor origin-y))
           (cell-z (floor origin-z))
           (step-x (cond ((plusp direction-x) 1)
                         ((minusp direction-x) -1)
                         (t 0)))
           (step-y (cond ((plusp direction-y) 1)
                         ((minusp direction-y) -1)
                         (t 0)))
           (step-z (cond ((plusp direction-z) 1)
                         ((minusp direction-z) -1)
                         (t 0)))
           (infinity most-positive-double-float)
           (delta-x (if (zerop step-x) infinity (abs (/ direction-x))))
           (delta-y (if (zerop step-y) infinity (abs (/ direction-y))))
           (delta-z (if (zerop step-z) infinity (abs (/ direction-z))))
           (boundary-x (if (plusp step-x) (1+ cell-x) cell-x))
           (boundary-y (if (plusp step-y) (1+ cell-y) cell-y))
           (boundary-z (if (plusp step-z) (1+ cell-z) cell-z))
           (next-x (if (zerop step-x)
                       infinity
                       (/ (- boundary-x origin-x) direction-x)))
           (next-y (if (zerop step-y)
                       infinity
                       (/ (- boundary-y origin-y) direction-y)))
           (next-z (if (zerop step-z)
                       infinity
                       (/ (- boundary-z origin-z) direction-z)))
           (distance 0d0)
           (adjacent nil))
      (loop
        (multiple-value-bind (block status)
            (describe-block-allocatingly world cell-x cell-y cell-z)
          (when (eq status :absent)
            (return (values nil :absent)))
          (when (funcall occupied-p block)
            (return
              (values
               (%make-block-ray-hit
                (make-world-coordinate cell-x cell-y cell-z)
                adjacent block (coerce distance 'double-float))
               :hit))))
        (let ((next-distance (min next-x next-y next-z)))
          (when (> next-distance max-distance)
            (return (values nil :miss)))
          (setf adjacent (make-world-coordinate cell-x cell-y cell-z)
                distance next-distance)
          (cond
            ((and (<= next-x next-y) (<= next-x next-z))
             (incf cell-x step-x)
             (incf next-x delta-x))
            ((and (<= next-y next-x) (<= next-y next-z))
             (incf cell-y step-y)
             (incf next-y delta-y))
            (t
             (incf cell-z step-z)
             (incf next-z delta-z))))))))
