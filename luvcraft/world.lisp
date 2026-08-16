;;; Discrete world coordinates, finite chunk domains, and resident block data.

(in-package #:luvcraft.world)

;;; Coordinate values are deliberately distinct even though all three carry
;;; integer components.  World, chunk, and domain-local addresses are not
;;; interchangeable.  World and chunk coordinates may be negative; local
;;; coordinates are checked by a particular chunk domain.

(declaim (inline %make-world-coordinate))
(defstruct (world-coordinate
             (:constructor %make-world-coordinate (x y z)))
  (x 0 :type integer :read-only t)
  (y 0 :type integer :read-only t)
  (z 0 :type integer :read-only t))

(declaim (inline make-world-coordinate))
(defun make-world-coordinate (x y z)
  (check-type x integer)
  (check-type y integer)
  (check-type z integer)
  (%make-world-coordinate x y z))

(declaim (inline %make-chunk-coordinate))
(defstruct (chunk-coordinate
             (:constructor %make-chunk-coordinate (x y z)))
  (x 0 :type integer :read-only t)
  (y 0 :type integer :read-only t)
  (z 0 :type integer :read-only t))

(declaim (inline make-chunk-coordinate))
(defun make-chunk-coordinate (x y z)
  (check-type x integer)
  (check-type y integer)
  (check-type z integer)
  (%make-chunk-coordinate x y z))

(declaim (inline %make-local-coordinate))
(defstruct (local-coordinate
             (:constructor %make-local-coordinate (x y z)))
  (x 0 :type integer :read-only t)
  (y 0 :type integer :read-only t)
  (z 0 :type integer :read-only t))

(declaim (inline make-local-coordinate))
(defun make-local-coordinate (x y z)
  (check-type x integer)
  (check-type y integer)
  (check-type z integer)
  (%make-local-coordinate x y z))

;;; A primitive direction is a displacement, not any of the affine coordinate
;;; kinds above.  The six shared instances are safe to retain indefinitely;
;;; temporary coordinates translated by them may instead be DYNAMIC-EXTENT.

(declaim (inline %make-voxel-direction))
(defstruct (voxel-direction
             (:constructor %make-voxel-direction (dx dy dz)))
  (dx 0 :type (integer -1 1) :read-only t)
  (dy 0 :type (integer -1 1) :read-only t)
  (dz 0 :type (integer -1 1) :read-only t))

(declaim (inline make-voxel-direction))
(defun make-voxel-direction (dx dy dz)
  (unless (and (integerp dx) (integerp dy) (integerp dz)
               (= 1 (+ (abs dx) (abs dy) (abs dz))))
    (error "(~S ~S ~S) is not a primitive voxel direction." dx dy dz))
  (%make-voxel-direction dx dy dz))

(defparameter +voxel-negative-x+ (make-voxel-direction -1 0 0))
(defparameter +voxel-positive-x+ (make-voxel-direction 1 0 0))
(defparameter +voxel-negative-y+ (make-voxel-direction 0 -1 0))
(defparameter +voxel-positive-y+ (make-voxel-direction 0 1 0))
(defparameter +voxel-negative-z+ (make-voxel-direction 0 0 -1))
(defparameter +voxel-positive-z+ (make-voxel-direction 0 0 1))

(defparameter *voxel-face-directions*
  (list +voxel-negative-x+ +voxel-positive-x+
        +voxel-negative-y+ +voxel-positive-y+
        +voxel-negative-z+ +voxel-positive-z+))

(declaim (inline world-coordinate-neighbor chunk-coordinate-neighbor))
(defun world-coordinate-neighbor (coordinate direction)
  "Translate a world cell address by one primitive DIRECTION."
  (check-type coordinate world-coordinate)
  (check-type direction voxel-direction)
  (make-world-coordinate
   (+ (world-coordinate-x coordinate) (voxel-direction-dx direction))
   (+ (world-coordinate-y coordinate) (voxel-direction-dy direction))
   (+ (world-coordinate-z coordinate) (voxel-direction-dz direction))))

(defun chunk-coordinate-neighbor (coordinate direction)
  "Translate a chunk address by one primitive DIRECTION."
  (check-type coordinate chunk-coordinate)
  (check-type direction voxel-direction)
  (make-chunk-coordinate
   (+ (chunk-coordinate-x coordinate) (voxel-direction-dx direction))
   (+ (chunk-coordinate-y coordinate) (voxel-direction-dy direction))
   (+ (chunk-coordinate-z coordinate) (voxel-direction-dz direction))))

(defun opposite-voxel-direction (direction)
  (check-type direction voxel-direction)
  (cond ((eq direction +voxel-negative-x+) +voxel-positive-x+)
        ((eq direction +voxel-positive-x+) +voxel-negative-x+)
        ((eq direction +voxel-negative-y+) +voxel-positive-y+)
        ((eq direction +voxel-positive-y+) +voxel-negative-y+)
        ((eq direction +voxel-negative-z+) +voxel-positive-z+)
        ((eq direction +voxel-positive-z+) +voxel-negative-z+)
        (t (make-voxel-direction (- (voxel-direction-dx direction))
                                 (- (voxel-direction-dy direction))
                                 (- (voxel-direction-dz direction))))))

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
            (vec3 (vec3-list extent))
            (sequence
             (unless (= (length extent) 3)
               (error "A voxel cell extent needs exactly three components."))
             (coerce extent 'list)))))
    (unless (every (lambda (component)
                     (and (realp component) (plusp component)))
                   components)
      (error "Voxel cell extents must be positive real numbers: ~S" extent))
    (apply #'make-vec3
           (mapcar (lambda (component) (coerce component 'double-float))
                   components))))

(defclass voxel-space ()
  ((id :initarg :id :reader voxel-space-id)
   (chunk-shape :initarg :chunk-shape :reader voxel-space-chunk-shape)
   (cell-extent
    :initarg :cell-extent
    :type vec3
    :quantity (:quantity :voxel-cell-extent
               :unit ((:metre 1) (:cell -1))
               :tensor-order 1)
    :reader voxel-space-cell-extent))
  (:metaclass luv.arithmetic.records:quantity-class))

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
    (make-vec3 (* (world-coordinate-x coordinate) (vec3-x extent))
               (* (world-coordinate-y coordinate) (vec3-y extent))
               (* (world-coordinate-z coordinate) (vec3-z extent)))))

(defun voxel-space-decompose-components (space x y z)
  "Decompose a world site into chunk and local scalar components.

Return CHUNK-X, CHUNK-Y, CHUNK-Z, LOCAL-X, LOCAL-Y, and LOCAL-Z without
constructing coordinate objects.  Euclidean division keeps every local
component non-negative, including for negative world coordinates.  This is
the dense traversal counterpart of WORLD-COORDINATE-CHUNK-AND-LOCAL."
  (check-type space voxel-space)
  (check-type x integer)
  (check-type y integer)
  (check-type z integer)
  (let ((shape (voxel-space-chunk-shape space)))
    (multiple-value-bind (chunk-x local-x) (floor x (chunk-shape-width shape))
      (multiple-value-bind (chunk-y local-y)
          (floor y (chunk-shape-height shape))
        (multiple-value-bind (chunk-z local-z)
            (floor z (chunk-shape-depth shape))
          (values chunk-x chunk-y chunk-z local-x local-y local-z))))))

(declaim (inline world-coordinate-chunk-and-local))
(defun world-coordinate-chunk-and-local (space coordinate)
  "Decompose COORDINATE by Euclidean division in SPACE.

The local result is always non-negative, including for negative world
coordinates."
  (check-type space voxel-space)
  (check-type coordinate world-coordinate)
  (multiple-value-bind (chunk-x chunk-y chunk-z local-x local-y local-z)
      (voxel-space-decompose-components
       space
       (world-coordinate-x coordinate)
       (world-coordinate-y coordinate)
       (world-coordinate-z coordinate))
    (values (make-chunk-coordinate chunk-x chunk-y chunk-z)
            (make-local-coordinate local-x local-y local-z))))

(declaim (inline chunk-local-world-coordinate))
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

(defmethod domains:domain-cardinality ((domain chunk-domain))
  (let ((shape (voxel-space-chunk-shape (chunk-domain-space domain))))
    (* (chunk-shape-width shape)
       (chunk-shape-height shape)
       (chunk-shape-depth shape))))

(defun chunk-domain-cardinality (domain)
  (domains:domain-cardinality domain))

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

(declaim (inline chunk-domain-local-coordinate))
(defun chunk-domain-local-coordinate (domain offset)
  (multiple-value-bind (x y z)
      (chunk-domain-local-components domain offset)
    (make-local-coordinate x y z)))

(declaim (inline chunk-domain-world-coordinate))
(defun chunk-domain-world-coordinate (domain local)
  "Return the world coordinate of LOCAL in DOMAIN."
  (check-type domain chunk-domain)
  (check-type local local-coordinate)
  (chunk-local-world-coordinate
   (chunk-domain-space domain) (chunk-domain-coordinate domain) local))

(defun chunk-domain-local-components (domain offset)
  "Map a dense offset to local scalar components without a row object."
  (check-type domain chunk-domain)
  (check-type offset (integer 0))
  (unless (< offset (chunk-domain-cardinality domain))
    (error "Offset ~D is outside domain ~S." offset domain))
  (let* ((shape (voxel-space-chunk-shape (chunk-domain-space domain)))
         (width (chunk-shape-width shape))
         (height (chunk-shape-height shape)))
    (multiple-value-bind (z remainder) (floor offset (* width height))
      (multiple-value-bind (y x) (floor remainder width)
        (values x y z)))))

(defun chunk-domain-world-components (domain local-x local-y local-z)
  "Map local scalar components to world scalar components without objects."
  ;; Use the checked offset operation as the single local-bounds authority.
  (chunk-domain-offset-components domain local-x local-y local-z)
  (let* ((shape (voxel-space-chunk-shape (chunk-domain-space domain)))
         (coordinate (chunk-domain-coordinate domain)))
    (values (+ (* (chunk-coordinate-x coordinate)
                  (chunk-shape-width shape))
               local-x)
            (+ (* (chunk-coordinate-y coordinate)
                  (chunk-shape-height shape))
               local-y)
            (+ (* (chunk-coordinate-z coordinate)
                  (chunk-shape-depth shape))
               local-z))))

(defmacro with-chunk-domain-step
    ((offset destination crossing) domain local direction &body body)
  "Execute BODY for one primitive step from LOCAL inside DOMAIN.

OFFSET, DESTINATION, and CROSSING are bound as by STEP-CHUNK-DOMAIN-SITE, but
DESTINATION has dynamic extent and must be copied before BODY retains it.  The
compiler-visible lifetime keeps rejected adjacency probes off the heap. #E4T0PD"
  (let ((domain-value (gensym "DOMAIN"))
        (local-value (gensym "LOCAL"))
        (direction-value (gensym "DIRECTION"))
        (shape (gensym "SHAPE"))
        (width (gensym "WIDTH"))
        (height (gensym "HEIGHT"))
        (depth (gensym "DEPTH"))
        (crossing-x (gensym "CROSSING-X"))
        (crossing-y (gensym "CROSSING-Y"))
        (crossing-z (gensym "CROSSING-Z"))
        (local-x (gensym "LOCAL-X"))
        (local-y (gensym "LOCAL-Y"))
        (local-z (gensym "LOCAL-Z")))
    `(let* ((,domain-value ,domain)
            (,local-value ,local)
            (,direction-value ,direction))
       (check-type ,local-value local-coordinate)
       (check-type ,direction-value voxel-direction)
       (chunk-domain-offset ,domain-value ,local-value)
       (let* ((,shape
                (voxel-space-chunk-shape
                 (chunk-domain-space ,domain-value)))
              (,width (chunk-shape-width ,shape))
              (,height (chunk-shape-height ,shape))
              (,depth (chunk-shape-depth ,shape)))
         (multiple-value-bind (,crossing-x ,local-x)
             (floor (+ (local-coordinate-x ,local-value)
                       (voxel-direction-dx ,direction-value))
                    ,width)
           (multiple-value-bind (,crossing-y ,local-y)
               (floor (+ (local-coordinate-y ,local-value)
                         (voxel-direction-dy ,direction-value))
                      ,height)
             (multiple-value-bind (,crossing-z ,local-z)
                 (floor (+ (local-coordinate-z ,local-value)
                           (voxel-direction-dz ,direction-value))
                        ,depth)
               (let ((,destination
                       (make-local-coordinate ,local-x ,local-y ,local-z))
                     (,offset
                       (+ ,local-x
                          (* ,width (+ ,local-y (* ,height ,local-z)))))
                     (,crossing
                       (and (not (zerop (+ (abs ,crossing-x)
                                           (abs ,crossing-y)
                                           (abs ,crossing-z))))
                            ,direction-value)))
                 (declare (dynamic-extent ,destination))
                 ,@body))))))))

(declaim (inline step-chunk-domain-site))
(defun step-chunk-domain-site (domain local direction)
  "Step from one local site in a primitive face direction.

Return the destination OFFSET and wrapped LOCAL-COORDINATE, followed by
DIRECTION when the step crosses into that adjacent chunk or NIL when it stays
inside DOMAIN."
  (with-chunk-domain-step (offset destination crossing)
      domain local direction
    (values offset (copy-local-coordinate destination) crossing)))

(defgeneric locate-chunk-window-site (window x y z)
  (:documentation
   "Resolve world site X,Y,Z through WINDOW.

Return (VALUES MATERIALIZATION OFFSET AVAILABILITY).  AVAILABILITY is
:AVAILABLE or :UNAVAILABLE; field and subsystem policy must interpret that
fact rather than making the spatial protocol call absence air, solid, zero,
or open sky.  Implementations keep their existing aggregate representation."))

(defmacro with-chunk-window-step
    ((offset destination crossing materialization availability)
     window domain local direction
     &body body)
  "Execute BODY for one DOMAIN step continued through WINDOW when necessary.

DESTINATION has dynamic extent.  MATERIALIZATION and AVAILABILITY are selected
through LOCATE-CHUNK-WINDOW-SITE only for a boundary crossing. #YUBB7X"
  (let ((window-value (gensym "WINDOW"))
        (domain-value (gensym "DOMAIN"))
        (local-value (gensym "LOCAL"))
        (direction-value (gensym "DIRECTION"))
        (local-offset (gensym "LOCAL-OFFSET"))
        (world-x (gensym "WORLD-X"))
        (world-y (gensym "WORLD-Y"))
        (world-z (gensym "WORLD-Z")))
    `(let ((,window-value ,window)
           (,domain-value ,domain)
           (,local-value ,local)
           (,direction-value ,direction))
       (with-chunk-domain-step (,local-offset ,destination ,crossing)
           ,domain-value ,local-value ,direction-value
         (multiple-value-bind (,materialization ,offset ,availability)
             (if ,crossing
                 (multiple-value-bind (,world-x ,world-y ,world-z)
                     (chunk-domain-world-components
                      ,domain-value
                      (local-coordinate-x ,local-value)
                      (local-coordinate-y ,local-value)
                      (local-coordinate-z ,local-value))
                   (locate-chunk-window-site
                    ,window-value
                    (+ ,world-x (voxel-direction-dx ,direction-value))
                    (+ ,world-y (voxel-direction-dy ,direction-value))
                    (+ ,world-z (voxel-direction-dz ,direction-value))))
                 (values nil ,local-offset :local))
           ,@body)))))

(defmacro do-chunk-window-neighbors
    ((offset destination crossing direction materialization availability
      window domain local directions &optional result)
     &body body)
  "Execute BODY for the DIRECTIONS neighboring LOCAL through WINDOW.

DIRECTIONS is the caller's explicit neighborhood policy.  DESTINATION has
dynamic extent on each iteration and must be copied before BODY retains it.
Interior steps remain domain arithmetic; window dispatch occurs only at an
actual chunk crossing. #E4T0PD"
  (let ((window-value (gensym "WINDOW"))
        (domain-value (gensym "DOMAIN"))
        (local-value (gensym "LOCAL"))
        (directions-value (gensym "DIRECTIONS")))
    `(let ((,window-value ,window)
           (,domain-value ,domain)
           (,local-value ,local)
           (,directions-value ,directions))
       (dolist (,direction ,directions-value ,result)
         (with-chunk-window-step
             (,offset ,destination ,crossing ,materialization ,availability)
             ,window-value ,domain-value ,local-value ,direction
           ,@body)))))

(defun continue-chunk-window-site (window domain local direction)
  "Step LOCAL and resolve WINDOW only when the step crosses DOMAIN.

Return the destination OFFSET, wrapped LOCAL-COORDINATE, and crossing as
STEP-CHUNK-DOMAIN-SITE does, followed by the neighboring MATERIALIZATION and
its AVAILABILITY.  A local step returns NIL and :LOCAL for the last two values.
The one generic window decision therefore occurs only at an aggregate
boundary, never for every site in a dense traversal. #L84JCX"
  (with-chunk-window-step
      (offset destination crossing materialization availability)
      window domain local direction
    (values offset (copy-local-coordinate destination) crossing
            materialization availability)))

(defmacro do-chunk-domain-sites
    ((offset local domain &optional result) &body body)
  "Execute BODY for every site in DOMAIN, in dense storage order.

LOCAL is a DYNAMIC-EXTENT LOCAL-COORDINATE and must not be retained after
BODY returns.  This gives dense algorithms a nominal coordinate without one
heap allocation per site.  See #B3UEVE."
  (let ((domain-value (gensym "DOMAIN"))
        (shape (gensym "SHAPE"))
        (width (gensym "WIDTH"))
        (height (gensym "HEIGHT"))
        (depth (gensym "DEPTH"))
        (x (gensym "X"))
        (y (gensym "Y"))
        (z (gensym "Z")))
    `(let* ((,domain-value ,domain)
            (,shape (voxel-space-chunk-shape
                     (chunk-domain-space ,domain-value)))
            (,width (chunk-shape-width ,shape))
            (,height (chunk-shape-height ,shape))
            (,depth (chunk-shape-depth ,shape))
            (,offset 0))
       (dotimes (,z ,depth)
         (dotimes (,y ,height)
           (dotimes (,x ,width)
             (let ((,local (make-local-coordinate ,x ,y ,z)))
               (declare (dynamic-extent ,local))
               ,@body)
             (incf ,offset))))
       ,result)))

(defmacro do-chunk-domain-face
    ((offset local domain direction &optional result) &body body)
  "Execute BODY for every site on DIRECTION's boundary face in DOMAIN.

LOCAL is a DYNAMIC-EXTENT LOCAL-COORDINATE and must not be retained after
BODY returns."
  (let ((domain-value (gensym "DOMAIN"))
        (direction-value (gensym "DIRECTION"))
        (shape (gensym "SHAPE"))
        (width (gensym "WIDTH"))
        (height (gensym "HEIGHT"))
        (depth (gensym "DEPTH"))
        (x-low (gensym "X-LOW"))
        (x-high (gensym "X-HIGH"))
        (y-low (gensym "Y-LOW"))
        (y-high (gensym "Y-HIGH"))
        (z-low (gensym "Z-LOW"))
        (z-high (gensym "Z-HIGH"))
        (x (gensym "X"))
        (y (gensym "Y"))
        (z (gensym "Z")))
    `(let* ((,domain-value ,domain)
            (,direction-value ,direction)
            (,shape (voxel-space-chunk-shape
                     (chunk-domain-space ,domain-value)))
            (,width (chunk-shape-width ,shape))
            (,height (chunk-shape-height ,shape))
            (,depth (chunk-shape-depth ,shape)))
       (check-type ,direction-value voxel-direction)
       (let ((,x-low (if (= (voxel-direction-dx ,direction-value) 1)
                         (1- ,width) 0))
             (,x-high (if (= (voxel-direction-dx ,direction-value) -1)
                          0 (1- ,width)))
             (,y-low (if (= (voxel-direction-dy ,direction-value) 1)
                         (1- ,height) 0))
             (,y-high (if (= (voxel-direction-dy ,direction-value) -1)
                          0 (1- ,height)))
             (,z-low (if (= (voxel-direction-dz ,direction-value) 1)
                         (1- ,depth) 0))
             (,z-high (if (= (voxel-direction-dz ,direction-value) -1)
                          0 (1- ,depth))))
         (loop for ,z from ,z-low to ,z-high do
           (loop for ,y from ,y-low to ,y-high do
             (loop for ,x from ,x-low to ,x-high do
               (let ((,offset (+ ,x (* ,width (+ ,y (* ,height ,z)))))
                     (,local (make-local-coordinate ,x ,y ,z)))
                 (declare (dynamic-extent ,local))
                 ,@body))))
         ,result))))

;;; Block content is a narrow semantic field.  The presentable value is a
;;; shared Lisp object (or NIL for air); the physical column is a dense u16
;;; palette index.  A site is an offset in a domain, not an object with an
;;; identity or allocation of its own.  Computational code should borrow the
;;; aggregate storage below and dispatch once per chunk.  Single-site world
;;; access still resolves through coordinate descriptors and a chunk key; it
;;; is for inspectors, sparse interaction, and other genuinely row-shaped work.

(defclass block-content-column ()
  ((definition
    :initarg :definition
    :initform (luvcraft.world.fields:field-definition-for :block-content)
    :reader block-content-column-definition)
   (palette :initarg :palette :reader block-content-column-palette)
   (indices :initarg :indices :reader block-content-column-indices)))

(defmethod luvcraft.world.fields:materialized-field-definition
    ((column block-content-column) (field-name (eql :block-content)))
  (declare (ignore field-name))
  (block-content-column-definition column))

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
   (incarnation :initarg :incarnation :initform 0
                :reader block-chunk-incarnation)
   (change-hook :initarg :change-hook :initform nil)
   (revision :initform 0 :reader block-chunk-revision)
   ;; -X, +X, -Y, +Y, -Z, +Z.  A derived product for one chunk depends on
   ;; only the opposing boundary revision of each neighbor, not on arbitrary
   ;; edits in that neighbor's interior.
   (boundary-revisions
    :initform (make-array 6 :element-type '(unsigned-byte 64)
                           :initial-element 0))
   ;; The chunk's derived voxel light, revised independently from block
   ;; content.  NIL until a lighting solver publishes a field.
   (light-field :initform nil :accessor block-chunk-light-field)))

(defun make-block-chunk (domain &key change-hook (incarnation 0) content)
  (check-type domain chunk-domain)
  (unless (or (null change-hook) (functionp change-hook))
    (error "A block chunk change hook must be a function or NIL."))
  (make-instance 'block-chunk
                 :domain domain
                 :incarnation incarnation
                 :change-hook change-hook
                 :content (or content
                              (make-block-content-column
                               (chunk-domain-cardinality domain)))))

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

(defun chunk-boundary-index (direction)
  (check-type direction voxel-direction)
  (let ((dx (voxel-direction-dx direction))
        (dy (voxel-direction-dy direction))
        (dz (voxel-direction-dz direction)))
    (cond ((and (= dx -1) (zerop dy) (zerop dz)) 0)
          ((and (= dx 1) (zerop dy) (zerop dz)) 1)
          ((and (zerop dx) (= dy -1) (zerop dz)) 2)
          ((and (zerop dx) (= dy 1) (zerop dz)) 3)
          ((and (zerop dx) (zerop dy) (= dz -1)) 4)
          ((and (zerop dx) (zerop dy) (= dz 1)) 5)
          (t (error "~S is not a block-face direction." direction)))))

(defun block-chunk-boundary-revision (chunk direction)
  "Return CHUNK's revision for the boundary facing DIRECTION."
  (check-type chunk block-chunk)
  (aref (slot-value chunk 'boundary-revisions)
        (chunk-boundary-index direction)))

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
  (let ((domain (block-chunk-domain chunk)))
    (unless (< offset (chunk-domain-cardinality domain))
      (error "Offset ~D is outside chunk ~S." offset chunk))
    (when (set-block-content-at-offset
           (block-chunk-content chunk) offset block)
      (incf (slot-value chunk 'revision))
      (multiple-value-bind (x y z)
          (chunk-domain-local-components domain offset)
        (note-chunk-boundary-change chunk x y z)
        ;; The hook receives the edited site so derived domains such as
        ;; lighting can react to the cell, not merely the chunk.
        (let ((change-hook (slot-value chunk 'change-hook)))
          (when change-hook
            (funcall change-hook chunk x y z))))))
  block)

(defun map-chunk-blocks (function chunk)
  "Describe each site to FUNCTION as BLOCK and LOCAL-COORDINATE.

This row-shaped convenience protocol is for presentation and irregular local
work.  Whole-domain algorithms should use WITH-BLOCK-CONTENT-STORAGE."
  (check-type chunk block-chunk)
  (with-block-content-storage (domain palette indices) chunk
    (do-chunk-domain-sites (offset local domain)
      ;; FUNCTION may retain LOCAL, so pass it an indefinite-extent copy.
      (funcall function
               (aref palette (aref indices offset))
               (copy-local-coordinate local))))
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
   ;; Derived-domain subscriptions.  The cell hook receives (CHUNK X Y Z)
   ;; in world coordinates after an authored content change; the residency
   ;; hook receives (X Y Z EVENT) with EVENT :ARRIVED or :DEPARTED in chunk
   ;; coordinates after residency publication.
   (cell-change-hook :initform nil :accessor block-world-cell-change-hook)
   (residency-change-hook :initform nil
                          :accessor block-world-residency-change-hook)
   (next-chunk-incarnation :initform 0)
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

(defun world-chunk-at-coordinate (world coordinate)
  "Return the resident chunk at CHUNK-COORDINATE and whether it exists."
  (check-type coordinate chunk-coordinate)
  (world-chunk-at world
                  (chunk-coordinate-x coordinate)
                  (chunk-coordinate-y coordinate)
                  (chunk-coordinate-z coordinate)))

(defun next-block-world-chunk-incarnation (world)
  (incf (slot-value world 'next-chunk-incarnation)))

(defun make-world-owned-block-chunk (world domain &key content)
  (make-block-chunk
   domain
   :incarnation (next-block-world-chunk-incarnation world)
   :content content
   :change-hook
   (lambda (changed-chunk local-x local-y local-z)
     (note-block-world-change world)
     (let ((cell-hook (block-world-cell-change-hook world)))
       (when cell-hook
         (let ((origin (chunk-domain-origin
                        (block-chunk-domain changed-chunk))))
           (funcall cell-hook changed-chunk
                    (+ (world-coordinate-x origin) local-x)
                    (+ (world-coordinate-y origin) local-y)
                    (+ (world-coordinate-z origin) local-z))))))))

(defun note-world-residency-event (world x y z event)
  (let ((hook (block-world-residency-change-hook world)))
    (when hook
      (funcall hook x y z event))))

(defun ensure-world-chunk (world x y z)
  "Return the resident chunk at X,Y,Z, creating an all-air chunk if absent."
  (multiple-value-bind (chunk present-p) (world-chunk-at world x y z)
    (if present-p
        chunk
        (let* ((coordinate (make-chunk-coordinate x y z))
               (domain (make-chunk-domain (block-world-space world) coordinate))
               (new-chunk (make-world-owned-block-chunk world domain)))
          (setf (gethash (chunk-key x y z) (block-world-chunks world))
                new-chunk)
          (incf (slot-value world 'residency-revision))
          (note-block-world-change world)
          (note-world-residency-event world x y z :arrived)
          new-chunk))))

(defun install-world-chunk-storage (world x y z palette indices)
  "Install transferred dense storage as a newly resident chunk.

The caller gives WORLD ownership of PALETTE and INDICES and must not mutate
them afterward.  Installation is deliberately a single-writer operation: it
publishes one complete chunk and advances residency/general revisions once."
  (check-type world block-world)
  (check-type palette vector)
  (check-type indices (array (unsigned-byte 16) (*)))
  (when (nth-value 1 (world-chunk-at world x y z))
    (error "Chunk (~D ~D ~D) is already resident." x y z))
  (let* ((coordinate (make-chunk-coordinate x y z))
         (domain (make-chunk-domain (block-world-space world) coordinate))
         (cardinality (chunk-domain-cardinality domain)))
    (unless (= (length indices) cardinality)
      (error "Transferred chunk storage has ~D indices; ~D are required."
             (length indices) cardinality))
    (dotimes (offset cardinality)
      (unless (< (aref indices offset) (length palette))
        (error "Palette index ~D at offset ~D exceeds palette length ~D."
               (aref indices offset) offset (length palette))))
    (let* ((content (make-instance 'block-content-column
                                   :palette palette :indices indices))
           (chunk (make-world-owned-block-chunk
                   world domain :content content)))
      (setf (gethash (chunk-key x y z) (block-world-chunks world)) chunk)
      (incf (slot-value world 'residency-revision))
      (note-block-world-change world)
      (note-world-residency-event world x y z :arrived)
      chunk)))

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
      (note-block-world-change world)
      (note-world-residency-event world x y z :departed))
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

(defgeneric world-block-at (world x y z)
  (:documentation
   "Return one site as BLOCK and :RESIDENT, or NIL and :ABSENT.

This sparse world-coordinate accessor constructs coordinate descriptors and a
chunk lookup key.  It is appropriate for inspectors, ray hits, collision
probes, and sparse interaction.  Algorithms over many cells should select a
chunk/domain once and use WITH-BLOCK-CONTENT-STORAGE instead."))

(defgeneric (setf world-block-at) (block world x y z)
  (:documentation
   "Set BLOCK at one resident world site and return BLOCK.

This follows the same sparse coordinate-resolution path as WORLD-BLOCK-AT.
It signals CHUNK-NOT-RESIDENT rather than materializing absent terrain, and
retains chunk revision, boundary revision, and world invalidation semantics."))

(defun locate-world-coordinate (world x y z)
  (let ((coordinate (make-world-coordinate x y z)))
    (multiple-value-bind (chunk-coordinate local-coordinate)
        (world-coordinate-chunk-and-local (block-world-space world) coordinate)
      (values coordinate chunk-coordinate local-coordinate))))

(defmethod locate-chunk-window-site ((world block-world) x y z)
  (multiple-value-bind
        (chunk-x chunk-y chunk-z local-x local-y local-z)
      (voxel-space-decompose-components (block-world-space world) x y z)
    (multiple-value-bind (chunk present-p)
        (world-chunk-at world chunk-x chunk-y chunk-z)
      (if present-p
          (values chunk
                  (chunk-domain-offset-components
                   (block-chunk-domain chunk) local-x local-y local-z)
                  :available)
          (values nil nil :unavailable)))))

(defmethod world-block-at ((world block-world) x y z)
  (multiple-value-bind (chunk offset availability)
      (locate-chunk-window-site world x y z)
    (ecase availability
      (:available
       (values (block-content-at-offset (block-chunk-content chunk) offset)
               :resident))
      (:unavailable (values nil :absent)))))

(defmethod (setf world-block-at)
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
               (setf (world-block-at world x y z) block)))))
       (block-edit-overlay-entries overlay))))
  chunk)

;;; Ray traversal is expressed in continuous lattice coordinates: integer
;;; planes are cell boundaries, independently of the physical cell extent.
;;; The traversal stops at absent terrain rather than silently seeing air.

(luv.arithmetic.records:define-quantity-struct
    (block-ray-hit
     (:constructor %make-block-ray-hit
         (coordinate adjacent-coordinate block distance)))
  (coordinate nil :type world-coordinate :read-only t)
  (adjacent-coordinate nil :type (or null world-coordinate) :read-only t)
  (block nil :read-only t)
  (distance 0d0 :type double-float :read-only t
            :quantity (:quantity :ray-distance :unit :cell)))

(defun raycast-block-world
    (world origin direction occupied-p &key max-distance)
  "Trace a ray through WORLD's resident lattice.

Return a BLOCK-RAY-HIT and :HIT, NIL and :ABSENT when traversal reaches a
non-resident chunk, or NIL and :MISS.  ORIGIN and DIRECTION are VEC3 values
in continuous cell coordinates.  The caller owns and must supply the maximum
ray distance appropriate to its interaction."
  (check-type world block-world)
  (check-type origin vec3)
  (check-type direction vec3)
  (check-type max-distance (real 0))
  (unless (functionp occupied-p)
    (error "OCCUPIED-P must be a function."))
  (let* ((origin-x (coerce (vec3-x origin) 'double-float))
         (origin-y (coerce (vec3-y origin) 'double-float))
         (origin-z (coerce (vec3-z origin) 'double-float))
         (raw-x (coerce (vec3-x direction) 'double-float))
         (raw-y (coerce (vec3-y direction) 'double-float))
         (raw-z (coerce (vec3-z direction) 'double-float))
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
            (world-block-at world cell-x cell-y cell-z)
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
