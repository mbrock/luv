(in-package #:luft)

;;; Integer site-stream surface materialization
;;;
;;; Geometry is a sum over lattice sites.  Face records select a fixed inset
;;; square, edge records select flat collars or crease bands, and vertex records
;;; select flat corner patches or Arc junction fans.  Records contain only a
;;; lattice base coordinate, a stock, and a template index.  Template vertices
;;; are exact small integer offsets from that base; vertex-owned offsets stay
;;; inside the configured bevel-width domain around the lattice site.

(defconstant +mesh-cell-size+ 8)
(defconstant +mesh-bevel-width+ 1
  "Default bevel width in eighth-cell integer ticks.")
(defconstant +mesh-instance-word-count+ 4)
(defconstant +mesh-template-vertex-word-count+ 4)
(defconstant +mesh-template-coordinate-bias+ 16)

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

(defstruct (mesh-template
             (:constructor %make-mesh-template (id vertices)))
  (id 0 :type (integer 0 *) :read-only t)
  (vertices nil :type list :read-only t))

(defstruct (mesh-instance
             (:constructor %make-mesh-instance (base stock template)))
  (base nil :type list :read-only t)
  (stock 0 :type (unsigned-byte 4) :read-only t)
  (template nil :type mesh-template :read-only t))

(defstruct (surface-mesh-builder
             (:constructor %make-surface-mesh-builder (domain bevel-width)))
  (domain nil :type world-domain :read-only t)
  (bevel-width +mesh-bevel-width+ :type (integer 1 4) :read-only t)
  (templates nil :type list)
  (template-table (make-hash-table :test #'equal) :type hash-table)
  (face-instances nil :type list)
  (band-instances nil :type list)
  (fan-instances nil :type list)
  (singular-star-count 0 :type (integer 0 *)))

(defun %point-with-component (point axis-number value)
  (let ((copy (copy-list point)))
    (setf (nth axis-number copy) value)
    copy))

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

(defun %local-point (base point)
  (loop for base-coordinate in base
        for coordinate in point
        collect (- coordinate (* +mesh-cell-size+ base-coordinate))))

(defun %triangle-template-vertices
    (base a b c normal kind boundary-edge-mask)
  (let ((orientation (%dot (%cross (%point- b a) (%point- c a)) normal)))
    (when (zerop orientation)
      (error "Degenerate ~A triangle ~S ~S ~S." kind a b c))
    (when (minusp orientation) (rotatef b c)))
  (loop for point in (list a b c)
        for barycentric-index below 3
        collect (list (%local-point base point)
                      (%normal-direction-code normal)
                      barycentric-index kind boundary-edge-mask)))

(defun %polygon-template-vertices (base points normal kind)
  (when (>= (length points) 3)
    (loop with first-tail = (rest points)
          with last = (car (last points))
          for tail on first-tail
          while (rest tail)
          append (%triangle-template-vertices
                  base (first points) (first tail) (second tail) normal kind
                  (logior #b001
                          (if (equal (second tail) last) #b010 0)
                          (if (eq tail first-tail) #b100 0))))))

(defun %intern-mesh-template (builder vertices)
  (or (gethash vertices (surface-mesh-builder-template-table builder))
      (let ((template
              (%make-mesh-template
               (length (surface-mesh-builder-templates builder)) vertices)))
        (setf (gethash vertices (surface-mesh-builder-template-table builder))
              template)
        (setf (surface-mesh-builder-templates builder)
              (append (surface-mesh-builder-templates builder)
                      (list template)))
        template)))

(defun %emit-template-instance (builder base vertices stock kind)
  (check-type stock (unsigned-byte 4))
  (let* ((template (%intern-mesh-template builder vertices))
         (instance (%make-mesh-instance base stock template)))
    (ecase kind
      (:face (push instance (surface-mesh-builder-face-instances builder)))
      (:band (push instance (surface-mesh-builder-band-instances builder)))
      (:junction (push instance (surface-mesh-builder-fan-instances builder))))
    instance))

(defun %emit-polygon (builder base points normal stock kind)
  (%emit-template-instance
   builder base (%polygon-template-vertices base points normal kind)
   stock kind))

(defun %simple-u32-vector (source)
  (let ((copy (make-array (length source) :element-type '(unsigned-byte 32))))
    (replace copy source)
    copy))

(defun %encode-template-coordinate (coordinate)
  (let ((encoded (+ coordinate +mesh-template-coordinate-bias+)))
    (unless (typep encoded '(unsigned-byte 5))
      (error "Template coordinate ~S does not fit the signed five-bit ABI."
             coordinate))
    encoded))

(defun %template-words (templates)
  (let ((words (make-array 256 :element-type '(unsigned-byte 32)
                               :adjustable t :fill-pointer 0))
        (ranges (make-array (* 2 (length templates))
                            :element-type '(unsigned-byte 32)))
        (vertex-start 0))
    (dolist (template templates)
      (let ((vertices (mesh-template-vertices template)))
        (setf (aref ranges (* 2 (mesh-template-id template))) vertex-start
              (aref ranges (1+ (* 2 (mesh-template-id template))))
              (length vertices))
        (dolist (vertex vertices)
          (destructuring-bind
              (point normal barycentric-index kind boundary-edge-mask) vertex
            (dolist (coordinate point)
              (vector-push-extend (%encode-template-coordinate coordinate)
                                  words))
            (vector-push-extend
             (%pack-template-attributes normal barycentric-index kind
                                        boundary-edge-mask)
             words)))
        (incf vertex-start (length vertices))))
    (values (%simple-u32-vector words) ranges)))

(defun %finish-instance-stream (instances ranges)
  (let* ((ordered
           (stable-sort (copy-list instances) #'<
                        :key (lambda (instance)
                               (mesh-template-id
                                (mesh-instance-template instance)))))
         (words (make-array (* +mesh-instance-word-count+ (length ordered))
                            :element-type '(unsigned-byte 32)))
         (draws nil)
         (triangle-count 0))
    (loop for instance in ordered
          for instance-index below (length ordered)
          for base = (mesh-instance-base instance)
          for template = (mesh-instance-template instance)
          for template-id = (mesh-template-id template)
          for vertex-start = (aref ranges (* 2 template-id))
          for vertex-count = (aref ranges (1+ (* 2 template-id)))
          do (loop for coordinate in base
                   for word from (* instance-index +mesh-instance-word-count+)
                   do (unless (typep coordinate '(unsigned-byte 32))
                        (error "Instance base coordinate is unsigned: ~S."
                               base))
                      (setf (aref words word) coordinate))
             (setf (aref words
                         (+ (* instance-index +mesh-instance-word-count+) 3))
                   (logior template-id
                           (ash (mesh-instance-stock instance) 16)))
             (incf triangle-count (/ vertex-count 3))
             (let ((draw (first draws)))
               (if (and draw (= template-id (first draw)))
                   (incf (fifth draw))
                   (push (list template-id vertex-start vertex-count
                               instance-index 1)
                         draws))))
    (values words (nreverse draws) triangle-count)))

(defun %finish-surface-mesh (builder)
  (multiple-value-bind (template-words template-ranges)
      (%template-words (surface-mesh-builder-templates builder))
    (multiple-value-bind (face-words face-draws face-triangles)
        (%finish-instance-stream
         (surface-mesh-builder-face-instances builder) template-ranges)
      (multiple-value-bind (band-words band-draws band-triangles)
          (%finish-instance-stream
           (surface-mesh-builder-band-instances builder) template-ranges)
        (multiple-value-bind (fan-words fan-draws fan-triangles)
            (%finish-instance-stream
             (surface-mesh-builder-fan-instances builder) template-ranges)
          (%make-surface-mesh
           (surface-mesh-builder-domain builder)
           (surface-mesh-builder-bevel-width builder)
           template-words template-ranges
           face-words face-draws band-words band-draws fan-words fan-draws
           face-triangles band-triangles fan-triangles
           (surface-mesh-builder-singular-star-count builder)))))))

(defun %cell-coordinates (cell)
  (list (site-x cell) (site-y cell) (site-z cell)))

(defun %occupancy-at (domain occupancy coordinates)
  (cell-occupancy-bit domain occupancy
                      (first coordinates) (second coordinates)
                      (third coordinates)))

(defun %offset-coordinates (coordinates axis-number delta)
  (let ((copy (copy-list coordinates)))
    (incf (nth axis-number copy) delta)
    copy))

(defun %cell-face (domain cell axis-number side)
  (let ((axis (index-axis axis-number)))
    (if (minusp side)
        (site-boundary-low domain cell axis)
        (site-boundary-high domain cell axis))))

(defun %face-crease-p
    (domain occupancy cell-coordinates normal-axis normal-side
     tangent-axis tangent-side)
  (let* ((tangent-neighbor
           (%offset-coordinates cell-coordinates tangent-axis tangent-side))
         (diagonal
           (%offset-coordinates tangent-neighbor normal-axis normal-side)))
    (not (and (= 1 (%occupancy-at domain occupancy tangent-neighbor))
              (= 0 (%occupancy-at domain occupancy diagonal))))))

(defun %emit-cell-face
    (builder domain occupancy cell axis-number side stock-function
     chamfer-stock-function)
  (let* ((bevel-width (surface-mesh-builder-bevel-width builder))
         (cell-coordinates (%cell-coordinates cell))
         (tangents (%other-axis-numbers axis-number))
         (plane (* +mesh-cell-size+
                   (+ (nth axis-number cell-coordinates)
                      (if (plusp side) 1 0))))
         (normal (loop for index below 3
                       collect (if (= index axis-number) side 0)))
         (face (%cell-face domain cell axis-number side))
         (stock (funcall stock-function face))
         ;; The central square remains face-owned.  Its four collars are the
         ;; planar part of the chamfer, so they must take the same one-stock
         ;; policy as bevel bands and vertex closures.
         (chamfer-stock (funcall chamfer-stock-function (list stock))))
    (destructuring-bind (u v) tangents
      (let* ((u-anchor (* +mesh-cell-size+ (nth u cell-coordinates)))
             (v-anchor (* +mesh-cell-size+ (nth v cell-coordinates)))
             (u-low (+ u-anchor
                       (if (%face-crease-p
                            domain occupancy cell-coordinates axis-number side
                            u -1)
                           bevel-width 0)))
             (u-high (- (+ u-anchor +mesh-cell-size+)
                        (if (%face-crease-p
                             domain occupancy cell-coordinates axis-number side
                             u 1)
                            bevel-width 0)))
             (v-low (+ v-anchor
                       (if (%face-crease-p
                            domain occupancy cell-coordinates axis-number side
                            v -1)
                           bevel-width 0)))
             (v-high (- (+ v-anchor +mesh-cell-size+)
                        (if (%face-crease-p
                             domain occupancy cell-coordinates axis-number side
                             v 1)
                            bevel-width 0)))
             (u-cuts (vector u-low (+ u-anchor bevel-width)
                             (- (+ u-anchor +mesh-cell-size+) bevel-width)
                             u-high))
             (v-cuts (vector v-low (+ v-anchor bevel-width)
                             (- (+ v-anchor +mesh-cell-size+) bevel-width)
                             v-high)))
        (labels ((point (u-value v-value)
                   (let ((result (list 0 0 0)))
                     (setf (nth axis-number result) plane
                           (nth u result) u-value
                           (nth v result) v-value)
                     result))
                 (site-base (u-cell v-cell)
                   (let ((base (copy-list cell-coordinates)))
                     (setf (nth axis-number base)
                           (+ (nth axis-number base) (if (plusp side) 1 0))
                           (nth u base) (+ (nth u base) u-cell)
                           (nth v base) (+ (nth v base) v-cell))
                     base)))
          ;; Uniformly partition the exact old face rectangle.  Its 6x6 heart
          ;; is face-owned and its nonempty side cells are edge-owned.  Leave
          ;; the four corner cells open: the lattice-site fan closes their
          ;; actual boundary after every face and edge instance is present.
          (dotimes (u-cell 3)
            (dotimes (v-cell 3)
              (let ((u0 (aref u-cuts u-cell))
                    (u1 (aref u-cuts (1+ u-cell)))
                    (v0 (aref v-cuts v-cell))
                    (v1 (aref v-cuts (1+ v-cell))))
                (when (and (< u0 u1) (< v0 v1)
                           (or (= u-cell 1) (= v-cell 1)))
                  (let ((kind (if (and (= u-cell 1) (= v-cell 1))
                                  :face
                                  :band)))
                    (%emit-polygon
                     builder
                     (site-base (if (= u-cell 2) 1 0)
                                (if (= v-cell 2) 1 0))
                     (list (point u0 v0) (point u1 v0)
                           (point u1 v1) (point u0 v1))
                     normal (if (eq kind :face) stock chamfer-stock)
                     kind)))))))))))

(defun %collect-edge-keys (solid)
  (let ((keys (make-hash-table :test #'equal)))
    (loop for cell across (chain-sites solid) do
      (let ((coordinates (%cell-coordinates cell)))
        (dotimes (axis-number 3)
          (destructuring-bind (u v) (%other-axis-numbers axis-number)
            (dotimes (u-side 2)
              (dotimes (v-side 2)
                (let ((anchor (copy-list coordinates)))
                  (incf (nth u anchor) u-side)
                  (incf (nth v anchor) v-side)
                  (setf (gethash (cons axis-number anchor) keys) t))))))))
    (sort (loop for key being the hash-keys of keys collect key)
          (lambda (left right)
            (or (< (first left) (first right))
                (and (= (first left) (first right))
                     (loop for l in (rest left)
                           for r in (rest right)
                           when (/= l r) return (< l r)
                           finally (return nil))))))))

(defun %cross-quadrants (axis-number)
  (destructuring-bind (u v) (%other-axis-numbers axis-number)
    (declare (ignore u v))
    #((-1 -1) (1 -1) (1 1) (-1 1))))

(defun %edge-quadrant-cell (anchor axis-number quadrant)
  (destructuring-bind (u v) (%other-axis-numbers axis-number)
    (let ((cell (copy-list anchor)))
      (when (minusp (first quadrant)) (decf (nth u cell)))
      (when (minusp (second quadrant)) (decf (nth v cell)))
      cell)))

(defun %edge-run-transition-groups (states)
  (let ((transitions
          (loop for index below 4
                unless (= (aref states index)
                          (aref states (mod (1+ index) 4)))
                  collect index)))
    (case (length transitions)
      (0 nil)
      (2 (list transitions))
      (4 (loop for index below 4
               when (= 1 (aref states index))
                 collect (list (mod (+ index 3) 4) index)))
      (t (error "Impossible edge transition count ~D." (length transitions))))))

(defun %edge-transition-data
    (domain anchor axis-number quadrants states transition-index bevel-width)
  (let* ((next-index (mod (1+ transition-index) 4))
         (left (aref quadrants transition-index))
         (right (aref quadrants next-index))
         (occupied (if (= 1 (aref states transition-index)) left right))
         (empty (if (eq occupied left) right left))
         (cross-axes (%other-axis-numbers axis-number))
         (normal-axis
           (if (/= (first occupied) (first empty))
               (first cross-axes)
               (second cross-axes)))
         (other-axis
           (if (= normal-axis (first cross-axes))
               (second cross-axes)
               (first cross-axes)))
         (normal (list 0 0 0))
         (offset (list 0 0 0))
         (occupied-cell
           (%edge-quadrant-cell anchor axis-number occupied)))
    (setf (nth normal-axis normal)
          (if (= normal-axis (first cross-axes))
              (if (plusp (first empty)) 1 -1)
              (if (plusp (second empty)) 1 -1))
          (nth other-axis offset)
          (* bevel-width
             (if (= other-axis (first cross-axes))
                 (first occupied)
                 (second occupied))))
    (let* ((cell (make-site domain
                            (first occupied-cell)
                            (second occupied-cell)
                            (third occupied-cell)
                            +cell-extent+ 1))
           (face (%cell-face domain cell normal-axis
                             (nth normal-axis normal))))
      (list :normal normal :offset offset :face face))))

(defun %normal-key (normal)
  (+ (+ 1 (first normal))
     (* 3 (+ 1 (second normal)))
     (* 9 (+ 1 (third normal)))))

(defun %parallel-normal-p (left right)
  (let ((left-key (%normal-key left)))
    (or (= left-key (%normal-key right))
        (= left-key (%normal-key (mapcar #'- right))))))

(defun %vertex-star-mask (domain occupancy coordinates)
  (site-star-occupancy-mask
   domain
   (make-site domain (first coordinates) (second coordinates)
              (third coordinates) +vertex-extent+ 1)
   occupancy))

(defun %emit-edge-bands
    (builder domain occupancy key stock-function chamfer-stock-function)
  (let* ((bevel-width (surface-mesh-builder-bevel-width builder))
         (axis-number (first key))
         (anchor (rest key))
         (quadrants (%cross-quadrants axis-number))
         (states (make-array 4 :element-type 'bit)))
    (dotimes (index 4)
      (setf (sbit states index)
            (%occupancy-at
             domain occupancy
             (%edge-quadrant-cell anchor axis-number
                                    (aref quadrants index)))))
    (dolist (group (%edge-run-transition-groups states))
      (let* ((left (%edge-transition-data
                    domain anchor axis-number quadrants states
                    (first group) bevel-width))
             (right (%edge-transition-data
                     domain anchor axis-number quadrants states
                     (second group) bevel-width))
             (left-normal (getf left :normal))
             (right-normal (getf right :normal)))
        ;; Equal normals are one flat face continued across a cell boundary;
        ;; opposite normals are two sheets touching at the lattice edge.
        ;; Neither relation owns a bevel band between the faces.
        (unless (%parallel-normal-p left-normal right-normal)
          (let* ((high-vertex (%offset-coordinates anchor axis-number 1))
                 (axis-low (* +mesh-cell-size+
                              (nth axis-number anchor)))
                 (axis-high (* +mesh-cell-size+
                               (nth axis-number high-vertex)))
                 (base (mapcar (lambda (coordinate)
                                 (* +mesh-cell-size+ coordinate))
                               anchor))
                 (normal (mapcar #'+ left-normal right-normal))
                 (stock
                   (funcall chamfer-stock-function
                            (list (funcall stock-function (getf left :face))
                                  (funcall stock-function
                                           (getf right :face))))))
            (labels ((rail (data coordinate)
                       (%point-with-component
                        (mapcar #'+ base (getf data :offset))
                        axis-number coordinate))
                     (patch (site-base low high)
                       (%emit-polygon
                        builder site-base
                        (list (rail left low) (rail right low)
                              (rail right high) (rail left high))
                        normal stock :band)))
              ;; The edge owns the middle after both vertex-site domains are
              ;; removed.  Those width-sized ends belong to the two fans.
              (patch anchor
                     (+ axis-low bevel-width)
                     (- axis-high bevel-width)))))))))

(defun %collect-vertex-keys (solid)
  (let ((vertices (make-hash-table :test #'equal)))
    (loop for cell across (chain-sites solid) do
      (let ((anchor (%cell-coordinates cell)))
        (dotimes (sample 8)
          (setf (gethash
                 (loop for axis-number below 3
                       collect (+ (nth axis-number anchor)
                                  (if (logbitp axis-number sample) 1 0)))
                 vertices)
                t))))
    (sort (loop for vertex being the hash-keys of vertices collect vertex)
          (lambda (left right)
            (loop for l in left for r in right
                  when (/= l r) return (< l r)
                  finally (return nil))))))

(defun %point-lexicographically-less-p (left right)
  (loop for l in left
        for r in right
        when (/= l r) return (< l r)
        finally (return nil)))

(defun %geometric-edge-key (left right)
  (if (%point-lexicographically-less-p left right)
      (list left right)
      (list right left)))

(defun %instance-point (instance local-point)
  (loop for coordinate in (mesh-instance-base instance)
        for local-coordinate in local-point
        collect (+ (* +mesh-cell-size+ coordinate) local-coordinate)))

(defun %builder-open-boundary-edges (builder)
  "Return the directed edges occurring once in the face and band streams."
  (let ((observations (make-hash-table :test #'equal)))
    (dolist (instance
             (append (surface-mesh-builder-face-instances builder)
                     (surface-mesh-builder-band-instances builder)))
      (loop for tail on (mesh-template-vertices
                         (mesh-instance-template instance))
            by #'cdddr
            while tail
            for points =
              (mapcar (lambda (vertex)
                        (%instance-point instance (first vertex)))
                      (list (first tail) (second tail) (third tail)))
            do (dotimes (index 3)
                 (let ((left (nth index points))
                       (right (nth (mod (1+ index) 3) points)))
                   (push (list left right (mesh-instance-stock instance))
                         (gethash (%geometric-edge-key left right)
                                  observations))))))
    (sort
     (loop for key being the hash-keys of observations
             using (hash-value edges)
           when (= 1 (length edges))
             collect (first edges)
           when (> (length edges) 2)
             do (error "Face and edge streams meet ~D times at ~S."
                       (length edges) key))
     (lambda (left right)
       (let ((left-key (%geometric-edge-key (first left) (second left)))
             (right-key (%geometric-edge-key (first right) (second right))))
         (or (%point-lexicographically-less-p
              (first left-key) (first right-key))
             (and (equal (first left-key) (first right-key))
                  (%point-lexicographically-less-p
                   (second left-key) (second right-key)))))))))

(defun %boundary-edge-site (left right bevel-width)
  "Find the lattice vertex whose bevel domain contains LEFT--RIGHT."
  (loop for l in left
        for r in right
        for coordinate = (round (/ (+ l r) (* 2 +mesh-cell-size+)))
        unless (and (<= (abs (- l (* +mesh-cell-size+ coordinate)))
                        bevel-width)
                    (<= (abs (- r (* +mesh-cell-size+ coordinate)))
                        bevel-width))
          do (error "Open edge ~S--~S is not local to a lattice vertex."
                    left right)
        collect coordinate))

(defun %boundary-edge-cycles (edges site)
  "Order the consistently directed boundary EDGES into loops at SITE."
  (let ((pending (copy-list edges))
        (cycles nil))
    (loop while pending do
      (let* ((first-edge (pop pending))
             (first-point (first first-edge))
             (next-point (second first-edge))
             (cycle (list first-edge)))
        (loop until (equal next-point first-point) do
          (let ((next-edge
                  (find next-point pending :key #'first :test #'equal)))
            (unless next-edge
              (error "Open boundary at lattice site ~S stops at ~S."
                     site next-point))
            (setf pending (delete next-edge pending :count 1 :test #'eq)
                  cycle (append cycle (list next-edge))
                  next-point (second next-edge))))
        (push cycle cycles)))
    (nreverse cycles)))

(defun %boundary-cycles-by-site (builder)
  (let ((by-site (make-hash-table :test #'equal))
        (bevel-width (surface-mesh-builder-bevel-width builder)))
    (dolist (edge (%builder-open-boundary-edges builder))
      (push edge
            (gethash (%boundary-edge-site
                      (first edge) (second edge) bevel-width)
                     by-site)))
    (sort
     (loop for site being the hash-keys of by-site
             using (hash-value edges)
           collect (cons site (%boundary-edge-cycles edges site)))
     #'%point-lexicographically-less-p :key #'first)))

(defun %emit-triangular-boundary-cap (builder site cycle stock)
  (let* ((a (first (first cycle)))
         (b (first (second cycle)))
         (c (first (third cycle)))
         (normal (%cross (%point- c a) (%point- b a))))
    ;; The observed loop follows the existing surface winding.  Reverse its
    ;; order so the cap pairs every boundary edge with opposite winding.
    (%emit-polygon builder site (list a c b) normal stock :junction)))

(defun %squared-distance (left right)
  (reduce #'+ (mapcar (lambda (l r)
                        (let ((difference (- l r)))
                          (* difference difference)))
                      left right)))

(defun %cycle-planar-through-site-p (site cycle)
  (let* ((origin (mapcar (lambda (coordinate)
                           (* +mesh-cell-size+ coordinate))
                         site))
         (vectors (mapcar (lambda (edge)
                            (%point- (first edge) origin))
                          cycle))
         (normal
           (loop for left on vectors
                 thereis
                 (loop for right in (rest left)
                       for cross = (%cross (first left) right)
                       when (some (complement #'zerop) cross)
                         return cross))))
    (and normal
         (every (lambda (vector) (zerop (%dot normal vector))) vectors))))

(defun %cycle-boundary-stock-table (cycle)
  (let ((stocks (make-hash-table :test #'equal)))
    (dolist (edge cycle stocks)
      (setf (gethash (%geometric-edge-key (first edge) (second edge)) stocks)
            (third edge)))))

(defun %triangle-boundary-mask (a b c boundary-stocks)
  (logior (if (gethash (%geometric-edge-key b c) boundary-stocks) #b001 0)
          (if (gethash (%geometric-edge-key c a) boundary-stocks) #b010 0)
          (if (gethash (%geometric-edge-key a b) boundary-stocks) #b100 0)))

(defun %emit-boundary-strip-triangle
    (builder site a b c boundary-stocks stock)
  (let ((normal (%cross (%point- b a) (%point- c a))))
    (%emit-template-instance
     builder site
     (%triangle-template-vertices
      site a b c normal :junction
      (%triangle-boundary-mask a b c boundary-stocks))
     stock :junction)))

(defun %emit-boundary-strip (builder site cycle stock)
  "Triangulate CYCLE without introducing its lattice-site origin as geometry."
  (let ((points (mapcar #'first cycle))
        (boundary-stocks (%cycle-boundary-stock-table cycle)))
    ;; Repeatedly remove the end whose replacement diagonal is shorter.  The
    ;; remaining vertices stay a contiguous interval of the boundary, giving
    ;; a deterministic local triangle strip rather than a long fan of spokes.
    (loop while (> (length points) 3) do
      (let* ((first (first points))
             (second (second points))
             (last (car (last points)))
             (penultimate (car (last points 2))))
        (if (<= (%squared-distance second last)
                (%squared-distance first penultimate))
            (progn
              (%emit-boundary-strip-triangle
               builder site first last second
               boundary-stocks stock)
              (setf points (rest points)))
            (progn
              (%emit-boundary-strip-triangle
               builder site first last penultimate
               boundary-stocks stock)
              (setf points (butlast points))))))
    (destructuring-bind (a b c) points
      (%emit-boundary-strip-triangle
       builder site a c b boundary-stocks stock))))

(defun %emit-centered-boundary-fan (builder site cycle stock)
  (let ((origin (mapcar (lambda (coordinate)
                          (* +mesh-cell-size+ coordinate))
                        site)))
    (dolist (edge cycle)
      (destructuring-bind (left right edge-stock) edge
        (declare (ignore edge-stock))
        ;; A boundary edge incident on the center is already a radial edge of
        ;; this fan.  Its neighboring non-radial segment emits the triangle.
        (unless (or (equal left origin) (equal right origin))
          (let ((normal (%cross (%point- right origin)
                                (%point- left origin))))
            (when (every #'zerop normal)
              (error "Open edge ~S--~S is radial to lattice site ~S."
                     left right site))
            ;; Bit zero denotes the outer RIGHT--LEFT edge.  The other two
            ;; edges are triangulation diagonals inside the complete fan.
            ;; Keep each sector as its own instance because the construction
            ;; mask varies around the junction, even though STOCK is uniform
            ;; for this whole chamfer.
            (%emit-template-instance
             builder site
             (%triangle-template-vertices
              site origin right left normal :junction #b001)
             stock :junction)))))))

(defun %vertex-fan-uses-center-p (domain occupancy site cycle)
  (let ((mask (%vertex-star-mask domain occupancy site)))
    (or (%cycle-planar-through-site-p site cycle)
        ;; The ordinary five-cell concave corner and its upside-down three-cell
        ;; complement both pass through SITE.  The six- and seven-cell
        ;; chamfer/fillet runs do not; coning those creates the ornaments.
        (member (logcount mask) '(3 5)))))

(defun %rescale-boundary-point (point site source-width target-width)
  "Evaluate POINT's site-local affine bevel coordinates at TARGET-WIDTH."
  (loop for coordinate in point
        for site-coordinate in site
        for origin = (* +mesh-cell-size+ site-coordinate)
        for numerator = (* (- coordinate origin) target-width)
        do (unless (zerop (rem numerator source-width))
             (error "Boundary coordinate ~S at site ~S has no integer ~D/~D limit."
                    point site target-width source-width))
        collect (+ origin (truncate numerator source-width))))

(defun %rescale-boundary-cycle (cycle site source-width target-width)
  (loop for (left right stock) in cycle
        collect (list (%rescale-boundary-point
                       left site source-width target-width)
                      (%rescale-boundary-point
                       right site source-width target-width)
                      stock)))

(defun %emit-boundary-derived-fans
    (builder domain occupancy chamfer-stock-function
     &optional (boundary-builder builder))
  "Close BOUNDARY-BUILDER's loops with site-local templates in BUILDER."
  (let ((source-width
          (surface-mesh-builder-bevel-width boundary-builder))
        (target-width (surface-mesh-builder-bevel-width builder)))
    (dolist (entry (%boundary-cycles-by-site boundary-builder))
      (let ((site (first entry)))
        (dolist (source-cycle (rest entry))
          (let ((cycle
                  (if (= source-width target-width)
                      source-cycle
                      (%rescale-boundary-cycle
                       source-cycle site source-width target-width))))
            (let ((stock (funcall chamfer-stock-function
                                  (mapcar #'third cycle))))
              (cond ((= 3 (length cycle))
                     (%emit-triangular-boundary-cap
                      builder site cycle stock))
                    ((%vertex-fan-uses-center-p domain occupancy site cycle)
                     (%emit-centered-boundary-fan
                      builder site cycle stock))
                    (t
                     (%emit-boundary-strip
                      builder site cycle stock))))))))))

(defun %count-singular-vertex-stars (builder domain occupancy vertices)
  (dolist (vertex vertices)
    (when (star-singular-p (%vertex-star-mask domain occupancy vertex))
      (incf (surface-mesh-builder-singular-star-count builder)))))

(defun make-surface-mesh
    (solid &key (stock-function (constantly 0))
                (chamfer-stock-function (lambda (stocks) (first stocks)))
                (bevel-width +mesh-bevel-width+))
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
  (let* ((domain (chain-domain solid))
         (builder (%make-surface-mesh-builder domain bevel-width))
         (boundary-builder
           (if (= bevel-width (/ +mesh-cell-size+ 2))
               (%make-surface-mesh-builder
                domain (1- (/ +mesh-cell-size+ 2)))
               builder))
         (occupancy (lambda (x y z)
                      (chain-cell-occupancy-bit solid x y z))))
    (loop for cell across (chain-sites solid) do
      (unless (and (= (site-extent cell) +cell-extent+)
                   (site-positive-p cell))
        (error "A solid mesh requires positive cells, not ~S." cell))
      (let ((coordinates (%cell-coordinates cell)))
        (dotimes (axis-number 3)
          (dolist (side '(-1 1))
            (when (= 0 (%occupancy-at
                        domain occupancy
                        (%offset-coordinates coordinates axis-number side)))
              (%emit-cell-face boundary-builder domain occupancy cell
                               axis-number side
                               stock-function chamfer-stock-function))))))
    (dolist (edge (%collect-edge-keys solid))
      (%emit-edge-bands boundary-builder domain occupancy edge stock-function
                        chamfer-stock-function))
    (let ((vertices (%collect-vertex-keys solid)))
      (%count-singular-vertex-stars builder domain occupancy vertices))
    (%emit-boundary-derived-fans builder domain occupancy
                                 chamfer-stock-function boundary-builder)
    (%finish-surface-mesh builder)))
