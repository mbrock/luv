(in-package #:luft)

;;; Integer site-stream surface materialization
;;;
;;; Geometry is a sum over lattice sites.  Face records select a fixed inset
;;; square, edge records select flat collars or crease bands, and vertex records
;;; select flat corner patches or Arc junction fans.  Records contain only a
;;; lattice base coordinate, a stock, and a template index.  Template vertices
;;; are exact small integer offsets from that base; vertex-owned offsets are all
;;; in {-1,0,1}^3.

(defconstant +mesh-cell-size+ 8)
(defconstant +mesh-bevel-width+ 1)
(defconstant +mesh-instance-word-count+ 4)
(defconstant +mesh-template-vertex-word-count+ 4)
(defconstant +mesh-template-coordinate-bias+ 16)

(defstruct (surface-mesh
             (:constructor %make-surface-mesh
                 (domain template-vertex-words template-ranges
                  face-instance-words face-draws
                  band-instance-words band-draws
                  fan-instance-words fan-draws
                  face-triangle-count band-triangle-count fan-triangle-count
                  singular-star-count))
             (:copier nil))
  (domain nil :type world-domain :read-only t)
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
             (:constructor %make-surface-mesh-builder (domain)))
  (domain nil :type world-domain :read-only t)
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

(defun %emit-polygon (builder base points normal stock kind)
  (check-type stock (unsigned-byte 4))
  (let* ((vertices (%polygon-template-vertices base points normal kind))
         (template (%intern-mesh-template builder vertices))
         (instance (%make-mesh-instance base stock template)))
    (ecase kind
      (:face (push instance (surface-mesh-builder-face-instances builder)))
      (:band (push instance (surface-mesh-builder-band-instances builder)))
      (:junction (push instance (surface-mesh-builder-fan-instances builder))))
    instance))

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
    (builder domain occupancy cell axis-number side stock-function)
  (let* ((cell-coordinates (%cell-coordinates cell))
         (tangents (%other-axis-numbers axis-number))
         (plane (* +mesh-cell-size+
                   (+ (nth axis-number cell-coordinates)
                      (if (plusp side) 1 0))))
         (normal (loop for index below 3
                       collect (if (= index axis-number) side 0)))
         (face (%cell-face domain cell axis-number side))
         (stock (funcall stock-function face)))
    (destructuring-bind (u v) tangents
      (let* ((u-anchor (* +mesh-cell-size+ (nth u cell-coordinates)))
             (v-anchor (* +mesh-cell-size+ (nth v cell-coordinates)))
             (u-low (+ u-anchor
                       (if (%face-crease-p
                            domain occupancy cell-coordinates axis-number side
                            u -1)
                           +mesh-bevel-width+ 0)))
             (u-high (- (+ u-anchor +mesh-cell-size+)
                        (if (%face-crease-p
                             domain occupancy cell-coordinates axis-number side
                             u 1)
                            +mesh-bevel-width+ 0)))
             (v-low (+ v-anchor
                       (if (%face-crease-p
                            domain occupancy cell-coordinates axis-number side
                            v -1)
                           +mesh-bevel-width+ 0)))
             (v-high (- (+ v-anchor +mesh-cell-size+)
                        (if (%face-crease-p
                             domain occupancy cell-coordinates axis-number side
                             v 1)
                            +mesh-bevel-width+ 0)))
             (u-cuts (vector u-low (1+ u-anchor)
                             (1- (+ u-anchor +mesh-cell-size+)) u-high))
             (v-cuts (vector v-low (1+ v-anchor)
                             (1- (+ v-anchor +mesh-cell-size+)) v-high)))
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
          ;; is face-owned; nonempty side cells are edge-owned; nonempty corner
          ;; cells are vertex-owned.
          (dotimes (u-cell 3)
            (dotimes (v-cell 3)
              (let ((u0 (aref u-cuts u-cell))
                    (u1 (aref u-cuts (1+ u-cell)))
                    (v0 (aref v-cuts v-cell))
                    (v1 (aref v-cuts (1+ v-cell))))
                (when (and (< u0 u1) (< v0 v1))
                  (let ((kind (cond ((and (= u-cell 1) (= v-cell 1))
                                     :face)
                                    ((or (= u-cell 1) (= v-cell 1)) :band)
                                    (t :junction))))
                    (%emit-polygon
                     builder
                     (site-base (if (= u-cell 2) 1 0)
                                (if (= v-cell 2) 1 0))
                     (list (point u0 v0) (point u1 v0)
                           (point u1 v1) (point u0 v1))
                     normal stock kind)))))))))))

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
    (domain anchor axis-number quadrants states transition-index)
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
          (* +mesh-bevel-width+
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

(defun %band-pair-key (left right)
  (let ((a (%normal-key left)) (b (%normal-key right)))
    (if (< a b) (cons a b) (cons b a))))

(defun %parallel-normal-p (left right)
  (let ((left-key (%normal-key left)))
    (or (= left-key (%normal-key right))
        (= left-key (%normal-key (mapcar #'- right))))))

(defun %boundary-edge-normal (mask edge)
  (let* ((low (%cube-edge-low edge))
         (high (%cube-edge-high edge))
         (occupied (if (logbitp low mask) low high))
         (empty (if (= occupied low) high low))
         (difference (logxor low high))
         (axis-number (1- (integer-length difference)))
         (normal (list 0 0 0)))
    (setf (nth axis-number normal)
          (%sample-direction-component empty axis-number))
    normal))

(defun %radial-band-pair-keys (mask axis-number sign)
  (loop for pair in (%radial-transition-groups mask axis-number sign)
        for left = (%boundary-edge-normal mask (first pair))
        for right = (%boundary-edge-normal mask (second pair))
        unless (%parallel-normal-p left right)
          collect (%band-pair-key left right)))

(defun %vertex-star-mask (domain occupancy coordinates)
  (site-star-occupancy-mask
   domain
   (make-site domain (first coordinates) (second coordinates)
              (third coordinates) +vertex-extent+ 1)
   occupancy))

(defun %band-continues-p
    (domain occupancy vertex axis-number radial-sign pair-key)
  (member pair-key
          (%radial-band-pair-keys
           (%vertex-star-mask domain occupancy vertex)
           axis-number radial-sign)
          :test #'equal))

(defun %emit-edge-bands
    (builder domain occupancy key stock-function)
  (let* ((axis-number (first key))
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
                    (first group)))
             (right (%edge-transition-data
                     domain anchor axis-number quadrants states
                     (second group)))
             (left-normal (getf left :normal))
             (right-normal (getf right :normal)))
        ;; Equal normals are one flat face continued across a cell boundary;
        ;; opposite normals are two sheets touching at the lattice edge.
        ;; Neither relation owns a bevel band between the faces.
        (unless (%parallel-normal-p left-normal right-normal)
          (let* ((pair-key (%band-pair-key left-normal right-normal))
                 (high-vertex (%offset-coordinates anchor axis-number 1))
                 (axis-low (* +mesh-cell-size+
                              (nth axis-number anchor)))
                 (axis-high (* +mesh-cell-size+
                               (nth axis-number high-vertex)))
                 (low-continues-p
                   (%band-continues-p
                    domain occupancy anchor axis-number -1 pair-key))
                 (high-continues-p
                   (%band-continues-p
                    domain occupancy high-vertex axis-number 1 pair-key))
                 (base (mapcar (lambda (coordinate)
                                 (* +mesh-cell-size+ coordinate))
                               anchor))
                 (normal (mapcar #'+ left-normal right-normal))
                 (stock (funcall stock-function (getf left :face))))
            (labels ((rail (data coordinate)
                       (%point-with-component
                        (mapcar #'+ base (getf data :offset))
                        axis-number coordinate))
                     (patch (site-base low high kind)
                       (%emit-polygon
                        builder site-base
                        (list (rail left low) (rail right low)
                              (rail right high) (rail left high))
                        normal stock kind)))
              ;; Every edge owns the same six-tick middle.  Any extension to
              ;; a lattice vertex is moved into that vertex's fan stream.
              (patch anchor
                     (+ axis-low +mesh-bevel-width+)
                     (- axis-high +mesh-bevel-width+) :band)
              (when low-continues-p
                (patch anchor axis-low (+ axis-low +mesh-bevel-width+)
                       :junction))
              (when high-continues-p
                (patch high-vertex (- axis-high +mesh-bevel-width+) axis-high
                       :junction)))))))))

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

(defun %cycle-stock-face (domain original-mask cycle vertex)
  (let* ((edge (first cycle))
         (low (%cube-edge-low edge))
         (high (%cube-edge-high edge))
         (occupied (if (logbitp low original-mask) low high))
         (empty (if (= occupied low) high low))
         (difference (logxor occupied empty))
         (normal-axis (1- (integer-length difference)))
         (normal-side (%sample-direction-component empty normal-axis))
         (cell-coordinates
           (loop for axis-number below 3
                 collect (+ (nth axis-number vertex)
                            (if (logbitp axis-number occupied) 0 -1))))
         (cell (make-site domain
                          (first cell-coordinates)
                          (second cell-coordinates)
                          (third cell-coordinates)
                          +cell-extent+ 1)))
    (%cell-face domain cell normal-axis normal-side)))

(defun %emit-vertex-junctions
    (builder domain occupancy vertex stock-function)
  (let* ((mask (%vertex-star-mask domain occupancy vertex))
         (cycles (%star-sheet-cycles mask)))
    (when (star-singular-p mask)
      (incf (surface-mesh-builder-singular-star-count builder)))
    (dolist (cycle cycles)
      (let* ((virtual-mask (%cycle-virtual-mask mask cycle))
             (entry (aref *arc-junction-table* virtual-mask))
             (stock (funcall stock-function
                             (%cycle-stock-face
                              domain mask cycle vertex)))
             (origin (mapcar (lambda (coordinate)
                               (* +mesh-cell-size+ coordinate))
                             vertex)))
        (unless (getf entry :regular-star)
          (error "Sheet cycle from ~2,'0X produced singular mask ~2,'0X."
                 mask virtual-mask))
        (dolist (face (getf entry :faces))
          (%emit-polygon
           builder
           vertex
           (mapcar (lambda (point) (mapcar #'+ origin point))
                   (getf face :points))
           (getf face :normal)
           stock :junction))))))

(defun make-surface-mesh
    (solid &key (stock-function (constantly 0)))
  "Classify SOLID into exact integer face, edge, and vertex instance streams.

Every exposed cell face emits the same six-by-six central square.  Coplanar
collars and bevel bands are edge-owned.  Flat corner patches and Arc junction
polygons are vertex-owned.  Each stream is sorted by template so the renderer
can issue direct instanced draws.  STOCK-FUNCTION is called with an oriented
boundary face."
  (check-type solid chain)
  (check-type stock-function function)
  (let* ((domain (chain-domain solid))
         (builder (%make-surface-mesh-builder domain))
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
              (%emit-cell-face builder domain occupancy cell axis-number side
                               stock-function))))))
    (dolist (edge (%collect-edge-keys solid))
      (%emit-edge-bands builder domain occupancy edge stock-function))
    (dolist (vertex (%collect-vertex-keys solid))
      (%emit-vertex-junctions builder domain occupancy vertex stock-function))
    (%finish-surface-mesh builder)))
