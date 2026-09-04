(in-package #:luft.render)

;;; A scene: one authored solid, its sparse cell-material field, torches,
;;; and the immutable generation values that justify a finished mesh.  The
;;; scene builder at the end is the authored construction vocabulary.

(defclass scene ()
  ((solid :initarg :solid :accessor scene-solid)
   (material-vocabulary :initarg :material-vocabulary
                        :reader scene-material-vocabulary)
   (material-cells :initarg :material-cells :accessor scene-material-cells)
   (authored-light-sources
    :initarg :authored-light-sources :accessor scene-authored-light-sources)
   (authored-light-opacity-table
    :initarg :authored-light-opacity-table
    :reader scene-authored-light-opacity-table)
   (authored-light-revision
    :initarg :authored-light-revision :accessor scene-authored-light-revision)
   (authored-light-provenance
    :initarg :authored-light-provenance
    :reader scene-authored-light-provenance)
   (authored-light-generation
    :initarg :authored-light-generation
    :accessor scene-authored-light-generation)
   (content-revision
    :initarg :content-revision :initform 0 :accessor scene-content-revision)
   (torch-light-emission
    :initarg :torch-light-emission
    :reader scene-torch-light-emission)
   (voxel-light-propagation-p
    :initarg :voxel-light-propagation-p
    :initform t
    :reader scene-voxel-light-propagation-p
    :type boolean)
   (torches :initarg :torches :initform #() :reader scene-torches)
   (player-p :initarg :player-p :initform nil :reader scene-player-p))
  (:documentation
   "One authored solid and its cell-material field.

The solid remains LUFT's topological truth. The sparse authored field stores
only dense vocabulary offsets; semantic material objects remain at the scene
boundary rather than being allocated per cell.  Its authored light generation
contains material sources only when its immutable propagation policy is true;
the diagnostic false policy owns the exact empty propagated generation.
Realized torch light is view/profile-dependent and therefore belongs to an
immutable SCENE-MESH-GENERATION, never this scene."))

(defun scene-authored-voxel-light (scene)
  "Return SCENE's immutable authored-policy base light field.

This field deliberately excludes torches: their source positions do not exist
until a particular final bevel surface has realized their attachment frames."
  (check-type scene scene)
  (realized-light-generation-field
   (scene-authored-light-generation scene)))

(defun scene-voxel-light (scene)
  "Compatibility name for SCENE-AUTHORED-VOXEL-LIGHT.

This is not necessarily the field visible in a rendered static or streaming
view.  Inspect SCENE-MESH-GENERATION-LIGHT-GENERATION, or the installed
STREAMING-SCENE-LIGHT-GENERATION, for profile-dependent realized torch light."
  (scene-authored-voxel-light scene))

(defclass torch-attachment ()
  ((support-cell :initarg :support-cell :reader torch-attachment-support-cell)
   (face :initarg :face :reader torch-attachment-face)
   (clearance-cell :initarg :clearance-cell
                   :reader torch-attachment-clearance-cell)
   (chart-u :initarg :chart-u :initform 0.0f0
            :reader torch-attachment-chart-u)
   (chart-v :initarg :chart-v :initform 0.0f0
            :reader torch-attachment-chart-v))
  (:documentation
   "A sparse semantic attachment keyed by an oriented face-chart point.

CLEARANCE-CELL is only the authored outward cell that must remain air.  It is
not a light source; the source is the wick of the final realized frame."))

(defconstant +unkeyed-scene-mesh-output+ :unkeyed-scene-mesh-output)

(defun copy-scene-generation-stamp-value (value)
  "Defensively own the cons/array structure of one exact generation stamp."
  (cond
    ((consp value)
     (cons (copy-scene-generation-stamp-value (car value))
           (copy-scene-generation-stamp-value (cdr value))))
    ((arrayp value)
     (let ((copy
             (make-array
              (array-dimensions value)
              :element-type (array-element-type value))))
       (loop for index below (array-total-size value) do
         (setf (row-major-aref copy index)
               (copy-scene-generation-stamp-value
                (row-major-aref value index))))
       copy))
    (t value)))

(defstruct (surface-mesh-tree-manifest
             (:constructor %make-surface-mesh-tree-manifest
                 (nodes child-counts attachment-frames))
             (:copier nil))
  "Private exact identity/derived-frame snapshot of one immutable mesh tree.

NODES and CHILD-COUNTS are parallel preorder vectors.  Geometry arrays are
ownership-immutable after generation; exact object identity therefore names
their topology.  ATTACHMENT-FRAMES additionally copies every mutable packed
torch-frame scalar, so later companion or frame mutation invalidates the
manifest instead of silently changing a published cohort."
  (nodes #() :type simple-vector :read-only t)
  (child-counts
    #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  (attachment-frames #() :type simple-vector :read-only t))

(defstruct (scene-mesh-output-manifest-entry
             (:constructor %make-scene-mesh-output-manifest-entry
                 (key keyed-p tree))
             (:copier nil))
  "One keyed streaming output, or one explicitly unkeyed static root."
  (key nil :read-only t)
  (keyed-p nil :type boolean :read-only t)
  (tree nil :type surface-mesh-tree-manifest :read-only t))

(defun surface-mesh-attachment-frame-snapshot (mesh)
  (let* ((frames (luft:surface-mesh-attachments mesh))
         (result
           (make-array
            (* (length frames) +torch-flame-instance-scalar-count+)
            :element-type 'single-float)))
    (loop for frame in frames
          for offset from 0 by +torch-flame-instance-scalar-count+ do
            (check-type frame torch-flame-instance-data)
            (replace result frame :start1 offset))
    result))

(defun snapshot-surface-mesh-tree (root)
  "Snapshot ROOT's exact object tree, companion shape, and frame scalars."
  (check-type root luft:surface-mesh)
  (let ((nodes nil)
        (child-counts nil)
        (attachment-frames nil)
        (path (make-hash-table :test #'eq)))
    (labels ((walk (mesh)
               (when (gethash mesh path)
                 (error "Surface mesh companion graph contains a cycle."))
               (setf (gethash mesh path) t)
               (let ((companions (luft:surface-mesh-companions mesh)))
                 (push mesh nodes)
                 (push (length companions) child-counts)
                 (push (surface-mesh-attachment-frame-snapshot mesh)
                       attachment-frames)
                 (dolist (companion companions) (walk companion)))
               (remhash mesh path)))
      (walk root))
    (%make-surface-mesh-tree-manifest
     (coerce (nreverse nodes) 'simple-vector)
     (coerce (nreverse child-counts)
             '(simple-array (unsigned-byte 32) (*)))
     (coerce (nreverse attachment-frames) 'simple-vector))))

(defun surface-mesh-frame-snapshot= (snapshot mesh)
  (let ((current (luft:surface-mesh-attachments mesh)))
    (and (= (length snapshot)
            (* (length current) +torch-flame-instance-scalar-count+))
         (loop for frame in current
               for offset from 0 by +torch-flame-instance-scalar-count+
               always
               (and (typep frame 'torch-flame-instance-data)
                    (loop for lane below +torch-flame-instance-scalar-count+
                          always
                          (eql (aref snapshot (+ offset lane))
                               (aref frame lane))))))))

(defun surface-mesh-tree-manifest-matches-p (manifest root)
  "Whether ROOT still has MANIFEST's exact tree identity and frame bytes."
  (let ((nodes (surface-mesh-tree-manifest-nodes manifest))
        (child-counts
          (surface-mesh-tree-manifest-child-counts manifest))
        (attachment-frames
          (surface-mesh-tree-manifest-attachment-frames manifest))
        (index 0))
    (labels ((walk (mesh)
               (let ((companions (luft:surface-mesh-companions mesh)))
                 (when (or (>= index (length nodes))
                           (not (eq mesh (aref nodes index)))
                           (/= (length companions)
                               (aref child-counts index))
                           (not
                            (surface-mesh-frame-snapshot=
                             (aref attachment-frames index) mesh)))
                   (return-from surface-mesh-tree-manifest-matches-p nil))
                 (incf index)
                 (dolist (companion companions) (walk companion)))))
      (walk root))
    (= index (length nodes))))

(defun make-scene-mesh-output-manifest (entries &key unkeyed-p)
  "Defensively snapshot exact output tree identities in ENTRIES.

Normal streaming entries are owner-keyed.  A public static root has no renderer
slot key yet, so UNKEYED-P explicitly permits its single tree to bind once at
publication."
  (when (and unkeyed-p (/= (length entries) 1))
    (error "An unkeyed scene generation must contain exactly one mesh tree."))
  (let ((roots (make-hash-table :test #'eq)))
    (coerce
     (loop for (key . mesh) in entries
           when (gethash mesh roots)
             do (error "One mesh tree appears more than once in a generation manifest.")
           do (setf (gethash mesh roots) t)
           collect
           (%make-scene-mesh-output-manifest-entry
            (if unkeyed-p +unkeyed-scene-mesh-output+ key)
            (not unkeyed-p)
            (snapshot-surface-mesh-tree mesh)))
     'simple-vector)))

(defstruct (scene-mesh-generation
             (:constructor %make-scene-mesh-generation
                 (scene request-stamp light-generation result-stamp
                  mesh-manifest slot-provenances))
             (:copier nil))
  "One immutable profile/residency-specific mesh-light generation.

REQUEST-STAMP names the exact geometry and torch semantics observed before
meshing.  RESULT-STAMP pairs it with the exact quantized realized-light stamp.
Meshes retain the generation's FIELD directly; publication retains this value
so visible geometry, light sidecars, body flags, and flame frames stay one
inspectable cohort."
  (scene nil :type scene :read-only t)
  (request-stamp nil :read-only t)
  (light-generation nil :type realized-light-generation :read-only t)
  (result-stamp nil :read-only t)
  ;; These are deliberately private, defensively owned vectors.  The public
  ;; request/result stamp remains semantic reproducibility evidence; exact
  ;; output object identity is a publication invariant, not EQUALP semantics.
  (mesh-manifest #() :type simple-vector :read-only t)
  (slot-provenances #() :type simple-vector :read-only t))

(defstruct (unlit-torch-frame
             (:constructor %make-unlit-torch-frame
                 (target attachment surface-frame wick-point))
             (:copier nil))
  "One final-surface attachment frame before the light field is solved."
  (target nil :type luft:surface-mesh :read-only t)
  (attachment nil :type torch-attachment :read-only t)
  (surface-frame nil :type luft:surface-attachment-frame :read-only t)
  (wick-point #() :type (simple-array single-float (3)) :read-only t))

(defclass scene-builder ()
  ((domain :initarg :domain :reader scene-builder-domain)
   (origin-x :initarg :origin-x :initform 0 :reader scene-builder-origin-x)
   (origin-y :initarg :origin-y :initform 0 :reader scene-builder-origin-y)
   (cells :initform (make-hash-table :test #'eql) :reader scene-builder-cells)
   (material-vocabulary :initform (make-scene-material-vocabulary)
                        :reader scene-builder-material-vocabulary)
   (material-cells :initform (make-hash-table :test #'eql)
                   :reader scene-builder-material-cells)
   (torches :initform (make-hash-table :test #'eql)
            :reader scene-builder-torches)
   (light-revision :initform 0 :accessor scene-builder-light-revision)))

(defun make-scene-builder (&key (horizontal-bits 6) (origin-x 0) (origin-y 0))
  (make-instance 'scene-builder
                 :domain (luft:make-world-domain
                          :x-bits horizontal-bits :y-bits horizontal-bits)
                 :origin-x origin-x :origin-y origin-y))

(defun scene-builder-cell
    (builder x y z &key (solid-p t) architecture-p (material nil material-p))
  (when (<= 0 z 254)
    (let ((site (luft:make-site
                 (scene-builder-domain builder)
                 (+ x (scene-builder-origin-x builder))
                 (+ y (scene-builder-origin-y builder))
                 z luft:+cell-extent+ 1)))
      (if solid-p
          (let ((placement
                  (if material-p material
                      (if architecture-p *sanctuary-material-placement*
                          *terrain-material-placement*))))
            (check-type placement material-placement)
            (setf (gethash site (scene-builder-cells builder)) t
                  (gethash site (scene-builder-material-cells builder))
                  (domains:identity-vocabulary-offset
                   (scene-builder-material-vocabulary builder) placement)))
          (progn
            (remhash site (scene-builder-cells builder))
            (remhash site (scene-builder-material-cells builder))))))
  (incf (scene-builder-light-revision builder))
  builder)

(defun scene-builder-torch (builder x y z axis side &key (u 0.0) (v 0.0))
  "Attach a torch to SUPPORT cell X/Y/Z's outward AXIS/SIDE face.

The attachment is not voxel occupancy.  The adjacent outward cell is clearance
only; its light source is the wick derived from the final realized frame.  U
and V are stable normalized coordinates in the oriented face chart; zero is
the face centre, edge values resolve onto bevel bands, and the logical square's
corners compact continuously onto the junction domain rather than missing a
chamfered corner.  Geometry and flame frames are resolved from the final
surface, so the same identity survives every width."
  (check-type axis luft:axis)
  (check-type side luft:side)
  (unless (and (realp u) (<= -1 u 1) (realp v) (<= -1 v 1))
    (error "Torch chart coordinates must lie in [-1,1], not (~S,~S)." u v))
  (let* ((domain (scene-builder-domain builder))
         (support
           (luft:make-site domain
                           (+ x (scene-builder-origin-x builder))
                           (+ y (scene-builder-origin-y builder)) z
                           luft:+cell-extent+ 1))
         (face (ecase side
                 (:low (luft:site-boundary-low domain support axis))
                 (:high (luft:site-boundary-high domain support axis))))
         (clearance (luft:step-site domain support axis
                                    (if (eq side :low) -1 1))))
    (unless clearance
      (error "Torch face ~S points outside the LUFT domain." face))
    (setf (gethash face (scene-builder-torches builder))
          (make-instance 'torch-attachment
                         :support-cell support :face face
                         :clearance-cell clearance
                         :chart-u (coerce u 'single-float)
                         :chart-v (coerce v 'single-float))))
  (incf (scene-builder-light-revision builder))
  builder)

(defun scene-builder-box
    (builder x0 x1 y0 y1 z0 z1
     &key (solid-p t) architecture-p
       (material (if architecture-p *sanctuary-material-placement*
                     *terrain-material-placement*)))
  (loop for z from z0 to z1 do
    (loop for y from y0 to y1 do
      (loop for x from x0 to x1 do
        (scene-builder-cell builder x y z :solid-p solid-p
                                           :material material))))
  builder)

(defun scene-builder-disc
    (builder cx cy radius z0 z1
     &key (solid-p t) architecture-p
       (material (if architecture-p *sanctuary-material-placement*
                     *terrain-material-placement*)))
  (let ((limit (expt (+ radius 0.5) 2)))
    (loop for x from (- cx (ceiling radius)) to (+ cx (ceiling radius)) do
      (loop for y from (- cy (ceiling radius)) to (+ cy (ceiling radius))
            for dx = (- (+ x 0.5) (+ cx 0.5))
            for dy = (- (+ y 0.5) (+ cy 0.5))
            when (<= (+ (* dx dx) (* dy dy)) limit)
              do (loop for z from z0 to z1 do
                   (scene-builder-cell builder x y z :solid-p solid-p
                                                      :material material)))))
  builder)

(defun scene-builder-ring
    (builder cx cy inner outer z0 z1
     &key (solid-p t) architecture-p
       (material (if architecture-p *sanctuary-material-placement*
                     *terrain-material-placement*)))
  (let ((low (expt (+ inner 0.5) 2))
        (high (expt (+ outer 0.5) 2)))
    (loop for x from (- cx (ceiling outer)) to (+ cx (ceiling outer)) do
      (loop for y from (- cy (ceiling outer)) to (+ cy (ceiling outer))
            for dx = (- (+ x 0.5) (+ cx 0.5))
            for dy = (- (+ y 0.5) (+ cy 0.5))
            for distance = (+ (* dx dx) (* dy dy))
            when (and (< low distance) (<= distance high))
              do (loop for z from z0 to z1 do
                   (scene-builder-cell builder x y z :solid-p solid-p
                                                      :material material)))))
  builder)

(defun arch-rise (offset radius)
  (let ((square (- (* radius radius) (* offset offset))))
    (if (plusp square) (round (sqrt square)) 0)))

(defun scene-builder-carve-arch
    (builder centre floor springing radius across &key (axis :x))
  (destructuring-bind (near . far) across
    (loop for offset from (- radius) to radius
          for rise = (arch-rise offset radius)
          when (plusp rise) do
            (loop for z from floor below (+ springing rise) do
              (loop for other from near to far do
                (if (eq axis :x)
                    (scene-builder-cell builder (+ centre offset) other z
                                        :solid-p nil)
                    (scene-builder-cell builder other (+ centre offset) z
                                        :solid-p nil))))))
  builder)

(defun scene-builder-corbel (builder x0 x1 y0 y1 z courses)
  (loop for course from 0 below courses
        for out = (1+ course)
        do (scene-builder-box builder (- x0 out) (+ x1 out)
                              (- y0 out) (+ y1 out)
                              (+ z course) (+ z course)
                              :architecture-p t))
  builder)

(defun scene-builder-crenellate (builder x0 x1 y0 y1 z)
  (loop for x from x0 to x1 do
    (loop for y from y0 to y1
          when (and (or (= x x0) (= x x1) (= y y0) (= y y1))
                    (zerop (mod (+ x y) 2)))
            do (scene-builder-cell builder x y z :architecture-p t)
               (scene-builder-cell builder x y (1+ z) :architecture-p t)))
  builder)

(defun scene-builder-staircase
    (builder x0 x1 y0 step-count base-top
     &key (boundary :open))
  "Author an ascending masonry stair and its optional support boundary.

BOUNDARY is a deliberately small modeling vocabulary.  :OPEN emits only the
treads, :BORDER adds a one-cell stone strip level with each tread, and
:LOW-WALL raises that strip one course above it.  The extra cells are ordinary
authored solid and material input; the bevel mesher receives no special-case
stair topology. #WSEK3C"
  (check-type boundary (member :open :border :low-wall))
  (let ((boundary-rise
          (ecase boundary
            (:open nil)
            (:border 0)
            (:low-wall 1))))
    (loop for step below step-count
          for y from y0
          for top = (+ base-top step)
          do (scene-builder-box builder x0 x1 y y 0 top
                                :architecture-p t)
             (when boundary-rise
               (scene-builder-box builder (1- x0) (1- x0) y y
                                   0 (+ top boundary-rise)
                                   :architecture-p t)
               (scene-builder-box builder (1+ x1) (1+ x1) y y
                                   0 (+ top boundary-rise)
                                   :architecture-p t))))
  builder)

(defun copy-scene-builder-material-cells (builder)
  "Return a fresh scene-owned copy of BUILDER's authored placement offsets."
  (let ((copy (make-hash-table
               :test #'eql
               :size (hash-table-count
                      (scene-builder-material-cells builder)))))
    (maphash (lambda (cell offset)
               (setf (gethash cell copy) offset))
             (scene-builder-material-cells builder))
    copy))

(defun copy-scene-builder-material-vocabulary (builder)
  "Clone BUILDER's placement vocabulary without changing any dense offset."
  (domains:make-identity-vocabulary-domain
   :members
   (coerce
    (domains:identity-vocabulary-members
     (scene-builder-material-vocabulary builder))
    'list)
   :limit #xff))

(defun finish-scene-builder
    (builder &key player-p (voxel-light-propagation-p t))
  (check-type voxel-light-propagation-p boolean)
  (let* ((cells (scene-builder-cells builder))
         ;; FINISH is an ownership boundary, not builder consumption.  A
         ;; builder may author a successor scene, but can never mutate the
         ;; finished scene's dense placement field or vocabulary contract.
         (material-cells (copy-scene-builder-material-cells builder))
         (material-vocabulary
           (copy-scene-builder-material-vocabulary builder))
         (chain-builder
           (luft:make-chain-builder (scene-builder-domain builder)
                                    :initial-capacity (hash-table-count cells))))
    (maphash (lambda (site present-p)
               (declare (ignore present-p))
               (luft:chain-builder-add-site chain-builder site))
             cells)
    (let ((torches nil))
      (maphash
       (lambda (face attachment)
         (declare (ignore face))
         (unless (gethash (torch-attachment-support-cell attachment) cells)
           (error "Torch support cell ~S is not occupied."
                  (torch-attachment-support-cell attachment)))
         (when (gethash (torch-attachment-clearance-cell attachment) cells)
           (error "Torch outward cell ~S is occupied."
                  (torch-attachment-clearance-cell attachment)))
         (push attachment torches))
       (scene-builder-torches builder))
      (setf torches
            (coerce (sort torches #'< :key #'torch-attachment-face) 'vector))
      (let* ((solid (luft:finish-chain-builder chain-builder))
             (sources
               (coerce
                (sort
                 (compile-material-light-sources
                  material-cells material-vocabulary)
                 #'<)
                '(simple-array (unsigned-byte 64) (*))))
             (opacity-table
               (compile-material-light-opacity-table material-vocabulary))
             (revision (scene-builder-light-revision builder))
             (authored-light-provenance (gensym "AUTHORED-LIGHT-"))
             (torch-light-emission
               (material-kind-packed-light-emission *torch-flame-material*))
             (base-generation
               (solve-realized-light-generation
                (scene-builder-domain builder) material-cells opacity-table
                (if voxel-light-propagation-p sources #())
                authored-light-provenance revision
                (make-realized-light-seeds #() #())
                :field-revision revision)))
        (make-instance
         'scene
         :solid solid
         :player-p player-p
         :material-vocabulary material-vocabulary
         :material-cells material-cells
         :torches torches
         :authored-light-sources sources
         :authored-light-opacity-table opacity-table
         :authored-light-revision revision
         :authored-light-provenance authored-light-provenance
         :authored-light-generation base-generation
         :torch-light-emission torch-light-emission
         :voxel-light-propagation-p voxel-light-propagation-p)))))
