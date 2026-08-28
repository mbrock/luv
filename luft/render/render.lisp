(in-package #:luft.render)

(defparameter *wireframe* 0.0
  "Global construction-edge strength.  The atelier toggles it between 0 and 1.")

(defparameter *render-scale* 0.75
  "Linear internal resolution of the LUFT scene before temporal upscaling.")

(defparameter *scene-sample-count* 4
  "Raster samples used by Luft's geometry, motion, and depth scene pass.")

(defparameter *temporal-upscaling-p* t
  "Whether LUFT uses temporal reconstruction on supported GPU devices.")

(defparameter *vulkan-temporal-history-weight* 0.97f0
  "Baseline retained history for Luft's inspectable Vulkan temporal resolve.")

(defparameter *flame-time* nil
  "Optional deterministic torch-flame time in seconds.

NIL lets the live viewer pass its monotonic presentation clock.  Captures may
dynamically bind a real value to reproduce the exact same flame field.")

(defconstant +exposure-probe-width+ 32)
(defconstant +exposure-probe-height+ 16)
(defconstant +exposure-probe-buffer-count+ 3)
(defconstant +exposure-probe-byte-count+
  (* 4 +exposure-probe-width+ +exposure-probe-height+))

(defconstant +sanctuary-origin-x+ 32)
(defconstant +sanctuary-origin-y+ 24)
(defparameter *sanctuary-beacon-x* 58)
(defparameter *sanctuary-beacon-y* 54)

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

(defun make-manifold-spike-scene ()
  "Three isolated singular-star fixtures for the manifold-sheet spike.

The plots exercise an edge-touching pair, a corner-touching pair, and the
four-sheet parity star.  Nothing else in the scene can hide their junctions.
#WSEK3C"
  (let ((builder (make-scene-builder :horizontal-bits 6)))
    (labels ((place-star (mask centre-x)
               (dotimes (sample 8)
                 (when (logbitp sample mask)
                   (scene-builder-cell
                    builder
                    (+ centre-x (if (logbitp 0 sample) 0 -1))
                    (+ 10 (if (logbitp 1 sample) 0 -1))
                    (+ 6 (if (logbitp 2 sample) 0 -1)))))))
      (place-star #x06 10)
      (place-star #x18 14)
      (place-star #x69 18))
    (finish-scene-builder builder)))

(defun make-bevel-limit-study-scene ()
  "One isolated stone cell for comparing sub-medial and medial bevels."
  (let ((builder (make-scene-builder :horizontal-bits 4)))
    (scene-builder-cell builder 6 4 3 :architecture-p t)
    (finish-scene-builder builder)))

(defun make-voxel-light-shrine-scene (&key (voxel-light-propagation-p t))
  "A compact production fixture for colored propagation and face torches."
  (let ((builder (make-scene-builder :horizontal-bits 6)))
    ;; A pale receiving room with a dark backing visible through the crystal.
    (scene-builder-box builder 6 18 6 18 4 4 :architecture-p t)
    (scene-builder-box builder 6 18 18 18 5 13 :architecture-p t)
    (scene-builder-box builder 6 6 7 18 5 11 :architecture-p t)
    (scene-builder-box builder 7 17 9 11 12 12 :architecture-p t)
    (scene-builder-box builder 10 14 17 17 5 9
                       :material *highland-rock-material-placement*)
    ;; Medial crystal silhouettes are point-contact jewels, not glass cubes.
    (scene-builder-cell builder 12 13 5
                        :material *crystal-material-placement*)
    (scene-builder-cell builder 15 15 5
                        :material *crystal-material-placement*)
    ;; Floor, back-wall, side-wall, and ceiling attachments exercise four
    ;; normals while remaining the same geometry at every bevel width.
    (scene-builder-torch builder 8 10 4 :z :high)
    (scene-builder-torch builder 8 18 8 :y :low)
    (scene-builder-torch builder 6 14 7 :x :high)
    (scene-builder-torch builder 16 10 12 :z :low)
    (finish-scene-builder
     builder :voxel-light-propagation-p voxel-light-propagation-p)))

(defconstant +sanctuary-plateau-height+ 19)

(defun mountain-sanctuary-terrain-height (x y)
  "The authored local-coordinate height of the sanctuary's mountain world."
  (let ((shore 11) (water 2) (plateau +sanctuary-plateau-height+))
    (floor
     (cond
       ((< y 14)
        (max water
             (+ shore
                (* 1.4 (sin (/ x 11.0)))
                (* 1.1 (sin (/ (+ x (* 0.75 y)) 9.0)))
                (- (* 1.6 (max 0 (- y 9))))
                ;; A long diagonal shoulder lifts the far eastern approach
                ;; without changing the bridge landing.
                (* 0.10 (max 0 (- (- x (* 0.9 y)) 58))))))
       ((>= y 36)
        (let* ((edge (+ 2.0 (* 3.0 (sin (/ x 9.0)))
                           (* 1.5 (sin (/ x 3.7)))))
               (inland (- y 36 edge)))
          (if (>= inland 0)
              (let* ((ridge
                       (min 11.0
                            (* 0.22 (max 0 (- (+ y (* 0.55 x)) 88)))))
                     (ravine
                       (max 0.0
                            (- 4.5
                               (* 1.35
                                  (abs (- y (+ 67 (* 0.34 x))))))))
                     (rolling
                       (* 1.3 (sin (/ x 12.0)) (cos (/ y 13.0)))))
                (+ plateau rolling ridge (- ravine)))
              (max water (+ plateau (* 9.0 inland))))))
       (t water)))))

(defun mountain-sanctuary-terrain-x-bounds (y)
  "Return the authored inclusive terrain span at local Y."
  (when (<= -15 y 81)
    (values
     (max -18 (+ -17 (round (* 1.8 (sin (/ y 6.0))))))
     (min 82 (- 81 (round (* 2.2 (cos (/ y 8.0))))))
     t)))

(defun scene-builder-mountain-border-wall (builder)
  "Guard the elevated authored rim with a limestone parapet.

The continuous two-course body exceeds the player's one-cell step.  Every
other column rises into a third course, making the finite scenery legible as
an intentional battlement rather than the accidental edge of a voxel field."
  (labels ((wall-column (x y)
             (let ((height (mountain-sanctuary-terrain-height x y)))
               (when (>= height +sanctuary-plateau-height+)
                 (scene-builder-box builder x x y y height (1+ height)
                                    :architecture-p t)
                 (when (evenp (+ x y))
                   (scene-builder-cell builder x y (+ height 2)
                                       :architecture-p t))))))
    ;; The side contours follow the terrain's authored west/east banks.
    (loop for y from -15 to 81 do
      (multiple-value-bind (west east present-p)
          (mountain-sanctuary-terrain-x-bounds y)
        (when present-p
          (wall-column west y)
          (wall-column east y))))
    ;; Close the elevated northern rim between those side contours.
    (multiple-value-bind (west east present-p)
        (mountain-sanctuary-terrain-x-bounds 81)
      (when present-p
        (loop for x from west to east do (wall-column x 81)))))
  builder)

(defun make-mountain-sanctuary-scene
    (&key (beacon-placement *beacon-material-placement*)
      (stair-boundary :low-wall) (player-p t))
  "A broad Lonely-Mountains world carrying a bridge and walled sanctuary.

This is the old Holm's architectural sentence with its material menagerie
removed: rolling approaches, a channel and rising high rock; a diagonal
processional way; a two-arched stone bridge; a gate, curtain wall, paired
turrets, an arcaded hall and a remote ridge beacon."
  (let* ((builder (make-scene-builder
                   :horizontal-bits 7
                   :origin-x +sanctuary-origin-x+
                   :origin-y +sanctuary-origin-y+))
         (water 2) (plateau +sanctuary-plateau-height+) (deck 13)
         (springing 7) (radius 4) (across (cons 27 32)))
    ;; Keep the packed world bounded away from its toroidal seam, while the
    ;; visible camera sees terrain continuing beyond every frame edge.
    (loop for y from -15 to 81 do
      (multiple-value-bind (west east present-p)
          (mountain-sanctuary-terrain-x-bounds y)
        (when present-p
          (loop for x from west to east do
            (let* ((height
                     (max 1 (mountain-sanctuary-terrain-height x y)))
                   ;; Keep one living-earth cap.  Only cells actually exposed
                   ;; above a lower cardinal neighbor become cliff rock; this
                   ;; is authored cell material, never material topology.
                   (exposed-base
                     (min (mountain-sanctuary-terrain-height (1- x) y)
                          (mountain-sanctuary-terrain-height (1+ x) y)
                          (mountain-sanctuary-terrain-height x (1- y))
                          (mountain-sanctuary-terrain-height x (1+ y)))))
              (loop for z below height do
                (scene-builder-cell
                 builder x y z
                 :material
                 (if (and (< z (1- height)) (>= z exposed-base))
                     *highland-rock-material-placement*
                     *terrain-material-placement*))))))))
    (scene-builder-mountain-border-wall builder)
    ;; A diagonal, gently climbing processional way makes the bridge belong
    ;; to the low country instead of beginning at the edge of the model.
    (loop for step from 0 below 20
          for x = (+ 6 step)
          for y = (+ -14 step)
          for top = (max (mountain-sanctuary-terrain-height x y)
                         (min deck (+ 10 (floor step 5))))
          do (scene-builder-box builder x (+ x 4) y (1+ y) 0 top
                                :architecture-p t)
             (when (zerop (mod step 4))
               (scene-builder-cell builder x y (1+ top)
                                   :architecture-p t)
               (scene-builder-cell builder (+ x 4) (1+ y) (1+ top)
                                   :architecture-p t)))
    (dolist (y '(15 25 35))
      (scene-builder-box builder 26 33 (1- y) (1+ y) water springing
                         :architecture-p t))
    (scene-builder-box builder 27 32 12 38 water (1- deck)
                       :architecture-p t)
    (dolist (arch '(20 30))
      (scene-builder-carve-arch builder arch (1+ water) springing radius
                                across :axis :y))
    (scene-builder-corbel builder 27 32 8 42 (1- deck) 1)
    (scene-builder-box builder 25 34 6 44 deck deck :architecture-p t)
    (check-type stair-boundary (member :open :border :low-wall))
    ;; The old bridge rail overlaps the first six stair courses.  Preserve it
    ;; for the open historical scene, but let authored stair boundaries own
    ;; that stretch so the comparison changes one modeling decision at a time.
    (loop for y from 6 to (if (eq stair-boundary :open) 44 38) do
      (scene-builder-cell builder 25 y (1+ deck) :architecture-p t)
      (scene-builder-cell builder 34 y (1+ deck) :architecture-p t)
      (when (zerop (mod y 5))
        (scene-builder-cell builder 25 y (+ deck 2) :architecture-p t)
        (scene-builder-cell builder 34 y (+ deck 2) :architecture-p t)))
    (scene-builder-box builder 26 33 38 47 deck (+ plateau 5) :solid-p nil)
    (scene-builder-staircase builder 26 33 39 7 deck
                             :boundary stair-boundary)
    ;; Bed the sanctuary into the uneven ridge before raising its walls.  The
    ;; two-course podium is shallow enough to disappear into the higher turf,
    ;; but where the mountain falls away it remains a continuous load path
    ;; instead of leaving the fixed-height curtain visibly suspended in air.
    ;; Its wider tower shoes also give the round keeps a deliberate transition
    ;; into the rectilinear retaining work.
    (scene-builder-box builder 13 47 44 48 (- plateau 2) (1- plateau)
                       :architecture-p t)
    (scene-builder-box builder 13 17 49 62 (- plateau 2) (1- plateau)
                       :architecture-p t)
    (scene-builder-box builder 43 47 49 62 (- plateau 2) (1- plateau)
                       :architecture-p t)
    (scene-builder-box builder 18 42 59 62 (- plateau 2) (1- plateau)
                       :architecture-p t)
    (dolist (corner '((15 46) (45 46)))
      (destructuring-bind (cx cy) corner
        (scene-builder-disc builder cx cy 6 (- plateau 2) (1- plateau)
                            :architecture-p t)))
    (scene-builder-box builder 14 46 45 47 plateau (+ plateau 6)
                       :architecture-p t)
    (scene-builder-box builder 14 16 45 61 plateau (+ plateau 6)
                       :architecture-p t)
    (scene-builder-box builder 44 46 45 61 plateau (+ plateau 6)
                       :architecture-p t)
    (scene-builder-corbel builder 14 46 45 61 (+ plateau 7) 1)
    (scene-builder-crenellate builder 13 47 44 62 (+ plateau 8))
    (scene-builder-carve-arch builder 30 plateau (+ plateau 3) 3
                              (cons 45 47))
    (dolist (corner '((15 46) (45 46)))
      (destructuring-bind (cx cy) corner
        (scene-builder-disc builder cx cy 5 (1- plateau) plateau
                            :architecture-p t)
        (scene-builder-ring builder cx cy 2 4 plateau (+ plateau 9)
                            :architecture-p t)
        (scene-builder-ring builder cx cy 2 5 (+ plateau 10) (+ plateau 11)
                            :architecture-p t)
        (scene-builder-disc builder cx cy 3 (+ plateau 11) (+ plateau 11)
                            :architecture-p t)))
    (scene-builder-box builder 20 40 55 60 plateau (+ plateau 5)
                       :architecture-p t)
    (scene-builder-box builder 21 39 56 59 (1+ plateau) (+ plateau 5)
                       :solid-p nil)
    (dolist (bay '(25 30 35))
      (scene-builder-carve-arch builder bay (1+ plateau) (+ plateau 3) 2
                                (cons 55 55)))
      ;; A hollow beacon on the eastern ridge gives the enlarged world a
      ;; distant inhabited landmark and a second architectural scale.
      (let* ((beacon-x *sanctuary-beacon-x*)
             (beacon-y *sanctuary-beacon-y*)
             (base (mountain-sanctuary-terrain-height beacon-x beacon-y)))
        (scene-builder-disc builder beacon-x beacon-y 3 base (1+ base)
                            :material beacon-placement)
        (scene-builder-ring builder beacon-x beacon-y 1 2 (+ base 2)
                            (+ base 6) :material beacon-placement)
        (scene-builder-ring builder beacon-x beacon-y 1 3 (+ base 7)
                            (+ base 8) :material beacon-placement)
        (scene-builder-disc builder beacon-x beacon-y 1 (+ base 8)
                            (+ base 8) :material beacon-placement))
      ;; An old eight-pillar sun court occupies the open lowland beside the
      ;; processional way.  Its diagonal stones echo the landscape's oblique
      ;; ridges without competing with the sanctuary's larger silhouette.
      (let* ((court-x 55)
             (court-y 4)
             (base (mountain-sanctuary-terrain-height court-x court-y)))
        (scene-builder-disc builder court-x court-y 5 base base
                            :architecture-p t)
        (dolist (offset '((-4 0) (-3 -3) (0 -4) (3 -3)
                          (4 0) (3 3) (0 4) (-3 3)))
          (destructuring-bind (dx dy) offset
            (scene-builder-box builder (+ court-x dx) (+ court-x dx)
                               (+ court-y dy) (+ court-y dy)
                               (1+ base) (+ base 4) :architecture-p t)
            (scene-builder-cell builder (+ court-x dx) (+ court-y dy)
                                (+ base 5) :architecture-p t))))
      ;; The sun never reaches the south-facing curtain, so the gate approach
      ;; shows both luminous materials live: warm flames flanking the gate and
      ;; cool crystal jewels bedded on the podium ledge below the wall.
      (scene-builder-torch builder 26 45 22 :y :low)
      (scene-builder-torch builder 34 45 22 :y :low)
      (scene-builder-cell builder 22 44 19
                          :material *crystal-material-placement*)
      (scene-builder-cell builder 38 44 19
                          :material *crystal-material-placement*)
      ;; The arcaded hall keeps a pair of flames on its north wall and one
      ;; floor crystal, visible through the central bay.
      (scene-builder-torch builder 26 60 22 :y :low)
      (scene-builder-torch builder 34 60 22 :y :low)
      (scene-builder-cell builder 30 57 20
                          :material *crystal-material-placement*)
      ;; Scattered jewels probe the mesher's material transitions: point
      ;; contacts capping three bridge-rail posts, corner stones completing
      ;; the parapet ring where its diagonal merlons leave a gap, one block
      ;; at a gate-jamb base, and shards bedded into the south cliff face.
      (dolist (post '((25 10) (34 20) (25 30)))
        (destructuring-bind (px py) post
          (scene-builder-cell builder px py (+ deck 3)
                              :material *crystal-material-placement*)))
      (scene-builder-cell builder 13 44 (+ plateau 8)
                          :material *crystal-material-placement*)
      (scene-builder-cell builder 47 44 (+ plateau 8)
                          :material *crystal-material-placement*)
      (scene-builder-cell builder 26 45 plateau
                          :material *crystal-material-placement*)
      (dolist (shard '((50 37 4) (52 37 3)))
        (destructuring-bind (sx sy depth) shard
          (scene-builder-cell
           builder sx sy
           (max 3 (- (mountain-sanctuary-terrain-height sx sy) depth))
           :material *crystal-material-placement*)))
      (finish-scene-builder builder :player-p player-p)))

(defun make-traveler-study-scene ()
  "A bare limestone dais under the traveler, clear from every direction.

The sanctuary is the scene he belongs in, but it is also a scene in which
half the useful camera angles look through a parapet.  This fixture keeps
his exact world position and his exact deck height and removes everything
else, so a turnaround can be shot around him without moving him."
  (let ((builder (make-scene-builder :horizontal-bits 7)))
    (scene-builder-box builder 56 67 40 58 11 13 :architecture-p t)
    (finish-scene-builder builder :player-p t)))

(defun make-miter-study-scene ()
  "Build the wall-side stepped mountain used to judge mixed miters. #Z5NDTA

The two L-shaped terraces retain five-, six-, and seven-cell vertex stars in
one architectural context.  Their back edges meet a continuous wall so the
same view also retains the truncated wall miter preserved by #DJK8HW."
  (let ((builder (make-scene-builder :horizontal-bits 5)))
    ;; Broad plinth and continuous back wall.
    (scene-builder-box builder 2 14 2 8 0 1 :architecture-p t)
    (scene-builder-box builder 2 14 8 9 0 7 :architecture-p t)
    ;; One isolated terrain cell makes the ordinary boulder-on-ground contact
    ;; inspectable beside the architectural mixed-miter cases.
    (scene-builder-cell builder 13 4 2)
    ;; The lower L supplies the outie, straight six-cell run, innie, and the
    ;; first wall termination.  The upper L repeats them without isolation.
    (scene-builder-box builder 4 11 5 7 2 2 :architecture-p t)
    (scene-builder-box builder 4 8 3 4 2 2 :architecture-p t)
    (scene-builder-box builder 5 10 6 7 3 3 :architecture-p t)
    (scene-builder-box builder 5 7 4 5 3 3 :architecture-p t)
    (finish-scene-builder builder)))

(defun face-solid-cell (solid face)
  "Return the occupied cell incident to boundary FACE and which side it is on."
  (let* ((domain (luft:chain-domain solid))
         (extent (luft:site-extent face))
         (axis (cond ((= extent luft:+xy-face-extent+) :z)
                     ((= extent luft:+xz-face-extent+) :y)
                     (t :x)))
         (x (luft:site-x face))
         (y (luft:site-y face))
         (z (luft:site-z face))
         (back-x (if (eq axis :x) (1- x) x))
         (back-y (if (eq axis :y) (1- y) y))
         (back-z (if (eq axis :z) (1- z) z)))
    (if (= 1 (luft:chain-cell-occupancy-bit solid x y z))
        (values (luft:make-site domain x y z luft:+cell-extent+ 1)
                axis :forward)
        (values (luft:make-site domain back-x back-y back-z
                                luft:+cell-extent+ 1)
                axis :backward))))

(defun default-face-stock (face)
  (mod (+ (luft:site-x face) (* 2 (luft:site-y face))
          (* 3 (luft:site-z face)) (luft:site-extent face))
       4))

(defun scene-authored-cell-occupied-p (scene cell)
  "Whether CELL is authored solid in SCENE's semantic material field."
  (nth-value 1 (gethash cell (scene-material-cells scene))))

(defun torch-support-surface-meshes (owners surface-context support-cell)
  "Return the at-most-four realized owners that can carry SUPPORT-CELL.

The half-open primitive convention emits a source owner's surface only to its
own owner and its +X, +Y, and +X+Y canonical owners.  Attachment resolution is
therefore local regardless of resident world size.  The explicit whole-domain
diagnostic owner NIL remains a single-mesh special form."
  (let* ((available (append owners surface-context))
         (whole-domain (assoc nil available)))
    (if whole-domain
        (list (cdr whole-domain))
        (let* ((support-key (luft:site-chunk-key support-cell))
               (support-x (luft:chunk-key-x support-key))
               (support-y (luft:chunk-key-y support-key)))
          (loop for delta-x from 0 to 1 append
            (loop for delta-y from 0 to 1
                  for key =
                    (luft:chunk-key-at
                     (* (+ support-x delta-x) luft:+chunk-size+)
                     (* (+ support-y delta-y) luft:+chunk-size+))
                  for entry = (assoc key available :test #'eql)
                  when entry collect (cdr entry)))))))

(defun pack-realized-torch-frame
    (scene attachment surface-frame wick-point light-generation)
  "Finalize ATTACHMENT's shared body/flame frame from realized light."
  (let* ((origin (luft:surface-attachment-frame-origin surface-frame))
         (normal (luft:surface-attachment-frame-normal surface-frame))
         (tangent (luft:surface-attachment-frame-tangent surface-frame))
         (seed (torch-flame-face-seed (torch-attachment-face attachment)))
         (field (realized-light-generation-field light-generation))
         (authored-light (scene-torch-light-emission scene))
         (packed-light
           (realized-torch-self-light
            field wick-point
            (lambda (cell) (scene-authored-cell-occupied-p scene cell))
            authored-light)))
    (pack-torch-flame-frame
     (aref origin 0) (aref origin 1) (aref origin 2) seed
     (aref normal 0) (aref normal 1) (aref normal 2)
     (pack-torch-body-frame-flags packed-light)
     (aref tangent 0) (aref tangent 1) (aref tangent 2) 1.0f0)))

(defun resolve-unlit-scene-torch-frames
    (owners scene surface-context resident-source-keys resident-source-keys-p)
  "Resolve resident torch attachments against one complete final surface."
  (let ((frames nil))
    (when owners
      (let ((whole-domain-p
              (and (= 1 (length owners)) (null (caar owners)))))
        (loop for attachment across (scene-torches scene)
              for owner-key = (luft:site-chunk-key
                               (torch-attachment-support-cell attachment))
              for target = (if whole-domain-p
                               (first owners)
                               (assoc owner-key owners :test #'eql))
              when (and target
                        (or (not resident-source-keys-p)
                            (member owner-key resident-source-keys
                                    :test #'eql))) do
                (let* ((meshes
                         (torch-support-surface-meshes
                          owners surface-context
                          (torch-attachment-support-cell attachment)))
                       (surface-frame
                         (luft:resolve-surface-attachment-frame
                          meshes (torch-attachment-face attachment)
                          :u (torch-attachment-chart-u attachment)
                          :v (torch-attachment-chart-v attachment)))
                       (wick-point
                         (realized-torch-wick-point
                          (luft:surface-attachment-frame-origin surface-frame)
                          (luft:surface-attachment-frame-normal surface-frame)
                          1.0f0 +torch-flame-wick-offset+)))
                  (push
                   (%make-unlit-torch-frame
                    (cdr target) attachment surface-frame wick-point)
                   frames)))))
    (nreverse frames)))

(defun unlit-torch-frame-seeds (scene frame)
  (realized-torch-light-seeds
   (luft:chain-domain (scene-solid scene))
   (lambda (cell) (scene-authored-cell-occupied-p scene cell))
   (unlit-torch-frame-wick-point frame)
   (scene-torch-light-emission scene)))

(defun scene-realized-light-generation
    (scene frames &optional reusable-light-generation)
  "Solve FRAMES, reusing an exact already installed source generation."
  (when (and (typep reusable-light-generation 'scene-mesh-generation)
             (not
              (eq (scene-authored-light-provenance scene)
                  (scene-authored-light-provenance
                   (scene-mesh-generation-scene
                    reusable-light-generation)))))
    (error "A reusable scene generation belongs to a different authored light input."))
  (let* ((seeds
           (if (scene-voxel-light-propagation-p scene)
               (apply #'merge-realized-light-seeds
                      (mapcar (lambda (frame)
                                (unlit-torch-frame-seeds scene frame))
                              frames))
               (make-realized-light-seeds #() #())))
         (stamp
           (make-realized-light-stamp
            (scene-authored-light-provenance scene)
            (scene-authored-light-revision scene) seeds))
         (reusable
           (etypecase reusable-light-generation
             (null nil)
             (realized-light-generation reusable-light-generation)
             (scene-mesh-generation
              (scene-mesh-generation-light-generation
               reusable-light-generation)))))
    (cond
      ((and reusable
            (realized-light-stamp=
             stamp (realized-light-generation-stamp reusable)))
       reusable)
      ((realized-light-stamp=
        stamp
        (realized-light-generation-stamp
         (scene-authored-light-generation scene)))
       (scene-authored-light-generation scene))
      (t
       (solve-realized-light-generation
        (luft:chain-domain (scene-solid scene))
        (scene-material-cells scene)
        (scene-authored-light-opacity-table scene)
        (if (scene-voxel-light-propagation-p scene)
            (scene-authored-light-sources scene)
            #())
        (scene-authored-light-provenance scene)
        (scene-authored-light-revision scene)
        seeds :field-revision (scene-authored-light-revision scene))))))

(defun make-scene-mesh-generation-value
    (scene request-stamp light-generation
     &key mesh-entries unkeyed-mesh-p slot-provenances)
  "Construct one defensively owned semantic and exact-output generation."
  (let* ((request-stamp
           (copy-scene-generation-stamp-value request-stamp))
         (stamp (realized-light-generation-stamp light-generation))
         (result-stamp
           (copy-scene-generation-stamp-value
            (list request-stamp
                  (realized-light-stamp-authored-light-provenance stamp)
                  (realized-light-stamp-authored-light-revision stamp)
                  (realized-light-stamp-seed-sites stamp)
                  (realized-light-stamp-seed-lights stamp))))
         (manifest
           (if mesh-entries
               (make-scene-mesh-output-manifest
                mesh-entries :unkeyed-p unkeyed-mesh-p)
               #()))
         (slot-provenances
           (if slot-provenances
               (coerce (copy-seq slot-provenances) 'simple-vector)
               #())))
    (unless (or (zerop (length slot-provenances))
                (= (length slot-provenances) (length manifest)))
      (error "A merged scene generation needs one provenance per mesh output."))
    (%make-scene-mesh-generation
     scene request-stamp light-generation result-stamp
     manifest slot-provenances)))

(defun scene-mesh-generation-result-stamp= (left right)
  "Exact structural equality for two mesh/light result stamps."
  (and (typep left 'scene-mesh-generation)
       (typep right 'scene-mesh-generation)
       (equalp (scene-mesh-generation-request-stamp left)
               (scene-mesh-generation-request-stamp right))
       (realized-light-stamp=
        (realized-light-generation-stamp
         (scene-mesh-generation-light-generation left))
        (realized-light-generation-stamp
         (scene-mesh-generation-light-generation right)))))

(defun decorate-scene-meshes
    (owners scene
     &key surface-context
       (attachment-source-owners nil attachment-source-owners-p)
       request-stamp reusable-light-generation
       (realize-torch-light-p t)
       (generation-scene scene))
  "Realize immutable light and final-surface semantic frames for OWNERS.

OWNERS is an alist of canonical chunk owner to a finished surface mesh.  A NIL
owner denotes the legacy whole-domain oracle.  SURFACE-CONTEXT is a nonpublished
alist of additional realized owners.  Frame resolution sees both sets because a
band or fan supporting an output-owned attachment may be canonically emitted by
a neighboring owner; packed frames and light remain resident only with their
support cell's output owner.

When ATTACHMENT-SOURCE-OWNERS is supplied, it names the logically resident
authored source chunks.  Canonical publication includes virtual +X/+Y owners,
which must never make a torch whose support source is absent look resident.

The second value is an immutable SCENE-MESH-GENERATION.  Geometry is resolved
before torch seeds; the field is solved or exactly reused next; only then are
mesh light sidecars and packed body/flame frames finalized."
  (check-type owners list)
  (check-type surface-context list)
  (let* ((frames
           (unless
               (every (lambda (entry)
                        (not (null (luft:surface-mesh-star-site-words
                                    (cdr entry)))))
                      owners)
             (resolve-unlit-scene-torch-frames
              owners scene surface-context attachment-source-owners
              attachment-source-owners-p)))
         (light-generation
           (if realize-torch-light-p
               (scene-realized-light-generation
                scene frames reusable-light-generation)
               (progn
                 (when frames
                   (error "A reused light field cannot finalize ~D changed torch frames."
                          (length frames)))
                 (or (etypecase reusable-light-generation
                       (null nil)
                       (realized-light-generation reusable-light-generation)
                       (scene-mesh-generation
                        (scene-mesh-generation-light-generation
                         reusable-light-generation)))
                     (error "A non-realizing mesh request needs a reusable light generation.")))))
         (field (realized-light-generation-field light-generation)))
    (let ((descriptors
            (compile-terrain-material-descriptors
             (scene-material-vocabulary scene))))
      (labels ((initialize (surface)
                 (compile-surface-mesh-appearance
                  surface (scene-material-cells scene) descriptors)
                 (setf (luft:surface-mesh-voxel-light surface) field
                       (luft:surface-mesh-attachments surface) nil)
                 (dolist (companion (luft:surface-mesh-companions surface))
                   (initialize companion))))
        (dolist (entry owners) (initialize (cdr entry)))))
    (dolist (frame frames)
      (push
       (pack-realized-torch-frame
        scene (unlit-torch-frame-attachment frame)
        (unlit-torch-frame-surface-frame frame)
        (unlit-torch-frame-wick-point frame) light-generation)
       (luft:surface-mesh-attachments (unlit-torch-frame-target frame))))
    (dolist (entry owners)
      (setf (luft:surface-mesh-attachments (cdr entry))
            (nreverse (luft:surface-mesh-attachments (cdr entry)))))
    ;; Snapshot only after light sidecars and every body/flame frame are final.
    (values
     owners
     (make-scene-mesh-generation-value
      generation-scene request-stamp light-generation :mesh-entries owners))))

(defun decorate-scene-mesh (mesh scene &optional chunk-key)
  "Compatibility wrapper around cohort-aware final-surface decoration."
  (multiple-value-bind (owners generation)
      (decorate-scene-meshes
       (list (cons chunk-key mesh)) scene
       :request-stamp
       (list :whole-domain-diagnostic chunk-key
             (scene-authored-light-provenance scene)
             (scene-voxel-light-propagation-p scene)))
    (let ((root (cdar owners)))
      (values
       root
       (make-scene-mesh-generation-value
        scene (scene-mesh-generation-request-stamp generation)
        (scene-mesh-generation-light-generation generation)
        :mesh-entries (list (cons +unkeyed-scene-mesh-output+ root))
        :unkeyed-mesh-p t)))))

(declaim (ftype function make-scene-regional-meshes))

(defun scene-regional-mesh-tree
    (scene bevel-width &key reusable-light-generation)
  "Flatten the regional star streams into one static mesh."
  (declare (ignore bevel-width))
  (multiple-value-bind (owners census diagnostics generation)
      (make-scene-regional-meshes
       scene 1
       :reusable-light-generation reusable-light-generation)
    (let* ((domain (luft:chain-domain (scene-solid scene)))
           (words
             (apply #'concatenate '(simple-array (unsigned-byte 32) (*))
                    (mapcar (lambda (entry)
                              (luft:surface-mesh-star-site-words (cdr entry)))
                            owners)))
           (appearance
             (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                    (mapcar
                     (lambda (entry)
                       (luft:surface-mesh-appearance-codes (cdr entry)))
                     owners)))
           (root (luft:make-surface-mesh domain words)))
      (setf (luft:surface-mesh-appearance-codes root) appearance)
      (when owners
        (setf (luft:surface-mesh-appearance-descriptor-words root)
              (luft:surface-mesh-appearance-descriptor-words
               (cdar owners))))
      (values
       root census diagnostics
       (make-scene-mesh-generation-value
        scene (scene-mesh-generation-request-stamp generation)
        (scene-mesh-generation-light-generation generation)
        :mesh-entries (list (cons +unkeyed-scene-mesh-output+ root))
        :unkeyed-mesh-p t)))))

(defun make-render-mesh
    (source &key stock-function chamfer-stock-function
                 (bevel-width luft:+mesh-bevel-width+)
                 reusable-light-generation)
  "Classify SOURCE through the production regional mesh compiler.

For a SCENE, the first value remains its surface mesh and the second is the
immutable SCENE-MESH-GENERATION which justifies its finalized light/frame data.
Custom stock callbacks and bare chains are whole-domain diagnostic policies;
use MAKE-WHOLE-DOMAIN-DIAGNOSTIC-MESH explicitly."
  (check-type source scene)
  (let* ((scene source)
         (solid (scene-solid scene))
         (custom-stock-policy-p (or stock-function chamfer-stock-function)))
    (check-type solid luft:chain)
    (when custom-stock-policy-p
      (error "Scene stock callbacks bypass the production regional compiler; use MAKE-WHOLE-DOMAIN-DIAGNOSTIC-MESH explicitly."))
    (when reusable-light-generation
      (check-type reusable-light-generation scene-mesh-generation)
      (unless
          (eq scene (scene-mesh-generation-scene reusable-light-generation))
        (error "A reusable generation belongs to a different authored scene input.")))
    (zone (:luft/rematerialize :value (luft:chain-count solid))
      (multiple-value-bind (mesh census diagnostics generation)
          (scene-regional-mesh-tree
           scene bevel-width
           :reusable-light-generation reusable-light-generation)
        (declare (ignore census diagnostics))
        (values mesh generation)))))

(defparameter *gallery*
  ;; Each entry is one isolated complex, named by the star configuration it
  ;; is there to exhibit.  Cells are offsets from the entry's plot origin.
  '((:one-cell
     ((0 0 0)))
    (:face-pair
     ((0 0 0) (1 0 0)))
    (:edge-pair
     ((0 0 0) (1 1 0)))
    (:corner-pair
     ((0 0 0) (1 1 1)))
    (:l-tromino
     ((0 0 0) (1 0 0) (0 1 0)))
    (:square
     ((0 0 0) (1 0 0) (0 1 0) (1 1 0)))
    (:stair
     ((0 0 0) (1 0 0) (1 0 1)))
    (:concave-vertex
     ((0 0 0) (1 0 0) (0 1 0) (1 1 0)
      (0 0 1) (1 0 1) (0 1 1)))
    (:six-of-eight
     ((0 0 0) (1 0 0) (0 1 0) (1 1 0)
      (0 0 1) (1 0 1)))
    (:full-block
     ((0 0 0) (1 0 0) (0 1 0) (1 1 0)
      (0 0 1) (1 0 1) (0 1 1) (1 1 1)))
    (:tower
     ((0 0 0) (0 0 1) (0 0 2)))
    (:cross
     ((1 1 0) (0 1 0) (2 1 0) (1 0 0) (1 2 0) (1 1 1))))
  "Small unconnected complexes, one per interesting occupancy star.")

(defparameter *gallery-columns* 4)
(defparameter *gallery-pitch* 6)

(defun gallery-plot-origin (index)
  "Return the lattice origin of gallery entry INDEX, laid out on a grid."
  (multiple-value-bind (row column) (floor index *gallery-columns*)
    (values (+ 2 (* column *gallery-pitch*))
            (+ 2 (* row *gallery-pitch*))
            1)))

(defun make-gallery-solid (&key (entries *gallery*))
  "Build one chain holding every gallery complex on its own plot.

Nothing here touches anything else, so each patch shown is entirely the
consequence of its own occupancy star and can be read on its own."
  (let* ((domain (luft:make-world-domain :x-bits 6 :y-bits 6))
         (builder (luft:make-chain-builder domain :initial-capacity 128))
         (seen (make-hash-table :test #'eql)))
    (loop for entry in entries
          for index from 0
          do (multiple-value-bind (ox oy oz) (gallery-plot-origin index)
               (loop for (dx dy dz) in (second entry)
                     for site = (luft:make-site domain (+ ox dx) (+ oy dy)
                                                (+ oz dz)
                                                luft:+cell-extent+ 1)
                     unless (gethash site seen)
                       do (setf (gethash site seen) t)
                          (luft:chain-builder-add-site builder site))))
    (luft:finish-chain-builder builder)))

(defun gallery-plot-report (&key (entries *gallery*))
  "Print where each gallery complex sits, so a capture can be aimed at one."
  (loop for entry in entries
        for index from 0
        do (multiple-value-bind (x y z) (gallery-plot-origin index)
             (format t "~&~2D ~24A origin ~D,~D,~D~%"
                     index (first entry) x y z)))
  (values))

(defun make-demo-solid ()
  "Make a compact stair-and-bridge solid with convex and concave stars."
  (let* ((domain (luft:make-world-domain :x-bits 5 :y-bits 5))
         (builder (luft:make-chain-builder domain :initial-capacity 96))
         (seen (make-hash-table :test #'eql)))
    (labels ((cell (x y z)
               (let ((site
                       (luft:make-site domain x y z luft:+cell-extent+ 1)))
                 (unless (gethash site seen)
                   (setf (gethash site seen) t)
                   (luft:chain-builder-add-site builder site))))
             (box (x0 x1 y0 y1 z0 z1)
               (loop for z from z0 to z1 do
                 (loop for y from y0 to y1 do
                   (loop for x from x0 to x1 do (cell x y z))))))
      (box 4 11 4 11 1 1)
      (box 5 10 5 10 2 2)
      (box 6 6 6 6 3 5)
      (box 9 9 6 6 3 5)
      (box 6 6 9 9 3 5)
      (box 9 9 9 9 3 5)
      (box 6 9 6 6 5 5)
      (box 6 9 9 9 5 5)
      (box 7 8 7 8 3 3))
    (luft:finish-chain-builder builder)))

(defconstant +render-template-vertex-count+ 6)

(defconstant +star-meshlet-triangle-capacity+ 25)
(defconstant +star-meshlet-vertex-capacity+
  (* 3 +star-meshlet-triangle-capacity+))
(defconstant +star-meshlet-record-count+
  (1+ +star-meshlet-vertex-capacity+))
(defconstant +star-meshlet-coordinate-bias+ 8)

(defun star-meshlet-template-words ()
  "Return the fixed 256-record triangle-soup atlas consumed by mesh shaders.

Each star owns one fixed-size block (#0UAD9N).  Its first uvec4 contains the triangle
count; the remaining records are the three vertices of each triangle in
outward order.  Coordinates are biased only to keep this first ABI unsigned."
  (let ((words
          (make-array (* 256 +star-meshlet-record-count+ 4)
                      :element-type '(unsigned-byte 32)
                      :initial-element 0)))
    (dotimes (star 256 words)
      (let* ((triangles (luft:star-atlas-owned-triangles star))
             (appearance (luft:star-atlas-owned-appearance-masks star))
             (triangle-count (length triangles))
             (block (* star +star-meshlet-record-count+ 4)))
        (when (> triangle-count +star-meshlet-triangle-capacity+)
          (error "Star #x~2,'0X owns ~D triangles; the meshlet ABI admits ~D."
                 star triangle-count +star-meshlet-triangle-capacity+))
        (setf (aref words block) triangle-count)
        (loop for triangle in triangles
              for (material-mask light-mask) in appearance
              for triangle-index from 0
              do (loop for point in triangle
                       for corner from 0
                       for record = (+ 1 (* 3 triangle-index) corner)
                       for offset = (+ block (* 4 record))
                       do (destructuring-bind (x y z) point
                            (setf (aref words offset)
                                  (+ x +star-meshlet-coordinate-bias+)
                                  (aref words (+ offset 1))
                                  (+ y +star-meshlet-coordinate-bias+)
                                  (aref words (+ offset 2))
                                  (+ z +star-meshlet-coordinate-bias+)
                                  (aref words (+ offset 3))
                                  (if (zerop corner)
                                      (logior material-mask
                                              (ash light-mask 8))
                                      0)))))))))

(defun pack-terrain-appearance-codes (codes)
  "Pack eight u8 sample codes per star into the GPU's parallel uvec2 lane."
  (unless (zerop (mod (length codes) 8))
    (error "Terrain appearance has ~D bytes, not eight per active star."
           (length codes)))
  (let ((words (make-array (/ (length codes) 4)
                           :element-type '(unsigned-byte 32))))
    (loop for offset from 0 below (length codes) by 4
          for word from 0
          do (setf (aref words word)
                   (logior (aref codes offset)
                           (ash (aref codes (+ offset 1)) 8)
                           (ash (aref codes (+ offset 2)) 16)
                           (ash (aref codes (+ offset 3)) 24))))
    words))

(defstruct (render-population
             (:constructor %make-render-population
                 (instance-words appearance-words descriptor-words
                  mesh-workgroup-count))
             (:copier nil))
  "Geometry sites plus a one-for-one, independently replaceable appearance."
  (instance-words #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  (appearance-words #() :type (simple-array (unsigned-byte 32) (*))
                    :read-only t)
  (descriptor-words #() :type (simple-array (unsigned-byte 32) (*))
                    :read-only t)
  (mesh-workgroup-count 0 :type (integer 0 *) :read-only t))

(defstruct (resident-population
             (:constructor %make-resident-population
                 (population instance-buffer template-buffer appearance-buffer
                  descriptor-buffer bind-group shadow-bind-group))
             (:copier nil))
  "One chunk's CPU population and independently retained GPU realization."
  (population nil :type render-population :read-only t)
  (instance-buffer nil :read-only t)
  (template-buffer nil :read-only t)
  (appearance-buffer nil :read-only t)
  (descriptor-buffer nil :read-only t)
  (bind-group nil :read-only t)
  (shadow-bind-group nil :read-only t))

(defstruct (prepared-render-mesh
             (:constructor %make-prepared-render-mesh (mesh population))
             (:copier nil))
  "Worker-transferable CPU realization of one semantic surface mesh."
  (mesh nil :type luft:surface-mesh :read-only t)
  (population nil :type render-population :read-only t))

(zdefun (prepare-render-mesh :zone :luft/prepare-population) (mesh)
  "Canonicalize one MESH before it crosses to the renderer owner."
  (check-type mesh luft:surface-mesh)
  (%make-prepared-render-mesh mesh (make-render-population (list mesh))))

(defun %make-star-render-population (meshes)
  "Flatten geometry and its parallel active-star appearance independently."
  (let* ((site-words
           (apply #'concatenate '(simple-array (unsigned-byte 32) (*))
                  (mapcar #'luft:surface-mesh-star-site-words meshes)))
         (appearance-codes
           (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                  (mapcar #'luft:surface-mesh-appearance-codes meshes)))
         (descriptor-words
           (or (loop for mesh in meshes
                     for words =
                       (luft:surface-mesh-appearance-descriptor-words mesh)
                     when (plusp (length words)) return words)
               (compile-terrain-material-descriptors
                (make-scene-material-vocabulary)))))
    (unless (= (* 2 (length site-words)) (length appearance-codes))
      (error "~D site words do not have a one-for-one eight-byte appearance (~D bytes)."
             (length site-words) (length appearance-codes)))
    (dolist (mesh meshes)
      (let ((words (luft:surface-mesh-appearance-descriptor-words mesh)))
        (unless (or (zerop (length words)) (equalp words descriptor-words))
          (error "One terrain population contains incompatible material palettes."))))
    (%make-render-population
     site-words (pack-terrain-appearance-codes appearance-codes)
     descriptor-words (/ (length site-words) 4))))

(defun make-render-population (meshes)
  "Prepare the sole terrain ABI: active sites indexing the fixed star atlas."
  (%make-star-render-population meshes))

(defstruct (renderer-slot-provenance
             (:constructor %make-renderer-slot-provenance
                 (key scene request-stamp result-stamp light-generation tree))
             (:copier nil))
  "Narrow immutable evidence for one exact installed semantic mesh tree."
  (key nil :read-only t)
  (scene nil :type scene :read-only t)
  (request-stamp nil :read-only t)
  (result-stamp nil :read-only t)
  (light-generation nil :type realized-light-generation :read-only t)
  (tree nil :type surface-mesh-tree-manifest :read-only t))

(defstruct (mesh-slot (:constructor %make-mesh-slot) (:copier nil))
  "One mesh's semantic residency and optional construction-overlay resources."
  (mesh nil)
  (provenance nil :type (or null renderer-slot-provenance))
  (resident nil)
  (lattice-point-buffer nil)
  (lattice-point-count 0)
  (lattice-point-group nil))

(defstruct (renderer-publication
             (:constructor %make-renderer-publication
                 (mesh-slots slot-order torch-frame-data flame-instance-count
                  flame-instance-buffer torch-body-bind-group
                  torch-body-shadow-bind-group scene-generation))
             (:copier nil))
  "One atomically installed renderer residency/resource generation.

The table itself is never mutated after publication.  Mesh slots retained from
the preceding generation may be shared, but the sorted order and every global
attachment resource describe exactly this table."
  (mesh-slots (make-hash-table :test #'eql) :read-only t)
  (slot-order nil :type list :read-only t)
  (torch-frame-data (make-array 0 :element-type 'single-float) :read-only t)
  (flame-instance-count 0 :type (integer 0 *) :read-only t)
  (flame-instance-buffer nil :read-only t)
  (torch-body-bind-group nil :read-only t)
  (torch-body-shadow-bind-group nil :read-only t)
  (scene-generation nil :type (or null scene-mesh-generation) :read-only t))

(defun %make-empty-renderer-publication
    (&key flame-instance-buffer torch-body-bind-group
          torch-body-shadow-bind-group)
  (%make-renderer-publication
   (make-hash-table :test #'eql) nil
   (make-array 0 :element-type 'single-float) 0
   flame-instance-buffer torch-body-bind-group
   torch-body-shadow-bind-group nil))

(defstruct (renderer-target-resources
             (:constructor %make-renderer-target-resources
                 (&key extent render-extent temporal-scaler
                  depth-msaa-texture depth-msaa-view depth-texture depth-view
                  scene-msaa-texture scene-msaa-view scene-texture scene-view
                  motion-msaa-texture motion-msaa-view
                  motion-texture motion-view
                  resolved-texture resolved-view history-texture history-view
                  temporal-bind-group composite-texture composite-view
                  composite-source-bind-group present-bind-group
                  exposure-probe-bind-group))
             (:copier nil))
  "One immutable output-size-dependent texture/view/resource cohort."
  (extent nil :type list :read-only t)
  (render-extent nil :type list :read-only t)
  (temporal-scaler nil :read-only t)
  (depth-msaa-texture nil :read-only t)
  (depth-msaa-view nil :read-only t)
  (depth-texture nil :read-only t)
  (depth-view nil :read-only t)
  (scene-msaa-texture nil :read-only t)
  (scene-msaa-view nil :read-only t)
  (scene-texture nil :read-only t)
  (scene-view nil :read-only t)
  (motion-msaa-texture nil :read-only t)
  (motion-msaa-view nil :read-only t)
  (motion-texture nil :read-only t)
  (motion-view nil :read-only t)
  (resolved-texture nil :read-only t)
  (resolved-view nil :read-only t)
  (history-texture nil :read-only t)
  (history-view nil :read-only t)
  (temporal-bind-group nil :read-only t)
  (composite-texture nil :read-only t)
  (composite-view nil :read-only t)
  (composite-source-bind-group nil :read-only t)
  (present-bind-group nil :read-only t)
  (exposure-probe-bind-group nil :read-only t))

(defstruct (renderer-flame-target-join
             (:constructor %make-renderer-flame-target-join (bind-group))
             (:copier nil))
  "One owned population-times-depth binding, independent of target textures."
  (bind-group nil :read-only t))

(defstruct (renderer-target-generation
             (:constructor %make-renderer-target-generation (resources flame-join))
             (:copier nil))
  "The immutable product of one target cohort and one flame/depth join.

A resize creates and owns a fresh pair.  A residency publication stages only a
new join while borrowing the installed resource cohort; at the noninterleavable
owner-thread commit, ownership of that exact cohort remains with the renderer
and only the old join retires.  Thus no target texture/view is ever cloned,
ambiguously co-owned, or retired by a population rollback."
  (resources nil :type renderer-target-resources :read-only t)
  (flame-join nil :type renderer-flame-target-join :read-only t))

(defun %make-empty-renderer-target-generation ()
  (%make-renderer-target-generation
   (%make-renderer-target-resources
    :extent nil :render-extent nil)
   (%make-renderer-flame-target-join nil)))

(defmacro define-renderer-target-resource-reader (name)
  `(defun ,(intern (format nil "RENDERER-TARGET-GENERATION-~A" name))
       (generation)
     (,(intern (format nil "RENDERER-TARGET-RESOURCES-~A" name))
      (renderer-target-generation-resources generation))))

(define-renderer-target-resource-reader extent)
(define-renderer-target-resource-reader render-extent)
(define-renderer-target-resource-reader temporal-scaler)
(define-renderer-target-resource-reader depth-msaa-texture)
(define-renderer-target-resource-reader depth-msaa-view)
(define-renderer-target-resource-reader depth-texture)
(define-renderer-target-resource-reader depth-view)
(define-renderer-target-resource-reader scene-msaa-texture)
(define-renderer-target-resource-reader scene-msaa-view)
(define-renderer-target-resource-reader scene-texture)
(define-renderer-target-resource-reader scene-view)
(define-renderer-target-resource-reader motion-msaa-texture)
(define-renderer-target-resource-reader motion-msaa-view)
(define-renderer-target-resource-reader motion-texture)
(define-renderer-target-resource-reader motion-view)
(define-renderer-target-resource-reader resolved-texture)
(define-renderer-target-resource-reader resolved-view)
(define-renderer-target-resource-reader history-texture)
(define-renderer-target-resource-reader history-view)
(define-renderer-target-resource-reader temporal-bind-group)
(define-renderer-target-resource-reader composite-texture)
(define-renderer-target-resource-reader composite-view)
(define-renderer-target-resource-reader composite-source-bind-group)
(define-renderer-target-resource-reader present-bind-group)
(define-renderer-target-resource-reader exposure-probe-bind-group)

(defun renderer-target-generation-flame-bind-group (generation)
  (renderer-flame-target-join-bind-group
   (renderer-target-generation-flame-join generation)))

(defun %copy-torch-frame-data (source)
  "Validate and copy packed three-Vec4 frames across an ownership boundary."
  (check-type source vector)
  (unless (zerop (mod (length source) +torch-flame-instance-scalar-count+))
    (error "A ~D-scalar flame stream is not an integral frame population."
           (length source)))
  (let ((copy (map '(simple-array single-float (*))
                   (lambda (scalar)
                     (check-type scalar single-float)
                     scalar)
                   source)))
    (loop for offset from 0 below (length copy)
            by +torch-flame-instance-scalar-count+
          do (validate-torch-flame-frame copy offset))
    copy))

(defun surface-mesh-torch-frame-data (mesh)
  "Flatten MESH's sparse attachment frames, including companion owners."
  (check-type mesh luft:surface-mesh)
  (let ((frames nil))
    (labels ((visit (surface)
               (dolist (frame (luft:surface-mesh-attachments surface))
                 (push frame frames))
               (dolist (companion (luft:surface-mesh-companions surface))
                 (visit companion))))
      (visit mesh))
    (let* ((frames (nreverse frames))
           (data
             (make-array (* +torch-flame-instance-scalar-count+ (length frames))
                         :element-type 'single-float)))
      (loop for frame in frames
            for offset from 0 by +torch-flame-instance-scalar-count+
            do (validate-torch-flame-frame frame)
               (replace data frame :start1 offset))
      data)))

(defun mesh-slots-torch-frame-data (entries)
  "Flatten attachment frames from sorted key-to-MESH-SLOT ENTRIES."
  (let* ((runs
           (mapcar (lambda (entry)
                     (surface-mesh-torch-frame-data
                      (mesh-slot-mesh (cdr entry))))
                   entries))
         (length (reduce #'+ runs :key #'length :initial-value 0))
         (data (make-array length :element-type 'single-float)))
    (loop with offset = 0
          for run in runs
          do (replace data run :start1 offset)
             (incf offset (length run)))
    data))

(defclass renderer ()
  ((device :initarg :device :reader renderer-device)
   ;; One owner-thread pointer swap publishes the complete keyed residency and
   ;; all resources derived from it.  Readers never observe a table paired
   ;; with another generation's order or attachment buffers.
   (publication :initarg :publication :accessor renderer-publication)
   (camera-buffer :initarg :camera-buffer :accessor renderer-camera-buffer)
   (star-template-buffer :initarg :star-template-buffer :initform nil
                         :accessor renderer-star-template-buffer)
   (frame-resources
    :initform (make-canvas-frame-resource-cache)
    :reader renderer-frame-resources)
   (layout :initarg :layout :accessor renderer-layout)
   (vertex-module :initarg :vertex-module :accessor renderer-vertex-module)
   (fragment-module :initarg :fragment-module :accessor renderer-fragment-module)
   (torch-body-fragment-module
    :initarg :torch-body-fragment-module
    :initform nil
    :accessor renderer-torch-body-fragment-module)
   (pipeline :initarg :pipeline :accessor renderer-pipeline)
   (flame-effect-buffer :initarg :flame-effect-buffer
                        :accessor renderer-flame-effect-buffer)
   (flame-layout :initarg :flame-layout :accessor renderer-flame-layout)
   (flame-vertex-module :initarg :flame-vertex-module
                        :accessor renderer-flame-vertex-module)
   (flame-fragment-module :initarg :flame-fragment-module
                          :accessor renderer-flame-fragment-module)
   (flame-pipeline :initarg :flame-pipeline
                   :accessor renderer-flame-pipeline)
   (flame-depth-sampler :initarg :flame-depth-sampler
                        :accessor renderer-flame-depth-sampler)
   (composite-layout :initarg :composite-layout
                     :accessor renderer-composite-layout)
   (composite-fragment-module :initarg :composite-fragment-module
                              :accessor renderer-composite-fragment-module)
   (composite-pipeline :initarg :composite-pipeline
                       :accessor renderer-composite-pipeline)
   ;; The opaque socket/shaft and animated flame consume the exact same
   ;; realized three-row frame population.  Only the immutable canonical body
   ;; vertices and their render pipelines differ.
   (torch-body-vertex-buffer :initarg :torch-body-vertex-buffer
                             :accessor renderer-torch-body-vertex-buffer)
   (torch-body-layout :initarg :torch-body-layout
                      :accessor renderer-torch-body-layout)
   (torch-body-vertex-module :initarg :torch-body-vertex-module
                             :accessor renderer-torch-body-vertex-module)
   (torch-body-shadow-vertex-module
    :initarg :torch-body-shadow-vertex-module
    :accessor renderer-torch-body-shadow-vertex-module)
   (torch-body-pipeline :initarg :torch-body-pipeline
                        :accessor renderer-torch-body-pipeline)
   (torch-body-shadow-pipeline :initarg :torch-body-shadow-pipeline
                               :accessor renderer-torch-body-shadow-pipeline)
   (shadow-texture :initarg :shadow-texture :accessor renderer-shadow-texture)
   (shadow-view :initarg :shadow-view :accessor renderer-shadow-view)
   (shadow-sampler :initarg :shadow-sampler :accessor renderer-shadow-sampler)
   (shadow-layout :initarg :shadow-layout :accessor renderer-shadow-layout)
   (shadow-vertex-module :initarg :shadow-vertex-module
                         :accessor renderer-shadow-vertex-module)
   (shadow-pipeline :initarg :shadow-pipeline
                    :accessor renderer-shadow-pipeline)
   (player-sdf-layout :initarg :player-sdf-layout
                      :accessor renderer-player-sdf-layout)
   (player-sdf-bind-group :initarg :player-sdf-bind-group
                          :accessor renderer-player-sdf-bind-group)
   (player-sdf-vertex-module :initarg :player-sdf-vertex-module
                             :accessor renderer-player-sdf-vertex-module)
   (player-sdf-fragment-module :initarg :player-sdf-fragment-module
                               :accessor renderer-player-sdf-fragment-module)
   (player-sdf-pipeline :initarg :player-sdf-pipeline
                        :accessor renderer-player-sdf-pipeline)
   (lattice-point-layout :initarg :lattice-point-layout
                         :accessor renderer-lattice-point-layout)
   (lattice-point-vertex-module :initarg :lattice-point-vertex-module
                                :accessor renderer-lattice-point-vertex-module)
   (lattice-point-fragment-module :initarg :lattice-point-fragment-module
                                  :accessor renderer-lattice-point-fragment-module)
   (lattice-point-pipeline :initarg :lattice-point-pipeline
                           :accessor renderer-lattice-point-pipeline)
   (sky-layout :initform nil :accessor renderer-sky-layout)
   (sky-bind-group :initform nil :accessor renderer-sky-bind-group)
   (sky-fragment-module :initform nil :accessor renderer-sky-fragment-module)
   (sky-pipeline :initform nil :accessor renderer-sky-pipeline)
   (color-format :initarg :color-format :reader renderer-color-format)
   (temporal-p :initarg :temporal-p :reader renderer-temporal-p)
   (temporal-resolve-kind :initarg :temporal-resolve-kind :initform nil
                          :reader renderer-temporal-resolve-kind)
   (temporal-layout :initform nil :accessor renderer-temporal-layout)
   (temporal-fragment-module :initform nil
                             :accessor renderer-temporal-fragment-module)
   (temporal-pipeline :initform nil :accessor renderer-temporal-pipeline)
   ;; The complete resize-owned identity is published by this one pointer.
   ;; Resource creation, target-dependent binding, and failure cleanup happen
   ;; before it changes; no frame can observe a partially replaced target set.
   (target-generation
    :initarg :target-generation
    :initform (%make-empty-renderer-target-generation)
    :accessor renderer-target-generation)
   (present-layout :initform nil :accessor renderer-present-layout)
   (present-vertex-module :initform nil
                          :accessor renderer-present-vertex-module)
   (present-fragment-module :initform nil
                            :accessor renderer-present-fragment-module)
   (present-pipeline :initform nil :accessor renderer-present-pipeline)
   (exposure-probe-layout :initform nil
                          :accessor renderer-exposure-probe-layout)
   (exposure-probe-texture :initform nil
                           :accessor renderer-exposure-probe-texture)
   (exposure-probe-view :initform nil
                        :accessor renderer-exposure-probe-view)
   (exposure-probe-fragment-module
    :initform nil :accessor renderer-exposure-probe-fragment-module)
   (exposure-probe-pipeline :initform nil
                            :accessor renderer-exposure-probe-pipeline)
   (exposure-probe-buffers :initform #()
                           :accessor renderer-exposure-probe-buffers)
   (exposure-probe-submitted :initform (make-array 0 :element-type 'bit)
                             :accessor renderer-exposure-probe-submitted)
   (exposure-probe-frames :initform #()
                          :accessor renderer-exposure-probe-frames)
   (exposure :initform 1.0f0 :accessor renderer-exposure)
   (sampler :initform nil :accessor renderer-sampler)
   (frame-index :initform 0 :accessor renderer-frame-index)
   (previous-view :initform nil :accessor renderer-previous-view)
   (history-valid-p :initform nil :accessor renderer-history-valid-p)
   (history-used-p :initform nil :accessor renderer-history-used-p)))

(defstruct (renderer-frame-state
             (:constructor %make-renderer-frame-state
                 (&key camera-buffer flame-effect-buffer)))
  "Mutable uploads and dependent bindings local to one presentation slot."
  camera-buffer
  flame-effect-buffer
  (bind-groups (make-hash-table :test #'equal)))

(defun make-renderer-frame-state (renderer)
  "Allocate one complete mutable upload cohort for RENDERER."
  (let ((camera nil)
        (effect nil)
        (completed-p nil))
    (unwind-protect
         (progn
           (setf camera
                 (create
                  (renderer-device renderer)
                  (make-buffer-descriptor
                   :label "luft presentation-slot camera state"
                   :size (shaders::scene-uniform-byte-size)
                   :usage '(:uniform :copy-dst)))
                 effect
                 (create
                  (renderer-device renderer)
                  (make-buffer-descriptor
                   :label "luft presentation-slot flame effect"
                   :size (torch-flame-effect-byte-size)
                   :usage '(:uniform :copy-dst))))
           (setf completed-p t)
           (%make-renderer-frame-state
            :camera-buffer camera :flame-effect-buffer effect))
      (unless completed-p
        (when effect (ignore-errors (destroy effect)))
        (when camera (ignore-errors (destroy camera)))))))

(defun destroy-renderer-frame-state (state)
  "Release one presentation-slot upload cohort and its derived bindings."
  (with-release-report
    (maphash
     (lambda (key group)
       (declare (ignore key))
       (releasing :frame-bind-group (destroy group)))
     (renderer-frame-state-bind-groups state))
    (clrhash (renderer-frame-state-bind-groups state))
    (releasing :frame-flame-effect-buffer
      (destroy (renderer-frame-state-flame-effect-buffer state)))
    (releasing :frame-camera-buffer
      (destroy (renderer-frame-state-camera-buffer state))))
  (values))

(defun clear-renderer-frame-bind-groups (renderer)
  "Drop bindings derived from a superseded target or scene generation."
  (map-canvas-frame-resources
   (lambda (state key)
     (declare (ignore key))
     (let ((groups (renderer-frame-state-bind-groups state)))
       (with-release-report
         (dolist (binding-key
                   (loop for key being the hash-keys of groups collect key))
           (releasing (list :frame-bind-group binding-key)
             (destroy (gethash binding-key groups))
             (remhash binding-key groups))))))
   (renderer-frame-resources renderer))
  renderer)

(defun renderer-frame-bind-group (renderer frame key label layout entries)
  "Return FRAME's binding KEY, creating it transactionally from ENTRIES."
  (let ((groups (renderer-frame-state-bind-groups frame)))
    (or (gethash key groups)
        (let ((group
                (create
                 (renderer-device renderer)
                 (make-bind-group-descriptor
                  :label label :layout layout :entries entries))))
          (setf (gethash key groups) group)))))

(defun renderer-frame-state-for (renderer context surface-texture)
  "Acquire RENDERER's safely reusable mutable state for SURFACE-TEXTURE."
  (canvas-frame-resource
   (renderer-frame-resources renderer) context surface-texture
   (lambda (key surface)
     (declare (ignore key surface))
     (make-renderer-frame-state renderer))))

(defun renderer-frame-resident-bind-group (renderer frame resident shadow-p)
  "Bind one immutable resident population to FRAME's camera upload."
  (let ((camera (renderer-frame-state-camera-buffer frame)))
    (if shadow-p
        (renderer-frame-bind-group
         renderer frame (list :resident-shadow resident)
         "luft frame-local resident shadow population"
         (renderer-shadow-layout renderer)
         `((:binding 0 :resource ,(resident-population-instance-buffer resident))
           (:binding 1 :resource ,(resident-population-template-buffer resident))
           (:binding 2 :resource ,camera)))
        (renderer-frame-bind-group
         renderer frame (list :resident-scene resident
                              (renderer-shadow-view renderer))
         "luft frame-local resident site population"
         (renderer-layout renderer)
         `((:binding 0 :resource ,(resident-population-instance-buffer resident))
           (:binding 1 :resource ,(resident-population-template-buffer resident))
           (:binding 2 :resource ,camera)
           (:binding 3 :resource ,(resident-population-appearance-buffer resident))
           (:binding 4 :resource ,(renderer-shadow-view renderer))
           (:binding 5 :resource ,(renderer-shadow-sampler renderer))
           (:binding 6 :resource ,(resident-population-descriptor-buffer resident)))))))

(defun renderer-frame-torch-body-bind-group (renderer frame shadow-p)
  (let ((camera (renderer-frame-state-camera-buffer frame))
        (instances (renderer-flame-instance-buffer renderer)))
    (if shadow-p
        (renderer-frame-bind-group
         renderer frame (list :torch-shadow instances)
         "luft frame-local torch-body shadows"
         (renderer-shadow-layout renderer)
         `((:binding 0 :resource ,instances)
           (:binding 1 :resource ,(renderer-torch-body-vertex-buffer renderer))
           (:binding 2 :resource ,camera)))
        (renderer-frame-bind-group
         renderer frame (list :torch-scene instances
                              (renderer-shadow-view renderer))
         "luft frame-local torch bodies"
         (renderer-torch-body-layout renderer)
         `((:binding 0 :resource ,instances)
           (:binding 1 :resource ,(renderer-torch-body-vertex-buffer renderer))
           (:binding 2 :resource ,camera)
           (:binding 4 :resource ,(renderer-shadow-view renderer))
           (:binding 5 :resource ,(renderer-shadow-sampler renderer)))))))

(defun renderer-frame-sky-bind-group (renderer frame)
  (renderer-frame-bind-group
   renderer frame '(:sky) "luft frame-local HDR sky"
   (renderer-sky-layout renderer)
   `((:binding 0 :resource ,(renderer-frame-state-camera-buffer frame)))))

(defun renderer-frame-player-bind-group (renderer frame)
  (renderer-frame-bind-group
   renderer frame (list :player (renderer-shadow-view renderer))
   "luft frame-local walking player SDF"
   (renderer-player-sdf-layout renderer)
   `((:binding 0 :resource ,(renderer-frame-state-camera-buffer frame))
     (:binding 1 :resource ,(renderer-shadow-view renderer))
     (:binding 2 :resource ,(renderer-shadow-sampler renderer)))))

(defun renderer-frame-lattice-bind-group (renderer frame slot)
  (renderer-frame-bind-group
   renderer frame (list :lattice slot (mesh-slot-lattice-point-buffer slot))
   "luft frame-local eighth-cell lattice points"
   (renderer-lattice-point-layout renderer)
   `((:binding 0 :resource ,(mesh-slot-lattice-point-buffer slot))
     (:binding 1 :resource ,(renderer-frame-state-camera-buffer frame)))))

(defun renderer-frame-temporal-bind-group (renderer frame)
  (renderer-frame-bind-group
   renderer frame
   (list :temporal (renderer-scene-view renderer)
         (renderer-motion-view renderer) (renderer-history-view renderer))
   "luft frame-local temporal resolve inputs"
   (renderer-temporal-layout renderer)
   `((:binding 0 :resource ,(renderer-scene-view renderer))
     (:binding 1 :resource ,(renderer-motion-view renderer))
     (:binding 2 :resource ,(renderer-history-view renderer))
     (:binding 3 :resource ,(renderer-sampler renderer))
     (:binding 4 :resource ,(renderer-frame-state-camera-buffer frame)))))

(defun renderer-frame-flame-bind-group (renderer frame)
  (renderer-frame-bind-group
   renderer frame
   (list :flame (renderer-flame-instance-buffer renderer)
         (renderer-depth-view renderer))
   "luft frame-local post-temporal torch flames"
   (renderer-flame-layout renderer)
   `((:binding 0 :resource ,(renderer-flame-instance-buffer renderer))
     (:binding 1 :resource ,(renderer-frame-state-camera-buffer frame))
     (:binding 2 :resource ,(renderer-frame-state-flame-effect-buffer frame))
     (:binding 3 :resource ,(renderer-depth-view renderer))
     (:binding 4 :resource ,(renderer-flame-depth-sampler renderer)))))

(defun renderer-frame-present-bind-group (renderer frame)
  (renderer-frame-bind-group
   renderer frame
   (list :present (renderer-composite-view renderer)
         (renderer-depth-view renderer))
   "luft frame-local HDR presentation"
   (renderer-present-layout renderer)
   `((:binding 0 :resource ,(renderer-composite-view renderer))
     (:binding 1 :resource ,(renderer-sampler renderer))
     (:binding 2 :resource ,(renderer-depth-view renderer))
     (:binding 3 :resource ,(renderer-frame-state-camera-buffer frame)))))

(defun renderer-extent (renderer)
  (renderer-target-generation-extent
   (renderer-target-generation renderer)))

(defun renderer-render-extent (renderer)
  (renderer-target-generation-render-extent
   (renderer-target-generation renderer)))

(defun renderer-temporal-scaler (renderer)
  (renderer-target-generation-temporal-scaler
   (renderer-target-generation renderer)))

(defun renderer-depth-texture (renderer)
  (renderer-target-generation-depth-texture
   (renderer-target-generation renderer)))

(defun renderer-depth-msaa-view (renderer)
  (renderer-target-generation-depth-msaa-view
   (renderer-target-generation renderer)))

(defun renderer-depth-view (renderer)
  (renderer-target-generation-depth-view
   (renderer-target-generation renderer)))

(defun renderer-scene-texture (renderer)
  (renderer-target-generation-scene-texture
   (renderer-target-generation renderer)))

(defun renderer-scene-msaa-view (renderer)
  (renderer-target-generation-scene-msaa-view
   (renderer-target-generation renderer)))

(defun renderer-scene-view (renderer)
  (renderer-target-generation-scene-view
   (renderer-target-generation renderer)))

(defun renderer-motion-texture (renderer)
  (renderer-target-generation-motion-texture
   (renderer-target-generation renderer)))

(defun renderer-motion-msaa-view (renderer)
  (renderer-target-generation-motion-msaa-view
   (renderer-target-generation renderer)))

(defun renderer-motion-view (renderer)
  (renderer-target-generation-motion-view
   (renderer-target-generation renderer)))

(defun renderer-resolved-texture (renderer)
  (renderer-target-generation-resolved-texture
   (renderer-target-generation renderer)))

(defun renderer-resolved-view (renderer)
  (renderer-target-generation-resolved-view
   (renderer-target-generation renderer)))

(defun renderer-history-texture (renderer)
  (renderer-target-generation-history-texture
   (renderer-target-generation renderer)))

(defun renderer-history-view (renderer)
  (renderer-target-generation-history-view
   (renderer-target-generation renderer)))

(defun renderer-temporal-bind-group (renderer)
  (renderer-target-generation-temporal-bind-group
   (renderer-target-generation renderer)))

(defun renderer-composite-texture (renderer)
  (renderer-target-generation-composite-texture
   (renderer-target-generation renderer)))

(defun renderer-composite-view (renderer)
  (renderer-target-generation-composite-view
   (renderer-target-generation renderer)))

(defun renderer-composite-source-bind-group (renderer)
  (renderer-target-generation-composite-source-bind-group
   (renderer-target-generation renderer)))

(defun renderer-present-bind-group (renderer)
  (renderer-target-generation-present-bind-group
   (renderer-target-generation renderer)))

(defun renderer-exposure-probe-bind-group (renderer)
  (renderer-target-generation-exposure-probe-bind-group
   (renderer-target-generation renderer)))

(defun renderer-mesh-slots (renderer)
  (renderer-publication-mesh-slots (renderer-publication renderer)))

(defun renderer-slot-order (renderer)
  (renderer-publication-slot-order (renderer-publication renderer)))

(defun renderer-torch-frame-data (renderer)
  (renderer-publication-torch-frame-data (renderer-publication renderer)))

(defun renderer-flame-instance-count (renderer)
  (renderer-publication-flame-instance-count
   (renderer-publication renderer)))

(defun renderer-flame-instance-buffer (renderer)
  (renderer-publication-flame-instance-buffer
   (renderer-publication renderer)))

(defun renderer-flame-bind-group (renderer)
  (renderer-target-generation-flame-bind-group
   (renderer-target-generation renderer)))

(defun renderer-torch-body-bind-group (renderer)
  (renderer-publication-torch-body-bind-group
   (renderer-publication renderer)))

(defun renderer-torch-body-shadow-bind-group (renderer)
  (renderer-publication-torch-body-shadow-bind-group
   (renderer-publication renderer)))

(defun %make-renderer-flame-resources (renderer source)
  "Build, but do not publish, one validated flame-frame GPU population."
  (check-type renderer renderer)
  (let* ((data (%copy-torch-frame-data source))
         (count (/ (length data) +torch-flame-instance-scalar-count+))
         (device (renderer-device renderer))
         (buffer nil)
         (body-bind-group nil)
         (body-shadow-bind-group nil)
         (completed-p nil))
    (unwind-protect
         (progn
           (setf buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft global torch flame instances"
                          :size (max 16 (* 4 (length data)))
                          :usage '(:storage :copy-dst))))
           (when (plusp (length data))
             (write-buffer buffer data))
           (setf body-bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft realized torch bodies"
                          :layout (renderer-torch-body-layout renderer)
                          :entries
                          `((:binding 0 :resource ,buffer)
                            (:binding 1
                             :resource
                             ,(renderer-torch-body-vertex-buffer renderer))
                            (:binding 2
                             :resource ,(renderer-camera-buffer renderer))
                            (:binding 4
                             :resource ,(renderer-shadow-view renderer))
                            (:binding 5
                             :resource ,(renderer-shadow-sampler renderer))))))
           (setf body-shadow-bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft realized torch-body shadows"
                          :layout (renderer-shadow-layout renderer)
                          :entries
                          `((:binding 0 :resource ,buffer)
                            (:binding 1
                             :resource
                             ,(renderer-torch-body-vertex-buffer renderer))
                            (:binding 2
                             :resource ,(renderer-camera-buffer renderer))))))
           (setf completed-p t)
           (values data count buffer body-bind-group body-shadow-bind-group))
      (unless completed-p
        (when body-shadow-bind-group
          (ignore-errors (destroy body-shadow-bind-group)))
        (when body-bind-group (ignore-errors (destroy body-bind-group)))
        (when buffer (ignore-errors (destroy buffer)))))))

(defun temporal-resolve-kind (device)
  "Return the temporal implementation selected for DEVICE, or NIL."
  #-darwin (declare (ignore device))
  (when *temporal-upscaling-p*
    #+darwin
    (if (typep device 'metal-gpu-device) :metalfx :shader)
    #-darwin :shader))

(defun renderer-metalfx-temporal-p (renderer)
  (eq :metalfx (renderer-temporal-resolve-kind renderer)))

(defun renderer-shader-temporal-p (renderer)
  (eq :shader (renderer-temporal-resolve-kind renderer)))

(defun make-renderer-flame-depth-sampler (device)
  "Create the renderer-lifetime nearest sampler used only for opaque depth."
  (create device
          (make-sampler-descriptor
           :label "luft torch flame opaque-depth sampler"
           :mag-filter :nearest :min-filter :nearest
           :mipmap-filter :nearest)))

(defun make-renderer-target-flame-bind-group
    (renderer flame-instance-buffer depth-view)
  "Join one immutable flame population to one immutable depth target."
  (unless (and flame-instance-buffer depth-view)
    (error "A flame composite binding needs both frame storage and opaque depth."))
  (create
   (renderer-device renderer)
   (make-bind-group-descriptor
    :label "luft post-temporal torch flames"
    :layout (renderer-flame-layout renderer)
    :entries
    `((:binding 0 :resource ,flame-instance-buffer)
      (:binding 1 :resource ,(renderer-camera-buffer renderer))
      (:binding 2 :resource ,(renderer-flame-effect-buffer renderer))
      (:binding 3 :resource ,depth-view)
      (:binding 4 :resource ,(renderer-flame-depth-sampler renderer))))))

(defun make-retargeted-renderer-target-generation
    (renderer flame-instance-buffer)
  "Stage a target record whose only new resource is its flame/depth join.

All other resources are borrowed from the currently installed immutable
generation.  The caller either publishes this record and transfers ownership
of that cohort, retiring only the old join, or destroys the returned new join
on rollback.  No target texture is duplicated for an ordinary residency edit."
  (let* ((old (renderer-target-generation renderer))
         (depth-view (renderer-target-generation-depth-view old)))
    (if (null depth-view)
        (values old nil)
        (let (flame-group candidate (completed-p nil))
          (unwind-protect
               (progn
                 (setf flame-group
                       (make-renderer-target-flame-bind-group
                        renderer flame-instance-buffer depth-view)
                       candidate
                       (%make-renderer-target-generation
                        (renderer-target-generation-resources old)
                        (%make-renderer-flame-target-join flame-group))
                       completed-p t)
                 (values candidate flame-group))
            (unless completed-p
              (when flame-group (ignore-errors (destroy flame-group)))))))))

(defun destroy-renderer-flame-target-join (join)
  "Retire only JOIN's population-times-depth bind group."
  (check-type join renderer-flame-target-join)
  (let ((group (renderer-flame-target-join-bind-group join)))
    (when group (ignore-errors (destroy group))))
  (values))

(defun destroy-renderer-target-resources (resources)
  "Retire one unreferenced output-size texture/view/resource cohort."
  (check-type resources renderer-target-resources)
  ;; Bind groups die before the views they name; views die before textures.
  ;; The temporal scaler has its own backend-owned auxiliary resources and is
  ;; retired before the external texture cohort used to execute it.
  (dolist (resource
            (list
             (renderer-target-resources-present-bind-group resources)
             (renderer-target-resources-exposure-probe-bind-group resources)
             (renderer-target-resources-composite-source-bind-group resources)
             (renderer-target-resources-temporal-bind-group resources)
             (renderer-target-resources-temporal-scaler resources)
             (renderer-target-resources-composite-view resources)
             (renderer-target-resources-history-view resources)
             (renderer-target-resources-resolved-view resources)
             (renderer-target-resources-motion-view resources)
             (renderer-target-resources-motion-msaa-view resources)
             (renderer-target-resources-scene-view resources)
             (renderer-target-resources-scene-msaa-view resources)
             (renderer-target-resources-depth-view resources)
             (renderer-target-resources-depth-msaa-view resources)
             (renderer-target-resources-composite-texture resources)
             (renderer-target-resources-history-texture resources)
             (renderer-target-resources-resolved-texture resources)
             (renderer-target-resources-motion-texture resources)
             (renderer-target-resources-motion-msaa-texture resources)
             (renderer-target-resources-scene-texture resources)
             (renderer-target-resources-scene-msaa-texture resources)
             (renderer-target-resources-depth-texture resources)
             (renderer-target-resources-depth-msaa-texture resources)))
    (when resource (ignore-errors (destroy resource))))
  (values))

(defun destroy-renderer-target-generation (generation)
  "Retire both independently owned halves of an unpublished generation."
  (check-type generation renderer-target-generation)
  (destroy-renderer-flame-target-join
   (renderer-target-generation-flame-join generation))
  (destroy-renderer-target-resources
   (renderer-target-generation-resources generation))
  (values))

(defun destroy-renderer-targets (renderer)
  "Unpublish and retire RENDERER's complete resize-owned generation."
  (let ((old-generation (renderer-target-generation renderer)))
    (setf (renderer-target-generation renderer)
          (%make-empty-renderer-target-generation))
    (destroy-renderer-target-generation old-generation)))

(defun render-scale-extent (extent)
  "Return the even-sized internal render extent for output EXTENT."
  (mapcar (lambda (dimension)
            (max 2 (* 2 (round (* 0.5 *render-scale* dimension)))))
          extent))

(defun renderer-render-scale-extent (renderer extent)
  "Return RENDERER's internal extent for output EXTENT.

MetalFX performs temporal upscaling from *RENDER-SCALE*.  Luft's inspectable
Vulkan resolve is the original native-resolution TAA algorithm; it accumulates
subpixel samples but does not claim a stable reconstruction-upscaling filter."
  (if (renderer-shader-temporal-p renderer)
      (copy-list extent)
      (render-scale-extent extent)))

(defun make-renderer-target-generation (renderer extent)
  "Stage one complete output-size generation without publishing it."
  (let* ((device (renderer-device renderer))
         (temporal-p (renderer-temporal-p renderer))
         (metalfx-p (renderer-metalfx-temporal-p renderer))
         (shader-temporal-p (renderer-shader-temporal-p renderer))
         ;; These owned copies are the immutable dimensions of the candidate.
         ;; Validate/list-copy before the first GPU allocation.
         (extent (copy-list extent))
         (render-extent (renderer-render-scale-extent renderer extent))
         scaler depth-msaa depth-msaa-view depth depth-view
         scene-msaa scene-msaa-view scene scene-view
         motion-msaa motion-msaa-view motion motion-view
         resolved resolved-view history history-view temporal-group
         composite composite-view
         composite-source-group flame-group present-group exposure-probe-group
         resource-cohort flame-join generation
         (completed-p nil))
    (labels ((usage (base extra)
               (remove-duplicates (append base extra)))
             (cleanup-locals ()
               (dolist (resource
                         (list present-group exposure-probe-group flame-group
                               composite-source-group temporal-group scaler
                               composite-view history-view resolved-view
                               motion-view motion-msaa-view
                               scene-view scene-msaa-view
                               depth-view depth-msaa-view composite
                               history resolved motion motion-msaa
                               scene scene-msaa depth depth-msaa))
                 (when resource (ignore-errors (destroy resource))))))
      (unwind-protect
           (progn
             (when metalfx-p
               (setf scaler
                     (create
                      device
                      (make-temporal-scaler-descriptor
                       :label "luft MetalFX temporal scaler"
                       :input-size render-extent :output-size extent))))
             (setf depth
                   (create
                    device
                    (make-texture-descriptor
                     :label "luft temporal depth" :size render-extent
                     :dimensions :2d :format :depth32-float
                     :usage
                     (usage '(:render-attachment :texture-binding)
                            (and scaler
                                 (gpu-temporal-scaler-depth-usage scaler)))))
                   depth-view
                   (create
                    device (make-texture-view-descriptor :texture depth))
                   depth-msaa
                   (create
                    device
                    (make-texture-descriptor
                     :label "luft multisampled depth" :size render-extent
                     :dimensions :2d :format :depth32-float
                     :usage :render-attachment
                     :sample-count *scene-sample-count*))
                   depth-msaa-view
                   (create
                    device
                    (make-texture-view-descriptor :texture depth-msaa))
                   scene
                   (create
                    device
                    (make-texture-descriptor
                     :label "luft HDR color" :size render-extent
                     :dimensions :2d :format :rgba16-float
                     :usage
                     (usage '(:render-attachment :texture-binding)
                            (and scaler
                                 (gpu-temporal-scaler-color-usage scaler)))))
                   scene-view
                   (create
                    device (make-texture-view-descriptor :texture scene))
                   scene-msaa
                   (create
                    device
                    (make-texture-descriptor
                     :label "luft multisampled HDR color" :size render-extent
                     :dimensions :2d :format :rgba16-float
                     :usage :render-attachment
                     :sample-count *scene-sample-count*))
                   scene-msaa-view
                   (create
                    device
                    (make-texture-view-descriptor :texture scene-msaa)))
             (when temporal-p
               (setf motion
                     (create
                      device
                      (make-texture-descriptor
                       :label "luft temporal motion" :size render-extent
                       :dimensions :2d :format :rg16-float
                       :usage
                       (usage (if shader-temporal-p
                                  '(:render-attachment :texture-binding)
                                  '(:render-attachment))
                              (and scaler
                                   (gpu-temporal-scaler-motion-usage scaler)))))
                     motion-view
                     (create
                      device (make-texture-view-descriptor :texture motion))
                     motion-msaa
                     (create
                      device
                      (make-texture-descriptor
                       :label "luft multisampled temporal motion"
                       :size render-extent :dimensions :2d
                       :format :rg16-float :usage :render-attachment
                       :sample-count *scene-sample-count*))
                     motion-msaa-view
                     (create
                      device
                      (make-texture-view-descriptor :texture motion-msaa))
                     resolved
                     (create
                      device
                      (make-texture-descriptor
                       :label "luft temporal resolve" :size extent
                       :dimensions :2d :format :rgba16-float
                       :usage
                       (usage
                        (if shader-temporal-p
                            '(:render-attachment :texture-binding :copy-src)
                            '(:texture-binding))
                        (and scaler
                             (gpu-temporal-scaler-output-usage scaler)))))
                     resolved-view
                     (create
                      device (make-texture-view-descriptor :texture resolved)))
               (when shader-temporal-p
                 (setf history
                       (create
                        device
                        (make-texture-descriptor
                         :label "luft temporal history" :size extent
                         :dimensions :2d :format :rgba16-float
                         :usage '(:texture-binding :copy-dst)))
                       history-view
                       (create
                        device (make-texture-view-descriptor :texture history))
                       temporal-group
                       (create
                        device
                        (make-bind-group-descriptor
                         :label "luft temporal resolve inputs"
                         :layout (renderer-temporal-layout renderer)
                         :entries
                         `((:binding 0 :resource ,scene-view)
                           (:binding 1 :resource ,motion-view)
                           (:binding 2 :resource ,history-view)
                           (:binding 3 :resource ,(renderer-sampler renderer))
                           (:binding 4
                            :resource ,(renderer-camera-buffer renderer))))))))
             (setf composite
                   (create
                    device
                    (make-texture-descriptor
                     :label "luft post-temporal HDR composite" :size extent
                     :dimensions :2d :format :rgba16-float
                     :usage '(:render-attachment :texture-binding)))
                   composite-view
                   (create
                    device (make-texture-view-descriptor :texture composite)))
             (let ((base-view (or resolved-view scene-view)))
               (setf composite-source-group
                     (create
                      device
                      (make-bind-group-descriptor
                       :label "luft HDR composite source"
                       :layout (renderer-composite-layout renderer)
                       :entries
                       `((:binding 0 :resource ,base-view)
                         (:binding 1 :resource ,(renderer-sampler renderer)))))
                     flame-group
                     (make-renderer-target-flame-bind-group
                      renderer (renderer-flame-instance-buffer renderer)
                      depth-view)
                     present-group
                     (create
                      device
                      (make-bind-group-descriptor
                       :label "luft HDR presentation"
                       :layout (renderer-present-layout renderer)
                       :entries
                       `((:binding 0 :resource ,composite-view)
                         (:binding 1 :resource ,(renderer-sampler renderer))
                         (:binding 2 :resource ,depth-view)
                         (:binding 3
                          :resource ,(renderer-camera-buffer renderer)))))
                     exposure-probe-group
                     (create
                      device
                      (make-bind-group-descriptor
                       :label "luft exposure probe source"
                       :layout (renderer-exposure-probe-layout renderer)
                       :entries
                       `((:binding 0 :resource ,composite-view)
                         (:binding 1
                          :resource ,(renderer-sampler renderer)))))))
             (setf resource-cohort
                   (%make-renderer-target-resources
                    :extent extent :render-extent render-extent
                    :temporal-scaler scaler
                    :depth-msaa-texture depth-msaa
                    :depth-msaa-view depth-msaa-view
                    :depth-texture depth :depth-view depth-view
                    :scene-msaa-texture scene-msaa
                    :scene-msaa-view scene-msaa-view
                    :scene-texture scene :scene-view scene-view
                    :motion-msaa-texture motion-msaa
                    :motion-msaa-view motion-msaa-view
                    :motion-texture motion :motion-view motion-view
                    :resolved-texture resolved :resolved-view resolved-view
                    :history-texture history :history-view history-view
                    :temporal-bind-group temporal-group
                    :composite-texture composite :composite-view composite-view
                    :composite-source-bind-group composite-source-group
                    :present-bind-group present-group
                    :exposure-probe-bind-group exposure-probe-group)
                   flame-join
                   (%make-renderer-flame-target-join flame-group)
                   generation
                   (%make-renderer-target-generation resource-cohort flame-join)
                   completed-p t)
             generation)
        (unless completed-p
          ;; A CPU condition can occur after the final GPU create but before
          ;; the immutable record exists.  Locals retain ownership until then.
          (if generation
              (destroy-renderer-target-generation generation)
              (cleanup-locals)))))))

(defvar *renderer-target-generation-precommit-hook* nil
  "Test hook called with renderer and complete target candidate before swap.")

(defun replace-renderer-target-generation (renderer extent)
  "Atomically publish a complete generation, then retire the prior one."
  (let ((old-generation (renderer-target-generation renderer))
        (candidate nil)
        (installed-p nil))
    (unwind-protect
         (progn
           (setf candidate (make-renderer-target-generation renderer extent))
           (when *renderer-target-generation-precommit-hook*
             (funcall *renderer-target-generation-precommit-hook*
                      renderer candidate))
           ;; This is the sole target-identity publication write.  Temporal
           ;; history is invalidated only after the complete resource cohort
           ;; is visible, and the old resources remain live until afterward.
           (setf (renderer-target-generation renderer) candidate
                 installed-p t)
           (with-release-warnings
             (releasing :superseded-frame-bindings
               (clear-renderer-frame-bind-groups renderer)))
           (setf (renderer-previous-view renderer) nil
                 (renderer-history-valid-p renderer) nil
                 (renderer-history-used-p renderer) nil)
           (destroy-renderer-target-generation old-generation)
           candidate)
      (unless installed-p
        (when candidate
          (destroy-renderer-target-generation candidate))))))

(defun create-frame-targets (renderer extent)
  "Compatibility entry point for initial target publication."
  (replace-renderer-target-generation renderer extent)
  renderer)

(defun ensure-renderer-extent (renderer extent)
  (unless (equal extent (renderer-extent renderer))
    (replace-renderer-target-generation renderer extent))
  renderer)

(zdefun (mesh-lattice-point-words :zone :luft/prepare-overlay) (mesh)
  "LUFT vertex sites, mesh vertices, and eighth-step boundary-edge samples."
  (let ((points (make-hash-table :test #'eql))
        (result (make-array 64 :element-type '(unsigned-byte 32)
                              :adjustable t :fill-pointer 0))
        (templates (luft:surface-mesh-template-vertex-words mesh))
        (ranges (luft:surface-mesh-template-ranges mesh)))
    (labels ((pack-point (x y z)
               ;; World coordinates are non-negative and comfortably below
               ;; twenty bits at the eighth-cell scale. One fixnum is a
               ;; cons-free hash key for the diagnostic point vocabulary.
               (unless (and (typep x '(unsigned-byte 20))
                            (typep y '(unsigned-byte 20))
                            (typep z '(unsigned-byte 20)))
                 (error "LUFT lattice point (~D ~D ~D) exceeds packed range."
                        x y z))
               (logior x (ash y 20) (ash z 40)))
             (remember (x y z marker-kind)
               (let ((key (pack-point x y z)))
                 (setf (gethash key points)
                       (max marker-kind (gethash key points 0)))))
             (template-coordinate (base vertex axis)
               (+ (* luft:+mesh-cell-size+ base)
                  (- (aref templates
                           (+ (* vertex luft:+mesh-template-vertex-word-count+)
                              axis))
                     luft:+mesh-template-coordinate-bias+)))
             (sample-axis-edge (ax ay az bx by bz)
               (cond
                 ((and (= ay by) (= az bz) (/= ax bx))
                  (loop for x from (min ax bx) to (max ax bx)
                        do (remember x ay az 0)))
                 ((and (= ax bx) (= az bz) (/= ay by))
                  (loop for y from (min ay by) to (max ay by)
                        do (remember ax y az 0)))
                 ((and (= ax bx) (= ay by) (/= az bz))
                  (loop for z from (min az bz) to (max az bz)
                        do (remember ax ay z 0)))))
             (visit-stream (words fan-p)
               (loop for instance-offset from 0 below (length words) by 4
                     for base-x = (aref words instance-offset)
                     for base-y = (aref words (+ instance-offset 1))
                     for base-z = (aref words (+ instance-offset 2))
                     for packed = (aref words (+ instance-offset 3))
                     for template-id = (ldb (byte 16 0) packed)
                     for vertex-start = (aref ranges (* 2 template-id))
                     for vertex-count = (aref ranges (1+ (* 2 template-id)))
                     do (when fan-p
                          (remember (* luft:+mesh-cell-size+ base-x)
                                    (* luft:+mesh-cell-size+ base-y)
                                    (* luft:+mesh-cell-size+ base-z) 2))
                        (loop for vertex from vertex-start
                                below (+ vertex-start vertex-count)
                              do (remember
                                  (template-coordinate base-x vertex 0)
                                  (template-coordinate base-y vertex 1)
                                  (template-coordinate base-z vertex 2) 1))
                        (loop for vertex from vertex-start
                                below (+ vertex-start vertex-count) by 3
                              for attributes =
                                (aref templates
                                      (+ (* vertex 4) 3))
                              for edge-mask = (ldb (byte 3 10) attributes)
                              for ax = (template-coordinate base-x vertex 0)
                              for ay = (template-coordinate base-y vertex 1)
                              for az = (template-coordinate base-z vertex 2)
                              for bx = (template-coordinate base-x (1+ vertex) 0)
                              for by = (template-coordinate base-y (1+ vertex) 1)
                              for bz = (template-coordinate base-z (1+ vertex) 2)
                              for cx = (template-coordinate base-x (+ vertex 2) 0)
                              for cy = (template-coordinate base-y (+ vertex 2) 1)
                              for cz = (template-coordinate base-z (+ vertex 2) 2)
                              when (logbitp 0 edge-mask)
                                do (sample-axis-edge bx by bz cx cy cz)
                              when (logbitp 1 edge-mask)
                                do (sample-axis-edge ax ay az cx cy cz)
                              when (logbitp 2 edge-mask)
                                do (sample-axis-edge ax ay az bx by bz)))))
      (visit-stream (luft:surface-mesh-face-instance-words mesh) nil)
      (visit-stream (luft:surface-mesh-band-instance-words mesh) nil)
      (visit-stream (luft:surface-mesh-fan-instance-words mesh) t))
    (maphash
     (lambda (point marker-kind)
       (vector-push-extend (ldb (byte 20 0) point) result)
       (vector-push-extend (ldb (byte 20 20) point) result)
       (vector-push-extend (ldb (byte 20 40) point) result)
       (vector-push-extend marker-kind result))
     points)
    (coerce result '(simple-array (unsigned-byte 32) (*)))))

(defun %destroy-mesh-slot (slot)
  (%destroy-resident-population (mesh-slot-resident slot))
  (dolist (resource (list (mesh-slot-lattice-point-group slot)
                          (mesh-slot-lattice-point-buffer slot)))
    (when resource (ignore-errors (destroy resource))))
  (values))

(defun mesh-slot-prepared-mesh (slot)
  "Borrow SLOT's immutable CPU realization for renderer reconstruction."
  (%make-prepared-render-mesh
   (mesh-slot-mesh slot)
   (resident-population-population (mesh-slot-resident slot))))

(defun %make-renderer-mesh-slot (renderer mesh-or-prepared provenance)
  "Upload one independently retained chunk slot.

MESH-OR-PREPARED may carry worker-built dense population arrays. Construction
overlay data is deliberately absent until construction mode asks for it."
  (let* ((prepared
           (if (typep mesh-or-prepared 'prepared-render-mesh)
               mesh-or-prepared
               (prepare-render-mesh mesh-or-prepared)))
         (mesh (prepared-render-mesh-mesh prepared))
         (slot (%make-mesh-slot :mesh mesh :provenance provenance))
         (completed-p nil))
    (unwind-protect
         (progn
           (setf (mesh-slot-resident slot)
                 (%upload-render-population
                  renderer (prepared-render-mesh-population prepared)))
           (setf completed-p t)
           slot)
      (unless completed-p
        (%destroy-mesh-slot slot)))))

(defun ensure-mesh-slot-lattice-points (renderer slot)
  "Create SLOT's diagnostic overlay on first use, never during normal streaming."
  (unless (mesh-slot-lattice-point-buffer slot)
    (let* ((device (renderer-device renderer))
           (camera-buffer (renderer-camera-buffer renderer))
           (lattice-point-words
             (mesh-lattice-point-words (mesh-slot-mesh slot)))
           (lattice-point-count (/ (length lattice-point-words) 4))
           (completed-p nil))
      (flet ((stream-buffer (label words)
               (let ((buffer (create device
                                     (make-buffer-descriptor
                                      :label label
                                      :size (max 16 (* 4 (length words)))
                                      :usage '(:storage :copy-dst)))))
                 (when (plusp (length words))
                   (write-buffer buffer words))
                 buffer)))
        (unwind-protect
             (progn
               (setf (mesh-slot-lattice-point-count slot) lattice-point-count
                     (mesh-slot-lattice-point-buffer slot)
                     (stream-buffer "luft unique eighth-cell lattice points"
                                    lattice-point-words))
               (setf (mesh-slot-lattice-point-group slot)
                     (create device
                             (make-bind-group-descriptor
                              :label "luft eighth-cell lattice points"
                              :layout (renderer-lattice-point-layout renderer)
                              :entries
                              `((:binding 0
                                 :resource ,(mesh-slot-lattice-point-buffer
                                             slot))
                                (:binding 1 :resource ,camera-buffer)))))
               (setf completed-p t))
          (unless completed-p
            (dolist (resource (list (mesh-slot-lattice-point-group slot)
                                    (mesh-slot-lattice-point-buffer slot)))
              (when resource (ignore-errors (destroy resource))))
            (setf (mesh-slot-lattice-point-group slot) nil
                  (mesh-slot-lattice-point-buffer slot) nil
                  (mesh-slot-lattice-point-count slot) 0))))))
  slot)

(defun %destroy-resident-population (resident)
  (when resident
    (dolist (resource (list (resident-population-bind-group resident)
                            (resident-population-shadow-bind-group resident)
                            (resident-population-descriptor-buffer resident)
                            (resident-population-appearance-buffer resident)
                            (resident-population-instance-buffer resident)))
      (when resource (ignore-errors (destroy resource)))))
  (values))

(zdefun (%upload-render-population :zone :luft/upload-slot)
    (renderer population)
  "Build and upload one candidate population without changing RENDERER."
  (let* ((device (renderer-device renderer))
         (instance-words (render-population-instance-words population))
         (appearance-words (render-population-appearance-words population))
         (descriptor-words (render-population-descriptor-words population))
         instance-buffer template-buffer appearance-buffer descriptor-buffer bind-group
         shadow-bind-group
         (completed-p nil))
    (flet ((stream-buffer (label words)
             (let ((buffer
                     (create device
                             (make-buffer-descriptor
                              :label label
                              :size (max 16 (* 4 (length words)))
                              :usage '(:storage :copy-dst)))))
               (when (plusp (length words))
                 (write-buffer buffer words))
               buffer)))
      (unwind-protect
           (progn
             (setf instance-buffer
                   (stream-buffer "luft resident site instances" instance-words)
                   template-buffer
                   (renderer-star-template-buffer renderer)
                   appearance-buffer
                   (stream-buffer "luft active-star appearance sidecars"
                                  appearance-words)
                   descriptor-buffer
                   (stream-buffer "luft terrain material descriptors"
                                  descriptor-words)
                   bind-group
                   (create device
                           (make-bind-group-descriptor
                            :label "luft resident site population"
                            :layout (renderer-layout renderer)
                            :entries
                            `((:binding 0 :resource ,instance-buffer)
                              (:binding 1 :resource ,template-buffer)
                              (:binding 2
                               :resource ,(renderer-camera-buffer renderer))
                              (:binding 3 :resource ,appearance-buffer)
                              (:binding 4
                               :resource ,(renderer-shadow-view renderer))
                              (:binding 5
                               :resource ,(renderer-shadow-sampler renderer))
                              (:binding 6 :resource ,descriptor-buffer))))
                   shadow-bind-group
                   (create device
                           (make-bind-group-descriptor
                            :label "luft resident shadow population"
                            :layout (renderer-shadow-layout renderer)
                            :entries
                            `((:binding 0 :resource ,instance-buffer)
                              (:binding 1 :resource ,template-buffer)
                              (:binding 2
                               :resource ,(renderer-camera-buffer renderer))))))
             (let ((resident
                     (%make-resident-population
                      population instance-buffer template-buffer appearance-buffer
                      descriptor-buffer bind-group shadow-bind-group)))
               (setf completed-p t)
               resident))
        (unless completed-p
          (dolist (resource
                    (list shadow-bind-group bind-group descriptor-buffer
                          appearance-buffer instance-buffer))
            (when resource (ignore-errors (destroy resource)))))))))

(defun renderer-set-mesh (renderer key mesh &key scene-generation)
  "Make MESH resident under KEY, replacing any previous resident mesh."
  (cdar (renderer-set-meshes
         renderer (list (cons key mesh))
         :scene-generation scene-generation)))

(defun renderer-set-meshes (renderer meshes &key scene-generation)
  "Transactionally replace the keyed MESHES as one visible residency cohort.

MESHES is an alist of key to surface mesh. Every GPU slot is created before
the renderer's table changes; a failed upload therefore leaves the installed
cohort untouched. No frame can interleave with the owner-thread publication."
  (renderer-update-meshes
   renderer meshes nil :scene-generation scene-generation))

(defun prospective-renderer-slot-entries (renderer candidates removed-keys)
  "Return the sorted slot population after applying one candidate cohort."
  (let ((entries nil))
    (loop for key being the hash-keys of (renderer-mesh-slots renderer)
          for slot = (gethash key (renderer-mesh-slots renderer))
          unless (or (assoc key candidates :test #'eql)
                     (member key removed-keys :test #'eql))
            do (push (cons key slot) entries))
    (dolist (entry candidates)
      (push entry entries))
    (sort entries #'< :key #'car)))

(defun renderer-slot-table (entries)
  "Materialize sorted key-to-slot ENTRIES into a fresh publication table."
  (let ((table (make-hash-table :test #'eql :size (length entries))))
    (dolist (entry entries table)
      (setf (gethash (car entry) table) (cdr entry)))))

(defun validate-renderer-mesh-update (meshes removed-keys)
  "Validate one keyed residency transaction before any GPU allocation."
  (check-type meshes list)
  (check-type removed-keys list)
  (let ((candidate-keys (make-hash-table :test #'eql))
        (removal-keys (make-hash-table :test #'eql)))
    (dolist (entry meshes)
      (unless (consp entry)
        (error "A renderer mesh cohort entry must be (KEY . MESH), not ~S."
               entry))
      (let ((key (car entry))
            (mesh (cdr entry)))
        (check-type key luft:chunk-key)
        (unless (or (typep mesh 'luft:surface-mesh)
                    (typep mesh 'prepared-render-mesh))
          (error "Renderer owner ~D has invalid mesh value ~S." key mesh))
        (when (nth-value 1 (gethash key candidate-keys))
          (error "Renderer mesh cohort repeats owner ~D." key))
        (setf (gethash key candidate-keys) t)))
    (dolist (key removed-keys)
      (check-type key luft:chunk-key)
      (when (nth-value 1 (gethash key removal-keys))
        (error "Renderer removal cohort repeats owner ~D." key))
      (when (nth-value 1 (gethash key candidate-keys))
        (error "Renderer owner ~D cannot be replaced and removed together."
               key))
      (setf (gethash key removal-keys) t)))
  (values))

(defun prepare-renderer-mesh-candidates (meshes)
  "Prepare MESHES into immutable CPU populations without allocating on the GPU."
  (loop for (key . mesh) in meshes
        for prepared = (if (typep mesh 'prepared-render-mesh)
                           mesh
                           (prepare-render-mesh mesh))
        collect (cons key prepared)))

(defun surface-mesh-tree-uses-light-field-p (mesh field)
  "Whether MESH and every companion retain exact immutable FIELD."
  (and (eq field (luft:surface-mesh-voxel-light mesh))
       (every (lambda (companion)
                (surface-mesh-tree-uses-light-field-p companion field))
              (luft:surface-mesh-companions mesh))))

(defun make-renderer-slot-provenance (key generation manifest-index)
  "Project GENERATION's matched output into one nonrecursive slot witness."
  (let* ((manifest
           (scene-mesh-generation-mesh-manifest generation))
         (entry (aref manifest manifest-index))
         (merged
           (scene-mesh-generation-slot-provenances generation)))
    (if (plusp (length merged))
        (let ((provenance (aref merged manifest-index)))
          (unless (and (eql key (renderer-slot-provenance-key provenance))
                       (eq (scene-mesh-generation-scene generation)
                           (renderer-slot-provenance-scene provenance)))
            (error "Merged scene-generation provenance does not name renderer owner ~S."
                   key))
          provenance)
        (%make-renderer-slot-provenance
         key (scene-mesh-generation-scene generation)
         (copy-scene-generation-stamp-value
          (scene-mesh-generation-request-stamp generation))
         (copy-scene-generation-stamp-value
          (scene-mesh-generation-result-stamp generation))
         (scene-mesh-generation-light-generation generation)
         (scene-mesh-output-manifest-entry-tree entry)))))

(defun consume-renderer-generation-manifest
    (prepared-meshes generation)
  "Match every candidate key/tree one-to-one against GENERATION's manifest."
  (let* ((manifest (scene-mesh-generation-mesh-manifest generation))
         (used (make-array (length manifest) :element-type 'bit
                                             :initial-element 0)))
    (unless (= (length prepared-meshes) (length manifest))
      (error "Renderer candidate count ~D does not match generation output count ~D."
             (length prepared-meshes) (length manifest)))
    (loop for (key . prepared) in prepared-meshes
          for mesh = (prepared-render-mesh-mesh prepared)
          for index =
            (loop for index below (length manifest)
                  for entry = (aref manifest index)
                  when (and (zerop (aref used index))
                            (or (not
                                 (scene-mesh-output-manifest-entry-keyed-p
                                  entry))
                                (eql key
                                     (scene-mesh-output-manifest-entry-key
                                      entry)))
                            (surface-mesh-tree-manifest-matches-p
                             (scene-mesh-output-manifest-entry-tree entry)
                             mesh))
                    return index)
          unless index
            do (error "Renderer owner ~S is not the exact mesh tree named by its scene generation."
                      key)
          do (setf (aref used index) 1)
          collect (cons key
                        (make-renderer-slot-provenance
                         key generation index)))))

(defun prospective-renderer-semantic-entries
    (renderer prepared-meshes removed-keys candidate-provenances)
  "Return sorted (KEY MESH PROVENANCE) tuples before any GPU allocation."
  (let ((entries nil))
    (loop for key being the hash-keys of (renderer-mesh-slots renderer)
          using (hash-value slot)
          unless (or (assoc key prepared-meshes :test #'eql)
                     (member key removed-keys :test #'eql))
            do (push (list key (mesh-slot-mesh slot)
                           (and
                            (renderer-publication-scene-generation
                             (renderer-publication renderer))
                            (mesh-slot-provenance slot)))
                     entries))
    (dolist (entry prepared-meshes)
      (push
       (list (car entry)
             (prepared-render-mesh-mesh (cdr entry))
             (cdr (assoc (car entry) candidate-provenances :test #'eql)))
       entries))
    (sort entries #'< :key #'car)))

(defun scene-generation-exactly-manifests-semantic-entries-p
    (generation entries)
  "Whether a previously merged GENERATION already names all keyed ENTRIES."
  (let* ((manifest (scene-mesh-generation-mesh-manifest generation))
         (provenances (scene-mesh-generation-slot-provenances generation))
         (used (make-array (length manifest) :element-type 'bit
                                             :initial-element 0)))
    (and (= (length entries) (length manifest) (length provenances))
         (every
          (lambda (semantic)
            (let ((key (first semantic))
                  (mesh (second semantic))
                  (provenance (third semantic)))
              (loop for index below (length manifest)
                    for entry = (aref manifest index)
                    when (and (zerop (aref used index))
                              (scene-mesh-output-manifest-entry-keyed-p entry)
                              (eql key
                                   (scene-mesh-output-manifest-entry-key entry))
                              (eq provenance (aref provenances index))
                              (surface-mesh-tree-manifest-matches-p
                               (scene-mesh-output-manifest-entry-tree entry)
                               mesh))
                      do (setf (aref used index) 1)
                         (return t)
                    finally (return nil))))
          entries))))

(defun make-renderer-publication-scene-generation (generation entries)
  "Merge the current transaction stamp with exact prospective slot lineage."
  (when generation
    (if (scene-generation-exactly-manifests-semantic-entries-p
         generation entries)
        generation
        (make-scene-mesh-generation-value
         (scene-mesh-generation-scene generation)
         (scene-mesh-generation-request-stamp generation)
         (scene-mesh-generation-light-generation generation)
         :mesh-entries
         (mapcar (lambda (entry) (cons (first entry) (second entry))) entries)
         :slot-provenances (map 'vector #'third entries)))))

(defun validate-renderer-scene-generation-cohort
    (renderer prepared-meshes removed-keys generation)
  "Validate and return prospective semantic entries before GPU allocation."
  (when generation
    (let* ((light-generation
             (scene-mesh-generation-light-generation generation))
           (field (realized-light-generation-field light-generation))
           (candidate-provenances
             (consume-renderer-generation-manifest
              prepared-meshes generation))
           (entries
             (prospective-renderer-semantic-entries
              renderer prepared-meshes removed-keys candidate-provenances)))
      (dolist (entry entries)
        (destructuring-bind (key mesh provenance) entry
          (unless provenance
            (error "Retained renderer owner ~S has no exact scene-generation provenance."
                   key))
          (unless (and
                   (eq (scene-mesh-generation-scene generation)
                       (renderer-slot-provenance-scene provenance))
                   (eq light-generation
                       (renderer-slot-provenance-light-generation provenance))
                   (surface-mesh-tree-uses-light-field-p mesh field)
                   (surface-mesh-tree-manifest-matches-p
                    (renderer-slot-provenance-tree provenance) mesh))
            (error "Renderer owner ~S does not retain its exact claimed mesh/light generation."
                   key))))
      entries)))

(defvar *renderer-publication-precommit-hook* nil
  "Optional test instrumentation run after complete staging, before commit.")

(defun renderer-publication-retired-slots (old-publication new-table)
  (let ((retired nil))
    (loop for key being the hash-keys of
          (renderer-publication-mesh-slots old-publication)
          using (hash-value old-slot)
          unless (eq old-slot (gethash key new-table))
            do (push old-slot retired))
    retired))

(defun destroy-renderer-publication-resources (publication)
  "Retire only PUBLICATION's global resources; mesh slots have separate sharing."
  (dolist (resource
            (list (renderer-publication-torch-body-shadow-bind-group publication)
                  (renderer-publication-torch-body-bind-group publication)
                  (renderer-publication-flame-instance-buffer publication)))
    (when resource (ignore-errors (destroy resource))))
  (values))

(zdefun (renderer-update-meshes :zone :luft/publish-residency)
    (renderer meshes removed-keys &key scene-generation)
  "Transactionally replace geometry and its realized torch-frame cohort."
  (validate-renderer-mesh-update meshes removed-keys)
  (when scene-generation
    (check-type scene-generation scene-mesh-generation))
  (let ((prepared-meshes nil)
        (semantic-entries nil)
        (publication-scene-generation nil)
        (candidates nil)
        (requested-candidates nil)
        (old-publication (renderer-publication renderer))
        (old-target-generation (renderer-target-generation renderer))
        (staged-publication nil)
        (staged-target-generation nil)
        (staged-target-flame-group nil)
        (publication-changed-p
          (or meshes
              (some (lambda (key)
                      (nth-value
                       1 (gethash key (renderer-mesh-slots renderer))))
                    removed-keys)))
        (torch-frame-data nil)
        (flame-count 0)
        (flame-buffer nil)
        (body-group nil)
        (body-shadow-group nil)
        (installed-p nil))
    (unless publication-changed-p
      (return-from renderer-update-meshes nil))
    (unwind-protect
         (progn
           ;; CPU canonicalization finishes before candidate GPU allocation.
           (setf prepared-meshes
                 (prepare-renderer-mesh-candidates meshes))
           (when scene-generation
             (setf semantic-entries
                   (validate-renderer-scene-generation-cohort
                    renderer prepared-meshes removed-keys scene-generation)
                   publication-scene-generation
                   (make-renderer-publication-scene-generation
                    scene-generation semantic-entries)))
           (dolist (entry prepared-meshes)
             (push (cons (car entry)
                         (%make-renderer-mesh-slot
                          renderer (cdr entry)
                          (and scene-generation
                               (third
                                (find (car entry) semantic-entries
                                      :key #'first :test #'eql)))))
                   candidates))
           (setf candidates (nreverse candidates))
           (setf requested-candidates
                 (mapcar
                  (lambda (entry)
                    (cons (car entry)
                          (cdr (assoc (car entry) candidates :test #'eql))))
                  prepared-meshes))
           (let* ((entries
                    (prospective-renderer-slot-entries
                     renderer candidates removed-keys))
                  (table (renderer-slot-table entries))
                  (order (mapcar #'car entries)))
             ;; Every fallible GPU and CPU staging operation finishes before
             ;; any part of this generation becomes visible.
             (multiple-value-setq
                 (torch-frame-data flame-count flame-buffer
                  body-group body-shadow-group)
               (%make-renderer-flame-resources
                renderer (mesh-slots-torch-frame-data entries)))
             (setf staged-publication
                   (%make-renderer-publication
                    table order torch-frame-data flame-count flame-buffer
                    body-group body-shadow-group publication-scene-generation))
             (multiple-value-setq
                 (staged-target-generation staged-target-flame-group)
               (make-retargeted-renderer-target-generation
                renderer flame-buffer))
             (when *renderer-publication-precommit-hook*
               (funcall *renderer-publication-precommit-hook*
                        renderer staged-publication))
             ;; Frames execute only on this owner thread.  These adjacent
             ;; pointer writes therefore publish the semantic residency and
             ;; its target-coupled flame/depth join as one noninterleavable
             ;; renderer transaction; both candidates are complete already.
             (setf (renderer-publication renderer) staged-publication
                   (renderer-target-generation renderer)
                   staged-target-generation
                   installed-p t)
             (with-release-warnings
               (releasing :superseded-frame-bindings
                 (clear-renderer-frame-bind-groups renderer)))
             ;; Residency changes invalidate the previous color/depth/motion
             ;; correspondence.  The next temporal encode must reset rather
             ;; than blend arrivals with history in which they did not exist,
             ;; or retain departed silhouettes as ghosts.
             (setf (renderer-history-valid-p renderer) nil)
             (dolist (slot
                       (renderer-publication-retired-slots
                        old-publication table))
               (%destroy-mesh-slot slot))
             (unless (eq staged-target-generation old-target-generation)
               (destroy-renderer-flame-target-join
                (renderer-target-generation-flame-join
                 old-target-generation)))
             (destroy-renderer-publication-resources old-publication))
           requested-candidates)
      (unless installed-p
        (when staged-target-flame-group
          (ignore-errors (destroy staged-target-flame-group)))
        (when staged-publication
          (destroy-renderer-publication-resources staged-publication))
        ;; A condition may occur after GPU staging but before the publication
        ;; record itself is allocated.  Those locals still own the resources.
        (unless staged-publication
          (when body-shadow-group (ignore-errors (destroy body-shadow-group)))
          (when body-group (ignore-errors (destroy body-group)))
          (when flame-buffer (ignore-errors (destroy flame-buffer))))
        (dolist (entry candidates) (%destroy-mesh-slot (cdr entry)))))))

(defun renderer-remove-mesh (renderer key)
  (renderer-update-meshes renderer nil (list key))
  (values))

(defun renderer-clear-meshes (renderer)
  (renderer-update-meshes renderer nil (copy-list (renderer-slot-order renderer)))
  (values))

(defun make-renderer (device color-format extent)
  "Create the shared LUFT pipeline state; meshes arrive via RENDERER-SET-MESH."
  (let* ((temporal-kind (temporal-resolve-kind device))
         (temporal-p (not (null temporal-kind)))
         (target-formats (if temporal-p
                             '(:rgba16-float :rg16-float)
                             '(:rgba16-float)))
         camera-buffer star-template-buffer
         layout
         vertex-module fragment-module pipeline
         shadow-texture shadow-view shadow-sampler shadow-layout
         shadow-vertex-module shadow-pipeline
         player-sdf-layout player-sdf-bind-group player-sdf-vertex-module
         player-sdf-fragment-module player-sdf-pipeline
         flame-layout flame-instance-buffer flame-effect-buffer
         flame-vertex-module flame-fragment-module flame-pipeline
         flame-depth-sampler composite-layout composite-fragment-module
         composite-pipeline
         torch-body-layout torch-body-vertex-buffer torch-body-bind-group
         torch-body-shadow-bind-group torch-body-vertex-module
         torch-body-shadow-vertex-module torch-body-fragment-module
         torch-body-pipeline
         torch-body-shadow-pipeline
         lattice-point-layout lattice-point-vertex-module
         lattice-point-fragment-module lattice-point-pipeline
         present-layout present-bind-group present-vertex-module
         present-fragment-module present-pipeline sampler
         temporal-layout temporal-fragment-module temporal-pipeline
         sky-layout sky-bind-group sky-fragment-module sky-pipeline
         exposure-probe-layout exposure-probe-bind-group
         exposure-probe-texture exposure-probe-view
         exposure-probe-fragment-module exposure-probe-pipeline
         exposure-probe-buffers
         renderer
         (completed-p nil))
    (unwind-protect
         (progn
           (setf camera-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft frame state"
                          :size (shaders::scene-uniform-byte-size)
                          :usage '(:uniform :copy-dst)))
                 flame-effect-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft torch flame effect"
                          :size (torch-flame-effect-byte-size)
                          :usage '(:uniform :copy-dst))))
           ;; Publish ownership to the constructor unwind list before the
           ;; first fallible upload touches this resource.
           (write-buffer flame-effect-buffer
                         (torch-flame-effect-uniform-data 0.0))
           (setf flame-instance-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft empty torch flame instances"
                          :size 16 :usage '(:storage :copy-dst)))
                 star-template-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft 256 star meshlets"
                          :size (* 4 256 +star-meshlet-record-count+ 4)
                          :usage '(:storage :copy-dst)))
                 shadow-texture
                 (create device
                         (make-texture-descriptor
                          :label "luft sun shadow depth"
                          :size (list +shadow-map-size+ +shadow-map-size+)
                          :dimensions :2d :format :depth32-float
                          :usage '(:render-attachment :texture-binding)))
                 shadow-view
                 (create device
                         (make-texture-view-descriptor :texture shadow-texture))
                 shadow-sampler
                 (create device
                         (make-sampler-descriptor
                          :label "luft soft shadow comparison sampler"
                          :mag-filter :linear :min-filter :linear
                          :mipmap-filter :nearest :compare :less-or-equal)))
           (write-buffer star-template-buffer (star-meshlet-template-words))
           (let ((body-vertices (torch-body-vertex-data)))
             (setf torch-body-vertex-buffer
                   (create device
                           (make-buffer-descriptor
                            :label "luft canonical framed torch body"
                            :size (* 4 (length body-vertices))
                            :usage '(:storage :copy-dst))))
             (write-buffer torch-body-vertex-buffer body-vertices))
           (setf layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft mesh layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :storage-buffer)
                                     (:binding 2 :type :uniform-buffer)
                                     (:binding 3 :type :storage-buffer)
                                     (:binding 4 :type :texture)
                                     (:binding 5 :type :sampler)
                                     (:binding 6 :type :storage-buffer))))
                 shadow-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft shadow layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :storage-buffer)
                                     (:binding 2 :type :uniform-buffer))))
                 vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft mesh vertex"
                          :language :mathematical
                          :code (shaders:mesh-vertex-specification)))
                 fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft mesh fragment" :language :mathematical
                          :code (shaders:star-fragment-specification)))
                 pipeline
                 (create device
                         (make-mesh-render-pipeline-descriptor
                          :label "luft site stream pipeline" :layout layout
                          :task nil :mesh `(:module ,vertex-module)
                          :fragment `(:module ,fragment-module
                                      :targets
                                      ,(mapcar (lambda (format)
                                                 `(:format ,format))
                                               target-formats))
                          :sample-count *scene-sample-count*
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled t
                            :depth-compare :less)))
                 shadow-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft shadow vertex"
                          :language :mathematical
                          :code (shaders:shadow-vertex-specification)))
                 shadow-pipeline
                 (create device
                         (make-mesh-render-pipeline-descriptor
                          :label "luft sun shadow pipeline"
                          :layout shadow-layout
                          :task nil :mesh `(:module ,shadow-vertex-module)
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled t
                            :depth-compare :less))))
           (setf torch-body-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft framed torch-body layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :storage-buffer)
                                     (:binding 2 :type :uniform-buffer)
                                     (:binding 4 :type :texture)
                                     (:binding 5 :type :sampler))))
                 torch-body-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft framed torch-body vertex"
                          :language :mathematical
                          :code
                          (shaders:torch-body-vertex-specification)))
                 torch-body-shadow-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft framed torch-body shadow vertex"
                          :language :mathematical
                          :code
                          (shaders:torch-body-shadow-vertex-specification)))
                 torch-body-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft framed torch-body fragment"
                          :language :mathematical
                          :code (shaders:torch-body-fragment-specification)))
                 torch-body-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft framed opaque torch bodies"
                          :layout torch-body-layout
                          :vertex `(:module ,torch-body-vertex-module)
                          :fragment
                          `(:module ,torch-body-fragment-module
                            :targets
                            ,(mapcar (lambda (format) `(:format ,format))
                                     target-formats))
                          :primitive '(:topology :triangle-list)
                          :sample-count *scene-sample-count*
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled t
                            :depth-compare :less)))
                 torch-body-shadow-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft framed torch-body shadows"
                          :layout shadow-layout
                          :vertex `(:module ,torch-body-shadow-vertex-module)
                          :primitive '(:topology :triangle-list)
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled t
                            :depth-compare :less)))
                 torch-body-bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft empty framed torch bodies"
                          :layout torch-body-layout
                          :entries
                          `((:binding 0 :resource ,flame-instance-buffer)
                            (:binding 1 :resource ,torch-body-vertex-buffer)
                            (:binding 2 :resource ,camera-buffer)
                            (:binding 4 :resource ,shadow-view)
                            (:binding 5 :resource ,shadow-sampler))))
                 torch-body-shadow-bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft empty framed torch-body shadows"
                          :layout shadow-layout
                          :entries
                          `((:binding 0 :resource ,flame-instance-buffer)
                            (:binding 1 :resource ,torch-body-vertex-buffer)
                            (:binding 2 :resource ,camera-buffer)))))
           (setf player-sdf-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft player sdf layout"
                          :entries '((:binding 0 :type :uniform-buffer)
                                     (:binding 1 :type :texture)
                                     (:binding 2 :type :sampler))))
                 player-sdf-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft player sdf vertex"
                          :language :mathematical
                          :code (shaders:player-sdf-vertex-specification)))
                 player-sdf-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft player sdf fragment"
                          :language :mathematical
                          :code (shaders:player-sdf-fragment-specification)))
                 player-sdf-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft walking player sdf pipeline"
                          :layout player-sdf-layout
                          :vertex `(:module ,player-sdf-vertex-module)
                          :fragment
                          `(:module ,player-sdf-fragment-module
                            :targets
                            ,(loop for format in target-formats
                                   for first = t then nil
                                   collect `(:format ,format
                                             ,@(when first
                                                 '(:blend :premultiplied-alpha)))))
                          :primitive '(:topology :triangle-list)
                          :sample-count *scene-sample-count*
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled nil
                            :depth-compare :less)))
                 player-sdf-bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft walking player sdf"
                          :layout player-sdf-layout
                          :entries
                          `((:binding 0 :resource ,camera-buffer)
                            (:binding 1 :resource ,shadow-view)
                            (:binding 2 :resource ,shadow-sampler)))))
           (setf flame-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft torch flame layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :uniform-buffer)
                                     (:binding 2 :type :uniform-buffer)
                                     (:binding 3 :type :texture)
                                     (:binding 4 :type :sampler))))
                 flame-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft torch flame vertex"
                          :language :mathematical
                          :code (shaders:torch-flame-vertex-specification)))
                 flame-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft torch flame fragment"
                          :language :mathematical
                          :code (shaders:torch-flame-fragment-specification)))
                 flame-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft volumetric torch flame pipeline"
                          :layout flame-layout
                          :vertex `(:module ,flame-vertex-module)
                          :fragment
                          `(:module ,flame-fragment-module
                            :targets
                            ((:format :rgba16-float
                              :blend :premultiplied-alpha)))
                          :primitive '(:topology :triangle-list))))
           (setf lattice-point-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft lattice point layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :uniform-buffer))))
                 lattice-point-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft lattice point vertex"
                          :language :mathematical
                          :code (shaders:lattice-point-vertex-specification)))
                 lattice-point-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft lattice point fragment"
                          :language :mathematical
                          :code (shaders:lattice-point-fragment-specification)))
                 lattice-point-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft eighth-cell lattice point pipeline"
                          :layout lattice-point-layout
                          :vertex `(:module ,lattice-point-vertex-module)
                          :fragment
                          `(:module ,lattice-point-fragment-module
                            :targets
                            ,(loop for format in target-formats
                                   for first = t then nil
                                   collect `(:format ,format
                                             ,@(when first
                                                 '(:blend :premultiplied-alpha)))))
                          :primitive '(:topology :triangle-list)
                          :sample-count *scene-sample-count*
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled nil
                            :depth-compare :less))))
           (setf sampler
                 (create device
                         (make-sampler-descriptor
                          :label "luft presentation sampler"
                          :mag-filter :linear :min-filter :linear))
                 flame-depth-sampler
                 (make-renderer-flame-depth-sampler device)
                 composite-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft HDR composite source layout"
                          :entries '((:binding 0 :type :texture)
                                     (:binding 1 :type :sampler))))
                 sky-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft HDR sky layout"
                          :entries '((:binding 0 :type :uniform-buffer))))
                 sky-bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft HDR sky"
                          :layout sky-layout
                          :entries `((:binding 0 :resource ,camera-buffer))))
                 exposure-probe-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft exposure probe layout"
                          :entries '((:binding 0 :type :texture)
                                     (:binding 1 :type :sampler))))
                 exposure-probe-texture
                 (create device
                         (make-texture-descriptor
                          :label "luft exposure log luminance"
                          :size (list +exposure-probe-width+
                                      +exposure-probe-height+)
                          :dimensions :2d :format :rgba8-unorm
                          :usage '(:render-attachment :copy-src)))
                 exposure-probe-view
                 (create device
                         (make-texture-view-descriptor
                          :texture exposure-probe-texture))
                 exposure-probe-buffers
                 (coerce
                  (loop repeat +exposure-probe-buffer-count+
                        collect
                        (create device
                                (make-buffer-descriptor
                                 :label "luft exposure readback"
                                 :size +exposure-probe-byte-count+
                                 :usage '(:copy-dst))))
                  'vector))
           (setf present-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft presentation layout"
                          :entries '((:binding 0 :type :texture)
                                     (:binding 1 :type :sampler)
                                     (:binding 2 :type :texture)
                                     (:binding 3 :type :uniform-buffer))))
                 present-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft presentation vertex"
                          :language :mathematical
                          :code (shaders:present-vertex-specification)))
                 present-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft presentation fragment"
                          :language :mathematical
                          :code (shaders:present-fragment-specification)))
                 present-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft HDR presentation pipeline"
                          :layout present-layout
                          :vertex `(:module ,present-vertex-module)
                          :fragment `(:module ,present-fragment-module
                                      :targets ((:format ,color-format)))
                          :primitive '(:topology :triangle-list)))
                 composite-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft HDR composite copy fragment"
                          :language :mathematical
                          :code
                          (shaders::torch-flame-composite-copy-fragment-specification)))
                 composite-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft post-temporal HDR composite copy"
                          :layout composite-layout
                          :vertex `(:module ,present-vertex-module)
                          :fragment `(:module ,composite-fragment-module
                                      :targets ((:format :rgba16-float)))
                          :primitive '(:topology :triangle-list)))
                 sky-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft HDR sky fragment"
                          :language :mathematical
                          :code (if temporal-p
                                    (shaders:sky-temporal-fragment-specification)
                                    (shaders:sky-fragment-specification))))
                 sky-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft HDR sky pipeline"
                          :layout sky-layout
                          :vertex `(:module ,present-vertex-module)
                          :fragment `(:module ,sky-fragment-module
                                      :targets
                                      ,(mapcar (lambda (format)
                                                 `(:format ,format))
                                               target-formats))
                          :primitive '(:topology :triangle-list)
                          :sample-count *scene-sample-count*
                          ;; The sky is drawn inside the scene pass, whose
                          ;; depth attachment geometry subsequently owns.
                          ;; Match that pass without touching its depth.
                          :depth-stencil
                          '(:format :depth32-float
                            :depth-write-enabled nil
                            :depth-compare :always)))
                 exposure-probe-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft exposure probe fragment"
                          :language :mathematical
                          :code
                          (shaders:exposure-probe-fragment-specification)))
                 exposure-probe-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft exposure probe pipeline"
                          :layout exposure-probe-layout
                          :vertex `(:module ,present-vertex-module)
                          :fragment `(:module ,exposure-probe-fragment-module
                                      :targets ((:format :rgba8-unorm)))
                          :primitive '(:topology :triangle-list))))
           (when (eq temporal-kind :shader)
             (setf temporal-layout
                   (create
                    device
                    (make-bind-group-layout-descriptor
                     :label "luft temporal resolve layout"
                     :entries '((:binding 0 :type :texture)
                                (:binding 1 :type :texture)
                                (:binding 2 :type :texture)
                                (:binding 3 :type :sampler)
                                (:binding 4 :type :uniform-buffer))))
                   temporal-fragment-module
                   (create
                    device
                    (make-shader-module-descriptor
                     :label "luft temporal resolve fragment"
                     :language :mathematical
                     :code (shaders:temporal-resolve-fragment-specification)))
                   temporal-pipeline
                   (create
                    device
                    (make-render-pipeline-descriptor
                     :label "luft temporal resolve pipeline"
                     :layout temporal-layout
                     :vertex `(:module ,present-vertex-module)
                     :fragment `(:module ,temporal-fragment-module
                                 :targets ((:format :rgba16-float)))
                     :primitive '(:topology :triangle-list)))))
           (setf renderer
                 (make-instance 'renderer
                                :device device
                                :color-format color-format
                                :temporal-p temporal-p
                                :temporal-resolve-kind temporal-kind
                                :camera-buffer camera-buffer
                                :star-template-buffer star-template-buffer
                                :publication
                                (%make-empty-renderer-publication
                                 :flame-instance-buffer flame-instance-buffer
                                 :torch-body-bind-group torch-body-bind-group
                                 :torch-body-shadow-bind-group
                                 torch-body-shadow-bind-group)
                                :layout layout
                                :vertex-module vertex-module
                                :fragment-module fragment-module
                                :pipeline pipeline
                                :flame-effect-buffer flame-effect-buffer
                                :flame-layout flame-layout
                                :flame-vertex-module flame-vertex-module
                                :flame-fragment-module flame-fragment-module
                                :flame-pipeline flame-pipeline
                                :flame-depth-sampler flame-depth-sampler
                                :composite-layout composite-layout
                                :composite-fragment-module
                                composite-fragment-module
                                :composite-pipeline composite-pipeline
                                :torch-body-vertex-buffer
                                torch-body-vertex-buffer
                                :torch-body-layout torch-body-layout
                                :torch-body-vertex-module
                                torch-body-vertex-module
                                :torch-body-shadow-vertex-module
                                torch-body-shadow-vertex-module
                                :torch-body-fragment-module
                                torch-body-fragment-module
                                :torch-body-pipeline torch-body-pipeline
                                :torch-body-shadow-pipeline
                                torch-body-shadow-pipeline
                                :shadow-texture shadow-texture
                                :shadow-view shadow-view
                                :shadow-sampler shadow-sampler
                                :shadow-layout shadow-layout
                                :shadow-vertex-module shadow-vertex-module
                                :shadow-pipeline shadow-pipeline
                                :player-sdf-layout player-sdf-layout
                                :player-sdf-bind-group player-sdf-bind-group
                                :player-sdf-vertex-module player-sdf-vertex-module
                                :player-sdf-fragment-module
                                player-sdf-fragment-module
                                :player-sdf-pipeline player-sdf-pipeline
                                :lattice-point-layout lattice-point-layout
                                :lattice-point-vertex-module
                                lattice-point-vertex-module
                                :lattice-point-fragment-module
                                lattice-point-fragment-module
                                :lattice-point-pipeline lattice-point-pipeline))
           (setf (renderer-present-layout renderer) present-layout
                 (renderer-sampler renderer) sampler
                 (renderer-present-vertex-module renderer)
                 present-vertex-module
                 (renderer-present-fragment-module renderer)
                 present-fragment-module
                 (renderer-present-pipeline renderer) present-pipeline)
           (setf (renderer-temporal-layout renderer) temporal-layout
                 (renderer-temporal-fragment-module renderer)
                 temporal-fragment-module
                 (renderer-temporal-pipeline renderer) temporal-pipeline)
           (setf (renderer-sky-layout renderer) sky-layout
                 (renderer-sky-bind-group renderer) sky-bind-group
                 (renderer-sky-fragment-module renderer) sky-fragment-module
                 (renderer-sky-pipeline renderer) sky-pipeline
                 (renderer-exposure-probe-layout renderer)
                 exposure-probe-layout
                 (renderer-exposure-probe-texture renderer)
                 exposure-probe-texture
                 (renderer-exposure-probe-view renderer) exposure-probe-view
                 (renderer-exposure-probe-fragment-module renderer)
                 exposure-probe-fragment-module
                 (renderer-exposure-probe-pipeline renderer)
                 exposure-probe-pipeline
                 (renderer-exposure-probe-buffers renderer)
                 exposure-probe-buffers
                 (renderer-exposure-probe-submitted renderer)
                 (make-array +exposure-probe-buffer-count+
                             :element-type 'bit :initial-element 0)
                 (renderer-exposure-probe-frames renderer)
                 (make-array +exposure-probe-buffer-count+
                             :element-type '(unsigned-byte 64)
                             :initial-element 0))
           (create-frame-targets renderer extent)
           (setf completed-p t)
           renderer)
      (unless completed-p
        (if renderer
            (destroy-renderer renderer)
            (dolist (resource
                      (append
                       (and exposure-probe-buffers
                            (coerce exposure-probe-buffers 'list))
                       (list temporal-pipeline temporal-fragment-module
                                        temporal-layout
                                        present-pipeline present-fragment-module
                                        composite-pipeline
                                        composite-fragment-module
                                        present-vertex-module sampler
                                        flame-depth-sampler composite-layout
                                        present-bind-group sky-pipeline
                                        sky-fragment-module sky-bind-group
                                        sky-layout exposure-probe-bind-group
                                        exposure-probe-pipeline
                                        exposure-probe-fragment-module
                                        exposure-probe-view exposure-probe-texture
                                        exposure-probe-layout
                                        present-layout lattice-point-pipeline
                                        lattice-point-fragment-module
                                        lattice-point-vertex-module
                                        lattice-point-layout
                                        player-sdf-bind-group player-sdf-pipeline
                                        player-sdf-fragment-module
                                        player-sdf-vertex-module player-sdf-layout
                                        torch-body-shadow-bind-group
                                        torch-body-bind-group
                                        torch-body-shadow-pipeline
                                        torch-body-pipeline
                                        torch-body-fragment-module
                                        torch-body-shadow-vertex-module
                                        torch-body-vertex-module
                                        torch-body-layout
                                        torch-body-vertex-buffer
                                        flame-pipeline
                                        flame-fragment-module flame-vertex-module
                                        flame-layout flame-effect-buffer
                                        flame-instance-buffer
                                        shadow-pipeline shadow-vertex-module
                                        shadow-layout shadow-sampler shadow-view
                                        shadow-texture
                                        pipeline fragment-module
                                        vertex-module layout star-template-buffer
                                        camera-buffer)))
              (when resource (ignore-errors (destroy resource)))))))))

(defun draw-resident-opaque-population (pass resident bind-group)
  "Dispatch one direct mesh workgroup per active lattice site."
  (let* ((population (resident-population-population resident))
         (workgroup-count
           (render-population-mesh-workgroup-count population)))
    (when (plusp workgroup-count)
      (set-bind-group pass 0 bind-group)
      (draw-mesh-workgroups pass workgroup-count))))

(defun exposure-probe-average-luminance (bytes)
  "Decode the geometric-mean luminance encoded by the 32x16 GPU probe."
  (unless (= (length bytes) +exposure-probe-byte-count+)
    (error "LUFT exposure probe returned ~D bytes, expected ~D."
           (length bytes) +exposure-probe-byte-count+))
  (let ((sum 0d0))
    (loop for index from 0 below (length bytes) by 4
          do (incf sum (aref bytes index)))
    (let* ((count (* +exposure-probe-width+ +exposure-probe-height+))
           (encoded (/ sum (* count 255d0)))
           (average-log (- (* encoded 11.98293d0) 9.21034d0)))
      (exp average-log))))

(defun adapted-exposure (current average-luminance)
  "Take one Moppe-style asymmetric eye-adaptation step."
  (let* ((target (max 0.55f0
                      (min 1.9f0
                           (/ 0.16f0 (coerce average-luminance
                                            'single-float)))))
         (rate (if (< target current) 0.10f0 0.04f0)))
    (+ current (* (- target current) rate))))

(defun maintain-renderer-exposure (renderer)
  "Consume the oldest completed probe without waiting for newer GPU work."
  (let ((buffers (renderer-exposure-probe-buffers renderer))
        (submitted (renderer-exposure-probe-submitted renderer))
        (frames (renderer-exposure-probe-frames renderer))
        (oldest nil))
    ;; A live DEFCLASS update can add this chronology lane to an existing
    ;; renderer between frames. Preserve its in-flight bits and give those
    ;; older probes one common age until the next transactional refresh.
    (unless (= (length frames) (length buffers))
      (setf frames
            (make-array (length buffers) :element-type '(unsigned-byte 64)
                                         :initial-element
                                         (renderer-frame-index renderer))
            (renderer-exposure-probe-frames renderer) frames))
    (dotimes (index (length buffers))
      (when (and (= 1 (aref submitted index))
                 (or (null oldest)
                     (< (aref frames index) (aref frames oldest))))
        (setf oldest index)))
    (when oldest
      (multiple-value-bind (bytes ready-p)
          (read-buffer-if-ready (aref buffers oldest))
        (when ready-p
          ;; One adaptation step per rendered frame keeps a CPU pause from
          ;; collapsing several delayed measurements into one visible jump.
          (setf (aref submitted oldest) 0
                (renderer-exposure renderer)
                (adapted-exposure
                 (renderer-exposure renderer)
                 (exposure-probe-average-luminance bytes)))))))
  (renderer-exposure renderer))

(defun encode-exposure-probe (renderer encoder)
  "Reduce committed post-temporal HDR radiance and queue one readback."
  (let* ((index (mod (renderer-frame-index renderer)
                     +exposure-probe-buffer-count+))
         (submitted (renderer-exposure-probe-submitted renderer)))
    ;; If the GPU is more than three frames behind, keep rendering and retain
    ;; the last exposure instead of overwriting an in-flight measurement.
    (when (zerop (aref submitted index))
      (let ((pass
              (begin-render-pass
               encoder
               (make-render-pass-descriptor
                :label "luft exposure probe"
                :color-attachments
                `((:view ,(renderer-exposure-probe-view renderer)
                   :load-op :clear :store-op :store
                   :clear-value #(0.0 0.0 0.0 1.0)))))))
        (set-pipeline pass (renderer-exposure-probe-pipeline renderer))
        (set-bind-group pass 0
                        (renderer-exposure-probe-bind-group renderer))
        (draw pass 3)
        (end-pass pass))
      (encode encoder
              (make-gpu-copy-texture-to-buffer-command
               :source (renderer-exposure-probe-texture renderer)
               :destination
               (aref (renderer-exposure-probe-buffers renderer) index)))
      (setf (aref submitted index) 1
            (aref (renderer-exposure-probe-frames renderer) index)
            (renderer-frame-index renderer)))))

(defun encode-renderer-frame
    (renderer frame encoder surface-texture extent camera-uniform-data
     &key jitter view player-p construction-p overlay-encoder
       (effect-time
         (or *flame-time* (/ (renderer-frame-index renderer) 60.0))))
  (ensure-renderer-extent renderer extent)
  (when (renderer-shader-temporal-p renderer)
    ;; These W components are padding to every geometry consumer.  The Vulkan
    ;; resolve reads them as its per-frame validity and accumulation weight.
    (setf (aref camera-uniform-data 27)
          (if (renderer-history-valid-p renderer) 1.0f0 0.0f0)
          (aref camera-uniform-data 31) *vulkan-temporal-history-weight*))
  (write-buffer (renderer-frame-state-camera-buffer frame) camera-uniform-data)
  (check-type effect-time real)
  (write-buffer
   (renderer-frame-state-flame-effect-buffer frame)
   (torch-flame-effect-uniform-data (coerce effect-time 'single-float)))
  (let ((shadow-pass
            (begin-render-pass
             encoder
             (make-render-pass-descriptor
              :label "luft sun shadow"
              :color-attachments nil
              :depth-stencil-attachment
              `(:view ,(renderer-shadow-view renderer)
                :depth-load-op :clear :depth-store-op :store
                :depth-clear-value 1.0)))))
      (set-pipeline shadow-pass (renderer-shadow-pipeline renderer))
      (dolist (key (renderer-slot-order renderer))
        (let ((resident
                (mesh-slot-resident
                 (gethash key (renderer-mesh-slots renderer)))))
          (draw-resident-opaque-population
           shadow-pass resident
           (renderer-frame-resident-bind-group
            renderer frame resident t))))
      (when (plusp (renderer-flame-instance-count renderer))
        (set-pipeline shadow-pass
                      (renderer-torch-body-shadow-pipeline renderer))
        (set-bind-group shadow-pass 0
                        (renderer-frame-torch-body-bind-group
                         renderer frame t))
        (draw shadow-pass (torch-body-vertex-count)
              (renderer-flame-instance-count renderer)))
      (end-pass shadow-pass))
  (prepare-texture encoder (renderer-shadow-texture renderer)
                   :texture-binding)
  (let* ((temporal-p (renderer-temporal-p renderer))
         (color-view (renderer-scene-msaa-view renderer))
         (color-attachments
           (if temporal-p
               `((:view ,color-view
                  :resolve-view ,(renderer-scene-view renderer)
                  :load-op :clear :store-op :store
                  :clear-value #(0.0 0.0 0.0 1.0))
                 (:view ,(renderer-motion-msaa-view renderer)
                  :resolve-view ,(renderer-motion-view renderer)
                  :load-op :clear :store-op :store
                  :clear-value #(0.0 0.0 0.0 0.0)))
               `((:view ,color-view
                  :resolve-view ,(renderer-scene-view renderer)
                  :load-op :clear :store-op :store
                  :clear-value #(0.0 0.0 0.0 1.0)))))
         (pass
           (begin-render-pass
            encoder
            (make-render-pass-descriptor
             :label "luft site streams"
             :color-attachments color-attachments
             :depth-stencil-attachment
             `(:view ,(renderer-depth-msaa-view renderer)
               :resolve-view ,(renderer-depth-view renderer)
               :depth-load-op :clear
               :depth-store-op :store
               :depth-clear-value 1.0)))))
    ;; The atmosphere is scene-linear world radiance: geometry overwrites it,
    ;; the selected temporal implementation reconstructs it, and the exposure
    ;; probe meters the same pixels presentation will grade.
    (set-pipeline pass (renderer-sky-pipeline renderer))
    (set-bind-group pass 0 (renderer-frame-sky-bind-group renderer frame))
    (draw pass 3)
    (set-pipeline pass (renderer-pipeline renderer))
    (dolist (key (renderer-slot-order renderer))
      (let ((resident
              (mesh-slot-resident
               (gethash key (renderer-mesh-slots renderer)))))
        (draw-resident-opaque-population
         pass resident
         (renderer-frame-resident-bind-group renderer frame resident nil))))
    (when (plusp (renderer-flame-instance-count renderer))
      (set-pipeline pass (renderer-torch-body-pipeline renderer))
      (set-bind-group pass 0
                      (renderer-frame-torch-body-bind-group renderer frame nil))
      (draw pass (torch-body-vertex-count)
            (renderer-flame-instance-count renderer)))
    (when player-p
      (set-pipeline pass (renderer-player-sdf-pipeline renderer))
      (set-bind-group pass 0 (renderer-frame-player-bind-group renderer frame))
      (draw pass 6 2))
    (when construction-p
      ;; Populate at most one diagnostic slot per frame. The overlay is a
      ;; debugging view, so progressive readiness is preferable to freezing
      ;; one frame while every resident chunk is scanned.
      (loop for key in (renderer-slot-order renderer)
            for slot = (gethash key (renderer-mesh-slots renderer))
            unless (mesh-slot-lattice-point-buffer slot)
              do (ensure-mesh-slot-lattice-points renderer slot)
                 (return))
      (set-pipeline pass (renderer-lattice-point-pipeline renderer))
      (dolist (key (renderer-slot-order renderer))
        (let ((slot (gethash key (renderer-mesh-slots renderer))))
          (when (plusp (mesh-slot-lattice-point-count slot))
            (set-bind-group pass 0
                            (renderer-frame-lattice-bind-group
                             renderer frame slot))
            (draw pass 6 (mesh-slot-lattice-point-count slot))))))
    (when (renderer-metalfx-temporal-p renderer)
      (signal-temporal-scaler-inputs pass
                                     (renderer-temporal-scaler renderer)))
    (end-pass pass)
    (when (renderer-metalfx-temporal-p renderer)
      (let ((scaler (renderer-temporal-scaler renderer))
            (history-valid-p (renderer-history-valid-p renderer))
            (render-extent (renderer-render-extent renderer)))
        (encode-temporal-scale
         encoder scaler
         (renderer-scene-texture renderer)
         (renderer-depth-texture renderer)
         (renderer-motion-texture renderer)
         (renderer-resolved-texture renderer)
         ;; JITTER is clip-space at the internal scene resolution.  MetalFX
         ;; takes the same offset in input pixels—not output pixels—so using
         ;; EXTENT here overstates it whenever temporal upscaling is active.
         (vector (* 0.5 (first render-extent) (aref jitter 0))
                 (* 0.5 (second render-extent) (aref jitter 1)))
         (not history-valid-p))
        (setf (renderer-previous-view renderer) view
              (renderer-history-valid-p renderer) t
              (renderer-history-used-p renderer) history-valid-p)))
    (when (renderer-shader-temporal-p renderer)
      (let ((history-valid-p (renderer-history-valid-p renderer)))
        (prepare-texture encoder (renderer-scene-texture renderer)
                         :texture-binding)
        (prepare-texture encoder (renderer-motion-texture renderer)
                         :texture-binding)
        (unless history-valid-p
          (encode encoder
                  (make-gpu-clear-texture-command
                   :texture (renderer-history-texture renderer)
                   :color #(0.0 0.0 0.0 0.0))))
        (prepare-texture encoder (renderer-history-texture renderer)
                         :texture-binding)
        (let ((resolve-pass
                (begin-render-pass
                 encoder
                 (make-render-pass-descriptor
                  :label "luft temporal resolve"
                  :color-attachments
                  `((:view ,(renderer-resolved-view renderer)
                     :load-op :clear :store-op :store
                     :clear-value #(0.0 0.0 0.0 1.0)))))))
          (set-pipeline resolve-pass (renderer-temporal-pipeline renderer))
          (set-bind-group resolve-pass 0
                          (renderer-frame-temporal-bind-group renderer frame))
          (draw resolve-pass 3)
          (end-pass resolve-pass))
        ;; One explicit full-resolution history keeps the extent cohort small:
        ;; resolve never reads and writes the same image, and the completed
        ;; result becomes next frame's input only after the render pass ends.
        (encode encoder
                (make-gpu-copy-texture-command
                 :source (renderer-resolved-texture renderer)
                 :destination (renderer-history-texture renderer)))
        (prepare-texture encoder (renderer-resolved-texture renderer)
                         :texture-binding)
        (prepare-texture encoder (renderer-history-texture renderer)
                         :texture-binding)
        (setf (renderer-previous-view renderer) view
              (renderer-history-valid-p renderer) t
              (renderer-history-used-p renderer) history-valid-p)))
    (unless temporal-p
      (prepare-texture encoder (renderer-scene-texture renderer)
                       :texture-binding))
    ;; Depth is read only after both the scene pass and the temporal encoder
    ;; have consumed it.  It is deliberately not attached to the following
    ;; pass, so sampling it while writing the distinct HDR composite cannot
    ;; form a Metal or Vulkan read/write texture hazard.
    (prepare-texture encoder (renderer-depth-texture renderer)
                     :texture-binding)
    (let ((composite-pass
            (begin-render-pass
             encoder
             (make-render-pass-descriptor
              :label "luft post-temporal HDR flame composite"
              :color-attachments
              `((:view ,(renderer-composite-view renderer)
                 :load-op :clear :store-op :store
                 :clear-value #(0.0 0.0 0.0 1.0)))))))
      (when (renderer-metalfx-temporal-p renderer)
        (wait-temporal-scaler-output
         composite-pass (renderer-temporal-scaler renderer)))
      (set-pipeline composite-pass (renderer-composite-pipeline renderer))
      (set-bind-group composite-pass 0
                      (renderer-composite-source-bind-group renderer))
      (draw composite-pass 3)
      (when (plusp (renderer-flame-instance-count renderer))
        (set-pipeline composite-pass (renderer-flame-pipeline renderer))
        (set-bind-group composite-pass 0
                        (renderer-frame-flame-bind-group renderer frame))
        (draw composite-pass 6 (renderer-flame-instance-count renderer)))
      (end-pass composite-pass))
    (prepare-texture encoder (renderer-composite-texture renderer)
                     :texture-binding)
    (encode-exposure-probe renderer encoder)
    (let ((present-pass
            (begin-render-pass
             encoder
             (make-render-pass-descriptor
              :label "luft HDR presentation"
              :color-attachments
              `((:view ,surface-texture :load-op :clear :store-op :store
                 :clear-value #(0.0 0.0 0.0 1.0)))))))
      (set-pipeline present-pass (renderer-present-pipeline renderer))
      (set-bind-group present-pass 0
                      (renderer-frame-present-bind-group renderer frame))
      (draw present-pass 3)
      ;; Both MetalFX and the direct HDR path publish here.  The atelier
      ;; overlay remains later than tone mapping and glow in either case.
      (when overlay-encoder
        (funcall overlay-encoder present-pass))
      (end-pass present-pass))
    ;; Character motion is presentation time, not a MetalFX capability.
    (incf (renderer-frame-index renderer)))
  renderer)

(defun destroy-renderer (renderer)
  (destroy-canvas-frame-resource-cache
   (renderer-frame-resources renderer) #'destroy-renderer-frame-state)
  (destroy-renderer-targets renderer)
  (loop for slot being the hash-values of (renderer-mesh-slots renderer)
        do (%destroy-mesh-slot slot))
  (destroy-renderer-publication-resources (renderer-publication renderer))
  (dolist (resource
            (append
             (coerce (renderer-exposure-probe-buffers renderer) 'list)
             (list (renderer-exposure-probe-pipeline renderer)
                  (renderer-exposure-probe-fragment-module renderer)
                  (renderer-exposure-probe-view renderer)
                  (renderer-exposure-probe-texture renderer)
                  (renderer-exposure-probe-layout renderer)
                  (renderer-sky-pipeline renderer)
                  (renderer-sky-fragment-module renderer)
                  (renderer-sky-bind-group renderer)
                  (renderer-sky-layout renderer)
                  (renderer-present-pipeline renderer)
                  (renderer-present-fragment-module renderer)
                  (renderer-temporal-pipeline renderer)
                  (renderer-temporal-fragment-module renderer)
                  (renderer-temporal-layout renderer)
                  (renderer-composite-pipeline renderer)
                  (renderer-composite-fragment-module renderer)
                  (renderer-present-vertex-module renderer)
                  (renderer-sampler renderer)
                  (renderer-flame-depth-sampler renderer)
                  (renderer-composite-layout renderer)
                  (renderer-present-layout renderer)
                  (renderer-lattice-point-pipeline renderer)
                  (renderer-lattice-point-fragment-module renderer)
                  (renderer-lattice-point-vertex-module renderer)
                  (renderer-lattice-point-layout renderer)
                  (renderer-torch-body-shadow-pipeline renderer)
                  (renderer-torch-body-pipeline renderer)
                  (renderer-torch-body-fragment-module renderer)
                  (renderer-torch-body-shadow-vertex-module renderer)
                  (renderer-torch-body-vertex-module renderer)
                  (renderer-torch-body-layout renderer)
                  (renderer-torch-body-vertex-buffer renderer)
                  (renderer-flame-pipeline renderer)
                  (renderer-flame-fragment-module renderer)
                  (renderer-flame-vertex-module renderer)
                  (renderer-flame-layout renderer)
                  (renderer-flame-effect-buffer renderer)
                  (renderer-player-sdf-bind-group renderer)
                  (renderer-player-sdf-pipeline renderer)
                  (renderer-player-sdf-fragment-module renderer)
                  (renderer-player-sdf-vertex-module renderer)
                  (renderer-player-sdf-layout renderer)
                  (renderer-shadow-pipeline renderer)
                  (renderer-shadow-vertex-module renderer)
                  (renderer-shadow-layout renderer)
                  (renderer-shadow-sampler renderer)
                  (renderer-shadow-view renderer)
                  (renderer-shadow-texture renderer)
                  (renderer-star-template-buffer renderer)
                  (renderer-pipeline renderer) (renderer-fragment-module renderer)
                  (renderer-vertex-module renderer)
                  (renderer-layout renderer)
                  (and (slot-boundp renderer 'camera-buffer)
                       (renderer-camera-buffer renderer)))))
    (when resource (ignore-errors (destroy resource))))
  (setf (renderer-present-pipeline renderer) nil
        (renderer-exposure-probe-pipeline renderer) nil
        (renderer-exposure-probe-fragment-module renderer) nil
        (renderer-exposure-probe-view renderer) nil
        (renderer-exposure-probe-texture renderer) nil
        (renderer-exposure-probe-layout renderer) nil
        (renderer-exposure-probe-buffers renderer) #()
        (renderer-exposure-probe-submitted renderer)
        (make-array 0 :element-type 'bit)
        (renderer-exposure-probe-frames renderer) #()
        (renderer-sky-pipeline renderer) nil
        (renderer-sky-fragment-module renderer) nil
        (renderer-sky-bind-group renderer) nil
        (renderer-sky-layout renderer) nil
        (renderer-present-fragment-module renderer) nil
        (renderer-temporal-pipeline renderer) nil
        (renderer-temporal-fragment-module renderer) nil
        (renderer-temporal-layout renderer) nil
        (renderer-composite-pipeline renderer) nil
        (renderer-composite-fragment-module renderer) nil
        (renderer-present-vertex-module renderer) nil
        (renderer-sampler renderer) nil
        (renderer-flame-depth-sampler renderer) nil
        (renderer-composite-layout renderer) nil
        (renderer-present-layout renderer) nil
        (renderer-lattice-point-pipeline renderer) nil
        (renderer-lattice-point-fragment-module renderer) nil
        (renderer-lattice-point-vertex-module renderer) nil
        (renderer-lattice-point-layout renderer) nil
        (renderer-publication renderer) (%make-empty-renderer-publication)
        (renderer-torch-body-shadow-pipeline renderer) nil
        (renderer-torch-body-pipeline renderer) nil
        (renderer-torch-body-fragment-module renderer) nil
        (renderer-torch-body-shadow-vertex-module renderer) nil
        (renderer-torch-body-vertex-module renderer) nil
        (renderer-torch-body-layout renderer) nil
        (renderer-torch-body-vertex-buffer renderer) nil
        (renderer-flame-pipeline renderer) nil
        (renderer-flame-fragment-module renderer) nil
        (renderer-flame-vertex-module renderer) nil
        (renderer-flame-layout renderer) nil
        (renderer-flame-effect-buffer renderer) nil
        (renderer-player-sdf-bind-group renderer) nil
        (renderer-player-sdf-pipeline renderer) nil
        (renderer-player-sdf-fragment-module renderer) nil
        (renderer-player-sdf-vertex-module renderer) nil
        (renderer-player-sdf-layout renderer) nil
        (renderer-shadow-pipeline renderer) nil
        (renderer-shadow-vertex-module renderer) nil
        (renderer-shadow-layout renderer) nil
        (renderer-shadow-sampler renderer) nil
        (renderer-shadow-view renderer) nil
        (renderer-shadow-texture renderer) nil
        (renderer-star-template-buffer renderer) nil
        (renderer-pipeline renderer) nil
        (renderer-fragment-module renderer) nil
        (renderer-vertex-module renderer) nil
        (renderer-layout renderer) nil
        (renderer-camera-buffer renderer) nil)
  (values))

;;; ---------------------------------------------------------------------------
;;; Streaming chunk scenes
;;;
;;; A finite fixture may still split an ordinary authored scene into chunks.
;;; Ordinary play instead retains a deterministic source and a sparse overlay;
;;; immutable resident values are produced on the worker and may be discarded.
;;; A bounded square window follows the camera. Each focus change installs the
;;; final desired residency first, then remeshes exactly the chunks whose 3 by 3
;;; dependency neighborhoods changed. MESH-CHUNK's probes into non-resident
;;; neighbors signal MISSING-CHUNK. Demand worlds capture a complete guard and
;;; reject an unknown probe rather than silently turning nonresidency into air.
;;; The canvas owner publishes replacements and departures as one complete
;;; cohort, so no frame observes a mixed seam generation.

(defconstant +large-world-horizontal-bits+ 11)
(defconstant +large-world-seed+ 121)

(defclass authored-world-source ()
  ((domain :initarg :domain :reader authored-world-source-domain)
   (seed :initarg :seed :initform +large-world-seed+
         :reader authored-world-source-seed)
   ;; Presence is meaningful: NIL is an authored removal, while absence means
   ;; that the deterministic source still owns the cell.
   (edits :initform (make-hash-table :test #'eql)
          :reader authored-world-source-edits))
  (:documentation
   "The canonical large-world description and its sparse semantic edits."))

(defstruct (resident-cell-chunk
             (:constructor %make-resident-cell-chunk
                 (key incarnation chain material-cells))
             (:copier nil))
  "One immutable, evictable materialization of an authored source chunk."
  (key 0 :type luft:chunk-key :read-only t)
  (incarnation 0 :type (integer 1 *) :read-only t)
  (chain nil :type luft:chain :read-only t)
  (material-cells nil :type hash-table :read-only t))

(defun large-world-road-centre-y (x)
  "Authored west-to-east route from the old sanctuary spawn to the citadel."
  (+ 48.0d0 (* 0.418d0 (- x 64))
     (* 18.0d0 (sin (/ (- x 64) 173.0d0)))))

(defun large-world-river-centre-x (y)
  "Authored north-to-south river course, independent of chunk partitioning."
  (+ 612.0d0 (* 58.0d0 (sin (/ y 149.0d0)))
     (* 17.0d0 (sin (/ y 43.0d0)))))

(defun large-world-road-height (x)
  (+ 15 (round (* 0.004d0 x))))

(defun large-world-terrain-height (source x y)
  "Deterministic composed terrain under the route, river, pass, and citadel."
  (let* ((seed (authored-world-source-seed source))
         (detail (+ (* 3.2d0 (landscape-value-noise x y 97 seed 2))
                    (* 1.5d0 (landscape-value-noise x y 31 seed 3))))
         (road-y (large-world-road-centre-y x))
         (road-distance (abs (- y road-y)))
         (river-x (large-world-river-centre-x y))
         (river-distance (abs (- x river-x)))
         (ridge-distance (abs (- x 970.0d0)))
         (pass-distance (abs (- y (large-world-road-centre-y 970))))
         (ridge (* 31.0d0
                   (max 0.0d0 (- 1.0d0 (/ ridge-distance 260.0d0)))
                   (min 1.0d0 (/ pass-distance 95.0d0))))
         (highlands (* 9.0d0
                       (max 0.0d0
                            (landscape-value-noise x y 311 seed 11))))
         (natural (+ 17.0d0 detail ridge highlands))
         (river-bed (- natural
                       (* 12.0d0
                          (expt (max 0.0d0
                                     (- 1.0d0 (/ river-distance 18.0d0)))
                                2))))
         (road-height (large-world-road-height x))
         (road-blend (max 0.0d0 (- 1.0d0 (/ road-distance 7.0d0))))
         (routed (interpolate-landscape-reading
                  river-bed road-height road-blend))
         (citadel-distance
           (max (abs (- x 1500.0d0)) (abs (- y 650.0d0))))
         (citadel-blend (max 0.0d0 (- 1.0d0 (/ citadel-distance 52.0d0)))))
    (max 3 (min 92
                (round (interpolate-landscape-reading
                        routed 23.0d0 citadel-blend))))))

(defun large-world-citadel-cell-p (x y z)
  "Whether X/Y/Z is authored limestone in the eastern destination."
  (let* ((dx (- x 1500))
         (dy (- y 650))
         (square-distance (max (abs dx) (abs dy)))
         (corner-distance
           (min (sqrt (+ (expt (- dx 34) 2) (expt (- dy 34) 2)))
                (sqrt (+ (expt (+ dx 34) 2) (expt (- dy 34) 2)))
                (sqrt (+ (expt (- dx 34) 2) (expt (+ dy 34) 2)))
                (sqrt (+ (expt (+ dx 34) 2) (expt (+ dy 34) 2)))))
         (gate-p (and (< dx -32) (<= (abs dy) 3) (<= z 29))))
    (or
     ;; Long curtain walls, with an open road gate on the west.
     (and (<= 34 square-distance 38) (<= 24 z 34) (not gate-p))
     ;; Four round towers break the square silhouette.
     (and (<= corner-distance 8.0d0) (<= 24 z 40))
     ;; A keep and stair-stepped beacon at the destination.
     (and (<= 8 dx 26) (<= (abs dy) 13) (<= 24 z 38)
          (or (<= (abs dy) 9) (<= z 31)))
     (and (<= 13 dx 21) (<= (abs dy) 5) (<= 39 z 47))
     ;; Sparse crenels remain ordinary cells.
     (and (<= 34 square-distance 38) (= z 36)
          (evenp (+ x y))))))

(defun large-world-base-placement
    (source x y z &key (height (large-world-terrain-height source x y)))
  "Return the source-owned placement at one cell, or NIL for authored air."
  (let* ((top (1- height))
         (road-p (<= (abs (- y (large-world-road-centre-y x))) 3.0d0))
         (river-p
           (<= (abs (- x (large-world-river-centre-x y))) 8.0d0)))
    (cond
      ((large-world-citadel-cell-p x y z)
       *sanctuary-material-placement*)
      ((>= z height) nil)
      ((and (= z top) road-p) *sanctuary-material-placement*)
      ((and (= z top) river-p) *highland-rock-material-placement*)
      ((and (>= z (- top 3))
            (or (> height 31)
                (>= (abs (- height
                            (large-world-terrain-height source (1+ x) y)))
                    2)))
       *highland-rock-material-placement*)
      (t *terrain-material-placement*))))

(defun authored-world-edit-at (source cell)
  "Return a sparse edited placement and whether SOURCE owns an edit at CELL."
  (gethash cell (authored-world-source-edits source)))

(defun capture-authored-world-chunk-edits (source key)
  "Copy the sparse edits belonging to KEY for immutable worker use."
  (let ((edits nil))
    (maphash
     (lambda (cell placement)
       (when (= key (luft:site-chunk-key cell))
         (push (cons cell placement) edits)))
     (authored-world-source-edits source))
    edits))

(defun materialize-authored-world-chunk (source key incarnation &key edits)
  "Build KEY bit-identically from SOURCE and an immutable sparse edit capture."
  (check-type source authored-world-source)
  (let* ((domain (authored-world-source-domain source))
         (x0 (luft:chunk-origin-x key))
         (y0 (luft:chunk-origin-y key))
         (x1 (min (+ x0 luft:+chunk-size+)
                  (luft:world-domain-x-limit domain)))
         (y1 (min (+ y0 luft:+chunk-size+)
                  (luft:world-domain-y-limit domain)))
         (vocabulary (make-scene-material-vocabulary))
         (terrain-offset
           (domains:identity-vocabulary-offset
            vocabulary *terrain-material-placement*))
         (rock-offset
           (domains:identity-vocabulary-offset
            vocabulary *highland-rock-material-placement*))
         (limestone-offset
           (domains:identity-vocabulary-offset
            vocabulary *sanctuary-material-placement*))
         (materials (make-hash-table :test #'eql :size 100000)))
    (loop for x from x0 below x1 do
      (loop for y from y0 below y1 do
        (let* ((height (large-world-terrain-height source x y))
               (top (1- height))
               (road-p
                 (<= (abs (- y (large-world-road-centre-y x))) 3.0d0))
               (river-p
                 (<= (abs (- x (large-world-river-centre-x y))) 8.0d0))
               (rock-p
                 (or (> height 31)
                     (>= (abs (- height
                                 (large-world-terrain-height source (1+ x) y)))
                         2))))
          (dotimes (z height)
            (setf (gethash
                   (luft:make-site domain x y z luft:+cell-extent+ 1)
                   materials)
                  (cond ((and (= z top) road-p) limestone-offset)
                        ((and (= z top) river-p) rock-offset)
                        ((and rock-p (>= z (- top 3))) rock-offset)
                        (t terrain-offset))))
          (when (and (<= (abs (- x 1500)) 45)
                     (<= (abs (- y 650)) 45))
            (loop for z from 24 to 47
                  when (large-world-citadel-cell-p x y z)
                    do (setf (gethash
                              (luft:make-site
                               domain x y z luft:+cell-extent+ 1)
                              materials)
                             limestone-offset))))))
    (dolist (edit edits)
      (if (cdr edit)
          (setf (gethash (car edit) materials)
                (domains:identity-vocabulary-offset vocabulary (cdr edit)))
          (remhash (car edit) materials)))
    (let ((builder
            (luft:make-chain-builder
             domain :initial-capacity (hash-table-count materials))))
      (maphash (lambda (cell offset)
                 (declare (ignore offset))
                 (luft:chain-builder-add-site builder cell))
               materials)
      (%make-resident-cell-chunk
       key incarnation (luft:finish-chain-builder builder) materials))))

(defclass authored-chunk-load-request (production:production-request)
  ((scene :initarg :scene :reader authored-chunk-load-request-scene)
   (source :initarg :source :reader authored-chunk-load-request-source)
   (chunk-key :initarg :chunk-key :reader authored-chunk-load-request-chunk-key)
   (demand-token :initarg :demand-token
                 :reader authored-chunk-load-request-demand-token)
   (incarnation :initarg :incarnation
                :reader authored-chunk-load-request-incarnation)
   (edits :initarg :edits :reader authored-chunk-load-request-edits)))

(defmethod production:perform-production-request
    ((request authored-chunk-load-request))
  (materialize-authored-world-chunk
   (authored-chunk-load-request-source request)
   (authored-chunk-load-request-chunk-key request)
   (authored-chunk-load-request-incarnation request)
   :edits (authored-chunk-load-request-edits request)))

(defclass streaming-scene (scene)
  ((source :initarg :source :initform nil :reader streaming-scene-source)
   (store :initform (make-hash-table :test #'eql)
          :reader streaming-scene-store)
   (desired :initform (make-hash-table :test #'eql)
            :reader streaming-scene-desired)
   (load-outstanding :initform (make-hash-table :test #'eql)
                     :reader streaming-scene-load-outstanding)
   (next-demand-token :initform 0
                      :accessor streaming-scene-next-demand-token)
   (next-incarnation :initform 0
                     :accessor streaming-scene-next-incarnation)
   (loaded :initform (make-hash-table :test #'eql)
           :reader streaming-scene-loaded)
   (outstanding :initform (make-hash-table :test #'eql)
                :reader streaming-scene-outstanding)
   (staged :initform (make-hash-table :test #'eql)
           :reader streaming-scene-staged)
   (staged-generation :initform nil
                      :accessor streaming-scene-staged-generation)
   (cohort :initform nil :accessor streaming-scene-cohort)
   (removals :initform nil :accessor streaming-scene-removals)
   (production-errors :initform nil
                      :accessor streaming-scene-production-errors)
   (frames-per-load :initarg :frames-per-load :initform 15
                    :accessor streaming-scene-frames-per-load)
   (residency-radius :initarg :residency-radius :initform 1
                     :accessor streaming-scene-residency-radius)
   (focus :initform nil :accessor streaming-scene-focus)
   (light-generation
    :initarg :light-generation
    :accessor streaming-scene-light-generation)
   (frame-counter :initform 0 :accessor streaming-scene-frame-counter)))

(defstruct (streaming-mesh-snapshot
             (:constructor %make-streaming-mesh-snapshot
                 (scene input-scene output-keys witness-keys resident-source-keys
                  bevel-width union-neighborhood stamp
                  realize-torch-light-p reusable-light-generation)))
  "Immutable CPU input for one dependency-closed regional mesh request."
  (scene nil :read-only t)
  ;; The owning streaming scene remains mutable on the canvas thread.  Workers
  ;; borrow this frozen scene value so a later edit cannot mix new materials or
  ;; light with the snapshot's old occupancy chains.
  (input-scene nil :type scene :read-only t)
  (output-keys nil :type list :read-only t)
  (witness-keys nil :type list :read-only t)
  ;; Logical authored residency is deliberately distinct from OUTPUT-KEYS:
  ;; the latter includes virtual canonical owners needed for closed geometry.
  (resident-source-keys nil :type list :read-only t)
  (bevel-width luft:+mesh-bevel-width+ :read-only t)
  ;; Occupancy phase is not topology.  A snapshot therefore captures exactly
  ;; one mixed-material union; render-population compilation classifies its
  ;; finished instances into opaque and translucent passes later.
  (union-neighborhood nil :type hash-table :read-only t)
  (stamp nil :read-only t)
  (realize-torch-light-p t :type boolean :read-only t)
  (reusable-light-generation nil
                             :type (or null realized-light-generation)
                             :read-only t))

(defclass streaming-mesh-request (production:production-request)
  ((snapshot :initarg :snapshot :reader streaming-mesh-request-snapshot)))

(defstruct (streaming-mesh-result
             (:constructor %make-streaming-mesh-result (meshes generation))
             (:copier nil))
  "One worker-complete prepared owner cohort and its exact light generation."
  (meshes nil :type list :read-only t)
  (generation nil :type scene-mesh-generation :read-only t))

(defun make-streaming-scene
    (scene &key (frames-per-load 15) (residency-radius 1))
  "Wrap SCENE in bounded camera-driven chunk residency.

Every resident owner uses the same uniform width, or participates in one
cohort-compiled material site policy. Distance-varying geometry was removed
because independently selected chunk widths open their shared lattice sites;
a future LoD must bring an explicit transition representation."
  (let ((streaming (make-instance
                    'streaming-scene
                    :solid (scene-solid scene)
                    :material-vocabulary (scene-material-vocabulary scene)
                    :material-cells (scene-material-cells scene)
                    :authored-light-sources
                    (scene-authored-light-sources scene)
                    :authored-light-opacity-table
                    (scene-authored-light-opacity-table scene)
                    :authored-light-revision
                    (scene-authored-light-revision scene)
                    :authored-light-provenance
                    (scene-authored-light-provenance scene)
                    :authored-light-generation
                    (scene-authored-light-generation scene)
                    :content-revision (scene-content-revision scene)
                    :torch-light-emission
                    (scene-torch-light-emission scene)
                    :voxel-light-propagation-p
                    (scene-voxel-light-propagation-p scene)
                    :light-generation (scene-authored-light-generation scene)
                    :torches (scene-torches scene)
                    :player-p (scene-player-p scene)
                    :frames-per-load frames-per-load
                    :residency-radius residency-radius)))
    (luft:map-chain-chunks
     (lambda (key chain)
       (setf (gethash key (streaming-scene-store streaming)) chain))
     (scene-solid scene))
    streaming))

(defun make-authored-world-streaming-scene
    (&key (horizontal-bits +large-world-horizontal-bits+)
      (seed +large-world-seed+) (frames-per-load 1) (residency-radius 0))
  "Make the canonical large demand world without materializing any chunk."
  (let* ((domain (luft:make-world-domain
                  :x-bits horizontal-bits :y-bits horizontal-bits))
         (source (make-instance 'authored-world-source
                                :domain domain :seed seed))
         (builder (make-scene-builder :horizontal-bits horizontal-bits))
         ;; Regional voxel light is deliberately not begun by this stage.
         (empty (finish-scene-builder
                 builder :player-p t :voxel-light-propagation-p nil)))
    (make-instance
     'streaming-scene
     :source source
     :solid (luft:make-chain domain)
     :material-vocabulary (scene-material-vocabulary empty)
     :material-cells (make-hash-table :test #'eql)
     :authored-light-sources #()
     :authored-light-opacity-table
     (scene-authored-light-opacity-table empty)
     :authored-light-revision 0
     :authored-light-provenance (scene-authored-light-provenance empty)
     :authored-light-generation (scene-authored-light-generation empty)
     :content-revision 0
     :torch-light-emission (scene-torch-light-emission empty)
     :voxel-light-propagation-p nil
     :light-generation (scene-authored-light-generation empty)
     :torches #()
     :player-p t
     :frames-per-load frames-per-load
     :residency-radius residency-radius)))

(defun streaming-store-chain (scene key &optional default)
  "Return KEY's chain from either a finite fixture or resident source value."
  (multiple-value-bind (value present-p)
      (gethash key (streaming-scene-store scene))
    (values
     (if present-p
         (etypecase value
           (luft:chain value)
           (resident-cell-chunk (resident-cell-chunk-chain value)))
         default)
     present-p)))

(defun streaming-store-incarnation (scene key)
  (let ((value (gethash key (streaming-scene-store scene))))
    (and (resident-cell-chunk-p value)
         (resident-cell-chunk-incarnation value))))

(defun streaming-scene-cell-state (scene x y z)
  "Classify a cell without conflating sky, finite boundary, and nonresidency."
  (let ((domain (luft:chain-domain (scene-solid scene))))
    (if (or (< x 0) (>= x (luft:world-domain-x-limit domain))
            (< y 0) (>= y (luft:world-domain-y-limit domain))
            (< z 0) (> z 254))
        :closed-boundary
        (let ((key (luft:chunk-key-at x y)))
          (multiple-value-bind (chain resident-p)
              (streaming-store-chain scene key)
            (cond
              ((not resident-p) :unknown-nonresident)
              ((= 1 (luft:chain-cell-occupancy-bit chain x y z)) :solid)
              (t :open-sky)))))))

(defun snapshot-streaming-scene-input (scene)
  "Freeze SCENE's replace-only authored values for a worker request."
  (make-instance
   'scene
   :solid (scene-solid scene)
   :material-vocabulary (scene-material-vocabulary scene)
   :material-cells (scene-material-cells scene)
   :authored-light-sources (scene-authored-light-sources scene)
   :authored-light-opacity-table (scene-authored-light-opacity-table scene)
   :authored-light-revision (scene-authored-light-revision scene)
   :authored-light-provenance (scene-authored-light-provenance scene)
   :authored-light-generation (scene-authored-light-generation scene)
   :content-revision (scene-content-revision scene)
   :torch-light-emission (scene-torch-light-emission scene)
   :voxel-light-propagation-p (scene-voxel-light-propagation-p scene)
   :torches (scene-torches scene)
   :player-p (scene-player-p scene)))

(defstruct (scene-edit
             (:constructor %make-scene-edit
                 (cell old-placement new-placement content-revision))
             (:copier nil))
  "One reversible authored cell transition already published to a scene."
  (cell 0 :type luft:site :read-only t)
  (old-placement nil :type (or null material-placement) :read-only t)
  (new-placement nil :type (or null material-placement) :read-only t)
  (content-revision 0 :type (integer 0 *) :read-only t))

(defun copy-scene-material-cells (scene)
  "Copy SCENE's replace-only cell-to-placement-offset field."
  (let ((copy (make-hash-table
               :test #'eql :size (hash-table-count
                                   (scene-material-cells scene)))))
    (maphash (lambda (cell offset) (setf (gethash cell copy) offset))
             (scene-material-cells scene))
    copy))

(defun scene-edit-torch-conflict-p (scene cell)
  "Whether changing CELL would invalidate a retained torch attachment."
  (loop for attachment across (scene-torches scene)
        thereis (or (= cell (torch-attachment-support-cell attachment))
                    (= cell (torch-attachment-clearance-cell attachment)))))

(defun make-cell-chain-delta (domain cell polarity)
  (let ((builder (luft:make-chain-builder domain :initial-capacity 1)))
    (luft:chain-builder-add-site
     builder (luft:site-with-polarity cell polarity))
    (luft:finish-chain-builder builder)))

(defun edit-streaming-scene-cell (scene cell new-placement)
  "Publish one complete authored cell edit and return EDIT, status, and chunk.

NEW-PLACEMENT fills an empty cell with an existing scene vocabulary member;
NIL removes an occupied cell.  All successor chains, material state, and light
are constructed before the canvas-owned scene is changed.  Active production
is deliberately rejected; the caller may retry after its current cohort has
published."
  (check-type scene streaming-scene)
  (check-type cell luft:site)
  (when new-placement (check-type new-placement material-placement))
  (when (or (streaming-scene-cohort scene)
            (streaming-scene-removals scene))
    (return-from edit-streaming-scene-cell (values nil :busy nil)))
  (let* ((solid (scene-solid scene))
         (domain (luft:chain-domain solid)))
    (luft:checked-site domain cell)
    (unless (and (= (luft:site-extent cell) luft:+cell-extent+)
                 (luft:site-positive-p cell))
      (error "A scene edit needs one positive cell in the scene domain, not ~S."
             cell))
    (when (scene-edit-torch-conflict-p scene cell)
      (return-from edit-streaming-scene-cell (values nil :attachment nil)))
    (multiple-value-bind (old-offset occupied-p)
        (gethash cell (scene-material-cells scene))
      (unless (eql occupied-p
                   (= 1 (luft:chain-cell-occupancy-bit
                         solid (luft:site-x cell) (luft:site-y cell)
                         (luft:site-z cell))))
        (error "Scene occupancy and material state disagree at ~S." cell))
      (cond ((and new-placement occupied-p)
             (return-from edit-streaming-scene-cell
               (values nil :occupied nil)))
            ((and (null new-placement) (not occupied-p))
             (return-from edit-streaming-scene-cell
               (values nil :empty nil))))
      (let ((new-offset
              (and new-placement
                   (domains:identity-vocabulary-offset
                    (scene-material-vocabulary scene) new-placement nil))))
        (when (and new-placement (null new-offset))
          (return-from edit-streaming-scene-cell
            (values nil :unknown-material nil)))
        (let* ((material-cells (copy-scene-material-cells scene))
               (polarity (if new-placement 1 -1))
               (delta (make-cell-chain-delta domain cell polarity))
               (new-solid (luft:chain+ solid delta))
               (key (luft:site-chunk-key cell))
               (empty (luft:make-chain domain))
               (old-chunk (streaming-store-chain scene key empty))
               (new-chunk (luft:chain+ old-chunk delta))
               (light-revision (1+ (scene-authored-light-revision scene))))
          (if new-placement
              (setf (gethash cell material-cells) new-offset)
              (remhash cell material-cells))
          (let* ((sources
                   (coerce
                    (sort
                     (compile-material-light-sources
                      material-cells (scene-material-vocabulary scene))
                     #'<)
                    '(simple-array (unsigned-byte 64) (*))))
                 (base-generation
                   (solve-realized-light-generation
                    domain material-cells
                    (scene-authored-light-opacity-table scene)
                    (if (scene-voxel-light-propagation-p scene) sources #())
                    (scene-authored-light-provenance scene) light-revision
                    (make-realized-light-seeds #() #())
                    :field-revision light-revision))
                 (content-revision (1+ (scene-content-revision scene)))
                 (edit
                   (%make-scene-edit
                    cell
                    (and occupied-p
                         (domains:identity-vocabulary-member
                          (scene-material-vocabulary scene) old-offset))
                    new-placement content-revision)))
            ;; These values are replace-only.  Existing worker snapshots retain
            ;; the old chains, hash table, and light generation without copying.
            (setf (scene-solid scene) new-solid
                  (scene-material-cells scene) material-cells
                  (scene-authored-light-sources scene) sources
                  (scene-authored-light-revision scene) light-revision
                  (scene-authored-light-generation scene) base-generation
                  (scene-content-revision scene) content-revision
                  (streaming-scene-light-generation scene) base-generation)
            (if (streaming-scene-source scene)
                (let ((local-materials (make-hash-table :test #'eql)))
                  (maphash
                   (lambda (material-cell offset)
                     (when (= key (luft:site-chunk-key material-cell))
                       (setf (gethash material-cell local-materials) offset)))
                   material-cells)
                  (setf (gethash cell
                                 (authored-world-source-edits
                                  (streaming-scene-source scene)))
                        new-placement
                        (gethash key (streaming-scene-store scene))
                        (%make-resident-cell-chunk
                         key
                         (incf (streaming-scene-next-incarnation scene))
                         new-chunk local-materials)))
                (if (luft:chain-empty-p new-chunk)
                    (remhash key (streaming-scene-store scene))
                    (setf (gethash key (streaming-scene-store scene))
                          new-chunk)))
            (values edit :edited key)))))))

(defun reset-streaming-scene-publication (scene)
  "Forget renderer publication while retaining source-owned resident values."
  (check-type scene streaming-scene)
  (dolist (table (list (streaming-scene-loaded scene)
                       (streaming-scene-outstanding scene)
                       (streaming-scene-staged scene)))
    (clrhash table))
  (setf (streaming-scene-cohort scene) nil
        (streaming-scene-removals scene) nil
        (streaming-scene-production-errors scene) nil
        (streaming-scene-staged-generation scene) nil
        (streaming-scene-focus scene) nil
        (streaming-scene-light-generation scene)
        (scene-authored-light-generation scene)
        (streaming-scene-frame-counter scene) 0)
  scene)

(defun streaming-scene-keys-near (scene focus-x focus-y)
  "Resident chunk keys inside SCENE's visible square window."
  (let ((radius (streaming-scene-residency-radius scene))
        (keys nil))
    (loop for key being the hash-keys of (streaming-scene-store scene)
          when (and (<= (abs (- (luft:chunk-key-x key) focus-x)) radius)
                    (<= (abs (- (luft:chunk-key-y key) focus-y)) radius))
            do (push key keys))
    (sort keys #'<)))

(defun chunk-keys-neighbor-p (left right)
  (and (<= (abs (- (luft:chunk-key-x left) (luft:chunk-key-x right))) 1)
       (<= (abs (- (luft:chunk-key-y left) (luft:chunk-key-y right))) 1)))

(defun streaming-scene-key-distance (key focus)
  "Chebyshev chunk distance between KEY and FOCUS."
  (max (abs (- (luft:chunk-key-x key) (car focus)))
       (abs (- (luft:chunk-key-y key) (cdr focus)))))

(declaim
 (ftype function
        streaming-scene-canonical-owner-closure
        streaming-scene-dependency-guard-keys))

(defun streaming-scene-resident-torch-support-keys (scene source-keys)
  "Return sorted support owners for torches resident in SOURCE-KEYS."
  (sort
   (remove-duplicates
    (loop for attachment across (scene-torches scene)
          for key = (luft:site-chunk-key
                     (torch-attachment-support-cell attachment))
          when (member key source-keys :test #'eql)
            collect key)
    :test #'eql)
   #'<))

(defun streaming-residency-changes-affect-torches-p
    (scene changes old-source-keys desired-source-keys)
  "Whether CHANGES can add, remove, or move a resident realized torch frame."
  (let ((support-keys
          (streaming-scene-resident-torch-support-keys
           scene (union old-source-keys desired-source-keys :test #'eql))))
    (some (lambda (support)
            (some (lambda (changed)
                    (chunk-keys-neighbor-p support changed))
                  changes))
          support-keys)))

(defun %retarget-resident-streaming-scene
    (scene production-system bevel-width world-x world-y)
  "Batch SCENE's desired window around a camera position and mesh it once.

The camera may stand outside the finite authored world while looking back at
its boundary.  Clamp that position to the nearest domain cell before forming
an unsigned chunk key, so a low-side coordinate cannot wrap to chunk 4095 and
silently empty the desired residency window."
  (when (or (streaming-scene-cohort scene)
            (streaming-scene-removals scene))
    (return-from %retarget-resident-streaming-scene nil))
  (let* ((domain (luft:chain-domain (scene-solid scene)))
         (focus-key
           (luft:chunk-key-at
            (max 0 (min (1- (luft:world-domain-x-limit domain))
                        (floor world-x)))
            (max 0 (min (1- (luft:world-domain-y-limit domain))
                        (floor world-y)))))
         (focus (cons (luft:chunk-key-x focus-key)
                      (luft:chunk-key-y focus-key)))
         (desired (streaming-scene-keys-near scene (car focus) (cdr focus)))
         (loaded (streaming-scene-loaded scene))
         (old-source-keys
           (sort (loop for key being the hash-keys of loaded collect key) #'<))
         (old-owner-keys
           (streaming-scene-canonical-owner-closure scene old-source-keys))
         (desired-owner-keys
           (streaming-scene-canonical-owner-closure scene desired))
         (desired-widths (make-hash-table :test #'eql))
         (arrivals
           (remove-if (lambda (key) (gethash key loaded)) desired))
         (departures
           (loop for key being the hash-keys of loaded
                 unless (member key desired :test #'eql)
                   collect key))
         (width-changes nil))
    ;; Geometry width is a cohort property.  Per-owner distance selection was
    ;; removed because neighboring widths move their shared lattice sites to
    ;; different points; a future geometric LoD needs an explicit transition
    ;; product rather than another scalar lookup here.
    (dolist (key desired)
      (let ((width bevel-width))
        (setf (gethash key desired-widths) width)
        (multiple-value-bind (old-width present-p) (gethash key loaded)
          (when (and present-p
                     (not (eql old-width width)))
            (push key width-changes)))))
    (let ((residency-changes (append arrivals departures))
          (owner-removals
            (set-difference old-owner-keys desired-owner-keys :test #'eql))
          (changes (append arrivals departures width-changes)))
      (let* ((torch-residency-change-p
               (streaming-residency-changes-affect-torches-p
                scene residency-changes old-source-keys desired))
             (desired-torch-p
               (plusp
                (length
                 (streaming-scene-resident-torch-support-keys
                  scene desired))))
             (full-closure-p
               (or width-changes torch-residency-change-p))
             (realize-torch-light-p
               (not
                (null
                 (or torch-residency-change-p
                     (and desired-torch-p width-changes))))))
        (setf (streaming-scene-focus scene) focus)
        (when changes
          (clrhash loaded)
          (dolist (key desired)
            (setf (gethash key loaded) (gethash key desired-widths)))
          (let ((affected
                  (if full-closure-p
                      desired-owner-keys
                      (remove-if-not
                       (lambda (key)
                         (some (lambda (changed)
                                 (chunk-keys-neighbor-p key changed))
                               residency-changes))
                       desired-owner-keys))))
            (setf (streaming-scene-cohort scene) affected
                  (streaming-scene-removals scene) owner-removals)
            (cond
              (affected
               (schedule-streaming-scene-cohort
                scene production-system affected bevel-width
                (reduce #'min affected
                        :key (lambda (key)
                               (streaming-scene-key-distance key focus)))
                :realize-torch-light-p realize-torch-light-p))
              (owner-removals
               ;; A removal can leave no affected output owner to send to a
               ;; worker. Preserve the installed realized field when resident
               ;; torch semantics are unchanged; only an exact empty resident
               ;; torch set returns to the material-only base generation.
               (setf (streaming-scene-staged-generation scene)
                     (make-scene-mesh-generation-value
                      scene
                      (streaming-scene-mesh-stamp
                       scene nil bevel-width)
                      (if desired-torch-p
                          (streaming-scene-light-generation scene)
                          (scene-authored-light-generation scene))))))
            t))))))

(defun streaming-scene-focus-key (scene world-x world-y)
  (let ((domain (luft:chain-domain (scene-solid scene))))
    (luft:chunk-key-at
     (max 0 (min (1- (luft:world-domain-x-limit domain)) (floor world-x)))
     (max 0 (min (1- (luft:world-domain-y-limit domain)) (floor world-y))))))

(defun streaming-domain-keys-near (scene focus radius)
  "Return every in-domain key in the square RADIUS around FOCUS."
  (let* ((domain (luft:chain-domain (scene-solid scene)))
         (maximum-x
           (1- (ceiling (luft:world-domain-x-limit domain) luft:+chunk-size+)))
         (maximum-y
           (1- (ceiling (luft:world-domain-y-limit domain) luft:+chunk-size+)))
         (focus-x (luft:chunk-key-x focus))
         (focus-y (luft:chunk-key-y focus)))
    (sort
     (loop for x from (max 0 (- focus-x radius))
             to (min maximum-x (+ focus-x radius)) append
       (loop for y from (max 0 (- focus-y radius))
               to (min maximum-y (+ focus-y radius))
             collect (luft:chunk-key-at (* x luft:+chunk-size+)
                                        (* y luft:+chunk-size+))))
     #'<)))

(defun rebuild-authored-world-resident-values (scene)
  "Publish the exact union/material views derived from SCENE's resident store."
  (let* ((source (streaming-scene-source scene))
         (domain (authored-world-source-domain source))
         (cell-count
           (loop for resident being the hash-values of
                   (streaming-scene-store scene)
                 sum (hash-table-count
                      (resident-cell-chunk-material-cells resident))))
         (builder (luft:make-chain-builder domain :initial-capacity cell-count))
         (materials (make-hash-table :test #'eql :size cell-count)))
    (maphash
     (lambda (key resident)
       (declare (ignore key))
       (luft:chain-builder-add-chain
        builder (resident-cell-chunk-chain resident))
       (maphash (lambda (cell offset) (setf (gethash cell materials) offset))
                (resident-cell-chunk-material-cells resident)))
     (streaming-scene-store scene))
    (setf (scene-solid scene) (luft:finish-chain-builder builder)
          (scene-material-cells scene) materials
          (scene-content-revision scene) (1+ (scene-content-revision scene)))
    scene))

(defun schedule-authored-world-chunk-load
    (scene production-system key demand-token priority)
  (let* ((source (streaming-scene-source scene))
         (incarnation (incf (streaming-scene-next-incarnation scene)))
         (request
           (make-instance
            'authored-chunk-load-request
            :key (list :luft-authored-load key demand-token)
            :priority priority :scene scene :source source :chunk-key key
            :demand-token demand-token :incarnation incarnation
            :edits (capture-authored-world-chunk-edits source key)))
         (ticket
           (production:schedule-production-request production-system request)))
    (setf (gethash key (streaming-scene-load-outstanding scene)) ticket)
    request))

(defun authored-world-residency-ready-p (scene)
  (loop for key being the hash-keys of (streaming-scene-desired scene)
        always (nth-value 1 (gethash key (streaming-scene-store scene)))))

(defun evict-undesired-authored-world-residents (scene)
  "Evict every derived CPU value outside SCENE's canonical desired set."
  (let ((departures nil)
        (desired (streaming-scene-desired scene)))
    (maphash (lambda (key value)
               (declare (ignore value))
               (unless (gethash key desired) (push key departures)))
             (streaming-scene-store scene))
    (dolist (key departures) (remhash key (streaming-scene-store scene)))
    departures))

(defun accept-authored-chunk-load-result (scene request resident)
  "Install RESIDENT only if REQUEST still owns KEY's exact demand incarnation."
  (let* ((key (authored-chunk-load-request-chunk-key request))
         (token (authored-chunk-load-request-demand-token request))
         (ticket (production:production-request-ticket request)))
    (when (and (eq scene (authored-chunk-load-request-scene request))
               (eql token (gethash key (streaming-scene-desired scene)))
               (eql ticket
                    (gethash key (streaming-scene-load-outstanding scene)))
               (= key (resident-cell-chunk-key resident))
               (= (authored-chunk-load-request-incarnation request)
                  (resident-cell-chunk-incarnation resident)))
      (setf (gethash key (streaming-scene-store scene)) resident)
      (remhash key (streaming-scene-load-outstanding scene))
      t)))

(defun retarget-authored-world
    (scene production-system bevel-width world-x world-y)
  "Demand, asynchronously materialize, and activate one bounded source window."
  (let* ((focus-key (streaming-scene-focus-key scene world-x world-y))
         (focus (cons (luft:chunk-key-x focus-key)
                      (luft:chunk-key-y focus-key)))
         (visible-keys
           (streaming-domain-keys-near
            scene focus-key (streaming-scene-residency-radius scene)))
         ;; Materialize exactly the complete occupancy capture which the
         ;; visible owner closure will need: owners, witness, then probe guard.
         ;; This is asymmetric at high seams and substantially smaller than a
         ;; conservative square radius.
         (desired-keys
           (streaming-scene-dependency-guard-keys
            scene
            (streaming-scene-dependency-guard-keys
             scene
             (streaming-scene-canonical-owner-closure scene visible-keys))))
         (desired (streaming-scene-desired scene)))
    (unless (equal focus (streaming-scene-focus scene))
      (let ((next (make-hash-table :test #'eql)))
        (dolist (key desired-keys)
          (setf (gethash key next)
                (or (gethash key desired)
                    (incf (streaming-scene-next-demand-token scene)))))
        (maphash
         (lambda (key token)
           (unless (gethash key next)
             (when (production:cancel-production-request
                    production-system
                    (list :luft-authored-load key token))
               (remhash key (streaming-scene-load-outstanding scene)))))
         desired)
        (clrhash desired)
        (maphash (lambda (key token) (setf (gethash key desired) token)) next))
      ;; Resident values are a cache. Unknown is never copied into the next
      ;; immutable mesh capture, and eviction remains bounded by DESIRED.
      (evict-undesired-authored-world-residents scene)
      (dolist (key desired-keys)
        (unless (nth-value 1 (gethash key (streaming-scene-store scene)))
          (schedule-authored-world-chunk-load
           scene production-system key (gethash key desired)
           (streaming-scene-key-distance key focus))))
      (setf (streaming-scene-focus scene) focus))
    (when (and (null (streaming-scene-cohort scene))
               (null (streaming-scene-removals scene))
               (authored-world-residency-ready-p scene))
      ;; Let the established cohort path observe the old published LOADED set;
      ;; it computes exact owner removals before replacing it with this focus.
      (setf (streaming-scene-focus scene) nil)
      (rebuild-authored-world-resident-values scene)
      (%retarget-resident-streaming-scene
       scene production-system bevel-width world-x world-y))))

(defun retarget-streaming-scene
    (scene production-system bevel-width world-x world-y)
  (if (streaming-scene-source scene)
      (retarget-authored-world
       scene production-system bevel-width world-x world-y)
      (%retarget-resident-streaming-scene
       scene production-system bevel-width world-x world-y)))

(defconstant +streaming-owner-dependency-radius+ 1)

(defun streaming-scene-canonical-owner-closure (scene source-keys)
  "Return the exact half-open geometry-owner closure of SOURCE-KEYS.

A source column can emit primitives to its own owner and to the +X, +Y, or
+X+Y owner at high seam anchors.  Low anchors remain source-owned.  The
directional closure publishes every real primitive without inventing five
unneeded low-side empty slots around a sparse source."
  (let* ((domain (luft:chain-domain (scene-solid scene)))
         (maximum-x
           (1- (ceiling (luft:world-domain-x-limit domain)
                        luft:+chunk-size+)))
         (maximum-y
           (1- (ceiling (luft:world-domain-y-limit domain)
                        luft:+chunk-size+)))
         (keys (make-hash-table :test #'eql)))
    (dolist (source source-keys)
      (let ((source-x (luft:chunk-key-x source))
            (source-y (luft:chunk-key-y source)))
        (loop for x from source-x to (min maximum-x (1+ source-x)) do
          (loop for y from source-y to (min maximum-y (1+ source-y)) do
            (setf (gethash
                   (luft:chunk-key-at (* x luft:+chunk-size+)
                                      (* y luft:+chunk-size+))
                   keys)
                  t)))))
    (sort (loop for key being the hash-keys of keys collect key) #'<)))

(defun streaming-scene-dependency-guard-keys
    (scene seeds &optional (radius +streaming-owner-dependency-radius+))
  "Return every in-domain owner key within RADIUS of SEEDS.

The result deliberately includes virtual empty owners.  Canonical face, band,
and fan ownership can cross a chunk seam even when the neighboring occupancy
chunk is empty; excluding that owner drops real boundary triangles."
  (check-type radius (integer 0 *))
  (let* ((domain (luft:chain-domain (scene-solid scene)))
         (maximum-x
           (1- (ceiling (luft:world-domain-x-limit domain)
                        luft:+chunk-size+)))
         (maximum-y
           (1- (ceiling (luft:world-domain-y-limit domain)
                        luft:+chunk-size+)))
         (keys (make-hash-table :test #'eql)))
    (dolist (seed seeds)
      (let ((seed-x (luft:chunk-key-x seed))
            (seed-y (luft:chunk-key-y seed)))
        (loop for x from (max 0 (- seed-x radius))
                to (min maximum-x (+ seed-x radius)) do
          (loop for y from (max 0 (- seed-y radius))
                  to (min maximum-y (+ seed-y radius)) do
            (setf (gethash
                   (luft:chunk-key-at (* x luft:+chunk-size+)
                                      (* y luft:+chunk-size+))
                   keys)
                  t)))))
    (sort (loop for key being the hash-keys of keys collect key) #'<)))

(defun scene-torch-semantics-signature (scene resident-source-keys)
  "Return exact sorted resident attachment semantics and authored emission."
  (list
   (scene-torch-light-emission scene)
   (loop for attachment across (scene-torches scene)
         for support = (torch-attachment-support-cell attachment)
         when (member (luft:site-chunk-key support) resident-source-keys
                      :test #'eql)
           collect
           (list support
                 (torch-attachment-face attachment)
                 (torch-attachment-clearance-cell attachment)
                 (torch-attachment-chart-u attachment)
                 (torch-attachment-chart-v attachment)))))

(defun streaming-scene-mesh-stamp (scene output-keys bevel-width)
  "Name exact content, owner, geometry, light, and torch request inputs."
  (let ((resident-source-keys
          (sort
           (loop for key being the hash-keys of (streaming-scene-loaded scene)
                 collect key)
           #'<)))
    (list :scene-mesh-request-v2
          (scene-content-revision scene)
          (copy-list output-keys)
          bevel-width
          (scene-authored-light-revision scene)
          (scene-authored-light-provenance scene)
          (scene-voxel-light-propagation-p scene)
          (scene-torch-semantics-signature scene resident-source-keys)
          (loop for key in resident-source-keys
                collect (list key
                              (gethash key (streaming-scene-loaded scene))
                              (streaming-store-incarnation scene key))))))

(defun make-streaming-region-snapshot
    (scene output-keys bevel-width &key (realize-torch-light-p t))
  "Capture one immutable union window and its width/repair guard ring."
  (check-type scene streaming-scene)
  (check-type output-keys list)
  (unless output-keys
    (error "A streaming region snapshot needs at least one output owner."))
  (let* ((output-keys
           (sort
            (remove-duplicates (copy-list output-keys) :test #'eql)
            #'<))
         (witness-keys
           (streaming-scene-dependency-guard-keys scene output-keys))
         (captured-keys
           (streaming-scene-dependency-guard-keys scene witness-keys))
         (union-neighborhood (make-hash-table :test #'eql))
         (loaded (streaming-scene-loaded scene))
         (resident-source-keys
           (sort (loop for key being the hash-keys of loaded collect key) #'<))
         (empty (luft:make-chain (luft:chain-domain (scene-solid scene)))))
    (unless (every (lambda (key) (member key witness-keys :test #'eql))
                   output-keys)
      (error "Streaming outputs ~S are not all resident in witness set ~S."
             output-keys witness-keys))
    (dolist (key captured-keys)
      ;; Presence records residency.  An empty union chain is still a captured
      ;; answer and must not be confused with an unknown/out-of-window chunk.
      (multiple-value-bind (chain present-p)
          (streaming-store-chain scene key empty)
        (cond
          (present-p
           (setf (gethash key union-neighborhood) chain))
          ((streaming-scene-source scene)
           (error "Demand snapshot crossed unknown nonresident chunk ~D." key))
          (t
           (setf (gethash key union-neighborhood) empty)))))
    (%make-streaming-mesh-snapshot
     scene (snapshot-streaming-scene-input scene)
     output-keys witness-keys resident-source-keys
     bevel-width union-neighborhood
     (streaming-scene-mesh-stamp
      scene output-keys bevel-width)
     realize-torch-light-p
     (streaming-scene-light-generation scene))))

(defun make-streaming-mesh-snapshot
    (scene key bevel-width &key (realize-torch-light-p t))
  "Capture the one-owner form of MAKE-STREAMING-REGION-SNAPSHOT."
  (make-streaming-region-snapshot
   scene (list key) bevel-width
   :realize-torch-light-p realize-torch-light-p))

(defun mesh-streaming-snapshot (snapshot)
  "Mesh one worker-owned regional snapshot without reading owner state.

The first value is an alist of output owner to final mesh.  Every guarded
owner uses the same width-one star selector; context owners are retained only
while scene decoration establishes cross-chunk light provenance."
  (let* ((owner-scene (streaming-mesh-snapshot-scene snapshot))
         (scene (streaming-mesh-snapshot-input-scene snapshot))
         (neighborhood (streaming-mesh-snapshot-union-neighborhood snapshot)))
    (labels ((mesh-owner (key)
               (let ((chain (gethash key neighborhood)))
                 (unless chain
                   (error "Chunk ~D was not captured by this regional snapshot."
                          key))
                 (zone (:luft/rematerialize :value (luft:chain-count chain))
                   (luft:mesh-star-chunk
                    chain key :outside-domain-policy :air))))
             (decorate-owners (owners &optional surface-context)
               (decorate-scene-meshes
                owners scene :surface-context surface-context
                :generation-scene owner-scene
                :attachment-source-owners
                (streaming-mesh-snapshot-resident-source-keys snapshot)
                :request-stamp (streaming-mesh-snapshot-stamp snapshot)
                :reusable-light-generation
                (streaming-mesh-snapshot-reusable-light-generation snapshot)
                :realize-torch-light-p
                (streaming-mesh-snapshot-realize-torch-light-p snapshot))))
      (handler-bind
          ((luft:missing-chunk
             (lambda (condition)
               (multiple-value-bind (chain present-p)
                   (gethash (luft:missing-chunk-key condition) neighborhood)
                 (if present-p
                     (invoke-restart 'luft:use-chunk chain)
                     (invoke-restart 'luft:treat-as-air)))))
           (luft:outside-domain
             (lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'luft:treat-as-air))))
        (let* ((output-keys
                 (streaming-mesh-snapshot-output-keys snapshot))
               (all-owners
                 (mapcar (lambda (key) (cons key (mesh-owner key)))
                         (streaming-mesh-snapshot-witness-keys snapshot)))
               (owners
                 (remove-if-not
                  (lambda (entry)
                    (member (car entry) output-keys :test #'eql))
                  all-owners))
               (surface-context
                 (remove-if
                  (lambda (entry)
                    (member (car entry) output-keys :test #'eql))
                  all-owners)))
          (multiple-value-bind (decorated generation)
              (decorate-owners owners surface-context)
            (values decorated nil nil generation)))))))

(defun make-scene-regional-meshes
    (scene bevel-width &key reusable-light-generation)
  "Compile all of SCENE through the same owner/halo path used by streaming.

This is the fully resident static form of the regional compiler.  The returned
alist retains canonical chunk ownership, while callers that require the legacy
single-mesh surface can borrow it as a companion tree."
  (check-type scene scene)
  (let* ((streaming (make-streaming-scene scene))
         (source-keys
           (sort (loop for key being the hash-keys of
                       (streaming-scene-store streaming)
                       collect key)
                 #'<))
         (owner-keys
           (streaming-scene-canonical-owner-closure streaming source-keys)))
    (when reusable-light-generation
      (when (and (typep reusable-light-generation 'scene-mesh-generation)
                 (not
                  (eq (scene-authored-light-provenance scene)
                      (scene-authored-light-provenance
                       (scene-mesh-generation-scene
                        reusable-light-generation)))))
        (error "A reusable generation belongs to a different authored scene input."))
      (setf (streaming-scene-light-generation streaming)
            (etypecase reusable-light-generation
              (realized-light-generation reusable-light-generation)
              (scene-mesh-generation
               (scene-mesh-generation-light-generation
                reusable-light-generation)))))
    (dolist (key source-keys)
      (setf (gethash key (streaming-scene-loaded streaming)) bevel-width))
    (if owner-keys
        (multiple-value-bind (owners census diagnostics generation)
            (mesh-streaming-snapshot
             (make-streaming-region-snapshot
              streaming owner-keys bevel-width))
          (values
           owners census diagnostics
           (make-scene-mesh-generation-value
            scene (scene-mesh-generation-request-stamp generation)
            (scene-mesh-generation-light-generation generation)
            :mesh-entries owners)))
        (let* ((request-stamp
                 (streaming-scene-mesh-stamp
                  streaming nil bevel-width))
               (light-generation
                 (or reusable-light-generation
                     (scene-authored-light-generation scene)))
               (generation
                 (make-scene-mesh-generation-value
                  scene request-stamp
                  (etypecase light-generation
                    (realized-light-generation light-generation)
                    (scene-mesh-generation
                     (scene-mesh-generation-light-generation
                      light-generation))))))
          (values nil nil nil generation)))))

(defun mesh-streaming-chunk (scene key bevel-width)
  "Synchronously mesh KEY's canonical publication closure.

The returned root is KEY's owner mesh; the +X/+Y/+X+Y owners required by the
half-open primitive convention are companions.  The second value is the exact
immutable SCENE-MESH-GENERATION that a publication boundary must retain.  This
synchronous compiler does not mutate the streaming scene's installed reusable
generation before publication succeeds."
  (let* ((resident-source-keys
           (sort (loop for resident being the hash-keys of
                       (streaming-scene-loaded scene)
                       collect resident)
                 #'<))
         (resident-torch-p
           (plusp
            (length
             (streaming-scene-resident-torch-support-keys
              scene resident-source-keys))))
         (output-keys
           (streaming-scene-canonical-owner-closure
            scene (if resident-torch-p resident-source-keys (list key))))
         (snapshot
           (make-streaming-region-snapshot
            scene output-keys bevel-width)))
    (multiple-value-bind (owners census diagnostics generation)
        (mesh-streaming-snapshot snapshot)
      (declare (ignore census diagnostics))
      (let ((root-entry (assoc key owners :test #'eql)))
        (unless root-entry
          (error "Synchronous streaming result omitted requested chunk ~D." key))
        (let ((root (cdr root-entry)))
          (setf (luft:surface-mesh-companions root)
                (append (luft:surface-mesh-companions root)
                        (mapcar #'cdr (remove root-entry owners :test #'eq))))
          ;; As with the static single-root API, owner companions were just
          ;; aggregated.  Return a generation for the exact resulting tree,
          ;; explicitly unbound from any eventual renderer slot key.
          (values
           root
           (make-scene-mesh-generation-value
            scene (scene-mesh-generation-request-stamp generation)
            (scene-mesh-generation-light-generation generation)
            :mesh-entries (list (cons +unkeyed-scene-mesh-output+ root))
            :unkeyed-mesh-p t)))))))

(defmethod production:perform-production-request
    ((request streaming-mesh-request))
  (multiple-value-bind (meshes census diagnostics generation)
      (mesh-streaming-snapshot (streaming-mesh-request-snapshot request))
    (declare (ignore census diagnostics))
    (%make-streaming-mesh-result
     (mapcar (lambda (entry)
               (cons (car entry) (prepare-render-mesh (cdr entry))))
             meshes)
     generation)))

(defconstant +streaming-cohort-production-key+ :luft-streaming-cohort)

(defvar *streaming-mesh-snapshot-observer* nil
  "Optional test instrumentation called with each snapshot before scheduling.")

(defun schedule-streaming-scene-cohort
    (scene production-system output-keys bevel-width priority
     &key (realize-torch-light-p t))
  "Schedule one dependency-guarded regional compilation for OUTPUT-KEYS."
  (let* ((snapshot
         (make-streaming-region-snapshot
            scene output-keys bevel-width
            :realize-torch-light-p realize-torch-light-p))
         (request
           (make-instance 'streaming-mesh-request
                          :key +streaming-cohort-production-key+
                          :priority priority :snapshot snapshot))
         (ticket
           (progn
             (when *streaming-mesh-snapshot-observer*
               (funcall *streaming-mesh-snapshot-observer* snapshot))
             (production:schedule-production-request production-system request))))
    (dolist (key output-keys)
      (setf (gethash key (streaming-scene-outstanding scene)) ticket))
    request))

(defun schedule-streaming-scene-edit
    (scene production-system changed-source-key bevel-width)
  "Remesh the resident scene after one already-published authored edit."
  (check-type scene streaming-scene)
  (when (or (streaming-scene-cohort scene)
            (streaming-scene-removals scene))
    (error "Cannot schedule an edit while another streaming cohort is active."))
  (let ((loaded (streaming-scene-loaded scene)))
    ;; Placement can create the first occupied cell in the empty chunk beside a
    ;; rendered seam.  Admit that source until ordinary camera retargeting next
    ;; applies the bounded residency window.
    (unless (nth-value 1 (gethash changed-source-key loaded))
      (when (loop for key being the hash-keys of loaded
                  thereis (chunk-keys-neighbor-p key changed-source-key))
        (setf (gethash changed-source-key loaded) bevel-width)))
    (let* ((source-keys
             (sort (loop for key being the hash-keys of loaded collect key) #'<))
           ;; Authored voxel and torch light share one resident field.  Until an
           ;; incremental light solver exists, every resident owner must receive
           ;; the newly solved immutable field together.
           (affected
             (streaming-scene-canonical-owner-closure scene source-keys)))
      (when affected
        (setf (streaming-scene-cohort scene) affected
              (streaming-scene-removals scene) nil
              (streaming-scene-frame-counter scene) 0)
        (schedule-streaming-scene-cohort
         scene production-system affected bevel-width
         (if (streaming-scene-focus scene)
             (reduce #'min affected
                     :key (lambda (key)
                            (streaming-scene-key-distance
                             key (streaming-scene-focus scene))))
             0)
         :realize-torch-light-p t))
      affected)))

(defun schedule-streaming-scene-mesh
    (scene production-system key bevel-width priority
     &key (realize-torch-light-p t))
  "Compatibility wrapper scheduling a one-owner regional cohort."
  (schedule-streaming-scene-cohort
   scene production-system (list key) bevel-width priority
   :realize-torch-light-p realize-torch-light-p))

(defun current-streaming-mesh-request-p (scene request)
  (let ((snapshot (streaming-mesh-request-snapshot request)))
    (and (eq scene (streaming-mesh-snapshot-scene snapshot))
         (equalp (streaming-mesh-snapshot-stamp snapshot)
                 (streaming-scene-mesh-stamp
                  scene
                  (streaming-mesh-snapshot-output-keys snapshot)
                  (streaming-mesh-snapshot-bevel-width snapshot))))))

(defun accept-streaming-mesh-result (scene request result)
  "Stage RESULT's complete meshes/generation for the current cohort ticket."
  (check-type result streaming-mesh-result)
  (let* ((snapshot (streaming-mesh-request-snapshot request))
         (keys (streaming-mesh-snapshot-output-keys snapshot))
         (ticket (production:production-request-ticket request))
         (meshes (streaming-mesh-result-meshes result))
         (generation (streaming-mesh-result-generation result)))
    (when (and (every (lambda (key)
                        (eql ticket
                             (gethash key
                                      (streaming-scene-outstanding scene))))
                      keys)
               (current-streaming-mesh-request-p scene request))
      (unless (equal keys (mapcar #'car meshes))
        (error "Streaming cohort returned owners ~S, expected ~S."
               (mapcar #'car meshes) keys))
      (unless (equalp (streaming-mesh-snapshot-stamp snapshot)
                      (scene-mesh-generation-request-stamp generation))
        (error "Streaming light result stamp does not match its mesh request."))
      (dolist (entry meshes)
        (remhash (car entry) (streaming-scene-outstanding scene))
        (setf (gethash (car entry) (streaming-scene-staged scene))
              (cons request (cdr entry))))
      (setf (streaming-scene-staged-generation scene) generation)
      t)))

(defun ready-streaming-scene-meshes (scene)
  "Return the complete current cohort and a readiness flag."
  (let ((cohort (streaming-scene-cohort scene))
        (staged (streaming-scene-staged scene))
        (active-p (or (streaming-scene-cohort scene)
                      (streaming-scene-removals scene))))
    (if (and active-p
             (every (lambda (key)
                      (let ((entry (gethash key staged)))
                        (and entry
                             (current-streaming-mesh-request-p
                              scene (car entry)))))
                    cohort))
        (values
         (mapcar (lambda (key) (cons key (cdr (gethash key staged)))) cohort)
         (streaming-scene-staged-generation scene)
         t)
        (values nil nil nil))))

(defun publish-ready-streaming-scene (scene renderer)
  "Install a complete current mesh cohort at the canvas-owner boundary."
  (multiple-value-bind (meshes generation ready-p)
      (ready-streaming-scene-meshes scene)
    (when ready-p
      (unless generation
        (error "A ready streaming mesh cohort has no realized-light generation."))
      (renderer-update-meshes
       renderer meshes (streaming-scene-removals scene)
       :scene-generation generation)
      ;; Renderer publication has succeeded.  This non-fallible pointer write
      ;; now makes the same immutable light generation reusable by later view
      ;; requests; failed GPU staging never reaches it.
      (setf (streaming-scene-light-generation scene)
            (scene-mesh-generation-light-generation generation))
      (dolist (entry meshes)
        (remhash (car entry) (streaming-scene-staged scene)))
      (setf (streaming-scene-cohort scene) nil
            (streaming-scene-removals scene) nil
            (streaming-scene-staged-generation scene) nil)
      (length meshes))))

(defun drain-streaming-scene-production
    (scene renderer production-system &key (limit 2))
  "Drain and publish bounded worker results on the canvas owner thread."
  (loop repeat limit
        do (multiple-value-bind (result present-p)
               (production:receive-production-result-no-hang production-system)
             (unless present-p (return))
             (let ((request (production:production-result-request result)))
               (etypecase request
                 (authored-chunk-load-request
                  (let* ((key (authored-chunk-load-request-chunk-key request))
                         (ticket (production:production-request-ticket request)))
                    (when (eql ticket
                               (gethash key
                                        (streaming-scene-load-outstanding scene)))
                      (if (production:production-result-condition result)
                          (progn
                            (remhash key
                                     (streaming-scene-load-outstanding scene))
                            (push result
                                  (streaming-scene-production-errors scene))
                            (error "LUFT source production for chunk ~D failed: ~A"
                                   key
                                   (production:production-result-condition
                                    result)))
                          (unless
                              (accept-authored-chunk-load-result
                               scene request
                               (production:production-result-value result))
                            (remhash
                             key
                             (streaming-scene-load-outstanding scene)))))))
                 (streaming-mesh-request
                  (let* ((snapshot (streaming-mesh-request-snapshot request))
                         (keys (streaming-mesh-snapshot-output-keys snapshot))
                         (ticket (production:production-request-ticket request)))
                    (when (every (lambda (key)
                                   (eql ticket
                                        (gethash
                                         key
                                         (streaming-scene-outstanding scene))))
                                 keys)
                      (if (production:production-result-condition result)
                          (progn
                            (dolist (key keys)
                              (remhash key
                                       (streaming-scene-outstanding scene)))
                            (push result
                                  (streaming-scene-production-errors scene))
                            (error "LUFT mesh production for cohort ~S failed: ~A"
                                   keys
                                   (production:production-result-condition
                                    result)))
                          (accept-streaming-mesh-result
                           scene request
                           (production:production-result-value result))))))))))
  (publish-ready-streaming-scene scene renderer))

(defun landscape-hash-reading (x y seed salt)
  "Return a stable coordinate reading in [-1, 1]."
  (let ((value
          (logand #xffffffff
                  (+ seed (* x 374761393) (* y 668265263)
                     (* salt 2246822519)))))
    (setf value
          (logand #xffffffff
                  (* (logxor value (ash value -13)) 1274126177)))
    (- (* 2.0d0
          (/ (logand #xffffffff (logxor value (ash value -16)))
             #xffffffff))
       1.0d0)))

(defun smooth-landscape-reading (reading)
  (* reading reading (- 3.0d0 (* 2.0d0 reading))))

(defun interpolate-landscape-reading (left right amount)
  (+ left (* (- right left) amount)))

(defun landscape-value-noise (x y period seed salt)
  "Sample stable smooth value noise without allocating terrain objects."
  (let* ((sample-x (/ x (coerce period 'double-float)))
         (sample-y (/ y (coerce period 'double-float)))
         (cell-x (floor sample-x))
         (cell-y (floor sample-y))
         (tx (smooth-landscape-reading (- sample-x cell-x)))
         (ty (smooth-landscape-reading (- sample-y cell-y)))
         (near
           (interpolate-landscape-reading
            (landscape-hash-reading cell-x cell-y seed salt)
            (landscape-hash-reading (1+ cell-x) cell-y seed salt)
            tx))
         (far
           (interpolate-landscape-reading
            (landscape-hash-reading cell-x (1+ cell-y) seed salt)
            (landscape-hash-reading (1+ cell-x) (1+ cell-y) seed salt)
            tx)))
    (interpolate-landscape-reading near far ty)))

(defun landscape-ramp (low high reading)
  (smooth-landscape-reading
   (max 0.0d0 (min 1.0d0 (/ (- reading low) (- high low))))))

(defun highland-landscape-height (x y size &key (seed 121))
  "Height of the authored highland at X,Y.

The invariant is that every reading is a pure function of X, Y, SIZE, and
SEED. Low-frequency domain warping bends a zero-contour into mountain chains;
independent fields add foothills, a basin, a river valley, and a terraced
upland instead of repeating one periodic profile."
  (let* ((warp-x
           (+ x (* 23.0d0 (landscape-value-noise x y 113 seed 40))))
         (warp-y
           (+ y (* 23.0d0 (landscape-value-noise x y 127 seed 41))))
         (continent (landscape-value-noise warp-x warp-y 211 seed 0))
         (fold (abs (landscape-value-noise warp-x warp-y 103 seed 7)))
         (ridge (expt (max 0.0d0 (- 1.0d0 (* 1.45d0 fold))) 1.55d0))
         (mountain-country
           (+ 0.3d0
              (* 0.7d0
                 (landscape-ramp
                  -0.35d0 0.55d0
                  (landscape-value-noise x y 237 seed 8)))))
         (foothills (landscape-value-noise warp-x warp-y 47 seed 2))
         (detail (landscape-value-noise x y 17 seed 3))
         (western-distance
           (sqrt (+ (expt (/ (- x (* size 0.27d0)) (* size 0.31d0)) 2)
                    (expt (/ (- y (* size 0.58d0)) (* size 0.40d0)) 2))))
         (eastern-distance
           (sqrt (+ (expt (/ (- x (* size 0.72d0)) (* size 0.24d0)) 2)
                    (expt (/ (- y (* size 0.30d0)) (* size 0.30d0)) 2))))
         (massif
           (max (expt (max 0.0d0 (- 1.0d0 western-distance)) 1.4d0)
                (* 0.78d0
                   (expt (max 0.0d0 (- 1.0d0 eastern-distance)) 1.3d0))))
         (river-centre
           (+ (* size 0.46d0)
              (* size 0.13d0
                 (landscape-value-noise x 0 139 seed 19))))
         (river-width (+ 7.0d0 (* size 0.018d0)))
         (river
           (expt (max 0.0d0
                      (- 1.0d0 (/ (abs (- y river-centre)) river-width)))
                 2))
         (basin-x (* size 0.24d0))
         (basin-y (* size 0.73d0))
         (basin-distance
           (sqrt (+ (expt (- x basin-x) 2) (expt (- y basin-y) 2))))
         (basin
           (expt (max 0.0d0
                      (- 1.0d0 (/ basin-distance (* size 0.22d0))))
                 2))
         (raw
           (- (+ 10.0d0 (* 5.0d0 continent)
                 (* 30.0d0 ridge mountain-country)
                 (* massif (+ 22.0d0 (* 10.0d0 ridge)))
                 (* 4.5d0 foothills) (* 2.6d0 detail))
              (* 9.0d0 river) (* 7.0d0 basin)))
         (plateau
           (landscape-ramp
            0.18d0 0.62d0
            (+ (landscape-value-noise x y 151 seed 29)
               (* 0.55d0 (/ (- (+ x y) size) size)))))
         (terraced (* 3.0d0 (round (/ raw 3.0d0))))
         (height
           (interpolate-landscape-reading raw terraced (* 0.35d0 plateau))))
    (max 2 (min 72 (round height)))))

(defun build-highland-lookout (builder x y base &key (citadel-p nil))
  "Build one sparse limestone lookout; CITADEL-P makes the regional anchor."
  (let ((platform-radius (if citadel-p 13 5))
        (tower-inner (if citadel-p 5 2))
        (tower-outer (if citadel-p 8 4))
        (tower-height (if citadel-p 15 10)))
    (scene-builder-disc builder x y platform-radius
                        (1- base) base :architecture-p t)
    (scene-builder-ring builder x y tower-inner tower-outer
                        (1+ base) (+ base tower-height) :architecture-p t)
    (scene-builder-ring builder x y tower-inner (1+ tower-outer)
                        (+ base tower-height 1) (+ base tower-height 2)
                        :architecture-p t)
    (unless citadel-p
      (scene-builder-disc builder x y tower-outer
                          (+ base tower-height 2) (+ base tower-height 2)
                          :architecture-p t))
    (when citadel-p
      (scene-builder-carve-arch
       builder x (+ base 1) (+ base 5) 2
       (cons (- y tower-outer) (+ y tower-outer)) :axis :x)
      (scene-builder-carve-arch
       builder y (+ base 1) (+ base 5) 2
       (cons (- x tower-outer) (+ x tower-outer)) :axis :y))))

(defun make-highland-sanctuary-scene
    (&key (horizontal-bits 9) (seed 121) (streaming-p t))
  "Make a large varied landscape with mountain chains and sparse ruins.

The default 512 by 512 world spans sixty-four LUFT chunks. STREAMING-P wraps
it in a seven-by-seven camera-driven window whose owners share one exact bevel
policy; distance geometry awaits an explicit seam-transition representation."
  (let* ((builder (make-scene-builder :horizontal-bits horizontal-bits))
         (size (ash 1 horizontal-bits)))
    (dotimes (x size)
      (dotimes (y size)
        (let* ((height (highland-landscape-height x y size :seed seed))
               (rock-depth (max 0 (min 6 (floor (- height 20) 3)))))
          (dotimes (z height)
            (scene-builder-cell
             builder x y z
             :material
             (if (>= z (- height rock-depth))
                 *highland-rock-material-placement*
                 *terrain-material-placement*))))))
    ;; Deliberately sparse, asymmetrical anchors replace the old tower in every
    ;; chunk. Their different scales make travel across the terrain legible.
    (dolist (landmark '((0.27d0 0.29d0 nil)
                        (0.71d0 0.68d0 t)
                        (0.79d0 0.24d0 nil)))
      (destructuring-bind (fraction-x fraction-y citadel-p) landmark
        (let* ((x (round (* size fraction-x)))
               (y (round (* size fraction-y)))
               (base (highland-landscape-height x y size :seed seed)))
          (build-highland-lookout builder x y base :citadel-p citadel-p))))
    (let ((scene (finish-scene-builder builder)))
      (if streaming-p
          (make-streaming-scene scene :residency-radius 3)
          scene))))
