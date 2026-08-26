(in-package #:luft.render)

(defparameter *wireframe* 0.0
  "Global construction-edge strength.  The atelier toggles it between 0 and 1.")

(defparameter *render-scale* 0.75
  "Linear internal resolution of the LUFT scene before temporal upscaling.")

(defparameter *temporal-upscaling-p* t
  "Whether LUFT uses MetalFX temporal reconstruction on Metal devices.")

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
  ((solid :initarg :solid :reader scene-solid)
   (material-vocabulary :initarg :material-vocabulary
                        :reader scene-material-vocabulary)
   (material-cells :initarg :material-cells :reader scene-material-cells)
   (material-program :initarg :material-program
                     :reader scene-material-program)
   (authored-light-sources
    :initarg :authored-light-sources :reader scene-authored-light-sources)
   (authored-light-opacity-table
    :initarg :authored-light-opacity-table
    :reader scene-authored-light-opacity-table)
   (authored-light-revision
    :initarg :authored-light-revision :reader scene-authored-light-revision)
   (authored-light-provenance
    :initarg :authored-light-provenance
    :reader scene-authored-light-provenance)
   (authored-light-generation
    :initarg :authored-light-generation
    :reader scene-authored-light-generation)
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
   "One authored solid and its vocabulary-closed material-placement field.

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
   :limit #x10000))

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
             (active-placement-offsets
               (sort
                (remove-duplicates
                 (loop for offset being the hash-values of material-cells
                       collect offset)
                 :test #'=)
                #'<))
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
         :material-program
         (make-material-program
          material-vocabulary
          :active-placement-offsets active-placement-offsets)
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
            (loop for z below
                  (max 1 (mountain-sanctuary-terrain-height x y)) do
              (scene-builder-cell builder x y z))))))
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

(defun make-material-bevel-transition-study-scene ()
  "Build the five-cell medial T-junction regression. #WSEK3C

Two ascending architectural columns meet the corner of a two-cell terrain
column.  Widths one, two, and four occur in one tiny surface; the width-four
medial collapse leaves exactly one long-edge/short-edge T-junction for the
site-local contraction pass to resolve."
  (let ((builder (make-scene-builder :horizontal-bits 4)))
    (scene-builder-box builder 6 6 4 4 2 3)
    (scene-builder-cell builder 5 4 2 :architecture-p t)
    (scene-builder-box builder 5 5 5 5 2 3 :architecture-p t)
    (finish-scene-builder builder)))

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

(defun scene-foundation-cell-p (scene cell)
  "Whether architectural CELL is immediately borne by non-architectural earth."
  (let ((z (luft:site-z cell)))
    (when (plusp z)
      (let ((below
              (luft:make-site
               (luft:chain-domain (scene-solid scene))
               (luft:site-x cell) (luft:site-y cell) (1- z)
               luft:+cell-extent+ 1)))
        (and (= 1 (luft:chain-cell-occupancy-bit
                   (scene-solid scene)
                   (luft:site-x below) (luft:site-y below) (luft:site-z below)))
             (multiple-value-bind (offset present-p)
                 (gethash below (scene-material-cells scene))
               (and present-p
                    (logtest
                     +material-placement-earth-flag+
                     (aref
                      (material-program-placement-flags
                       (scene-material-program scene))
                      offset)))))))))

(defun scene-material-placement-at (scene cell)
  "Return the authored placement at occupied CELL in SCENE."
  (multiple-value-bind (offset present-p)
      (gethash cell (scene-material-cells scene))
    (unless present-p
      (error "Occupied scene cell ~S has no authored material placement." cell))
    (domains:identity-vocabulary-member
     (scene-material-vocabulary scene) offset)))

(defun scene-face-reading (scene face)
  "Derive FACE's semantic reading without allocating a per-face object."
  (multiple-value-bind (cell axis side)
      (face-solid-cell (scene-solid scene) face)
    (let ((placement (scene-material-placement-at scene cell)))
      (material-face-reading (material-placement-kind placement)
                             placement scene cell axis side))))

(defun make-scene-face-stock-function (scene)
  "Capture SCENE's dense material tables for source-provenant boundaries."
  (let* ((semantic-solid (scene-solid scene))
         (domain (luft:chain-domain semantic-solid))
         (material-cells (scene-material-cells scene))
         (program (scene-material-program scene))
         (placement-flags
           (the (simple-array (unsigned-byte 8) (*))
                (material-program-placement-flags program)))
         (face-stocks
           (the (simple-array (unsigned-byte 16) (*))
                (material-program-placement-face-stocks program))))
    (labels ((foundation-p (cell)
               (let ((z (luft:site-z cell)))
                 (when (plusp z)
                   (let ((below
                           (luft:make-site
                            domain
                            (luft:site-x cell) (luft:site-y cell) (1- z)
                            luft:+cell-extent+ 1)))
                     (and (= 1 (luft:chain-cell-occupancy-bit
                                semantic-solid
                                (luft:site-x below)
                                (luft:site-y below)
                                (luft:site-z below)))
                          (multiple-value-bind (offset present-p)
                              (gethash below material-cells)
                            (and present-p
                                 (logtest
                                  +material-placement-earth-flag+
                                  (aref placement-flags offset))))))))))
      (lambda (face cell axis side)
        (declare (optimize (speed 3) (safety 1)))
        (declare (ignore face))
        (multiple-value-bind (placement-offset present-p)
            (gethash cell material-cells)
          (unless present-p
            (error "Occupied scene cell ~S has no authored material placement."
                   cell))
          (let* ((flags (aref placement-flags placement-offset))
                 (face-index
                   (if (and
                        (logtest +material-placement-architecture-flag+
                                 flags)
                        (foundation-p cell))
                       6
                       (+ (* (ecase axis (:x 0) (:y 1) (:z 2)) 2)
                          (if (eq side :forward) 1 0)))))
            (aref face-stocks
                  (+ (* placement-offset
                        +material-placement-face-stride+)
                     face-index))))))))

(defun scene-face-stock (scene face)
  "The current packed assembly offset for FACE in SCENE."
  (multiple-value-bind (cell axis side)
      (face-solid-cell (scene-solid scene) face)
    (funcall (make-scene-face-stock-function scene)
             face cell axis side)))

(defun scene-chamfer-stock (stocks &optional material-program)
  "Resolve one whole chamfer from its incident face STOCKS.

The paper palette's terrain top is grass, terrain side is soil, and terrain
underside is dark soil.  A unanimous closure continues that face material;
a mixed terrain chamfer exposes soil.  Stone--terrain chamfers retain the
deepest incident substrate, so the shader can weather a turf line differently
from an exposed or buried foundation without adding per-site material objects."
  (if material-program
      (compiled-material-chamfer-stock material-program stocks)
      (surface-assembly-offset
       (closure-surface-assembly (mapcar #'surface-assembly-at stocks)))))

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
         (body-stock (surface-assembly-offset *torch-body-surface*))
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
     (pack-torch-body-frame-flags body-stock packed-light)
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
       (realize-torch-light-p t))
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
           (resolve-unlit-scene-torch-frames
            owners scene surface-context attachment-source-owners
            attachment-source-owners-p))
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
    (labels ((initialize (surface)
               (setf (luft:surface-mesh-voxel-light surface) field
                     (luft:surface-mesh-attachments surface) nil)
               (dolist (companion (luft:surface-mesh-companions surface))
                 (initialize companion))))
      (dolist (entry owners) (initialize (cdr entry))))
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
      scene request-stamp light-generation :mesh-entries owners))))

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

(defun %make-scene-union-mesh
    (scene solid bevel-width stock-function chamfer-stock-function)
  "Build SCENE's one undecorated closed occupied-union boundary."
  (luft:make-surface-mesh
   solid
   :stock-function (or stock-function (constantly 0))
   :source-stock-function
   (unless stock-function (make-scene-face-stock-function scene))
   :chamfer-stock-function chamfer-stock-function
   :bevel-width bevel-width))

(declaim (ftype function make-scene-regional-meshes))

(defun scene-regional-mesh-tree
    (scene bevel-width &optional bevel-profile
     &key reusable-light-generation)
  "Return the common regional compiler's meshes as one borrow-only tree."
  (multiple-value-bind (owners census diagnostics generation)
      (make-scene-regional-meshes
       scene bevel-width bevel-profile
       :reusable-light-generation reusable-light-generation)
    (if owners
        (let* ((meshes (mapcar #'cdr owners))
               (root
                 (or (find-if
                      (lambda (mesh)
                        (plusp (luft:surface-mesh-triangle-count mesh)))
                      meshes)
                     (first meshes))))
          (let ((companions (remove root meshes :test #'eq)))
            (when companions
              (setf (luft:surface-mesh-companions root)
                    (append (luft:surface-mesh-companions root) companions))))
          ;; ROOT has just acquired the other canonical owner trees as
          ;; companions.  Rebuild its unkeyed manifest after that mutation;
          ;; the worker's keyed owner manifest no longer describes this public
          ;; single-tree product.
          (values
           root census diagnostics
           (make-scene-mesh-generation-value
            scene (scene-mesh-generation-request-stamp generation)
            (scene-mesh-generation-light-generation generation)
            :mesh-entries (list (cons +unkeyed-scene-mesh-output+ root))
            :unkeyed-mesh-p t)))
        (multiple-value-bind (root empty-generation)
            (decorate-scene-mesh
             (%make-scene-union-mesh
              scene (scene-solid scene) bevel-width nil
              (make-compiled-material-chamfer-stock-function
               (scene-material-program scene)))
             scene)
          (values root census diagnostics empty-generation)))))

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
           scene bevel-width nil
           :reusable-light-generation reusable-light-generation)
        (declare (ignore census diagnostics))
        (values mesh generation)))))

(defun make-whole-domain-diagnostic-mesh
    (source &key stock-function chamfer-stock-function
                 (bevel-width luft:+mesh-bevel-width+))
  "Build the explicit whole-domain chain/scene oracle for diagnostics only."
  (etypecase source
    (scene
     (let* ((chamfer
              (or chamfer-stock-function
                  (make-compiled-material-chamfer-stock-function
                   (scene-material-program source))))
            (mesh
              (%make-scene-union-mesh
               source (scene-solid source) bevel-width stock-function chamfer)))
       (decorate-scene-mesh mesh source)))
    (luft:chain
     (luft:make-surface-mesh
      source :stock-function (or stock-function #'default-face-stock)
      :chamfer-stock-function
      (or chamfer-stock-function (lambda (stocks) (first stocks)))
      :bevel-width bevel-width))))

(defun make-material-bevel-mesh (scene profile)
  "Build one watertight mesh with a semantic material width at each site.

The ordinary width-one mesher supplies one exact topology witness.  PROFILE is
compiled after that build into a dense stock-to-material-mask lane.  Each
canonical lattice vertex ORs the masks of all incident stocks: terrain-only,
architecture-only, and mixed stars select the profile's terrain, architecture,
and contact widths.  The unchanged witness triangles form the transitions.

The second value is a five-entry vector counting sites at widths zero through
four; the third is repair diagnostics; the fourth is the immutable realized
SCENE-MESH-GENERATION.  Production always contracts T-junctions."
  (check-type scene scene)
  (check-type profile material-bevel-profile)
  (scene-regional-mesh-tree scene 1 profile))

(defun make-uncontracted-material-bevel-diagnostic-mesh (scene profile)
  "Build the deliberately open whole-domain pre-contraction bevel oracle."
  (check-type scene scene)
  (check-type profile material-bevel-profile)
  (let* ((solid (scene-solid scene))
         (chamfer-stock-function
           (make-compiled-material-chamfer-stock-function
            (scene-material-program scene)))
         (witness
           (%make-scene-union-mesh
            scene solid 1 nil chamfer-stock-function)))
    (multiple-value-bind (stock-masks site-widths)
        (compile-material-bevel-site-policy profile)
      (multiple-value-bind (mesh census diagnostic)
          (luft:vary-uncontracted-surface-mesh-bevel-widths-from-stock-masks-diagnostic
           witness stock-masks site-widths)
        (multiple-value-bind (decorated generation)
            (decorate-scene-mesh mesh scene)
          (values decorated census diagnostic generation))))))

(defun make-material-bevel-meshes (scene profile)
  "Return the single site-local material bevel mesh in renderer slot zero."
  (multiple-value-bind (mesh census diagnostics generation)
      (make-material-bevel-mesh scene profile)
    (declare (ignore census diagnostics))
    (values (list (cons 0 mesh)) generation)))

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

(defstruct (render-population
             (:constructor %make-render-population
                 (template-words instance-words light-words
                  opaque-triangle-instance-count opaque-quad-instance-count
                  translucent-triangle-instance-count
                  translucent-quad-instance-count material-vocabulary
                  material-vocabulary-revision material-descriptor-count
                  material-descriptor-words))
             (:copier nil))
  "One compact geometry/light population split into opaque and alpha cohorts.

MATERIAL-VOCABULARY, its revision, count, and copied descriptor words are the
exact immutable dense-material ABI under which every packed assembly ID was
read.  They cross the worker boundary with the population so renderer
publication can reject a divergent ABI before allocation or transactionally
grow an append-compatible GPU descriptor population."
  (template-words #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  (instance-words #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  ;; Two packed u32 words parallel every four-word instance.  Triangles carry
  ;; three RGB4 samples; quads carry their four unique corner samples.
  (light-words #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  (opaque-triangle-instance-count 0 :type (integer 0 *) :read-only t)
  (opaque-quad-instance-count 0 :type (integer 0 *) :read-only t)
  (translucent-triangle-instance-count 0 :type (integer 0 *) :read-only t)
  (translucent-quad-instance-count 0 :type (integer 0 *) :read-only t)
  (material-vocabulary nil :read-only t)
  (material-vocabulary-revision 0 :type (integer 0 *) :read-only t)
  (material-descriptor-count 0 :type (integer 0 *) :read-only t)
  (material-descriptor-words
    #() :type (simple-array single-float (*)) :read-only t))

(defun render-population-triangle-instance-count (population)
  (+ (render-population-opaque-triangle-instance-count population)
     (render-population-translucent-triangle-instance-count population)))

(defun render-population-quad-instance-count (population)
  (+ (render-population-opaque-quad-instance-count population)
     (render-population-translucent-quad-instance-count population)))

(defstruct (resident-population
             (:constructor %make-resident-population
                 (population instance-buffer template-buffer light-buffer
                  bind-group shadow-bind-group))
             (:copier nil))
  "One chunk's CPU population and independently retained GPU realization."
  (population nil :type render-population :read-only t)
  (instance-buffer nil :read-only t)
  (template-buffer nil :read-only t)
  (light-buffer nil :read-only t)
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

(defun %render-light-point-key (x y z)
  (logior z (ash y 12) (ash x 33)))

(defun %render-instance-light-words
    (mesh template-id vertex-count base-x base-y base-z
     point-cache lattice-cache)
  "Return the two-word RGB4 sidecar for one mesh-local instance."
  (let ((field (luft:surface-mesh-voxel-light mesh)))
    ;; Every finished scene owns an immutable field, including source-free
    ;; scenes.  Avoid the exact 64-probe point sampler entirely when that
    ;; field has no materialized pages; the parallel GPU ABI still receives
    ;; two zero words per instance.
    (if (or (null field)
            (zerop (luft:voxel-light-field-page-count field)))
        (values 0 0)
        (let* ((ranges (luft:surface-mesh-template-ranges mesh))
               (vertices (luft:surface-mesh-template-vertex-words mesh))
               (start (aref ranges (* 2 template-id))))
          (labels ((sample (local-index)
                     (let* ((vertex (+ start local-index))
                            (word (* vertex
                                     luft:+mesh-template-vertex-word-count+))
                            (x (+ (* luft:+mesh-cell-size+ base-x)
                                  (- (aref vertices word)
                                     luft:+mesh-template-coordinate-bias+)))
                            (y (+ (* luft:+mesh-cell-size+ base-y)
                                  (- (aref vertices (+ word 1))
                                     luft:+mesh-template-coordinate-bias+)))
                            (z (+ (* luft:+mesh-cell-size+ base-z)
                                  (- (aref vertices (+ word 2))
                                     luft:+mesh-template-coordinate-bias+)))
                            (key (%render-light-point-key x y z)))
                       (multiple-value-bind (light present-p)
                           (gethash key point-cache)
                         (if present-p
                             light
                             (setf (gethash key point-cache)
                                   (luft:voxel-light-at-mesh-point
                                    field x y z lattice-cache)))))))
            (declare (inline sample))
            (let ((sample-0 (sample 0))
                  (sample-1 (sample 1))
                  (sample-2 (sample 2))
                  (sample-3 (if (= vertex-count 3) 0 (sample 5))))
              (values (logior sample-0 (ash sample-1 16))
                      (logior sample-2 (ash sample-3 16)))))))))

(defun make-render-population (meshes)
  "Canonicalize MESHES into geometry, colored-light, and render-class runs.

Templates remain interned and padded to six vertices.  Instances and their
two-word light sidecars are laid out as opaque triangles, opaque quads,
translucent triangles, then translucent quads.  Each class therefore needs at
most two direct instanced draws, while shared exact world points receive the
same cached trilinear voxel-light sample across bevel primitives."
  (let ((template-index (make-hash-table :test #'equalp))
        (template-words
          (make-array 256 :element-type '(unsigned-byte 32)
                          :adjustable t :fill-pointer 0))
        (instance-runs
          (make-array
           4 :initial-contents
           (loop repeat 4 collect
             (make-array 256 :element-type '(unsigned-byte 32)
                             :adjustable t :fill-pointer 0))))
        (light-runs
          (make-array
           4 :initial-contents
           (loop repeat 4 collect
             (make-array 128 :element-type '(unsigned-byte 32)
                             :adjustable t :fill-pointer 0))))
        (material-vocabulary *surface-assembly-vocabulary*)
        (material-vocabulary-revision
          (domains:identity-vocabulary-revision
           *surface-assembly-vocabulary*))
        ;; IDENTITY-VOCABULARY-MEMBERS is deliberately a borrowed adjustable
        ;; vector.  A render population crosses an ownership boundary, so its
        ;; dense interpretation must instead be a closed snapshot.
        (assemblies
          (copy-seq
           (domains:identity-vocabulary-members
            *surface-assembly-vocabulary*)))
        (material-descriptor-words
          (surface-assembly-descriptor-words
           *surface-assembly-vocabulary*))
        (render-classes nil)
        (counts (make-array 4 :initial-element 0)))
    ;; Collapse semantic opacity to one dense render-class byte before the
    ;; instance loop.  The vocabulary is frozen for this population build.
    (setf render-classes
          (map '(simple-array (unsigned-byte 8) (*))
               (lambda (assembly)
                 (if (surface-assembly-translucent-p assembly) 1 0))
               assemblies))
    (labels ((intern-template (mesh template-id)
               (let* ((ranges (luft:surface-mesh-template-ranges mesh))
                      (vertices (luft:surface-mesh-template-vertex-words mesh))
                      (range-offset (* 2 template-id))
                      (vertex-start (aref ranges range-offset))
                      (vertex-count (aref ranges (1+ range-offset)))
                      (word-start
                        (* vertex-start
                           luft:+mesh-template-vertex-word-count+))
                      (word-count
                        (* vertex-count
                           luft:+mesh-template-vertex-word-count+))
                      (key (make-array (1+ word-count)
                                       :element-type '(unsigned-byte 32))))
                 (unless (member vertex-count '(3 6))
                   (error "LUFT render template ~D has unsupported arity ~D."
                          template-id vertex-count))
                 (setf (aref key 0) vertex-count)
                 (replace key vertices :start1 1 :start2 word-start
                                       :end2 (+ word-start word-count))
                 (multiple-value-bind (global-id present-p)
                     (gethash key template-index)
                   (unless present-p
                     (setf global-id
                           (/ (fill-pointer template-words)
                              (* +render-template-vertex-count+
                                 luft:+mesh-template-vertex-word-count+)))
                     (unless (typep global-id '(unsigned-byte 16))
                       (error "LUFT render template vocabulary exceeds 16 bits."))
                     (loop for index from 1 below (length key)
                           do (vector-push-extend (aref key index)
                                                  template-words))
                     (loop repeat (- (* +render-template-vertex-count+
                                        luft:+mesh-template-vertex-word-count+)
                                     word-count)
                           do (vector-push-extend 0 template-words))
                     (setf (gethash key template-index) global-id))
                   (values global-id vertex-count))))
             (append-stream
                 (mesh words global-ids vertex-counts
                  point-cache lattice-cache)
               (loop for offset from 0 below (length words)
                       by luft:+mesh-instance-word-count+
                     for packed = (aref words (+ offset 3))
                     for local-id = (ldb (byte 16 0) packed)
                     for assembly-id =
                       (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                            packed)
                     for global-id = (aref global-ids local-id)
                     for vertex-count = (aref vertex-counts local-id)
                     do (unless (< assembly-id (length assemblies))
                          (error "LUFT surface assembly ~D is outside the resident vocabulary of ~D entries."
                                 assembly-id (length assemblies)))
                        (let* ((translucent-p
                                 (= 1 (aref render-classes assembly-id)))
                               (run (+ (if translucent-p 2 0)
                                       (if (= vertex-count 3) 0 1)))
                               (destination (aref instance-runs run))
                               (light-destination (aref light-runs run))
                               (base-x (aref words offset))
                               (base-y (aref words (+ offset 1)))
                               (base-z (aref words (+ offset 2))))
                          (loop for word-offset below 3
                                do (vector-push-extend
                                    (aref words (+ offset word-offset))
                                    destination))
                          (vector-push-extend
                           (logior global-id (logand packed #xffff0000))
                           destination)
                          (multiple-value-bind (light-0 light-1)
                              (%render-instance-light-words
                               mesh local-id vertex-count base-x base-y base-z
                               point-cache lattice-cache)
                            (vector-push-extend light-0 light-destination)
                            (vector-push-extend light-1 light-destination))
                          (incf (aref counts run)))))
             (append-mesh (mesh)
               ;; Mesh-local template IDs are dense. Resolve each one exactly
               ;; once, then the large instance streams become a linear copy.
               (let* ((template-count
                        (/ (length (luft:surface-mesh-template-ranges mesh)) 2))
                      (global-ids
                        (make-array template-count
                                    :element-type '(unsigned-byte 16)))
                      (vertex-counts
                        (make-array template-count
                                    :element-type '(unsigned-byte 8)))
                      (point-cache (make-hash-table :test #'eql))
                      (lattice-cache (make-hash-table :test #'eql)))
                 (dotimes (local-id template-count)
                   (multiple-value-bind (global-id vertex-count)
                       (intern-template mesh local-id)
                     (setf (aref global-ids local-id) global-id
                           (aref vertex-counts local-id) vertex-count)))
                 (append-stream
                  mesh (luft:surface-mesh-face-instance-words mesh)
                  global-ids vertex-counts point-cache lattice-cache)
                 (append-stream
                  mesh (luft:surface-mesh-band-instance-words mesh)
                  global-ids vertex-counts point-cache lattice-cache)
                 (append-stream
                 mesh (luft:surface-mesh-fan-instance-words mesh)
                  global-ids vertex-counts point-cache lattice-cache)
                 (dolist (companion (luft:surface-mesh-companions mesh))
                   (append-mesh companion)))))
      (dolist (mesh meshes) (append-mesh mesh)))
    (unless (= (length material-descriptor-words)
               (* (length assemblies)
                  +surface-assembly-descriptor-row-count+ 4))
      (error "Surface assembly descriptor compilation does not match the population vocabulary snapshot."))
    (unless (and (eq material-vocabulary *surface-assembly-vocabulary*)
                 (= material-vocabulary-revision
                    (domains:identity-vocabulary-revision
                     material-vocabulary)))
      (error "The surface assembly vocabulary changed while a render population was being prepared."))
    (%make-render-population
     (coerce template-words '(simple-array (unsigned-byte 32) (*)))
     (concatenate '(simple-array (unsigned-byte 32) (*))
                  (aref instance-runs 0) (aref instance-runs 1)
                  (aref instance-runs 2) (aref instance-runs 3))
     (concatenate '(simple-array (unsigned-byte 32) (*))
                  (aref light-runs 0) (aref light-runs 1)
                  (aref light-runs 2) (aref light-runs 3))
     (aref counts 0) (aref counts 1) (aref counts 2) (aref counts 3)
     material-vocabulary material-vocabulary-revision (length assemblies)
     material-descriptor-words)))

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
                  torch-body-shadow-bind-group material-buffer
                  material-vocabulary material-vocabulary-revision
                  material-descriptor-count material-descriptor-words
                  scene-generation))
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
  (material-buffer nil :read-only t)
  (material-vocabulary nil :read-only t)
  (material-vocabulary-revision 0 :type (integer 0 *) :read-only t)
  (material-descriptor-count 0 :type (integer 0 *) :read-only t)
  (material-descriptor-words
    #() :type (simple-array single-float (*)) :read-only t)
  (scene-generation nil :type (or null scene-mesh-generation) :read-only t))

(defun %make-empty-renderer-publication
    (&key flame-instance-buffer torch-body-bind-group
          torch-body-shadow-bind-group material-buffer material-vocabulary
          (material-vocabulary-revision 0) (material-descriptor-count 0)
          (material-descriptor-words
            (make-array 0 :element-type 'single-float)))
  (%make-renderer-publication
   (make-hash-table :test #'eql) nil
   (make-array 0 :element-type 'single-float) 0
   flame-instance-buffer torch-body-bind-group
   torch-body-shadow-bind-group material-buffer material-vocabulary
   material-vocabulary-revision material-descriptor-count
   material-descriptor-words nil))

(defstruct (renderer-target-resources
             (:constructor %make-renderer-target-resources
                 (extent render-extent temporal-scaler
                  depth-texture depth-view scene-texture scene-view
                  motion-texture motion-view resolved-texture resolved-view
                  composite-texture composite-view
                  composite-source-bind-group present-bind-group
                  exposure-probe-bind-group))
             (:copier nil))
  "One immutable output-size-dependent texture/view/resource cohort."
  (extent nil :type list :read-only t)
  (render-extent nil :type list :read-only t)
  (temporal-scaler nil :read-only t)
  (depth-texture nil :read-only t)
  (depth-view nil :read-only t)
  (scene-texture nil :read-only t)
  (scene-view nil :read-only t)
  (motion-texture nil :read-only t)
  (motion-view nil :read-only t)
  (resolved-texture nil :read-only t)
  (resolved-view nil :read-only t)
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
    nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
   (%make-renderer-flame-target-join nil)))

(defmacro define-renderer-target-resource-reader (name)
  `(defun ,(intern (format nil "RENDERER-TARGET-GENERATION-~A" name))
       (generation)
     (,(intern (format nil "RENDERER-TARGET-RESOURCES-~A" name))
      (renderer-target-generation-resources generation))))

(define-renderer-target-resource-reader extent)
(define-renderer-target-resource-reader render-extent)
(define-renderer-target-resource-reader temporal-scaler)
(define-renderer-target-resource-reader depth-texture)
(define-renderer-target-resource-reader depth-view)
(define-renderer-target-resource-reader scene-texture)
(define-renderer-target-resource-reader scene-view)
(define-renderer-target-resource-reader motion-texture)
(define-renderer-target-resource-reader motion-view)
(define-renderer-target-resource-reader resolved-texture)
(define-renderer-target-resource-reader resolved-view)
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
    (loop for run in runs
          for offset = 0 then (+ offset (length run))
          do (replace data run :start1 offset))
    data))

(defclass renderer ()
  ((device :initarg :device :reader renderer-device)
   ;; One owner-thread pointer swap publishes the complete keyed residency and
   ;; all resources derived from it.  Readers never observe a table paired
   ;; with another generation's order or attachment buffers.
   (publication :initarg :publication :accessor renderer-publication)
   (camera-buffer :initarg :camera-buffer :accessor renderer-camera-buffer)
   (layout :initarg :layout :accessor renderer-layout)
   (vertex-module :initarg :vertex-module :accessor renderer-vertex-module)
   (fragment-module :initarg :fragment-module :accessor renderer-fragment-module)
   (pipeline :initarg :pipeline :accessor renderer-pipeline)
   (translucent-pipeline :initarg :translucent-pipeline
                         :accessor renderer-translucent-pipeline)
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

(defun renderer-depth-view (renderer)
  (renderer-target-generation-depth-view
   (renderer-target-generation renderer)))

(defun renderer-scene-texture (renderer)
  (renderer-target-generation-scene-texture
   (renderer-target-generation renderer)))

(defun renderer-scene-view (renderer)
  (renderer-target-generation-scene-view
   (renderer-target-generation renderer)))

(defun renderer-motion-texture (renderer)
  (renderer-target-generation-motion-texture
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

(defun renderer-material-buffer (renderer)
  (renderer-publication-material-buffer (renderer-publication renderer)))

(defun renderer-material-vocabulary (renderer)
  (renderer-publication-material-vocabulary (renderer-publication renderer)))

(defun renderer-material-vocabulary-revision (renderer)
  (renderer-publication-material-vocabulary-revision
   (renderer-publication renderer)))

(defun renderer-material-descriptor-count (renderer)
  (renderer-publication-material-descriptor-count
   (renderer-publication renderer)))

(defun renderer-material-descriptor-words (renderer)
  (renderer-publication-material-descriptor-words
   (renderer-publication renderer)))

(defun %make-renderer-flame-resources
    (renderer source &optional (material-buffer
                                 (renderer-material-buffer renderer)))
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
                            (:binding 3
                             :resource ,material-buffer)
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

(defun metal-temporal-device-p (device)
  #+darwin (and *temporal-upscaling-p* (typep device 'metal-gpu-device))
  #-darwin (declare (ignore device))
  #-darwin nil)

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
             (renderer-target-resources-temporal-scaler resources)
             (renderer-target-resources-composite-view resources)
             (renderer-target-resources-resolved-view resources)
             (renderer-target-resources-motion-view resources)
             (renderer-target-resources-scene-view resources)
             (renderer-target-resources-depth-view resources)
             (renderer-target-resources-composite-texture resources)
             (renderer-target-resources-resolved-texture resources)
             (renderer-target-resources-motion-texture resources)
             (renderer-target-resources-scene-texture resources)
             (renderer-target-resources-depth-texture resources)))
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

(defun make-renderer-target-generation (renderer extent)
  "Stage one complete output-size generation without publishing it."
  (let* ((device (renderer-device renderer))
         (temporal-p (renderer-temporal-p renderer))
         ;; These owned copies are the immutable dimensions of the candidate.
         ;; Validate/list-copy before the first GPU allocation.
         (extent (copy-list extent))
         (render-extent (render-scale-extent extent))
         scaler depth depth-view scene scene-view motion motion-view
         resolved resolved-view composite composite-view
         composite-source-group flame-group present-group exposure-probe-group
         resource-cohort flame-join generation
         (completed-p nil))
    (labels ((usage (base extra)
               (remove-duplicates (append base extra)))
             (cleanup-locals ()
               (dolist (resource
                         (list present-group exposure-probe-group flame-group
                               composite-source-group scaler composite-view
                               resolved-view motion-view scene-view depth-view
                               composite resolved motion scene depth))
                 (when resource (ignore-errors (destroy resource))))))
      (unwind-protect
           (progn
             (when temporal-p
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
                    device (make-texture-view-descriptor :texture scene)))
             (when temporal-p
               (setf motion
                     (create
                      device
                      (make-texture-descriptor
                       :label "luft temporal motion" :size render-extent
                       :dimensions :2d :format :rg16-float
                       :usage
                       (usage '(:render-attachment)
                              (gpu-temporal-scaler-motion-usage scaler))))
                     motion-view
                     (create
                      device (make-texture-view-descriptor :texture motion))
                     resolved
                     (create
                      device
                      (make-texture-descriptor
                       :label "luft temporal resolve" :size extent
                       :dimensions :2d :format :rgba16-float
                       :usage
                       (usage '(:texture-binding)
                              (gpu-temporal-scaler-output-usage scaler))))
                     resolved-view
                     (create
                      device (make-texture-view-descriptor :texture resolved))))
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
                    extent render-extent scaler depth depth-view scene scene-view
                    motion motion-view resolved resolved-view composite
                    composite-view composite-source-group present-group
                    exposure-probe-group)
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

(defun %make-renderer-mesh-slot
    (renderer mesh-or-prepared provenance
     &optional (material-buffer (renderer-material-buffer renderer)))
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
                  renderer (prepared-render-mesh-population prepared)
                  material-buffer))
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
                            (resident-population-light-buffer resident)
                            (resident-population-template-buffer resident)
                            (resident-population-instance-buffer resident)))
      (when resource (ignore-errors (destroy resource)))))
  (values))

(zdefun (%upload-render-population :zone :luft/upload-slot)
    (renderer population &optional (material-buffer
                                     (renderer-material-buffer renderer)))
  "Build and upload one candidate population without changing RENDERER."
  (let* ((device (renderer-device renderer))
         (instance-words (render-population-instance-words population))
         (template-words (render-population-template-words population))
         (light-words (render-population-light-words population))
         instance-buffer template-buffer light-buffer bind-group
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
                   (stream-buffer "luft canonical site templates" template-words)
                   light-buffer
                   (stream-buffer "luft resident voxel-light sidecars" light-words)
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
                              (:binding 3 :resource ,material-buffer)
                              (:binding 4
                               :resource ,(renderer-shadow-view renderer))
                              (:binding 5
                               :resource ,(renderer-shadow-sampler renderer))
                              (:binding 6 :resource ,light-buffer))))
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
                      population instance-buffer template-buffer light-buffer
                      bind-group shadow-bind-group)))
               (setf completed-p t)
               resident))
        (unless completed-p
          (dolist (resource
                    (list shadow-bind-group bind-group light-buffer
                          template-buffer instance-buffer))
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

(defun material-descriptor-prefix-p (prefix whole)
  "Whether immutable float32 descriptor vector PREFIX is an exact WHOLE prefix."
  (and (<= (length prefix) (length whole))
       (loop for index below (length prefix)
             always (eql (aref prefix index) (aref whole index)))))

(defun compatible-renderer-material-snapshot (renderer prepared-meshes)
  "Return the longest mutually compatible append-only material snapshot.

The actual descriptor words are the ABI.  Vocabulary object identity may
change across a live source reload, but a retained packed stock remains valid
exactly when its complete old descriptor vector is a prefix of the new one."
  (let ((words (renderer-material-descriptor-words renderer))
        (vocabulary (renderer-material-vocabulary renderer))
        (revision (renderer-material-vocabulary-revision renderer))
        (count (renderer-material-descriptor-count renderer)))
    (dolist (entry prepared-meshes)
      (let* ((population
               (prepared-render-mesh-population (cdr entry)))
             (candidate
               (render-population-material-descriptor-words population)))
        (cond ((material-descriptor-prefix-p candidate words))
              ((material-descriptor-prefix-p words candidate)
               (setf words candidate
                     vocabulary
                     (render-population-material-vocabulary population)
                     revision
                     (render-population-material-vocabulary-revision population)
                     count
                     (render-population-material-descriptor-count population)))
              (t
               (error "Renderer owner ~D has a non-prefix surface assembly descriptor ABI."
                      (car entry))))))
    (values words vocabulary revision count)))

(defun make-renderer-material-buffer (device words)
  "Upload immutable descriptor WORDS, retiring the buffer on write failure."
  (let ((buffer nil)
        (completed-p nil))
    (unwind-protect
         (progn
           (setf buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft surface assembly descriptors"
                          :size (max 16 (* 4 (length words)))
                          :usage '(:storage :copy-dst))))
           (when (plusp (length words))
             (write-buffer buffer words))
           (setf completed-p t)
           buffer)
      (unless completed-p
        (when buffer (ignore-errors (destroy buffer)))))))

(defun retained-renderer-prepared-meshes
    (renderer requested-prepared removed-keys)
  "Borrow retained CPU populations for a material-buffer generation change."
  (loop for key being the hash-keys of (renderer-mesh-slots renderer)
        using (hash-value slot)
        unless (or (assoc key requested-prepared :test #'eql)
                   (member key removed-keys :test #'eql))
          collect (cons key (mesh-slot-prepared-mesh slot))))

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

(defun destroy-renderer-publication-resources
    (publication &key material-buffer-p)
  "Retire only PUBLICATION's global resources; mesh slots have separate sharing."
  (dolist (resource
            (append
             (list (renderer-publication-torch-body-shadow-bind-group publication)
                   (renderer-publication-torch-body-bind-group publication)
                   (renderer-publication-flame-instance-buffer publication))
             (and material-buffer-p
                  (list (renderer-publication-material-buffer publication)))))
    (when resource (ignore-errors (destroy resource))))
  (values))

(zdefun (renderer-update-meshes :zone :luft/publish-residency)
    (renderer meshes removed-keys &key scene-generation)
  "Transactionally replace geometry and its realized torch-frame cohort."
  (validate-renderer-mesh-update meshes removed-keys)
  (when scene-generation
    (check-type scene-generation scene-mesh-generation))
  (let ((prepared-meshes nil)
        (staged-prepared-meshes nil)
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
        (material-buffer nil)
        (material-buffer-owned-p nil)
        (material-vocabulary nil)
        (material-vocabulary-revision 0)
        (material-descriptor-count 0)
        (material-descriptor-words nil)
        (installed-p nil))
    (unless publication-changed-p
      (return-from renderer-update-meshes nil))
    (unwind-protect
         (progn
           ;; CPU canonicalization and exact descriptor-prefix compatibility
           ;; finish before the first candidate buffer is allocated.
           (setf prepared-meshes
                 (prepare-renderer-mesh-candidates meshes))
           (when scene-generation
             (setf semantic-entries
                   (validate-renderer-scene-generation-cohort
                    renderer prepared-meshes removed-keys scene-generation)
                   publication-scene-generation
                   (make-renderer-publication-scene-generation
                    scene-generation semantic-entries)))
           (multiple-value-setq
               (material-descriptor-words material-vocabulary
                material-vocabulary-revision material-descriptor-count)
             (compatible-renderer-material-snapshot renderer prepared-meshes))
           (setf material-buffer-owned-p
                 (not (eq material-descriptor-words
                          (renderer-material-descriptor-words renderer)))
                 material-buffer
                 (if material-buffer-owned-p
                     (make-renderer-material-buffer
                      (renderer-device renderer) material-descriptor-words)
                     (renderer-material-buffer renderer))
                 ;; Bind groups retain a concrete buffer.  A descriptor growth
                 ;; therefore re-realizes every retained immutable population
                 ;; under the candidate buffer before the one-pointer commit.
                 staged-prepared-meshes
                 (if material-buffer-owned-p
                     (append
                      prepared-meshes
                      (retained-renderer-prepared-meshes
                       renderer prepared-meshes removed-keys))
                     prepared-meshes))
           (dolist (entry staged-prepared-meshes)
             (push (cons (car entry)
                         (%make-renderer-mesh-slot
                          renderer (cdr entry)
                          (and scene-generation
                               (third
                                (find (car entry) semantic-entries
                                      :key #'first :test #'eql)))
                          material-buffer))
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
                renderer (mesh-slots-torch-frame-data entries)
                material-buffer))
             (setf staged-publication
                   (%make-renderer-publication
                    table order torch-frame-data flame-count flame-buffer
                    body-group body-shadow-group material-buffer
                    material-vocabulary material-vocabulary-revision
                    material-descriptor-count material-descriptor-words
                    publication-scene-generation))
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
                   staged-target-generation)
             (setf installed-p t)
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
             (destroy-renderer-publication-resources
              old-publication :material-buffer-p material-buffer-owned-p))
           requested-candidates)
      (unless installed-p
        (when staged-target-flame-group
          (ignore-errors (destroy staged-target-flame-group)))
        (when staged-publication
          (destroy-renderer-publication-resources
           staged-publication :material-buffer-p material-buffer-owned-p))
        ;; A condition may occur after GPU staging but before the publication
        ;; record itself is allocated.  Those locals still own the resources.
        (unless staged-publication
          (when body-shadow-group (ignore-errors (destroy body-shadow-group)))
          (when body-group (ignore-errors (destroy body-group)))
          (when flame-buffer (ignore-errors (destroy flame-buffer)))
          (when (and material-buffer-owned-p material-buffer)
            (ignore-errors (destroy material-buffer))))
        (dolist (entry candidates) (%destroy-mesh-slot (cdr entry)))))))

(defun renderer-remove-mesh (renderer key)
  (renderer-update-meshes renderer nil (list key))
  (values))

(defun renderer-clear-meshes (renderer)
  (renderer-update-meshes renderer nil (copy-list (renderer-slot-order renderer)))
  (values))

(defun surface-assembly-descriptor-snapshot ()
  "Freeze the exact semantic ABI and GPU descriptor words for one renderer."
  (let* ((vocabulary *surface-assembly-vocabulary*)
         (revision (domains:identity-vocabulary-revision vocabulary))
         (count
           (length (domains:identity-vocabulary-members vocabulary)))
         (words (surface-assembly-descriptor-words vocabulary)))
    (unless (= (length words)
               (* count +surface-assembly-descriptor-row-count+ 4))
      (error "Surface assembly descriptor compilation returned ~D words for ~D assemblies."
             (length words) count))
    (unless (and (eq vocabulary *surface-assembly-vocabulary*)
                 (= revision
                    (domains:identity-vocabulary-revision vocabulary))
                 (= count
                    (length
                     (domains:identity-vocabulary-members vocabulary))))
      (error "The surface assembly vocabulary changed while its renderer ABI was being frozen."))
    (values words vocabulary revision count)))

(defun make-renderer (device color-format extent)
  "Create the shared LUFT pipeline state; meshes arrive via RENDERER-SET-MESH."
  (let* ((temporal-p (metal-temporal-device-p device))
         (target-formats (if temporal-p
                             '(:rgba16-float :rg16-float)
                             '(:rgba16-float)))
         camera-buffer material-buffer material-descriptor-words
         material-vocabulary material-vocabulary-revision
         material-descriptor-count
         layout
         vertex-module fragment-module pipeline translucent-pipeline
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
         torch-body-shadow-vertex-module torch-body-pipeline
         torch-body-shadow-pipeline
         lattice-point-layout lattice-point-vertex-module
         lattice-point-fragment-module lattice-point-pipeline
         present-layout present-bind-group present-vertex-module
         present-fragment-module present-pipeline sampler
         sky-layout sky-bind-group sky-fragment-module sky-pipeline
         exposure-probe-layout exposure-probe-bind-group
         exposure-probe-texture exposure-probe-view
         exposure-probe-fragment-module exposure-probe-pipeline
         exposure-probe-buffers
         renderer
         (completed-p nil))
    (unwind-protect
         (progn
           (multiple-value-setq
               (material-descriptor-words material-vocabulary
                material-vocabulary-revision material-descriptor-count)
             (surface-assembly-descriptor-snapshot))
           (setf camera-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft frame state"
                          :size 432 :usage '(:uniform :copy-dst)))
                 flame-effect-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft torch flame effect"
                          :size 16 :usage '(:uniform :copy-dst))))
           ;; Publish ownership to the constructor unwind list before the
           ;; first fallible upload touches this resource.
           (write-buffer flame-effect-buffer
                         (torch-flame-effect-uniform-data 0.0))
           (setf flame-instance-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft empty torch flame instances"
                          :size 16 :usage '(:storage :copy-dst)))
                 material-buffer
                 (make-renderer-material-buffer
                  device material-descriptor-words)
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
                          :code (shaders:mesh-fragment-specification)))
                 pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft site stream pipeline" :layout layout
                          :vertex `(:module ,vertex-module)
                          :fragment `(:module ,fragment-module
                                      :targets
                                      ,(mapcar (lambda (format)
                                                 `(:format ,format))
                                               target-formats))
                          :primitive '(:topology :triangle-list)
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled t
                            :depth-compare :less)))
                 translucent-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft translucent site stream pipeline"
                          :layout layout
                          :vertex `(:module ,vertex-module)
                          :fragment
                          `(:module ,fragment-module
                            :targets
                            ,(loop for format in target-formats
                                   for first = t then nil
                                   collect `(:format ,format
                                             ,@(when first
                                                 '(:blend
                                                   :premultiplied-alpha)))))
                          :primitive '(:topology :triangle-list)
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled nil
                            :depth-compare :less)))
                 shadow-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft shadow vertex"
                          :language :mathematical
                          :code (shaders:shadow-vertex-specification)))
                 shadow-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft sun shadow pipeline"
                          :layout shadow-layout
                          :vertex `(:module ,shadow-vertex-module)
                          :primitive '(:topology :triangle-list)
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
                                     (:binding 3 :type :storage-buffer)
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
                 torch-body-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft framed opaque torch bodies"
                          :layout torch-body-layout
                          :vertex `(:module ,torch-body-vertex-module)
                          :fragment
                          `(:module ,fragment-module
                            :targets
                            ,(mapcar (lambda (format) `(:format ,format))
                                     target-formats))
                          :primitive '(:topology :triangle-list)
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
                            (:binding 3 :resource ,material-buffer)
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
           (setf renderer
                 (make-instance 'renderer
                                :device device
                                :color-format color-format
                                :temporal-p temporal-p
                                :camera-buffer camera-buffer
                                :publication
                                (%make-empty-renderer-publication
                                 :flame-instance-buffer flame-instance-buffer
                                 :torch-body-bind-group torch-body-bind-group
                                 :torch-body-shadow-bind-group
                                 torch-body-shadow-bind-group
                                 :material-buffer material-buffer
                                 :material-vocabulary material-vocabulary
                                 :material-vocabulary-revision
                                 material-vocabulary-revision
                                 :material-descriptor-count
                                 material-descriptor-count
                                 :material-descriptor-words
                                 material-descriptor-words)
                                :layout layout
                                :vertex-module vertex-module
                                :fragment-module fragment-module
                                :pipeline pipeline
                                :translucent-pipeline translucent-pipeline
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
                       (list present-pipeline present-fragment-module
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
                                        shadow-texture translucent-pipeline
                                        pipeline fragment-module
                                        vertex-module layout material-buffer
                                        camera-buffer)))
              (when resource (ignore-errors (destroy resource)))))))))

(defun draw-resident-opaque-population (pass resident bind-group)
  "Draw only depth-writing, shadow-casting instances from RESIDENT."
  (let* ((population (resident-population-population resident))
         (triangle-count
           (render-population-opaque-triangle-instance-count population))
         (quad-count
           (render-population-opaque-quad-instance-count population)))
    (when (plusp (+ triangle-count quad-count))
      (set-bind-group pass 0 bind-group)
      (when (plusp triangle-count)
        (draw pass 3 triangle-count))
      (when (plusp quad-count)
        (draw pass 6 quad-count 0 triangle-count)))))

(defun draw-resident-translucent-population (pass resident bind-group)
  "Draw alpha-blended instances after the complete opaque scene."
  (let* ((population (resident-population-population resident))
         (opaque-offset
           (+ (render-population-opaque-triangle-instance-count population)
              (render-population-opaque-quad-instance-count population)))
         (triangle-count
           (render-population-translucent-triangle-instance-count population))
         (quad-count
           (render-population-translucent-quad-instance-count population)))
    (when (plusp (+ triangle-count quad-count))
      (set-bind-group pass 0 bind-group)
      (when (plusp triangle-count)
        (draw pass 3 triangle-count 0 opaque-offset))
      (when (plusp quad-count)
        (draw pass 6 quad-count 0 (+ opaque-offset triangle-count))))))

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
    (renderer encoder surface-texture extent camera-uniform-data
     &key jitter view player-p construction-p overlay-encoder
       (effect-time
         (or *flame-time* (/ (renderer-frame-index renderer) 60.0))))
  (ensure-renderer-extent renderer extent)
  (write-buffer (renderer-camera-buffer renderer) camera-uniform-data)
  (check-type effect-time real)
  (write-buffer
   (renderer-flame-effect-buffer renderer)
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
           (resident-population-shadow-bind-group resident))))
      (when (plusp (renderer-flame-instance-count renderer))
        (set-pipeline shadow-pass
                      (renderer-torch-body-shadow-pipeline renderer))
        (set-bind-group shadow-pass 0
                        (renderer-torch-body-shadow-bind-group renderer))
        (draw shadow-pass (torch-body-vertex-count)
              (renderer-flame-instance-count renderer)))
      (end-pass shadow-pass))
  (prepare-texture encoder (renderer-shadow-texture renderer)
                   :texture-binding)
  (let* ((temporal-p (renderer-temporal-p renderer))
         (color-view (renderer-scene-view renderer))
         (color-attachments
           (if temporal-p
               `((:view ,color-view :load-op :clear :store-op :store
                  :clear-value #(0.0 0.0 0.0 1.0))
                 (:view ,(renderer-motion-view renderer)
                  :load-op :clear :store-op :store
                  :clear-value #(0.0 0.0 0.0 0.0)))
               `((:view ,color-view :load-op :clear :store-op :store
                  :clear-value #(0.0 0.0 0.0 1.0)))))
         (pass
           (begin-render-pass
            encoder
            (make-render-pass-descriptor
             :label "luft site streams"
             :color-attachments color-attachments
             :depth-stencil-attachment
             `(:view ,(renderer-depth-view renderer)
               :depth-load-op :clear
               :depth-store-op :store
               :depth-clear-value 1.0)))))
    ;; The atmosphere is scene-linear world radiance: geometry overwrites it,
    ;; MetalFX reconstructs it, and the exposure probe meters the same pixels
    ;; presentation will grade.
    (set-pipeline pass (renderer-sky-pipeline renderer))
    (set-bind-group pass 0 (renderer-sky-bind-group renderer))
    (draw pass 3)
    (set-pipeline pass (renderer-pipeline renderer))
    (dolist (key (renderer-slot-order renderer))
      (let ((resident
              (mesh-slot-resident
               (gethash key (renderer-mesh-slots renderer)))))
        (draw-resident-opaque-population
         pass resident (resident-population-bind-group resident))))
    (when (plusp (renderer-flame-instance-count renderer))
      (set-pipeline pass (renderer-torch-body-pipeline renderer))
      (set-bind-group pass 0 (renderer-torch-body-bind-group renderer))
      (draw pass (torch-body-vertex-count)
            (renderer-flame-instance-count renderer)))
    (set-pipeline pass (renderer-translucent-pipeline renderer))
    (dolist (key (renderer-slot-order renderer))
      (let ((resident
              (mesh-slot-resident
               (gethash key (renderer-mesh-slots renderer)))))
        (draw-resident-translucent-population
         pass resident (resident-population-bind-group resident))))
    (when player-p
      (set-pipeline pass (renderer-player-sdf-pipeline renderer))
      (set-bind-group pass 0 (renderer-player-sdf-bind-group renderer))
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
            (set-bind-group pass 0 (mesh-slot-lattice-point-group slot))
            (draw pass 6 (mesh-slot-lattice-point-count slot))))))
    (when temporal-p
      (signal-temporal-scaler-inputs pass
                                     (renderer-temporal-scaler renderer)))
    (end-pass pass)
    (when temporal-p
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
      (when temporal-p
        (wait-temporal-scaler-output
         composite-pass (renderer-temporal-scaler renderer)))
      (set-pipeline composite-pass (renderer-composite-pipeline renderer))
      (set-bind-group composite-pass 0
                      (renderer-composite-source-bind-group renderer))
      (draw composite-pass 3)
      (when (plusp (renderer-flame-instance-count renderer))
        (set-pipeline composite-pass (renderer-flame-pipeline renderer))
        (set-bind-group composite-pass 0 (renderer-flame-bind-group renderer))
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
      (set-bind-group present-pass 0 (renderer-present-bind-group renderer))
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
  (destroy-renderer-targets renderer)
  (loop for slot being the hash-values of (renderer-mesh-slots renderer)
        do (%destroy-mesh-slot slot))
  (destroy-renderer-publication-resources
   (renderer-publication renderer) :material-buffer-p t)
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
                  (renderer-translucent-pipeline renderer)
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
        (renderer-translucent-pipeline renderer) nil
        (renderer-pipeline renderer) nil
        (renderer-fragment-module renderer) nil
        (renderer-vertex-module renderer) nil
        (renderer-layout renderer) nil
        (renderer-camera-buffer renderer) nil)
  (values))

;;; ---------------------------------------------------------------------------
;;; Streaming chunk scenes
;;;
;;; A streaming scene is an ordinary authored scene whose solid is split into
;;; chunk chains. A bounded square window follows the camera. Each focus change
;;; installs the final desired residency first, then remeshes exactly the chunks
;;; whose 3 by 3 dependency neighborhoods changed. MESH-CHUNK's probes into
;;; non-resident neighbors signal MISSING-CHUNK; immutable worker snapshots
;;; answer USE-CHUNK for the captured neighborhood and TREAT-AS-AIR otherwise.
;;; The canvas owner publishes replacements and departures as one complete
;;; cohort, so no frame observes a mixed seam generation.

(defclass streaming-scene (scene)
  ((store :initform (make-hash-table :test #'eql)
          :reader streaming-scene-store)
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
   (geometry-policy-signature
    :initform :uninitialized
    :accessor streaming-scene-geometry-policy-signature)
   (light-generation
    :initarg :light-generation
    :accessor streaming-scene-light-generation)
   (frame-counter :initform 0 :accessor streaming-scene-frame-counter)))

(defstruct (streaming-mesh-snapshot
             (:constructor %make-streaming-mesh-snapshot
                 (scene output-keys witness-keys resident-source-keys
                  bevel-width bevel-profile union-neighborhood stamp
                  realize-torch-light-p reusable-light-generation)))
  "Immutable CPU input for one dependency-closed regional mesh request."
  (scene nil :read-only t)
  (output-keys nil :type list :read-only t)
  (witness-keys nil :type list :read-only t)
  ;; Logical authored residency is deliberately distinct from OUTPUT-KEYS:
  ;; the latter includes virtual canonical owners needed for closed geometry.
  (resident-source-keys nil :type list :read-only t)
  (bevel-width luft:+mesh-bevel-width+ :read-only t)
  (bevel-profile nil :type (or null material-bevel-profile) :read-only t)
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
                    :material-program (scene-material-program scene)
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

(defun reset-streaming-scene-publication (scene)
  "Forget renderer-specific residency while retaining SCENE's immutable store."
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
        (streaming-scene-geometry-policy-signature scene) :uninitialized
        (streaming-scene-light-generation scene)
        (scene-authored-light-generation scene)
        (streaming-scene-frame-counter scene) 0)
  scene)

(defun streaming-scene-keys-near (scene focus-x focus-y)
  "Stored chunk keys inside SCENE's square residency window."
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
        streaming-scene-dependency-guard-keys
        material-bevel-profile-geometry-signature))

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

(defun retarget-streaming-scene
    (scene production-system bevel-width world-x world-y &optional bevel-profile)
  "Batch SCENE's desired window around a camera position and mesh it once.

The camera may stand outside the finite authored world while looking back at
its boundary.  Clamp that position to the nearest domain cell before forming
an unsigned chunk key, so a low-side coordinate cannot wrap to chunk 4095 and
silently empty the desired residency window."
  (when (or (streaming-scene-cohort scene)
            (streaming-scene-removals scene))
    (return-from retarget-streaming-scene nil))
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
         (geometry-policy-signature
           (material-bevel-profile-geometry-signature bevel-profile))
         (geometry-policy-change-p
           (not (equalp
                 geometry-policy-signature
                 (streaming-scene-geometry-policy-signature scene))))
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
               (or geometry-policy-change-p width-changes
                   torch-residency-change-p))
             (realize-torch-light-p
               (not
                (null
                 (or torch-residency-change-p
                     (and desired-torch-p
                          (or geometry-policy-change-p width-changes)))))))
        (setf (streaming-scene-focus scene) focus)
        (when (or changes geometry-policy-change-p)
          (clrhash loaded)
          (dolist (key desired)
            (setf (gethash key loaded) (gethash key desired-widths)))
          (setf (streaming-scene-geometry-policy-signature scene)
                geometry-policy-signature)
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
                scene production-system affected bevel-width bevel-profile
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
                       scene nil bevel-width bevel-profile)
                      (if desired-torch-p
                          (streaming-scene-light-generation scene)
                          (scene-authored-light-generation scene))))))
            t))))))

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

(defun material-bevel-profile-geometry-signature (profile)
  "Return PROFILE's complete dense site policy as immutable stamp data."
  (when profile
    (multiple-value-bind (stock-masks site-widths)
        (compile-material-bevel-site-policy profile)
      (list (copy-seq stock-masks) (copy-seq site-widths)))))

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

(defun streaming-scene-mesh-stamp (scene output-keys bevel-width bevel-profile)
  "Name exact owner, width/profile, authored-light, and torch request inputs."
  (let ((resident-source-keys
          (sort
           (loop for key being the hash-keys of (streaming-scene-loaded scene)
                 collect key)
           #'<)))
    (list :scene-mesh-request-v1
          (copy-list output-keys)
          bevel-width
          (material-bevel-profile-geometry-signature bevel-profile)
          (scene-authored-light-revision scene)
          (scene-authored-light-provenance scene)
          (scene-voxel-light-propagation-p scene)
          (scene-torch-semantics-signature scene resident-source-keys)
          (loop for key in resident-source-keys
                collect (cons key (gethash key
                                          (streaming-scene-loaded scene)))))))

(defun make-streaming-region-snapshot
    (scene output-keys bevel-width &optional bevel-profile
     &key (realize-torch-light-p t))
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
         (store (streaming-scene-store scene))
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
      (setf (gethash key union-neighborhood)
            (if (nth-value 1 (gethash key loaded))
                (gethash key store empty)
                empty)))
    (%make-streaming-mesh-snapshot
     scene output-keys witness-keys resident-source-keys
     bevel-width bevel-profile
     union-neighborhood
     (streaming-scene-mesh-stamp
      scene output-keys bevel-width bevel-profile)
     realize-torch-light-p
     (streaming-scene-light-generation scene))))

(defun make-streaming-mesh-snapshot
    (scene key bevel-width &optional bevel-profile
     &key (realize-torch-light-p t))
  "Capture the one-owner form of MAKE-STREAMING-REGION-SNAPSHOT."
  (make-streaming-region-snapshot
   scene (list key) bevel-width bevel-profile
   :realize-torch-light-p realize-torch-light-p))

(defun mesh-streaming-snapshot (snapshot)
  "Mesh one worker-owned regional snapshot without reading owner state.

The first value is an alist of output owner to final mesh.  A material profile
is evaluated once over all guarded width-one witnesses, so shared sites and
medial-collapse repairs cannot diverge at chunk seams."
  (luft:with-surface-mesh-workspace ()
    (let* ((scene (streaming-mesh-snapshot-scene snapshot))
           (neighborhood (streaming-mesh-snapshot-union-neighborhood snapshot))
           (material-program (scene-material-program scene))
           (chamfer-stock-function
             (make-compiled-material-chamfer-stock-function
              material-program)))
      (labels ((mesh-owner (key width)
                 (let ((chain (gethash key neighborhood)))
                   (unless chain
                     (error "Chunk ~D was not captured by this regional snapshot."
                            key))
                   (zone (:luft/rematerialize :value (luft:chain-count chain))
                     (luft:mesh-chunk
                      chain key
                      :source-stock-function
                      (make-scene-face-stock-function scene)
                      :chamfer-stock-function chamfer-stock-function
                      :chamfer-algebra
                      (material-program-chamfer-algebra material-program)
                      :outside-domain-policy :air
                      :bevel-width width))))
               (decorate-owners (owners &optional surface-context)
                 (decorate-scene-meshes
                  owners scene :surface-context surface-context
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
          (let ((profile (streaming-mesh-snapshot-bevel-profile snapshot)))
            (if profile
                (let ((witnesses
                        (mapcar (lambda (key) (cons key (mesh-owner key 1)))
                                (streaming-mesh-snapshot-witness-keys snapshot)))
                      (output-keys
                        (streaming-mesh-snapshot-output-keys snapshot))
                      (context-keys
                        (set-difference
                         (streaming-mesh-snapshot-witness-keys snapshot)
                         (streaming-mesh-snapshot-output-keys snapshot)
                         :test #'eql)))
                  (multiple-value-bind (stock-masks site-widths)
                      (compile-material-bevel-site-policy profile)
                    (multiple-value-bind
                          (owners census diagnostics surface-context)
                        (luft:vary-surface-mesh-cohort-bevel-widths-from-stock-masks
                         witnesses stock-masks site-widths
                         :output-owners output-keys
                         :realize-context-owners context-keys)
                      (multiple-value-bind (decorated generation)
                          (decorate-owners owners surface-context)
                        (values decorated census diagnostics generation)))))
                (let* ((output-keys
                         (streaming-mesh-snapshot-output-keys snapshot))
                       (all-owners
                         (mapcar
                          (lambda (key)
                            (cons key
                                  (mesh-owner
                                   key
                                   (streaming-mesh-snapshot-bevel-width
                                    snapshot))))
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
                    (values decorated nil nil generation))))))))))

(defun make-scene-regional-meshes
    (scene bevel-width &optional bevel-profile
     &key reusable-light-generation)
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
              streaming owner-keys bevel-width bevel-profile))
          (values
           owners census diagnostics
           (make-scene-mesh-generation-value
            scene (scene-mesh-generation-request-stamp generation)
            (scene-mesh-generation-light-generation generation)
            :mesh-entries owners)))
        (let* ((request-stamp
                 (streaming-scene-mesh-stamp
                  streaming nil bevel-width bevel-profile))
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
          (values nil (and bevel-profile
                           (make-array 5 :element-type '(unsigned-byte 32)
                                         :initial-element 0))
                  (and bevel-profile
                       (list :collapsed-triangle-count 0
                             :unmatched-edge-count 0
                             :repaired-edge-count 0
                             :residual-edge-count 0
                             :candidate-splits nil))
                  generation)))))

(defun mesh-streaming-chunk (scene key bevel-width &optional bevel-profile)
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
            scene output-keys bevel-width bevel-profile)))
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
    (scene production-system output-keys bevel-width bevel-profile priority
     &key (realize-torch-light-p t))
  "Schedule one dependency-guarded regional compilation for OUTPUT-KEYS."
  (let* ((snapshot
         (make-streaming-region-snapshot
            scene output-keys bevel-width bevel-profile
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

(defun schedule-streaming-scene-mesh
    (scene production-system key bevel-width priority &optional bevel-profile
     &key (realize-torch-light-p t))
  "Compatibility wrapper scheduling a one-owner regional cohort."
  (schedule-streaming-scene-cohort
   scene production-system (list key) bevel-width bevel-profile priority
   :realize-torch-light-p realize-torch-light-p))

(defun current-streaming-mesh-request-p (scene request)
  (let ((snapshot (streaming-mesh-request-snapshot request)))
    (and (eq scene (streaming-mesh-snapshot-scene snapshot))
         (equalp (streaming-mesh-snapshot-stamp snapshot)
                 (streaming-scene-mesh-stamp
                  scene
                  (streaming-mesh-snapshot-output-keys snapshot)
                  (streaming-mesh-snapshot-bevel-width snapshot)
                  (streaming-mesh-snapshot-bevel-profile snapshot))))))

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
             (let* ((request (production:production-result-request result))
                    (snapshot (streaming-mesh-request-snapshot request))
                    (keys (streaming-mesh-snapshot-output-keys snapshot))
                    (ticket (production:production-request-ticket request)))
               (when (every (lambda (key)
                              (eql ticket
                                   (gethash key
                                            (streaming-scene-outstanding scene))))
                            keys)
                 (if (production:production-result-condition result)
                     (progn
                       (dolist (key keys)
                         (remhash key (streaming-scene-outstanding scene)))
                       (push result
                             (streaming-scene-production-errors scene))
                       (error "LUFT mesh production for cohort ~S failed: ~A"
                              keys
                              (production:production-result-condition result)))
                     (accept-streaming-mesh-result
                      scene request
                      (production:production-result-value result)))))))
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
