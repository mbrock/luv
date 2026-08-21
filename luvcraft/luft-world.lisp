;;; A finite LUFT scene is the resident rendering materialization of the
;;; authored block world.  The authored world remains the authority for block
;;; identity, persistence, collision, and behavior; this object owns only the
;;; dense solid geometry which LUFT derives into drawable boundary products.

(in-package #:luvcraft)

(defconstant +luft-world-maximum-horizontal-bits+ 9
  "The largest dense LUFT domain a Luvcraft materialization will allocate.

Nine bits are 512 by 512 by 256 cells.  LUFT's scene keeps dense occupancy
and stock words over that whole domain, so accepting its packed site's
twenty-four-bit coordinate capacity here would promise an impossible host
allocation long before a useful resident window existed.")

(defclass luft-world-materialization ()
  ((world
    :initarg :world
    :reader luft-world-materialization-world)
   (domain
    :initarg :domain
    :reader luft-world-materialization-domain)
   (scene
    :initarg :scene
    :reader luft-world-materialization-scene)
   (horizontal-bits
    :initarg :horizontal-bits
    :reader luft-world-materialization-horizontal-bits)
   (vertical-origin
    :initarg :vertical-origin
    :reader luft-world-materialization-vertical-origin)
   (solid-lut-revision
    :initform nil
    :accessor luft-world-materialization-solid-lut-revision)
   (solid-lut
    :initform #*
    :accessor luft-world-materialization-solid-lut
    :documentation
    "BLOCK-SOLID-P materialized once per world-vocabulary revision.")
   (solidity-rebuild-required-p
    :initform nil
    :accessor luft-world-materialization-solidity-rebuild-required-p
    :documentation
    "Whether the next reconciliation must reclassify every resident cell.")
   (resident-chunk-solids
    :initform (make-hash-table :test #'eq)
    :accessor luft-world-materialization-resident-chunk-solids
    :documentation
    "One dense solidity bit vector for each resident authored chunk.")
   (resident-solid-counts
    :initform (make-hash-table :test #'eql)
    :accessor luft-world-materialization-resident-solid-counts
    :documentation
    "The number of resident authored solids at each canonical LUFT cell.")
   (pending-solid-cells
    :initform (make-hash-table :test #'eql)
    :reader luft-world-materialization-pending-solid-cells
    :documentation
    "Canonical LUFT cells dirtied by resident solid-membership changes.")
   (resident-center
    :initform nil
    :accessor luft-world-materialization-resident-center
    :documentation
    "NIL, or a fresh #(WORLD-X WORLD-Z) center for horizontal unwrapping.")
   (resident-window-dirty-p
    :initform t
    :accessor luft-world-materialization-resident-window-dirty-p
    :documentation
    "Whether residency changed since the last validated reconciliation."))
  (:documentation
   "One block world's finite resident solid materialized as one LUFT scene.

Luvcraft remains Y-up.  LUFT is Z-up, so a world cell (X,Y,Z) becomes the
LUFT cell (X,Z,Y-VERTICAL-ORIGIN).  Horizontal coordinates are canonicalized
by DOMAIN's torus; RECONCILE-LUFT-WORLD-MATERIALIZATION first proves that the
whole resident window fits with one separating torus column.  #2TTUQD"))

(defun make-luft-world-materialization
    (world &key (horizontal-bits 8) (vertical-origin 0))
  "Make an unattached, initially empty LUFT materialization for WORLD."
  (check-type world block-world)
  (check-type horizontal-bits (integer 1))
  (when (> horizontal-bits +luft-world-maximum-horizontal-bits+)
    (error "A dense Luvcraft LUFT world supports at most ~D horizontal bits, not ~D."
           +luft-world-maximum-horizontal-bits+ horizontal-bits))
  (check-type vertical-origin integer)
  (let ((domain (luft:make-world-domain :horizontal-bits horizontal-bits)))
    (make-instance 'luft-world-materialization
                   :world world
                   :domain domain
                   :scene (luft.render:make-scene domain)
                   :horizontal-bits horizontal-bits
                   :vertical-origin vertical-origin)))

(defun invalidate-luft-world-solidity (materialization)
  "Make the next reconciliation reapply BLOCK-SOLID-P to all resident cells.

Call this after changing BLOCK-SOLID-P semantics.  The current scene and dense
resident products remain readable until a complete replacement is built and
published by RECONCILE-LUFT-WORLD-MATERIALIZATION."
  (check-type materialization luft-world-materialization)
  (setf (luft-world-materialization-solidity-rebuild-required-p
         materialization)
        t
        (luft-world-materialization-solid-lut-revision materialization)
        nil)
  materialization)

(defun luft-world-cell-site (materialization x y z &optional (polarity 1))
  "Return WORLD cell X,Y,Z as a canonical signed LUFT cell site."
  (check-type materialization luft-world-materialization)
  (luft:make-site
   (luft-world-materialization-domain materialization)
   x z (- y (luft-world-materialization-vertical-origin materialization))
   luft:+cell-extent+ polarity))

(defun luft-world-cell-geometry-if-valid (materialization x y z)
  "Return X,Y,Z's canonical LUFT cell geometry and whether its row is valid."
  (let ((luft-z
          (- y (luft-world-materialization-vertical-origin materialization))))
    (if (<= 0 luft-z (- luft:+vertical-cell-rows+ 2))
        (values
         (luft:site-geometry
          (luft-world-cell-site materialization x y z))
         t)
        (values nil nil))))

(defun change-luft-world-resident-solid-count
    (materialization geometry delta)
  "Apply DELTA to GEOMETRY's exact resident-solid count and dirty it."
  (let* ((counts
           (luft-world-materialization-resident-solid-counts materialization))
         (count (+ (gethash geometry counts 0) delta)))
    (when (minusp count)
      (error "LUFT resident-solid count for ~S became ~D." geometry count))
    (if (zerop count)
        (remhash geometry counts)
        (setf (gethash geometry counts) count))
    (setf (gethash geometry
                   (luft-world-materialization-pending-solid-cells
                    materialization))
          t)))

(defun note-luft-world-cell-count-change (materialization x y z delta)
  "Apply one authored cell's resident-solid DELTA when its LUFT row is valid."
  (multiple-value-bind (geometry valid-p)
      (luft-world-cell-geometry-if-valid materialization x y z)
    (when valid-p
      (change-luft-world-resident-solid-count
       materialization geometry delta))))

(defun make-luft-world-solid-lut-candidate (materialization)
  "Build a complete BLOCK-SOLID-P table, returning it and its source revision."
  (let* ((vocabulary
           (block-world-vocabulary
            (luft-world-materialization-world materialization)))
         (revision (block-vocabulary-revision vocabulary))
         (members (block-vocabulary-members vocabulary))
         (solid (make-array (length members) :element-type 'bit)))
    (dotimes (offset (length members))
      (setf (aref solid offset)
            (if (block-solid-p (aref members offset)) 1 0)))
    (unless (= revision (block-vocabulary-revision vocabulary))
      (error "Block vocabulary changed while its LUFT solidity table was built."))
    (values solid revision)))

(defun luft-world-solid-lut (materialization)
  "Return the world vocabulary's BLOCK-SOLID-P bit table at its revision."
  (let* ((vocabulary
           (block-world-vocabulary
            (luft-world-materialization-world materialization)))
         (revision (block-vocabulary-revision vocabulary)))
    (unless (eql revision
                 (luft-world-materialization-solid-lut-revision
                  materialization))
      (multiple-value-bind (solid candidate-revision)
          (make-luft-world-solid-lut-candidate materialization)
        (setf (luft-world-materialization-solid-lut materialization) solid
              (luft-world-materialization-solid-lut-revision materialization)
              candidate-revision)))
    (luft-world-materialization-solid-lut materialization)))

(defun map-chunk-solid-cells (function chunk solid)
  "Call FUNCTION with world X,Y,Z and dense OFFSET for each solid in CHUNK.

SOLID is the world-vocabulary bit table, built once per vocabulary revision.
The hot scan reads specialized indices and this table directly; it does not
perform CLOS dispatch or construct a coordinate object per cell."
  (check-type function function)
  (check-type chunk block-chunk)
  (with-block-content-storage (domain palette indices) chunk
    (unless (= (length palette) (length solid))
      (error "Chunk palette and LUFT solid table have cardinalities ~D and ~D."
             (length palette) (length solid)))
    (let* ((shape (voxel-space-chunk-shape (chunk-domain-space domain)))
           (width (chunk-shape-width shape))
           (height (chunk-shape-height shape))
           (depth (chunk-shape-depth shape))
           (origin (chunk-domain-origin domain))
           (origin-x (world-coordinate-x origin))
           (origin-y (world-coordinate-y origin))
           (origin-z (world-coordinate-z origin))
           (offset 0))
      (dotimes (local-z depth)
        (dotimes (local-y height)
          (dotimes (local-x width)
            (when (= 1 (aref solid (aref indices offset)))
              (funcall function
                       (+ origin-x local-x)
                       (+ origin-y local-y)
                       (+ origin-z local-z)
                       offset))
            (incf offset))))))
  chunk)

(defstruct (luft-world-solidity-candidate
             (:constructor %make-luft-world-solidity-candidate
                 (lut-revision lut resident-chunk-solids
                  resident-solid-counts)))
  "A complete unpublished replacement for a materialization's solidity state."
  lut-revision
  lut
  resident-chunk-solids
  resident-solid-counts)

(defun add-luft-world-candidate-solid
    (materialization counts x y z)
  "Add one resident solid to unpublished canonical COUNTS."
  (multiple-value-bind (geometry valid-p)
      (luft-world-cell-geometry-if-valid materialization x y z)
    (when valid-p
      (incf (gethash geometry counts 0)))))

(defun build-luft-world-solidity-candidate (materialization)
  "Build complete resident solidity state without mutating MATERIALIZATION."
  (let* ((world (luft-world-materialization-world materialization))
         (world-revision (block-world-revision world))
         (residency-revision (block-world-residency-revision world)))
    (multiple-value-bind (solid revision)
        (make-luft-world-solid-lut-candidate materialization)
      (let ((resident (make-hash-table :test #'eq))
            (counts (make-hash-table :test #'eql)))
        (dolist (chunk
                 (resident-world-chunks world))
          (let ((bits
                  (make-array
                   (chunk-domain-cardinality (block-chunk-domain chunk))
                   :element-type 'bit :initial-element 0)))
            (map-chunk-solid-cells
             (lambda (x y z offset)
               (setf (aref bits offset) 1)
               (add-luft-world-candidate-solid
                materialization counts x y z))
             chunk solid)
            (setf (gethash chunk resident) bits)))
        (unless (= residency-revision (block-world-residency-revision world))
          (error "Block-world residency changed during LUFT solidity rebuild."))
        (unless (= world-revision (block-world-revision world))
          (error "Block-world content changed during LUFT solidity rebuild."))
        (%make-luft-world-solidity-candidate
         revision solid resident counts)))))

(defun luft-world-rebuild-dirty-cells (scene candidate)
  "Return every canonical cell needed to replace SCENE with CANDIDATE."
  (let ((dirty (make-hash-table :test #'eql)))
    (loop for site across (luft:chain-sites (luft.render:scene-solid scene))
          do (setf (gethash (luft:site-geometry site) dirty) t))
    (maphash
     (lambda (geometry count)
       (declare (ignore count))
       (setf (gethash geometry dirty) t))
     (luft-world-solidity-candidate-resident-solid-counts candidate))
    dirty))

(defun publish-luft-world-solidity-candidate (materialization candidate)
  "Install a completely built CANDIDATE at MATERIALIZATION's owner boundary."
  (setf (luft-world-materialization-solid-lut-revision materialization)
        (luft-world-solidity-candidate-lut-revision candidate)
        (luft-world-materialization-solid-lut materialization)
        (luft-world-solidity-candidate-lut candidate)
        (luft-world-materialization-resident-chunk-solids materialization)
        (luft-world-solidity-candidate-resident-chunk-solids candidate)
        (luft-world-materialization-resident-solid-counts materialization)
        (luft-world-solidity-candidate-resident-solid-counts candidate))
  materialization)

(defun queue-luft-world-chunk-solids (materialization chunk solid-p)
  "Add or remove CHUNK's dense solid membership from MATERIALIZATION."
  (let ((resident
          (luft-world-materialization-resident-chunk-solids materialization)))
    (if solid-p
        (progn
          (when (nth-value 1 (gethash chunk resident))
            (error "LUFT materialization already contains resident chunk ~S."
                   chunk))
          (let ((bits
                  (make-array
                   (chunk-domain-cardinality (block-chunk-domain chunk))
                   :element-type 'bit :initial-element 0)))
            (map-chunk-solid-cells
             (lambda (x y z offset)
               (setf (aref bits offset) 1)
               (note-luft-world-cell-count-change
                materialization x y z 1))
             chunk (luft-world-solid-lut materialization))
            (setf (gethash chunk resident) bits)))
        (multiple-value-bind (bits present-p) (gethash chunk resident)
          (unless present-p
            (error "LUFT materialization does not contain departed chunk ~S."
                   chunk))
          (let* ((domain (block-chunk-domain chunk))
                 (origin (chunk-domain-origin domain))
                 (origin-x (world-coordinate-x origin))
                 (origin-y (world-coordinate-y origin))
                 (origin-z (world-coordinate-z origin)))
            (dotimes (offset (length bits))
              (when (= 1 (aref bits offset))
                (multiple-value-bind (local-x local-y local-z)
                    (chunk-domain-local-components domain offset)
                  (note-luft-world-cell-count-change
                   materialization
                   (+ origin-x local-x)
                   (+ origin-y local-y)
                   (+ origin-z local-z)
                   -1)))))
          (remhash chunk resident))))
  chunk)

(defun queue-luft-world-cell
    (materialization chunk x y z solid-p)
  "Update one resident chunk bit and dirty its canonical LUFT cell if needed."
  (multiple-value-bind (bits present-p)
      (gethash chunk
               (luft-world-materialization-resident-chunk-solids
                materialization))
    (unless present-p
      (error "LUFT materialization does not contain edited chunk ~S." chunk))
    (let* ((origin (chunk-domain-origin (block-chunk-domain chunk)))
           (local-x (- x (world-coordinate-x origin)))
           (local-y (- y (world-coordinate-y origin)))
           (local-z (- z (world-coordinate-z origin)))
           (offset
             (chunk-domain-offset-components
              (block-chunk-domain chunk) local-x local-y local-z))
           (desired (if solid-p 1 0))
           (current (aref bits offset)))
      (unless (= desired current)
        (setf (aref bits offset) desired)
        (note-luft-world-cell-count-change
         materialization x y z (if solid-p 1 -1)))))
  materialization)

(defmethod observe-block-world-cell-change
    ((materialization luft-world-materialization)
     (world block-world) chunk x y z)
  (unless (eq world (luft-world-materialization-world materialization))
    (error "LUFT materialization ~S observed its non-owned world ~S."
           materialization world))
  (handler-bind
      ((error
         (lambda (condition)
           (declare (ignore condition))
           (invalidate-luft-world-solidity materialization))))
    (let ((origin (chunk-domain-origin (block-chunk-domain chunk))))
      (queue-luft-world-cell
       materialization chunk x y z
       (block-solid-p
        (chunk-block-at chunk
                        (- x (world-coordinate-x origin))
                        (- y (world-coordinate-y origin))
                        (- z (world-coordinate-z origin)))))))
  materialization)

(defmethod observe-block-world-residency-change
    ((materialization luft-world-materialization)
     (world block-world) chunk event)
  (unless (eq world (luft-world-materialization-world materialization))
    (error "LUFT materialization ~S observed its non-owned world ~S."
           materialization world))
  (setf (luft-world-materialization-resident-window-dirty-p materialization)
        t)
  (handler-bind
      ((error
         (lambda (condition)
           (declare (ignore condition))
           (invalidate-luft-world-solidity materialization))))
    (ecase event
      (:arrived
       (queue-luft-world-chunk-solids materialization chunk t))
      (:departed
       ;; A departed chunk is deliberately still readable to observers.
       (queue-luft-world-chunk-solids materialization chunk nil))))
  materialization)

(defun attach-luft-world-materialization
    (world &key (horizontal-bits 8) (vertical-origin 0))
  "Subscribe a fresh LUFT materialization and queue resident solid cells."
  (let ((materialization
          (make-luft-world-materialization
           world :horizontal-bits horizontal-bits
                 :vertical-origin vertical-origin))
        (completed-p nil))
    ;; Register first: an owner-thread edit made by a future registration hook
    ;; cannot fall into the gap before the initial resident scan.
    (add-block-world-observer world materialization)
    (unwind-protect
         (progn
           (dolist (chunk (resident-world-chunks world))
             (queue-luft-world-chunk-solids materialization chunk t))
           (setf completed-p t)
           materialization)
      (unless completed-p
        (remove-block-world-observer world materialization)))))

(defun detach-luft-world-materialization (materialization)
  "Stop MATERIALIZATION from observing its world, returning it."
  (remove-block-world-observer
   (luft-world-materialization-world materialization) materialization)
  materialization)

(defun resident-luft-world-center (materialization)
  "Validate the resident window and return its #(WORLD-X WORLD-Z) center.

The horizontal check is deliberately over the full bounding spans, including
empty cells between sparse chunks.  A span must be strictly smaller than its
period: injective cell anchors are not enough, because LUFT incidence also
identifies the high boundary of the last column with the first column."
  (let ((minimum-x nil)
        (minimum-y nil)
        (minimum-z nil)
        (maximum-x nil)
        (maximum-y nil)
        (maximum-z nil))
    (dolist (chunk
             (resident-world-chunks
              (luft-world-materialization-world materialization)))
      (let* ((chunk-domain (block-chunk-domain chunk))
             (shape
               (voxel-space-chunk-shape
                (chunk-domain-space chunk-domain)))
             (origin (chunk-domain-origin chunk-domain))
             (low-x (world-coordinate-x origin))
             (low-y (world-coordinate-y origin))
             (low-z (world-coordinate-z origin))
             (high-x (+ low-x (chunk-shape-width shape)))
             (high-y (+ low-y (chunk-shape-height shape)))
             (high-z (+ low-z (chunk-shape-depth shape))))
        (setf minimum-x (if minimum-x (min minimum-x low-x) low-x)
              minimum-y (if minimum-y (min minimum-y low-y) low-y)
              minimum-z (if minimum-z (min minimum-z low-z) low-z)
              maximum-x (if maximum-x (max maximum-x high-x) high-x)
              maximum-y (if maximum-y (max maximum-y high-y) high-y)
              maximum-z (if maximum-z (max maximum-z high-z) high-z))))
    (unless minimum-x
      (return-from resident-luft-world-center nil))
    (let* ((domain (luft-world-materialization-domain materialization))
           (x-period (luft:world-domain-x-period domain))
           ;; Luvcraft Z is LUFT's second horizontal axis.
           (z-period (luft:world-domain-y-period domain))
           (vertical-origin
             (luft-world-materialization-vertical-origin materialization))
           (low-luft-z (- minimum-y vertical-origin))
           (high-luft-z (- (1- maximum-y) vertical-origin)))
      (unless (< (- maximum-x minimum-x) x-period)
        (error "Resident X span [~D,~D) fills or exceeds LUFT period ~D."
               minimum-x maximum-x x-period))
      (unless (< (- maximum-z minimum-z) z-period)
        (error "Resident Z span [~D,~D) fills or exceeds LUFT period ~D."
               minimum-z maximum-z z-period))
      (unless (and (<= 0 low-luft-z)
                   (< high-luft-z (1- luft:+vertical-cell-rows+)))
        (error
         "Resident vertical cells [~D,~D] map outside LUFT cell rows 0..~D."
         minimum-y (1- maximum-y) (- luft:+vertical-cell-rows+ 2)))
      (vector (floor (+ minimum-x maximum-x) 2)
              (floor (+ minimum-z maximum-z) 2)))))

(defun reconcile-luft-world-materialization (materialization)
  "Publish all pending authored solidity as at most one exact LUFT edit.

Observer events maintain exact resident-solid counts at canonical LUFT cells;
PENDING-SOLID-CELLS only says which counts must be compared with the scene.
Thus a temporary alias which arrives and departs cannot erase an unchanged
resident solid.  Invalidated solidity is rebuilt as a complete candidate and
swapped in only after its one exact scene edit succeeds.  The current resident
bounds are validated before either the scene or resident center is published.
Returns the scene and the number of changed canonical cells."
  (check-type materialization luft-world-materialization)
  (let* ((scene (luft-world-materialization-scene materialization))
         (pending
           (luft-world-materialization-pending-solid-cells materialization))
         (window-dirty-p
           (luft-world-materialization-resident-window-dirty-p
            materialization))
         (rebuild-required-p
           (luft-world-materialization-solidity-rebuild-required-p
            materialization)))
    (when (and (not window-dirty-p)
               (not rebuild-required-p)
               (zerop (hash-table-count pending)))
      (return-from reconcile-luft-world-materialization (values scene 0)))
    (let* ((center
             (if window-dirty-p
                 (resident-luft-world-center materialization)
                 (luft-world-materialization-resident-center materialization)))
           (domain (luft-world-materialization-domain materialization))
           (candidate
             (when rebuild-required-p
               (build-luft-world-solidity-candidate materialization)))
           (resident-counts
             (if candidate
                 (luft-world-solidity-candidate-resident-solid-counts candidate)
                 (luft-world-materialization-resident-solid-counts
                  materialization)))
           (dirty
             (if candidate
                 (luft-world-rebuild-dirty-cells scene candidate)
                 pending)))
      (let ((edit (luft:make-chain domain))
            (changed 0))
        (maphash
         (lambda (geometry dirty-p)
           (declare (ignore dirty-p))
           (let ((desired-solid-p
                   (plusp (gethash geometry resident-counts 0)))
                 (current-solid-p
                   (luft.render:scene-cell-p
                    scene
                    (luft:site-x geometry)
                    (luft:site-y geometry)
                    (luft:site-z geometry))))
             (unless (eql (not (null desired-solid-p))
                          (not (null current-solid-p)))
               (luft:add-chain-site
                edit
                (luft:site-with-polarity
                 geometry (if desired-solid-p 1 -1)))
               (incf changed))))
         dirty)
        (when (plusp changed)
          ;; One reconciliation is one exact revision, regardless of how many
          ;; authored edits or residency transitions it coalesced.
          (luft.render:apply-scene-edit scene edit))
        (when candidate
          (publish-luft-world-solidity-candidate materialization candidate))
        (clrhash pending)
        (setf (luft-world-materialization-resident-center materialization)
              center
              (luft-world-materialization-resident-window-dirty-p
               materialization)
              nil
              (luft-world-materialization-solidity-rebuild-required-p
               materialization)
              nil)
        (values scene changed)))))
